/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use super::JSContext;
use super::JSManaged;
use super::JSTraceable;

use js::jsapi::JSTracer;

use std::cell::UnsafeCell;
use std::marker::PhantomData;
use std::mem;
use std::ops::Deref;
use std::ops::DerefMut;
use std::os::raw::c_void;
use std::ptr;

/// A stack allocated root containing data of type `T` with lifetime `'a`.
pub struct JSRoot<'a, T: 'a> {
    value: Option<T>,
    pin: JSUntypedPinnedRoot,
    marker: PhantomData<&'a ()>, // NOTE: this is variant in `'a`.
}

const DUMMY: *mut JSTraceable = &() as *const JSTraceable as *mut JSTraceable;

impl<'a, T:'a> JSRoot<'a, T> {
    pub fn new<S>(_cx: &mut JSContext<S>) -> JSRoot<'a, T> {
        JSRoot {
            value: None,
            pin: JSUntypedPinnedRoot {
                value: DUMMY,
                next: ptr::null_mut(),
                prev: ptr::null_mut(),
            },
            marker: PhantomData,
        }
    }
}

/// A stack allocated root that has been pinned, so the backing store can't move.
pub struct JSPinnedRoot<'a, T: 'a> (&'a mut JSRoot<'a, T>);

/// A doubly linked list with all the pinned roots.
#[derive(Eq, PartialEq)]
pub struct JSPinnedRoots(*mut JSUntypedPinnedRoot);

impl JSPinnedRoots {
    unsafe fn insert(&mut self, root: &mut JSUntypedPinnedRoot) {
        debug!("Adding root {:p}.", root);
        root.next = self.0;
        root.prev = ptr::null_mut();
        if let Some(next) = root.next.as_mut() {
            next.prev = root;
        }
        self.0 = root;
    }

    unsafe fn remove(&mut self, root: &mut JSUntypedPinnedRoot) {
        if let Some(next) = root.next.as_mut() {
            debug!("Removing root.next.prev for {:p}.", root);
            next.prev = root.prev;
        }
        if let Some(prev) = root.prev.as_mut() {
            debug!("Removing root.prev.next for {:p}.", root);
            prev.next = root.next;
        } else if self.0 == root {
            debug!("Removing root {:p} from rootset.", root);
            self.0 = root.next;
        }
        root.value = DUMMY;
        root.next = ptr::null_mut();
        root.prev = ptr::null_mut();
    }
}

/// The thread-local list of all roots
thread_local! { static ROOTS: UnsafeCell<JSPinnedRoots> = UnsafeCell::new(JSPinnedRoots(ptr::null_mut())); }

unsafe fn thread_local_roots() -> *mut JSPinnedRoots {
    ROOTS.with(UnsafeCell::get)
}

pub unsafe extern "C" fn trace_thread_local_roots(trc: *mut JSTracer, _: *mut c_void) {
    debug!("Tracing roots.");

    if let Some(roots) = thread_local_roots().as_mut() {
        let mut curr = roots.0;
        while let Some(root) = curr.as_ref() {
            debug!("Tracing root {:p}.", root);
            (&*root.value).trace(trc);
            curr = root.next;
        }
    }

    debug!("Done tracing roots.");
}

/// A stack allocated root that has been pinned, but we don't have a type for the contents
struct JSUntypedPinnedRoot {
    value: *mut JSTraceable,
    next: *mut JSUntypedPinnedRoot,
    prev: *mut JSUntypedPinnedRoot,
}

impl Drop for JSUntypedPinnedRoot {
    fn drop(&mut self) {
        unsafe { (&mut *thread_local_roots()).remove(self); }
    }
}

/// Data which can be rooted.

pub unsafe trait JSLifetime<'a> {
    type Aged;

    unsafe fn change_lifetime(self) -> Self::Aged where Self: Sized {
        let result = mem::transmute_copy(&self);
        mem::forget(self);
        result
    }

    fn contract_lifetime(self) -> Self::Aged where Self: 'a + Sized {
        unsafe { self.change_lifetime() }
    }

    fn in_root(self, root: &'a mut JSRoot<'a, Self::Aged>) -> Self::Aged where
        Self: Sized,
        Self::Aged: Copy + JSTraceable,
    {
        root.set(self)
    }
}

unsafe impl<'a> JSLifetime<'a> for String { type Aged = String; }
unsafe impl<'a> JSLifetime<'a> for usize { type Aged = usize; }
unsafe impl<'a, T> JSLifetime<'a> for Option<T> where T: JSLifetime<'a> { type Aged = Option<T::Aged>; }
unsafe impl<'a, T> JSLifetime<'a> for Vec<T> where T: JSLifetime<'a> { type Aged = Vec<T::Aged>; }

unsafe impl<'a, 'b, C, T> JSLifetime<'a> for JSManaged<'b, C, T> where
    T: JSLifetime<'a>,
{
    type Aged = JSManaged<'a, C, T::Aged>;
}

impl<'a, T> JSRoot<'a, T> {
    // This uses Sgeo's trick to stop the JSRoot being forgotten by `mem::forget`.
    // The pin takes a `&'a mut JSRoot<'a, T>`, so borrows the root for the
    // duration of `'a`, so the type is no longer valid after the pin is dropped.
    pub fn pin<U>(&'a mut self, value: U) -> &'a T where
        T: JSTraceable,
        U: JSLifetime<'a, Aged=T>,
    {
        let roots = unsafe { &mut *thread_local_roots() };
        self.value = Some(unsafe { value.change_lifetime() });
        self.pin.value = self.value.as_mut_ptr();
        unsafe { roots.insert(&mut self.pin) };
        self.value.as_ref().unwrap()
    }

    pub fn set<U>(&'a mut self, value: U) -> T where
        T: Copy + JSTraceable,
        U: JSLifetime<'a, Aged=T>,
    {
        *self.pin(value)
    }

    pub unsafe fn unpin(&mut self) {
        let roots = &mut *thread_local_roots();
        roots.remove(&mut self.pin);
    }
}

impl<'a, T> Deref for JSPinnedRoot<'a, T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.0.value.as_ref().unwrap()
    }
}

impl<'a, T> DerefMut for JSPinnedRoot<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        self.0.value.as_mut().unwrap()
    }
}

unsafe impl<#[may_dangle] 'a, #[may_dangle] T> Drop for JSRoot<'a, T> {
    fn drop(&mut self) {
        unsafe { self.unpin() }
    }
}
