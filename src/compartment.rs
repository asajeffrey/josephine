/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use super::InCompartment;
use super::JSContext;
use super::JSManaged;
use super::JSString;

use std::fmt::Debug;
use std::hash::Hash;
use std::marker::PhantomData;

/// A marker trait for JS compartments.
/// We mark it as `Copy` so that anything that uses `[#derive{Copy)]` will be copyable.
/// Ditto `Eq` and `Hash`.
pub trait Compartment: Copy + Debug + Eq + Hash {}

/// A wildcard compartment name.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct SOMEWHERE(());

/// A compartment name that remembers the lifetime it was bound for.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct BOUND<'a>(PhantomData<&'a mut &'a ()>);
impl<'a> Compartment for BOUND<'a> {}

/// Data which can be transplanted from compartment C into compartment D.
pub unsafe trait JSCompartmental<C, D> {
    type ChangeCompartment;
    fn is_in_compartment<S>(&self, _cx: &JSContext<S>) -> bool where
        S: InCompartment<D>;
}

unsafe impl<C, D> JSCompartmental<C, D> for String {
    type ChangeCompartment = String;
    fn is_in_compartment<S>(&self, _cx: &JSContext<S>) -> bool where
        S: InCompartment<D>
    {
        true
    }
}

unsafe impl<C, D> JSCompartmental<C, D> for usize {
    type ChangeCompartment = usize;
    fn is_in_compartment<S>(&self, _cx: &JSContext<S>) -> bool where
        S: InCompartment<D>
    {
        true
    }
}

unsafe impl<C, D> JSCompartmental<C, D> for () {
    type ChangeCompartment = ();
    fn is_in_compartment<S>(&self, _cx: &JSContext<S>) -> bool where
        S: InCompartment<D>
    {
        true
    }
}

unsafe impl<C, D, T> JSCompartmental<C, D> for Option<T> where
    T: JSCompartmental<C, D>
{
    type ChangeCompartment = Option<T::ChangeCompartment>;
    fn is_in_compartment<S>(&self, cx: &JSContext<S>) -> bool where
        S: InCompartment<D>
    {
        self.iter().all(|this| this.is_in_compartment(cx))
    }
}

unsafe impl<C, D, T> JSCompartmental<C, D> for Vec<T> where
    T: JSCompartmental<C, D>
{
    type ChangeCompartment = Vec<T::ChangeCompartment>;
    fn is_in_compartment<S>(&self, cx: &JSContext<S>) -> bool where
        S: InCompartment<D>
    {
        self.iter().all(|this| this.is_in_compartment(cx))
    }
}

unsafe impl<'a, C, D, T> JSCompartmental<C, D> for JSManaged<'a, C, T> where
    T: JSCompartmental<C, D>
{
    type ChangeCompartment = JSManaged<'a, D, T::ChangeCompartment>;
    fn is_in_compartment<S>(&self, cx: &JSContext<S>) -> bool where
        S: InCompartment<D>
    {
        self.in_compartment(cx).is_some()
    }
}


unsafe impl<'a, C, D> JSCompartmental<C, D> for JSString<'a, C> {
    type ChangeCompartment = JSString<'a, D>;
    fn is_in_compartment<S>(&self, _cx: &JSContext<S>) -> bool where
        S: InCompartment<D>
    {
        // Rather annoyingly the Rust jsapi bindings don't export
        // GetStringZone which we'd need in order to implement this properly
        false
    }
}

