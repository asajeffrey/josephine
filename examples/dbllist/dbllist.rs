/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

//! A simple doubly linked list class.

use josephine::CanAccess;
use josephine::CanAlloc;
use josephine::Compartment;
use josephine::InCompartment;
use josephine::Initialized;
use josephine::IsInitialized;
use josephine::IsInitializing;
use josephine::JSContext;
use josephine::JSManaged;
use josephine::JSLifetime;

#[derive(Copy, Clone, JSTraceable, JSLifetime, JSCompartmental)]
pub struct DoublyLinkedList<'a, C>(JSManaged<'a, C, NativeDoublyLinkedList<'a, C>>);

#[derive(JSTraceable, JSLifetime, JSInitializable, JSCompartmental)]
pub struct NativeDoublyLinkedList<'a, C> {
    first: Option<Cell<'a, C>>,
    last: Option<Cell<'a, C>>,
}

#[derive(Copy, Clone, JSTraceable, JSLifetime, JSCompartmental)]
pub struct Cell<'a, C>(JSManaged<'a, C, NativeCell<'a, C>>);

#[derive(JSTraceable, JSLifetime, JSInitializable, JSCompartmental)]
pub struct NativeCell<'a, C> {
    data: String,
    prev: Option<Cell<'a, C>>,
    next: Option<Cell<'a, C>>,
}

impl<'a, C:'a> DoublyLinkedList<'a, C> {
    pub fn init<S>(cx: JSContext<S>) -> JSContext<Initialized<'a, C, NativeDoublyLinkedList<'a, C>>> where
        S: IsInitializing<'a, C, NativeDoublyLinkedList<'a, C>>,
    {
        cx.global_manage(NativeDoublyLinkedList { first: None, last: None })
    }

    pub fn global<S>(cx: &JSContext<S>) -> DoublyLinkedList<'a, C> where
        S: IsInitialized<'a, C, NativeDoublyLinkedList<'a, C>>,
    {
        DoublyLinkedList(cx.global())
    }
}

impl<'a, C:'a> DoublyLinkedList<'a, C> {
    pub fn first<S>(self, cx: &'a JSContext<S>) -> Option<Cell<'a, C>> where
        S: CanAccess,
        C: Compartment,
    {
        self.0.borrow(cx).first
    }

    pub fn last<S>(self, cx: &'a JSContext<S>) -> Option<Cell<'a, C>> where
        S: CanAccess,
        C: Compartment,
    {
        self.0.borrow(cx).last
    }

    pub fn push<S>(self, cx: &mut JSContext<S>, data: String) where
        S: CanAccess + CanAlloc + InCompartment<C>,
        C: Compartment,
    {
        let ref mut root1 = cx.new_root();
        let ref mut root2 = cx.new_root();
        let old_last = self.last(cx).in_root(root1);
        let new_last = Cell::new(cx, data, old_last, None).in_root(root2);
        self.0.borrow_mut(cx).last = Some(new_last);
        if let Some(old_last) = old_last {
            old_last.0.borrow_mut(cx).next = Some(new_last);
        } else {
            self.0.borrow_mut(cx).first = Some(new_last);
        }
    }
}

impl<'a, C:'a> Cell<'a, C> {
    fn new<S>(cx: &'a mut JSContext<S>, data: String, prev: Option<Cell<'a, C>>, next: Option<Cell<'a, C>>) -> Cell<'a, C> where
        S: CanAlloc + InCompartment<C>,
        C: Compartment,
    {
        Cell(cx.manage(NativeCell {
            data: data,
            prev: prev,
            next: next,
        }))
    }

    pub fn data<S>(self, cx: &'a JSContext<S>) -> &'a str where
        S: CanAccess,
        C: Compartment,
    {
        &*self.0.borrow(cx).data
    }

    pub fn prev<S>(self, cx: &'a JSContext<S>) -> Option<Cell<'a, C>> where
        S: CanAccess,
        C: Compartment,
    {
        self.0.borrow(cx).prev
    }

    pub fn next<S>(self, cx: &'a JSContext<S>) -> Option<Cell<'a, C>> where
        S: CanAccess,
        C: Compartment,
    {
        self.0.borrow(cx).next
    }
}
