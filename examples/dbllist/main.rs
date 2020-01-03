/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#[macro_use]
extern crate josephine;

mod dbllist;

use dbllist::DoublyLinkedList;

use josephine::JSContext;

fn main() {
    // Create a new JS context
    let ref mut cx = JSContext::new().expect("Creating a JSContext failed");

    // Create a new doubly-linked list in its own compartment
    let ref mut cx = DoublyLinkedList::init(cx.create_compartment());

    // Get the list from the newly created compartment
    let list = DoublyLinkedList::global(cx);

    // Push some data onto the list
    list.push(cx, String::from("hello"));
    list.push(cx, String::from("world"));

    // Check that the list has the data we expect
    assert_eq!(list.first(cx).unwrap().data(cx), "hello");
    assert_eq!(list.first(cx).unwrap().next(cx).unwrap().data(cx), "world");
    assert_eq!(list.last(cx).unwrap().data(cx), "world");
    assert_eq!(list.last(cx).unwrap().prev(cx).unwrap().data(cx), "hello");
}
