/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#![feature(const_fn)]
#![feature(const_ptr_null)]
#![allow(dead_code)]
#![deny(unsafe_code)]

extern crate env_logger;
extern crate mozjs as js;
extern crate libc;
#[macro_use] extern crate josephine;
#[macro_use] extern crate log;

mod fake_codegen;
mod minidom;

use josephine::JSContext;
use josephine::JSLifetime;
use minidom::Window;

fn main() {
    let _ = env_logger::init();

    debug!("Creating JSContext.");
    let ref mut cx = JSContext::new();

    debug!("Creating global.");
    let ref mut root = cx.new_root();
    let window = Window::new(cx).in_root(root);

    debug!("Entering compartment.");
    let ref mut cx = cx.enter_unknown_compartment(window.0);

    debug!("Printing hello");
    cx.evaluate("console.log('Hello World. 😃')").unwrap();

    debug!("Printing body");
    cx.evaluate("console.log(document.body.tagName)").unwrap();

    debug!("Done.");
}
