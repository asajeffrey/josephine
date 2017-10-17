#![feature(const_fn)]
#![feature(const_ptr_null)]
#![allow(dead_code)]
#![deny(unsafe_code)]

extern crate env_logger;
extern crate js;
extern crate libc;
extern crate linjs;
#[macro_use] extern crate linjs_derive;
#[macro_use] extern crate log;

mod fake_codegen;
mod minidom;

use linjs::JSContext;
use linjs::JSRootable;
use minidom::Window;

fn main() {
    env_logger::init().unwrap();

    debug!("Creating JSContext.");
    let mut cx = JSContext::new();

    debug!("Creating compartment");
    let ref mut root = cx.new_root();
    let _window = Window(cx.new_global().in_root(root));

    debug!("Printing hello");
    cx.evaluate("console.log('hello')").unwrap();

    debug!("Done.");
}
