#![feature(const_fn)]
#![feature(const_ptr_null)]
#![allow(dead_code)]
#![deny(unsafe_code)]

extern crate env_logger;
extern crate js;
extern crate libc;
#[macro_use] extern crate linjs;
#[macro_use] extern crate linjs_derive;
#[macro_use] extern crate log;

mod fake_codegen;
mod minidom;

use linjs::CanCreate;
use linjs::HasGlobal;
use linjs::JSContext;

use minidom::WindowClass;

struct PrintHello;

fn main() {
    env_logger::init().unwrap();

    debug!("Creating JSContext.");
    let mut cx = JSContext::new();

    debug!("Creating compartment");
    cx.new_global::<WindowClass>();

    debug!("Printing hello");
    cx.evaluate("console.log('hello')");

    debug!("Done.");
}
