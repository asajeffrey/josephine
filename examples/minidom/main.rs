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
use linjs::JSRunnable;

use minidom::init_window;
use minidom::WindowClass;

struct Main;

impl JSRunnable<WindowClass> for Main {
    fn run<C, S>(self, cx: JSContext<S>) where
        S: CanCreate<C>,
        C: HasGlobal<WindowClass>,
    {
        let ref mut cx = init_window(cx);

        // Just GCing for testing purposes
        cx.gc();
    }
}

fn main() {
    env_logger::init().unwrap();

    debug!("Running main");
    Main.start();
    debug!("Done running main");
}
