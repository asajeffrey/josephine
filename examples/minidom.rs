#![allow(dead_code)]
#![deny(unsafe_code)]

extern crate libc;
#[macro_use] extern crate linjs;
#[macro_use] extern crate linjs_derive;

use linjs::CanAlloc;
use linjs::CanInitialize;
use linjs::Initialized;
use linjs::JSThreadLocalClass;
use linjs::JSOwnedClass;
use linjs::JSContext;
use linjs::JSGlobalizeable;
use linjs::JSManaged;
use linjs::JSRunnable;

// -------------------------------------------------------------------

type Window<'a, C> = JSManaged<'a, C, NativeWindow<'a, C>>;

#[derive(JSTraceable, JSManageable)]
struct NativeWindow<'a, C> {
    console: Console<'a, C>,
    body: Element<'a, C>,
}

type DOMContext<'a, C> = JSContext<Initialized<Window<'a, C>>>;

fn init_window<'a, C, S>(cx: JSContext<S>) -> DOMContext<'a, C> where
    C: 'a,
    S: CanInitialize<C>,
{
    let mut cx = cx.pre_init();
    rooted!(in(cx) let console = new_console(&mut cx));
    rooted!(in(cx) let body = new_element(&mut cx));
    cx.post_init(NativeWindow {
        console: console,
        body: body,
    })
}

// This is boilerplate which should be deriveable.

thread_local! { static WINDOW_CLASS: JSOwnedClass = JSOwnedClass::new("Window"); }

impl<'a, C> JSGlobalizeable for NativeWindow<'a, C> {
    fn js_class() -> JSThreadLocalClass { &WINDOW_CLASS }
}

// -------------------------------------------------------------------

type Console<'a, C> = JSManaged<'a, C, NativeConsole>;

#[derive(JSTraceable, JSManageable)]
struct NativeConsole();

fn new_console<'a, C, S>(cx: &'a mut JSContext<S>) -> Console<'a, C> where
    C: 'a,
    S: CanAlloc<C>,
{
    cx.manage(NativeConsole())
}

// -------------------------------------------------------------------

type Element<'a, C> = JSManaged<'a, C, NativeElement<'a, C>>;

#[derive(JSTraceable, JSManageable)]
struct NativeElement<'a, C> {
    parent: Option<Element<'a, C>>,
    children: Vec<Element<'a, C>>,
}

fn new_element<'a, C, S>(cx: &'a mut JSContext<S>) -> Element<'a, C> where
    S: CanAlloc<C>,
    C: 'a,
{
    cx.manage(NativeElement {
        parent: None,
        children: Vec::new(),
    })
}

// -------------------------------------------------------------------

struct Main;

impl JSRunnable for Main {
    fn run<C, S>(self, cx: JSContext<S>) where S: CanInitialize<C> {
        let ref mut _cx = init_window(cx);
    }
}

fn main() {
    Main.start();
}
