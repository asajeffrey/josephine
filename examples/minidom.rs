extern crate linjs;
#[macro_use] extern crate linjs_derive;

use linjs::CanAlloc;
use linjs::CanInitialize;
use linjs::JSContext;
use linjs::JSManaged;
use linjs::JSManageable;
use linjs::JSRunnable;

use std::marker::PhantomData;

// -------------------------------------------------------------------

type Window<'a, C> = JSManaged<'a, C, NativeWindow<'a, C>>;

#[derive(JSManageable)]
struct NativeWindow<'a, C> {
    console: Console<'a, C>,
    body: Element<'a, C>,
}

// -------------------------------------------------------------------

type Console<'a, C> = JSManaged<'a, C, NativeConsole<'a, C>>;

#[derive(JSManageable)]
struct NativeConsole<'a, C>(PhantomData<(&'a(), C)>);

fn new_console<'a, C, S>(cx: &'a mut JSContext<S>) -> Console<'a, C> where
    C: 'a,
    S: CanAlloc<C>,
{
    cx.manage(NativeConsole(PhantomData))
}

// -------------------------------------------------------------------

type Element<'a, C> = JSManaged<'a, C, NativeElement<'a, C>>;

#[derive(JSManageable)]
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
        println!("Hello, world.");
    }
}

fn main() {
    Main.start();
}
