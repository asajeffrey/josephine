#![allow(dead_code)]

extern crate linjs;

use linjs::JSCompartment;
use linjs::JSContext;
use linjs::JSManaged;
use linjs::JSManageable;

// -------------------------------------------------------------------

type Window<'a, C> = JSManaged<'a, C, NativeWindow<'a, C>>;

struct NativeWindow<'a, C: JSCompartment> {
    document: Element<'a, C>,
}

unsafe impl<'a, 'b, C: JSCompartment> JSManageable<'b, C> for NativeWindow<'a, C> {
    type Aged = NativeWindow<'b, C>;
}

// -------------------------------------------------------------------

type Element<'a, C> = JSManaged<'a, C, NativeElement<'a, C>>;

struct NativeElement<'a, C: JSCompartment> {
    parent: Option<Element<'a, C>>,
    children: Vec<Element<'a, C>>,
}

unsafe impl<'a, 'b, C: JSCompartment> JSManageable<'b, C> for NativeElement<'a, C> {
    type Aged = NativeElement<'b, C>;
}

// -------------------------------------------------------------------

struct Main;

impl Main {
    fn run<'a, C: JSCompartment>(self, cx: &'a mut JSContext<C>) {
        let roots = cx.roots();
        let document = cx.manage(NativeElement {
            parent: None,
            children: Vec::new(),
        }).root(&roots);
        let _window = cx.manage(NativeWindow {
            document: document,
        }).root(&roots);
        // let cx = cx.set_global(window);
        // cx.execute("console.log('hi');");
    }
}

fn main() {
    println!("hello");
}
