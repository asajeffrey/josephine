#![feature(const_fn)]
#![allow(dead_code)]
#![deny(unsafe_code)]

extern crate js;
extern crate libc;
#[macro_use] extern crate linjs;
#[macro_use] extern crate linjs_derive;

use js::jsapi;
use js::jsapi::HandleObject;
use js::jsapi::JSClass;
use js::jsapi::JSClassOps;
use js::jsapi::JSNativeWrapper;
use js::jsapi::JSPropertySpec;
use js::jsapi::JS_InitClass;
use js::jsapi::JS_InitStandardClasses;
use js::jsapi::Value;

use libc::c_char;
use libc::c_uint;

use linjs::CanAlloc;
use linjs::CanInitialize;
use linjs::CanRoot;
use linjs::Initialized;
use linjs::HasClass;
use linjs::JSContext;
use linjs::JSDelegate;
use linjs::JSInitializer;
use linjs::JSManaged;
use linjs::JSRunnable;
use linjs::null_property;
use linjs::null_wrapper;

use std::ptr;

// -------------------------------------------------------------------

type Window<'a, C> = JSManaged<'a, C, NativeWindow<'a, C>>;

#[derive(JSTraceable, JSManageable)]
struct NativeWindow<'a, C> {
    console: Console<'a, C>,
    document: Document<'a, C>,
}

type DOMContext<'a, C> = JSContext<Initialized<Window<'a, C>>>;

impl<'a, C> HasClass for NativeWindow<'a, C> {
    type Class = WindowClass;
}

fn init_window<'a, C, S>(cx: JSContext<S>) -> DOMContext<'a, C> where
    C: 'a,
    S: CanInitialize<C>,
{
    let mut cx = cx.pre_init();
    rooted!(in(cx) let console = new_console(&mut cx));
    rooted!(in(cx) let document = new_document(&mut cx));
    cx.post_init(NativeWindow {
        console: console,
        document: document,
    })
}

struct WindowClass;

impl WindowMethods for WindowClass {
    fn Console<'a, C>(cx: &'a mut DOMContext<'a, C>, this: Window<'a, C>) -> Console<'a, C> {
        this.get(cx).console
    }

    fn Document<'a, C>(cx: &'a mut DOMContext<'a, C>, this: Window<'a, C>) -> Document<'a, C> {
        this.get(cx).document
    }
}

impl JSDelegate for WindowClass {
    type Target = WindowInitializer;
}

// Stuff which is produced by codegen

#[allow(non_snake_case)]
trait WindowMethods {
    fn Console<'a, C>(cx: &'a mut DOMContext<'a, C>, this: Window<'a, C>) -> Console<'a, C>;
    fn Document<'a, C>(cx: &'a mut DOMContext<'a, C>, this: Window<'a, C>) -> Document<'a, C>;
}

static WINDOW_CLASS: JSClass = JSClass {
    name: b"Window\0" as *const u8 as *const c_char,
    flags: js::JSCLASS_IS_GLOBAL,
    cOps: &JSClassOps {
        addProperty: None,
        call: None,
        construct: None,
        delProperty: None,
        enumerate: None,
        finalize: None,
        getProperty: None,
        hasInstance: None,
        mayResolve: None,
        resolve: None,
        setProperty: None,
        trace: None,
    },
    reserved: [0 as *mut _; 3],
};

const WINDOW_PROPERTIES: &[JSPropertySpec] = &[
    JSPropertySpec {
        name: b"document\0" as *const u8 as *const c_char,
        flags: 0,
        getter: JSNativeWrapper {
            op: Some(window_document_getter),
            info: ptr::null(),
        },
        setter: null_wrapper(),
    },
    null_property(),
];

#[allow(unsafe_code)]
unsafe extern "C" fn window_document_getter(_cx: *mut jsapi::JSContext, _argc: c_uint, _vp: *mut Value) -> bool {
    // let args = CallArgs::from_vp(vp, argc);
    // let window = args.thisv();
    // let ref mut cx = JSContext::from(cx);
    // let result = WindowClass::Console(cx, window);
    // args.rval().set(result);
    true
}

// Stuff which could be produced by codegen

struct WindowInitializer;

impl JSInitializer for WindowInitializer {
    #[allow(unsafe_code)]
    unsafe fn js_init_class(cx: *mut jsapi::JSContext, obj: HandleObject) {
        JS_InitStandardClasses(cx, obj);
        JS_InitClass(
            cx,
            obj,
            HandleObject::null(),
            &WINDOW_CLASS,
            None,
            0,
            &WINDOW_PROPERTIES[0],
            ptr::null(),
            ptr::null(),
            ptr::null()
        );
    }
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

type Document<'a, C> = JSManaged<'a, C, NativeDocument<'a, C>>;

#[derive(JSTraceable, JSManageable)]
struct NativeDocument<'a, C> {
    body: Element<'a, C>,
}

fn new_document<'a, C, S>(cx: &'a mut JSContext<S>) -> Document<'a, C> where
    C: 'a,
    S: CanAlloc<C> + CanRoot,
{
    rooted!(in(cx) let body = new_element(cx));
    cx.manage(NativeDocument {
        body: body,
    })
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
