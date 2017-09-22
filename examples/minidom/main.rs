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

use js::jsapi;
use js::jsapi::JSClass;
use js::jsapi::JSClassOps;
use js::jsapi::JSNativeWrapper;
use js::jsapi::JSPropertySpec;
use js::jsapi::Value;

use libc::c_char;
use libc::c_uint;

use linjs::CanAccess;
use linjs::CanAlloc;
use linjs::CanCreate;
use linjs::HasClass;
use linjs::HasInstance;
use linjs::HasGlobal;
use linjs::InCompartment;
use linjs::Initialized;
use linjs::JSContext;
use linjs::JSInitializer;
use linjs::JSManaged;
use linjs::JSRunnable;
use linjs::null_property;
use linjs::null_wrapper;
use linjs::jsclass_global_flags_with_slots;

use std::ptr;

// -------------------------------------------------------------------

type Window<'a, C> = JSManaged<'a, C, WindowClass>;

#[derive(JSTraceable, JSRootable)]
struct NativeWindow<'a, C> {
    console: Console<'a, C>,
    document: Document<'a, C>,
}

fn init_window<'a, C, S>(cx: JSContext<S>) -> JSContext<Initialized<C>> where
    S: CanCreate<C>,
    C: HasGlobal<WindowClass>,
{
    let mut cx = cx.create_compartment();
    rooted!(in(cx) let console = new_console(&mut cx));
    rooted!(in(cx) let document = new_document(&mut cx));
    cx.global_manage(NativeWindow {
        console: console,
        document: document,
    })
}

struct WindowClass;

impl<'a, C> HasClass for NativeWindow<'a, C> {
    type Class = WindowClass;
    type Init = WindowInitializer;
}

impl<'a, C> HasInstance<'a, C> for WindowClass {
    type Instance = NativeWindow<'a, C>;
}

impl WindowMethods for WindowClass {
    fn Console<'a, C, S>(cx: &'a mut JSContext<S>, this: Window<'a, C>) -> Console<'a, C> where
        S: CanAccess + InCompartment<C>,
        C: 'a,
    {
        this.borrow(cx).console
    }

    fn Document<'a, C, S>(cx: &'a mut JSContext<S>, this: Window<'a, C>) -> Document<'a, C> where
        S: CanAccess + InCompartment<C>,
        C: 'a,
    {
        this.borrow(cx).document
    }
}

// -------------------------------------------------------------------

type Console<'a, C> = JSManaged<'a, C, NativeConsoleClass>;

#[derive(HasClass, JSTraceable, JSRootable)]
struct NativeConsole();

fn new_console<'a, C, S>(cx: &'a mut JSContext<S>) -> Console<'a, C> where
    C: 'a,
    S: CanAlloc + InCompartment<C>,
{
    cx.manage(NativeConsole())
}

// -------------------------------------------------------------------

type Document<'a, C> = JSManaged<'a, C, NativeDocumentClass>;

#[derive(HasClass, JSTraceable, JSRootable)]
struct NativeDocument<'a, C> {
    body: Element<'a, C>,
}

fn new_document<'a, C, S>(cx: &'a mut JSContext<S>) -> Document<'a, C> where
    C: 'a,
    S: CanAlloc + InCompartment<C>,
{
    rooted!(in(cx) let body = new_element(cx));
    cx.manage(NativeDocument {
        body: body,
    })
}

// -------------------------------------------------------------------

type Element<'a, C> = JSManaged<'a, C, NativeElementClass>;

#[derive(HasClass, JSTraceable, JSRootable)]
struct NativeElement<'a, C> {
    parent: Option<Element<'a, C>>,
    children: Vec<Element<'a, C>>,
}

fn new_element<'a, C, S>(cx: &'a mut JSContext<S>) -> Element<'a, C> where
    S: CanAlloc + InCompartment<C>,
    C: 'a,
{
    cx.manage(NativeElement {
        parent: None,
        children: Vec::new(),
    })
}

// -------------------------------------------------------------------

struct Main;

impl JSRunnable<WindowClass> for Main {
    fn run<C, S>(self, cx: JSContext<S>) where
        S: CanCreate<C>,
        C: HasGlobal<WindowClass>,
    {
        let ref mut _cx = init_window(cx);
    }
}

fn main() {
    env_logger::init().unwrap();

    debug!("Running main");
    Main.start();
    debug!("Done running main");
}

// -------------------------------------------------------------------

// Stuff which is produced by codegen

#[allow(non_snake_case)]
trait WindowMethods {
    fn Console<'a, C, S>(cx: &'a mut JSContext<S>, this: Window<'a, C>) -> Console<'a, C> where
        S: 'a + CanAccess + CanAlloc + InCompartment<C>,
        C: 'a + HasGlobal<WindowClass>;

    fn Document<'a, C, S>(cx: &'a mut JSContext<S>, this: Window<'a, C>) -> Document<'a, C> where
        S: 'a + CanAccess + CanAlloc + InCompartment<C>,
        C: 'a + HasGlobal<WindowClass>;
}

static WINDOW_CLASS: JSClass = JSClass {
    name: b"Window\0" as *const u8 as *const c_char,
    flags: jsclass_global_flags_with_slots(1),
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
    unsafe fn global_classp() -> *const JSClass {
        &WINDOW_CLASS
    }

    #[allow(unsafe_code)]
    unsafe fn properties() -> *const JSPropertySpec {
        &WINDOW_PROPERTIES[0]
    }
}
