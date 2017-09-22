use linjs::CanAccess;
use linjs::CanAlloc;
use linjs::CanCreate;
use linjs::HasClass;
use linjs::HasInstance;
use linjs::HasGlobal;
use linjs::InCompartment;
use linjs::Initialized;
use linjs::JSContext;
use linjs::JSManaged;

use fake_codegen::WindowInitializer;
use fake_codegen::WindowMethods;

// -------------------------------------------------------------------

pub type Window<'a, C> = JSManaged<'a, C, WindowClass>;

#[derive(JSTraceable, JSRootable)]
pub struct NativeWindow<'a, C> {
    console: Console<'a, C>,
    document: Document<'a, C>,
}

pub fn init_window<'a, C, S>(cx: JSContext<S>) -> JSContext<Initialized<C>> where
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

pub struct WindowClass;

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

pub type Console<'a, C> = JSManaged<'a, C, NativeConsoleClass>;

#[derive(HasClass, JSTraceable, JSRootable)]
pub struct NativeConsole();

pub fn new_console<'a, C, S>(cx: &'a mut JSContext<S>) -> Console<'a, C> where
    C: 'a,
    S: CanAlloc + InCompartment<C>,
{
    cx.manage(NativeConsole())
}

// -------------------------------------------------------------------

pub type Document<'a, C> = JSManaged<'a, C, NativeDocumentClass>;

#[derive(HasClass, JSTraceable, JSRootable)]
pub struct NativeDocument<'a, C> {
    body: Element<'a, C>,
}

pub fn new_document<'a, C, S>(cx: &'a mut JSContext<S>) -> Document<'a, C> where
    C: 'a,
    S: CanAlloc + InCompartment<C>,
{
    rooted!(in(cx) let body = new_element(cx));
    cx.manage(NativeDocument {
        body: body,
    })
}

// -------------------------------------------------------------------

pub type Element<'a, C> = JSManaged<'a, C, NativeElementClass>;

#[derive(HasClass, JSTraceable, JSRootable)]
pub struct NativeElement<'a, C> {
    parent: Option<Element<'a, C>>,
    children: Vec<Element<'a, C>>,
}

pub fn new_element<'a, C, S>(cx: &'a mut JSContext<S>) -> Element<'a, C> where
    S: CanAlloc + InCompartment<C>,
    C: 'a,
{
    cx.manage(NativeElement {
        parent: None,
        children: Vec::new(),
    })
}

// -------------------------------------------------------------------
