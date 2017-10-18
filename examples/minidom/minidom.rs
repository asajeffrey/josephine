use linjs::CanAccess;
use linjs::CanAlloc;
use linjs::Compartment;
use linjs::CanCreateCompartments;
use linjs::InCompartment;
use linjs::JSContext;
use linjs::JSInitializable;
use linjs::JSManaged;
use linjs::JSRootable;
use linjs::JSString;
use linjs::SOMEWHERE;

use fake_codegen::ConsoleInitializer;
use fake_codegen::ConsoleMethods;
use fake_codegen::WindowInitializer;
use fake_codegen::WindowMethods;

// -------------------------------------------------------------------

// TODO: the contents are pub so that codegen can get at it, this should be fixed!
// https://github.com/asajeffrey/linjs/issues/27
#[derive(Copy, Clone, Debug, Eq, PartialEq, JSTraceable, JSRootable, JSTransplantable)]
pub struct Window<'a, C> (pub JSManaged<'a, C, NativeWindow<'a, C>>);

#[derive(JSTraceable, JSRootable, JSTransplantable)]
pub struct NativeWindow<'a, C> {
    console: Console<'a, C>,
    document: Document<'a, C>,
}

pub struct WindowClass;

impl<'a, C> JSInitializable for NativeWindow<'a, C> {
    type Init = WindowInitializer;
}

impl<'a> Window<'a, SOMEWHERE> {
    pub fn new<S>(cx: &'a mut JSContext<S>) -> Window<'a, SOMEWHERE> where
        S: CanAlloc + CanCreateCompartments,
    {
        let mut cx = cx.create_compartment();
        let ref mut console_root = cx.new_root();
        let ref mut document_root = cx.new_root();
        let console = Console::new(&mut cx).in_root(console_root);
        let document = Document::new(&mut cx).in_root(document_root);
        let cx = cx.global_manage(NativeWindow {
            console: console,
            document: document,
        });
        let global: JSManaged<'a, _, _> = cx.global();
        Window(global.forget_compartment())
    }
}

impl<'a, C> WindowMethods<'a, C> for Window<'a, C> where C: 'a {
    fn Console<S>(self, cx: &'a JSContext<S>) -> Console<'a, C> where
        S: CanAccess,
        C: Compartment,
    {
        self.0.borrow(cx).console
    }

    fn Document<S>(self, cx: &'a JSContext<S>) -> Document<'a, C> where
        S: CanAccess,
        C: Compartment,
    {
        self.0.borrow(cx).document
    }

    fn Window<S>(self, _cx: &'a JSContext<S>) -> Window<'a, C> {
        self
    }
}

// -------------------------------------------------------------------

#[derive(Copy, Clone, JSTraceable, JSRootable, JSTransplantable)]
pub struct Console<'a, C> (pub JSManaged<'a, C, NativeConsole>);

#[derive(JSTraceable, JSRootable, JSTransplantable)]
pub struct NativeConsole(());

pub struct ConsoleClass;

impl JSInitializable for NativeConsole {
    type Init = ConsoleInitializer;
}

impl<'a, C> Console<'a, C> {
    fn new<S>(cx: &'a mut JSContext<S>) -> Console<'a, C> where
        S: CanAlloc + InCompartment<C>,
        C: Compartment,
    {
        Console(cx.manage(NativeConsole(())))
    }
}

impl<'a, C> ConsoleMethods<'a, C> for Console<'a, C> {
    fn Log<S>(self, _cx: &mut JSContext<S>, arg: JSString<'a, C>) {
        debug!("Logging");
        println!("{}", arg);
    }
}

// -------------------------------------------------------------------

#[derive(Copy, Clone, Debug, Eq, PartialEq, JSTraceable, JSRootable, JSTransplantable)]
pub struct Document<'a, C> (pub JSManaged<'a, C, NativeDocument<'a, C>>);

#[derive(JSTraceable, JSRootable, JSTransplantable)]
pub struct NativeDocument<'a, C> {
    body: Element<'a, C>,
}

impl<'a, C> JSInitializable for NativeDocument<'a, C> {}

impl<'a, C:'a> Document<'a, C> {
    pub fn new<S>(cx: &'a mut JSContext<S>) -> Document<'a, C> where
        S: CanAlloc + InCompartment<C>,
        C: Compartment,
    {
        let ref mut root = cx.new_root();
        let body = Element::new(cx).in_root(root);
        Document(cx.manage(NativeDocument {
            body: body,
        }))
    }
}

// -------------------------------------------------------------------

#[derive(Copy, Clone, Debug, Eq, PartialEq, JSTraceable, JSRootable, JSTransplantable)]
pub struct Element<'a, C> (pub JSManaged<'a, C, NativeElement<'a, C>>);

#[derive(JSTraceable, JSRootable, JSTransplantable)]
pub struct NativeElement<'a, C> {
    parent: Option<Element<'a, C>>,
    children: Vec<Element<'a, C>>,
}


impl<'a, C> JSInitializable for NativeElement<'a, C> {}

impl<'a, C:'a> Element<'a, C> {
    pub fn new<S>(cx: &'a mut JSContext<S>) -> Element<'a, C> where
        S: CanAlloc + InCompartment<C>,
        C: Compartment,
    {
        Element(cx.manage(NativeElement {
            parent: None,
            children: Vec::new(),
        }))
    }
}
