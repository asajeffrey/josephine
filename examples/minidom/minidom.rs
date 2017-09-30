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

use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::ops::Deref;

// -------------------------------------------------------------------

#[derive(Debug, Eq, PartialEq, JSTraceable, JSRootable)]
pub struct Window<'a, C> (JSManaged<'a, C, WindowClass>);

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
    rooted!(in(cx) let console = Console::new(&mut cx));
    rooted!(in(cx) let document = Document::new(&mut cx));
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

impl<'a, C> WindowMethods<'a, C> for Window<'a, C> {
    fn Console<S>(self, cx: &'a JSContext<S>) -> Console<'a, C> where
        S: CanAccess + InCompartment<C>,
        C: 'a,
    {
        self.borrow(cx).console
    }

    fn Document<S>(self, cx: &'a JSContext<S>) -> Document<'a, C> where
        S: CanAccess + InCompartment<C>,
        C: 'a,
    {
        self.borrow(cx).document
    }
}

// -------------------------------------------------------------------

#[derive(JSTraceable, JSRootable)]
pub struct Console<'a, C> (JSManaged<'a, C, NativeConsoleClass>);

#[derive(HasClass, JSTraceable, JSRootable)]
pub struct NativeConsole();

impl<'a, C> Console<'a, C> {
    fn new<S>(cx: &'a mut JSContext<S>) -> Console<'a, C> where
        S: CanAlloc + InCompartment<C>,
    {
        Console(cx.manage(NativeConsole()))
    }
}

// -------------------------------------------------------------------

#[derive(Debug, Eq, PartialEq, JSTraceable, JSRootable)]
pub struct Document<'a, C> (JSManaged<'a, C, NativeDocumentClass>);

#[derive(HasClass, JSTraceable, JSRootable)]
pub struct NativeDocument<'a, C> {
    body: Element<'a, C>,
}

impl<'a, C> Document<'a, C> {
    pub fn new<S>(cx: &'a mut JSContext<S>) -> Document<'a, C> where
        S: CanAlloc + InCompartment<C>,
    {
        rooted!(in(cx) let body = Element::new(cx));
        Document(cx.manage(NativeDocument {
            body: body,
        }))
    }
}

// -------------------------------------------------------------------

#[derive(Debug, Eq, PartialEq, JSTraceable, JSRootable)]
pub struct Element<'a, C> (JSManaged<'a, C, NativeElementClass>);

#[derive(HasClass, JSTraceable, JSRootable)]
pub struct NativeElement<'a, C> {
    parent: Option<Element<'a, C>>,
    children: Vec<Element<'a, C>>,
}

impl<'a, C> Element<'a, C> {
    pub fn new<S>(cx: &'a mut JSContext<S>) -> Element<'a, C> where
        S: CanAlloc + InCompartment<C>,
    {
        Element(cx.manage(NativeElement {
            parent: None,
            children: Vec::new(),
        }))
    }
}

// -------------------------------------------------------------------

// The rest of the file is stuff which we should be able to write a #derive for

impl<'a, C> From<JSManaged<'a, C, WindowClass>> for Window<'a, C> {
    fn from(value: JSManaged<'a, C, WindowClass>) -> Window<'a, C> {
        Window(value)
    }
}

impl<'a, C> Copy for Window<'a, C> {
}

impl<'a, C> Clone for Window<'a, C> {
    fn clone(&self) -> Self {
        Window(self.0)
    }
}

impl<'a, C> Deref for Window<'a, C> {
    type Target = JSManaged<'a, C, WindowClass>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a, C> From<JSManaged<'a, C, NativeConsoleClass>> for Console<'a, C> {
    fn from(value: JSManaged<'a, C, NativeConsoleClass>) -> Console<'a, C> {
        Console(value)
    }
}

impl<'a, C> Copy for Console<'a, C> {
}

impl<'a, C> Clone for Console<'a, C> {
    fn clone(&self) -> Self {
        Console(self.0)
    }
}

impl<'a, C> Deref for Console<'a, C> {
    type Target = JSManaged<'a, C, NativeConsoleClass>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a, C> PartialEq for Console<'a, C> {
    fn eq(&self, other: &Console<'a, C>) -> bool {
        &self.0 == &other.0
    }
}

impl<'a, C> Debug for Console<'a, C> {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), fmt::Error> {
        self.0.fmt(fmt)
    }
}

impl<'a, C> Copy for Document<'a, C> {
}

impl<'a, C> Clone for Document<'a, C> {
    fn clone(&self) -> Self {
        Document(self.0)
    }
}

impl<'a, C> Deref for Document<'a, C> {
    type Target = JSManaged<'a, C, NativeDocumentClass>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a, C> Copy for Element<'a, C> {
}

impl<'a, C> Clone for Element<'a, C> {
    fn clone(&self) -> Self {
        Element(self.0)
    }
}

impl<'a, C> Deref for Element<'a, C> {
    type Target = JSManaged<'a, C, NativeElementClass>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
