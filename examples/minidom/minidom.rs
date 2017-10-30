use josephine::CanAccess;
use josephine::CanAlloc;
use josephine::Compartment;
use josephine::InCompartment;
use josephine::JSContext;
use josephine::JSInitializable;
use josephine::JSManaged;
use josephine::JSRootable;
use josephine::JSString;
use josephine::SOMEWHERE;

use fake_codegen::ConsoleInitializer;
use fake_codegen::ConsoleMethods;
use fake_codegen::DocumentInitializer;
use fake_codegen::DocumentMethods;
use fake_codegen::ElementInitializer;
use fake_codegen::ElementMethods;
use fake_codegen::WindowInitializer;
use fake_codegen::WindowMethods;

// -------------------------------------------------------------------

// TODO: the contents are pub so that codegen can get at it, this should be fixed!
// https://github.com/asajeffrey/josephine/issues/27
#[derive(Copy, Clone, Debug, Eq, PartialEq, JSTraceable, JSRootable, JSCompartmental)]
pub struct Window<'a, C> (pub JSManaged<'a, C, NativeWindow<'a, C>>);

#[derive(JSTraceable, JSRootable, JSCompartmental)]
pub struct NativeWindow<'a, C> {
    console: Console<'a, C>,
    document: Document<'a, C>,
}

impl<'a, C> JSInitializable for NativeWindow<'a, C> {
    type Init = WindowInitializer;
}

impl<'a> Window<'a, SOMEWHERE> {
    pub fn new<S>(cx: &'a mut JSContext<S>) -> Window<'a, SOMEWHERE> where
        S: CanAccess + CanAlloc,
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
        Window(cx.global().forget_compartment())
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

#[derive(Copy, Clone, JSTraceable, JSRootable, JSCompartmental)]
pub struct Console<'a, C> (pub JSManaged<'a, C, NativeConsole>);

#[derive(JSTraceable, JSRootable, JSCompartmental)]
pub struct NativeConsole(());

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

#[derive(Copy, Clone, Debug, Eq, PartialEq, JSTraceable, JSRootable, JSCompartmental)]
pub struct Document<'a, C> (pub JSManaged<'a, C, NativeDocument<'a, C>>);

#[derive(JSTraceable, JSRootable, JSCompartmental)]
pub struct NativeDocument<'a, C> {
    body: Element<'a, C>,
}

impl<'a, C> JSInitializable for NativeDocument<'a, C> {
    type Init = DocumentInitializer;
}

impl<'a, C:'a> Document<'a, C> {
    pub fn new<S>(cx: &'a mut JSContext<S>) -> Document<'a, C> where
        S: CanAlloc + InCompartment<C>,
        C: Compartment,
    {
        let ref mut root1 = cx.new_root();
        let ref mut root2 = cx.new_root();
        let name = JSString::from_str(cx, "body").in_root(root1);
        let body = Element::new(cx, name).in_root(root2);
        Document(cx.manage(NativeDocument {
            body: body,
        }))
    }
}

impl<'a, C> DocumentMethods<'a, C> for Document<'a, C> where C: 'a {
    fn Body<S>(self, cx: &'a mut JSContext<S>) -> Element<'a, C> where
        S: CanAccess,
        C: Compartment,
    {
        self.0.borrow(cx).body
    }
}

// -------------------------------------------------------------------

#[derive(Copy, Clone, Debug, Eq, PartialEq, JSTraceable, JSRootable, JSCompartmental)]
pub struct Element<'a, C> (pub JSManaged<'a, C, NativeElement<'a, C>>);

#[derive(JSTraceable, JSRootable, JSCompartmental)]
pub struct NativeElement<'a, C> {
    name: JSString<'a, C>,
    parent: Option<Element<'a, C>>,
    children: Vec<Element<'a, C>>,
}

impl<'a, C> JSInitializable for NativeElement<'a, C> {
    type Init = ElementInitializer;
}

impl<'a, C:'a> Element<'a, C> {
    pub fn new<S>(cx: &'a mut JSContext<S>, name: JSString<C>) -> Element<'a, C> where
        S: CanAlloc + InCompartment<C>,
        C: Compartment,
    {
        Element(cx.manage(NativeElement {
            name: name,
            parent: None,
            children: Vec::new(),
        }))
    }

    fn shallow_clone<S, D:'a>(self, cx: &'a mut JSContext<S>) -> Element<'a, D> where
        S: CanAccess + CanAlloc + InCompartment<D>,
        C: Compartment,
        D: Compartment,
    {
        let ref mut root1 = cx.new_root();
        let ref mut root2 = cx.new_root();
        let name = self.0.borrow(cx).name.in_root(root1).clone_in(cx).in_root(root2);
        Element::new(cx, name)
    }

    fn clone_children_from<S, D:'a>(self, cx: &mut JSContext<S>, element: Element<D>) where
        S: CanAccess + CanAlloc + InCompartment<C>,
        C: Compartment,
        D: Compartment,
    {
        let mut i = 0;
        loop {
            let ref mut root = cx.new_root();
            let child = match element.0.borrow(cx).children.get(i).cloned().in_root(root) {
                None => return,
                Some(child) => child,
            };
            self.append_clone(cx, child);
            i = i+1;
        }
    }

    fn append_clone<S, D:'a>(self, cx: &mut JSContext<S>, child: Element<D>) where
        S: CanAccess + CanAlloc + InCompartment<C>,
        C: Compartment,
        D: Compartment,
    {
        let ref mut root = cx.new_root();
        let clone = child.shallow_clone(cx).in_root(root);
        clone.clone_children_from(cx, child);
        self.append_child(cx, clone);
    }

    fn append_child<S>(self, cx: &'a mut JSContext<S>, child: Element<'a, C>) where
        S: CanAccess + CanAlloc + InCompartment<C>,
        C: Compartment,
    {
        self.0.borrow_mut(cx).children.push(child);
        child.0.borrow_mut(cx).parent = Some(self);
    }

    fn in_compartment<S, D>(self, cx: &JSContext<S>) -> Option<Element<'a, D>> where
        S: InCompartment<D>,
    {
        self.0.in_compartment(cx).map(Element)
    }
}

impl<'a, C> ElementMethods<'a, C> for Element<'a, C> where C: 'a {
    fn Append<S, D>(self, cx: &'a mut JSContext<S>, child: Element<'a, D>) where
        S: CanAccess + CanAlloc,
        C: Compartment,
        D: Compartment,
    {
        let ref mut cx = cx.enter_known_compartment(self.0);
        if let Some(child) = child.in_compartment(cx) {
            self.append_child(cx, child);
        } else {
            self.append_clone(cx, child);
        }
    }

    fn Parent<S>(self, cx: &'a mut JSContext<S>) -> Option<Element<'a, C>> where
        S: CanAccess,
        C: Compartment,
    {
        self.0.borrow(cx).parent
    }

    fn TagName<S>(self, cx: &'a mut JSContext<S>) -> JSString<'a, C> where
        S: CanAccess,
        C: Compartment,
    {
        self.0.borrow(cx).name
    }
}
