//! An outline of how linear types could be combined with JS-managed data
//!
//! The idea is to use the JS context as a linear token.
//! Each JS context gets its own type `Cx` which implements the `JSContext` trait.
//! Access to read-only JS-managed data requires a context of type `&Cx`.
//! Access to read-write JS-managed data requires a context of type `&mut Cx`.
//! Linear use of the context ensures that Rust's memory safety is preserved (er, I hope).
//!
//! This API doesn't address tracing, which would have to be handled similarly
//! to servo's current JS bindings.
//!
//! One thing that makes this easier is that we can arrange for any
//! calls into the JS engine which might trigger GC to take a `&mut Cx`
//! argument, which gives us control of when GC might occur.

use std::marker::PhantomData;

/// The trait for native JS-manageable data.
pub unsafe trait JSManageable<'a> {
    /// This type should have the same mnemory represention as `Self`.
    /// The only difference between `Self` and `Self::ChangeLifetime`
    /// is that any `JSManaged<'b, Cx, T>` should be replaced by
    /// `JSManaged<'a, Cx, T::ChangeLifetime>`.
    type ChangeLifetime: 'a;
}

/// The trait for accessing JS-managed data.
pub trait JSAccess<Cx>: Sized {
    /// Read-only access to native JS-managed data.
    fn get<'a, 'b:'a, T>(&'a self, managed: JSManaged<'b, Cx, T>) -> &'a T::ChangeLifetime where
        T: JSManageable<'a>;
    
    /// Read-write access to native JS-managed data.
    fn get_mut<'a, 'b:'a, T>(&'a mut self, managed: JSManaged<'b, Cx, T>) -> &'a mut T::ChangeLifetime where
        T: JSManageable<'a>;
}

/// The trait for JS contexts.
pub trait JSContext: JSAccess<Self> {
    /// Get a snapshot of the JS state.
    /// The snapshot only allows access to the methods that are guaranteed not to call GC,
    /// so we don't need to root JS-managed pointers during the lifetime of a snapshot.
    fn snapshot(&mut self) -> JSSnapshot<Self>;
    
    /// Give ownership of data to JS.
    /// This allocates JS heap, which may trigger GC.
    fn manage<'a, T>(&'a self, value: T) -> JSManaged<'a, Self, T>
        where T: 'a + JSManageable<'a, ChangeLifetime=T>;
    
    // A real implementation would also have JS methods such as those in jsapi.
}

/// A placholder for the real `JSTraceable`.
pub unsafe trait JSTraceable<Cx> {}

/// A user of a JS context implements `JSContextConsumer`, which is called back
/// with a fresh JS context.
pub trait JSContextConsumer<T> {
    /// This callback is called with a fresh JS context.
    fn consume<Cx>(self, cx: &mut Cx) -> T where Cx: JSContext;
}

/// To create a fresh JS context, the user implements `JSContextConsumer`
/// for their type `K`, builds a `k:K`, then calls `with_js_context(k)`.
/// ```rust
///   struct MyConsumer;
///   impl JSContextConsumer<()> for MyConsumer {
///      fn consume<Cx: JSContext>(cx: Cx) {
///         // Do stuff with the JS context cx.
///      }
///   }
///   with_js_context(MyConsumer);
/// ```
pub fn with_js_context<C, T>(consumer: C) -> T where
    C: JSContextConsumer<T>
{
    // A real implementation would allocate a JS context
    let mut cx = JSContextImpl {};
    consumer.consume(&mut cx)
}

/// The type of JS-managed data in a JS context `Cx`.
///
/// If the user has access to a `&JSManaged`, then the JS-managed
/// data is live for the lifetime of the reference. In particular,
/// this means that there should never be any stack-allocated
/// `JSManaged` values, since they would allow the user to construct
/// `&JSManaged` references with arbitrary lifetimes.
pub struct JSManaged<'a, Cx, T> {
    // JS reflector goes here
    raw: *mut T,
    marker: PhantomData<(&'a(),Cx)>,
}

impl<'a, Cx, T> Clone for JSManaged<'a, Cx, T> {
    fn clone(&self) -> Self {
        JSManaged {
            raw: self.raw,
            marker: self.marker,
        }
    }
}

impl<'a, Cx, T> Copy for JSManaged<'a, Cx, T> {
}

unsafe impl<'a, 'b, Cx, T> JSManageable<'b> for JSManaged<'a, Cx, T> where
    Cx: 'b,
    T: JSManageable<'b>,
{
    type ChangeLifetime = JSManaged<'b, Cx, T::ChangeLifetime>;
}

impl<'a, Cx, T> JSManaged<'a, Cx, T> {
    /// Convenience method for `access.get(self)`.
    pub fn get<'b, Access: JSAccess<Cx>>(self, access: &'b Access) -> &'b T::ChangeLifetime where
        T: JSManageable<'b>,
        'a: 'b,
    {
        access.get::<'b, 'b, T>(self)
    }

    /// Convenience method for `access.get_mut(self)`.
    pub fn get_mut<'b, Access: JSAccess<Cx>>(self, access: &'b mut Access) -> &'b mut T::ChangeLifetime where
        T: JSManageable<'b>,
        'a: 'b,
    {
        access.get_mut(self)
    }

    /// Change the lifetime of JS-managed data.
    pub unsafe fn change_lifetime<'b>(self) -> JSManaged<'b, Cx, T::ChangeLifetime> where
        T: JSManageable<'b>,
    {
        JSManaged {
            raw: self.raw as *mut T::ChangeLifetime,
            marker: PhantomData,
        }
    }

    /// It's safe to contract the lifetime of JS-managed data.
    pub fn contract_lifetime<'b>(self) -> JSManaged<'b, Cx, T::ChangeLifetime> where
        T: JSManageable<'b>,
        'a: 'b,
    {
        unsafe { self.change_lifetime() }
    }

    /// It's safe to extend the lifetime of JS-managed data if it has been snapshotted.
    pub fn extend_lifetime<'b>(self, _: &JSSnapshot<'b, Cx>) -> JSManaged<'b, Cx, T::ChangeLifetime> where
        T: JSManageable<'b>,
        'b: 'a,
    {
        unsafe { self.change_lifetime() }
    }
}

/// A root set.
pub struct JSRoots<Cx> {
    // The real thing would contain a set of rooted JS objects.
    marker: PhantomData<Cx>,
}

impl<Cx> JSRoots<Cx> {
    pub fn new() -> JSRoots<Cx> {
        JSRoots {
            marker: PhantomData,
        }
    }
    pub fn root<'a, 'b, T>(&'a self, managed: JSManaged<'b, Cx, T>) -> JSManaged<'a, Cx, T::ChangeLifetime> where
        'a: 'b,
        T: JSManageable<'a>,
    {
        // The real thing would add the JS reflector to the root set
        unsafe { managed.change_lifetime() }
    }
}

impl<Cx> Drop for JSRoots<Cx> {
    fn drop(&mut self) {
        // The real thing would unroot the root set.
    }
}

/// A snapshot of a JS context.
///
/// The idea here is that during the lifetime of a JSSnapshot<Cx>, the JS state
/// doesn't change, and in particular GC doesn't happen. This allows us to avoid
/// some rooting.
pub struct JSSnapshot<'c, Cx: 'c>(&'c mut Cx);

impl<'c, Cx: 'c> JSSnapshot<'c, Cx> where
    Cx: JSContext
{
    /// Build a new snapshot
    pub fn new(cx: &mut Cx) -> JSSnapshot<Cx> {
        JSSnapshot(cx)
    }
}

impl<'c, Cx: 'c> JSAccess<Cx> for JSSnapshot<'c, Cx> where
    Cx: JSContext
{
    /// Read-only access to native JS-managed data.
    fn get<'a, 'b:'a, T>(&'a self, managed: JSManaged<'b, Cx, T>) -> &'a T::ChangeLifetime where
        T: JSManageable<'a>
    {
        self.0.get(managed)
    }
    
    /// Read-write access to native JS-managed data.
    fn get_mut<'a, 'b:'a, T>(&'a mut self, managed: JSManaged<'b, Cx, T>) -> &'a mut T::ChangeLifetime where
        T: JSManageable<'a>
    {
        self.0.get_mut(managed)
    }
}

// It is important for safety that this implemention is not made public!
struct JSContextImpl {
    // JS context implementation goes here
}

impl JSAccess<JSContextImpl> for JSContextImpl {
    // The claim is that this is safe, since it takes a `&self`.
    fn get<'a, 'b:'a, T>(&'a self, managed: JSManaged<'b, Self, T>) -> &'a T::ChangeLifetime where
        T: JSManageable<'a>
    {
        unsafe { &*(managed.raw as *mut T::ChangeLifetime) }
    }

    // The claim is that this is safe, since it takes a `&mut self`.
    fn get_mut<'a, 'b:'a, T>(&'a mut self, managed: JSManaged<'b, Self, T>) -> &'a mut T::ChangeLifetime where
        T: JSManageable<'a>
    {
        unsafe { &mut *(managed.raw as *mut T::ChangeLifetime) }
    }
}

impl JSContext for JSContextImpl {
    fn snapshot(&mut self) -> JSSnapshot<Self> {
        JSSnapshot(self)
    }

    // This outline implementation just space-leaks all data,
    // the real thing would create a reflector, and add a finalizer hook.
    fn manage<'a, T>(&'a self, value: T) -> JSManaged<'a, Self, T>
        where T: 'a + JSManageable<'a, ChangeLifetime=T>
    {
        JSManaged {
            raw: Box::into_raw(Box::new(value)),
            marker: PhantomData,
        }
    }
}

#[test]
// This test constructs a two-node cyclic graph, which is the smallest
// example of something that uses `RefCell`s in servo's JS bindings.
fn test() {
    // A graph type
    type Graph<'a, Cx> = JSManaged<'a, Cx, NativeGraph<'a, Cx>>;
    struct NativeGraph<'a, Cx> {
        nodes: Vec<Node<'a, Cx>>,
    }
    unsafe impl<'a, 'b, Cx: 'b> JSManageable<'b> for NativeGraph<'a, Cx> { type ChangeLifetime = NativeGraph<'b, Cx>; }
    unsafe impl<'a, Cx> JSTraceable<Cx> for NativeGraph<'a, Cx> {}
    // A node type
    type Node<'a, Cx> = JSManaged<'a, Cx, NativeNode<'a, Cx>>;
    struct NativeNode<'a, Cx> {
        data: usize,
        edges: Vec<Node<'a, Cx>>,
    }
    unsafe impl<'a, 'b, Cx: 'b> JSManageable<'b> for NativeNode<'a, Cx> { type ChangeLifetime = NativeNode<'b, Cx>; }
    unsafe impl<'a, Cx> JSTraceable<Cx> for NativeNode<'a, Cx> {}
    // Build a cyclic graph
    struct Test;
    impl JSContextConsumer<()> for Test {
        fn consume<Cx>(self, cx: &mut Cx) where Cx: JSContext {
            let roots = JSRoots::new();
            let graph = roots.root(cx.manage(NativeGraph { nodes: vec![] }));
            self.add_nodes(cx, graph);
            assert_eq!(graph.get(cx).nodes[0].get(cx).data, 1);
            assert_eq!(graph.get(cx).nodes[1].get(cx).data, 2);
            self.add_edges(&mut cx.snapshot(), graph);
            assert_eq!(graph.get(cx).nodes[0].get(cx).edges[0].get(cx).data, 2);
            assert_eq!(graph.get(cx).nodes[1].get(cx).edges[0].get(cx).data, 1);
        }
    }
    impl Test {
        fn add_nodes<'a, 'b, Cx: JSContext>(&'a self, cx: &'a mut Cx, graph: Graph<'b, Cx>) {
            // Creating nodes does memory allocation, which may trigger GC,
            // so the nodes need to be rooted while they are being added.
            let roots = JSRoots::new();
            let node1 = roots.root(cx.manage(NativeNode { data: 1, edges: vec![] }));
            let node2 = roots.root(cx.manage(NativeNode { data: 2, edges: vec![] }));
            graph.get_mut(cx).nodes.push(node1.contract_lifetime());
            graph.get_mut(cx).nodes.push(node2.contract_lifetime());
        }
        fn add_edges<Cx: JSContext>(&self, cx: &mut JSSnapshot<Cx>, graph: Graph<Cx>) {
            // Note that there's no rooting here.
            let node1 = graph.get(cx).nodes[0].extend_lifetime(cx);
            let node2 = graph.get(cx).nodes[1].extend_lifetime(cx);
            node1.get_mut(cx).edges.push(node2.contract_lifetime());
            node2.get_mut(cx).edges.push(node1.contract_lifetime());
        }
    }
    #[allow(dead_code)]
    // Test that we can contract the lifetimes of nodes and graphs.
    fn contract_graph<'a, 'b:'a, Cx: 'b>(graph: Graph<'b, Cx>) -> Graph<'a, Cx> {
        graph.contract_lifetime()
    }
    with_js_context(Test);
}

#[test]
fn test_covariant() {
    #[allow(dead_code)]
    fn cast<'a, 'b:'a, Cx, T>(managed: JSManaged<'b, Cx, T>)
                              -> JSManaged<'a, Cx, T>
    {
        managed
    }
}
