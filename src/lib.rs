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
use std::ops::Deref;

/// The trait for accessing JS-managed data.
pub trait JSAccess<Cx>: Sized {
    /// Read-only access to native JS-managed data.
    fn get<T>(&self, managed: &JSManaged<Cx, T>) -> &T;
    
    /// Read-write access to native JS-managed data.
    fn get_mut<T>(&mut self, managed: &JSManaged<Cx, T>) -> &mut T;
}

/// The trait for JS contexts.
pub trait JSContext: JSAccess<Self> {
    /// Get a snapshot of the JS state.
    /// The snapshot only allows access to the methods that are guaranteed not to call GC,
    /// so we don't need to root JS-managed pointers during the lifetime of a snapshot.
    fn snapshot(&mut self) -> JSSnapshot<Self>;

    /// Call a setter of JS-managed data.
    /// The reason for this function to exist is that it takes ownership of a JS-managed reference,
    /// since the object is passed in as a `&JSManaged`, but when the setter is called it is a `JSMansaged`.
    /// A typical use of it is something like:
    /// ```rust
    /// struct Foo<Cx> { bar: JSManaged<Cx, Bar> };
    /// impl<Cx> Foo<Cx> { fn set_bar(&mut self, bar: JSManaged<Cx, Bar>) { self.bar = bar; }
    /// fn example<Cx: JSContext>(cx: &mut Cx, foo: &JSManaged<Cx, Foo>, bar: &JSManaged<Cx, Bar>) {
    ///     cx.call_setter(foo, Foo::set_bar, bar);
    /// }
    /// ```
    /// (If Rust allowed write-only references, we could probably avoid this, but
    /// it doesn't, so we do a hack to stop the `JSManaged` value from escaping.
    fn call_setter<S, T>(&mut self, subject: &JSManaged<Self, S>, verb: fn(&mut S, JSManaged<Self, T>), object: &JSManaged<Self, T>) where
        S: JSTraceable<Self>;
    
    /// Give ownership of data to JS.
    /// This allocates JS heap, which may trigger GC.
    fn manage<T>(&self, value: T) -> &JSManaged<Self, T>;
    
    // A real implementation would also have JS methods such as those in jsapi.
}

/// A placholder for the real `JSTraceable`.
pub trait JSTraceable<Cx> {}

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
pub struct JSManaged<Cx, T> {
    // JS reflector goes here
    raw: *mut T,
    marker: PhantomData<Cx>,
}

impl<Cx, T> JSManaged<Cx, T> {
    // Under the hood, `JSManaged` data is cloneable,
    // but it is important for safety that this function
    // is only called when we have some reason to be confident
    // in its safety.
    unsafe fn clone(&self) -> Self {
        JSManaged {
            raw: self.raw,
            marker: self.marker,
        }
    }

    /// Convenience method for `access.get(self)`.
    pub fn get<'a, Access: JSAccess<Cx>>(&'a self, access: &'a Access) -> &'a T {
        access.get(self)
    }

    /// Convenience method for `access.get_mut(self)`.
    pub fn get_mut<'a, Access: JSAccess<Cx>>(&'a self, access: &'a mut Access) -> &'a mut T {
        access.get_mut(self)
    }

    /// Convenience method for `snapshot.extend_lifetime(self)`.
    pub fn extend_lifetime<'a>(&self, snapshot: &JSSnapshot<'a, Cx>) -> &'a JSManaged<Cx, T> where
        Cx: JSContext
    {
        snapshot.extend_lifetime(self)
    }
}

/// The type of rooted JS-managed data in a JS context `Cx`.
///
/// If a user needs to keep JS-managed data on the stack,
/// they can do it by rooting the data.
pub struct JSRooted<Cx, T>(JSManaged<Cx, T>);

impl<Cx, T> JSRooted<Cx, T> {
    // The real thing would add the JS reflector to the root multiset
    pub fn new(managed: &JSManaged<Cx, T>) -> JSRooted<Cx, T> {
        JSRooted(unsafe { managed.clone() })
    }
}

impl<Cx, T> Clone for JSRooted<Cx, T> {
    // The real thing would add the JS reflector to the root multiset
    fn clone(&self) -> JSRooted<Cx, T> {
        JSRooted(unsafe { self.0.clone() })
    }
}

impl<Cx, T> Drop for JSRooted<Cx, T> {
    // The real thing would remove the JS reflector from the root multiset
    fn drop(&mut self) {
    }
}

impl<Cx, T> Deref for JSRooted<Cx, T> {
    type Target = JSManaged<Cx, T>;
    fn deref(&self) -> &JSManaged<Cx, T> {
        &self.0
    }
}

/// A snapshot of a JS context.
///
/// The idea here is that during the lifetime of a JSSnapshot<Cx>, the JS state
/// doesn't change, and in particular GC doesn't happen. This allows us to avoid
/// some rooting.
pub struct JSSnapshot<'a, Cx: 'a>(&'a mut Cx);

impl<'a, Cx: 'a> JSSnapshot<'a, Cx> where
    Cx: JSContext
{
    /// Build a new snapshot
    pub fn new(cx: &mut Cx) -> JSSnapshot<Cx> {
        JSSnapshot(cx)
    }
            
    /// Extend the lifetime of JS-managed data.
    /// This is safe because no GC will happen during the lifetime of this snapshot.
    pub fn extend_lifetime<T>(&self, managed: &JSManaged<Cx, T>) -> &'a JSManaged<Cx, T> {
        let raw = managed as *const JSManaged<Cx, T>;
        unsafe { &*raw }
    }

    /// Call a setter of JS-managed data.
    pub fn call_setter<S, T>(&mut self, subject: &JSManaged<Cx, S>, verb: fn(&mut S, JSManaged<Cx, T>), object: &JSManaged<Cx, T>) where
        S: JSTraceable<Cx>
    {
        self.0.call_setter(subject, verb, object);
    }
}

impl<'a, Cx: 'a> JSAccess<Cx> for JSSnapshot<'a, Cx> where
    Cx: JSContext
{
    /// Read-only access to JS-managed data.
    fn get<T>(&self, managed: &JSManaged<Cx, T>) -> &T {
        self.0.get(managed)
    }
    
    /// Read-write access to JS-managed data.
    /// Note that this doesn't give access to the JS heap, just to the native heap,
    /// since the user only has access to a `JSSnaphot`, not a `JSContext`.
    fn get_mut<T>(&mut self, managed: &JSManaged<Cx, T>) -> &mut T {
        self.0.get_mut(managed)
    }
}

// It is important for safety that this implemention is not made public!
struct JSContextImpl {
    // JS context implementation goes here
}

impl JSAccess<JSContextImpl> for JSContextImpl {
    // The claim is that this is safe, since it takes a `&self`.
    fn get<T>(&self, managed: &JSManaged<Self, T>) -> &T {
        unsafe { &*managed.raw }
    }

    // The claim is that this is safe, since it takes a `&mut self`.
    fn get_mut<T>(&mut self, managed: &JSManaged<Self, T>) -> &mut T {
        unsafe { &mut *managed.raw }
    }
}

impl JSContext for JSContextImpl {
    fn call_setter<S, T>(&mut self, subject: &JSManaged<Self, S>, verb: fn(&mut S, JSManaged<Self, T>), object: &JSManaged<Self, T>) {
        verb(self.get_mut(subject), unsafe { object.clone() });
    }
    
    fn snapshot(&mut self) -> JSSnapshot<Self> {
        JSSnapshot(self)
    }

    // This outline implementation just space-leaks all data,
    // the real thing would create a reflector, and add a finalizer hook.
    fn manage<T>(&self, value: T) -> &JSManaged<Self, T> {
        let managed = JSManaged{
            raw: Box::into_raw(Box::new(value)),
            marker: PhantomData,
        };
        let raw = Box::into_raw(Box::new(managed));
        unsafe { &*raw }
    }
}

#[test]
// This test constructs a two-node cyclic graph, which is the smallest
// example of something that uses `RefCell`s in servo's JS bindings.
fn test() {
    // A graph type
    type Graph<Cx> = JSManaged<Cx, NativeGraph<Cx>>;
    struct NativeGraph<Cx> {
        nodes: Vec<Node<Cx>>,
    }
    impl<Cx> NativeGraph<Cx> {
        fn push_node(&mut self, node: Node<Cx>) {
            self.nodes.push(node);
        }
    }
    impl<Cx> JSTraceable<Cx> for NativeGraph<Cx> {}
    // A node type
    type Node<Cx> = JSManaged<Cx, NativeNode<Cx>>;
    struct NativeNode<Cx> {
        data: usize,
        edges: Vec<Node<Cx>>,
    }
    impl<Cx> NativeNode<Cx> {
        fn push_edge(&mut self, node: Node<Cx>) {
            self.edges.push(node);
        }
    }
    impl<Cx> JSTraceable<Cx> for NativeNode<Cx> {}
    // Build a cyclic graph
    struct Test;
    impl JSContextConsumer<()> for Test {
        fn consume<Cx>(self, cx: &mut Cx) where Cx: JSContext {
            let graph = JSRooted::new(cx.manage(NativeGraph { nodes: vec![] }));
            self.add_nodes(cx, &graph);
            assert_eq!(graph.get(cx).nodes[0].get(cx).data, 1);
            assert_eq!(graph.get(cx).nodes[1].get(cx).data, 2);
            self.add_edges(&mut cx.snapshot(), &graph);
            assert_eq!(graph.get(cx).nodes[0].get(cx).edges[0].get(cx).data, 2);
            assert_eq!(graph.get(cx).nodes[1].get(cx).edges[0].get(cx).data, 1);
        }
    }
    impl Test {
        fn add_nodes<Cx: JSContext>(&self, cx: &mut Cx, graph: &Graph<Cx>) {
            // Creating nodes does memory allocation, which may trigger GC,
            // so the nodes need to be rooted while they are being added.
            let node1 = JSRooted::new(cx.manage(NativeNode { data: 1, edges: vec![] }));
            let node2 = JSRooted::new(cx.manage(NativeNode { data: 2, edges: vec![] }));
            cx.call_setter(&graph, NativeGraph::push_node, &node1);
            cx.call_setter(&graph, NativeGraph::push_node, &node2);
        }
        fn add_edges<Cx: JSContext>(&self, cx: &mut JSSnapshot<Cx>, graph: &Graph<Cx>) {
            // Note that there's no rooting here.
            let node1 = graph.get(cx).nodes[0].extend_lifetime(cx);
            let node2 = graph.get(cx).nodes[1].extend_lifetime(cx);
            cx.call_setter(node1, NativeNode::push_edge, node2);
            cx.call_setter(node2, NativeNode::push_edge, node1);
        }
    }
    with_js_context(Test);
}
