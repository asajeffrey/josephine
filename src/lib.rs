//! An outline of how linear types could be combined with JS-managed data
//!
//! The goals are:
//! 1. Ensure that JS objects are only accessed in the right JS context.
//! 2. Remove the need for the rooting lint.
//! 3. Don't require rooting in code that can't perform GC.
//! 4. Allow `&mut T` access to JS-managed data, so we don't need as much interior mutability.
//!
//! The idea is that Rust data can be given to JS to manage, and then accessed.
//! ```rust
//!     let x: JSManaged<Cx, String> = cx.manage(String::from("hello"));
//!     let x_ref: &String = x.get(cx);
//! ```
//! We use polymorphism to track the type of the JS context 'cx: &Cx`
//! where there is an opaque type `Cx: JSContext`. Each JS context has
//! a different type. so objects from one JS context cannot
//! accidentally be used in another.
//!
//! To remove the rooting lint, we track the lifetime of JS-managed
//! data, with a type `JSManaged<'a, Cx, T>`. The lifetime parameter is
//! the lifetime of any reachable JS-managed data reachable.
//! For example, being more explicit about lifetimes, if `cx: &'a Cx`:
//! ```rust
//!     let x: JSManaged<'a, Cx, String> = cx.manage(String::from("hello"));
//!     let x_ref: &'a String = x.get(cx);
//! ```
//! JS-managed data can be explicity converted to a more constrained
//! lifetime, for example if `'b` is a sublifetime of `'a`:
//! ```rust
//!     let x: JSManaged<'a, Cx, String> = cx.manage(String::from("hello"));
//!     let y: JSManaged<'b, Cx, String> = x.contract_lifetime();
//! ```
//! Things get interesting when managed references are nested,
//! since the nested lifetimes also change:
//! ```rust
//!     type JSHandle<'a, Cx, T> = JSManaged<'a, Cx, JSManaged<'a, Cx, T>>;
//!     let x: JSHandle<'a, Cx, String> = cx.manage(cx.manage(String::from("hello")));
//!     let y: JSHandle<'b, Cx, String> = x.contact_lifetime();
//! ```
//! There is a `JSManageable` trait which drives these changes of lifetime.
//! If `T: JSManageable<'a>` then `T::ChangeLifetime` is the same type as `T`,
//! but with any reachable JS-managed data now with lifetime `'a`.
//! For example `JSHandle<'a, Cx, String>` implements `JSManageable<'b>`,
//! with `ChangeLifetime=JSHandle<'b, Cx, String>`.
//!
//! A full implementation would provide a `[#derive(JSManageable)]` annotation,
//! for user-defined types, but for the moment users have to implement this by hand.
//! For example:
//! ```rust
//!     type Node<'a, Cx> = JSManaged<'a, Cx, NativeNode<'a, Cx>>;
//!     struct NativeNode<'a, Cx> {
//!         data: usize,
//!         edges: Vec<Node<'a, Cx>>,
//!     }
//!     unsafe impl<'a, Cx> JSTraceable for NativeNode<'a, Cx> {}
//!     unsafe impl<'a, 'b, Cx: 'b> JSManageable<'b> for NativeNode<'a, Cx> { type ChangeLifetime = NativeNode<'b, Cx>; }
//! ```
//! To avoid rooting in code that can't perform GC, we allow a snapshot to
//! be taken of the JS context. Snapshots are limited as to what JS
//! functionality is supported, to ensure that no GC is performed
//! while a snapshot is live. The benefit of this is that the lifetimes
//! of JS-managed data can be extended to the lifetime of the snapshot.
//! For example, if `cx: &'c JSSnapshot<Cx>` and `'c` is a superlifetime of `'b`:
//! ```rust
//!     let y: JSManaged<'b, Cx, String> = x.contract_lifetime();
//!     let z: JSManaged<'c, Cx, String> = y.extend_lifetime(cx);
//! ```
//! The other way to safely extend the lifetime of JS-managed data
//! is to root it, in a root set.
//! For example, if `roots: &'c JSRoots<Cx>` and `'c` is a superlifetime of `'b`:
//! ```rust
//!     let y: JSManaged<'b, Cx, String> = x.contract_lifetime();
//!     let z: JSManaged<'c, Cx, String> = y.root(roots);
//! ```
//! We allow mutable JS contexts to gain mutable access to JS-managed data.
//! Since we require the JS context to be mutable, we can only safely
//! access one JS-managed value at a time. To do this safely, we either need to root
//! the data or use a snapshot. For example with rooting:
//! ```rust
//!     let roots: JSRoots<Cx> = cx.roots();
//!     let x: JSHandle<Cx, String> = cx.manage(cx.manage(String::from("hello"))).root(roots);
//!     let y: JSManaged<Cx, String> = cx.manage(String::from("world")).root(roots);
//!     cx.get_mut(x) = y;
//! ```

extern crate js;

use js::rust::Runtime;
use std::marker::PhantomData;

/// A marker trait for accessing JS-managed data.
pub unsafe trait JSAccess<Cx>: Sized {}

/// The trait for JS contexts.
pub trait JSContext: 'static + JSAccess<Self> {
    /// Get a snapshot of the JS state.
    /// The snapshot only allows access to the methods that are guaranteed not to call GC,
    /// so we don't need to root JS-managed pointers during the lifetime of a snapshot.
    fn snapshot(&mut self) -> JSSnapshot<Self>;

    /// Add a new root set to the context.
    fn roots(&mut self) -> JSRoots<Self>;

    /// Give ownership of data to JS.
    /// This allocates JS heap, which may trigger GC.
    fn manage<'a, T>(&'a self, value: T) -> JSManaged<'a, Self, T::ChangeLifetime>
        where T: 'static + JSManageable<'a>;
    
    /// The backing JS runtime.
    fn runtime(&self) -> &Runtime;
}

/// A placholder for the real `JSTraceable`.
pub unsafe trait JSTraceable {}

/// The trait for native JS-manageable data.
pub unsafe trait JSManageable<'a>: JSTraceable {
    /// This type should have the same mnemory represention as `Self`.
    /// The only difference between `Self` and `Self::ChangeLifetime`
    /// is that any `JSManaged<'b, Cx, T>` should be replaced by
    /// `JSManaged<'a, Cx, T::ChangeLifetime>`.
    type ChangeLifetime: 'a + JSManageable<'a, ChangeLifetime=Self::ChangeLifetime>;
}

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
    // TODO: proper error handling
    let mut cx = JSContextImpl {
        runtime: Runtime::new().expect("Failed to crate runtime."),
    };
    consumer.consume(&mut cx)
}

/// The type of JS-managed data in a JS context `Cx`, with lifetime `'a`.
///
/// If the user has access to a `JSManaged`, then the JS-managed
/// data is live for the given lifetime.
pub struct JSManaged<'a, Cx, T: ?Sized> {
    // JS reflector goes here
    raw: *mut T,
    marker: PhantomData<(&'a(),Cx)>,
}

impl<'a, Cx, T: ?Sized> Clone for JSManaged<'a, Cx, T> {
    fn clone(&self) -> Self {
        JSManaged {
            raw: self.raw,
            marker: self.marker,
        }
    }
}

impl<'a, Cx, T: ?Sized> Copy for JSManaged<'a, Cx, T> {
}

unsafe impl<'a, Cx, T: ?Sized> JSTraceable for JSManaged<'a, Cx, T> where
    T: JSTraceable
{
}

unsafe impl<'a, 'b, Cx, T: ?Sized> JSManageable<'b> for JSManaged<'a, Cx, T> where
    Cx: 'b,
    T: 'a + JSManageable<'b>,
{
    type ChangeLifetime = JSManaged<'b, Cx, T::ChangeLifetime>;
}

impl<'a, Cx, T: ?Sized> JSManaged<'a, Cx, T> {
    /// Read-only access to JS-managed data.
    pub fn get<'b, Access: JSAccess<Cx>>(self, _: &'b Access) -> &'b T::ChangeLifetime where
        T: JSManageable<'b>,
        'a: 'b,
    {
        unsafe { &*self.contract_lifetime().raw }
    }

    /// Read-write access to JS-managed data.
    pub fn get_mut<'b, Access: JSAccess<Cx>>(self, _: &'b mut Access) -> &'b mut T::ChangeLifetime where
        T: JSManageable<'b>,
        'a: 'b,
    {
        unsafe { &mut *self.contract_lifetime().raw }
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

    /// It's safe to extend the lifetime of JS-managed data by rooting it.
    pub fn root<'b>(self, _: &'b JSRoots<Cx>) -> JSManaged<'b, Cx, T::ChangeLifetime> where
        T: JSManageable<'b>,
        'b: 'a,
    {
        // The real thing would add the reflector to the root set.
        unsafe { self.change_lifetime() }
    }
}

/// A root set.
pub struct JSRoots<Cx> {
    // The real thing would contain a set of rooted JS objects.
    marker: PhantomData<Cx>,
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

unsafe impl<'c, Cx> JSAccess<Cx> for JSSnapshot<'c, Cx> where
    Cx: 'c + JSContext
{
}

// It is important for safety that this implemention is not made public!
struct JSContextImpl {
    // The underlying rust-mozjs runtime
    runtime: Runtime,
}

unsafe impl JSAccess<JSContextImpl> for JSContextImpl {
}

impl JSContext for JSContextImpl {
    fn snapshot(&mut self) -> JSSnapshot<Self> {
        JSSnapshot(self)
    }

    fn roots(&mut self) -> JSRoots<Self> {
        JSRoots { marker: PhantomData }
    }

    // This outline implementation just space-leaks all data,
    // the real thing would create a reflector, and add a finalizer hook.
    fn manage<'a, T>(&'a self, value: T) -> JSManaged<'a, Self, T::ChangeLifetime>
        where T: 'static + JSManageable<'a>
    {
        JSManaged {
            raw: Box::into_raw(Box::new(value)) as *mut T::ChangeLifetime,
            marker: PhantomData,
        }
    }

    fn runtime(&self) -> &Runtime {
        &self.runtime
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
    unsafe impl<'a, Cx> JSTraceable for NativeGraph<'a, Cx> {}
    unsafe impl<'a, 'b, Cx: 'b> JSManageable<'b> for NativeGraph<'a, Cx> { type ChangeLifetime = NativeGraph<'b, Cx>; }
    // A node type
    type Node<'a, Cx> = JSManaged<'a, Cx, NativeNode<'a, Cx>>;
    struct NativeNode<'a, Cx> {
        data: usize,
        edges: Vec<Node<'a, Cx>>,
    }
    unsafe impl<'a, Cx> JSTraceable for NativeNode<'a, Cx> {}
    unsafe impl<'a, 'b, Cx: 'b> JSManageable<'b> for NativeNode<'a, Cx> { type ChangeLifetime = NativeNode<'b, Cx>; }
    // Build a cyclic graph
    struct Test;
    impl JSContextConsumer<()> for Test {
        fn consume<Cx>(self, cx: &mut Cx) where Cx: JSContext {
            let roots = cx.roots();
            let graph = cx.manage(NativeGraph { nodes: vec![] }).root(&roots);
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
            let roots = cx.roots();
            let node1 = cx.manage(NativeNode { data: 1, edges: vec![] }).root(&roots);
            let node2 = cx.manage(NativeNode { data: 2, edges: vec![] }).root(&roots);
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
