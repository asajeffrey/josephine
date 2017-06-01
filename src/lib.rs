//! An outline of how linear types could be combined with JS-managed data
//!
//! The goals are:
//! 1. Ensure that JS objects are only accessed in the right JS compartment.
//! 2. Remove the need for the rooting lint.
//! 3. Don't require rooting in code that can't perform GC.
//! 4. Allow `&mut T` access to JS-managed data, so we don't need as much interior mutability.
//!
//! The idea is that Rust data can be given to JS to manage, and then accessed.
//! ```rust
//!     let x: JSManaged<C, String> = cx.manage(String::from("hello"));
//!     ...
//!     let x_ref: &String = x.get(cx);
//! ```
//! We use polymorphism to track the type of the JS context 'cx: &mut JSContext<C>`
//! where there is an opaque type `C: JSCompartment`. Each JS compartment has
//! a different type. so objects from one JS compartment cannot
//! accidentally be used in another.
//!
//! Unfortunately, even this simple example is not safe, due to garbage collection.
//! If GC happened during the `...`, there is nothing keeping `x` alive, so
//! the access might be to GC'd memory. To avoid this, we introduce root sets,
//! that keep memory alive. For example:
//! ```rust
//!     let roots: JSRoots<C> = cx.roots();
//!     let x: JSManaged<C, String> = cx.manage(String::from("hello")).root(&roots);
//!     ...
//!     let x_ref: &String = x.get(cx);
//! ```
//! This example is now safe, since `x` is rooted during its access. To see why
//! the modified example type-checks, but the original does not, we introduce
//! explicit lifetimes (where `&roots` has type `&'a JSRoots<C>`):
//! ```rust
//!     let x: JSManaged<'a, C, String> = cx.manage(String::from("hello")).root(&roots);
//!     ...
//!     let x_ref: &'a String = x.get(cx);
//! ```
//! Without the rooting, the lifetimes do not match, since the call to `cx.manage`
//! requires a mutable borrow of `cx`, which will have lifetime `'b`, so
//! `x` has type `JSManaged<'b, C, String>`. The call to `x.get(cx)` requires
//! an immutable borrow of `cx`, which will have lifetime `'c` which does not overlap
//! with `'b`, and so `x.get(cx)` does not pass the borrow-checker.
//! This use of lifetimes allows safe access to JS-managed data without a special
//! rooting lint.
//!
//! JS-managed data can be explicity converted to a more constrained
//! lifetime, for example if `'b` is a sublifetime of `'a`:
//! ```rust
//!     let x: JSManaged<'a, C, String> = cx.manage(String::from("hello")).root(&roots);
//!     let y: JSManaged<'b, C, String> = x.contract_lifetime();
//! ```
//! Things get interesting when managed references are nested,
//! since the nested lifetimes also change:
//! ```rust
//!     type JSHandle<'a, C, T> = JSManaged<'a, C, JSManaged<'a, C, T>>;
//!     let x: JSManaged<'a, C, String> = cx.manage(String::from("hello")).root(&roots);
//!     let y: JSHandle<'a, C, String> = cx.manage(x).root(&roots);
//!     let z: JSHandle<'b, C, String> = x.contact_lifetime();
//! ```
//! There is a `JSManageable` trait which drives these changes of lifetime.
//! If `T: JSManageable<'a>` then `T::Aged` is the same type as `T`,
//! but with any reachable JS-managed data now with lifetime `'a`.
//! For example `JSHandle<'a, Cx, String>` implements `JSManageable<'b>`,
//! with `Aged=JSHandle<'b, Cx, String>`.
//!
//! A full implementation would provide a `[#derive(JSManageable)]` annotation,
//! for user-defined types, but for the moment users have to implement this by hand.
//! For example:
//! ```rust
//!     type Node<'a, C> = JSManaged<'a, C, NativeNode<'a, C>>;
//!     struct NativeNode<'a, C: JSCompartment> {
//!         data: usize,
//!         edges: Vec<Node<'a, C>>,
//!     }
//!     unsafe impl<'a, 'b, C: JSCompartment> JSManageable<'b> for NativeNode<'a, C> {
//!         type Aged = NativeNode<'b, C>;
//!     }
//! ```
//! To avoid rooting in code that can't perform GC, we allow a snapshot to
//! be taken of the JS context. Snapshots are limited as to what JS
//! functionality is supported, to ensure that no GC is performed
//! while a snapshot is live. The benefit of this is that the lifetimes
//! of JS-managed data can be extended to the lifetime of the snapshot.
//! For example, if `cx: &'c JSSnapshot<C>` and `'c` is a superlifetime of `'b`:
//! ```rust
//!     let y: JSManaged<'b, C, String> = x.contract_lifetime();
//!     let z: JSManaged<'c, C, String> = y.extend_lifetime(cx);
//! ```
//! We allow mutable JS contexts to gain mutable access to JS-managed data.
//! Since we require the JS context to be mutable, we can only safely
//! access one JS-managed value at a time. To do this safely, we either need to root
//! the data or use a snapshot. For example with rooting:
//! ```rust
//!     let roots: JSRoots<C> = cx.roots();
//!     let x: JSManaged<'a, C, String> = cx.manage(String::from("hello")).root(&roots);
//!     let y: JSHandle<'a, C, String> = cx.manage(x).root(&roots);
//!     let z: JSManaged<'a, C, String> = cx.manage(String::from("world")).root(&roots);
//!     y.get_mut(cx) = z.contract_lifetime();
//! ```
//! A common case is to create JS-managed data, and to add it to an existing
//! JS-managed object. Since no GC can be performed between the creation and
//! the assignment, this is safe. To support this, there is a `cx.snapshot_manage(data)`
//! method, which JS-manages the data, then takes a snapshot immediately afterwards,
//! For example:
//! ```rust
//!     let ref roots = cx.roots();
//!     let x = cx.manage(String::from("hello")).root(roots);
//!     let y = cx.manage(x).root(roots);
//!     let (ref mut cx, z) = cx.snapshot_manage(String::from("world"));
//!     y.get_mut(cx) = z;
//! ```
//! Note that `z` does not need to be rooted, since the snapshot is taken just after
//! `z` is allocated

use std::marker::PhantomData;

/// A marker trait for JS compartments.
pub unsafe trait JSCompartment: 'static {}

/// A marker trait for accessing JS-managed data in compartment `C`.
pub unsafe trait JSAccessToken<C: JSCompartment>: Sized {}

/// The type for JS contexts whose current compartment is `C`.
pub struct JSContext<C: JSCompartment> {
    marker: PhantomData<C>,
}

unsafe impl<C> JSAccessToken<C> for JSContext<C> where
    C: JSCompartment,
{
}

impl<C> JSContext<C> where
    C: JSCompartment
{
    /// Get a snapshot of the JS state.
    /// The snapshot only allows access to the methods that are guaranteed not to call GC,
    /// so we don't need to root JS-managed pointers during the lifetime of a snapshot.
    pub fn snapshot<'a>(&'a mut self) -> JSSnapshot<'a, C> {
        JSSnapshot {
            marker: PhantomData,
        }        
    }

    /// Add a new root set to the context.
    pub fn roots(&mut self) -> JSRoots<C> {
        JSRoots {
            marker: PhantomData,
        }
    }

    /// Give ownership of data to JS.
    /// This allocates JS heap, which may trigger GC.
    pub fn manage<'a, T>(&'a mut self, value: T) -> JSManaged<'a, C, T::Aged>
        where T: 'static + JSManageable<'a, C>
    {
        // The real thing would use a JS reflector to manage the space,
        // this just space-leaks
        JSManaged {
            raw: Box::into_raw(Box::new(value)) as *mut T::Aged,
            marker: PhantomData,
        }
    }

    /// Give ownership of data to JS.
    /// This allocates JS heap, which may trigger GC.
    pub fn snapshot_manage<'a, T>(&'a mut self, value: T) -> (JSSnapshot<'a, C>, JSManaged<'a, C, T::Aged>)
        where T: 'static + JSManageable<'a, C>
    {
        // The real thing would use a JS reflector to manage the space,
        // this just space-leaks
        let managed = JSManaged {
            raw: Box::into_raw(Box::new(value)) as *mut T::Aged,
            marker: PhantomData,
        };
        let snapshot = JSSnapshot {
            marker: PhantomData,
        };
        (snapshot, managed)
    }

    // A real implementation would also have JS methods such as those in jsapi.
}

/// Change the JS-managed lifetime of a type.
/// The real thing would include a JS tracer.
pub unsafe trait JSManageable<'a, C> {
    /// This type should have the same memory represention as `Self`.
    /// The only difference between `Self` and `Self::Aged`
    /// is that any `JSManaged<'b, C, T>` should be replaced by
    /// `JSManaged<'a, C, T::Aged>`.
    type Aged: 'a + JSManageable<'a, C, Aged=Self::Aged>;
}

unsafe impl<'a, C> JSManageable<'a, C> for String { type Aged = String; }
unsafe impl<'a, C> JSManageable<'a, C> for usize { type Aged = usize; }
unsafe impl<'a, C, T> JSManageable<'a, C> for Vec<T> where T: JSManageable<'a, C> { type Aged = Vec<T::Aged>; }
// etc.

/// A user of a JS runtime implements `JSRunnable`.
pub trait JSRunnable: Sized {
    /// This callback is called with a fresh JS compartment type `C`.
    fn run<C: JSCompartment>(self, rt: &mut JSRuntime<C>);

    /// To trigger the callback, call `rt.start()`.
    fn start(self) {
        struct JSCompartmentImpl;
        unsafe impl JSCompartment for JSCompartmentImpl {}
        let mut rt = JSRuntime {
            cx: JSContext {
                marker: PhantomData,
            }
        };
        self.run::<JSCompartmentImpl>(&mut rt);
    }
}

pub struct JSRuntime<C: JSCompartment> {
    // The real thing would have a JS runtime
    cx: JSContext<C>,
}

impl<C> JSRuntime<C> where
    C: JSCompartment
{
    pub fn manage<'a, T>(&'a mut self, value: T) -> (&'a mut JSContext<C>, JSManaged<'a, C, T::Aged>)
        where T: 'static + JSManageable<'a, C>
    {
        // The real thing would set the global of `cx`.
        let global = JSManaged {
            raw: Box::into_raw(Box::new(value)) as *mut T::Aged,
            marker: PhantomData,            
        };
        (&mut self.cx, global)
    }
}

/// The type of JS-managed data in a JS compartment `C`, with lifetime `'a`.
///
/// If the user has access to a `JSManaged`, then the JS-managed
/// data is live for the given lifetime.
pub struct JSManaged<'a, C: JSCompartment, T: ?Sized> {
    // JS reflector goes here
    raw: *mut T,
    marker: PhantomData<(&'a(),C)>,
}

impl<'a, C, T: ?Sized> Clone for JSManaged<'a, C, T> where
    C: JSCompartment
{
    fn clone(&self) -> Self {
        JSManaged {
            raw: self.raw,
            marker: self.marker,
        }
    }
}

impl<'a, C, T: ?Sized> Copy for JSManaged<'a, C, T> where
    C: JSCompartment,
{
}

unsafe impl<'a, 'b, C, T: ?Sized> JSManageable<'b, C> for JSManaged<'a, C, T> where
    C: JSCompartment,
    T: JSManageable<'b, C>,
{
    type Aged = JSManaged<'b, C, T::Aged>;
}

impl<'a, C, T: ?Sized> JSManaged<'a, C, T> where
    C: JSCompartment
{
    /// Read-only access to JS-managed data.
    pub fn get<'b, A: JSAccessToken<C>>(self, _: &'b A) -> &'b T::Aged where
        T: JSManageable<'b, C>,
        'a: 'b,
    {
        unsafe { &*self.contract_lifetime().raw }
    }

    /// Read-write access to JS-managed data.
    pub fn get_mut<'b, A: JSAccessToken<C>>(self, _: &'b mut A) -> &'b mut T::Aged where
        T: JSManageable<'b, C>,
        'a: 'b,
    {
        unsafe { &mut *self.contract_lifetime().raw }
    }

    /// Change the lifetime of JS-managed data.
    pub unsafe fn change_lifetime<'b>(self) -> JSManaged<'b, C, T::Aged> where
        T: JSManageable<'b, C>,
    {
        JSManaged {
            raw: self.raw as *mut T::Aged,
            marker: PhantomData,
        }
    }

    /// It's safe to contract the lifetime of JS-managed data.
    pub fn contract_lifetime<'b>(self) -> JSManaged<'b, C, T::Aged> where
        T: JSManageable<'b, C>,
        'a: 'b,
    {
        unsafe { self.change_lifetime() }
    }

    /// It's safe to extend the lifetime of JS-managed data if it has been snapshotted.
    pub fn extend_lifetime<'b>(self, _: &JSSnapshot<'b, C>) -> JSManaged<'b, C, T::Aged> where
        T: JSManageable<'b, C>,
        'b: 'a,
    {
        unsafe { self.change_lifetime() }
    }

    /// It's safe to extend the lifetime of JS-managed data by rooting it.
    pub fn root<'b>(self, _: &'b JSRoots<C>) -> JSManaged<'b, C, T::Aged> where
        T: JSManageable<'b, C>,
        'b: 'a,
    {
        // The real thing would add the reflector to the root set.
        unsafe { self.change_lifetime() }
    }
}

/// A root set.
pub struct JSRoots<C> {
    // The real thing would contain a set of rooted JS objects.
    marker: PhantomData<C>,
}

impl<C> Drop for JSRoots<C> {
    fn drop(&mut self) {
        // The real thing would unroot the root set.
    }
}

/// A snapshot of a JS context.
///
/// The idea here is that during the lifetime of a JSSnapshot<C>, the JS state
/// doesn't change, and in particular GC doesn't happen. This allows us to avoid
/// some rooting.
pub struct JSSnapshot<'a, C: JSCompartment> {
    // The real thing would have a JS context.
    marker: PhantomData<&'a C>,
}

unsafe impl<'a, C> JSAccessToken<C> for JSSnapshot<'a, C> where
    C: JSCompartment
{
}

#[test]
// This test constructs a two-node cyclic graph, which is the smallest
// example of something that uses `RefCell`s in servo's JS bindings.
fn test() {
    // A graph type
    type Graph<'a, C> = JSManaged<'a, C, NativeGraph<'a, C>>;
    struct NativeGraph<'a, C: JSCompartment> {
        nodes: Vec<Node<'a, C>>,
    }
    unsafe impl<'a, 'b, C: JSCompartment> JSManageable<'b, C> for NativeGraph<'a, C> {
        type Aged = NativeGraph<'b, C>;
    }
    // A node type
    type Node<'a, C> = JSManaged<'a, C, NativeNode<'a, C>>;
    struct NativeNode<'a, C: JSCompartment> {
        data: usize,
        edges: Vec<Node<'a, C>>,
    }
    unsafe impl<'a, 'b, C: JSCompartment> JSManageable<'b, C> for NativeNode<'a, C> {
        type Aged = NativeNode<'b, C>;
    }
    // Build a cyclic graph
    struct Test;
    impl JSRunnable for Test {
        fn run<C: JSCompartment>(self, rt: &mut JSRuntime<C>) {
            let (cx, graph) = rt.manage(NativeGraph { nodes: vec![] });
            self.add_node1(cx, graph);
            self.add_node2(cx, graph);
            assert_eq!(graph.get(cx).nodes[0].get(cx).data, 1);
            assert_eq!(graph.get(cx).nodes[1].get(cx).data, 2);
            let ref mut cx = cx.snapshot();
            self.add_edges(cx, graph);
            assert_eq!(graph.get(cx).nodes[0].get(cx).edges[0].get(cx).data, 2);
            assert_eq!(graph.get(cx).nodes[1].get(cx).edges[0].get(cx).data, 1);
        }
    }
    impl Test {
        fn add_node1<C: JSCompartment>(&self, cx: &mut JSContext<C>, graph: Graph<C>) {
            // Creating nodes does memory allocation, which may trigger GC,
            // so we need to be careful about lifetimes while they are being added.
            // Approach 1 is to root the node.
            let ref roots = cx.roots();
            let node1 = cx.manage(NativeNode { data: 1, edges: vec![] }).root(roots);
            graph.get_mut(cx).nodes.push(node1.contract_lifetime());
        }
        fn add_node2<C: JSCompartment>(&self, cx: &mut JSContext<C>, graph: Graph<C>) {
            // Approach 2 is to take a snapshot of the context right after allocation.
            let (ref mut cx, node2) = cx.snapshot_manage(NativeNode { data: 2, edges: vec![] });
            graph.get_mut(cx).nodes.push(node2.contract_lifetime());
        }
        fn add_edges<C: JSCompartment>(&self, cx: &mut JSSnapshot<C>, graph: Graph<C>) {
            // Note that there's no rooting here.
            let node1 = graph.get(cx).nodes[0].extend_lifetime(cx);
            let node2 = graph.get(cx).nodes[1].extend_lifetime(cx);
            node1.get_mut(cx).edges.push(node2.contract_lifetime());
            node2.get_mut(cx).edges.push(node1.contract_lifetime());
        }
    }
    #[allow(dead_code)]
    // Test that we can contract the lifetimes of nodes and graphs.
    fn contract_graph<'a, 'b:'a, C: JSCompartment>(graph: Graph<'b, C>) -> Graph<'a, C> {
        graph.contract_lifetime()
    }
    Test.start();
}

#[test]
fn test_covariant() {
    #[allow(dead_code)]
    fn cast<'a, 'b:'a, C: JSCompartment, T>(managed: JSManaged<'b, C, T>)
                                            -> JSManaged<'a, C, T>
    {
        managed
    }
}
