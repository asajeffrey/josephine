//! An outline of how linear types could be combined with JS-managed data
//!
//! The goals are:
//! 
//! 1. Ensure that JS objects are only accessed in the right JS compartment.
//! 2. Remove the need for the rooting lint.
//! 3. Don't require rooting in code that can't perform GC.
//! 4. Allow `&mut T` access to JS-managed data, so we don't need as much interior mutability.
//!
//! The idea is that Rust data can be given to JS to manage, and then accessed.
//! 
//! ```ignore
//! let x: JSManaged<C, String> = cx.manage(String::from("hello"));
//! ...
//! let x_ref: &String = x.get(cx);
//! ```
//! We use polymorphism to track the type of the JS context `cx: &mut JSContext<C>`
//! where there is an opaque type `C`. Each JS compartment has
//! a different type, so objects from one JS compartment cannot
//! accidentally be used in another.
//!
//! Unfortunately, even this simple example is not safe, due to garbage collection.
//! If GC happened during the `...`, there is nothing keeping `x` alive, so
//! the access might be to GC'd memory. To avoid this, we introduce root sets,
//! that keep memory alive. For example:
//! 
//! ```ignore
//! let roots: JSRoots<C> = cx.roots();
//! let x: JSManaged<C, String> = cx.manage(String::from("hello")).root(&roots);
//! ...
//! let x_ref: &String = x.get(cx);
//! ```
//! This example is now safe, since `x` is rooted during its access. To see why
//! the modified example type-checks, but the original does not, we introduce
//! explicit lifetimes (where `&roots` has type `&'a JSRoots<C>`):
//! 
//! ```ignore
//! let x: JSManaged<'a, C, String> = cx.manage(String::from("hello")).root(&roots);
//! ...
//! let x_ref: &'a String = x.get(cx);
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
//! 
//! ```ignore
//! let x: JSManaged<'a, C, String> = cx.manage(String::from("hello")).root(&roots);
//! let y: JSManaged<'b, C, String> = x.contract_lifetime();
//! ```
//! Things get interesting when managed references are nested,
//! since the nested lifetimes also change:
//! 
//! ```ignore
//! type JSHandle<'a, C, T> = JSManaged<'a, C, JSManaged<'a, C, T>>;
//! let x: JSManaged<'a, C, String> = cx.manage(String::from("hello")).root(&roots);
//! let y: JSHandle<'a, C, String> = cx.manage(x).root(&roots);
//! let z: JSHandle<'b, C, String> = x.contact_lifetime();
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
//! 
//! ```ignore
//! type Node<'a, C> = JSManaged<'a, C, NativeNode<'a, C>>;
//! struct NativeNode<'a, C> {
//!     data: usize,
//!     edges: Vec<Node<'a, C>>,
//! }
//! unsafe impl<'a, 'b, C> JSManageable<'b> for NativeNode<'a, C> {
//!     type Aged = NativeNode<'b, C>;
//! }
//! ```
//! To avoid rooting in code that can't perform GC, we allow a snapshot to
//! be taken of the JS context. Snapshots are limited as to what JS
//! functionality is supported, to ensure that no GC is performed
//! while a snapshot is live. The benefit of this is that the lifetimes
//! of JS-managed data can be extended to the lifetime of the snapshot.
//! For example, if `cx: &'c JSSnapshot<C>` and `'c` is a superlifetime of `'b`:
//! 
//! ```ignore
//! let y: JSManaged<'b, C, String> = x.contract_lifetime();
//! let z: JSManaged<'c, C, String> = y.extend_lifetime(cx);
//! ```
//! We allow mutable JS contexts to gain mutable access to JS-managed data.
//! Since we require the JS context to be mutable, we can only safely
//! access one JS-managed value at a time. To do this safely, we either need to root
//! the data or use a snapshot. For example with rooting:
//! 
//! ```ignore
//! let roots: JSRoots<C> = cx.roots();
//! let x: JSManaged<'a, C, String> = cx.manage(String::from("hello")).root(&roots);
//! let y: JSHandle<'a, C, String> = cx.manage(x).root(&roots);
//! let z: JSManaged<'a, C, String> = cx.manage(String::from("world")).root(&roots);
//! y.get_mut(cx) = z.contract_lifetime();
//! ```
//! A common case is to create JS-managed data, and to add it to an existing
//! JS-managed object. Since no GC can be performed between the creation and
//! the assignment, this is safe. To support this, there is a `cx.snapshot_manage(data)`
//! method, which JS-manages the data, then takes a snapshot immediately afterwards,
//! For example:
//! 
//! ```ignore
//! let ref roots = cx.roots();
//! let x = cx.manage(String::from("hello")).root(roots);
//! let y = cx.manage(x).root(roots);
//! let (ref mut cx, z) = cx.snapshot_manage(String::from("world"));
//! y.get_mut(cx) = z;
//! ```
//! Note that `z` does not need to be rooted, since the snapshot is taken just after
//! `z` is allocated
//!
//! #Examples
//!
//! This is an example of building a two-node cyclic graph, which is the smallest
//! example that Rust would need `Rc` and `RefCell` for. Note that this builds
//! the graph with no need for rooting.
//!
//! ```
//! extern crate linjs;
//! #[macro_use] extern crate linjs_derive;
//! use linjs::{CanAlloc, CanAccess, CanExtend, CanInitialize};
//! use linjs::{JSContext, JSManageable, JSManaged, JSRunnable};
//!
//! // A graph type
//! type Graph<'a, C> = JSManaged<'a, C, NativeGraph<'a, C>>;
//! #[derive(JSManageable)]
//! struct NativeGraph<'a, C> {
//!     nodes: Vec<Node<'a, C>>,
//! }
//!
//! // A node type
//! type Node<'a, C> = JSManaged<'a, C, NativeNode<'a, C>>;
//! #[derive(JSManageable)]
//! struct NativeNode<'a, C> {
//!     data: usize,
//!     edges: Vec<Node<'a, C>>,
//! }
//!
//! // Build a cyclic graph
//! struct Example;
//! impl JSRunnable for Example {
//!     fn run<C, S>(self, cx: JSContext<S>) where
//!         S: CanInitialize<C>
//!     {
//!         let ref mut cx = cx.init(NativeGraph { nodes: vec![] });
//!         let graph = cx.global();
//!         self.add_node1(cx, graph);
//!         self.add_node2(cx, graph);
//!         assert_eq!(graph.get(cx).nodes[0].get(cx).data, 1);
//!         assert_eq!(graph.get(cx).nodes[1].get(cx).data, 2);
//!         let ref mut cx = cx.snapshot();
//!         self.add_edges(cx, graph);
//!         assert_eq!(graph.get(cx).nodes[0].get(cx).edges[0].get(cx).data, 2);
//!         assert_eq!(graph.get(cx).nodes[1].get(cx).edges[0].get(cx).data, 1);
//!     }
//! }
//!
//! impl Example {
//!     fn add_node1<S, C>(&self, cx: &mut JSContext<S>, graph: Graph<C>) where
//!         S: CanAccess<C> + CanAlloc<C>
//!     {
//!         // Creating nodes does memory allocation, which may trigger GC,
//!         // so we need to be careful about lifetimes while they are being added.
//!         // Approach 1 is to root the node.
//!         let ref roots = cx.roots();
//!         let node1 = cx.manage(NativeNode { data: 1, edges: vec![] }).root(roots);
//!         graph.get_mut(cx).nodes.push(node1.contract_lifetime());
//!     }
//!     fn add_node2<S, C>(&self, cx: &mut JSContext<S>, graph: Graph<C>) where
//!         S: CanAccess<C> + CanAlloc<C>
//!      {
//!         // Approach 2 is to take a snapshot of the context right after allocation.
//!         let (ref mut cx, node2) = cx.snapshot_manage(NativeNode { data: 2, edges: vec![] });
//!         graph.get_mut(cx).nodes.push(node2.contract_lifetime());
//!     }
//!     fn add_edges<'a, S, C>(&self, cx: &mut JSContext<S>, graph: Graph<C>) where
//!         C: 'a,
//!         S: CanAccess<C> + CanExtend<'a, C>
//!      {
//!         // Note that there's no rooting here.
//!         let node1 = graph.get(cx).nodes[0].extend_lifetime(cx);
//!         let node2 = graph.get(cx).nodes[1].extend_lifetime(cx);
//!         node1.get_mut(cx).edges.push(node2.contract_lifetime());
//!         node2.get_mut(cx).edges.push(node1.contract_lifetime());
//!     }
//! }
//!
//! fn main() { Example.start(); }
//! ```

use std::marker::PhantomData;
use std::mem;

/// The type for JS contexts whose current state is `S`.
pub struct JSContext<S> {
    state: S,
}

/// A context state in an initialized compartment with global of type `G`.
pub struct Initialized<G> (G);

/// A context state in snapshotted compartment in underlying state `S`,
/// which guarantees that no GC will happen during the lifetime `'a`.
pub struct Snapshotted<'a, S: 'a> (&'a S);

/// A context state in uninitialized compartment `C`.
pub struct Uninitialized<C> (PhantomData<C>);

/// A context state in the middle of initializing a compartment with global of type `G`.
pub struct Initializing<G> (G);

/// A marker trait for JS contexts that can access native state
pub trait CanAccess<C> {}
impl<'a, C, T> CanAccess<C> for Initialized<JSManaged<'a, C, T>> {}
impl<'a, C, S> CanAccess<C> for Snapshotted<'a, S> where S: CanAccess<C> {}

/// A marker trait for JS contexts that can extend the lifetime of objects
pub trait CanExtend<'a, C> {}
impl<'a, C, S> CanExtend<'a, C> for Snapshotted<'a, S> where S: CanAccess<C> {}

/// A marker trait for JS contexts that can allocate objects
pub trait CanAlloc<C> {}
impl<'a, C, T> CanAlloc<C> for Initialized<JSManaged<'a, C, T>> {}
impl<'a, C, T> CanAlloc<C> for Initializing<JSManaged<'a, C, T>> {}

/// A marker trait for JS contexts that can be initialized
pub trait CanInitialize<C> {}
impl<C> CanInitialize<C> for Uninitialized<C> {}

/// A marker trait for JS contexts that are in the middle of initializing
pub trait IsInitializing<G>: HasGlobal<G> {}
impl<G: Clone> IsInitializing<G> for Initializing<G> {}

/// A trait for JS contexts that have a global
pub trait HasGlobal<G> {
    fn global(&self) -> G;
}
impl<G> HasGlobal<G> for Initialized<G> where
    G: Clone,
{
    fn global(&self) -> G {
        self.0.clone()
    }
}
impl<G> HasGlobal<G> for Initializing<G> where
    G: Clone,
{
    fn global(&self) -> G {
        self.0.clone()
    }
}
impl<'a, G, S> HasGlobal<G> for Snapshotted<'a, S> where
    S: HasGlobal<G>,
{
    fn global(&self) -> G {
        self.0.global()
    }
}

impl<S> JSContext<S> {
    /// Get a snapshot of the JS state.
    /// The snapshot only allows access to the methods that are guaranteed not to call GC,
    /// so we don't need to root JS-managed pointers during the lifetime of a snapshot.
    pub fn snapshot<'a>(&'a mut self) -> JSContext<Snapshotted<'a, S>> {
        JSContext {
           state: Snapshotted(&self.state)
        }
    }

    /// Add a new root set to the context.
    pub fn roots<C>(&mut self) -> JSRoots<C> where
        S: CanAlloc<C>,
    {
        JSRoots {
            marker: PhantomData,
        }
    }

    /// Give ownership of data to JS.
    /// This allocates JS heap, which may trigger GC.
    pub fn manage<'a, C, T>(&'a mut self, value: T) -> JSManaged<'a, C, T::Aged> where
        S: CanAlloc<C>,
        T: JSManageable<'a, C>
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
    pub fn snapshot_manage<'a, C, T>(&'a mut self, value: T) -> (JSContext<Snapshotted<'a, S>>, JSManaged<'a, C, T::Aged>) where
        S: CanAlloc<C>,
        T: JSManageable<'a, C>
    {
        // The real thing would use a JS reflector to manage the space,
        // this just space-leaks
        let managed = JSManaged {
            raw: Box::into_raw(Box::new(value)) as *mut T::Aged,
            marker: PhantomData,
        };
        let snapshot = JSContext {
            state: Snapshotted(&self.state),
        };
        (snapshot, managed)
    }

    /// Initialize a JS Context
    pub fn init<'a, C, T>(self, value: T) -> JSContext<Initialized<JSManaged<'a, C, T::Aged>>> where
        S: CanInitialize<C>,
        T: JSManageable<'a, C>,
    {
        self.pre_init().post_init(value)
    }

    /// Prepare a JS context for initialization
    pub fn pre_init<'a, C, T>(self) -> JSContext<Initializing<JSManaged<'a, C, T>>> where
        S: CanInitialize<C>,
    {
        // This is dangerous!
        // This is only safe because dereferencing this pointer is only done by user code
        // in posession of a context whose state is `CanAccess<C>`. The only way a user can
        // access such a context is by calling `post_init`, which initializes the raw pointer.
        // TODO: check that `Drop` and GC tracing are safe.
        // TODO: check the performance of the safer version of this code, which stores an `Option<T>` rather than a `T`.
        let raw = unsafe { Box::into_raw(Box::new(mem::uninitialized())) };
        let global = JSManaged {
            raw: raw,
            marker: PhantomData,
        };
        JSContext {
            state: Initializing(global),
        }
    }

    /// Finish initializing a JS Context
    pub fn post_init<'a, C, T>(self, value: T) -> JSContext<Initialized<JSManaged<'a, C, T::Aged>>> where
        S: IsInitializing<JSManaged<'a, C, T::Aged>>,
        T: JSManageable<'a, C>,
    {
        let global = self.state.global();
        let raw = global.raw as *mut T;
        unsafe { *raw = value; }
        JSContext {
            state: Initialized(global)
        }
    }

    /// Get the global of an initialized context.
    pub fn global<G>(&self) -> G where
        S: HasGlobal<G>,
        G: Clone,
    {
        self.state.global()
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
    fn run<C, S>(self, cx: JSContext<S>) where S: CanInitialize<C>;

    /// To trigger the callback, call `rt.start()`.
    fn start(self) {
        struct JSCompartmentImpl;
        let cx = JSContext {
            state: Uninitialized(PhantomData),
        };
        self.run::<JSCompartmentImpl, Uninitialized<JSCompartmentImpl>>(cx);
    }
}

/// The type of JS-managed data in a JS compartment `C`, with lifetime `'a`.
///
/// If the user has access to a `JSManaged`, then the JS-managed
/// data is live for the given lifetime.
pub struct JSManaged<'a, C, T: ?Sized> {
    // JS reflector goes here
    raw: *mut T,
    marker: PhantomData<(&'a(),C)>,
}

impl<'a, C, T: ?Sized> Clone for JSManaged<'a, C, T> {
    fn clone(&self) -> Self {
        JSManaged {
            raw: self.raw,
            marker: self.marker,
        }
    }
}

impl<'a, C, T: ?Sized> Copy for JSManaged<'a, C, T> {
}

unsafe impl<'a, 'b, C: 'b, T: ?Sized> JSManageable<'b, C> for JSManaged<'a, C, T> where
    T: JSManageable<'b, C>,
{
    type Aged = JSManaged<'b, C, T::Aged>;
}

impl<'a, C, T: ?Sized> JSManaged<'a, C, T> {
    /// Read-only access to JS-managed data.
    pub fn get<'b, S>(self, _: &'b JSContext<S>) -> &'b T::Aged where
        S: CanAccess<C>,
        T: JSManageable<'b, C>,
        'a: 'b,
    {
        unsafe { &*self.contract_lifetime().raw }
    }

    /// Read-write access to JS-managed data.
    pub fn get_mut<'b, S>(self, _: &'b mut JSContext<S>) -> &'b mut T::Aged where
        S: CanAccess<C>,
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
    pub fn extend_lifetime<'b, 'c, S>(self, _: &'c JSContext<S>) -> JSManaged<'b, C, T::Aged> where
        C: 'b,
        S: CanExtend<'b, C>,
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
