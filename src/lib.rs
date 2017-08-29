//! An outline of how linear types could be combined with JS-managed data
//!
//! The goals are:
//! 
//! 1. Ensure that JS objects are only accessed in the right JS compartment.
//! 2. Support stack-allocated roots.
//! 3. Remove the need for the rooting lint.
//! 4. Don't require rooting in code that can't perform GC.
//! 5. Allow `&mut T` access to JS-managed data, so we don't need as much interior mutability.
//!
//! The idea is that Rust data can be given to JS to manage, and then accessed,
//! using the JS context. This is passed as a variable of type `JSContext<S>`,
//! where the type parameter `S` is used to track the state of the context.
//!
//! For example, we can give JS some Rust data to manage in compartment
//! `C` when the context state implements the `CanAlloc<C>` trait:
//!
//! ```rust
//! # use linjs::*;
//! fn example<C, S>(cx: &mut JSContext<S>) where
//!     S: CanAlloc<C>,
//! {
//!     let x: JSManaged<C, String> = cx.manage(String::from("hello"));
//! }
//! ```
//!
//! JS-managed data in compartment `C` can be accessed if the context state
//! implements the `CanAccess<C>` trait:
//!
//! ```rust
//! # use linjs::*;
//! fn example<C, S>(cx: &mut JSContext<S>, x: JSManaged<C, String>) where
//!     S: CanAccess<C>,
//! {
//!     println!("{} world", x.get(cx));
//! }
//! ```
//!
//! Unfortunately, combining these two examples is not memory-safe, due to
//! garbage collection:
//!
//! ```rust,ignore
//! # use linjs::*;
//! fn unsafe_example<C, S>(cx: &mut JSContext<S>) where
//!     S: CanAlloc<C> + CanAccess<C>,
//! {
//!     let x: JSManaged<C, String> = cx.manage(String::from("hello"));
//!     // Imagine something triggers GC here
//!     println!("{} world", x.get(cx));
//! }
//! ```
//!
//! This example is not safe, as there is nothing keeping `x` alive in JS,
//! so if garbage collection is triggered, then `x` will be reclaimed
//! which will drop the Rust data, and so the call to `x.get(cx)` will be a use-after-free.
//!
//! This example is not memory-safe, and fortunately fails to typecheck:
//!
//! ```text
//! 	error[E0502]: cannot borrow `*cx` as immutable because it is also borrowed as mutable
//!  --> <anon>:7:32
//!   |
//! 5 |     let x: JSManaged<C, String> = cx.manage(String::from("hello"));
//!   |                                   -- mutable borrow occurs here
//! 6 |     // Imagine something triggers GC here
//! 7 |     println!("{} world", x.get(cx));
//!   |                                ^^ immutable borrow occurs here
//! 8 | }
//!   | - mutable borrow ends here
//! ```
//!
//! To see why this example fails to typecheck, we can introduce explicit lifetimes:
//!
//! ```rust,ignore
//! # use linjs::*;
//! fn unsafe_example<'a, C, S>(cx: &'a mut JSContext<S>) where
//!     S: CanAlloc<C> + CanAccess<C>,
//! {
//!     // x has type JSManaged<'b, C, String>
//!     let x = cx.manage(String::from("hello"));
//!     // Imagine something triggers GC here
//!     // x_ref has type &'c String
//!     let x_ref = x.get(cx);
//!     println!("{} world", x_ref);
//! }
//! ```
//!
//! We can now see why this fails to typecheck: since `cx` is borrowed mutably at type
//! `&'b mut JSContext<S>`, then immutably at type `&'c mut JSContext<S>` these lifetimes
//! cannot overlap, but the call to `x.get(cx)` requires them to overlap. These contradicting
//! constraints cause the example to fail to compile.
//!
//! To fix this example, we need to make sure that `x` lives long enough. One way to do this is
//! to root `x`, so that it will not be garbage collected. 
//!
//! ```rust
//! # use linjs::*;
//! fn example<'a, C: 'a, S>(cx: &'a mut JSContext<S>) where
//!     S: CanAlloc<C> + CanAccess<C> + CanRoot,
//! {
//!     // Function body has lifetime 'b
//!     // pinned has type JSPinnedRoot<'b, JSRoot<JSManaged<'b, C, String>>>
//!     rooted!(in(cx) let pinned = cx.manage(String::from("hello")));
//!     // x has type JSManaged<'b, C, String> 
//!     let x = pinned.get();
//!     // Imagine something triggers GC here
//!     // x_ref has type &'c String
//!     let x_ref = x.get(cx);
//!     println!("{} world", x_ref);
//! }
//! ```
//!
//! This example is now safe, since `x` is rooted during its access.
//! The example typechecks because the root has lifetime `'b`, and there is
//! no constraint that `'b` and `'c` don't overlap.
//! This use of lifetimes allows safe access to JS-managed data without a special
//! rooting lint.
//!
//! JS-managed lifetimes are variant, so can be converted to a more constrained
//! lifetime, for example if `'b` is a sublifetime of `'a`:
//! 
//! ```rust
//! # use linjs::*;
//! fn example<'a, 'b, C, S>(cx: &'a mut JSContext<S>) where
//!    'a: 'b,
//!    S: CanAlloc<C>,
//! {
//!    let x: JSManaged<'a, C, String> = cx.manage(String::from("hello"));
//!    let y: JSManaged<'b, C, String> = x;
//! }
//! ```
//!
//! JS managed data can be accessed mutably as well as immutably.
//! This is safe because mutably accessing JS manage data requires
//! mutably borrowing the JS context, so there cannot be two simultaneous
//! mutable accesses.
//!
//! ```rust
//! # use linjs::*;
//! fn example<C, S>(cx: &mut JSContext<S>, x: JSManaged<C, String>) where
//!     S: CanAccess<C>,
//! {
//!     println!("{} world", x.get(cx));
//!     *x.get_mut(cx) = String::from("hi");
//!     println!("{} world", x.get(cx));
//! }
//! ```
//!
//! An attempt to mutably access JS managed data more than once simultaneously
//! results in an error from the borrow-checker, for example:
//!
//! ```rust,ignore
//! # use linjs::*; use std::mem;
//! fn unsafe_example<C, S>(cx: &mut JSContext<S>, x: JSManaged<C, String>, y: JSManaged<C, String>) where
//!     S: CanAccess<C>,
//! {
//!     mem::swap(x.get_mut(cx), y.get_mut(cx));
//! }
//! ```
//!
//! ```text
//!         error[E0499]: cannot borrow `*cx` as mutable more than once at a time
//!  --> <anon>:7:40
//!   |
//! 7 |     mem::swap(x.get_mut(cx), y.get_mut(cx));
//!   |                         --             ^^ - first borrow ends here
//!   |                         |              |
//!   |                         |              second mutable borrow occurs here
//!   |                         first mutable borrow occurs here
//! ```
//!
//! One way to build cyclic structures is by mutable update, for example:
//!
//! ```rust
//! # #[macro_use] extern crate linjs;
//! # #[macro_use] extern crate linjs_derive;
//! # use linjs::*;
//! #[derive(JSManageable)]
//! struct NativeLoop<'a, C> {
//!    next: Option<Loop<'a, C>>,
//! }
//! type Loop<'a, C> = JSManaged<'a, C, NativeLoop<'a, C>>;
//! fn example<C, S>(cx: &mut JSContext<S>) where
//!     S: CanAccess<C> + CanAlloc<C> + CanRoot,
//! {
//!    rooted!(in(cx) let l = cx.manage(NativeLoop { next: None }));
//!    l.get().get_mut(cx).next = Some(l.get());
//! }
//! # fn main() {}
//! ```
//!
//! Some cases of building JS managed data require rooting, but in some cases
//! the rooting can be avoided, since the program does nothing to trigger
//! garbage collection. In this case, we can snapshot the JS context after
//! performing allocation. The snapshot supports accessing JS managed data,
//! but does not support any calls that might trigger garbage collection.
//! As a result, we know that any data which is live at the beginning of
//! the snapshot is also live at the end.
//!
//! ```rust
//! # #[macro_use] extern crate linjs;
//! # #[macro_use] extern crate linjs_derive;
//! # use linjs::*;
//! # #[derive(JSManageable)]
//! # struct NativeLoop<'a, C> {
//! #    next: Option<Loop<'a, C>>,
//! # }
//! # type Loop<'a, C> = JSManaged<'a, C, NativeLoop<'a, C>>;
//! fn example<C, S>(cx: &mut JSContext<S>) where
//!     S: CanAccess<C> + CanAlloc<C>
//! {
//!    let (ref mut cx, l) = cx.snapshot_manage(NativeLoop { next: None });
//!    l.get_mut(cx).next = Some(l);
//! }
//! # fn main() {}
//! ```
//!
//! A program which tries to use a function which might trigger GC will
//! not typecheck, as the snapshotted JS context state does not support
//! the appropriate traits. For example:
//!
//! ```rust,ignore
//! # #[macro_use] extern crate linjs;
//! # #[macro_use] extern crate linjs_derive;
//! # use linjs::*;
//! # #[derive(JSManageable)]
//! # struct NativeLoop<'a, C> {
//! #    next: Option<Loop<'a, C>>,
//! # }
//! # type Loop<'a, C> = JSManaged<'a, C, NativeLoop<'a, C>>;
//! fn might_trigger_gc<C, S>(cx: &mut JSContext<S>) where
//!     S: CanAccess<C> + CanAlloc<C>
//! { }
//!
//! fn unsafe_example<C, S>(cx: &mut JSContext<S>) where
//!     S: CanAccess<C> + CanAlloc<C>
//! {
//!    let (ref mut cx, l) = cx.snapshot_manage(NativeLoop { next: None });
//!    might_trigger_gc(cx);
//!    l.get_mut(cx).next = Some(l);
//! }
//! # fn main() {}
//! ```
//!
//! In this program, the function `might_trigger_gc` requires the state
//! to support `CanAlloc<C>`, which is not allowed by the snapshotted state.
//! 
//! ```text
//! 	error[E0277]: the trait bound `linjs::Snapshotted<'_, S>: linjs::CanAlloc<C>` is not satisfied
//!   --> <anon>:16:4
//!    |
//! 16 |    might_trigger_gc(cx);
//!    |    ^^^^^^^^^^^^^^^^ the trait `linjs::CanAlloc<C>` is not implemented for `linjs::Snapshotted<'_, S>`
//!    |
//!    = note: required by `might_trigger_gc`
//! ```

// TODO: write docs for globals, runnables, cyclic initialization.

//! #Examples
//!
//! This is an example of building a two-node cyclic graph, which is the smallest
//! example that Rust would need `Rc` and `RefCell` for. Note that this builds
//! the graph with no need for rooting.
//!
//! ```
//! #[macro_use] extern crate linjs;
//! #[macro_use] extern crate linjs_derive;
//! use linjs::{CanAlloc, CanAccess, CanExtend, CanInitialize, CanRoot};
//! use linjs::{JSContext, JSManageable, JSManaged, JSRunnable, JSTraceable};
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
//!         S: CanAccess<C> + CanAlloc<C> + CanRoot
//!     {
//!         // Creating nodes does memory allocation, which may trigger GC,
//!         // so we need to be careful about lifetimes while they are being added.
//!         // Approach 1 is to root the node.
//!         rooted!(in(cx) let pinned = cx.manage(NativeNode { data: 1, edges: vec![] }));
//!         let node1 = pinned.get();
//!         graph.get_mut(cx).nodes.push(node1);
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

#![feature(generic_param_attrs)]
#![feature(dropck_eyepatch)]

use std::marker::PhantomData;
use std::mem;
use std::ptr;

/// The type for JS contexts whose current state is `S`.
pub struct JSContext<S> {
    state: S,
}

/// A context state in an initialized compartment with global of type `G`.
pub struct Initialized<G> {
    global: G,
    roots: JSPinnedRoots,
}

/// A context state in snapshotted compartment in underlying state `S`,
/// which guarantees that no GC will happen during the lifetime `'a`.
pub struct Snapshotted<'a, S: 'a> (&'a mut S);

/// A context state in uninitialized compartment `C`.
pub struct Uninitialized<C> (PhantomData<C>);

/// A context state in the middle of initializing a compartment with global of type `G`.
pub struct Initializing<G> {
    global: G,
    roots: JSPinnedRoots,
}

/// A marker trait for JS contexts that can access native state
pub trait CanAccess<C> {}
impl<'a, C, T> CanAccess<C> for Initialized<JSManaged<'a, C, T>> {}
impl<'a, C, S> CanAccess<C> for Snapshotted<'a, S> where S: CanAccess<C> {}

/// A marker trait for JS contexts that can extend the lifetime of objects
pub trait CanExtend<'a, C> {}
impl<'a, C, S> CanExtend<'a, C> for Snapshotted<'a, S> where S: CanAccess<C> {}

/// A trait for JS contexts that can create roots
pub trait CanRoot {
    fn roots(self) -> JSPinnedRoots;
    fn roots_ref(&self) -> &JSPinnedRoots;
    fn roots_mut(&mut self) -> &mut JSPinnedRoots;
}
impl<G> CanRoot for Initialized<G> {
    fn roots(self) -> JSPinnedRoots {
        self.roots
    }
    fn roots_ref(&self) -> &JSPinnedRoots {
        &self.roots
    }
    fn roots_mut(&mut self) -> &mut JSPinnedRoots {
        &mut self.roots
    }
}
impl<G> CanRoot for Initializing<G> {
    fn roots(self) -> JSPinnedRoots {
        self.roots
    }
    fn roots_ref(&self) -> &JSPinnedRoots {
        &self.roots
    }
    fn roots_mut(&mut self) -> &mut JSPinnedRoots {
        &mut self.roots
    }
}

/// A marker trait for JS contexts that can allocate objects
pub trait CanAlloc<C> {}
impl<'a, C, T> CanAlloc<C> for Initialized<JSManaged<'a, C, T>> {}
impl<'a, C, T> CanAlloc<C> for Initializing<JSManaged<'a, C, T>> {}

/// A marker trait for JS contexts that can be initialized
pub trait CanInitialize<C> {}
impl<C> CanInitialize<C> for Uninitialized<C> {}

/// A marker trait for JS contexts that are in the middle of initializing
pub trait IsInitializing<G>: CanRoot + HasGlobal<G> {}
impl<G: Clone> IsInitializing<G> for Initializing<G> {}

/// A trait for JS contexts that have a global
pub trait HasGlobal<G> {
    fn global(&self) -> G;
}
impl<G> HasGlobal<G> for Initialized<G> where
    G: Clone,
{
    fn global(&self) -> G {
        self.global.clone()
    }
}
impl<G> HasGlobal<G> for Initializing<G> where
    G: Clone,
{
    fn global(&self) -> G {
        self.global.clone()
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
            state: Snapshotted(&mut self.state),
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
            state: Snapshotted(&mut self.state),
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
            state: Initializing {
                global: global,
                roots: JSPinnedRoots(ptr::null_mut()),
            }
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
            state: Initialized {
                global: global,
                roots: self.state.roots(),
            }
        }
    }

    /// Get the global of an initialized context.
    pub fn global<G>(&self) -> G where
        S: HasGlobal<G>,
        G: Clone,
    {
        self.state.global()
    }

    /// Create a new root.
    pub fn new_root<T>(&mut self) -> JSRoot<T> where
        S: CanRoot,
    {
        JSRoot {
            value: None,
            pin: JSUntypedPinnedRoot {
                value: unsafe { mem::zeroed() },
                next: ptr::null_mut(),
                prev: ptr::null_mut(),
            },
            roots: self.state.roots_mut(),
        }
    }

    // A real implementation would also have JS methods such as those in jsapi.
}

/// This is a placeholder for the real JSTraceable trait
pub unsafe trait JSTraceable {
    fn as_ptr(&self) -> *const JSTraceable where Self: Sized {
        unsafe { mem::transmute(self as &JSTraceable) }
    }
    fn as_mut_ptr(&mut self) -> *mut JSTraceable where Self: Sized {
        unsafe { mem::transmute(self as &mut JSTraceable) }
    }
}

unsafe impl JSTraceable for String {}
unsafe impl JSTraceable for usize {}
unsafe impl<T> JSTraceable for Option<T> where T: JSTraceable {}
unsafe impl<T> JSTraceable for Vec<T> where T: JSTraceable {}
// etc.

/// Change the JS-managed lifetime of a type.
/// The real thing would include a JS tracer.
pub unsafe trait JSManageable<'a, C> : JSTraceable {
    /// This type should have the same memory represention as `Self`.
    /// The only difference between `Self` and `Self::Aged`
    /// is that any `JSManaged<'b, C, T>` should be replaced by
    /// `JSManaged<'a, C, T::Aged>`.
    type Aged: 'a + JSManageable<'a, C, Aged=Self::Aged>;

    unsafe fn change_lifetime(self) -> Self::Aged where Self: Sized {
        let result = mem::transmute_copy(&self);
        mem::forget(self);
        result
    }

    unsafe fn change_lifetime_ref(&'a self) -> &'a Self::Aged {
        &*(self as *const Self as *const Self::Aged)
    }

    unsafe fn change_lifetime_mut(&'a mut self) -> &'a mut Self::Aged {
        &mut *(self as *mut Self as *mut Self::Aged)
    }

    fn contract_lifetime(self) -> Self::Aged where Self: 'a + Sized {
        unsafe { self.change_lifetime() }
    }

    fn contract_lifetime_ref(&'a self) -> &'a Self::Aged where Self: 'a {
        unsafe { self.change_lifetime_ref() }
    }

    fn contract_lifetime_mut(&'a mut self) -> &'a mut Self::Aged where Self: 'a {
        unsafe { self.change_lifetime_mut() }
    }
}

unsafe impl<'a, C> JSManageable<'a, C> for String {
    type Aged = String;
    unsafe fn change_lifetime(self) -> Self::Aged where Self: Sized {
        self
    }
}

unsafe impl<'a, C> JSManageable<'a, C> for usize {
    type Aged = usize;
    unsafe fn change_lifetime(self) -> Self::Aged where Self: Sized {
        self
    }
}

unsafe impl<'a, C, T> JSManageable<'a, C> for Vec<T> where T: JSManageable<'a, C> {
    type Aged = Vec<T::Aged>;
    unsafe fn change_lifetime(self) -> Self::Aged where Self: Sized {
        mem::transmute(self)
    }
}
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

unsafe impl<'a, C, T: ?Sized> JSTraceable for JSManaged<'a, C, T> where
    T: JSTraceable
{}

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
}

/// A stack allocated root
pub struct JSRoot<T> {
    value: Option<T>,
    pin: JSUntypedPinnedRoot,
    roots: *mut JSPinnedRoots,
}

/// A stack allocated root that haz been pinned, so the backing store can't move.
pub struct JSPinnedRoot<'a, T:'a> (&'a mut JSRoot<T>);

/// A doubly linked list with all the pinned roots.
#[derive(Eq, PartialEq)]
pub struct JSPinnedRoots(*mut JSUntypedPinnedRoot);

/// A stack allocated root that has been pinned, but we don't have a type for the contents
struct JSUntypedPinnedRoot {
    value: *mut JSTraceable,
    next: *mut JSUntypedPinnedRoot,
    prev: *mut JSUntypedPinnedRoot,
}

impl<T> JSRoot<T> {
    // Very annoyingly, this function has to be marked as unsafe,
    // because we can't rely on the destructor for the pinned root running.
    // See the discussion about `mem::forget` being safe at
    // https://github.com/rust-lang/rfcs/pull/1066.
    // This is safe as long as it is unpinned before the memory
    // is reclaimed, but Rust does not enforce that.
    pub unsafe fn pin<'a, C, U>(&'a mut self, value: U) -> JSPinnedRoot<'a, T> where
        T: JSManageable<'a, C, Aged=T>,
        U: JSManageable<'a, C, Aged=T>,
    {
        self.value = Some(value.change_lifetime());
        self.pin.value = self.value.as_mut_ptr();
        self.pin.next = (*self.roots).0;
        self.pin.prev = ptr::null_mut();
        if let Some(next) = self.pin.next.as_mut() {
            next.prev = &mut self.pin;
        }
        *self.roots = JSPinnedRoots(&mut self.pin);
        JSPinnedRoot(self)
    }

    pub unsafe fn unpin(&mut self) {
        if let Some(next) = self.pin.next.as_mut() {
            next.prev = self.pin.prev;
        }
        if let Some(prev) = self.pin.prev.as_mut() {
            prev.next = self.pin.next;
        }
        if *self.roots == JSPinnedRoots(&mut self.pin) {
            *self.roots = JSPinnedRoots(self.pin.next);
        }
        self.value = None;
        self.pin.value = mem::zeroed();
        self.pin.next = ptr::null_mut();
        self.pin.prev = ptr::null_mut();
    }
}

impl<'a, T> JSPinnedRoot<'a, T> {
    pub fn get<'b, C>(&'b self) -> T::Aged where
        T: JSManageable<'b, C>,
        T::Aged: Copy,
    {
        *self.get_ref()
    }

    pub fn get_ref<'b, C>(&'b self) -> &'b T::Aged where
        T: JSManageable<'b, C>,
    {
        self.0.value.as_ref().unwrap().contract_lifetime_ref()
    }

    pub fn get_mut<'b, C>(&'b mut self) -> &'b mut T::Aged where
        T: JSManageable<'b, C>,
    {
        self.0.value.as_mut().unwrap().contract_lifetime_mut()
    }
}


#[macro_export]
macro_rules! rooted {
    (in($cx:expr) let $name:ident = $init:expr) => (
        let mut __root = $cx.new_root();
        let ref $name = unsafe { __root.pin($init) };
    );
    (in($cx:expr) let mut $name:ident = $init:expr) => (
        let mut __root = $cx.new_root();
        let ref mut $name = unsafe { __root.pin($init) };
    )
}
