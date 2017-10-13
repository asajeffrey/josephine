//! An outline of how Rust's ownership, borrowing and lifetimes could be combined with JS-managed data
//!
//! The goals are:
//!
//! 1. Ensure that JS objects are only accessed in the right JS compartment.
//! 2. Support stack-allocated roots.
//! 3. Remove the need for the rooting lint.
//! 4. Don't require rooting in code that can't perform GC.
//! 5. Allow `&mut T` access to JS-managed data, so we don't need as much interior mutability.
//!
//! # JS-managed data
//!
//! The idea is that Rust data can be given to JS to manage, and then accessed,
//! using the JS context. This is passed as a variable of type `JSContext<S>`,
//! where the type parameter `S` is used to track the state of the context.
//!
//! For example, we can give JS some Rust data to manage in compartment
//! `C` when the context state implements the `CanAlloc` trait:
//!
//! ```rust
//! # use linjs::*;
//! fn example<C, S>(cx: &mut JSContext<S>) where
//!     S: CanAlloc + InCompartment<C>,
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
//!     S: CanAccess + InCompartment<C>,
//! {
//!     println!("{} world", x.borrow(cx));
//! }
//! ```
//!
//! # Lifetimes of JS-managed data
//!
//! Unfortunately, combining these two examples is not memory-safe, due to
//! garbage collection:
//!
//! ```rust,ignore
//! # use linjs::*;
//! fn unsafe_example<C, S>(cx: &mut JSContext<S>) where
//!     S: CanAlloc + CanAccess + InCompartment<C>,
//! {
//!     let x: JSManaged<C, String> = cx.manage(String::from("hello"));
//!     // Imagine something triggers GC here
//!     println!("{} world", x.borrow(cx));
//! }
//! ```
//!
//! This example is not safe, as there is nothing keeping `x` alive in JS,
//! so if garbage collection is triggered, then `x` will be reclaimed
//! which will drop the Rust data, and so the call to `x.borrow(cx)` will be a use-after-free.
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
//! 7 |     println!("{} world", x.borrow(cx));
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
//!     S: CanAlloc + CanAccess + InCompartment<C>,
//! {
//!     // x has type JSManaged<'b, C, String>
//!     let x = cx.manage(String::from("hello"));
//!     // Imagine something triggers GC here
//!     // x_ref has type &'c String
//!     let x_ref = x.borrow(cx);
//!     println!("{} world", x_ref);
//! }
//! ```
//!
//! We can now see why this fails to typecheck: since `cx` is borrowed mutably at type
//! `&'b mut JSContext<S>`, then immutably at type `&'c mut JSContext<S>` these lifetimes
//! cannot overlap, but the call to `x.borrow(cx)` requires them to overlap. These contradicting
//! constraints cause the example to fail to compile.
//!
//! # Rooting
//!
//! To fix this example, we need to make sure that `x` lives long enough. One way to do this is
//! to root `x`, so that it will not be garbage collected.
//!
//! ```rust
//! # use linjs::*;
//! fn example<'a, C: 'a, S>(cx: &'a mut JSContext<S>) where
//!     S: CanAlloc + CanAccess + InCompartment<C>,
//! {
//!     // Function body has lifetime 'b
//!     // x has type JSManaged<'b, C, String>
//!     rooted!(in(cx) let x = cx.manage(String::from("hello")));
//!     // Imagine something triggers GC here
//!     // x_ref has type &'c String
//!     let x_ref = x.borrow(cx);
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
//! We can root any data which implements the `JSRootable` and `JSTraceable` traits,
//! which includes JS-managed data. These traits can be derived,
//! using the `#[derive(JSRootable, JSTraceable)]` type annotation.
//!
//! # Mutating JS-managed data
//!
//! JS managed data can be accessed mutably as well as immutably.
//! This is safe because mutably accessing JS manage data requires
//! mutably borrowing the JS context, so there cannot be two simultaneous
//! mutable accesses.
//!
//! ```rust
//! # use linjs::*;
//! fn example<C, S>(cx: &mut JSContext<S>, x: JSManaged<C, String>) where
//!     S: CanAccess + InCompartment<C>,
//! {
//!     println!("{} world", x.borrow(cx));
//!     *x.borrow_mut(cx) = String::from("hi");
//!     println!("{} world", x.borrow(cx));
//! }
//! ```
//!
//! An attempt to mutably access JS managed data more than once simultaneously
//! results in an error from the borrow-checker, for example:
//!
//! ```rust,ignore
//! # use linjs::*; use std::mem;
//! fn unsafe_example<C, S>(cx: &mut JSContext<S>, x: JSManaged<C, String>, y: JSManaged<C, String>) where
//!     S: CanAccess + InCompartment<C>,
//! {
//!     mem::swap(x.borrow_mut(cx), y.borrow_mut(cx));
//! }
//! ```
//!
//! ```text
//!         error[E0499]: cannot borrow `*cx` as mutable more than once at a time
//!  --> <anon>:7:40
//!   |
//! 7 |     mem::swap(x.borrow_mut(cx), y.borrow_mut(cx));
//!   |                         --             ^^ - first borrow ends here
//!   |                         |              |
//!   |                         |              second mutable borrow occurs here
//!   |                         first mutable borrow occurs here
//! ```
//!
//! Mutable update allows the construction of cyclic structures, for example:
//!
//! ```rust
//! # #[macro_use] extern crate linjs;
//! # #[macro_use] extern crate linjs_derive;
//! # use linjs::*;
//! #[derive(HasClass, JSTraceable, JSRootable)]
//! pub struct NativeLoop<'a, C> {
//!    next: Option<Loop<'a, C>>,
//! }
//! type Loop<'a, C> = JSManaged<'a, C, NativeLoopClass>;
//! fn example<C, S>(cx: &mut JSContext<S>) where
//!     S: CanAccess + CanAlloc + InCompartment<C>,
//! {
//!    rooted!(in(cx) let l = cx.manage(NativeLoop { next: None }));
//!    l.borrow_mut(cx).next = Some(l);
//! }
//! # fn main() {}
//! ```
//!
//! # Snapshots
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
//! # extern crate linjs;
//! # #[macro_use] extern crate linjs_derive;
//! # use linjs::*;
//! # #[derive(HasClass, JSTraceable, JSRootable)]
//! # pub struct NativeLoop<'a, C> {
//! #    next: Option<Loop<'a, C>>,
//! # }
//! # type Loop<'a, C> = JSManaged<'a, C, NativeLoopClass>;
//! fn example<C, S>(cx: &mut JSContext<S>) where
//!     S: CanAccess + CanAlloc + InCompartment<C>,
//! {
//!    let (ref mut cx, l) = cx.snapshot_manage(NativeLoop { next: None });
//!    l.borrow_mut(cx).next = Some(l);
//! }
//! # fn main() {}
//! ```
//!
//! A program which tries to use a function which might trigger GC will
//! not typecheck, as the snapshotted JS context state does not support
//! the appropriate traits. For example:
//!
//! ```rust,ignore
//! # extern crate linjs;
//! # #[macro_use] extern crate linjs_derive;
//! # use linjs::*;
//! # #[derive(HasClass, JSTraceable, JSRootable)]
//! # pub struct NativeLoop<'a, C> {
//! #    next: Option<Loop<'a, C>>,
//! # }
//! # type Loop<'a, C> = JSManaged<'a, C, NativeLoopClass>;
//! fn might_trigger_gc<C, S>(cx: &mut JSContext<S>) where
//!     S: CanAccess + CanAlloc + InCompartment<C>
//! { }
//!
//! fn unsafe_example<C, S>(cx: &mut JSContext<S>) where
//!     S: CanAccess + CanAlloc + InCompartment<C>
//! {
//!    let (ref mut cx, l) = cx.snapshot_manage(NativeLoop { next: None });
//!    might_trigger_gc(cx);
//!    l.borrow_mut(cx).next = Some(l);
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
//!
//! # Globals
//!
//! JS contexts require initialization. In particular, each compartment has a global,
//! which should be JS managed data. The global can be initialized using `cx.init(value)`,
//! which updates the state of the context from uninitialized to initialized.
//!
//! ```rust
//! # extern crate linjs;
//! # #[macro_use] extern crate linjs_derive;
//! # use linjs::*;
//! #[derive(HasClass, JSTraceable, JSRootable)]
//! pub struct NativeMyGlobal { name: String }
//! type MyGlobal<'a, C> = JSManaged<'a, C, NativeMyGlobalClass>;
//!
//! fn example<'a, C, S>(cx: JSContext<S>) -> JSContext<Initialized<C>> where
//!    S: CanCreate<C>,
//!    C: HasGlobal<NativeMyGlobalClass>,
//! {
//!    let cx = cx.create_compartment();
//!    let name = String::from("Alice");
//!    cx.global_manage(NativeMyGlobal { name: name })
//! }
//! # fn main() {}
//! ```
//!
//! The current global can be accessed from the JS context, for example:
//!
//! ```rust
//! # #[macro_use] extern crate linjs;
//! # #[macro_use] extern crate linjs_derive;
//! # use linjs::*;
//! #[derive(HasClass, JSTraceable, JSRootable)]
//! # pub struct NativeMyGlobal { name: String }
//! # type MyGlobal<'a, C> = JSManaged<'a, C, NativeMyGlobalClass>;
//! #
//! fn example<'a, C, S>(cx: &JSContext<S>) where
//!    S: CanAccess + InCompartment<C>,
//!    C: HasGlobal<NativeMyGlobalClass>,
//! {
//!    println!("My global is named {}.", cx.global().borrow(cx).name);
//! }
//! # fn main() {}
//! ```
//!
//! In some cases, the global contains some JS-managed data, which is why the initialization
//! is split into two steps: creating the compartment, and
//! providing the JS-managed data for the global, for example:
//!
//! ```rust
//! # #[macro_use] extern crate linjs;
//! # #[macro_use] extern crate linjs_derive;
//! # use linjs::*;
//! #[derive(JSTraceable, JSRootable)]
//! struct NativeMyGlobal<'a, C> { name: JSManaged<'a, C, String> }
//! impl<'a, C> HasClass for NativeMyGlobal<'a, C> { type Class = MyGlobalClass; }
//!
//! struct MyGlobalClass;
//! impl<'a, C> HasInstance<'a, C> for MyGlobalClass { type Instance = NativeMyGlobal<'a, C>; }
//!
//! fn example<'a, C, S>(cx: JSContext<S>) -> JSContext<Initialized<C>> where
//!    S: CanCreate<C>,
//!    C: HasGlobal<MyGlobalClass>,
//! {
//!    let mut cx = cx.create_compartment();
//!    rooted!(in(cx) let name = cx.manage(String::from("Alice")));
//!    cx.global_manage(NativeMyGlobal { name: name })
//! }
//! # fn main() {}
//! ```
//!
//! During initialization, it is safe to perform allocation, but
//! not much else, as the global is still uninitialized.
//! For example:
//!
//! ```rust,ignore
//! # #[macro_use] extern crate linjs;
//! # #[macro_use] extern crate linjs_derive;
//! # use linjs::*;
//! # #[derive(HasClass, JSTraceable, JSRootable)]
//! # pub struct NativeMyGlobal<'a, C> { name: JSManaged<'a, C, String> }
//! # type MyGlobal<'a, C> = JSManaged<'a, C, NativeMyGlobalClass>;
//! #
//! fn unsafe_example<'a, C, S>(cx: JSContext<S>) -> JSContext<Initialized<C>> where
//!    S: CanCreate<C>,
//!    C: HasGlobal<NativeMyGlobalClass>,
//! {
//!    let mut cx = cx.create_compartment();
//!    let oops = cx.global().borrow(&cx).name.borrow(&cx);
//!    rooted!(in(cx) let name = cx.manage(String::from("Alice")));
//!    cx.global_manage(NativeMyGlobal { name: name })
//! }
//! # fn main() {}
//! ```
//!
//! This code is unsafe, since the global is accessed before it is initialized,
//! but does not typecheck because the context state does not allow accessing
//! JS-managed data during initialization.
//!
//! ```text
//! 	error[E0277]: the trait bound `linjs::Initializing<linjs::JSManaged<'_, C, _>>: linjs::CanAccess<C>` is not satisfied
//!   --> <anon>:14:27
//!    |
//! 14 |    let oops = cx.global().borrow(&cx).name.borrow(&cx);
//!    |                           ^^^ the trait `linjs::CanAccess<C>` is not implemented for `linjs::Initializing<linjs::JSManaged<'_, C, _>>`
//! ```
//!
//! # Bootstrapping
//!
//! To bootstrap initialization, a user defines a type which implements the `JSGlobal` trait.
//! This requires a `init` method, which takes the JS context as an argument.
//!
//! ```rust
//! # extern crate linjs;
//! # #[macro_use] extern crate linjs_derive;
//! # use linjs::*;
//! #[derive(HasClass, JSTraceable, JSRootable)]
//! pub struct NativeMyGlobal { name: String }
//!
//! impl JSGlobal for NativeMyGlobalClass {
//!     fn init<C, S>(cx: JSContext<S>) -> JSContext<Initialized<C>> where
//!         S: CanCreate<C>,
//!         C: HasGlobal<NativeMyGlobalClass>,
//!     {
//!         let cx = cx.create_compartment();
//!         let name = String::from("Alice");
//!         let cx = cx.global_manage(NativeMyGlobal { name: name });
//!         assert_eq!(cx.global().borrow(&cx).name, "Alice");
//!         cx
//!     }
//! }
//!
//! fn main() {
//!     let mut cx = JSContext::new();
//!     cx.new_global::<NativeMyGlobalClass>();
//! }
//! ```
//!
//! #Examples
//!
//! This is an example of building a two-node cyclic graph, which is the smallest
//! example that Rust would need `Rc` and `RefCell` for. Note that this builds
//! the graph with no need for rooting.
//!
//! ```
//! #[macro_use] extern crate linjs;
//! #[macro_use] extern crate linjs_derive;
//! use linjs::{CanAlloc, CanAccess, CanCreate};
//! use linjs::{HasClass, HasGlobal, HasInstance, InCompartment};
//! use linjs::{Initialized, JSContext, JSManaged, JSGlobal, JSRootable};
//!
//! // A graph type
//! type Graph<'a, C> = JSManaged<'a, C, GraphClass>;
//! #[derive(JSTraceable, JSRootable)]
//! struct NativeGraph<'a, C> {
//!     nodes: Vec<Node<'a, C>>,
//! }
//! impl<'a, C> HasClass for NativeGraph<'a, C> { type Class = GraphClass; }
//!
//! struct GraphClass;
//! impl<'a, C> HasInstance<'a, C> for GraphClass { type Instance = NativeGraph<'a, C>; }
//!
//! // A node type
//! type Node<'a, C> = JSManaged<'a, C, NodeClass>;
//! #[derive(JSTraceable, JSRootable)]
//! struct NativeNode<'a, C> {
//!     data: usize,
//!     edges: Vec<Node<'a, C>>,
//! }
//! impl<'a, C> HasClass for NativeNode<'a, C> { type Class = NodeClass; }
//!
//! struct NodeClass;
//! impl<'a, C> HasInstance<'a, C> for NodeClass { type Instance = NativeNode<'a, C>; }
//!
//! // Build a cyclic graph
//! impl JSGlobal for GraphClass {
//!     fn init<C, S>(cx: JSContext<S>) -> JSContext<Initialized<C>> where
//!         S: CanCreate<C>,
//!         C: HasGlobal<GraphClass>,
//!     {
//!         let cx = cx.create_compartment();
//!         let mut cx = cx.global_manage(NativeGraph { nodes: vec![] });
//!         let ref mut root = cx.new_root();
//!         let graph = cx.global().in_root(root);
//!         add_node1(&mut cx, graph);
//!         add_node2(&mut cx, graph);
//!         assert_eq!(graph.borrow(&cx).nodes[0].borrow(&cx).data, 1);
//!         assert_eq!(graph.borrow(&cx).nodes[1].borrow(&cx).data, 2);
//!         add_edges(&mut cx, graph);
//!         assert_eq!(graph.borrow(&cx).nodes[0].borrow(&cx).edges[0].borrow(&cx).data, 2);
//!         assert_eq!(graph.borrow(&cx).nodes[1].borrow(&cx).edges[0].borrow(&cx).data, 1);
//!         cx
//!     }
//! }
//!
//! fn add_node1<S, C>(cx: &mut JSContext<S>, graph: Graph<C>) where
//!     S: CanAccess + CanAlloc + InCompartment<C>
//! {
//!     // Creating nodes does memory allocation, which may trigger GC,
//!     // so we need to be careful about lifetimes while they are being added.
//!     // Approach 1 is to root the node.
//!     rooted!(in(cx) let node1 = cx.manage(NativeNode { data: 1, edges: vec![] }));
//!     graph.borrow_mut(cx).nodes.push(node1);
//! }
//!
//! fn add_node2<S, C>(cx: &mut JSContext<S>, graph: Graph<C>) where
//!     S: CanAccess + CanAlloc + InCompartment<C>
//!  {
//!     // Approach 2 is to take a snapshot of the context right after allocation.
//!     let (ref mut cx, node2) = cx.snapshot_manage(NativeNode { data: 2, edges: vec![] });
//!     graph.borrow_mut(cx).nodes.push(node2);
//! }
//!
//! fn add_edges<S, C>(cx: &mut JSContext<S>, graph: Graph<C>) where
//!     S: CanAccess + InCompartment<C>
//!  {
//!     let ref mut cx = cx.snapshot();
//!     // Note that there's no rooting here.
//!     let node1 = graph.borrow(cx).nodes[0].extend_lifetime(cx);
//!     let node2 = graph.borrow(cx).nodes[1].extend_lifetime(cx);
//!     node1.borrow_mut(cx).edges.push(node2);
//!     node2.borrow_mut(cx).edges.push(node1);
//! }
//!
//! fn main() {
//!     let mut cx = JSContext::new();
//!     cx.new_global::<GraphClass>();
//! }
//! ```

#![feature(associated_type_defaults)]
#![feature(const_fn)]
#![feature(const_ptr_null)]
#![feature(generic_param_attrs)]
#![feature(dropck_eyepatch)]

extern crate js;
extern crate libc;
#[macro_use] extern crate log;

use js::JSCLASS_GLOBAL_SLOT_COUNT;
use js::JSCLASS_IS_GLOBAL;
use js::JSCLASS_RESERVED_SLOTS_MASK;

use js::glue::CallObjectTracer;
use js::glue::NewCompileOptions;

use js::jsapi;
use js::jsapi::CompartmentOptions;
use js::jsapi::Evaluate2;
use js::jsapi::GCTraceKindToAscii;
use js::jsapi::Handle;
use js::jsapi::HandleValue;
use js::jsapi::Heap;
use js::jsapi::InitSelfHostedCode;
use js::jsapi::JSAutoCompartment;
use js::jsapi::JSCLASS_RESERVED_SLOTS_SHIFT;
use js::jsapi::JSClass;
use js::jsapi::JSClassOps;
use js::jsapi::JSFunctionSpec;
use js::jsapi::JSNative;
use js::jsapi::JSNativeWrapper;
use js::jsapi::JSObject;
use js::jsapi::JSPrincipals;
use js::jsapi::JSPropertySpec;
use js::jsapi::JSRuntime;
use js::jsapi::JSVersion;
use js::jsapi::JS_AddExtraGCRootsTracer;
use js::jsapi::JS_ClearPendingException;
use js::jsapi::JS_DefineFunctions;
use js::jsapi::JS_DefineProperties;
use js::jsapi::JS_DestroyRuntime;
use js::jsapi::JS_FlattenString;
use js::jsapi::JS_GC;
use js::jsapi::JS_GetContext;
use js::jsapi::JS_GetLatin1FlatStringChars;
use js::jsapi::JS_GetObjectPrototype;
use js::jsapi::JS_GetPendingException;
use js::jsapi::JS_GetReservedSlot;
use js::jsapi::JS_GetRuntime;
use js::jsapi::JS_GetStringLength;
use js::jsapi::JS_GetTwoByteFlatStringChars;
use js::jsapi::JS_Init;
use js::jsapi::JS_InitClass;
use js::jsapi::JS_InitStandardClasses;
use js::jsapi::JS_IsExceptionPending;
use js::jsapi::JS_IsNative;
use js::jsapi::JS_NewRuntime;
use js::jsapi::JS_NewGlobalObject;
use js::jsapi::JS_NewObjectWithGivenProto;
use js::jsapi::JS_SetReservedSlot;
use js::jsapi::JS_ShutDown;
use js::jsapi::JS_StringHasLatin1Chars;
use js::jsapi::MutableHandle;
use js::jsapi::OnNewGlobalHookOption;
use js::jsapi::TraceKind;

use js::jsval::JSVal;
use js::jsval::ObjectValue;
use js::jsval::PrivateValue;
use js::jsval::StringValue;
use js::jsval::UndefinedValue;

pub use js::jsapi::JSTracer;

use libc::c_char;
use libc::c_uint;

use std::any::TypeId;
use std::cell::UnsafeCell;
use std::char;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Write;
use std::marker::PhantomData;
use std::mem;
use std::ops::Deref;
use std::ops::DerefMut;
use std::os::raw::c_void;
use std::ptr;
use std::rc::Rc;
use std::slice;
use std::str;

/// The type for JS contexts whose current state is `S`.
pub struct JSContext<S> {
    jsapi_context: *mut jsapi::JSContext,
    global_js_object: *mut Heap<*mut JSObject>,
    global_raw: *mut (),
    auto_compartment: Option<JSAutoCompartment>,
    runtime: Option<OwnedJSRuntime>,
    marker: PhantomData<S>,
}

/// A context state in an initialized compartment `C`.
pub struct Initialized<C> (PhantomData<C>);

/// A context state which can initialize compartment `C`.
pub struct Uninitialized<C> (PhantomData<C>);

/// A context state in the middle of initializing a compartment `C`.
pub struct Initializing<C> (PhantomData<C>);

/// A context state that is visiting `C`.
pub struct Visiting<C> (PhantomData<C>);

/// A context state for JS contexts owned by Rust.
pub struct Owned (());

/// A context state for callbacks from JS,
pub struct FromJS (());

/// A context state in snapshotted compartment in underlying state `S`,
/// which guarantees that no GC will happen during the lifetime `'a`.
pub struct Snapshotted<'a, S> (PhantomData<(&'a(), S)>);

/// A marker trait for JS contexts in compartment `C`
pub trait InCompartment<C> {}
impl<C> InCompartment<C> for Initializing<C> {}
impl<C> InCompartment<C> for Initialized<C> {}
impl<C> InCompartment<C> for Visiting<C> {}
impl<'a, C, S> InCompartment<C> for Snapshotted<'a, S> where S: InCompartment<C> {}

/// A marker trait for JS contexts that can access native state
pub trait CanAccess {}
impl<C> CanAccess for Initialized<C> {}
impl<C> CanAccess for Visiting<C> {}
impl CanAccess for FromJS {}
impl<'a, S> CanAccess for Snapshotted<'a, S> where S: CanAccess {}
impl CanAccess for Owned {}

/// A marker trait for JS contexts that can extend the lifetime of objects
pub trait CanExtend<'a> {}
impl<'a, S> CanExtend<'a> for Snapshotted<'a, S> {}

/// A marker trait for JS contexts that can (de)allocate objects
pub trait CanAlloc {}
impl<C> CanAlloc for Initialized<C> {}
impl<C> CanAlloc for Initializing<C> {}
impl<C> CanAlloc for Visiting<C> {}
impl CanAlloc for FromJS {}
impl CanAlloc for Owned {}

/// A marker trait for JS contexts that can create compartment `C`.
pub trait CanCreate<C> {}
impl<C> CanCreate<C> for Uninitialized<C> {}

/// A marker trait for JS contexts that can create new compartments.
pub trait CanCreateCompartments {}
impl CanCreateCompartments for Owned {}

/// A marker trait for JS contexts that are in the middle of initializing
pub trait IsInitializing {}
impl<C> IsInitializing for Initializing<C> {}

/// A marker trait for JS compartments that have a global of class `K`.
pub trait HasGlobal<K> {}

impl JSContext<Owned> {
    /// Create a new JSContext.
    pub fn new() -> JSContext<Owned> {
        // TODO: set options on the runtime?
        let runtime = OwnedJSRuntime::new();
        let jsapi_context = unsafe { JS_GetContext(runtime.0) };
        unsafe { JS_AddExtraGCRootsTracer(runtime.0, Some(trace_thread_local_roots), ptr::null_mut()); }
        JSContext {
            jsapi_context: jsapi_context,
            global_js_object: ptr::null_mut(),
            global_raw: ptr::null_mut(),
            auto_compartment: None,
            runtime: Some(runtime),
            marker: PhantomData,
        }
    }
}

impl<S> JSContext<S> {
    /// Get a snapshot of the JS state.
    /// The snapshot only allows access to the methods that are guaranteed not to call GC,
    /// so we don't need to root JS-managed pointers during the lifetime of a snapshot.
    pub fn snapshot<'a>(&'a mut self) -> JSContext<Snapshotted<'a, S>> {
        debug!("Creating snapshot.");
        JSContext {
            jsapi_context: self.jsapi_context,
            global_js_object: self.global_js_object,
            global_raw: self.global_raw,
            auto_compartment: None,
            runtime: None,
            marker: PhantomData,
        }
    }

    /// Give ownership of data to JS.
    /// This allocates JS heap, which may trigger GC.
    pub fn manage<'a, 'b, C, K, T>(&'a mut self, value: T) -> JSManaged<'a, C, K> where
        S: CanAlloc + InCompartment<C>,
        T: JSTraceable + HasClass<Class = K>,
        K: HasInstance<'b, C, Instance = T>,
    {
        debug!("Managing native data.");
        let cx = self.jsapi_context;
        let global = unsafe { &*self.global_js_object }.handle();
        let prototypes = unsafe { &mut *(JS_GetReservedSlot(global.get(), 2).to_private() as *mut HashMap<TypeId, Box<Heap<*mut JSObject>>>) };
        let prototype = prototypes.entry(TypeId::of::<K>()).or_insert_with(|| {
            let boxed = Box::new(Heap::default());
            boxed.set(unsafe { T::Init::js_init_class(cx, global) });
            boxed
        }).handle();

        let boxed_jsobject = Box::new(Heap::default());
        debug!("Boxed object {:p}", boxed_jsobject);
        let unboxed_jsobject = unsafe { JS_NewObjectWithGivenProto(cx, T::Init::classp(), prototype) };
        debug!("Unboxed object {:p}", unboxed_jsobject);
        assert!(!unboxed_jsobject.is_null());
        boxed_jsobject.set(unboxed_jsobject);

        // Save a pointer to the native value in a private slot
        let boxed_value: Box<JSManageable> = Box::new(Some(value));
        let fat_value: [*const libc::c_void; 2] = unsafe { mem::transmute(boxed_value) };
        unsafe { JS_SetReservedSlot(boxed_jsobject.get(), 0, PrivateValue(fat_value[0])) };
        unsafe { JS_SetReservedSlot(boxed_jsobject.get(), 1, PrivateValue(fat_value[1])) };

        // TODO: can we be sure that this won't trigger GC? Or do we need to root the boxed object?
        debug!("Initializing JS object.");
        unsafe { T::Init::js_init_object(cx, boxed_jsobject.handle()) };

        debug!("Managed native data.");
        JSManaged {
            js_object: Box::into_raw(boxed_jsobject),
            raw: fat_value[0] as *mut (),
            marker: PhantomData,
        }
    }

    /// Give ownership of data to JS.
    /// This allocates JS heap, which may trigger GC.
    pub fn snapshot_manage<'a, 'b, C, K, T>(&'a mut self, value: T) -> (JSContext<Snapshotted<'a, S>>, JSManaged<'a, C, K>) where
        S: CanAlloc + InCompartment<C>,
        T: JSTraceable + HasClass<Class = K>,
        K: HasInstance<'b, C, Instance = T>,
    {
        let jsapi_context = self.jsapi_context;
        let global_js_object = self.global_js_object;
        let global_raw =  self.global_raw;
        let managed = self.manage(value);

        debug!("Creating snapshot while managing.");
        let snapshot = JSContext {
            jsapi_context: jsapi_context,
            global_js_object: global_js_object,
            global_raw: global_raw,
            auto_compartment: None,
            runtime: None,
            marker: PhantomData,
        };
        (snapshot, managed)
    }

    /// Create a compartment
    pub fn create_compartment<'a, C, K, T>(self) -> JSContext<Initializing<C>> where
        S: CanCreate<C>,
        C: HasGlobal<K>,
        T: JSTraceable + HasClass<Class = K>,
        K: HasInstance<'a, C, Instance = T>,
    {
        debug!("Creating compartment.");
        let value: Option<T> = None;
        let boxed_value: Box<JSManageable> = Box::new(value);
        let fat_value: [*const libc::c_void; 2] = unsafe { mem::transmute(boxed_value) };

        let classp = unsafe { T::Init::global_classp() };
        let principals = unsafe { T::Init::global_principals() };
        let hook_options = unsafe { T::Init::global_hook_option() };
        let options = unsafe { T::Init::global_options() };
        let properties = unsafe { T::Init::properties() };
        let functions = unsafe { T::Init::functions() };

        let boxed_jsobject = Box::new(Heap::default());
        debug!("Boxed global {:p}", boxed_jsobject);
        let unboxed_jsobject = unsafe { JS_NewGlobalObject(self.jsapi_context, classp, principals, hook_options, &options) };
        debug!("Unboxed global {:p}", unboxed_jsobject);
        assert!(!unboxed_jsobject.is_null());
        boxed_jsobject.set(unboxed_jsobject);

        // TODO: can we be sure that this won't trigger GC? Or do we need to root the boxed object?
        debug!("Entering compartment.");
        let ac = JSAutoCompartment::new(self.jsapi_context, boxed_jsobject.get());

        // Save a pointer to the native value in a private slot
        unsafe { JS_SetReservedSlot(boxed_jsobject.get(), 0, PrivateValue(fat_value[0])) };
        unsafe { JS_SetReservedSlot(boxed_jsobject.get(), 1, PrivateValue(fat_value[1])) };

        // Keep a hash map of all the class prototypes
        // TODO: Fix this space leak!
        let prototypes: Box<HashMap<TypeId, Box<Heap<*mut JSObject>>>> = Box::new(HashMap::new());
        unsafe { JS_SetReservedSlot(boxed_jsobject.get(), 2, PrivateValue(Box::into_raw(prototypes) as *const _)) };

        // Define the properties and functions of the global
        if !properties.is_null() { unsafe { JS_DefineProperties(self.jsapi_context, boxed_jsobject.handle(), properties) }; }
        if !functions.is_null() { unsafe { JS_DefineFunctions(self.jsapi_context, boxed_jsobject.handle(), functions) }; }

        // TODO: can we be sure that this won't trigger GC? Or do we need to root the boxed object?
        debug!("Initializing compartment.");
        unsafe { T::Init::js_init_global(self.jsapi_context, boxed_jsobject.handle()) };

        debug!("Created compartment.");
        JSContext {
            jsapi_context: self.jsapi_context,
            global_js_object: Box::into_raw(boxed_jsobject),
            global_raw: fat_value[0] as *mut (),
            auto_compartment: Some(ac),
            runtime: self.runtime,
            marker: PhantomData,
        }
    }

    /// Finish initializing a JS Context
    pub fn global_manage<'a, C, K, T>(self, value: T) -> JSContext<Initialized<C>> where
        S: IsInitializing + InCompartment<C>,
        C: HasGlobal<K>,
        K: HasInstance<'a, C, Instance = T>,
        T: JSTraceable + HasClass<Class = K>,
    {
        debug!("Managing native global.");
        let raw = self.global_raw as *mut Option<T>;
        let uninitialized = unsafe { mem::replace(&mut *raw, Some(value)) };
        mem::forget(uninitialized);

        debug!("Initialized compartment.");
        JSContext {
            jsapi_context: self.jsapi_context,
            global_js_object: self.global_js_object,
            global_raw: self.global_raw,
            auto_compartment: self.auto_compartment,
            runtime: self.runtime,
            marker: PhantomData,
        }
    }

    /// Get the global of an initialized context.
    pub fn global<'a, C, G>(&'a self) -> JSManaged<'a, C, G> where
        S: InCompartment<C>,
        C: HasGlobal<G>,
    {
        JSManaged {
            js_object: self.global_js_object,
            raw: self.global_raw,
            marker: PhantomData,
        }
    }

    /// Create a new global.
    /// TODO: return a JSManaged with a wildcard compartment.
    pub fn new_global<'a, K>(&'a mut self) -> JSManaged<'a, SOMEWHERE, K> where
        S: CanCreateCompartments,
        K: JSGlobal,
    {
        struct JSCompartmentImpl;
        impl<K> HasGlobal<K> for JSCompartmentImpl {}
        let cx: JSContext<Uninitialized<JSCompartmentImpl>> = JSContext {
            jsapi_context: self.jsapi_context,
            global_js_object: ptr::null_mut(),
            global_raw: ptr::null_mut(),
            auto_compartment: None,
            runtime: None,
            marker: PhantomData,
        };
        unsafe { K::init(cx).global().forget_compartment().change_lifetime() }
    }
    
    /// Create a new root.
    pub fn new_root<'a, 'b, T>(&'b mut self) -> JSRoot<'a, T> {
        JSRoot {
            value: None,
            pin: JSUntypedPinnedRoot {
                value: unsafe { mem::zeroed() },
                next: ptr::null_mut(),
                prev: ptr::null_mut(),
            },
            marker: PhantomData,
        }
    }

    pub fn cx(&self) -> *mut jsapi::JSContext {
        self.jsapi_context
    }

    pub fn rt(&self) -> *mut jsapi::JSRuntime {
        unsafe { JS_GetRuntime(self.jsapi_context) }
    }

    pub fn gc(&mut self) where
        S: CanAlloc,
    {
        unsafe { JS_GC(self.rt()); }
    }

    pub fn evaluate(&mut self, code: &str) -> Result<JSVal, JSEvaluateErr> {
        let cx = self.jsapi_context;

        let options = unsafe { NewCompileOptions(cx, &0, 0) };

        let code_utf16: Vec<u16> = code.encode_utf16().collect();
        let code_ptr = &code_utf16[0] as *const u16;
        let code_len = code_utf16.len() as usize;

        let mut result = UndefinedValue();
        let result_mut = unsafe { MutableHandle::from_marked_location(&mut result) };

        unsafe { Evaluate2(cx, options, code_ptr, code_len, result_mut) };
        self.take_pending_exception()?;

        Ok(result)
    }

    pub fn take_pending_exception(&mut self) -> Result<(), JSEvaluateErr> {
        let cx = self.jsapi_context;
        if !unsafe { JS_IsExceptionPending(cx) } { return Ok(()); }

        let ref mut exn_ref = UndefinedValue();
        let exn_mut = unsafe { MutableHandle::from_marked_location(exn_ref) };
        let _exn_handle = exn_mut.handle();

        unsafe { JS_GetPendingException(cx, exn_mut) };
        // TODO: include the exception in the error report
        unsafe { JS_ClearPendingException(cx) };

        Err(JSEvaluateErr::JSException)
    }
}

/// A JS runtime owned by Rust
pub struct OwnedJSRuntime(*mut JSRuntime, Rc<OwnedJSInit>);

const DEFAULT_HEAPSIZE: u32 = 32_u32 * 1024_u32 * 1024_u32;
const CHUNK_SIZE: u32 = 1 << 20;

impl OwnedJSRuntime {
    fn new() -> OwnedJSRuntime {
        let js_init = OWNED_JSINIT.with(|init| init.clone());
        let js_runtime = unsafe { JS_NewRuntime(DEFAULT_HEAPSIZE, CHUNK_SIZE, ptr::null_mut()) };
        let js_context = unsafe { JS_GetContext(js_runtime) };
        unsafe { InitSelfHostedCode(js_context) };
        OwnedJSRuntime(js_runtime, js_init)
    }
}

impl Drop for OwnedJSRuntime {
    fn drop(&mut self) {
        unsafe { JS_DestroyRuntime(self.0) };
    }
}

/// A capability to create JSRuntimes, owned by Rust.
struct OwnedJSInit;

thread_local! { static OWNED_JSINIT: Rc<OwnedJSInit> = { unsafe { JS_Init() }; Rc::new(OwnedJSInit) }; }

impl Drop for OwnedJSInit {
    fn drop(&mut self) {
        unsafe { JS_ShutDown() };
    }
}

/// The errors which might be returned from cx.evaluate("code")
// TODO: store more information about the reason for the error
#[derive(Clone, Debug)]
pub enum JSEvaluateErr {
    JSException,
    NotAnObject,
    NotAString,
    NotJSManaged,
    WrongClass,
}

/// A trait for Rust data that can be traced.
pub unsafe trait JSTraceable {
    unsafe fn trace(&self, trc: *mut JSTracer);

    fn as_ptr(&self) -> *const JSTraceable where Self: Sized {
        unsafe { mem::transmute(self as &JSTraceable) }
    }

    fn as_mut_ptr(&mut self) -> *mut JSTraceable where Self: Sized {
        unsafe { mem::transmute(self as &mut JSTraceable) }
    }
}

unsafe impl JSTraceable for String {
    unsafe fn trace(&self, _trc: *mut JSTracer) {}
}

unsafe impl JSTraceable for usize {
    unsafe fn trace(&self, _trc: *mut JSTracer) {}
}

unsafe impl JSTraceable for () {
    unsafe fn trace(&self, _trc: *mut JSTracer) {}
}

unsafe impl<T> JSTraceable for Option<T> where T: JSTraceable {
    unsafe fn trace(&self, trc: *mut JSTracer) {
        if let Some(ref val) = *self {
            val.trace(trc);
        }
    }
}

unsafe impl<T> JSTraceable for Vec<T> where T: JSTraceable {
    unsafe fn trace(&self, trc: *mut JSTracer) {
        for val in self {
            val.trace(trc);
        }
    }
}

// etc.

/// A trait for Rust data which can be reflected

pub trait HasClass {
    type Class: 'static;
    type Init: JSInitializer = DefaultInitializer;
}

pub trait HasInstance<'a, C>: 'static + Sized {
    type Instance: HasClass<Class = Self>;
}

/// Basic types
impl HasClass for String { type Class = String; }
impl<'a, C> HasInstance<'a, C> for String { type Instance = String; }

impl<T> HasClass for Option<T> where T: HasClass { type Class = Option<T::Class>; }
impl<'a, C, K> HasInstance<'a, C> for Option<K> where K: HasInstance<'a, C> { type Instance = Option<K::Instance>; }

// etc.

/// A trait for Rust data which can be managed

pub trait JSManageable: JSTraceable {
    unsafe fn class_id(&self) -> TypeId;
}

impl<T> JSManageable for T where
    T: JSTraceable + HasClass
{
    unsafe fn class_id(&self) -> TypeId {
        TypeId::of::<T::Class>()
    }
}

/// Initialize JS data

pub trait JSInitializer {
    unsafe fn parent_prototype(cx: *mut jsapi::JSContext, global: jsapi::HandleObject) -> *mut JSObject {
        JS_GetObjectPrototype(cx, global)
    }

    unsafe fn classp() -> *const JSClass {
        &DEFAULT_CLASS
    }

    unsafe fn global_classp() -> *const JSClass {
        &DEFAULT_GLOBAL_CLASS
    }

    unsafe fn global_principals() -> *mut JSPrincipals {
        ptr::null_mut()
    }

    unsafe fn global_hook_option() -> OnNewGlobalHookOption {
         OnNewGlobalHookOption::DontFireOnNewGlobalHook
    }

    unsafe fn global_options() -> CompartmentOptions {
        let mut options = CompartmentOptions::default();
        options.behaviors_.version_ = JSVersion::JSVERSION_ECMA_5;
        options.creationOptions_.sharedMemoryAndAtomics_ = true;
        options
    }

    unsafe fn constructor() -> (JSNative, c_uint) {
        (None, 0)
    }

    unsafe fn properties() -> *const JSPropertySpec {
        ptr::null()
    }

    unsafe fn functions() -> *const JSFunctionSpec {
        ptr::null()
    }

    unsafe fn static_properties() -> *const JSPropertySpec {
        ptr::null()
    }

    unsafe fn static_functions() -> *const JSFunctionSpec {
        ptr::null()
    }

    unsafe fn js_init_class(cx: *mut jsapi::JSContext, global: jsapi::HandleObject) -> *mut JSObject {
        let ref parent_proto = Self::parent_prototype(cx, global);
        let parent_proto_handle = Handle::from_marked_location(parent_proto);
        let classp = Self::classp();
        let (constructor, nargs) = Self::constructor();
        let ps = Self::properties();
        let fs = Self::functions();
        let static_ps = Self::static_properties();
        let static_fs = Self::static_functions();
        JS_InitClass(cx, global, parent_proto_handle, classp, constructor, nargs, ps, fs, static_ps, static_fs)
    }

    unsafe fn js_init_object(_cx: *mut jsapi::JSContext, _obj: jsapi::HandleObject) {
    }

    unsafe fn js_init_global(cx: *mut jsapi::JSContext, global: jsapi::HandleObject) {
        JS_InitStandardClasses(cx, global);
    }
}

// Repating stuff from https://dxr.mozilla.org/mozilla-central/source/js/public/Class.h
// (it uses #defines which are not available in Rust)

pub const fn jsclass_has_reserved_slots(n: c_uint) -> c_uint {
    (n & JSCLASS_RESERVED_SLOTS_MASK) << JSCLASS_RESERVED_SLOTS_SHIFT
}

pub const fn jsclass_global_flags_with_slots(n: c_uint) -> c_uint {
    JSCLASS_IS_GLOBAL | jsclass_has_reserved_slots(JSCLASS_GLOBAL_SLOT_COUNT + n)
}

/// A default class.

pub struct DefaultInitializer;

impl JSInitializer for DefaultInitializer {}

static DEFAULT_CLASS: JSClass = JSClass {
    name: b"[Object]\0" as *const u8 as *const c_char,
    flags: jsclass_has_reserved_slots(2),
    cOps: &JSClassOps {
        addProperty: None,
        call: None,
        construct: None,
        delProperty: None,
        enumerate: None,
        finalize: Some(finalize_jsobject_with_native_data),
        getProperty: None,
        hasInstance: None,
        mayResolve: None,
        resolve: None,
        setProperty: None,
        trace: Some(trace_jsobject_with_native_data),
    },
    reserved: [0 as *mut _; 3],
};

static DEFAULT_GLOBAL_CLASS: JSClass = JSClass {
    name: b"[Global]\0" as *const u8 as *const c_char,
    flags: jsclass_global_flags_with_slots(2),
    cOps: &JSClassOps {
        addProperty: None,
        call: None,
        construct: None,
        delProperty: None,
        enumerate: None,
        finalize: Some(finalize_jsobject_with_native_data),
        getProperty: None,
        hasInstance: None,
        mayResolve: None,
        resolve: None,
        setProperty: None,
        trace: Some(trace_jsobject_with_native_data),
    },
    reserved: [0 as *mut _; 3],
};

pub const fn null_wrapper() -> JSNativeWrapper {
    JSNativeWrapper {
        op: None,
        info: ptr::null(),
    }
}

pub const fn null_property() -> JSPropertySpec {
    JSPropertySpec {
        name: ptr::null(),
        flags: 0,
        getter: null_wrapper(),
        setter: null_wrapper(),
    }
}

pub const fn null_function() -> JSFunctionSpec {
    JSFunctionSpec {
        name: ptr::null(),
        flags: 0,
        call: null_wrapper(),
        nargs: 0,
        selfHostedName: ptr::null(),
    }
}

pub unsafe extern "C" fn trace_jsobject_with_native_data(trc: *mut JSTracer, obj: *mut JSObject) {
    if !JS_IsNative(obj) {
        debug!("Not a native object (should be a prototype).");
    }

    debug!("Tracing {:p}.", obj);
    let slot0 = JS_GetReservedSlot(obj, 0);
    let slot1 = JS_GetReservedSlot(obj, 1);
    if slot0.is_undefined() || slot1.is_undefined() {
        return debug!("Tracing uninitialized object.");
    }
    let traceable: &JSManageable = mem::transmute([ slot0.to_private(), slot1.to_private() ]);
    debug!("Tracing native {:p}.", traceable);
    traceable.trace(trc);
}

pub unsafe extern "C" fn finalize_jsobject_with_native_data(_op: *mut js::jsapi::JSFreeOp, obj: *mut JSObject) {
    if !JS_IsNative(obj) {
        debug!("Not a native object (should be a prototype).");
        // TODO: remove the object from the prototype hash table?
    }

    debug!("Finalizing {:p}.", obj);
    let slot0 = JS_GetReservedSlot(obj, 0);
    let slot1 = JS_GetReservedSlot(obj, 1);
    if slot0.is_undefined() || slot1.is_undefined() {
        return debug!("Finalizing uninitialized object.");
    }
    let traceable: *mut JSManageable = mem::transmute([ slot0.to_private(), slot1.to_private() ]);
    debug!("Finalizing native {:p}.", traceable);
    Box::from_raw(traceable);
}

pub unsafe fn jscontext_called_from_js(cx: *mut jsapi::JSContext) -> JSContext<FromJS> {
    JSContext {
        jsapi_context: cx,
        global_js_object: ptr::null_mut(),
        global_raw: ptr::null_mut(),
        auto_compartment: None,
        runtime: None,
        marker: PhantomData,
    }
}

pub unsafe fn jsmanaged_called_from_js<'a, K>(js_value: HandleValue) -> Result<JSManaged<'a, FromJS, K>, JSEvaluateErr> where
    K: 'static
{
    if !js_value.is_object() {
        return Err(JSEvaluateErr::NotAnObject);
    }

    let js_object = js_value.to_object();

    if !JS_IsNative(js_object) {
        return Err(JSEvaluateErr::NotJSManaged);
    }

    let slot0 = JS_GetReservedSlot(js_object, 0);
    let slot1 = JS_GetReservedSlot(js_object, 1);
    if slot0.is_undefined() || slot1.is_undefined() {
        return Err(JSEvaluateErr::NotJSManaged);
    }
    // This unsafe if there are raw uses of jsapi that are doing
    // other things with the reserved slots.
    let native: &mut JSManageable = mem::transmute([ slot0.to_private(), slot1.to_private() ]);

    // TODO: inheritance
    if TypeId::of::<Option<K>>() != native.class_id() {
        return Err(JSEvaluateErr::WrongClass);
    }
    let raw = native as *mut _ as *mut ();

    // TODO: these boxes never get deallocated, which is a space leak!
    let boxed = Box::new(Heap::default());
    boxed.set(js_object);

    Ok(JSManaged {
        js_object: Box::into_raw(boxed),
        raw: raw,
        marker: PhantomData,
    })
}

/// A trait for a Rust class.

pub trait HasJSClass {
    fn js_class() -> &'static JSClass;
}

/// A trait for classes that can be used as a global.
pub trait JSGlobal: Sized {
    /// This callback is called with a fresh JS compartment type `C`.
    fn init<C, S>(cx: JSContext<S>) -> JSContext<Initialized<C>> where
        S: CanCreate<C>,
        C: HasGlobal<Self>;
}

/// The type of JS-managed strings in the same zone as compartment `C`, with lifetime `a`.
/// Rust is much happier with flat string representations, so we flatten
/// strings when they come into Rust.
pub struct JSString<'a, C> {
    js_string: *mut jsapi::JSFlatString,
    marker: PhantomData<(&'a(), C)>,
}

impl<'a, C> Clone for JSString<'a, C> {
    fn clone(&self) -> JSString<'a, C> {
        JSString {
            js_string: self.js_string,
            marker: PhantomData,
        }
    }
}

impl<'a, C> Copy for JSString<'a, C> {}

impl<'a, C> JSString<'a, C> {
    pub fn to_jsstring(self) -> *mut jsapi::JSString {
        self.js_string as *mut jsapi::JSString
    }

    pub fn to_jsval(self) -> JSVal {
        StringValue(unsafe { &*self.to_jsstring() })
    }

    pub fn len(self) -> usize {
        unsafe { JS_GetStringLength(self.to_jsstring()) }
    }

    pub fn has_latin1_chars(self) -> bool {
        unsafe { JS_StringHasLatin1Chars(self.to_jsstring()) }
    }

    pub unsafe fn get_latin1_chars(self) -> &'a str {
        let nogc = ptr::null_mut(); // TODO: build an AutoCheckCannotGC?
        let raw = JS_GetLatin1FlatStringChars(nogc, self.js_string);
        str::from_utf8_unchecked(slice::from_raw_parts(raw, self.len()))
    }

    pub unsafe fn get_two_byte_chars(self) -> &'a [u16] {
        let nogc = ptr::null_mut(); // TODO: build an AutoCheckCannotGC?
        let raw = JS_GetTwoByteFlatStringChars(nogc, self.js_string);
        slice::from_raw_parts(raw, self.len())
    }

    pub fn contents(self) -> JSStringContents<'a> {
        if self.has_latin1_chars() {
            JSStringContents::Latin1(unsafe { self.get_latin1_chars() })
        } else {
            JSStringContents::TwoByte(unsafe { self.get_two_byte_chars() })
        }
    }
}

impl<'a, C> Display for JSString<'a, C> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.contents().fmt(f)
    }
}

pub enum JSStringContents<'a> {
    Latin1(&'a str),
    TwoByte(&'a[u16]),
}

impl<'a> Display for JSStringContents<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            JSStringContents::Latin1(ref string) => f.write_str(string),
            JSStringContents::TwoByte(ref slice) => char::decode_utf16(slice.iter().cloned())
                .map(|ch| ch.unwrap_or(char::REPLACEMENT_CHARACTER))
                .map(|ch| f.write_char(ch))
                .find(Result::is_err)
                .unwrap_or(Ok(()))
        }
    }
}

pub unsafe fn jsstring_called_from_js<'a>(cx: *mut jsapi::JSContext, value: HandleValue) -> Result<JSString<'a, FromJS>, JSEvaluateErr> {
    if value.is_string() {
        Ok(JSString {
            js_string: JS_FlattenString(cx, value.to_string()),
            marker: PhantomData,
        })
    } else {
        Err(JSEvaluateErr::NotAString)
    }
}

/// The type of JS-managed data in a JS compartment `C`, with lifetime `'a` and class `K`.
///
/// If the user has access to a `JSManaged`, then the JS-managed
/// data is live for the given lifetime.
pub struct JSManaged<'a, C, K> {
    js_object: *mut Heap<*mut JSObject>,
    raw: *mut (),
    marker: PhantomData<(&'a(), C, K)>
}

impl<'a, C, K> Clone for JSManaged<'a, C, K> {
    fn clone(&self) -> Self {
        JSManaged {
            js_object: self.js_object,
            raw: self.raw,
            marker: PhantomData,
        }
    }
}

impl<'a, C, K> Copy for JSManaged<'a, C, K> {
}

impl<'a, C, K> Debug for JSManaged<'a, C, K> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        fmt.debug_struct("JSManaged")
            .field("js_object", &self.js_object)
            .field("raw", &self.raw)
            .finish()
    }
}

impl<'a, C, K> PartialEq for JSManaged<'a, C, K> {
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}

impl<'a, C, K> Eq for JSManaged<'a, C, K> {
}

unsafe impl<'a, C, K> JSTraceable for JSManaged<'a, C, K> where
    K: HasInstance<'a, C>,
    K::Instance: JSTraceable,
{
    unsafe fn trace(&self, trc: *mut JSTracer) {
        debug!("Tracing JSManaged {:p}.", self.js_object);
        CallObjectTracer(trc, self.js_object, GCTraceKindToAscii(TraceKind::Object));
    }
}

impl<'a, C, K> JSManaged<'a, C, K> {
    /// Read-only access to JS-managed data.
    pub fn get<'b, S>(self, _: &'b JSContext<S>) -> K::Instance where
        S: CanAccess,
        K: HasInstance<'b, C>,
        K::Instance: Copy,
        'a: 'b,
    {
        let result = unsafe { *(self.raw as *mut Option<K::Instance>) };
        result.unwrap()
    }

    pub fn borrow<'b, S>(self, _: &'b JSContext<S>) -> &'b K::Instance where
        S: CanAccess,
        K: HasInstance<'b, C>,
        'a: 'b,
    {
        let result = unsafe { &*(self.raw as *mut Option<K::Instance>) };
        result.as_ref().unwrap()
    }

    /// Read-write access to JS-managed data.
    pub fn borrow_mut<'b, S>(self, _: &'b mut JSContext<S>) -> &'b mut K::Instance where
        S: CanAccess + InCompartment<C>,
        K: HasInstance<'b, C>,
        'a: 'b,
    {
        let result = unsafe { &mut *(self.raw as *mut Option<K::Instance>) };
        result.as_mut().unwrap()
    }

    /// Change the lifetime of JS-managed data.
    pub unsafe fn change_lifetime<'b>(self) -> JSManaged<'b, C, K> {
        JSManaged {
            js_object: self.js_object,
            raw: self.raw,
            marker: PhantomData,
        }
    }

    /// It's safe to extend the lifetime of JS-managed data if it has been snapshotted.
    pub fn extend_lifetime<'b, 'c, S>(self, _: &'c JSContext<S>) -> JSManaged<'b, C, K> where
        S: CanExtend<'b>,
    {
        unsafe { self.change_lifetime() }
    }

    /// Forget about which compartment the managed data is in.
    /// This is safe because when we mutate data in compartment `C` we do so with a
    /// JS context in compartment `C`, which means it is never `SOMEWHERE`.
    pub fn forget_compartment(self) -> JSManaged<'a, SOMEWHERE, K> {
        JSManaged {
            js_object: self.js_object,
            raw: self.raw,
            marker: PhantomData,
        }
    }

    /// Visit the compartment of this object.
    pub fn visit_compartment<S, V>(self, cx: &'a mut JSContext<S>, visitor: V) -> V::Result where
        S: CanAccess + CanAlloc,
        V: VisitCompartment<'a, K>,
    {
        debug!("Visiting compartment.");
        let ac = JSAutoCompartment::new(cx.jsapi_context, self.to_jsobject());
        let ref mut cx = JSContext {
            jsapi_context: cx.jsapi_context,
            global_js_object: ptr::null_mut(),
            global_raw: ptr::null_mut(),
            auto_compartment: Some(ac),
            runtime: None,
            marker: PhantomData,
        };
        visitor.visit::<C, Visiting<C>>(cx, self)
    }

    pub fn to_jsobject(self) -> *mut JSObject {
        unsafe { &*self.js_object }.get()
    }

    pub fn to_jsval(self) -> JSVal {
        ObjectValue(self.to_jsobject())
    }
}

/// A wildcard compartment name.
pub struct SOMEWHERE(());

/// A trait for entering a compartment, and binding it to `C`.
pub trait VisitCompartment<'a, K> {
    type Result = ();
    fn visit<C, S>(self, cx: &mut JSContext<S>, managed: JSManaged<'a, C, K>) -> Self::Result where
        S: CanAccess + CanAlloc + InCompartment<C>;
}

/// A stack allocated root containing data of type `T` with lifetime `'a`.
pub struct JSRoot<'a, T: 'a> {
    value: Option<T>,
    pin: JSUntypedPinnedRoot,
    marker: PhantomData<&'a ()>, // NOTE: this is variant in `'a`.
}

/// A stack allocated root that has been pinned, so the backing store can't move.
pub struct JSPinnedRoot<'a, T: 'a> (&'a mut JSRoot<'a, T>);

/// A doubly linked list with all the pinned roots.
#[derive(Eq, PartialEq)]
pub struct JSPinnedRoots(*mut JSUntypedPinnedRoot);

impl JSPinnedRoots {
    unsafe fn insert(&mut self, root: &mut JSUntypedPinnedRoot) {
        debug!("Adding root {:p}.", root);
        root.next = self.0;
        root.prev = ptr::null_mut();
        if let Some(next) = root.next.as_mut() {
            next.prev = root;
        }
        self.0 = root;
    }

    unsafe fn remove(&mut self, root: &mut JSUntypedPinnedRoot) {
        if let Some(next) = root.next.as_mut() {
            debug!("Removing root.next.prev for {:p}.", root);
            next.prev = root.prev;
        }
        if let Some(prev) = root.prev.as_mut() {
            debug!("Removing root.prev.next for {:p}.", root);
            prev.next = root.next;
        } else if self.0 == root {
            debug!("Removing root {:p} from rootset.", root);
            self.0 = root.next;
        }
        root.value = mem::zeroed();
        root.next = ptr::null_mut();
        root.prev = ptr::null_mut();
    }
}

/// The thread-local list of all roots
thread_local! { static ROOTS: UnsafeCell<JSPinnedRoots> = UnsafeCell::new(JSPinnedRoots(ptr::null_mut())); }

unsafe fn thread_local_roots() -> *mut JSPinnedRoots {
    ROOTS.with(UnsafeCell::get)
}

unsafe extern "C" fn trace_thread_local_roots(trc: *mut JSTracer, _: *mut c_void) {
    debug!("Tracing roots.");

    if let Some(roots) = thread_local_roots().as_mut() {
        let mut curr = roots.0;
        while let Some(root) = curr.as_ref() {
            debug!("Tracing root {:p}.", root);
            (&*root.value).trace(trc);
            curr = root.next;
        }
    }

    debug!("Done tracing roots.");
}

/// A stack allocated root that has been pinned, but we don't have a type for the contents
struct JSUntypedPinnedRoot {
    value: *mut JSTraceable,
    next: *mut JSUntypedPinnedRoot,
    prev: *mut JSUntypedPinnedRoot,
}

impl Drop for JSUntypedPinnedRoot {
    fn drop(&mut self) {
        unsafe { (&mut *thread_local_roots()).remove(self); }
    }
}

/// Data which can be rooted.

pub unsafe trait JSRootable<'a> {
    type Aged;

    unsafe fn change_lifetime(self) -> Self::Aged where Self: Sized {
        let result = mem::transmute_copy(&self);
        mem::forget(self);
        result
    }

    fn contract_lifetime(self) -> Self::Aged where Self: 'a + Sized {
        unsafe { self.change_lifetime() }
    }

    fn in_root(self, root: &'a mut JSRoot<'a, Self::Aged>) -> Self::Aged where
        Self: Sized,
        Self::Aged: Copy + JSTraceable,
    {
        root.set(self)
    }
}

unsafe impl<'a, 'b, C, K> JSRootable<'a> for JSManaged<'b, C, K>
{
    type Aged = JSManaged<'a, C, K>;
}

impl<'a, T> JSRoot<'a, T> {
    // This uses Sgeo's trick to stop the JSRoot being forgotten by `mem::forget`.
    // The pin takes a `&'a mut JSRoot<'a, T>`, so borrows the root for the
    // duration of `'a`, so the type is no longer valid after the pin is dropped.
    pub fn pin<U>(&'a mut self, value: U) -> &'a T where
        T: JSTraceable,
        U: JSRootable<'a, Aged=T>,
    {
        let roots = unsafe { &mut *thread_local_roots() };
        self.value = Some(unsafe { value.change_lifetime() });
        self.pin.value = self.value.as_mut_ptr();
        unsafe { roots.insert(&mut self.pin) };
        self.value.as_ref().unwrap()
    }

    pub fn set<U>(&'a mut self, value: U) -> T where
        T: Copy + JSTraceable,
        U: JSRootable<'a, Aged=T>,
    {
        *self.pin(value)
    }

    pub unsafe fn unpin(&mut self) {
        let roots = &mut *thread_local_roots();
        roots.remove(&mut self.pin);
    }
}

impl<'a, T> Deref for JSPinnedRoot<'a, T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.0.value.as_ref().unwrap()
    }
}

impl<'a, T> DerefMut for JSPinnedRoot<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        self.0.value.as_mut().unwrap()
    }
}

unsafe impl<#[may_dangle] 'a, #[may_dangle] T> Drop for JSRoot<'a, T> {
    fn drop(&mut self) {
        unsafe { self.unpin() }
    }
}

#[macro_export]
macro_rules! rooted {
    (in($cx:expr) let $name:ident = $init:expr) => (
        let mut __root = $cx.new_root();
        let $name = __root.set($init);
    );
    (in($cx:expr) let mut $name:ident = $init:expr) => (
        let mut __root = $cx.new_root();
        let mut $name = __root.set($init);
    );
    (in($cx:expr) let ref $name:ident = $init:expr) => (
        let mut __root = $cx.new_root();
        let $name = __root.pin($init);
    )
}
