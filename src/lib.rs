//! A library which uses JavaScript to safely manage the lifetimes of Rust data.
//!
//! (
//! [Repo](https://github.com/asajeffrey/josephine) |
//! [CI](https://travis-ci.org/asajeffrey/josephine)
//! )
//!
//! This library allows Rust data to be attached to JavaScript objects:
//! the lifetime of the Rust data is then the same as the JS object it is attached to.
//! Since JS is garbage collected, it is safe to copy and discard references to
//! JS managed data, and allows examples like doubly-linked lists which would
//! otherwise require reference counting. Reference counting requires dynamic checks,
//! for example getting mutable access to reference-counted data panics if the reference
//! count is more than 1.
//!
//! The goals are:
//!
//! 1. Use JS to manage the lifetime of Rust data.
//! 2. Allow references to JS managed data to be freely copied and discarded, relying on
//!    the garbage collector for safety.
//! 3. Maintain Rust memory safety (for example no mutable aliasing),
//!    without requiring additional static analysis such as a lint.
//! 4. Allow mutable and immutable access to Rust data via JS managed references, so
//!    we do not need to rely on interior mutability.
//! 5. Provide a rooting API to ensure that JS managed data is not garbage collected
//!    while it is being used.
//!
//! To support safe access to JS managed data, the API uses a *JS context*, which
//! is used as an access token to allow JS managed data to be accessed, allocated
//! or deallocated. Mutable access to JS managed data requires mutable access to the
//! JS context, which is how the API achieves memory safety even though JS managed
//! references can be copied and discarded freely.
//!
//! JS managed memory is split into *compartments*, which are
//! separately garbage collected, so the garbage collector can avoid
//! scanning the entire heap. The API statically tracks compartments, to
//! ensure that there are no direct references between compartments.
//!
//! The API is implemented as bindings to the SpiderMonkey JS engine,
//! from which it borrows the garbage collector and the notions of compartment
//! and JS context. The API allows calling into JavaScript
//! from Rust, and calling back into Rust from JavaScript. These bindings are unsafe,
//! and are intended for use by a trusted bindings generator.
//!
//! # JS-managed data
//!
//! Rust data can be given to JS to manage, and then accessed,
//! using the JS context. The JS context is passed as a variable of type `JSContext<S>`,
//! where the type parameter `S` is used to track the state of the context.
//! The context comes with the permissions it grants, such as `CanAlloc`
//! and `CanAccess`. These permissions are modelled as traits, for example
//! a context in state `S` can allocate memory when `S: CanAlloc`.
//!
//! JS managed memory is split into compartments. Each JS context has a notion of
//! the current compartment, which is part of the state. A JS context in compartment
//! `C` has type `JSCompartment<S>` where `S: InCompartment<C>` and `C: Compartment`.
//! A reference to JS managed data in compartment `C`, where the Rust data being
//! managed by JS has type `T`, is given type `JSManaged<C, T>`.
//!
//! For example, we can give JS a Rust string to manage in compartment
//! `C`:
//!
//! ```rust
//! # use josephine::*;
//! fn example<C, S>(cx: &mut JSContext<S>) where
//!     S: CanAlloc + InCompartment<C>,
//!     C: Compartment,
//! {
//!     let x: JSManaged<C, String> = cx.manage(String::from("hello"));
//! }
//! ```
//!
//! and then access it:
//!
//! ```rust
//! # use josephine::*;
//! fn example<C, S>(cx: &mut JSContext<S>, x: JSManaged<C, String>) where
//!     S: CanAccess,
//!     C: Compartment,
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
//! # use josephine::*;
//! fn unsafe_example<C, S>(cx: &mut JSContext<S>) where
//!     S: CanAlloc + CanAccess + InCompartment<C>,
//!     C: Compartment,
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
//!   --> src/lib.rs:10:35
//!    |
//! 8  |     let x: JSManaged<C, String> = cx.manage(String::from("hello"));
//!    |                                   -- mutable borrow occurs here
//! 9  |     // Imagine something triggers GC here
//! 10 |     println!("{} world", x.borrow(cx));
//!    |                                   ^^ immutable borrow occurs here
//! 11 | }
//!    | - mutable borrow ends here
//! ```
//!
//! To see why this example fails to typecheck, we can introduce explicit lifetimes:
//!
//! ```rust,ignore
//! # use josephine::*;
//! fn unsafe_example<'a, C, S>(cx: &'a mut JSContext<S>) where
//!     S: CanAlloc + CanAccess + InCompartment<C>,
//!     C: Compartment,
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
//! # use josephine::*;
//! fn example<C, S>(cx: &mut JSContext<S>) where
//!     S: CanAlloc + CanAccess + InCompartment<C>,
//!     C: Compartment,
//! {
//!     // Declare a root which will be used to keep x alive during its lifetime
//!     let ref mut root = cx.new_root();
//!     // Store a reference to x in the root
//!     let x = cx.manage(String::from("hello")).in_root(root);
//!     // This is what is keeping x alive ------^
//!     // Imagine something triggers GC here
//!     // The root ensures that x survives GC, so is safe to use
//!     println!("{} world", x.borrow(cx));
//! }
//! ```
//!
//! To see why this example now typechecks, we again introduce explicit lifetimes:
//!
//! ```rust
//! # use josephine::*;
//! fn example<'a, C, S>(cx: &'a mut JSContext<S>) where
//!     S: CanAlloc + CanAccess + InCompartment<C>,
//!     C: Compartment,
//! {
//!     // root has type JSRoot<'b, String>
//!     let ref mut root = cx.new_root();
//!     // x has type JSManaged<'b, C, String>
//!     let x = {
//!         // x_unrooted has type JSManaged<'d, C, String>
//!         let x_unrooted = cx.manage(String::from("hello"));
//!         // By rooting it, its lifetime changes from 'd to 'b (the lifetime of the root)
//!         x_unrooted.in_root(root)
//!     };
//!     // Imagine something triggers GC here
//!     // x_ref has type &'c String
//!     let x_ref = x.borrow(cx);
//!     println!("{} world", x_ref);
//! }
//! ```
//!
//! The example typechecks because the 
//! constraints are that `'b` overlaps with `'c` and `'d`, and that
//! `'c` and `'d` don't overlap. These constraints are satisfiable, so the
//! example typechecks.
//!
//! # Mutating JS-managed data
//!
//! JS managed data can be accessed mutably as well as immutably.
//! This is safe because mutably accessing JS manage data requires
//! mutably borrowing the JS context, so there cannot be two simultaneous
//! mutable accesses.
//!
//! ```rust
//! # use josephine::*;
//! fn example<C, S>(cx: &mut JSContext<S>, x: JSManaged<C, String>) where
//!     S: CanAccess,
//!     C: Compartment,
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
//! # use josephine::*; use std::mem;
//! fn unsafe_example<C, S>(cx: &mut JSContext<S>, x: JSManaged<C, String>, y: JSManaged<C, String>) where
//!     S: CanAccess,
//!     C: Compartment,
//! {
//!     mem::swap(x.borrow_mut(cx), y.borrow_mut(cx));
//! }
//! ```
//!
//! ```text
//! 	error[E0499]: cannot borrow `*cx` as mutable more than once at a time
//!  --> src/lib.rs:8:46
//!   |
//! 8 |     mem::swap(x.borrow_mut(cx), y.borrow_mut(cx));
//!   |                            --                ^^ - first borrow ends here
//!   |                            |                 |
//!   |                            |                 second mutable borrow occurs here
//!   |                            first mutable borrow occurs here
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
//! # use josephine::*;
//! fn example<C, S>(cx: &mut JSContext<S>) where
//!     S: CanAlloc + CanAccess + InCompartment<C>,
//!     C: Compartment,
//! {
//!     let (ref cx, x) = cx.snapshot_manage(String::from("hello"));
//!     // Since the context is snapshotted it can't trigger GC
//!     println!("{} world", x.borrow(cx));
//! }
//! ```
//!
//! A program which tries to use a function which might trigger GC will
//! not typecheck, as the snapshotted JS context state does not support
//! the appropriate traits. For example:
//!
//! ```rust,ignore
//! # use josephine::*;
//! fn unsafe_example<C, S>(cx: &mut JSContext<S>) where
//!     S: CanAlloc + CanAccess + InCompartment<C>,
//!     C: Compartment,
//! {
//!     let (ref mut cx, x) = cx.snapshot_manage(String::from("hello"));
//!     cx.gc();
//!     println!("{} world", x.borrow(cx));
//! }
//! ```
//!
//! In this program, the call to `cx.gc()` requires the state
//! to support `CanAlloc<C>`, which is not allowed by the snapshotted state.
//!
//! ```text
//! 	error[E0277]: the trait bound `josephine::Snapshotted<'_, S>: josephine::CanAlloc` is not satisfied
//!  --> src/lib.rs:9:8
//!   |
//! 9 |     cx.gc();
//!   |        ^^ the trait `josephine::CanAlloc` is not implemented for `josephine::Snapshotted<'_, S>`
//! ```
//!
//! # Working with compartments
//!
//! To enter the compartment of a JS managed object, you can use `cx.enter_known_compartment(managed)`.
//! This returns a context whose current compartment is that of the JS managed objecct.
//!
//! ```rust
//! # use josephine::*;
//! fn example<C, S>(cx: &mut JSContext<S>, x: JSManaged<C, String>) where
//!     S: CanAccess + CanAlloc,
//!     C: Compartment,
//! {
//!     // We can't allocate data without entering the comartment.
//!     // Commenting out the next line gives an error
//!     // the trait `josephine::InCompartment<_>` is not implemented for `S`.
//!     let ref mut cx = cx.enter_known_compartment(x);
//!     let ref mut root = cx.new_root();
//!     let y = cx.manage(String::from("world")).in_root(root);
//!     println!("Hello, {}.", y.borrow(cx));
//! }
//! ```
//!
//! Working with named compartmens is fine when there is a fixed number of them, but not when
//! the number of compartments is unbounded. For example, the type `Vec<JSManaged<C, T>>` contains
//! a vector of managed objects, all in the same compartment, but sometimes you need a vector of
//! objects in different compartments. This is what *wildcards* are for (borrowed from
//! [Java wildcards](https://docs.oracle.com/javase/tutorial/java/generics/wildcards.html)
//! which solve a similar problem).
//!
//! The wildcard compartment is called `SOMEWHERE`, and `JSManaged<SOMEWHERE, T>` refers
//! to JS managed data whose compartment is unknown. For example `Vec<JSManaged<SOMEWHERE, T>>`
//! contains a vector of managed objects, which may all be in different compartments.
//!
//! To create a `JSManaged<SOMEWHERE, T>`, we use `managed.forget_compartment()`.
//!
//! ```rust
//! # use josephine::*;
//! fn example<C, S>(cx: &mut JSContext<S>) -> JSManaged<SOMEWHERE, String> where
//!     S: CanAlloc + InCompartment<C>,
//!     C: Compartment,
//! {
//!     cx.manage(String::from("hello")).forget_compartment()
//! }
//! ```
//!
//! To access data with a wildcard compartment, first enter the compartment
//! using `cx.enter_unknown_compartment(managed)`.
//!
//! ```rust
//! # use josephine::*;
//! fn example<S>(cx: &mut JSContext<S>, x: JSManaged<SOMEWHERE, String>) where
//!     S: CanAccess,
//! {
//!     // We can't access x without first entering its compartment.
//!     // Commenting out the next two lines gives an error
//!     // the trait `josephine::Compartment` is not implemented for `josephine::SOMEWHERE`.
//!     let ref mut cx = cx.enter_unknown_compartment(x);
//!     let x = cx.entered();
//!     println!("Hello, {}.", x.borrow(cx));
//! }
//! ```
//!
//! Sometimes you need to check to see if some JS managed data is in the current compartment or not.
//! This is done with `managed.in_compartment(cx)`, which returns an `Option<JSManaged<C, T>>`
//! when the context's current compartment is `C`. The result is `Some(managed)` if `managed` was in
//! compartment `C`, and `None` if it was in a different compartment.
//!
//! ```rust
//! # use josephine::*;
//! fn example<S, C>(cx: &mut JSContext<S>, x: JSManaged<SOMEWHERE, String>) where
//!     S: CanAccess + InCompartment<C>,
//!     C: Compartment,
//! {
//!     if let Some(x) = x.in_compartment(cx) {
//!         println!("Hello, {}.", x.borrow(cx));
//!     }
//! }
//! ```
//!
//! # User-defined types
//!
//! There are more types to manage than just `String`!
//!
//! To be managed by `JSManageable`, a type should implement the following traits:
//!
//! * `JSTraceable`: values of the type can be *traced*, that is can tell the garbage
//!    collector which objects are reachable from them.
//! * `JSRootable`: values of the type have their lifetime managed by JS.
//! * `JSTransplantable`: values of the type have their compartment managed by JS.
//! * `JSInitializable`: this type knows how to initialize a JS object which is used
//!    to manage its lifetime.
//!
//! Each of these traits can be derived. The fields of a `JSTraceable` type should be
//! `JSTraceable`, and similarly for `JSRootable` and `JSTransplantable`. This requirement
//! is *not* true for `JSInitializable`.
//!
//! A typical use is to define two types: the *native* type `NativeThing`
//! and then the *managed* type `Thing<'a, C>` which is just a `JSManaged<'a, C, NativeThing>`.
//!
//! ```rust
//! # extern crate josephine;
//! # #[macro_use] extern crate josephine_derive;
//! # use josephine::*;
//! #[derive(JSInitializable, JSTraceable, JSRootable, JSTransplantable)]
//! struct NativeName { name: String }
//!
//! #[derive(Clone, Copy, Debug, Eq, PartialEq, JSInitializable, JSTraceable, JSRootable, JSTransplantable)]
//! pub struct Name<'a, C> (JSManaged<'a, C, NativeName>);
//!
//! impl<'a, C:'a> Name<'a, C> {
//!     pub fn new<S>(cx: &'a mut JSContext<S>, name: &str) -> Name<'a, C> where
//!         S: CanAlloc + InCompartment<C>,
//!         C: Compartment,
//!     {
//!         Name(cx.manage(NativeName { name: String::from(name) }))
//!     }
//!     pub fn name<S>(self, cx: &'a JSContext<S>) -> &'a str where
//!         S: CanAccess,
//!         C: Compartment,
//!     {
//!         &*self.0.borrow(cx).name
//!     }
//! }
//!
//! # fn main() {
//! # let ref mut cx = JSContext::new();
//! # let ref mut cx = cx.create_compartment().global_manage(37);
//! let ref mut root = cx.new_root();
//! let hello = Name::new(cx, "hello").in_root(root);
//! assert_eq!(hello.name(cx), "hello");
//! # }
//! ```
//!
//! Sometimes the native type will itself contain references to JS-managed data,
//! so will need to be parameterized on a lifetime and compartment.
//!
//! ```rust
//! # extern crate josephine;
//! # #[macro_use] extern crate josephine_derive;
//! # use josephine::*;
//! # #[derive(JSInitializable, JSTraceable, JSRootable, JSTransplantable)]
//! # struct NativeName { name: String }
//! #
//! # #[derive(Clone, Copy, Debug, Eq, PartialEq, JSInitializable, JSTraceable, JSRootable, JSTransplantable)]
//! # pub struct Name<'a, C> (JSManaged<'a, C, NativeName>);
//! #
//! # impl<'a, C> Name<'a, C> {
//! #     pub fn new<S>(cx: &'a mut JSContext<S>, name: &str) -> Name<'a, C> where
//! #         S: CanAlloc + InCompartment<C>,
//! #         C: Compartment,
//! #     {
//! #         Name(cx.manage(NativeName { name: String::from(name) }))
//! #     }
//! #     pub fn name<S>(self, cx: &'a JSContext<S>) -> &'a str where
//! #         S: CanAccess,
//! #         C: Compartment,
//! #     {
//! #         &*self.0.borrow(cx).name
//! #     }
//! # }
//! #
//! #[derive(JSInitializable, JSTraceable, JSRootable, JSTransplantable)]
//! struct NativeNames<'a, C> { names: Vec<Name<'a, C>> }
//!
//! #[derive(Clone, Copy, Debug, JSInitializable, JSTraceable, JSRootable, JSTransplantable)]
//! pub struct Names<'a, C> (JSManaged<'a, C, NativeNames<'a, C>>);
//! impl<'a, C:'a> Names<'a, C> {
//!     pub fn new<S>(cx: &'a mut JSContext<S>) -> Names<'a, C> where
//!         S: CanAlloc + InCompartment<C>,
//!         C: Compartment,
//!     {
//!         Names(cx.manage(NativeNames { names: vec![] }))
//!     }
//!     pub fn push_str<S>(self, cx: &'a mut JSContext<S>, name: &str) where
//!         S: CanAccess + CanAlloc + InCompartment<C>,
//!         C: Compartment,
//!     {
//!         let ref mut root = cx.new_root();
//!         let name = Name::new(cx, name).in_root(root);
//!         self.0.borrow_mut(cx).names.push(name);
//!     }
//!     pub fn get<S>(self, cx: &'a JSContext<S>, index: usize) -> Option<Name<'a, C>> where
//!         S: CanAccess,
//!         C: Compartment,
//!     {
//!         self.0.borrow(cx).names.get(index).cloned()
//!     }
//! }
//!
//! # fn main() {
//! # let ref mut cx = JSContext::new();
//! # let ref mut cx = cx.create_compartment().global_manage(37);
//! let ref mut root = cx.new_root();
//! let hello_world = Names::new(cx).in_root(root);
//! hello_world.push_str(cx, "hello");
//! hello_world.push_str(cx, "world");
//! assert_eq!(hello_world.get(cx, 0).map(|name| name.name(cx)), Some("hello"));
//! assert_eq!(hello_world.get(cx, 1).map(|name| name.name(cx)), Some("world"));
//! # }
//! ```
//!
//! # Calling between JS and Rust
//!
//! Josephine exposes an unsafe API to allow JS to call Rust and back again.
//! This is just a thin wrapper round the SpiderMonkey API.
//! See the [`ffi`](ffi/index.html) module for details.
//!
//! # Globals
//!
//! Each JS compartment has a special object called a *global*.
//! The compartment can be created using `cx.create_compartment()`,
//! and the global can be given native data to manage with `cx.global_manage(data)`.
//! The global can be accessed with `cx.global()`.
//!
//! ```rust
//! # extern crate josephine;
//! # #[macro_use] extern crate josephine_derive;
//! # use josephine::*;
//! #[derive(JSInitializable, JSTraceable, JSRootable, JSTransplantable)]
//! pub struct NativeMyGlobal { name: String }
//! type MyGlobal<'a, C> = JSManaged<'a, C, NativeMyGlobal>;
//!
//! fn example<'a, S>(cx: &'a mut JSContext<S>) -> MyGlobal<'a, SOMEWHERE> where
//!    S: CanAccess + CanAlloc,
//! {
//!    let cx = cx.create_compartment();
//!    let name = String::from("Alice");
//!    let cx = cx.global_manage(NativeMyGlobal { name: name });
//!    cx.global().forget_compartment()
//! }
//! # fn main() {}
//! ```
//!
//! In some cases, the global contains some JS-managed data, which is why the initialization
//! is split into two steps: creating the compartment, and
//! providing the JS-managed data for the global, for example:
//!
//! ```rust
//! # extern crate josephine;
//! # #[macro_use] extern crate josephine_derive;
//! # use josephine::*;
//! #[derive(JSInitializable, JSTraceable, JSRootable, JSTransplantable)]
//! pub struct NativeMyGlobal<'a, C> { name: JSManaged<'a, C, String> }
//! type MyGlobal<'a, C> = JSManaged<'a, C, NativeMyGlobal<'a, C>>;
//!
//! fn example<'a, S>(cx: &'a mut JSContext<S>) -> MyGlobal<'a, SOMEWHERE> where
//!    S: CanAccess + CanAlloc,
//! {
//!    let mut cx = cx.create_compartment();
//!    let ref mut root = cx.new_root();
//!    let name = cx.manage(String::from("Alice")).in_root(root);
//!    let ref cx = cx.global_manage(NativeMyGlobal { name: name });
//!    cx.global().forget_compartment()
//! }
//! # fn main() {}
//! ```
//!
//! # Bootstrapping
//!
//! The JSContext is built using `JSContext::new`.
//!
//! ```rust
//! # extern crate josephine;
//! # #[macro_use] extern crate josephine_derive;
//! # use josephine::*;
//! #[derive(JSInitializable, JSTraceable, JSRootable, JSTransplantable)]
//! pub struct NativeMyGlobal { name: String }
//! type MyGlobal<'a, C> = JSManaged<'a, C, NativeMyGlobal>;
//!
//! fn example<'a, S>(cx: &'a mut JSContext<S>) -> MyGlobal<'a, SOMEWHERE> where
//!    S: CanAccess + CanAlloc,
//! {
//!    let cx = cx.create_compartment();
//!    let name = String::from("Alice");
//!    let cx = cx.global_manage(NativeMyGlobal { name: name });
//!    cx.global().forget_compartment()
//! }
//! fn main() {
//!    let ref mut cx = JSContext::new();
//!    example(cx);
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

pub mod context;
pub use context::JSContext;
pub use context::CanAccess;
pub use context::CanAlloc;
pub use context::InCompartment;
pub use context::Initialized;
pub use context::IsInitialized;
pub use context::IsInitializing;
pub use context::IsSnapshot;

pub mod compartment;
pub use compartment::SOMEWHERE;
pub use compartment::Compartment;
pub use compartment::JSTransplantable;

pub mod runtime;

pub mod ffi;
pub use ffi::JSInitializable;

pub mod managed;
pub use managed::JSManaged;

pub mod root;
pub use root::JSRoot;
pub use root::JSRootable;

pub mod string;
pub use string::JSString;

pub mod trace;
pub use trace::JSTraceable;
