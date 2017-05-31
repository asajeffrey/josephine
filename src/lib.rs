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

use std::marker::PhantomData;

/// A marker trait for accessing JS-managed data in compartment `C`.
pub unsafe trait JSAccessToken<C>: Sized {}

/// The type for JS contexts whose current compartment is `C`.
pub struct JSContext<C> {
    snapshot: JSSnapshot<C>,
}

unsafe impl<C> JSAccessToken<C> for JSContext<C> {}

impl<C> JSContext<C> {
    /// Get a snapshot of the JS state.
    /// The snapshot only allows access to the methods that are guaranteed not to call GC,
    /// so we don't need to root JS-managed pointers during the lifetime of a snapshot.
    pub fn snapshot<'a>(&'a mut self) -> &'a mut JSSnapshot<C> {
        &mut self.snapshot
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

    // A real implementation would also have JS methods such as those in jsapi.
}

/// A placholder for the real `JSTraceable`.
pub unsafe trait JSTraceable<C> {}

/// Change the JS-managed lifetime of a type.
pub unsafe trait JSAgeable<'a> {
    /// This type should have the same memory represention as `Self`.
    /// The only difference between `Self` and `Self::Aged`
    /// is that any `JSManaged<'b, C, T>` should be replaced by
    /// `JSManaged<'a, C, T::Aged>`.
    type Aged: 'a + JSAgeable<'a, Aged=Self::Aged>;
}

/// The trait for native JS-manageable data.
pub trait JSManageable<'a, C>: JSTraceable<C> + JSAgeable<'a> {}

/// A user of a JS runtime implements `JSRunnable`.
pub trait JSRunnable: Sized {
    /// This callback is called with a fresh JS compartment type `C`.
    fn run<'a, C>(self, rt: &'a mut JSRuntime<C>);

    /// To trigger the callback, call `rt.start()`.
    fn start(self) {
        let mut rt = JSRuntime {
            cx: JSContext {
                snapshot: JSSnapshot {
                    marker: PhantomData,
                }
            }
        };
        self.run::<'static, ()>(&mut rt);
    }
}

pub struct JSRuntime<C> {
    // The real thing would have a JS runtime
    cx: JSContext<C>,
}

impl<C> JSRuntime<C> {
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

unsafe impl<'a, C, T: ?Sized> JSTraceable<C> for JSManaged<'a, C, T> where
    T: JSTraceable<C>
{
}

unsafe impl<'a, 'b, C, T: ?Sized> JSAgeable<'b> for JSManaged<'a, C, T> where
    C: 'b,
    T: JSAgeable<'b>,
{
    type Aged = JSManaged<'b, C, T::Aged>;
}

impl<'a, 'b, C, T: ?Sized> JSManageable<'b, C> for JSManaged<'a, C, T> where
    C: 'b,
    T: JSManageable<'b, C>,
{}

impl<'a, C, T: ?Sized> JSManaged<'a, C, T> {
    /// Read-only access to JS-managed data.
    pub fn get<'b, A: JSAccessToken<C>>(self, _: &'b A) -> &'b T::Aged where
        T: JSAgeable<'b>,
        'a: 'b,
    {
        unsafe { &*self.contract_lifetime().raw }
    }

    /// Read-write access to JS-managed data.
    pub fn get_mut<'b, A: JSAccessToken<C>>(self, _: &'b mut A) -> &'b mut T::Aged where
        T: JSAgeable<'b>,
        'a: 'b,
    {
        unsafe { &mut *self.contract_lifetime().raw }
    }

    /// Change the lifetime of JS-managed data.
    pub unsafe fn change_lifetime<'b>(self) -> JSManaged<'b, C, T::Aged> where
        T: JSAgeable<'b>,
    {
        JSManaged {
            raw: self.raw as *mut T::Aged,
            marker: PhantomData,
        }
    }

    /// It's safe to contract the lifetime of JS-managed data.
    pub fn contract_lifetime<'b>(self) -> JSManaged<'b, C, T::Aged> where
        T: JSAgeable<'b>,
        'a: 'b,
    {
        unsafe { self.change_lifetime() }
    }

    /// It's safe to extend the lifetime of JS-managed data if it has been snapshotted.
    pub fn extend_lifetime<'b>(self, _: &'b JSSnapshot<C>) -> JSManaged<'b, C, T::Aged> where
        T: JSAgeable<'b>,
        'b: 'a,
    {
        unsafe { self.change_lifetime() }
    }

    /// It's safe to extend the lifetime of JS-managed data by rooting it.
    pub fn root<'b>(self, _: &'b JSRoots<C>) -> JSManaged<'b, C, T::Aged> where
        T: JSAgeable<'b>,
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
pub struct JSSnapshot<C> {
    // The real thing would have a JS context.
    marker: PhantomData<C>,
}

unsafe impl<C> JSAccessToken<C> for JSSnapshot<C> {}

#[test]
// This test constructs a two-node cyclic graph, which is the smallest
// example of something that uses `RefCell`s in servo's JS bindings.
fn test() {
    // A graph type
    type Graph<'a, C> = JSManaged<'a, C, NativeGraph<'a, C>>;
    struct NativeGraph<'a, C> {
        nodes: Vec<Node<'a, C>>,
    }
    unsafe impl<'a, C> JSTraceable<C> for NativeGraph<'a, C> {}
    unsafe impl<'a, 'b, C: 'b> JSAgeable<'b> for NativeGraph<'a, C> { type Aged = NativeGraph<'b, C>; }
    impl<'a, 'b, C: 'b> JSManageable<'b, C> for NativeGraph<'a, C> {}
    // A node type
    type Node<'a, C> = JSManaged<'a, C, NativeNode<'a, C>>;
    struct NativeNode<'a, C> {
        data: usize,
        edges: Vec<Node<'a, C>>,
    }
    unsafe impl<'a, C> JSTraceable<C> for NativeNode<'a, C> {}
    unsafe impl<'a, 'b, C: 'b> JSAgeable<'b> for NativeNode<'a, C> { type Aged = NativeNode<'b, C>; }
    impl<'a, 'b, C: 'b> JSManageable<'b, C> for NativeNode<'a, C> {}
    // Build a cyclic graph
    struct Test;
    impl JSRunnable for Test {
        fn run<C: 'static>(self, rt: &mut JSRuntime<C>) {
            let (cx, graph) = rt.manage(NativeGraph { nodes: vec![] });
            self.add_nodes(cx, graph);
            assert_eq!(graph.get(cx).nodes[0].get(cx).data, 1);
            assert_eq!(graph.get(cx).nodes[1].get(cx).data, 2);
            self.add_edges(cx.snapshot(), graph);
            assert_eq!(graph.get(cx).nodes[0].get(cx).edges[0].get(cx).data, 2);
            assert_eq!(graph.get(cx).nodes[1].get(cx).edges[0].get(cx).data, 1);
        }
    }
    impl Test {
        fn add_nodes<C: 'static>(&self, cx: &mut JSContext<C>, graph: Graph<C>) {
            // Creating nodes does memory allocation, which may trigger GC,
            // so the nodes need to be rooted while they are being added.
            let roots = cx.roots();
            let node1 = cx.manage(NativeNode { data: 1, edges: vec![] }).root(&roots);
            let node2 = cx.manage(NativeNode { data: 2, edges: vec![] }).root(&roots);
            graph.get_mut(cx).nodes.push(node1.contract_lifetime());
            graph.get_mut(cx).nodes.push(node2.contract_lifetime());
        }
        fn add_edges<C>(&self, cx: &mut JSSnapshot<C>, graph: Graph<C>) {
            // Note that there's no rooting here.
            let node1 = graph.get(cx).nodes[0].extend_lifetime(cx);
            let node2 = graph.get(cx).nodes[1].extend_lifetime(cx);
            node1.get_mut(cx).edges.push(node2.contract_lifetime());
            node2.get_mut(cx).edges.push(node1.contract_lifetime());
        }
    }
    #[allow(dead_code)]
    // Test that we can contract the lifetimes of nodes and graphs.
    fn contract_graph<'a, 'b:'a, C: 'b>(graph: Graph<'b, C>) -> Graph<'a, C> {
        graph.contract_lifetime()
    }
    Test.start();
}

#[test]
fn test_covariant() {
    #[allow(dead_code)]
    fn cast<'a, 'b:'a, C, T>(managed: JSManaged<'b, C, T>)
                              -> JSManaged<'a, C, T>
    {
        managed
    }
}
