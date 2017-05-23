//! An outline of how linear types could be combined with JS-managed data
//!
//! The idea is to use the JS context as a linear token.
//! Each JS context gets its own type `Cx` which implements the `JSContext` trait.
//! Access to read-only JS-managed data requires a context of type `&Cx`.
//! Access to read-write JS-managed data requires a context of type `&mut Cx`.
//! Linear use of the context ensures that Rust's memory safety is preserved (er, I hope).
//!
//! This API doesn't address rooting, tracing, or using JS destructors
//! to reclaim Rust memory, which would have to be handled similarly
//! to servo's current JS bindings. One thing that might make this easier is
//! if we can arrange for any calls into the JS engine which might trigger GC
//! to take a `&mut Cx` argument, which would give us control of when GC might
//! occur.

use std::marker::PhantomData;

/// The trait for JS contexts.
pub trait JSContext: Sized {
    /// Read-only access to JS-managed data.
    fn get<T>(&self, managed: JSManaged<Self, T>) -> &T;
    
    /// Read-write access to JS-managed data.
    fn get_mut<T>(&mut self, managed: JSManaged<Self, T>) -> &mut T;

    /// Give ownership of data to JS.
    fn manage<T>(&mut self, value: T) -> JSManaged<Self, T>;

    // A real implementation would also have JS methods such as those in jsapi.
}

/// A user of a JS context implements `JSContextConsumer`, which is called back
/// with a fresh JS context.
pub trait JSContextConsumer<T> {
    /// This callback is called with a fresh JS context.
    fn consume<Cx>(self, cx: Cx) -> T where Cx: JSContext;
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
    consumer.consume(JSContextImpl{})
}

/// The type of JS-managed data in a JS context `Cx`.
pub struct JSManaged<Cx, T> {
    // JS reflector goes here
    raw: *mut T,
    marker: PhantomData<Cx>,
}

impl<Cx, T> Clone for JSManaged<Cx, T> {
    fn clone(&self) -> Self {
        JSManaged {
            raw: self.raw,
            marker: self.marker,
        }
    }
}

impl<Cx, T> Copy for JSManaged<Cx, T> {
}

// It is important for safety that this implemention is not made public!
struct JSContextImpl {
    // JS context implementation goes here
}

impl JSContext for JSContextImpl {
    // The claim is that this is safe, since it takes a `&self`.
    fn get<T>(&self, managed: JSManaged<Self, T>) -> &T {
        unsafe { &*managed.raw }
    }

    // The claim is that this is safe, since it takes a `&mut self`.
    fn get_mut<T>(&mut self, managed: JSManaged<Self, T>) -> &mut T {
        unsafe { &mut *managed.raw }
    }

    // This outline implementation just space-leaks all data,
    // the real thing would need a finalizer hook,
    // and use rooting and tracing to make sure that any Rust-reachable
    // data is not GCd.
    fn manage<T>(&mut self, value: T) -> JSManaged<Self, T> {
        let boxed = Box::new(value);
        JSManaged {
            raw: Box::into_raw(boxed),
            marker: PhantomData,
        }
    }
}

#[test]
// This test constructs a two-node cyclic graph, which is the smallest
// example of something that uses `RefCell`s in servo's JS bindings.
fn test() {
    // A graph type
    struct Node<Cx> {
        data: usize,
        edges: Vec<JSManaged<Cx, Node<Cx>>>,
    }
    struct TestConsumer;
    impl JSContextConsumer<()> for TestConsumer {
        fn consume<Cx>(self, mut cx: Cx) where Cx: JSContext {
            // Build a two-node cycle
            let node1 = cx.manage(Node { data: 1, edges: vec![] });
            let node2 = cx.manage(Node { data: 2, edges: vec![node1] });
            cx.get_mut(node1).edges.push(node2);
            assert_eq!(cx.get(node1).data, 1);
            assert_eq!(cx.get(cx.get(node1).edges[0]).data, 2);
        }
    }
    with_js_context(TestConsumer);
}
