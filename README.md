# josephine: using JavaScript to safely manage the lifetimes of Rust data

[![Build Status](https://travis-ci.org/asajeffrey/josephine.svg)](https://travis-ci.org/asajeffrey/josephine)

This library allows Rust data to be attached to JavaScript objects:
the lifetime of the Rust data is then the same as the JS object it is attached to.
Since JS is garbage collected, it is safe to copy and discard references to
JS managed data, using an API similar to the `Rc` API, but which does not need
dynamic checks since those are handled by the garbage collector. This makes it possible
to implement structures like doubly-linked lists without dynamic checking.

The goals are:

1. Use JS to manage the lifetime of Rust data.
2. Allow references JS managed data to be freely copied and discarded, relying on
   the garbage collector for safety.
3. Maintain Rust memory safety (for example no mutable aliasing),
   without requiring additional static analysis such as a lint.
4. Allow mutable and immutable access to Rust data via JS managed references, so
   we do not need to rely on interior mutability.
5. Provide a rooting API to ensure that JS managed data is not garbage collected
   while it is being used.

To support safe access to JS managed data, the API uses a *JS context*, which
is used as an access token to allow JS managed data to be accessed, allocated
or deallocated. Mutable access to JS managed data requires mutable access to the
JS context, which is how the API achieves memory safety even though JS managed
references can be copied and discarded freely.

JS managed memory is split into *compartments*, which are
separately garbage collected, so the garbage collector can avoid
scanning the entire heap. The API statically tracks compartments, to
ensure that there are no direct references between compartments.

The API is implemented as bindings to the SpiderMonkey JS engine,
from which it borrows the garbage collector and the notions of compartment
and JS context. The API allows calling into JavaScript
from Rust, and calling back into Rust from JavaScript. These bindings are unsafe,
and are intended for use by a trusted bindings generator.

## Using

This crate is based on the `rust-mozjs` crate for the SpiderMonkey engine,
which uses nightly features, so `josephine` also requires nightly.

In your `Cargo.toml`:
```
[dependencies]
josephine = { git = "https://github.com/asajeffrey/josephine" }
josephine_derive = { git = "https://github.com/asajeffrey/josephine" }
```
Build with `rustup run nightly cargo build`.

## Examples

A minimal example, which uses JS to manage the lifetime of a string:
```rust,skt-josephine
use josephine::JSContext;

// Giving JavaScript some data to manage.
pub fn main() {
    // Create a new JavaScript context.
    // All interaction with JavaScript takes place in a context.
    let ref mut cx = JSContext::new();

    // Create a new compartment in that context.
    // All memory managed by JavaScript is divided into compartments,
    // which do not directly refer to each other.
    // Each compartment has a global object, which we can
    // give some Rust data to manage.
    let native = String::from("hello");
    let ref cx = cx.create_compartment().global_manage(native);

    // Check that the global contains the expected "hello" message.
    let global = cx.global();
    assert_eq!(global.borrow(cx), "hello");
}
```

Larger examples are:

* [Doubly linked lists](examples/dbllist): an implementation of a simple cyclic data structure.
  Run with `rustup nightly run cargo run --example dbllist`.
* [A fragment of the DOM](examples/minidom): A tiny subset of the HTML Document Object Model.
  Run with `rustup nightly run cargo run --example minidom`.
