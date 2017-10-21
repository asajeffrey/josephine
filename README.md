# josephine

[![Build Status](https://travis-ci.org/asajeffrey/josephine.svg)](https://travis-ci.org/asajeffrey/josephine)

The goal of this library is to provide safe bindings into JavaScript
from Rust. If you can cause a panic or segmentation fault from safe code
using it, congratulations you found a bug! Please report an issue!

## Example

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
