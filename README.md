# linjs

[![Build Status](https://travis-ci.org/asajeffrey/linjs.svg)](https://travis-ci.org/asajeffrey/linjs)

The goal of this library is to provide safe bindings into JavaScript
from Rust. If you can cause a panic or segmentation fault from safe code
using it, congratulations you found a bug! Please report an issue!

## Example

```rust,skt-linjs
use linjs::JSContext;

// Giving JS some data to manage
pub fn main() {
    // Create a new JavaScript context,
    let ref mut cx = JSContext::new();

    // Create a new compartment in that context, and give it some native data to manage.
    let native = String::from("hello");
    let ref cx = cx.create_compartment().global_manage(native);

    // Check that the global contains the expected "hello" message.
    let global = cx.global();
    assert_eq!(global.borrow(cx), "hello");
}
```
