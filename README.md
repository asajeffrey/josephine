# linjs

[![Build Status](https://travis-ci.org/asajeffrey/linjs.svg)](https://travis-ci.org/asajeffrey/linjs)

The goal of this library is to provide safe bindings into JavaScript
from Rust. If you can cause a panic or segmentation fault from safe code
using it, congratulations you found a bug! Please report an issue!

## Example

```rust,skt-linjs
use linjs::CanCreate;
use linjs::HasClass;
use linjs::HasInstance;
use linjs::HasGlobal;
use linjs::Initialized;
use linjs::JSContext;
use linjs::JSGlobal;

struct MyGlobalClass;
impl<'a, C> HasInstance<'a, C> for MyGlobalClass {
    type Instance = NativeMyGlobal;
}
impl JSGlobal for MyGlobalClass {
    fn init<C, S>(cx: JSContext<S>) -> JSContext<Initialized<C>> where
        S: CanCreate<C>,
	C: HasGlobal<MyGlobalClass>,
    {
        // Some native data whose lifetime will be managed by JavaScript
        let native = NativeMyGlobal { message: String::from("hello") };

        // Create the JavaScript compartment and give the global native data to manage
        cx.create_compartment().global_manage(native)
    }
}

#[derive(Debug, JSTraceable)]
struct NativeMyGlobal {
    message: String,
}
impl HasClass for NativeMyGlobal {
    type Class = MyGlobalClass;
}

// Run the example
pub fn main() {
    // Create a new JavaScript context,
    let ref mut cx = JSContext::new();

    // Create a new global in that context.
    // We have to root the global to stop it being garbage collected.
    // If we don't root it, this code won't compile!
    rooted!(in(cx) let global = cx.new_global::<MyGlobalClass>());

    // Make sure the global has the native data we expect.
    assert_eq!(global.borrow(cx).message, "hello");
}
```
