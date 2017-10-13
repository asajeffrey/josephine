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
use linjs::JSRootable;

struct MyGlobalClass;
impl<'a, C> HasInstance<'a, C> for MyGlobalClass {
    type Instance = NativeMyGlobal;
}
impl JSGlobal for MyGlobalClass {
    fn init<C, S>(cx: JSContext<S>) -> JSContext<Initialized<C>> where
        S: CanCreate<C>,
	C: HasGlobal<MyGlobalClass>,
    {
        // Create the JavaScript compartment and give the global native data to manage
        cx.create_global(NativeMyGlobal::new())
    }
}

#[derive(Debug, JSTraceable)]
struct NativeMyGlobal {
    message: String,
}
impl NativeMyGlobal {
    fn new() -> NativeMyGlobal {
        NativeMyGlobal {
	    message: String::from("hello"),
	}
    }
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
    let ref mut root = cx.new_root();
    let global = cx.new_global::<MyGlobalClass>().in_root(root);

    // Make sure the global has the native data we expect.
    assert_eq!(global.borrow(cx).message, "hello");
}
```
