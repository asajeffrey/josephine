# linjs

[![Build Status](https://travis-ci.org/asajeffrey/linjs.svg)](https://travis-ci.org/asajeffrey/linjs)

The goal of this library is to provide safe bindings into JavaScript
from Rust. If you can cause a panic or segmentation fault from safe code
using it, congratulations you found a bug! Please report an issue!

## Example

```rust,skt-linjs
use linjs::CanAccess;
use linjs::CanCreateCompartments;
use linjs::JSContext;
use linjs::JSManaged;
use linjs::JSRootable;
use linjs::SOMEWHERE;

// Some native Rust data which we hand to JS to manage its lifetime.
#[derive(JSTraceable, JSRootable, HasClass)]
pub struct NativeMyGlobal {
    message: String,
}
impl NativeMyGlobal {
    fn new() -> NativeMyGlobal {
        NativeMyGlobal {
	    message: String::from("hello"),
	}
    }
}

// A JS global, with JS-managed lifetime `'a`, in compartment `C`.
// This global is managing some native data.
#[derive(Copy, Clone, JSTraceable, JSRootable)]
struct MyGlobal<'a, C> (JSManaged<'a, C, NativeMyGlobalClass>);

impl<'a> MyGlobal<'a, SOMEWHERE> {
    fn new<S>(cx: &'a mut JSContext<S>) -> MyGlobal<'a, SOMEWHERE> where
        S: CanCreateCompartments,
    {
        let native = NativeMyGlobal::new();
        let cx = cx.create_compartment().global_manage(native);
        MyGlobal(cx.global().forget_compartment())
    }

    fn assert_is_hello<S>(self, cx: &JSContext<S>) where
        S: CanAccess,
    {
        // We have to unpack the global first, since it is in the wildcard
        // `SOMEWHERE` compartment
        self.0.unpack(|managed| assert_eq!(managed.borrow(cx).message, "hello"))
    }
}

// Run the example
pub fn main() {
    // Create a new JavaScript context,
    let ref mut cx = JSContext::new();

    // Create a new global in that context.
    // We have to root the global to stop it being garbage collected.
    // If we don't root it, this code won't compile!
    let ref mut root = cx.new_root();
    let global = MyGlobal::new(cx).in_root(root);

    // Check that the global contains the expected "hello" message.
    global.assert_is_hello(cx);
}
```
