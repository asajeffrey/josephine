# linjs

[![Build Status](https://travis-ci.org/asajeffrey/linjs.svg)](https://travis-ci.org/asajeffrey/linjs)

The goal of this library is to provide safe bindings into JavaScript
from Rust. If you can cause a panic or segmentation fault from safe code
using it, congratulations you found a bug! Please report an issue!

## Example

```rust,skt-linjs
use linjs::HasInstance;
use linjs::CanAccess;
use linjs::CanInitialize;
use linjs::CanCreate;
use linjs::HasClass;
use linjs::Compartment;
use linjs::CreateCompartment;
use linjs::HasGlobal;
use linjs::InCompartment;
use linjs::Initialized;
use linjs::JSContext;
use linjs::JSGlobal;
use linjs::JSRootable;
use linjs::JSManaged;
use linjs::JSRootable;
use linjs::SOMEWHERE;
use linjs::VisitCompartment;

#[derive(Debug, JSTraceable, JSRootable)]
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

#[derive(Clone, Copy, Debug, JSTraceable, JSRootable)]
struct MyGlobal<'a, C>(JSManaged<'a, C, NativeMyGlobal>);

impl<'a> CreateCompartment<'a> for MyGlobal<'a, SOMEWHERE> {
    fn init<C, S>(cx: JSContext<S>) -> JSContext<Initialized<C>> where
        S: CanInitialize + InCompartment<C>,
	C: Compartment + HasGlobal<MyGlobal<'a, C>>,
    {
        // Give the global native data to manage
        cx.manage_global(NativeMyGlobal::new());
    }
}

// Run the example
pub fn main() {
    // Create a new JavaScript context.
    let ref mut cx = JSContext::new();

    // Create a compartment in that context.
    let cx = cx.create_compartment();
    
    // Create a new global in that context.
    // We have to root the global to stop it being garbage collected.
    // If we don't root it, this code won't compile!
    let ref mut root = cx.new_root();
    let global = MyGlobal(cx.new_compartment()).in_root(root);

    // Check that the global contains the expected "hello" message.
    // We have to unpack the global first, since it is in the wildcard
    // `SOMEWHERE` compartment
    global.unpack(|global| global.borrow(cx).hello());
}
```
