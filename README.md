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
use linjs::JSRunnable;
use linjs::JSContext;

struct MyGlobalClass;
impl<'a, C> HasInstance<'a, C> for MyGlobalClass {
    type Instance = NativeMyGlobal;
}
impl JSRunnable<MyGlobalClass> for MyGlobalClass {
    fn run<C, S>(self, cx: JSContext<S>) where
        S: CanCreate<C>,
	C: HasGlobal<MyGlobalClass>,
    {
        // Create the JavaScript global
        let cx = cx.create_compartment();
        let native = NativeMyGlobal { message: String::from("hello") };
	let ref cx = cx.global_manage(native);
	let global = cx.global();

        // The global is now managed by JavaScript.
	// We can borrow the native data being managed by JavaScript.
	assert_eq!(global.borrow(cx).message, "hello");
    }
}

#[derive(Debug, JSTraceable)]
struct NativeMyGlobal {
    // The lifetime of this message is managed by JavaScript.
    message: String,
}
impl HasClass for NativeMyGlobal {
    type Class = MyGlobalClass;
}

// Run the example
pub fn main() {
    MyGlobalClass.start()
}
```