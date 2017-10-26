use js::jsapi::InitSelfHostedCode;
use js::jsapi::JS_ShutDown;
use js::jsapi::JS_NewRuntime;
use js::jsapi::JSRuntime;
use js::jsapi::JS_DestroyRuntime;
use js::jsapi::JS_GetContext;
use js::jsapi::JS_Init;

use std::ptr;
use std::rc::Rc;

/// A JS runtime owned by Rust
pub struct OwnedJSRuntime(*mut JSRuntime, Rc<OwnedJSInit>);

const DEFAULT_HEAPSIZE: u32 = 32_u32 * 1024_u32 * 1024_u32;
const CHUNK_SIZE: u32 = 1 << 20;

impl OwnedJSRuntime {
    pub fn new() -> OwnedJSRuntime {
        let js_init = OWNED_JSINIT.with(|init| init.clone());
        let js_runtime = unsafe { JS_NewRuntime(DEFAULT_HEAPSIZE, CHUNK_SIZE, ptr::null_mut()) };
        let js_context = unsafe { JS_GetContext(js_runtime) };
        unsafe { InitSelfHostedCode(js_context) };
        OwnedJSRuntime(js_runtime, js_init)
    }

    pub fn rt(&self) -> *mut JSRuntime {
        self.0
    }
}

impl Drop for OwnedJSRuntime {
    fn drop(&mut self) {
        unsafe { JS_DestroyRuntime(self.0) };
    }
}

/// A capability to create JSRuntimes, owned by Rust.
struct OwnedJSInit;

thread_local! { static OWNED_JSINIT: Rc<OwnedJSInit> = { unsafe { JS_Init() }; Rc::new(OwnedJSInit) }; }

impl Drop for OwnedJSInit {
    fn drop(&mut self) {
        unsafe { JS_ShutDown() };
    }
}

