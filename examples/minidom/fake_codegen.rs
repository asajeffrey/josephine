use js::jsapi;
use js::jsapi::CallArgs;
use js::jsapi::JSClass;
use js::jsapi::JSClassOps;
use js::jsapi::JSFunctionSpec;
use js::jsapi::JSNativeWrapper;
use js::jsapi::JSPropertySpec;

use js::jsval::JSVal;
use js::jsval::UndefinedValue;

use js::panic::wrap_panic;

use libc::c_char;
use libc::c_uint;

use linjs::CanAccess;
use linjs::CanAlloc;
use linjs::JSContext;
use linjs::JSEvaluateErr;
use linjs::JSInitializer;
use linjs::JSString;
use linjs::finalize_jsobject_with_native_data;
use linjs::jsclass_global_flags_with_slots;
use linjs::jscontext_called_from_js;
use linjs::jsmanaged_called_from_js;
use linjs::jsstring_called_from_js;
use linjs::null_function;
use linjs::null_property;
use linjs::null_wrapper;
use linjs::trace_jsobject_with_native_data;

use minidom::Console;
use minidom::Document;
use minidom::Window;

use std::panic;
use std::ptr;

// In a real example, this code would be produced by a codegen tool
// from webidl. For the moment, we do it by hand.

// Window

#[allow(non_snake_case)]
pub trait WindowMethods<'a, C> {
    fn Console<S>(self, cx: &'a JSContext<S>) -> Console<'a, C> where S: CanAccess + CanAlloc;
    fn Document<S>(self, cx: &'a JSContext<S>) -> Document<'a, C> where S: CanAccess + CanAlloc;
    fn Window<S>(self, cx: &'a JSContext<S>) -> Window<'a, C> where S: CanAccess + CanAlloc;
}

static WINDOW_CLASS: JSClass = JSClass {
    name: b"Window\0" as *const u8 as *const c_char,
    flags: jsclass_global_flags_with_slots(2),
    cOps: &JSClassOps {
        addProperty: None,
        call: None,
        construct: None,
        delProperty: None,
        enumerate: None,
        finalize: Some(finalize_jsobject_with_native_data),
        getProperty: None,
        hasInstance: None,
        mayResolve: None,
        resolve: None,
        setProperty: None,
        trace: Some(trace_jsobject_with_native_data),
    },
    reserved: [0 as *mut _; 3],
};

const WINDOW_PROPERTIES: &[JSPropertySpec] = &[
    JSPropertySpec {
        name: b"window\0" as *const u8 as *const c_char,
        flags: 0,
        getter: JSNativeWrapper {
            op: Some(window_window_getter_op),
            info: ptr::null(),
        },
        setter: null_wrapper(),
    },
    JSPropertySpec {
        name: b"console\0" as *const u8 as *const c_char,
        flags: 0,
        getter: JSNativeWrapper {
            op: Some(window_console_getter_op),
            info: ptr::null(),
        },
        setter: null_wrapper(),
    },
    JSPropertySpec {
        name: b"document\0" as *const u8 as *const c_char,
        flags: 0,
        getter: JSNativeWrapper {
            op: Some(window_document_getter_op),
            info: ptr::null(),
        },
        setter: null_wrapper(),
    },
    null_property(),
];

// Just stub methods for now.

#[allow(unsafe_code)]
unsafe extern "C" fn window_window_getter_op(cx: *mut jsapi::JSContext, argc: c_uint, vp: *mut JSVal) -> bool {
    wrap_panic(panic::AssertUnwindSafe(|| {
        let args = CallArgs::from_vp(vp, argc);
        match window_window_getter(cx, args) {
            Ok(result) => { args.rval().set(result); true },
            Err(_err) => { false } // TODO: set the exception
        }
    }), false)
}

#[allow(unsafe_code)]
unsafe fn window_window_getter(cx: *mut jsapi::JSContext, args: CallArgs) -> Result<JSVal, JSEvaluateErr> {
    debug!("Getting window.");
    let this = Window::from(jsmanaged_called_from_js(args.thisv())?);
    let ref mut cx = jscontext_called_from_js(cx);
    let result = this.Window(cx);
    Ok(result.to_jsval())
}

#[allow(unsafe_code)]
unsafe extern "C" fn window_console_getter_op(cx: *mut jsapi::JSContext, argc: c_uint, vp: *mut JSVal) -> bool {
    wrap_panic(panic::AssertUnwindSafe(|| {
        let args = CallArgs::from_vp(vp, argc);
        match window_console_getter(cx, args) {
            Ok(result) => { args.rval().set(result); true },
            Err(_err) => { false } // TODO: set the exception
        }
    }), false)
}

#[allow(unsafe_code)]
unsafe fn window_console_getter(cx: *mut jsapi::JSContext, args: CallArgs) -> Result<JSVal, JSEvaluateErr> {
    debug!("Getting console.");
    let this = Window::from(jsmanaged_called_from_js(args.thisv())?);
    let ref mut cx = jscontext_called_from_js(cx);
    let result = this.Console(cx);
    Ok(result.to_jsval())
}

#[allow(unsafe_code)]
unsafe extern "C" fn window_document_getter_op(cx: *mut jsapi::JSContext, argc: c_uint, vp: *mut JSVal) -> bool {
    wrap_panic(panic::AssertUnwindSafe(|| {
        let args = CallArgs::from_vp(vp, argc);
        match window_document_getter(cx, args) {
            Ok(result) => { args.rval().set(result); true },
            Err(_err) => { false } // TODO: set the exception
        }
    }), false)
}

#[allow(unsafe_code)]
unsafe fn window_document_getter(cx: *mut jsapi::JSContext, args: CallArgs) -> Result<JSVal, JSEvaluateErr> {
    debug!("Getting document.");
    let this = Window::from(jsmanaged_called_from_js(args.thisv())?);
    let ref mut cx = jscontext_called_from_js(cx);
    let result = this.Document(cx);
    Ok(result.to_jsval())
}

pub struct WindowInitializer;

impl JSInitializer for WindowInitializer {
    #[allow(unsafe_code)]
    unsafe fn global_classp() -> *const JSClass {
        &WINDOW_CLASS
    }

    #[allow(unsafe_code)]
    unsafe fn properties() -> *const JSPropertySpec {
        &WINDOW_PROPERTIES[0]
    }
}

// Console

#[allow(non_snake_case)]
pub trait ConsoleMethods<'a, C> {
    fn Log<S>(self, cx: &'a mut JSContext<S>, arg: JSString<'a, C>) where S: CanAccess + CanAlloc;
}

static CONSOLE_CLASS: JSClass = JSClass {
    name: b"Console\0" as *const u8 as *const c_char,
    flags: jsclass_global_flags_with_slots(2),
    cOps: &JSClassOps {
        addProperty: None,
        call: None,
        construct: None,
        delProperty: None,
        enumerate: None,
        finalize: Some(finalize_jsobject_with_native_data),
        getProperty: None,
        hasInstance: None,
        mayResolve: None,
        resolve: None,
        setProperty: None,
        trace: Some(trace_jsobject_with_native_data),
    },
    reserved: [0 as *mut _; 3],
};

const CONSOLE_FUNCTIONS: &[JSFunctionSpec] = &[
    JSFunctionSpec {
        name: b"log\0" as *const u8 as *const c_char,
        selfHostedName: ptr::null(),
        flags: 0,
        nargs: 1,
        call: JSNativeWrapper {
            op: Some(console_log_op),
            info: ptr::null(),
        },
    },
    null_function(),
];

pub struct ConsoleInitializer;

impl JSInitializer for ConsoleInitializer {
    #[allow(unsafe_code)]
    unsafe fn global_classp() -> *const JSClass {
        &CONSOLE_CLASS
    }

    #[allow(unsafe_code)]
    unsafe fn functions() -> *const JSFunctionSpec {
        &CONSOLE_FUNCTIONS[0]
    }
}

#[allow(unsafe_code)]
unsafe extern "C" fn console_log_op(cx: *mut jsapi::JSContext, argc: c_uint, vp: *mut JSVal) -> bool {
    wrap_panic(panic::AssertUnwindSafe(|| {
        let args = CallArgs::from_vp(vp, argc);
        match console_log(cx, args) {
            Ok(result) => { args.rval().set(result); true },
            Err(_err) => { false } // TODO: set the exception
        }
    }), false)
}

#[allow(unsafe_code)]
unsafe fn console_log(cx: *mut jsapi::JSContext, args: CallArgs) -> Result<JSVal, JSEvaluateErr> {
    debug!("Logging.");
    let this = Console::from(jsmanaged_called_from_js(args.thisv())?);
    let arg = jsstring_called_from_js(cx, args.get(0))?;
    let ref mut cx = jscontext_called_from_js(cx);
    this.Log(cx, arg);
    Ok(UndefinedValue())
}

