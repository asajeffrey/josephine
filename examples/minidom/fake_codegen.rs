/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

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

use josephine::CanAccess;
use josephine::CanAlloc;
use josephine::Compartment;
use josephine::JSContext;
use josephine::JSString;
use josephine::ffi::JSEvaluateErr;
use josephine::ffi::JSInitializer;
use josephine::ffi::finalize_jsobject_with_native_data;
use josephine::ffi::jsclass_global_flags_with_slots;
use josephine::ffi::jsclass_has_reserved_slots;
use josephine::ffi::jscontext_called_from_js;
use josephine::ffi::jsmanaged_called_from_js;
use josephine::ffi::jsstring_called_from_js;
use josephine::ffi::null_function;
use josephine::ffi::null_property;
use josephine::ffi::null_wrapper;
use josephine::ffi::trace_jsobject_with_native_data;

use minidom::Console;
use minidom::Document;
use minidom::Element;
use minidom::Window;

use std::panic;
use std::ptr;

// In a real example, this code would be produced by a codegen tool
// from webidl. For the moment, we do it by hand.

// Window

#[allow(non_snake_case)]
pub trait WindowMethods<'a, C> {
    fn Console<S>(self, cx: &'a JSContext<S>) -> Console<'a, C> where S: CanAccess + CanAlloc, C: Compartment;
    fn Document<S>(self, cx: &'a JSContext<S>) -> Document<'a, C> where S: CanAccess + CanAlloc, C: Compartment;
    fn Window<S>(self, cx: &'a JSContext<S>) -> Window<'a, C> where S: CanAccess + CanAlloc, C: Compartment;
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
    let this = Window(jsmanaged_called_from_js(args.thisv())?);
    let ref mut cx = jscontext_called_from_js(cx);
    let result = this.Window(cx);
    Ok(result.0.to_jsval())
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
    let this = Window(jsmanaged_called_from_js(args.thisv())?);
    let ref mut cx = jscontext_called_from_js(cx);
    let result = this.Console(cx);
    Ok(result.0.to_jsval())
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
    let this = Window(jsmanaged_called_from_js(args.thisv())?);
    let ref mut cx = jscontext_called_from_js(cx);
    let result = this.Document(cx);
    Ok(result.0.to_jsval())
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
    fn Log<S>(self, cx: &'a mut JSContext<S>, arg: JSString<'a, C>) where S: CanAccess + CanAlloc, C: Compartment;
}

static CONSOLE_CLASS: JSClass = JSClass {
    name: b"Console\0" as *const u8 as *const c_char,
    flags: jsclass_has_reserved_slots(2),
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
    unsafe fn classp() -> *const JSClass {
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
    let this = Console(jsmanaged_called_from_js(args.thisv())?);
    let arg = jsstring_called_from_js(cx, args.get(0))?;
    let ref mut cx = jscontext_called_from_js(cx);
    this.Log(cx, arg);
    Ok(UndefinedValue())
}

// Document

#[allow(non_snake_case)]
pub trait DocumentMethods<'a, C> {
    fn Body<S>(self, cx: &'a mut JSContext<S>) -> Element<'a, C> where S: CanAccess + CanAlloc, C: Compartment;
}

static DOCUMENT_CLASS: JSClass = JSClass {
    name: b"Document\0" as *const u8 as *const c_char,
    flags: jsclass_has_reserved_slots(2),
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

const DOCUMENT_PROPERTIES: &[JSPropertySpec] = &[
    JSPropertySpec {
        name: b"body\0" as *const u8 as *const c_char,
        flags: 0,
        getter: JSNativeWrapper {
            op: Some(document_body_getter_op),
            info: ptr::null(),
        },
        setter: null_wrapper(),
    },
    null_property(),
];

pub struct DocumentInitializer;

impl JSInitializer for DocumentInitializer {
    #[allow(unsafe_code)]
    unsafe fn classp() -> *const JSClass {
        &DOCUMENT_CLASS
    }

    #[allow(unsafe_code)]
    unsafe fn properties() -> *const JSPropertySpec {
        &DOCUMENT_PROPERTIES[0]
    }
}

#[allow(unsafe_code)]
unsafe extern "C" fn document_body_getter_op(cx: *mut jsapi::JSContext, argc: c_uint, vp: *mut JSVal) -> bool {
    wrap_panic(panic::AssertUnwindSafe(|| {
        let args = CallArgs::from_vp(vp, argc);
        match document_body_getter(cx, args) {
            Ok(result) => { args.rval().set(result); true },
            Err(_err) => { false } // TODO: set the exception
        }
    }), false)
}

#[allow(unsafe_code)]
unsafe fn document_body_getter(cx: *mut jsapi::JSContext, args: CallArgs) -> Result<JSVal, JSEvaluateErr> {
    debug!("Getting body.");
    let this = Document(jsmanaged_called_from_js(args.thisv())?);
    let ref mut cx = jscontext_called_from_js(cx);
    let result = this.Body(cx);
    Ok(result.0.to_jsval())
}

// Element

#[allow(non_snake_case)]
pub trait ElementMethods<'a, C> {
    fn Parent<S>(self, cx: &'a mut JSContext<S>) -> Option<Element<'a, C>> where S: CanAccess + CanAlloc, C: Compartment;
    fn TagName<S>(self, cx: &'a mut JSContext<S>) -> JSString<'a, C> where S: CanAccess + CanAlloc, C: Compartment;
    fn Append<S, D>(self, cx: &'a mut JSContext<S>, child: Element<'a, D>) where S: CanAccess + CanAlloc, C: Compartment, D: Compartment;
}

static ELEMENT_CLASS: JSClass = JSClass {
    name: b"Element\0" as *const u8 as *const c_char,
    flags: jsclass_has_reserved_slots(2),
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

const ELEMENT_PROPERTIES: &[JSPropertySpec] = &[
    JSPropertySpec {
        name: b"parent\0" as *const u8 as *const c_char,
        flags: 0,
        getter: JSNativeWrapper {
            op: Some(element_parent_getter_op),
            info: ptr::null(),
        },
        setter: null_wrapper(),
    },
    JSPropertySpec {
        name: b"tagName\0" as *const u8 as *const c_char,
        flags: 0,
        getter: JSNativeWrapper {
            op: Some(element_tagName_getter_op),
            info: ptr::null(),
        },
        setter: null_wrapper(),
    },
    null_property(),
];

pub struct ElementInitializer;

impl JSInitializer for ElementInitializer {
    #[allow(unsafe_code)]
    unsafe fn classp() -> *const JSClass {
        &ELEMENT_CLASS
    }

    #[allow(unsafe_code)]
    unsafe fn properties() -> *const JSPropertySpec {
        &ELEMENT_PROPERTIES[0]
    }
}

#[allow(unsafe_code)]
unsafe extern "C" fn element_parent_getter_op(cx: *mut jsapi::JSContext, argc: c_uint, vp: *mut JSVal) -> bool {
    wrap_panic(panic::AssertUnwindSafe(|| {
        let args = CallArgs::from_vp(vp, argc);
        match element_parent_getter(cx, args) {
            Ok(result) => { args.rval().set(result); true },
            Err(_err) => { false } // TODO: set the exception
        }
    }), false)
}

#[allow(unsafe_code)]
unsafe fn element_parent_getter(cx: *mut jsapi::JSContext, args: CallArgs) -> Result<JSVal, JSEvaluateErr> {
    debug!("Getting parent.");
    let this = Element(jsmanaged_called_from_js(args.thisv())?);
    let ref mut cx = jscontext_called_from_js(cx);
    let result = this.Parent(cx);
    Ok(result.map(|result| result.0.to_jsval()).unwrap_or(UndefinedValue()))
}

#[allow(unsafe_code,non_snake_case)]
unsafe extern "C" fn element_tagName_getter_op(cx: *mut jsapi::JSContext, argc: c_uint, vp: *mut JSVal) -> bool {
    wrap_panic(panic::AssertUnwindSafe(|| {
        let args = CallArgs::from_vp(vp, argc);
        match element_tagName_getter(cx, args) {
            Ok(result) => { args.rval().set(result); true },
            Err(_err) => { false } // TODO: set the exception
        }
    }), false)
}

#[allow(unsafe_code,non_snake_case)]
unsafe fn element_tagName_getter(cx: *mut jsapi::JSContext, args: CallArgs) -> Result<JSVal, JSEvaluateErr> {
    debug!("Getting tagName.");
    let this = Element(jsmanaged_called_from_js(args.thisv())?);
    let ref mut cx = jscontext_called_from_js(cx);
    let result = this.TagName(cx);
    Ok(result.to_jsval())
}

