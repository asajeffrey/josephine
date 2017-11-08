/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use super::Compartment;
use super::managed::JSManageable;

pub use super::context::jscontext_called_from_js;
pub use super::managed::jsmanaged_called_from_js;
pub use super::string::jsstring_called_from_js;

use js::jsapi;
use js::jsapi::CompartmentOptions;
use js::jsapi::Handle;
use js::jsapi::JSCLASS_RESERVED_SLOTS_SHIFT;
use js::jsapi::JSClass;
use js::jsapi::JSClassOps;
use js::jsapi::JSFreeOp;
use js::jsapi::JSFunctionSpec;
use js::jsapi::JSNative;
use js::jsapi::JSNativeWrapper;
use js::jsapi::JSObject;
use js::jsapi::JSPrincipals;
use js::jsapi::JSPropertySpec;
use js::jsapi::JSTracer;
use js::jsapi::JSVersion;
use js::jsapi::JS_GetObjectPrototype;
use js::jsapi::JS_GetReservedSlot;
use js::jsapi::JS_InitClass;
use js::jsapi::JS_InitStandardClasses;
use js::jsapi::JS_IsNative;
use js::jsapi::OnNewGlobalHookOption;

use js::JSCLASS_GLOBAL_SLOT_COUNT;
use js::JSCLASS_IS_GLOBAL;
use js::JSCLASS_RESERVED_SLOTS_MASK;

use libc::c_char;
use libc::c_uint;

use std::mem;
use std::ptr;

/// The errors which might be returned from cx.evaluate("code")
// TODO: store more information about the reason for the error
#[derive(Clone, Debug)]
pub enum JSEvaluateErr {
    JSException,
    NotAnObject,
    NotAString,
    NotJSManaged,
    WrongClass,
}

/// An unsafe compartment name, which we only give access to via unsafe code.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct UNSAFE(());
impl Compartment for UNSAFE {}

/// A trait for Rust data which can be reflected

pub trait JSInitializable {
    type Init: 'static + JSInitializer = DefaultInitializer;
}

/// Basic types
impl JSInitializable for String {}
impl JSInitializable for usize {}
// etc.

/// Initialize JS data

pub trait JSInitializer {
    unsafe fn parent_prototype(cx: *mut jsapi::JSContext, global: jsapi::HandleObject) -> *mut JSObject {
        JS_GetObjectPrototype(cx, global)
    }

    unsafe fn classp() -> *const JSClass {
        &DEFAULT_CLASS
    }

    unsafe fn global_classp() -> *const JSClass {
        &DEFAULT_GLOBAL_CLASS
    }

    unsafe fn global_principals() -> *mut JSPrincipals {
        ptr::null_mut()
    }

    unsafe fn global_hook_option() -> OnNewGlobalHookOption {
         OnNewGlobalHookOption::FireOnNewGlobalHook
    }

    unsafe fn global_options() -> CompartmentOptions {
        let mut options = CompartmentOptions::default();
        options.behaviors_.version_ = JSVersion::JSVERSION_ECMA_5;
        options.creationOptions_.sharedMemoryAndAtomics_ = true;
        options
    }

    unsafe fn constructor() -> (JSNative, c_uint) {
        (None, 0)
    }

    unsafe fn properties() -> *const JSPropertySpec {
        ptr::null()
    }

    unsafe fn functions() -> *const JSFunctionSpec {
        ptr::null()
    }

    unsafe fn static_properties() -> *const JSPropertySpec {
        ptr::null()
    }

    unsafe fn static_functions() -> *const JSFunctionSpec {
        ptr::null()
    }

    unsafe fn js_init_class(cx: *mut jsapi::JSContext, global: jsapi::HandleObject) -> *mut JSObject {
        let ref parent_proto = Self::parent_prototype(cx, global);
        let parent_proto_handle = Handle::from_marked_location(parent_proto);
        let classp = Self::classp();
        let (constructor, nargs) = Self::constructor();
        let ps = Self::properties();
        let fs = Self::functions();
        let static_ps = Self::static_properties();
        let static_fs = Self::static_functions();
        JS_InitClass(cx, global, parent_proto_handle, classp, constructor, nargs, ps, fs, static_ps, static_fs)
    }

    unsafe fn js_init_object(_cx: *mut jsapi::JSContext, _obj: jsapi::HandleObject) {
    }

    unsafe fn js_init_global(cx: *mut jsapi::JSContext, global: jsapi::HandleObject) {
        JS_InitStandardClasses(cx, global);
    }
}

/// A default class.

pub struct DefaultInitializer;

impl JSInitializer for DefaultInitializer {}

static DEFAULT_CLASS: JSClass = JSClass {
    name: b"[Object]\0" as *const u8 as *const c_char,
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

static DEFAULT_GLOBAL_CLASS: JSClass = JSClass {
    name: b"[Global]\0" as *const u8 as *const c_char,
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

pub const fn null_wrapper() -> JSNativeWrapper {
    JSNativeWrapper {
        op: None,
        info: ptr::null(),
    }
}

pub const fn null_property() -> JSPropertySpec {
    JSPropertySpec {
        name: ptr::null(),
        flags: 0,
        getter: null_wrapper(),
        setter: null_wrapper(),
    }
}

pub const fn null_function() -> JSFunctionSpec {
    JSFunctionSpec {
        name: ptr::null(),
        flags: 0,
        call: null_wrapper(),
        nargs: 0,
        selfHostedName: ptr::null(),
    }
}

// Repating stuff from https://dxr.mozilla.org/mozilla-central/source/js/public/Class.h
// (it uses #defines which are not available in Rust)

pub const fn jsclass_has_reserved_slots(n: c_uint) -> c_uint {
    (n & JSCLASS_RESERVED_SLOTS_MASK) << JSCLASS_RESERVED_SLOTS_SHIFT
}

pub const fn jsclass_global_flags_with_slots(n: c_uint) -> c_uint {
    JSCLASS_IS_GLOBAL | jsclass_has_reserved_slots(JSCLASS_GLOBAL_SLOT_COUNT + n)
}

pub unsafe extern "C" fn trace_jsobject_with_native_data(trc: *mut JSTracer, obj: *mut JSObject) {
    if !JS_IsNative(obj) {
        debug!("Not a native object (should be a prototype).");
    }

    debug!("Tracing {:p}.", obj);
    let slot0 = JS_GetReservedSlot(obj, 0);
    let slot1 = JS_GetReservedSlot(obj, 1);
    if slot0.is_undefined() || slot1.is_undefined() {
        return debug!("Tracing uninitialized object.");
    }
    let traceable: &JSManageable = mem::transmute([ slot0.to_private(), slot1.to_private() ]);
    debug!("Tracing native {:p}.", traceable);
    traceable.trace(trc);
}

pub unsafe extern "C" fn finalize_jsobject_with_native_data(_op: *mut JSFreeOp, obj: *mut JSObject) {
    if !JS_IsNative(obj) {
        debug!("Not a native object (should be a prototype).");
        // TODO: remove the object from the prototype hash table?
    }

    debug!("Finalizing {:p}.", obj);
    let slot0 = JS_GetReservedSlot(obj, 0);
    let slot1 = JS_GetReservedSlot(obj, 1);
    if slot0.is_undefined() || slot1.is_undefined() {
        return debug!("Finalizing uninitialized object.");
    }
    let traceable: *mut JSManageable = mem::transmute([ slot0.to_private(), slot1.to_private() ]);
    debug!("Finalizing native {:p}.", traceable);
    Box::from_raw(traceable);
}
