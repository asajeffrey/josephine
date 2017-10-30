use super::CanAccess;
use super::CanAlloc;
use super::Compartment;
use super::InCompartment;
use super::IsSnapshot;
use super::JSContext;
use super::JSInitializable;
use super::JSRootable;
use super::JSTraceable;
use super::JSTransplantable;
use super::SOMEWHERE;
use super::ffi::JSEvaluateErr;
use super::ffi::JSInitializer;
use super::ffi::UNSAFE;

use js::glue::CallObjectTracer;

use js::jsapi::GCTraceKindToAscii;
use js::jsapi::HandleValue;
use js::jsapi::Heap;
use js::jsapi::JSObject;
use js::jsapi::JSTracer;
use js::jsapi::JS_GetReservedSlot;
use js::jsapi::JS_IsNative;
use js::jsapi::TraceKind;
use js::jsapi::JS_NewObjectWithGivenProto;
use js::jsapi::JS_SetReservedSlot;

use js::jsval::JSVal;
use js::jsval::ObjectValue;
use js::jsval::PrivateValue;

use js::rust::get_context_compartment;
use js::rust::get_object_compartment;

use libc;

use std::any::TypeId;
use std::fmt;
use std::fmt::Debug;
use std::mem;
use std::marker::PhantomData;

/// The type of JS-managed data in a JS compartment `C`, with lifetime `'a` and type `T`.
///
/// If the user has access to a `JSManaged`, then the JS-managed
/// data is live for the given lifetime.
pub struct JSManaged<'a, C, T> {
    js_object: *mut Heap<*mut JSObject>,
    raw: *mut (),
    marker: PhantomData<(&'a(), C, T)>
}

impl<'a, C, T> Clone for JSManaged<'a, C, T> {
    fn clone(&self) -> Self {
        JSManaged {
            js_object: self.js_object,
            raw: self.raw,
            marker: PhantomData,
        }
    }
}

impl<'a, C, T> Copy for JSManaged<'a, C, T> {
}

impl<'a, C, T> Debug for JSManaged<'a, C, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt.debug_struct("JSManaged")
            .field("js_object", &self.js_object)
            .field("raw", &self.raw)
            .finish()
    }
}

impl<'a, C, T> PartialEq for JSManaged<'a, C, T> {
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}

impl<'a, C, T> Eq for JSManaged<'a, C, T> {
}

unsafe impl<'a, C, T> JSTraceable for JSManaged<'a, C, T> where
    T: JSTraceable,
{
    unsafe fn trace(&self, trc: *mut JSTracer) {
        debug!("Tracing JSManaged {:p}.", self.js_object);
        CallObjectTracer(trc, self.js_object, GCTraceKindToAscii(TraceKind::Object));
    }
}

impl<'a, C, T> JSManaged<'a, C, T> {
    pub fn new<S>(cx: &'a mut JSContext<S>, value: T) -> JSManaged<'a, C, T::Aged> where
        S: CanAlloc + InCompartment<C>,
        C: Compartment,
        T: JSTraceable + JSInitializable + JSRootable<'a>,
    {
        debug!("Managing native data.");
        let jsapi_context = cx.cx();
        let prototype = cx.prototype_of::<T>();

        let boxed_jsobject = Box::new(Heap::default());
        debug!("Boxed object {:p}", boxed_jsobject);
        let unboxed_jsobject = unsafe { JS_NewObjectWithGivenProto(jsapi_context, T::Init::classp(), prototype) };
        debug!("Unboxed object {:p}", unboxed_jsobject);
        assert!(!unboxed_jsobject.is_null());
        boxed_jsobject.set(unboxed_jsobject);

        // Save a pointer to the native value in a private slot
        let boxed_value: Box<JSManageable> = Box::new(Some(value));
        let fat_value: [*const libc::c_void; 2] = unsafe { mem::transmute(boxed_value) };
        unsafe { JS_SetReservedSlot(boxed_jsobject.get(), 0, PrivateValue(fat_value[0])) };
        unsafe { JS_SetReservedSlot(boxed_jsobject.get(), 1, PrivateValue(fat_value[1])) };

        // TODO: can we be sure that this won't trigger GC? Or do we need to root the boxed object?
        debug!("Initializing JS object.");
        unsafe { T::Init::js_init_object(jsapi_context, boxed_jsobject.handle()) };

        debug!("Managed native data.");
        JSManaged {
            js_object: Box::into_raw(boxed_jsobject),
            raw: fat_value[0] as *mut (),
            marker: PhantomData,
        }
    }

    pub unsafe fn from_raw(js_object: *mut Heap<*mut JSObject>, raw: *mut ()) -> JSManaged<'a, C, T> {
        JSManaged {
            js_object: js_object,
            raw: raw,
            marker: PhantomData,
        }        
    }

    /// Read-only access to JS-managed data.
    pub fn get<'b, S>(self, _: &'b JSContext<S>) -> T::Aged where
        S: CanAccess,
        C: Compartment,
        T: JSRootable<'b>,
        T::Aged: Copy,
        'a: 'b,
    {
        let result = unsafe { *(self.raw as *mut Option<T::Aged>) };
        result.unwrap()
    }

    pub fn borrow<'b, S>(self, _: &'b JSContext<S>) -> &'b T::Aged where
        S: CanAccess,
        C: Compartment,
        T: JSRootable<'b>,
        'a: 'b,
    {
        let result = unsafe { &*(self.raw as *mut Option<T::Aged>) };
        result.as_ref().unwrap()
    }

    /// Read-write access to JS-managed data.
    pub fn borrow_mut<'b, S>(self, _: &'b mut JSContext<S>) -> &'b mut T::Aged where
        S: CanAccess,
        C: Compartment,
        T: JSRootable<'b>,
        'a: 'b,
    {
        let result = unsafe { &mut *(self.raw as *mut Option<T::Aged>) };
        result.as_mut().unwrap()
    }

    /// Change the compartment of JS-managed data.
    pub unsafe fn change_compartment<D>(self) -> JSManaged<'a, D, T::Transplanted> where
        T: JSTransplantable<D>,
    {
        JSManaged {
            js_object: self.js_object,
            raw: self.raw,
            marker: PhantomData,
        }
    }

    /// Change the lifetime of JS-managed data.
    pub unsafe fn change_lifetime<'b>(self) -> JSManaged<'b, C, T::Aged> where
        T: JSRootable<'b>,
    {
        JSManaged {
            js_object: self.js_object,
            raw: self.raw,
            marker: PhantomData,
        }
    }

    /// It's safe to extend the lifetime of JS-managed data if it has been snapshotted.
    pub fn extend_lifetime<'b, 'c, S>(self, _: &'c JSContext<S>) -> JSManaged<'b, C, T::Aged> where
        S: IsSnapshot<'b>,
        T: JSRootable<'b>,
    {
        unsafe { self.change_lifetime() }
    }

    /// Forget about which compartment the managed data is in.
    /// This is safe because when we mutate data in compartment `C` we require
    /// `C: Compartment`, which means it is never `SOMEWHERE`.
    pub fn forget_compartment(self) -> JSManaged<'a, SOMEWHERE, T::Transplanted> where
        T: JSTransplantable<SOMEWHERE>,
    {
        unsafe { self.change_compartment() }
    }

    /// Check to see if the current object is in the same compartment as another.
    pub fn in_compartment<S, D>(self, cx: &JSContext<S>) -> Option<JSManaged<'a, D, T::Transplanted>> where
        T: JSTransplantable<D>,
        S: InCompartment<D>,
    {
        let self_compartment = unsafe { get_object_compartment(self.to_jsobject()) };
        let cx_compartment = unsafe { get_context_compartment(cx.cx()) };
        if self_compartment == cx_compartment {
            Some(unsafe { self.change_compartment() })
        } else {
            None
        }
    }

    pub fn to_raw_native(self) -> *mut Option<T> {
        self.raw as *mut Option<T>
    }

    pub fn to_heap_object(self) -> *mut Heap<*mut JSObject> {
        self.js_object
    }

    pub fn to_jsobject(self) -> *mut JSObject {
        unsafe { &*self.to_heap_object() }.get()
    }

    pub fn to_jsval(self) -> JSVal {
        ObjectValue(self.to_jsobject())
    }
}

/// A trait for Rust data which can be managed

// TODO: this trait shouldn't be public

pub trait JSManageable: JSTraceable {
    unsafe fn class_id(&self) -> TypeId;
}

impl<T> JSManageable for Option<T> where
    T: JSTraceable + JSInitializable
{
    unsafe fn class_id(&self) -> TypeId {
        TypeId::of::<T::Init>()
    }
}

pub unsafe fn jsmanaged_called_from_js<'a, T>(js_value: HandleValue) -> Result<JSManaged<'a, UNSAFE, T>, JSEvaluateErr> where
    T: JSInitializable,
{
    if !js_value.is_object() {
        return Err(JSEvaluateErr::NotAnObject);
    }

    let js_object = js_value.to_object();

    if !JS_IsNative(js_object) {
        return Err(JSEvaluateErr::NotJSManaged);
    }

    let slot0 = JS_GetReservedSlot(js_object, 0);
    let slot1 = JS_GetReservedSlot(js_object, 1);
    if slot0.is_undefined() || slot1.is_undefined() {
        return Err(JSEvaluateErr::NotJSManaged);
    }
    // This unsafe if there are raw uses of jsapi that are doing
    // other things with the reserved slots.
    let native: &mut JSManageable = mem::transmute([ slot0.to_private(), slot1.to_private() ]);

    // TODO: inheritance
    if TypeId::of::<T::Init>() != native.class_id() {
        return Err(JSEvaluateErr::WrongClass);
    }
    let raw = native as *mut _ as *mut ();

    // TODO: these boxes never get deallocated, which is a space leak!
    let boxed = Box::new(Heap::default());
    boxed.set(js_object);

    Ok(JSManaged {
        js_object: Box::into_raw(boxed),
        raw: raw,
        marker: PhantomData,
    })
}
