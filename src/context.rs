use super::Compartment;
use super::JSInitializable;
use super::JSManaged;
use super::JSRoot;
use super::JSRootable;
use super::JSTraceable;
use super::JSTransplantable;
use super::compartment::BOUND;
use super::ffi::JSEvaluateErr;
use super::ffi::JSInitializer;
use super::managed::JSManageable;
use super::root::trace_thread_local_roots;
use super::runtime::OwnedJSRuntime;

use js::glue::NewCompileOptions;

use js::jsapi;
use js::jsapi::Evaluate2;
use js::jsapi::Handle;
use js::jsapi::Heap;
use js::jsapi::JSAutoCompartment;
use js::jsapi::JSObject;
use js::jsapi::JS_AddExtraGCRootsTracer;
use js::jsapi::JS_ClearPendingException;
use js::jsapi::JS_DefineFunctions;
use js::jsapi::JS_DefineProperties;
use js::jsapi::JS_GC;
use js::jsapi::JS_GetContext;
use js::jsapi::JS_GetPendingException;
use js::jsapi::JS_GetReservedSlot;
use js::jsapi::JS_GetRuntime;
use js::jsapi::JS_IsExceptionPending;
use js::jsapi::JS_NewGlobalObject;
use js::jsapi::JS_SetReservedSlot;
use js::jsapi::MutableHandle;

use js::jsval::JSVal;
use js::jsval::PrivateValue;
use js::jsval::UndefinedValue;

use libc;

use std::any::TypeId;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::mem;
use std::ptr;
use std::str;

/// The type for JS contexts whose current state is `S`.
pub struct JSContext<S> {
    jsapi_context: *mut jsapi::JSContext,
    global_js_object: *mut Heap<*mut JSObject>,
    global_raw: *mut (),
    auto_compartment: Option<JSAutoCompartment>,
    runtime: Option<OwnedJSRuntime>,
    marker: PhantomData<S>,
}

/// A context state in an initialized compartment `C` with lifetime `'a` and global type `T`.
pub struct Initialized<'a, C, T> (PhantomData<(&'a(), C, T)>);

/// A context state in the middTle of initializing a compartment `C` with lifetime `'a` and global type `T`.
pub struct Initializing<'a, C, T> (PhantomData<(&'a(), C, T)>);

/// A context state that has entered compartment `C` via an object with lifetime `'a` and global type `T`.
/// The previous context state was `S`.
pub struct Entered<'a, C, T, S> (PhantomData<(&'a(), C, T, S)>);

/// A context state for JS contexts owned by Rust.
pub struct Owned (());

/// A context state for callbacks from JS,
pub struct FromJS (());

/// A context state in snapshotted compartment in underlying state `S`,
/// which guarantees that no GC will happen during the lifetime `'a`.
pub struct Snapshotted<'a, S> (PhantomData<(&'a(), S)>);

/// A marker trait for JS contexts in compartment `C`
pub trait InCompartment<C> {}
impl<'a, C, T> InCompartment<C> for Initializing<'a, C, T> {}
impl<'a, C, T> InCompartment<C> for Initialized<'a, C, T> {}
impl<'a, C, T, S> InCompartment<C> for Entered<'a, C, T, S> {}
impl<'a, C, S> InCompartment<C> for Snapshotted<'a, S> where S: InCompartment<C> {}

/// A marker trait for JS contexts that can access native state
pub trait CanAccess {}
impl<'a, C, T> CanAccess for Initialized<'a, C, T> {}
impl CanAccess for FromJS {}
impl CanAccess for Owned {}
impl<'a, C, T, S> CanAccess for Entered<'a, C, T, S> where S: CanAccess {}
impl<'a, S> CanAccess for Snapshotted<'a, S> where S: CanAccess {}

/// A marker trait for JS contexts that are snapshots
pub trait IsSnapshot<'a> {}
impl<'a, S> IsSnapshot<'a> for Snapshotted<'a, S> {}
impl<'a, 'b, C, T, S> IsSnapshot<'a> for Entered<'b, C, T, S> where S: IsSnapshot<'a> {}

/// A marker trait for JS contexts that can (de)allocate objects
pub trait CanAlloc {}
impl<'a, C, T> CanAlloc for Initialized<'a, C, T> {}
impl<'a, C, T> CanAlloc for Initializing<'a, C, T> {}
impl CanAlloc for FromJS {}
impl CanAlloc for Owned {}
impl<'a, C, T, S> CanAlloc for Entered<'a, C, T, S> where S: CanAlloc {}

/// A marker trait for JS contexts that are in the middle of initializing
pub trait IsInitializing<'a, C, T> {}
impl<'a, C, T> IsInitializing<'a, C, T> for Initializing<'a, C, T> {}

/// A marker trait for JS contexts that have initialized a global
pub trait IsInitialized<'a, C, T> {}
impl<'a, C, T> IsInitialized<'a, C, T> for Initialized<'a, C, T> {}

/// A marker trait for JS contexts that have been entered
pub trait IsEntered<'a, C, T> {}
impl<'a, C, T, S> IsEntered<'a, C, T> for Entered<'a, C, T, S> {}

impl JSContext<Owned> {
    /// Create a new JSContext.
    pub fn new() -> JSContext<Owned> {
        // TODO: set options on the runtime?
        let runtime = OwnedJSRuntime::new();
        let jsapi_context = unsafe { JS_GetContext(runtime.rt()) };
        unsafe { JS_AddExtraGCRootsTracer(runtime.rt(), Some(trace_thread_local_roots), ptr::null_mut()); }
        JSContext {
            jsapi_context: jsapi_context,
            global_js_object: ptr::null_mut(),
            global_raw: ptr::null_mut(),
            auto_compartment: None,
            runtime: Some(runtime),
            marker: PhantomData,
        }
    }
}

impl<S> JSContext<S> {
    /// Get a snapshot of the JS state.
    /// The snapshot only allows access to the methods that are guaranteed not to call GC,
    /// so we don't need to root JS-managed pointers during the lifetime of a snapshot.
    pub fn snapshot<'a>(&'a mut self) -> JSContext<Snapshotted<'a, S>> {
        debug!("Creating snapshot.");
        JSContext {
            jsapi_context: self.jsapi_context,
            global_js_object: self.global_js_object,
            global_raw: self.global_raw,
            auto_compartment: None,
            runtime: None,
            marker: PhantomData,
        }
    }

    /// Enter a known compartment.
    pub fn enter_known_compartment<'a, 'b, C, T>(&'a mut self, managed: JSManaged<'b, C, T>) -> JSContext<Entered<'a, C, T::Aged, S>> where
        T: JSRootable<'a>,
        C: Compartment,
    {
        debug!("Entering compartment.");
        let ac = JSAutoCompartment::new(self.jsapi_context, managed.to_jsobject());
        JSContext {
            jsapi_context: self.jsapi_context,
            global_js_object: managed.to_heap_object(),
            global_raw: managed.to_raw_native() as *mut (),
            auto_compartment: Some(ac),
            runtime: None,
            marker: PhantomData,
        }
    }

    /// Enter a compartment.
    pub fn enter_unknown_compartment<'a, 'b, C, T>(&'a mut self, managed: JSManaged<'b, C, T>) -> JSContext<Entered<'a, BOUND<'a>, T::Aged, S>> where
        T: JSRootable<'a>,
    {
        debug!("Entering compartment.");
        let ac = JSAutoCompartment::new(self.jsapi_context, managed.to_jsobject());
        JSContext {
            jsapi_context: self.jsapi_context,
            global_js_object: managed.to_heap_object(),
            global_raw: managed.to_raw_native() as *mut (),
            auto_compartment: Some(ac),
            runtime: None,
            marker: PhantomData,
        }
    }

    /// Give ownership of data to JS.
    /// This allocates JS heap, which may trigger GC.
    pub fn manage<'a, C, T>(&'a mut self, value: T) -> JSManaged<'a, C, T::Aged> where
        S: CanAlloc + InCompartment<C>,
        C: Compartment,
        T: JSTraceable + JSInitializable + JSRootable<'a>,
    {
        JSManaged::new(self, value)
    }

    /// Give ownership of data to JS.
    /// This allocates JS heap, which may trigger GC.
    pub fn snapshot_manage<'a, C, T>(&'a mut self, value: T) -> (JSContext<Snapshotted<'a, S>>, JSManaged<'a, C, T::Aged>) where
        S: CanAlloc + InCompartment<C>,
        C: Compartment,
        T: JSTraceable + JSInitializable + JSRootable<'a>,
    {
        let jsapi_context = self.jsapi_context;
        let global_js_object = self.global_js_object;
        let global_raw =  self.global_raw;
        let managed = self.manage(value);

        debug!("Creating snapshot while managing.");
        let snapshot = JSContext {
            jsapi_context: jsapi_context,
            global_js_object: global_js_object,
            global_raw: global_raw,
            auto_compartment: None,
            runtime: None,
            marker: PhantomData,
        };
        (snapshot, managed)
    }

    /// Create a compartment
    pub fn create_compartment<'a, T>(&'a mut self) -> JSContext<Initializing<'a, BOUND<'a>, T>> where
        S: CanAlloc + CanAccess,
        T: JSInitializable + JSTraceable,
    {
        debug!("Creating compartment.");
        let value: Option<T> = None;
        let boxed_value: Box<JSManageable> = Box::new(value);
        let fat_value: [*const libc::c_void; 2] = unsafe { mem::transmute(boxed_value) };

        let classp = unsafe { T::Init::global_classp() };
        let principals = unsafe { T::Init::global_principals() };
        let hook_options = unsafe { T::Init::global_hook_option() };
        let options = unsafe { T::Init::global_options() };
        let properties = unsafe { T::Init::properties() };
        let functions = unsafe { T::Init::functions() };

        let boxed_jsobject = Box::new(Heap::default());
        debug!("Boxed global {:p}", boxed_jsobject);
        let unboxed_jsobject = unsafe { JS_NewGlobalObject(self.jsapi_context, classp, principals, hook_options, &options) };
        debug!("Unboxed global {:p}", unboxed_jsobject);
        assert!(!unboxed_jsobject.is_null());
        boxed_jsobject.set(unboxed_jsobject);

        // TODO: can we be sure that this won't trigger GC? Or do we need to root the boxed object?
        debug!("Entering compartment.");
        let ac = JSAutoCompartment::new(self.jsapi_context, boxed_jsobject.get());

        // Save a pointer to the native value in a private slot
        unsafe { JS_SetReservedSlot(boxed_jsobject.get(), 0, PrivateValue(fat_value[0])) };
        unsafe { JS_SetReservedSlot(boxed_jsobject.get(), 1, PrivateValue(fat_value[1])) };

        // Keep a hash map of all the class prototypes
        // TODO: Fix this space leak!
        let prototypes: Box<HashMap<TypeId, Box<Heap<*mut JSObject>>>> = Box::new(HashMap::new());
        unsafe { JS_SetReservedSlot(boxed_jsobject.get(), 2, PrivateValue(Box::into_raw(prototypes) as *const _)) };

        // Define the properties and functions of the global
        if !properties.is_null() { unsafe { JS_DefineProperties(self.jsapi_context, boxed_jsobject.handle(), properties) }; }
        if !functions.is_null() { unsafe { JS_DefineFunctions(self.jsapi_context, boxed_jsobject.handle(), functions) }; }

        // TODO: can we be sure that this won't trigger GC? Or do we need to root the boxed object?
        debug!("Initializing compartment.");
        unsafe { T::Init::js_init_global(self.jsapi_context, boxed_jsobject.handle()) };

        debug!("Created compartment.");
        JSContext {
            jsapi_context: self.jsapi_context,
            global_js_object: Box::into_raw(boxed_jsobject),
            global_raw: fat_value[0] as *mut (),
            auto_compartment: Some(ac),
            runtime: None,
            marker: PhantomData,
        }
    }

    /// Finish initializing a JS Context
    pub fn global_manage<'a, C, T>(self, value: T) -> JSContext<Initialized<'a, C, T::Aged>> where
        S: IsInitializing<'a, C, T>,
        T: JSTraceable + JSRootable<'a> + JSTransplantable<C, C, Transplanted = T>,
    {
        debug!("Managing native global.");
        let raw = self.global_raw as *mut Option<T>;
        let uninitialized = unsafe { mem::replace(&mut *raw, Some(value)) };
        mem::forget(uninitialized);

        debug!("Initialized compartment.");
        JSContext {
            jsapi_context: self.jsapi_context,
            global_js_object: self.global_js_object,
            global_raw: self.global_raw,
            auto_compartment: self.auto_compartment,
            runtime: self.runtime,
            marker: PhantomData,
        }
    }

    /// Get the object we entered the current compartment via
    pub fn entered<'a, C, T>(&self) -> JSManaged<'a, C, T> where
        S: IsEntered<'a, C, T>
    {
        unsafe { JSManaged::from_raw(self.global_js_object, self.global_raw) }
    }

    /// Get the global of an initialized context.
    pub fn global<'a, C, T>(&self) -> JSManaged<'a, C, T> where
        S: IsInitialized<'a, C, T>
    {
        unsafe { JSManaged::from_raw(self.global_js_object, self.global_raw) }
    }

    /// Create a new root.
    pub fn new_root<'a, 'b, T>(&'b mut self) -> JSRoot<'a, T> {
        JSRoot::new(self)
    }

    pub fn cx(&self) -> *mut jsapi::JSContext {
        self.jsapi_context
    }

    pub fn rt(&self) -> *mut jsapi::JSRuntime {
        unsafe { JS_GetRuntime(self.jsapi_context) }
    }

    pub fn gc(&mut self) where
        S: CanAlloc,
    {
        unsafe { JS_GC(self.rt()); }
    }

    pub fn prototype_of<T>(&mut self) -> Handle<*mut JSObject> where
        T: JSInitializable,
    {
        let global = unsafe { &*self.global_js_object }.handle();
        let prototypes = unsafe { &mut *(JS_GetReservedSlot(global.get(), 2).to_private() as *mut HashMap<TypeId, Box<Heap<*mut JSObject>>>) };
        prototypes.entry(TypeId::of::<T::Init>()).or_insert_with(|| {
            let boxed = Box::new(Heap::default());
            boxed.set(unsafe { T::Init::js_init_class(self.jsapi_context, global) });
            boxed
        }).handle()
    }

    pub fn evaluate<C>(&mut self, code: &str) -> Result<JSVal, JSEvaluateErr> where
        S: InCompartment<C>,
    {
        let cx = self.jsapi_context;

        let options = unsafe { NewCompileOptions(cx, &0, 0) };

        let code_utf16: Vec<u16> = code.encode_utf16().collect();
        let code_ptr = &code_utf16[0] as *const u16;
        let code_len = code_utf16.len() as usize;

        let mut result = UndefinedValue();
        let result_mut = unsafe { MutableHandle::from_marked_location(&mut result) };

        unsafe { Evaluate2(cx, options, code_ptr, code_len, result_mut) };
        self.take_pending_exception()?;

        Ok(result)
    }

    pub fn take_pending_exception(&mut self) -> Result<(), JSEvaluateErr> {
        let cx = self.jsapi_context;
        if !unsafe { JS_IsExceptionPending(cx) } { return Ok(()); }

        let ref mut exn_ref = UndefinedValue();
        let exn_mut = unsafe { MutableHandle::from_marked_location(exn_ref) };
        let _exn_handle = exn_mut.handle();

        unsafe { JS_GetPendingException(cx, exn_mut) };
        // TODO: include the exception in the error report
        unsafe { JS_ClearPendingException(cx) };

        Err(JSEvaluateErr::JSException)
    }
}

pub unsafe fn jscontext_called_from_js(cx: *mut jsapi::JSContext) -> JSContext<FromJS> {
    JSContext {
        jsapi_context: cx,
        global_js_object: ptr::null_mut(),
        global_raw: ptr::null_mut(),
        auto_compartment: None,
        runtime: None,
        marker: PhantomData,
    }
}
