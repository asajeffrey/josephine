/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use super::ffi::JSEvaluateErr;
use super::ffi::UNSAFE;
use super::CanAlloc;
use super::InCompartment;
use super::JSContext;
use super::JSLifetime;
use super::JSTraceable;

use js::glue::CallStringTracer;

use js::jsapi;
use js::jsapi::Heap;
use js::jsapi::JSTracer;
use js::jsapi::JS_FlattenString;
use js::jsapi::JS_GetLatin1FlatStringChars;
use js::jsapi::JS_GetStringLength;
use js::jsapi::JS_GetTwoByteFlatStringChars;
use js::jsapi::JS_NewStringCopyN;
use js::jsapi::JS_NewUCStringCopyN;
use js::jsapi::JS_StringHasLatin1Chars;
use js::jsapi::JS::GCTraceKindToAscii;
use js::jsapi::JS::HandleValue;
use js::jsapi::JS::TraceKind;

use js::jsapi::JS::Value;

use js::jsval::StringValue;

use std::char;
use std::fmt;
use std::fmt::Display;
use std::fmt::Write;
use std::marker::PhantomData;
use std::ptr;
use std::slice;
use std::str;

/// The type of JS-managed strings in the same zone as compartment `C`, with lifetime `a`.
/// Rust is much happier with flat string representations, so we flatten
/// strings when they come into Rust.
pub struct JSString<'a, C> {
    // This string is always flattened, but we're not allowed to build a `Heap<*mut JSFlatString>`.
    js_string: *mut Heap<*mut jsapi::JSString>,
    marker: PhantomData<(&'a (), C)>,
}

impl<'a, C> Clone for JSString<'a, C> {
    fn clone(&self) -> JSString<'a, C> {
        JSString {
            js_string: self.js_string,
            marker: PhantomData,
        }
    }
}

impl<'a, C> Copy for JSString<'a, C> {}

impl<'a, C> JSString<'a, C> {
    pub fn to_jsstring(self) -> *mut jsapi::JSString {
        unsafe { &*self.js_string }.get()
    }

    pub fn to_jsflatstring(self) -> *mut jsapi::JSFlatString {
        self.to_jsstring() as *mut jsapi::JSFlatString
    }

    pub fn to_jsval(self) -> Value {
        StringValue(unsafe { &*self.to_jsstring() })
    }

    pub fn len(self) -> usize {
        unsafe { JS_GetStringLength(self.to_jsstring()) }
    }

    pub fn has_latin1_chars(self) -> bool {
        unsafe { JS_StringHasLatin1Chars(self.to_jsstring()) }
    }

    pub unsafe fn get_latin1_chars(self) -> &'a str {
        let nogc = ptr::null_mut(); // TODO: build an AutoCheckCannotGC?
        let raw = JS_GetLatin1FlatStringChars(nogc, self.to_jsflatstring());
        str::from_utf8_unchecked(slice::from_raw_parts(raw, self.len()))
    }

    pub unsafe fn get_two_byte_chars(self) -> &'a [u16] {
        let nogc = ptr::null_mut(); // TODO: build an AutoCheckCannotGC?
        let raw = JS_GetTwoByteFlatStringChars(nogc, self.to_jsflatstring());
        slice::from_raw_parts(raw, self.len())
    }

    pub fn contents(self) -> JSStringContents<'a> {
        if self.has_latin1_chars() {
            JSStringContents::Latin1(unsafe { self.get_latin1_chars() })
        } else {
            JSStringContents::TwoByte(unsafe { self.get_two_byte_chars() })
        }
    }

    pub unsafe fn from_jsstring(js_string: *mut jsapi::JSString) -> JSString<'a, C> {
        // TODO: these boxes never get deallocated, which is a space leak!
        let boxed = Box::new(Heap::default());
        boxed.set(js_string);
        JSString {
            js_string: Box::into_raw(boxed),
            marker: PhantomData,
        }
    }

    pub unsafe fn from_latin1_unchecked<S>(
        cx: &'a mut JSContext<S>,
        string: &str,
    ) -> JSString<'a, C>
    where
        S: CanAlloc + InCompartment<C>,
    {
        JSString::from_jsstring(JS_NewStringCopyN(
            cx.cx(),
            string as *const str as *const i8,
            string.len(),
        ))
    }

    pub fn from_latin1<S>(cx: &'a mut JSContext<S>, string: &str) -> Option<JSString<'a, C>>
    where
        S: CanAlloc + InCompartment<C>,
    {
        if string.bytes().all(|byte| byte < 128) {
            Some(unsafe { JSString::from_latin1_unchecked(cx, string) })
        } else {
            None
        }
    }

    pub fn from_twobyte<S>(cx: &'a mut JSContext<S>, slice: &[u16]) -> JSString<'a, C>
    where
        S: CanAlloc + InCompartment<C>,
    {
        unsafe {
            JSString::from_jsstring(JS_NewUCStringCopyN(
                cx.cx(),
                &slice[0] as *const u16,
                slice.len(),
            ))
        }
    }

    pub fn from_str<S>(cx: &'a mut JSContext<S>, string: &str) -> JSString<'a, C>
    where
        S: CanAlloc + InCompartment<C>,
    {
        if string.bytes().all(|byte| byte < 128) {
            unsafe { JSString::from_latin1_unchecked(cx, string) }
        } else {
            let utf16: Vec<u16> = string.encode_utf16().collect();
            JSString::from_twobyte(cx, &*utf16)
        }
    }

    pub fn clone_in<S, D>(self, cx: &'a mut JSContext<S>) -> JSString<'a, D>
    where
        S: CanAlloc + InCompartment<D>,
    {
        match self.contents() {
            JSStringContents::Latin1(string) => unsafe {
                JSString::from_latin1_unchecked(cx, string)
            },
            JSStringContents::TwoByte(slice) => JSString::from_twobyte(cx, slice),
        }
    }
}

impl<'a, C> Display for JSString<'a, C> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.contents().fmt(f)
    }
}

unsafe impl<'a, C> JSTraceable for JSString<'a, C> {
    unsafe fn trace(&self, trc: *mut JSTracer) {
        debug!("Tracing JSString {:p}.", self.js_string);
        CallStringTracer(trc, self.js_string, GCTraceKindToAscii(TraceKind::String));
    }
}

unsafe impl<'a, 'b, C> JSLifetime<'a> for JSString<'b, C> {
    type Aged = JSString<'a, C>;
}

pub enum JSStringContents<'a> {
    Latin1(&'a str),
    TwoByte(&'a [u16]),
}

impl<'a> Display for JSStringContents<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            JSStringContents::Latin1(ref string) => f.write_str(string),
            JSStringContents::TwoByte(ref slice) => char::decode_utf16(slice.iter().cloned())
                .map(|ch| ch.unwrap_or(char::REPLACEMENT_CHARACTER))
                .map(|ch| f.write_char(ch))
                .find(Result::is_err)
                .unwrap_or(Ok(())),
        }
    }
}

pub unsafe fn jsstring_called_from_js<'a>(
    cx: *mut jsapi::JSContext,
    value: HandleValue,
) -> Result<JSString<'a, UNSAFE>, JSEvaluateErr> {
    if !value.is_string() {
        return Err(JSEvaluateErr::NotAString);
    }

    let flattened = JS_FlattenString(cx, value.to_string());

    // TODO: these boxes never get deallocated, which is a space leak!
    let boxed = Box::new(Heap::default());
    boxed.set(flattened as *mut jsapi::JSString);

    Ok(JSString {
        js_string: Box::into_raw(boxed),
        marker: PhantomData,
    })
}
