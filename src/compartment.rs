use super::InCompartment;
use super::JSContext;
use super::JSManaged;
use super::JSString;

use std::fmt::Debug;
use std::hash::Hash;
use std::marker::PhantomData;

/// A marker trait for JS compartments.
/// We mark it as `Copy` so that anything that uses `[#derive{Copy)]` will be copyable.
/// Ditto `Eq` and `Hash`.
pub trait Compartment: Copy + Debug + Eq + Hash {}

/// A wildcard compartment name.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct SOMEWHERE(());

/// A compartment name that remembers the lifetime it was bound for.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct BOUND<'a>(PhantomData<&'a mut &'a ()>);
impl<'a> Compartment for BOUND<'a> {}

/// Data which can be transplanted from compartment C into compartment D.
pub unsafe trait JSTransplantable<C, D> {
    type Transplanted;
    fn is_in_compartment<S>(&self, _cx: &JSContext<S>) -> bool where
        S: InCompartment<D>;
}

unsafe impl<C, D> JSTransplantable<C, D> for String {
    type Transplanted = String;
    fn is_in_compartment<S>(&self, _cx: &JSContext<S>) -> bool where
        S: InCompartment<D>
    {
        true
    }
}

unsafe impl<C, D> JSTransplantable<C, D> for usize {
    type Transplanted = usize;
    fn is_in_compartment<S>(&self, _cx: &JSContext<S>) -> bool where
        S: InCompartment<D>
    {
        true
    }
}

unsafe impl<C, D> JSTransplantable<C, D> for () {
    type Transplanted = ();
    fn is_in_compartment<S>(&self, _cx: &JSContext<S>) -> bool where
        S: InCompartment<D>
    {
        true
    }
}

unsafe impl<C, D, T> JSTransplantable<C, D> for Option<T> where
    T: JSTransplantable<C, D>
{
    type Transplanted = Option<T::Transplanted>;
    fn is_in_compartment<S>(&self, cx: &JSContext<S>) -> bool where
        S: InCompartment<D>
    {
        self.iter().all(|this| this.is_in_compartment(cx))
    }
}

unsafe impl<C, D, T> JSTransplantable<C, D> for Vec<T> where
    T: JSTransplantable<C, D>
{
    type Transplanted = Vec<T::Transplanted>;
    fn is_in_compartment<S>(&self, cx: &JSContext<S>) -> bool where
        S: InCompartment<D>
    {
        self.iter().all(|this| this.is_in_compartment(cx))
    }
}

unsafe impl<'a, C, D, T> JSTransplantable<C, D> for JSManaged<'a, C, T> where
    T: JSTransplantable<C, D>
{
    type Transplanted = JSManaged<'a, D, T::Transplanted>;
    fn is_in_compartment<S>(&self, cx: &JSContext<S>) -> bool where
        S: InCompartment<D>
    {
        self.in_compartment(cx).is_some()
    }
}


unsafe impl<'a, C, D> JSTransplantable<C, D> for JSString<'a, C> {
    type Transplanted = JSString<'a, D>;
    fn is_in_compartment<S>(&self, _cx: &JSContext<S>) -> bool where
        S: InCompartment<D>
    {
        // Rather annoyingly the Rust jsapi bindings don't export
        // GetStringZone which we'd need in order to implement this properly
        false
    }
}

