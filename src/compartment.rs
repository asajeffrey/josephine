use super::JSManaged;

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

/// Data which can be transplanted into compartment C.
pub unsafe trait JSTransplantable<C> {
    type Transplanted;
}

unsafe impl<C> JSTransplantable<C> for String { type Transplanted = String; }
unsafe impl<C> JSTransplantable<C> for usize { type Transplanted = usize; }
unsafe impl<C, T> JSTransplantable<C> for Option<T> where T: JSTransplantable<C> { type Transplanted = Option<T::Transplanted>; }
unsafe impl<C, T> JSTransplantable<C> for Vec<T> where T: JSTransplantable<C> { type Transplanted = Vec<T::Transplanted>; }
unsafe impl<'a, C, D, T> JSTransplantable<C> for JSManaged<'a, D, T> where T: JSTransplantable<C> { type Transplanted = JSManaged<'a, C, T::Transplanted>; }

