extern crate proc_macro;
extern crate syn;

#[macro_use]
extern crate quote;

use proc_macro::TokenStream;
use syn::{MetaItem, Lit, Ident};

#[proc_macro_derive(JSManageable, attributes(jsmanaged))]
pub fn derive_js_manageable(input: TokenStream) -> TokenStream {
    let s = input.to_string();
    let ast = syn::parse_derive_input(&s).unwrap();
    let gen = impl_js_manageable(&ast);  
    gen.parse().unwrap()
}

fn impl_js_manageable(ast: &syn::DeriveInput) -> quote::Tokens {
    let name = &ast.ident;

    let mut managed_name = None;
    for attr in &ast.attrs {
        if let &MetaItem::NameValue(ref ident, ref lit) = &attr.value {
            if ident == "jsmanaged" {
                managed_name = match *lit {
                    Lit::Str(ref s, _) => Some(Ident::from(s.to_string())),
                    _ => panic!("the jsmanaged attribute requires a string name")
                };
            }
        }
    }

    let managed_name = managed_name.expect("the jsmanaged attribute is required, example: #[jsmanaged=\"ManagedAlias\"]");

    quote! {
        type #managed_name<'a, C> = JSManaged<'a, C, #name<'a, C>>;

        unsafe impl<'a, 'b, C: JSCompartment> JSManageable<'b, C> for #name<'a, C> {
            type Aged = #name<'b, C>;
        }
    }
}
