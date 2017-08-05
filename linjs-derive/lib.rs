extern crate proc_macro;
extern crate syn;

#[macro_use]
extern crate quote;

use proc_macro::TokenStream;

#[proc_macro_derive(JSManageable)]
pub fn derive_js_manageable(input: TokenStream) -> TokenStream {
    let s = input.to_string();
    let ast = syn::parse_derive_input(&s).unwrap();
    let gen = impl_js_manageable(&ast);
    gen.parse().unwrap()
}

fn impl_js_manageable(ast: &syn::DeriveInput) -> quote::Tokens {
    let name = &ast.ident;
    let (_, ty_generics, _) = ast.generics.split_for_impl();

    // we assume there's only one lifetime param, not named 'b
    assert!(ast.generics.lifetimes.len() == 1, "deriving JSManageable requires a single lifetime");    
    
    let impl_lifetime = &ast.generics.lifetimes[0].lifetime.ident;
    assert!(impl_lifetime != "'b", "deriving JSManageable requires the lifetime to not be named 'b");

    // we assume there's at least 1 generic type param: the JSCompartment
    assert!(ast.generics.ty_params.len() >= 1);
    let compartment = &ast.generics.ty_params[0].ident;
    let impl_generics = ast.generics.ty_params.iter().map(|ty| quote! { #ty });
    let impl_generics = quote! { #(#impl_generics),* };

    // append the lifetime constraints to the generic type parameters following the JSCompartment
    let lifetime_constraints = ast.generics.ty_params.iter().skip(1).map(|ty| {
        let ident = &ty.ident;
        quote! { #ident: 'b }
    });
    let where_clause_predicates = ast.generics.where_clause.predicates.iter().map(|pred| quote! { #pred });
    let where_clause_items = lifetime_constraints.chain(where_clause_predicates).collect::<Vec<_>>();
    let where_clause = if where_clause_items.is_empty() {
        quote! { }
    } else {
        quote! { where #(#where_clause_items),* }
    };

    // the `Aged` associated type params are the ty_params without their bounds
    let aged_ty_params = ast.generics.ty_params.iter().map(|ty| {
        let ident = &ty.ident;
        quote! { #ident }
    });
    let aged_ty_params = quote! { #(#aged_ty_params),* };

    quote! {
        unsafe impl<#impl_lifetime, 'b, #impl_generics> JSManageable<'b, #compartment> for #name #ty_generics #where_clause {
            type Aged = #name<'b, #aged_ty_params>;
        }
    }
}
