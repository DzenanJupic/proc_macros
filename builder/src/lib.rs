#![feature(str_strip)]
extern crate proc_macro;

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput, Field};
use quote::{quote, format_ident};
use syn::export::Span;
use syn::spanned::Spanned;

/// returns a compile error in form of a TokenStream
fn mk_compile_error<T: std::fmt::Display>(span: Span, msg: T) -> TokenStream {
    syn::Error::new(span, msg)
        .to_compile_error()
        .into()
}
fn is_ident(ident: &str, path: &syn::Path) -> bool {
    if !path.segments.is_empty() && path.segments.last().unwrap().ident == ident {
        true
    } else {
        false
    }
}
/// extracts the type of types
/// Option<T> -> T
/// Vec<T> -> T
/// Result<T, E> -> T, E
/// String -> None
fn extract_inner_types(ty: &syn::Type) -> Option<Vec<&syn::Type>> {
    if let syn::Type::Path(syn::TypePath {ref path, ..}) = ty {
        if !path.segments.is_empty() {
            if let syn::PathArguments::AngleBracketed(ref bracketed_generics) = path.segments.last().unwrap().arguments {
                let mut ty_vec = Vec::new();

                for generic in bracketed_generics.args.iter() {
                    if let syn::GenericArgument::Type(ref ty) = generic {
                        ty_vec.push(ty);
                    }
                }

                if !ty_vec.is_empty() {
                    return Some(ty_vec);
                }
            }
        }
    } else {
        unimplemented!("Currently you can just extract Type::Path")
    }
    None
}
/// a wrapper around extract_inner_types that makes sure that the type has a specific ident and
/// a specific number of types
fn unwrap_types<'a>(ident: &str, types: usize, ty: &'a syn::Type) -> Option<Vec<&'a syn::Type>> {
    if let syn::Type::Path(syn::TypePath {ref path, ..}) = ty {
        if is_ident(&ident, &path) && !path.segments.is_empty() {
            let inner = extract_inner_types(ty);
            let len;
            if let Some(ref inner) = inner {
                len = inner.len()
            } else {
                len = 0;
            }
            if len == types {
                return inner;
            } else {
                panic!("Type `{}` has more inner Types then expected! (expected: {} | got: {})", ident, types, len);
            }
        } else if !is_ident(&ident, &path) {
            let res_ident = path.get_ident();
            if let Some(res_ident) = res_ident {
                panic!("Expected Type `{:?}`, got `{:?}`", ident, res_ident);
            } else {
                panic!("Expected Type `{:?}`, but {:?} has no type!", ident, path);
            }
        }
    }
    None
}
/// a wrapper around unwrap_types
fn unwrap_option(ty: &syn::Type) -> &syn::Type {
    unwrap_types("Option", 1, ty).unwrap()[0]
}
/// a wrapper around unwrap_types
fn unwrap_vec(ty: &syn::Type) -> &syn::Type {
    unwrap_types("Vec", 1, ty).unwrap()[0]
}
/// a wrapper around extract_inner_types
fn is_type(ident: &str, inner_types: usize, ty: &syn::Type) -> bool {
    if let syn::Type::Path(syn::TypePath {ref path, ..}) = ty {
        if is_ident(&ident, &path) && path.segments.len() == inner_types { // TODO: Currently: checks if path.segments == inner_types / shouldn't it be extract_types() == inner_types ?
            return true;
        }
    }
    false
}
/// a wrapper around is_type
fn is_option(ty: &syn::Type) -> bool {
    is_type("Option", 1, ty)
}
/// a wrapper around is_type
fn is_vec(ty: &syn::Type) -> bool {
    is_type("Vec", 1, ty)
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive_builder(input: TokenStream) -> TokenStream {

    /* FUNCTIONS, ENUMS & STRUCTS */
    enum BuilderAttrs<'a> {
        Each(syn::Ident, &'a syn::Type)
    }
    /// a Field-attribute parser
    fn parse_attrs(field: &syn::Field) -> Result<Vec<BuilderAttrs>, TokenStream> {
        let mut builder_attrs = Vec::new();
        for attr in field.attrs.iter() {
            let args: syn::Meta;
            match attr.parse_args() {
                Ok(a) => args = a,
                Err(e) => return Err(e.to_compile_error().into())
            }

            // #[builder(...)] attribute
            if attr.path.segments.len() == 1 && attr.path.segments[0].ident == "builder" {
                if let syn::Meta::NameValue(mnv) = &args {
                    if mnv.path.segments.len() == 1 {
                        match &mnv.path.segments[0].ident {

                            // #[builder(each = ...)]
                            i if i == "each" => {
                                if let syn::Lit::Str(ref str) = mnv.lit {
                                    let str = str.value();
                                    let ty = unwrap_vec(&field.ty);
                                    builder_attrs.push(BuilderAttrs::Each(syn::Ident::new(&str, attr.span()), ty));
                                } else {
                                    return Err(mk_compile_error(mnv.lit.span(), format!("Expected string, got {:#?}\nhelp: Consider #[builder(each = \"...\")]", &mnv.lit)));
                                }
                            }

                            _ => return Err(mk_compile_error(attr.path.span().join(attr.tokens.span()).unwrap(), "expected `builder(each = \"...\")`"))
                        }
                        continue;
                    }
                }
                return Err(mk_compile_error(attr.span(), "Invalid builder-attribute values\nhelp: Consider writing #[builder(... = \"...\")]"));
            } else {
                return Err(mk_compile_error(attr.span(), "Unknown attribute\navailable: [ builder(...) ]"));
            }
        }
        Ok(builder_attrs)
    }
    /// takes a iterator over some syn::Fields and return a Vec<([field_ident], [field_type], Vec<[field_attributes]>)>
    fn parse_fields<'a, I: Iterator<Item = &'a Field>>(fields: I) -> Result<Vec<(syn::Ident, &'a syn::Type, Vec<BuilderAttrs<'a>>)>, TokenStream> {
        let mut fields_vec = Vec::new();
        for field in fields.into_iter() {
            fields_vec.push((field.ident.clone().unwrap(), &field.ty, parse_attrs(&field)? ));
        }
        Ok(fields_vec)
    }


    /* CODE */
    let input = parse_macro_input!(input as DeriveInput);

    let src_ident = input.ident;
    let src_fields;
    // check if input is a struct with named fields
    if let syn::Data::Struct(syn::DataStruct {fields: syn::Fields::Named(syn::FieldsNamed {ref named, ..}), ..}) = input.data {
        let parsed = parse_fields(named.iter());
        if let Err(err) = parsed {
            return err;
        }
        src_fields = parsed.ok().unwrap();
    } else {
        return mk_compile_error(src_ident.span(), "A Builder can just be applied to structs with named fields");
    }

    let builder_ident = format_ident!("{}Builder", src_ident);
    let mut builder_fields = Vec::new();
    let mut setter_methods = Vec::new();
    let mut initial_field_values = Vec::new();
    let mut build_values = Vec::new();


    for field in src_fields {
        let ident = field.0;
        let ty = field.1;
        let attrs = field.2;

        let mut each = None;
        for attr in attrs {
            match attr {
                BuilderAttrs::Each(i, t) => {
                    if !is_vec(&ty) {
                        return mk_compile_error(i.span(), "The each attribute is just for Vec");
                    }
                    each = Some((i, t));
                }
            }
        }

        if is_option(&ty) { // all fields of type option
            if is_vec(extract_inner_types(&ty).unwrap()[0]) { // checks if a option wraps a Vec<_>, what is not allowed
                return mk_compile_error(ident.span(), "The field {} contains a Option<Vec<_>>\nPlease use a empty Vec<_> as None instead");
            }
            let inner_ty = unwrap_option(&ty);

            builder_fields.push(quote! { #ident: #ty });
            setter_methods.push(quote! {
                pub fn #ident(&mut self, #ident: #inner_ty) -> &mut Self {
                    self.#ident = ::std::option::Option::Some(#ident);
                    self
                }
            });
            initial_field_values.push(quote! { #ident: None });
            build_values.push(quote! { #ident: self.#ident.clone() });
        } else { // all fields not a Option<_>
            if is_vec(&ty) { // all fields that are of type Vec<_>
                builder_fields.push(quote! { #ident: #ty });
                let mut each_conflicts = false;
                if let Some((i, t)) = each {
                    setter_methods.push(quote! {
                        pub fn #i(&mut self, #i: #t) -> &mut Self {
                            self.#ident.push(#i);
                            self
                        }
                    });
                    if i == ident {
                        each_conflicts = true;
                    }
                }
                if !each_conflicts { // checks if the builder (#[builder(each = "...")]) conflicts with the real name
                    setter_methods.push(quote! {
                        pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                            self.#ident = #ident;
                            self
                        }
                    });
                }
                initial_field_values.push(quote! { #ident: ::std::vec::Vec::new() });
                build_values.push(quote! { #ident: self.#ident.clone() });
            } else { // all fields left
                builder_fields.push(quote! { #ident: ::std::option::Option<#ty> });
                setter_methods.push(quote! {
                    pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = ::std::option::Option::Some(#ident);
                        self
                    }
                });
                initial_field_values.push(quote! { #ident: ::std::option::Option::None });
                build_values.push(quote! { #ident: self.#ident.clone().ok_or(concat!(stringify!(#ident), " is not set"))? });
            }
        }
    }


    (quote! {
        pub struct #builder_ident {
            #( #builder_fields, )*
        }

        impl #builder_ident {
            #( #setter_methods )*
            fn build(&mut self) -> ::std::result::Result<#src_ident, ::std::boxed::Box<dyn std::error::Error>> {
                ::std::result::Result::Ok(#src_ident {
                    #( #build_values, )*
                })
            }
        }

        impl #src_ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #( #initial_field_values, )*
                }
            }
        }
    }).into()
}
