// TODO: just for now
#![allow(unused_imports, unused, unused_mut, dead_code)]
extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{self, TokenStream as TokenStream2};
use syn::{self, parse_macro_input, Item, Ident};
use syn::spanned::Spanned;
use quote::{quote, format_ident};


fn mk_error<D: std::fmt::Display>(span: proc_macro2::Span, msg: D) -> TokenStream2 {
    syn::Error::new(span, msg).to_compile_error()
}

fn extract_bit_size(field_type: &syn::Type) -> Result<TokenStream2, TokenStream2> {
    if let syn::Type::Path(syn::TypePath {path, ..}) = field_type {
        Ok(quote! { #path::BITS })
    } else {
        Err(mk_error(field_type.span(), "Expected a TypePath"))
    }
}


#[proc_macro_attribute]
pub fn bitfield(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let input = parse_macro_input!(input as Item);

    let src_visibility: syn::Visibility;
    let src_ident: syn::Ident;
    let mut total_bit_size: TokenStream2;

    match input {
        Item::Struct(item_struct) => {
            // get basic struct information
            src_visibility = item_struct.vis.clone();
            src_ident = item_struct.ident.clone();

            let mut fields;
            if let syn::Fields::Named(syn::FieldsNamed { named , ..}) = item_struct.fields {
                fields = named;
            } else {
                return mk_error(item_struct.span(), "The #[bitfield] attribute can just be applied to structs with named fields").into();
            }

            // calculate the total bit size of the struct
            total_bit_size = quote! { 0u8 };
            for field in fields {
                match extract_bit_size(&field.ty) {
                    Ok(field_size) => total_bit_size = quote! { #total_bit_size + #field_size },
                    Err(error) => return error.into(),
                }
            }
            total_bit_size = quote! { ((#total_bit_size)/8) as usize };
        }
        _ => return mk_error(input.span(), "The #[bitfield] attribute can just be applied to structs").into()
    }

    (quote! {
        #[repr(C)]
        #src_visibility struct #src_ident {
            data: [u8; #total_bit_size]
        }
    }).into()
}
