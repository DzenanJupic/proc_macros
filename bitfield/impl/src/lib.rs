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

fn extract_field_byte_size(field_type: &syn::Type) -> Result<TokenStream2, TokenStream2> {
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
    let mut bit_sizes: Vec<TokenStream2> = Vec::new();
    let total_byte_size: TokenStream2;
    let mut getter_functions: Vec<TokenStream2> = Vec::new();
    let mut setter_functions: Vec<TokenStream2> = Vec::new();

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

            // iterate over the struct fields and
            // - calculate the total bit size of the struct
            // - create getter and setter methods for the resulting bitfield struct
            for (field_index, field) in fields.iter().enumerate() {
                let field_ident= field.ident.clone().unwrap(); // can unwrap cause unnamed fields are filtered out above
                let ref field_type = field.ty;

                // get the field size for the current field
                match extract_field_byte_size(field_type) {
                    Ok(field_size) => bit_sizes.push(field_size),
                    Err(error) => return error.into(),
                }

                // create the getter and setter method names for the current field
                let getter_ident = format_ident!("get_{}", field_ident);
                let setter_ident = format_ident!("set_{}", field_ident);

                // create the getter and setter methods for the current field
                getter_functions.push(quote! {
                    fn #getter_ident(&self) -> u64 {
                        self.data[#field_index] as u64
                    }
                });
                setter_functions.push(quote! {
                    fn #setter_ident(&mut self, value: u64) {
                        self.data[#field_index] = value as u8;
                    }
                });
            }
            total_byte_size = quote! { (( 0u8 #( + #bit_sizes )* )/8u8) as usize };
        }
        _ => return mk_error(input.span(), "The #[bitfield] attribute can just be applied to structs").into()
    }

    (quote! {
        #[repr(C)]
        #src_visibility struct #src_ident {
            data: [u8; #total_byte_size]
        }

        impl #src_ident {
            fn new() -> Self {
                Self {
                    data: [0u8; #total_byte_size]
                }
            }

            #( #getter_functions )*
            #( #setter_functions )*
        }
    }).into()
}
