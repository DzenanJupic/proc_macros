// TODO: just for now
#![allow(unused_imports, unused, unused_mut, dead_code)]
extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{self, TokenStream as TokenStream2};
use syn::{self, parse_macro_input, Item, Ident};
use syn::spanned::Spanned;
use quote::{quote, format_ident};


#[inline]
fn mk_error<D: std::fmt::Display>(span: proc_macro2::Span, msg: D) -> TokenStream2 {
    syn::Error::new(span, msg).to_compile_error()
}

fn get_field_type_path(field_type: &syn::Type) -> Result<TokenStream2, TokenStream2> {
    if let syn::Type::Path(syn::TypePath {path, ..}) = field_type {
        Ok(quote! { #path })
    } else {
        Err(mk_error(field_type.span(), "Expected a TypePath"))
    }
}

#[inline]
fn field_bits(path: &TokenStream2) -> TokenStream2 {
    // path is the path of the bit enum (B<N>)
    // BITS is the trait constant of Specifier that was implemented for B<N>
    // EXAMPLE: B42 -> B42::BITS = 42
    quote! { #path::BITS }
}

#[inline]
fn field_data_type(path: &TokenStream2) -> TokenStream2 {
    // path is the path of the bit enum (B<N>)
    // Specifier is the trait that was implemented for B<N>
    // UINT is the trait type of Specifier that was implemented for B<N>
    // EXAMPLE: B42 -> B42::UINT = u64     // (the next greater multiple of one byte or 8 bits)
    quote! { <#path as Specifier>::UINT }
}

#[inline]
fn field_byte_index(bit_sizes: &Vec<TokenStream2>) -> TokenStream2 {
    // bit_sizes is a vector all the bite sizes of the previous fields (generated through [`field_bit`]
    // EXAMPLE:
    //      #[bitfield] struct S { field1: B42, field2: B22}
    // field_byte_index(field1) -> 0 | field_byte_index(field2) -> 6
    quote! { ((0u8 #( + #bit_sizes )*) as f64 / 8).cleil() as usize }
}

fn parse_struct_fields(fields: syn::punctuated::Iter<syn::Field>, bit_sizes: &mut Vec<TokenStream2>, getter_methods: &mut Vec<TokenStream2>, setter_methods: &mut Vec<TokenStream2>) -> Result<(), TokenStream2> {
    // iterate over the struct fields and
    // - calculate the total bit size of the struct
    // - create getter and setter methods for the resulting bitfield struct
    for (field_index, field) in fields.enumerate() {
        let field_ident= field.ident.clone().unwrap(); // can unwrap cause unnamed fields are filtered out above
        let ref field_type = field.ty;
        let field_type_path = match get_field_type_path(field_type) {
            Ok(type_path) => type_path,
            Err(error) => return Err(error),
        };
        let field_data_type = field_data_type(&field_type_path);
        let field_bits = field_bits(&field_type_path);

        // create the getter and setter method names for the current field
        let getter_ident = format_ident!("get_{}", field_ident);
        let setter_ident = format_ident!("set_{}", field_ident);

        // create the getter and setter methods for the current field
        getter_methods.push(quote! {
            pub fn #getter_ident(&self) -> #field_data_type {
                // let start =
                self.get_bytes_as_int(&self, start: usize, bytes: usize)
                let bytes: u8 = (#field_bits as f64/8f64).ceil() as u8;
                self.data[#field_index] as #field_data_type
            }
        });
        setter_methods.push(quote! {
            pub fn #setter_ident(&mut self, value: #field_data_type) {
                self.data[#field_index] = value as u64;
            }
        });

        bit_sizes.push(field_bits);
    }
    Ok(())
}

#[proc_macro_attribute]
pub fn bitfield(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let input = parse_macro_input!(input as Item);

    let src_visibility: syn::Visibility;
    let src_ident: syn::Ident;

    let mut bit_sizes: Vec<TokenStream2> = Vec::new();
    let total_byte_size: TokenStream2;
    let mut getter_methods: Vec<TokenStream2> = Vec::new();
    let mut setter_methods: Vec<TokenStream2> = Vec::new();

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

            let parsed = parse_struct_fields(fields.iter(), &mut bit_sizes, &mut getter_methods, &mut setter_methods);
            if let Err(error) = parsed {
                return error.into();
            }
            total_byte_size = quote! { (( 0u8 #( + #bit_sizes as u64 )* )/8u64) as usize };
        }
        _ => return mk_error(input.span(), "The #[bitfield] attribute can just be applied to structs").into()
    }

    (quote! {
        #[repr(C)]
        #src_visibility struct #src_ident {
            data: [u8; #total_byte_size]
        }

        impl #src_ident {
            pub fn new() -> Self {
                Self {
                    data: [0u8; #total_byte_size]
                }
            }

            #( #getter_methods )*
            #( #setter_methods )*

            fn get_bytes_as_int<T: ::std::marker::Copy>(&self, start: usize, bytes: usize) -> T {
                unsafe {
                    ::std::mem::transmut::<&[u8; bytes], T>(&self.data[start..(start+bytes)])
                }
            }
            fn set_int_as_bytes<T: ::std::marker::Copy>(&mut self, start: usize, int: T) {
                // TODO: This will probably not work cause size of is (probably not constant)
                // TRY: const instead of let
                // TRY: <Self as Specifier>::UINT in size_of
                // TRY: [u8; (<Self as Specifier>::BITS as f64/8f64).ceil() as usize]
                // TRY: same like above but with a BYTES const in Specifier
                const size_of: usize = ::std::mem::size_of::<T>();
                let transmuted;
                unsafe {
                    transmuted = ::std::mem::transmute::<T, [u8; size_of]>();
                }
                for i in start..(start+size_of) {
                    self.data[i] = transmuted[i-start];
                }
            }

            /// This function should _never_ be called or run!
            /// It's a function that's purpose is to throw a compile error if the user declares
            /// a bitfield that has not a even number of bytes (8 bits).
            /// It works by trying to transmute a unit ( () <- this is a unit ) into a array
            /// of u8's.
            /// The length of that array is sum of the bit size of each field in bitfield modulo 8.
            /// Since all the values are known at compile time, the program won't compile if the
            /// user defines a bitfield of for example 7 bits.
            /// The big downside is, that the error message is not helpful in any way and does not
            /// enable the user to find his mistake!
            unsafe fn _unsafe_bitfield_check_modular_8_sized() {
                ::std::mem::transmute::<(), [u8; (( 0u64 #( + #bit_sizes as u64 )* )%8u64) as usize]>(());
            }
        }
    }).into()
}
