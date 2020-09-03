// TODO: just for now
#![allow(unused_imports, unused, unused_mut, dead_code)]
extern crate proc_macro;

use proc_macro::TokenStream;

use proc_macro2::{self, TokenStream as TokenStream2};
use syn::{self, Ident, Item, parse_macro_input};
use syn::spanned::Spanned;

use quote::{format_ident, quote};

#[inline]
fn mk_error<D: std::fmt::Display>(span: proc_macro2::Span, msg: D) -> TokenStream2 {
    syn::Error::new(span, msg).to_compile_error()
}

fn get_field_type_path(field_type: &syn::Type) -> Result<TokenStream2, TokenStream2> {
    if let syn::Type::Path(syn::TypePath { path, .. }) = field_type {
        Ok(quote! { #path })
    } else {
        Err(mk_error(field_type.span(), "Expected a TypePath"))
    }
}

#[inline]
fn field_data_type(path: &TokenStream2) -> TokenStream2 {
    // path is the path of the bit enum (B<N>)
    // Specifier is the trait that was implemented for B<N>
    // UINT is the trait type of Specifier that was implemented for B<N>
    // EXAMPLE: B42 -> B42::UINT = u64     // (the next greater multiple of one byte or 8 bits)
    quote! { <#path as Specifier>::UINT }
}

struct BitField {
    pub bits: TokenStream2,

    pub getter: TokenStream2,
    pub setter: TokenStream2,
}

impl BitField {
    pub fn empty() -> Self {
        Self {
            bits: TokenStream2::new(),
            getter: TokenStream2::new(),
            setter: TokenStream2::new(),
        }
    }
}

fn parse_struct_fields(fields: syn::Fields) -> Result<Vec<BitField>, TokenStream2> {
    let fields = match fields {
        syn::Fields::Named(syn::FieldsNamed { named, .. }) => named,
        _ => return Err(mk_error(fields.span(), "The #[bitfield] attribute can just be applied to structs with named fields").into())
    };

    let mut bit_fields: Vec<BitField> = Vec::new();
    let mut bit_field_bits: Vec<TokenStream2> = Vec::new();

    for field in fields.iter() {
        let field_ident = field.ident.clone().unwrap(); // safe cause checked above
        let ref field_type = field.ty;
        let field_type_path = match get_field_type_path(field_type) {
            Ok(type_path) => type_path,
            Err(err) => return Err(err)
        };
        let field_bit_size = quote! { #field_type_path::BITS as u32 };
        let field_byte_size = quote! { #field_type_path::BYTES as u32 };
        let field_bit_uint = quote! { <#field_type_path as Specifier>::UINT };

        let field_start_byte = quote! { ((0u32 #( + #bit_field_bits )*) / 8u32) };
        let field_start_bit = quote! { ((0u32 #( + #bit_field_bits )*) % 8u32) };

        bit_field_bits.push(field_bit_size.clone());

        let field_end_byte = quote! { ((0u32 #( + #bit_field_bits )* - 1u32) / 8u32) };
        let field_end_bit = quote! { ((0u32 #( + #bit_field_bits )* - 1u32) % 8u32) };

        let getter_ident = format_ident!("get_{}", field_ident);
        let field_getter = quote! {
            pub fn #getter_ident(&self) -> #field_bit_uint {
                let mut ret_int: #field_bit_uint = 0;

                let mut first_byte: u8 = self.data[#field_start_byte as usize];
                eprintln!("GET\tfirst_byte: {0} | {0:b}", first_byte);
                //clear first few bits of first byte
                first_byte = first_byte.checked_shl(#field_start_bit).unwrap_or(0);
                first_byte = first_byte.checked_shr(#field_start_bit).unwrap_or(0);
                eprintln!("GET\tcleared first few bits: {0} | {0:b}", first_byte);
                // clear last few bites of first byte
                first_byte = first_byte.checked_shr(7u32 - #field_end_bit).unwrap_or(0);
                first_byte = first_byte.checked_shl(7u32 - #field_end_bit).unwrap_or(0);
                eprintln!("GET\tcleared last few bits: {0} | {0:b}", first_byte);
                ret_int = first_byte as #field_bit_uint;

                if #field_end_byte - #field_start_byte > 0u32 {
                    eprintln!("GET\tfirst_byte: {0} | {0:b}", first_byte);
                    for i in (#field_start_byte + 1u32)..#field_end_byte {
                        let byte: u8 = self.data[i as usize];
                        ret_int = ret_int.checked_shl(8).unwrap_or(0) + byte as #field_bit_uint;
                    }

                    let mut last_byte: u8 = self.data[#field_end_byte as usize];
                    // clear the last few bits
                    last_byte = last_byte.checked_shr(7u32 - #field_end_bit).unwrap_or(0);
                    last_byte = last_byte.checked_shl(7u32 - #field_end_bit).unwrap_or(0);

                    ret_int = ret_int.checked_shr(#field_end_bit + 1u32).unwrap_or(0) + last_byte as #field_bit_uint;
                }

                // ret_int = ret_int.checked_shr(7u32 - #field_end_bit).unwrap_or(0); // move value to the right
                eprintln!("GET\tResult: {0} | {0:b}", ret_int);
                ret_int
            }
        };
        let setter_ident = format_ident!("set_{}", field_ident);
        let field_setter = quote! {
            pub fn #setter_ident(&mut self, mut value: #field_bit_uint) {
                eprintln!("SET\tgiven value: {0} | {0:b}", value);
                if value > (2u32.pow(#field_bit_size) - 1u32) as #field_bit_uint { // check if value fits inside of the bit field
                    panic!("The provided value {} does not fit inside a {} bit field!", value, #field_bit_size);
                }

                // clear the first few bits
                value = value.checked_shl(#field_start_bit).unwrap_or(0);
                value = value.checked_shr(#field_start_bit).unwrap_or(0);
                eprintln!("SET\tcleared first bits of value: {0} | {0:b}", value);

                // cleat the first few bits
                eprintln!("SET\tgiven data: {0} | {0:b}", self.data[#field_start_byte as usize]);
                self.data[#field_start_byte as usize] = self.data[#field_start_byte as usize].checked_shl(#field_start_bit).unwrap_or(0);
                self.data[#field_start_byte as usize] = self.data[#field_start_byte as usize].checked_shr(#field_start_bit).unwrap_or(0);
                eprintln!("SET\tcleared first bits of data: {0} | {0:b}", self.data[#field_start_byte as usize]);
                // cleat the last few bits
                self.data[#field_end_byte as usize] = self.data[#field_end_byte as usize].checked_shr(#field_end_bit).unwrap_or(0);
                self.data[#field_end_byte as usize] = self.data[#field_end_byte as usize].checked_shl(#field_end_bit).unwrap_or(0);
                eprintln!("SET\tcleared last bits of data: {0} | {0:b}", self.data[#field_start_byte as usize]);

                for byte in #field_start_byte..=#field_end_byte {
                    eprintln!("SET\tvalue before shifting: {0} | {0:b}", value);
                    eprintln!("SET\t(#field_byte_size * 8u32) - (8u32 * (#field_end_byte - #field_start_byte) * (#field_end_byte - byte))");
                    eprintln!("SET\t({} * 8u32) - (8u32 * ({} - {}) * ({} - {}))", #field_byte_size, #field_end_byte, #field_start_byte, #field_end_byte, byte);
                    eprintln!("SET\tResult: {}", (8u32 * #field_byte_size * (#field_end_byte - byte)));


                    let data = if byte == #field_start_byte {
                        value.shr().shl(8 - #field_bit_size)
                    } else if byte == #field_end_byte {
                        value.shl(8 - #field_bit_size)
                    }

                    let data: u8 = value.checked_shl(8u32 * (#field_end_byte - #field_start_byte) * (#field_end_byte - byte)).unwrap_or(0) as u8 + self.data[byte as usize];
                    eprintln!("SET\tshifted bits in byte {1}: {0} | {0:b}", data, byte);
                    self.data[byte as usize] = data;
                }
                self.data.iter().for_each(|data| eprintln!("SET\tNEW DATA: {0} | {0:b}", data));
            }
        };

        bit_fields.push(BitField {
            bits: field_bit_size,
            getter: field_getter,
            setter: field_setter,
        });
    }
    Ok(bit_fields)
}

#[proc_macro_attribute]
pub fn bitfield(args: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Item);

    let src_visibility: syn::Visibility;
    let src_ident: syn::Ident;

    let total_byte_size: TokenStream2;
    let is_mod_8: TokenStream2;
    let mut getter_methods: TokenStream2 = TokenStream2::new();
    let mut setter_methods: TokenStream2 = TokenStream2::new();

    match input {
        Item::Struct(item_struct) => {
            // get basic struct information
            src_visibility = item_struct.vis.clone();
            src_ident = item_struct.ident.clone();
            let src_fields = item_struct.fields;

            let parse_fields = match parse_struct_fields(src_fields) {
                Ok(parsed) => parsed,
                Err(err) => return err.into()
            };

            let mut sizes = quote! { 0u32 };
            parse_fields
                .iter()
                .for_each(|field| {
                    let ref size = field.bits;
                    let ref getter = field.getter;
                    let ref setter = field.setter;

                    sizes = quote! { #sizes + #size  };
                    getter_methods = quote! { #getter_methods #getter };
                    setter_methods = quote! { #setter_methods #setter };
                });

            total_byte_size = quote! { (( #sizes )/8u32) };
            is_mod_8 = quote! { (( #sizes )%8u32) };
        }
        _ => return mk_error(input.span(), "The #[bitfield] attribute can just be applied to structs").into()
    }

    (quote! {
        #[repr(C)]
        #src_visibility struct #src_ident {
            data: [u8; #total_byte_size as usize]
        }

        impl #src_ident {
            pub fn new() -> Self {
                Self {
                    data: [0u8; #total_byte_size as usize]
                }
            }

            #getter_methods
            #setter_methods

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
                ::std::mem::transmute::<(), [u8; #is_mod_8 as usize]>(());
            }
        }
    }).into()
}