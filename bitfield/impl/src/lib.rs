// TODO: just for now
#![allow(unused_imports, unused, unused_mut, dead_code)]
extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{self, TokenStream as TokenStream2};
use syn::{self, parse_macro_input, DeriveInput, Ident};
use quote::{quote, format_ident};


#[proc_macro_attribute]
pub fn bitfield(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let input = parse_macro_input!(input as DeriveInput);


    // return the input
    (quote! { #input }).into()
}
