extern crate proc_macro;

use proc_macro::TokenStream;

use proc_macro2;
use syn::{self, Item, parse_macro_input};
use syn::spanned::Spanned;
use syn::visit_mut::{self, VisitMut};

use quote::quote;

fn mk_error<D: std::fmt::Display>(span: proc_macro2::Span, msg: D) -> proc_macro2::TokenStream {
    syn::Error::new(span, msg).to_compile_error()
}

struct FsnSorted {
    error: Option<proc_macro2::TokenStream>
}

impl VisitMut for FsnSorted {
    fn visit_expr_match_mut(&mut self, node: &mut syn::ExprMatch) {
        let mut sorted: Option<usize> = None;
        for (i, attr) in node.attrs.iter().enumerate() {
            if attr.path.is_ident("sorted") {
                sorted = Some(i);
            }
        }

        if let Some(index) = sorted {
            node.attrs.remove(index);

            if let Err(error) = match_arm_order_is_correct(&node.arms) {
                self.error = Some(error);
            }
        }

        // Delegate to the default impl to visit nested expressions.
        visit_mut::visit_expr_match_mut(self, node);
    }
}

// TODO: merge the logic of variant_order_is_correct and match_arm_order_is_correct
fn match_arm_order_is_correct(arms: &Vec<syn::Arm>) -> Result<(), proc_macro2::TokenStream> {
    let mut arm_iter = arms.iter();
    let first = arm_iter.next();
    let unsupported_pattern = |span: proc_macro2::Span| mk_error(span, "unsupported match arm pattern\
    \n help: #[sorted] can just be applied to match statements that mach enum variants");

    let prev_ident;

    match first {
        Some(syn::Arm { pat: syn::Pat::TupleStruct(syn::PatTupleStruct { path, .. }), .. }) => {
            prev_ident = &path
                .segments
                .iter()
                .last()
                .unwrap()
                .ident;
        }
        Some(syn::Arm { pat: syn::Pat::Ident(syn::PatIdent { ident, .. }), .. }) => prev_ident = ident,
        Some(syn::Arm { pat: syn::Pat::Wild(wild), ..}) => {
            return if arm_iter.clone().next().is_some() {
                let error = mk_error(wild.span(), "_ should be at last position");
                Err(error)
            } else {
                Ok(())
            }
        }
        Some(arm) => return Err(unsupported_pattern(arm.pat.span())),
        None => unreachable!()
    }

    let ref mut prev_ident = prev_ident.to_string();
    let mut cur_ident;

    while let Some(arm) = &arm_iter.next() {
        match arm {
            syn::Arm { pat: syn::Pat::TupleStruct(syn::PatTupleStruct { path, .. }), .. } => {
                cur_ident =
                    &path
                        .segments
                        .iter()
                        .last()
                        .unwrap()
                        .ident;
            }
            syn::Arm { pat: syn::Pat::Ident(syn::PatIdent { ident, .. }), .. } => cur_ident = ident,
            syn::Arm { pat: syn::Pat::Wild(wild), ..} => {
                return if arm_iter.clone().next().is_some() {
                    let error = mk_error(wild.span(), "_ should be at last position");
                    Err(error)
                } else {
                    Ok(())
                }
            }
            arm => {eprintln!("PATTERN: {:#?}", arm.pat);return Err(unsupported_pattern(arm.pat.span()))},
        }

        let cur_ident_str = (*cur_ident).to_string();

        if cur_ident_str < *prev_ident {
            let error = mk_error(cur_ident.span(), format!("{} should sort before {}", cur_ident_str, prev_ident));
            return Err(error);
        }

        *prev_ident = cur_ident_str;
    }

    Ok(())
}

fn variant_order_is_correct(item_enum: &syn::ItemEnum) -> Result<(), proc_macro2::TokenStream> {
    let mut variant_iter = item_enum.variants.iter();

    if let Some(first) = variant_iter.next() {
        let ref mut prev_ident = first.ident.to_string();
        let mut cur_ident;

        for variant in variant_iter {
            cur_ident = variant.ident.to_string();

            if cur_ident < *prev_ident {
                let error = mk_error(variant.ident.span(), format!("{} should sort before {}", cur_ident, prev_ident));
                return Err(error);
            }

            *prev_ident = cur_ident;
        }
    }

    Ok(())
}


#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = proc_macro2::TokenStream::from(args);
    let input = parse_macro_input!(input as Item);

    if let Item::Enum(ref item_enum) = input {
        if let Err(error) = variant_order_is_correct(item_enum) {
            return error.into();
        }
    } else {
        return mk_error(args.span(), "sorted is for enums only\
        \nhelp: you applied the #[sorted] attribute here\
        \nhelp: consider removing #[sorted] or refactoring into a struct\
        \nhelp: #[check] can be applied to functions to allow #[sorted] for match expressions").into();
    };

// return the input without any changes
    (quote! { # input }).into()
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = proc_macro2::TokenStream::from(args);
    let mut input = parse_macro_input!(input as Item);

    if let Item::Fn(ref mut item_fn) = input {
        let mut fn_sorted = FsnSorted { error: None };
        fn_sorted.visit_item_fn_mut(item_fn);

        if let Some(error) = fn_sorted.error {
// let input = input.to_token_stream();
            return (quote! { # input # error }).into();
        }
    } else {
        return mk_error(args.span(), "check is for functions only\
        \nhelp: you applied the #[check] attribute here\
        \nhelp: consider removing #[check] or refactoring into a function\
        \nhelp: #[sorted] can be applied to enums\
        \nhelp: #[sorted] can be applied to match expression inside functions with the #[check] attribute").into();
    }

// return the input without any changes
    (quote! { # input }).into()
}
