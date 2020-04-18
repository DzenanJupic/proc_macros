extern crate proc_macro;

use proc_macro::TokenStream;

use proc_macro2::{self, TokenStream as TokenStream2};
use syn::{self, Ident, Item, parse_macro_input};
use syn::spanned::Spanned;
use syn::visit_mut::{self, VisitMut};

use quote::quote;

/// creates a syn::Error from a span and a message
fn mk_error<D: std::fmt::Display>(span: proc_macro2::Span, msg: D) -> TokenStream2 {
    syn::Error::new(span, msg).to_compile_error()
}

/// used to implements VisitMut for
/// this trait can hold an error that otherwise could not be returned from VisitMut
struct FsnSorted {
    error: Option<TokenStream2>
}

/// implementation of VisitMut for  FnSorted
/// This is necessary to be able to walk a Syntax tree recursively and find all the match
/// expressions if function with the `#[check]` attribute
impl VisitMut for FsnSorted {
    /// gets called, if a match expression is found.
    /// This function has the task to remove the `#[sorted]` attribute of every match expression
    /// cause attributes are not allowed for match expressions currently.
    /// If a `#[sorted]` attribute was found and removes this function checks, that the order
    /// of the match arms is correct
    /// If the order is not correct or an arm has a unknown pattern (not a enum variant or _)
    /// the error value of FnSorted is set. [`match_arm_order_is_correct`] is used for this.
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

/// checks that match arms are in right order
/// gets a vector of syn::Arm's and creates a iterator over the syn::Ident's of these arms
/// This Iterator is then handed to [`idents_in_order`] to check the order.
/// If the order is not correct or an arm has a unknown pattern (not a enum variant or _)
/// a error is returned
fn match_arm_order_is_correct(arms: &Vec<syn::Arm>) -> Result<(), TokenStream2> {
    let mut arms_iter = arms.iter();
    let mut ident_vec: Vec<Ident> = Vec::new();

    while let Some(arm) = arms_iter.next() {
        let ident = match arm {
            syn::Arm { pat: syn::Pat::TupleStruct(syn::PatTupleStruct { path, .. }), .. } => path.segments.last().unwrap().ident.clone(),
            syn::Arm { pat: syn::Pat::Ident(syn::PatIdent { ident, .. }), .. } => ident.clone(),
            syn::Arm { pat: syn::Pat::Wild(wild), .. } => {
                if arms_iter.clone().next().is_some() {
                    let error = mk_error(wild.span(), "_ should be at last position");
                    return Err(error);
                }
                continue;
            },
            arm => return Err(mk_error(arm.span(), "unsupported match arm pattern\nhelp: #[sorted] can just be applied to match statements that mach enum variants"))
        };
        ident_vec.push(ident);
    }

    idents_in_order(ident_vec.into_iter())
}

/// checks that the order of enum variants is alphabetical
/// takes a ItemEnum and creates an iterator over the idents of the individual variants
/// This Iterator is then handed to [`idents_in_order`] to check the order.
/// If the order is not correct a error is returned
fn variant_order_is_correct(item_enum: &syn::ItemEnum) -> Result<(), TokenStream2> {
    let ident_iter = item_enum.variants
        .iter()
        .map(|variant| variant.ident.clone());

    idents_in_order(ident_iter)
}

/// takes an iterator over idents and checks that these idents are in alphabetical order
/// If that's not the case a error is returned
fn idents_in_order<I: Iterator<Item=Ident>>(mut idents: I) -> Result<(), TokenStream2> {
    if let Some(mut first) = idents.next() {
        let ref mut prev_ident = first;
        let mut cur_ident: Ident;

        for ident in idents {
            cur_ident = ident;
            let cur_ident_str = cur_ident.to_string();
            let prev_ident_str = (*prev_ident).to_string();

            if cur_ident_str < prev_ident_str {
                let error = mk_error(cur_ident.span(), format!("{} should sort before {}", cur_ident_str, prev_ident_str));
                return Err(error);
            }

            *prev_ident = cur_ident;
        }
    }

    Ok(())
}

/// a macro to make sure enum variants stay in alphabetical order
/// This is particular useful when working with big teams or when the source code is  public.
/// Since it's a macro and expansion takes place at compile time there is no run time cost
/// for this macro. All the work happens at compile time and the source code is not modified at all.
///
/// This attribute can also be applied to match expressions. Since attributes for match expressions
/// are currently not allowed in rust, the function the match expression is contained in needs to
/// have a [`#[check]`] attribute. Look at [`check`] for more information.
///
/// # Example
///
/// This will compile:
/// ```
///#   use sorted::sorted;
///
///    #[sorted]
///    enum E {
///        A,
///        B,
///        C
///    }
/// ```
/// This won't
/// ``` compile_fail
///#   use sorted::sorted;
///
///    #[sorted]
///    enum E {
///        C,
///        B,
///        A
///    }
/// ```
#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = TokenStream2::from(args);
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
    (quote! { #input }).into()
}

/// a attribute that can be applied to functions
/// This attribute does not affect the function it self at all. It just provides the possibility
/// to apply [`#[sorted]`] attributes to match expressions.
/// Since attributes currently are not allowed in rust, this attribute takes care to remove
/// these attributes before the compiler complains
///
/// # Example
///
/// ```
///#   use sorted::*;
///
///    #[sorted]
///    enum E {
///        A,
///        B,
///        C
///    }
///
///    #[check]
///    fn f() {
///        let e = E::A;
///
///        #[sorted]       // < would usually cause a compile error
///        match e {
///            E::A => (),
///            E::B => (),
///            E::C => (),
///        }
///    }
/// ```
#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = TokenStream2::from(args);
    let mut input = parse_macro_input!(input as Item);

    if let Item::Fn(ref mut item_fn) = input {
        let mut fn_sorted = FsnSorted { error: None };
        fn_sorted.visit_item_fn_mut(item_fn);

        if let Some(error) = fn_sorted.error {
            return (quote! { #input #error }).into();
        }
    } else {
        return mk_error(args.span(), "check is for functions only\
        \nhelp: you applied the #[check] attribute here\
        \nhelp: consider removing #[check] or refactoring into a function\
        \nhelp: #[sorted] can be applied to enums\
        \nhelp: #[sorted] can be applied to match expression inside functions with the #[check] attribute").into();
    }

    // return the input without any changes
    (quote! { #input }).into()
}
