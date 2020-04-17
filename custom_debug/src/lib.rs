extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{self, TokenStream as TokenStream2};
use syn::{self, Ident, Meta, parse_macro_input};
use syn::spanned::Spanned;
use quote::quote;

use std::collections::HashMap;

/// creates a syn::Error from a span and a errormessage
fn mk_error<D: std::fmt::Display>(span: proc_macro2::Span, msg: D) -> TokenStream2 {
    syn::Error::new(span, msg).to_compile_error()
}

/// a enum that holds parsed Attributes
/// these attributes can be either struct or struct field attributes
///
/// # Fields
/// * Debug: holds a String that specifies how the debug string should be formatted. The default
/// is "{:?}". If applied, the default for all fields is overwritten. Field attributes override
/// struct attributes. If multiple are set, the last one is used.
enum CustomDebugAttrs {
    Debug(String),
    Bound((syn::Type, syn::punctuated::Punctuated<syn::TypeParamBound, syn::token::Add>)),
}

/// parsed attributes into Vector of CustomDebugAttrs
/// This function takes a reference to a Vector if syn::Attribute's and converts them into
/// CustomDebugAttrs. If an attribute can't be parsed, a proc_macro2::TokenStream compiler error
/// is returned.
fn parse_attrs(attributes: &Vec<syn::Attribute>) -> Result<Vec<CustomDebugAttrs>, TokenStream2> {
    let mut parsed = Vec::new(); // the vector that will be returned in the end
    for ref attr in attributes {
        let meta = attr.parse_meta(); // parsed the whole attribute #[debug ...] => debug ...
        let args: syn::Result<Meta> = attr.parse_args(); // just parses attribute args, if existing #[debug(...)] => ...

        match &meta {
            Ok(Meta::NameValue(named)) if named.path.is_ident("debug") => { // check for #[debug = ...]
                if let syn::Lit::Str(ref lit_str) = named.lit { // check for #[debug = "..."]
                    parsed.push(CustomDebugAttrs::Debug(lit_str.value()));
                    continue;
                }
            }
            Ok(Meta::List(list)) if list.path.is_ident("debug") => match &args { // check for #[debug(...)]
                Ok(Meta::NameValue(named)) if named.path.is_ident("bound") => { // check for #[debug(bound = ...)]
                    if let syn::Lit::Str(ref lit_str) = named.lit {
                        let trait_bound: syn::Result<syn::WherePredicate> = lit_str.parse(); // parses "TypePath: Trait"
                        match trait_bound {
                            Ok(trait_bound) => {
                                if let syn::WherePredicate::Type(predicate_type) = trait_bound {
                                    parsed.push(CustomDebugAttrs::Bound((predicate_type.bounded_ty, predicate_type.bounds)));
                                    continue;
                                }
                            }
                            Err(err) => return Err(err.to_compile_error())
                        }
                    }
                }
                _ => ()
            }
            _ => ()
        }
        return Err(mk_error(attr.span(), "Unexpected Attribute!"));
    }
    Ok(parsed)
}

/// checks if a type (like Vec<T>) is generic about a generic Type (like T)
/// If this is the case, Some(syn::Type) is returned
/// The returned type is either the outer type (Vec of Vec<T>) if <T> contains just T (or
/// other/more generics or types) or the generic path (T::Value of <T::Value>) if the generic
/// is part of a path
/// If the generic can't be found, None is returned
fn type_is_generic_about(ty: &syn::Type, generic_ident: &Ident) -> Option<syn::Type> {
    // this is usually true
    if let syn::Type::Path(syn::TypePath { ref path, .. }) = ty {
        if path.is_ident(&(*generic_ident).to_string()) {
            // if the provided type itself already is the generic parameter, return Some(ty) where ty is the provided type
            return Some((*ty).clone());
        } else if path.segments.len() > 1 && path.segments[0].ident == *generic_ident {
            // if the provided type is a path, with the generic at the beginning, return Some(ty) where ty is T::Value of <T::Value>
            let inner_ty = syn::Type::Path(syn::TypePath { qself: None, path: (*path).clone() });
            return Some(inner_ty);
        }
        // if not iterate over each path segment of the path
        // [std, fmt, Debug] are the path segments of std::fmt::Debug
        for segment in path.segments.iter() {
            // check if the current path segment has angle-bracketed arguments
            // [T, String] are the angle-bracketed arguments of Result<T, String>
            if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments { ref args, .. }) = segment.arguments {
                // if that's the case, iterate over each of these arguments
                for generic_arg in args.iter() {
                    // check if the arguments is a type (could also be a lifetime, ...)
                    if let syn::GenericArgument::Type(ref generic_type) = generic_arg {
                        // if that's the case call this function recursively
                        // this is needed cause the angle-bracketed argument could be a normal
                        // type it self, like it is in Option<Vec<T>> or Option<Result<T, E>>
                        // only Some should be returned cause in case of Result<T, E>, E could
                        // be the desired generic, so returning None cause T is not the
                        // desired generic would be wrong
                        let is_generic_about = type_is_generic_about(&generic_type, &generic_ident);
                        if let Some(ref inner_type) = is_generic_about {
                            // check if the result is a Type::Path | should be always true
                            if let syn::Type::Path(syn::TypePath { path, .. }) = inner_type {
                                return if path.is_ident(&(*generic_ident).to_string()) {
                                    // if the returned value has the ident of the generic_ident,
                                    // the provided Type (ty) should be returned.
                                    // This is the case, if the provided type is for example T::Value
                                    // In this case you need T::Value as type, ty is generic about
                                    Some((*ty).clone())
                                } else {
                                    // if that's not the case, the value inside is_generic_about
                                    // must be a generic itself. In this case, the generic should
                                    // be returned
                                    is_generic_about
                                };
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

/// a derive macro, that lets you customize the way, debug strings should be formattet.
/// This macro let's you specifie the debug format not only on struct level, but also for each
/// indiviual field.
///
/// # Example
///
/// ```
///#    use derive_debug::CustomDebug;
///
///     #[derive(CustomDebug)]
///     struct S {
///         field1: String,
///         #[debug = "STRING: {:?}"]
///         field2: String
///     }
///
///     let field1 = String::from("string1");
///     let field2 = String::from("string2");
///     let s = S { field1, field2};
///
///     assert_eq!(format!("{:?}", s), String::from("S { field1: \"string1\", field2: STRING: \"string2\" }"))
/// ```
///
/// # Edge Cases
///
/// Sometimes it's nessecery to help CustomDebug out. This for expample the case if you want to
/// derive it for a struct, like this:
///
/// ``` no_run
///#   use derive_debug::CustomDebug;
///#   use std::fmt::Debug;
///
///    trait Trait {
///        type Value;
///    }
///
///    #[derive(CustomDebug)]
///    #[debug(bound = "T::Value: Debug")]
///    struct Wrapper<T: Trait> {
///        field: Field<T>,
///    }
///
///    #[derive(CustomDebug)]
///    struct Field<T: Trait> {
///        values: Vec<T::Value>,
///    }
///
///    struct NoDebug;
///    impl Trait for NoDebug {
///        type Value = u8;
///    }
///
///    fn assert_debug<F: Debug>() {}
///    assert_debug::<Wrapper<NoDebug>>();
/// ```
///
/// In this case the struct NoDebug does not implement the Trait Debug. But the CustomDebug macro
/// depends on each field of the struct impplementing Debug. And a Type that is generic over
/// another type just implements Debug, if this other type implements Debug.
/// So in this example you could not derive CustomDebug on Wrapper nor Field, cause both are generic
/// over a Type T, that implemets the trait `Trait`. So the compiler can't garuantie, that T
/// implements `Debug`.
/// But in fanct, T doesn't need to implement Debug, cause the type `Value` is the important part.
/// Unfortunatly there's no way for CustomDebug to dectect these dependencies. So it needs a hint
/// (in this example `#[debug(bound = "T::Value: Debug")]`). This overrides the automaticly
/// generated trait bound by Cusom Debug.
///
/// ## Imoprtant!
///
/// If you use bound, make sure you use it at the right place! If you use it on the struct itself,
/// it will override all other traitbounds and not generate any automaticly. You can use this, if
/// the struct has just one generic argument, or if you need to override all generics.
/// If you use bound on a field, it will override the automaticaly generated trait-bounds for this
/// field. If multible fields use the same generic, this coulb be usefull. Also, if you just want
/// to override the generics of one field, this is the way to go.
/// (In the example above, you could have used both methods)
///
#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);

    /* get the fundamental data out of the input struct */

    let src_fields;
    // check if the input really is a struct
    if let syn::Data::Struct(syn::DataStruct { fields, .. }) = input.data {
        src_fields = fields;
    } else {
        return mk_error(input.ident.span(), "Currently you can just derive CustomDebug on structs").into();
    }
    let src_ident = input.ident;
    let src_ident_str = src_ident.to_string();
    // impl_generics is are the generics that the std::fmt::Debug implementation for the src-struct
    // has to be generic about
    // src_generics are the generics the src-struct is generic about (also needed in the implementation)
    // src_where_clause if the where clause of the src-struct
    let (impl_generics, src_generics, src_where_clause) = input.generics.split_for_impl();
    let src_attrs = match parse_attrs(&input.attrs) {
        Ok(attrs) => attrs,
        Err(err) => return err.into()
    };

    /* parse src_attrs */

    // special struct and field bounds give the user the option to override the automatically
    // generated bound of CustomDebug in case these bounds are wrong or lead to bugs
    // this can sometimes happen if the input struct is generic over parameters that don't implement
    // std::fmt::Debug
    // In this case the use can override all trait bounds by setting one or more struct attributes
    // or he can override individual fields. In this case CustomDebug will not create any trait
    // bounds for any generic parameter this field is generic about
    // attributes should look like #[debug(bound = "TypePath: Trait")]
    // in probably all cases the trait should be std::fmt::Debug
    let mut special_struct_bounds: Vec<TokenStream2> = Vec::new();
    let mut special_field_bounds: Vec<TokenStream2> = Vec::new();
    let mut generated_generic_bounds: HashMap<Ident, TokenStream2> = HashMap::new();
    // the default pattern to format struct fields
    let mut default_format_pattern = String::from("{:?}");

    // parse the preprocessed struct attributes
    for attr in src_attrs {
        match attr {
            CustomDebugAttrs::Debug(str) => default_format_pattern = str,
            CustomDebugAttrs::Bound((bound_type, bound_traits)) => special_struct_bounds.push(quote! { #bound_type : #bound_traits }),
        }
    }
    // parse the struct generics
    // src_generic_idents is a vector that holds the syn::Ident's of each generic parameter
    // the input struct is generic about
    let mut src_generic_idents = Vec::new();
    for generic in input.generics.params.iter() {
        if let syn::GenericParam::Type(ref type_param) = generic {
            src_generic_idents.push(type_param.ident.clone());
        }
    }

    /* parse the individual fields of the struct */

    let mut formatter_field_args = Vec::new();
    let formatter_fn;

    // check what type of struct was handed as input
    match &src_fields {
        // struct with named fields (i.e. struct S { field1: T, field2: T, ... } )
        syn::Fields::Named(_) => formatter_fn = quote! { debug_struct( #src_ident_str ) },
        // tuple struct with no field names (i.e. struct S(T, T, ...); )
        syn::Fields::Unnamed(_) => formatter_fn = quote! { debug_tuple( #src_ident_str ) },
        // unit struct (i.e. struct S; )
        // in this case there's no need for more computation cause a unit struct
        // can just be formatted as it's name
        syn::Fields::Unit => {
            return (quote! {
                        impl #impl_generics ::std::fmt::Debug for #src_ident #src_generics #src_where_clause {
                            fn fmt(&self, formatter: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                                formatter.write_str( #src_ident_str )
                            }
                        }
                    }).into();
        }
    }

    for (i, field) in src_fields.iter().enumerate() {
        /* get the fundamental data out of the field */

        let field_ident = &field.ident;
        let field_attrs = match parse_attrs(&field.attrs) {
            Ok(attrs) => attrs,
            Err(err) => return err.into()
        };
        let field_type = &field.ty;
        let mut override_format_patter: Option<String> = None;

        /* set field data */

        let mut special_bounds = false;
        for attr in field_attrs {
            match attr {
                CustomDebugAttrs::Debug(str) => override_format_patter = Some(str),
                CustomDebugAttrs::Bound((bound_type, bound_traits)) => {
                    special_field_bounds.push(quote! { #bound_type : #bound_traits });
                    special_bounds = true;
                }
            }
        }
        // if field_bounds is empty, there were no attributes set by the user to override
        // the generated trait-bounds
        if !special_bounds {
            for generic_ident in src_generic_idents.iter() {
                let is_generic_about = type_is_generic_about(&field_type, &generic_ident);
                if is_generic_about.is_some() && !generated_generic_bounds.contains_key(&generic_ident.clone().into()) {
                    let generic_type = is_generic_about.unwrap();
                    generated_generic_bounds.insert(generic_ident.clone().into(), quote! { #generic_type: ::std::fmt::Debug });
                }
            }
        }

        let pattern = &override_format_patter.unwrap_or(default_format_pattern.clone());

        if let Some(ref ident) = field_ident {
            let ident_str = (*ident).to_string();
            formatter_field_args.push(quote! { #ident_str, &format_args!( #pattern , &self.#ident ) });
        } else {
            let i = proc_macro2::Literal::usize_unsuffixed(i);
            formatter_field_args.push(quote! { &self.#i });
        }
    }

    /* generate right where clause */

    let mut generic_bounds: Vec<TokenStream2> = Vec::new();
    for (_, bound) in generated_generic_bounds {
        generic_bounds.push(bound)
    }
    let generated_where_clause;

    if !special_struct_bounds.is_empty() {
        generated_where_clause = quote! { where #( #special_struct_bounds, )* #( #special_field_bounds, )* };
    } else {
        if let Some(where_clause) = src_where_clause {
            generated_where_clause = quote! { #where_clause, #( #special_field_bounds, )* #( #generic_bounds, )* }
        } else {
            generated_where_clause = quote! { where #( #special_field_bounds, )* #( #generic_bounds, )* }
        }
    }

    /* return the custom implementation of std::fmt::Debug */

    (quote! {
        impl #impl_generics ::std::fmt::Debug for #src_ident #src_generics #generated_where_clause {
            fn fmt(&self, formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                formatter.#formatter_fn
                #(  .field(#formatter_field_args)   )*
                    .finish()
            }
        }
    }).into()
}
