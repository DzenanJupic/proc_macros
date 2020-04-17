#![feature(proc_macro_hygiene)]
extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use syn::{parse_macro_input, braced, Token};
use syn::parse::{Parse, ParseStream, Result};
use quote::{quote, format_ident};
use std::ops::Range;


struct SeqInput {
    ts: TokenStream,
}

fn contains_loop(input_token_stream: &proc_macro2::TokenStream) -> bool {
    let token_tree_vec: Vec<TokenTree> = input_token_stream
        .clone()
        .into_iter()
        .map(|item| item)
        .collect();
    let iter = token_tree_vec.iter().enumerate();

    for (i, token_tree) in iter {
        match &token_tree {
            TokenTree::Punct(punct) if punct.as_char() == '#' => { // checks for #(...)*
                if let Some(TokenTree::Group(ref group)) = token_tree_vec.get(i+1) {  // checks if there is a group after #
                    if let proc_macro2::Delimiter::Parenthesis = group.delimiter() { // checks if Group is wrapped in Parentheses
                        match &token_tree_vec.get(i+2) {
                            Some(TokenTree::Punct(punct)) if punct.as_char() == '*' => { // check for * after the Group
                                // checked successfully for #(...)*
                                // now append (...) N times to the TokenStream
                                return true;
                            },
                            _ => ()
                        }
                    }
                }
            },
            TokenTree::Group(group ) => {
                if contains_loop(&group.stream()) {
                    return true;
                }
            }
            _ => ()
        }
    }
    false
}


/// parses the seq! body
/// in comments below N is used as the example ident set by the user
fn parser(input_token_stream: &proc_macro2::TokenStream, ident: &syn::Ident, range: Range<i128>, counter: Option<i128>) -> Result<proc_macro2::TokenStream> {
    let mut token_stream: Vec<proc_macro2::TokenStream> = Vec::new(); // the in the end returned TokenStream
    // create a TokenTree Vec out of the supplied TokenStream so its possible to access next and previous fields
    let token_tree_vec: Vec<TokenTree> = input_token_stream
        .clone()
        .into_iter()
        .map(|item| item)
        .collect();
    let mut iter = token_tree_vec.iter().enumerate();

    while let Some((i, token_tree)) = iter.next() { // iterate over the TokenTree Vec
        match &token_tree {
            TokenTree::Punct(punct) if punct.as_char() == '#' => { // checks for #(...)* or whatever#N
                match &token_tree_vec.get(i+1) {
                    Some(TokenTree::Group(group)) => { // #(...)* or #{N}
                        if let proc_macro2::Delimiter::Parenthesis = group.delimiter() { // #(...)*
                            for counter in range.clone() {
                                let parsed_group = parser(&group.stream(), &ident, range.clone(), Some(counter))?;
                                token_stream.push(quote! { #parsed_group });
                            }
                            iter.next(); // skipp (...)
                            iter.next(); // skipp *
                            continue;
                        } else if let proc_macro2::Delimiter::Brace = group.delimiter() { // #{N}
                            // TODO
                            panic!("At TODO");
                        } else {
                            token_stream.push(quote! { #punct })
                        }
                    },
                    Some(TokenTree::Ident(next_ident)) if *next_ident == *ident && counter.is_some() => { // maybe#N
                        let counter = counter.unwrap(); // safe, cause checked in match arm
                        let mut prev_ident: Option<&syn::Ident> = None;

                        // check if there is a ident before #N
                        if i > 0 {
                            if let Some(TokenTree::Ident(ref pi)) = token_tree_vec.get(i-1) {
                                prev_ident = Some(pi);
                            }
                        }

                        if let Some(prev_ident) = prev_ident { // if there is a prev_ident, a ident should be created
                            let ident = format_ident!("{}{}", prev_ident, counter.to_string());
                            token_stream.pop(); // pop the prev_ident
                            token_stream.push(quote! { #ident });
                        } else { // if there is no prev_ident, a literal should be created
                            let lit = proc_macro2::Literal::i128_unsuffixed(counter);
                            token_stream.push(quote! { #lit });
                        }

                        iter.next(); // skip N
                    },
                    // if above checks failed re-append the punct
                    _ => token_stream.push(quote!{ #punct })
                }
            },
            TokenTree::Ident(ref number_ident) if *number_ident == *ident => {
                if let Some(counter) = counter { // check if the parser currently is inside a loop
                    let lit = proc_macro2::Literal::i128_unsuffixed(counter);
                    token_stream.push(quote!{ #lit });
                } else {
                    // TODO: nice Error message
                    panic!("Ident but not in a loop\nTOKENS: {:#?}", input_token_stream);
                }
            },
            TokenTree::Group(ref group) => { // if it's a group parse the group and add the pared group to the token_stream
                let parsed_group = parser(&group.stream(), &ident, range.clone(), counter)?;
                let parsed_group = proc_macro2::Group::new(group.delimiter(), parsed_group);
                token_stream.push(quote!{ #parsed_group });
            },
            tt => token_stream.push(quote!{ #tt })
        }
    }

    Ok(quote!{ #( #token_stream )* })
}


impl Parse for SeqInput {
    fn parse(input: ParseStream) -> Result<Self> {
        // parse the basic structure of the seq! macro
        // ```
        //  seq!(
        //      N in 0..100     // <<< this part
        //      { /* ... */ }
        //  );
        // ```
        let ident: syn::Ident = input.parse()?;
        let _in: Token![in] = input.parse()?;
        let i1: syn::LitInt = input.parse()?;
        let i1: i128 = i1.base10_parse()?;
        let _double_punct: Token![..] = input.parse()?;
        let inclusive = input.peek(Token![=]);
        if inclusive {
            let _eq: Token![=] = input.parse()?;
        }
        let i2: syn::LitInt = input.parse()?;
        let mut i2: i128 = i2.base10_parse()?;
        if inclusive {
            i2 += 1;
        }
        let range: Range<i128> = i1..i2;

        // parse the expression block
        // ```
        //  seq!(
        //      N in 0..100
        //      { /* ... */ }    // <<< this part
        //  );
        // ```
        let content: syn::parse::ParseBuffer;
        let _braces = braced!(content in input);
        let contains_loop = contains_loop(&content.fork().parse()?);
        let content: proc_macro2::TokenStream = content.parse()?;

        let token_stream;
        if contains_loop {
            token_stream = parser(&content, &ident, range.clone(), None)?;
        } else {
            let mut results = proc_macro2::TokenStream::new();
            for counter in range.clone() {
                let r = parser(&content, &ident, range.clone(), Some(counter))?;
                results.extend(r);
            }
            token_stream = results;
        }

        Ok(SeqInput{
            ts: token_stream.into()
        })
    }
}


#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as SeqInput);
    input.ts
}

#[proc_macro]
pub fn eseq(input: TokenStream) -> TokenStream {
    seq(input)
}
