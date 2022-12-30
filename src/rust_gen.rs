use std::collections::HashMap;

use crate::ast::Constructor;
use crate::ast::DisplayToken;
use crate::ast::INSTRUCTION;
use crate::context::OffAndSize;
use crate::context::SleighContext;
use crate::context::SymbolData;
use crate::context::TokenField;

use heck::ToUpperCamelCase;
use proc_macro2::Ident;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

pub struct RustCodeGenerator<'a> {
    ctx: &'a SleighContext,
    root_table: HashMap<&'a str, Vec<&'a Constructor<OffAndSize>>>,
}

impl<'a> RustCodeGenerator<'a> {
    pub fn new(ctx: &'a SleighContext) -> Self {
        let root_table = ctx
            .symbols
            .iter()
            .find_map(|(symbol, data)| match (symbol.as_str(), data) {
                (INSTRUCTION, SymbolData::Subtable(cs)) => Some(cs),
                _ => None,
            })
            .unwrap()
            .iter()
            .map(|constructor| match constructor.display.toks.as_slice() {
                [DisplayToken::Symbol(mnemonic), ..] => (mnemonic, constructor),
                _ => panic!("instruction has no mnemonic"),
            })
            .fold(
                HashMap::<&str, Vec<&Constructor<OffAndSize>>>::new(),
                |mut acc, (mnemonic, constructor)| {
                    acc.entry(mnemonic).or_default().push(constructor);
                    acc
                },
            );

        Self { ctx, root_table }
    }

    fn gen_dup_mnemonic_enums(&self) -> TokenStream {
        self.root_table
            .iter()
            .map(|(mnemonic, constructors)| {
                if constructors.len() == 1 {
                    return quote![];
                }
                let name = format_ident!("{}", mnemonic.to_upper_camel_case());
                let variants = constructors
                    .iter()
                    .map(|constructor| generate_enum_variant_ctor(self.ctx, &constructor))
                    .collect::<TokenStream>();
                quote![enum #name { #variants }]
            })
            .collect()
    }

    fn gen_instruction_enum_variants(&self) -> TokenStream {
        self.root_table
            .iter()
            .map(|(mnemonic, constructors)| match constructors.as_slice() {
                [] => unimplemented!(),
                [constructor] => {
                    let doc = constructor.display.to_string();
                    let variant_values =
                        display_to_tuple_values(self.ctx, &constructor.display.toks);
                    generate_enum_variant(Some(&doc), &mnemonic, &variant_values)
                }
                _ => generate_enum_variant(None, &mnemonic, &[&mnemonic]),
            })
            .collect()
    }

    fn gen_instruction_enum(&self) -> TokenStream {
        let variants = self.gen_instruction_enum_variants();
        quote!(enum Instruction { #variants })
    }

    fn token_fields(&self) -> impl Iterator<Item = (Ident, TokenField)> + 'a {
        self.ctx
            .symbols
            .iter()
            .filter_map(|(symbol, data)| match data {
                SymbolData::Value(token_field) => Some((
                    format_ident!("{}", symbol.to_upper_camel_case()),
                    *token_field,
                )),
                _ => None,
            })
    }

    fn gen_token_types(&self) -> TokenStream {
        self.token_fields()
            .map(|(name, token_field)| {
                let bits = token_field.token_size * 8;
                let inner_int_type = format_ident!("u{bits}");
                quote![struct #name(#inner_int_type);]
            })
            .collect()
    }

    fn non_root_constructors(
        &self,
    ) -> impl Iterator<Item = (Ident, &Vec<Constructor<OffAndSize>>)> {
        self.ctx
            .symbols
            .iter()
            .filter(|(symbol, _)| *symbol != INSTRUCTION)
            .filter_map(|(symbol, data)| match data {
                SymbolData::Subtable(cs) => {
                    Some((format_ident!("{}", symbol.to_upper_camel_case()), cs))
                }
                _ => None,
            })
    }

    fn non_root_singleton_constructors(
        &self,
    ) -> impl Iterator<Item = (Ident, &Constructor<OffAndSize>)> {
        self.non_root_constructors()
            .filter_map(|(name, cs)| match cs.as_slice() {
                [] => unreachable!(),
                [c] => Some((name, c)),
                _ => None,
            })
    }

    fn non_root_multi_constructors(
        &self,
    ) -> impl Iterator<Item = (Ident, &Vec<Constructor<OffAndSize>>)> {
        self.non_root_constructors()
            .filter_map(|(name, cs)| match cs.as_slice() {
                [] => unreachable!(),
                [c] => None,
                _ => Some((name, cs)),
            })
    }

    fn gen_non_root_singleton_constructor_types(&self) -> TokenStream {
        self.non_root_singleton_constructors()
            .map(|(name, c)| {
                let doc = c.display.to_string();
                let values = display_to_tuple_values(self.ctx, &c.display.toks);
                let values = generate_tuple(&values);
                quote![
                    #[doc = #doc]
                    struct #name #values ;
                ]
            })
            .collect()
    }

    fn gen_non_root_multi_constructor_types(&self) -> TokenStream {
        self.non_root_multi_constructors()
            .map(|(name, cs)| {
                let variants = cs
                    .iter()
                    .map(|c| generate_enum_variant_ctor(self.ctx, &c))
                    .collect::<TokenStream>();
                quote![enum #name { #variants }]
            })
            .collect()
    }

    fn gen_constructor_types(&self) -> TokenStream {
        let singleton = self.gen_non_root_singleton_constructor_types();
        let multi = self.gen_non_root_multi_constructor_types();
        quote! {
            #singleton
            #multi
        }
    }

    fn gen_all_types(&self) -> TokenStream {
        let mnemonic_enums = self.gen_dup_mnemonic_enums();
        let instruction_enum = self.gen_instruction_enum();
        let token_types = self.gen_token_types();
        let constructor_types = self.gen_constructor_types();
        quote![
            #mnemonic_enums
            #instruction_enum
            #token_types
            #constructor_types
        ]
    }

    fn gen_token_types_display_impl(&self) -> TokenStream {
        self.token_fields()
            .map(|(name, _)| gen_display_impl(name, quote! { write!(f, "{}", self.0) }))
            .collect()
    }

    fn gen_non_root_singleton_constructor_types_display_impl(&self) -> TokenStream {
        self.non_root_singleton_constructors()
            .map(|(name, constructor)| {
                let writes = gen_display_write(&constructor.display.toks);
                gen_display_impl(name, quote! { #writes Ok(()) })
            })
            .collect()
    }

    fn gen_non_root_multi_constructor_types_display_impl(&self) -> TokenStream {
        todo!()
    }

    pub fn out(&self) -> TokenStream {
        let types = self.gen_all_types();
        let d1 = self.gen_token_types_display_impl();
        let d2 = self.gen_non_root_singleton_constructor_types_display_impl();
        quote! { #types #d1 #d2 }
    }
}

fn char_name(c: char) -> Option<&'static str> {
    match c {
        '#' => Some("hash"),
        ',' => Some("comma"),
        '(' => Some("open paren"),
        ')' => Some("close paren"),
        _ => None,
    }
}

fn string_to_ident(s: &str) -> Option<String> {
    s.chars().map(char_name).collect::<Option<String>>()
}

fn display_to_ident(toks: &[DisplayToken]) -> String {
    let s = toks
        .iter()
        .map(|tok| match tok {
            DisplayToken::Caret => "caret".to_owned(),
            DisplayToken::String(s) => string_to_ident(s).unwrap(),
            DisplayToken::Char(c) => char_name(*c).unwrap().to_owned(),
            DisplayToken::Space => "".to_owned(),
            DisplayToken::Symbol(s) => s.to_owned(),
        })
        .collect::<Vec<String>>()
        .join(" ");
    s.to_upper_camel_case()
}

fn display_to_tuple_values(ctx: &SleighContext, toks: &[DisplayToken]) -> Vec<String> {
    toks.iter()
        .filter_map(|tok| match tok {
            DisplayToken::Symbol(s) => Some(s),
            _ => None,
        })
        .filter_map(|s| match ctx.symbols.get(s) {
            Some(SymbolData::Subtable(_) | SymbolData::Value(_)) => Some(s),
            _ => None,
        })
        .map(|s| s.to_upper_camel_case())
        .collect()
}

fn generate_tuple<S: AsRef<str>>(values: &[S]) -> TokenStream {
    let to_ident = |value: &S| format_ident!("{}", value.as_ref().to_upper_camel_case());
    match values {
        [] => quote!(),
        [value] => {
            let value = to_ident(value);
            quote!((#value))
        }
        _ => {
            let iter = values.iter().map(to_ident);
            quote!((#(#iter),*))
        }
    }
}

fn generate_enum_variant<S: AsRef<str>>(
    doc: Option<&str>,
    name: &str,
    values: &[S],
) -> TokenStream {
    let doc = match doc {
        Some(s) => quote!(#[doc = #s]),
        None => quote!(),
    };
    let name = format_ident!("{}", name.to_upper_camel_case());
    let values = generate_tuple(values);
    quote![
        #doc
        #name #values ,
    ]
}

fn generate_enum_variant_ctor(ctx: &SleighContext, ctor: &Constructor<OffAndSize>) -> TokenStream {
    let toks = &ctor.display.toks;
    let doc = ctor.display.to_string();
    let variant_name = display_to_ident(toks);
    let variant_values = display_to_tuple_values(ctx, toks);
    generate_enum_variant(Some(&doc), &variant_name, &variant_values)
}

fn iter_display_tok_fields(
    toks: &[DisplayToken],
) -> impl Iterator<Item = (Option<usize>, &DisplayToken)> {
    toks.iter().scan(0, |state, tok| match tok {
        DisplayToken::Symbol(_) => {
            let i = *state;
            *state += 1;
            Some((Some(i), tok))
        }
        _ => Some((None, tok)),
    })
}

fn gen_display_write(toks: &[DisplayToken]) -> TokenStream {
    iter_display_tok_fields(toks)
        .map(|(opt_field, tok)| match (opt_field, tok) {
            (None, DisplayToken::Caret) => quote! {},
            (None, DisplayToken::String(s)) => quote! { f.write_str(#s)?; },
            (None, DisplayToken::Char(c)) => quote! { f.write_char(#c)?; },
            (None, DisplayToken::Space) => quote! { f.write_char(' ')?; },
            (Some(i), DisplayToken::Symbol(_)) => {
                let var = format_ident!("_{i}");
                quote! { write!(f, "{}", #var)?; }
            }
            (None, DisplayToken::Symbol(_)) | (Some(_), _) => unreachable!(),
        })
        .collect()
}

fn gen_display_impl(name: Ident, body: TokenStream) -> TokenStream {
    quote! {
        impl std::fmt::Display for #name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                #body
            }
        }
    }
}
