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

type RootTable<'a> = HashMap<Ident, Vec<&'a Constructor<OffAndSize>>>;

pub struct RustCodeGenerator<'a> {
    ctx: &'a SleighContext,
    token_fields: Vec<TokenFieldData>,
    non_root_sing_ctors: Vec<NonRootSingletonConstructor<'a>>,
    non_root_mult_ctors: Vec<MultiConstructor<'a>>,
    instruction_enum: Vec<InstructionEnumVariant<'a>>,
    mnemonic_enums: Vec<MultiConstructor<'a>>,
}

/// ```rust
/// struct Name(InnerIntType);
/// ```
struct TokenFieldData {
    name: Ident,
    field: TokenField,
    inner_int_type: Ident,
}

/// ```text
/// NAME: I1 I2 I3
/// ```
///
/// ```rust
/// struct Name(I1, I2, I3);
/// ```
struct NonRootSingletonConstructor<'a> {
    name: Ident,
    ctor: &'a Constructor<OffAndSize>,
    struct_def: TokenStream,
    destruct_stmt: TokenStream,
}

/// ```text
/// NAME: A1 A2 A3
/// NAME: B1 B2 B3
/// NAME: C1 C2 C3
/// ```
///
/// ```rust
/// enum Name {
///     A1A2A3(A1, A2, A3),
///     B1B2B3(B1, B2, B3),
///     C1C2C3(C1, C2, C3),
/// }
/// ```
struct MultiConstructor<'a> {
    name: Ident,
    variants: Vec<CtorEnumVariant<'a>>,
}

struct CtorEnumVariant<'a> {
    ctor: &'a Constructor<OffAndSize>,
    inner: EnumVariant,
}

struct EnumVariant {
    enum_def_line: TokenStream,
    match_pattern: TokenStream,
}

enum InstructionEnumVariant<'a> {
    Duplicate(EnumVariant),
    Unique(CtorEnumVariant<'a>),
}

fn make_root_table<'a>(ctx: &'a SleighContext) -> RootTable<'a> {
    ctx.symbols
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
        .map(|(mnemonic, constructor)| {
            (
                format_ident!("{}", mnemonic.to_upper_camel_case()),
                constructor,
            )
        })
        .fold(RootTable::new(), |mut acc, (mnemonic, constructor)| {
            acc.entry(mnemonic).or_default().push(constructor);
            acc
        })
}

fn make_token_fields(ctx: &SleighContext) -> Vec<TokenFieldData> {
    ctx.symbols
        .iter()
        .filter_map(|(symbol, data)| match data {
            SymbolData::Value(token_field) => Some(TokenFieldData {
                name: format_ident!("{}", symbol.to_upper_camel_case()),
                field: *token_field,
                inner_int_type: format_ident!("u{}", token_field.token_size * 8),
            }),
            _ => None,
        })
        .collect()
}

fn non_root_iter(
    ctx: &SleighContext,
) -> impl Iterator<Item = (Ident, &Vec<Constructor<OffAndSize>>)> {
    ctx.symbols
        .iter()
        .filter(|(symbol, _)| *symbol != INSTRUCTION)
        .filter_map(|(symbol, data)| match data {
            SymbolData::Subtable(cs) => {
                Some((format_ident!("{}", symbol.to_upper_camel_case()), cs))
            }
            _ => None,
        })
}

fn make_non_root_sing_ctors(ctx: &SleighContext) -> Vec<NonRootSingletonConstructor> {
    non_root_iter(ctx)
        .filter_map(|(name, cs)| match cs.as_slice() {
            [] => unreachable!(),
            [ctor] => {
                let toks = &ctor.display.toks;
                let tuple_type = gen_tuple_type(ctx, toks);
                let struct_def = quote! { struct #name #tuple_type ; };
                let tuple_destruct = gen_tuple_destruct(ctx, toks);
                let destruct_stmt = quote! { let #name #tuple_destruct = self ; };
                Some(NonRootSingletonConstructor {
                    name,
                    ctor,
                    struct_def,
                    destruct_stmt,
                })
            }
            _ => None,
        })
        .collect()
}

fn make_non_root_multi_ctors(ctx: &SleighContext) -> Vec<MultiConstructor> {
    non_root_iter(ctx)
        .filter_map(|(name, cs)| match cs.as_slice() {
            [] => unreachable!(),
            [_] => None,
            _ => {
                let variants = cs
                    .iter()
                    .map(|ctor| {
                        let toks = &ctor.display.toks;
                        let variant_name = display_to_ident(toks);
                        let tuple_type = gen_tuple_type(ctx, toks);
                        let enum_def_line = quote! { #variant_name #tuple_type , };
                        let tuple_destruct = gen_tuple_destruct(ctx, toks);
                        let match_pattern = quote! { #name :: #variant_name #tuple_destruct };
                        CtorEnumVariant {
                            ctor,
                            inner: EnumVariant {
                                enum_def_line,
                                match_pattern,
                            },
                        }
                    })
                    .collect();
                Some(MultiConstructor { name, variants })
            }
        })
        .collect()
}

fn make_mnemonic_enums<'a>(
    ctx: &'a SleighContext,
    root_table: &RootTable<'a>,
) -> Vec<MultiConstructor<'a>> {
    root_table
        .iter()
        .filter(|(_, ctors)| ctors.len() > 1)
        .map(|(mnemonic, constructors)| {
            let variants = constructors
                .iter()
                .map(|ctor| {
                    let toks = &&ctor.display.toks;
                    let name = display_to_ident(toks.get(1..).unwrap());
                    let tuple_type = gen_tuple_type(ctx, toks);
                    let tuple_destruct = gen_tuple_destruct(ctx, toks);
                    CtorEnumVariant {
                        ctor,
                        inner: EnumVariant {
                            enum_def_line: quote! { #name #tuple_type , },
                            match_pattern: quote! { #mnemonic :: #name #tuple_destruct },
                        },
                    }
                })
                .collect();
            MultiConstructor {
                name: mnemonic.clone(),
                variants,
            }
        })
        .collect()
}

fn make_instruction_enum<'a>(
    ctx: &'a SleighContext,
    root_table: &RootTable<'a>,
) -> Vec<InstructionEnumVariant<'a>> {
    root_table
        .iter()
        .map(|(mnemonic, constructors)| match constructors.as_slice() {
            [] => unimplemented!(),
            [ctor] => {
                let toks = &ctor.display.toks;
                let doc = ctor.display.to_string();
                let tuple_type = gen_tuple_type(ctx, toks);
                let tuple_destruct = gen_tuple_destruct(ctx, toks);
                InstructionEnumVariant::Unique(CtorEnumVariant {
                    ctor,
                    inner: EnumVariant {
                        enum_def_line: quote! { #mnemonic #tuple_type , },
                        match_pattern: quote! { Instruction :: #mnemonic #tuple_destruct },
                    },
                })
            }
            _ => InstructionEnumVariant::Duplicate(EnumVariant {
                enum_def_line: quote! { #mnemonic ( #mnemonic ) , },
                match_pattern: quote! { Instruction :: #mnemonic ( _0 ) },
            }),
        })
        .collect()
}

impl<'a> RustCodeGenerator<'a> {
    pub fn new(ctx: &'a SleighContext) -> Self {
        let root_table = make_root_table(ctx);
        let token_fields = make_token_fields(ctx);
        let non_root_sing_ctors = make_non_root_sing_ctors(ctx);
        let non_root_mult_ctors = make_non_root_multi_ctors(ctx);
        let root_table = make_root_table(ctx);
        let mnemonic_enums = make_mnemonic_enums(ctx, &root_table);
        let instruction_enum = make_instruction_enum(ctx, &root_table);

        Self {
            ctx,
            token_fields,
            non_root_sing_ctors,
            non_root_mult_ctors,
            mnemonic_enums,
            instruction_enum,
        }
    }

    fn gen_mnemonic_enums(&self) -> TokenStream {
        gen_multi_ctors(self.mnemonic_enums.iter())
    }

    fn gen_instruction_enum(&self) -> TokenStream {
        let variants = self
            .instruction_enum
            .iter()
            .map(|variant| match variant {
                InstructionEnumVariant::Duplicate(EnumVariant { enum_def_line, .. }) => {
                    enum_def_line
                }
                InstructionEnumVariant::Unique(CtorEnumVariant {
                    inner: EnumVariant { enum_def_line, .. },
                    ..
                }) => enum_def_line,
            })
            .cloned()
            .collect::<TokenStream>();
        quote!(enum Instruction { #variants })
    }

    fn gen_token_types(&self) -> TokenStream {
        self.token_fields
            .iter()
            .map(
                |TokenFieldData {
                     name,
                     inner_int_type,
                     ..
                 }| quote! { struct #name ( #inner_int_type ) ; },
            )
            .collect()
    }

    fn gen_non_root_singleton_constructor_types(&self) -> TokenStream {
        self.non_root_sing_ctors
            .iter()
            .map(|NonRootSingletonConstructor { struct_def, .. }| struct_def)
            .cloned()
            .collect()
    }

    fn gen_non_root_multi_constructor_types(&self) -> TokenStream {
        gen_multi_ctors(self.non_root_mult_ctors.iter())
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
        let mnemonic_enums = self.gen_mnemonic_enums();
        let instruction_enum = self.gen_instruction_enum();
        let token_types = self.gen_token_types();
        let constructor_types = self.gen_constructor_types();
        quote! {
            #mnemonic_enums
            #instruction_enum
            #token_types
            #constructor_types
        }
    }

    fn gen_token_types_display_impl(&self) -> TokenStream {
        self.token_fields
            .iter()
            .map(|TokenFieldData { name, .. }| {
                gen_display_impl(name, quote! { write!(f, "{}", self.0) })
            })
            .collect()
    }

    fn gen_non_root_sing_ctor_types_display_impl(&self) -> TokenStream {
        self.non_root_sing_ctors
            .iter()
            .map(
                |NonRootSingletonConstructor {
                     name,
                     ctor,
                     destruct_stmt,
                     ..
                 }| {
                    let writes = self.gen_display_write_stmts(&ctor.display.toks);
                    let body = quote! {
                        #destruct_stmt
                        #writes
                        Ok(())
                    };
                    gen_display_impl(name, body)
                },
            )
            .collect()
    }

    fn gen_multi_ctor_display_impl_arm(
        &self,
        CtorEnumVariant {
            ctor,
            inner: EnumVariant { match_pattern, .. },
        }: &CtorEnumVariant,
    ) -> TokenStream {
        let writes = self.gen_display_write_stmts(&ctor.display.toks);
        quote! { #match_pattern => { #writes } }
    }

    fn gen_multi_ctor_display_impl(
        &self,
        iter: impl Iterator<Item = &'a MultiConstructor<'a>>,
    ) -> TokenStream {
        iter.map(|MultiConstructor { name, variants }| {
            let arms = variants
                .iter()
                .map(|variant| self.gen_multi_ctor_display_impl_arm(variant))
                .collect::<TokenStream>();
            let body = quote! {
                match self {
                    #arms
                }
                Ok(())
            };
            gen_display_impl(name, body)
        })
        .collect()
    }

    fn gen_non_root_multi_ctor_types_display_impl(&self) -> TokenStream {
        self.gen_multi_ctor_display_impl(self.non_root_mult_ctors.iter())
    }

    fn iter_display_tok_fields(
        &'a self,
        toks: &'a [DisplayToken],
    ) -> impl Iterator<Item = (Option<usize>, &'a DisplayToken)> {
        toks.iter().scan(0, |state, tok| {
            if token_is_live_symbol(self.ctx, tok).is_some() {
                let i = *state;
                *state += 1;
                Some((Some(i), tok))
            } else {
                Some((None, tok))
            }
        })
    }

    fn gen_display_write_stmts(&self, toks: &[DisplayToken]) -> TokenStream {
        self.iter_display_tok_fields(toks)
            .map(|(opt_field, tok)| match (opt_field, tok) {
                (None, DisplayToken::Caret) => quote! {},
                (None, DisplayToken::String(s) | DisplayToken::Symbol(s)) => {
                    quote! { f.write_str(#s)?; }
                }
                (None, DisplayToken::Char(c)) => quote! { f.write_char(#c)?; },
                (None, DisplayToken::Space) => quote! { f.write_char(' ')?; },
                (Some(i), DisplayToken::Symbol(_)) => {
                    let var = format_ident!("_{i}");
                    quote! { write!(f, "{}", #var)?; }
                }
                (Some(_), _) => unreachable!(),
            })
            .collect()
    }

    fn gen_mnemonic_enums_display_impl(&self) -> TokenStream {
        self.gen_multi_ctor_display_impl(self.mnemonic_enums.iter())
    }

    fn gen_insn_enum_display_impl(&self) -> TokenStream {
        let arms = self
            .instruction_enum
            .iter()
            .map(|variant| match variant {
                InstructionEnumVariant::Duplicate(EnumVariant { match_pattern, .. }) => {
                    quote! {
                        #match_pattern => {
                            write!(f, "{}", _0)?;
                        }
                    }
                }
                InstructionEnumVariant::Unique(ctor_variant) => {
                    self.gen_multi_ctor_display_impl_arm(ctor_variant)
                }
            })
            .collect::<TokenStream>();
        let body = quote! {
            match self {
                #arms
            }
            Ok(())
        };
        gen_display_impl(&instruction_ident(), body)
    }

    fn gen_all_display_impls(&self) -> TokenStream {
        let token = self.gen_token_types_display_impl();
        let sing_ctors = self.gen_non_root_sing_ctor_types_display_impl();
        let multi_ctors = self.gen_non_root_multi_ctor_types_display_impl();
        let mn = self.gen_mnemonic_enums_display_impl();
        let insn = self.gen_insn_enum_display_impl();
        quote! {
            #token
            #sing_ctors
            #multi_ctors
            #mn
            #insn
        }
    }

    pub fn out(&self) -> TokenStream {
        let types = self.gen_all_types();
        let display_impls = self.gen_all_display_impls();
        quote! {
            #types
            #display_impls
        }
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

fn display_to_ident(toks: &[DisplayToken]) -> Ident {
    let s = toks
        .iter()
        .map(|tok| match tok {
            DisplayToken::Caret => "caret".to_owned(),
            DisplayToken::String(s) => s
                .chars()
                .map(char_name)
                .collect::<Option<String>>()
                .unwrap(),
            DisplayToken::Char(c) => char_name(*c).unwrap().to_owned(),
            DisplayToken::Space => "".to_owned(),
            DisplayToken::Symbol(s) => s.to_owned(),
        })
        .collect::<Vec<String>>()
        .join(" ");
    format_ident!("{}", s.to_upper_camel_case())
}

fn token_is_live_symbol<'a>(ctx: &SleighContext, tok: &'a DisplayToken) -> Option<&'a str> {
    match tok {
        DisplayToken::Symbol(s) => match ctx.symbols.get(s) {
            Some(SymbolData::Subtable(_) | SymbolData::Value(_)) => Some(s),
            _ => None,
        },
        _ => None,
    }
}

fn iter_live_symbols<'a>(
    ctx: &'a SleighContext,
    toks: &'a [DisplayToken],
) -> impl Iterator<Item = &'a str> {
    toks.iter().filter_map(|tok| token_is_live_symbol(ctx, tok))
}

fn gen_tuple_type(ctx: &SleighContext, toks: &[DisplayToken]) -> TokenStream {
    let types = iter_live_symbols(ctx, toks)
        .map(|s| format_ident!("{}", s.to_upper_camel_case()))
        .collect::<Vec<Ident>>();
    gen_tuple(&types)
}

fn gen_tuple_destruct(ctx: &SleighContext, toks: &[DisplayToken]) -> TokenStream {
    let bindings = iter_live_symbols(ctx, toks)
        .enumerate()
        .map(|(i, _)| format_ident!("_{i}"))
        .collect::<Vec<Ident>>();
    gen_tuple(&bindings)
}

fn gen_tuple(values: &[Ident]) -> TokenStream {
    match values {
        [] => quote!(),
        [value] => {
            quote!((#value))
        }
        _ => {
            quote!((#(#values),*))
        }
    }
}

fn gen_display_impl(name: &Ident, body: TokenStream) -> TokenStream {
    quote! {
        impl std::fmt::Display for #name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                #body
            }
        }
    }
}

fn gen_multi_ctors<'a>(iter: impl Iterator<Item = &'a MultiConstructor<'a>>) -> TokenStream {
    iter.map(|MultiConstructor { name, variants }| {
        let variants = variants
            .iter()
            .map(
                |CtorEnumVariant {
                     inner: EnumVariant { enum_def_line, .. },
                     ..
                 }| enum_def_line,
            )
            .cloned()
            .collect::<TokenStream>();
        quote! { enum #name { #variants } }
    })
    .collect()
}

fn instruction_ident() -> Ident {
    format_ident!("Instruction")
}
