use std::collections::BTreeMap;

use sleigh_types::{
    ast::{
        Atomic, Constraint, ConstraintCompare, ConstraintCompareOp, Constructor, ContextFieldDef,
        ContextListItem, DisplayToken, Endian, FieldDef, PExpression, PExpressionBin,
        PExpressionBinOp, PExpressionUnary, PExpressionUnaryOp, PatternEquation,
        PatternEquationBin, PatternEquationBinOp, PatternEquationInner, TokenParentInfo,
        INSTRUCTION,
    },
    context::{OffAndSize, SleighContext, SymbolData, TokenField},
};

use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};

type RootTable<'a> = BTreeMap<Ident, Vec<&'a Constructor<OffAndSize>>>;

pub struct RustCodeGenerator<'a> {
    ctx: &'a SleighContext,
    context_int_type: Option<Ident>,
    token_fields: BTreeMap<&'a str, TokenFieldData>,
    non_root_sing_ctors: BTreeMap<&'a str, NonRootSingletonConstructor<'a>>,
    non_root_mult_ctors: BTreeMap<&'a str, MultiConstructor<'a>>,
    instruction_enum: Vec<InstructionEnumVariant<'a>>,
    mnemonic_enums: Vec<MultiConstructor<'a>>,
}

/// ```text
/// mod parent {
///     struct Name(InnerIntType);
/// }
/// ```
struct TokenFieldData {
    parent: Ident,
    name: Ident,
    qualified_name: TokenStream,
    field: TokenField,
    inner_int_type: Ident,
}

/// ```text
/// NAME: I1 I2 I3
/// ```
///
/// ```text
/// struct Name(I1, I2, I3);
/// ```
struct NonRootSingletonConstructor<'a> {
    name: Ident,
    ctor: &'a Constructor<OffAndSize>,
}

/// ```text
/// NAME: A1 A2 A3
/// NAME: B1 B2 B3
/// NAME: C1 C2 C3
/// ```
///
/// ```text
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
    name: Ident,
    qualified_name: TokenStream,
}

enum InstructionEnumVariant<'a> {
    Duplicate(EnumVariant),
    Unique(CtorEnumVariant<'a>),
}

#[derive(Copy, Clone)]
struct ContextItemSymbol<'a> {
    size: u8,
    expr: &'a PExpression,
}

#[derive(Copy, Clone)]
enum LiveSymbol<'a> {
    Subtable(Subtable<'a>),
    Value(&'a TokenFieldData),
    ContextBlockItem(ContextItemSymbol<'a>),
}

#[derive(Copy, Clone)]
enum ReadableSymbol<'a> {
    Token(&'a TokenFieldData),
    Context(&'a ContextFieldDef),
    InstNext,
}

#[derive(Copy, Clone)]
enum Subtable<'a> {
    Singleton(&'a NonRootSingletonConstructor<'a>),
    Multi(&'a MultiConstructor<'a>),
    Root,
}

impl<'a> LiveSymbol<'a> {
    fn to_type(self) -> TokenStream {
        match self {
            LiveSymbol::Subtable(s) => s.to_type(),
            LiveSymbol::Value(s) => s.qualified_name.clone(),
            LiveSymbol::ContextBlockItem(s) => s.to_type(),
        }
    }

    fn gen_call_disasm(
        self,
        generator: &RustCodeGenerator,
        input: &TokenStream,
        inst_next: &TokenStream,
    ) -> TokenStream {
        match self {
            LiveSymbol::Subtable(s) => s.gen_call_disasm(input, inst_next),
            LiveSymbol::Value(s) => s.gen_call_disasm(input),
            LiveSymbol::ContextBlockItem(s) => s.gen_call_disasm(generator, input, inst_next),
        }
    }

    fn find_offset(self, sym_str: &str, ctor: &Constructor<OffAndSize>) -> u64 {
        match self {
            LiveSymbol::Subtable(_) | LiveSymbol::Value(_) => {
                ctor.p_equation.find_offset(sym_str).unwrap()
            }
            LiveSymbol::ContextBlockItem(_) => 0,
        }
    }
}

impl<'a> ReadableSymbol<'a> {
    fn gen_read(
        self,
        generator: &RustCodeGenerator,
        input: &TokenStream,
        inst_next: &TokenStream,
    ) -> TokenStream {
        match self {
            ReadableSymbol::Token(field) => {
                let value = field.gen_call_disasm(input);
                quote! { #value.0 }
            }
            ReadableSymbol::Context(field) => generator.gen_read_context_field(field),
            ReadableSymbol::InstNext => inst_next.clone(),
        }
    }
}

impl<'a> Subtable<'a> {
    fn name(&self) -> Ident {
        match self {
            Subtable::Singleton(NonRootSingletonConstructor { name, .. })
            | Subtable::Multi(MultiConstructor { name, .. }) => name.clone(),
            Subtable::Root => instruction_ident(),
        }
    }

    fn to_type(&self) -> TokenStream {
        let name = self.name();
        quote!(#name)
    }

    fn gen_call_disasm(&self, input: &TokenStream, inst_next: &TokenStream) -> TokenStream {
        let typ = self.to_type();
        quote! { #typ::disasm(#input, #inst_next)? }
    }
}

impl<'a> ContextItemSymbol<'a> {
    fn to_type(&self) -> TokenStream {
        make_int_type(self.size, false).to_token_stream()
    }

    fn gen_call_disasm(
        &self,
        generator: &RustCodeGenerator,
        input: &TokenStream,
        inst_next: &TokenStream,
    ) -> TokenStream {
        generator.gen_p_expr(self.expr, input, inst_next)
    }
}

fn symbol_to_type_ident(s: &str) -> Ident {
    use heck::ToUpperCamelCase;
    let s = s.to_upper_camel_case();
    if s.is_empty() {
        format_ident!("Empty")
    } else {
        format_ident!("{s}")
    }
}

fn symbol_to_mod_ident(s: &str) -> Ident {
    use heck::ToSnakeCase;
    let s = s.to_snake_case();
    match syn::parse_str::<syn::Ident>(&s) {
        Ok(_) => format_ident!("{s}"),
        // If `syn` fails to parse the ident, it is probably a keyword,
        // so turn it into a valid identifier by putting an underscore afterwards
        Err(_) => format_ident!("{s}_"),
    }
}

fn collect_to_map_vec<K: Ord, V>(iter: impl Iterator<Item = (K, V)>) -> BTreeMap<K, Vec<V>> {
    iter.fold(BTreeMap::new(), |mut acc, (mnemonic, constructor)| {
        acc.entry(mnemonic).or_default().push(constructor);
        acc
    })
}

fn make_root_table<'a>(ctx: &'a SleighContext) -> RootTable<'a> {
    let iter = ctx
        .symbols
        .iter()
        .find_map(|(symbol, data)| match (symbol.as_str(), data) {
            (INSTRUCTION, SymbolData::Subtable(cs)) => Some(cs),
            _ => None,
        })
        .unwrap()
        .iter()
        .filter_map(|constructor| match constructor.display.toks.as_slice() {
            [DisplayToken::Symbol(mnemonic), ..] => Some((mnemonic, constructor)),
            _ => None,
        })
        .map(|(mnemonic, constructor)| (symbol_to_type_ident(&mnemonic), constructor));
    collect_to_map_vec(iter)
}

fn make_int_type(bytes: u8, signed: bool) -> Ident {
    let sign = if signed { "i" } else { "u" };
    let bits = bytes * 8;
    format_ident!("{sign}{bits}")
}

fn make_context_int_type(ctx: &SleighContext) -> Option<Ident> {
    match ctx.symbols.get(ctx.context_var_node.as_ref()?).unwrap() {
        SymbolData::VarNode { size, .. } => Some(make_int_type((*size).try_into().unwrap(), false)),
        _ => panic!(),
    }
}

fn make_token_fields(ctx: &SleighContext) -> BTreeMap<&str, TokenFieldData> {
    ctx.symbols
        .iter()
        .filter_map(|(symbol, data)| match data {
            SymbolData::Value { field, .. } => {
                let name = symbol_to_type_ident(&symbol);
                let parent = symbol_to_mod_ident(&field.parent_info.name);
                let qualified_name = quote!(#parent::#name);
                let data = TokenFieldData {
                    name,
                    parent,
                    qualified_name,
                    field: field.clone(),
                    inner_int_type: make_int_type(field.parent_info.size, field.field_info.signed),
                };
                Some((symbol.as_str(), data))
            }
            _ => None,
        })
        .collect()
}

fn non_root_iter(
    ctx: &SleighContext,
) -> impl Iterator<Item = (&str, &Vec<Constructor<OffAndSize>>)> {
    ctx.symbols
        .iter()
        .filter(|(symbol, _)| *symbol != INSTRUCTION)
        .filter_map(|(symbol, data)| match data {
            SymbolData::Subtable(cs) => Some((symbol.as_str(), cs)),
            _ => None,
        })
}

fn make_non_root_sing_ctors(ctx: &SleighContext) -> BTreeMap<&str, NonRootSingletonConstructor> {
    non_root_iter(ctx)
        .filter_map(|(name_str, cs)| match cs.as_slice() {
            [] => unreachable!(),
            [ctor] => {
                let name_ident = symbol_to_type_ident(name_str);
                Some((
                    name_str,
                    NonRootSingletonConstructor {
                        name: name_ident,
                        ctor,
                    },
                ))
            }
            _ => None,
        })
        .collect()
}

fn make_non_root_multi_ctors(ctx: &SleighContext) -> BTreeMap<&str, MultiConstructor> {
    non_root_iter(ctx)
        .filter_map(|(name_str, cs)| match cs.as_slice() {
            [] => unreachable!(),
            [_] => None,
            _ => {
                let name_ident = symbol_to_type_ident(name_str);
                let variants = cs
                    .iter()
                    .map(|ctor| {
                        let toks = &ctor.display.toks;
                        let variant_name = display_to_ident(toks);
                        let qualified_name = quote!(#name_ident::#variant_name);
                        CtorEnumVariant {
                            ctor,
                            inner: EnumVariant {
                                name: variant_name,
                                qualified_name,
                            },
                        }
                    })
                    .collect();
                Some((
                    name_str,
                    MultiConstructor {
                        name: name_ident,
                        variants,
                    },
                ))
            }
        })
        .collect()
}

fn make_mnemonic_enums<'a>(root_table: &RootTable<'a>) -> Vec<MultiConstructor<'a>> {
    root_table
        .iter()
        .filter(|(_, ctors)| ctors.len() > 1)
        .map(|(mnemonic, constructors)| {
            let variants = constructors
                .iter()
                .map(|ctor| {
                    let toks = &&ctor.display.toks;
                    let name = display_to_ident(toks.get(1..).unwrap());
                    let qualified_name = quote!(#mnemonic::#name);
                    CtorEnumVariant {
                        ctor,
                        inner: EnumVariant {
                            name,
                            qualified_name,
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

fn make_instruction_enum<'a>(root_table: &RootTable<'a>) -> Vec<InstructionEnumVariant<'a>> {
    root_table
        .iter()
        .map(|(mnemonic, constructors)| {
            let qualified_name = quote!(Instruction::#mnemonic);
            match constructors.as_slice() {
                [] => unimplemented!(),
                [ctor] => InstructionEnumVariant::Unique(CtorEnumVariant {
                    ctor,
                    inner: EnumVariant {
                        name: mnemonic.clone(),
                        qualified_name,
                    },
                }),
                _ => InstructionEnumVariant::Duplicate(EnumVariant {
                    name: mnemonic.clone(),
                    qualified_name,
                }),
            }
        })
        .collect()
}

impl<'a> RustCodeGenerator<'a> {
    pub fn new(ctx: &'a SleighContext) -> Self {
        let context_int_type = make_context_int_type(ctx);
        let token_fields = make_token_fields(ctx);
        let non_root_sing_ctors = make_non_root_sing_ctors(ctx);
        let non_root_mult_ctors = make_non_root_multi_ctors(ctx);
        let root_table = make_root_table(ctx);
        let mnemonic_enums = make_mnemonic_enums(&root_table);
        let instruction_enum = make_instruction_enum(&root_table);

        Self {
            ctx,
            context_int_type,
            token_fields,
            non_root_sing_ctors,
            non_root_mult_ctors,
            mnemonic_enums,
            instruction_enum,
        }
    }

    fn gen_addr_int_type(&self) -> Ident {
        make_int_type(self.ctx.default_space.size, false)
    }

    fn gen_read_context_field(&self, field: &ContextFieldDef) -> TokenStream {
        gen_compute_bit_range(
            &quote!(context),
            self.context_int_type.as_ref().unwrap(),
            field.low.into(),
            field.high.into(),
        )
    }

    fn lookup_subtable(&self, name: &str) -> Option<Subtable> {
        if name == INSTRUCTION {
            Some(Subtable::Root)
        } else {
            self.non_root_sing_ctors
                .get(name)
                .map(|c| Subtable::Singleton(c))
                .or_else(|| {
                    self.non_root_mult_ctors
                        .get(name)
                        .map(|c| Subtable::Multi(c))
                })
        }
    }

    fn lookup_live_symbol(
        &'a self,
        symbol: &str,
        ctor: &'a Constructor<OffAndSize>,
    ) -> Option<LiveSymbol<'a>> {
        self.token_fields
            .get(symbol)
            .map(LiveSymbol::Value)
            .or_else(|| self.lookup_subtable(symbol).map(LiveSymbol::Subtable))
            .or_else(|| {
                ctor.context_block
                    .context_list
                    .iter()
                    .find_map(|item| match item {
                        ContextListItem::Eq(s, expr) => {
                            if s == symbol {
                                Some(LiveSymbol::ContextBlockItem(ContextItemSymbol {
                                    size: self.ctx.default_space.size,
                                    expr,
                                }))
                            } else {
                                None
                            }
                        }
                        ContextListItem::Set(_, _) => todo!(),
                    })
            })
    }

    fn lookup_readable_symbol(&'a self, symbol: &str) -> ReadableSymbol<'a> {
        match &self.ctx.symbols[symbol] {
            SymbolData::Value { .. } => ReadableSymbol::Token(&self.token_fields[symbol]),
            SymbolData::Context { field, .. } => ReadableSymbol::Context(field),
            SymbolData::End => ReadableSymbol::InstNext,
            _ => panic!("invalid readable symbol"),
        }
    }

    fn gen_read_readable_symbol(
        &self,
        symbol: &str,
        input: &TokenStream,
        inst_next: &TokenStream,
    ) -> TokenStream {
        self.lookup_readable_symbol(symbol)
            .gen_read(self, input, inst_next)
    }

    fn token_to_live_symbol(
        &'a self,
        tok: &'a DisplayToken,
        ctor: &'a Constructor<OffAndSize>,
    ) -> Option<(&'a str, LiveSymbol<'a>)> {
        match tok {
            DisplayToken::Symbol(s) => self
                .lookup_live_symbol(s, ctor)
                .map(|sym| (s.as_str(), sym)),
            _ => None,
        }
    }

    fn iter_live_symbols(
        &'a self,
        ctor: &'a Constructor<OffAndSize>,
    ) -> impl Iterator<Item = (&'a str, LiveSymbol<'a>)> + 'a {
        ctor.display
            .toks
            .iter()
            .filter_map(|tok| self.token_to_live_symbol(tok, ctor))
    }

    fn gen_tuple_type(&self, inner_pub: bool, ctor: &Constructor<OffAndSize>) -> TokenStream {
        let types = self
            .iter_live_symbols(ctor)
            .map(|s| s.1.to_type())
            .collect::<Vec<TokenStream>>();
        let prefix = if inner_pub { quote!(pub) } else { quote!() };
        gen_tuple(&prefix, &types)
    }

    fn gen_enum_variant(
        &self,
        CtorEnumVariant {
            ctor,
            inner: EnumVariant { name, .. },
        }: &CtorEnumVariant,
    ) -> TokenStream {
        let tuple_type = self.gen_tuple_type(false, ctor);
        quote! { #name #tuple_type , }
    }

    fn gen_multi_ctors(&self, iter: impl Iterator<Item = &'a MultiConstructor<'a>>) -> TokenStream {
        iter.map(|MultiConstructor { name, variants }| {
            let derives = gen_derives();
            let variants = variants
                .iter()
                .map(|variant| self.gen_enum_variant(variant))
                .collect::<TokenStream>();
            quote! {
                #derives
                pub enum #name {
                    #variants
                }
            }
        })
        .collect()
    }

    fn gen_mnemonic_enums(&self) -> TokenStream {
        self.gen_multi_ctors(self.mnemonic_enums.iter())
    }

    fn gen_instruction_enum(&self) -> TokenStream {
        let derives = gen_derives();
        let variants = self
            .instruction_enum
            .iter()
            .map(|variant| match variant {
                InstructionEnumVariant::Duplicate(EnumVariant { name, .. }) => {
                    quote! { #name(#name), }
                }
                InstructionEnumVariant::Unique(CtorEnumVariant {
                    ctor,
                    inner: EnumVariant { name, .. },
                }) => {
                    let tuple_type = self.gen_tuple_type(false, ctor);
                    quote! { #name #tuple_type , }
                }
            })
            .collect::<TokenStream>();
        quote! {
            #derives
            pub enum Instruction {
                #variants
            }
        }
    }

    fn gen_token_types(&self) -> TokenStream {
        let derives = gen_derives();
        let token_defs = self
            .token_fields
            .values()
            .map(|field| (&field.parent, field));
        let token_defs = collect_to_map_vec(token_defs);
        token_defs
            .iter()
            .map(|(parent, fields)| {
                let fields = fields
                    .iter()
                    .map(
                        |TokenFieldData {
                             name,
                             inner_int_type,
                             ..
                         }| {
                            quote! {
                                #derives
                                pub struct #name(pub #inner_int_type);
                            }
                        },
                    )
                    .collect::<TokenStream>();
                quote! {
                    pub mod #parent {
                        #fields
                    }
                }
            })
            .collect()
    }

    fn gen_non_root_singleton_constructor_types(&self) -> TokenStream {
        let derives = gen_derives();
        self.non_root_sing_ctors
            .values()
            .map(|NonRootSingletonConstructor { name, ctor, .. }| {
                let tuple_type = self.gen_tuple_type(true, ctor);
                quote! {
                    #derives
                    pub struct #name #tuple_type ;
                }
            })
            .collect()
    }

    fn gen_non_root_multi_constructor_types(&self) -> TokenStream {
        self.gen_multi_ctors(self.non_root_mult_ctors.values())
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
            .values()
            .map(|data| {
                let write = gen_int_write(data.field.parent_info.size, quote!(self.0));
                gen_display_impl(&data.qualified_name, write)
            })
            .collect()
    }

    fn gen_match_pattern(
        &self,
        CtorEnumVariant {
            ctor,
            inner: EnumVariant { qualified_name, .. },
        }: &CtorEnumVariant,
    ) -> TokenStream {
        let tuple_destruct = self.gen_tuple_destruct(ctor);
        quote!(#qualified_name #tuple_destruct)
    }

    fn gen_match_pattern_insn_dup(
        &self,
        EnumVariant { qualified_name, .. }: &EnumVariant,
    ) -> TokenStream {
        quote!(#qualified_name(_0))
    }

    fn gen_destruct_stmt(
        &self,
        NonRootSingletonConstructor { name, ctor, .. }: &NonRootSingletonConstructor,
    ) -> TokenStream {
        let tuple_destruct = self.gen_tuple_destruct(ctor);
        quote! { let #name #tuple_destruct = self; }
    }

    fn gen_non_root_sing_ctor_types_display_impl(&self) -> TokenStream {
        self.non_root_sing_ctors
            .values()
            .map(|nrsc @ NonRootSingletonConstructor { name, .. }| {
                let destruct_stmt = self.gen_destruct_stmt(nrsc);
                let writes = self.gen_display_write_stmts(&nrsc.ctor);
                let body = quote! {
                    #destruct_stmt
                    #writes
                    Ok(())
                };
                gen_display_impl(&quote!(#name), body)
            })
            .collect()
    }

    fn gen_multi_ctor_display_impl_arm(&self, variant: &CtorEnumVariant) -> TokenStream {
        let match_pattern = self.gen_match_pattern(variant);
        let writes = self.gen_display_write_stmts(&variant.ctor);
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
            gen_display_impl(&quote!(#name), body)
        })
        .collect()
    }

    fn gen_non_root_multi_ctor_types_display_impl(&self) -> TokenStream {
        self.gen_multi_ctor_display_impl(self.non_root_mult_ctors.values())
    }

    fn iter_display_tok_fields(
        &'a self,
        ctor: &'a Constructor<OffAndSize>,
    ) -> impl Iterator<Item = (Option<(usize, LiveSymbol)>, &'a DisplayToken)> {
        let toks = &ctor.display.toks;

        // Skip first token if it's a space
        let toks = match toks.as_slice() {
            [DisplayToken::Space, rest @ ..] => rest,
            toks => toks,
        };

        toks.iter()
            .scan(0, |state, tok| match self.token_to_live_symbol(tok, ctor) {
                Some((_, live_symbol)) => {
                    let i = *state;
                    *state += 1;
                    Some((Some((i, live_symbol)), tok))
                }
                None => Some((None, tok)),
            })
    }

    fn gen_tuple_destruct(&self, ctor: &Constructor<OffAndSize>) -> TokenStream {
        let bindings = self
            .iter_live_symbols(ctor)
            .enumerate()
            .map(|(i, _)| {
                let ident = format_ident!("_{i}");
                quote!(#ident)
            })
            .collect::<Vec<TokenStream>>();
        gen_tuple(&quote!(), &bindings)
    }

    fn gen_display_write_stmts(&self, ctor: &Constructor<OffAndSize>) -> TokenStream {
        self.iter_display_tok_fields(ctor)
            .map(|(opt_field, tok)| match (opt_field, tok) {
                (None, DisplayToken::Caret) => quote! {},
                (None, DisplayToken::String(s) | DisplayToken::Symbol(s)) => {
                    quote! { f.write_str(#s)?; }
                }
                (None, DisplayToken::Char(c)) => quote! { f.write_char(#c)?; },
                (None, DisplayToken::Space) => quote! { f.write_char(' ')?; },
                (
                    Some((i, LiveSymbol::Subtable(_) | LiveSymbol::Value(_))),
                    DisplayToken::Symbol(_),
                ) => {
                    let var = format_ident!("_{i}");
                    quote! { write!(f, "{}", #var)?; }
                }
                (Some((i, LiveSymbol::ContextBlockItem(ctx_block))), DisplayToken::Symbol(_)) => {
                    let var = format_ident!("_{i}");
                    let var = quote!(#var);
                    let write = gen_int_write(ctx_block.size, var);
                    quote! { #write?; }
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
                InstructionEnumVariant::Duplicate(variant) => {
                    let match_pattern = self.gen_match_pattern_insn_dup(variant);
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
        gen_display_impl(&quote!(Instruction), body)
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

    fn gen_p_expr(
        &self,
        expr: &PExpression,
        input: &TokenStream,
        inst_next: &TokenStream,
    ) -> TokenStream {
        use PExpression::*;
        match expr {
            ConstantValue(x) => {
                let x = Literal::u64_unsuffixed(*x);
                quote!(#x)
            }
            Symbol(s) => self.gen_read_readable_symbol(s, input, inst_next),
            Bin(bin) => {
                let PExpressionBin { op, l, r } = &**bin;
                let op = op.gen();
                let l = self.gen_p_expr(l, input, inst_next);
                let r = self.gen_p_expr(r, input, inst_next);
                quote!(#l.#op(#r))
            }
            Unary(unary) => {
                let PExpressionUnary { op, operand } = &**unary;
                let op = op.gen();
                let operand = self.gen_p_expr(operand, input, inst_next);
                quote!((#op #operand))
            }
        }
    }

    fn gen_tok_disasms(&self) -> TokenStream {
        self.token_fields
            .values()
            .map(|field| gen_tok_disasm(self.ctx.endian, field))
            .collect()
    }

    fn gen_check_pattern(
        &self,
        input: &TokenStream,
        inst_next: &TokenStream,
        PatternEquation {
            inner,
            type_data: OffAndSize { off, .. },
        }: &PatternEquation<OffAndSize>,
    ) -> TokenStream {
        match inner {
            PatternEquationInner::EllEq(ell_eq) => match &ell_eq.ell_rt.atomic {
                Atomic::Constraint(Constraint::Compare(ConstraintCompare { op, symbol, expr })) => {
                    let op = op.gen();
                    let expr = self.gen_p_expr(expr, input, inst_next);
                    let input = gen_input_slice(input, *off);
                    let lhs = self.gen_read_readable_symbol(symbol, &input, inst_next);
                    quote! { (#lhs #op #expr) }
                }
                Atomic::Constraint(Constraint::Symbol(_)) => quote!(true),
                Atomic::Parenthesized(p) => self.gen_check_pattern(input, inst_next, p),
            },
            PatternEquationInner::Bin(bin) => {
                let PatternEquationBin { op, l, r } = &**bin;
                let l = self.gen_check_pattern(input, inst_next, l);
                let r = self.gen_check_pattern(input, inst_next, r);
                match op {
                    PatternEquationBinOp::And | PatternEquationBinOp::Cat => {
                        quote! { ( #l && #r ) }
                    }
                    PatternEquationBinOp::Or => quote! { ( #l || #r ) },
                }
            }
        }
    }

    fn gen_disasm_ctor(
        &self,
        name: &TokenStream,
        input: &TokenStream,
        addr: &TokenStream,
        ctor: &Constructor<OffAndSize>,
    ) -> TokenStream {
        let construct_args = self
            .iter_live_symbols(ctor)
            .map(|(sym_str, sym_live)| {
                let off = sym_live.find_offset(sym_str, ctor);
                let input = gen_input_slice(input, off);
                sym_live.gen_call_disasm(self, &input, addr)
            })
            .collect::<Vec<TokenStream>>();
        let construct_args = gen_tuple(&quote!(), &construct_args);
        quote!(#name #construct_args)
    }

    fn gen_non_root_sing_ctor_disasm(
        &self,
        NonRootSingletonConstructor { name, ctor, .. }: &NonRootSingletonConstructor,
    ) -> TokenStream {
        let input = quote!(input);
        let inst_next = quote!(inst_next);
        let check_pattern = self.gen_check_pattern(&input, &inst_next, &ctor.p_equation);
        let disasm_ctor = self.gen_disasm_ctor(&quote!(#name), &input, &inst_next, ctor);
        let addr_int_type = self.gen_addr_int_type();
        quote! {
            impl #name {
                fn disasm(input: &[u8], inst_next: #addr_int_type) -> Option<Self> {
                    if #check_pattern {
                        Some(#disasm_ctor)
                    } else {
                        None
                    }
                }
            }
        }
    }

    fn gen_non_root_sing_ctor_disasms(&self) -> TokenStream {
        self.non_root_sing_ctors
            .values()
            .map(|c| self.gen_non_root_sing_ctor_disasm(c))
            .collect()
    }

    fn gen_mult_ctor_disasm(
        &self,
        MultiConstructor { name, variants }: &MultiConstructor,
        is_mnemonic_enum: bool,
    ) -> TokenStream {
        let param_name = if is_mnemonic_enum {
            quote!(addr)
        } else {
            quote!(inst_next)
        };
        let addr_int_type = self.gen_addr_int_type();
        let checks = variants
            .iter()
            .map(
                |CtorEnumVariant {
                     ctor,
                     inner: EnumVariant { qualified_name, .. },
                 }| {
                    let input = quote!(input);
                    let inst_size =
                        Literal::u64_unsuffixed(ctor.p_equation.type_data.size.unwrap());
                    let inst_next = if is_mnemonic_enum {
                        quote!(addr + #inst_size)
                    } else {
                        quote!(inst_next)
                    };
                    let check_pattern =
                        self.gen_check_pattern(&input, &inst_next, &ctor.p_equation);
                    let disasm_ctor =
                        self.gen_disasm_ctor(qualified_name, &input, &inst_next, ctor);
                    quote! {
                        if #check_pattern {
                            Some(#disasm_ctor)
                        } else
                    }
                },
            )
            .collect::<TokenStream>();
        quote! {
            impl #name {
                fn disasm(input: &[u8], #param_name: #addr_int_type) -> Option<Self> {
                    if false {
                        unreachable!()
                    } else #checks {
                        None
                    }
                }
            }
        }
    }

    fn gen_non_root_mult_ctor_disasms(&self) -> TokenStream {
        self.non_root_mult_ctors
            .values()
            .map(|c| self.gen_mult_ctor_disasm(c, false))
            .collect()
    }

    fn gen_mnemonic_enum_disasms(&self) -> TokenStream {
        self.mnemonic_enums
            .iter()
            .map(|c| self.gen_mult_ctor_disasm(c, true))
            .collect()
    }

    fn gen_insn_enum_disasm(&self) -> TokenStream {
        let addr_int_type = self.gen_addr_int_type();
        let params = match &self.context_int_type {
            Some(context_int_type) => quote! {
                input: &[u8],
                context: #context_int_type,
                addr: #addr_int_type
            },
            None => quote! { input: &[u8], addr: #addr_int_type },
        };
        let checks = self
            .instruction_enum
            .iter()
            .map(|variant| {
                let (check_pattern, disasm_ctor) = match variant {
                    InstructionEnumVariant::Duplicate(EnumVariant {
                        name,
                        qualified_name,
                    }) => {
                        let disasm = quote!(#name::disasm(input, addr));
                        (quote!(#disasm.is_some()), quote!(#qualified_name(#disasm?)))
                    }
                    InstructionEnumVariant::Unique(CtorEnumVariant { ctor, inner }) => {
                        let input = quote!(input);
                        let inst_size =
                            Literal::u64_unsuffixed(ctor.p_equation.type_data.size.expect("TODO"));
                        let inst_next = quote!(addr + #inst_size);
                        (
                            self.gen_check_pattern(&input, &inst_next, &ctor.p_equation),
                            self.gen_disasm_ctor(&inner.qualified_name, &input, &inst_next, ctor),
                        )
                    }
                };
                quote! {
                    if #check_pattern {
                        Some(#disasm_ctor)
                    } else
                }
            })
            .collect::<TokenStream>();
        quote! {
            impl Instruction {
                pub fn disasm(#params) -> Option<Self> {
                    if false {
                        unreachable!()
                    } else #checks {
                        None
                    }
                }
            }
        }
    }

    fn gen_disasm(&self) -> TokenStream {
        let tok_reads = self.gen_tok_disasms();
        let non_root_sing_ctor_disasms = self.gen_non_root_sing_ctor_disasms();
        let non_root_mult_ctor_disasms = self.gen_non_root_mult_ctor_disasms();
        let mnemonic_enum_disasms = self.gen_mnemonic_enum_disasms();
        let insn_enum_disasm = self.gen_insn_enum_disasm();
        quote! {
            #tok_reads
            #non_root_sing_ctor_disasms
            #non_root_mult_ctor_disasms
            #mnemonic_enum_disasms
            #insn_enum_disasm
        }
    }

    pub fn out(&self) -> TokenStream {
        let types = self.gen_all_types();
        let display_impls = self.gen_all_display_impls();
        let disasm = self.gen_disasm();
        quote! {
            #types
            #display_impls
            #disasm
        }
    }
}

fn char_name(c: char) -> String {
    match c {
        '#' => "hash".into(),
        ',' => "comma".into(),
        '(' => "open paren".into(),
        ')' => "close paren".into(),
        '[' => "open bracket".into(),
        ']' => "close bracket".into(),
        '+' => "plus".into(),
        '*' => "star".into(),
        ':' => "colon".into(),
        '.' => "dot".into(),
        '/' => "slash".into(),
        ' ' | '_' => " ".into(),
        c if c.is_ascii_alphabetic() => c.into(),
        _ => panic!("unnamed char {c}"),
    }
}

fn display_to_ident(toks: &[DisplayToken]) -> Ident {
    dbg!(toks);
    let s = toks
        .iter()
        .map(|tok| match tok {
            DisplayToken::Caret => "caret".to_owned(),
            DisplayToken::String(s) => s.chars().map(char_name).collect::<String>(),
            DisplayToken::Char(c) => char_name(*c),
            DisplayToken::Space => "".to_owned(),
            DisplayToken::Symbol(s) => s.to_owned(),
        })
        .collect::<Vec<String>>()
        .join(" ");
    symbol_to_type_ident(&s)
}

fn gen_tuple(prefix: &TokenStream, values: &[TokenStream]) -> TokenStream {
    match values {
        [] => quote!(),
        [value] => {
            quote!((#prefix #value))
        }
        _ => {
            quote!((#(#prefix #values),*))
        }
    }
}

fn gen_display_impl(name: &TokenStream, body: TokenStream) -> TokenStream {
    quote! {
        impl std::fmt::Display for #name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                use std::fmt::Write;
                #body
            }
        }
    }
}

fn instruction_ident() -> Ident {
    format_ident!("Instruction")
}

fn endian_rust_abbrev(endian: Endian) -> Ident {
    match endian {
        Endian::Big => format_ident!("be"),
        Endian::Little => format_ident!("le"),
    }
}

fn gen_from_endian_bytes(endian: Endian) -> Ident {
    let endian = endian_rust_abbrev(endian);
    format_ident!("from_{endian}_bytes")
}

fn gen_tok_disasm(
    default_endian: Endian,
    TokenFieldData {
        qualified_name,
        field:
            TokenField {
                field_info: FieldDef { low, high, .. },
                parent_info: TokenParentInfo { size, endian, .. },
            },
        inner_int_type,
        ..
    }: &TokenFieldData,
) -> TokenStream {
    let endian = endian.unwrap_or(default_endian);
    let from_endian_bytes = gen_from_endian_bytes(endian);
    let token_size = Literal::u8_unsuffixed(*size);
    let compute_bit_range =
        gen_compute_bit_range(&quote!(tok), inner_int_type, (*low).into(), (*high).into());
    quote! {
        impl #qualified_name {
            fn disasm(bytes: &[u8]) -> Option<Self> {
                let bytes = bytes.get(..#token_size)?;
                let bytes: [u8; #token_size] = bytes.try_into().ok()?;
                let tok = #inner_int_type::#from_endian_bytes(bytes);
                let out = #compute_bit_range;
                Some(Self(out))
            }
        }
    }
}

fn gen_compute_bit_range(
    input_int: &TokenStream,
    int_type: &Ident,
    low: u64,
    high: u64,
) -> TokenStream {
    quote! {
        {
            let input_int = #input_int;
            let mask = (((1 << (#high + 1)) - 1) - ((1 << #low) - 1)) as #int_type;
            (input_int & mask) >> #low
        }
    }
}

fn gen_input_slice(input: &TokenStream, off: u64) -> TokenStream {
    let off = Literal::u64_unsuffixed(off);
    quote!(#input.get(#off..)?)
}

fn gen_int_write(size_in_bytes: u8, expr: TokenStream) -> TokenStream {
    let num_digits = size_in_bytes * 2;
    let fmt = format!("{{:0{num_digits}X}}");
    quote!(write!(f, #fmt, #expr))
}

fn gen_derives() -> TokenStream {
    quote! {
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
    }
}

trait Generate {
    fn gen(self) -> TokenStream;
}

impl Generate for ConstraintCompareOp {
    fn gen(self) -> TokenStream {
        use ConstraintCompareOp::*;
        match self {
            Equal => quote!(==),
            NotEqual => quote!(!=),
            Less => quote!(<),
            LessEqual => quote!(<=),
            Greater => quote!(>),
            GreaterEqual => quote!(>=),
        }
    }
}

impl Generate for PExpressionBinOp {
    fn gen(self) -> TokenStream {
        use PExpressionBinOp::*;
        match self {
            Add => quote!(wrapping_add),
            Sub => quote!(wrapping_sub),
            Mult => quote!(mul),
            LeftShift => quote!(shl),
            RightShift => quote!(shr),
            And => quote!(bitand),
            Or => quote!(bitor),
            Xor => quote!(bitxor),
            Div => quote!(div),
        }
    }
}

impl Generate for PExpressionUnaryOp {
    fn gen(self) -> TokenStream {
        use PExpressionUnaryOp::*;
        match self {
            Minus => quote!(-),
            Not => quote!(~),
        }
    }
}

trait FindOffset {
    fn find_offset(&self, target_symbol: &str) -> Option<u64>;
}

impl FindOffset for PatternEquation<OffAndSize> {
    fn find_offset(&self, target_symbol: &str) -> Option<u64> {
        let Self { inner, type_data } = self;
        match inner {
            PatternEquationInner::EllEq(ell_eq) => match &ell_eq.ell_rt.atomic {
                Atomic::Constraint(constraint) => match constraint {
                    Constraint::Compare(_) => None,
                    Constraint::Symbol(s) => {
                        if s == target_symbol {
                            Some(type_data.off)
                        } else {
                            None
                        }
                    }
                },
                Atomic::Parenthesized(p) => p.find_offset(target_symbol),
            },
            PatternEquationInner::Bin(bin) => bin
                .l
                .find_offset(target_symbol)
                .or_else(|| bin.r.find_offset(target_symbol)),
        }
    }
}

impl TokenFieldData {
    fn gen_call_disasm(&self, input: &TokenStream) -> TokenStream {
        let Self { qualified_name, .. } = self;
        quote! { #qualified_name::disasm(#input)? }
    }
}
