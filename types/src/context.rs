use std::collections::HashMap;

use crate::ast::*;

#[derive(Debug, Copy, Clone)]
pub struct OffAndSize {
    pub off: u64,
    pub size: u64,
}

#[derive(Debug, Clone)]
pub struct TokenField {
    pub parent_info: TokenParentInfo,
    pub field_info: FieldDef,
}

pub enum SymbolData {
    // Space name
    //
    // `define space NAME`
    Space(SpaceDef),

    /// Main (non-field) token name
    ///
    /// `define token NAME`
    Token,

    /// Custom pcode op name
    ///
    /// `define pcodeop NAME;`
    UserOp,

    /// Macro name
    ///
    /// `macro NAME`
    Macro,

    /// Table header name
    ///
    /// `NAME:`
    // The default if none (the root constructor) seems to be "instruction"
    Subtable(Vec<Constructor<OffAndSize>>),

    /// Token field
    ///
    /// `NAME = (0,7)`
    Value(TokenField),

    /// Attached value
    ///
    /// `attach values [ NAME ]`
    ValueMap,

    /// Context field name
    ///
    /// `define context contextreg NAME=`
    Context(ContextFieldDef),

    /// Attached name
    ///
    /// `attach names [ NAME ]`
    Name,

    /// Var node definition
    ///
    /// `define space_name offset=0 size=1 [ NAME ];`
    VarNode(OffAndSize),

    /// Attached variable
    ///
    /// `attach variables [ NAME ]`
    VarList,

    /// Predefined `inst_start` symbol
    Start,

    /// Predefined `inst_next` symbol
    End,

    /// Predefined `inst_next2` symbol
    Next2,

    /// Defined bit range
    ///
    /// `define bitrange NAME=`
    Bit,
}

pub struct SleighContext {
    pub endian: Endian,
    align: u8,
    pub symbols: HashMap<String, SymbolData>,
    pub default_space: SpaceDef,
}

impl SleighContext {
    fn lookup_symbol_size(&self, symbol: &str) -> Option<u64> {
        match self.symbols.get(symbol)? {
            SymbolData::Subtable(c) => Some(c.get(0)?.p_equation.type_data.size),
            SymbolData::Value(token_field) => Some(token_field.parent_info.size.into()),
            SymbolData::VarNode(off_size) => Some(off_size.size),
            _ => None,
        }
    }

    pub fn new(sleigh: &Sleigh) -> SleighContext {
        let mut ret = SleighContext {
            endian: Endian::Little,
            align: 0,
            symbols: HashMap::new(),
            default_space: SpaceDef {
                name: String::new(),
                typ: SpaceType::Ram,
                size: 0,
                default: false,
            },
        };

        ret.symbols
            .insert("inst_start".to_owned(), SymbolData::Start);
        ret.symbols.insert("inst_next".to_owned(), SymbolData::End);
        ret.symbols
            .insert("inst_next2".to_owned(), SymbolData::Next2);

        for def in &sleigh.defs {
            match def {
                Def::EndianDef(EndianDef { endian }) => ret.endian = *endian,
                Def::AlignDef(AlignDef { align }) => ret.align = *align,
                Def::Definition(def) => match def {
                    Definition::TokenDef(TokenDef { info, fields }) => {
                        ret.symbols.insert(info.name.to_owned(), SymbolData::Token);
                        for field in fields {
                            ret.symbols.insert(
                                field.name.to_owned(),
                                SymbolData::Value(TokenField {
                                    parent_info: info.clone(),
                                    field_info: field.clone(),
                                }),
                            );
                        }
                    }
                    Definition::ContextDef(ContextDef { var_sym, fields }) => {
                        for field in fields {
                            ret.symbols
                                .insert(field.name.to_owned(), SymbolData::Context(field.clone()));
                        }
                    }
                    Definition::SpaceDef(space_def) => {
                        ret.symbols
                            .insert(space_def.name.clone(), SymbolData::Space(space_def.clone()));
                        if space_def.default {
                            ret.default_space = space_def.clone()
                        }
                    }
                    Definition::VarNodeDef(var_node_def) => {
                        for (i, name) in var_node_def.names.iter().enumerate() {
                            if name != "_" {
                                ret.symbols.insert(
                                    name.clone(),
                                    SymbolData::VarNode(OffAndSize {
                                        off: var_node_def.offset + i as u64 * var_node_def.size,
                                        size: var_node_def.size,
                                    }),
                                );
                            }
                        }
                    }
                    Definition::BitRangeDef(_) => todo!(),
                    Definition::PCodeOpDef(p_code_op_def) => {
                        for op in &p_code_op_def.ops {
                            ret.symbols.insert(op.clone(), SymbolData::UserOp);
                        }
                    }
                    Definition::ValueAttach(_) => todo!(),
                    Definition::NameAttach(_) => todo!(),
                    Definition::VarAttach(_) => todo!(),
                },
                Def::Constructorlike(constructorlike) => match constructorlike {
                    Constructorlike::Constructor(constructor) => {
                        let Constructor {
                            header: id,
                            display,
                            p_equation,
                            context_block,
                            rtl_body,
                        } = constructor.clone();
                        let constructor = Constructor {
                            header: id.clone(),
                            display,
                            p_equation: ret.type_pattern(&p_equation, 0),
                            context_block,
                            rtl_body,
                        };
                        let sym_data = ret
                            .symbols
                            .entry(id)
                            .or_insert(SymbolData::Subtable(Vec::new()));
                        let sym_data = match sym_data {
                            SymbolData::Subtable(constructors) => constructors,
                            _ => panic!("constructor name conflict"),
                        };
                        sym_data.push(constructor);
                    }
                    Constructorlike::MacroDef(_) => {}
                    Constructorlike::WithBlock(_) => todo!(),
                },
            }
        }

        ret
    }

    fn type_pattern(
        &mut self,
        PatternEquation {
            inner,
            type_data: (),
        }: &PatternEquation<()>,
        off: u64,
    ) -> PatternEquation<OffAndSize> {
        match inner {
            PatternEquationInner::EllEq(ell_eq) => {
                let EllEq {
                    ellipsis_left,
                    ell_rt:
                        EllRt {
                            atomic,
                            ellipsis_right,
                        },
                } = *ell_eq.clone();
                match atomic {
                    Atomic::Constraint(constraint) => match constraint {
                        Constraint::Compare(ConstraintCompare {
                            op: _,
                            ref symbol,
                            expr: _,
                        })
                        | Constraint::Symbol(ref symbol) => PatternEquation {
                            inner: PatternEquationInner::EllEq(Box::new(EllEq {
                                ellipsis_left,
                                ell_rt: EllRt {
                                    atomic: Atomic::Constraint(constraint.clone()),
                                    ellipsis_right,
                                },
                            })),
                            type_data: OffAndSize {
                                off,
                                size: { self.lookup_symbol_size(symbol).unwrap() },
                            },
                        },
                    },
                    Atomic::Parenthesized(p_eq) => self.type_pattern(&p_eq, off),
                }
            }
            PatternEquationInner::Bin(bin) => {
                let PatternEquationBin { op, l, r } = &**bin;
                match op {
                    PatternEquationBinOp::And | PatternEquationBinOp::Or => {
                        let l = self.type_pattern(l, off);
                        let r = self.type_pattern(r, off);
                        assert_eq!(l.type_data.off, r.type_data.off);
                        let type_data = OffAndSize {
                            off: l.type_data.off,
                            size: u64::max(l.type_data.size, r.type_data.size),
                        };
                        PatternEquation {
                            inner: PatternEquationInner::Bin(Box::new(PatternEquationBin {
                                op: op.clone(),
                                l,
                                r,
                            })),
                            type_data,
                        }
                    }
                    PatternEquationBinOp::Cat => {
                        let l = self.type_pattern(l, off);
                        let mid = l.type_data.off + l.type_data.size;
                        let r = self.type_pattern(r, mid);
                        assert_eq!(mid, r.type_data.off);
                        let off = l.type_data.off;
                        let size = l.type_data.size + r.type_data.size;
                        PatternEquation {
                            inner: PatternEquationInner::Bin(Box::new(PatternEquationBin {
                                op: op.clone(),
                                l,
                                r,
                            })),
                            type_data: OffAndSize { off, size },
                        }
                    }
                }
            }
        }
    }

    fn compute_token_symbol(&self, symbol: &str, input: &[u8]) -> u8 {
        if let Some(SymbolData::Value(TokenField {
            field_info: FieldDef { low, high, .. },
            ..
        })) = self.symbols.get(symbol)
        {
            compute_bit_range(*input.get(0).unwrap(), *low, *high)
        } else {
            panic!("token field {symbol} not found")
        }
    }

    fn compute_p_expression(&self, expr: &PExpression) -> u64 {
        match expr {
            PExpression::ConstantValue(v) => *v,
            PExpression::Symbol(_) => todo!(),
            PExpression::Bin(binary_expr) => {
                let PExpressionBin { op, l, r } = &**binary_expr;
                let l = self.compute_p_expression(l);
                let r = self.compute_p_expression(r);
                match op {
                    PExpressionBinOp::Add => l + r,
                    PExpressionBinOp::Sub => l - r,
                    PExpressionBinOp::Mult => l * r,
                    PExpressionBinOp::LeftShift => l << r,
                    PExpressionBinOp::RightShift => l >> r,
                    PExpressionBinOp::And => l & r,
                    PExpressionBinOp::Or => l | r,
                    PExpressionBinOp::Xor => l ^ r,
                    PExpressionBinOp::Div => l / r,
                }
            }
            PExpression::Unary(unary_expr) => {
                let PExpressionUnary { op, operand } = &**unary_expr;
                let operand = self.compute_p_expression(operand);
                match op {
                    PExpressionUnaryOp::Minus => -(operand as i64) as u64,
                    PExpressionUnaryOp::Not => !operand,
                }
            }
        }
    }

    fn check_p_equation(
        &self,
        PatternEquation { inner, type_data }: &PatternEquation<OffAndSize>,
        input: &[u8],
    ) -> bool {
        let input = input.get(type_data.off as usize..).unwrap();
        match inner {
            PatternEquationInner::EllEq(ell_eq) => {
                let EllEq {
                    ellipsis_left: _,
                    ell_rt:
                        EllRt {
                            atomic,
                            ellipsis_right: _,
                        },
                } = &**ell_eq;
                match atomic {
                    Atomic::Constraint(constraint) => match constraint {
                        Constraint::Compare(ConstraintCompare { op, symbol, expr }) => {
                            let l = self.compute_token_symbol(&symbol, input);
                            let r = self.compute_p_expression(&expr);
                            let l = l as u64;
                            match op {
                                ConstraintCompareOp::Equal => l == r,
                                ConstraintCompareOp::NotEqual => l != r,
                                ConstraintCompareOp::Less => l < r,
                                ConstraintCompareOp::LessEqual => l <= r,
                                ConstraintCompareOp::Greater => l > r,
                                ConstraintCompareOp::GreaterEqual => l >= r,
                            }
                        }
                        Constraint::Symbol(_) => true,
                    },
                    Atomic::Parenthesized(p_eq) => self.check_p_equation(&p_eq, input),
                }
            }
            PatternEquationInner::Bin(bin) => {
                let PatternEquationBin { op, l, r } = &**bin;
                let l = self.check_p_equation(&l, input);
                let r = self.check_p_equation(&r, input);
                match op {
                    PatternEquationBinOp::And | PatternEquationBinOp::Cat => l && r,
                    PatternEquationBinOp::Or => l || r,
                }
            }
        }
    }

    pub fn disasm_insn(&self, off: u64, table_header: &str, input: &[u8], out: &mut String) {
        match self.symbols.get(table_header) {
            Some(SymbolData::Subtable(constructors)) => {
                if let Some(constructor) = constructors
                    .iter()
                    .find(|constructor| self.check_p_equation(&constructor.p_equation, input))
                {
                    for tok in &constructor.display.toks {
                        match tok {
                            DisplayToken::Caret => {}
                            DisplayToken::String(s) => out.push_str(&s),
                            DisplayToken::Char(c) => out.push(*c),
                            DisplayToken::Space => out.push(' '),
                            DisplayToken::Symbol(s) => {
                                if let Some(OffAndSize { off, size: _ }) =
                                    p_equation_symm_off(&constructor.p_equation, &s)
                                {
                                    self.disasm_insn(off, &s, input, out)
                                } else {
                                    out.push_str(&s)
                                }
                            }
                        }
                    }
                } else {
                    todo!()
                }
            }
            Some(SymbolData::Value(TokenField {
                field_info: FieldDef { low, high, .. },
                ..
            })) => {
                let b = compute_bit_range(*input.get(off as usize).unwrap(), *low, *high);
                out.push_str(&format!("{b:#x}"))
            }
            Some(_) => todo!(),
            None => out.push_str(table_header),
        }
    }
}

fn compute_bit_range(b: u8, low: u8, high: u8) -> u8 {
    let low = low as u64;
    let high = high as u64;
    let mask = (((1 << (high + 1)) - 1) - ((1 << low) - 1)) as u8;
    assert!(low <= high && high <= 7);
    (b & mask) >> low
}

fn p_equation_symm_off(
    PatternEquation { inner, type_data }: &PatternEquation<OffAndSize>,
    target_symbol: &str,
) -> Option<OffAndSize> {
    match inner {
        PatternEquationInner::EllEq(ell_eq) => {
            let EllEq {
                ellipsis_left: _,
                ell_rt:
                    EllRt {
                        atomic,
                        ellipsis_right: _,
                    },
            } = &**ell_eq;
            match atomic {
                Atomic::Constraint(constraint) => match constraint {
                    Constraint::Compare(ConstraintCompare {
                        op: _,
                        symbol,
                        expr: _,
                    })
                    | Constraint::Symbol(symbol) => {
                        if symbol == target_symbol {
                            Some(*type_data)
                        } else {
                            None
                        }
                    }
                },
                Atomic::Parenthesized(p_eq) => p_equation_symm_off(p_eq, target_symbol),
            }
        }
        PatternEquationInner::Bin(bin) => {
            let PatternEquationBin { op: _, l, r } = &**bin;
            p_equation_symm_off(l, target_symbol).or_else(|| p_equation_symm_off(r, target_symbol))
        }
    }
}
