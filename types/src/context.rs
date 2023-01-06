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
    Value {
        field: TokenField,

        /// Attached variable
        ///
        /// `attach variables [ NAME ] ATTACHED_VARS;`
        attached_vars: Option<Vec<String>>,
    },

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
            SymbolData::Value { field, .. } => Some(field.parent_info.size.into()),
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
                                SymbolData::Value {
                                    field: TokenField {
                                        parent_info: info.clone(),
                                        field_info: field.clone(),
                                    },
                                    attached_vars: None,
                                },
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
                    Definition::VarAttach(var_attach) => {
                        for field in &var_attach.fields {
                            if let Some(SymbolData::Value { attached_vars, .. }) =
                                ret.symbols.get_mut(field)
                            {
                                *attached_vars = Some(var_attach.registers.clone());
                            } else {
                                panic!("attach variable with missing field")
                            }
                        }
                    }
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

        let mut table = HashMap::<String, Vec<Constructor<OffAndSize>>>::new();

        // Sort constructors
        let mut z3cfg = z3::Config::new();
        z3cfg.set_proof_generation(false);
        z3cfg.set_model_generation(false);
        z3cfg.set_timeout_msec(1000);
        let z3ctx = z3::Context::new(&z3cfg);
        for (name, symbol_data) in &ret.symbols {
            if let SymbolData::Subtable(ctors) = symbol_data {
                use itertools::Itertools;
                use petgraph::graph::{Graph, NodeIndex};

                let max_size = ctors
                    .iter()
                    .map(|ctor| ctor.p_equation.type_data.size)
                    .max()
                    .unwrap();
                let mut graph = Graph::<usize, ()>::new();
                let nodes = (0..ctors.len())
                    .into_iter()
                    .map(|i| graph.add_node(i))
                    .collect::<Vec<NodeIndex>>();
                let edges = nodes
                    .iter()
                    .cloned()
                    .cartesian_product(nodes.iter().cloned())
                    .filter_map(|(n1, n2)| {
                        let bv = z3::ast::BV::new_const(
                            &z3ctx,
                            "input",
                            (max_size * 8).try_into().unwrap(),
                        );
                        let c1 = &ctors[graph[n1]];
                        let c2 = &ctors[graph[n2]];
                        let p1 = ret.peq_to_z3(&z3ctx, &bv, &c1.p_equation);
                        let p2 = ret.peq_to_z3(&z3ctx, &bv, &c2.p_equation);
                        let subset = z3::ast::forall_const(&z3ctx, &[&bv], &[], &p1.implies(&p2));
                        let superset = z3::ast::forall_const(&z3ctx, &[&bv], &[], &p2.implies(&p1));
                        let proper_subset = z3::ast::Bool::and(&z3ctx, &[&subset, &superset.not()]);
                        let solver = z3::Solver::new(&z3ctx);
                        solver.assert(&proper_subset);
                        let result = solver.check();
                        println!("{proper_subset} {result:?} {n1:?} {n2:?}");
                        match result {
                            z3::SatResult::Unsat => None,
                            z3::SatResult::Unknown => panic!(),
                            z3::SatResult::Sat => Some((n1, n2)),
                        }
                    })
                    .collect::<Vec<(NodeIndex, NodeIndex)>>();
                dbg!(edges.len());
                graph.extend_with_edges(edges);
                let sorted_nodes = match petgraph::algo::toposort(&graph, None) {
                    Ok(sorted) => sorted,
                    Err(_) => panic!("cycle at {name}, {graph:#?}"),
                };
                assert_eq!(sorted_nodes.len(), ctors.len());
                let ctors = sorted_nodes
                    .iter()
                    .cloned()
                    .map(|n| &ctors[graph[n]])
                    .cloned()
                    .collect::<Vec<Constructor<OffAndSize>>>();
                table.insert(name.to_string(), ctors.clone());
            }
        }

        // Put sorted constructors in context
        for (name, ctors) in table {
            ret.symbols.insert(name, SymbolData::Subtable(ctors));
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

    fn peq_to_z3<'z>(
        &self,
        z3ctx: &'z z3::Context,
        bv: &z3::ast::BV<'z>,
        peq: &PatternEquation<OffAndSize>,
    ) -> z3::ast::Bool<'z> {
        use z3::ast::Ast;
        use ConstraintCompareOp::*;
        use PatternEquationInner::*;
        match &peq.inner {
            EllEq(e) => match &e.ell_rt.atomic {
                Atomic::Constraint(c) => match c {
                    Constraint::Compare(ConstraintCompare { symbol, op, expr }) => {
                        let symbol = if let SymbolData::Value { field, .. } =
                            self.symbols.get(symbol.as_str()).unwrap()
                        {
                            let FieldDef { high, low, .. } = field.field_info;
                            bv.extract(high.into(), low.into())
                        } else {
                            panic!()
                        };
                        let expr = self.pexpr_to_z3(z3ctx, symbol.get_size(), &expr);
                        match op {
                            Equal => symbol._eq(&expr),
                            NotEqual => !symbol._eq(&expr),
                            Less => symbol.bvult(&expr),
                            LessEqual => symbol.bvule(&expr),
                            Greater => symbol.bvugt(&expr),
                            GreaterEqual => symbol.bvuge(&expr),
                        }
                    }
                    Constraint::Symbol(_) => z3::ast::Bool::from_bool(z3ctx, true),
                },
                Atomic::Parenthesized(peq) => self.peq_to_z3(z3ctx, bv, &peq),
            },
            Bin(bin) => {
                use PatternEquationBinOp::*;
                let PatternEquationBin { op, l, r } = &**bin;
                let l = self.peq_to_z3(z3ctx, bv, l);
                let r = self.peq_to_z3(z3ctx, bv, r);
                match op {
                    And | Cat => l & r,
                    Or => l | r,
                }
            }
        }
    }

    fn pexpr_to_z3<'z>(
        &self,
        z3ctx: &'z z3::Context,
        bit_width: u32,
        e: &PExpression,
    ) -> z3::ast::BV<'z> {
        use PExpression::*;
        match e {
            ConstantValue(v) => z3::ast::BV::from_u64(z3ctx, *v, bit_width),
            Symbol(_) => todo!(),
            Bin(bin) => {
                use PExpressionBinOp::*;
                let PExpressionBin { op, l, r } = &**bin;
                let l = self.pexpr_to_z3(z3ctx, bit_width, l);
                let r = self.pexpr_to_z3(z3ctx, bit_width, r);
                match op {
                    Add => l + r,
                    Sub => l - r,
                    Mult => l * r,
                    LeftShift => l.bvshl(&r),
                    RightShift => l.bvlshr(&r),
                    And => l & r,
                    Or => l | r,
                    Xor => l ^ r,
                    Div => l.bvudiv(&r),
                }
            }
            Unary(unary) => {
                use PExpressionUnaryOp::*;
                let PExpressionUnary { op, operand } = &**unary;
                let operand = self.pexpr_to_z3(z3ctx, bit_width, operand);
                match op {
                    Minus => -operand,
                    Not => !operand,
                }
            }
        }
    }

    fn compute_token_symbol(&self, symbol: &str, input: &[u8]) -> u8 {
        if let Some(SymbolData::Value {
            field:
                TokenField {
                    field_info: FieldDef { low, high, .. },
                    ..
                },
            ..
        }) = self.symbols.get(symbol)
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
            Some(SymbolData::Value {
                field:
                    TokenField {
                        field_info: FieldDef { low, high, .. },
                        ..
                    },
                ..
            }) => {
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
