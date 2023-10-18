use cranelift_isle::ast;
use std::collections::HashMap;

use cranelift_isle::ast::Defs;
use cranelift_isle::ast::Ident;
use cranelift_isle::ast::Model;
use cranelift_isle::ast::SpecExpr;
use cranelift_isle::ast::SpecOp;
use cranelift_isle::lexer::Pos;
use cranelift_isle::sema::TypeEnv;
use veri_ir::annotation_ir::Width;
use veri_ir::annotation_ir::{BoundVar, Const, Expr, TermAnnotation, TermSignature, Type};

static RESULT: &str = "result";

#[derive(Clone, Debug)]
pub struct ParsingEnv<'a> {
    pub typeenv: &'a TypeEnv,
    pub enums: HashMap<String, Expr>,
}

#[derive(Clone, Debug)]
pub struct AnnotationEnv {
    pub annotation_map: HashMap<String, TermAnnotation>,
}

impl AnnotationEnv {
    pub fn new(annotation_env: HashMap<String, TermAnnotation>) -> Self {
        AnnotationEnv {
            annotation_map: annotation_env,
        }
    }

    pub fn get_annotation_for_term(&self, term: &str) -> Option<TermAnnotation> {
        if self.annotation_map.contains_key(term) {
            return Some(self.annotation_map[term].clone());
        }
        None
    }
}

pub fn string_from_ident(_env: &ParsingEnv, id: &ast::Ident) -> String {
    format!("{}", &id.0)
}

pub fn spec_to_annotation_bound_var(i: &Ident, env: &ParsingEnv) -> BoundVar {
    BoundVar {
        name: string_from_ident(env, i),
        ty: None,
    }
}

fn spec_to_usize(s: &SpecExpr) -> Option<usize> {
    match s {
        SpecExpr::ConstInt { val, pos: _ } => Some(*val as usize),
        _ => None,
    }
}

fn spec_op_to_expr(s: &SpecOp, args: &Vec<SpecExpr>, pos: &Pos, env: &ParsingEnv) -> Expr {
    fn unop<F: Fn(Box<Expr>, u32) -> Expr>(
        u: F,
        args: &Vec<SpecExpr>,
        pos: &Pos,
        env: &ParsingEnv,
    ) -> Expr {
        assert_eq!(
            args.len(),
            1,
            "Unexpected number of args for unary operator {:?}",
            pos
        );
        return u(Box::new(spec_to_expr(&args[0], env)), 0);
    }
    fn binop<F: Fn(Box<Expr>, Box<Expr>, u32) -> Expr>(
        b: F,
        args: &Vec<SpecExpr>,
        _pos: &Pos,
        env: &ParsingEnv,
    ) -> Expr {
        assert_eq!(
            args.len(),
            2,
            "Unexpected number of args for binary operator {:?}",
            args
        );
        b(
            Box::new(spec_to_expr(&args[0], env)),
            Box::new(spec_to_expr(&args[1], env)),
            0,
        )
    }

    fn variadic_binop<F: Fn(Box<Expr>, Box<Expr>, u32) -> Expr>(
        b: F,
        args: &Vec<SpecExpr>,
        pos: &Pos,
        env: &ParsingEnv,
    ) -> Expr {
        assert!(
            args.len() >= 1,
            "Unexpected number of args for variadic binary operator {:?}",
            pos
        );
        let mut expr_args: Vec<Expr> = args.iter().map(|a| spec_to_expr(&a, env)).collect();
        let last = expr_args.remove(expr_args.len() - 1);

        // Reverse to keep the order of the original list
        expr_args
            .iter()
            .rev()
            .fold(last, |acc, a| b(Box::new(a.clone()), Box::new(acc), 0))
    }

    match s {
        // Unary
        SpecOp::Not => unop(|x, i| Expr::Not(x, i), args, pos, env),
        SpecOp::BVNot => unop(|x, i| Expr::BVNot(x, i), args, pos, env),
        SpecOp::BVNeg => unop(|x, i| Expr::BVNeg(x, i), args, pos, env),
        SpecOp::Rev => unop(|x, i| Expr::Rev(x, i), args, pos, env),
        SpecOp::Clz => unop(|x, i| Expr::CLZ(x, i), args, pos, env),
        SpecOp::Cls => unop(|x, i| Expr::CLS(x, i), args, pos, env),
        SpecOp::Popcnt => unop(|x, i| Expr::BVPopcnt(x, i), args, pos, env),
        SpecOp::BV2Int => unop(|x, i| Expr::BVToInt(x, i), args, pos, env),

        // Variadic binops
        SpecOp::And => variadic_binop(|x, y, i| Expr::And(x, y, i), args, pos, env),
        SpecOp::Or => variadic_binop(|x, y, i| Expr::Or(x, y, i), args, pos, env),

        // Binary
        SpecOp::Eq => binop(|x, y, i| Expr::Eq(x, y, i), args, pos, env),
        SpecOp::Lt => binop(|x, y, i| Expr::Lt(x, y, i), args, pos, env),
        SpecOp::Lte => binop(|x, y, i| Expr::Lte(x, y, i), args, pos, env),
        SpecOp::Gt => binop(|x, y, i| Expr::Lt(y, x, i), args, pos, env),
        SpecOp::Gte => binop(|x, y, i| Expr::Lte(y, x, i), args, pos, env),
        SpecOp::BVAnd => binop(|x, y, i| Expr::BVAnd(x, y, i), args, pos, env),
        SpecOp::BVOr => binop(|x, y, i| Expr::BVOr(x, y, i), args, pos, env),
        SpecOp::BVXor => binop(|x, y, i| Expr::BVXor(x, y, i), args, pos, env),
        SpecOp::BVAdd => binop(|x, y, i| Expr::BVAdd(x, y, i), args, pos, env),
        SpecOp::BVSub => binop(|x, y, i| Expr::BVSub(x, y, i), args, pos, env),
        SpecOp::BVMul => binop(|x, y, i| Expr::BVMul(x, y, i), args, pos, env),
        SpecOp::BVUdiv => binop(|x, y, i| Expr::BVUDiv(x, y, i), args, pos, env),
        SpecOp::BVUrem => binop(|x, y, i| Expr::BVUrem(x, y, i), args, pos, env),
        SpecOp::BVSdiv => binop(|x, y, i| Expr::BVSDiv(x, y, i), args, pos, env),
        SpecOp::BVSrem => binop(|x, y, i| Expr::BVSrem(x, y, i), args, pos, env),
        SpecOp::BVShl => binop(|x, y, i| Expr::BVShl(x, y, i), args, pos, env),
        SpecOp::BVLshr => binop(|x, y, i| Expr::BVShr(x, y, i), args, pos, env),
        SpecOp::BVAshr => binop(|x, y, i| Expr::BVAShr(x, y, i), args, pos, env),
        SpecOp::BVUle => binop(|x, y, i| Expr::BVUlte(x, y, i), args, pos, env),
        SpecOp::BVUlt => binop(|x, y, i| Expr::BVUlt(x, y, i), args, pos, env),
        SpecOp::BVUgt => binop(|x, y, i| Expr::BVUgt(x, y, i), args, pos, env),
        SpecOp::BVUge => binop(|x, y, i| Expr::BVUgte(x, y, i), args, pos, env),
        SpecOp::BVSlt => binop(|x, y, i| Expr::BVSlt(x, y, i), args, pos, env),
        SpecOp::BVSle => binop(|x, y, i| Expr::BVSlte(x, y, i), args, pos, env),
        SpecOp::BVSgt => binop(|x, y, i| Expr::BVSgt(x, y, i), args, pos, env),
        SpecOp::BVSge => binop(|x, y, i| Expr::BVSgte(x, y, i), args, pos, env),
        SpecOp::Rotr => binop(|x, y, i| Expr::BVRotr(x, y, i), args, pos, env),
        SpecOp::Rotl => binop(|x, y, i| Expr::BVRotl(x, y, i), args, pos, env),
        SpecOp::ZeroExt => match spec_to_usize(&args[0]) {
            Some(i) => Expr::BVZeroExtTo(
                Box::new(Width::Const(i)),
                Box::new(spec_to_expr(&args[1], env)),
                0,
            ),
            None => binop(|x, y, i| Expr::BVZeroExtToVarWidth(x, y, i), args, pos, env),
        },
        SpecOp::SignExt => match spec_to_usize(&args[0]) {
            Some(i) => Expr::BVSignExtTo(
                Box::new(Width::Const(i)),
                Box::new(spec_to_expr(&args[1], env)),
                0,
            ),
            None => binop(|x, y, i| Expr::BVSignExtToVarWidth(x, y, i), args, pos, env),
        },
        SpecOp::ConvTo => binop(|x, y, i| Expr::BVConvToVarWidth(x, y, i), args, pos, env),

        // AVH TODO
        SpecOp::Concat => {
            let cases: Vec<Expr> = args.iter().map(|a| spec_to_expr(a, env)).collect();
            Expr::BVConcat(cases, 0)
        }
        SpecOp::Extract => {
            assert_eq!(
                args.len(),
                3,
                "Unexpected number of args for extract operator {:?}",
                pos
            );
            Expr::BVExtract(
                spec_to_usize(&args[0]).unwrap(),
                spec_to_usize(&args[1]).unwrap(),
                Box::new(spec_to_expr(&args[2], env)),
                0,
            )
        }
        SpecOp::Int2BV => {
            assert_eq!(
                args.len(),
                2,
                "Unexpected number of args for Int2BV operator {:?}",
                pos
            );
            Expr::BVIntToBv(
                spec_to_usize(&args[0]).unwrap(),
                Box::new(spec_to_expr(&args[1], env)),
                0,
            )
        }
        SpecOp::Subs => {
            assert_eq!(
                args.len(),
                3,
                "Unexpected number of args for subs operator {:?}",
                pos
            );
            Expr::BVSubs(
                Box::new(spec_to_expr(&args[0], env)),
                Box::new(spec_to_expr(&args[1], env)),
                Box::new(spec_to_expr(&args[2], env)),
                0,
            )
        }
        SpecOp::WidthOf => unop(|x, i| Expr::WidthOf(x, i), args, pos, env),
        SpecOp::If => {
            assert_eq!(
                args.len(),
                3,
                "Unexpected number of args for extract operator {:?}",
                pos
            );
            Expr::Conditional(
                Box::new(spec_to_expr(&args[0], env)),
                Box::new(spec_to_expr(&args[1], env)),
                Box::new(spec_to_expr(&args[2], env)),
                0,
            )
        }
        SpecOp::Switch => {
            assert!(
                args.len() > 1,
                "Unexpected number of args for switch operator {:?}",
                pos
            );
            let swith_on = spec_to_expr(&args[0], env);
            let arms: Vec<(Expr, Expr)> = args[1..]
                .iter()
                .map(|a| match a {
                    SpecExpr::Pair { l, r } => {
                        let l_expr = spec_to_expr(l, env);
                        let r_expr = spec_to_expr(r, env);
                        (l_expr, r_expr)
                    }
                    _ => unreachable!(),
                })
                .collect();
            Expr::Switch(Box::new(swith_on), arms, 0)
        }
    }
}

fn spec_to_expr(s: &SpecExpr, env: &ParsingEnv) -> Expr {
    match s {
        SpecExpr::ConstInt { val, pos: _ } => Expr::Const(
            Const {
                ty: Type::Int,
                value: *val,
                width: 0,
            },
            0,
        ),
        SpecExpr::ConstBitVec { val, width, pos: _ } => Expr::Const(
            Const {
                ty: Type::BitVectorWithWidth(*width as usize),
                value: *val,
                width: (*width as usize),
            },
            0,
        ),
        SpecExpr::ConstBool { val, pos: _ } => Expr::Const(
            Const {
                ty: Type::Bool,
                value: *val as i128,
                width: 0,
            },
            0,
        ),
        SpecExpr::Var { var, pos: _ } => Expr::Var(string_from_ident(env, var), 0),
        SpecExpr::Op { op, args, pos } => spec_op_to_expr(op, args, pos, env),
        SpecExpr::Pair { l, r } => {
            unreachable!(
                "pairs currently only parsed as part of Switch statements, {:?} {:?}",
                l, r
            )
        }
        SpecExpr::Enum { name } => {
            let n = string_from_ident(&env, name);
            if let Some(e) = env.enums.get(&n) {
                e.clone()
            } else {
                panic!("Can't find model for enum {}", n);
            }
        }
    }
}

pub fn parse_annotations(defs: &Defs, typeenv: &TypeEnv) -> AnnotationEnv {
    let mut annotation_map = HashMap::new();

    let mut env = ParsingEnv {
        typeenv,
        enums: HashMap::new(),
    };

    // Traverse models to process spec annotations for enums
    for def in &defs.defs {
        match def {
            &ast::Def::Model(Model { ref name, ref val }) => {
                match val {
                    ast::ModelValue::TypeValue(_) => {
                        // AVH todo, skipping for now
                        ()
                    }
                    ast::ModelValue::EnumValues(vals) => {
                        for (v, e) in vals {
                            let name = string_from_ident(&env, name);
                            let v = string_from_ident(&env, v);
                            let enum_name = format!("{}.{}", name, v);
                            let val = spec_to_expr(e, &env);
                            let ty = match val {
                                Expr::Const(Const { ref ty, .. }, _) => ty,
                                _ => unreachable!(),
                            };
                            env.enums.insert(enum_name.clone(), val.clone());
                            let result = BoundVar {
                                name: RESULT.to_string(),
                                ty: Some(ty.clone()),
                            };
                            let sig = TermSignature {
                                args: vec![],
                                ret: result,
                            };
                            let annotation = TermAnnotation {
                                sig,
                                assumptions: vec![Box::new(Expr::Eq(
                                    Box::new(Expr::Var(RESULT.to_string(), 0)),
                                    Box::new(val),
                                    0,
                                ))],
                                assertions: vec![],
                            };
                            annotation_map.insert(enum_name, annotation);
                        }
                    }
                }
            }
            _ => (),
        }
    }

    // Traverse defs to process spec annotations
    for def in &defs.defs {
        match def {
            &ast::Def::Spec(ref spec) => {
                let termname = string_from_ident(&env, &spec.term);
                // dbg!(&termname);
                let sig = TermSignature {
                    args: spec
                        .args
                        .iter()
                        .map(|a| spec_to_annotation_bound_var(a, &env))
                        .collect(),
                    ret: BoundVar {
                        name: RESULT.to_string(),
                        ty: None,
                    },
                };

                let mut assumptions = vec![];
                let mut assertions = vec![];
                for a in &spec.provides {
                    assumptions.push(Box::new(spec_to_expr(a, &env)));
                }

                for a in &spec.requires {
                    assertions.push(Box::new(spec_to_expr(a, &env)));
                }

                let annotation = TermAnnotation {
                    sig,
                    assumptions,
                    assertions,
                };
                annotation_map.insert(termname, annotation);
            }
            _ => {}
        }
    }
    AnnotationEnv { annotation_map }
}
