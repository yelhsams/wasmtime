//! Printer for ISLE language.

#![allow(missing_docs)]

use crate::ast::*;
use crate::error::Errors;
use pretty::{Doc, Pretty, RcAllocator, RcDoc};
use std::io::Write;

pub fn print<W>(defs: &Defs, width: usize, out: &mut W) -> Result<(), Errors>
where
    W: ?Sized + Write,
{
    defs.to_doc()
        .render(width, out)
        .map_err(|e| Errors::from_io(e, "failed to print isle"))
}

impl Defs {
    fn to_doc(&self) -> RcDoc<()> {
        let sep = RcDoc::hardline().append(Doc::hardline());
        RcDoc::intersperse(self.defs.iter().map(|d| d.to_doc()), sep).append(Doc::hardline())
    }
}

impl Def {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Def::Type(ref t) => sexp(vec![RcDoc::text("type"), t.name.to_doc(), t.ty.to_doc()]),
            Def::Rule(ref r) => {
                let mut parts = Vec::new();
                parts.push(RcDoc::text("rule"));
                if let Some(name) = &r.name {
                    parts.push(name.to_doc());
                }
                if let Some(prio) = &r.prio {
                    parts.push(RcDoc::as_string(prio));
                }
                parts.push(r.pattern.to_doc());
                // TODO(mbm): if-lets
                parts.push(r.expr.to_doc());
                sexp(parts)
            }
            Def::Extractor(ref e) => sexp(vec![
                RcDoc::text("extractor"),
                sexp(
                    Vec::from([e.term.to_doc()])
                        .into_iter()
                        .chain(e.args.iter().map(|v| v.to_doc())),
                ),
                e.template.to_doc(),
            ]),
            Def::Decl(ref d) => {
                let mut parts = Vec::new();
                parts.push(RcDoc::text("decl"));
                if d.pure {
                    parts.push(RcDoc::text("pure"));
                }
                if d.multi {
                    parts.push(RcDoc::text("multi"));
                }
                if d.partial {
                    parts.push(RcDoc::text("partial"));
                }
                parts.push(d.term.to_doc());
                parts.push(sexp(d.arg_tys.iter().map(|ty| ty.to_doc())));
                parts.push(d.ret_ty.to_doc());
                sexp(parts)
            }
            // TODO: Spec(Spec),
            // TODO: Model(Model),
            // TODO: Form(Form),
            // TODO: Instantiation(Instantiation),
            // TODO: Extern(Extern),
            Def::Extern(ref e) => e.to_doc(),
            Def::Converter(ref c) => sexp(vec![
                RcDoc::text("convert"),
                c.inner_ty.to_doc(),
                c.outer_ty.to_doc(),
                c.term.to_doc(),
            ]),
            _ => todo!("def: {:?}", self),
        }
    }
}

impl Ident {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text(self.0.clone())
    }
}

impl TypeValue {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            TypeValue::Primitive(ref name, _) => {
                sexp(vec![RcDoc::text("primitive"), name.to_doc()])
            }
            TypeValue::Enum(ref variants, _) => sexp(
                // TODO(mbm): convenience for sexp with a fixed first element
                Vec::from([RcDoc::text("enum")])
                    .into_iter()
                    .chain(variants.iter().map(|v| v.to_doc())),
            ),
        }
    }
}

impl Variant {
    fn to_doc(&self) -> RcDoc<()> {
        sexp(
            // TODO(mbm): convenience for sexp with a fixed first element
            Vec::from([self.name.to_doc()])
                .into_iter()
                .chain(self.fields.iter().map(|f| f.to_doc())),
        )
    }
}

impl Field {
    fn to_doc(&self) -> RcDoc<()> {
        sexp(vec![self.name.to_doc(), self.ty.to_doc()])
    }
}

impl Pattern {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Pattern::Var { var, .. } => var.to_doc(),
            Pattern::BindPattern { var, subpat, .. } => RcDoc::intersperse(
                vec![var.to_doc(), RcDoc::text("@"), subpat.to_doc()],
                Doc::space(),
            ),
            Pattern::ConstInt { val, .. } => RcDoc::as_string(val),
            Pattern::ConstPrim { val, .. } => val.to_doc(),
            Pattern::Wildcard { .. } => RcDoc::text("_"),
            Pattern::Term { sym, args, .. } => sexp(
                // TODO(mbm): convenience for sexp with a fixed first element
                Vec::from([sym.to_doc()])
                    .into_iter()
                    .chain(args.iter().map(|f| f.to_doc())),
            ),
            _ => todo!("pattern: {:?}", self),
        }
    }
}

impl Expr {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Expr::Term { sym, args, .. } => sexp(
                // TODO(mbm): convenience for sexp with a fixed first element
                Vec::from([sym.to_doc()])
                    .into_iter()
                    .chain(args.iter().map(|f| f.to_doc())),
            ),
            Expr::Var { name, .. } => name.to_doc(),
            Expr::ConstInt { val, .. } => RcDoc::as_string(val),
            Expr::ConstPrim { val, .. } => val.to_doc(),
            Expr::Let { defs, body, .. } => {
                let mut parts = Vec::new();
                parts.push(RcDoc::text("let"));
                parts.extend(defs.iter().map(|d| d.to_doc()));
                parts.push(body.to_doc());
                sexp(parts)
            }
        }
    }
}

impl LetDef {
    fn to_doc(&self) -> RcDoc<()> {
        sexp(vec![self.var.to_doc(), self.ty.to_doc(), self.val.to_doc()])
    }
}

impl Extern {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Extern::Extractor {
                term,
                func,
                pos: _,
                infallible,
            } => {
                let mut parts = vec![RcDoc::text("extern"), RcDoc::text("extractor")];
                if *infallible {
                    parts.push(RcDoc::text("infallible"));
                }
                parts.push(term.to_doc());
                parts.push(func.to_doc());
                sexp(parts)
            }
            Extern::Constructor { term, func, .. } => sexp(vec![
                RcDoc::text("extern"),
                RcDoc::text("constructor"),
                term.to_doc(),
                func.to_doc(),
            ]),
            _ => todo!("extern: {:?}", self),
        }
    }
}

fn sexp<'a, I, A>(docs: I) -> RcDoc<'a, A>
where
    I: IntoIterator,
    I::Item: Pretty<'a, RcAllocator, A>,
    A: Clone,
{
    RcDoc::text("(")
        .append(RcDoc::intersperse(docs, Doc::line()).nest(4).group())
        .append(RcDoc::text(")"))
}
