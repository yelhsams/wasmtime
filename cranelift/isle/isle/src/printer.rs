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
            _ => RcDoc::as_string("TODO"),
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
