//! Printer for ISLE language.

#![allow(missing_docs)]

use crate::ast::*;
use crate::error::Errors;
use pretty::{Doc, RcDoc};
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
            Def::Type(t) => RcDoc::text("(")
                .append(
                    RcDoc::intersperse(
                        vec![RcDoc::text("type"), t.name.to_doc(), t.ty.to_doc()],
                        Doc::line(),
                    )
                    .nest(4)
                    .group(),
                )
                .append(RcDoc::text(")")),
            _ => RcDoc::as_string("def"),
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
        RcDoc::text("type_value")
    }
}
