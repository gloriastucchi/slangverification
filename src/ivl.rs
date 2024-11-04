use crate::slang::ast::{Expr, Name, Type, Cases};
use crate::slang::Span;
use std::fmt;

#[derive(Clone, Debug)]
pub struct IVLCmd {
    pub span: Span,
    pub kind: IVLCmdKind,
}

#[derive(Clone, Debug)]
pub enum IVLCmdKind {
    Assignment { name: Name, expr: Expr },
    Assume { condition: Expr },
    Assert { condition: Expr, message: String },
    Seq(Box<IVLCmd>, Box<IVLCmd>),
    NonDet(Box<IVLCmd>, Box<IVLCmd>),
    Loop { invariants: Vec<Expr>, variant: Option<Expr>, body: Box<IVLCmd> },
    Conditional { condition: Expr, body: Box<IVLCmd> },
    Return { expr: Option<Expr> },
    Havoc { name: Name, ty: Type },
}

impl IVLCmd {
    pub fn new(kind: IVLCmdKind, span: Span) -> Self {
        IVLCmd { span, kind }
    }
}

impl fmt::Display for IVLCmd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            IVLCmdKind::Assignment { name, expr } => write!(f, "{} := {}", name, expr),
            IVLCmdKind::Havoc { name, .. } => write!(f, "havoc {}", name),
            IVLCmdKind::Assume { condition } => write!(f, "assume {}", condition),
            IVLCmdKind::Assert { condition, .. } => write!(f, "assert {}", condition),
            IVLCmdKind::Seq(c1, c2) => write!(f, "{} ; {}", c1, c2),
            IVLCmdKind::NonDet(c1, c2) => write!(f, "{{ {} }} [] {{ {} }}", c1, c2),
            IVLCmdKind::Loop { .. } => write!(f, "loop {{ ... }}"),
            IVLCmdKind::Conditional { condition, body } => write!(f, "if {} then {}", condition, body),
            IVLCmdKind::Return { expr } => {
                if let Some(expr) = expr {
                    write!(f, "return {}", expr)
                } else {
                    write!(f, "return")
                }
            }
        }
    }
}
