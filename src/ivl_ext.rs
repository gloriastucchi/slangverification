use crate::slang::{
    ast::{Expr, Name, Type, Cases},
    Span,
};
use crate::ivl::{IVLCmd, IVLCmdKind};

impl IVLCmd {
    pub fn assign(name: &Name, expr: &Expr) -> IVLCmd {
        IVLCmd {
            span: expr.span,
            kind: IVLCmdKind::Assignment {
                name: name.clone(),
                expr: expr.clone(),
            },
        }
    }

    pub fn seq(&self, other: &IVLCmd) -> IVLCmd {
        IVLCmd {
            span: Span::union(self.span, other.span),
            kind: IVLCmdKind::Seq(Box::new(self.clone()), Box::new(other.clone())),
        }
    }

    pub fn _loop(invariants: &Vec<Expr>, variant: &Option<Expr>, body: &IVLCmd) -> IVLCmd {
        IVLCmd {
            span: Span::default(),
            kind: IVLCmdKind::Loop {
                invariants: invariants.clone(),
                variant: variant.clone(),
                body: Box::new(body.clone()),
            },
        }
    }

    pub fn nondet(&self, other: &IVLCmd) -> IVLCmd {
        IVLCmd {
            span: Span::default(),
            kind: IVLCmdKind::NonDet(Box::new(self.clone()), Box::new(other.clone())),
        }
    }

    pub fn nondets(cmds: &[IVLCmd]) -> IVLCmd {
        cmds.iter()
            .cloned()
            .reduce(|a, b| IVLCmd::nondet(&a, &b))
            .unwrap_or_else(IVLCmd::nop)
    }

    pub fn assume(condition: &Expr) -> IVLCmd {
        IVLCmd {
            span: condition.span,
            kind: IVLCmdKind::Assume {
                condition: condition.clone(),
            },
        }
    }

    pub fn assert(condition: &Expr, message: &str) -> IVLCmd {
        IVLCmd {
            span: condition.span,
            kind: IVLCmdKind::Assert {
                condition: condition.clone(),
                message: message.to_owned(),
            },
        }
    }

    pub fn havoc(name: &Name, ty: &Type) -> IVLCmd {
        IVLCmd {
            kind: IVLCmdKind::Havoc {
                name: name.clone(),
                ty: ty.clone(),
            },
            span: Span::default(),
        }
    }

    pub fn nop() -> IVLCmd {
        IVLCmd::assume(&Expr::bool(true))
    }

    pub fn unreachable() -> IVLCmd {
        IVLCmd::assume(&Expr::bool(false))
    }

    pub fn _return(expr: &Option<Expr>) -> IVLCmd {
        IVLCmd {
            span: Span::default(),
            kind: IVLCmdKind::Return {
                expr: expr.clone(),
            },
        }
    }
}
