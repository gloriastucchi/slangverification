mod ivl;
mod ivl_ext;

use ivl::{IVLCmd, IVLCmdKind};
use crate::slang::ast::{Cmd, CmdKind, Expr, PrefixOp, ExprKind, Type, Range};
use crate::slang::Span;
use slang_ui::prelude::*;

pub struct App;

impl slang_ui::Hook for App {
    fn analyze(&self, cx: &mut slang_ui::Context, file: &slang::SourceFile) -> Result<()> {
        let mut solver = cx.solver()?;

        for m in file.methods() {
            let pres = m.requires();
            let pre = pres.cloned().reduce(|a, b| a & b).unwrap_or(Expr::bool(true));
            let spre = pre.smt()?;
            solver.assert(spre.as_bool()?)?;

            let cmd = &m.body.clone().unwrap().cmd;
            let ivl = cmd_to_ivlcmd(cmd)?;

            let posts = m.ensures();
            let mut post: Vec<(Expr, String)> = posts
                .cloned()
                .map(|expr| (
                    expr.clone(),
                    format!("Error ensuring the property {}", expr),
                ))
                .collect();
            
            if post.is_empty() {
                post.push((Expr::bool(true), "Default post condition".to_string()));
            }
            
            let post_correct_spans: Vec<(Expr, String)> = post
                .into_iter()
                .map(|(expr, msg)| (expr, msg))
                .collect();
            
            let oblig = wp(&ivl, post_correct_spans)?;

            let mut stop = false;
            for i in 0..oblig.len() {
                if stop { break; }
                solver.scope(|solver| {
                    let mut or_oblig = Expr::bool(false);
                    let mut last_obl = Expr::bool(false); 
                    let mut last_msg = String::new();
                    for (obl, msg) in oblig[0..=i].iter() {
                        or_oblig = or_oblig.or(&obl.prefix(PrefixOp::Not));
                        last_obl.span = obl.span.clone();
                        last_msg = msg.clone();
                    }
                    if let Ok(soblig) = or_oblig.smt() { 
                        solver.assert(soblig.as_bool()?)?;
                        match solver.check_sat()? {
                            smtlib::SatResult::Sat => {
                                cx.error(last_obl.span, format!("Verification failed: {last_msg}."));
                                stop = true;
                            }
                            smtlib::SatResult::Unknown => {
                                cx.warning(last_obl.span, format!("Verification uncertain for: {last_msg}. Unknown SAT result."));
                            }
                            smtlib::SatResult::Unsat => (),
                        }
                    } else {
                        println!("Failed with {:#?}", or_oblig);
                    }
                    Ok(())
                })?;
            }
        }
        Ok(())
    }
}

// Convert a command to IVL command representation with invariant handling for loops
fn cmd_to_ivlcmd(cmd: &Cmd) -> Result<IVLCmd> {
    match &cmd.kind {
        CmdKind::Assert { condition, .. } => Ok(IVLCmd::assert(condition, "Assert might fail!")),
        CmdKind::Seq(cmd1, cmd2) => Ok(IVLCmd::seq(&cmd_to_ivlcmd(cmd1)?, &cmd_to_ivlcmd(cmd2)?)),
        CmdKind::VarDefinition { name, ty, expr } => { 
            if let Some(expr) = expr {
                Ok(IVLCmd::assign(name, expr))
            } else {
                Ok(IVLCmd::nop())
            }
        },
        CmdKind::Assignment { name, expr } => Ok(IVLCmd::assign(name, expr)),
        CmdKind::Assume { condition } => Ok(IVLCmd::assume(condition)),

        // Handle bounded for-loops by unrolling them
        CmdKind::For { name, range, body, invariants: _, variant: _ } => {
            if let Some((start, end)) = extract_constant_range(range) {
                let mut ivl_cmds = Vec::new();

                for i in start..end {
                    let index_expr = Expr::new_typed(ExprKind::Num(i), Type::Int);
                    ivl_cmds.push(IVLCmd::assign(name, &index_expr));
                    
                    ivl_cmds.push(cmd_to_ivlcmd(&body.cmd)?);
                }

                Ok(ivl_cmds.into_iter().reduce(|acc, cmd| IVLCmd::seq(&acc, &cmd)).unwrap_or(IVLCmd::nop()))
            } else {
                Err(eyre::eyre!("Expected a constant range in for-loop"))
            }
        },
        
        CmdKind::Loop { invariants, variant: _, body } => {
            let cases_ivl: Vec<IVLCmd> = body.cases.iter()
                .map(|case| {
                    let case_cmd = Cmd::seq(&Cmd::assume(&case.condition), &case.cmd);
                    cmd_to_ivlcmd(&case_cmd)
                })
                .collect::<Result<Vec<_>>>()?;
            let body_ivl = IVLCmd::nondets(&cases_ivl);

            Ok(IVLCmd::_loop(invariants, &None, &body_ivl))
        },

        CmdKind::Return { expr } => {
            let mut ivl_cmd = IVLCmd::_return(expr);
            ivl_cmd.span = cmd.span;
            Ok(ivl_cmd)
        },
        _ => todo!(" Not supported (yet)."),
    }
}

// Helper function to extract constant range values
fn extract_constant_range(range: &Range) -> Option<(i64, i64)> {
    if let Range::FromTo(from, to) = range {
        if let (ExprKind::Num(start_val), ExprKind::Num(end_val)) = (&from.kind, &to.kind) {
            // Dereference `start_val` and `end_val` to get `i64`
            return Some((*start_val, *end_val));
        }
    }
    None
}



// Weakest precondition function (WP) with adjusted invariant checks
fn wp<'a>(ivl: &IVLCmd, mut post: Vec<(Expr, String)>) -> Result<Vec<(Expr, String)>> {
    match &ivl.kind {
        IVLCmdKind::Assert { condition, message } => {
            println!("Evaluating assertion with condition: {:?}", condition);
            post.push((condition.clone(), message.clone()));
            Ok(post)
        }
        
        IVLCmdKind::Assume { condition } => {
            let mut new_post = Vec::new();
            for (post_expr, msg) in post.iter() {
                new_post.push((condition.imp(post_expr), msg.clone()));
            }
            Ok(new_post)
        }
        IVLCmdKind::Seq(cmd1, cmd2) => {
            let post2 = wp(cmd2, post)?;
            let post1 = wp(cmd1, post2)?;
            Ok(post1)
        }
        IVLCmdKind::Assignment { name, expr } => {
            let mut new_post = Vec::new();
            for (post_expr, msg) in post.iter() {
                let new_expr = fix_span(expr.clone(), post_expr.span.clone());
                new_post.push((post_expr.clone().subst(|x| x.is_ident(&name.ident), &new_expr), msg.to_string()));
            }
            Ok(new_post)
        }
        
        IVLCmdKind::Loop { invariants, variant: _, body } => {
            let mut loop_obligations = Vec::new();

            // Check that invariants hold before entering the loop
            for invariant in invariants {
                loop_obligations.push((
                    invariant.clone(),
                    "Loop invariant does not hold at start".to_string(),
                ));
            }

            let body_obligations = wp(body, invariants.clone()
                .into_iter()
                .map(|inv| (inv.clone(), "Loop invariant might not hold after iteration".to_string()))
                .collect())?;

            loop_obligations.extend(body_obligations);

            // Ensure that invariants hold after loop terminates
            for invariant in invariants {
                loop_obligations.push((
                    invariant.clone(),
                    "Loop invariant does not hold after loop".to_string(),
                ));
            }

            Ok(loop_obligations)
        }
        
        IVLCmdKind::NonDet(cmd1, cmd2) => {
            let mut post1 = wp(cmd1, post.clone())?;
            let post2 = wp(cmd2, post)?;
            post1.extend(post2);
            Ok(post1)
        }
        
        IVLCmdKind::Return { expr } => {
            if let Some(acc_expr) = expr {
                let mut new_post = Vec::new();
                for (post_expr, msg) in post.iter() {
                    let new_acc_expr = fix_span(acc_expr.clone(), post_expr.span.clone());
                    new_post.push((post_expr.clone().subst(|x| matches!(x.kind, ExprKind::Result), &new_acc_expr), msg.to_string()));
                }
                Ok(new_post)
            } else {
                Ok(post)
            }
        }
        _ => todo!("Not supported (yet)."),
    }
}

// Adjust span of expressions for error reporting
fn fix_span(mut expr_in: Expr, span: Span) -> Expr {
    match &expr_in.kind {
        ExprKind::Infix(e1, op, e2) => Expr::op(&fix_span(*e1.clone(), span), *op, &fix_span(*e2.clone(), span)),
        _ => { expr_in.span = span.clone(); expr_in }
    }
}
