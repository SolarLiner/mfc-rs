use pest::error::Error;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use pest::Parser;

use crate::ast;
use crate::ast::{CAst, EAst, RAst, SAst};
use crate::env::FnSignature;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct GENParser;

lazy_static! {
    static ref PC_EXPR: PrecClimber<Rule> = {
        use Assoc::*;
        use Rule::*;
        PrecClimber::new(vec![
            Operator::new(badd, Left) | Operator::new(bsub, Left),
            Operator::new(bmul, Left),
        ])
    };
    static ref PC_COND: PrecClimber<Rule> = {
        use Assoc::*;
        use Rule::*;
        PrecClimber::new(vec![Operator::new(cand, Left), Operator::new(cor, Left)])
    };
}

pub fn parse(input: &str) -> anyhow::Result<SAst> {
    parse_raw(input)?
        .next()
        .ok_or(anyhow::Error::msg(format!("Expected statement")))
        .map(into_ast_stmt)
}

pub fn parse_raw(input: &str) -> Result<Pairs<Rule>, Error<Rule>> {
    GENParser::parse(Rule::root, input)
}

fn into_ast_stmt(pair: Pair<Rule>) -> SAst {
    match pair.as_rule() {
        Rule::fun_decl => {
            let mut inner = pair.into_inner();
            let ident = inner.next().unwrap().as_str().to_owned();
            let mut args: Vec<_> = inner.collect();
            let block = args.pop().unwrap();
            let returns = block
                .clone()
                .into_inner()
                .filter(|p| p.as_rule() == Rule::return_stmt)
                .count();
            SAst::DeclareFun(
                FnSignature(ident, args.len(), returns.max(0).min(1)),
                block.into_inner().map(into_ast_stmt).collect(),
            )
        }
        Rule::forced_block => SAst::Block(pair.into_inner().map(into_ast_stmt).collect()),
        Rule::block => SAst::Block(pair.into_inner().map(into_ast_stmt).collect()),
        Rule::stmt => pair.into_inner().map(into_ast_stmt).next().unwrap(),
        Rule::extern_fun_decl => {
            let mut inner = pair.into_inner();
            let ident = inner.next().unwrap().as_str().to_owned();
            let args: Vec<_> = inner.collect();
            SAst::DeclareExternFun(FnSignature(ident, args.len(), 0))
        }
        Rule::decls => SAst::Block(pair.into_inner().map(into_ast_stmt).collect()),
        Rule::decl_stmt => {
            let ident = pair
                .into_inner()
                .filter(|p| p.as_rule() == Rule::ident)
                .next()
                .unwrap();
            SAst::Declare(ident.as_str().to_owned())
        }
        Rule::assign_stmt => {
            let mut inner = pair.into_inner();
            let ident = inner.next().unwrap().as_str();
            let expr = into_ast_expr(Pairs::single(inner.next().unwrap()));
            //let expr = into_ast_expr(inner.filter(|p| p.as_rule() == Rule::expr
            SAst::Set(RAst::Id(ident.to_owned()), expr)
        }
        Rule::fun_call => {
            let mut inner = pair.into_inner();
            let ident = inner.next().unwrap().as_str().to_owned();
            let args = inner.map(Pairs::single).map(into_ast_expr).collect();
            SAst::Call(RAst::Id(ident), args)
        }
        Rule::return_stmt => SAst::Ret(into_ast_expr(pair.into_inner())),
        Rule::if_stmt => {
            let mut inner = pair.into_inner();
            let cond = into_ast_cond(Pairs::single(inner.next().unwrap()));
            let block = into_ast_stmt(inner.next().unwrap());
            let belse = if let Some(p) = inner.next() {
                into_ast_stmt(p)
            } else {
                SAst::Block(vec![])
            };
            SAst::If(cond, Box::from(block), Box::from(belse))
        }
        Rule::while_stmt => {
            let mut inner = pair.into_inner();
            let cond = inner.next().map(Pairs::single).map(into_ast_cond).unwrap();
            let block = inner.next().map(into_ast_stmt).unwrap();
            SAst::While(cond, Box::from(block))
        }
        _ => unimplemented!(),
    }
}

fn into_ast_expr(pairs: Pairs<Rule>) -> EAst {
    use EAst::*;
    PC_EXPR.climb(
        pairs,
        |pair: Pair<Rule>| match dbg!(&pair).as_rule() {
            Rule::number => Cst(pair.as_str().trim().parse().unwrap()),
            Rule::expr => into_ast_expr(pair.into_inner()),
            Rule::ident => {
                let ident = pair.as_str().to_owned();
                Ref(RAst::Id(ident))
            }
            Rule::fun_call => {
                let mut inner = pair.into_inner();
                let ident = inner.next().unwrap().as_str().to_owned();
                let args = inner.map(Pairs::single).map(into_ast_expr).collect();
                ECall(RAst::Id(ident), args)
            }
            _ => unreachable!(),
        },
        |lhs: EAst, op: Pair<Rule>, rhs: EAst| match op.as_rule() {
            Rule::badd => Binop(ast::Binop::Add, Box::from(lhs), Box::from(rhs)),
            Rule::bsub => Binop(ast::Binop::Sub, Box::from(lhs), Box::from(rhs)),
            Rule::bmul => Binop(ast::Binop::Mult, Box::from(lhs), Box::from(rhs)),
            _ => unreachable!(),
        },
    )
}

fn into_ast_cond(pairs: Pairs<Rule>) -> CAst {
    use CAst::*;
    PC_COND.climb(
        pairs,
        |pair: Pair<Rule>| match pair.as_rule() {
            Rule::cond => into_ast_cond(pair.into_inner()),
            Rule::uatom => {
                let mut inner = pair.into_inner();
                match inner.next().map(|p| p.as_rule()) {
                    Some(Rule::unot) => CAst::Not(
                        inner
                            .next()
                            .map(Pairs::single)
                            .map(into_ast_cond)
                            .map(Box::from)
                            .unwrap(),
                    ),
                    _ => unreachable!(),
                }
            }
            Rule::batom => {
                let mut inner = pair.into_inner();
                let left = inner.next().map(Pairs::single).map(into_ast_expr).unwrap();
                let cmp = inner
                    .next()
                    .ok_or(anyhow::Error::msg("Missing comparison token"))
                    .and_then(|s| s.as_str().parse())
                    .unwrap();
                let right = inner.next().map(Pairs::single).map(into_ast_expr).unwrap();
                CAst::Cmp(cmp, left, right)
            }
            _ => unreachable!(),
        },
        |lhs: CAst, op: Pair<Rule>, rhs: CAst| match op.as_rule() {
            Rule::cand => CAst::And(Box::from(lhs), Box::from(rhs)),
            Rule::cor => CAst::Or(Box::from(lhs), Box::from(rhs)),
            _ => unreachable!(),
        },
    )
}
