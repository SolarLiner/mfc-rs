use pest::error::Error;
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest::prec_climber::{Assoc, Operator, PrecClimber};

use crate::ast;
use crate::ast::{EAst, RAst, SAst};
use crate::env::FnSignature;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct GENParser;

lazy_static! {
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Assoc::*;
        use Rule::*;
        PrecClimber::new(vec![
            Operator::new(badd, Left) | Operator::new(bsub, Left),
            Operator::new(bmul, Left),
        ])
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
    match dbg!(&pair).as_rule() {
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
        _ => unimplemented!(),
    }
}

fn into_ast_expr(pairs: Pairs<Rule>) -> EAst {
    use EAst::*;
    PREC_CLIMBER.climb(
        pairs,
        |pair: Pair<Rule>| match pair.as_rule() {
            Rule::number => Cst(pair.as_str().parse().unwrap()),
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
