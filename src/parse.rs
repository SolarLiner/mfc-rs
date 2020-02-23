use pest::error::Error;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use pest::Parser;

use crate::ast;
use crate::ast::EAst;

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

pub fn parse(input: &str) -> Result<EAst, Error<Rule>> {
    Ok(into_ast(parse_raw(input)?))
}

pub fn parse_raw(input: &str) -> Result<Pairs<Rule>, Error<Rule>> {
    GENParser::parse(Rule::root, input)
}

fn into_ast(pairs: Pairs<Rule>) -> EAst {
    use EAst::*;
    PREC_CLIMBER.climb(
        pairs,
        |pair: Pair<Rule>| match pair.as_rule() {
            Rule::number => Cst(pair.as_str().parse().unwrap()),
            Rule::expr => into_ast(pair.into_inner()),
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
