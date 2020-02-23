use pest::error::Error;
use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use pest::Parser;

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

pub fn parse(input: &str) -> Result<Expr, Error<Rule>> {
    Ok(into_ast(parse_raw(input)?))
}

pub fn parse_raw(input: &str) -> Result<Pairs<Rule>, Error<Rule>> {
    GENParser::parse(Rule::root, input)
}

fn into_ast(pairs: Pairs<Rule>) -> Expr {
    PREC_CLIMBER.climb(
        pairs,
        |pair: Pair<Rule>| match dbg!(&pair).as_rule() {
            Rule::number => Expr::Atom(pair.as_str().parse().unwrap()),
            Rule::expr => into_ast(pair.into_inner()),
            _ => unreachable!(),
        },
        |lhs: Expr, op: Pair<Rule>, rhs: Expr| match op.as_rule() {
            Rule::badd => Expr::Add(BExpr::from(lhs), BExpr::from(rhs)),
            Rule::bsub => Expr::Sub(BExpr::from(lhs), BExpr::from(rhs)),
            Rule::bmul => Expr::Mul(BExpr::from(lhs), BExpr::from(rhs)),
            _ => unreachable!(),
        },
    )
}

pub type BExpr = Box<Expr>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    Add(BExpr, BExpr),
    Sub(BExpr, BExpr),
    Mul(BExpr, BExpr),
    Atom(i32),
}
