use std::fmt::{Debug, Display, Error, Formatter};
use std::str::FromStr;

use crate::env::FnSignature;

pub type Value = isize;
type BEAst = Box<EAst>;
type BCAst = Box<CAst>;
type BSAst = Box<SAst>;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Binop {
    Add,
    Sub,
    Mult,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Compare {
    Ge,
    Gt,
    Le,
    Lt,
    Ne,
    Eq,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Unop {
    Neg,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RAst {
    Id(String),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum EAst {
    Cst(Value),
    Ref(RAst),
    ECall(RAst, Vec<EAst>),
    EUnop(Unop, BEAst),
    EBinop(Binop, BEAst, BEAst),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CAst {
    And(BCAst, BCAst),
    Or(BCAst, BCAst),
    Not(BCAst),
    Cmp(Compare, EAst, EAst),
    Bool(bool),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SAst {
    Set(RAst, EAst),
    If(CAst, BSAst, BSAst),
    While(CAst, BSAst),
    Call(RAst, Vec<EAst>),
    Ret(EAst),
    Declare(String),
    DeclareExternFun(FnSignature),
    DeclareFun(FnSignature, Vec<SAst>),
    Block(Vec<SAst>),
}

impl Unop {
    pub fn exec(&self, v: Value) -> Value {
        match self {
            Unop::Neg => -v,
        }
    }
}

impl Binop {
    pub fn prec(&self) -> usize {
        match self {
            Binop::Add | Binop::Sub => 0,
            Binop::Mult => 1,
        }
    }
    pub fn exec(&self, a: Value, b: Value) -> Value {
        match self {
            Binop::Add => a + b,
            Binop::Sub => a - b,
            Binop::Mult => a * b,
        }
    }
}

impl Compare {
    pub fn compare(&self, a: isize, b: isize) -> bool {
        use Compare::*;
        match self {
            Lt => a < b,
            Le => a <= b,
            Gt => a > b,
            Ge => a >= b,
            Ne => a != b,
            Eq => a == b,
        }
    }
}

impl EAst {
    pub fn simplify(&self) -> Option<Value> {
        use EAst::*;
        match self {
            Cst(a) => Some(*a),
            EUnop(unop, b) => b.simplify().map(|b| unop.exec(b)),
            EBinop(binop, a, b) => match (a.simplify(), b.simplify()) {
                (Some(a), Some(b)) => Some(binop.exec(a, b)),
                _ => None,
            },
            _ => None,
        }
    }
}

impl SAst {
    pub fn simplify(self) -> Self {
        use SAst::*;
        match self {
            If(c, by, bn) => {
                if let Some(r) = c.simplify() {
                    if r {
                        *by
                    } else {
                        *bn
                    }
                } else {
                    If(c, by, bn)
                }
            }
            While(c, b) => {
                if let Some(true) = c.simplify() {
                    eprintln!("WARNING: Infinite loop:\n{}", While(c, b.clone()));
                    While(CAst::Bool(true), b)
                } else {
                    Block(vec![])
                }
            }
            x => x,
        }
    }
}

impl CAst {
    pub fn simplify(&self) -> Option<bool> {
        use CAst::*;
        use EAst::*;
        match self {
            Cmp(c, Cst(a), Cst(b)) => Some(c.compare(*a, *b)),
            Cmp(c, a, b) => match (a.simplify(), b.simplify()) {
                (Some(a), Some(b)) => Some(c.compare(a, b)),
                _ => None,
            },
            And(a, b) => match (a.simplify(), b.simplify()) {
                (Some(a), Some(b)) => Some(a && b),
                _ => None,
            },
            Or(a, b) => match (a.simplify(), b.simplify()) {
                (Some(a), Some(b)) => Some(a || b),
                _ => None,
            },
            Not(a) => a.simplify().map(|b| !b),
            Bool(b) => Some(*b),
        }
    }
}

impl FromStr for Compare {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Compare::*;
        let res = match s.trim() {
            ">" => Gt,
            ">=" => Ge,
            "<" => Lt,
            "<=" => Le,
            "==" => Eq,
            "!=" => Ne,
            _ => {
                return Err(anyhow::Error::msg(format!(
                    "Unknown comparison token {}",
                    s
                )))
            }
        };
        Ok(res)
    }
}

impl Display for Compare {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use Compare::*;
        let token = match self {
            Lt => "<",
            Le => "<=",
            Gt => ">",
            Ge => ">=",
            Ne => "!=",
            Eq => "==",
        };
        write!(f, "{}", token)
    }
}

impl Display for Binop {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        let s = match self {
            Binop::Sub => "-",
            Binop::Mult => "*",
            Binop::Add => "+",
        };
        write!(f, "{}", s)
    }
}

impl Display for Unop {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "-")
    }
}

impl Display for RAst {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        let s = match self {
            RAst::Id(s) => s,
        };
        write!(f, "{}", s)
    }
}

impl Display for EAst {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            EAst::EUnop(u, o) => write!(f, "{} {}", u, o),
            EAst::EBinop(b, o1, o2) => {
                let l = match o1.as_ref() {
                    EAst::EBinop(e, o11, o12) if b.prec() > e.prec() => {
                        format!("({} {} {})", o11, e, o12)
                    }
                    x => format!("{}", x),
                };
                let r = match o2.as_ref() {
                    EAst::EBinop(e, o21, o22) if b.prec() > e.prec() => {
                        format!("({} {} {})", o21, e, o22)
                    }
                    x => format!("{}", x),
                };
                write!(f, "{} {} {}", l, b, r)
            }
            EAst::Cst(c) => write!(f, "{}", c),
            EAst::Ref(r) => write!(f, "{}", r),
            EAst::ECall(n, a) => write!(
                f,
                "{}({})",
                n,
                a.into_iter()
                    .map(|e| format!("{}", e))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl Display for CAst {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            CAst::Cmp(c, e1, e2) => write!(f, "{} {} {}", e1, c, e2),
            CAst::Not(c) => write!(f, "not {}", c),
            CAst::Or(l, r) => write!(f, "{} or {}", l, r),
            CAst::And(l, r) => write!(f, "{} and {}", l, r),
            CAst::Bool(b) => write!(f, "{}", b),
        }
    }
}

impl SAst {
    pub fn pprint(&self) -> String {
        self.pinternal(0)
    }
    fn pinternal(&self, indent: usize) -> String {
        let is = std::iter::repeat("  ")
            .take(indent)
            .fold(String::new(), |mut s, i| {
                s.push_str(i);
                s
            });
        let s = match self {
            SAst::Ret(e) => format!("return {}", e),
            SAst::Set(r, e) => format!("{} = {}", r, e),
            SAst::Declare(i) => format!("var {}", i),
            SAst::DeclareFun(FnSignature(s, r, q), b) => format!(
                "fn {}(r={}, q={}) {{\n{is}{}\n{is}}}",
                s,
                r,
                q,
                b.into_iter()
                    .map(|s| s.pinternal(indent + 1))
                    .collect::<Vec<_>>()
                    .join("\n"),
                is = is
            ),
            SAst::DeclareExternFun(FnSignature(n, r, q)) => {
                format!("extern fun {}(r={}, q={})", n, r, q)
            }
            SAst::Block(v) => format!(
                "{}{{\n{}\n{}}}",
                &is,
                v.into_iter()
                    .map(pinternal_ident(indent + 1))
                    .collect::<Vec<_>>()
                    .join("\n"),
                &is
            ),
            SAst::Call(r, v) => format!(
                "{}({})",
                r,
                v.into_iter().map(do_fmt).collect::<Vec<_>>().join(", ")
            ),
            SAst::If(c, tb, fb) => format!(
                "if({})\n{}\n{}else\n{}",
                c,
                tb.pinternal(indent + 1),
                &is,
                fb.pinternal(indent + 1),
            ),
            SAst::While(c, b) => format!("while({})\n{}", c, b.pinternal(indent + 1)),
        };
        format!("{}{}", is, s)
    }
}

impl Display for SAst {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", self.pprint())
    }
}

fn do_fmt<T: Display>(e: T) -> String {
    format!("{}", e)
}

fn pinternal_ident(indent: usize) -> impl Fn(&SAst) -> String {
    move |s| s.pinternal(indent)
}
