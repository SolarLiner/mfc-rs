use std::fmt::{Debug, Display, Error, Formatter};

use crate::env::FnSignature;

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
    Cst(usize),
    Ref(RAst),
    ECall(RAst, Vec<EAst>),
    Unop(Unop, BEAst),
    Binop(Binop, BEAst, BEAst),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CAst {
    And(BCAst, BCAst),
    Or(BCAst, BCAst),
    Not(BCAst),
    Cmp(Compare, EAst, EAst),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SAst {
    Set(RAst, EAst),
    If(CAst, BSAst, BSAst),
    While(CAst, BSAst),
    Call(RAst, Vec<EAst>),
    Ret(EAst),
    Declare(String),
    DeclareFun(FnSignature),
    Block(Vec<SAst>),
}

impl Binop {
    pub fn prec(&self) -> usize {
        match self {
            Binop::Add | Binop::Sub => 0,
            Binop::Mult => 1,
        }
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
            EAst::Unop(u, o) => write!(f, "{} {}", u, o),
            EAst::Binop(b, o1, o2) => {
                let l = match o1.as_ref() {
                    EAst::Binop(e, o11, o12) if b.prec() > e.prec() => {
                        format!("({} {} {})", o11, e, o12)
                    }
                    x => format!("{}", x),
                };
                let r = match o2.as_ref() {
                    EAst::Binop(e, o21, o22) if b.prec() > e.prec() => {
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
            SAst::DeclareFun(FnSignature(n, r, q)) => format!("fun {}(r={}, q={})", n, r, q),
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
