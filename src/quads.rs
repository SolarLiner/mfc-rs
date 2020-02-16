use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

use anyhow::Error;

use crate::ast::*;
use crate::env::{Env, FnSignature, Frame};
use crate::quads::Quad::{QBinop, QBranch, QCmp, QLabel, QSeti};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Quad {
    QBinop(Binop, String, String, String),
    QIfp(String, usize),
    QUnop(Unop, String, String),
    QSet(String, String),
    QSeti(String, usize),
    QStr(String, String),
    QLd(String, String),
    QLabel(String),
    QPush(String),
    QPop(String),
    QGoto(String),
    QCmp(String, String),
    QBranch(Compare, String),
}

impl Quad {
    pub fn quad_s(s: SAst, env: &mut Env) -> anyhow::Result<Vec<Quad>> {
        use Quad::*;
        use RAst::*;
        use SAst::*;
        match s {
            Set(Id(i), e) => match env.lookup_top(&i) {
                Some(off) => {
                    let v = env.new_tmp();
                    let (mut q1, v1) = Self::quad_e(e, env);
                    q1.extend(vec![QIfp(v.clone(), off), QStr(v1, v)]);
                    Ok(q1)
                }
                None => Err(Error::from(QuadError::UnknownVariable(i))),
            },
            Block(v) => v
                .into_iter()
                .map(|s| Self::quad_s(s, env))
                .collect::<Result<Vec<_>, _>>()
                .map(|v| v.into_iter().flatten().collect()),
            Call(Id(i), le) => {
                let le_len = le.len();
                let lres: Vec<_> = le.into_iter().map(|e| Self::quad_e(e, env)).collect();
                let (lq, lr): (Vec<Vec<_>>, Vec<String>) = lres.into_iter().unzip();
                let q: Vec<_> = lq.into_iter().flatten().collect();
                let push: Vec<_> = lr.into_iter().map(QPush).collect();
                match env.lookup_function(&i) {
                    Some(FnSignature(l, r, p)) if r == 0 && p == le_len => {
                        let mut v = vec![];
                        v.extend(q.into_iter());
                        v.extend(push.into_iter());
                        v.push(QGoto(l));
                        Ok(v)
                    }
                    Some(FnSignature(name, _, actual)) => {
                        Err(Error::from(QuadError::MismatchedFunctionCall {
                            name,
                            expected: le_len,
                            actual,
                        }))
                    }
                    _ => unreachable!(),
                }
            }
            If(c, s1, s2) => {
                let si = env.new_label();
                let sinon = env.new_label();
                let qc = Self::quad_c(c, env, &si, &sinon);
                let q1 = Self::quad_s(*s1, env)?;
                let q2 = Self::quad_s(*s2, env)?;
                let r = {
                    let mut v = vec![];
                    v.extend(qc.into_iter());
                    v.push(QLabel(si));
                    v.extend(q1.into_iter());
                    v.push(QLabel(sinon));
                    v.extend(q2.into_iter());
                    v
                };
                Ok(r)
            }
            While(c, s) => {
                let r#loop = env.new_label();
                let body = env.new_label();
                let end = env.new_label();
                let qc = Self::quad_c(c, env, &body, &end);
                let q = Self::quad_s(*s, env)?;
                let r = {
                    let mut v = vec![QLabel(r#loop)];
                    v.extend(qc.into_iter());
                    v.push(QLabel(body));
                    v.extend(q.into_iter());
                    v.push(QLabel(end));
                    v
                };
                Ok(r)
            }
            Ret(e) => {
                let (mut qe, ve) = Self::quad_e(e, env);
                qe.push(QPush(ve));
                Ok(qe)
            }
            Declare(s) => {
                env.new_local(s);
                Ok(vec![])
            }
            DeclareFun(FnSignature(s, r, p)) => {
                env.new_function(s.clone(), FnSignature(s, r, p));
                Ok(vec![])
            }
        }
    }

    pub fn quad_e(e: EAst, env: &mut Env) -> (Vec<Quad>, String) {
        use EAst::*;
        match e {
            Binop(op, e1, e2) => {
                let r = env.new_tmp();
                let (q1, r1) = Self::quad_e(*e1, env);
                let (q2, r2) = Self::quad_e(*e2, env);
                let res = {
                    let mut v = vec![];
                    v.extend(q1.into_iter());
                    v.extend(q2.into_iter());
                    v.push(QBinop(op, r.clone(), r1, r2));
                    v
                };
                (res, r)
            }
            Cst(i) => {
                let r = env.new_tmp();
                (vec![QSeti(r.clone(), i)], r)
            }
            _ => (vec![], env.new_tmp()),
        }
    }

    pub fn quad_c(c: CAst, env: &mut Env, si: &str, sinon: &str) -> Vec<Quad> {
        Self::cond(c, env, si, sinon, true)
    }

    fn inv(c: Compare) -> Compare {
        use Compare::*;
        match c {
            Lt => Ge,
            Le => Gt,
            Gt => Le,
            Ge => Lt,
            Ne => Eq,
            Eq => Ne,
        }
    }

    fn cond(c: CAst, env: &mut Env, si: &str, sinon: &str, p: bool) -> Vec<Quad> {
        use CAst::*;
        use Quad::QGoto;
        match c {
            Not(c) => Self::cond(*c, env, si, sinon, true),
            Or(c1, c2) => {
                let l = env.new_label();
                let q1 = Self::cond(*c1, env, &l, sinon, true);
                let q2 = Self::cond(*c2, env, si, sinon, true);
                Self::make_quad_list_from_cond(q1, q2, &l)
            }
            And(c1, c2) => {
                let l = env.new_label();
                let q1 = Self::cond(*c1, env, &l, sinon, false);
                let q2 = Self::cond(*c2, env, si, sinon, false);
                Self::make_quad_list_from_cond(q1, q2, &l)
            }
            Cmp(c, e1, e2) => {
                let (q1, v1) = Self::quad_e(e1, env);
                let (q2, v2) = Self::quad_e(e2, env);
                if p {
                    let mut v = q1;
                    v.extend(q2.into_iter());
                    v.extend(vec![
                        QCmp(v1, v2),
                        QBranch(c, si.to_string()),
                        QGoto(sinon.to_string()),
                    ]);
                    v
                } else {
                    let mut v = q1;
                    v.extend(q2.into_iter());
                    v.extend(vec![
                        QCmp(v1, v2),
                        QBranch(Self::inv(c), sinon.to_string()),
                        QGoto(si.to_string()),
                    ]);
                    v
                }
            }
        }
    }

    fn make_quad_list_from_cond(q1: Vec<Quad>, q2: Vec<Quad>, l: &str) -> Vec<Quad> {
        use Quad::QGoto;
        match q1.last().as_ref() {
            Some(&QGoto(a)) if a == l => {
                let l = q1.len() - 1;
                let mut v: Vec<_> = q1.into_iter().take(l).collect();
                v.extend(q2.into_iter());
                v
            }
            _ => {
                let mut v = q1;
                v.push(QLabel(l.to_string()));
                v.extend(q2.into_iter());
                v
            }
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum QuadError {
    UnknownVariable(String),
    MismatchedFunctionCall {
        name: String,
        expected: usize,
        actual: usize,
    },
}

impl Display for QuadError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            QuadError::UnknownVariable(s) => write!(f, "Unknown variable {}", s),
            QuadError::MismatchedFunctionCall {
                name,
                expected,
                actual,
            } => write!(
                f,
                "Mismatched function call to {}: Expected {} arguments, found {}",
                name, expected, actual
            ),
        }
    }
}

impl std::error::Error for QuadError {}

impl Display for Quad {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        use Quad::*;
        match self {
            QBinop(b, r1, r2, ro) => write!(f, "bo {:?}\t{}, {} -> {}", b, r1, r2, ro),
            QUnop(u, ri, ro) => write!(f, "  uo {:?}\t{} -> {}", u, ri, ro),
            QIfp(s, i) => write!(f, " ifp\t{} {}", s, i),
            QSet(s, t) => write!(f, " set\t{} <- {}", s, t),
            QSeti(s, i) => write!(f, "seti\t{} <- {}", s, i),
            QStr(s, t) => write!(f, " str\t{}, {}", s, t),
            QLd(s, t) => write!(f, " ld\t{}, {}", s, t),
            QLabel(s) => write!(f, "\n{}:", s),
            QPush(s) => write!(f, "push\t{}", s),
            QPop(s) => write!(f, " pop\t{}", s),
            QGoto(s) => write!(f, "goto\t{}", s),
            QCmp(s, t) => write!(f, " cmp\t{}, {}", s, t),
            QBranch(c, l) => write!(f, "b {:?}\t{}", c, l),
        }
    }
}
