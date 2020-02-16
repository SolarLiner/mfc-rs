use crate::ast::{Binop, CAst, Compare, EAst, RAst, SAst};
use crate::env::{Env, FnSignature};
use crate::quads::Quad;

mod ast;
mod env;
mod quads;

fn main() {
    let mut env = Env::new();
    let expr = EAst::Binop(
        Binop::Mult,
        Box::new(EAst::Cst(35)),
        Box::new(EAst::Binop(
            Binop::Sub,
            Box::new(EAst::Cst(40)),
            Box::new(EAst::Ref(RAst::Id(format!("x")))),
        )),
    );
    let stmt = SAst::Block(vec![
        SAst::DeclareFun(FnSignature(format!("read"), 1, 1)),
        SAst::Declare(format!("x")),
        SAst::Set(
            RAst::Id(format!("x")),
            EAst::ECall(RAst::Id(format!("read")), vec![]),
        ),
        SAst::If(
            CAst::Cmp(
                Compare::Lt,
                EAst::Ref(RAst::Id(format!("x"))),
                EAst::Cst(40),
            ),
            Box::new(SAst::Ret(expr)),
            Box::new(SAst::Ret(EAst::Cst(0))),
        ),
    ]);
    println!("{}", stmt);
    let q = Quad::quad_s(stmt, &mut env);
    let s = match q {
        Ok(v) => v
            .into_iter()
            .map(|q| format!("{}", q))
            .collect::<Vec<_>>()
            .join("\n"),
        Err(e) => format!("E: {:?}", e),
    };
    println!("\n{}", s);
}
