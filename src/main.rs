extern crate from_pest;
#[macro_use]
extern crate lazy_static;
extern crate pest;
#[macro_use]
extern crate pest_derive;

use linefeed::{Interface, ReadResult};

mod ast;
mod env;
mod parse;
mod quads;

fn main() -> anyhow::Result<()> {
    let mut env = env::Env::new();
    let reader = Interface::new("parser")?;
    reader.set_prompt(">>> ")?;
    while let ReadResult::Input(input) = reader.read_line()? {
        match parse::parse(&input) {
            Ok(p) => {
                println!("{:#?}", &p);
                match quads::Quad::quad_s(p, &mut env) {
                    Ok(q) => println!(
                        "{}",
                        q.into_iter()
                            .map(|q| format!("{}\n", q))
                            .collect::<String>()
                    ),
                    Err(e) => eprintln!("Quads error: {:#}", e),
                }
            }
            Err(e) => eprintln!("Parsing error! {:#}", e),
        }
        reader.add_history_unique(input);
    }
    println!("Bye");
    Ok(())
}
