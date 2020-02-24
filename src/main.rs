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
    let reader = Interface::new("parser")?;
    reader.set_prompt(">>> ")?;
    while let ReadResult::Input(input) = reader.read_line()? {
        match parse::parse(&input) {
            Ok(p) => println!("{:?}", p),
            Err(e) => eprintln!("Error! {}", e),
        }
        reader.add_history_unique(input);
    }
    println!("Bye");
    Ok(())
}
