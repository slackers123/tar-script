pub mod ast;
// pub mod astint;
pub mod bcvm;
pub mod bcasm;

use std::fs;

extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate core;

#[derive(Parser)]
#[grammar = "tar-script.pest"]
pub struct TarParser;

use pest::iterators::Pairs;
use pest::Parser;

fn main() {

    let program = fs::read_to_string("main.tar").unwrap();

    let pairs: Pairs<Rule> = TarParser::parse(Rule::Program, program.as_str()).unwrap();

    let (defs, funcs) = ast::parse_to_ast(pairs);

    let (mut funcs, entry) = bcasm::assemble_bc(defs, funcs);

    // println!("{:?}", funcs);
    bcvm::run_func(entry.unwrap(), &mut funcs, vec![], 0);
}