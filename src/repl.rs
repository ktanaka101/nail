use std::cell::RefCell;
use std::io;
use std::io::Write;
use std::rc::Rc;

use inkwell::context::Context;

use crate::evaluator::env::Environment;
use crate::evaluator::object;
use crate::evaluator::{define_macros, eval_node, expand_macros};
use crate::lexer::Lexer;
use crate::llvm::compiler::Compiler;
use crate::parser::Parser;

const PROMPT: &str = ">> ";

pub enum Executer {
    Evaluator,
    LLVM,
}

pub fn start(executer: Executer) {
    match executer {
        Executer::Evaluator => start_evaluator(),
        Executer::LLVM => start_llvm(),
    }
}

fn start_llvm() {
    let context = Context::create();
    let compiler = Compiler::new(&context);
    let sum = compiler.compile().unwrap();
    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        let mut line = String::new();
        if io::stdin().read_line(&mut line).is_err() || line == "\n" {
            continue;
        }

        let x = 1u64;
        let y = 2u64;
        let z = 3u64;

        unsafe {
            println!("{} + {} + {} = {}", x, y, z, sum.call(x, y, z));
            assert_eq!(sum.call(x, y, z), x + y + z);
        }
    }
}

fn start_evaluator() {
    let env = Rc::new(RefCell::new(Environment::new(None)));
    let macro_env = Rc::new(RefCell::new(Environment::new(None)));

    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        let mut line = String::new();
        if io::stdin().read_line(&mut line).is_err() || line == "\n" {
            continue;
        }

        let lexer = Lexer::new(line);
        let mut parser = Parser::new(lexer);

        let mut program = match parser.parse_program() {
            Ok(p) => p,
            Err(x) => {
                println!("Parse error: {}", x);
                continue;
            }
        };

        let res = define_macros(&mut program, Rc::clone(&macro_env));
        if let Err(e) = res {
            println!("Define macro error: {}", e);
            continue;
        }

        let expanded = match expand_macros(program.into(), Rc::clone(&macro_env)) {
            Ok(expanded) => expanded,
            Err(e) => {
                println!("Expand macro error: {}", e);
                continue;
            }
        };

        let evaluated = eval_node(&expanded, Rc::clone(&env));
        match evaluated {
            Ok(o) => match o {
                object::Object::Null(_) => continue,
                o => println!("{}", o),
            },
            Err(e) => {
                println!("Eval error: {}", e);
                continue;
            }
        }
    }
}
