use std::cell::RefCell;
use std::io;
use std::io::Write;
use std::rc::Rc;

use inkwell::context::Context;
use inkwell::OptimizationLevel;

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
    let mut inputs = "".to_string();

    loop {
        let context = Context::create();
        let module = context.create_module("top");
        let builder = context.create_builder();
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        let mut compiler = Compiler::new(&context, &module, &builder, &execution_engine);

        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        let mut line = String::new();
        if io::stdin().read_line(&mut line).is_err() || line == "\n" {
            continue;
        }
        line.push_str(";");

        let mut try_inputs = inputs.clone();
        try_inputs.push('\n');
        try_inputs.push_str(&line);

        let lexer = Lexer::new(try_inputs.clone());
        let mut parser = Parser::new(lexer);

        let program = match parser.parse_program() {
            Ok(p) => p,
            Err(e) => {
                println!("Parse error: {}", e);
                continue;
            }
        };

        let main_fn = match compiler.compile(&program.into(), true) {
            Ok(f) => f,
            Err(e) => {
                println!("LLVM error: {}", e);
                continue;
            }
        };

        let result = unsafe { main_fn.call() };
        println!("{}", result);

        inputs = try_inputs.clone();
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
