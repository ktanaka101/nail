mod terminal;

use std::cell::RefCell;
use std::convert::{TryFrom, TryInto};
use std::ffi::CString;
use std::io;
use std::io::Write;
use std::rc::Rc;

use anyhow::Result;
use inkwell::context::Context;
use inkwell::OptimizationLevel;
use termion::event::Key;
use termion::input::TermRead;
use termion::raw::IntoRawMode;

use crate::evaluator::env::Environment;
use crate::evaluator::object;
use crate::evaluator::{define_macros, eval_node, expand_macros};
use crate::lexer::Lexer;
use crate::llvm::compiler;
use crate::llvm::compiler::Compiler;
use crate::parser::Parser;

const PROMPT: &str = ">> ";

pub enum Interface {
    TTY,
    STD,
}

pub enum Executer {
    Evaluator,
    LLVM(Interface),
}

pub fn start(executer: Executer) {
    match executer {
        Executer::Evaluator => start_evaluator(),
        Executer::LLVM(interface) => start_llvm(interface),
    }
}

fn start_llvm(interface: Interface) {
    match interface {
        Interface::STD => start_llvm_on_std(),
        Interface::TTY => start_llvm_on_tty(),
    }
}

fn start_llvm_on_std() {
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

        let main_fn = match compiler.compile(&program.into(), true, compiler::Output::CStringPtr) {
            Ok(f) => f,
            Err(e) => {
                println!("LLVM error: {}", e);
                continue;
            }
        };

        let result_string = {
            let c_string_ptr = unsafe { main_fn.call() };
            unsafe { CString::from_raw(c_string_ptr) }
                .into_string()
                .unwrap()
        };

        println!("{}", result_string);

        inputs = try_inputs.clone();
    }
}

fn llvm_run(code: &str) -> Result<String> {
    let context = Context::create();
    let module = context.create_module("top");
    let builder = context.create_builder();
    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();
    let mut compiler = Compiler::new(&context, &module, &builder, &execution_engine);

    let lexer = Lexer::new(code.to_string());
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program()?;
    let main_fn = compiler.compile(&program.into(), false, compiler::Output::CStringPtr)?;

    let result_string = {
        let c_string_ptr = unsafe { main_fn.call() };
        unsafe { CString::from_raw(c_string_ptr) }
            .into_string()
            .unwrap()
    };

    Ok(result_string)
}

fn start_llvm_on_tty() {
    let stdin = io::stdin();
    let stdout = io::stdout().into_raw_mode().unwrap();

    let mut term = terminal::Terminal::new(stdout);
    term.init();
    term.write(PROMPT);

    let mut inputs = "".to_string();
    let mut line = "".to_string();

    for c in stdin.keys() {
        match c.unwrap() {
            Key::Char('q') => {
                term.write("q");
                break;
            }
            Key::Char(c) => match c {
                '\n' => {
                    if line.is_empty() {
                        term.next_line();
                        term.write(PROMPT);
                        continue;
                    }

                    line.push(';');

                    let try_inputs = {
                        let mut inputs = inputs.clone();
                        inputs.push('\n');
                        inputs.push_str(&line);
                        inputs
                    };

                    line = "".to_string();

                    match llvm_run(try_inputs.as_str()) {
                        Ok(result_string) => {
                            term.next_line();
                            term.write(result_string.as_str());

                            term.next_line();
                            term.write(PROMPT);

                            inputs = try_inputs.clone();
                        }
                        Err(e) => {
                            term.next_line();
                            term.write(e.to_string().as_str());

                            term.next_line();
                            term.write(PROMPT);

                            continue;
                        }
                    }
                }
                c => {
                    let pos = term.cursor_pos();
                    let idx = usize::from(pos.col) - PROMPT.len() - 1;
                    line.insert(idx, c);

                    term.clear_current_line();
                    term.write(format!("{}{}", PROMPT, line.as_str()).as_str());

                    term.move_cursor(terminal::MoveCursorAction::Pos(pos));
                    term.move_cursor(terminal::MoveCursorAction::Right(1));
                }
            },
            Key::Alt(c) => println!("Alt-{}", c),
            Key::Ctrl(c) => println!("Ctrl-{}", c),
            Key::Left => {
                let left_limit = u16::try_from(PROMPT.len()).unwrap() + 1;
                if term.cursor_pos().col <= left_limit {
                    continue;
                }

                term.move_cursor(terminal::MoveCursorAction::Left(1));
            }
            Key::Right => println!("<right>"),
            Key::Up => println!("<up>"),
            Key::Down => println!("<down>"),
            _ => println!("Other"),
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
