use std::cell::RefCell;
use std::convert::TryInto;
use std::ffi::CString;
use std::io;
use std::io::Write;
use std::rc::Rc;

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

fn start_llvm_on_tty() {
    let stdin = io::stdin();
    let mut stdout = io::stdout().into_raw_mode().unwrap();

    let mut inputs = "".to_string();
    write!(
        stdout,
        "{}{}>> ",
        termion::clear::All,
        termion::cursor::Goto(1, 1),
    )
    .unwrap();

    stdout.flush().unwrap();
    let mut row = 1;

    let mut line = "".to_string();

    for c in stdin.keys() {
        match c.unwrap() {
            Key::Char('q') => {
                write!(stdout, "q").unwrap();
                break;
            }
            Key::Char(c) => match c {
                '\n' => {
                    if line.is_empty() {
                        row += 1;
                        write!(stdout, "{}>> ", termion::cursor::Goto(1, row)).unwrap();
                        stdout.flush().unwrap();
                        continue;
                    }

                    let context = Context::create();
                    let module = context.create_module("top");
                    let builder = context.create_builder();
                    let execution_engine = module
                        .create_jit_execution_engine(OptimizationLevel::None)
                        .unwrap();

                    let mut compiler =
                        Compiler::new(&context, &module, &builder, &execution_engine);

                    let mut try_inputs = inputs.clone();
                    try_inputs.push('\n');

                    line.push(';');
                    try_inputs.push_str(&line);
                    line = "".to_string();

                    let lexer = Lexer::new(try_inputs.clone());
                    let mut parser = Parser::new(lexer);

                    let program = match parser.parse_program() {
                        Ok(p) => p,
                        Err(e) => {
                            row += 1;
                            write!(
                                stdout,
                                "{}Parse error: {}",
                                termion::cursor::Goto(1, row),
                                e
                            )
                            .unwrap();
                            row += 1;
                            write!(stdout, "{}>> ", termion::cursor::Goto(1, row)).unwrap();
                            stdout.flush().unwrap();
                            continue;
                        }
                    };

                    let main_fn = match compiler.compile(
                        &program.into(),
                        false,
                        compiler::Output::CStringPtr,
                    ) {
                        Ok(f) => f,
                        Err(e) => {
                            row += 1;
                            write!(stdout, "{}LLVM error: {}", termion::cursor::Goto(1, row), e)
                                .unwrap();
                            row += 1;
                            write!(stdout, "{}>> ", termion::cursor::Goto(1, row)).unwrap();
                            stdout.flush().unwrap();
                            continue;
                        }
                    };

                    let result_string = {
                        let c_string_ptr = unsafe { main_fn.call() };
                        unsafe { CString::from_raw(c_string_ptr) }
                            .into_string()
                            .unwrap()
                    };

                    row += 1;
                    write!(stdout, "{}{}", termion::cursor::Goto(1, row), result_string).unwrap();

                    row += 1;
                    write!(stdout, "{}>> ", termion::cursor::Goto(1, row)).unwrap();

                    inputs = try_inputs.clone();
                }
                c => {
                    line.push(c);
                    let line_offset: u16 = line.len().try_into().unwrap();
                    let offset = line_offset + 4;
                    write!(
                        stdout,
                        "{}>> {}{}",
                        termion::cursor::Goto(1, row),
                        line,
                        termion::cursor::Goto(offset, row)
                    )
                    .unwrap();
                }
            },
            Key::Alt(c) => println!("Alt-{}", c),
            Key::Ctrl(c) => println!("Ctrl-{}", c),
            Key::Left => println!("<left>"),
            Key::Right => println!("<right>"),
            Key::Up => println!("<up>"),
            Key::Down => println!("<down>"),
            _ => println!("Other"),
        }

        stdout.flush().unwrap();
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
