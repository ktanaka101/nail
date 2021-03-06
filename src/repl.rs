mod terminal;

use std::cell::RefCell;
use std::convert::TryFrom;
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

use crate::ast_parser::Parser;
use crate::evaluator::env::Environment;
use crate::evaluator::object;
use crate::evaluator::{define_macros, eval_node, expand_macros};
use crate::lexer::Lexer;
use crate::llvm::codegen;
use crate::llvm::codegen::Codegen;
use crate::normalizer;
use crate::type_checker;

const PROMPT: &str = ">> ";

#[derive(Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
struct History {
    stack: Vec<String>,
    ptr: usize,
}

impl History {
    fn new() -> Self {
        History {
            stack: vec![],
            ptr: 0,
        }
    }

    fn push(&mut self, line: String) {
        self.stack.push(line);
        self.ptr += 1;
    }

    fn next(&mut self) -> Result<()> {
        if self.ptr < self.stack.len() {
            self.ptr += 1;
            Ok(())
        } else {
            anyhow::bail!("The stack pointer points to the wrong position.")
        }
    }

    fn back(&mut self) -> Result<()> {
        if self.ptr > 1 {
            self.ptr -= 1;
            Ok(())
        } else {
            anyhow::bail!("The stack pointer points to the wrong position.")
        }
    }

    fn get_current(&self) -> &String {
        self.stack
            .get(self.ptr - 1)
            .expect("The stack pointer points to the wrong position.")
    }

    fn get_curernt_mut(&mut self) -> &mut String {
        self.stack
            .get_mut(self.ptr - 1)
            .expect("The stack pointer points to the wrong position.")
    }
}

pub enum Executer {
    Evaluator,
    Llvm,
}

pub fn start(executer: Executer) {
    match executer {
        Executer::Evaluator => start_evaluator(),
        Executer::Llvm => start_llvm(),
    }
}

fn llvm_run(code: &str) -> Result<String> {
    let context = Context::create();
    let module = context.create_module("top");
    let builder = context.create_builder();
    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();
    let mut codegen = Codegen::new(&context, &module, &builder, &execution_engine);

    let lexer = Lexer::new(code.to_string());
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program()?;

    let node = program.into();
    {
        let checker = type_checker::Checker::new();
        checker.check(&node)?;
    }

    let node = {
        let normalizer = normalizer::Normalizer::new();
        normalizer.normalize(node)?
    };

    let main_fn = codegen.gen(&node, false, codegen::Output::CStringPtr)?;

    let result_string = {
        let c_string_ptr = unsafe { main_fn.call() };
        unsafe { CString::from_raw(c_string_ptr) }
            .into_string()
            .unwrap()
    };

    Ok(result_string)
}

fn start_llvm() {
    let stdin = io::stdin();
    let stdout = io::stdout().into_raw_mode().unwrap();

    let mut term = terminal::Terminal::new(stdout);
    term.init();
    term.write(PROMPT);

    let mut inputs = "".to_string();
    let mut line = "".to_string();

    let mut history = History::new();
    let mut current_history = history.clone();
    current_history.push("".to_string());

    current_history.push("".to_string());

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

                    history.push(line.clone());
                    current_history = history.clone();

                    line.push(';');

                    let try_inputs = {
                        let mut inputs = inputs.clone();
                        inputs.push('\n');
                        inputs.push_str(&line);
                        inputs
                    };

                    line = "".to_string();
                    current_history.push(line.clone());

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

                    let current = current_history.get_curernt_mut();
                    current.insert(idx, c);

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
            Key::Right => {
                let right_limit = u16::try_from(PROMPT.len() + line.len()).unwrap();
                if term.cursor_pos().col > right_limit {
                    continue;
                }

                term.move_cursor(terminal::MoveCursorAction::Right(1));
            }
            Key::Up => {
                if current_history.back().is_ok() {
                    line = current_history.get_current().to_string();
                }

                term.clear_current_line();
                term.write(format!("{}{}", PROMPT, line.as_str()).as_str());
            }
            Key::Down => {
                if current_history.next().is_ok() {
                    line = current_history.get_current().to_string();
                }

                term.clear_current_line();
                term.write(format!("{}{}", PROMPT, line.as_str()).as_str());
            }
            Key::Backspace => {
                let left_limit = u16::try_from(PROMPT.len()).unwrap() + 1;
                if term.cursor_pos().col <= left_limit {
                    continue;
                }

                let pos = term.cursor_pos();
                let idx = usize::from(pos.col) - PROMPT.len() - 2;
                line.remove(idx);

                let current = current_history.get_curernt_mut();
                current.remove(idx);

                term.clear_current_line();
                term.write(format!("{}{}", PROMPT, line.as_str()).as_str());

                term.move_cursor(terminal::MoveCursorAction::Pos(pos));
                term.move_cursor(terminal::MoveCursorAction::Left(1));
            }
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

        let node = {
            let normalizer = normalizer::Normalizer::new();
            match normalizer.normalize(expanded) {
                Ok(node) => node,
                Err(e) => {
                    println!("Normalization error: {}", e);
                    continue;
                }
            }
        };

        {
            let checker = type_checker::Checker::new();
            match checker.check(&node) {
                Ok(_) => (),
                Err(e) => {
                    println!("Type error: {}", e);
                    continue;
                }
            }
        }

        let evaluated = eval_node(&node, Rc::clone(&env));
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
