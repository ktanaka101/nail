mod terminal;

use std::{convert::TryFrom, ffi::CString, io, os::raw::c_char};

use anyhow::Result;
use ast::AstNode;
use inkwell::{context::Context, OptimizationLevel};
use termion::{event::Key, input::TermRead, raw::IntoRawMode};

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
    Llvm,
    Dev,
}

pub fn start(executer: Executer) {
    start_terminal(executer);
}

fn dev_run(code: &str) -> Result<String> {
    let mut message = "".to_string();

    let parse = parser::parse(code);
    message.push_str(parse.debug_tree().as_str());

    let syntax = parse.syntax();

    for error in ast::validation::validate(&syntax) {
        message.push_str(format!("\n{error}").as_str());
    }

    let source_file = ast::SourceFile::cast(syntax).unwrap();
    let lower_result = hir::lower(source_file);
    message.push_str(format!("\n{lower_result:?}").as_str());

    Ok(message)
}

fn llvm_run(code: &str) -> Result<String> {
    let code_with_entry_point = format!(
        r#"
            fn main() -> int {{
                {code}
            }}
        "#
    );

    let context = Context::create();
    let module = context.create_module("top");
    let builder = context.create_builder();
    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    let parse = parser::parse(&code_with_entry_point);
    let syntax = parse.syntax();
    let validation_errors = ast::validation::validate(&syntax);
    if !validation_errors.is_empty() {
        anyhow::bail!(
            "\n{}",
            validation_errors
                .iter()
                .map(|err| err.to_string())
                .collect::<Vec<_>>()
                .join("\n")
        );
    }

    let source_file = ast::SourceFile::cast(syntax).unwrap();
    let hir_lower_result = hir::lower(source_file);
    if !hir_lower_result.errors.is_empty() {
        anyhow::bail!(
            "\n{}",
            hir_lower_result
                .errors
                .iter()
                .map(|err| match err {
                    hir::LowerError::UndefinedEntryPoint =>
                        "error: undefined entry point.".to_string(),
                })
                .collect::<Vec<_>>()
                .join("\n")
        );
    }
    let hir_ty_lower_result = hir_ty::lower(&hir_lower_result);
    let type_check_errors = &hir_ty_lower_result.type_check_result.errors;
    if !type_check_errors.is_empty() {
        anyhow::bail!(
            "\n{}",
            type_check_errors
                .iter()
                .map(|err| format!("{err:?}"))
                .collect::<Vec<_>>()
                .join("\n")
        );
    }

    let mir_lower_result = mir::lower(&hir_lower_result, &hir_ty_lower_result);
    let codegen_result = codegen_llvm::codegen(
        &hir_lower_result,
        &mir_lower_result,
        &context,
        &module,
        &builder,
        &execution_engine,
        true,
    );

    let result_string = {
        let c_string_ptr = unsafe { codegen_result.function.call() };
        unsafe { CString::from_raw(c_string_ptr as *mut c_char) }
            .into_string()
            .unwrap()
    };

    Ok(result_string)
}

fn start_terminal(executer: Executer) {
    let run = match executer {
        Executer::Llvm => llvm_run,
        Executer::Dev => dev_run,
    };

    let stdin = io::stdin();
    let stdout = io::stdout().into_raw_mode().unwrap();

    let mut term = terminal::Terminal::new(stdout).expect("Failed to create a terminal.");
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

                    match run(try_inputs.as_str()) {
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
            Key::Alt(c) => println!("Alt-{c}"),
            Key::Ctrl(c) => println!("Ctrl-{c}"),
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
