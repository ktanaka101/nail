pub mod context;
mod line_index;
pub mod message;
pub mod protocol;

use std::io;

use anyhow::Result;
use lsp_types::notification::{self, Notification};
use lsp_types::request::{self, Request};

pub fn server() -> Result<()> {
    let mut stdin = io::stdin().lock();
    let mut stdout = io::stdout().lock();
    let mut context = context::Context::default();

    loop {
        let message = protocol::read_message(&mut stdin)?;
        match message {
            message::Message::Request(request) => {
                eprintln!("== RECEIVE REQUEST==\n\t{:?}", &request);

                if request.method == request::Shutdown::METHOD {
                    return Ok(());
                }

                let method_not_found = message::Response::Error {
                    id: request.id,
                    error: message::ResponseError {
                        code: message::ErrorCode::MethodNotFound,
                        message: format!("Method not found: {}", request.method),
                        data: None,
                    },
                };

                eprintln!("== SEND RESPONSE==\n\t{:?}", &method_not_found);
                protocol::write_message(
                    &mut stdout,
                    &message::Message::Response(method_not_found),
                )?;
            }

            message::Message::Response(response) => {
                eprintln!("== RECEIVE RESPONSE==\n\t{:?}", &response);
            }

            message::Message::Notification(notification) => {
                eprintln!("== RECEIVE NOTIFICATION==\n\t{:?}", &notification);
                handle_notification(&notification, &mut context, &mut stdout)?;
            }
        }
    }
}

fn parse_params<R: notification::Notification>(params: serde_json::Value) -> Result<R::Params> {
    serde_json::from_value(params).map_err(Into::into)
}

fn handle_notification(
    notification: &message::Notification,
    context: &mut context::Context,
    out: &mut io::StdoutLock,
) -> Result<()> {
    match notification {
        notification if notification.method == notification::DidOpenTextDocument::METHOD => {
            let params =
                parse_params::<notification::DidOpenTextDocument>(notification.params.clone())?;

            context.add_file(params.text_document)?;
            publish_all_diagnostics(context, out)?;

            Ok(())
        }

        notification if notification.method == notification::DidChangeTextDocument::METHOD => {
            let params =
                parse_params::<notification::DidChangeTextDocument>(notification.params.clone())?;

            context.update_file(&params.text_document, &params.content_changes)?;
            publish_all_diagnostics(context, out)?;

            Ok(())
        }

        notification if notification.method == notification::DidCloseTextDocument::METHOD => {
            let params =
                parse_params::<notification::DidCloseTextDocument>(notification.params.clone())?;
            context.remove_file(params.text_document)?;

            Ok(())
        }

        _ => unimplemented!(),
    }
}

fn publish_all_diagnostics(context: &context::Context, out: &mut io::StdoutLock) -> Result<()> {
    for analysis in context.analyses() {
        let diagnostics = analysis.diagnostics();
        let diagnostics = diagnostics
            .iter()
            .map(|diagnostic| {
                lsp_types::Diagnostic::new_simple(
                    diagnostic.range(&analysis.line_index),
                    diagnostic.display(&analysis.line_index),
                )
            })
            .collect::<Vec<_>>();

        let params =
            lsp_types::PublishDiagnosticsParams::new(analysis.uri.clone(), diagnostics, None);
        notify::<notification::PublishDiagnostics>(params, out)?;
    }

    Ok(())
}

fn notify<N: notification::Notification>(
    params: N::Params,
    out: &mut io::StdoutLock,
) -> Result<()> {
    let notification = message::Message::Notification(message::Notification {
        method: N::METHOD.to_string(),
        params: serde_json::to_value(params).unwrap(),
    });
    eprintln!("== SEND NOTIFICATION ==\n\t{:?}", &notification);
    protocol::write_message(out, &notification)
}
