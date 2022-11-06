pub mod context;
pub mod message;
pub mod protocol;

use std::io;

use anyhow::Result;
use lsp_types::notification::{self, Notification};
use lsp_types::request::{self, Request};

pub fn server() -> Result<()> {
    let mut stdin = io::stdin().lock();
    let mut stdout = io::stdout().lock();

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
                unimplemented!()
            }
        }
    }
}

fn parse_params<R: notification::Notification>(params: serde_json::Value) -> Result<R::Params> {
    serde_json::from_value(params).map_err(Into::into)
}

fn handle_notification(notification: &message::Notification) -> Result<()> {
    match notification {
        notification if notification.method == notification::DidOpenTextDocument::METHOD => {
            let _params =
                parse_params::<notification::DidOpenTextDocument>(notification.params.clone())?;

            unimplemented!()
        }

        _ => unimplemented!(),
    }
}
