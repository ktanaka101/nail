/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#baseProtocol
use crate::message::Message;
use anyhow::Result;
use std::io::{BufRead, Write};

pub fn read_message(reader: &mut impl BufRead) -> Result<Message> {
    let header = read_header(reader)?;
    read_content(reader, &header)
}

pub fn write_message(writer: &mut impl Write, message: &Message) -> Result<()> {
    let serialized = serde_json::to_string(message).unwrap();

    writer.write_all(
        format!("Content-Length: {}\r\n\r\n{}", serialized.len(), serialized).as_bytes(),
    )?;
    writer.flush()?;

    Ok(())
}

fn read_content(reader: &mut impl BufRead, header: &Header) -> Result<Message> {
    let mut buf = vec![0; header.content_length.try_into()?];
    let slice = buf.as_mut_slice();
    reader.read_exact(slice)?;

    let message = serde_json::from_slice(slice)?;

    Ok(message)
}

/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#headerPart
struct Header {
    content_length: i32,
}

fn read_header(reader: &mut impl BufRead) -> Result<Header> {
    let mut content_length = None;

    {
        let mut seen_content_type = false;
        let mut buf = String::new();
        loop {
            buf.clear();
            reader.read_line(&mut buf)?;
            if buf == "\r\n" {
                break;
            }

            let field = read_header_field(&buf)?;
            match field.name {
                "Content-Length" => {
                    if content_length.is_some() {
                        anyhow::bail!("duplicate Content-Length header");
                    }

                    content_length = Some(field.value.parse()?);
                }

                "Conetnt-Type" => {
                    if seen_content_type {
                        anyhow::bail!("duplicate Content-Type header");
                    }

                    match field.name {
                        "application/vscode-jsonrpc; charset=utf-8"
                        | "application/vscode-jsonrpc; charset=utf8" => (),
                        unknown => anyhow::bail!("unknown Content-Type: {}", unknown),
                    }

                    seen_content_type = true;
                }

                _ => anyhow::bail!("unknown header field: {}", field.name),
            }
        }
    }

    if let Some(content_length) = content_length {
        Ok(Header { content_length })
    } else {
        anyhow::bail!("Content-Length header is missing");
    }
}

struct HeaderField<'a> {
    name: &'a str,
    value: &'a str,
}

fn read_header_field(buf: &str) -> Result<HeaderField> {
    let field = match buf.strip_suffix("\r\n") {
        Some(field) => field,
        None => anyhow::bail!("invalid header. missing CRLF"),
    };

    match field.split_once(": ") {
        Some((name, value)) => Ok(HeaderField { name, value }),
        None => anyhow::bail!("invalid header. missing colon"),
    }
}

#[cfg(test)]
mod tests {
    use crate::message::{ErrorCode, Notification, Request, RequestId, Response, ResponseError};

    use super::*;

    fn check_read(actual: &str, expected_message: Message) {
        let message = read_message(&mut actual.as_bytes()).unwrap();
        assert_eq!(message, expected_message);
    }

    fn check_write(actual: Message, expected_content: &str) {
        let expected = {
            let expected_content = expected_content.replace([' ', '\n'], "");
            let expected_content_length = expected_content.bytes().len();

            format!(
                "Content-Length: {}\r\n\r\n{}",
                expected_content_length, expected_content
            )
        };

        let mut buf = vec![];
        write_message(&mut buf, &actual).unwrap();

        assert_eq!(String::from_utf8(buf), Ok(expected));
    }

    #[test]
    fn test_read_request() {
        let content = r#"{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "textDocument/didOpen",
    "params": {
        "key1": "value1"
    }
}"#;
        let input = format!("Content-Length: 123\r\n\r\n{}", content);

        check_read(
            input.as_str(),
            Message::Request(Request {
                id: RequestId::Integer(1),
                method: "textDocument/didOpen".to_string(),
                params: serde_json::json!({
                    "key1": "value1",
                }),
            }),
        );
    }

    #[test]
    fn test_read_success_response() {
        let content = r#"{
    "jsonrpc": "2.0",
    "id": 2,
    "result": 10
}"#;
        let input = format!("Content-Length: 55\r\n\r\n{}", content);

        check_read(
            input.as_str(),
            Message::Response(Response::Success {
                id: RequestId::Integer(2),
                result: serde_json::json!(10),
            }),
        );
    }

    #[test]
    fn test_read_error_response() {
        let content = r#"{
    "jsonrpc": "2.0",
    "id": 3,
    "error": {
        "code": -32601,
        "message": "Method not found"
    }
}"#;
        let input = format!("Content-Length: 121\r\n\r\n{}", content);

        check_read(
            input.as_str(),
            Message::Response(Response::Error {
                id: RequestId::Integer(3),
                error: ResponseError {
                    code: ErrorCode::MethodNotFound,
                    message: "Method not found".to_string(),
                    data: None,
                },
            }),
        );
    }

    #[test]
    fn test_read_notification() {
        let content = r#"{
    "jsonrpc": "2.0",
    "method": "Method A",
    "params": [1, 2]
}"#;
        let input = format!("Content-Length: 72\r\n\r\n{}", content);

        check_read(
            input.as_str(),
            Message::Notification(Notification {
                method: "Method A".to_string(),
                params: serde_json::json!([1, 2]),
            }),
        );
    }

    #[test]
    fn test_write_request() {
        check_write(
            Message::Request(Request {
                id: RequestId::Integer(1),
                method: "textDocument/didOpen".to_string(),
                params: serde_json::json!({
                    "key1": "value1",
                    "key2": "value2"
                }),
            }),
            r#"{
                "jsonrpc": "2.0",
                "id": 1,
                "method": "textDocument/didOpen",
                "params": {
                    "key1": "value1",
                    "key2": "value2"
                }
            }"#,
        );
    }

    #[test]
    fn test_write_success_response() {
        check_write(
            Message::Response(Response::Success {
                id: RequestId::Integer(2),
                result: serde_json::json!(10),
            }),
            r#"{
                "jsonrpc": "2.0",
                "id": 2,
                "result": 10
            }"#,
        );
    }

    #[test]
    fn test_write_error_response() {
        check_write(
            Message::Response(Response::Error {
                id: RequestId::Integer(3),
                error: ResponseError {
                    code: ErrorCode::MethodNotFound,
                    message: "MethodNotFound".to_string(),
                    data: None,
                },
            }),
            r#"{
                "jsonrpc": "2.0",
                "id": 3,
                "error": {
                    "code": -32601,
                    "message": "MethodNotFound"
                }
            }"#,
        );
    }

    #[test]
    fn test_write_notification() {
        check_write(
            Message::Notification(Notification {
                method: "MethodA".to_string(),
                params: serde_json::json!([1, 2]),
            }),
            r#"{
                "jsonrpc": "2.0",
                "method": "MethodA",
                "params": [1,2]
            }"#,
        );
    }
}
