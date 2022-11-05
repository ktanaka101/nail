/// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#baseProtocol
use crate::message::Message;
use anyhow::Result;
use std::io::BufRead;

pub fn read_message(reader: &mut impl BufRead) -> Result<Message> {
    let header = read_header(reader)?;
    read_content(reader, &header)
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

            let field = read_header_field(&mut buf)?;

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

fn read_header_field<'a>(buf: &'a mut str) -> Result<HeaderField<'a>> {
    let field = match buf.strip_suffix("\r\n") {
        Some(field) => field,
        None => anyhow::bail!("invalid header."),
    };

    match field.split_once(": ") {
        Some((name, value)) => Ok(HeaderField { name, value }),
        None => anyhow::bail!("invalid header."),
    }
}
