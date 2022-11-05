use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub enum Message {
    Request,
    Response,
    Notification,
}

/// see: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#responseMessage
pub struct ResponseError {
    code: i32,
    message: String,
    data: Option<serde_json::Value>,
}

pub enum ErrorCode {
    ParseError = -32700,
    InvalidRequest = -32600,
    MethodNotFound = -32601,
    InvalidParams = -32602,
    InternalError = -32603,

    // JsonrpcReservedErrorRangeStart = -32099,
    ServerNotInitialized = -32002,
    Unknown = -32001,
    // JsonrpcReservedErrorRangeEnd = -32000,

    // LspReservedErrorRangeStart = -32899,
    RequestFailed = -32803,
    ServerCanceled = -32802,
    ContentModified = -32801,
    RequestCancelled = -32800,
    // LspReservedErrorRangeEnd = -32800,
}
