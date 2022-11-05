use serde::{ser::SerializeStruct, Deserialize, Serialize};

#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum Message {
    Request(Request),
    Response(Response),
    Notification(Notification),
}

impl Serialize for Message {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        const JSONRPC_VERSION: &str = "2.0";

        match self {
            Self::Request(request) => {
                let mut s = serializer.serialize_struct("Request", 4)?;
                s.serialize_field("jsonrpc", JSONRPC_VERSION)?;
                s.serialize_field("id", &request.id)?;
                s.serialize_field("method", &request.method)?;
                s.serialize_field("params", &request.params)?;
                s.end()
            }

            Self::Response(response) => match response {
                Response::Success { id, result } => {
                    let mut s = serializer.serialize_struct("Response::Success", 3)?;
                    s.serialize_field("jsonrpc", JSONRPC_VERSION)?;
                    s.serialize_field("id", id)?;
                    s.serialize_field("result", result)?;
                    s.end()
                }
                Response::Error { id, error } => {
                    let mut s = serializer.serialize_struct("Response::Error", 3)?;
                    s.serialize_field("jsonrpc", JSONRPC_VERSION)?;
                    s.serialize_field("id", id)?;
                    s.serialize_field("error", error)?;
                    s.end()
                }
            },

            Self::Notification(notification) => {
                let mut s = serializer.serialize_struct("Notification", 3)?;
                s.serialize_field("jsonrpc", JSONRPC_VERSION)?;
                s.serialize_field("method", &notification.method)?;
                s.serialize_field("params", &notification.params)?;
                s.end()
            }
        }
    }
}

#[derive(Debug, Deserialize)]
pub struct Request {
    pub id: RequestId,
    pub method: String,
    pub params: serde_json::Value,
}

#[derive(Debug, Deserialize)]
pub enum Response {
    Success { id: RequestId, result: String },
    Error { id: RequestId, error: ResponseError },
}

/// see: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#responseMessage
#[derive(Debug, Serialize, Deserialize)]
pub struct ResponseError {
    code: i32,
    message: String,
    data: Option<serde_json::Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum RequestId {
    Integer(i32),
    String(String),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Notification {
    pub method: String,
    pub params: serde_json::Value,
}

#[derive(Debug, Serialize, Deserialize)]
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
