pub const UhppotedError = error{
    NoReply,
    InvalidReply,
    InvalidReplyStartOfMessage,
    InvalidReplyFunctionCode,
    InvalidEventFunctionCode,
};
