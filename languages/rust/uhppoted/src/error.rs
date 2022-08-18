use std::fmt;
use std::sync::PoisonError;

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
}

#[derive(Clone, Copy, Debug)]
pub enum ErrorKind {
    NoResponse,
    Timeout,
    IO,
    Oops,
}

impl std::error::Error for Error {}

impl Error {
    pub fn new(kind: ErrorKind) -> Error {
        Error { kind: kind }
    }

    pub fn kind(&self) -> ErrorKind {
        self.kind
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.kind())
    }
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Error {
        Error {
            kind: ErrorKind::IO,
        }
    }
}

impl From<PoisonError<std::sync::RwLockReadGuard<'_, String>>> for Error {
    fn from(err: PoisonError<std::sync::RwLockReadGuard<'_, String>>) -> Error {
        Error {
            kind: ErrorKind::Oops,
        }
    }
}

impl ErrorKind {
    pub(crate) fn as_str(&self) -> &'static str {
        use ErrorKind::*;
        match *self {
            NoResponse => "no response to request",
            Timeout => "timeout waiting for response from controller",
            IO => "I/O error",
            Oops => "oops",
        }
    }
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.write_str(self.as_str())
    }
}
