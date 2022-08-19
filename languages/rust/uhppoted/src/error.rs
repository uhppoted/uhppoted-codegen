use std::fmt;
use std::sync::PoisonError;

#[derive(Debug)]
pub struct Error {
    _kind: ErrorKind,
    message: String,
}

#[derive(Clone, Copy, Debug)]
pub enum ErrorKind {
    NoResponse,
    Timeout,
    IO,
    Other,
}

impl std::error::Error for Error {}

impl Error {
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Error {
        Error {
            _kind: kind,
            message: format!("{}", kind),
        }
    }
}

impl From<String> for Error {
    fn from(message: String) -> Error {
        Error {
            _kind: ErrorKind::Other,
            message: format!("{}", message),
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Error {
        Error {
            _kind: ErrorKind::IO,
            message: format!("{}", err),
        }
    }
}

impl From<PoisonError<std::sync::RwLockReadGuard<'_, String>>> for Error {
    fn from(err: PoisonError<std::sync::RwLockReadGuard<'_, String>>) -> Error {
        Error {
            _kind: ErrorKind::Other,
            message: format!("{}", err),
        }
    }
}

impl ErrorKind {
    pub(crate) fn as_str(&self) -> &'static str {
        use ErrorKind::*;
        match *self {
            NoResponse => "no response to request",
            Timeout => "timeout waiting for response from controller",
            IO => "I/O",
            Other => "Other",
        }
    }
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.write_str(self.as_str())
    }
}
