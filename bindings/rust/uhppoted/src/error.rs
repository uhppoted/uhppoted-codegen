use std::fmt;
use std::sync::PoisonError;
use async_std::future::TimeoutError;

#[derive(Debug)]
pub struct Error {
    _kind: ErrorKind,
    message: String,
}

#[derive(Clone, Copy, Debug)]
pub enum ErrorKind {
    _NoResponse,
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

impl From<&str> for Error {
    fn from(message: &str) -> Error {
        Error {
            _kind: ErrorKind::Other,
            message: format!("{}", message),
        }
    }
}

impl From<TimeoutError> for Error {
    fn from(_: TimeoutError) -> Error {
        Error {
            _kind: ErrorKind::Timeout,
            message: format!("{}", ErrorKind::Timeout),
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

impl From<std::array::TryFromSliceError> for Error {
    fn from(err: std::array::TryFromSliceError) -> Error {
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
            _NoResponse => "no response to request",
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
