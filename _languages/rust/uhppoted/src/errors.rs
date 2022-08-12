use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub struct Oops;

impl Error for Oops {}

impl fmt::Display for Oops {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ooops")
    }
}

#[derive(Debug)]
pub struct Timeout;

impl Error for Timeout {}

impl fmt::Display for Timeout {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "timeout waiting for response from controller")
    }
}

#[derive(Debug)]
pub struct NoResponse;

impl Error for NoResponse {}

impl fmt::Display for NoResponse {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "no response to request")
    }
}
