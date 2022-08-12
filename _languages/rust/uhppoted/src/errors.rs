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
