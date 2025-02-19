use std::str::FromStr;

use thompson::from_regexp_iter;

mod determinization;
mod dfs;
mod errors;
mod minimization;
mod thompson;

#[derive(Clone)]
pub struct Regex {
    pub(crate) fa: dfs::FiniteAutomata,
}

impl Regex {
    pub fn new(exp: &str) -> Result<Self, ()> {
        Ok(Regex {
            fa: from_regexp_iter(&mut exp.chars().peekable())
        })
    }

    pub fn captures() {
        // TODO: Implement capturing groups in regular expressions
        unimplemented!()
    }

    pub fn find() {
        // TODO: Implement regular expression matching
        unimplemented!()
    }

    pub fn is_match() {
        // TODO: Implement regular expression matching
        unimplemented!()
    }

    pub fn replace() {
        // TODO: Implement regular expression replacement
        unimplemented!()
    }

    pub fn split() {
        // TODO: Implement regular expression splitting
        unimplemented!()
    }
}

impl FromStr for Regex {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Regex::new(s))
    }
}
