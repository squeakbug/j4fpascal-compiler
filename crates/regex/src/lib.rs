use std::{borrow::Cow, str::FromStr};

use thompson::from_regexp_iter;

mod determinization;
mod dfs;
mod errors;
mod minimization;
mod thompson;

pub struct Captures {

}

pub trait Replacer {
    fn replace_append(&mut self, caps: Captures, std: &mut String);
    fn no_expansion(&mut self) -> Option<Cow<str>>;
    fn by_ref(&mut self) -> &Self;
}

#[derive(Clone)]
pub struct Regex {
    pub(crate) fa: dfs::FiniteAutomata,
}

pub struct Split {
    
}

pub struct Match {

}

impl Regex {
    pub fn new(exp: &str) -> Result<Self, ()> {
        Ok(Regex {
            fa: from_regexp_iter(&mut exp.chars().peekable())
        })
    }

    pub fn as_str(&self) -> &str {
        // TODO: Implement to string conversion
        unimplemented!()
    }

    pub fn captures(&self, haystack: &str) -> Option<Captures> {
        // TODO: Implement capturing groups in regular expressions
        unimplemented!()
    }

    pub fn find(&self, haystack: &str) -> Option<Match> {
        // TODO: Implement regular expression matching
        unimplemented!()
    }

    pub fn is_match(&self, haystack: &str) -> bool {
        // TODO: Implement regular expression matching

        /*
        let mut current_state = self.start_state;
        for c in input.bytes() {
            let next_state = self.transition_table
                .get(&current_state)
                .and_then(|table| table.get(&SymbolType::Alpha(c)))
                .cloned()
                .unwrap_or_default();
            // println!("char: {}; next_state: {:?}", c, next_state);
            if next_state.is_empty() {
                return false;
            } else {
                current_state = *next_state.iter().next().unwrap();
            }
        }
        self.final_states.contains(&current_state)
        */

        unimplemented!()
    }

    pub fn replace<R: Replacer>(
        &self,
        haystack: &str,
        rep: R
    ) -> Cow<str> {
        // TODO: Implement regular expression replacement
        unimplemented!()
    }

    pub fn split(&self, haystack: &str) -> Split {
        // TODO: Implement regular expression splitting
        unimplemented!()
    }
}

impl FromStr for Regex {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Regex::new(s)
    }
}
