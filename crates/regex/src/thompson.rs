use std::{iter::Peekable, str::Chars};

use crate::dfs::SymbolType;
use crate::dfs::{concat_fa, ref_trans_closure, trans_closure, union_fa, FiniteAutomata};

fn from_sym(ch: char) -> FiniteAutomata {
    let mut fa = FiniteAutomata::minimal();
    let new_state = fa.add_state();
    fa.add_transition(fa.get_start_state(), SymbolType::Alpha(ch as u8), new_state);
    fa.set_final_state(new_state);
    fa
}

pub fn from_regexp_iter(input: &mut Peekable<Chars<'_>>) -> FiniteAutomata {
    let mut fa = FiniteAutomata::minimal();
    while let Some(ch) = input.next() {
        if char::is_alphabetic(ch) {
            fa.alphabet.insert(ch as u8);
        }
        match ch {
            '(' => {
                let mut tmp = from_regexp_iter(input);
                if let Some('*') = input.peek() {
                    input.next();
                    tmp = ref_trans_closure(tmp);
                } else if let Some('+') = input.peek() {
                    input.next();
                    tmp = trans_closure(tmp);
                } else if let Some('|') = input.peek() {
                    input.next();
                    let inner_fa = from_regexp_iter(input);
                    tmp = union_fa(tmp, inner_fa);
                } else if let Some(')') = input.peek() {
                    input.next();
                    fa = concat_fa(fa, tmp);
                    break;
                } else if input.peek().is_some() {
                    let inner_fa = from_regexp_iter(input);
                    tmp = concat_fa(tmp, inner_fa);
                }
                fa = concat_fa(fa, tmp);
            }
            ')' => {
                break;
            }
            '|' | '*' | '+' => {}
            ch => {
                let mut tmp = from_sym(ch);
                if let Some('*') = input.peek() {
                    tmp = ref_trans_closure(tmp);
                    fa = concat_fa(fa, tmp);
                } else if let Some('+') = input.peek() {
                    tmp = trans_closure(tmp);
                    fa = concat_fa(fa, tmp);
                } else if let Some('|') = input.peek() {
                    fa = concat_fa(fa, tmp);
                    let inner_fa = from_regexp_iter(input);
                    fa = union_fa(fa, inner_fa);
                    break;
                } else {
                    fa = concat_fa(fa, tmp);
                }
            }
        };
    }
    fa
}

#[cfg(test)]
mod thompson_compiler_tests {
    use crate::dfs::DotPrintable;

    use super::from_regexp_iter;

    #[test]
    fn check_empty() {
        let regexp = "";

        let result_fa = from_regexp_iter(&mut regexp.chars().peekable());

        assert!(result_fa.states.len() == 1);
    }

    #[test]
    fn check_concat() {
        let regexp = "(ab|cd|ef)gh(ab|cd|ef)";

        let result_fa = from_regexp_iter(&mut regexp.chars().peekable());

        let dot_nfa = result_fa.to_dot_notation();
        println!("{}", &dot_nfa);
    }

    #[test]
    fn check_union() {
        let regexp = "(ab|cd)*";

        let result_fa = from_regexp_iter(&mut regexp.chars().peekable());

        let dot_nfa = result_fa.to_dot_notation();
        println!("{}", &dot_nfa);
    }

    #[test]
    fn check_trans_closure() {
        let regexp = "a+";

        let result_fa = from_regexp_iter(&mut regexp.chars().peekable());

        let dot_nfa = result_fa.to_dot_notation();
        println!("{}", &dot_nfa);
    }

    #[test]
    fn check_ref_trans_closure() {
        let regexp = "a*";

        let result_fa = from_regexp_iter(&mut regexp.chars().peekable());

        let dot_nfa = result_fa.to_dot_notation();
        println!("{}", &dot_nfa);
    }

    #[test]
    fn check_parens_union() {
        let regexp = "(a|b)";

        let result_fa = from_regexp_iter(&mut regexp.chars().peekable());

        let dot_nfa = result_fa.to_dot_notation();
        println!("{}", &dot_nfa);
    }

    #[test]
    fn check_parens_concat() {
        let regexp = "(ab)";

        let result_fa = from_regexp_iter(&mut regexp.chars().peekable());

        let dot_nfa = result_fa.to_dot_notation();
        println!("{}", &dot_nfa);
    }

    #[test]
    fn check_parens_closure() {
        let regexp = "((((ab)*)|((cd)*)))*cd((ef)*)";

        let result_fa = from_regexp_iter(&mut regexp.chars().peekable());

        let dot_nfa = result_fa.to_dot_notation();
        println!("{}", &dot_nfa);
    }

    #[test]
    fn check_all_in_one() {
        let regexp = "(ab|cd)*cd(ef|gh)+";

        let result_fa = from_regexp_iter(&mut regexp.chars().peekable());

        let dot_nfa = result_fa.to_dot_notation();
        println!("{}", &dot_nfa);
    }
}