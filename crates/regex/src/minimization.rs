use std::collections::{HashMap, HashSet, VecDeque};

use crate::dfs::{exchange_states, FiniteAutomata, StateID, SymbolType, TrTableType};

pub fn get_reverse_transitions(fa: &FiniteAutomata) -> TrTableType {
    let old_tr_table = &fa.transition_table;
    let mut new_tr_table = TrTableType::new();

    for (from_state, from_state_map) in old_tr_table.iter() {
        for (sym, to_states) in from_state_map.iter() {
            for to_state in to_states.iter() {
                if let Some(map) = new_tr_table.get_mut(&to_state) {
                    if let Some(set) = map.get_mut(&sym) {
                        set.insert(from_state.clone());
                    } else {
                        let mut new_set = HashSet::new();
                        new_set.insert(from_state.clone());
                        map.insert(sym.clone(), new_set);
                    }
                } else {
                    let mut new_map = HashMap::new();
                    new_map.insert(
                        sym.clone(),
                        HashSet::from_iter(vec![from_state.clone()].into_iter()),
                    );
                    new_tr_table.insert(to_state.clone(), new_map);
                }
            }
        }
    }

    return new_tr_table;
}

fn build_table(fa: &FiniteAutomata, rev_tr_table: &TrTableType) -> Vec<Vec<bool>> {
    let mut result: Vec<Vec<bool>> = vec![];
    let states_cnt = fa.states.len();
    for _ in 0..states_cnt {
        let mut v = Vec::with_capacity(states_cnt);
        for _ in 0..states_cnt {
            v.push(false);
        }
        result.push(v);
    }

    let mut queue = VecDeque::<(&StateID, &StateID)>::new();
    for state_i in fa.states.iter() {
        for state_j in fa.states.iter() {
            if result[*state_i][*state_j] == false
                && (fa.final_states.contains(&state_i) != fa.final_states.contains(&state_j))
            {
                result[*state_i][*state_j] = true;
                result[*state_j][*state_i] = true;
                queue.push_back((&state_i, &state_j));
            }
        }
    }

    while let Some((u, v)) = queue.pop_front() {
        for &c in fa.alphabet.iter() {
            if let Some(u_map) = rev_tr_table.get(u) {
                if let Some(to_states_u) = u_map.get(&SymbolType::Alpha(c)) {
                    for state_u in to_states_u.iter() {
                        if let Some(v_map) = rev_tr_table.get(v) {
                            if let Some(to_states_v) = v_map.get(&SymbolType::Alpha(c)) {
                                for state_v in to_states_v.iter() {
                                    if result[*state_u][*state_v] == false {
                                        result[*state_u][*state_v] = true;
                                        result[*state_v][*state_u] = true;
                                        queue.push_back((&state_u, &state_v));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    return result;
}

fn get_components(table: &Vec<Vec<bool>>) -> Vec<i32> {
    let states_cnt = table.len();
    let mut components_cnt = -1;
    let mut components = vec![0; states_cnt];
    for i in 0..states_cnt {
        components[i] = -1;
    }
    for i in 0..states_cnt {
        if components[i] == -1 {
            components_cnt += 1;
            components[i] = components_cnt;
            for j in 0..states_cnt {
                if table[i][j] == false && components[j] == -1 {
                    components[j] = components_cnt;
                }
            }
        }
    }
    return components;
}

fn build_new_tr_table(
    mut tr_table: TrTableType,
    components: &Vec<i32>,
    states: &HashSet<usize>,
) -> TrTableType {
    for state in states.iter() {
        let eq_states: Vec<usize> = components
            .iter()
            .enumerate()
            .filter(|(_, new_state)| **new_state == *state as i32)
            .map(|(old_state, _)| old_state)
            .collect();

        for eq_state in eq_states {
            tr_table = exchange_states(tr_table, eq_state, *state + 100);
        }
    }
    tr_table
}

pub fn minimize(fa: FiniteAutomata) -> FiniteAutomata {
    let rev_tr_table = get_reverse_transitions(&fa);
    let table = build_table(&fa, &rev_tr_table);
    let components = get_components(&table);
    let mut new_fa = FiniteAutomata::minimal();

    let mut new_states = HashSet::new();
    for &state in components.iter() {
        new_states.insert(state as usize);
    }
    new_fa.states = new_states;
    new_fa.start_state = (components[fa.start_state] as usize).clone();
    for state in new_fa.get_final_states().iter() {
        new_fa.remove_final_state(*state);
    }
    for state in 0..components.len() {
        if fa.final_states.contains(&state) {
            new_fa.add_final_state((components[state] as usize).clone());
        }
    }
    new_fa.alphabet = fa.alphabet.clone();
    new_fa.transition_table = build_new_tr_table(fa.transition_table, &components, &new_fa.states);

    new_fa.final_states = new_fa.final_states.into_iter().map(|x| x + 100).collect();
    new_fa.start_state = new_fa.start_state + 100;
    new_fa.states = new_fa.states.into_iter().map(|x| x + 100).collect();

    return new_fa;
}

#[cfg(test)]
mod minimization_tests {
    use std::collections::HashSet;

    use crate::dfs::DotPrintable;
    use crate::dfs::FiniteAutomata;
    use crate::dfs::SymbolType;
    use crate::minimization::build_table;
    use crate::minimization::get_reverse_transitions;
    use crate::minimization::minimize;

    use super::get_components;

    fn build_test_fa() -> FiniteAutomata {
        let mut fa = FiniteAutomata::minimal();
        let s0 = fa.get_start_state();
        let s1 = fa.add_state();
        let s2 = fa.add_state();
        let s3 = fa.add_state();
        let s4 = fa.add_state();
        let s5 = fa.add_state();
        let s6 = fa.add_state();
        let s7 = fa.add_state();
        fa.add_transition(s0, SymbolType::Alpha(b'1'), s1);
        fa.add_transition(s0, SymbolType::Alpha(b'0'), s7);
        fa.add_transition(s1, SymbolType::Alpha(b'1'), s0);
        fa.add_transition(s1, SymbolType::Alpha(b'0'), s7);
        fa.add_transition(s7, SymbolType::Alpha(b'0'), s2);
        fa.add_transition(s7, SymbolType::Alpha(b'1'), s2);
        fa.add_transition(s2, SymbolType::Alpha(b'0'), s4);
        fa.add_transition(s2, SymbolType::Alpha(b'1'), s5);
        fa.add_transition(s3, SymbolType::Alpha(b'0'), s4);
        fa.add_transition(s3, SymbolType::Alpha(b'1'), s5);
        fa.add_transition(s4, SymbolType::Alpha(b'0'), s5);
        fa.add_transition(s4, SymbolType::Alpha(b'1'), s6);
        fa.add_transition(s5, SymbolType::Alpha(b'0'), s5);
        fa.add_transition(s5, SymbolType::Alpha(b'1'), s5);
        fa.add_transition(s6, SymbolType::Alpha(b'0'), s6);
        fa.add_transition(s6, SymbolType::Alpha(b'1'), s5);
        fa.set_final_state(s5);
        fa.add_final_state(s6);
        fa.alphabet = HashSet::from_iter(vec![b'0', b'1']);
        fa
    }

    #[test]
    fn check_add_state() {
        let mut fa = FiniteAutomata::minimal();
        let s0 = fa.get_start_state();
        let s1 = fa.add_state();
        let s2 = fa.add_state();
        let s3 = fa.add_state();
        fa.add_transition(s0, SymbolType::Alpha(b'a'), s1);
        fa.add_transition(s0, SymbolType::Alpha(b'b'), s2);
        fa.add_transition(s1, SymbolType::Alpha(b'b'), s3);
        fa.add_transition(s2, SymbolType::Alpha(b'a'), s3);

        let rev_tr_table = get_reverse_transitions(&fa);

        assert_eq!(
            rev_tr_table
                .get(&s1)
                .unwrap()
                .get(&SymbolType::Alpha(b'a'))
                .unwrap(),
            &HashSet::from_iter(vec![s0.clone()].into_iter())
        );
        assert_eq!(
            rev_tr_table
                .get(&s2)
                .unwrap()
                .get(&SymbolType::Alpha(b'b'))
                .unwrap(),
            &HashSet::from_iter(vec![s0.clone()].into_iter())
        );
        assert_eq!(
            rev_tr_table
                .get(&s3)
                .unwrap()
                .get(&SymbolType::Alpha(b'a'))
                .unwrap(),
            &HashSet::from_iter(vec![s2.clone()].into_iter())
        );
        assert_eq!(
            rev_tr_table
                .get(&s3)
                .unwrap()
                .get(&SymbolType::Alpha(b'b'))
                .unwrap(),
            &HashSet::from_iter(vec![s1.clone()].into_iter())
        );
    }

    #[test]
    fn check_build_table() {
        let fa = build_test_fa();

        let rev_transition_table = get_reverse_transitions(&fa);
        let table = build_table(&fa, &rev_transition_table);

        println!("{:?}", rev_transition_table);
        println!("{:?}", table);
    }

    #[test]
    fn check_get_components() {
        let table = vec![
            vec![false, false, true, true],
            vec![false, false, true, true],
            vec![true, true, false, false],
            vec![true, true, false, false],
        ];

        let components = get_components(&table);

        assert_eq!(components, vec![0, 0, 1, 1]);
    }

    #[test]
    fn check_minimization() {
        let fa = build_test_fa();

        let minimized_fa = minimize(fa);

        println!("{}", minimized_fa.to_dot_notation());
    }
}
