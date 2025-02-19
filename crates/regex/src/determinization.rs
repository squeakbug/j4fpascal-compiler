use std::collections::{HashMap, HashSet, VecDeque};

use crate::dfs::{FiniteAutomata, StateID, SymbolType, exchange_states};
use crate::dfs::TrTableType;

pub fn get_new_state(
    fa: &FiniteAutomata,
    from_states: &HashSet<StateID>,
    ch: SymbolType,
) -> HashSet<StateID> {
    let mut new_state_set = HashSet::<StateID>::new();
    for &from_s in from_states {
        let mb_transitions = fa.transition_table.get(&from_s);
        if let Some(transitions) = mb_transitions {
            let mb_to_states = transitions.get(&ch);
            if let Some(to_states) = mb_to_states {
                for state in to_states {
                    new_state_set.insert(*state);
                }
            }
        }
    }
    new_state_set
}

pub fn determ(fa: FiniteAutomata) -> FiniteAutomata {
    let mut state_queue = VecDeque::<(StateID, HashSet<StateID>)>::new();
    let mut proc_vec = Vec::<(StateID, HashSet<StateID>)>::new();
    let mut dfa = FiniteAutomata::minimal();

    let mut init_set = HashSet::<StateID>::new();
    init_set.insert(fa.get_start_state());
    dfa.final_states.clear();
    if fa.final_states.contains(&fa.get_start_state()) {
        dfa.final_states.insert(dfa.get_start_state());
    }
    state_queue.push_back((dfa.get_start_state(), init_set.clone()));
    proc_vec.push((dfa.get_start_state(), init_set));
    while let Some((from, set)) = state_queue.pop_front() {
        for &c in &fa.alphabet {
            let new_state_set = get_new_state(&fa, &set, SymbolType::Alpha(c));
            if let Some(pos) = proc_vec.iter().position(|(_, set)| { *set == new_state_set }) {
                dfa.add_transition(from, SymbolType::Alpha(c), proc_vec[pos].0);
            } else {
                if !new_state_set.is_empty() {
                    let new_state = dfa.add_state();
                    dfa.add_transition(from, SymbolType::Alpha(c), new_state);

                    for &set_state in new_state_set.iter() {
                        if fa.final_states.contains(&set_state) {
                            dfa.final_states.insert(new_state);
                        }
                    }

                    if !proc_vec.contains(&(new_state, new_state_set.clone())) {
                        state_queue.push_back((new_state, new_state_set.clone()));
                        proc_vec.push((new_state, new_state_set));
                    }
                }
            }
        }
    }
    dfa.alphabet = fa.alphabet.clone();
    
    dfa
}

pub fn get_no_eps_reachable_states(fa: &FiniteAutomata) -> HashSet<StateID> {
    let mut visited = Vec::<StateID>::new();
    let mut q = VecDeque::<StateID>::new();
    let mut new_states = fa.states.clone();

    q.push_back(fa.start_state);
    while let Some(from) = q.pop_front() {
        visited.push(from);
        let mb_to_states = fa
            .transition_table
            .get(&from)
            .unwrap()
            .get(&SymbolType::Eps);
        if let Some(to_states) = mb_to_states {
            for to in to_states {
                new_states.remove(to);
            }
        }
        let transitions = fa
            .transition_table
            .get(&from)
            .unwrap();
        for (_, to_states) in transitions {
            for state in to_states {
                if !visited.contains(state) {
                    q.push_back(*state);
                }
            }
        }
    }
    new_states
}

fn in_connected_to_finite_via_eps(table: &TrTableType, final_states: &HashSet<StateID>, visited: HashSet<StateID>, state: StateID) -> Option<bool> {
    let neighs = table.get(&state)?.get(&SymbolType::Eps)?;
    for &n in neighs.iter() {
        if final_states.contains(&n) {
            return Some(true);
        }
        
        let mut new_visited = visited.clone();
        new_visited.insert(state);
        if let Some(true) = in_connected_to_finite_via_eps(table, final_states, new_visited, n) {
            return Some(true);
        }
    }

    Some(false)
}

/// Удаление eps-переходов
///
/// 1) Получение вершин (X), которые не положительно инцидентные какому-либо eps-переходу
/// 2) Для каждой вершины (x) из X:
///     2.1) p = x
///     2.2) Для каждого символа (sym), по которому существует переход из x:
///         2.2.1) Если sym != eps, то для каждой вершины (y), положительно инцидентной переходу по sym
///             добавить переход (p->y) в новую таблицу, если его еще нет
///         2.2.2) Если sym == eps, то для каждой вершины (y), положительно инцидентной переходу по sym:
///             2.2.2.1) x = y
///             2.2.2.2) Перейти на шаг 2.2)
pub fn remove_eps(mut fa: FiniteAutomata) -> FiniteAutomata {
    let no_eps_states = get_no_eps_reachable_states(&fa);
    let mut new_table = HashMap::<StateID, HashMap<SymbolType, HashSet<StateID>>>::new();

    let old_final_states = fa.final_states.clone();
    for &state in fa.states.iter() {
        if let Some(true) = in_connected_to_finite_via_eps(&fa.transition_table, &old_final_states, HashSet::new(), state) {
            fa.final_states.insert(state);
        }
    }

    for parent in no_eps_states.iter() {
        let from_map = fa.transition_table.get(parent).unwrap();
        let mut new_hashmap = HashMap::new();
        add_eps_transition(&mut new_hashmap, &fa, from_map);
        new_table.insert(*parent, new_hashmap);
    }
    fa.transition_table = new_table;
    fa.states = no_eps_states;

    let mut states: Vec<StateID> = fa.states.clone().into_iter().collect();
    states.sort();
    let new_states: Vec<StateID> = (0..states.len()).collect();
    for (&old_state, &new_state) in states.iter().zip(new_states.iter()) {
        fa.transition_table = exchange_states(fa.transition_table, old_state, new_state);
        if fa.final_states.contains(&old_state) {
            fa.final_states.remove(&old_state);
            fa.final_states.insert(new_state);
        }
        if fa.start_state == old_state {
            fa.start_state = new_state;
        }
    }
    fa.states = HashSet::from_iter(new_states.into_iter());
    fa.final_states = fa.final_states.into_iter().filter(|x| fa.states.contains(x)).collect();

    fa
}

pub fn add_eps_transition(
    new_hashmap: &mut HashMap<SymbolType, HashSet<StateID>>,
    fa: &FiniteAutomata,
    from_map: &HashMap<SymbolType, HashSet<StateID>>,
) {
    for tr in from_map {
        let (&sym, to_states) = (tr.0, tr.1);
        match sym {
            SymbolType::Alpha(_) => {
                if let Some(old_vec) = new_hashmap.get_mut(&sym) {
                    old_vec.extend(to_states);
                } else {
                    new_hashmap.insert(sym, to_states.clone());
                }
            }
            SymbolType::Eps => {
                for to in to_states {
                    let new_from_map = fa.transition_table.get(to).unwrap();
                    add_eps_transition(new_hashmap, fa, new_from_map);
                }
            }
        }
    }
}

#[cfg(test)]
mod detern_tests {
    use std::collections::HashSet;

    use crate::dfs::{DotPrintable, FiniteAutomata, SymbolType};

    use super::remove_eps;

    #[test]
    fn remove_eps_default() {
        let mut fa = FiniteAutomata::minimal();
        let state1 = fa.add_state();
        let state2 = fa.add_state();
        fa.add_transition(fa.start_state, SymbolType::Eps, state1);
        fa.add_transition(state1, SymbolType::Alpha(b'a'), state2);
        fa.set_final_state(state2);

        let new_fa = remove_eps(fa);

        assert!(
            new_fa
                .transition_table
                .get(&0)
                .unwrap()
                .get(&SymbolType::Alpha(b'a'))
                .unwrap()
                == &HashSet::from_iter(vec![1])
        )
    }

    #[test]
    fn remove_eps_multiple_eps() {
        let mut fa = FiniteAutomata::minimal();
        let state1 = fa.add_state();
        let state2 = fa.add_state();
        let state3 = fa.add_state();
        fa.add_transition(fa.start_state, SymbolType::Eps, state1);
        fa.add_transition(state1, SymbolType::Eps, state2);
        fa.add_transition(state2, SymbolType::Alpha(b'a'), state3);
        fa.set_final_state(state3);

        let new_fa = remove_eps(fa);

        assert!(
            new_fa
                .transition_table
                .get(&0)
                .unwrap()
                .get(&SymbolType::Alpha(b'a'))
                .unwrap()
                == &HashSet::from_iter(vec![1])
        )
    }

    #[test]
    fn remove_eps_multiple_paths_with_same_symbol() {
        let mut fa = FiniteAutomata::minimal();
        let state1 = fa.add_state();
        let state2 = fa.add_state();
        let state3 = fa.add_state();
        let state4 = fa.add_state();
        fa.add_transition(fa.start_state, SymbolType::Eps, state1);
        fa.add_transition(fa.start_state, SymbolType::Eps, state4);
        fa.add_transition(state1, SymbolType::Eps, state2);
        fa.add_transition(state2, SymbolType::Alpha(b'a'), state3);
        fa.add_transition(state4, SymbolType::Alpha(b'a'), state3);
        fa.set_final_state(state3);

        let dot_nfa = fa.to_dot_notation();
        println!("{}", &dot_nfa);

        let new_fa = remove_eps(fa);

        let dot_nfa = new_fa.to_dot_notation();
        println!("{}", &dot_nfa);

        assert!(true)
    }

    #[test]
    fn remove_eps_chain_symbols() {
        let mut fa = FiniteAutomata::minimal();
        let state1 = fa.add_state();
        let state2 = fa.add_state();
        let state3 = fa.add_state();
        let state4 = fa.add_state();
        let state5 = fa.add_state();
        fa.add_transition(fa.start_state, SymbolType::Alpha(b'a'), state1);
        fa.add_transition(state1, SymbolType::Eps, state2);
        fa.add_transition(state2, SymbolType::Alpha(b'a'), state3);
        fa.add_transition(state3, SymbolType::Eps, state4);
        fa.add_transition(state4, SymbolType::Alpha(b'a'), state5);
        fa.set_final_state(state5);

        let dot_nfa = fa.to_dot_notation();
        println!("{}", &dot_nfa);

        let new_fa = remove_eps(fa);

        let dot_nfa = new_fa.to_dot_notation();
        println!("{}", &dot_nfa);

        assert!(true)
    }
}