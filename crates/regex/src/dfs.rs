use std::collections::{HashMap, HashSet, VecDeque};

pub type StateID = usize;
#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub enum SymbolType {
    Alpha(u8),
    Eps,
}

pub type TrTableType = HashMap<StateID, HashMap<SymbolType, HashSet<StateID>>>;

#[derive(Clone, Debug)]
pub struct FiniteAutomata {
    pub transition_table: TrTableType,
    pub states: HashSet<StateID>,
    pub start_state: StateID,
    pub final_states: HashSet<StateID>,
    pub alphabet: HashSet<u8>,
}

impl FiniteAutomata {
    pub fn minimal() -> Self {
        let start_stateid = 0;
        let mut transition_table = HashMap::new();
        transition_table.insert(start_stateid, HashMap::new());
        Self {
            transition_table,
            final_states: HashSet::from_iter(vec![start_stateid]),
            start_state: start_stateid,
            states: HashSet::from_iter(vec![start_stateid]),
            alphabet: HashSet::new(),
        }
    }

    pub fn is_eps_reachable(&self, a: StateID, b: StateID) -> Option<bool> {
        let mut visited = Vec::<StateID>::new();
        let mut q = VecDeque::<StateID>::new();

        q.push_back(a);
        while let Some(v) = q.pop_front() {
            visited.push(v);
            let neighs = self.transition_table.get(&v)?.get(&SymbolType::Eps)?;
            if neighs.contains(&b) {
                return Some(true);
            }

            for &n in neighs.iter() {
                if !visited.contains(&n) {
                    q.push_back(n);
                }
            }
        }
        Some(false)
    }

    pub fn add_state(&mut self) -> StateID {
        let new_stateid = self.states.len();
        self.states.insert(new_stateid);
        self.transition_table.insert(new_stateid, HashMap::new());
        new_stateid
    }

    pub fn add_transition(&mut self, from: StateID, symbol: SymbolType, to: StateID) -> Option<()> {
        assert!(self.is_valid(to));
        assert!(self.is_valid(from));

        let from_map = self.transition_table.get_mut(&from)?;
        if let Some(old_vec) = from_map.get_mut(&symbol) {
            old_vec.extend(vec![to]);
        } else {
            from_map.insert(symbol, HashSet::from_iter(vec![to]));
        }
        Some(())
    }

    pub fn set_start_state(&mut self, state: StateID) {
        assert!(self.states.contains(&state));
        self.start_state = state;
    }

    pub fn get_start_state(&self) -> StateID {
        self.start_state
    }

    pub fn set_final_state(&mut self, state: StateID) {
        assert!(self.states.contains(&state));
        assert!(!self.final_states.contains(&state));
        self.final_states = HashSet::from_iter(vec![state]);
    }

    pub fn add_final_state(&mut self, state: StateID) {
        assert!(self.states.contains(&state));
        if !self.final_states.contains(&state) {
            self.final_states.insert(state);
        }
    }

    pub fn remove_final_state(&mut self, state: StateID) {
        assert!(self.final_states.contains(&state));
        self.final_states.remove(&state);
    }

    pub fn get_final_states(&self) -> HashSet<StateID> {
        self.final_states.clone()
    }

    pub fn visit_mut(&mut self, visitor: &dyn VisitorMut<FiniteAutomata>) {
        visitor.visit(self)
    }

    fn is_valid(&self, state_id: StateID) -> bool {
        state_id < self.states.len()
    }
}

pub fn exchange_states(tr_table: TrTableType, old_state: StateID, new_state: StateID) -> TrTableType {
    let mut new_table = tr_table.clone();

    if let Some(old_state_transitions) = tr_table.get(&old_state).clone() {
        new_table.remove(&old_state);
        new_table.insert(new_state, HashMap::new());
        let new_state_transitions = new_table.get_mut(&new_state).expect("No new state transitions");
        for (symbol, to_states) in old_state_transitions {
            new_state_transitions.insert(*symbol, to_states.clone());
        }
    }

    for (_, transitions) in new_table.iter_mut() {
        for (_, to_states) in transitions.iter_mut() {
            if to_states.contains(&old_state) {
                to_states.retain(|to| *to != old_state);
                to_states.insert(new_state);
            }
        }
    }

   new_table
}

pub fn concat_fa(mut fa1: FiniteAutomata, mut fa2: FiniteAutomata) -> FiniteAutomata {
    // Сдвигаем нумерацию во втором КА
    let state_offset = fa1.states.len();
    let rename_f = |st: StateID| st + state_offset;
    let rename_visitor = RenameStatesVisitor::new(&rename_f);
    fa2.visit_mut(&rename_visitor);

    // Добавление eps переходом между конечными состояниями КА1
    // и начальными состояниями КА2
    let fa1_old_final_states = fa1.get_final_states();
    let fa2_start_state = fa2.get_start_state();

    // Расширяем граф КА1 графом КА2
    fa1.states.extend(fa2.states);
    fa1.transition_table.extend(fa2.transition_table);
    fa1.final_states = fa2.final_states;

    for state in fa1_old_final_states.into_iter() {
        fa1.add_transition(state, SymbolType::Eps, fa2_start_state);
    }

    fa1.alphabet = fa1.alphabet.union(&fa2.alphabet).cloned().collect();

    fa1
}

pub fn union_fa(mut fa1: FiniteAutomata, mut fa2: FiniteAutomata) -> FiniteAutomata {
    // Сдвигаем нумерацию во втором КА
    let state_offset = fa1.states.len();
    let rename_f = |st: StateID| st + state_offset;
    let rename_visitor = RenameStatesVisitor::new(&rename_f);
    fa2.visit_mut(&rename_visitor);

    // Расширяем граф КА1 графом КА2
    fa1.states.extend(fa2.states.iter());
    fa1.transition_table.extend(fa2.transition_table.clone());

    // Новое начальное состояния
    let new_start_state = fa1.add_state();
    fa1.add_transition(new_start_state, SymbolType::Eps, fa1.start_state);
    fa1.add_transition(new_start_state, SymbolType::Eps, fa2.start_state);
    fa1.start_state = new_start_state;

    // Новое конечное состояние
    let new_final_state = fa1.add_state();
    for final_state in fa1.get_final_states() {
        fa1.add_transition(final_state, SymbolType::Eps, new_final_state);
    }
    for final_state in fa2.get_final_states() {
        fa1.add_transition(final_state, SymbolType::Eps, new_final_state);
    }
    fa1.final_states = HashSet::from_iter(vec![new_final_state]);

    fa1.alphabet = fa1.alphabet.union(&fa2.alphabet).cloned().collect();

    fa1
}

pub fn trans_closure(mut fa: FiniteAutomata) -> FiniteAutomata {
    let start_state = fa.start_state;

    for final_state in fa.get_final_states() {
        fa.add_transition(final_state, SymbolType::Eps, start_state);
    }

    fa
}

pub fn ref_trans_closure(mut fa: FiniteAutomata) -> FiniteAutomata {
    let start_state = fa.start_state;
    let final_states = fa.get_final_states();
    for &final_state in final_states.iter() {
        fa.add_transition(final_state, SymbolType::Eps, start_state);
    }

    let new_start_state = fa.add_state();
    fa.add_transition(new_start_state, SymbolType::Eps, start_state);
    fa.set_start_state(new_start_state);

    let new_final_state = fa.add_state();
    for &final_state in final_states.iter() {
        fa.add_transition(final_state, SymbolType::Eps, new_final_state);
    }
    fa.add_transition(new_start_state, SymbolType::Eps, new_final_state);
    fa.set_final_state(new_final_state);

    fa
}

pub trait VisitorMut<T> {
    fn visit(&self, t: &mut T);
}

pub struct RenameStatesVisitor<'a> {
    pub rename_f: &'a dyn Fn(StateID) -> StateID,
}

impl<'a> RenameStatesVisitor<'a> {
    pub fn new(rename_f: &'a dyn Fn(StateID) -> StateID) -> Self {
        RenameStatesVisitor { rename_f }
    }
}

impl<'a> VisitorMut<FiniteAutomata> for RenameStatesVisitor<'a> {
    fn visit(&self, fa: &mut FiniteAutomata) {
        fa.states = fa
            .states
            .iter()
            .map(|&state| (self.rename_f)(state))
            .collect::<HashSet<_>>();
        fa.final_states = fa
            .final_states
            .iter()
            .map(|&state| (self.rename_f)(state))
            .collect::<HashSet<_>>();
        fa.start_state = (self.rename_f)(fa.start_state);

        let mut new_table = HashMap::<StateID, HashMap<SymbolType, HashSet<StateID>>>::new();
        for node in fa.transition_table.iter() {
            let &from = node.0;
            let new_from = (self.rename_f)(from);
            let mut new_hashmap = HashMap::new();
            for tr in node.1.iter() {
                let new_to =
                    tr.1.clone()
                        .into_iter()
                        .map(|st| (self.rename_f)(st))
                        .collect::<HashSet<_>>();
                let &sym = tr.0;
                new_hashmap.insert(sym, new_to);
            }
            new_table.insert(new_from, new_hashmap);
        }
        fa.transition_table = new_table;
    }
}

pub trait DotPrintable {
    fn to_dot_notation(&self) -> String;
}

impl FiniteAutomata {
    fn stringify_state(&self, state: StateID) -> String {
        let mut output_str = String::new();
        output_str.push_str(&state.to_string());
        output_str.push_str(" [ ");
        if self.final_states.contains(&state) {
            output_str.push_str("shape = doublecircle ");
        }
        output_str.push_str(" ] ");
        output_str.push_str(";\n");
        output_str
    }

    fn stringify_transition(&self, from: StateID, sym: SymbolType, to: StateID) -> String {
        let mut output_str = String::new();
        output_str.push_str(&from.to_string());
        output_str.push_str(" -> ");
        output_str.push_str(&to.to_string());
        output_str.push_str("[ label = \"");
        if let SymbolType::Alpha(sym) = sym {
            output_str.push(sym as char);
        } else {
            output_str.push_str("eps");
        };
        output_str.push_str("\" ];\n");
        output_str
    }
}

impl DotPrintable for FiniteAutomata {
    fn to_dot_notation(&self) -> String {
        let mut output_str = String::from("digraph FiniteAutomata {\n");
        let mut state_queue = VecDeque::<StateID>::new();
        let mut processed_vec = HashSet::<StateID>::new();
        let mut processed_tr = HashSet::<(StateID, SymbolType, StateID)>::new();

        for &state in self.states.iter() {
            let state_str = self.stringify_state(state);
            output_str.push_str(&state_str);
        }

        for &state in self.states.iter() {
            state_queue.push_back(state);
            while let Some(from) = state_queue.pop_front() {
                if processed_vec.contains(&from) {
                    continue;
                }
                if let Some(transitions) = self.transition_table.get(&from) {
                    for tr in transitions {
                        let &sym = tr.0;
                        let to_states = tr.1;

                        for &to in to_states {
                            if !processed_tr.contains(&(from, sym, to)) {
                                processed_tr.insert((from, sym, to));
                                let state_str = self.stringify_transition(from, sym, to);
                                output_str.push_str(&state_str);

                                if !processed_vec.contains(&to) {
                                    state_queue.push_back(to);
                                }
                            }
                        }
                    }
                }
                processed_vec.insert(from);
            }
        }
        output_str.push('}');
        output_str
    }
}

#[cfg(test)]
mod dfs_tests {
    use std::collections::HashSet;

    use super::{concat_fa, FiniteAutomata, StateID};
    use crate::dfs::{
        ref_trans_closure, trans_closure, union_fa, DotPrintable, RenameStatesVisitor, SymbolType,
    };

    #[test]
    fn check_add_state() {
        let mut fa = FiniteAutomata::minimal();

        let _ = fa.add_state();

        assert!(fa.states.len() == 2);
    }

    #[test]
    fn check_rename() {
        let mut fa = FiniteAutomata::minimal();
        let _ = fa.add_state();
        let rename_f = &|x: StateID| x + 10;
        let rename_visitor = RenameStatesVisitor::new(rename_f);

        fa.visit_mut(&rename_visitor);

        assert!(fa.states.len() == 2);
        assert!(fa.states == HashSet::from_iter(vec![10, 11]));
    }

    #[test]
    fn check_empty_concat() {
        let fa1 = FiniteAutomata::minimal();
        let mut fa2 = FiniteAutomata::minimal();
        let state21 = fa2.add_state();
        fa2.add_transition(fa1.start_state, SymbolType::Alpha(b'a'), state21);
        fa2.remove_final_state(0);
        fa2.set_final_state(state21);

        let fa = concat_fa(fa1, fa2);

        let dot_nfa = fa.to_dot_notation();
        println!("{}", &dot_nfa);

        assert!(fa.states == HashSet::from_iter(vec![0, 1, 2]));
        assert!(
            fa.transition_table
                .get(&0)
                .unwrap()
                .get(&SymbolType::Eps)
                .unwrap()
                == &HashSet::from_iter(vec![1])
        );
    }

    #[test]
    fn check_concat() {
        let mut fa1 = FiniteAutomata::minimal();
        let state11 = fa1.add_state();
        let state12 = fa1.add_state();
        fa1.add_transition(0, SymbolType::Alpha(b'a'), state11);
        fa1.add_transition(state11, SymbolType::Alpha(b'b'), state12);
        fa1.remove_final_state(0);
        fa1.set_final_state(state12);
        let mut fa2 = FiniteAutomata::minimal();
        let state21 = fa2.add_state();
        let state22 = fa2.add_state();
        fa2.add_transition(0, SymbolType::Alpha(b'a'), state21);
        fa2.add_transition(state21, SymbolType::Alpha(b'b'), state22);
        fa2.remove_final_state(0);
        fa2.set_final_state(state22);

        let fa = concat_fa(fa1, fa2);

        assert!(fa.states == HashSet::from_iter(vec![0, 1, 2, 3, 4, 5]));
        assert!(
            fa.transition_table
                .get(&0)
                .unwrap()
                .get(&SymbolType::Alpha(b'a'))
                .unwrap()
                == &HashSet::from_iter(vec![1])
        );
        assert!(
            fa.transition_table
                .get(&1)
                .unwrap()
                .get(&SymbolType::Alpha(b'b'))
                .unwrap()
                == &HashSet::from_iter(vec![2])
        );
        assert!(
            fa.transition_table
                .get(&2)
                .unwrap()
                .get(&SymbolType::Eps)
                .unwrap()
                == &HashSet::from_iter(vec![3])
        );
        assert!(
            fa.transition_table
                .get(&3)
                .unwrap()
                .get(&SymbolType::Alpha(b'a'))
                .unwrap()
                == &HashSet::from_iter(vec![4])
        );
        assert!(
            fa.transition_table
                .get(&4)
                .unwrap()
                .get(&SymbolType::Alpha(b'b'))
                .unwrap()
                == &HashSet::from_iter(vec![5])
        );
    }

    #[test]
    fn check_union() {
        let mut fa1 = FiniteAutomata::minimal();
        let state11 = fa1.add_state();
        let state12 = fa1.add_state();
        fa1.add_transition(0, SymbolType::Alpha(b'a'), state11);
        fa1.add_transition(state11, SymbolType::Alpha(b'b'), state12);
        fa1.remove_final_state(0);
        fa1.set_final_state(state12);
        let mut fa2 = FiniteAutomata::minimal();
        let state21 = fa2.add_state();
        let state22 = fa2.add_state();
        fa2.add_transition(0, SymbolType::Alpha(b'a'), state21);
        fa2.add_transition(state21, SymbolType::Alpha(b'b'), state22);
        fa2.remove_final_state(0);
        fa2.set_final_state(state22);

        let fa = union_fa(fa1, fa2);

        assert!(fa.states == HashSet::from_iter(vec![0, 1, 2, 3, 4, 5, 6, 7]));
        let start_state = fa.start_state;
        assert!(
            fa.transition_table
                .get(&start_state)
                .unwrap()
                .get(&SymbolType::Eps)
                .unwrap()
                == &HashSet::from_iter(vec![0, 3])
        );
        let &final_state = Vec::from_iter(fa.get_final_states()).get(0).unwrap();
        assert!(
            fa.transition_table
                .get(&5)
                .unwrap()
                .get(&SymbolType::Eps)
                .unwrap()
                == &HashSet::from_iter(vec![final_state])
        );
        assert!(
            fa.transition_table
                .get(&2)
                .unwrap()
                .get(&SymbolType::Eps)
                .unwrap()
                == &HashSet::from_iter(vec![final_state])
        );
    }

    #[test]
    fn check_trans_closure() {
        let mut fa = FiniteAutomata::minimal();
        let state1 = fa.add_state();
        let state2 = fa.add_state();
        fa.add_transition(0, SymbolType::Alpha(b'a'), state1);
        fa.add_transition(state1, SymbolType::Alpha(b'b'), state2);
        fa.remove_final_state(0);
        fa.set_final_state(state2);

        let new_fa = trans_closure(fa);

        assert!(new_fa.states == HashSet::from_iter(vec![0, 1, 2]));
        assert!(
            new_fa
                .transition_table
                .get(&0)
                .unwrap()
                .get(&SymbolType::Alpha(b'a'))
                .unwrap()
                == &HashSet::from_iter(vec![1])
        );
        assert!(
            new_fa
                .transition_table
                .get(&1)
                .unwrap()
                .get(&SymbolType::Alpha(b'b'))
                .unwrap()
                == &HashSet::from_iter(vec![2])
        );
        assert!(
            new_fa
                .transition_table
                .get(&2)
                .unwrap()
                .get(&SymbolType::Eps)
                .unwrap()
                == &HashSet::from_iter(vec![0])
        );
    }

    #[test]
    fn check_ref_trans_closure() {
        let mut fa = FiniteAutomata::minimal();
        let state1 = fa.add_state();
        let state2 = fa.add_state();
        fa.add_transition(0, SymbolType::Alpha(b'a'), state1);
        fa.add_transition(state1, SymbolType::Alpha(b'b'), state2);
        fa.remove_final_state(0);
        fa.set_final_state(state2);

        let new_fa = ref_trans_closure(fa);

        assert!(new_fa.states == HashSet::from_iter(vec![0, 1, 2, 3, 4]));
        assert!(
            new_fa
                .transition_table
                .get(&0)
                .unwrap()
                .get(&SymbolType::Alpha(b'a'))
                .unwrap()
                == &HashSet::from_iter(vec![1])
        );
        assert!(
            new_fa
                .transition_table
                .get(&1)
                .unwrap()
                .get(&SymbolType::Alpha(b'b'))
                .unwrap()
                == &HashSet::from_iter(vec![2])
        );
        let &new_final_state = Vec::from_iter(new_fa.get_final_states()).get(0).unwrap();
        assert!(
            new_fa
                .transition_table
                .get(&2)
                .unwrap()
                .get(&SymbolType::Eps)
                .unwrap()
                == &HashSet::from_iter(vec![0, new_final_state])
        );
        let start_state = new_fa.get_start_state();
        assert!(
            new_fa
                .transition_table
                .get(&start_state)
                .unwrap()
                .get(&SymbolType::Eps)
                .unwrap()
                == &HashSet::from_iter(vec![0, new_final_state])
        );
    }

    #[test]
    fn check_eps_reachable() {
        let mut fa = FiniteAutomata::minimal();
        let state1 = fa.add_state();
        let state2 = fa.add_state();
        fa.add_transition(fa.start_state, SymbolType::Eps, state1);
        fa.add_transition(state1, SymbolType::Eps, state2);

        let is_reachable = fa.is_eps_reachable(fa.start_state, state2);

        assert!(is_reachable == Some(true));
    }

    #[test]
    fn check_eps_not_reachable() {
        let mut fa = FiniteAutomata::minimal();
        let state1 = fa.add_state();
        let state2 = fa.add_state();
        fa.add_transition(fa.start_state, SymbolType::Eps, state1);
        fa.add_transition(state1, SymbolType::Alpha(b'a'), state2);

        let is_reachable = fa.is_eps_reachable(fa.start_state, state2);

        assert!(is_reachable == None);
    }
}