use std::collections::{HashMap, HashSet};
use std::convert::identity;

use crate::desmos::evaluate::EvalExpr;
pub use get_deps::CanDepend;

mod get_deps;

pub fn reorder_inplace<T>(data: &mut [T], mut indices: Vec<usize>) {
    for idx in 0..data.len() {
        if indices[idx] != idx {
            let mut current_idx = idx;
            loop {
                let target_idx = indices[current_idx];
                indices[current_idx] = current_idx;
                if indices[target_idx] == target_idx {
                    break;
                }
                data.swap(current_idx, target_idx);
                current_idx = target_idx;
            }
        }
    }
}

pub fn topological_indices<T: CanDepend>(
    var_defs: &[(impl Into<usize> + Copy, T)],
    mut dep_preproc: impl FnMut(Vec<usize>) -> Vec<usize>,
) -> Vec<usize> {
    topological_sort(
        var_defs
            .iter()
            .map(|(ident, expr)| ((*ident).into(), dep_preproc(expr.get_deps())))
            .collect::<Vec<_>>(),
    )
}

pub fn topological_sort(mut nodes: Vec<(usize, Vec<usize>)>) -> Vec<usize> {
    let mut indices = Vec::with_capacity(nodes.len());

    let mut remaining = vec![true; nodes.len()];

    while remaining.iter().copied().any(identity) {
        let Some(index) = nodes
            .iter()
            .zip(remaining.iter())
            .position(|((_, deps), &rem)| rem && deps.is_empty())
        else {
            let ns: Vec<_> = nodes
                .iter()
                .zip(remaining)
                .filter_map(|(v, rem)| rem.then_some(v.0))
                .collect();
            println!("{ns:?}");
            let pairs: Vec<_> = nodes
                .iter()
                .flat_map(|(node, deps)| deps.iter().map(|&d| (*node, d)))
                .collect();
            println!("{pairs:?}");

            unreachable!("Cannot be sorted at {nodes:?}");
        };

        let (node, _) = nodes[index];
        remaining[index] = false;
        indices.push(index);

        //TODO: easy optimization if bijection information added
        nodes.iter_mut().for_each(|(_, deps)| {
            while let Some(pos) = deps.iter().position(|&v| v == node) {
                deps.swap_remove(pos);
            }
        });
    }

    indices
}
