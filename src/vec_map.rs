#![allow(dead_code)]

use std::marker::PhantomData;
use std::mem::replace;
use std::ops::{Index, IndexMut};

pub trait AsIndex {
    fn as_index(&self) -> usize;
}

#[derive(Debug, Clone)]
pub struct VecMap<K: AsIndex, V> {
    data: Vec<Option<V>>,
    _phantom: PhantomData<K>,
    len: usize,
}

impl<K: AsIndex, V> Default for VecMap<K, V> {
    fn default() -> Self {
        Self {
            data: Vec::new(),
            _phantom: PhantomData,
            len: 0,
        }
    }
}

impl<K: AsIndex, V> VecMap<K, V> {
    fn extend_to_contain(&mut self, index: usize) {
        while self.data.len() <= index {
            self.data.push(None);
        }
    }

    pub fn insert(&mut self, key: &K, value: V) {
        let index = key.as_index();
        self.extend_to_contain(index);
        self.data[index] = Some(value);
    }

    pub fn remove(&mut self, key: &K) -> Option<V> {
        let index = key.as_index();
        (index < self.data.len())
            .then(|| {
                let at = self
                    .data
                    .get_mut(index)
                    .unwrap_or_else(|| unreachable!("Handled by earlier contains_key check"));
                replace(at, None)
            })
            .flatten()
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.data.get(key.as_index()).map(Option::as_ref).flatten()
    }
    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.data
            .get_mut(key.as_index())
            .map(Option::as_mut)
            .flatten()
    }

    pub fn contains_key(&self, key: &K) -> bool {
        let index = key.as_index();
        (index < self.data.len()) && (self.data[index].is_some())
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn iter(&self) -> impl Iterator<Item = (usize, &V)> {
        self.data
            .iter()
            .enumerate()
            .filter_map(|(a, b)| b.as_ref().map(|x| (a, x)))
    }
}

impl<K: AsIndex, V> Index<&K> for VecMap<K, V> {
    type Output = V;

    fn index(&self, index: &K) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl<K: AsIndex, V> IndexMut<&K> for VecMap<K, V> {
    fn index_mut(&mut self, index: &K) -> &mut Self::Output {
        self.get_mut(index).unwrap()
    }
}
