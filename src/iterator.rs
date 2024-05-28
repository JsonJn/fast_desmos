use crate::pooled_vec::PooledVec;
use std::mem::take;
use std::slice;
use std::slice::Windows;

pub struct OptWindowIter<'a, T> {
    pub iter: Option<Windows<'a, T>>,
}

impl<'a, T> Iterator for OptWindowIter<'a, T> {
    type Item = &'a [T];

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.as_mut().and_then(|v| v.next())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        if let Some(x) = &self.iter {
            x.size_hint()
        } else {
            (0, Some(0))
        }
    }
}

impl<'a, T> ExactSizeIterator for OptWindowIter<'a, T> {}

pub enum OptIntoIterRep<T: Clone + 'static> {
    One(T),
    Many { vec: PooledVec<T>, index: usize },
}

pub enum OptIntoIter<T: Clone + 'static> {
    One(Option<T>),
    Many { vec: PooledVec<T>, index: usize },
}

pub enum OptIterRep<'a, T> {
    One(&'a T),
    Many(slice::Iter<'a, T>),
}

pub enum OptIter<'a, T> {
    One(Option<&'a T>),
    Many(slice::Iter<'a, T>),
}

impl<'a, T> Iterator for OptIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            OptIter::One(o) => {
                let x = *o;
                *o = None;
                x
            }
            OptIter::Many(many) => many.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            OptIter::One(_) => (1, Some(1)),
            OptIter::Many(left) => {
                let len = left.len();
                (len, Some(len))
            }
        }
    }
}

impl<'a, T> ExactSizeIterator for OptIter<'a, T> {}

impl<T: Clone> Iterator for OptIntoIterRep<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            OptIntoIterRep::One(x) => Some(x.clone()),
            OptIntoIterRep::Many { vec, index } => {
                let v = vec.get(*index).cloned();
                if v.is_some() {
                    *index += 1;
                }
                v
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::One(_) => (0, None),
            Self::Many { vec, index } => {
                let index = *index;
                let exact = vec.len() - index;
                (exact, Some(exact))
            }
        }
    }
}

impl<T: Clone> Iterator for OptIntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::One(x) => take(x),
            Self::Many { vec, index } => {
                let v = vec.get(*index).cloned();
                if v.is_some() {
                    *index += 1;
                }
                v
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::One(_) => (0, None),
            Self::Many { vec, index } => {
                let index = *index;
                let exact = vec.len() - index;
                (exact, Some(exact))
            }
        }
    }
}

impl<'a, T> Iterator for OptIterRep<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            OptIterRep::One(x) => Some(x),
            OptIterRep::Many(iter) => iter.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::One(_) => (0, None),
            Self::Many(iter) => {
                let len = iter.len();
                (len, Some(len))
            }
        }
    }
}
