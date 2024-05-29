use std::fmt::{Debug, Formatter};
use std::mem::{size_of, ManuallyDrop};
use std::ops::{Deref, DerefMut, Index, IndexMut};
use std::sync::RwLock;
use std::{cmp, mem};

use rustc_hash::FxHashMap;

type CloneNumber = u16;

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct Id {
    id: u32,
}

impl Id {
    pub const fn new(x: u32) -> Self {
        Self { id: x }
    }

    pub const fn zeroed() -> Self {
        Self::new(0)
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
struct InternalId {
    id: Id,
    clone_number: CloneNumber,
}

impl InternalId {
    pub fn new(x: Id, clone_number: CloneNumber) -> Self {
        Self {
            id: x,
            clone_number,
        }
    }
}

impl From<Id> for InternalId {
    fn from(value: Id) -> Self {
        Self::new(value, 0)
    }
}

pub struct VecPool<T> {
    vecs: RwLock<FxHashMap<InternalId, Option<Vec<T>>>>,
}

impl<T: Debug> Debug for VecPool<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            write!(f, "{:#?}", self.vecs)
        } else {
            write!(f, "{:?}", self.vecs)
        }
    }
}

impl<T> Default for VecPool<T> {
    fn default() -> Self {
        assert_ne!(size_of::<T>(), 0, "PooledVec doesn't support ZSTs (yet)");
        Self {
            vecs: RwLock::new(FxHashMap::default()),
        }
    }
}

impl<T> VecPool<T> {
    pub fn new() -> Self {
        Self::default()
    }

    fn get_or_else(&self, id: InternalId, default: impl FnOnce() -> Vec<T>) -> Vec<T> {
        // if id.id.id == 10068 {
        //     panic!();
        // }

        let mut vecs = self.vecs.write().unwrap();
        let vec = vecs.remove(&id).map_or_else(default, |v| {
            if let Some(x) = v {
                x
            } else {
                unreachable!("Cannot take two vecs with the same id {id:?}.");
            }
        });
        vecs.insert(id, None);
        vec
    }

    fn get_with_capacity(&self, id: InternalId, capacity: usize) -> Vec<T> {
        // if id.id.id == 10068 {
        //     panic!();
        // }

        let mut vecs = self.vecs.write().unwrap();
        let v = vecs.remove(&id).map_or_else(
            || Vec::with_capacity(capacity),
            |v| {
                let Some(mut v) = v else {
                    unreachable!("Cannot take two vecs with the same id {id:?}.");
                };
                v.reserve(capacity);
                v
            },
        );
        vecs.insert(id, None);
        v
    }

    fn get_or_new(&self, id: InternalId) -> Vec<T> {
        self.get_or_else(id, Vec::new)
    }

    fn return_vec(&self, id: InternalId, mut vec: Vec<T>) {
        let mut vecs = self.vecs.write().unwrap();
        let already_at = vecs.get(&id);
        if already_at.is_none() {
            unreachable!("Cannot return un-taken Vec (wtf?)")
        }
        if already_at.unwrap().is_some() {
            unreachable!("Cannot return already returned Vec (wtf?)")
        }
        vec.clear();
        vecs.insert(id, Some(vec));
    }

    // pub fn get_clone_count(&self, id: u32) -> u8 {
    //     self.clone_counts
    //         .read()
    //         .unwrap()
    //         .get(&id)
    //         .copied()
    //         .unwrap_or(0)
    // }

    /// Increment clone count, return the new value.
    fn get_next_derived(&self, id: Id) -> InternalId {
        let vecs = self.vecs.read().unwrap();
        InternalId::new(
            id,
            (0..=CloneNumber::MAX)
                .find(|&i| {
                    !vecs
                        .get(&InternalId {
                            id,
                            clone_number: i,
                        })
                        .is_some_and(|v| v.is_none())
                })
                .expect("More than 65536 clones were made!"),
        )
    }
}

pub struct PooledVec<T: 'static> {
    pool: &'static VecPool<T>,
    internal_id: InternalId,
    vec: ManuallyDrop<Vec<T>>,
}

impl<T: PartialEq> PartialEq for PooledVec<T> {
    fn eq(&self, other: &Self) -> bool {
        self.vec == other.vec
    }
}

impl<T: Debug> Debug for PooledVec<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Pooled")?;
        if f.alternate() {
            write!(f, "{:#?}", self.vec)
        } else {
            write!(f, "{:?}", self.vec)
        }
    }
}

impl<T: Clone> Clone for PooledVec<T> {
    fn clone(&self) -> Self {
        let mut clone = self.new_derived();
        Vec::clone_into(&self.vec, &mut clone.vec);
        clone
    }
}

impl<T> PooledVec<T> {
    pub fn new(pool: &'static VecPool<T>, id: Id) -> Self {
        Self::new_internal(pool, pool.get_next_derived(id))
    }

    fn new_internal(pool: &'static VecPool<T>, internal_id: InternalId) -> Self {
        Self {
            vec: ManuallyDrop::new(pool.get_or_new(internal_id)),
            pool,
            internal_id,
        }
    }

    fn new_derived_id(pool: &'static VecPool<T>, id: Id) -> Self {
        Self::new_internal(pool, pool.get_next_derived(id))
    }

    pub fn new_derived(&self) -> Self {
        let new_id = self.pool.get_next_derived(self.internal_id.id);
        PooledVec::with_capacity_internal(self.pool, new_id, self.len())
    }

    pub fn with_capacity(pool: &'static VecPool<T>, id: Id, capacity: usize) -> Self {
        Self::with_capacity_internal(pool, pool.get_next_derived(id), capacity)
    }

    fn with_capacity_internal(
        pool: &'static VecPool<T>,
        internal_id: InternalId,
        capacity: usize,
    ) -> Self {
        Self {
            vec: ManuallyDrop::new(pool.get_with_capacity(internal_id, capacity)),
            pool,
            internal_id,
        }
    }

    pub fn from_iter<I: IntoIterator<Item = T>>(
        pool: &'static VecPool<T>,
        id: Id,
        iter: I,
    ) -> Self {
        let iter = iter.into_iter();
        let mut vec = Self::new(pool, id);
        let (min, _) = iter.size_hint();
        vec.reserve(min);
        vec.extend(iter);
        vec
    }

    unsafe fn take_vec(&mut self) -> Vec<T> {
        unsafe { ManuallyDrop::take(&mut self.vec) }
    }

    pub fn zip_map_same<B: Default>(
        mut self,
        mut other: PooledVec<B>,
        mut f: impl FnMut(T, B) -> T,
    ) -> Self {
        let len = cmp::min(self.len(), other.len());
        for i in 0..len {
            let a_at = self.get_mut(i).unwrap();
            let b_at = other.get_mut(i).unwrap();

            take_mut::take(a_at, |v| f(v, mem::take(b_at)));
        }
        self
    }

    pub fn map_same(mut self, mut f: impl FnMut(T) -> T) -> Self {
        for i in 0..self.len() {
            let at = self.get_mut(i).unwrap();
            take_mut::take(at, &mut f);
        }
        self
    }
}

impl<T: Clone> PooledVec<T> {
    pub fn pooled_to_vec(self) -> Vec<T> {
        ManuallyDrop::into_inner(self.vec.clone())
    }
}

impl<T: Default> PooledVec<T> {
    pub fn zip_map_different<B: Default, C>(
        pool: &'static VecPool<C>,
        mut a: PooledVec<T>,
        mut b: PooledVec<B>,
        mut f: impl FnMut(T, B) -> C,
    ) -> PooledVec<C> {
        let len = cmp::min(a.len(), b.len());
        let mut result = PooledVec::new_derived_id(pool, a.internal_id.id);
        result.reserve(len);
        for i in 0..len {
            let a_at = a.get_mut(i).unwrap();
            let b_at = b.get_mut(i).unwrap();
            let (a, b) = (mem::take(a_at), mem::take(b_at));
            result.push(f(a, b));
        }
        // All elements in `self` are now `mem::zeroed()`

        unsafe {
            // SAFETY: Setting to 0 has no safety concerns
            a.set_len(0);
            b.set_len(0);
        }

        result
    }

    pub fn map_different<U>(
        mut self,
        new_pool: &'static VecPool<U>,
        mut f: impl FnMut(T) -> U,
    ) -> PooledVec<U> {
        let len = self.len();
        let mut result = PooledVec::new_derived_id(new_pool, self.internal_id.id);
        result.reserve(len);
        for i in 0..len {
            let at = self.get_mut(i).unwrap();
            let at = mem::take(at);
            result.push(f(at));
        }
        result
    }
}

impl<T> AsRef<Vec<T>> for PooledVec<T> {
    fn as_ref(&self) -> &Vec<T> {
        &self.vec
    }
}

impl<T> AsMut<Vec<T>> for PooledVec<T> {
    fn as_mut(&mut self) -> &mut Vec<T> {
        &mut self.vec
    }
}

impl<T> Deref for PooledVec<T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.vec
    }
}

impl<T> DerefMut for PooledVec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.vec
    }
}

impl<T> Index<usize> for PooledVec<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.vec.index(index)
    }
}

impl<T> IndexMut<usize> for PooledVec<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.vec.index_mut(index)
    }
}

impl<T> Drop for PooledVec<T> {
    fn drop(&mut self) {
        let taken = unsafe { self.take_vec() };
        self.pool.return_vec(self.internal_id, taken);
    }
}
