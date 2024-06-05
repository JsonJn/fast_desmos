#![allow(dead_code)]

use std::cell::Cell;
use std::fmt::{Debug, Display, Formatter};
use std::panic::Location;
use std::ptr::addr_of;
#[cfg(feature = "log")]
use std::sync::atomic::{AtomicU32, Ordering};

use crate::log;

#[cfg(feature = "log")]
pub static GUARDS_ACTIVE: AtomicU32 = AtomicU32::new(0);

pub fn is_digit(c: u8) -> bool {
    c.is_ascii_digit()
}

pub fn is_alphabet(c: u8) -> bool {
    c.is_ascii_alphabetic()
}

pub fn is_alphanumeric(c: u8) -> bool {
    c.is_ascii_alphanumeric()
}

pub fn is_word(c: u8) -> bool {
    is_alphabet(c) || c == b'_'
}

pub struct AdvGuardCopy<'a, T: Copy> {
    vec: &'a mut AdvVec<T>,
    restore_index: usize,
    restore: Cell<bool>,
}

impl<T: Copy> AdvGuardCopy<'_, T> {
    pub fn accepted(&self) {
        self.restore.set(false);
    }

    pub fn denied(&self) {
        self.restore.set(true);
    }
}

impl<T: Copy> Drop for AdvGuardCopy<'_, T> {
    fn drop(&mut self) {
        #[cfg(feature = "log")]
        GUARDS_ACTIVE.fetch_sub(1, Ordering::Relaxed);
        if self.restore.get() {
            self.vec.index.set(self.restore_index);
        }
    }
}

pub struct AdvVec<T> {
    data: Vec<T>,
    index: Cell<usize>,
}

impl<T: Debug> Debug for AdvVec<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "AdvVec {{ index: {:?}, at: {:?} }}",
            self.index,
            unsafe { self.peek() }
        )
    }
}

impl<T: Debug> Display for AdvVec<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "\ntokens: {:?}\nindex: {}\n",
            self.data,
            self.index.get()
        )
    }
}

impl AdvVec<u8> {
    pub fn from_ascii(data: String) -> Option<Self> {
        data.is_ascii().then(|| Self::new(data.into_bytes()))
    }
}

impl<T: Debug> AdvVec<T> {
    pub fn new(data: Vec<T>) -> Self {
        Self {
            data,
            index: Cell::new(0),
        }
    }

    pub unsafe fn peek(&self) -> Option<&T> {
        self.data.get(self.index.get())
    }

    pub unsafe fn advance(&self) -> Option<&T> {
        let result = unsafe { self.peek() };
        self.index.set(self.index.get() + 1);
        result
    }

    pub unsafe fn un_advance(&self) -> Option<&T> {
        let result = unsafe { self.peek() };
        self.index.set(self.index.get() - 1);
        result
    }

    pub fn has_remaining(&self) -> bool {
        self.index.get() < self.data.len()
    }

    pub fn is_done(&self) -> bool {
        !self.has_remaining()
    }

    pub fn take_remaining(self) -> Vec<T> {
        let mut data = self.data;
        data.split_off(self.index.get())
    }

    pub fn guard_denied_silent(&self) -> AdvGuard<T> {
        #[cfg(feature = "log")]
        GUARDS_ACTIVE.fetch_add(1, Ordering::Relaxed);
        AdvGuard {
            restore_index: self.index.get(),
            vec: addr_of!(*self).cast_mut(),
            restore: Cell::new(true),
        }
    }

    pub fn guard_denied(&self) -> AdvGuard<T> {
        log!("NEW GUARD CREATING");
        #[cfg(feature = "log")]
        GUARDS_ACTIVE.fetch_add(1, Ordering::Relaxed);
        AdvGuard {
            restore_index: self.index.get(),
            vec: addr_of!(*self).cast_mut(),
            restore: Cell::new(true),
        }
    }

    pub fn guard_accepted(&self) -> AdvGuard<T> {
        log!("NEW GUARD CREATING");
        #[cfg(feature = "log")]
        GUARDS_ACTIVE.fetch_add(1, Ordering::Relaxed);
        AdvGuard {
            restore_index: self.index.get(),
            vec: addr_of!(*self).cast_mut(),
            restore: Cell::new(false),
        }
    }
}

impl<T: Copy> AdvVec<T> {
    pub fn guard_copy_denied(&mut self) -> AdvGuardCopy<T> {
        log!("NEW GUARD CREATING");
        #[cfg(feature = "log")]
        GUARDS_ACTIVE.fetch_add(1, Ordering::Relaxed);
        AdvGuardCopy {
            restore_index: self.index.get(),
            vec: self,
            restore: Cell::new(true),
        }
    }

    pub fn guard_copy_accepted(&mut self) -> AdvGuardCopy<T> {
        log!("NEW GUARD CREATING");
        #[cfg(feature = "log")]
        GUARDS_ACTIVE.fetch_add(1, Ordering::Relaxed);
        AdvGuardCopy {
            restore_index: self.index.get(),
            vec: self,
            restore: Cell::new(false),
        }
    }
}

impl<T: Copy> AdvGuardCopy<'_, T> {
    pub fn advance(&self) -> Option<T> {
        let at = self.peek();
        self.vec.index.set(self.vec.index.get() + 1);
        at
    }

    pub fn peek(&self) -> Option<T> {
        self.vec.data.get(self.vec.index.get()).copied()
    }

    pub fn peek_next(&self) -> Option<T> {
        self.vec.data.get(self.vec.index.get() + 1).copied()
    }

    pub fn advance_if(&mut self, pred: impl FnOnce(T) -> bool) -> bool {
        let peek = self.peek().map(pred);
        if let Some(true) = peek {
            self.advance();
        }
        peek.unwrap_or(false)
    }

    pub fn advance_while(&mut self, pred: impl FnMut(T) -> bool) -> Option<Vec<T>> {
        let bytes = self.advance_while_unchecked(pred);
        (!bytes.is_empty()).then_some(bytes)
    }

    pub fn advance_while_unchecked(&mut self, mut pred: impl FnMut(T) -> bool) -> Vec<T> {
        let mut bytes = Vec::new();
        while let Some(at) = self.peek() {
            if pred(at) {
                bytes.push(at);
                self.advance();
            } else {
                break;
            }
        }
        bytes
    }
}

impl<T: Copy + PartialEq> AdvGuardCopy<'_, T> {
    pub fn advance_if_eq(&mut self, val: T) -> bool {
        self.advance_if(|v| v == val)
    }

    pub fn must_equal(&mut self, value: T) {
        if !self.advance_if_eq(value) {
            panic!("Did not equal required value!");
        }
    }
}

pub struct AdvGuard<T: Debug> {
    vec: *mut AdvVec<T>,
    restore_index: usize,
    restore: Cell<bool>,
}

impl<T: Debug> AdvGuard<T> {
    fn v(&self) -> &mut AdvVec<T> {
        unsafe { self.vec.as_mut() }.unwrap()
    }
}

impl<T: Debug> AdvGuard<T> {
    pub fn advance(&self) -> Option<&T> {
        let at = self.peek();

        let new_pos = self.v().index.get() + 1;
        log!("ADVANCING TO {new_pos}");
        self.v().index.set(new_pos);
        log!("ADVANCED: {at:?}");
        at
    }

    pub fn un_advance(&self) -> Option<&T> {
        unsafe { self.v().un_advance() }
    }

    pub fn peek(&self) -> Option<&T> {
        unsafe { self.v().peek() }
    }

    pub fn peek_next(&self) -> Option<&T> {
        self.v().data.get(self.v().index.get() + 1)
    }

    pub fn advance_if(&self, pred: impl FnOnce(&T) -> bool) -> bool {
        let peek = self.peek().map(pred);
        if let Some(true) = peek {
            self.advance();
        }
        peek.unwrap_or(false)
    }

    pub fn advance_if_some<R>(&self, pred: impl FnOnce(&T) -> Option<R>) -> Option<R> {
        let peek = self.peek().and_then(pred);
        if peek.is_some() {
            self.advance();
        }
        peek
    }

    pub fn advance_while(&mut self, pred: impl FnMut(&T) -> bool) -> Option<Vec<&T>> {
        let bytes = self.advance_while_unchecked(pred);
        (!bytes.is_empty()).then_some(bytes)
    }

    pub fn advance_while_unchecked(&mut self, mut pred: impl FnMut(&T) -> bool) -> Vec<&T> {
        let mut bytes = Vec::new();
        while let Some(at) = self.peek() {
            if pred(at) {
                bytes.push(at);
                self.advance();
            } else {
                break;
            }
        }
        bytes
    }

    pub fn accepted(&self) {
        self.restore.set(false);
    }

    pub fn denied(&self) {
        self.restore.set(true);
    }
}

impl<T: Debug> Drop for AdvGuard<T> {
    fn drop(&mut self) {
        log!("status: {:?}", self.v());
        if self.restore.get() {
            log!("FAILED: RESTORED TO {}", self.restore_index);
            self.v().index.set(self.restore_index);
        } else {
            log!("SUCCESS: DID NOT RESTORE");
        }
        #[cfg(feature = "log")]
        GUARDS_ACTIVE.fetch_sub(1, Ordering::Relaxed);
    }
}

impl<T: PartialEq + Debug> AdvGuard<T> {
    #[track_caller]
    pub fn advance_if_eq(&self, val: &T) -> bool {
        let result = self.advance_if(|v| v == val);
        let _caller = Location::caller();
        log!(
            "CALLED MATCH at {}:{}:{}:",
            _caller.file(),
            _caller.line(),
            _caller.column()
        );
        if result {
            log!("MATCHED successfully: {val:?}");
        } else {
            log!("MATCH FAILED: {val:?}");
        }
        result
    }

    #[must_use = "Use advance_if_eq for ignorant cases."]
    pub fn eq_else_none(&self, val: &T) -> Option<()> {
        self.advance_if_eq(val).then_some(())
    }
}
