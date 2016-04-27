//! A `std::vec::Vec`, but 1-indexed instead of 0-indexed.

use std::ops::Index;
use std::ops::IndexMut;

/// Like a `std::vec::Vec`, but 1-indexed instead of 0-indexed.
#[derive(Debug)]
pub struct OneIndexedVec<T> {
    vec: Vec<T>,
}

impl<T> OneIndexedVec<T> {
    /// Returns the element of a slice at the given index, or None if the index is out of bounds.
    pub fn get(&self, index: usize) -> Option<&T> {
        if index == 0 {
            panic!("index is 0");
        }
        self.vec.get(index - 1)
    }

    /// Returns the number of elements in the slice.
    pub fn len(&self) -> usize {
        self.vec.len()
    }

    /// Returns true if the slice has a length of 0.
    pub fn is_empty(&self) -> bool {
        self.vec.is_empty()
    }

    /// Returns an iterator over the slice.
    pub fn iter(&self) -> ::std::slice::Iter<T> {
        self.vec.iter()
    }

    /// Returns an iterator that allows modifying each value.
    pub fn iter_mut(&mut self) -> ::std::slice::IterMut<T> {
        self.vec.iter_mut()
    }
}

impl<T> Index<usize> for OneIndexedVec<T> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        if index == 0 {
            panic!("index is 0");
        }
        &self.vec[index - 1]
    }
}

impl<T> IndexMut<usize> for OneIndexedVec<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        if index == 0 {
            panic!("index is 0");
        }
        &mut self.vec[index - 1]
    }
}

impl<T> From<Vec<T>> for OneIndexedVec<T> {
    fn from(vec: Vec<T>) -> Self {
        OneIndexedVec { vec: vec }
    }
}

impl<T> IntoIterator for OneIndexedVec<T> {
    type Item = T;
    type IntoIter = ::std::vec::IntoIter<T>;
    fn into_iter(self) -> Self::IntoIter {
        self.vec.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a OneIndexedVec<T> {
    type Item = &'a T;
    type IntoIter = ::std::slice::Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.vec.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut OneIndexedVec<T> {
    type Item = &'a mut T;
    type IntoIter = ::std::slice::IterMut<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.vec.iter_mut()
    }
}
