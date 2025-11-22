use common::{id::IdSet, u32_vec::U32Vec};
use proc_macro2::Ident;
use token::{Span, token::Lit};

use crate::ast::{Node, NodeCollection, NodeSet, NodeVec, UniqueNode};

impl Node for Span {}
impl Node for Lit {}

impl<T> NodeCollection<T> for Vec<T> {
    fn node_get(&self, idx: u32) -> Option<&T> {
        self.get(idx as usize)
    }

    fn node_get_mut(&mut self, idx: u32) -> Option<&mut T> {
        self.get_mut(idx as usize)
    }
}

impl<T: Node> NodeVec<T> for Vec<T> {
    fn node_insert(&mut self, value: T) -> Option<u32> {
        let len = u32::try_from(self.len()).ok()?;
        self.push(value);
        Some(len)
    }
}

impl<T> NodeCollection<T> for U32Vec<T> {
    fn node_get(&self, idx: u32) -> Option<&T> {
        self.get(idx)
    }

    fn node_get_mut(&mut self, idx: u32) -> Option<&mut T> {
        self.get_mut(idx)
    }
}

impl<T: Node> NodeVec<T> for U32Vec<T> {
    fn node_insert(&mut self, value: T) -> Option<u32> {
        let len = u32::try_from(self.len()).ok()?;
        self.push(value);
        Some(len)
    }
}

impl<T> NodeCollection<T> for IdSet<u32, T> {
    fn node_get(&self, idx: u32) -> Option<&T> {
        self.get(idx)
    }

    fn node_get_mut(&mut self, idx: u32) -> Option<&mut T> {
        self.get_mut(idx)
    }
}

impl<T: UniqueNode> NodeSet<T> for IdSet<u32, T> {
    fn node_insert(&mut self, value: T) -> Option<u32> {
        self.push(value)
    }
}

impl UniqueNode for Ident {}
