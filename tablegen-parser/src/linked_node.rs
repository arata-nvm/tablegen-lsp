use std::rc::Rc;

use crate::{error::Range, node::SyntaxNode};

#[derive(Debug, Clone)]
pub struct LinkedNode<'a> {
    node: &'a SyntaxNode,
    parent: Option<Rc<Self>>,
    index: usize,
}

impl<'a> LinkedNode<'a> {
    pub fn new(root: &'a SyntaxNode) -> Self {
        Self {
            node: root,
            parent: None,
            index: 0,
        }
    }

    pub fn new_with_parent(root: &'a SyntaxNode, parent: Rc<Self>, index: usize) -> Self {
        Self {
            node: root,
            parent: Some(parent),
            index,
        }
    }

    pub fn node(&self) -> &SyntaxNode {
        self.node
    }

    pub fn find(&self, range: Range) -> Option<Self> {
        if self.node.range() == range {
            return Some(self.clone());
        }

        for child in self.children() {
            let child_range = child.node.range();
            if child_range.start <= range.start && range.end <= child_range.end {
                return child.find(range);
            }
        }

        None
    }
}

impl<'a> LinkedNode<'a> {
    pub fn parent(&self) -> Option<&LinkedNode<'a>> {
        self.parent.as_deref()
    }

    pub fn children(&self) -> LinkedChildren<'a> {
        LinkedChildren::new(Rc::new(self.clone()), self.node.children().enumerate())
    }

    pub fn prev_sibling(&self) -> Option<Self> {
        let parent = self.parent()?;
        let new_index = self.index.checked_sub(1)?;
        let sibling = parent.children().nth(new_index)?;
        Some(sibling)
    }
}

pub struct LinkedChildren<'a> {
    parent: Rc<LinkedNode<'a>>,
    iter: std::iter::Enumerate<std::slice::Iter<'a, SyntaxNode>>,
}

impl<'a> LinkedChildren<'a> {
    pub fn new(
        parent: Rc<LinkedNode<'a>>,
        iter: std::iter::Enumerate<std::slice::Iter<'a, SyntaxNode>>,
    ) -> Self {
        Self { parent, iter }
    }
}

impl<'a> Iterator for LinkedChildren<'a> {
    type Item = LinkedNode<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|(index, node)| LinkedNode::new_with_parent(node, Rc::clone(&self.parent), index))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl<'a> DoubleEndedIterator for LinkedChildren<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.iter
            .next_back()
            .map(|(index, node)| LinkedNode::new_with_parent(node, Rc::clone(&self.parent), index))
    }
}

impl<'a> ExactSizeIterator for LinkedChildren<'a> {}
