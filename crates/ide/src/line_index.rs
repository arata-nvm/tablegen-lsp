use ropey::Rope;

use syntax::parser::TextSize;

#[derive(Debug, Eq, PartialEq)]
pub struct LineIndex {
    rope: Rope,
}

impl LineIndex {
    pub fn new(text: &str) -> Self {
        Self {
            rope: Rope::from_str(text),
        }
    }

    pub fn pos_to_line(&self, pos: TextSize) -> Option<usize> {
        self.rope.try_char_to_line(pos.into()).ok()
    }

    pub fn line_to_pos(&self, line: usize) -> Option<TextSize> {
        self.rope
            .try_line_to_char(line)
            .map(|pos| pos.try_into().unwrap())
            .ok()
    }
}
