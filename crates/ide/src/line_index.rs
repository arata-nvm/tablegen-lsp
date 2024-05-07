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

    pub fn pos_to_line(&self, pos: TextSize) -> usize {
        self.rope.char_to_line(pos.into())
    }

    pub fn line_to_pos(&self, line: usize) -> TextSize {
        let pos = self.rope.line_to_char(line);
        TextSize::try_from(pos).expect("line index out of bounds")
    }
}
