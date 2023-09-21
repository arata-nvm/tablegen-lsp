use ropey::Rope;
use tablegen_parser::error::Span;
use tower_lsp::lsp_types::{Location, Position, Range};

use super::symbol::SourceLocation;

pub fn span_to_range(text: &Rope, span: Span) -> Range {
    let start = loc_to_position(text, span.start);
    let end = loc_to_position(text, span.end);
    Range::new(start, end)
}

pub fn loc_to_position(text: &Rope, loc: usize) -> Position {
    let line = text.try_char_to_line(loc).unwrap_or_default();
    let line_first = text.try_line_to_char(line).unwrap_or_default();
    let character = loc - line_first;
    Position::new(line as u32, character as u32)
}

pub fn position_to_loc(text: &Rope, position: Position) -> usize {
    text.try_line_to_char(position.line as usize)
        .unwrap_or_default()
        + position.character as usize
}

pub fn source_location_to_location(text: &Rope, loc: SourceLocation) -> Location {
    let uri = loc.uri.clone();
    let range = span_to_range(text, loc.span.clone());
    Location::new(uri, range)
}
