//! Helpers your solutions can share: `use aoc::ints;` to use them, and add your own.

use std::collections::HashMap;

/// Every integer in the text, including negative ones: `ints("x=-3, y=12")` is `[-3, 12]`.
pub fn ints(text: &str) -> Vec<i64> {
    let mut numbers = Vec::new();
    let mut current = String::new();
    for ch in text.chars().chain(std::iter::once(' ')) {
        if ch.is_ascii_digit() || (ch == '-' && current.is_empty()) {
            current.push(ch);
        } else {
            if current != "-" && !current.is_empty() {
                numbers.push(current.parse().unwrap());
            }
            current.clear();
            if ch == '-' {
                current.push(ch);
            }
        }
    }
    numbers
}

/// A grid of characters by (row, column), from lines of text.
pub fn grid(text: &str) -> HashMap<(i64, i64), char> {
    let mut cells = HashMap::new();
    for (r, line) in text.lines().enumerate() {
        for (c, ch) in line.chars().enumerate() {
            cells.insert((r as i64, c as i64), ch);
        }
    }
    cells
}

/// The positions above, below, left and right.
pub fn neighbours4((r, c): (i64, i64)) -> [(i64, i64); 4] {
    [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]
}

/// The positions around, diagonals included.
pub fn neighbours8((r, c): (i64, i64)) -> Vec<(i64, i64)> {
    let mut around = Vec::new();
    for dr in -1..=1 {
        for dc in -1..=1 {
            if (dr, dc) != (0, 0) {
                around.push((r + dr, c + dc));
            }
        }
    }
    around
}
