use std::{fmt::Write, ops::Range};

const SNIPPET_SIZE: usize = 80;

enum Truncation {
    None,
    Start,
    End,
    Both,
}

pub fn render_line(source: &str, location: Range<usize>, terminal: bool) -> String {
    if source == "" {
        panic!("an empty string should not be able to generate an error")
    }

    let lines = get_lines(source);
    let start_line = find_line(location.start, &lines);
    let end_line = find_line(location.end, &lines);
    let start_col = find_column(location.start, lines[start_line].0, lines[start_line].1);

    let error_columns = if start_line != end_line {
        1
    } else {
        let end_col = find_column(location.end, lines[start_line].0, lines[start_line].1);
        end_col - start_col
    };

    let (snippet_str, offset, trunc) =
        extract_snippet(lines[start_line].1, start_col, error_columns);

    let mut buf = String::new();

    if matches!(trunc, Truncation::Both | Truncation::Start) {
        write!(buf, "...").unwrap();
    }

    for (idx, c) in snippet_str.chars().enumerate() {
        if idx == offset {
            if terminal {
                write!(buf, "\x1b[30;47m").unwrap()
            } else {
                write!(buf, "%").unwrap()
            }
        }

        buf.push(c);

        if idx + 1 == offset + error_columns {
            if terminal {
                write!(buf, "\x1b[0m").unwrap()
            } else {
                write!(buf, "%").unwrap()
            }
        }
    }

    if matches!(trunc, Truncation::Both | Truncation::End) {
        write!(buf, "...").unwrap();
    }

    buf
}

pub fn render_block(source: &str, location: Range<usize>, message: &str) -> String {
    if source == "" {
        panic!("an empty string should not be able to generate an error")
    }

    let lines = get_lines(source);
    let start_line = find_line(location.start, &lines);
    let end_line = find_line(location.end, &lines);
    let start_col = find_column(location.start, lines[start_line].0, lines[start_line].1);

    let error_columns = if start_line != end_line {
        1
    } else {
        let end_col = find_column(location.end, lines[start_line].0, lines[start_line].1);
        end_col - start_col
    };

    let (snippet_str, offset, trunc) =
        extract_snippet(lines[start_line].1, start_col, error_columns);

    let line_number_len = ((start_line + 1).ilog10() + 1) as usize;

    let mut buf = String::new();
    writeln!(buf, "{}", message).unwrap();
    writeln!(buf, "--> {}:{}", start_line + 1, start_col + 1).unwrap();
    writeln!(buf, "{:>width$} |", " ", width = line_number_len).unwrap();

    write!(
        buf,
        "{:>width$} | ",
        start_line + 1,
        width = line_number_len
    )
    .unwrap();

    let offset = if matches!(trunc, Truncation::Both | Truncation::Start) {
        write!(buf, "...").unwrap();
        offset + 3
    } else {
        offset
    };

    write!(buf, "{}", snippet_str).unwrap();
    if matches!(trunc, Truncation::Both | Truncation::End) {
        writeln!(buf, "...").unwrap();
    } else {
        writeln!(buf).unwrap();
    }

    write!(buf, "{:>width$} | ", " ", width = line_number_len).unwrap();
    for _ in 0..offset {
        buf.push(' ');
    }
    for _ in 0..error_columns {
        buf.push('^');
    }
    writeln!(buf).unwrap();
    buf
}

fn get_lines(source: &str) -> Vec<(usize, &str)> {
    source
        .lines()
        .map(|x| (unsafe { str_offset_from(x, source) as usize }, x))
        .collect::<Vec<_>>()
}

fn find_line(offset: usize, lines: &[(usize, &str)]) -> usize {
    match lines.binary_search_by_key(&offset, |(a, _)| *a) {
        Ok(x) => x,
        Err(x) => {
            if x == 0 {
                return 0;
            }
            x - 1
        }
    }
}

fn find_column(offset: usize, line_offset: usize, line: &str) -> usize {
    assert!(offset >= line_offset);

    let in_line_offset = offset - line_offset;
    if in_line_offset >= line.len() {
        return line.chars().count();
    }

    let mut count = 0;
    let mut chars = line.chars();
    while chars.next().is_some() {
        if unsafe { str_offset_from(chars.as_str(), line) } as usize > in_line_offset {
            break;
        }
        count += 1;
    }
    count
}

fn extract_snippet(line: &str, start_col: usize, columns: usize) -> (&str, usize, Truncation) {
    let line_columns = line.chars().count();
    // can we fit the entire line
    if line_columns < SNIPPET_SIZE {
        return (line, start_col, Truncation::None);
    }

    // can we cut the line of at the end
    if start_col + columns <= SNIPPET_SIZE {
        return (
            slice_chars(line, 0..SNIPPET_SIZE),
            start_col,
            Truncation::End,
        );
    }

    if columns > SNIPPET_SIZE {
        let truncation = if start_col > 0 {
            if start_col + columns < line_columns {
                Truncation::Both
            } else {
                Truncation::Start
            }
        } else {
            if start_col + columns < line_columns {
                Truncation::End
            } else {
                Truncation::None
            }
        };

        return (
            slice_chars(line, start_col..(start_col + columns)),
            0,
            truncation,
        );
    }

    let chars_around = (SNIPPET_SIZE - columns) / 2;
    let start = start_col - chars_around;
    let end = (start + SNIPPET_SIZE).min(line_columns);

    let trunc = if end == line_columns {
        Truncation::Start
    } else {
        Truncation::Both
    };

    (slice_chars(line, start..end), start, trunc)
}

fn slice_chars(line: &str, range: Range<usize>) -> &str {
    if range.is_empty() {
        return &line[range.start..range.start];
    }
    let mut chars = line.chars();
    for _ in 0..range.start {
        chars.next();
    }
    let start = chars.as_str();
    for _ in 0..(range.end - range.start) {
        chars.next();
    }
    let offset = unsafe { str_offset_from(chars.as_str(), start) } as usize;
    &start[..offset]
}

unsafe fn str_offset_from(a: &str, b: &str) -> isize {
    unsafe { a.as_ptr().offset_from(b.as_ptr()) }
}
