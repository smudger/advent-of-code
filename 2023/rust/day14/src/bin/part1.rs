use std::cmp::Ordering;
use itertools::Itertools;

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

fn solve(input: &str) -> String {
    transpose(input)
        .into_iter()
        .map(|row| {
            row
                .into_iter()
                .group_by(|c| *c == '#')
                .into_iter()
                .flat_map(|(_, group)| group
                    .sorted_unstable_by(|a, b| match (*a, *b) {
                        ('.', 'O') => Ordering::Less,
                        ('O', '.') => Ordering::Greater,
                        _ => Ordering::Equal
                    })
                    .collect::<Vec<_>>()
                )
                .enumerate()
                .filter_map(|(index, c)| (c == 'O').then_some(index + 1))
                .sum::<usize>()
        })
        .sum::<usize>()
        .to_string()
}

fn transpose(input: &str) -> Vec<Vec<char>> {
    let mut rows = input.lines().peekable();
    let row_length = rows.peek().expect("there is at least one row").len();

    let mut row_vecs = rows
        .into_iter()
        .map(|row| row.chars().into_iter())
        .rev()
        .collect::<Vec<_>>();

    (0..row_length)
        .map(|_| {
            row_vecs
                .iter_mut()
                .map(|chars| chars.next().unwrap())
                .collect::<Vec<_>>()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_example() {
        let input = "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
";
        assert_eq!(solve(input), "136");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "109755");
    }
}
