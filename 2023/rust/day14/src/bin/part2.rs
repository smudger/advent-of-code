use std::cmp::Ordering;
use itertools::Itertools;

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

fn solve(input: &str) -> String {
    let grid = input
        .lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let rolled_grid = (0..1000)
        .fold(grid, |acc, _| cycle(acc));
    
    
    rolled_grid
        .iter()
        .rev()
        .enumerate()
        .map(|(index, row)| {
            row
                .iter()
                .filter_map(|c| (*c == 'O').then_some(index + 1))
                .sum::<usize>()
        })
        .sum::<usize>()
        .to_string()
}

fn cycle(grid: Vec<Vec<char>>) -> Vec<Vec<char>> {
    // NORTH ROLL
    let grid = rotate_ccw(roll(rotate_cw(grid)));
    // WEST ROLL
    let grid = rotate_cw(rotate_cw(roll(rotate_cw(rotate_cw(grid)))));
    // SOUTH ROLL
    let grid = rotate_cw(roll(rotate_ccw(grid)));
    // EAST ROLL
    let grid = roll(grid);
    
    grid
}

fn roll(grid: Vec<Vec<char>>) -> Vec<Vec<char>> {
    grid
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
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>()
}

fn rotate_ccw(grid: Vec<Vec<char>>) -> Vec<Vec<char>> {
    let mut grid = grid.into_iter().peekable();
    let row_length = grid.peek().expect("there is at least one row").len();

    let mut row_vecs = grid
        .into_iter()
        .map(|row| row.into_iter().rev())
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

fn rotate_cw(grid: Vec<Vec<char>>) -> Vec<Vec<char>> {
    let mut grid = grid.into_iter().peekable();
    let row_length = grid.peek().expect("there is at least one row").len();

    let mut row_vecs = grid
        .into_iter()
        .map(|row| row.into_iter())
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
        assert_eq!(solve(input), "64");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "90928");
    }
}
