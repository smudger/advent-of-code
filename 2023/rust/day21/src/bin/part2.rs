use std::collections::{HashMap, HashSet};
use glam::IVec2;
use indicatif::ProgressIterator;
use itertools::Itertools;

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input, 26501365);
    dbg!(output);
}

fn solve(input: &str, steps: u32) -> String {
    let grid = input
        .lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    
    let rows = grid.len() as i32;
    let cols = grid[0].len() as i32;

    let starting_position = grid
        .iter()
        .enumerate()
        .find_map(|(y, row)| {
            row
                .iter()
                .enumerate()
                .find_map(|(x, c)| (*c == 'S').then_some(x))
                .and_then(|x| Some(IVec2{ x: x as i32, y: y as i32 }))
        })
        .expect("there is a starting position");

    (1..=steps)
        .into_iter()
        .progress_count((1..=steps).count() as u64)
        .fold(HashSet::from([starting_position]), |positions, _| {
            positions
                .into_iter()
                .flat_map(|position| {
                    let dirs = vec![
                        position + IVec2::X,
                        position + IVec2::NEG_X,
                        position + IVec2::Y,
                        position + IVec2::NEG_Y,
                    ];

                    dirs
                        .into_iter()
                        .filter(|dir| {
                            let c = grid
                                .get(((dir.y).rem_euclid(rows)) as usize)
                                .and_then(|row| row.get((dir.x.rem_euclid(cols)) as usize))
                                .expect("can find grid entry");

                            c != &'#'
                        })
                })
                .collect::<HashSet<_>>()
        })
        .len()
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_first_example() {
        let input = "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
";
        assert_eq!(solve(input, 6), "16");
    }

    #[test]
    fn it_solves_the_second_example() {
        let input = "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
";
        assert_eq!(solve(input, 10), "50");
    }

    #[test]
    fn it_solves_the_third_example() {
        let input = "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
";
        assert_eq!(solve(input, 50), "1594");
    }

    #[test]
    fn it_solves_the_fourth_example() {
        let input = "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
";
        assert_eq!(solve(input, 100), "6536");
    }

    #[test]
    fn it_solves_the_fifth_example() {
        let input = "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
";
        assert_eq!(solve(input, 500), "167004");
    }

    #[test]
    fn it_solves_the_sixth_example() {
        let input = "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
";
        assert_eq!(solve(input, 1000), "668697");
    }

    #[test]
    fn it_solves_the_seventh_example() {
        let input = "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
";
        assert_eq!(solve(input, 5000), "16733044");
    }

    // #[test]
    // fn it_solves_the_puzzle() {
    //     let input = include_str!("./input.txt");
    //     assert_eq!(solve(input, 64), "3617");
    // }
}
