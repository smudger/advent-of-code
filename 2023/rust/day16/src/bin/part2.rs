use std::collections::{HashSet, VecDeque};
use glam::IVec2;
use crate::Direction::{Down, Left, Right, Up};
use crate::Tile::{BackwardMirror, Empty, ForwardMirror, HorizontalSplitter, VerticalSplitter};

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn move_vec(&self) -> IVec2 {
        match self {
            Up => IVec2::NEG_Y,
            Down => IVec2::Y,
            Left => IVec2::NEG_X,
            Right => IVec2::X,
        }
    }
}

#[derive(Debug)]
enum Tile {
    Empty,
    ForwardMirror,
    BackwardMirror,
    VerticalSplitter,
    HorizontalSplitter,
}

#[derive(Debug, Copy, Clone, Eq, Hash, PartialEq)]
struct Position(IVec2, Direction);

fn solve(input: &str) -> String {
    let grid = input
        .lines()
        .map(|line| line
            .chars()
            .map(|c| match c {
                '.' => Empty,
                '/' => ForwardMirror,
                '\\' => BackwardMirror,
                '-' => HorizontalSplitter,
                '|' => VerticalSplitter,
                _ => unimplemented!("unknown character: {}", c)
            })
            .collect::<Vec<_>>()
        )
        .collect::<Vec<_>>();

    let row_len = grid[0].len();
    
    let max_left_start = (0..grid.len())
        .into_iter()
        .map(|y| {
            let starting_position = Position(IVec2 { x: 0, y: y as i32 }, Right);
            
            let energized_tiles = calculate_energized_tiles(starting_position, &grid);
            
            energized_tiles.len()
        })
        .max()
        .unwrap()
        .to_string();
    let max_right_start = (0..grid.len())
        .into_iter()
        .map(|y| {
            let starting_position = Position(IVec2 { x: (row_len - 1) as i32, y: y as i32 }, Left);

            let energized_tiles = calculate_energized_tiles(starting_position, &grid);

            energized_tiles.len()
        })
        .max()
        .unwrap()
        .to_string();
    let max_top_start = (0..row_len)
        .into_iter()
        .map(|x| {
            let starting_position = Position(IVec2 { x: x as i32, y: 0 }, Down);

            let energized_tiles = calculate_energized_tiles(starting_position, &grid);

            energized_tiles.len()
        })
        .max()
        .unwrap()
        .to_string();
    let max_bottom_start = (0..row_len)
        .into_iter()
        .map(|x| {
            let starting_position = Position(IVec2 { x: x as i32, y: (grid.len() - 1) as i32 }, Up);

            let energized_tiles = calculate_energized_tiles(starting_position, &grid);

            energized_tiles.len()
        })
        .max()
        .unwrap()
        .to_string();
    
    max_left_start
        .max(max_bottom_start)
        .max(max_right_start)
        .max(max_top_start)
}

fn calculate_energized_tiles(starting_pos: Position, grid: &Vec<Vec<Tile>>) -> HashSet<IVec2> {
    let mut queue = VecDeque::new();
    queue.push_back(starting_pos);

    let mut energized_tiles = HashSet::new();
    let mut visited_positions = HashSet::new();

    while let Some(current_pos) = queue.pop_front() {
        if visited_positions.contains(&current_pos) {
            continue;
        };

        visited_positions.insert(current_pos);
        energized_tiles.insert(current_pos.0);

        let current_tile = grid
            .get(current_pos.0.y as usize)
            .expect("the current position y is in the grid")
            .get(current_pos.0.x as usize)
            .expect("the current position x is in the grid");

        match current_tile {
            Empty => {
                let new_loc = current_pos.0 + current_pos.1.move_vec();

                let Some(row) = grid.get(new_loc.y as usize) else {
                    continue;
                };

                let Some(_) = row.get(new_loc.x as usize) else {
                    continue;
                };

                queue.push_back(Position(new_loc, current_pos.1));
            }
            VerticalSplitter => {
                let new_dirs = match current_pos.1 {
                    Up => vec![Up],
                    Down => vec![Down],
                    Left | Right => vec![Up, Down],
                };

                for new_dir in new_dirs.into_iter() {
                    let new_loc = current_pos.0 + new_dir.move_vec();

                    let Some(row) = grid.get(new_loc.y as usize) else {
                        continue;
                    };

                    let Some(_) = row.get(new_loc.x as usize) else {
                        continue;
                    };

                    queue.push_back(Position(new_loc, new_dir));
                }
            },
            HorizontalSplitter => {
                let new_dirs = match current_pos.1 {
                    Left => vec![Left],
                    Right => vec![Right],
                    Up | Down => vec![Left, Right],
                };

                for new_dir in new_dirs.into_iter() {
                    let new_loc = current_pos.0 + new_dir.move_vec();

                    let Some(row) = grid.get(new_loc.y as usize) else {
                        continue;
                    };

                    let Some(_) = row.get(new_loc.x as usize) else {
                        continue;
                    };

                    queue.push_back(Position(new_loc, new_dir));
                }
            }
            ForwardMirror => {
                let new_dir = match current_pos.1 {
                    Up => Right,
                    Down => Left,
                    Left => Down,
                    Right => Up,
                };
                let new_loc = current_pos.0 + new_dir.move_vec();

                let Some(row) = grid.get(new_loc.y as usize) else {
                    continue;
                };

                let Some(_) = row.get(new_loc.x as usize) else {
                    continue;
                };

                queue.push_back(Position(new_loc, new_dir));
            }
            BackwardMirror => {
                let new_dir = match current_pos.1 {
                    Up => Left,
                    Down => Right,
                    Left => Up,
                    Right => Down,
                };
                let new_loc = current_pos.0 + new_dir.move_vec();

                let Some(row) = grid.get(new_loc.y as usize) else {
                    continue;
                };

                let Some(_) = row.get(new_loc.x as usize) else {
                    continue;
                };

                queue.push_back(Position(new_loc, new_dir));
            }
        }
    }

    energized_tiles
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_example() {
        let input = r".|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
";
        assert_eq!(solve(input), "51");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "7665");
    }
}
