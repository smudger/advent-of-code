use std::collections::HashSet;
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
            Direction::Up => IVec2::NEG_Y,
            Direction::Down => IVec2::Y,
            Direction::Left => IVec2::NEG_X,
            Direction::Right => IVec2::X,
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
    let starting_position = Position(IVec2 { x: 0, y: 0 }, Right);
    
    let (_, energized_tiles) = calculate_energized_tiles(starting_position, &grid, HashSet::new());
    energized_tiles.len().to_string()
}

fn calculate_energized_tiles(current_pos: Position, grid: &Vec<Vec<Tile>>, mut visited_positions: HashSet<Position>) -> (HashSet<Position>, HashSet<IVec2>) {
    let mut energized_tiles = HashSet::new();
    if visited_positions.contains(&current_pos) {
        return (visited_positions, energized_tiles);
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
                return (visited_positions, energized_tiles);
            };
            
            let Some(_) = row.get(new_loc.x as usize) else {
                return (visited_positions, energized_tiles);
            };
            
            let (new_visited_positions, new_energized_tiles) = calculate_energized_tiles(Position(new_loc, current_pos.1), grid, visited_positions);
            (new_visited_positions, energized_tiles
                .union(&new_energized_tiles)
                .map(|vec| *vec)
                .collect())
        }
        VerticalSplitter => {
            let new_dirs = match current_pos.1 {
                Up => vec![Up],
                Down => vec![Down],
                Left | Right => vec![Up, Down],
            };

            return new_dirs
                .into_iter()
                .fold((visited_positions, energized_tiles), |acc, new_dir| {
                    let new_loc = current_pos.0 + new_dir.move_vec();

                    let Some(row) = grid.get(new_loc.y as usize) else {
                        return acc;
                    };

                    let Some(_) = row.get(new_loc.x as usize) else {
                        return acc;
                    };

                    let (new_visited_positions, new_energized_tiles) = calculate_energized_tiles(Position(new_loc, new_dir), grid, acc.0);
                    (new_visited_positions, acc.1
                        .union(&new_energized_tiles)
                        .map(|vec| *vec)
                        .collect())
                });
        },
        HorizontalSplitter => {
            let new_dirs = match current_pos.1 {
                Left => vec![Left],
                Right => vec![Right],
                Up | Down => vec![Left, Right],
            };

            return new_dirs
                .into_iter()
                .fold((visited_positions, energized_tiles), |acc, new_dir| {
                    let new_loc = current_pos.0 + new_dir.move_vec();

                    let Some(row) = grid.get(new_loc.y as usize) else {
                        return acc;
                    };

                    let Some(_) = row.get(new_loc.x as usize) else {
                        return acc;
                    };

                    let (new_visited_positions, new_energized_tiles) = calculate_energized_tiles(Position(new_loc, new_dir), grid, acc.0);
                    (new_visited_positions, acc.1
                        .union(&new_energized_tiles)
                        .map(|vec| *vec)
                        .collect())
                });
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
                return (visited_positions, energized_tiles);
            };

            let Some(_) = row.get(new_loc.x as usize) else {
                return (visited_positions, energized_tiles);
            };

            let (new_visited_positions, new_energized_tiles) = calculate_energized_tiles(Position(new_loc, new_dir), grid, visited_positions);
            (new_visited_positions, energized_tiles
                .union(&new_energized_tiles)
                .map(|vec| *vec)
                .collect())
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
                return (visited_positions, energized_tiles);
            };

            let Some(_) = row.get(new_loc.x as usize) else {
                return (visited_positions, energized_tiles);
            };

            let (new_visited_positions, new_energized_tiles) = calculate_energized_tiles(Position(new_loc, new_dir), grid, visited_positions);
            (new_visited_positions, energized_tiles
                .union(&new_energized_tiles)
                .map(|vec| *vec)
                .collect())
        }
    }
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
        assert_eq!(solve(input), "46");
    }

    // #[test]
    // fn it_solves_the_puzzle() {
    //     let input = include_str!("./input.txt");
    //     assert_eq!(solve(input), "7392");
    // }
}
