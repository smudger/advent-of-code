use std::collections::HashMap;
use std::iter::successors;
use glam::IVec2;
use crate::Pipe::{EastWest, Ground, NorthEast, NorthSouth, NorthWest, SouthEast, SouthWest, Start};

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum Pipe {
    NorthSouth,
    EastWest,
    NorthEast,
    NorthWest,
    SouthWest,
    SouthEast,
    Ground,
    Start,
}

impl Pipe {
    fn next_direction(&self, inbound: IVec2) -> IVec2 {
        let options = match self {
            NorthSouth => [IVec2::Y, IVec2::NEG_Y],
            EastWest => [IVec2::X, IVec2::NEG_X],
            NorthEast => [IVec2::NEG_Y, IVec2::X],
            NorthWest => [IVec2::NEG_Y, IVec2::NEG_X],
            SouthWest => [IVec2::Y, IVec2::NEG_X],
            SouthEast => [IVec2::Y, IVec2::X],
            Ground | Start => unimplemented!()
        };

        *options
            .iter()
            .find(|direction| *(*direction) != (inbound * -1))
            .expect("there is a next direction")
    }
}

fn solve(input: &str) -> String {
    let pipe_network = input
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line
                .chars()
                .enumerate()
                .map(|(x, c)|  (
                    IVec2{ x: x as i32, y: y as i32 },
                    match c {
                        '|' => NorthSouth,
                        '-' => EastWest,
                        'L' => NorthEast,
                        'J' => NorthWest,
                        '7' => SouthWest,
                        'F' => SouthEast,
                        '.' => Ground,
                        'S' => Start,
                        _ => panic!("unexpected char: {c}")
                    }
                ))
                .collect::<Vec<_>>()
        })
        .collect::<HashMap<_, _>>();

    let starting_coordinate = pipe_network
        .iter()
        .find_map(|(coordinate, pipe)| (*pipe == Start).then_some(coordinate))
        .expect("there is a starting coordinate");

    let starting_directions = [
        (IVec2::NEG_X, [EastWest, NorthEast, SouthEast]),
        (IVec2::X, [EastWest, NorthWest, SouthWest]),
        (IVec2::NEG_Y, [NorthSouth, SouthWest, SouthEast]),
        (IVec2::Y, [NorthSouth, NorthEast, NorthWest]),
    ]
        .iter()
        .filter_map(|(direction, valid_pipes)| {
            let Some(pipe) = pipe_network.get(&(*direction + *starting_coordinate)) else {
                return None;
            };

            if !valid_pipes.contains(pipe) {
               return None;
            }
            
            Some(direction)
        })
        .collect::<Vec<_>>();
    
    let mut start_replacement: Option<Pipe> = None;
    
    if starting_directions == vec![&IVec2::NEG_X, &IVec2::X] {
        start_replacement = Some(EastWest);
    };
    if starting_directions == vec![&IVec2::NEG_Y, &IVec2::Y] {
        start_replacement = Some(NorthSouth);
    };
    if starting_directions == vec![&IVec2::X, &IVec2::NEG_Y] {
        start_replacement = Some(NorthEast);
    };
    if starting_directions == vec![&IVec2::X, &IVec2::Y] {
        start_replacement = Some(SouthEast);
    };
    if starting_directions == vec![&IVec2::NEG_X, &IVec2::Y] {
        start_replacement = Some(SouthWest);
    };
    if starting_directions == vec![&IVec2::NEG_X, &IVec2::NEG_Y] {
        start_replacement = Some(NorthWest);
    };
    
    let starting_direction = starting_directions[0];

    let height = input.lines().count();
    let width = input.lines().next().expect("there is a line").chars().count();

    let mut habitat = (0..height)
        .into_iter()
        .map(|_| vec![0f32; width])
        .collect::<Vec<_>>();
    
    let path = successors(
        Some((*starting_coordinate, *starting_direction)),
        |(current_location, direction_to_travel)| {
            let (next_location, pipe) = pipe_network
                .get_key_value(&(*current_location + *direction_to_travel))
                .expect("the next location is in the network");
    
            match *pipe {
                Start => None,
                _ => Some((*next_location, pipe.next_direction(*direction_to_travel)))
            }
    
        }
    );
    
    path.clone().for_each(|(loc, dir)| {
        habitat[loc.y as usize][loc.x as usize] = get_modifier_with_start(
            pipe_network.get(&loc).expect("the location is in the habitat"),
            &start_replacement.expect("we found a start replacement"),
            &dir
        );
    });
    
    let path = path
        .map(|(loc, _)| loc)
        .collect::<Vec<_>>();

    habitat
        .iter()
        .enumerate()
        .map(|(y, row)| {
            row
                .iter()
                .enumerate()
                .filter(|(x, _)| {
                    if path.contains(&IVec2 { x:*x as i32, y: y as i32 }) {
                        return false;
                    }
                    
                    return row[..*x]
                        .iter()
                        .sum::<f32>() != 0_f32
                })
                .count()
        })
        .sum::<usize>()
        .to_string()
}

fn get_modifier_with_start(pipe: &Pipe, start_replacement: &Pipe, dir: &IVec2) -> f32 {
    match pipe {
        Start => get_modifier(start_replacement, dir),
        _ => get_modifier(pipe, dir),
    }
}

fn get_modifier(pipe: &Pipe, dir: &IVec2) -> f32 {
    match pipe {
        NorthSouth => match dir.y {
            1 => 1f32,
            -1 => -1f32,
            _ => panic!("NorthSouth: {}", dir)
        },
        NorthEast | NorthWest => match dir.y {
            -1 => -0.5f32,
            0 => 0.5f32,
            _ => panic!("North: {}", dir)
        },
        SouthEast | SouthWest => match dir.y {
            1 => 0.5f32,
            0 => -0.5f32,
            _ => panic!("South: {}", dir)
        },
        EastWest => 0f32,
        Ground | Start => unimplemented!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_first_example() {
        let input = "...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
";
        assert_eq!(solve(input), "4");
    }

    #[test]
    fn it_solves_the_second_example() {
        let input = "..........
.S------7.
.|F----7|.
.||....||.
.||....||.
.|L-7F-J|.
.|..||..|.
.L--JL--J.
..........
";
        assert_eq!(solve(input), "4");
    }

    #[test]
    fn it_solves_the_third_example() {
        let input = ".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...
";
        assert_eq!(solve(input), "8");
    }

    #[test]
    fn it_solves_the_fourth_example() {
        let input = "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L
";
        assert_eq!(solve(input), "10");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "273");
    }
}
