use std::collections::HashMap;
use std::iter::successors;
use std::ops::Div;
use glam::IVec2;
use crate::Pipe::{EastWest, Ground, NorthEast, NorthSouth, NorthWest, SouthEast, SouthWest, Start};

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

#[derive(Debug, PartialEq)]
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
    
    let starting_direction = [
        (IVec2::NEG_X, [EastWest, NorthEast, SouthEast]),
        (IVec2::X, [EastWest, NorthWest, SouthWest]),
        (IVec2::NEG_Y, [NorthSouth, SouthWest, SouthEast]),
        (IVec2::Y, [NorthSouth, NorthEast, NorthWest]),
    ]
        .iter()
        .find(|(direction, valid_pipes)| {
            let Some(pipe) = pipe_network.get(&(*direction + *starting_coordinate)) else {
                return false;
            };
            
            valid_pipes.contains(pipe)
        })
        .expect("there should be two valid starting directions")
        .0;
    
    successors(
        Some((*starting_coordinate, starting_direction)),
        |(current_location, direction_to_travel)| {
            let (next_location, pipe) = pipe_network
                .get_key_value(&(*current_location + *direction_to_travel))
                .expect("the next location is in the network");
            
            match *pipe {
                Start => None,
                _ => Some((*next_location, pipe.next_direction(*direction_to_travel)))
            }
            
        }
    )
        .count()
        .div(2)
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_first_example() {
        let input = "-L|F7
7S-7|
L|7||
-L-J|
L|-JF
";
        assert_eq!(solve(input), "4");
    }
    
    #[test]
    fn it_solves_the_second_example() {
        let input = "7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ
";
        assert_eq!(solve(input), "8");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "6947");
    }
}
