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

    let height = input.lines().count() as i32;
    let width = input.lines().next().expect("there is a line").chars().count() as i32;

    let mut habitat = (0..height)
        .into_iter()
        .flat_map(|y| {
            (0..width)
                .into_iter()
                .map(|x| (IVec2{ x, y }, '.'))
                .collect::<Vec<_>>()
        })
        .collect::<HashMap<_, _>>();
    
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
        .for_each(|(loc, dir)| {
            habitat.entry(loc).and_modify(|mut val| {
                *val = match pipe_network.get(&loc).expect("the location is in the habitat") {
                    NorthSouth => ,
                    EastWest => 'X',
                    NorthEast => '|',
                    NorthWest => '|',
                    SouthWest => '|',
                    SouthEast => '|',
                    Ground => '.',
                    Start => 'S',
                }
            });
        });

    print_habitat(habitat, height, width);
    2.to_string()
}

fn print_habitat(habitat: HashMap<IVec2, char>, height: i32, width: i32) {
    (0..height)
        .into_iter()
        .for_each(|y| {
            let row = (0..width)
                .into_iter()
                .map(|x| habitat.get(&IVec2 {x, y})
                    .expect("the location is in the habitat").to_string()
                )
                .collect::<Vec<_>>()
                .join("");
            println!("{}", row);
        });
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

    // #[test]
    // fn it_solves_the_puzzle() {
    //     let input = include_str!("./input.txt");
    //     assert_eq!(solve(input), "6947");
    // }
}
