use std::collections::{HashSet};
use glam::IVec2;
use itertools::Itertools;
use nom::branch::alt;
use nom::bytes::complete::{tag, take, take_until};
use nom::character::complete::{line_ending};
use nom::combinator::{map, map_res};
use nom::error::Error;
use nom::multi::{separated_list1};
use nom::sequence::{delimited, preceded, tuple};
use crate::Direction::{Down, Left, Right, Up};

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

fn solve(input: &str) -> String {
    let (trenches, vertices, _) = parse_input(input)
        .into_iter()
        .fold((HashSet::from([IVec2::ZERO]), vec![], IVec2::ZERO), |(mut trenches, mut vertices, cur_loc), instruction| {
            vertices.push(cur_loc);
            
            let move_vec = match instruction.0 {
                Up => IVec2::Y,
                Down => IVec2::NEG_Y,
                Right => IVec2::X,
                Left => IVec2::NEG_X
            };
    
            (1..=instruction.1)
                .for_each(|step| {
                    trenches.insert(cur_loc + (step * move_vec));
                });
    
            (trenches, vertices, cur_loc + (instruction.1 * move_vec))
        });
    
    // Shoelace Formula - Trapezoid Formula
    let area = vertices
        .into_iter()
        .tuple_windows()
        .map(|(a, b)| ((a.y + b.y) as isize) * ((a.x - b.x) as isize))
        .sum::<isize>()
        .abs() / 2;
    
    // Pick's Theorem
    let interior_points = ((area + 1) as usize) - (trenches.len() / 2);

    (interior_points + trenches.len()).to_string()
}

#[derive(Debug)]
enum Direction {
    Up,
    Down,
    Right,
    Left,
}

fn parse_input(input: &str) -> Vec<(Direction, i32)> {
    let (_, instructions) = separated_list1(
        line_ending::<&str, Error<&str>>,
        map(preceded(
            take_until("#"),
            delimited(
                tag("#"),
                tuple((
                    map_res(take(5usize), |hex| i32::from_str_radix(hex, 16)),
                    alt((
                        map(tag("0"), |_| Right),
                        map(tag("1"), |_| Down),
                        map(tag("2"), |_| Left),
                        map(tag("3"), |_| Up),
                    ))
                )),
                tag(")")
            )
        ), |(a, b)| (b, a))
    )(input).expect("input can be parsed");

    instructions
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_example() {
        let input = "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
";
        assert_eq!(solve(input), "952408144115");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "111131796939729");
    }
}
