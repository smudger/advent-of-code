use std::collections::HashMap;
use itertools::FoldWhile::{Continue, Done};
use itertools::Itertools;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, line_ending};
use nom::combinator::{map, opt};
use nom::IResult;
use nom::multi::{count, fold_many0, many1};
use nom::sequence::{delimited, separated_pair, terminated};
use crate::Direction::{Left, Right};

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

#[derive(Debug)]
enum Direction {
    Left,
    Right
}

fn solve(input: &str) -> String {
    let (directions, maze) = parse_input(input);
    directions
        .iter()
        .cycle()
        .fold_while(("AAA", 0), |acc, direction| {
            let options = maze
                .get(acc.0)
                .expect("the current location is in the maze");
            
            let next_location = match direction {
                Left => options.0,
                Right => options.1
            };

            match next_location {
                "ZZZ" => Done((next_location, acc.1 + 1)),
                &_ => Continue((next_location, acc.1 + 1))
            }
        })
        .into_inner()
        .1
        .to_string()
}

fn parse_input(input: &str) -> (Vec<Direction>, HashMap<&str, (&str, &str)>) {
    let (_, (directions, maze)) = separated_pair(
        parse_directions,
        count(line_ending, 2),
            parse_maze
    )(input).expect("the input can be parsed");

    (directions, maze)
}

fn parse_directions(input: &str) -> IResult<&str, Vec<Direction>> {
    many1(alt((
        map(tag("L"), |_| Left),
        map(tag("R"), |_| Right),
    )))(input)
}

fn parse_maze(input: &str) -> IResult<&str, HashMap<&str, (&str, &str)>> {
    fold_many0(
        terminated(
            separated_pair(
                alpha1,
                tag(" = "),
                delimited(
                    tag("("),
                    separated_pair(
                       alpha1,
                        tag(", "),
                        alpha1
                    ),
                    tag(")")
                )
            ),
            opt(line_ending)
        ),
        HashMap::new,
        |mut acc, (key, value)| {
            acc.insert(key, value);
            
            acc
        }
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_first_example() {
        let input = "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
";
        assert_eq!(solve(input), "2");
    }
    #[test]
    fn it_solves_the_second_example() {
        let input = "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
";
        assert_eq!(solve(input), "6");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "13771");
    }
}
