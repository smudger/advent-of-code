use nom::bytes::complete::tag;
use nom::character::complete;
use nom::character::complete::{line_ending, space1};
use nom::combinator::map;
use nom::error::Error;
use nom::multi::separated_list1;
use nom::sequence::{preceded, separated_pair, tuple};

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

fn solve(input: &str) -> String {
    let races = parse_input(input);
    
    races
        .into_iter()
        .map(|race| {
            (0..=race.time)
                .filter(|time_holding_button| {
                    (time_holding_button * (race.time - time_holding_button)) > race.record
                })
                .count()
        })
        .product::<usize>()
        .to_string()
}

#[derive(Debug)]
struct Race {
    time: u32,
    record: u32
}

fn parse_input(input: &str) -> Vec<Race> {
    let (_, races) = map(separated_pair(
        preceded(
            preceded(tag("Time:"), space1::<&str, Error<&str>>),
            separated_list1(space1, complete::u32)),
        line_ending,
        preceded(
            tuple((tag("Distance:"), space1::<&str, Error<&str>>)),
            separated_list1(space1, complete::u32)),
    ), |(times, records)| {
        times
            .into_iter()
            .zip(records)
            .map(|(time, record)| Race { time, record })
            .collect::<Vec<_>>()
    })(input).expect("input can be parsed");
    
    races
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_example() {
        let input = "Time:      7  15   30
Distance:  9  40  200";
        assert_eq!(solve(input), "288");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "5133600");
    }
}
