use nom::bytes::complete::tag;
use nom::character::complete::{digit1, line_ending, space0, space1};
use nom::combinator::{map, map_res};
use nom::error::Error;
use nom::IResult;
use nom::multi::{fold_many1};
use nom::sequence::{preceded, separated_pair, terminated, tuple};

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

fn solve(input: &str) -> String {
    let race = parse_input(input);

    (0..=race.time)
        .filter(|time_holding_button| {
            (time_holding_button * (race.time - time_holding_button)) > race.record
        })
        .count()
        .to_string()
}

#[derive(Debug)]
struct Race {
    time: usize,
    record: usize
}

fn parse_input(input: &str) -> Race {
    let (_, race) = map(separated_pair(
        preceded(
            preceded(tag("Time:"), space1::<&str, Error<&str>>),
            parse_digits
            ),
        line_ending,
        preceded(
            tuple((tag("Distance:"), space1::<&str, Error<&str>>)),
            parse_digits
        )
    ), |(time, record)| {
        Race { time, record }
    })(input).expect("input can be parsed");

    race
}

fn parse_digits(digits: &str) -> IResult<&str, usize> {
    map_res(
        fold_many1(
            terminated(digit1, space0::<&str, Error<&str>>),
            || "".to_owned(),
            |mut acc, digits| {
                acc.push_str(digits);
        
                acc
            }
        ),
        |digits| digits.parse::<usize>()
    )(digits)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_example() {
        let input = "Time:      7  15   30
Distance:  9  40  200";
        assert_eq!(solve(input), "71503");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "40651271");
    }
}
