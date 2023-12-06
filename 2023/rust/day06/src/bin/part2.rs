use nom::bytes::complete::tag;
use nom::character::complete::{digit1, line_ending, space1};
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
            separated_list1(space1, digit1)),
        line_ending,
        preceded(
            tuple((tag("Distance:"), space1::<&str, Error<&str>>)),
            separated_list1(space1, digit1)),
    ), |(time_vec, record_vec)| {
        let time = time_vec
            .into_iter()
            .fold("".to_owned(), |mut acc, digits| {
                acc.push_str(digits);
                
                acc
            })
            .parse::<usize>()
            .expect("can parse time");
        let record = record_vec
            .into_iter()
            .fold("".to_owned(), |mut acc, digits| {
                acc.push_str(digits);

                acc
            })
            .parse::<usize>()
            .expect("can parse record");
        
        Race { time, record }
    })(input).expect("input can be parsed");

    race
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
