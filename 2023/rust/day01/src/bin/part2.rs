use std::cmp::{max, min};

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

const NUMBERS: [(&str, &str); 10] = [
    ("0", "zero"),
    ("1", "one"),
    ("2", "two"),
    ("3", "three"),
    ("4", "four"),
    ("5", "five"),
    ("6", "six"),
    ("7", "seven"),
    ("8", "eight"),
    ("9", "nine"),
];

fn solve(input: &str) -> String {
    input
        .lines()
        .map(|line| {
            let mut first_indexes = NUMBERS
                .iter()
                .enumerate()
                .map(|(index, (digit, text))| {
                    match (line.find(digit), line.find(text)) {
                        (Some(i), Some(j)) => (index, Some(min(i, j))),
                        (Some(i), None) => (index, Some(i)),
                        (None, Some(j)) => (index, Some(j)),
                        (None, None) => (index, None),
                    }
                })
                .filter(|(_, location)| location.is_some())
                .collect::<Vec<(usize, Option<usize>)>>();
            first_indexes.sort_by(|a, b| a.1.cmp(&b.1));
            
            let mut last_indexes = NUMBERS
                .iter()
                .enumerate()
                .map(|(index, (digit, text))| {
                    match (line.rfind(digit), line.rfind(text)) {
                        (Some(i), Some(j)) => (index, Some(max(i, j))),
                        (Some(i), None) => (index, Some(i)),
                        (None, Some(j)) => (index, Some(j)),
                        (None, None) => (index, None),
                    }
                })
                .filter(|(_, location)| location.is_some())
                .collect::<Vec<(usize, Option<usize>)>>();
            last_indexes.sort_by(|a, b| a.1.cmp(&b.1));

            (first_indexes.first().unwrap().0, last_indexes.last().unwrap().0)
        })
        .map(|(a, b)| (a * 10) + b)
        .sum::<usize>()
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_example() {
        let input = "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
9seveneight9
7pqrstsixteen";
        assert_eq!(solve(input), "380");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "52834");
    }
}
