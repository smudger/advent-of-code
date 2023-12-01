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
            let mut locations = NUMBERS
                .iter()
                .map(|(digit, text)| {
                    let first_index = match (line.find(digit), line.find(text)) {
                        (Some(i), Some(j)) => Some(min(i, j)),
                        (Some(i), None) => Some(i),
                        (None, Some(j)) => Some(j),
                        (None, None) => None,
                    };
                    
                    let last_index = match (line.rfind(digit), line.rfind(text)) {
                        (Some(i), Some(j)) => Some(max(i, j)),
                        (Some(i), None) => Some(i),
                        (None, Some(j)) => Some(j),
                        (None, None) => None,
                    };

                    (first_index, last_index)
                })
                .enumerate()
                .filter_map(|(number, (first_index, last_index))| match (first_index, last_index) {
                    (Some(first), Some(last)) => Some((number, first, last)),
                    _ => None
                })
                .collect::<Vec<_>>();

            locations.sort_by(|a, b| a.1.cmp(&b.1));
            let first_number = locations.first().unwrap().0;
            locations.sort_by(|a, b| a.2.cmp(&b.2));
            let last_number = locations.last().unwrap().0;
                
            (first_number, last_number)
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
