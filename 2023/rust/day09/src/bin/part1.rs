use std::str::FromStr;
use itertools::FoldWhile::{Continue, Done};
use itertools::Itertools;

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

fn solve(input: &str) -> String {
    input
        .lines()
        .map(|line| {
            let sequence = line
                .split_whitespace()
                .map(i32::from_str)
                .flatten()
                .collect::<Vec<_>>();

            let sequences = (0..)
                .into_iter()
                .fold_while(vec![sequence], |mut acc, _| {
                    let diffs = acc
                        .last()
                        .expect("there is a sequence to consider")
                        .windows(2)
                        .map(|w| w[1] - w[0])
                        .collect::<Vec<_>>();
                    
                    acc.push(diffs.clone());
                    
                    match diffs.windows(2).all(|w| w[0] == w[1]) {
                        true => Done(acc),
                        false => Continue(acc)
                    }
                })
                .into_inner();
            
            sequences
                .iter()
                .rev()
                .fold(0i32, |acc, seq| {
                    acc + seq.last().expect("each sequence contains at least one element")
                })
        })
        .sum::<i32>()
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_example() {
        let input = "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45";
        assert_eq!(solve(input), "114");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "1972648895");
    }
}
