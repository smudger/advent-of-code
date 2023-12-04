use std::collections::HashSet;
use std::ops::Sub;

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

fn solve(input: &str) -> String {
    input
        .lines()
        .fold(input.lines().map(|_| 1).collect::<Vec<usize>>(), |mut acc, card| {
            match card
                .split(": ")
                .last()
                .expect("the card contains a ': '")
                .split("| ")
                .map(|group| {
                    group
                        .split_whitespace()
                        .map(|number| number.parse::<u32>().expect("each element is a u32"))
                        .collect::<HashSet<u32>>()
                })
                .reduce(|acc, group| {
                    acc.intersection(&group).map(|number| *number).collect::<HashSet<u32>>()
                })
                .expect("there are two groups")
                .len() {
                0 => acc,
                wins => {
                    let card_index = card
                        .split(": ")
                        .next()
                        .expect("the card contains a ': '")
                        .split_whitespace()
                        .last()
                        .expect("the card id is of the form 'Card X'")
                        .parse::<usize>()
                        .expect("the card id is a number")
                        .sub(1);
                    let times_won = *acc
                        .get(card_index)
                        .expect("the card index is present in the accumulator");
                    (1..=wins).for_each(|index| {
                        acc[card_index + index] += times_won
                    });
                    
                    acc
                }
            }
        })
        .iter()
        .sum::<usize>()
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_example() {
        let input = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11";
        assert_eq!(solve(input), "30");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "14624680");
    }
}
