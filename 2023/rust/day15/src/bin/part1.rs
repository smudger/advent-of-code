use std::ops::{Add, Mul, Rem};

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

fn solve(input: &str) -> String {
    input
        .split(',')
        .map(|sequence| {
            sequence
                .chars()
                .fold(0u32, hash_char)
        })
        .sum::<u32>()
        .to_string()
}

fn hash_char(current_value: u32, c: char) -> u32 {
    (c as u32)
        .add(current_value)
        .mul(17)
        .rem(256)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_example() {
        let input = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7";
        assert_eq!(solve(input), "1320");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "516469");
    }
}
