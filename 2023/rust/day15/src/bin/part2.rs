use std::ops::{Add, Mul, Rem};
use indexmap::IndexMap;
use nom::branch::alt;
use nom::character::complete;
use nom::character::complete::{alpha1, char};
use nom::combinator::{map};
use nom::error::Error;
use nom::multi::separated_list1;
use nom::sequence::{preceded, tuple};
use crate::Operation::{Remove, Replace};

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

#[derive(Debug)]
enum Operation {
    Replace(u32),
    Remove,
}

fn solve(input: &str) -> String {
    let (_, sequence) = separated_list1(
        char::<&str, Error<&str>>(','),
        tuple((
            alpha1,
            alt((
                map(char('-'), |_| Remove),
                map(preceded(char('='), complete::u32), |focal_length| Replace(focal_length))
            )))
        )
    )(input).expect("the sequence can be parsed");
    
    sequence
        .into_iter()
        .fold(vec![IndexMap::<&str, u32>::new(); 256], |mut boxes, (label, operation)| {
            let box_index = hash_str(&label);
            
            match operation {
                Remove => {
                    boxes[box_index as usize].shift_remove(label);
                }
                Replace(focal_length) => {
                    *boxes[box_index as usize]
                        .entry(label)
                        .or_insert(focal_length) = focal_length;
                }
            };
            
            boxes
        })
        .into_iter()
        .enumerate()
        .flat_map(|(box_index, current_box)| {
            current_box
                .values()
                .enumerate()
                .map(|(lens_index, focal_length)| (box_index + 1) * (lens_index + 1) * (*focal_length as usize))
                .collect::<Vec<_>>()
        })
        .sum::<usize>()
        .to_string()
}

fn hash_str(input: &str) -> u32 {
    input
        .chars()
        .fold(0, hash_char)
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
        assert_eq!(solve(input), "145");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "221627");
    }
}
