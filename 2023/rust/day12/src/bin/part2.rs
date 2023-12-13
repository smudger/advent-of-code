use std::collections::VecDeque;
use indicatif::ParallelProgressIterator;
use memoize::memoize;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete;
use nom::character::complete::{line_ending, space1};
use nom::combinator::map;
use nom::error::Error;
use nom::multi::{many1, separated_list1};
use nom::sequence::separated_pair;
use rayon::prelude::IntoParallelIterator;
use rayon::iter::ParallelIterator;
use crate::Spring::{Damaged, Operational, Unknown};

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

fn solve(input: &str) -> String {
    parse_input(input)
        .into_par_iter()
        .progress()
        .map(|row| {
            let expanded_springs = vec![row.0; 5];
            let expanded_springs = expanded_springs
                .into_iter()
                .reduce(|mut acc, mut item| {
                    acc.push(Unknown);
                    acc.append(&mut item);

                    acc
                })
                .expect("the springs can be expanded");

            let expanded_groups = vec![row.1; 5];
            let expanded_groups = expanded_groups
                .into_iter()
                .reduce(|mut acc, mut item| {
                    acc.append(&mut item);

                    acc
                })
                .expect("the groups can be expanded");
            
            calculate_combinations(&(expanded_springs, expanded_groups))
        })
        .sum::<usize>()
        .to_string()
}

fn calculate_combinations(initial_row: &(Vec<Spring>, Vec<usize>)) -> usize {
    let mut queue = VecDeque::from([initial_row.clone()]);
    let mut combinations = 0;
    
    while let Some((springs, damaged_groups)) = queue.pop_front() {
        let (additions, extra_combos) = consider(springs, damaged_groups);
        
        combinations += extra_combos;
        additions.into_iter().for_each(|addition| queue.push_back(addition));
    }

    combinations
}

#[memoize]
fn consider(mut springs: Vec<Spring>, damaged_groups: Vec<usize>) -> (Vec<(Vec<Spring>, Vec<usize>)>, usize) {
    let Some(group_length) = damaged_groups.get(0) else {
        if springs
            .into_iter()
            .all(|s| s != Damaged) {
            return (vec![], 1);
        }
        
        return (vec![], 0);
    };

    if springs.len() < &damaged_groups.iter().sum::<usize>() + &damaged_groups.len() - 1 {
        return (vec![], 0);
    }

    let Some(next_spring) = springs.get(0) else {
        return (vec![], 0);
    };

    return match next_spring {
        Operational => {
            (vec![(springs[1..].to_owned(), damaged_groups)], 0)
        }
        Unknown => {
            let mut additions = vec![];
            additions.push((springs[1..].to_owned(), damaged_groups.clone()));
            springs[0] = Damaged;
            additions.push((springs, damaged_groups));

            (additions, 0)
        }
        Damaged => {
            if springs.len() < *group_length {
                return (vec![], 0);
            }

            let spring_group = &springs[..*group_length];

            if spring_group.iter().any(|s| *s == Operational) {
                return (vec![], 0);
            }

            let Some(next_spring) = springs.get(*group_length) else {
                if damaged_groups.len() == 1 {
                    return (vec![], 1);
                }
                
                return (vec![], 0);
            };

            match next_spring {
                Damaged => (vec![], 0),
                Unknown | Operational => (vec![(springs[(group_length + 1)..].to_owned(), damaged_groups[1..].to_owned())], 0)
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
enum Spring {
    Operational,
    Damaged,
    Unknown
}

fn parse_input(input: &str) -> Vec<(Vec<Spring>, Vec<usize>)> {
    let (_, rows) = separated_list1(
        line_ending::<&str, Error<&str>>,
        separated_pair(
            many1(alt((
                map(complete::char('.'), |_| Operational),
                map(complete::char('#'), |_| Damaged),
                map(complete::char('?'), |_| Unknown),
            ))),
            space1,
            separated_list1(tag(","), map(complete::u32, |num| num as usize))
        )
    )(input)
        .expect("the input can be parsed");

    rows
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_first_example() {
        let input = "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
";
        assert_eq!(solve(input), "525152");
    }

    // #[test]
    // fn it_solves_the_puzzle() {
    //     let input = include_str!("./input.txt");
    //     assert_eq!(solve(input), "7506");
    // }
}
