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
            
            calculate_combinations(expanded_springs, expanded_groups)
        })
        .sum::<usize>()
        .to_string()
}

#[memoize]
fn calculate_combinations(springs: Vec<Spring>, groups: Vec<usize>) -> usize {
    let Some(group_length) = groups.get(0) else {
        if springs
            .into_iter()
            .all(|s| s != Damaged) {
            return 1;
        }

        return 0;
    };

    if springs.len() < &groups.iter().sum::<usize>() + &groups.len() - 1 {
        return 0;
    }

    let Some(next_spring) = springs.get(0) else {
        return 0;
    };

    return match next_spring {
        Operational => {
            calculate_combinations(springs[1..].to_owned(), groups)
        }
        Unknown => {
            let mut springs = springs;
            springs[0] = Damaged;
            calculate_combinations(springs[1..].to_owned(), groups.clone())
                + 
            calculate_combinations(springs, groups)
        }
        Damaged => {
            if springs.len() < *group_length {
                return 0;
            }

            let spring_group = &springs[..*group_length];

            if spring_group.iter().any(|s| *s == Operational) {
                return 0;
            }

            let Some(next_spring) = springs.get(*group_length) else {
                if groups.len() == 1 {
                    return 1;
                }

                return 0;
            };

            match next_spring {
                Damaged => 0,
                Unknown | Operational => calculate_combinations(springs[(group_length + 1)..].to_owned(), groups[1..].to_owned())
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

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "548241300348335");
    }
}
