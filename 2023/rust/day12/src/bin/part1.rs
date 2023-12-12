use std::collections::VecDeque;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete;
use nom::character::complete::{line_ending, space1};
use nom::combinator::map;
use nom::error::Error;
use nom::multi::{many1, separated_list1};
use nom::sequence::separated_pair;
use crate::Spring::{Damaged, Operational, Unknown};

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

fn solve(input: &str) -> String {
    parse_input(input)
        .into_iter()
        .map(|row| {
            let mut queue = VecDeque::from([row]);
            let mut combinations = 0;
            
            while let Some((mut springs, damaged_groups)) = queue.pop_front() {
                let Some(group_length) = damaged_groups.get(0) else {
                    springs
                        .into_iter()
                        .all(|s| s != Damaged)
                        .then(|| combinations += 1);
                    continue;
                };
                
                let Some(next_spring) = springs.get(0) else {
                    continue;
                };
                
                match next_spring {
                    Operational => {
                        queue.push_back((springs[1..].to_owned(), damaged_groups))
                    }
                    Unknown => {
                        queue.push_back((springs[1..].to_owned(), damaged_groups.clone()));
                        springs[0] = Damaged;
                        queue.push_back((springs, damaged_groups))
                    }
                    Damaged => {
                        if springs.len() < *group_length {
                            continue;
                        }

                        let spring_group = &springs[..*group_length];
                        
                        if spring_group.iter().any(|s| *s == Operational) {
                            continue;
                        }
                        
                        let Some(next_spring) = springs.get(*group_length) else {
                            (damaged_groups.len() == 1).then(|| combinations += 1);
                            continue;
                        };
                        
                        match next_spring {
                            Damaged => continue,
                            Unknown | Operational => queue.push_back((springs[(group_length + 1)..].to_owned(), damaged_groups[1..].to_owned()))
                        };
                    }
                }
            }
            
            combinations
        })
        .sum::<u32>()
        .to_string()
}

#[derive(Debug, PartialEq, Clone)]
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
    fn it_solves_the_example() {
        let input = "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
";
        assert_eq!(solve(input), "21");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "7506");
    }
}
