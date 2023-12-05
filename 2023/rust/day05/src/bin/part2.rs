use std::cmp::{max, min};
use std::collections::{BTreeMap};
use std::ops::Range;
use nom::bytes::complete::tag;
use nom::character::complete::{digit1, line_ending, not_line_ending, space0, space1};
use nom::combinator::{map, map_res};
use nom::IResult;
use nom::multi::{count, separated_list1};
use nom::sequence::{pair, preceded, separated_pair, terminated, tuple};
use std::str::FromStr;
use itertools::Itertools;

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

fn solve(input: &str) -> String {
    let (seeds, all_modifications) = parse_input(input);
    
    let sorted_modifications = all_modifications
        .into_iter()
        .map(|mut modifications| {
            modifications.sort_by(|a, b| a.source_range.start.cmp(&b.source_range.start));
            
            modifications
        })
        .collect::<Vec<_>>();

    seeds
        .iter()
        .map(|seed_range| {
            sorted_modifications
                .iter()
                .fold(vec![seed_range.clone()], |acc, modifications| {
                    acc
                        .into_iter()
                        .flat_map(|seed_range| {
                            let modification_map = modifications
                                .iter()
                                .fold(BTreeMap::new(), |mut acc, modification| {
                                    acc.insert(modification.source_range.start, modification.modifier);
                                    acc.entry(modification.source_range.end).or_insert(0);
                                    acc
                                });
                            let first_index = modification_map.keys().next().expect("first index exists");
                            let last_index = modification_map.keys().last().expect("last index exists");
                            let mut new_modification_map = modification_map
                                .iter()
                                .tuple_windows()
                                .map(|((start, modifier), (end, _))| {
                                    (max(*start, seed_range.start)..min(*end, seed_range.end), *modifier)
                                })
                                .collect::<Vec<_>>();
                            
                            new_modification_map.push((seed_range.start..min(*first_index, seed_range.end), 0 as isize));
                            new_modification_map.push((max(*last_index, seed_range.start)..seed_range.end, 0 as isize));
                            
                            let result = new_modification_map.iter().filter_map(|(range, modifier)| {
                               match range.is_empty() {
                                   true => None,
                                   false => Some((((range.start as isize) + modifier) as usize)..(((range.end as isize) + modifier) as usize))
                               } 
                            }).collect::<Vec<_>>();
                            
                            result
                        })
                        .collect::<_>()
                })
                .iter()
                .filter_map(|seed_range| match seed_range.is_empty() {
                    true => None,
                    false => Some(seed_range.start)
                })
                .min()
                .expect("there is a minimum per seed range")
        })
        .min()
        .expect("there is a minimum overall")
        .to_string()
}

#[derive(Debug, Clone)]
struct Modification {
    modifier: isize,
    source_range: Range<usize>
}

fn parse_input(input: &str) -> (Vec<Range<usize>>, Vec<Vec<Modification>>) {
    let (_, result) = separated_pair(parse_seeds, count(line_ending, 2), parse_modifications)
        (input)
        .expect("almanac can be parsed");

    result
}

fn parse_seeds(input: &str)-> IResult<&str, Vec<Range<usize>>> {
    preceded(
        tag("seeds: "),
        separated_list1(
            space1,
            map(separated_pair(
                map_res(digit1, usize::from_str),
                space1,
                map_res(digit1, usize::from_str)
            ), |(start, length)| start..(start + length))
        ))(input)
}

fn parse_modifications(input: &str)-> IResult<&str, Vec<Vec<Modification>>> {
    separated_list1(
        count(line_ending, 2),
        preceded(
            pair(not_line_ending, line_ending),
            separated_list1(line_ending, parse_modification)
        )
    )(input)
}

fn parse_modification(input: &str) -> IResult<&str, Modification> {
    map(tuple((
        terminated(map_res(digit1, usize::from_str), space0),
        terminated(map_res(digit1, usize::from_str), space0),
        terminated(map_res(digit1, usize::from_str), space0)
    )), |(destination_start, source_start, range_length)| {
        Modification {
            modifier: (destination_start as isize) - (source_start as isize),
            source_range: source_start..(source_start + range_length),
        }
    })(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_example() {
        let input = "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4";
        assert_eq!(solve(input), "46");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "17729182");
    }
}
