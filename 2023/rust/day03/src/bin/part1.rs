use std::ops::Add;
use itertools::Itertools;

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

#[derive(Debug, PartialEq, Copy, Clone)]
struct Coordinate(usize, usize);

#[derive(Debug)]
struct Marker {
    symbol: char,
    location: Coordinate,
}

#[derive(Debug)]
struct PartNumber {
    number: u32,
    location: Coordinate,
}

impl PartNumber {
    fn bounding_box(&self) -> Vec<Coordinate> {
        let length = self.number.to_string().chars().count();
        let min_x = self.location.0
            .checked_sub(1)
            .unwrap_or(self.location.0);
        let min_y = self.location.1
            .checked_sub(1)
            .unwrap_or(self.location.1);
        (min_x..=self.location.0.add(length))
            .flat_map(move |x| {
                (min_y..=self.location.1.add(1))
                    .map(move |y| Coordinate(x, y))
            })
            .collect::<Vec<_>>()
    }
}

fn solve(input: &str) -> String {
    let markers = parse_markers(input);
    let marker_coordinates: Vec<_> = markers
        .iter()
        .map(|marker| marker.location)
        .collect();
    
    parse_part_numbers(input)
        .iter()
        .filter_map(|part_number| {
            match part_number
                .bounding_box()
                .iter()
                .any(|coordinate| marker_coordinates.contains(&coordinate)) {
                false => None,
                true => Some(part_number.number)
            }
        })
        .sum::<u32>()
        .to_string()
}

fn parse_part_numbers(input: &str) -> Vec<PartNumber> {
    input
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line
                .chars()
                .enumerate()
                .group_by(|(_, c)| c.is_digit(10))
                .into_iter()
                .filter_map(|(is_digit, group)| {
                    match is_digit {
                        false => None,
                        true => {
                            let mut group = group.peekable();
                            let x = group.peek().expect("there is a digit in the group").0;
                            Some(PartNumber {
                                number: group.map(|(_, digit)| digit).collect::<String>().parse::<u32>().expect("part number can be parsed as u32"),
                                location: Coordinate(x, y)
                            })
                        }
                    }
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

fn parse_markers(input: &str) -> Vec<Marker> {
    input
        .lines()
        .enumerate()
        .flat_map(|(y, row)| {
            row
                .chars()
                .enumerate()
                .map(move |(x, symbol)| Marker {
                    symbol,
                    location: Coordinate(x, y)
                })
        })
        .filter(|marker| {
            !marker.symbol.is_digit(10)
            && marker.symbol != '.'
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_example() {
        let input = "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..";
        assert_eq!(solve(input), "4361");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "530495");
    }
}
