use itertools::Itertools;
use std::ops::Add;

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Coordinate(usize, usize);

#[derive(Debug)]
pub struct Marker {
    pub symbol: char,
    pub location: Coordinate,
}

#[derive(Debug)]
pub struct PartNumber {
    pub number: u32,
    pub location: Coordinate,
}

impl PartNumber {
    pub fn bounding_box(&self) -> Vec<Coordinate> {
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

pub fn parse_part_numbers(input: &str) -> Vec<PartNumber> {
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

pub fn parse_markers(input: &str) -> Vec<Marker> {
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


