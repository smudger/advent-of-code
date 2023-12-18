use std::collections::{HashSet, VecDeque};
use glam::IVec2;
use itertools::Itertools;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete;
use nom::character::complete::{line_ending, not_line_ending, space1};
use nom::combinator::map;
use nom::error::Error;
use nom::multi::separated_list1;
use nom::sequence::{separated_pair, terminated};
use crate::Direction::{Down, Left, Right, Up};

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

fn solve(input: &str) -> String {
    let (trenches, _) = parse_input(input)
        .into_iter()
        .fold((HashSet::from([IVec2::ZERO]), IVec2::ZERO), |(mut trenches, cur_loc), instruction| {
            let move_vec = match instruction.0 {
                Up => IVec2::Y,
                Down => IVec2::NEG_Y,
                Right => IVec2::X,
                Left => IVec2::NEG_X
            };

            (1..=instruction.1)
                .for_each(|step| {
                    trenches.insert(cur_loc + (step * move_vec));
                });

            (trenches, cur_loc + (instruction.1 * move_vec))
        });
    
    let mut x_trenches = trenches
        .iter()
        .sorted_by(|a, b| Ord::cmp(&a.x, &b.x));
    let min_x = x_trenches.next().expect("there is at least one trench").x - 1;
    let max_x = x_trenches.last().expect("there are at least two trenches").x + 1;
    let mut y_trenches = trenches
        .iter()
        .sorted_by(|a, b| Ord::cmp(&a.y, &b.y));
    let min_y = y_trenches.next().expect("there is at least one trench").y - 1;
    let max_y = y_trenches.last().expect("there are at least two trenches").y + 1;
    
    let starting_point = IVec2{ x: min_x, y: min_y };
    
    let mut queue = VecDeque::from([starting_point]);
    let mut connected_points = HashSet::new();
    
    while let Some(point) = queue.pop_front() {
        if connected_points.contains(&point) {
            continue;
        }
        
        connected_points.insert(point);
        
        let cardinals = vec![
            IVec2::X,
            IVec2::NEG_X,
            IVec2::Y,
            IVec2::NEG_Y
        ];
                
        cardinals
            .into_iter()
            .filter_map(|cardinal| {
                let new_point = point + cardinal;
                
                if new_point.x < min_x {
                    return None;
                }
                
                if new_point.x > max_x {
                    return None;
                }
                
                if new_point.y < min_y {
                    return None;
                }
                
                if new_point.y > max_y {
                    return None;
                }
                
                if trenches.contains(&new_point) {
                    return None;
                }
                
                Some(new_point)
            })
            .for_each(|point| {
                queue.push_back(point);
            });
    };

    // (min_y..=max_y)
    //     .for_each(|y| {
    //         (min_x..=max_x)
    //             .for_each(|x| {
    //                 match trenches.contains(&IVec2{ x, y }) {
    //                     true => print!("#"),
    //                     false => print!("."),
    //                 }
    //             });
    // 
    //         print!("\n");
    //     });
    // println!();
    // (min_y..=max_y)
    //     .for_each(|y| {
    //         (min_x..=max_x)
    //             .for_each(|x| {
    //                 match connected_points.contains(&IVec2{ x, y }) {
    //                     true => print!("#"),
    //                     false => print!("."),
    //                 }
    //             });
    //         
    //         print!("\n");
    //     });
    
    let total_area = (min_x..=max_x).count() * (min_y..=max_y).count();
    let outside_area = connected_points.len();

    (total_area - outside_area).to_string()
}

#[derive(Debug)]
enum Direction {
    Up,
    Down,
    Right,
    Left,
}

fn parse_input(input: &str) -> Vec<(Direction, i32)> {
    let (_, instructions) = separated_list1(
        line_ending::<&str, Error<&str>>,
        separated_pair(
            alt((
                    map(tag("U"), |_| Up),
                    map(tag("D"), |_| Down),
                    map(tag("R"), |_| Right),
                    map(tag("L"), |_| Left),
                )),
                space1,
            terminated(
                complete::i32,
                not_line_ending
            )
        )
    )(input).expect("input can be parsed");
    
    instructions
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_example() {
        let input = "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
";
        assert_eq!(solve(input), "62");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "61661");
    }
}
