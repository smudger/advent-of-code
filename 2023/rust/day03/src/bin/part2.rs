use day03::{parse_markers, parse_part_numbers};

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

fn solve(input: &str) -> String {
    let markers = parse_markers(input);
    let part_numbers = parse_part_numbers(input);
    markers
        .iter()
        .filter(|marker| marker.symbol == '*')
        .filter_map(|marker| {
            let gear_parts = part_numbers
                .iter()
                .filter_map(|part_number| {
                    match part_number
                        .bounding_box()
                        .iter()
                        .any(|coordinate| vec![marker.location].contains(&coordinate)) {
                        false => None,
                        true => Some(part_number.number)
                    }
                })
                .collect::<Vec<_>>();
            
            match gear_parts.iter().count() {
                2 => Some(gear_parts.iter().product::<u32>()),
                _ => None
            }
        })
        .sum::<u32>()
        .to_string()
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
        assert_eq!(solve(input), "467835");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "80253814");
    }
}
