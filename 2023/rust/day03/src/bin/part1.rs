use day03::{parse_markers, parse_part_numbers};

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
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
