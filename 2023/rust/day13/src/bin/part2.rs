fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

fn solve(input: &str) -> String {
    input
        .split("\n\n")
        .into_iter()
        .map(|pattern| {
            let rows = pattern
                .lines()
                .map(|row| row.to_string())
                .collect::<Vec<_>>();

            if let Some(row_reflection_index) = calculate_reflection_index(&rows) {
                return row_reflection_index * 100;
            }

            calculate_reflection_index(&transpose(rows))
                .expect("there is a row or column reflection index")
        })
        .sum::<u32>()
        .to_string()
}

fn transpose(rows: Vec<String>) -> Vec<String> {
    let row_length = rows[0].len();

    let mut row_vecs = rows
        .into_iter()
        .map(|row| row.chars().into_iter().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    (0..row_length)
        .map(|_| {
            row_vecs
                .iter_mut()
                .map(|chars| chars.pop().unwrap())
                .collect()
        })
        .collect::<Vec<_>>()
        .into_iter()
        .rev()
        .collect()
}

fn calculate_reflection_index(rows: &Vec<String>) -> Option<u32> {
    (1..rows.len())
        .into_iter()
        .find_map(|index| {
            let (start, end) = rows.split_at(index);

            let diff_counts = start
                .into_iter()
                .rev()
                .zip(end.into_iter())
                .map(|(a, b)| {
                    a.chars()
                        .zip(b.chars())
                        .filter(|(a, b)| a != b)
                        .count()
                })
                .collect::<Vec<_>>();

            (diff_counts.iter().filter(|count| *count > &1).count() == 0
                && diff_counts.iter().filter(|count| *count == &1).count() == 1)
                .then_some(index as u32)
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_example() {
        let input = "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
";
        assert_eq!(solve(input), "400");
    }
    
    #[test]
    fn it_solves_the_second_example() {
        let input = ".##.#.#
###..##
#..#.#.
#..#...
#..#...
#.##.#.
###..##
.##.#.#
.##.#.#
###..##
#.##.#.
#..#...
#..#...
#..#.#.
###..##
.##.#.#
....#..
";
        assert_eq!(solve(input), "400");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "37478");
    }
}
