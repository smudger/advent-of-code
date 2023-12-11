use itertools::Itertools;

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input, 1_000_000);
    dbg!(output);
}

fn solve(input: &str, multiplier: usize) -> String {
    let universe = input
        .lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let rows_to_expand = (
        0..input.lines().count()
    )
        .into_iter()
        .filter_map(|index| {
            universe[index]
                .iter()
                .all(|c| *c == '.')
                .then_some(index)
        })
        .collect::<Vec<_>>();
    
    let cols_to_expand = (
        0..input.lines().next().expect("there is at least one row").chars().count()
    )
        .into_iter()
        .filter_map(|index| {
            universe
                .iter()
                .map(|row| row[index])
                .all(|c| c == '.')
                .then_some(index)
        })
        .collect::<Vec<_>>();

    universe
        .into_iter()
        .enumerate()
        .flat_map(|(y, row)| {
            row
                .into_iter()
                .enumerate()
                .filter_map(move |(x, c)| (c == '#')
                    .then_some((x as isize, y as isize))
                )
        })
        .combinations(2)
        .map(|combo| {
            let (min_x, max_x) = combo
                .iter()
                .map(|(x, _)| *x as usize)
                .sorted()
                .collect_tuple()
                .expect("can be collected as a tuple");
            let x_range = min_x..max_x;
            let (min_y, max_y) = combo
                .iter()
                .map(|(_, y)| *y as usize)
                .sorted()
                .collect_tuple()
                .expect("can be collected as a tuple");
            let y_range = min_y..max_y;
            let mut short_path = ((combo[1].0 - combo[0].0).abs() + (combo[1].1 - combo[0].1).abs()) as usize;
            
            short_path += rows_to_expand
                .iter()
                .filter_map(|index| y_range.contains(index).then_some(multiplier - 1))
                .sum::<usize>();
            
            short_path += cols_to_expand
                .iter()
                .filter_map(|index| x_range.contains(index).then_some(multiplier - 1))
                .sum::<usize>();
            
            short_path
        })
        .sum::<usize>()
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_first_example() {
        let input = "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
";
        assert_eq!(solve(input, 2), "374");
    }

    #[test]
    fn it_solves_the_second_example() {
        let input = "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
";
        assert_eq!(solve(input, 10), "1030");
    }

    #[test]
    fn it_solves_the_third_example() {
        let input = "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
";
        assert_eq!(solve(input, 100), "8410");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input, 1_000_000), "597714117556");
    }
}
