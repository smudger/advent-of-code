use itertools::Itertools;

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

fn solve(input: &str) -> String {
    let row_expanded_universe = input
        .lines()
        .flat_map(|line| match line.chars().all(|c| c == '.') {
            true => vec![line.chars().collect::<Vec<_>>(), line.chars().collect::<Vec<_>>()],
            false => vec![line.chars().collect::<Vec<_>>()]
        })
        .collect::<Vec<_>>();
    
    let cols_to_expand = (
        0..input.lines().next().expect("there is at least one row").chars().count()
    )
        .into_iter()
        .filter_map(|index| {
            row_expanded_universe
                .iter()
                .map(|row| row[index])
                .all(|c| c == '.')
                .then_some(index)
        })
        .collect::<Vec<_>>();
    
    let expanded_universe = row_expanded_universe
        .iter()
        .map(|row| {
            row
                .iter()
                .enumerate()
                .flat_map(|(index, c)| match cols_to_expand.contains(&index) {
                    true => vec![c, c],
                    false => vec![c]
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    
    expanded_universe
        .iter()
        .enumerate()
        .flat_map(|(y, row)| {
            row
                .into_iter()
                .enumerate()
                .filter_map(move |(x, c)| (**c == '#')
                    .then_some((x as i32, y as i32))
                )
        })
        .combinations(2)
        .map(|combo| (combo[1].0 - combo[0].0).abs() + (combo[1].1 - combo[0].1).abs())
        .sum::<i32>()
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_example() {
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
        assert_eq!(solve(input), "374");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "9312968");
    }
}
