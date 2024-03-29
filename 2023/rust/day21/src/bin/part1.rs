use std::collections::HashSet;
use glam::IVec2;

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input, 64);
    dbg!(output);
}

fn solve(input: &str, steps: u32) -> String {
    let grid = input
        .lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    
    let starting_position = grid
        .iter()
        .enumerate()
        .find_map(|(y, row)| {
            row
                .iter()
                .enumerate()
                .find_map(|(x, c)| (*c == 'S').then_some(x))
                .and_then(|x| Some(IVec2{ x: x as i32, y: y as i32 }))
        })
        .expect("there is a starting position");

    (1..=steps)
        .into_iter()
        .fold(HashSet::from([starting_position]), |positions, _| {
            positions
                .into_iter()
                .flat_map(|position| {
                    let dirs = vec![
                        position + IVec2::X,
                        position + IVec2::NEG_X,
                        position + IVec2::Y,
                        position + IVec2::NEG_Y,
                    ];
                    
                    dirs
                        .into_iter()
                        .filter(|dir| {
                            let Some(c) = grid
                                .get(dir.y as usize)
                                .and_then(|row| row.get(dir.x as usize)) else {
                                return false;
                            };
                            
                            *c != '#'
                        })
                })
                .collect::<HashSet<_>>()
        })
        .len()
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_example() {
        let input = "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
";
        assert_eq!(solve(input, 6), "16");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input, 64), "3617");
    }
}
