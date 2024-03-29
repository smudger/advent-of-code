use std::collections::{HashMap, HashSet};
use glam::IVec2;

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input, 26501365);
    dbg!(output);
}

fn solve(input: &str, steps: u32) -> String {
    let grid = input
        .lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    
    let rows = grid.len() as i32;
    let cols = grid[0].len() as i32;

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
    
    let mod_vec = IVec2 { x: cols, y: rows };

    (1..=steps)
        .into_iter()
        .fold(HashMap::from([(starting_position, HashSet::from([IVec2::ZERO]))]), |positions, _| {
            let result = positions
                .into_iter()
                .fold(HashMap::new(), |mut new_positions, (position, quots)| {
                    let dirs = vec![
                        IVec2::X,
                        IVec2::NEG_X,
                        IVec2::Y,
                        IVec2::NEG_Y,
                    ];

                    dirs
                        .into_iter()
                        .map(|dir| (
                            (position + dir).rem_euclid(mod_vec),
                            (position + dir).div_euclid(mod_vec),
                        ))
                        .filter(|(rem, _quot)| {
                            let c = grid
                                .get(rem.y as usize)
                                .and_then(|row| row.get(rem.x as usize))
                                .expect("can find grid entry");

                            c != &'#'
                        })
                        .map(|(rem, quot)| {
                            let quots_to_insert = quots
                                .iter()
                                .map(|existing_quot| quot + *existing_quot)
                                .collect::<HashSet<_>>();

                            (rem, quots_to_insert)
                        })
                        .for_each(|(rem, quots_to_insert)| {
                            new_positions
                                .entry(rem)
                                .and_modify(|entry: &mut HashSet<IVec2>| {
                                    entry.extend(&quots_to_insert);
                                })
                                .or_insert(quots_to_insert);
                        });
                    
                    new_positions
                });
            
            dbg!(&result
                .iter()
                .filter(|(_, vals)| vals.contains(&IVec2 { x: 2, y: 0 }))
                .count()
            );
            
            result
        })
        .values()
        .map(|quots| quots.len())
        .sum::<usize>()
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

//     #[test]
//     fn it_solves_the_first_example() {
//         let input = "...........
// .....###.#.
// .###.##..#.
// ..#.#...#..
// ....#.#....
// .##..S####.
// .##..#...#.
// .......##..
// .##.#.####.
// .##..##.##.
// ...........
// ";
//         assert_eq!(solve(input, 6), "16");
//     }
// 
//     #[test]
//     fn it_solves_the_second_example() {
//         let input = "...........
// .....###.#.
// .###.##..#.
// ..#.#...#..
// ....#.#....
// .##..S####.
// .##..#...#.
// .......##..
// .##.#.####.
// .##..##.##.
// ...........
// ";
//         assert_eq!(solve(input, 10), "50");
//     }
// 
//     #[test]
//     fn it_solves_the_third_example() {
//         let input = "...........
// .....###.#.
// .###.##..#.
// ..#.#...#..
// ....#.#....
// .##..S####.
// .##..#...#.
// .......##..
// .##.#.####.
// .##..##.##.
// ...........
// ";
//         assert_eq!(solve(input, 50), "1594");
//     }
// 
    #[test]
    fn it_solves_the_fourth_example() {
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
        assert_eq!(solve(input, 100), "6536");
    }

//     #[test]
//     fn it_solves_the_fifth_example() {
//         let input = "...........
// .....###.#.
// .###.##..#.
// ..#.#...#..
// ....#.#....
// .##..S####.
// .##..#...#.
// .......##..
// .##.#.####.
// .##..##.##.
// ...........
// ";
//         assert_eq!(solve(input, 500), "167004");
//     }

//     #[test]
//     fn it_solves_the_sixth_example() {
//         let input = "...........
// .....###.#.
// .###.##..#.
// ..#.#...#..
// ....#.#....
// .##..S####.
// .##..#...#.
// .......##..
// .##.#.####.
// .##..##.##.
// ...........
// ";
//         assert_eq!(solve(input, 1000), "668697");
//     }
// 
//     #[test]
//     fn it_solves_the_seventh_example() {
//         let input = "...........
// .....###.#.
// .###.##..#.
// ..#.#...#..
// ....#.#....
// .##..S####.
// .##..#...#.
// .......##..
// .##.#.####.
// .##..##.##.
// ...........
// ";
//         assert_eq!(solve(input, 5000), "16733044");
//     }

    // #[test]
    // fn it_solves_the_puzzle() {
    //     let input = include_str!("./input.txt");
    //     assert_eq!(solve(input, 64), "3617");
    // }
}
