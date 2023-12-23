use glam::IVec2;
use pathfinding::prelude::yen;

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

fn solve(input: &str) -> String {
    let grid = input
        .lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    
    let start_pos = IVec2 { x: 1, y: 0 };
    let end_pos = IVec2 { x: (grid[0].len() - 2) as i32, y: (grid.len() - 1) as i32 };
    
    yen(
        &start_pos,
        |cur_pos| {
            match grid[cur_pos.y as usize][cur_pos.x as usize] {
                '^' => vec![(IVec2 { x: cur_pos.x, y: cur_pos.y - 1}, 1)],
                '>' => vec![(IVec2 { x: cur_pos.x + 1, y: cur_pos.y}, 1)],
                'v' => vec![(IVec2 { x: cur_pos.x, y: cur_pos.y + 1}, 1)],
                '<' => vec![(IVec2 { x: cur_pos.x - 1, y: cur_pos.y}, 1)],
                _ => {
                    let dirs = vec![
                        *cur_pos + IVec2::NEG_X,
                        *cur_pos + IVec2::X,
                        *cur_pos + IVec2::Y,
                        *cur_pos + IVec2::NEG_Y,
                    ];
                    
                    dirs
                        .into_iter()
                        .filter_map(|dir| {
                            let Some(row) = grid.get(dir.y as usize) else {
                                return None;
                            };
                            
                            let Some(c) = row.get(dir.x as usize) else {
                                return None;
                            };

                            (c != &'#').then_some((dir, 1))
                        })
                        .collect()
                }
            }
        },
        |pos| pos == &end_pos,
        usize::MAX,
    )
        .into_iter()
        .map(|(_, len)| len)
        .max()
        .expect("there is a longest path")
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_example() {
        let input = "#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#
";
        assert_eq!(solve(input), "94");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "2306");
    }
}
