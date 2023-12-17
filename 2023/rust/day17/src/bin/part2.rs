use glam::IVec2;
use pathfinding::prelude::dijkstra;
use crate::Direction::{North, East, South, West};

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum Direction {
    North,
    East,
    South,
    West,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct Position {
    loc: IVec2,
    dir: Direction,
    sld: u8
}

impl Position {
    fn successors(&self, grid: &Vec<Vec<u32>>) -> Vec<(Position, usize)> {
        let possible_moves = match self.dir {
            North => {
                let mut result = vec![];
                
                if self.sld >= 4 {
                    result.push(Position { loc: self.loc + (4 * IVec2::NEG_X), dir: West, sld: 4 });
                    result.push(Position { loc: self.loc + (4 * IVec2::X), dir: East, sld: 4 });
                }

                if self.sld < 4 {
                    result.push(Position { loc: self.loc + ((4 - self.sld) as i32 * IVec2::NEG_Y), dir: North, sld: 4 });
                } else if self.sld < 10 {
                    result.push(Position { loc: self.loc + IVec2::NEG_Y, dir: North, sld: self.sld + 1 });
                }

                result
            }
            East => {
                let mut result = vec![];

                if self.sld >= 4 {
                    result.push(Position { loc: self.loc + (4 * IVec2::NEG_Y), dir: North, sld: 4 });
                    result.push(Position { loc: self.loc + (4 * IVec2::Y), dir: South, sld: 4 });
                }

                if self.sld < 4 {
                    result.push(Position { loc: self.loc + ((4 - self.sld) as i32 * IVec2::X), dir: East, sld: 4 });
                } else if self.sld < 10 {
                    result.push(Position { loc: self.loc + IVec2::X, dir: East, sld: self.sld + 1 });
                }

                result
            }
            South => {
                let mut result = vec![];

                if self.sld >= 4 {
                    result.push(Position { loc: self.loc + (4 * IVec2::X), dir: East, sld: 4 });
                    result.push(Position { loc: self.loc + (4 * IVec2::NEG_X), dir: West, sld: 4 });
                }

                if self.sld < 4 {
                    result.push(Position { loc: self.loc + ((4 - self.sld) as i32 * IVec2::Y), dir: South, sld: 4 });
                } else if self.sld < 10 {
                    result.push(Position { loc: self.loc + IVec2::Y, dir: South, sld: self.sld + 1 });
                }

                result
            }
            West => {
                let mut result = vec![];

                if self.sld >= 4 {
                    result.push(Position { loc: self.loc + (4 * IVec2::NEG_Y), dir: North, sld: 4 });
                    result.push(Position { loc: self.loc + (4 * IVec2::Y), dir: South, sld: 4 });
                }

                if self.sld < 4 {
                    result.push(Position { loc: self.loc + ((4 - self.sld) as i32 * IVec2::NEG_X), dir: West, sld: 4 });
                } else if self.sld < 10 {
                    result.push(Position { loc: self.loc + IVec2::NEG_X, dir: West, sld: self.sld + 1 });
                }

                result
            }
        };

        possible_moves
            .into_iter()
            .filter_map(|position| {
                let Some(row) = grid.get(position.loc.y as usize) else {
                    return None;
                };

                let Some(heat_loss) = row.get(position.loc.x as usize) else {
                    return None;
                };
                
                if self.loc.distance_squared(position.loc) == 1 {
                    return Some((position, *heat_loss as usize));
                }
                
                let heat_loss: u32 = match self.loc.y == position.loc.y {
                    true => {
                        // y diff
                        let range = match self.loc.x < position.loc.x {
                            true => ((self.loc.x + 1)..=position.loc.x).collect::<Vec<_>>(),
                            false => (position.loc.x..self.loc.x).collect::<Vec<_>>(),
                        };
                        
                        range
                            .into_iter()
                            .map(|x| {
                                grid
                                    .get(position.loc.y as usize)
                                    .expect("the current position y is in the grid")
                                    .get(x as usize)
                                    .expect("the current position x is in the grid")
                            })
                            .sum()
                    },
                    false => {
                        let range = match self.loc.y < position.loc.y {
                            true => ((self.loc.y + 1)..=position.loc.y).collect::<Vec<_>>(),
                            false => (position.loc.y..self.loc.y).collect::<Vec<_>>(),
                        };
                        range
                            .into_iter()
                            .map(|y| {
                                grid
                                    .get(y as usize)
                                    .expect("the current position y is in the grid")
                                    .get(position.loc.x as usize)
                                    .expect("the current position x is in the grid")
                            })
                            .sum()
                    }
                };
                
                Some((position, heat_loss as usize))
            })
            .collect()
    }
}

fn solve(input: &str) -> String {
    let grid = input
        .lines()
        .map(|line|
            line
                .chars()
                .map(|c| c.to_digit(10))
                .flatten()
                .collect::<Vec<_>>()
        )
        .collect::<Vec<_>>();

    let finish_loc = IVec2 {
        x: (grid[0].len() - 1) as i32,
        y: (grid.len() - 1) as i32,
    };

    let (_, heat_loss_south) = dijkstra(
        &Position{ loc: IVec2 { x: 0, y: 0}, dir: South, sld: 0 },
        |cur_pos| cur_pos.successors(&grid),
        |cur_pos| cur_pos.loc == finish_loc
    ).expect("can find a path from south start to finish");
    let (_, heat_loss_east) = dijkstra(
        &Position{ loc: IVec2 { x: 0, y: 0}, dir: East, sld: 0 },
        |cur_pos| cur_pos.successors(&grid),
        |cur_pos| cur_pos.loc == finish_loc
    ).expect("can find a path from east start to finish");

    heat_loss_south.min(heat_loss_east).to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_first_example() {
        let input = "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
";
        assert_eq!(solve(input), "94");
    }

    #[test]
    fn it_solves_the_second_example() {
        let input = "111111111111
999999999991
999999999991
999999999991
999999999991
";
        assert_eq!(solve(input), "71");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "734");
    }
}
