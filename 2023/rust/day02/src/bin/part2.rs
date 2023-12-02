fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

#[derive(Debug)]
struct Draw {
    red: u32,
    green: u32,
    blue: u32,
}

#[derive(Debug)]
struct Game {
    id: u32,
    draws: Vec<Draw>,
}

fn solve(input: &str) -> String {
    let games = parse_games(input);

    games
        .iter()
        .map(|game| {
            let max_red = game
                .draws
                .iter()
                .map(|draw| draw.red)
                .max()
                .unwrap_or(0);
            let max_green = game
                .draws
                .iter()
                .map(|draw| draw.green)
                .max()
                .unwrap_or(0);
            let max_blue = game
                .draws
                .iter()
                .map(|draw| draw.blue)
                .max()
                .unwrap_or(0);
            
            max_red * max_green * max_blue
        })
        .sum::<u32>()
        .to_string()
}

fn parse_games(input: &str) -> Vec<Game> {
    input
        .lines()
        .map(|line| {
            let mut splits = line.split(": ");
            let game_id_string = &splits
                .next()
                .expect("can retrieve the first half")
                [5..];
            let id = game_id_string
                .parse::<u32>()
                .expect("game id is a number");

            let draws = splits
                .next()
                .expect("can retrieve the second half")
                .split("; ")
                .map(|draw| {
                    let mut color_counts = draw
                        .split(", ")
                        .map(|color_count| {
                            let mut it = color_count.split(" ");

                            (
                                it
                                    .next()
                                    .expect("there is a count")
                                    .parse::<u32>()
                                    .expect("the count is a number"),
                                it.next().expect("there is a color")
                            )
                        });

                    Draw {
                        red: color_counts.clone().find(|color_count| color_count.1 == "red").unwrap_or((0, "red")).0,
                        green: color_counts.clone().find(|color_count| color_count.1 == "green").unwrap_or((0, "green")).0,
                        blue: color_counts.find(|color_count| color_count.1 == "blue").unwrap_or((0, "blue")).0,
                    }
                })
                .collect::<Vec<_>>();

            Game {
                id,
                draws,
            }
        })
        .collect::<Vec<_>>()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_example() {
        let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green";
        assert_eq!(solve(input), "2286");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "72422");
    }
}
