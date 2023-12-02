use day02::parse_input;

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

fn solve(input: &str) -> String {
    parse_input(input)
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
