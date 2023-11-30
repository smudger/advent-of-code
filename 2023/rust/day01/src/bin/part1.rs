fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

fn solve(input: &str) -> String {
    "Hello, world!".to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./example.txt");
        assert_eq!(solve(input), "Hello, world!");
    }
}
