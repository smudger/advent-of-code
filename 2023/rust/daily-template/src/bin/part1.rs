fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

fn solve(input: &str) -> String {
    todo!("part 1");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_example() {
        let input = "";
        assert_eq!(solve(input), "Hello, world!");
    }

    // #[test]
    // fn it_solves_the_puzzle() {
    //     let input = include_str!("./input.txt");
    //     assert_eq!(solve(input), "Hello, world!");
    // }
}
