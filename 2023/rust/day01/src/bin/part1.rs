fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

fn solve(input: &str) -> String {
    input
        .lines()
        .map(|line| {
            line
                .chars()
                .filter_map(|character| {
                    character.to_digit(10)
                })
                .collect::<Vec<u32>>()
        })
        .map(|digits| (digits.first().unwrap() * 10) + digits.last().unwrap())
        .sum::<u32>()
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_example() {
        let input = "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet";
        assert_eq!(solve(input), "142");
    }
    
    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "53334");
    }
}
