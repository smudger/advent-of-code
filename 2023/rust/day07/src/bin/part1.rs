use itertools::Itertools;

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

const HANDS: [[usize; 5]; 7] = [
    [1, 1, 1, 1, 1],
    [2, 1, 1, 1, 0],
    [2, 2, 1, 0, 0],
    [3, 1, 1, 0, 0],
    [3, 2, 0, 0, 0],
    [4, 1, 0, 0, 0],
    [5, 0, 0, 0, 0],
];

const CARDS: [char; 13] = ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A'];

fn solve(input: &str) -> String {
    let mut hands = input
        .lines()
        .map(|line| {
            let (hand, bid) = line
                .split_once(" ")
                .expect("the line contains a space");

            (
                hand,
                bid.parse::<usize>().expect("the bid is a u32")
            )
        })
        .collect::<Vec<_>>();
    
    hands.sort_unstable_by(|a, b| {
        let a_counts = a.0
            .chars()
            .counts()
            .into_values()
            .sorted()
            .rev()
            .collect::<Vec<_>>();
        let b_counts = b.0
            .chars()
            .counts()
            .into_values()
            .sorted()
            .rev()
            .collect::<Vec<_>>();

        match a_counts == b_counts {
            true => {
                let (a_card, b_card) = a.0
                    .chars()
                    .zip(b.0.chars())
                    .find(|(a, b)| a != b)
                    .expect("a and b are not the same string");
                
                let a_strength = CARDS.into_iter().find_position(|card| *card == a_card).expect("a_card is a valid card").0;
                let b_strength = CARDS.into_iter().find_position(|card| *card == b_card).expect("b_card is a valid card").0;
                a_strength.cmp(&b_strength)
            },
            false => {
                let a_hand = HANDS
                    .into_iter()
                    .find_position(|counts| counts[..a_counts.len()] == a_counts[..])
                    .expect("a_counts is a valid hand")
                    .0;
                let b_hand = HANDS
                    .into_iter()
                    .find_position(|counts| counts[..b_counts.len()] == b_counts[..])
                    .expect("b_counts is a valid hand")
                    .0;
                
                a_hand.cmp(&b_hand)
            }
        }
    });
    
    hands
        .iter()
        .enumerate()
        .map(|(index, (_, bid))| {
            (index + 1) * bid
        })
        .sum::<usize>()
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_example() {
        let input = "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483";
        assert_eq!(solve(input), "6440");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "250946742");
    }
}
