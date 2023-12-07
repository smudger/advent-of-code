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

const CARDS: [char; 13] = ['J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A'];

struct Round<'a> {
    hand: &'a str,
    bid: usize
}

impl Round<'_> {
    fn structure(&self) -> usize {
        let mut counts = self.hand
            .chars()
            .filter(|c| c != &'J')
            .counts()
            .into_values()
            .sorted()
            .rev()
            .collect::<Vec<_>>();
        if let None = counts.get(0) {
            counts.push(0);
        };
        counts[0] += self.hand.chars().filter(|c| c == &'J').count();
        
        HANDS
            .into_iter()
            .find_position(|hand| hand[..counts.len()] == counts[..])
            .expect("a_counts is a valid hand")
            .0
    }
}

fn solve(input: &str) -> String {
    let mut hands = input
        .lines()
        .map(|line| {
            let (hand, bid) = line
                .split_once(" ")
                .expect("the line contains a space");

            Round {
                hand,
                bid: bid.parse::<usize>().expect("the bid is a usize")
            }
        })
        .collect::<Vec<_>>();

    hands.sort_unstable_by(|a, b| {
        if a.structure() != b.structure() {
            return a.structure().cmp(&b.structure());
        }
        
        let (a_card, b_card) = a.hand
            .chars()
            .zip(b.hand.chars())
            .find(|(a, b)| a != b)
            .expect("a and b are not the same string");

        let a_strength = CARDS.into_iter().find_position(|card| *card == a_card).expect("a_card is a valid card").0;
        let b_strength = CARDS.into_iter().find_position(|card| *card == b_card).expect("b_card is a valid card").0;
        a_strength.cmp(&b_strength)
    });

    hands
        .iter()
        .enumerate()
        .map(|(index, round)| {
            (index + 1) * round.bid
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
        assert_eq!(solve(input), "5905");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "251824095");
    }
}
