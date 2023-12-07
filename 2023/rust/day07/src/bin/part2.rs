use std::cmp::Ordering;
use itertools::Itertools;

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

fn solve(input: &str) -> String {
    input
        .lines()
        .map(Round::from)
        .sorted_unstable()
        .enumerate()
        .map(|(index, round)| {
            (index + 1) * round.bid
        })
        .sum::<usize>()
        .to_string()
}

const CARDS: [char; 13] = ['J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A'];

fn card_strength(my_card: char) -> usize {
    CARDS
        .into_iter()
        .find_position(|card| card == &my_card)
        .expect("my_card is a valid card")
        .0
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

#[derive(Eq, PartialEq, Ord)]
struct Round {
    cards: [char; 5],
    bid: usize
}

impl Round {
    fn hand_strength(&self) -> usize {
        let (jokers, remaining): (Vec<_>, Vec<_>) = self.cards
            .into_iter()
            .partition(|c| c == &'J');
        
        if jokers.len() == 5 {
            return HANDS.len() - 1;
        }
        
        let mut my_hand = remaining
            .into_iter()
            .filter(|c| c != &'J')
            .counts()
            .into_values()
            .sorted()
            .rev()
            .collect::<Vec<_>>();
        my_hand[0] += jokers.len();
        
        HANDS
            .into_iter()
            .find_position(|hand| hand[..my_hand.len()] == my_hand[..])
            .expect("my_hand is a valid hand")
            .0
    }
}

impl PartialOrd<Self> for Round {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.hand_strength() != other.hand_strength() {
            return Some(self.hand_strength().cmp(&other.hand_strength()));
        }
        
        let (my_card, other_card) = self.cards
            .into_iter()
            .zip(other.cards)
            .find(|(a, b)| a != b)
            .expect("the two card lists are not identical");
        
        Some(card_strength(my_card).cmp(&card_strength(other_card)))
    }
}

impl From<&str> for Round {
    fn from(line: &str) -> Self {
        let (hand, bid) = line
            .split_once(" ")
            .expect("the line contains a space");
    
        Round {
            cards: hand
                .chars()
                .collect::<Vec<char>>()
                .try_into()
                .expect("there are 5 cards"),
            bid: bid
                .parse::<usize>()
                .expect("the bid is a usize")
        }
    }
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
