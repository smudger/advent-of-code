use nom::bytes::complete::tag;
use nom::character::complete::{char, digit1, line_ending};
use nom::combinator::{map, map_res};
use nom::IResult;
use nom::multi::separated_list0;
use nom::sequence::{preceded, separated_pair};
use std::str::FromStr;
use nom::branch::alt;

#[derive(Debug)]
pub struct Draw {
    pub red: u32,
    pub green: u32,
    pub blue: u32,
}

#[derive(Debug)]
pub struct Game {
    pub id: u32,
    pub draws: Vec<Draw>,
}

pub fn parse_input(input: &str) ->Vec<Game> {
    let (_, games) = separated_list0(
        line_ending,
        parse_game,
    )(input).expect("parsing games");

    games
}

fn parse_game(input: &str) -> IResult<&str, Game> {
    map(
        separated_pair(parse_game_id, tag(": "), parse_draws),
        |(id, draws)| Game { id, draws }
    )(input)
}

fn parse_game_id(input: &str) -> IResult<&str, u32> {
    preceded(
        tag("Game "),
        map_res(digit1, u32::from_str)
    )(input)
}

fn parse_draws(input: &str) -> IResult<&str, Vec<Draw>> {
    separated_list0(
        tag("; "),
        parse_draw
    )(input)
}

fn parse_draw(input: &str) -> IResult<&str, Draw> {
    map(separated_list0(
        tag(", "),
        parse_color_count
    ), |color_counts| {
        Draw {
            red: color_counts.iter().find(|color_count| color_count.1 == "red").unwrap_or(&(0, "red")).0,
            green: color_counts.iter().find(|color_count| color_count.1 == "green").unwrap_or(&(0, "green")).0,
            blue: color_counts.iter().find(|color_count| color_count.1 == "blue").unwrap_or(&(0, "blue")).0,
        }
    })(input)
}

fn parse_color_count(input: &str) -> IResult<&str, (u32, &str)>
{
    separated_pair(
        map_res(digit1, u32::from_str),
        char(' '),
        alt((tag("red"), tag("green"), tag("blue"))),
    )(input)
}

