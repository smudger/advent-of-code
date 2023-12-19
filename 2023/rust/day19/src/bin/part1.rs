use std::collections::HashMap;
use std::mem::take;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_until};
use nom::character::complete;
use nom::character::complete::{alpha1, line_ending};
use nom::combinator::{map, opt, success};
use nom::IResult;
use nom::multi::{count, fold_many1, separated_list1};
use nom::sequence::{delimited, preceded, separated_pair, terminated, tuple};
use Condition::GreaterThan;
use crate::Condition::{Any, LessThan};
use crate::Outcome::{Accept, Consider, Reject};
use crate::PartKey::{A, M, S, X};

fn main() {
    let input = include_str!("./input.txt");
    let output = solve(input);
    dbg!(output);
}

fn solve(input: &str) -> String {
    let (workflows, parts) = parse_input(input);
    dbg!(workflows, parts);
    
    todo!("part 1");
}

#[derive(Debug, Clone)]
struct Part {
    x: u32,
    m: u32,
    a: u32,
    s: u32,
}

#[derive(Debug, Clone)]
enum PartKey {
    X,
    M,
    A,
    S,
}

#[derive(Debug, Clone)]
enum Outcome {
    Accept,
    Reject,
    Consider(String),
}

#[derive(Debug, Clone)]
enum Condition {
    Any,
    LessThan(PartKey, u32),
    GreaterThan(PartKey, u32),
}

#[derive(Debug, Clone)]
struct Rule {
    condition: Condition,
    outcome: Outcome,
}

fn parse_input(input: &str) -> (HashMap<&str, Vec<Rule>>, Vec<Part>) {
    let (_, result) = separated_pair(
        parse_workflows,
        line_ending,
        parse_parts,
    )(input).expect("the input can be parsed");
    
    result
}

fn parse_workflows(input: &str) -> IResult<&str, HashMap<&str, Vec<Rule>>> {
    fold_many1(
        terminated(
            parse_workflow,
            opt(line_ending),
        ),
        HashMap::new,
        |mut acc, (key, value)| {
            acc.insert(key, value);
            
            acc
        },
    )(input)
}

fn parse_workflow(input: &str) -> IResult<&str, (&str, Vec<Rule>)> {
    tuple((
        take_until("{"),
        delimited(
            tag("{"),
            separated_list1(
                tag(","),
                parse_rule,
            ),
            tag("}"),
        ),
    ))(input)
}

fn parse_rule(input: &str) -> IResult<&str, Rule> {
    alt((
        map(tuple((
            alt((
                map(tag("x"), |_| X),
                map(tag("m"), |_| M),
                map(tag("a"), |_| A),
                map(tag("s"), |_| S),
            )),
            alt((tag("<"), tag(">"))),
            complete::u32,
            tag(":"),
            map(alpha1, |key| match key {
                "A" => Accept,
                "R" => Reject,
                _ => Consider(key.to_string()),
            })
            )),
            |(part_key, sign, num, _, outcome)| Rule {
                condition: match sign {
                    ">" => GreaterThan(part_key, num),
                    "<" => LessThan(part_key, num),
                    _ => unimplemented!(),
                },
                outcome
            },
        ),
        map(alpha1, |key| Rule {
            condition: Any,
            outcome: match key {
                "A" => Accept,
                "R" => Reject,
                _ => Consider(key.to_string()),
            }
        }),
    ))(input)
}

fn parse_parts(input: &str) -> IResult<&str, Vec<Part>> {
    separated_list1(
        line_ending,
        map(delimited(
            tag("{"),
            tuple((
                preceded(tag("x="), complete::u32),
                preceded(tag(",m="), complete::u32),
                preceded(tag(",a="), complete::u32),
                preceded(tag(",s="), complete::u32),
            )),
            tag("}"),
        ), |(x, m, a, s)| Part { x, m, a, s })
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_solves_the_example() {
        let input = "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
";
        assert_eq!(solve(input), "19114");
    }

    // #[test]
    // fn it_solves_the_puzzle() {
    //     let input = include_str!("./input.txt");
    //     assert_eq!(solve(input), "Hello, world!");
    // }
}
