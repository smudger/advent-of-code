use std::collections::{HashMap, VecDeque};
use itertools::FoldWhile::{Continue, Done};
use itertools::Itertools;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_until};
use nom::character::complete;
use nom::character::complete::{alpha1, line_ending};
use nom::combinator::{map, opt};
use nom::IResult;
use nom::multi::{fold_many1, separated_list1};
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
    let (workflows, _) = parse_input(input);
    let mut combinations: u64 = 0;
    let mut queue = VecDeque::from([ConstraintSet {
        workflow: "in".to_string(),
        x: Constraint { min: 1, max: 4000 },
        m: Constraint { min: 1, max: 4000 },
        a: Constraint { min: 1, max: 4000 },
        s: Constraint { min: 1, max: 4000 },
    }]);
    
    while let Some(constraint_set) = queue.pop_front() {
        workflows
            .get(&*constraint_set.workflow)
            .expect("workflow exists")
            .iter()
            .fold_while(constraint_set, |set, rule| {
                match &rule.condition {
                    Any => {
                        match &rule.outcome {
                            Accept => combinations += set.combinations(),
                            Reject => {},
                            Consider(workflow_id) => queue.push_back(ConstraintSet {
                                workflow: workflow_id.clone(),
                                x: set.x.clone(),
                                m: set.m.clone(),
                                a: set.a.clone(),
                                s: set.s.clone(),
                            }),
                        };
                        Done(set)
                    },
                    LessThan(part_key, num) => {
                        let (min, max) = match part_key {
                            X => (set.x.min, set.x.max),
                            M => (set.m.min, set.m.max),
                            A => (set.a.min, set.a.max),
                            S => (set.s.min, set.s.max),
                        };
                        
                        // all match
                        if *num > max {
                            match &rule.outcome {
                                Accept => combinations += set.combinations(),
                                Reject => {},
                                Consider(workflow_id) => queue.push_back(ConstraintSet {
                                    workflow: workflow_id.clone(),
                                    x: set.x.clone(),
                                    m: set.m.clone(),
                                    a: set.a.clone(),
                                    s: set.s.clone(),
                                }),
                            };
                            
                            return Done(set);
                        }
                        
                        // none match
                        if *num <= min {
                            return Continue(set);
                        }

                        let mut matching_set = set.clone();
                        let new_max = num.clone() - 1;
                        match part_key {
                            X => {
                                matching_set.x = Constraint { min: matching_set.x.min, max: new_max }
                            }
                            M => {
                                matching_set.m = Constraint { min: matching_set.m.min, max: new_max }
                            }
                            A => {
                                matching_set.a = Constraint { min: matching_set.a.min, max: new_max }
                            }
                            S => {
                                matching_set.s = Constraint { min: matching_set.s.min, max: new_max }
                            }
                        }
                        
                        match &rule.outcome {
                            Accept => combinations += matching_set.combinations(),
                            Reject => {},
                            Consider(workflow_id) => queue.push_back(ConstraintSet {
                                workflow: workflow_id.clone(),
                                x: matching_set.x,
                                m: matching_set.m,
                                a: matching_set.a,
                                s: matching_set.s,
                            }),
                        };
                        
                        let mut non_matching_set = set.clone();
                        match part_key {
                            X => {
                                non_matching_set.x = Constraint { min: *num, max: non_matching_set.x.max }
                            }
                            M => {
                                non_matching_set.m = Constraint { min: *num, max: non_matching_set.m.max }
                            }
                            A => {
                                non_matching_set.a = Constraint { min: *num, max: non_matching_set.a.max }
                            }
                            S => {
                                non_matching_set.s = Constraint { min: *num, max: non_matching_set.s.max }
                            }
                        }
                        
                        Continue(non_matching_set)
                    }
                    GreaterThan(part_key, num) => {
                        let (min, max) = match part_key {
                            X => (set.x.min, set.x.max),
                            M => (set.m.min, set.m.max),
                            A => (set.a.min, set.a.max),
                            S => (set.s.min, set.s.max),
                        };

                        // all match
                        if *num < min {
                            match &rule.outcome {
                                Accept => combinations += set.combinations(),
                                Reject => {},
                                Consider(workflow_id) => queue.push_back(ConstraintSet {
                                    workflow: workflow_id.clone(),
                                    x: set.x.clone(),
                                    m: set.m.clone(),
                                    a: set.a.clone(),
                                    s: set.s.clone(),
                                }),
                            };

                            return Done(set);
                        }

                        // none match
                        if *num >= max {
                            return Continue(set);
                        }

                        let mut matching_set = set.clone();
                        match part_key {
                            X => {
                                matching_set.x = Constraint { min: *num + 1, max: matching_set.x.max }
                            }
                            M => {
                                matching_set.m = Constraint { min: *num + 1, max: matching_set.m.max }
                            }
                            A => {
                                matching_set.a = Constraint { min: *num + 1, max: matching_set.a.max }
                            }
                            S => {
                                matching_set.s = Constraint { min: *num + 1, max: matching_set.s.max }
                            }
                        }

                        match &rule.outcome {
                            Accept => combinations += matching_set.combinations(),
                            Reject => {},
                            Consider(workflow_id) => queue.push_back(ConstraintSet {
                                workflow: workflow_id.clone(),
                                x: matching_set.x,
                                m: matching_set.m,
                                a: matching_set.a,
                                s: matching_set.s,
                            }),
                        };

                        let mut non_matching_set = set.clone();
                        match part_key {
                            X => {
                                non_matching_set.x = Constraint { min: non_matching_set.x.min, max: *num }
                            }
                            M => {
                                non_matching_set.m = Constraint { min: non_matching_set.m.min, max: *num }
                            }
                            A => {
                                non_matching_set.a = Constraint { min: non_matching_set.a.min, max: *num }
                            }
                            S => {
                                non_matching_set.s = Constraint { min: non_matching_set.s.min, max: *num }
                            }
                        }

                        Continue(non_matching_set)
                    }
                }
            });
    }
    
    combinations.to_string()
}

struct Part {
    x: u64,
    m: u64,
    a: u64,
    s: u64,
}

#[derive(Debug, Clone)]
struct ConstraintSet {
    workflow: String,
    x: Constraint,
    m: Constraint,
    a: Constraint,
    s: Constraint,
}

impl ConstraintSet {
    fn combinations(&self) -> u64 {
        (self.x.max - self.x.min + 1)
        * (self.m.max - self.m.min + 1)
        * (self.a.max - self.a.min + 1)
        * (self.s.max - self.s.min + 1)
    }
}

#[derive(Debug, Clone)]
struct Constraint {
    min: u64,
    max: u64,
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
    LessThan(PartKey, u64),
    GreaterThan(PartKey, u64),
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
            complete::u64,
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
                preceded(tag("x="), complete::u64),
                preceded(tag(",m="), complete::u64),
                preceded(tag(",a="), complete::u64),
                preceded(tag(",s="), complete::u64),
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
        assert_eq!(solve(input), "167409079868000");
    }

    #[test]
    fn it_solves_the_puzzle() {
        let input = include_str!("./input.txt");
        assert_eq!(solve(input), "125355665599537");
    }
}
