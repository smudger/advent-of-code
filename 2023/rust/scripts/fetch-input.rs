#!/usr/bin/env cargo +nightly -Zscript

//! ```cargo
//! [package]
//! edition = "2021"
//!
//! [dependencies]
//! clap = { version = "4.2", features = ["derive"] }
//! nom = "7.1.3"
//! reqwest = { version = "0.11.22", features=["blocking"] }
//! ```

use clap::{error::ErrorKind, CommandFactory, Parser};
use nom::{
    bytes::complete::tag, character::complete,
    sequence::preceded, IResult,
};
use reqwest::{blocking::Client, header::COOKIE};
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[clap(version)]
struct Args {
    /// day is expected to be formatted as
    /// `day01` to match all other commands in
    /// the repo
    #[clap(short, long)]
    day: String,
    /// the location of the justfile and the
    /// directory in which the project for each
    /// day is stored
    #[clap(long)]
    root_dir: PathBuf,
    /// the session cookie to use when retrieving
    /// input from the Advent of Code website
    #[clap(long)]
    session: String,
}

fn parse_day(input: &str) -> IResult<&str, u32> {
    preceded(tag("day"), complete::u32)(input)
}

fn main() -> Result<(), reqwest::Error> {
    let args = Args::parse();
    let Ok((_, day)) = parse_day(&args.day) else {
        let mut cmd = Args::command();
        cmd.error(
            ErrorKind::ValueValidation,
            format!(
                "day `{}` must be formatted as `day01`",
                args.day
            ),
        )
            .exit()
    };

    let url = format!(
        "https://adventofcode.com/2023/day/{day}/input"
    );
    println!("fetching input from `{}`", url);

    let client = Client::new();
    let input_data = client
        .get(url)
        .header(COOKIE, format!("session={}", args.session))
        .send()?
        .text()?;

    let file_path = args
        .root_dir
        .join(&args.day)
        .join("src")
        .join("bin")
        .join("input.txt");
    let mut file = File::create(&file_path)
        .expect("should be able to create a file");
    
    file.write_all(input_data.as_bytes()).expect(
        "should be able to write to input file",
    );
    println!("saved input to `{}`", file_path.display());

    Ok(())
}