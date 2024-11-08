#!/bin/bash

set -e
set -u
set -o pipefail

day=$1
session=$2

if [ -z "$session" ]; then
    echo "Session token is not set. Aborting."
    exit 1
fi

if [ -z "$day" ]; then
    echo "Day is not specified. Aborting."
    exit 1
fi

day_number=$(echo "$day" | sed 's/^day0*//')
url="https://adventofcode.com/2023/day/$day_number/input"
output_dir="$day"
output_file="$output_dir/input.txt"

mkdir -p "$output_dir"

curl -s --cookie "session=$session" "$url" -o "$output_file"