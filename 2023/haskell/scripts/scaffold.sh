#!/bin/bash
set -e
set -u
set -o pipefail

day=$1

if [ -d "$day" ]; then
    echo "Directory $day already exists. Aborting."
    exit 1
fi

cp -r daily-template "$day"
mv "$day/dayxx.cabal" "$day/$day.cabal"
sed -i '' 's/dayxx/'"$day"'/g' "$day/$day.cabal"