#!/bin/sh
set -e

num=$1

cd lambda-client
python3 lambda-cli.py getmininginfo puzzle > "../puzzles/puzzle$num.cond"
cd ../
python python/miner/createTask.py "puzzles/puzzle$num.cond" "puzzles/puzzle$num.desc"
stack run "puzzles/puzzle$num.desc" > "puzzles/puzzle$num.sol"

