#!/bin/sh
set -e

lastBlock=$1
num=$lastBlock

while :
do
    cd lambda-client
    while [[ $num == $lastBlock ]]
    do
        num=$(./lambda-cli.py getmininginfo block)
        sleep 10
    done
    echo $lastBlock $num
    python3 lambda-cli.py getmininginfo puzzle > "../puzzles/puzzle$num.cond"
    cd ../
    python3 python/miner/createTask.py "puzzles/puzzle$num.cond" "puzzles/puzzle$num.desc"
    stack run "puzzles/puzzle$num.desc" > "puzzles/puzzle$num.sol"
    cd lam bda-client
    python3 lambda-cli.py submit $num "../puzzles/puzzle$num.sol" "../puzzles/puzzle$num.desc"
done
