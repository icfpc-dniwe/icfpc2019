#!/bin/sh
set -e

lastBlock=$1
num=$lastBlock

cp .stack-work/install/x86_64-linux-tinfo6/lts-13.26/8.6.5/bin/icfpc2019-exe ./mine-exe

while :
do
    cd lambda-client
    while [[ $num == $lastBlock ]]
    do
        num=$(./lambda-cli.py getmininginfo block)
        sleep 10
    done
    lastBlock=$num
    echo $lastBlock $num
    python3 lambda-cli.py getmininginfo puzzle > "../puzzles/puzzle$num.cond"
    python3 lambda-cli.py getmininginfo task > "../puzzles/puzzle$num.ts"
    cd ../
    python3 python/miner/createTask.py "puzzles/puzzle$num.cond" "puzzles/puzzle$num.desc" &
    P1=$!
    ./mine-exe "puzzles/puzzle$num.ts" > "puzzles/puzzle$num.sol" &
    P2=$!
    wait $P1 $P2
    cd lambda-client
    python3 lambda-cli.py submit $num "../puzzles/puzzle$num.sol" "../puzzles/puzzle$num.desc"
    cd ../
done
