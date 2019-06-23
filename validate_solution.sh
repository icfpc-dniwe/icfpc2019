#!/bin/bash

NODE="npm start"
NODE_PROJECT_RELPATH="./horrible_things"
PROBLEM_EXT="desc"
PROBLEMS_RELPATH="./problems"
SOLUTION_EXT="sol"
SOLUTIONS_RELPATH="./solutions"
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

args=("$@")
TASK_NAME=${args[0]}

result=`cd $NODE_PROJECT_RELPATH && $NODE "$DIR/$PROBLEMS_RELPATH/$TASK_NAME.$PROBLEM_EXT" "$DIR/$SOLUTIONS_RELPATH/$TASK_NAME.$SOLUTION_EXT" && cd -`
if [[ $result == *"Success"* ]]; then
  echo "OK";
else
  echo result
fi
