#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

NODE="npm start"
NODE_PROJECT_RELPATH="./horrible_things"

args=("$@")
PROBLEM_PATH=`realpath ${args[0]}`
SOLUTION_PATH=`realpath ${args[1]}`

result=`cd $NODE_PROJECT_RELPATH && $NODE "$PROBLEM_PATH" "$SOLUTION_PATH" && cd -`
if [[ $result == *"Success"* ]]; then
  echo "OK";
else
  echo "$result"
fi
