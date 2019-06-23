#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
VALIDATE="$DIR/validate_solution.sh"

PROBLEM_EXT="desc"
SOLUTION_EXT="sol"

args=("$@")
PROBLEMS_FOLDER=`realpath ${args[0]}`
SOLUTIONS_FOLDER=`realpath ${args[1]}`
echo "batch validating: problems folder '$PROBLEMS_FOLDER', solutions folder '$SOLUTIONS_FOLDER'"

for file in `ls "$SOLUTIONS_FOLDER/" | grep $SOLUTION_EXT` ; do
    taskname="$(basename $file .$SOLUTION_EXT)"
    taskfile="$PROBLEMS_FOLDER/$taskname.$PROBLEM_EXT"
    solfile="$SOLUTIONS_FOLDER/$taskname.$SOLUTION_EXT"
    
    echo "Validating task '$taskfile', solution: '$solfile'"
    result=`$VALIDATE $taskfile $solfile`
    if [[ $result != "OK" ]]; then
        echo "Solution is INVALID for $taskname"
    else
        echo "OK"
    fi
done
