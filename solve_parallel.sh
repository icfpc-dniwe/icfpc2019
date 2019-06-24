#!/bin/sh
set -e

cp .stack-work/install/x86_64-linux-tinfo6/lts-13.26/8.6.5/bin/icfpc2019-exe ./
ls problems/*.desc | tail -196 | parallel -j4 -- sh -c "echo $@; ./icfpc2019-exe {} > batch_solutions/{/.}.sol"
