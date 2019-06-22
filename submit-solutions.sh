#!/bin/sh
set -e

tmpfile="$(mktemp).zip"
(
  cd best_solutions
  zip "$tmpfile" *.sol
)
curl -F "private_id=305c47431f548d935d3d3604" -F "file=@${tmpfile}" https://monadic-lab.org/submit
