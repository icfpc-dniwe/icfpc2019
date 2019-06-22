#!/bin/sh
set -e

first=$1
second=$2

for path in $first/*.sol; do
    filename="$(basename "$path")"
    size1=$(wc -c <"$first/$filename")
    if [[ $size1 > 0 ]]; then
        if [ -e "$second/$filename" ];
        then
            size2=$(wc -c < "$second/$filename")
            # echo $size1 $size2
            if [[ $size1 < $size2 ]]; then
                echo "copying better $filename"
                cp "$first/$filename" "$second/$filename"
            fi
        else
            echo "copying nonexistant $filename"
            cp "$first/$filename" "$second/$filename"
        fi
    fi
done
