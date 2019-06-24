#!/bin/sh
set -e

first=$1
second=$2

function count_size() {
    file="$1"
    sed 's/W|A|S|D|Z|E|Q|B([0-9]*,[0-9]*)|F|L|R|T([0-9]*,[0-9]*)/u/g' "$file" | tr -d ' \n' | wc -c
}

for path in $first/*.sol; do
    filename="$(basename "$path")"
    problem="problems/${filename%.sol}.desc"
    size1=$(count_size "$first/$filename")
    if [[ $size1 > 0 ]] && node horrible_things/index.js "$problem" "$path"; then
        if [ -e "$second/$filename" ];
        then
            size2=$(count_size "$second/$filename")
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
