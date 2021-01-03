#!/usr/bin/env bash

shopt -s extglob

good_tests_dir="lattests/good/*"
bad_tests_dir="lattests/bad/*"
extension_tests_dir="lattests/extensions/*/*"
program_path="./latc"

more_good_tests="my_tests/basic/*.lat"
more_bad_tests="my_tests/bad/*/*.lat"

NC='\033[0m' # No Color
RED='\033[0;31m'
GREEN='\033[0;32m'

function run_tests_in() {
    for file in $1; do
        if [[ "$file" == *".lat" ]]; then
            basename="$(echo $file | cut -f1 -d".")"
            $program_path $file 2>/dev/null
            if [ -f "$basename.input" ]; then
                ./$basename <"$basename.input" | diff "$basename.output" -
            else
                ./$basename | diff "$basename.output" -
            fi

            if [ "$?" -eq $2 ]; then
                echo -ne "${GREEN}OK${NC}\t"
            else
                echo -ne "${RED}FAIL${file}${NC}\t"
            fi
            echo "$file"
        fi
    done
}

run_tests_in "$good_tests_dir" 0
# run_tests_in "$bad_tests_dir" 1
# run_tests_in "$extension_tests_dir" 0

# run_tests_in "$more_good_tests" 0
# run_tests_in "$more_bad_tests" 1