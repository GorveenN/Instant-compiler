#!/usr/bin/env bash

shopt -s extglob

good_tests_dir="lattests/good/*"
bad_tests_dir="lattests/bad/*"
extension_tests_dir="lattests/extensions/*/*"
objects_test_dir="lattests/extensions/objects1/*"
objects_test_dir2="lattests/extensions/objects2/*"
struct_test_dir="lattests/extensions/struct/*"
arrays_test_dir="lattests/extensions/arrays1/*"
extensions="lattests/extensions/more_good/*"
program_path="./latc_x86"

more_good_tests="lattests/basic/*.lat"
more_bad_tests="my_tests/bad/*/*.lat"

NC='\033[0m' # No Color
RED='\033[0;31m'
GREEN='\033[0;32m'

function run_tests_in() {
    for file in $1; do
        if [[ "$file" == *".lat" ]]; then
            echo $file
            basename="$(echo $file | cut -f1 -d".")"
            $program_path $file 2>/dev/null
            if [ -f "$basename.input" ]; then
                if [ -f "$basename.output" ]; then
                    ./$basename <"$basename.input" | diff "$basename.output" -
                else
                    ./$basename <"$basename.input"
                fi
            else
                if [ -f "$basename.output" ]; then
                    ./$basename | diff "$basename.output" -
                else
                    ./$basename
                fi
            fi

            if [ "$?" -eq $2 ]; then
                echo -ne "${GREEN}OK${NC}\t"
            else
                echo -ne "${RED}FAIL${NC}\t"
            fi
            echo "$file"
        fi
    done
}

run_tests_in "$good_tests_dir" 0
run_tests_in "$objects_test_dir" 0
run_tests_in "$objects_test_dir2" 0
run_tests_in "$struct_test_dir" 0
run_tests_in "$arrays_test_dir" 0
run_tests_in "$extensions" 0
run_tests_in "lattests/others/*" 0
# run_tests_in "$bad_tests_dir" 1
# run_tests_in "$extension_tests_dir" 0

# run_tests_in "$more_good_tests" 0
# run_tests_in "$more_bad_tests" 1
