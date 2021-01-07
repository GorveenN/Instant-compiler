#!/usr/bin/env bash

shopt -s extglob

good_tests_dir="lattests/good/*"
bad_tests_dir="lattests/bad/*"
extension_tests_dir="lattests/extensions/*/*"
objects_test_dir="lattests/extensions/objects1/*"
objects_test_dir2="lattests/extensions/objects2/*"
struct_test_dir="lattests/extensions/struct/*"
arrays_test_dir="lattests/extensions/arrays1/*"
program_path="./latc_x86"

NC='\033[0m' # No Color
RED='\033[0;31m'
GREEN='\033[0;32m'

function run_tests_in() {
    find $1 -type f ! -name "*.input" ! -name "*.lat" ! -name "*.output" -exec rm {} \;
    for file in $1; do
        if [[ "$file" == *".lat" ]]; then
            basename="$(echo $file | cut -f1 -d".")"
            $program_path $file 2>/dev/null
            result_code="$?"

            if [ "0" -eq $2 ]; then
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
                result_code="$?"
            fi

            if [ $result_code -eq $2 ]; then
                echo -ne "${GREEN}OK${NC}\t"
            else
                echo -ne "${RED}FAIL${NC}\t"
            fi
            echo "$file"
        fi

    find $1 -type f ! -name "*.input" ! -name "*.lat" ! -name "*.output" -exec rm {} \;
    done
}

run_tests_in "$good_tests_dir" 0
run_tests_in "$objects_test_dir" 0
run_tests_in "$objects_test_dir2" 0
run_tests_in "$struct_test_dir" 0
run_tests_in "$arrays_test_dir" 0
run_tests_in "lattests/others/*" 0
run_tests_in "$bad_tests_dir" 1
