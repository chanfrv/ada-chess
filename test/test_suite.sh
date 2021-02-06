#!/bin/bash


test_dir="$(dirname $0)/../obj"

#if [ "$#" -ne 1 ] || ([ "$1" != "hlr" ] && [ "$1" != "llr" ]); then
#    echo "Needs 'hlr' or 'llr' as argument"
#    exit 1
#fi
[[ "$#" -ne 0 ]] && exit 1

#for test in "$test_dir/$1"*_{pass,fail}; do
for test in "$test_dir/llr"*_{pass,fail}; do
    ./$test 2>&1 > /dev/null
    if [ $? -eq 0 ]; then
        echo -e "$(basename $test)... \e[0;92mpass\e[0m"
    else
        echo -e "$(basename $test)... \e[0;91mfail\e[0m"
    fi
done
