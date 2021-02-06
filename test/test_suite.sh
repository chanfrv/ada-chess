#!/bin/sh


test_dir="$(dirname $0)/../obj"

case $1 in
    "hlr")
        test_prefix="hlr" ;;
    "llr")
        test_prefix="llr" ;;
    ""|"*")
        echo "Needs 'hlr' or 'llr' as argument"
        exit 1 ;;
esac

for test in "$test_dir/$test_prefix"*_pass "$test_dir/$test_prefix"*_fail; do
    ./$test 2>&1 > /dev/null
    if [ $? -eq 0 ]; then
        echo -e "$test... \e[0;92mpass\e[0m"
    else
        echo -e "$test... \e[0;91mfail\e[0m"
    fi
done
