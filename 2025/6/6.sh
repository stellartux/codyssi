#!/bin/sh
# parts 1 and 2
grep -o "[a-zA-Z]" "$1" | sort | uniq -c | awk '

BEGIN {
    s = "abcdefghijklmnopqrstuvwxyz"
    s = s toupper(s)
}
{
    sum1 += $1
    sum2 += $1 * index(s, $2)
}
END {
    print sum1
    print sum2
}'
