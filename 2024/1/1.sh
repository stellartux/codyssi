#!/bin/sh
awk '{ total += $1 } END { print +total }' "$1"
sort -rn "$1" | awk 'NR > 20 { total += $1 } END { print +total }'
awk '{ total += NR % 2 ? $1 : -$1 } END { print +total }' "$1"
