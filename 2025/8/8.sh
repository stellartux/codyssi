#!/bin/sh
# part 1
tr -cd a-zA-Z <"$1" | wc -c
