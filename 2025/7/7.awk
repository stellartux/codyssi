#!/bin/awk
# parts 1 and 2

BEGIN {
  FS="-"
  READ_TRACKS = 0
  DO_SWAPS = 1
  PRINT_TEST = 2
}

MODE == PRINT_TEST {
  print t[$1]
  print v[$1]
}

/^$/ && MODE++ == DO_SWAPS {
  tmp = v[y]
  v[y] = v[x]
  v[x] = v[b]
  v[b] = tmp
}

MORE == READ_TRACKS {
  t[NR] = $1
  v[NR] = $1
}

MODE == DO_SWAPS && !b { b = $1 }

MODE == DO_SWAPS && x && y {
  tmp = v[$1]
  v[$1] = v[y]
  v[y] = v[x]
  v[x] = tmp
}

MODE == DO_SWAPS {
  tmp = t[$1];
  t[$1] = t[$2];
  t[$2] = tmp;
  x = $1;
  y = $2;
}
