#!/bin/awk

# part one
{
  danger = 0
  for (i = 1; i <= NF; ++i) {
    danger += $i
    coldanger[i] += $i
  }
  if (danger < mindanger || NR == 1) {
    mindanger = danger
  }
}

# part two and three
NR == 1 {
  for (i = 1; i <= NF; ++i) {
    paths[i] = $i + paths[i - 1]
  }
}

NR > 1 {
  paths[1] += $1
  for (i = 2; i <= NF; ++i) {
    paths[i] = $i + min(paths[i - 1], paths[i])
  }
}

NR == 15 {
  parttwo = paths[15]
}

END	{
  for (i = 1; i <= NF; ++i) {
    if (coldanger[i] < mindanger) {
      mindanger = coldanger[i]
    }
  }
  print +mindanger
  print +parttwo
  print +paths[NF]
}

function min(a, b) {
  return a < b ? a : b
}
