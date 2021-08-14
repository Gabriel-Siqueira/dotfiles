#!/usr/bin/env bash

if [ $(timew get dom.active) -eq 0 ]
then
  t="00:00:00"
else
    t=$(timew get dom.active.duration)
    s=$(echo $t | rev | cut -d"M" -f1 | rev | sed -E 's/[A-Z]//g')
    m=$(echo $t | rev | cut -d"H" -f1 | rev | cut -d"M" -f1 -s | sed -E 's/[A-Z]//g')
    h=$(echo $t | cut -d"H" -f1 -s | sed -E 's/[A-Z]//g')
    t=$(echo "$(printf "%02d" $h):$(printf "%02d" $m):$(printf "%02d" $s)")
fi

echo $t
