#!/usr/bin/env bash

blue=$(tput setaf 4)
normal=$(tput sgr0)

if [ $(timew get dom.active) -eq 0 ]
then
  t="00:00:00"
else
    t=$(timew get dom.active.duration)
    l=$(timew get dom.active.tag.1)
    s=$(echo $t | rev | cut -d"M" -f1 | rev | sed -E 's/[A-Z]//g')
    m=$(echo $t | rev | cut -d"H" -f1 | rev | cut -d"M" -f1 -s | sed -E 's/[A-Z]//g')
    h=$(echo $t | cut -d"H" -f1 -s | sed -E 's/[A-Z]//g')
    if [[ $l == *"down time"* ]]
    then
        t=$(echo "D ${blue}$(printf "%02d" $h):$(printf "%02d" $m):$(printf "%02d" $s)${normal}")
    else
        t=$(echo "U $(printf "%02d" $h):$(printf "%02d" $m):$(printf "%02d" $s)")
    fi
fi

echo $t
