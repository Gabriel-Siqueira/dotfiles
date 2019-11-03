#!/bin/sh
current=`i3-msg -t get_workspaces | jq '.[] | select(.focused==true).name' | cut -d"\"" -f2`
other=$1

i3-msg rename workspace to swap
i3-msg rename workspace $other to $current
i3-msg rename workspace swap to $other
i3-msg workspace $current
