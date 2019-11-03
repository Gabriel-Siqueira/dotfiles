#!/bin/bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -x polybar >/dev/null; do sleep 1; done

for m in $(polybar --list-monitors | cut -d":" -f1); do
    MONITOR=$m polybar main&
done

rm -f /tmp/xmonad-log
mkfifo /tmp/xmonad-log
while IFS='$\n' read -r line; do
    echo $line > /tmp/xmonad-log
done
