#!/bin/bash
BATPATH=/sys/class/power_supply/BAT1
BAT_FULL=$BATPATH/charge_full
BAT_NOW=$BATPATH/charge_now

while true; do
    bf=$(cat $BAT_FULL)
    bn=$(cat $BAT_NOW)
    if [ $(( 100 * $bn / $bf )) -lt 15 ]
    then
         twmnc --bg 'red' --fg 'yellow' -c "               low battery               "
        sleep 1m
    else
        sleep 5m
    fi
done
