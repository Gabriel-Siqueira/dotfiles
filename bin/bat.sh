#!/bin/bash
BATPATH=/sys/class/power_supply/BAT1
BAT_FULL=$BATPATH/charge_full
BAT_NOW=$BATPATH/charge_now

while true; do
    bf=$(cat $BAT_FULL)
    bn=$(cat $BAT_NOW)
    if [ $(( 100 * $bn / $bf )) -lt 10 ]
    then
         twmnc -s 50 --bg 'red' --fg 'yellow' -c "
    >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> low battery  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
        sleep 1m
    else
        sleep 5m
    fi
done
