#!/bin/bash
URL=$(rofi -lines 0 -dmenu -i -p 'url')
if [[ -n $URL ]]
then
    qutebrowser "$URL" | i3-msg workspace auxE
fi
