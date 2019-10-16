#!/bin/bash
MACRO=$(ls ~/.local/share/xmacro/ | rofi -dmenu -i -p 'macro')
xmacroplay "$DISPLAY" < ~/.local/share/xmacro/$MACRO
