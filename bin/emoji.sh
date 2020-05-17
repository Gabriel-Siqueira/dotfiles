#!/bin/zsh

# Based on script from LukeSmithxyz https://github.com/LukeSmithxyz/voidrice/blob/master/.local/bin/dmenuunicode
# Must have xclip installed to even show menu.
xclip -h 2>/dev/null || exit 1

eval emojis=$MY_EMOJIS
chosen=$(cut -d ';' -f1 $emojis | rofi -dmenu -i -l 20 -matching normal | sed "s/ .*//")

[ "$chosen" != "" ] || exit

echo "$chosen" | tr -d '\n' | xclip -selection clipboard
