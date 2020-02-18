#!/bin/zsh
eval MACROS_DIR=$MY_MACROS
MACRO=$(ls $MACROS_DIR | rofi -dmenu -i -p 'macro')
xmacroplay "$DISPLAY" < $MACROS_DIR/$MACRO
