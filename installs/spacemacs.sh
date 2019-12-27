#!/usr/bin/env bash

if [ ! -e "$HOME/.spacemacs" ]
then
	git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
	rm -f ~/.spacemacs
fi
