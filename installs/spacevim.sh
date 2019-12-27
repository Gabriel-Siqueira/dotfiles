#!/usr/bin/env bash

if [ ! -e "$HOME/.SpaceVim" ]
then
    curl -sLf https://spacevim.org/install.sh | bash
    rm -rf ~/.SpaceVim.d
fi
