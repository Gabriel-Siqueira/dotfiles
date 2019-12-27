#!/bin/bash
mv ~/.local/share/qutebrowser/sessions /home/${USER}/.local/share/Trash/files/sessions_$(date +%s)
cp -r ~/Dropbox/Backup/pc/qutebrowser/sessions ~/.local/share/qutebrowser/sessions
