#!/bin/bash

GTK2_CONF=~/.gtkrc-2.0
GTK3_CONF=~/.config/gtk-3.0/settings.ini

if [ `cat /tmp/theme.txt` = "light" ]
then
    cat $GTK2_CONF | sed -e 's/Adwaita"/Adwaita-dark"/' > $GTK2_CONF.tmp && mv $GTK2_CONF.tmp $GTK2_CONF
    cat $GTK3_CONF | sed -e 's/Adwaita$/Adwaita-dark/' > $GTK3_CONF.tmp && mv $GTK3_CONF.tmp $GTK3_CONF
    gsettings set org.gnome.desktop.interface gtk-theme "Adwaita-dark"
    ln -s -f ~/.config/kitty/dark.conf ~/.config/kitty/theme.conf
    kitty @ --to unix:@mykitty set-colors -a -c ~/.config/kitty/dark.conf
    echo "dark" > /tmp/theme.txt
else
    cat $GTK2_CONF | sed -e 's/Adwaita-dark"/Adwaita"/' > $GTK2_CONF.tmp && mv $GTK2_CONF.tmp $GTK2_CONF
    cat $GTK3_CONF | sed -e 's/Adwaita-dark$/Adwaita/' > $GTK3_CONF.tmp && mv $GTK3_CONF.tmp $GTK3_CONF
    ln -s -f ~/.config/kitty/light.conf ~/.config/kitty/theme.conf
    kitty @ --to unix:@mykitty set-colors -a -c ~/.config/kitty/light.conf
    echo "light" > /tmp/theme.txt
fi

pkill firefox && firefox
