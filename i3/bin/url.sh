URL=$(rofi -lines 0 -dmenu -i -p 'url')
if [[ ! -z $URL ]]
then
    qutebrowser $URL | i3-msg workspace auxE
fi
