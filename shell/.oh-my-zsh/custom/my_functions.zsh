# Save/Load tmux session with tmuxp
function tmux_ic() {
    sed -i -e 's/\/home\/gabriel/\/home\/ec2014\/ra155446/g' ~/Dropbox/backup/tmux/last
    sed -i -e 's/\/MEGA\/unicamp/\/Dropbox/g' ~/Dropbox/backup/tmux/last
}
function tmux_casa() {
    sed -i -e 's/\/home\/ec2014\/ra155446/\/home\/gabriel/g' ~/Dropbox/backup/tmux/last
}

# Use ranger to switch directories
function rcd() {
    tmp="$(mktemp)"
    ranger --choosedir="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp"
        [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
    fi
}

# betterbib without abbrev
function bbib() {
  pyenv activate 3.8
  pyenv exec betterbib up -l $1 | pyenv exec betterbib f -b - > $2
  pyenv deactivate
}
