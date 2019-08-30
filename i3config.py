from sys import argv
from pathlib import Path
import socket

def choose(gene,gama,default):
    if pc == "GENe":
        return gene
    elif pc == "GAMa":
        return gama
    else:
        return default

pc = socket.gethostname()
my_wallpaper = choose("green_circle","blue_circle","yellow_circle") + ".jpg"
my_terminal = choose("termite","termite","i3-sensible-terminal")
my_s_terminal = choose("terminator","terminator","i3-sensible-terminal")
my_term_launch = choose("termite --name=","termite --name=","urxvt -name ")
my_menu = choose('"rofi -show run"','"rofi -show run"',"dmenu_run")
my_s_menu = choose("morc_menu","morc_menu","dmenu_run")
my_screen_shot = choose("gnome-screenshot -a","gnome-screenshot -a","gnome-screenshot -a")
my_bar = choose("polybar","polybar","i3bar")
my_lock = "exec i3lock -t -i ~/Dropbox/Pictures/lock_und_dm/guide_to_the_galaxy.png"
game = "game"
midi = "midi"
virM = "VirM"
docs = "docs"
auxE = "auxE"
deft = "deft"
auxD = "auxD"
read = "read"
deve = "deve"
mail = "mail"
role1 = "."
role2 = ".."
startup_workspace = deft
kp_ws = [game, virM, deve, mail, auxE, deft, auxD, midi, read, docs]
num_ws = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
ws = kp_ws + num_ws

def basic():
        s = ""
        s += "font pango:DejaVu Sans Mono 0\n"
        # use Mouse+$mod to drag floating windows to their wanted position
        s += "floating_modifier $mod\n"
        # no bar on single windom
        s += "new_window pixel\n"
        # hide border on screen edge
        s += "hide_edge_borders none\n"
        # focus does not folow mouse
        s += "focus_follows_mouse no\n"
        # layout for workspace level
        s += "workspace_layout tabbed\n"
        # mode key
        s += "set $mod Mod4\n"
        return s

def keys():
    terminals = [
          ("$mod+Return", "exec " + my_terminal)
        , ("$mod+Shift+Return","exec " + my_s_terminal)
    ]
    menus = [
          ("$mod+d",      "exec " + my_menu)
        , ("$mod+Shift+d","exec " + my_s_menu)
        , ("$mod+a",      'exec "rofi -show drun"')
        , ("$mod+o",      'exec "locate home | rofi -matching regex  -dmenu -i -p \'locate\' | xargs -r -0 xdg-open"')
        , ("$mod+w",      'exec ~/bin/url.sh')
    ]
    reshape = [("$mod+r","mode \"reshape\"")]
    split_containers = [("$mod+v","split v")]
    quit_reload_lock = [
          ("$mod+KP_Delete", my_lock)
        , ("$mod+Mod2+KP_Separator", my_lock)
        , ("$mod+e","reload")
        , ("$mod+Shift+r","restart")
        , ("$mod+Shift+e","exec \"i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -b 'Yes, exit i3' 'i3-msg exit'\"")
    ]
    screen_shot = [("Print","exec " + my_screen_shot)]
    volume = [
          ("$mod+F5",              "exec amixer set Master 5%+")
        , ("XF86AudioRaiseVolume", "exec amixer set Master 5%+")
        , ("$mod+F6",              "exec amixer set Master 5%-")
        , ("XF86AudioLowerVolume", "exec amixer set Master 5%-")
        , ("XF86AudioMute",        "exec amixer set Master toggle")
    ]
    brightness = [
          ("XF86MonBrightnessDown","exec xbacklight -dec 5")
        , ("$mod+F7",              "exec xbacklight -dec 1")
        , ("XF86MonBrightnessUp",  "exec xbacklight -inc 5")
        , ("$mod+F8",              "exec xbacklight -inc 1")
    ]
    media = [
          ("XF86AudioPlay",  "exec playerctl play")
        , ("XF86AudioPause", "exec playerctl pause")
        , ("XF86AudioNext",  "exec playerctl next")
        , ("XF86AudioPrev",  "exec playerctl previous")
        , ("XF86AudioStop",  "exec playerctl stop")
    ]
    close = [("$mod+BackSpace","kill")]
    full_screen = [("$mod+Shift+f", "fullscreen")]
    l = terminals + menus + reshape + split_containers + quit_reload_lock + screen_shot + volume + brightness + media + close + full_screen + k_focus_move() + k_workspaces() + k_scratchpad()
    return add_keys(l)

def add_keys(l):
    return "\n".join(["bindsym " + key + " " + com for (key,com) in l])

def k_focus_move():
    focus = [
          ("$mod+h",    "focus left")
        , ("$mod+j",    "focus down")
        , ("$mod+k",    "focus up")
        , ("$mod+l",    "focus right")
        , ("$mod+Left", "focus left")
        , ("$mod+Down", "focus down")
        , ("$mod+Up",   "focus up")
        , ("$mod+Right","focus right")
        , ("$mod+u", "[urgent=latest] focus")
        , ("$mod+space", "focus mode_toggle")
    ]
    move = [
          ("$mod+Shift+h",    "move left")
        , ("$mod+Shift+j",    "move down")
        , ("$mod+Shift+k",    "move up")
        , ("$mod+Shift+l",    "move right")
        , ("$mod+Shift+Left", "move left")
        , ("$mod+Shift+Down", "move down")
        , ("$mod+Shift+Up",   "move up")
        , ("$mod+Shift+Right","move right")
    ]
    return focus + move

def k_workspaces():
    toggle = [
          ("$mod+Tab", "workspace back_and_forth")
        , ("$mod+m", "mark swap")
        , ("$mod+Shift+Tab", "[con_mark=\"swap\"] focus")
    ]
    mult_screen = [
          ("$mod+x", "move workspace to output right")
        , ("$mod+Shift+x", "move workspace to output left")
    ]
    kp = ["KP_Insert", "KP_End", "KP_Down", "KP_Page_Down", "KP_Left", "KP_Begin", "KP_Right", "KP_Home", "KP_Up", "KP_Page_Up"]
    mech_keys = ["Insert", "Home", "Page_Up", "Delete", "End", "Page_Down","Mod1+Insert","Mod1+Home","Mod1+Page_Up","Mod1+Delete","Mod1+End","Mod1+Page_Down"]
    mech_wp = [midi,read,docs,auxE,deft,auxD,virM,deve,mail,role1,game,role2]
    kp_nl = ["KP_0", "KP_1", "KP_2", "KP_3", "KP_4", "KP_5", "KP_6", "KP_7", "KP_8", "KP_9"]
    num = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
    switch_ws1 = [("$mod+" + k, "workspace " + w) for (k,w) in zip(kp, kp_ws)]
    switch_ws_mech = [("$mod+" + k, "workspace " + w) for (k,w) in zip(mech_keys, mech_wp)]
    switch_ws2 = [("$mod+Mod2+" + k, "workspace " + w) for (k,w) in zip(kp_nl, kp_ws)]
    switch_ws3 = [("$mod+" + k, "workspace " + w) for (k,w) in zip(num, num_ws)]
    move_ws1 = [("$mod+Shift+" + k, "move container to workspace " + w) for (k,w) in zip(kp_nl, kp_ws)]
    move_ws_mech = [("$mod+Shift" + k, "move container to workspace " + w) for (k,w) in zip(mech_keys, mech_wp)]
    move_ws2 = [("$mod+Shift+Mod2+" + k, "move container to workspace " + w) for (k,w) in zip(kp, kp_ws)]
    move_ws3 = [("$mod+Shift+" + k, "move container to workspace " + w) for (k,w) in zip(num, num_ws)]
    return toggle + mult_screen + switch_ws_mech + switch_ws1 + switch_ws2 + switch_ws3 + move_ws1 + move_ws_mech + move_ws2 + move_ws3

def k_scratchpad():
        sp = [
                  ("$mod+f", "file", "sp-file")
                , ("$mod+t", "ster", "sp-term")
                , ("$mod+p", "top", "sp-top")
        ]
        return [ (k, '[instance="' + i + '"] scratchpad show; [instance="' + i + '"] move position center; mode "' + m + '"') for (k,i,m) in sp]

def modes():
        reshape = [
                  ("minus",       "resize shrink width  5 px or 5 ppt")
                , ("plus",        "resize grow   width  5 px or 5 ppt")
                , ("Shift+minus", "resize shrink height 5 px or 5 ppt")
                , ("Shift+plus",  "resize grow   height 5 px or 5 ppt")
                , ("Escape","mode \"default\"")
                , ("f",       "fullscreen")
                , ("s",       "layout stacking")
                , ("t",       "layout tabbed")
                , ("h",       "layout splith")
                , ("v",       "layout splitv")
                , ("Shift+f", "floating toggle")
                , ("p",       "focus parent")
                , ("c",       "focus child")
                , ("Escape",  "mode \"default\"")
                , ("$mod+BackSpace","kill")
        ]
        def sp(k,i):
                return [
                          (k, '[instance="' + i + '"] focus; [instance="' + i + '"] scratchpad show; mode \"default\"')
                        , ("$mod+s", '[instance="' + i + '"] focus')
                        , ("$mod+h",        "move left 20px")
                        , ("$mod+j",        "move down 20px")
                        , ("$mod+k",        "move up 20px")
                        , ("$mod+l",        "move right 20px")
                        , ("$mod+Left",     "move left 20px")
                        , ("$mod+Down",     "move down 20px")
                        , ("$mod+Up",       "move up 20px")
                        , ("$mod+Right",    "move right 20px")
                        , ("$mod+space",    "move position 0px 0px")
                        , ("$mod+Escape",   "mode \"default\"")
                ]
        sp_file = sp("$mod+f","file")
        sp_term = sp("$mod+t","ster")
        sp_top = sp("$mod+p","top")
        mod = [("reshape",reshape + k_focus_move() + k_workspaces()), ("sp-file", sp_file), ("sp-term", sp_term), ("sp-top", sp_top)]
        return "\n".join(['mode "' + m + '" {\n' + add_keys(l) + '\n}' for (m,l) in mod])

def colors():
#       class                     border  backgr.  text   indic. child_border
        return """
        client.focused           #aaaaaa #aaaaaa #00ff00 #111111 #111111
        client.unfocused         #aaaaaa #111111 #888888 #111111 #111111
        client.urgent            #ff0000 #ff0000 #ff0000 #111111 #111111
        client.placeholder       #111111 #111111 #ffffff #111111 #111111
        client.focused_inactive  #aaaaaa #555555 #ffffff #111111 #111111
        client.background        #ffffff """

def bar():
        if my_bar == "polybar":
                return "exec_always --no-startup-id $HOME/bin/polybar.sh"
        else:
        # colors: <colorclass>  <border>  <background>  <text>
                return """
        bar {
        status_command i3status
        position top
        workspace_buttons yes
        binding_mode_indicator yes
        font pango:Terminus 11px
        colors{
            background #111111
            statusline #eeeeee
            separator  #666666
            focused_workspace  #111111 #111111 #ffff00
            active_workspace   #111111 #111111 #ffff00
            inactive_workspace #111111 #111111 #999900"
            urgent_workspace   #111111 #111111 #ff0000"
            binding_mode       #111111 #111111 #ff5500"
        }
        } """

def place():
    def clas(name):
            return '[class="^' + name + '$"]'
    def inst(name):
            return '[instance="^' + name + '$"]'
    wp_list = [
              (game, clas, ["Steam", "Mainwindow.py", "Minetest"])
            , (midi, clas, ["Vlc", "Kodi", "Spotify", "Lollypop"])
            , (virM, clas, ["VirtualBox"])
            , (docs, clas, ["libreoffice", "libreoffice-startcenter", "libreoffice-writer", "libreoffice-calc", "libreoffice-impress", "libreoffice-draw", "libreoffice-math", "libreoffice-base"])
            , (auxE, clas, ["Firefox", "qutebrowser"])
            , (deft, clas, ["Emacs"])
            , (auxD, clas, ["Chromium", "google-chrome", "vivaldi-stable", "Opera"])
            , (read, clas, ["calibre"])
            , (deve, clas, ["Eclipse"])
            , (mail, clas, ["thunderbird", "TelegramDesktop", "Franz", "Inboxer"])
    ]
    com_list = [
        ("file", inst, ["move scratchpad","floating enable","resize set 1100 600"])
        , ("simplenote", inst, ["move scratchpad","floating enable","resize set 1200 700"])
        , ("ster", inst, ["move scratchpad","floating enable","resize set 1100 600"])
        , ("top", inst, ["move scratchpad","floating enable","resize set 1100 600"])
    ]
    float_list = [
              ("pavucontrol", clas)
    ]
    fix = ["\n".join(["for_window " + criteria(name) + " move to workspace " + wp for name in names]) for (wp, criteria, names) in wp_list]
    comands = ["\n".join(["for_window " + criteria(name) + " " + com for com in coms]) for (name, criteria, coms) in com_list]
    float_ = ["for_window " + criteria(name) + " floating enable" for (name, criteria) in float_list]
    return "\n".join(fix + comands + float_)

def auto_start():
        apps = [  "dropbox"
                , "redshift-gtk"
                , "alarm-clock-applet --hidden"
                , "nm-applet"
                , "xfce4-power-manager"
                , "feh --bg-fill ~/Dropbox/Pictures/mywallpaper/" + my_wallpaper
                , "compton"
                , "emacs"
                , "qutebrowser"
                , "chromium"
                , "--no-startup-id dunst"
                # , "franz"
                , "gdfs default ~/Drive"
                , my_term_launch + "ster -e tmux"
                , my_term_launch + "file -e ranger"
                , my_term_launch + "top -e htop"
                ]
        return "\n".join(["exec " + app for app in apps])

def start():
        return "exec --no-startup-id i3-msg workspace " + startup_workspace

def main():
        with open(Path.joinpath(Path.home(),".config/i3/config"),'w') as f:
                f.write("\n".join([basic(), keys(), modes(), colors(), bar(), place(), auto_start(), start()]))

if __name__=="__main__":
    main()
