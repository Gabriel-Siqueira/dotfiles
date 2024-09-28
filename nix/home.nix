{ config, pkgs, lib, specialArgs, ... }:

let
  inherit (specialArgs) withGUI inWSL;
  inherit (lib) optionals mkIf;
in
{

  home = {
    inherit (pkgs) stateVersion;
    inherit (pkgs) username;
    inherit (pkgs) homeDirectory;

    sessionVariables = {
      DROPBOX = if inWSL then "/mnt/c/Users/Dell/Dropbox" else "$HOME/Dropbox";
      MY_WIKI = "$DROPBOX/Local/Wiki/";
      MY_LEDGER = "$DROPBOX/Personal/finance.ledger";
      MY_REFS = "$DROPBOX/Local/Ref/pdfs/";
      MY_BIB = "$DROPBOX/Local/Ref/better-ref.bib";
      MY_OBSIDIAN = "$DROPBOX/Local/Obsidian";
      MY_NIX = "$HOME/.dotfiles/nix";
    };

    packages =
      with pkgs;
      let

        tex = (pkgs.texlive.combine {
          inherit (pkgs.texlive)
            adjustbox
            amsmath
            babel
            babel-portuges
            beamertheme-metropolis
            biblatex
            comment
            csquotes
            enumitem
            environ
            hyperref
            ifoddpage
            lipsum
            makecell
            multirow
            placeins
            relsize
            scheme-medium
            subfigure
            tabu
            threeparttable
            titlesec
            titling
            todonotes
            ulem
            wrapfig
            xurl
            ;
        });

        python-with-packages = pkgs.python3.withPackages (ps: with ps; [
          csv2md
          ipython
          jinja2
          markdown
          matplotlib
          numpy
          pandas
          pytz
          weasyprint
          # (
          #   buildPythonPackage rec {
          #     pname = "csvtomd";
          #     version = "0.3.0";
          #     src = fetchPypi {
          #       inherit pname version;
          #       sha256 = "sha256-ofvx24bUt7YqddwlmAdxmyMB7QHbXR19m7ScSohYd4s=";
          #     };
          #     doCheck = false;
          #     propagatedBuildInputs = [
          #       # Specify dependencies
          #     ];
          #   }
          # )
        ]);

        # agda-with-packages = agda.withPackages (ps: with ps; [
        #   standard-library
        # ]);

        my-rPackages = with rPackages; [
          FactoMineR
          Hmisc
          NbClust
          ade4
          akima
          amap
          apcluster
          caret
          corrplot
          cowplot
          dbscan
          dplyr
          factoextra
          fclust
          forecast
          fpc
          fpp2
          ggplot2
          ggrepel
          ggridges
          ggstatsplot
          glmnet
          gridExtra
          gt
          hexbin
          hrbrthemes
          kableExtra
          patchwork
          pROC
          plot3D
          plotly
          ppclust
          ramify
          randomForest
          reshape2
          rgl
          rpart
          rpart_plot
          sjPlot
          skimr
          tidyverse
          zoo
        ];
        r-with-packages = rWrapper.override {
          packages = my-rPackages;
        };
        rstudio-with-packages = rstudioWrapper.override {
          packages = my-rPackages;
        };

      in
      optionals withGUI [
        android-tools
        calibre
        droidcam
        dropbox
        filelight
        firefox
        google-chrome
        kitty
        libreoffice
        obs-studio
        obsidian
        openvpn
        pulseaudio
        spotify
        vlc
        write_stylus
        yt-dlp

        # KDE
        kdePackages.yakuake

        # Social
        discord
        skypeforlinux
        slack
        zoom-us

        # Programming
        rstudio-with-packages
      ]
      ++ optionals (!withGUI) [
        r-with-packages
      ]
      ++ [
        # Misc command line tools
        biber
        fzf
        ghostscript
        groff
        htop
        hugo
        ledger
        libwebp
        lshw
        openjdk
        pandoc
        poppler_utils
        ranger
        unzip
        usbutils
        wget
        xclip
        zip

        # Programming
        # agda-with-packages
        gcc
        cmake
        git-filter-repo
        gnumake
        go
        lean4
        perl
        python-with-packages
        tex
        valgrind

        # Packages used in vim
        fd
        ripgrep
        tree-sitter

        # Language Servers
        ccls # C/C++
        ltex-ls # Latex (grammar)
        lua-language-server # lua
        pyright # python
        nil # nix
        texlab # Latex
        # haskellPackages.agda-language-server # Agda
      ];
  };

  programs = {
    git = {
      enable = true;
      userName = "Gabriel-Siqueira";
      userEmail = "gabriel.gabrielhs@gmail.com";
    };

    ssh = {
      enable = true;
      extraConfig = ''
        ${builtins.readFile(./ssh_config)}
      '';
    };

    firefox = mkIf (withGUI) {
      package = pkgs.firefox.override {
        cfg.nativeMessagingHosts.packages = [ pkgs.kdePackages.plasma-browser-integration ];
      };
      enable = true;
      profiles.defaut.userChrome = ''
        /* Hide tab bar in FF Quantum */
        #TabsToolbar {
          visibility: collapse !important;
        margin-bottom: 21px !important;
        }

        #sidebar-box[sidebarcommand="treestyletab_piro_sakura_ne_jp-sidebar-action"] #sidebar-header {
          visibility: collapse !important;
        }
      '';
    };

    neovim =
      {
        enable = true;
        defaultEditor = true;
        vimAlias = true;

        plugins = with pkgs.vimPlugins; [

          vim-sleuth # Guess tab related settings for each file
          vim-commentary # Command to comment and uncomment lines
          vim-visual-star-search
          vim-surround # Change surrounding things
          vim-unimpaired # Multiple pairs of keybindings
          # vim-textobj-between         # Motion between for any character
          targets-vim # More targets for surrounding characters

          {
            # Fuzzy Finder for a lot of stuff
            plugin = telescope-nvim;
            type = "lua";
            config = ''
              ${builtins.readFile(./vim/plugins_conf/telescope.lua)}
            '';
          }
          telescope-fzf-native-nvim
          telescope-undo-nvim
          pkgs.vimExtraPlugins.telescope-bibtex-nvim

          {
            # Alternative UI
            plugin = noice-nvim;
            type = "lua";
            config = ''
              ${builtins.readFile(./vim/plugins_conf/noice.lua)}
            '';
          }

          {
            # Deal with git
            plugin = fugitive;
            type = "lua";
            config = ''
              ${builtins.readFile(./vim/plugins_conf/fugitive.lua)}
            '';
          }

          {
            # Deal with ledger
            plugin = vim-ledger;
            type = "lua";
            config = ''
              ${builtins.readFile(./vim/plugins_conf/ledger.lua)}
            '';
          }

          {
            # Show keymaps
            plugin = which-key-nvim;
            type = "lua";
            config = ''
              ${builtins.readFile(./vim/plugins_conf/which_key.lua)}
            '';
          }

          {
            # Asynchronous make
            plugin = vim-dispatch;
            type = "lua";
            config = ''
              ${builtins.readFile(./vim/plugins_conf/dispatch.lua)}
            '';
          }

          {
            # Open alternative files
            plugin = other-nvim;
            type = "lua";
            config = ''
              ${builtins.readFile(./vim/plugins_conf/other.lua)}
            '';
          }

          {
            # move following letters
            plugin = hop-nvim;
            type = "lua";
            config = ''
              ${builtins.readFile(./vim/plugins_conf/hop.lua)}
            '';
          }

          {
            # Seamless navigation with zellij-nav-nvim
            plugin = zellij-nav-nvim;
            config = ''
              ${builtins.readFile(./vim/plugins_conf/zellij_navigator.vim)}
            '';
          }

          {
            # Syntax highlighting and other functionalities using tree-sitter
            plugin = nvim-treesitter.withAllGrammars;
            type = "lua";
            config = ''
              ${builtins.readFile(./vim/plugins_conf/treesitter.lua)}
            '';
          }

          {
            # To edit files from obsidian vault
            plugin = pkgs.vimExtraPlugins.obsidian-nvim;
            type = "lua";
            config = ''
              ${builtins.readFile(./vim/plugins_conf/obsidian.lua)}
            '';
          }

          {
            # Snippets
            plugin = luasnip;
            type = "lua";
            config = ''
              ${builtins.readFile(./vim/plugins_conf/luasnip.lua)}
            '';
          }

          {
            # Collection of language servers configurations
            plugin = nvim-lspconfig;
            type = "lua";
            config = ''
              ${builtins.readFile(./vim/plugins_conf/lsp_config.lua)}
            '';
          }

          {
            # Autocompletion plugin and other plugins for specific type of completions
            plugin = nvim-cmp;
            type = "lua";
            config = ''
              ${builtins.readFile(./vim/plugins_conf/cmp.lua)}
            '';
          }
          cmp-nvim-lsp
          cmp-buffer
          cmp-path
          cmp-cmdline
          cmp_luasnip

          {
            # Use IA for code suggestions
            plugin = copilot-vim;
            config = ''
              ${builtins.readFile(./vim/plugins_conf/copilot.vim)}
            '';
          }

          {
            # Theme
            plugin = gruvbox-material;
            config = ''
              ${builtins.readFile(./vim/plugins_conf/gruvbox_material.vim)}
            '';
          }

          {
            # Status line
            plugin = lualine-nvim;
            type = "lua";
            config = ''
              ${builtins.readFile(./vim/plugins_conf/lualine.lua)}
            '';
          }

          {
            # Lean support
            plugin = lean-nvim;
            type = "lua";
            config = ''
              ${builtins.readFile(./vim/plugins_conf/lean.lua)}
            '';
          }

        ];

        extraLuaConfig =
          ''
            ${builtins.readFile(./vim/config.lua)}
          '';
      };

    zellij = {
      enable = true;
      enableZshIntegration = false;
      settings = {
        theme = "gruvbox";
        pane_frames = false;
        # keybindings.normal = {
        #   "bind \"Ctrl Left\"" = { MoveFocus = "Left"; };
        #   "bind \"Ctrl Right\"" = { MoveFocus = "Right"; };
        #   "bind \"Ctrl Down\"" = { MoveFocus = "Down"; };
        #   "bind \"Ctrl Up\"" = { MoveFocus = "Up"; };
        # };
      };
    };

    tmux = {
      enable = true;
      plugins = with pkgs.tmuxPlugins; [
        sensible
        pain-control
        {
          plugin = catppuccin;
          extraConfig = ''
            ${builtins.readFile(./tmux/tmux_catppuccin.conf)}
          '';
        }
        yank
        resurrect
        continuum
      ];
      extraConfig = ''
        ${builtins.readFile(./tmux/tmux_extra.conf)}
      '';
    };

    # Better cd
    zoxide = {
      enable = true;
    };

    # Better ls
    eza = {
      enable = true;
    };

    zsh = {
      enable = true;
      autosuggestion.enable = true;
      enableCompletion = true;
      autocd = true;
      history = {
        size = 100000;
        save = 100000;
        path = "${config.xdg.dataHome}/zsh/history";
        ignoreDups = true;
      };

      shellAliases = {
        ls = "eza";
        ll = "eza -l";
        la = "eza -a";
        lh = "eza -lh";
        ltr = "eza -ltr";
        cd = "z";
        "cd.." = "z ..";
        "." = "pwd";
        ".." = "z ..";
        "..." = "z ../..";
        cp = "cp -ai";
        rm = "rm -i";
        grep = "grep --color=auto";
        clr = "clear";
        df = "df -h";
        df10 = "df -H";
        du = "du -h";
        update = "sudo nixos-rebuild switch --flake ~/.dotfiles/nix/";
        r = "R";
      };

      dirHashes = {
        dow = "$HOME/Downloads";
        doc = "$HOME/Documents";
        dro = "$DROPBOX";
        dri = "$HOME/Drive";
        ath = "$DROPBOX/Context/mestrado/";
        had = "$DROPBOX/Context/mestrado/had/";
        min = "$DROPBOX/Context/doutorado/min/";
        cyc = "$DROPBOX/Context/doutorado/GitHub/cyc/";
        hub = "$DROPBOX/Context/doutorado/GitHub/";
        chi = "$DROPBOX/Context/doutorado/chi/";
        jup = "$DROPBOX/Context/doutorado/jup/";
        chim = "$DROPBOX/Context/doutorado/chim/";
        tho = "$DROPBOX/Context/doutorado/tho/";
        maze = "$DROPBOX/Context/programing/maze/";
        her = "$DROPBOX/Context/doutorado/her";
        ran = "$HOME/Random";
        trash = "$HOME/.local/share/Trash/files";
      };

      zplug = {
        enable = true;
        plugins = [
          { name = "zsh-users/zsh-autosuggestions"; }
          { name = "zsh-users/zsh-syntax-highlighting"; }
          { name = "hcgraf/zsh-sudo"; }
          { name = "zuxfoucault/colored-man-pages_mod"; }
          { name = "unixorn/fzf-zsh-plugin"; }
          {
            name = "romkatv/powerlevel10k";
            tags = [ as:theme depth:1 ];
          }
        ];
      };

      initExtra = ''
        source ~/.p10k.zsh
      '';
    };

    plasma = mkIf (withGUI) {
      enable = true;
      shortcuts = {
        "ActivityManager"."switch-to-activity-21001635-79a1-4b14-84d4-144a95ac249f" = "Meta+Shift+G";
        "ActivityManager"."switch-to-activity-a4a61117-3acf-49d3-adb6-a9e3fc329ce4" = "Meta+Shift+L";
        "ActivityManager"."switch-to-activity-b3fad55d-ee09-4b18-b1b5-b3360c222daf" = "Meta+Shift+S";
        "ActivityManager"."switch-to-activity-c53d6251-38db-43e1-b32b-5426eec2108c" = "Meta+Shift+W";
        "kmix"."decrease_microphone_volume" = "Microphone Volume Down";
        "kmix"."decrease_volume" = "Volume Down";
        "kmix"."increase_microphone_volume" = "Microphone Volume Up";
        "kmix"."increase_volume" = "Volume Up";
        "kmix"."mic_mute" = [ "Microphone Mute" "Meta+Volume Mute" ];
        "kmix"."mute" = "Volume Mute";
        "ksmserver"."Lock Session" = [ "Meta+L" "Screensaver" ];
        "kwin"."ExposeAll" = "Meta+W";
        "kwin"."ExposeClass" = "Meta+Alt+W";
        "kwin"."Kill Window" = "Meta+Shift+Backspace";
        "kwin"."Toggle Grid View" = "Meta+Num+0";
        "kwin"."Switch Window Down" = "Meta+Down";
        "kwin"."Switch Window Left" = "Meta+Left";
        "kwin"."Switch Window Right" = "Meta+Right";
        "kwin"."Switch Window Up" = "Meta+Up";
        "kwin"."Window Quick Tile Bottom" = "Meta+Shift+Down";
        "kwin"."Window Quick Tile Top" = "Meta+Shift+Up";
        "kwin"."Window Quick Tile Left" = "Meta+Shift+Left";
        "kwin"."Window Quick Tile Right" = "Meta+Shift+Right";
        "kwin"."Switch to Desktop 1" = "Meta+Num+1";
        "kwin"."Switch to Desktop 2" = "Meta+Num+2";
        "kwin"."Switch to Desktop 3" = "Meta+Num+3";
        "kwin"."Switch to Desktop 4" = "Meta+Num+4";
        "kwin"."Switch to Desktop 5" = "Meta+Num+5";
        "kwin"."Switch to Desktop 6" = "Meta+Num+6";
        "kwin"."Switch to Desktop 7" = "Meta+Num+7";
        "kwin"."Switch to Desktop 8" = "Meta+Num+8";
        "kwin"."Switch to Desktop 9" = "Meta+Num+9";
        "kwin"."Switch to Next Screen" = "Meta+N";
        "kwin"."Switch to Previous Screen" = "Meta+P";
        "kwin"."Window Close" = "Meta+Backspace";
        "kwin"."Window Fullscreen" = "Meta+Shift+F";
        "kwin"."Window Maximize" = "Meta+F";
        "kwin"."Window Minimize" = "Meta+M";
        "kwin"."Window to Desktop 1" = "Meta+Shift+Num+End";
        "kwin"."Window to Desktop 2" = "Meta+Shift+Num+Down";
        "kwin"."Window to Desktop 3" = "Meta+Shift+Num+PgDown";
        "kwin"."Window to Desktop 4" = "Meta+Shift+Num+Left";
        "kwin"."Window to Desktop 5" = "Meta+Shift+Num+Clear";
        "kwin"."Window to Desktop 6" = "Meta+Shift+Num+Right";
        "kwin"."Window to Desktop 7" = "Meta+Shift+Num+Home";
        "kwin"."Window to Desktop 8" = "Meta+Shift+Num+Up";
        "kwin"."Window to Desktop 9" = "Meta+Shift+Num+PgUp";
        "kwin"."Window to Next Screen" = "Meta+Shift+N";
        "kwin"."Window to Previous Screen" = "Meta+Shift+P";
        "kwin"."view_actual_size" = "Meta+=";
        "kwin"."view_zoom_in" = "Meta++";
        "kwin"."view_zoom_out" = "Meta+-";
        "mediacontrol"."nextmedia" = "Media Next";
        "mediacontrol"."pausemedia" = "Media Pause";
        "mediacontrol"."playpausemedia" = "Media Play";
        "mediacontrol"."previousmedia" = "Media Previous";
        "mediacontrol"."stopmedia" = "Media Stop";
        "org.kde.kitty.desktop"."_launch" = "Meta+Return";
        "org.kde.krunner.desktop"."_launch" = [ "Search" "Meta+D" ];
        "org.kde.plasma.emojier.desktop"."_launch" = "Meta+.";
        "org.kde.spectacle.desktop"."ActiveWindowScreenShot" = "Meta+Shift+Print";
        "org.kde.spectacle.desktop"."FullScreenScreenShot" = "Shift+Print";
        "org.kde.spectacle.desktop"."RectangularRegionScreenShot" = "Meta+Print";
        "org.kde.spectacle.desktop"."_launch" = "Print";
        "org_kde_powerdevil"."Decrease Keyboard Brightness" = "Keyboard Brightness Down";
        "org_kde_powerdevil"."Decrease Screen Brightness" = "Monitor Brightness Down";
        "org_kde_powerdevil"."Hibernate" = "Hibernate";
        "org_kde_powerdevil"."Increase Keyboard Brightness" = "Keyboard Brightness Up";
        "org_kde_powerdevil"."Increase Screen Brightness" = "Monitor Brightness Up";
        "org_kde_powerdevil"."PowerDown" = "Power Down";
        "org_kde_powerdevil"."PowerOff" = "Power Off";
        "org_kde_powerdevil"."Sleep" = "Sleep";
        "org_kde_powerdevil"."Toggle Keyboard Backlight" = "Keyboard Light On/Off";
        "plasmashell"."manage activities" = "Meta+A";
        "plasmashell"."next activity" = "Meta+Tab";
        "plasmashell"."previous activity" = "Meta+Shift+Tab";
        "systemsettings.desktop"."_launch" = "Tools";
        "yakuake"."toggle-window-state" = "Meta+T";
      };
      configFile = {
        "kactivitymanagerdrc"."activities"."21001635-79a1-4b14-84d4-144a95ac249f".value = "G";
        "kactivitymanagerdrc"."activities"."a4a61117-3acf-49d3-adb6-a9e3fc329ce4".value = "L";
        "kactivitymanagerdrc"."activities"."b3fad55d-ee09-4b18-b1b5-b3360c222daf".value = "S";
        "kactivitymanagerdrc"."activities"."c53d6251-38db-43e1-b32b-5426eec2108c".value = "W";
        "kactivitymanagerdrc"."activities-icons"."21001635-79a1-4b14-84d4-144a95ac249f".value = "/home/gabriel/Dropbox/Backup/pc/icons/purple.svg";
        "kactivitymanagerdrc"."activities-icons"."a4a61117-3acf-49d3-adb6-a9e3fc329ce4".value = "/home/gabriel/Dropbox/Backup/pc/icons/yellow.svg";
        "kactivitymanagerdrc"."activities-icons"."b3fad55d-ee09-4b18-b1b5-b3360c222daf".value = "/home/gabriel/Dropbox/Backup/pc/icons/blue.svg";
        "kactivitymanagerdrc"."activities-icons"."c53d6251-38db-43e1-b32b-5426eec2108c".value = "/home/gabriel/Dropbox/Backup/pc/icons/red.svg";
        "kactivitymanagerdrc"."main"."currentActivity".value = "c53d6251-38db-43e1-b32b-5426eec2108c";
        "kwinrc"."Desktops"."Id_1".value = "83b69802-e2e5-4bd4-8075-4f437301100c";
        "kwinrc"."Desktops"."Id_2".value = "0817145f-47e6-4908-a1df-44ea8d12d4d1";
        "kwinrc"."Desktops"."Id_3".value = "1ef811de-5c05-48a1-8d42-e5cb2d4edb9a";
        "kwinrc"."Desktops"."Id_4".value = "95b3ace1-1d66-46e6-a8a4-7bd7954c1a39";
        "kwinrc"."Desktops"."Id_5".value = "b144560d-0a68-49c2-9cf2-5229a65faf23";
        "kwinrc"."Desktops"."Id_6".value = "35d581c4-bf54-48ce-b529-48b71b4a3b90";
        "kwinrc"."Desktops"."Id_7".value = "8747464e-26eb-4705-8062-97044541849f";
        "kwinrc"."Desktops"."Id_8".value = "a6145aa2-40a3-44a0-872a-c89b9e5fe7a1";
        "kwinrc"."Desktops"."Id_9".value = "2dd20c0c-40de-4da4-b52b-ffb94fa4e998";
        "kwinrc"."Desktops"."Number".value = 9;
        "kwinrc"."Desktops"."Rows".value = 4;
        "kwinrc"."Effect-magnifier"."Height".value = 500;
        "kwinrc"."Effect-magnifier"."Width".value = 500;
        "kwinrc"."NightColor"."Active".value = true;
        "kwinrc"."Plugins"."magnifierEnabled".value = true;
        "kwinrc"."Plugins"."zoomEnabled".value = false;
        "kwinrulesrc"."1"."Description".value = "Firefox in all activities";
        "kwinrulesrc"."1"."activity".value = "00000000-0000-0000-0000-000000000000";
        "kwinrulesrc"."1"."activityrule".value = 2;
        "kwinrulesrc"."1"."wmclass".value = "firefox";
        "kwinrulesrc"."1"."wmclassmatch".value = 2;
        "kwinrulesrc"."General"."count".value = 1;
        "kwinrulesrc"."General"."rules".value = 1;
        "kwinrulesrc"."fbfd0f9c-d44c-4e3a-b00e-0365005b9463"."Description".value = "Firefox in all activities";
        "kwinrulesrc"."fbfd0f9c-d44c-4e3a-b00e-0365005b9463"."activity".value = "00000000-0000-0000-0000-000000000000";
        "kwinrulesrc"."fbfd0f9c-d44c-4e3a-b00e-0365005b9463"."activityrule".value = 3;
        "kwinrulesrc"."fbfd0f9c-d44c-4e3a-b00e-0365005b9463"."wmclass".value = "Firefox";
        "kwinrulesrc"."fbfd0f9c-d44c-4e3a-b00e-0365005b9463"."wmclassmatch".value = 2;
        "plasma-localerc"."Formats"."LANG".value = "en_US.UTF-8";
        "plasmarc"."Wallpapers"."usersWallpapers".value = "/home/gabriel/Dropbox/Backup/pc/mywallpaper/purple_circle.jpg,/home/gabriel/Dropbox/Backup/pc/mywallpaper/red_circle.jpg,/home/gabriel/Dropbox/Backup/pc/mywallpaper/yellow_circle.jpg,/home/gabriel/Dropbox/Backup/pc/mywallpaper/blue_circle.jpg";
      };
      workspace = {
        theme = "breeze-dark";
        colorScheme = "BreezeDark";
      };
    };

  };

  xdg.configFile = {
    "ranger/commands.py".source = ./ranger/commands.py;
    "ranger/rc.conf".source = ./ranger/rc.conf;
    "ranger/rifle.conf".source = ./ranger/rifle.conf;
    "kitty/kitty.conf".source = ./kitty/kitty.conf;
    "kitty/neighboring_window.py".source = ./kitty/neighboring_window.py;
    "kitty/pass_keys.py".source = ./kitty/pass_keys.py;
  };

  xdg.mimeApps.defaultApplications = {
    "text/html" = [ "firefox.desktop" ];
    "text/xml" = [ "firefox.desktop" ];
    "x-scheme-handler/http" = [ "firefox.desktop" ];
    "x-scheme-handler/https" = [ "firefox.desktop" ];
  };

  home.file = {
    ".ghci".source = ./haskell/ghci;
    ".haskeline".source = ./haskell/haskeline;
    ".hindent.yaml".source = ./haskell/hindent.yaml;
  };
}

