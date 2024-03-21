{ config, pkgs, lib, specialArgs, ... }:

let
  inherit (specialArgs) withGUI inWSL;
  inherit (lib) optionals;
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
            beamertheme-metropolis
            biblatex
            enumitem
            environ
            hyperref
            ifoddpage
            lipsum
            makecell
            multirow
            relsize
            scheme-medium
            subfigure
            tabu
            threeparttable
            titling
            todonotes
            ulem
            xurl
            ;
        });
        python-with-packages = pkgs.python3.withPackages (ps: with ps; [
          ipython
          jinja2
          markdown
          matplotlib
          numpy
          pandas
          weasyprint
          (
            buildPythonPackage rec {
              pname = "csvtomd";
              version = "0.3.0";
              src = fetchPypi {
                inherit pname version;
                sha256 = "sha256-ofvx24bUt7YqddwlmAdxmyMB7QHbXR19m7ScSohYd4s=";
              };
              doCheck = false;
              propagatedBuildInputs = [
                # Specify dependencies
              ];
            }
          )
        ]);
        my-rPackages = with rPackages; [
          FactoMineR
          Hmisc
          ade4
          akima
          amap
          caret
          corrplot
          cowplot
          dplyr
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
          ramify
          randomForest
          reshape2
          rgl
          rpart
          rpart_plot
          sjPlot
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
        dropbox
        firefox
        google-chrome
        libreoffice
        obs-studio
        obsidian
        openvpn
        pulseaudio
        spotify
        write_stylus

        # KDE
        plasma5Packages.kio-gdrive
        yakuake

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
        htop
        hugo
        ledger
        lshw
        openjdk
        poppler_utils
        ranger
        unzip
        usbutils
        wget
        xclip
        zip

        # Programming
        clang
        cmake
        git-filter-repo
        gnumake
        go
        perl
        python-with-packages
        tex
        valgrind

        # Packages used in vim
        fd
        ripgrep
        tree-sitter

        # Language Servers
        clang-tools # add clangd for C/C++
        ltex-ls # Latex (grammar)
        lua-language-server # lua
        pyright # python
        rnix-lsp # nix
        texlab # Latex
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
            # Seamless navigation with tmux
            plugin = vim-tmux-navigator;
            config = ''
              ${builtins.readFile(./vim/plugins_conf/tmux_navigator.vim)}
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

        ];

        extraLuaConfig =
          ''
            ${builtins.readFile(./vim/config.lua)}
          '';
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

    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      autocd = true;
      history = {
        size = 100000;
        save = 100000;
        path = "${config.xdg.dataHome}/zsh/history";
        ignoreDups = true;
      };

      shellAliases = {
        ll = "ls -l";
        la = "ls -a";
        lh = "ls -lh";
        ltr = "ls -ltr";
        "cd.." = "cd ..";
        "." = "pwd";
        ".." = "cd ..";
        "..." = "cd ../..";
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

  };

  xdg.configFile = {
    "ranger/commands.py".source = ./ranger/commands.py;
    "ranger/rc.conf".source = ./ranger/rc.conf;
    "ranger/rifle.conf".source = ./ranger/rifle.conf;
  };

  home.file = {
    ".ghci".source = ./haskell/ghci;
    ".haskeline".source = ./haskell/haskeline;
    ".hindent.yaml".source = ./haskell/hindent.yaml;
  };
}
