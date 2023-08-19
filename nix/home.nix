{ config, pkgs, lib, ... }:

{

  home = {
    inherit (pkgs) stateVersion;
    inherit (pkgs) username;
    inherit (pkgs) homeDirectory;

    sessionVariables = {
      DROPBOX = "/mnt/c/Users/Dell/Dropbox";
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
          inherit (pkgs.texlive) scheme-medium amsmath ulem hyperref;
        });
        R-with-packages = rWrapper.override { packages = with rPackages; [ ggplot2 dplyr tidyverse ]; };
      in
      [
        ranger
        fzf
        wget
        openjdk
        unzip
        tex
        ledger
        gnumake
        R-with-packages
        git-filter-repo

        # Packages used in vim
        tree-sitter
        ripgrep
        fd
        # Language Servers
        rnix-lsp # nix
        texlab # Latex
        ltex-ls # Latex (grammar)
        lua-language-server # lua
      ];
  };

  programs = {
    git = {
      enable = true;
      userName = "Gabriel-Siqueira";
      userEmail = "gabriel.gabrielhs@gmail.com";
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
        catppuccin
        yank
        resurrect
        continuum
      ];
      extraConfig = ''
        ${builtins.readFile(./tmux_extra.conf)}
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
