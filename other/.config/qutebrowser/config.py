# settings {{{
c.tabs.title.format = "{audio}{index} {current_title}"
c.aliases = {'w': 'session-save',
             'q': 'quit',
             'wq': 'quit --save',
             'bookmark-show': 'open -t qute://bookmarks/',
            }
c.auto_save.session = True
c.backend = 'webengine'
c.messages.timeout = 4000
c.content.pdfjs = False
c.editor.command = ["emacsclient", "{}"]
c.hints.chars = 'qwertyuiopasdfghjklçzxcvbnm'
c.scrolling.bar = "when-searching"
c.scrolling.smooth = True
c.session.lazy_restore = True
c.tabs.show = 'multiple'
c.url.default_page = 'https://booky.io/'
c.url.searchengines = {'DEFAULT':'https://www.google.com/search?q={}',
                       'gg':'https://www.google.com/search?q={}',
                       'dd':'https://duckduckgo.com/?q={}',
                       'ww':'https://www.wikiwand.com/en/{}',
                       'wp':'https://en.wikipedia.org/wiki/{}',
                       'tw':'https://twitter.com/search?q={}',
                       'rd':'https://www.reddit.com/search?q={}',
                       'vm':'https://vimeo.com/search?q=+{}',
                       'yt':'https://www.youtube.com/results?search_query={}',
                       'gp':'https://play.google.com/store/search?q={}',
                       'gs':'https://scholar.google.com/scholar?q={}',
                       'gt':'https://translate.google.com/?q={}',
                       'du':'https://www.duden.de/suchen/dudenonline/{}',
                       'di':'http://www.dictionary.com/browse/{}',
                       'nf':'https://www.netflix.com/search?q={}',
                       'aw':'https://wiki.archlinux.org/index.php?search={}',
                       'gh':'https://github.com/search?q={}',
                       'wf':'https://www.wolframalpha.com/input/?i={}',
                       'hg':'https://www.haskell.org/hoogle/?hoogle={}',
                       'py':'https://docs.python.org/3.6/search.html?q={}',
                       'az':'https://www.amazon.com/s/field-keywords={}',
                       'ya':'https://answers.search.yahoo.com/search?p={}',
                       'lg':'http://gen.lib.rus.ec/search.php?req={}',
                        }
c.url.start_pages = ['https://booky.io/']
c.content.autoplay = False
c.input.insert_mode.leave_on_load = False
c.url.open_base_url = True
# }}}
# color {{{
base00 = "#1f2022"
base01 = "#282828"
base02 = "#444155"
base03 = "#474747"
base04 = "#b8b8b8"
base05 = "#eeeeee"
base06 = "#e8e8e8"
base07 = "#f8f8f8"
base08 = "#f2241f"
base09 = "#ffa500"
base0A = "#b1951d"
base0B = "#00ff00"
base0C = "#2d9574"
base0D = "#4f97d7"
base0E = "#a31db1"
base0F = "#b03060"
c.colors.completion.category.bg = base00
c.colors.completion.category.border.bottom= base00
c.colors.completion.category.border.top= base00
c.colors.completion.category.fg = base0A
c.colors.completion.fg = base05
c.colors.completion.item.selected.bg = base0A
c.colors.completion.item.selected.border.bottom = base0A
c.colors.completion.item.selected.border.top = base0A
c.colors.completion.item.selected.fg = base01
c.colors.completion.match.fg = base0B
c.colors.completion.odd.bg = base03
c.colors.completion.even.bg = base00
c.colors.completion.scrollbar.bg = base00
c.colors.completion.scrollbar.fg = base05
c.colors.downloads.bar.bg = base00
c.colors.downloads.error.fg = base08
c.colors.downloads.start.bg = base0D
c.colors.downloads.start.fg = base00
c.colors.downloads.stop.bg = base0C
c.colors.downloads.stop.fg = base00
c.colors.hints.bg = base0A
c.colors.hints.fg = base00
c.colors.hints.match.fg = base05
c.colors.keyhint.bg = base00
c.colors.keyhint.fg = base05
c.colors.keyhint.suffix.fg = base05
c.colors.messages.error.fg = base00
c.colors.messages.error.bg = base08
c.colors.messages.error.border = base08
c.colors.messages.info.bg = base00
c.colors.messages.info.border = base00
c.colors.messages.info.fg = base05
c.colors.messages.warning.bg = base0E
c.colors.messages.warning.border = base0E
c.colors.messages.warning.fg = base00
c.colors.prompts.bg = base00
c.colors.prompts.border = base00
c.colors.prompts.fg = base05
c.colors.prompts.selected.bg = base0A
c.colors.statusbar.caret.bg = base0E
c.colors.statusbar.caret.fg = base00
c.colors.statusbar.caret.selection.bg = base0D
c.colors.statusbar.caret.selection.fg = base00
c.colors.statusbar.command.bg = base00
c.colors.statusbar.command.fg = base05
c.colors.statusbar.command.private.bg = base00
c.colors.statusbar.command.private.fg = base05
c.colors.statusbar.insert.bg = base0D
c.colors.statusbar.insert.fg = base00
c.colors.statusbar.normal.bg = base00
c.colors.statusbar.normal.fg = base0A
c.colors.statusbar.passthrough.bg = base0C
c.colors.statusbar.passthrough.fg = base00
c.colors.statusbar.private.bg = base03
c.colors.statusbar.private.fg = base00
c.colors.statusbar.progress.bg = base0D
c.colors.statusbar.url.error.fg = base08
c.colors.statusbar.url.fg = base05
c.colors.statusbar.url.hover.fg = base05
c.colors.statusbar.url.success.http.fg = base0C
c.colors.statusbar.url.success.https.fg =base0A
c.colors.statusbar.url.warn.fg = base0E
c.colors.tabs.bar.bg = base00
c.colors.tabs.even.bg = base00
c.colors.tabs.even.fg = base05
c.colors.tabs.indicator.error = base08
c.colors.tabs.indicator.start = base0D
c.colors.tabs.indicator.stop = base0C
c.colors.tabs.odd.bg = base03
c.colors.tabs.odd.fg = base05
c.colors.tabs.selected.even.bg = base05
c.colors.tabs.selected.even.fg = base00
c.colors.tabs.selected.odd.bg = base05
c.colors.tabs.selected.odd.fg = base00
# }}}
# bindings {{{
config.bind("a", 'enter-mode insert')
config.bind('zp', 'open -t https://getpocket.com/edit?url={url}')
config.bind("e", 'session-save ;; set-cmd-text -s :session-load -c')
config.bind('<Ctrl-L>', 'tab-next')
config.bind('<Ctrl-H>', 'tab-prev')
config.bind('gt', 'tab-next')
config.bind('[b', 'tab-prev')
config.bind(']b', 'tab-next')
config.bind('gT', 'tab-prev')
config.bind('J', 'back')
config.bind('K', 'forward')
config.bind('L', 'tab-move +')
config.bind('H', 'tab-move -')
config.bind('<Ctrl-b>', 'set-cmd-text -s :buffer')
config.bind('<Ctrl-\>', 'leave-mode', mode='passthrough')
# }}}

# Local Variables:
# origami-fold-style: triple-braces
# End:
