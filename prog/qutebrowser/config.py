# config.load_autoconfig()
c.aliases = {'w': 'session-save',
             'q': 'quit',
             'wq': 'quit --save',
             'bookmark-show': 'open -t qute://bookmarks/',
            }
# c.auto_save.interval = 15000
c.auto_save.session = True
c.backend = 'webengine'
# c.bindings.key_mappings = {'<Ctrl-[>': '<Escape>', '<Ctrl-6>': '<Ctrl-^>', '<Ctrl-M>': '<Return>', '<Ctrl-J>': '<Return>', '<Shift-Return>': '<Return>', '<Enter>': '<Return>', '<Shift-Enter>': '<Return>', '<Ctrl-Enter>': '<Ctrl-Return>'}
# c.confirm_quit = ['never']
# c.history_gap_interval = 30
c.messages.timeout = 4000
# c.spellcheck.languages = ['pt-BR','en-US']

# Colors {{{
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


# set qutebrowser colors
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
# c.colors.webpage.bg = base00
# }}}
# Completition {{{
# c.completion.cmd_history_max_items = 100
# c.completion.delay = 0
# c.completion.height = '50%'
# c.completion.min_chars = 1
# c.completion.quick = True
# c.completion.scrollbar.padding = 2
# c.completion.scrollbar.width = 12
# c.completion.show = 'always'
# c.completion.shrink = False
# c.completion.timestamp_format = '%Y-%m-%d'
# c.completion.use_best_match = False
# c.completion.web_history_max_items = -1
## }}}
# Content {{{
# c.content.cache.appcache = True
# c.content.cache.maximum_pages = 0
# c.content.cache.size = None
# c.content.cookies.accept = 'no-3rdparty'
# c.content.cookies.store = True
# c.content.default_encoding = 'iso-8859-1'
# c.content.developer_extras = False
# c.content.dns_prefetch = True
# c.content.frame_flattening = False
# c.content.geolocation = 'ask'
# c.content.headers.accept_language = 'en-US,en'
# c.content.headers.custom = {}
# c.content.headers.do_not_track = True
# c.content.headers.referer = 'same-domain'
# c.content.headers.user_agent = None
# c.content.host_blocking.enabled = True
# c.content.host_blocking.lists = ['https://www.malwaredomainlist.com/hostslist/hosts.txt', 'http://someonewhocares.org/hosts/hosts', 'http://winhelp2002.mvps.org/hosts.zip', 'http://malwaredomains.lehigh.edu/files/justdomains.zip', 'https://pgl.yoyo.org/adservers/serverlist.php?hostformat=hosts&mimetype=plaintext']
# c.content.host_blocking.whitelist = ['piwik.org']
# c.content.hyperlink_auditing = False
# c.content.images = True
# c.content.javascript.alert = True
# c.content.javascript.can_access_clipboard = False
# c.content.javascript.can_close_tabs = False
# c.content.javascript.can_open_tabs_automatically = False
# c.content.javascript.enabled = True
# c.content.javascript.log = {'unknown': 'debug', 'info': 'debug', 'warning': 'debug', 'error': 'debug'}
# c.content.javascript.modal_dialog = False
# c.content.javascript.prompt = True
# c.content.local_content_can_access_file_urls = True
# c.content.local_content_can_access_remote_urls = False
# c.content.local_storage = True
# c.content.media_capture = 'ask'
# c.content.netrc_file = None
# c.content.notifications = 'ask'
# c.content.pdfjs = True
# c.content.plugins = False
# c.content.print_element_backgrounds = True
# c.content.private_browsing = False
# c.content.proxy = 'system'
# c.content.proxy_dns_requests = True
# c.content.ssl_strict = 'ask'
# c.content.user_stylesheets = []
# c.content.webgl = True
# c.content.windowed_fullscreen = False
# c.content.xss_auditing = False
# }}}
# Dowload {{{
# c.downloads.location.directory = None
# c.downloads.location.prompt = True
# c.downloads.location.remember = True
# c.downloads.location.suggestion = 'path'
# c.downloads.open_dispatcher = None
# c.downloads.position = 'top'
# c.downloads.remove_finished = -1
# }}}
# editor {{{
c.editor.command = ['terminator','-e nvim {}']
# c.editor.encoding = 'utf-8'
# }}}
# Fonts {{{
# c.fonts.completion.category = 'bold 10pt monospace'
# c.fonts.completion.entry = '10pt monospace'
# c.fonts.debug_console = '10pt monospace'
# c.fonts.downloads = '10pt monospace'
# c.fonts.hints = 'bold 10pt monospace'
# c.fonts.keyhint = '10pt monospace'
# c.fonts.messages.error = '10pt monospace'
# c.fonts.messages.info = '10pt monospace'
# c.fonts.messages.warning = '10pt monospace'
# c.fonts.monospace = '"xos4 Terminus", Terminus, Monospace, "DejaVu Sans Mono", Monaco, "Bitstream Vera Sans Mono", "Andale Mono", "Courier New", Courier, "Liberation Mono", monospace, Fixed, Consolas, Terminal'
# c.fonts.prompts = '10pt sans-serif'
# c.fonts.statusbar = '10pt monospace'
# c.fonts.tabs = '10pt monospace'
# c.fonts.web.family.cursive = ''
# c.fonts.web.family.fantasy = ''
# c.fonts.web.family.fixed = ''
# c.fonts.web.family.sans_serif = ''
# c.fonts.web.family.serif = ''
# c.fonts.web.family.standard = ''
# c.fonts.web.size.default = 16
# c.fonts.web.size.default_fixed = 13
# c.fonts.web.size.minimum = 0
# c.fonts.web.size.minimum_logical = 6
# }}}
# hints {{{
# c.hints.auto_follow = 'unique-match'
# c.hints.auto_follow_timeout = 0
# c.hints.border = '1px solid #E3BE23'
c.hints.chars = 'qwertyuiopasdfghjklçzxcvbnm'
# c.hints.dictionary = '/usr/share/dict/words'
# c.hints.find_implementation = 'python'
# c.hints.hide_unmatched_rapid_hints = True
# c.hints.min_chars = 1
# c.hints.mode = 'letter'
# c.hints.next_regexes = ['\\bnext\\b', '\\bmore\\b', '\\bnewer\\b', '\\b[>→≫]\\b', '\\b(>>|»)\\b', '\\bcontinue\\b']
# c.hints.prev_regexes = ['\\bprev(ious)?\\b', '\\bback\\b', '\\bolder\\b', '\\b[<←≪]\\b', '\\b(<<|«)\\b']
# c.hints.scatter = True
# c.hints.uppercase = False
# }}}
# Input {{{
# c.input.forward_unbound_keys = 'auto'
# c.input.insert_mode.auto_enter = True
# c.input.insert_mode.auto_leave = True
# c.input.insert_mode.auto_load = False
# c.input.insert_mode.plugins = False
# c.input.links_included_in_focus_chain = True
# c.input.partial_timeout = 5000
# c.input.rocker_gestures = False
# c.input.spatial_navigation = False
# }}}
# Keyhint {{{
# c.keyhint.blacklist = []
# c.keyhint.delay = 500
# c.keyhint.radius = 6
# }}}
# new_instance_open_taget {{{
# c.new_instance_open_target = 'tab'
# c.new_instance_open_target_window = 'last-focused'
# }}}
# Prompt {{{
# c.prompt.filebrowser = True
# c.prompt.radius = 8
# }}}
# Qt {{{
# c.qt.args = []
# c.qt.force_platform = None
# c.qt.force_software_rendering = False
# c.qt.highdpi = False
# }}}
# Scrolling {{{
c.scrolling.bar = False
c.scrolling.smooth = True
# }}}
# Search {{{
# c.search.ignore_case = 'smart'
# c.search.incremental = True
# }}}
# Session {{{
# c.session.default_name = None
c.session.lazy_restore = True
# }}}
# Statusbar {{{
# c.statusbar.hide = False
# c.statusbar.padding = {'top': 1, 'bottom': 1, 'left': 0, 'right': 0}
# c.statusbar.position = 'bottom'
# c.statusbar.widgets = ['keypress', 'url', 'scroll', 'history', 'tabs', 'progress']
# }}}
# Tabs {{{
# c.tabs.background = False
# c.tabs.close_mouse_button = 'middle'
# c.tabs.close_mouse_button_on_bar = 'new-tab'
# c.tabs.favicons.scale = 1.0
# c.tabs.favicons.show = True
# c.tabs.indicator.padding = {'top': 2, 'bottom': 2, 'left': 0, 'right': 4}
# c.tabs.indicator.width = 3
# c.tabs.last_close = 'ignore'
# c.tabs.mode_on_change = 'normal'
# c.tabs.mousewheel_switching = True
# c.tabs.new_position.related = 'next'
# c.tabs.new_position.unrelated = 'last'
# c.tabs.padding = {'top': 0, 'bottom': 0, 'left': 5, 'right': 5}
# c.tabs.pinned.shrink = True
# c.tabs.position = 'top'
# c.tabs.select_on_remove = 'next'
c.tabs.show = 'multiple'
# c.tabs.show_switching_delay = 800
# c.tabs.tabs_are_windows = False
# c.tabs.title.alignment = 'left'
# c.tabs.title.format = '{index}: {title}'
# c.tabs.title.format_pinned = '{index}'
# c.tabs.width = '20%'
# c.tabs.wrap = True
# }}}
# URL {{{
# c.url.auto_search = 'naive'
c.url.default_page = 'https://everhelper.me/client/'
# c.url.incdec_segments = ['path', 'query']
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
c.url.start_pages = ['https://everhelper.me/client/']
# c.url.yank_ignored_parameters = ['ref', 'utm_source', 'utm_medium', 'utm_campaign', 'utm_term', 'utm_content']
# }}}
# Window {{{ 
# c.window.hide_wayland_decoration = False
# c.window.title_format = '{perc}{title}{title_sep}qutebrowser'
# }}}
# Zoom {{{ 
# c.zoom.default = '100%'
# c.zoom.levels = ['25%', '33%', '50%', '67%', '75%', '90%', '100%', '110%', '125%', '150%', '175%', '200%', '250%', '300%', '400%', '500%']
# c.zoom.mouse_divider = 512
# c.zoom.text_only = False
# }}}

# Bindings for normal mode {{{
config.bind("e", 'session-save ;; set-cmd-text -s :session-load -c')
# config.bind('<Escape>', 'clear-keychain ;; search ;; fullscreen --leave')
# config.bind("'", 'enter-mode jump_mark')
# config.bind('+', 'zoom-in')
# config.bind('-', 'zoom-out')
# config.bind('.', 'repeat-command')
# config.bind('/', 'set-cmd-text /')
# config.bind(':', 'set-cmd-text :')
# config.bind(';I', 'hint images tab')
# config.bind(';O', 'hint links fill :open -t -r {hint-url}')
# config.bind(';R', 'hint --rapid links window')
# config.bind(';Y', 'hint links yank-primary')
# config.bind(';b', 'hint all tab-bg')
# config.bind(';d', 'hint links download')
# config.bind(';f', 'hint all tab-fg')
# config.bind(';h', 'hint all hover')
# config.bind(';i', 'hint images')
# config.bind(';o', 'hint links fill :open {hint-url}')
# config.bind(';r', 'hint --rapid links tab-bg')
# config.bind(';t', 'hint inputs')
# config.bind(';y', 'hint links yank')
# config.bind('<Alt-1>', 'tab-focus 1')
# config.bind('<Alt-2>', 'tab-focus 2')
# config.bind('<Alt-3>', 'tab-focus 3')
# config.bind('<Alt-4>', 'tab-focus 4')
# config.bind('<Alt-5>', 'tab-focus 5')
# config.bind('<Alt-6>', 'tab-focus 6')
# config.bind('<Alt-7>', 'tab-focus 7')
# config.bind('<Alt-8>', 'tab-focus 8')
# config.bind('<Alt-9>', 'tab-focus -1')
# config.bind('<Ctrl-A>', 'navigate increment')
# config.bind('<Ctrl-Alt-p>', 'print')
# config.bind('<Ctrl-B>', 'scroll-page 0 -1')
# config.bind('<Ctrl-D>', 'scroll-page 0 0.5')
# config.bind('<Ctrl-F5>', 'reload -f')
# config.bind('<Ctrl-F>', 'scroll-page 0 1')
# config.bind('<Ctrl-N>', 'open -w')
# config.bind('<Ctrl-PgDown>', 'tab-next')
# config.bind('<Ctrl-PgUp>', 'tab-prev')
# config.bind('<Ctrl-Q>', 'quit')
# config.bind('<Ctrl-Return>', 'follow-selected -t')
# config.bind('<Ctrl-Shift-N>', 'open -p')
# config.bind('<Ctrl-Shift-T>', 'undo')
# config.bind('<Ctrl-Shift-W>', 'close')
# config.bind('<Ctrl-T>', 'open -t')
# config.bind('<Ctrl-Tab>', 'tab-focus last')
# config.bind('<Ctrl-U>', 'scroll-page 0 -0.5')
# config.bind('<Ctrl-V>', 'enter-mode passthrough')
# config.bind('<Ctrl-W>', 'tab-close')
# config.bind('<Ctrl-X>', 'navigate decrement')
# config.bind('<Ctrl-^>', 'tab-focus last')
# config.bind('<Ctrl-h>', 'home')
# config.bind('<Ctrl-p>', 'tab-pin')
# config.bind('<Ctrl-s>', 'stop')
# config.bind('<Escape>', 'clear-keychain ;; search ;; fullscreen --leave')
# config.bind('<F11>', 'fullscreen')
# config.bind('<F5>', 'reload')
# config.bind('<Return>', 'follow-selected')
# config.bind('<back>', 'back')
# config.bind('<forward>', 'forward')
# config.bind('=', 'zoom')
# config.bind('?', 'set-cmd-text ?')
# config.bind('@', 'run-macro')
# config.bind('B', 'set-cmd-text -s :quickmark-load -t')
# config.bind('D', 'tab-close -o')
# config.bind('F', 'hint all tab')
# config.bind('G', 'scroll-to-perc')
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
# config.bind('M', 'bookmark-add')
# config.bind('N', 'search-prev')
# config.bind('O', 'set-cmd-text -s :open -t')
# config.bind('PP', 'open -t -- {primary}')
# config.bind('Pp', 'open -t -- {clipboard}')
# config.bind('R', 'reload -f')
# config.bind('Sb', 'open qute://bookmarks#bookmarks')
# config.bind('Sh', 'open qute://history')
# config.bind('Sq', 'open qute://bookmarks')
# config.bind('Ss', 'open qute://settings')
# config.bind('T', 'tab-focus')
# config.bind('ZQ', 'quit')
# config.bind('ZZ', 'quit --save')
# config.bind('[[', 'navigate prev')
# config.bind(']]', 'navigate next')
# config.bind('`', 'enter-mode set_mark')
# config.bind('ad', 'download-cancel')
# config.bind('b', 'set-cmd-text -s :quickmark-load')
# config.bind('cd', 'download-clear')
# config.bind('co', 'tab-only')
# config.bind('d', 'tab-close')
# config.bind('f', 'hint')
# config.bind('g$', 'tab-focus -1')
# config.bind('g0', 'tab-focus 1')
# config.bind('gB', 'set-cmd-text -s :bookmark-load -t')
# config.bind('gC', 'tab-clone')
# config.bind('gO', 'set-cmd-text :open -t -r {url:pretty}')
# config.bind('gU', 'navigate up -t')
# config.bind('g^', 'tab-focus 1')
# config.bind('ga', 'open -t')
# config.bind('gb', 'set-cmd-text -s :bookmark-load')
# config.bind('gd', 'download')
# config.bind('gf', 'view-source')
# config.bind('gg', 'scroll-to-perc 0')
# config.bind('gl', 'tab-move -')
# config.bind('gm', 'tab-move')
# config.bind('go', 'set-cmd-text :open {url:pretty}')
# config.bind('gr', 'tab-move +')
config.bind('<Ctrl-b>', 'set-cmd-text -s :buffer')
# config.bind('gu', 'navigate up')
# config.bind('h', 'scroll left')
# config.bind('i', 'enter-mode insert')
# config.bind('j', 'scroll down')
# config.bind('k', 'scroll up')
# config.bind('l', 'scroll right')
# config.bind('m', 'quickmark-save')
# config.bind('n', 'search-next')
# config.bind('o', 'set-cmd-text -s :open')
# config.bind('pP', 'open -- {primary}')
# config.bind('pp', 'open -- {clipboard}')
# config.bind('q', 'record-macro')
# config.bind('r', 'reload')
# config.bind('sf', 'save')
# config.bind('sk', 'set-cmd-text -s :bind')
# config.bind('sl', 'set-cmd-text -s :set -t')
# config.bind('ss', 'set-cmd-text -s :set')
# config.bind('tPH', 'config-cycle -p -u *://*.{url:host}/* content.plugins ;; reload')
# config.bind('tPh', 'config-cycle -p -u *://{url:host}/* content.plugins ;; reload')
# config.bind('tPu', 'config-cycle -p -u {url} content.plugins ;; reload')
# config.bind('tSH', 'config-cycle -p -u *://*.{url:host}/* content.javascript.enabled ;; reload')
# config.bind('tSh', 'config-cycle -p -u *://{url:host}/* content.javascript.enabled ;; reload')
# config.bind('tSu', 'config-cycle -p -u {url} content.javascript.enabled ;; reload')
# config.bind('th', 'back -t')
# config.bind('tl', 'forward -t')
# config.bind('tpH', 'config-cycle -p -t -u *://*.{url:host}/* content.plugins ;; reload')
# config.bind('tph', 'config-cycle -p -t -u *://{url:host}/* content.plugins ;; reload')
# config.bind('tpu', 'config-cycle -p -t -u {url} content.plugins ;; reload')
# config.bind('tsH', 'config-cycle -p -t -u *://*.{url:host}/* content.javascript.enabled ;; reload')
# config.bind('tsh', 'config-cycle -p -t -u *://{url:host}/* content.javascript.enabled ;; reload')
# config.bind('tsu', 'config-cycle -p -t -u {url} content.javascript.enabled ;; reload')
# config.bind('u', 'undo')
# config.bind('v', 'enter-mode caret')
# config.bind('wB', 'set-cmd-text -s :bookmark-load -w')
# config.bind('wO', 'set-cmd-text :open -w {url:pretty}')
# config.bind('wP', 'open -w -- {primary}')
# config.bind('wb', 'set-cmd-text -s :quickmark-load -w')
# config.bind('wf', 'hint all window')
# config.bind('wh', 'back -w')
# config.bind('wi', 'inspector')
# config.bind('wl', 'forward -w')
# config.bind('wo', 'set-cmd-text -s :open -w')
# config.bind('wp', 'open -w -- {clipboard}')
# config.bind('xO', 'set-cmd-text :open -b -r {url:pretty}')
# config.bind('xo', 'set-cmd-text -s :open -b')
# config.bind('yD', 'yank domain -s')
# config.bind('yP', 'yank pretty-url -s')
# config.bind('yT', 'yank title -s')
# config.bind('yY', 'yank -s')
# config.bind('yd', 'yank domain')
# config.bind('yp', 'yank pretty-url')
# config.bind('yt', 'yank title')
# config.bind('yy', 'yank')
# config.bind('{{', 'navigate prev -t')
# config.bind('}}', 'navigate next -t')
# }}}
# Bindings for caret mode {{{
# config.bind('$', 'move-to-end-of-line', mode='caret')
# config.bind('0', 'move-to-start-of-line', mode='caret')
# config.bind('<Ctrl-Space>', 'drop-selection', mode='caret')
# config.bind('<Escape>', 'leave-mode', mode='caret')
# config.bind('<Return>', 'yank selection', mode='caret')
# config.bind('<Space>', 'toggle-selection', mode='caret')
# config.bind('G', 'move-to-end-of-document', mode='caret')
# config.bind('H', 'scroll left', mode='caret')
# config.bind('J', 'scroll down', mode='caret')
# config.bind('K', 'scroll up', mode='caret')
# config.bind('L', 'scroll right', mode='caret')
# config.bind('Y', 'yank selection -s', mode='caret')
# config.bind('[', 'move-to-start-of-prev-block', mode='caret')
# config.bind(']', 'move-to-start-of-next-block', mode='caret')
# config.bind('b', 'move-to-prev-word', mode='caret')
# config.bind('c', 'enter-mode normal', mode='caret')
# config.bind('e', 'move-to-end-of-word', mode='caret')
# config.bind('gg', 'move-to-start-of-document', mode='caret')
# config.bind('h', 'move-to-prev-char', mode='caret')
# config.bind('j', 'move-to-next-line', mode='caret')
# config.bind('k', 'move-to-prev-line', mode='caret')
# config.bind('l', 'move-to-next-char', mode='caret')
# config.bind('v', 'toggle-selection', mode='caret')
# config.bind('w', 'move-to-next-word', mode='caret')
# config.bind('y', 'yank selection', mode='caret')
# config.bind('{', 'move-to-end-of-prev-block', mode='caret')
# config.bind('}', 'move-to-end-of-next-block', mode='caret')
# }}}
# Bindings for command mode {{{
# config.bind('<Alt-B>', 'rl-backward-word', mode='command')
# config.bind('<Alt-Backspace>', 'rl-backward-kill-word', mode='command')
# config.bind('<Alt-D>', 'rl-kill-word', mode='command')
# config.bind('<Alt-F>', 'rl-forward-word', mode='command')
# config.bind('<Ctrl-?>', 'rl-delete-char', mode='command')
# config.bind('<Ctrl-A>', 'rl-beginning-of-line', mode='command')
# config.bind('<Ctrl-B>', 'rl-backward-char', mode='command')
# config.bind('<Ctrl-C>', 'completion-item-yank', mode='command')
# config.bind('<Ctrl-D>', 'completion-item-del', mode='command')
# config.bind('<Ctrl-E>', 'rl-end-of-line', mode='command')
# config.bind('<Ctrl-F>', 'rl-forward-char', mode='command')
# config.bind('<Ctrl-H>', 'rl-backward-delete-char', mode='command')
# config.bind('<Ctrl-K>', 'rl-kill-line', mode='command')
# config.bind('<Ctrl-N>', 'command-history-next', mode='command')
# config.bind('<Ctrl-P>', 'command-history-prev', mode='command')
# config.bind('<Ctrl-Return>', 'command-accept --rapid', mode='command')
# config.bind('<Ctrl-Shift-C>', 'completion-item-yank --sel', mode='command')
# config.bind('<Ctrl-Shift-Tab>', 'completion-item-focus prev-category', mode='command')
# config.bind('<Ctrl-Tab>', 'completion-item-focus next-category', mode='command')
# config.bind('<Ctrl-U>', 'rl-unix-line-discard', mode='command')
# config.bind('<Ctrl-W>', 'rl-unix-word-rubout', mode='command')
# config.bind('<Ctrl-Y>', 'rl-yank', mode='command')
# config.bind('<Down>', 'completion-item-focus --history next', mode='command')
# config.bind('<Escape>', 'leave-mode', mode='command')
# config.bind('<Return>', 'command-accept', mode='command')
# config.bind('<Shift-Delete>', 'completion-item-del', mode='command')
# config.bind('<Shift-Tab>', 'completion-item-focus prev', mode='command')
# config.bind('<Tab>', 'completion-item-focus next', mode='command')
# config.bind('<Up>', 'completion-item-focus --history prev', mode='command')
# }}}
# Bindings for hint mode {{{
# config.bind('<Ctrl-B>', 'hint all tab-bg', mode='hint')
# config.bind('<Ctrl-F>', 'hint links', mode='hint')
# config.bind('<Ctrl-R>', 'hint --rapid links tab-bg', mode='hint')
# config.bind('<Escape>', 'leave-mode', mode='hint')
# config.bind('<Return>', 'follow-hint', mode='hint')
# }}}
# Bindings for insert mode {{{
# config.bind('<Ctrl-E>', 'open-editor', mode='insert')
# config.bind('<Escape>', 'leave-mode', mode='insert')
# config.bind('<Shift-Ins>', 'insert-text {primary}', mode='insert')
# }}}
# Bindings for passthrough mode {{{
config.bind('<Ctrl-\>', 'leave-mode', mode='passthrough')
config.unbind('<Ctrl-v>', mode='passthrough')
# }}}
# Bindings for prompt mode {{{
# config.bind('<Alt-B>', 'rl-backward-word', mode='prompt')
# config.bind('<Alt-Backspace>', 'rl-backward-kill-word', mode='prompt')
# config.bind('<Alt-D>', 'rl-kill-word', mode='prompt')
# config.bind('<Alt-F>', 'rl-forward-word', mode='prompt')
# config.bind('<Alt-Shift-Y>', 'prompt-yank --sel', mode='prompt')
# config.bind('<Alt-Y>', 'prompt-yank', mode='prompt')
# config.bind('<Ctrl-?>', 'rl-delete-char', mode='prompt')
# config.bind('<Ctrl-A>', 'rl-beginning-of-line', mode='prompt')
# config.bind('<Ctrl-B>', 'rl-backward-char', mode='prompt')
# config.bind('<Ctrl-E>', 'rl-end-of-line', mode='prompt')
# config.bind('<Ctrl-F>', 'rl-forward-char', mode='prompt')
# config.bind('<Ctrl-H>', 'rl-backward-delete-char', mode='prompt')
# config.bind('<Ctrl-K>', 'rl-kill-line', mode='prompt')
# config.bind('<Ctrl-U>', 'rl-unix-line-discard', mode='prompt')
# config.bind('<Ctrl-W>', 'rl-unix-word-rubout', mode='prompt')
# config.bind('<Ctrl-X>', 'prompt-open-download', mode='prompt')
# config.bind('<Ctrl-Y>', 'rl-yank', mode='prompt')
# config.bind('<Down>', 'prompt-item-focus next', mode='prompt')
# config.bind('<Escape>', 'leave-mode', mode='prompt')
# config.bind('<Return>', 'prompt-accept', mode='prompt')
# config.bind('<Shift-Tab>', 'prompt-item-focus prev', mode='prompt')
# config.bind('<Tab>', 'prompt-item-focus next', mode='prompt')
# config.bind('<Up>', 'prompt-item-focus prev', mode='prompt')
# }}}
# Bindings for register mode {{{
# config.bind('<Escape>', 'leave-mode', mode='register')
# }}}
# Bindings for yesno mode {{{
# config.bind('<Alt-Shift-Y>', 'prompt-yank --sel', mode='yesno')
# config.bind('<Alt-Y>', 'prompt-yank', mode='yesno')
# config.bind('<Escape>', 'leave-mode', mode='yesno')
# config.bind('<Return>', 'prompt-accept', mode='yesno')
# config.bind('n', 'prompt-accept no', mode='yesno')
# config.bind('y', 'prompt-accept yes', mode='yesno')
# }}}

# vim:foldmethod=marker foldlevel=0
