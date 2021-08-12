config.load_autoconfig(False)
config.set('content.cookies.accept', 'all', 'devtools://*')
config.set('content.headers.accept_language', '', 'https://matchmaker.krunker.io/*')
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}) AppleWebKit/{webkit_version} (KHTML, like Gecko) {upstream_browser_key}/{upstream_browser_version} Safari/{webkit_version}', 'https://web.whatsapp.com/')
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}; rv:90.0) Gecko/20100101 Firefox/90.0', 'https://accounts.google.com/*')
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99 Safari/537.36', 'https://*.slack.com/*')
config.set('content.images', True, 'chrome-devtools://*')
config.set('content.images', True, 'devtools://*')
config.set('content.javascript.enabled', True, 'chrome-devtools://*')
config.set('content.javascript.enabled', True, 'devtools://*')
config.set('content.javascript.enabled', True, 'chrome://*/*')
config.set('content.javascript.enabled', True, 'qute://*/*')

config.set('content.cookies.accept', 'all', 'chrome-devtools://*')
config.set("editor.command", ["emacsclient", "-a", "''", "-c", "{file}"])
config.set("scrolling.smooth", True)
config.set("zoom.default", "120%")

c.downloads.location.directory = '~/Downloads'
c.tabs.show = 'always'
c.url.searchengines = {'DEFAULT': 'https://duckduckgo.com/?q={}', 'goog': 'https://www.google.com/search?q={}', 'hoog': 'https://hoogle.haskell.org/?hoogle={}', 're': 'https://www.reddit.com/r/{}', 'ub': 'https://www.urbandictionary.com/define.php?term={}', 'wiki': 'https://en.wikipedia.org/wiki/{}', 'git': 'https://www.github.com/search?q={}', 'yt': 'https://www.youtube.com/results?search_query={}'}
c.content.autoplay = False
# c.spellcheck.languages = ['en-US', 'pl-PL']

## Adblocking
# Use (superior) Brave adblock if available, or fall back to host blocking
c.content.blocking.method = "auto"
c.content.blocking.hosts.lists = [
    'https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts',
    'https://pgl.yoyo.org/adservers/serverlist.php?hostformat=hosts&mimetype=plaintext'
    # 'https://www.malwaredomain
    # 'http://someonewhocares.org/hosts/hosts',
    # 'http://winhelp2002.mvps.org/hosts.zip',
    # 'http://malwaredomains.lehigh.edu/files/justdomains.zip',
]

# Setting dark mode
config.set("colors.webpage.bg", "")
config.set("colors.webpage.darkmode.enabled", False)
config.set("colors.webpage.preferred_color_scheme", "dark")

# Theme
c.colors.completion.fg = ['#91B9C7', '#E4E4E8', '#E4E4E8']
c.colors.completion.odd.bg = '#161618'
c.colors.completion.even.bg = '#18181B'
c.colors.completion.category.fg = '#845A84'
c.colors.completion.category.bg = 'qlineargradient(x1:0, y1:0, x2:0, y2:1, stop:0 #000000, stop:1 #232429)'
c.colors.completion.category.border.top = '#4B5254'
c.colors.completion.category.border.bottom = '#4B5254'
c.colors.completion.item.selected.fg = '#18181B'
c.colors.completion.item.selected.bg = '#DBAC66'
c.colors.completion.item.selected.match.fg = '#6FB593'
c.colors.completion.match.fg = '#6FB593'
c.colors.completion.scrollbar.fg = 'white'
c.colors.downloads.bar.bg = '#18181B'
c.colors.downloads.error.bg = '#CD5C60'
c.colors.hints.fg = '#18181B'
c.colors.hints.match.fg = '#6FB593'
c.colors.messages.info.bg = '#18181B'
c.colors.statusbar.normal.bg = '#18181B'
c.colors.statusbar.insert.fg = '#E4E4E8'
c.colors.statusbar.insert.bg = '#35BF88'
c.colors.statusbar.passthrough.bg = '#3B84CC'
c.colors.statusbar.command.bg = '#18181B'
c.colors.statusbar.url.warn.fg = '#EED891'
c.colors.statusbar.url.fg = '#E4E4E8'
c.colors.statusbar.url.error.fg = '#CD5C60'
c.colors.statusbar.url.success.https.fg = '#6FB593'
c.colors.tabs.bar.bg = '#4B5254'
c.colors.tabs.odd.fg = '#879193'
c.colors.tabs.odd.bg = '#18181B'
c.colors.tabs.even.fg = '#879193'
c.colors.tabs.even.bg = '#18181B'
c.colors.tabs.selected.odd.bg = '#4B5254'
c.colors.tabs.selected.even.bg = '#4B5254'
c.colors.tabs.pinned.odd.bg = '#6FB593'
c.colors.tabs.pinned.even.bg = '#4D9391'
c.colors.tabs.pinned.selected.odd.bg = '#18181B'
c.colors.tabs.pinned.selected.even.bg = '#18181B'
c.fonts.default_family = '"Iosevka Nerd Font"'
c.fonts.default_size = '11pt'
c.fonts.completion.entry = '11pt "Iosevka Nerd Font"'
c.fonts.debug_console = '11pt "Iosevka Nerd Font"'
c.fonts.prompts = '11pt "Iosevka Etoile"'
c.fonts.statusbar = '11pt "Iosevka Nerd Font"'

## Bindings for normal mode
config.bind('M', 'hint links spawn mpv {hint-url}')
config.bind('Z', 'hint links spawn alacritty -e youtube-dl {hint-url}')
config.bind('t', 'set-cmd-text -s :open -t')
config.bind('xb', 'config-cycle statusbar.show always never')
config.bind('xt', 'config-cycle tabs.show always never')
config.bind('xx', 'config-cycle statusbar.show always never;; config-cycle tabs.show always never')

# Bindings for cycling through CSS stylesheets from Solarized Everything CSS:
# https://github.com/alphapapa/solarized-everything-css
config.bind(',ap', 'config-cycle content.user_stylesheets ~/.config/solarized-everything-css/css/apprentice/apprentice-all-sites.css ""')
config.bind(',dr', 'config-cycle content.user_stylesheets ~/.config/solarized-everything-css/css/darculized/darculized-all-sites.css ""')
config.bind(',gr', 'config-cycle content.user_stylesheets ~/.config/solarized-everything-css/css/gruvbox/gruvbox-all-sites.css ""')
config.bind(',sd', 'config-cycle content.user_stylesheets ~/.config/solarized-everything-css/css/solarized-dark/solarized-dark-all-sites.css ""')
config.bind(',sl', 'config-cycle content.user_stylesheets ~/.config/solarized-everything-css/css/solarized-light/solarized-light-all-sites.css ""')
