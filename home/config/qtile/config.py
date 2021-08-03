import os, subprocess
from libqtile import hook, bar, layout, widget
from libqtile.config import (
    Click,
    Drag,
    Group,
    Key,
    KeyChord,
    Match,
    Screen,
    ScratchPad,
    DropDown,
)
from libqtile.lazy import lazy
from typing import List  # noqa: F401

mod = "mod4"
terminal = "alacritty"
editor = terminal + " nvim"
browser = "firefox-devedition"
file_manager = terminal + " ranger"
dotfiles = " /home/alternateved/.nixos-config"

keys = [
    # Core
    Key([mod, "shift"], "r", lazy.restart()),
    Key([mod, "shift"], "q", lazy.shutdown()),
    Key([mod, "control"], "r", lazy.spawn(editor + dotfiles + "home/qtile/config.py")),
    Key([mod, "shift"], "Return", lazy.spawn(terminal)),
    # Prompt
    Key([mod], "p", lazy.spawncmd()),
    # Windows
    Key([mod, "shift"], "c", lazy.window.kill()),
    # Floating windows
    Key([mod], "t", lazy.window.toggle_floating()),
    # Windows navigation
    Key([mod], "h", lazy.layout.left()),
    Key([mod], "l", lazy.layout.right()),
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "k", lazy.layout.up()),
    Key([mod], "space", lazy.layout.next()),
    Key([mod, "shift"], "h", lazy.layout.shuffle_left()),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right()),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up()),
    # Layouts
    Key([mod], "Tab", lazy.next_layout()),
    Key([mod, "shift"], "space", lazy.layout.toggle_split()),
    Key([mod, "shift"], "Tab", lazy.layout.rotate(), lazy.layout.flip()),
    Key(
        [mod, "control"],
        "h",
        lazy.layout.grow_left(),
        lazy.layout.shrink().when(layout="monadtall"),
    ),
    Key(
        [mod, "control"],
        "l",
        lazy.layout.grow_right(),
        lazy.layout.grow().when(layout="monadtall"),
    ),
    Key([mod, "control"], "j", lazy.layout.grow_down()),
    Key([mod, "control"], "k", lazy.layout.grow_up()),
    Key([mod], "n", lazy.layout.normalize()),
    # Jump to layout
    KeyChord(
        ["control"],
        "l",
        [
            Key([], "t", lazy.to_layout_index(0)),
            Key([], "w", lazy.to_layout_index(1)),
            Key([], "c", lazy.to_layout_index(2)),
            Key([], "m", lazy.to_layout_index(4)),
            Key([], "b", lazy.to_layout_index(3)),
        ],
    ),
    # Screens
    Key([mod], "w", lazy.to_screen(0)),
    Key([mod], "e", lazy.to_screen(1)),
    # Notifications
    Key(["control", "mod1"], "backslash", lazy.spawn("dunstctl set-paused toggle")),
    # Scratchpads
    KeyChord(
        ["control"],
        "s",
        [
            Key([], "t", lazy.group["scratchpad"].dropdown_toggle("term")),
            Key([], "c", lazy.group["scratchpad"].dropdown_toggle("calc")),
            Key([], "v", lazy.group["scratchpad"].dropdown_toggle("mixer")),
        ],
    ),
    # My applications
    Key([mod, "mod1"], "f", lazy.spawn(file_manager)),
    Key([mod, "mod1"], "b", lazy.spawn(browser)),
    # Multimedia Keys
    Key(
        [], "XF86AudioLowerVolume", lazy.spawn("amixer -q set Master 5%-")
    ),
    Key([], "XF86AudioRaiseVolume", lazy.spawn("amixer -q set Master 5%+")),
    Key([], "XF86AudioMute", lazy.spawn("amixer -q set Master toggle")),
    Key(
        [],
        "XF86MonBrightnessDown",
        lazy.spawn("xbacklight -dec 10"),
    ),
    Key(
        [],
        "XF86MonBrightnessUp",
        lazy.spawn(dotfiles + "xbacklight -inc 10"),
    ),
    Key(
        [],
        "Print",
        lazy.spawn("flameshot full -p /home/alternateved/Pictures/Screenshots"),
    ),
    Key(["shift"], "Print", lazy.spawn("flameshot launcher")),
    # Session menu
    KeyChord(
        [mod, "shift"],
        "q",
        [
            Key([], "p", lazy.spawn("poweroff")),
            Key([], "r", lazy.spawn("reboot")),
            Key([], "o", lazy.shutdown()),
            Key([], "l", lazy.spawn("slock")),
        ],
    ),
]
workspaces = [
    {"name": "1", "key": "1", "matches": [Match(wm_class="firefoxdeveloperedition")]},
    {
        "name": "2",
        "key": "2",
        "matches": [Match(wm_class="Thunderbird"), Match(wm_class="signal-desktop")],
    },
    {"name": "3", "key": "3", "matches": []},
    {"name": "4", "key": "4", "matches": []},
    {"name": "5", "key": "5", "matches": []},
    {"name": "6", "key": "6", "matches": []},
    {"name": "7", "key": "7", "matches": []},
    {"name": "8", "key": "8", "matches": []},
    {"name": "9", "key": "9", "matches": []},
]

groups = [
    ScratchPad(
        "scratchpad",
        [
            DropDown(
                "term",
                "kitty",
                height=0.6,
                on_focus_lost_hide=True,
                opacity=1,
                warp_pointer=True,
            ),
            DropDown(
                "calc",
                "qalculate-gtk",
                height=0.6,
                on_focus_lost_hide=True,
                opacity=1,
                warp_pointer=True,
            ),
            DropDown(
                "mixer",
                "pavucontrol",
                height=0.6,
                on_focus_lost_hide=True,
                opacity=1,
                warp_pointer=True,
            ),
        ],
    ),
]

for workspace in workspaces:
    matches = workspace["matches"] if "matches" in workspace else None
    groups.append(Group(workspace["name"], matches=matches, layout="monadtall"))
    keys.append(Key([mod], workspace["key"], lazy.group[workspace["name"]].toscreen()))
    keys.append(
        Key([mod, "shift"], workspace["key"], lazy.window.togroup(workspace["name"]))
    )

colors = {
    "background": ["#1d1f21"],
    "foreground": ["#c4c8c5"],
    "highlight": ["#313335"],
    "inactive": ["#545B68"],
    "active": ["#ecf0ed"],
    "red": ["#cc6666"],
    "blue": ["#80a1bd"],
    "green": ["#b5bd68"],
}

layout_theme = {
    "border_width": 1,
    "margin": 5,
    "border_focus": colors["foreground"][0],
    "border_normal": colors["background"][0],
}

layouts = [
    layout.MonadTall(**layout_theme, new_client_position="top"),
    layout.MonadWide(**layout_theme),
    layout.Columns(**layout_theme),
    layout.TreeTab(
        fontsize=10,
        sections=[""],
        section_fontsize=0,
        section_top=0,
        section_bottom=0,
        border_width=2,
        bg_color=colors["background"],
        active_bg=colors["highlight"],
        active_fg=colors["active"],
        inactive_bg=colors["background"],
        inactive_fg=colors["inactive"],
        padding_left=0,
        padding_x=0,
        padding_y=5,
        level_shift=8,
        vspace=3,
        panel_width=150,
    ),
    layout.Max(),
    # layout.Bsp(**layout_theme),
    # layout.Matrix(**layout_theme),
    # layout.RatioTile(**layout_theme),
    # layout.Floating(**layout_theme)
    # layout.Tile(**layout_theme),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font="JetBrainsMonoMedium Nerd Font",
    fontsize=11,
    padding=3,
    background=colors["background"],
    foreground=colors["foreground"],
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.CurrentScreen(
                    active_text="",
                    inactive_text="",
                    active_color=colors["active"],
                    inactive_color=colors["foreground"],
                    padding=7,
                ),
                widget.TextBox(text="|"),
                widget.GroupBox(
                    borderwidth=1,
                    active=colors["foreground"],
                    inactive=colors["inactive"],
                    urgent_border=colors["red"],
                    urgent_text=colors["foreground"],
                    disable_drag=True,
                    rounded=False,
                    highlight_color=colors["background"],
                    highlight_method="line",
                    this_current_screen_border=colors["foreground"],
                    this_screen_border=colors["foreground"],
                    other_current_screen_border=colors["blue"],
                    other_screen_border=colors["blue"],
                ),
                widget.TextBox(text="|"),
                widget.CurrentLayout(),
                widget.TextBox(text="|"),
                widget.Prompt(
                    prompt="Run: ",
                    ignore_dups_history=True,
                ),
                widget.WindowName(),
                widget.Chord(),
                widget.Systray(),
                widget.CheckUpdates(
                    display_format="Updates: {updates} |",
                    colour_have_updates=colors["foreground"],
                ),
                widget.OpenWeather(
                    app_key="f5c2f7ddce129955c85c723e691eab9b",
                    location="lodz,PL",
                    format="{weather_details} {main_temp}°{units_temperature}",
                ),
                widget.TextBox(text="|"),
                widget.Net(
                    interface="wlan0",
                    format="↓{down} ↑{up}",
                ),
                widget.TextBox(text="|"),
                widget.CPU(format=" {load_percent}%"),
                widget.TextBox(text="|"),
                widget.Memory(
                    format=" {MemUsed:.0f}{mm}/{MemTotal:.0f}{mm}",
                ),
                widget.TextBox(text="|"),
                widget.Battery(format=" {percent:2.0%}", show_short_text=False),
                widget.TextBox(text="|"),
                widget.Clock(format="%H:%M "),
            ],
            30,
        ),
    ),
    Screen(
        top=bar.Bar(
            [
                widget.CurrentScreen(
                    active_text="",
                    inactive_text="",
                    active_color=colors["active"],
                    inactive_color=colors["foreground"],
                    padding=7,
                ),
                widget.TextBox(text="|"),
                widget.AGroupBox(
                    border=None,
                    padding_x=-3,
                ),
                widget.TextBox(text="|"),
                widget.CurrentLayout(),
                widget.TextBox(text="|"),
                widget.WindowName(),
                widget.Clock(format="%H:%M "),
            ],
            30,
        ),
    ),
]

mouse = [
    Drag(
        [mod],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
follow_mouse_focus = True
bring_front_click = False
cursor_warp = True

floating_layout = layout.Floating(
    **layout_theme,
    float_rules=[
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True
auto_minimize = True
wmname = "LG3D"


@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser("~/.config/qtile/autostart.sh")
    subprocess.call([home])
