# MMOSMacs

MMOSMacs is my personal Emacs configuration, but it's also the precursor to a project I'm planning, ***Elder Linux***, which will be completely based around Emacs as a desktop environment.

MMOSMacs is the core of my computing experience, and is meant to be a jack of all trades, just like me. As such, this is a "kitchen sink" configuration. I make no attempt to separate my personal preferences from the main functionality.

## Features

### Current Features

- Illiterate configuration
- Modern configuration using `straight.el` and `use-package`
- Emacs as a desktop environment (EXWM)
- The beginnings of an opinionated ergonomic keybind setup (still a long ways off though)
- The beginnings of a development environemnt (also a long ways off)
- Opinionated PKMS system

### Plans

- Custom theme, modeline, & anti-distraction features
- Custom ergonomic keybind system
- Full-featured desktop environment (EDNC, EMMS, etc.)
- Full-featured development environment
  - POSIX Shell, bash
  - Emacs Lisp, Common Lisp
  - Assembly, C, Haskell
  - SQL, AQL, DQL
  - CSV, JSON, XML
  - Markdown, HTML, JS, CSS
- Communications platform
  - Email, mailing lists
  - Web Feeds (RSS)
  - IRC, Matrix
  - SDR
- Productivity Suite
  - Finance
  - Editing & Publishing
  - Programmatic CAD
- System & Network Administration tools
- Security & cryptography tools
- Games

## Installation

### Get the files in place

First, clone this repository into your `~/.emacs.d` directory.

```bash
git clone https://github.com/MountainMan1312/MMOSMacs.git ~/.emacs.d
```

Next create symlinks to the `.xinitrc` and `emacs.desktop` files so X
knows what to do:

```bash
# Backup your `.xinitrc` file if you feel like it.
# Replace `.xinitrc.old` with whatever you want to name the backup.
mv ~/.xinitrc ~/.xinitrc.old

# Symlink your `.xinitrc` file
ln -s ~/.emacs.d/.xinitrc ~/.xinitrc

# Symlink `emacs.desktop` to add MMOSMacs as an entry to your Display
# Manager / login screen.
sudo ln -s ~/.emacs.d/emacs.desktop /usr/share/xsessions/emacs.desktop
```

Optionally create a symlink to the `.sbclrc` file for SBCL:

```bash
ln -s ~/.emacs.d/.sbclrc ~/.sbclrc
```

### Install Dependencies

You'll also need the following packages installed on your system. Gentoo packages are listed here along with their USE flags.

```
app-admin/sudo
app-dicts/aspell
app-editors/emacs cairo dbus dynamic-loading gif gui jit jpeg libxml2 png ssl svg threads tiff X xft zlib
app-emacs/emacs-common gui libxml2 X
app-emacs/emacs-daemon
app-emacs/pdf-tools
app-text/poppler png cairo
app-text/texlive X extra graphics png truetype
app-text/texlive-core xetex
dev-lang/ghc
dev-lang/rust clippy doc rustfmt
dev-lisp/sbcl
dev-scheme/racket
dev-util/shellcheck hscolour
dev-vcs/git
media-gfx/ditaa
media-gfx/graphviz
media-gfx/plantuml
media-libs/harfbuzz icu
media-libs/libpng
media-libs/libvpx postproc
media-fonts/jetbrains-mono
media-fonts/noto
net-libs/nodejs
net-p2p/syncthing tools
sys-devel/autoconf
sys-devel/automake
sys-devel/gcc jit
sys-devel/gdb
sys-libs/glibc
sys-libs/zlib
x11-apps/setxkbmap
x11-base/xorg-server
x11-libs/cairo X
x11-libs/libxkbcommon X
x11-misc/arandr
```

### Setup `~/kb` and Syncthing for `org-roam` knowledgebase

Next you need to setup the `~/kb` directory, which will contain:

- `org-roam-directory`
- `org-agenda-files`

Follow [the Syncthing documentation](https://docs.syncthing.net/) and [the Gentoo Wiki entry for Syncthing](https://wiki.gentoo.org/wiki/Syncthing) to get Syncthing setup.

If you need to access the Syncthing GUI from a different computer, you can allow this by modifying `~/.config/syncthing/config.xml`. Find the following in that file:

```xml
<gui enabled="true" tls="false" debugging="false">
        <address>127.0.0.1:40469</address>
```

Change it to the following, but be sure to disable it later for security:

```xml
<gui enabled="true" tls="false" debugging="false">
        <address>0.0.0.0:40469</address>
```

### Run MMOSMacs for the first time

`cd` into your home directory and run `startx` to launch MMOSMacs for the first time. There might be some weird stuff that happens and some warnings, so `M-x kill-emacs` and `startx` again.

Then, run the first-time-setup commands for the following packages:

- `M-x all-the-icons-install-fonts RET y`
- `M-x lsp-install-server RET bash-ls RET`

## Contributing

This is my personal project. I will do what I can to make it accessible
to other people, but ultimately it is crafted to suit my own needs.

If you want to contribute to MMOSMacs, please read `CONTRIBUTING.md`. If
you do not have a copy of this file, it can be found at either:
- https://github.com/MountainMan1312/MMOSMacs/blob/stable/CONTRIBUTING.md
- https://raw.githubusercontent.com/MountainMan1312/MMOSMacs/stable/CONTRIBUTING.md

Feel free to fork this repository. There is no need to give me credit.
It is wrong to claim ownership of knowledge.
