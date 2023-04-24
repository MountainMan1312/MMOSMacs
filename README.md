# MMOSMacs

MMOSMacs is the Emacs configuration for MMOS, which is currently just an
idea I have for an Emacs-centric Linux distribution. MMOSMacs is the
core of my computing experience, and is designed to be a
jack-of-all-trades just like me.


## Installation

First, clone this repository into your `~/.emacs.d` directory.
```bash
cd
git clone https://github.com/MountainMan1312/MMOSMacs.git .emacs.d
```

Next create symlinks to the `xinitrc` and `emacs.desktop` files so X
knows what to do:
```bash
# Backup your `.xinitrc` file if you feel like it.
# Replace `.xinitrc.old` with whatever you want to name the backup.
mv .xinitrc .xinitrc.old

# Symlink your `.xinitrc` file
ln -s .emacs.d/xinitrc .xinitrc

# Symlink `emacs.desktop` to add MMOSMacs as an entry to your Display
# Manager / login screen.
sudo ln -s .emacs.d/emacs.desktop /usr/share/xsessions/emacs.desktop
```


## Plans

- Non-literate configuration
- `straight.el` + `use-package` package management
- Emacs as a desktop environment (`EXWM`, `EDNC`)
- System & network administration suite
- Security & crytography
- Software development environment
- Communications (Email, RSS, IRC, Matrix, etc.)
- Opinionated PKMS and publishing suite
- Common desktop applications
    - Web browser, FTP, Torrent
    - Multimedia production
    - OpenSCAD / ImplicitCAD
    - Finance
    - Games
- Custom modeline & anti-distraction features
- Custom theme deisnged to reduce eye-strain
- Custom keybind system, menus, & dialogs


## Contributing

This is my personal project. I will do what I can to make it accessible
to other people, but ultimately it is crafted to suit my own needs.

If you want to contribute to MMOSMacs, please read `CONTRIBUTING.md`. If
you do not have a copy of this file, it can be found at either:
- https://github.com/MountainMan1312/MMOSMacs/blob/stable/README.md
- https://raw.githubusercontent.com/MountainMan1312/MMOSMacs/stable/README.md

Feel free to fork this repository. There is no need to give me credit.
It is wrong to claim ownership of knowledge.
