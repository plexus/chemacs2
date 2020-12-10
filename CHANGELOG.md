# Unreleased

- Add support for early-init.el
- Change installation from `~/.emacs` to `~/.emacs.d`
- Allow .emacs-profiles and .emacs-profile to be stored in $XDG\_CONFIG\_HOME/chemacs
- Allow loading a literal profile from the cli (e.g. `emacs --with-profile '((user-emacs-directory . "/path/to/config"))'` works)

# 1.0 (2020-10-01 / 4dad0684)

- Only load `custom-file` when it is different from `init-file` (prevent double loading of `init-file`)
- Fixes to the install script for OS X
- Documentation fixes
- Introduce `chemacs-version` variable
- start keeping a CHANGELOG

# 0.6 (2020-02-23 / 71e30878)

- NixOS support in installation script
- Add Powershell installation script

# 0.5 (2020-01-14 / 4c279476)

- First class support for straight.el
- Documentation fixes

# 0.4 (2018-10-01 / 68382d50)

- Support GNU style `--with-profile=profilename` (so with equal sign)

# 0.3 (2018-09-30 / 1140501d)

- Allow selection of default profile using `~/.emacs-profile`

# 0.2 (2018-06-06 / 1f5601a9)

- Add installer script
- Improve documentation
- Improve support for older Emacsen
- Document how to use with Doom

# 0.1 (2018-05-18 / 8500636a)

- Initial release
