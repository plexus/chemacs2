#!/bin/sh

chemacs_path=$(realpath "$(dirname "$0")")

main() {
    if [ -L "$HOME/.emacs" ]; then
        check_existing_symlink
    else
        create_symlink_if_possible
    fi
}

check_existing_symlink() {
    target=$(readlink -f "$HOME/.emacs")

    if [ "$target" != "$chemacs_path/.emacs" ]; then
        warn "~/.emacs symlink points elsewhere -> $target"
    else
        ok "chemacs already linked, you're all good."
    fi
}

create_symlink_if_possible() {
    if [ -e "$HOME/.emacs" ]; then
        warn "chemacs can't be installed, ~/.emacs is in the way"
    else
        ok "Creating symlink ~/.emacs -> $chemacs_path/.emacs"
        ln -s "$chemacs_path/.emacs" "$HOME"
    fi
}

warn() {
    printf "WARN\t%s\n" "$1"
}

ok()   {
    printf "OK\t%s\n" "$1"
}

main
