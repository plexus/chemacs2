#!/bin/bash

function main() {
    if [[ -L "$HOME/.emacs" ]]; then
        check_existing_symlink
    else
        create_symlink_if_possible
    fi
}

function check_existing_symlink() {
    local target=$(readlink -f $HOME/.emacs)

    if [[ "$target" != "`chemacs_home`.emacs" ]]; then
        warn "~/.emacs symlink points elsewhere -> $target"
    else
        ok "chemacs already linked, you're all good."
    fi
}

function create_symlink_if_possible() {
    if [[ -e "$HOME/.emacs" ]]; then
        warn "chemacs can't be installed, ~/.emacs is in the way"
    else
        ok "Creating symlink ~/.emacs -> `chemacs_home`.emacs"
        ln -s "`chemacs_home`.emacs" "$HOME"
    fi
}

function chemacs_home() {
    cd `dirname "${BASH_SOURCE[0]}"` && echo "`pwd`/$1"
}

function warn() {
    echo -e "WARN\t$1"
}
function ok()   {
    echo -e "OK\t$1"
}


main
