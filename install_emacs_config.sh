#!/usr/bin/env bash
[[ -e ~/.emacs.d/config ]] && rm -r ~/.emacs.d/config
cp -r common/_emacs.d/* ~/.emacs.d
