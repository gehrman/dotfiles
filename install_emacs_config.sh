#!/usr/bin/env bash
[[ -e ~/.emacs.d/config ]] && rm -r ~/.emacs.d/config
cp -r _emacs.d/* ~/.emacs.d
