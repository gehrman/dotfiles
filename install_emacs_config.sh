#!/usr/bin/env bash
[[ -f ~/.emacs.d/init.el ]] && rm -r ~/.emacs.d/init.el
ln common/_emacs.d/init.el ~/.emacs.d/

[[ -e ~/.emacs.d/config ]] && rm -rf ~/.emacs.d/config && mkdir ~/.emacs.d/config
for f in `ls common/_emacs.d/config/config-*.el`; do
    ln $f ~/.emacs.d/config/
done
