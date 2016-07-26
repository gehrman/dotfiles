#!/usr/bin/env bash

# This is somewhat more manual than the current dotfile install process, but
# we're not going to worry about that for now, since this config has considerably
# more structure - we only need to create the appropriate directories if they
# don't yet exist (just .emacs.d and config) and link init.el and the config
# files.

mkdir -p ~/.emacs.d/config
ln -s common/_emacs.d/init.el ~/.emacs.d/

for f in `ls common/_emacs.d/config/config-*.el`; do
    ln -s $f ~/.emacs.d/config/
done
