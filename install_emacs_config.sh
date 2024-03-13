#!/usr/bin/env bash

# This is somewhat more manual than the current dotfile install process, but
# we're not going to worry about that for now, since this config has considerably
# more structure - we only need to create the appropriate directories if they
# don't yet exist (just .emacs.d and config) and link init.el and the config
# files.

mkdir -p ~/.emacs.d/{config,lisp,package-config}
for f in `ls common/_emacs.d/*.el`; do
  if [[ $f  == *flycheck*.el ]]; then
    echo skipping flycheck file
    continue
  fi
  ln -s `pwd`/$f ~/.emacs.d/
  if [[ $? == 0 ]]; then
    echo Linked $f in .emacs.d.
  fi
done

for dir in "config" "lisp" "package-config"; do
  for f in `ls common/_emacs.d/$dir/*.el`; do
    ln -s `pwd`/$f ~/.emacs.d/$dir/
    if [[ $? == 0 ]]; then
      echo Linked $f .emacs.d/$dir
    fi
  done
done
