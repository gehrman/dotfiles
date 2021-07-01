#! /bin/bash

grep -q "/usr/local/bin/zsh" /etc/shells
zsh_present=$?

# Include homebrew ZSH in shells file, if it's not there already
if [[ zsh_present -ne 0 ]]; then
  echo "/usr/local/bin/zsh" >> /etc/shells
fi
