#! /bin/bash

packages=("amazon-music"
          "iterm2"
          "pyenv"
          "pyenv-virtualenv"
          "zsh"
          "homebrew/cask/emacs")

for package in ${packages[@]}; do
  brew install $package
done
