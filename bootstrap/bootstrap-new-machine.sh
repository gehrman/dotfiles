#! /bin/bash

# Bootstrap Chrome
if [[ -f /usr/local/bin/brew ]]; then
  echo Chrome found, skipping install
else
  curl https://dl.google.com/chrome/mac/stable/GGRO/googlechrome.dmg > gc.dmg
  open gc.dmg
fi

# Get Brew
if [[ -f /usr/local/bin/brew ]]; then
  echo Brew found, skipping install
else
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

# Get Ansible
#brew install pyenv
#pyenv install 3.9.5
#pyenv global 3.9.5
#brew install ansible

# Setup ZSH
brew install zsh
sudo bash setup-homebrew-zsh.sh

# bash get-fonts.sh
bash chrome-extensions.sh
bash install-packages.sh
