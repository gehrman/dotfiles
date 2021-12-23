#!/bin/bash

if [[ -d ./fonts ]]; then
  echo Fonts cloned, refreshing checkout
  pushd fonts > /dev/null
  git pull
  popd > /dev/null
else
  git clone https://github.com/gehrman/fonts.git
fi

pushd fonts/font-files/ > /dev/null
find . -type f -name '*' -print0 | while IFS= read -r -d '' file; do
  echo Installing $file
  open "$file"
done
popd > /dev/null
