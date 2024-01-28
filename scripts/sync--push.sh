#!/bin/sh

owner=${1%/*}
repo=${1#*/}
branch="$2"

config_dir="${XDG_CONFIG_HOME:-$HOME/.config}/toptobes-utils"
cd "$config_dir" || exit 1

if ! test -s .git; then
  echo "Init git"
  git init
  git remote add origin "https://github.com/$owner/$repo.git"
else
  curr_repo="$(git remote get-url origin | sed -e 's@.*/\(.*\)/\(.*\)\.git@\1/\2@')"

  if [ "$owner/$repo" != "$curr_repo" ]; then
    echo "Expected repo '$owner/$repo'; got '$curr_repo'"
    exit 1
  fi
fi

git fetch
git checkout -b "$branch" "origin/$branch"
git add templates scripts config.json
git commit -am 'cli-sync'
git push origin "$branch"
