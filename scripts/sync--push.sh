#!/bin/sh

owner=${1%/*}
repo=${1#*/}
branch="$2"

tmp_dir="${XDG_CONFIG_HOME:-$HOME/.config}/toptobes-utils/git-sync-tmp"
mkdir -p "$tmp_dir"
cd "$tmp_dir" || exit 1

git init
git remote add origin "https://github.com/$owner/$repo.git"
git fetch
git checkout -b "$branch" "origin/$branch"

for file in config.share.json templates scripts; do
  rm -r $file
  cp -r ../$file .
  git add $file
done

git commit -am 'cli-sync'
git push origin "$branch"

cd ..
rm -rf git-sync-tmp
