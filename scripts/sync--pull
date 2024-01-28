#!/bin/sh

owner=${1%/*}
repo=${1#*/}
branch="$2"

config_dir="${XDG_CONFIG_HOME:-$HOME/.config}/toptobes-utils"
mkdir -p "$config_dir"
cd "$config_dir" || exit 1

echo "Using repo $owner/$repo#$branch"
echo

if ! test -s config.json; then
  echo "Initializing config.json"
  echo
  echo '{"ecp":{"forall":"∀","lambda":"λ"}}' > config.json
fi

for dir in templates scripts; do
  mkdir -p "$dir"
  cd "$dir" || exit 2

  rm -r -- *

  pwd

  echo
  curl "https://codeload.github.com/$owner/$repo/tar.gz/$branch" | tar -xz --strip=2 "$repo-$branch/$dir"

  ls --color=auto .
  echo

  cd ..
done

echo "Giving exec perms to scripts"
chmod +x scripts/*
