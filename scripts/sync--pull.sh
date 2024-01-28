#!/bin/sh

owner=${1%/*}
repo=${1#*/}
branch="$2"

config_dir="${XDG_CONFIG_HOME:-$HOME/.config}/toptobes-utils"
mkdir -p "$config_dir"
cd "$config_dir" || exit 1

echo "Using repo $owner/$repo#$branch"
echo

if ! test -s config.local.json; then
  echo "Initializing config.local.json"
  echo
  echo '{}' > config.local.json
fi

curl -o repo.tar.gz "https://codeload.github.com/$owner/$repo/tar.gz/$branch"
echo

to_replace="config.share.json templates scripts"

# shellcheck disable=SC2086
rm -r $to_replace

for dir in $to_replace; do
  tar -xzf repo.tar.gz --strip=1 "$repo-$branch/$dir"
done

rm repo.tar.gz

tree
echo

echo "Giving exec perms to scripts"
chmod +x scripts/*
