#!/bin/sh

owner=${1%/*}
repo=${1#*/}
branch="$2"

config_dir="${XDG_CONFIG_HOME:-$HOME/.config}/toptobes-utils"
mkdir -p "$config_dir"
cd "$config_dir" || exit 1

echo "Using repo $owner/$repo#$branch"
echo

curl -o repo.tar.gz "https://codeload.github.com/$owner/$repo/tar.gz/$branch"

# if ! test -s config.json; then
#   echo "Initializing config.json"
#   echo
#   echo '{"ecp":{"forall":"∀","lambda":"λ"}}' > config.json
# fi

rm -r config.json templates scripts

for dir in templates scripts config.json; do
  tar -xzf repo.tar.gz --strip=1 "$repo-$branch/$dir"
done

rm repo.tar.gz

tree
echo

echo "Giving exec perms to scripts"
chmod +x scripts/*
