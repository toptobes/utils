#!/bin/bash

ide_type="$1"
project_path="$2"

base_dir="/mnt/c/Program Files/JetBrains"

find_latest_version()
{
  ide_name=$1
  latest_version=$(ls -d "$base_dir/$ide_name"*/ 2>/dev/null | sort -Vr | head -n 1)
  echo "$latest_version"
}

convert_path()
{
  echo "$1" | sed -e 's@/mnt/c@C:@' -e 's@/@\\@g'
}

case "$ide_type" in
  idea)
    latest_version=$(find_latest_version "IntelliJ IDEA")
    ;;
  clion)
    latest_version=$(find_latest_version "CLion")
    ;;
  webstorm)
    latest_version=$(find_latest_version "WebStorm")
    ;;
  *)
    echo "Usage: jb <idea|clion|webstorm> [project_path]"
    exit 1
    ;;
esac

if [[ -n "$latest_version" ]]; then
  exec_path="${latest_version}bin/${ide_type}64.exe"
  exec_path_windows=$(convert_path "$exec_path")
  powershell.exe -Command "Start-Process '$exec_path_windows' $project_path"
else
  echo "Could not find the IDE: $ide_type"
  exit 1
fi
