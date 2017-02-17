#!/bin/sh

base_dir=~/Pictures

function main () {
  file="$(echo $base_dir/*-wlpr.*)"
  if [ -e "$file" ]; then
    render_mode=$(basename "$file" | sed -n 's/^\(.*\)-.*$/\1/p')
    feh --bg-$render_mode $file
  fi
}

main

inotify_id=$(pgrep inotifywait)
if [ -z "$inotify_id" ]; then
  inotifywait -e create,modify,delete $base_dir && while :; do main; done &
fi
