#!/bin/sh

base_dir=~/Pictures

function main () {
  file="$(ls $base_dir | grep -e '^\(fill\|tile\|center\|max\|scale\)-wlpr.\(jpe\?g\|png\|gif\)$')"
  if [ -n "$file" ]; then
    render_mode="$(echo $file | sed -n 's/^\(.*\)-.*$/\1/p')"
    feh --bg-$render_mode "$base_dir/$file"
  fi
}

main

inotify_id=$(pgrep inotifywait)
if [ -z "$inotify_id" ]; then
  inotifywait -e create,modify,delete $base_dir && while :; do main; done &
fi
