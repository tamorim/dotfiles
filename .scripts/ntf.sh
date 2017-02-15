#!/bin/sh
if [ "$1" != "$(echo "$1" | tr -d " ")" ]; then
  cmd=$(echo "$1" | sed -n 's/^\(.*\)\s.*$/\1/p')
  args=$(echo "$1" | sed -n 's/^.*\s\(.*\)$/\1/p')
else
  cmd=$1
  args=""
fi
id_file=~/.dunst_$cmd
id=$(cat $id_file 2>/dev/null)
message=$($1 2>/dev/null)
if [ ! "$message" ]; then
  message=$(~/.scripts/$cmd.sh $args)
fi

if [ "$id" ]; then
  dunstify -p -r $id "$message" > $id_file
else
  dunstify -p "$message" > $id_file
fi
