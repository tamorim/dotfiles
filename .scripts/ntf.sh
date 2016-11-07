#!/bin/sh
if [[ $1 != $(tr -d " " <<< "$1") ]]; then
  cmd=$(sed -n 's/^\(.*\)\s.*$/\1/p' <<< "$1")
else
  cmd=$1
fi
id_file=~/.dunst_$cmd
id=$(cat $id_file 2>/dev/null)
message=$($1 2>/dev/null)
if [[ ! $message ]]; then
  message=$(~/.scripts/$cmd.sh)
fi

if [[ $id ]]; then
  dunstify -p -r $id "$message" > $id_file
else
  dunstify -p "$message" > $id_file
fi
