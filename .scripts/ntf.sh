#!/bin/sh
cmd=$(sed -n 's/^\(.*\)\s\?.*$/\1/p' <<< "$1")
id_file="~/.dunst_${cmd}"
id=$(cat $id_file 2>/dev/null)
message=$(eval "$1 2>/dev/null")
if [[ ! $message ]]; then
  message=$(eval "~/.scripts/${cmd}.sh")
fi

if [[ $id ]]; then
  eval "dunstify -p -r $id \"$message\" > $id_file"
else
  eval "dunstify -p \"$message\" > $id_file"
fi
