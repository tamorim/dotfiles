#!/bin/sh
unmuted=$(amixer get Master | grep '\[off\]')

if [ -n "$unmuted" ]; then
  echo ""
else
  volume=$(amixer get Master | grep -o -m 1 '\[[0-9]\+%\]' | sed -n 's/^\[\([0-9]\+\)%\]$/\1/p')
  if [ "$volume" = "100" ] || (($volume > 49)); then
    echo "   ${volume}%"
  elif (($volume < 50)) && (($volume > 0)); then
    echo "   ${volume}%"
  else
    echo ""
  fi
fi
