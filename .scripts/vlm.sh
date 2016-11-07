#!/bin/sh
unmuted=$(amixer get Master | grep '\[off\]')

if [[ -n $unmuted ]]; then
  echo ""
else
  volume=$(amixer get Master | grep -o -m 1 '\[[0-9]\+%\]' | sed -n 's/^\[\([0-9]\+%\)\]$/\1/p')
  if [[ $volume =~ ^(100|[5-9][0-9])%$ ]]; then
    echo "   ${volume}"
  elif [[ $volume =~ ^([1-4][0-9]|[1-9])%$ ]]; then
    echo "   ${volume}"
  else
    echo "   ${volume}"
  fi
fi
