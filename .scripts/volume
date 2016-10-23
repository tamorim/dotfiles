#!/bin/sh
unmuted=$(amixer get Master | sed -n 'N;s/^.*\[\(\w*\)\]$/\1/p')

if [ $unmuted == "off" ]; then
  echo ""
else
  volume=$(amixer get Master | sed -n 'N;s/^.*\[\([0-9]\+%\).*$/\1/p')
  if [[ $volume =~ ^(100|[5-9][0-9])%$ ]]; then
    echo "   ${volume}"
  elif [[ $volume =~ ^([1-4][0-9]|[1-9])%$ ]]; then
    echo "   ${volume}"
  else
    echo "   ${volume}"
  fi
fi
