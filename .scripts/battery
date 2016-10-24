#!/bin/sh
BATC=$(cat /sys/class/power_supply/BAT0/capacity)
BATS=$(cat /sys/class/power_supply/BAT0/status)

if [ $BATS = "Charging" ]; then
  echo "   ${BATC}"
else
  if [ $BATC = "100%" ]; then
    echo "   ${BATC}"
  elif [[ $BATC =~ ^([8-9][0-9]|7[5-9])$ ]]; then
    echo "   ${BATC}"
  elif [[ $BATC =~ ^(7[0-4]|[5-6][0-9])$ ]]; then
    echo "   ${BATC}"
  elif [[ $BATC =~ ^([2-4][0-9]|1[1-9])$ ]]; then
    echo "   ${BATC}"
  else
    echo "   ${BATC}"
  fi
fi
