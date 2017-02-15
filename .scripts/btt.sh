#!/bin/sh
set -e

BATC=$(cat /sys/class/power_supply/BAT0/capacity)
BATS=$(cat /sys/class/power_supply/BAT0/status)

function has_battery () {
  if [ -n "$(ls /sys/class/power_supply)" ]; then
    return 0
  else
    return 1
  fi
}

function capacity () {
  echo "$BATC"
}

function status () {
  echo "$BATS"
}

function format () {
  if [ has_battery ]; then
    if [ "$BATS" = "Charging" ]; then
      echo "   ${BATC}"
    else
      if [ "$BATC" = "100" ] || (($BATC > 89)); then
        echo "   ${BATC}"
      elif (($BATC < 90)) && (($BATC > 69)); then
        echo "   ${BATC}"
      elif (($BATC < 70)) && (($BATC > 49)); then
        echo "   ${BATC}"
      elif (($BATC < 50)) && (($BATC > 9)); then
        echo "   ${BATC}"
      else
        echo "   ${BATC}"
      fi
    fi
  else
    echo "No battery info to show"
  fi
}

cmd=$1
$cmd
