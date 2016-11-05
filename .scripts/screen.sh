#!/bin/sh
set -e

primary_monitor=$(xrandr | grep -o '^.*\sconnected\sprimary' | sed -n 's/^\(.*\)\sconnected.*$/\1/p')
secondary_monitor=$(xrandr | grep -o '^.*\sconnected' | grep -v $primary_monitor | sed -n 's/^\(.*\)\sconnected/\1/p')

function get_primary () {
  echo $primary_monitor
}

function get_secondary () {
  echo $secondary_monitor
}

function center_width () {
  height=$(xrandr | grep '^.*\sconnected' | grep -v $primary_monitor | sed -n 's/^.*connected\s.*x\(.*\)+.*+.*\s(.*$/\1/p')
  large_width=$(xrandr | grep '^.*\sconnected' | grep -v $primary_monitor | sed -n 's/^.*connected\s\(.*\)x.*+.*+.*\s(.*$/\1/p')
  small_width=$(xrandr | grep '^.*\sconnected' | grep $primary_monitor | sed -n 's/^.*connected\s.*\s\(.*\)x.*+.*+.*\s(.*$/\1/p')
  centralized_width=$(echo "($large_width - $small_width) / 2" | bc)
  xrandr --output $primary_monitor --pos "${centralized_width}x${height}"
}

function right () {
  if [[ $secondary_monitor ]]; then
    xrandr --output $secondary_monitor --right-of LVDS1
  else
    echo "No secondary monitor found"
  fi
}

function left () {
  if [[ $secondary_monitor ]]; then
    xrandr --output $secondary_monitor --left-of LVDS1
  else
    echo "No secondary monitor found"
  fi
}

function mirror () {
  if [[ $secondary_monitor ]]; then
    xrandr --output $primary_monitor --same-as $secondary_monitor
  else
    echo "No secondary monitor found"
  fi
}

function primary () {
  monitor=$1
  xrandr --output $monitor --primary
}

function on () {
  monitor=$1
  if [[ $monitor ]]; then
    xrandr --output $monitor --auto
  else
    xrandr --output $secondary_monitor --auto
  fi
}

function off () {
  monitor=$1
  if [[ $monitor ]]; then
    xrandr --output $monitor --off
  else
    xrandr --output $secondary_monitor --off
  fi
}

cmd=$1
arg=$2

$cmd $arg
