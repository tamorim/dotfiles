#!/bin/sh
set -e

primary_monitor=$(xrandr | grep -o '^.*\sconnected\sprimary' | sed -n 's/^\(.*\)\sconnected.*$/\1/p')
secondary_monitor=$(xrandr | grep -o '^.*\sconnected' | grep -v $primary_monitor 2> /dev/null | sed -n 's/^\(.*\)\sconnected/\1/p')

function get_primary () {
  if [[ -n $primary_monitor ]]; then
    echo $primary_monitor
  else
    echo "No primary monitor found" >&2
  fi
}

function get_secondary () {
  if [[ -n $secondary_monitor ]]; then
    echo $secondary_monitor
  else
    echo "No secondary monitor found" >&2
  fi
}

function centerw () {
  if [[ -n $secondary_monitor ]]; then
    height=$(xrandr | grep '^.*\sconnected' | grep -v $primary_monitor | sed -n 's/^.*connected\s.*x\(.*\)+.*+.*\s(.*$/\1/p')
    large_width=$(xrandr | grep '^.*\sconnected' | grep -v $primary_monitor | sed -n 's/^.*connected\s\(.*\)x.*+.*+.*\s(.*$/\1/p')
    small_width=$(xrandr | grep '^.*\sconnected' | grep $primary_monitor | sed -n 's/^.*connected\s.*\s\(.*\)x.*+.*+.*\s(.*$/\1/p')
    centralized_width=$(echo "($large_width - $small_width) / 2" | bc)
    xrandr --output $primary_monitor --pos "${centralized_width}x${height}"
  else
    echo "No secondary monitor found" >&2
  fi
}

function right () {
  if [[ -n $secondary_monitor ]]; then
    xrandr --output $secondary_monitor --right-of LVDS1
  else
    echo "No secondary monitor found" >&2
  fi
}

function left () {
  if [[ -n $secondary_monitor ]]; then
    xrandr --output $secondary_monitor --left-of LVDS1
  else
    echo "No secondary monitor found" >&2
  fi
}

function mirror () {
  if [[ -n $secondary_monitor ]]; then
    primary_resolutions=$(xrandr | sed -n '/'"$primary_monitor"'/,/^\w/{/^\w/b;p}' | grep -o '[0-9]\+x[0-9]\+')
    secondary_resolutions=$(xrandr | sed -n '/'"$secondary_monitor"'/,/^\w/{/^\w/b;p}' | grep -o '[0-9]\+x[0-9]\+')
    mirrorable=false

    for primary_resolution in $primary_resolutions; do
      for secondary_resolution in $secondary_resolutions; do
        if [[ $primary_resolution = $secondary_resolution ]]; then
          mirrorable=true
          xrandr --output $primary_monitor --mode $primary_resolution --output $secondary_monitor --mode $primary_resolution
          break
        fi
      done
      if [[ $mirrorable = true ]]; then
        break
      fi
    done

    if [[ $mirrorable = true ]]; then
      xrandr --output $primary_monitor --same-as $secondary_monitor
    else
      echo "No matching resolution was found" >&2
    fi
  else
    echo "No secondary monitor found" >&2
  fi
}

function primary () {
  monitor=$1
  if [[ -n $monitor ]]; then
    xrandr --output $monitor --primary
  else
    echo "No monitor passed" >&2
  fi
}

function on () {
  monitor=$1
  if [[ -n $monitor ]]; then
    xrandr --output $monitor --auto
  else
    if [[ -n $secondary_monitor ]]; then
      xrandr --output $secondary_monitor --auto
    else
      echo "No secondary monitor found" >&2
    fi
  fi
}

function off () {
  monitor=$1
  if [[ -n $monitor ]]; then
    xrandr --output $monitor --off
  else
    if [[ -n $secondary_monitor ]]; then
      xrandr --output $secondary_monitor --off
    else
      echo "No secondary monitor found" >&2
    fi
  fi
}

cmd=$1
arg=$2

$cmd $arg
