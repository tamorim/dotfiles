#!/bin/sh

for script in $(ls ~/.scripts); do
  if [ "$script" != "bootstrap.sh" ]; then
    alias_name=$(echo $script | sed -n 's/^\(.*\)\.sh/\1/p')
    alias $alias_name=~/.scripts/$script
  fi
done
