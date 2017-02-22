#!/bin/sh

for script in $(ls ~/.scripts | grep -v bootstrap); do
  alias_name=$(echo $script | sed -n 's/^\(.*\)\.sh/\1/p')
  alias $alias_name=~/.scripts/$script
done
