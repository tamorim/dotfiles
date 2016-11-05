#!/bin/sh
current=$(~/.scripts/spotify.sh current)
title=$(echo "${current}" | grep '^Title' | sed -n 's/^Title\s\+\(.*\)/\1/p')
artist=$(echo "${current}" | grep '^Artist' | sed -n 's/^Artist\s\+\(.*\)/\1/p')
album=$(echo "${current}" | grep '^Album\s' | sed -n 's/^Album\s\+\(.*\)/\1/p')

echo "${title}\n${artist} - ${album}"
