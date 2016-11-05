#!/bin/sh
desktop=$(bspc query -T -m -d | jshon -e name -u)

echo "   ${desktop}"
