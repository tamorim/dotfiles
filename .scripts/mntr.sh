#!/bin/sh
monitor=$(bspc query -T -m | jshon -e name -u)

echo "   ${monitor}"
