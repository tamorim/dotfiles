#!/bin/sh
eth_icon=""
wifi_icon=""

read lo int1 int2 <<< `ip link | sed -n 's/^[0-9]: \(.*\):.*$/\1/p'`

if iwconfig $int1 > /dev/null 2>&1; then
  wifi=$int1
  eth0=$int2
else
  wifi=$int2
  eth0=$int1
fi

if ping -I $eth0 -c 1 8.8.8.8 > /dev/null 2>&1; then
  message="${eth_icon}   connected"
else
  message="${eth_icon}   disconnected"
fi

if [[ $wifi ]]; then
  if ping -I $wifi -c 1 8.8.8.8 > /dev/null 2>&1; then
    message="${message}\n\n${wifi_icon}   connected"
  else
    message="${message}\n\n${wifi_icon}   disconnected"
  fi
fi

echo "$message"
