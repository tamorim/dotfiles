#!/bin/sh
eth_icon=""
wifi_icon=""
interfaces=($(ip link | sed -n 's/^[0-9]: \(.*\):.*$/\1/p'))
int1=${interfaces[1]}
int2=${interfaces[2]}

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

if [[ -n $wifi ]]; then
  if ping -I $wifi -c 1 8.8.8.8 > /dev/null 2>&1; then
    message="${message}\n\n${wifi_icon}   connected"
  else
    message="${message}\n\n${wifi_icon}   disconnected"
  fi
fi

echo "$message"
