#!/bin/bash

stat=$(/usr/bin/mpc status | awk 'NR == 2 { print $1 }')
title=$(/usr/bin/mpc status | sed 1q)
title=${title::-4}

case $stat in
  "[playing]") echo "  $title";;
  *) echo "  $title";;
esac


