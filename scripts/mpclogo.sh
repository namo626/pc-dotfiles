#!/bin/bash

stat=$(/usr/bin/mpc status | awk 'NR == 2 { print $1 }')

case $stat in
  "[playing]") s= ;;
  *) s= ;;
esac

echo $s


