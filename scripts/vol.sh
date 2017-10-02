#!/bin/bash
vol=$(pamixer --get-mute)
stat=$(pamixer --get-volume)

case $vol in
	"true") bar=;;
	*) bar= ;;
esac

echo $stat%
