#!/bin/bash

#Prints time and date, lists all logged-in users, gives system uptime.
# Saves result to a logfile.

INFO="$(date)\n$(who)\n$(uptime)"
echo -e $INFO
echo -e $INFO > ~/scripts/logfile

exit
