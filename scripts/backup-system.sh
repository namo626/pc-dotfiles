#!/bin/bash

# backup-system.sh - backup files in root using tar

DATE=$(date +%Y-%m-%d)
LOC="/var/backups"
COMMAND="tar -cvpzf /var/backups/$DATE-backup.tar.gz --exclude=/var/backups/$DATE-backup.tar.gz --one-file-system /"

while true; do
  read -p "Make a whole system backup in ${LOC}/ ? [y/N]" arg
  case $arg in
    [Yy]* ) echo "Starting backup..."; break;;
    [Nn]* ) exit;;
    "" ) exit;;
    * ) echo "Answer yes or no";;
  esac
done

$COMMAND


