#!/bin/bash
OPTIONS="Yes No"
select opt in $OPTIONS; do
  if [ $opt == "Yes" ]; then
    echo "You selected Yes"
    exit
  else 
    echo "You selected No"
    exit
  fi
done
