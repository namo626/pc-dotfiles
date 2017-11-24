#!/bin/bash

title=$(/usr/bin/mpc status | sed 1q)
title=${title::-4}

echo $title
