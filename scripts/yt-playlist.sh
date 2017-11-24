#!/bin/bash
/usr/bin/youtube-dl --extract-audio --audio-format mp3 --prefer-ffmpeg -w -i -o "~/Music/$1/%(title)s.%(ext)s" $2
