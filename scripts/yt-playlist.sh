#!/bin/bash
# This script uses youtube-dl to download a youtube video/playlist to a specified directory
# yt-playlist FILE URL

/usr/bin/youtube-dl --extract-audio --audio-format mp3 --prefer-ffmpeg -w -i -o "$1%(title)s.%(ext)s" $2
