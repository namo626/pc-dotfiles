
#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
#PS1='[\u@\h \W]\$ '
COLOR1="\[$(tput setaf 47)\]"
COLOR2="\[$(tput setaf 45)\]"
COLOR3="\[$(tput setaf 226)\]"
COLOR4="\[$(tput setaf 172)\]"
WHITE="\[$(tput setaf 15)\]"
BOLD="\[$(tput bold)\]"

RESET="\[$(tput sgr0)\]"
export PS1="${COLOR4}${BOLD}[${COLOR1}\u${WHITE}:${RESET}${COLOR2}\w${BOLD}${COLOR4}]${COLOR3} ${RESET} "

#powerline-daemon -q
#POWERLINE_BASH_CONTINUATION=1
#POWERLINE_BASH_SELECT=1
#. /usr/lib/python3.6/site-packages/powerline/bindings/bash/powerline.sh

