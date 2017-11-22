
#
# ~/.bashrc
#

# If not running interactively, don't do anything
#[[ $- != *i* ]] && return
#powerline_init() {
#
#    is_powerline_session() {
#        hash powerline-daemon >/dev/null 2>&1
#    }
#
#    if is_powerline_session; then 
#        powerline-daemon -q
#        export POWERLINE_BASH_CONTINUATION=1
#        export POWERLINE_BASH_SELECT=1
#        . /usr/lib/python3.6/site-packages/powerline/bindings/bash/powerline.sh
#    fi
#}
alias ls='ls --color=auto'
alias emacsclient='emacsclient -c'
#PS1='\u@\h:\w\$ '
GRAY1="\[$(tput setaf 116)\]"
GRAY2="\[$(tput setaf 251)\]"
COLOR1="\[$(tput setaf 178)\]"
COLOR2="\[$(tput setaf 148)\]"
COLOR3="\[$(tput setaf 188)\]"
COLOR4="\[$(tput setaf 137)\]"
WHITE="\[$(tput setaf 15)\]"
BOLD="\[$(tput bold)\]"
RESET="\[$(tput sgr0)\]"
export PS1="${BOLD}${GRAY2}\u:\w \$${RESET} "

#export PS1="${COLOR4}${BOLD}${COLOR1}\u${WHITE}:${COLOR2}\w${COLOR4}${COLOR3} \$${RESET} "
export PATH=$PATH:~/.local/bin:~/scripts
export EDITOR=vim
#powerline-daemon -q
#POWERLINE_BASH_CONTINUATION=1
#POWERLINE_BASH_SELECT=1
#. ~/.local/lib/python3.6/site-packages/powerline/bindings/bash/powerline.sh
#powerline_init
#source ~/.bash-powerline.sh

#exports
#export QT_SCALE_FACTOR=1
#export MATLAB_JAVA=/usr/lib/jvm/java-7-openjdk/jre

#ntfs folder color
eval $(dircolors -b ~/.dircolors)
