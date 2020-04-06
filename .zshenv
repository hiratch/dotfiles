# $HOME/.zshenv
#
# comment for rsync auto login.
#if [ ! -z "$SSH_CLIENT" -a -z "$SSH_TTY" ]; then
#    return
#fi

#if [ "$SSH_TTY" = "" ]; then
#    return
#fi

# echo "Loading $HOME/.zshenv"

### env

# directory for configuration files
ZDOTDIR=$HOME/.zsh
ZUSRDIR=$ZDOTDIR
# prompt

#PROMPT="%n@%l'%U%m%u[`jobcount()`]:%4~%# "
PROMPT="%n@%l''%U%m%u[\${#jobstates}]:%~%# "

#RPROMPT='    20%D %D{(%a) %H:%M:%S}'
#RPROMPT='%D{%H:%M:%S}'            

#SPROMPT='zsh: correct '\''%R'\'' to '\''%r'\'' [nyae]? '
SPROMPT='zsh: replace '\''%R'\'' to '\''%r'\'' ? [Yes/No/Abort/Edit] '
#SPROMPT='zsh: replace '\''%R'\'' -> '\''%r'\'' ? [Yes/No/Abort/Edit] '

# root attention.
if [ $UID = 0 ]; then
#    PROMPT="%BROOT%b@%l'%U%m%u:%~%# "
    PROMPT="%BROOT%b@%l'%U%m%u[\${#jobstates}]:%5~%# "
fi

# set defaault
PROMPT_DEFAULT="$PROMPT"
RPROMPT_DEFAULT="$RPROMPT"

# jless
LESSCHARSET=

# for X client
#DISPLAY="192.168.0.210:0.0"

# term
TERM=xterm

# pager
PAGER=lv

# editor
EDITOR=vim

# surpress perl warning
export PERL_BADLANG=0
export PERLDOC_PAGER=${PAGER}

# less
export LESS='-g -i -M -R'

### path setting
PATH="/opt/local/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH"

# #path=($HOME/bin)
# path=(/usr/local/bin /usr/bin    \
#       /usr/X11R6/bin /bin        \
#       /usr/sbin /sbin)

# # X
# if [ -x /usr/X11R6/bin/X ]; then
#     export X11HOME=/usr/X11R6
#     path=($path $X11HOME/bin $X11HOME/libexec)
# fi

# FreeBSD
#system=`uname -s`
#if [ $system == 'FreeBSD' ]; then
#	path=($path /stand)
#fi

if [ ! -f $HOME/.ssh/known_hosts2 ]; then
    touch $HOME/.ssh/known_hosts2
fi

# apache
#if [ -f /usr/local/apache/bin ]; then
#    path=($path /usr/local/apache/bin)
#fi

# namazu
#if [ -f /usr/local/namazu/bin ];then
#    path=($path /usr/local/namazu/bin)
#fi

# samba
#if [ -f /usr/local/samba/bin ]; then
#    path=($path /usr/local/samba/bin)
#fi

# ssl
#if [ -f /usr/local/ssl ]; then
#    path=($path /usr/local/ssl/bin)
#fi

# pg
#if [ -f /usr/local/pgsql/bin ]; then
#    path=($path /usr/local/pgsql/bin)
#fi

# mysql
#if [ -f /usr/local/mysql/bin ]; then
#    path=($path /usr/local/mysql/bin /usr/local/mysql/libexec)
#fi

# qmail
#if [ -f /var/qmail/bin ]; then
#    path=($path /var/qmail/bin)
#fi

# proftpd
#if [ -f /usr/local/proftpd/sbin ]; then
#    path=($path /usr/local/proftpd/sbin)
#fi

# majordomo
#if [ -f /usr/local/majordomo/bin ]; then
#    path=($path /usr/local/majordomo/bin)
#fi

