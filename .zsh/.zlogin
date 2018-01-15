# $ZDOTDIR/.zlogin
#
echo "Loading $ZDOTDIR/.zlogin"

# set prompt (see .zsh/zshrc.user)
#prompt 5

# set my directory
#HOME=/home/ryo

# clean display
#clear

# display (or not) messages from other users
#mesg y

# mail notification
#biff y

# show system status
uptime
echo '------------------------'

# hostname
# echo "this machine is '${HOST}'."
# echo '------------------------'

# who?
who | sort
echo '------------------------'

# mail
#from 6>/dev/null
#subject

# welcome message
#echo "*** How are you today, I'm ready to go. :-) ***"
echo ''

# not in use
#fortune
#stty dec new cr0 -tabs
#ttyctl -f  # freeze the terminal modes... can't change without a ttyctl -u

## use instead 'MacSSH'.
# nifencode auto-login
#if [ "x${SSH_CLIENT}" = "x127.0.0.1" ]; then
#        echo "*** nifencode enabled.";
#else
#	echo "starting nifencode ...";
##	${HOME}/bin/Nifencode.sh
#fi


