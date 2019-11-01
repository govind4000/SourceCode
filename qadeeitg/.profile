
# @(#)B.11.31_LR       

# Default user .profile file (/usr/bin/sh initialization).

# Set up the terminal:
	export TERM=vt220
	stty erase "^H" kill "^U" intr "^C" eof "^D"
	stty hupcl ixon ixoff
	tabs

# NOTE: '.' is added to $PATH for compatibility reasons only. This
#       default will be changed in a future release. If "." is not
#       needed for compatibility it is better to omit this line. 
#	Please edit .profile according to your site requirements.

# Set up the search paths:
        PATH=$PATH:/opt/qadee/local/bin:/usr/local/bin:/opt/qadee/progress/dlc113:/opt/qadee/progress/dlc113/bin:.	

# Set up the shell environment:
	set -u
	trap "echo 'logout'" 0

# Set up the shell variables:
	EDITOR=vi
	export EDITOR
mesg n
export TMOUT=0
export FRAGMENT=""
