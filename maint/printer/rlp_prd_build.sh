#!/usr/bin/sh
# Shellscript: rlp_prd_build.sh
# Description: Create remote LP
#
# %Z% SCCS-Id:%P% %I% %G% %U% tkl
# REVISION: EE   LAST MODIFIED: 21 Jul 2015  BY: HP/TKL


LogFile="/tmp/rlp_prd_build.log"
date > $LogFile
echo " " >> $LogFile
echo "lpstat -v list:" >> $LogFile
echo " " >> $LogFile
lpstat -v >> $LogFile
echo " " >> $LogFile
lpshut
sleep 10
echo "Check if lp scheduler has stopped" >> $LogFile
echo " " >> $LogFile
ps -ef | grep lpsched >> $LogFile
echo " " >> $LogFile
echo "lp config before running printer install" >> $LogFile
echo " " >> $LogFile

QueueRun="./queues.prd.db"
echo "got to QueueFile: $QueueRun" >> $LogFile
echo " " >> $LogFile
echo "Now adding printers from $QueueFile" >> $LogFile
echo " " >> $LogFile
while read PNAME SNAME RPNAME
do
	lpadmin -p$PNAME -mrmodel -v/dev/null -orm$SNAME -orp$RPNAME -ocmrcmodel -osmrsmodel
        enable $PNAME
        accept $PNAME
#       echo
#       echo "Be sure $PNAME is configured on $SNAME"
#       echo
done < $QueueRun
date >> $LogFile
echo "Starting lp scheduler" >> $LogFile
echo " " >> $LogFile
lpsched -v -a
echo "lpstat -v list: final lp config" >> $LogFile
echo " " >> $LogFile
lpstat -v >> $LogFile
echo " " >> $LogFile
echo "rlp_build.sh is Done!" >> $LogFile
echo "rlp_build.sh is Done!"
chmod 644 $LogFile
mailx -s rlp__prd_build_results theo.kleijkers@hp.com < $LogFile
