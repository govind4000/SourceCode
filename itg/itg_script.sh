#!/usr/bin/sh
# Shellscript: isproc.status
# Description: All HP IndigoServe Processes - Status
#              Admin Script Version: EE
#              For PROGRESS release: 11.3
#
# %Z% SCCS-Id:%P% %I% %G% %U% 
# REVISION: EE   LAST MODIFIED: 19 Sep 2018  BY: HP/YGR

lcday="`date | cut -c1-3`"
echo $lcday

case $lcday in 

   Sat) echo "Saturday"
        ssh -l qadeeitg qadee-itg.austin.hpicorp.net /opt/qadee/local/bin/admin/itg_scripts_${lcday}.sh;;
        
esac
