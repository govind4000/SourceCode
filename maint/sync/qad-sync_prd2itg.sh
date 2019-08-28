#!/usr/bin/sh 
# Shellscript: qad-sync-prd2itg.sh
# Description: rsync QAD EE to Single Tier
#
# %Z% SCCS-Id:%P% %I% %G% %U% tkl
# REVISION: EE   LAST MODIFIED: 08 Aug 2015  BY: HP/TKL

APPNAME=$(basename $0 | sed "s/\.sh$//") 
 
# ----------------------------------------------------------------------------- 
# Log functions 
# ----------------------------------------------------------------------------- 
  
log_info()  { echo "$APPNAME: $1"; } 
log_warn()  { echo "$APPNAME: [WARNING] $1" 1>&2; } 
log_error() { echo "$APPNAME: [ERROR] $1" 1>&2; } 

while : ; do

    LOG_FILE="$APPNAME-$(date +"%Y-%m-%d-%H%M%S").log"
 
    rsync -a -r -v -o -t -z --stats --progress \
       --log-file="$LOG_FILE" --log-format='%t %o %B %l %M %f' \
       --exclude-from '/home/thkl/sync/qad_exclude' \
       -e 'ssh -i /home/thkl/.ssh/id_rsa_qadeeitg_aio' /opt/qadee/ee2013/ qadeeitg@qadee-itg.austin.hpicorp.net:/opt/qadee/ee2013/tkl/
 
    exit 0
done
