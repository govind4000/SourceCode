#!/usr/bin/sh
# Shellscript: sftp.inc
# Description: Include with logic for sftp
#
# %Z% SCCS-Id:%P% %I% %G% %U% tkl
# REVISION: eE   LAST MODIFIED: 14 Jul 2015  BY: HP/RDU
# REVISION: EE   LAST MODIFIED: 20 Jul 2015  BY: HP/TKL

################################################################################
## GENERAL FUNCTIONS                                                          ##
################################################################################

#logPath="${HOME}"
logPath="/opt/shared/sys/log/sftp"


dateTimeTag () {
   date "+(%b %d, %Y %H:%M:%S) "
} # dateTimeTag

exitError () {
   echo "${1} (${2})"
   exit ${2}
} # exitError

exitHelp () {
   echo "Usage: ${thisScriptFile} ( start | status ) [ -quiet ]"
   exit ${1}
} # exitHelp

isFileDOS () {
   local fileName="${1}"
      
   if [ ! -f "${fileName}" ] || [ ! -r "${fileName}" ]
   then
      echo 9
   else
      # UX files use \n, DOS files \r\n. 1 line ending \r thus will indicate DOS file
      echo $(head -1 "${fileName}"|awk '{if ($0~/\r$/){print 1} else {print 0}; exit}')
   fi
} # isFileDOS

isFileUX () {
   local isFileDOS="$(isFileDOS "${1}")"
   
   case ${isFileDOS} in
      0)
         echo 1
      ;;
      1)
         echo 0
      ;;
      *)
         echo ${isFileDOS}
      ;;
   esac
} # isFileUX

lineCount () {
   if [ -z "${1}" ]
   then
      echo 0
   else
      echo "$(echo "${1}" | wc -l)"
   fi
} # lineCount

toLower () {
   echo "${1}"|awk '{print tolower($0)}'
} # toLower

toUpper () {
   echo "${1}"|awk '{print toupper($0)}'
} # toUpper

trim () {
   echo "${1}"|awk 'gsub("^[ ]*|[ ]* $","",$0) {print $0}'
} # trim

################################################################################
## LOGGING FUNCTIONS                                                          ##
################################################################################

handleInfo () {
   # Expects two parameters
   # 1) Function
   # 2) Info Text
   
   writeOutput "$(printf "%-25s" "${1}")    INFO: ${2}"
   
   return 0
} # handleInfo

handleError () {
   # Expects three parameters
   # 1) Function
   # 2) Error Code
   # 3) Error Text
   
   if [ ${2} = 0 ]
   then
      return 0
   fi
   
   writeOutput "$(printf "%-25s" "${1}")   ERROR: ${3} (${2})"
   
   return ${2}
} # handleError

handleWarning () {
   # Expects three parameters
   # 1) Function
   # 2) Error Code
   # 3) Error Text
   
   if [ ${2} = 0 ]
   then
      return 0
   fi
   
   writeOutput "$(printf "%-25s" "${1}") WARNING: ${3} (${2})"
   
   return ${2}
} # handleWarning

writeOutput () {
   writeLog "${@}"
   writeScreen "${@}"
} # writeOutput

writeLog () {
   local logMessage="${1}"
   
   [ "$(toLower "${2}")" = "nodatetimetag" ] || \
   logMessage="$(dateTimeTag)${logMessage}"
   
   echo "${logMessage}" >> "${logFile}"
} # writeLog

writeScreen () {
   [ "$(toLower "${sftpScriptRunQuiet}")" = "no" ] && echo "${1}" 1>&2
} # writeScreen

writeSep () {
   local separator=""
   
   if [ "$(toLower "${1}")" = "light" ]
   then 
      separator="--------------------------------------------------------------------------------"
   else
      separator="################################################################################"
   fi
   writeLog "${separator}" "noDateTimeTag"
} # writeSep

################################################################################
## SFTP SCRIPT STARTUP / INITIALIZATION FUNCTIONS                             ##
################################################################################

sftpScriptActionStart () {
   local sftpScriptProcessId="$(sftpScriptProcessId)"
   
   local osReturnCode=0
   
   [ -z "${sftpScriptProcessId}" ] || exitError "Process ${thisScriptFile} already started with PID ${sftpScriptProcessId}." 101
   
   sftpTransfer
   osReturnCode=${?}
   if [ ${osReturnCode} -ne 0 ]
   then
      exitError "${thisScriptFile} ${thisScriptOptions} has not worked. An error occurred when calling function sftpTransfer." ${osReturnCode}
   fi
   
   exit 0
} # sftpScriptActionStart

sftpScriptActionStatus () {
   local sftpScriptProcessId="$(sftpScriptProcessId)"
   
   writeScreen "${thisScriptFile} is \c"
   if [ -z "${sftpScriptProcessId}" ]
   then
      writeScreen "not running." 
   else
      writeScreen "running with PID ${sftpScriptProcessId}." 
   fi
   
   exit 0
} # sftpScriptActionStatus

sftpScriptInitialize () {
   local dateTime=""
   local parameterCount=0
   
   sftpScriptInitialized="no"
   
   # THIS SCRIPT VARIABLES
   thisScriptOptions="${@}"
   thisScriptFileFull="${0}"
   thisScriptFile="$(basename "${thisScriptFileFull}")"
   thisScriptPath="$(dirname "${thisScriptFileFull}")"
   thisScriptProcessId=${$}
   
   [ "${thisScriptFile}" = "sftp.inc" ] && exitError "Include ${thisScriptFile} cannot be executed." 100

   bitBucket=/dev/null
   logFile=""
   logFileDebug=""
   
   sftpScriptAction=""
   
   sftpScriptRunQuiet="no"
   sftpScriptDebug="no"
   
   while [ ${#} -ne 0 ]
   do
      parameterCount=$((${parameterCount}+1))
      
      
      if [ ${parameterCount} -eq 1 ]
      then
         case "${1}" in
            "start"|"status")
               sftpScriptAction="$(toLower "${1}")"
            ;;
            
            "help"|"-help"|"-h"|"/h"|"-?"|"/?")
               exitHelp
            ;;
         
            *)
               exitError "${thisScriptFile} ${thisScriptOptions} has not worked; Action ${1} is invalid." 101
            ;;
         esac
      else
         case "${sftpScriptAction}" in
            "start")
                  
               case "${1}" in
                  "-quiet")
                     sftpScriptRunQuiet="yes"
                  ;;
                  
                  "-debug")
                     sftpScriptDebug="yes"            
                  ;;
                  
                  *)
                     exitError "${thisScriptFile} ${thisScriptOptions} has not worked; Option "${1}" is invalid for action ${sftpScriptAction} ." 102
                  ;;
               esac
            ;;
            *)
               exitError "${thisScriptFile} ${thisScriptOptions} has not worked; Option "${1}" is invalid for action ${sftpScriptAction} ." 103
            ;;
         esac
      fi
      
      [ ${#} -ne 0 ] && shift
   done

   # SFTP LOG FILES
   #dateTime="$(date "+%Y%m%d%H%M%S")"
   dateTime="$(date "+%Y%m%d")"
   logFile="${logPath}/${thisScriptFile}.${dateTime}.log"
   logFileDebug="${logPath}/${thisScriptFile}.${dateTime}.debug" 
   
   case "${sftpScriptAction}" in
      "start")
         : >> "${logFile}"
         [ "${sftpScriptDebug}" = "yes" ] && : >> "${logFileDebug}"
      ;;
   esac
   
   # SFTP SERVER CONNECTION VARIABLES
   sftpStatus=""
   sftpStatusRemoteHost=""
   sftpStatusRemoteUser=""
   sftpPrivateKey=""
   sftpCommandOptions=""

   # SFTP TRANSFER OPTION VARIABLES
   sftpSetOptions
   
   #RDU TODO
   #sftpOptionActionPreTransfer="none"  # none, ux2dos, dos2ux
   #sftpOptionActionPostTransfer="none" # none, ux2dos, dos2ux

   sftpScriptInitialized="yes"
} # sftpScriptInitialize

sftpScriptProcessId () {
   local sftpScriptProcessId=""
   
   while true
   do
      sftpScriptProcessId="$(ps -xfe | awk -v thisScriptProcessId=${thisScriptProcessId} -v thisScriptFile="${thisScriptFile}" '($2!=thisScriptProcessId) && ($3!=thisScriptProcessId) && ($9~"^(.*[/]){0,1}"thisScriptFile"$") && ($10=="start") {print $2; exit}')"
      
      # No process found; leave
      [ -z "${sftpScriptProcessId}" ] && break
      
      # Process exists; leave
      [ "$(ps -xfp ${sftpScriptProcessId}| awk -v sftpScriptProcessId=${sftpScriptProcessId} -v thisScriptFile="${thisScriptFile}" '($2==sftpScriptProcessId) && ($9~"^(.*[/]){0,1}"thisScriptFile"$") && ($10=="start") {print $2; exit}')" = "${sftpScriptProcessId}" ] && \
      break
   done
   
   echo ${sftpScriptProcessId}
} # sftpScriptProcessId

################################################################################
## SFTP GENERAL FUNCTIONS                                                     ##
################################################################################

sftpSetOptions () {
   local thisFunction="sftpSetOptions"
   
   sftpOptionOverwrite="$(toLower "${sftpOptionOverwrite}")"
   case "${sftpOptionOverwrite}" in
      "yes"|"no")
         # OK
      ;;
      *)
         sftpOptionOverwrite="no"
      ;;
   esac
   
   sftpOptionStagedTransfer="$(toLower "${sftpOptionStagedTransfer}")"
   case "${sftpOptionStagedTransfer}" in
      "yes"|"no")
         # OK
      ;;
      *)
         sftpOptionStagedTransfer="no"
      ;;
   esac
   
   sftpOptionActionError="$(toLower "${sftpOptionActionError}")"
   case "${sftpOptionActionError}" in
      "none"|"move"|"delete")
         # OK
      ;;
      *)
         sftpOptionActionError="none"
      ;;
   esac
   
   sftpOptionActionArchive="$(toLower "${sftpOptionActionArchive}")"
   case "${sftpOptionActionArchive}" in
      "none"|"move"|"archive")
         # OK
      ;;
      *)
         sftpOptionActionArchive="none"
      ;;
   esac
} # sftpSetOptions

sftpSetConnection () {
   local thisFunction="sftpSetConnection"
   
   local sftpReturnCode=0
   local sftpOutput=""
   
   local sftpCommandOptionsFixed="-oPubkeyAuthentication=yes -oPasswordAuthentication=no -oKbdInteractiveAuthentication=no -oStrictHostKeyChecking=no"

   [ ! "${sftpScriptInitialized}" = "yes" ] && exitError "${thisScriptFile} ${thisScriptOptions} has not worked; Script not initialized." 199
   
   [ ${#} -ge 1 ] && sftpRemoteHost="${1}"
   [ ${#} -ge 2 ] && sftpRemoteUser="${2}"
   [ ${#} -ge 3 ] && sftpPrivateKey="${3}"
   
   sftpRemoteHost="$(trim "${sftpRemoteHost}")"
   sftpRemoteUser="$(trim "${sftpRemoteUser}")"
   sftpPrivateKey="$(trim "${sftpPrivateKey}")"
   sftpCommandOptions="${sftpCommandOptionsFixed}"
   
   case "${sftpStatus}" in
      "0"|"1")
         # OK
      ;;
      *)
         sftpStatus=""
      ;;
   esac
   
   # sftpSetConnection calls with Host & User parameters force a fresh check
   # by cleaning cache
   if [ ${#} -ne 0 ] || [ -z "${sftpStatus}" ]
   then
      sftpStatus=0
      sftpStatusRemoteHost=""
      sftpStatusRemoteUser=""
      sftpStatusPrivateKey=""
      sftpStatusCommandOptions=""
   fi
   
   if [ -z "${sftpRemoteHost}" ]
   then
      handleError "${thisFunction}" 100 "No remote host specified."
      return ${?}
   fi
   
   # If remote user was not set, set to local user
   [ -z "${sftpRemoteUser}" ] && sftpRemoteUser="$(whoami)"
   
   if [ ! -z "${sftpPrivateKey}" ]
   then
      case "$(sftpLocalFileExists "${sftpPrivateKey}" "readable")" in # Returns 0, 1, 2 or 9
         "0") # File does not exist
            handleError "${thisFunction}" 101 "Private key ${sftpPrivateKey} does not exist."
            return ${?}
         ;;
         "1") # File exists
            # OK
         ;;
         "2") # File exists but has incorrect permissions
            handleError "${thisFunction}" 102 "Private key ${sftpPrivateKey} is not readable."
            return ${?}
         ;;
         "9") # Handled error
            handleError "${thisFunction}" 103 "Private key ${sftpPrivateKey} could not be checked due to an error."
            return ${?}
         ;;
         *) # Unknown error
            handleError "${thisFunction}" 104 "Private key ${sftpPrivateKey} could not be checked due to an unknown error."
            return ${?}
         ;;
      esac
   
      sftpCommandOptions="${sftpCommandOptions} -oIdentityFile=${sftpPrivateKey}"
      sftpCommandOptions="$(trim "${sftpCommandOptions}")"
   fi
   
   sftpSetOptions
   
   # If connection parameters did not change from the last check, we're done
   if [ "${sftpStatusRemoteHost}"     = "${sftpRemoteHost}"     ] && \
      [ "${sftpStatusRemoteUser}"     = "${sftpRemoteUser}"     ] && \
      [ "${sftpStatusCommandOptions}" = "${sftpCommandOptions}" ] && \
      [ ! -z "${sftpStatus}" ]
   then
      return 0
   fi
   
   sftpStatus=0
   sftpStatusRemoteHost="${sftpRemoteHost}"
   sftpStatusRemoteUser="${sftpRemoteUser}"
   sftpStatusPrivateKey="${sftpPrivateKey}"
   sftpStatusCommandOptions="${sftpCommandOptions}"
   
   writeSep
   handleInfo "${thisFunction}" "SFTP STATUS:"
   handleInfo "${thisFunction}" "Host=${sftpRemoteHost}"
   handleInfo "${thisFunction}" "User=${sftpRemoteUser}"
   [ -z "${sftpPrivateKey}" ] || handleInfo "${thisFunction}" "Private Key=${sftpPrivateKey}"
   handleInfo "${thisFunction}" "Options=${sftpCommandOptions}"
   
   sftpOutput="#${thisFunction}\n$(sftp ${sftpCommandOptions} ${sftpRemoteUser}@${sftpRemoteHost} -b <<EOF 2>&1
      bye
      EOF)"
   sftpReturnCode=${?}

   [ "$(toLower "${sftpScriptDebug}")" = "yes" ] && echo "${sftpOutput}" >> "${logFileDebug}"
   
   if [ ${sftpReturnCode} -ne 0 ]
   then
      handleInfo "${thisFunction}" "Status=DOWN"
      
      handleError "${thisFunction}" ${sftpReturnCode} "An sftp error occurred."
      return ${?}
   fi
   
   handleInfo "${thisFunction}" "Status=UP"
   sftpStatus=1
   
   return 0
} # sftpSetConnection

################################################################################

sftpStatus () {
   local thisFunction="sftpStatus"
   
   local sftpReturnCode=0
   
   sftpSetConnection "${@}"
   sftpReturnCode=${?}
   
   echo ${sftpStatus}
   return ${sftpReturnCode}
} # sftpStatus

################################################################################
## SFTP LOCAL FILE FUNCTIONS                                                  ##
################################################################################

sftpLocalDirectoryExists () {
   local thisFunction="sftpLocalDirectoryExists"
   # Displays
   # 0: Directory does not exist
   # 1: Directory exists
   # 2: Directory exists but has incorrect permissions
   # 9: Directory existence cannot be checked due to an error 

   local sftpLocalDirectory="${1}"
   local sftpLocalDirectoryPermission="$(toLower "${2}")"
   
   if [ -z "${sftpLocalDirectory}" ]
   then
      echo 9
      handleError "${thisFunction}" 100 "No local directory specified."
      return ${?}
   fi
   
   sftpLocalDirectory="$(dirname "${sftpLocalDirectory}/basename_dummy")"
   
   if [ ! -d "${sftpLocalDirectory}" ]
   then
      echo 0
      return 0
   fi

   # Directory exists; check permissions
   case "${sftpLocalDirectoryPermission}" in
      "r|readable")
         if [ -r "${sftpLocalDirectory}" ]
         then
            echo 1
         else
            echo 2
         fi
      ;;   
      "w|writeable")
         if [ -w "${sftpLocalDirectory}" ]
         then
            echo 1
         else
            echo 2
         fi
      ;;
      *)
         echo 1
      ;;
   esac
   
   return 0
} # sftpLocalDirectoryExists

################################################################################

sftpLocalFileDelete () {
   local thisFunction="sftpLocalFileDelete"
   # Displays
   # 0: Delete failure
   # 1: Delete success
   # 2: File does not exist
   # 9: Delete failure; Delete cannot be executed/validated due to an error
   
   local osReturnCode=0
   
   local sftpLocalFile="${1}"
   
   if [ -z "${sftpLocalFile}" ]
   then
      echo 9
      handleError "${thisFunction}" 100 "No local file specified."
      return ${?}
   fi
   
   handleInfo "${thisFunction}" "Delete ${sftpLocalFile}"
   
   # Pre-check
   case "$(sftpLocalFileExists "${sftpLocalFile}")" in # Returns 0, 1, 2 or 9
      "0") # File does not exist
         echo 2
         handleError "${thisFunction}" 101 "Local file ${sftpLocalFile} does not exist."
         return ${?}
      ;;
      "1") # File exists
         # OK
      ;;
      #"2") # File exists but has incorrect permissions
      #   echo 0
      #   handleError "${thisFunction}" 102 "Local file ${sftpLocalFile} has incorrect permissions."
      #   return ${?}
      #;;
      "9") # Handled error
         echo 9
         handleError "${thisFunction}" 103 "Local file ${sftpLocalFile} could not be checked due to an error."
         return ${?}
      ;;
      *) # Unknown error
         echo 9
         handleError "${thisFunction}" 104 "Local file ${sftpLocalFile} could not be checked due to an unknown error."
         return ${?}
      ;;
   esac
   
   rm -f "${sftpLocalFile}" > ${bitBucket} 2>&1
   osReturnCode=${?}
   
   #if [ ${osReturnCode} -ne 0 ]
   #then
   #   echo 0
   #   handleError "${thisFunction}" ${osReturnCode} "An OS error occurred."
   #   return ${?}
   #fi
   
   # Post-check
   case "$(sftpLocalFileExists "${sftpLocalFile}")" in # Returns 0, 1, 2 or 9
      "0") # File does not exist
         # OK
      ;;
      "1") # File exists
         if [ ${osReturnCode} -ne 0 ]
         then
            echo 0
            handleError "${thisFunction}" ${osReturnCode} "Local file ${sftpLocalFile} could not be deleted."
            return ${?}
         fi
         
         echo 9
         handleError "${thisFunction}" 105 "Local file ${sftpLocalFile} could not be deleted."
         return ${?}
      ;;
      #"2") # File exists but has incorrect permissions
      #   echo 0
      #   handleError "${thisFunction}" 106 "Local file ${sftpLocalFile} has incorrect permissions."
      #   return ${?}
      #;;
      "9") # Handled error
         echo 9
         handleError "${thisFunction}" 108 "Local file ${sftpLocalFile} could not be checked due to an error."
         return ${?}
      ;;
      *) # Unknown error
         echo 9
         handleError "${thisFunction}" 109 "Local file ${sftpLocalFile} could not be checked due to an unknown error."
         return ${?}
      ;;
   esac
   
   echo 1
   return 0
} # sftpLocalFileDelete

################################################################################

sftpLocalFileExists () {
   local thisFunction="sftpLocalFileExists"
   # Displays
   # 0: File does not exist
   # 1: File exists
   # 2: File exists but has incorrect permissions
   # 9: File existence cannot be checked due to an error 

   local sftpLocalFile="${1}"
   local sftpLocalFilePermission="$(toLower "${2}")"
   
   if [ -z "${sftpLocalFile}" ]
   then
      echo 9
      handleError "${thisFunction}" 100 "No local file specified."
      return ${?}
   fi
   
   if [ ! -f "${sftpLocalFile}" ]
   then
      echo 0
      return 0
   fi
   
   # File exists; check permissions
   case "${sftpLocalFilePermission}" in
      "r|readable")
         if [ -r "${sftpLocalFile}" ]
         then
            echo 1
         else
            echo 2
         fi
      ;;   
      "w|writeable")
         if [ -w "${sftpLocalFile}" ]
         then
            echo 1
         else
            echo 2
         fi
      ;;
      *)
         echo 1
      ;;
   esac
   
   return 0
} # sftpLocalFileExists

################################################################################

sftpLocalFileMove () {
   local thisFunction="sftpLocalFileMove"
   # Displays
   # 0: Move failure
   # 1: Move success
   # 9: Move failure; Move cannot be executed/validated due to an error
   
   local osReturnCode=0
   
   local sftpLocalFileSource="${1}"
   local sftpLocalFileTarget="${2}"
   
   if [ -z "${sftpLocalFileSource}" ]
   then
      echo 9
      handleError "${thisFunction}" 100 "No local source file specified."
      return ${?}
   fi
   
   if [ -z "${sftpLocalFileTarget}" ]
   then
      echo 9
      handleError "${thisFunction}" 101 "No local target file specified."
      return ${?}
   fi
   
   handleInfo "${thisFunction}" "Move ${sftpLocalFileSource} to ${sftpLocalFileTarget}"
   
   # Pre-check on Source
   case "$(sftpLocalFileExists "${sftpLocalFileSource}")" in # Returns 0, 1, 2 or 9
      "0") # File does not exist
         echo 0
         handleError "${thisFunction}" 102 "Local file ${sftpLocalFileSource} does not exist."
         return ${?}
      ;;
      "1") # File exists
         # OK
      ;;
      #"2") # File exists but has incorrect permissions
      #   echo 0
      #   handleError "${thisFunction}" 103 "Local file ${sftpLocalFileSource} has incorrect permissions."
      #   return ${?}
      #;;
      "9") # Handled error
         echo 9
         handleError "${thisFunction}" 104 "Local file ${sftpLocalFileSource} could not be checked due to an error."
         return ${?}
      ;;
      *) # Unknown error
         echo 9
         handleError "${thisFunction}" 105 "Local file ${sftpLocalFileSource} could not be checked due to an unknown error."
         return ${?}
      ;;
   esac
   
   # Pre-check on Target
   case "$(sftpLocalFileExists "${sftpLocalFileTarget}")" in # Returns 0, 1, 2 or 9
      "0") # File does not exist
         # OK
      ;;
      "1") # File exists
         echo 0
         handleError "${thisFunction}" 106 "Local file ${sftpLocalFileTarget} already exists."
         return ${?}
      ;;
      #"2") # File exists but has incorrect permissions
      #   echo 0
      #   handleError "${thisFunction}" 107 "Local file ${sftpLocalFileTarget} has incorrect permissions."
      #   return ${?}
      #;;
      "9") # Handled error
         echo 9
         handleError "${thisFunction}" 108 "Local file ${sftpLocalFileTarget} could not be checked due to an error."
         return ${?}
      ;;
      *) # Unknown error
         echo 9
         handleError "${thisFunction}" 109 "Local file ${sftpLocalFileTarget} could not be checked due to an unknown error."
         return ${?}
      ;;
   esac
   
   mv -f "${sftpLocalFileSource}" "${sftpLocalFileTarget}" > ${bitBucket} 2>&1
   osReturnCode=${?}
   
   #if [ ${osReturnCode} -ne 0 ]
   #then
   #   echo 0
   #   handleError "${thisFunction}" ${osReturnCode} "An OS error occurred."
   #   return ${?}
   #fi
   
   # Post-check on Source
   case "$(sftpLocalFileExists "${sftpLocalFileSource}")" in # Returns 0, 1, 2 or 9
      "0") # File does not exist
         # OK
      ;;
      "1") # File exists
         if [ ${osReturnCode} -ne 0 ]
         then
            echo 0
            handleError "${thisFunction}" ${osReturnCode} "Local file ${sftpLocalFileSource} could not be moved to ${sftpLocalFileTarget}."
            return ${?}
         fi
         
         echo 9
         handleError "${thisFunction}" 110 "Local file ${sftpLocalFileSource} could not be moved to ${sftpLocalFileTarget}."
         return ${?}
      ;;
      #"2") # File exists but has incorrect permissions
      #   echo 0
      #   handleError "${thisFunction}" 111 "Local file ${sftpLocalFileSource} has incorrect permissions."
      #   return ${?}
      #;;
      "9") # Handled error
         echo 9
         handleError "${thisFunction}" 112 "Local file ${sftpLocalFileSource} could not be checked due to an error."
         return ${?}
      ;;
      *) # Unknown error
         echo 9
         handleError "${thisFunction}" 113 "Local file ${sftpLocalFileSource} could not be checked due to an unknown error."
         return ${?}
      ;;
   esac
   
   # Post-check on Target
   case "$(sftpLocalFileExists "${sftpLocalFileTarget}")" in # Returns 0, 1, 2 or 9
      "0") # File does not exist
         if [ ${osReturnCode} -ne 0 ]
         then
            echo 9
            handleError "${thisFunction}" ${osReturnCode} "Local file ${sftpLocalFileSource} could not be moved to ${sftpLocalFileTarget}."
            return ${?}
         fi
         
         echo 9
         handleError "${thisFunction}" 114 "Local file ${sftpLocalFileSource} could not be moved to ${sftpLocalFileTarget}."
         return ${?}
      ;;
      "1") # File exists
         # OK
      ;;
      #"2") # File exists but has incorrect permissions
      #   echo 0
      #   handleError "${thisFunction}" 115 "Local file ${sftpLocalFileTarget} has incorrect permissions."
      #   return ${?}
      #;;
      "9") # Handled error
         echo 9
         handleError "${thisFunction}" 116 "Local file ${sftpLocalFileTarget} could not be checked due to an error."
         return ${?}
      ;;
      *) # Unknown error
         echo 9
         handleError "${thisFunction}" 117 "Local file ${sftpLocalFileTarget} could not be checked due to an unknown error."
         return ${?}
      ;;
   esac
   
   echo 1
   return 0
} # sftpLocalFileMove

################################################################################

sftpLocalFilePut () {
   local thisFunction="sftpLocalFilePut"
   # Displays
   # 0: Transfer failure
   # 1: Transfer success
   
   local sftpReturnCode=0
   local sftpOutput=""
   
   local sftpLocalFile="${1}"
   local sftpRemoteFile="${2}"
   local sftpRemoteFileCheck=""
   local sftpRemoteFileStaging="${3}"
   
   sftpSetConnection
   if [ ${sftpStatus} -eq 0 ]
   then
      echo 0
      handleError "${thisFunction}" 199 "No sftp connection to ${sftpRemoteUser}@${sftpRemoteHost}."
      return ${?}
   fi
   
   if [ -z "${sftpLocalFile}" ]
   then
      echo 0
      handleError "${thisFunction}" 100 "No local file specified."
      return ${?}
   fi
   
   if [ -z "${sftpRemoteFile}" ]
   then
      echo 0
      handleError "${thisFunction}" 101 "No remote file specified."
      return ${?}
   fi
   
   if [ -z "${sftpRemoteFileStaging}" ]
   then
      # Even if staging is not on, transfer through staging file
      sftpRemoteFileStaging="${sftpRemoteFile}"
   fi
   
   # Pre-check on Local Source
   case "$(sftpLocalFileExists "${sftpLocalFile}" "readable")" in # Returns 0, 1, 2 or 9
      "0") # File does not exist
         echo 0
         handleError "${thisFunction}" 102 "Local file ${sftpLocalFile} does not exist."
         return ${?}
      ;;
      "1") # File exists
         # OK
      ;;
      "2") # File exists but has incorrect permissions
         echo 0
         handleError "${thisFunction}" 103 "Local file ${sftpLocalFile} is not readable."
         return ${?}
      ;;
      "9") # Handled error
         echo 0
         handleError "${thisFunction}" 104 "Local file ${sftpLocalFile} could not be checked due to an error."
         return ${?}
      ;;
      *) # Unknown error
         echo 0
         handleError "${thisFunction}" 105 "Local file ${sftpLocalFile} could not be checked due to an unknown error."
         return ${?}
      ;;
   esac
   
   if [ "${sftpOptionOverwrite}" = "yes" ]
   then
      # Delete Remote Target
      case "$(sftpRemoteFileDelete "${sftpRemoteFile}")" in # Returns 0, 1, 2 or 9
         "0") # Delete failure
            echo 0
            handleError "${thisFunction}" 106 "Check permissions for remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile}."
            return ${?}
         ;;
         "1") # Delete success
            # OK
         ;;
         "2") # File does not exist
            # OK
         ;;
         "9") # Handled or sftp error
            echo 0
            handleError "${thisFunction}" 107 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} could not be deleted due to an error."
            return ${?}
         ;;
         *) # Unknown error
            echo 0
            handleError "${thisFunction}" 108 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} could not be deleted due to an unknown error."
            return ${?}
         ;;
      esac
   else
      # Pre-check on Remote Target
      case "$(sftpRemoteFileExists "${sftpRemoteFile}")" in # Returns 0, 1 or 9
         "0") # File does not exist
            # OK
         ;;
         "1") # File exists
            echo 0
            handleError "${thisFunction}" 109 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} already exists."
            return ${?}
         ;;
         "9") # Handled or sftp error
            echo 0
            handleError "${thisFunction}" 110 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} could not be checked due to an error."
            return ${?}
         ;;
         *) # Unknown error
            echo 0
            handleError "${thisFunction}" 111 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} could not be checked due to an unknown error."
            return ${?}
         ;;
      esac
   fi
   
   # Pre-check on Remote Staging
   if [ ! "${sftpRemoteFile}" = "${sftpRemoteFileStaging}" ]
   then
      case "$(sftpRemoteFileExists "${sftpRemoteFileStaging}")" in # Returns 0, 1, 2 or 9
         "0") # File does not exist
            # OK
         ;;
         "1") # File exists
            echo 0
            handleError "${thisFunction}" 112 "Remote staging file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileStaging} already exists."
            return ${?}
         ;;
         "9") # Handled error
            echo 0
            handleError "${thisFunction}" 113 "Remote staging file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileStaging} could not be checked due to an error."
            return ${?}
         ;;
         *) # Unknown error
            echo 0
            handleError "${thisFunction}" 114 "Remote staging file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileStaging} could not be checked due to an unknown error."
            return ${?}
         ;;
      esac
   fi
   
   handleInfo "${thisFunction}" "Upload ${sftpLocalFile} to ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileStaging}"
   
   sftpOutput="#${thisFunction}\n$(sftp ${sftpCommandOptions} ${sftpRemoteUser}@${sftpRemoteHost} -b <<EOF 2>&1
      put "${sftpLocalFile}" "${sftpRemoteFileStaging}"
      ls -1tr "${sftpRemoteFileStaging}"
      bye
      EOF)"
   sftpReturnCode=${?}
   
   [ "$(toLower "${sftpScriptDebug}")" = "yes" ] && echo "${sftpOutput}" >> "${logFileDebug}"
   
   if [ ${sftpReturnCode} -ne 0 ]
   then
      echo 0
      handleError "${thisFunction}" ${sftpReturnCode} "An sftp error occurred."
      return ${?}
   fi
   
   # Last line is the filename from the ls -1tr command in case transfer was successful
   sftpRemoteFileCheck="$(echo "${sftpOutput}" | awk '($1!="sftp>"){print}' | tail -n 1)"
   
   if [ ! "${sftpRemoteFileStaging}" = "${sftpRemoteFileCheck}" ]
   then
      echo 0
      handleError "${thisFunction}" 115 "Transfer of ${sftpLocalFile} to ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileStaging} failed."
      return ${?}
   fi
   
   if [ ! "${sftpRemoteFile}" = "${sftpRemoteFileStaging}" ]
   then
      case "$(sftpRemoteFileMove "${sftpRemoteFileStaging}" "${sftpRemoteFile}")" in # Returns 0, 1 or 9
         "0")
            echo 0
            handleError "${thisFunction}" 116 "Transfer to ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} failed."
            return ${?}
         ;;
         "1")
            # OK
         ;;
         "9")
            echo 0
            handleError "${thisFunction}" 117 "Transfer to ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} failed due to an error."
            return ${?}
         ;;
         *)
            echo 0
            handleError "${thisFunction}" 118 "Transfer to ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} failed due to an unknown error."
            return ${?}
         ;;
      esac
   fi
   
   echo 1
   return 0
} # sftpLocalFilePut

################################################################################
## SFTP LOCAL FILE LIST FUNCTIONS                                             ##
################################################################################

sftpLocalFileList () {
   local thisFunction="sftpLocalFileList"
   # Displays File List
   
   local sftpLocalFilePatternList="${1}"
   local sftpLocalDirectory="$(trim "${2}")"
   
   local sftpLocalFilePattern=""
   local sftpLocalFile=""
   local sftpLocalFileList=""
   
   if [ -z "${sftpLocalFilePatternList}" ]
   then
      echo ""
      handleError "${thisFunction}" 100 "No local file pattern specified."
      return ${?}
   fi
   
   if [ -z "${sftpLocalDirectory}" ]
   then
      echo ""
      handleError "${thisFunction}" 101 "No local directory specified."
      return ${?}
   fi
   
   sftpLocalDirectory="$(dirname "${sftpLocalDirectory}/basename_dummy")"
   
   case "$(sftpLocalDirectoryExists "${sftpLocalDirectory}" "readable")" in # Returns 0, 1, 2 or 9
      "0") # Directory does not exist
         echo ""
         handleError "${thisFunction}" 102 "Local directory ${sftpLocalDirectory} does not exist."
         return ${?}
      ;;
      "1") # Directory exists
         # OK
      ;;
      "2") # Directory exists but has incorrect permissions
         echo ""
         handleError "${thisFunction}" 103 "Local directory ${sftpLocalDirectory} is not readable."
         return ${?}
      ;;
      "9") # Handled error
         echo ""
         handleError "${thisFunction}" 104 "Local directory ${sftpLocalDirectory} could not be checked due to an error."
         return ${?}
      ;;
      *) # Unknown error
         echo ""
         handleError "${thisFunction}" 105 "Local directory ${sftpLocalDirectory} could not be checked due to an unknown error."
         return ${?}
      ;;
   esac

   sftpLocalFilePatternList="$(echo "${sftpLocalFilePatternList}" | awk '{gsub("\*","@@64;",$0); gsub("\?","@@63;",$0); print}')"
   
   for sftpLocalFilePattern in ${sftpLocalFilePatternList}
   do
      sftpLocalFilePattern="$(echo "${sftpLocalFilePattern}" | awk '{gsub("@@64;","*",$0); gsub("@@63;","?",$0); print}')"
      
      for sftpLocalFile in ${sftpLocalDirectory}/${sftpLocalFilePattern}
      do
         [ -f "${sftpLocalFile}" ] && sftpLocalFileList="${sftpLocalFileList}\n${sftpLocalFile}"
      done
   done
   
   sftpLocalFileList="$(echo "${sftpLocalFileList}" | awk -v lineMatch="^${sftpLocalDirectory}/" '($0~lineMatch){print}')"
   
   echo "${sftpLocalFileList}"
   return 0
} # sftpLocalFileList

################################################################################

sftpLocalFileListCount () {
   local thisFunction="sftpLocalFileListCount"
   # Displays File List Count
   
   local sftpLocalFilePatternList="${1}"
   local sftpLocalDirectory="${2}"
   
   echo $(lineCount "$(sftpLocalFileList "${sftpLocalFilePatternList}" "${sftpLocalDirectory}")")
   return ${?}
} # sftpLocalFileListCount

################################################################################

sftpLocalFileListPut () {
   local thisFunction="sftpLocalFileListPut"
   # Displays
   # 0: Transfer failure
   # 1: Transfer success
   
   local sftpReturnCode=0
   
   local sftpLocalFilePatternList="${1}"
   local sftpLocalDirectory="$(trim "${2}")"
   local sftpRemoteDirectory="$(trim "${3}")"
   local sftpRemoteDirectoryStaging="$(trim "${4}")"
   local sftpLocalDirectoryError="$(trim "${5}")"
   local sftpLocalDirectoryArchive="$(trim "${6}")"
   
   local sftpLocalFileList=""
   local sftpLocalFileListCount=0
   local sftpLocalFileCount=0
   local sftpLocalFileCountOK=0
   local sftpLocalFileCountKO=0
   local sftpLocalFileCountSkip=0
   local sftpLocalFileCountWarning=0
   
   local sftpLocalFile=""
   local sftpRemoteFile=""
   local sftpRemoteFileStaging=""
   local sftpLocalFileError=""
   local sftpLocalFileArchive=""
   
   sftpSetConnection
   
   writeSep "light"
   
   if [ ${sftpStatus} -eq 0 ]
   then
      echo 0
      handleError "${thisFunction}" 199 "No sftp connection to ${sftpRemoteUser}@${sftpRemoteHost}."
      return ${?}
   fi
   
   if [ -z "${sftpLocalFilePatternList}" ]
   then
      echo 0
      handleError "${thisFunction}" 100 "No local file pattern specified."
      return ${?}
   fi
   
   if [ -z "${sftpLocalDirectory}" ]
   then
      echo 0
      handleError "${thisFunction}" 101 "No local directory specified."
      return ${?}
   fi

   if [ -z "${sftpRemoteDirectory}" ]
   then
      echo 0
      handleError "${thisFunction}" 102 "No remote directory specified."
      return ${?}
   fi
   
   sftpLocalDirectory="$(dirname "${sftpLocalDirectory}/basename_dummy")"
   sftpRemoteDirectory="$(dirname "${sftpRemoteDirectory}/basename_dummy")"
   
   if [ "${sftpOptionStagedTransfer}" = "yes" ]
   then
      if [ -z "${sftpRemoteDirectoryStaging}" ]
      then
         sftpRemoteDirectoryStaging="${sftpRemoteDirectory}"
      else
         sftpRemoteDirectoryStaging="$(dirname "${sftpRemoteDirectoryStaging}/basename_dummy")"
      fi
   else
      sftpRemoteDirectoryStaging=""
   fi
   
   case "${sftpOptionActionError}" in
      "move")
         if [ -z "${sftpLocalDirectoryError}" ]
         then
            echo 0
            handleError "${thisFunction}" 103 "No local error directory specified."
            return ${?}
         fi
         
         if [ "${sftpLocalDirectory}" = "${sftpLocalDirectoryError}" ]
         then
            echo 0
            handleError "${thisFunction}" 104 "Local error directory equals local directory ${sftpLocalDirectory}."
            return ${?}
         fi
         
         sftpLocalDirectoryError="$(dirname "${sftpLocalDirectoryError}/basename_dummy")"
      ;;
      "none"|"delete"|*)
         sftpLocalDirectoryError=""
      ;;
   esac
   
   case "${sftpOptionActionSuccess}" in
      "archive")
         if [ -z "${sftpLocalDirectoryArchive}" ]
         then
            echo 0
            handleError "${thisFunction}" 105 "No local archive directory specified."
            return ${?}
         fi
         
         if [ "${sftpLocalDirectory}" = "${sftpLocalDirectoryArchive}" ]
         then
            echo 0
            handleError "${thisFunction}" 106 "Local archive directory equals local directory ${sftpLocalDirectory}."
            return ${?}
         fi
         
         sftpLocalDirectoryArchive="$(dirname "${sftpLocalDirectoryArchive}/basename_dummy")"
      ;;
      "none"|"delete"|*)
         sftpLocalDirectoryArchive=""
      ;;
   esac
   
   handleInfo "${thisFunction}" "SFTP TRANSFER SETTINGS:"
   handleInfo "${thisFunction}" "Local Source Directory=${sftpLocalDirectory}"
   handleInfo "${thisFunction}" "File Pattern=${sftpLocalFilePatternList}"
   handleInfo "${thisFunction}" "Remote Target Directory=${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteDirectory}"
   handleInfo "${thisFunction}" "Overwrite Target Files=${sftpOptionOverwrite}"
   
   handleInfo "${thisFunction}" "Staged Transfer=${sftpOptionStagedTransfer}"
   [ -z "${sftpRemoteDirectoryStaging}" ] || \
      handleInfo "${thisFunction}" "Remote Staging Directory=${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteDirectoryStaging}"
   
   [ "${sftpOptionActionError}" = "none" ] || \
   handleInfo "${thisFunction}" "On error=${sftpOptionActionError} file"
   [ -z "${sftpLocalDirectoryError}" ] || \
      handleInfo "${thisFunction}" "Local Error Directory=${sftpLocalDirectoryError}"
   
   [ "${sftpOptionActionSuccess}" = "none" ] || \
   handleInfo "${thisFunction}" "On success=${sftpOptionActionSuccess} file"
   [ -z "${sftpLocalDirectoryArchive}" ] || \
      handleInfo "${thisFunction}" "Local Archive Directory=${sftpLocalDirectoryArchive}"
   
   writeSep "light"
   
   sftpLocalFileList="$(sftpLocalFileList "${sftpLocalFilePatternList}" "${sftpLocalDirectory}")"
   sftpReturnCode=${?}
   
   if [ ${sftpReturnCode} -ne 0 ]
   then
      echo 0
      handleError "${thisFunction}" 107 "Local file list could not be retrieved due to an error."
      return ${?}
   fi
   
   sftpLocalFileListCount=$(lineCount "${sftpLocalFileList}")
   
   # Pre-check on Remote Target
   case "$(sftpRemoteDirectoryExists "${sftpRemoteDirectory}")" in # Returns 0, 1 or 9
      "0") # Directory does not exist
         echo 0
         handleError "${thisFunction}" 108 "Remote directory ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteDirectory} does not exist."
         return ${?}
      ;;
      "1") # Directory exists
         # OK
      ;;
      "9") # Handled or sftp error
         echo 0
         handleError "${thisFunction}" 109 "Remote directory ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteDirectory} could not be checked due to an error."
         return ${?}
      ;;
      *) # Unknown error
         echo 0
         handleError "${thisFunction}" 110 "Remote directory ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteDirectory} could not be checked due to an unknown error."
         return ${?}
      ;;
   esac
   
   # Pre-check on Remote Staging Directory
   if [ ! -z "${sftpRemoteDirectoryStaging}" ] && [ ! "${sftpRemoteDirectoryStaging}" = "${sftpRemoteDirectory}" ]
   then
      case "$(sftpRemoteDirectoryExists "${sftpRemoteDirectoryStaging}")" in # Returns 0, 1 or 9
         "0") # Directory does not exist
            echo 0
            handleError "${thisFunction}" 111 "Remote staging directory ${sftpRemoteDirectoryStaging} does not exist."
            return ${?}
         ;;
         "1") # Directory exists
            # OK
         ;;
         "9") # Handled or sftp error
            echo 0
            handleError "${thisFunction}" 112 "Remote staging directory ${sftpRemoteDirectoryStaging} could not be checked due to an error."
            return ${?}
         ;;
         *) # Unknown error
            echo 0
            handleError "${thisFunction}" 113 "Remote staging directory ${sftpRemoteDirectoryStaging} could not be checked due to an unknown error."
            return ${?}
         ;;
      esac
   fi
   
   # Pre-check on Local Error Directory
   if [ ! -z "${sftpLocalDirectoryError}" ]
   then
      case "$(sftpLocalDirectoryExists "${sftpLocalDirectoryError}" "writeable")" in # Returns 0, 1, 2 or 9
         "0") # Directory does not exist
            echo 0
            handleError "${thisFunction}" 114 "Local error directory ${sftpLocalDirectoryError} does not exist."
            return ${?}
         ;;
         "1") # Directory exists
            # OK
         ;;
         "2") # Directory exists but has incorrect permissions
            echo 0
            handleError "${thisFunction}" 115 "Local error directory ${sftpLocalDirectoryError} is not writeable."
            return ${?}
         ;;
         "9") # Handled error
            echo 0
            handleError "${thisFunction}" 116 "Local error directory ${sftpLocalDirectoryError} could not be checked due to an error."
            return ${?}
         ;;
         *) # Unknown error
            echo 0
            handleError "${thisFunction}" 117 "Local error directory ${sftpLocalDirectoryError} could not be checked due to an unknown error."
            return ${?}
         ;;
      esac
   fi
   
   # Pre-check on Local Archive Directory
   if [ ! -z "${sftpLocalDirectoryArchive}" ]
   then
      case "$(sftpLocalDirectoryExists "${sftpLocalDirectoryArchive}" "writeable")" in # Returns 0, 1 or 9
         "0") # Directory does not exist
            echo 0
            handleError "${thisFunction}" 118 "Local archive directory ${sftpLocalDirectoryArchive} does not exist."
            return ${?}
         ;;
         "1") # Directory exists
            # OK
         ;;
         "2") # Directory exists but has incorrect permissions
            echo 0
            handleError "${thisFunction}" 119 "Local archive directory ${sftpLocalDirectoryArchive} is not writeable."
            return ${?}
         ;;
         "9") # Handled or sftp error
            echo 0
            handleError "${thisFunction}" 120 "Local archive directory ${sftpLocalDirectoryArchive} could not be checked due to an error."
            return ${?}
         ;;
         *) # Unknown error
            echo 0
            handleError "${thisFunction}" 121 "Local archive directory ${sftpLocalDirectoryArchive} could not be checked due to an unknown error."
            return ${?}
         ;;
      esac
   fi
   
   if [ ${sftpLocalFileListCount} -eq 0 ]
   then
      echo 1
      handleInfo "${thisFunction}" "No files found to process."
      return ${?}
   fi
   
   sftpLocalFileCount=0
   sftpLocalFileCountOK=0
   sftpLocalFileCountKO=0
   sftpLocalFileCountSkip=0
   
   for sftpLocalFile in ${sftpLocalFileList}
   do
      (( sftpLocalFileCount+=1 ))
      
      handleInfo "${thisFunction}" "Processing file ${sftpLocalFileCount} of ${sftpLocalFileListCount}: ${sftpLocalFile}"
      
      sftpRemoteFile="${sftpRemoteDirectory}/$(basename "${sftpLocalFile}")"
      sftpRemoteFileStaging=""
      [ "${sftpOptionStagedTransfer}" = "yes" ] && \
         sftpRemoteFileStaging="${sftpRemoteDirectoryStaging}/$(basename "${sftpLocalFile}").${thisScriptProcessId}.sftptmp"
      
      if [ ! "$(sftpLocalFilePut "${sftpLocalFile}" "${sftpRemoteFile}" "${sftpRemoteFileStaging}")" = "1" ]
      then
         (( sftpLocalFileCountKO+=1 ))
         
         case "${sftpOptionActionError}" in
            "move")
               sftpLocalFileError="${sftpLocalDirectoryError}/$(basename "${sftpLocalFile}")"
               case "$(sftpLocalFileMove "${sftpLocalFile}" "${sftpLocalFileError}")" in # Returns 0, 1 or 9
                  "0") # Move failure
                     handleError "${thisFunction}" 122 "Check permissions for local file ${sftpLocalFile} and local error directory ${sftpLocalDirectoryError}."
                  ;;
                  "1") # Move success
                     # OK
                  ;;
                  "9") # Handled error
                     handleError "${thisFunction}" 123 "Local file ${sftpLocalFile} could not be moved to local error directory ${sftpLocalDirectoryError} due to an error."
                  ;;
                  *) # Unknown error
                     handleError "${thisFunction}" 124 "Local file ${sftpLocalFile} could not be moved to local error directory ${sftpLocalDirectoryError} due to an unknown error."
                  ;;
               esac
            ;;
            "delete")
               case "$(sftpLocalFileDelete "${sftpLocalFile}")" in # Returns 0, 1, 2 or 9
                  "0") # Delete failure
                     handleError "${thisFunction}" 125 "Check permissions for local file ${sftpLocalFile}."
                  ;;
                  "1") # Delete success
                     # OK
                  ;;
                  "2") # File does not exist
                     # OK
                  ;;
                  "9") # Handled error
                     handleError "${thisFunction}" 126 "Local file ${sftpLocalFile} could not be deleted due to an error."
                  ;;
                  *) # Unknown error
                     handleError "${thisFunction}" 127 "Local file ${sftpLocalFile} could not be deleted due to an unknown error."
                  ;;
               esac
            ;;
         esac
         
         continue
      fi
      
      (( sftpLocalFileCountOK+=1 ))
      
      case "${sftpOptionActionSuccess}" in
         "archive")
            sftpLocalFileArchive="${sftpLocalDirectoryArchive}/$(basename "${sftpLocalFile}")"
            
            case "$(sftpLocalFileMove "${sftpLocalFile}" "${sftpLocalFileArchive}")" in # Returns 0, 1 or 9
               "0") # Move failure
                  handleError "${thisFunction}" 128 "Check permissions for local file ${sftpLocalFile} and local archive directory ${sftpLocalDirectoryArchive}."
               ;;
               "1") # Move success
                  # OK
               ;;
               "9") # Handled error
                  handleError "${thisFunction}" 129 "Local file ${sftpLocalFile} could not be moved to local archive directory ${sftpLocalDirectoryArchive} due to an error."
               ;;
               *) # Unknown error
                  handleError "${thisFunction}" 130 "Local file ${sftpLocalFile} could not be moved to local archive directory ${sftpLocalDirectoryArchive} due to an unknown error."
               ;;
            esac
         ;;
         "delete")
            case "$(sftpLocalFileDelete "${sftpLocalFile}")" in # Returns 0, 1, 2 or 9
               "0") # Delete failure
                  handleError "${thisFunction}" 131 "Check permissions for local file ${sftpLocalFile}."
               ;;
               "1") # Delete success
                  # OK
               ;;
               "2") # File does not exist
                  # OK
               ;;
               "9") # Handled or sftp error
                  handleError "${thisFunction}" 132 "Local file ${sftpLocalFile} could not be deleted due to an error."
               ;;
               *) # Unknown error
                  handleError "${thisFunction}" 133 "Local file ${sftpLocalFile} could not be deleted due to an unknown error."
               ;;
            esac
         ;;
      esac
   done
   
   writeSep "light"
   
   handleInfo "${thisFunction}" "SFTP TRANSFER SUMMARY:"
   #handleInfo "${thisFunction}" "Total=${sftpLocalFileListCount}"
   #handleInfo "${thisFunction}" "OK=${sftpLocalFileCountOK}"
   #handleInfo "${thisFunction}" "Failed=${sftpLocalFileCountKO}"
   #handleInfo "${thisFunction}" "Skipped=${sftpLocalFileCountSkip}"
   handleInfo "${thisFunction}" "Total=${sftpLocalFileListCount}, OK=${sftpLocalFileCountOK}, Failed=${sftpLocalFileCountKO}, Skipped=${sftpLocalFileCountSkip}"
   
   if [ ${sftpLocalFileCountKO} -eq 0 ]
   then
      echo 1
   else
      echo 0
   fi
   
   return 0
} # sftpLocalFileListPut

################################################################################
## SFTP REMOTE FILE FUNCTIONS                                                 ##
################################################################################

sftpRemoteDirectoryExists () {
   local thisFunction="sftpRemoteDirectoryExists"
   # Displays
   # 0: Directory does not exist
   # 1: Directory exists
   # 9: Directory existence cannot be checked due to an error 
   
   local sftpReturnCode=0
   local sftpOutput=""
   
   local sftpRemoteDirectory="${1}"
   local sftpRemoteDirectoryCheck=""
   
   sftpSetConnection
   if [ ${sftpStatus} -eq 0 ]
   then
      echo 9
      handleError "${thisFunction}" 199 "No sftp connection to ${sftpRemoteUser}@${sftpRemoteHost}."
      return ${?}
   fi
   
   if [ -z "${sftpRemoteDirectory}" ]
   then
      echo 9
      handleError "${thisFunction}" 100 "No remote directory specified."
      return ${?}
   fi
   
   sftpRemoteDirectory="$(dirname "${sftpRemoteDirectory}/basename_dummy")"
   
   sftpOutput="#${thisFunction}\n$(sftp ${sftpCommandOptions} ${sftpRemoteUser}@${sftpRemoteHost} -b <<EOF 2>&1
      cd "${sftpRemoteDirectory}"
      pwd
      bye
      EOF)"
   sftpReturnCode=${?}
   
   [ "$(toLower "${sftpScriptDebug}")" = "yes" ] && echo "${sftpOutput}" >> "${logFileDebug}"
   
   if [ ${sftpReturnCode} -ne 0 ]
   then
      echo 9
      handleError "${thisFunction}" ${sftpReturnCode} "An sftp error occurred."
      return ${?}
   fi
   
   #sftpRemoteDirectoryCheck="$(echo "${sftpOutput}" | awk '($1!="sftp>"){print}')"
   sftpRemoteDirectoryCheck="$(echo "${sftpOutput}" | awk -F":" '$1=="Remote working directory"{print $2}')"
   sftpRemoteDirectoryCheck="$(trim "${sftpRemoteDirectoryCheck}")"
   
   if [ "${sftpRemoteDirectory}" = "${sftpRemoteDirectoryCheck}" ]
   then
      echo 1
      return 0
   fi
   
   echo 0
   return 0
} # sftpRemoteDirectoryExists

################################################################################

sftpRemoteFileDelete () {
   local thisFunction="sftpRemoteFileDelete"
   # Displays
   # 0: Delete failure
   # 1: Delete success
   # 2: File does not exist
   # 9: Delete failure; Delete cannot be executed/validated due to an error
   
   local sftpReturnCode=0
   local sftpOutput=""
   
   local sftpRemoteFile="${1}"
   local sftpRemoteFileCheck=""
   
   sftpSetConnection
   if [ ${sftpStatus} -eq 0 ]
   then
      echo 9
      handleError "${thisFunction}" 199 "No sftp connection to ${sftpRemoteUser}@${sftpRemoteHost}."
      return ${?}
   fi
   
   if [ -z "${sftpRemoteFile}" ]
   then
      echo 9
      handleError "${thisFunction}" 100 "No remote file specified."
      return ${?}
   fi
   
   handleInfo "${thisFunction}" "Delete ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile}"
   
   # SKIP BELOW TO SAVE AN SFTP CALL; A NON EXISTING FILE CANNOT BE DELETED ANYWAY :-)
   # Pre-check
   #case "$(sftpRemoteFileExists "${sftpRemoteFile}")" in # Returns 0, 1 or 9
   #   "0") # File does not exist
   #      echo 0
   #      handleError "${thisFunction}" 101 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} does not exist."
   #      return ${?}
   #   ;;
   #   "1") # File exists
   #      # OK
   #   ;;
   #   "9") # Handled or sftp error
   #      echo 9
   #      handleError "${thisFunction}" 102 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} could not be checked due to an error."
   #      return ${?}
   #   ;;
   #   *) # Unknown error
   #      echo 9
   #      handleError "${thisFunction}" 103 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} could not be checked due to an unknown error."
   #      return ${?}
   #   ;;
   #esac
   
   sftpOutput="#${thisFunction}\n$(sftp ${sftpCommandOptions} ${sftpRemoteUser}@${sftpRemoteHost} -b <<EOF 2>&1
      rm "${sftpRemoteFile}"
      ls -1tr "${sftpRemoteFile}"
      bye
      EOF)"
   sftpReturnCode=${?}

   [ "$(toLower "${sftpScriptDebug}")" = "yes" ] && echo "${sftpOutput}" >> "${logFileDebug}"
   
   if [ ${sftpReturnCode} -ne 0 ]
   then
      echo 9
      handleError "${thisFunction}" ${sftpReturnCode} "An sftp error occurred."
      return ${?}
   fi
   
   # Last line is the filename from the ls -1tr command in case delete was unsuccessful
   sftpRemoteFileCheck="$(echo "${sftpOutput}" | awk '($1!="sftp>"){print}' | tail -n 1)"
   
   if [ "${sftpRemoteFile}" = "${sftpRemoteFileCheck}" ]
   then
      echo 0
      handleError "${thisFunction}" 104 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} could not be deleted."
      return ${?}
   fi
   
   echo 1
   return 0
} # sftpRemoteFileDelete

################################################################################

sftpRemoteFileExists () {
   local thisFunction="sftpRemoteFileExists"
   # Displays
   # 0: File does not exist
   # 1: File exists
   # 9: File existence cannot be checked due to an error 
   
   local sftpReturnCode=0
   local sftpOutput=""
   
   local sftpRemoteFile="${1}"
   local sftpRemoteDirectory="$(dirname "${sftpRemoteFile}")"
   local sftpRemoteFileCheck=""
   
   sftpSetConnection
   if [ ${sftpStatus} -eq 0 ]
   then
      echo 9
      handleError "${thisFunction}" 199 "No sftp connection to ${sftpRemoteUser}@${sftpRemoteHost}."
      return ${?}
   fi
   
   if [ -z "${sftpRemoteFile}" ]
   then
      echo 9
      handleError "${thisFunction}" 100 "No remote file specified."
      return ${?}
   fi
   
   sftpOutput="#${thisFunction}\n$(sftp ${sftpCommandOptions} ${sftpRemoteUser}@${sftpRemoteHost} -b <<EOF 2>&1
      ls -1tr "${sftpRemoteFile}"
      bye
      EOF)"
   sftpReturnCode=${?}
   
   [ "$(toLower "${sftpScriptDebug}")" = "yes" ] && echo "${sftpOutput}" >> "${logFileDebug}"
   
   if [ ${sftpReturnCode} -ne 0 ]
   then
      echo 9
      handleError "${thisFunction}" ${sftpReturnCode} "An sftp error occurred."
      return ${?}
   fi
   
   sftpRemoteFileCheck="$(echo "${sftpOutput}" | awk -v lineMatch="^${sftpRemoteDirectory}/" '($0~lineMatch){print}')"
   if [ $(lineCount "${sftpRemoteFileCheck}") -gt 1 ]
   then
      echo 9
      handleError "${thisFunction}" 101 "More than one remote file found for ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile}."
      return ${?}
   fi
   
   if [ "${sftpRemoteFile}" = "${sftpRemoteFileCheck}" ]
   then
      echo 1
      return 0
   fi
   
   echo 0
   return 0
} # sftpRemoteFileExists

################################################################################

sftpRemoteFileGet () {
   local thisFunction="sftpRemoteFileGet"
   # Displays
   # 0: Transfer failure
   # 1: Transfer success
   
   local sftpReturnCode=0
   local sftpOutput=""
   
   local sftpRemoteFile="${1}"
   local sftpLocalFile="${2}"
   local sftpLocalFileCheck=""
   local sftpLocalFileStaging="${3}"

   sftpSetConnection
   if [ ${sftpStatus} -eq 0 ]
   then
      echo 0
      handleError "${thisFunction}" 199 "No sftp connection to ${sftpRemoteUser}@${sftpRemoteHost}."
      return ${?}
   fi
   
   if [ -z "${sftpRemoteFile}" ]
   then
      echo 0
      handleError "${thisFunction}" 100 "No remote file specified."
      return ${?}
   fi
   
   if [ -z "${sftpLocalFile}" ]
   then
      echo 0
      handleError "${thisFunction}" 101 "No local file specified."
      return ${?}
   fi
   
   if [ -z "${sftpLocalFileStaging}" ]
   then
      # Even if staging is not on, transfer through staging file
      sftpLocalFileStaging="${sftpLocalFile}"
   fi
   
   # Pre-check on Remote Source
   case "$(sftpRemoteFileExists "${sftpRemoteFile}")" in # Returns 0, 1 or 9
      "0") # File does not exist
         echo 0
         handleError "${thisFunction}" 102 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} does not exist."
         return ${?}
      ;;
      "1") # File exists
         # OK
      ;;
      "9") # Handled or sftp error
         echo 0
         handleError "${thisFunction}" 103 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} could not be checked due to an error."
         return ${?}
      ;;
      *) # Unknown error
         echo 0
         handleError "${thisFunction}" 104 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} could not be checked due to an unknown error."
         return ${?}
      ;;
   esac
   
   # Pre-check on Local Target
   case "$(sftpLocalFileExists "${sftpLocalFile}")" in # Returns 0, 1, 2 or 9
      "0") # File does not exist
         # OK
      ;;
      "1") # File exists
         if [ ! "${sftpOptionOverwrite}" = "yes" ]
         then
            echo 0
            handleError "${thisFunction}" 105 "Local file ${sftpLocalFile} already exists."
            return ${?}
         fi
         
         case "$(sftpLocalFileDelete "${sftpLocalFile}")" in # Returns 0, 1, 2 or 9
            "0") # Delete failure
               echo 0
               handleError "${thisFunction}" 106 "Check permissions for local file ${sftpLocalFile}."
               return ${?}
            ;;
            "1") # Delete success
               # OK
            ;;
            "2") # File does not exist
               # OK
            ;;
            "9") # Handled error
               echo 0
               handleError "${thisFunction}" 107 "Local file ${sftpLocalFile} could not be deleted due to an error."
               return ${?}
            ;;
            *) # Unknown error
               echo 0
               handleError "${thisFunction}" 108 "Local file ${sftpLocalFile} could not be deleted due to an unknown error."
               return ${?}
            ;;
         esac
      ;;
      #"2") # File exists but has incorrect permissions
      #   echo 0
      #   handleError "${thisFunction}" 109 "Local file ${sftpLocalFile} has incorrect permissions."
      #   return ${?}
      #;;
      "9") # Handled error
         echo 0
         handleError "${thisFunction}" 110 "Local file ${sftpLocalFile} could not be checked due to an error."
         return ${?}
      ;;
      *) # Unknown error
         echo 0
         handleError "${thisFunction}" 111 "Local file ${sftpLocalFile} could not be checked due to an unknown error."
         return ${?}
      ;;
   esac
   
   # Pre-check on Local Staging
   if [ ! "${sftpLocalFile}" = "${sftpLocalFileStaging}" ]
   then
      case "$(sftpLocalFileExists "${sftpLocalFileStaging}")" in # Returns 0, 1, 2 or 9
         "0") # File does not exist
            # OK
         ;;
         "1") # File exists
            echo 0
            handleError "${thisFunction}" 111 "Local staging file ${sftpLocalFileStaging} already exists."
            return ${?}
         ;;
         #"2") # File exists but has incorrect permissions
         #   echo 0
         #   handleError "${thisFunction}" 112 "Local staging file ${sftpLocalFileStaging} has incorrect permissions."
         #   return ${?}
         #;;
         "9") # Handled error
            echo 0
            handleError "${thisFunction}" 113 "Local staging file ${sftpLocalFileStaging} could not be checked due to an error."
            return ${?}
         ;;
         *) # Unknown error
            echo 0
            handleError "${thisFunction}" 114 "Local staging file ${sftpLocalFileStaging} could not be checked due to an unknown error."
            return ${?}
         ;;
      esac
   fi
   
   handleInfo "${thisFunction}" "Fetch ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} to ${sftpLocalFileStaging}"
   
   sftpOutput="#${thisFunction}\n$(sftp ${sftpCommandOptions} ${sftpRemoteUser}@${sftpRemoteHost} -b <<EOF 2>&1
      get "${sftpRemoteFile}" "${sftpLocalFileStaging}"
      !ls -1tr "${sftpLocalFileStaging}"
      bye
      EOF)"
   sftpReturnCode=${?}

   [ "$(toLower "${sftpScriptDebug}")" = "yes" ] && echo "${sftpOutput}" >> "${logFileDebug}"
   
   if [ ${sftpReturnCode} -ne 0 ]
   then
      echo 0
      handleError "${thisFunction}" ${sftpReturnCode} "An sftp error occurred."
      return ${?}
   fi
   
   # Last line is the filename from the ls -1tr command in case transfer was successful
   sftpLocalFileCheck="$(echo "${sftpOutput}" | awk '($1!="sftp>"){print}' | tail -n 1)"
   
   if [ ! "${sftpLocalFileStaging}" = "${sftpLocalFileCheck}" ]
   then
      echo 0
      handleError "${thisFunction}" 115 "Transfer of ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} to ${sftpLocalFileStaging} failed."
      return ${?}
   fi
   
   if [ ! "${sftpLocalFile}" = "${sftpLocalFileStaging}" ]
   then
      case "$(sftpLocalFileMove "${sftpLocalFileStaging}" "${sftpLocalFile}")" in # Returns 0, 1 or 9
         "0")
            echo 0
            handleError "${thisFunction}" 116 "Transfer to ${sftpLocalFile} failed."
            return ${?}
         ;;
         "1")
            # OK
         ;;
         "9")
            echo 0
            handleError "${thisFunction}" 117 "Transfer to ${sftpLocalFile} failed due to an error."
            return ${?}
         ;;
         *)
            echo 0
            handleError "${thisFunction}" 118 "Transfer to ${sftpLocalFile} failed due to an unknown error."
            return ${?}
         ;;
      esac
   fi
   
   echo 1
   return 0
} # sftpRemoteFileGet

################################################################################

sftpRemoteFileMove () {
   local thisFunction="sftpRemoteFileMove"
   # Displays
   # 0: Move failure
   # 1: Move success
   # 9: Handled error
   
   local sftpReturnCode=0
   local sftpOutput=""
   
   local sftpExtendedCheckPre="no"
   local sftpExtendedCheckPost="no"
   
   local sftpRemoteFileSource="${1}"
   local sftpRemoteFileTarget="${2}"
   
   local sftpRemoteFileCheck=""
   
   sftpSetConnection
   if [ ${sftpStatus} -eq 0 ]
   then
      echo 9
      handleError "${thisFunction}" 199 "No sftp connection to ${sftpRemoteUser}@${sftpRemoteHost}."
      return ${?}
   fi
   
   if [ -z "${sftpRemoteFileSource}" ]
   then
      echo 9
      handleError "${thisFunction}" 100 "No remote source file specified."
      return ${?}
   fi
   
   if [ -z "${sftpRemoteFileTarget}" ]
   then
      echo 9
      handleError "${thisFunction}" 101 "No remote target file specified."
      return ${?}
   fi
   
   handleInfo "${thisFunction}" "Move ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileSource} to ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileTarget}"
   
   if [ "${sftpExtendedCheckPre}" = "yes" ]
   then
      # Pre-check on Source
      case "$(sftpRemoteFileExists "${sftpRemoteFileSource}")" in # Returns 0, 1 or 9
         "0") # File does not exist
            echo 0
            handleError "${thisFunction}" 102 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileSource} does not exist."
            return ${?}
         ;;
         "1") # File exists
            # OK
         ;;
         "9") # Handled or sftp error
            echo 9
            handleError "${thisFunction}" 103 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileSource} could not be checked due to an error."
            return ${?}
         ;;
         *) # Unknown error
            echo 9
            handleError "${thisFunction}" 104 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileSource} could not be checked due to an unknown error."
            return ${?}
         ;;
      esac
      
      # Pre-check on Target
      case "$(sftpRemoteFileExists "${sftpRemoteFileTarget}")" in # Returns 0, 1 or 9
         "0") # File does not exist
            # OK
         ;;
         "1") # File exists
            echo 0
            handleError "${thisFunction}" 105 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileTarget} already exists."
            return ${?}
         ;;
         "9") # Handled or sftp error
            echo 9
            handleError "${thisFunction}" 106 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileTarget} could not be checked due to an error."
            return ${?}
         ;;
         *) # Unknown error
            echo 9
            handleError "${thisFunction}" 107 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileTarget} could not be checked due to an unknown error."
            return ${?}
         ;;
      esac
   else
      # BELOW AN ALTERNATE TO CHECKS USING sftpRemoteFileExists FUNCTION AS ABOVE
      # BELOW SAVES ONE SFTP CALL AND IS JUST AS GOOD... I THINK :-)
      
      sftpOutput="#${thisFunction}\n$(sftp ${sftpCommandOptions} ${sftpRemoteUser}@${sftpRemoteHost} -b <<EOF 2>&1
         ls -1tr "${sftpRemoteFileSource}"
         ls -1tr "${sftpRemoteFileTarget}"
         bye
         EOF)"
      sftpReturnCode=${?}

      [ "$(toLower "${sftpScriptDebug}")" = "yes" ] && echo "${sftpOutput}" >> "${logFileDebug}"
      
      if [ ${sftpReturnCode} -ne 0 ]
      then
         echo 9
         handleError "${thisFunction}" ${sftpReturnCode} "An sftp error occurred."
         return ${?}
      fi
   
      # Pre-check on Source
      # The first line of last two lines is the source filename from the first ls -1tr command in case the source exists
      sftpRemoteFileCheck="$(echo "${sftpOutput}" | awk '($1!="sftp>"){print}' | tail -n 2 | head -n 1)"
      
      if [ ! "${sftpRemoteFileSource}" = "${sftpRemoteFileCheck}" ]
      then
         echo 0
         handleError "${thisFunction}" 102 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileSource} does not exist."
         return ${?}
      fi

      # Pre-check on Target
      # The last line is the target filename from the second ls -1tr command in case the target already exists
      sftpRemoteFileCheck="$(echo "${sftpOutput}" | awk '($1!="sftp>"){print}' | tail -n 1)"
      
      if [ "${sftpRemoteFileTarget}" = "${sftpRemoteFileCheck}" ]
      then
         echo 0
         handleError "${thisFunction}" 105 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileTarget} already exists."
         return ${?}
      fi
   fi
   
   # Now execute the move and list source and target after rename... only target should remain
   sftpOutput="#${thisFunction}\n$(sftp ${sftpCommandOptions} ${sftpRemoteUser}@${sftpRemoteHost} -b <<EOF 2>&1
      rename "${sftpRemoteFileSource}" "${sftpRemoteFileTarget}"
      ls -1tr "${sftpRemoteFileSource}"
      ls -1tr "${sftpRemoteFileTarget}"
      bye
      EOF)"
   sftpReturnCode=${?}

   [ "$(toLower "${sftpScriptDebug}")" = "yes" ] && echo "${sftpOutput}" >> "${logFileDebug}"
   
   if [ ${sftpReturnCode} -ne 0 ]
   then
      echo 9
      handleError "${thisFunction}" ${sftpReturnCode} "An sftp error occurred."
      return ${?}
   fi

   if [ "${sftpExtendedCheckPost}" = "yes" ]
   then
      # Post-check on Source
      case "$(sftpRemoteFileExists "${sftpRemoteFileSource}")" in # Returns 0, 1 or 9
         "0") # File does not exist
            # OK
         ;;
         "1") # File exists
            echo 0
            handleError "${thisFunction}" 108 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileSource} could not be moved to ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileTarget}."
            return ${?}
         ;;
         "9") # Handled or sftp error
            echo 9
            handleError "${thisFunction}" 109 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileSource} could not be checked due to an error."
            return ${?}
         ;;
         *) # Unknown error
            echo 9
            handleError "${thisFunction}" 110 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileSource} could not be checked due to an unknown error."
            return ${?}
         ;;
      esac
      
      # Post-check on Target
      case "$(sftpRemoteFileExists "${sftpRemoteFileTarget}")" in # Returns 0, 1 or 9
         "0") # File does not exist
            echo 9
            handleError "${thisFunction}" 111 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileSource} could not be moved to ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileTarget}."
            return ${?}
         ;;
         "1") # File exists
            # OK
         ;;
         "9") # Handled or sftp error
            echo 9
            handleError "${thisFunction}" 112 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileTarget} could not be checked due to an error."
            return ${?}
         ;;
         *) # Unknown error
            echo 9
            handleError "${thisFunction}" 113 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileTarget} could not be checked due to an unknown error."
            return ${?}
         ;;
      esac
   else
      # BELOW AN ALTERNATE TO CHECKS USING sftpRemoteFileExists FUNCTION AS ABOVE
      # BELOW SAVES TWO SFTP CALL AND IS JUST AS GOOD... I THINK :-)
      
      # Post-check on Source
      # The first line of the last two lines is the source filename from the first ls -1tr command in case move was unsuccessful
      sftpRemoteFileCheck="$(echo "${sftpOutput}" | awk '($1!="sftp>"){print}' | tail -n 2 | head -n 1)"
      
      if [ "${sftpRemoteFileSource}" = "${sftpRemoteFileCheck}" ]
      then
         echo 0
         handleError "${thisFunction}" 108 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileSource} could not be moved to ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileTarget}."
         return ${?}
      fi

      # Post-check on Target (should never fail and already caught by source check, but let's check anyway)
      # The last line is the target filename from the second ls -1tr command in case move was successful
      sftpRemoteFileCheck="$(echo "${sftpOutput}" | awk '($1!="sftp>"){print}' | tail -n 1)"
      
      if [ ! "${sftpRemoteFileTarget}" = "${sftpRemoteFileCheck}" ]
      then
         echo 9
         handleError "${thisFunction}" 111 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileSource} could not be moved to ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFileTarget}."
         return ${?}
      fi
   fi
   
   echo 1
   return 0
}

################################################################################
## SFTP REMOTE FILE LIST FUNCTIONS                                            ##
################################################################################

sftpRemoteFileList () {
   local thisFunction="sftpRemoteFileList"
   # Displays File List
   
   local sftpReturnCode=0
   local sftpOutput=""
   
   local sftpRemoteFilePatternList="${1}"
   local sftpRemoteDirectory="$(trim "${2}")"
   
   local sftpRemoteFilePattern=""
   local sftpRemoteFileList=""
   
   sftpSetConnection
   if [ ${sftpStatus} -eq 0 ]
   then
      echo ""
      handleError "${thisFunction}" 199 "No sftp connection to ${sftpRemoteUser}@${sftpRemoteHost}."
      return ${?}
   fi
   
   if [ -z "${sftpRemoteFilePatternList}" ]
   then
      echo ""
      handleError "${thisFunction}" 100 "No remote file pattern specified."
      return ${?}
   fi
   
   if [ -z "${sftpRemoteDirectory}" ]
   then
      echo ""
      handleError "${thisFunction}" 101 "No remote directory specified."
      return ${?}
   fi
   
   sftpRemoteDirectory="$(dirname "${sftpRemoteDirectory}/basename_dummy")"
   
   case "$(sftpRemoteDirectoryExists "${sftpRemoteDirectory}")" in # Returns 0, 1 or 2
      "0") # Directory does not exist
         echo ""
         handleError "${thisFunction}" 102 "Remote directory ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteDirectory} does not exist."
         return ${?}
      ;;
      "1") # Directory exists
         # OK
      ;;
      "9") # Handled error
         echo ""
         handleError "${thisFunction}" 103 "Remote directory ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteDirectory} could not be checked due to an error."
         return ${?}
      ;;
      *) # Unknown error
         echo ""
         handleError "${thisFunction}" 104 "Remote directory ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteDirectory} could not be checked due to an unknown error."
         return ${?}
      ;;
   esac
   
   sftpRemoteFilePatternList="$(echo "${sftpRemoteFilePatternList}" | awk '{gsub("\*","@@64;",$0); gsub("\?","@@63;",$0); print}')"
   
   for sftpRemoteFilePattern in ${sftpRemoteFilePatternList}
   do
      sftpRemoteFilePattern="$(echo "${sftpRemoteFilePattern}" | awk '{gsub("@@64;","*",$0); gsub("@@63;","?",$0); print}')"
      
      sftpOutput="#${thisFunction}\n$(sftp ${sftpCommandOptions} ${sftpRemoteUser}@${sftpRemoteHost} -b <<EOF 2>&1
         ls -1tr ${sftpRemoteDirectory}/${sftpRemoteFilePattern}
         bye
         EOF)"
      sftpReturnCode=${?}
      
      [ "$(toLower "${sftpScriptDebug}")" = "yes" ] && echo "${sftpOutput}" >> "${logFileDebug}"
      
      if [ ${sftpReturnCode} -ne 0 ]
      then
         echo ""
         handleError "${thisFunction}" ${sftpReturnCode} "An sftp error occurred."
         return ${?}
      fi
      sftpRemoteFileList="${sftpRemoteFileList}\n$(echo "${sftpOutput}")"
   done
   
   sftpRemoteFileList="$(echo "${sftpRemoteFileList}"  | awk -v lineMatch="^${sftpRemoteDirectory}/" '($0~lineMatch){print}')"
   
   echo "${sftpRemoteFileList}"
   return 0
} # sftpRemoteFileList

################################################################################

sftpRemoteFileListCount () {
   local thisFunction="sftpRemoteFileListCount"
   # Displays File List Count
   
   local sftpRemoteFilePatternList="${1}"
   local sftpRemoteDirectory="${2}"

   sftpSetConnection
   if [ ${sftpStatus} -eq 0 ]
   then
      echo 0
      handleError "${thisFunction}" 199 "No sftp connection to ${sftpRemoteUser}@${sftpRemoteHost}."
      return ${?}
   fi
   
   echo $(lineCount "$(sftpRemoteFileList "${sftpRemoteFilePatternList}" "${sftpRemoteDirectory}")")
   return ${?}
} # sftpRemoteFileListCount

################################################################################

sftpRemoteFileListGet () {
   local thisFunction="sftpRemoteFileListGet"
   # Displays
   # 0: Transfer failure
   # 1: Transfer success
   
   local sftpReturnCode=0
   
   local sftpRemoteFilePatternList="${1}"
   local sftpRemoteDirectory="$(trim "${2}")"
   local sftpLocalDirectory="$(trim "${3}")"
   local sftpLocalDirectoryStaging="$(trim "${4}")"
   local sftpRemoteDirectoryError="$(trim "${5}")"
   local sftpRemoteDirectoryArchive="$(trim "${6}")"
   
   local sftpRemoteFileList=""
   local sftpRemoteFileListCount=0
   local sftpRemoteFileCount=0
   local sftpRemoteFileCountOK=0
   local sftpRemoteFileCountKO=0
   local sftpRemoteFileCountSkip=0
   
   local sftpRemoteFile=""
   local sftpLocalFile=""
   local sftpLocalFileStaging=""
   local sftpRemoteFileError=""
   local sftpRemoteFileArchive=""
   
   sftpSetConnection
   writeSep "light"
   
   if [ ${sftpStatus} -eq 0 ]
   then
      echo 0
      handleError "${thisFunction}" 199 "No sftp connection to ${sftpRemoteUser}@${sftpRemoteHost}."
      return ${?}
   fi
   
   if [ -z "${sftpRemoteFilePatternList}" ]
   then
      echo 0
      handleError "${thisFunction}" 100 "No remote file pattern specified."
      return ${?}
   fi
   
   if [ -z "${sftpRemoteDirectory}" ]
   then
      echo 0
      handleError "${thisFunction}" 101 "No remote directory specified."
      return ${?}
   fi

   if [ -z "${sftpLocalDirectory}" ]
   then
      echo 0
      handleError "${thisFunction}" 102 "No local directory specified."
      return ${?}
   fi
   
   sftpRemoteDirectory="$(dirname "${sftpRemoteDirectory}/basename_dummy")"
   sftpLocalDirectory="$(dirname "${sftpLocalDirectory}/basename_dummy")"
   
   if [ "${sftpOptionStagedTransfer}" = "yes" ]
   then
      if [ -z "${sftpLocalDirectoryStaging}" ]
      then
         sftpLocalDirectoryStaging="${sftpLocalDirectory}"
      else
         sftpLocalDirectoryStaging="$(dirname "${sftpLocalDirectoryStaging}/basename_dummy")"
      fi
   else
      sftpLocalDirectoryStaging=""
   fi
   
   case "${sftpOptionActionError}" in
      "move")
         if [ -z "${sftpRemoteDirectoryError}" ]
         then
            echo 0
            handleError "${thisFunction}" 103 "No remote error directory specified."
            return ${?}
         fi
         
         if [ "${sftpRemoteDirectory}" = "${sftpRemoteDirectoryError}" ]
         then
            echo 0
            handleError "${thisFunction}" 104 "Remote error directory equals remote directory ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteDirectory}."
            return ${?}
         fi
         
         sftpRemoteDirectoryError="$(dirname "${sftpRemoteDirectoryError}/basename_dummy")"
      ;;
      "none"|"delete"|*)
         sftpRemoteDirectoryError=""
      ;;
   esac
   
   case "${sftpOptionActionSuccess}" in
      "archive")
         if [ -z "${sftpRemoteDirectoryArchive}" ]
         then
            echo 0
            handleError "${thisFunction}" 105 "No remote archive directory specified."
            return ${?}
         fi
         
         if [ "${sftpRemoteDirectory}" = "${sftpRemoteDirectoryArchive}" ]
         then
            echo 0
            handleError "${thisFunction}" 106 "Remote archive directory equals remote directory ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteDirectory}."
            return ${?}
         fi
         
         sftpRemoteDirectoryArchive="$(dirname "${sftpRemoteDirectoryArchive}/basename_dummy")"
      ;;
      "none"|"delete"|*)
         sftpRemoteDirectoryArchive=""
      ;;
   esac
   
   handleInfo "${thisFunction}" "SFTP TRANSFER SETTINGS:"
   handleInfo "${thisFunction}" "Remote Source Directory=${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteDirectory}"
   handleInfo "${thisFunction}" "File Pattern=${sftpRemoteFilePatternList}"
   handleInfo "${thisFunction}" "Local Target Directory=${sftpLocalDirectory}"
   handleInfo "${thisFunction}" "Overwrite Target Files=${sftpOptionOverwrite}"
   
   handleInfo "${thisFunction}" "Staged Transfer=${sftpOptionStagedTransfer}"
   [ -z "${sftpLocalDirectoryStaging}" ] || \
      handleInfo "${thisFunction}" "Local Staging Directory=${sftpLocalDirectoryStaging}"
   
   [ "${sftpOptionActionError}" = "none" ] || \
   handleInfo "${thisFunction}" "On Upload Error=${sftpOptionActionError} file"
   [ -z "${sftpRemoteDirectoryError}" ] || \
      handleInfo "${thisFunction}" "Remote Error Directory=${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteDirectoryError}"
   
   [ "${sftpOptionActionSuccess}" = "none" ] || \
   handleInfo "${thisFunction}" "On Upload Success=${sftpOptionActionSuccess} file"
   [ -z "${sftpRemoteDirectoryArchive}" ] || \
      handleInfo "${thisFunction}" "Remote Archive Directory=${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteDirectoryArchive}"
   
   writeSep "light"
   
   sftpRemoteFileList="$(sftpRemoteFileList "${sftpRemoteFilePatternList}" "${sftpRemoteDirectory}")"
   sftpReturnCode=${?}
   
   if [ ${sftpReturnCode} -ne 0 ]
   then
      echo 0
      handleError "${thisFunction}" 107 "Remote file list could not be retrieved due to an error."
      return ${?}
   fi
   
   sftpRemoteFileListCount=$(lineCount "${sftpRemoteFileList}")
   
   # Pre-check on Local Target Directory
   case "$(sftpLocalDirectoryExists "${sftpLocalDirectory}" "writeable")" in # Returns 0, 1, 2 or 9
      "0") # Directory does not exist
         echo 0
         handleError "${thisFunction}" 108 "Local directory ${sftpLocalDirectory} does not exist."
         return ${?}
      ;;
      "1") # Directory exists
         # OK
      ;;
      "2") # Directory exists but has incorrect permissions
         echo 0
         handleError "${thisFunction}" 109 "Local directory ${sftpLocalDirectory} is not writeable."
         return ${?}
      ;;
      "9") # Handled or sftp error
         echo 0
         handleError "${thisFunction}" 110 "Local directory ${sftpLocalDirectory} could not be checked due to an error."
         return ${?}
      ;;
      *) # Unknown error
         echo 0
         handleError "${thisFunction}" 111 "Local directory ${sftpLocalDirectory} could not be checked due to an unknown error."
         return ${?}
      ;;
   esac
   
   # Pre-check on Local Staging Directory
   if [ ! -z "${sftpLocalDirectoryStaging}" ] && [ ! "${sftpLocalDirectoryStaging}" = "${sftpLocalDirectory}" ]
   then
      case "$(sftpLocalDirectoryExists "${sftpLocalDirectoryStaging}" "writeable")" in # Returns 0, 1, 2 or 9
         "0") # Directory does not exist
            echo 0
            handleError "${thisFunction}" 112 "Local staging directory ${sftpLocalDirectoryStaging} does not exist."
            return ${?}
         ;;
         "1") # Directory exists
            # OK
         ;;
         "2") # Directory exists but has incorrect permissions
            echo 0
            handleError "${thisFunction}" 113 "Local staging directory ${sftpLocalDirectoryStaging} is not writeable."
            return ${?}
         ;;
         "9") # Handled error
            echo 0
            handleError "${thisFunction}" 114 "Local staging directory ${sftpLocalDirectoryStaging} could not be checked due to an error."
            return ${?}
         ;;
         *) # Unknown error
            echo 0
            handleError "${thisFunction}" 115 "Local staging directory ${sftpLocalDirectoryStaging} could not be checked due to an unknown error."
            return ${?}
         ;;
      esac
   fi
   
   # Pre-check on Remote Error Directory
   if [ ! -z "${sftpRemoteDirectoryError}" ]
   then
      case "$(sftpRemoteDirectoryExists "${sftpRemoteDirectoryError}")" in # Returns 0, 1 or 9
         "0") # Directory does not exist
            echo 0
            handleError "${thisFunction}" 116 "Remote error directory ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteDirectoryError} does not exist."
            return ${?}
         ;;
         "1") # Directory exists
            # OK
         ;;
         "9") # Handled or sftp error
            echo 0
            handleError "${thisFunction}" 117 "Remote error directory ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteDirectoryError} could not be checked due to an error."
            return ${?}
         ;;
         *) # Unknown error
            echo 0
            handleError "${thisFunction}" 118 "Remote error directory ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteDirectoryError} could not be checked due to an unknown error."
            return ${?}
         ;;
      esac
   fi
   
   # Pre-check on Remote Archive Directory
   if [ ! -z "${sftpRemoteDirectoryArchive}" ]
   then
      case "$(sftpRemoteDirectoryExists "${sftpRemoteDirectoryArchive}")" in # Returns 0, 1 or 9
         "0") # Directory does not exist
            echo 0
            handleError "${thisFunction}" 119 "Remote archive directory ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteDirectoryArchive} does not exist."
            return ${?}
         ;;
         "1") # Directory exists
            # OK
         ;;
         "9") # Handled or sftp error
            echo 0
            handleError "${thisFunction}" 120 "Remote archive directory ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteDirectoryArchive} could not be checked due to an error."
            return ${?}
         ;;
         *) # Unknown error
            echo 0
            handleError "${thisFunction}" 121 "Remote archive directory ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteDirectoryArchive} could not be checked due to an unknown error."
            return ${?}
         ;;
      esac
   fi
   
   if [ ${sftpRemoteFileListCount} -eq 0 ]
   then
      echo 1
      handleInfo "${thisFunction}" "No files found to process."
      return ${?}
   fi
   
   sftpRemoteFileCount=0
   sftpRemoteFileCountOK=0
   sftpRemoteFileCountKO=0
   sftpRemoteFileCountSkip=0
   
   for sftpRemoteFile in ${sftpRemoteFileList}
   do
      (( sftpRemoteFileCount+=1 ))
      
      handleInfo "${thisFunction}" "Processing file ${sftpRemoteFileCount} of ${sftpRemoteFileListCount}: ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile}"
      
      sftpLocalFile="${sftpLocalDirectory}/$(basename "${sftpRemoteFile}")"
      sftpLocalFileStaging=""
      [ "${sftpOptionStagedTransfer}" = "yes" ] && \
         sftpLocalFileStaging="${sftpLocalDirectoryStaging}/$(basename "${sftpRemoteFile}").${$}.sftptmp"
      
      if [ ! "$(sftpRemoteFileGet "${sftpRemoteFile}" "${sftpLocalFile}" "${sftpLocalFileStaging}")" = "1" ]
      then
         (( sftpRemoteFileCountKO+=1 ))
         
         case "${sftpOptionActionError}" in
            "move")
               sftpRemoteFileError="${sftpRemoteDirectoryError}/$(basename "${sftpRemoteFile}")"
               case "$(sftpRemoteFileMove "${sftpRemoteFile}" "${sftpRemoteFileError}")" in # Returns 0, 1 or 9
                  "0") # Move failure
                     handleError "${thisFunction}" 122 "Check permissions for remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} and remote error directory ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteDirectoryError}."
                  ;;
                  "1") # Move success
                     # OK
                  ;;
                  "9") # Handled or sftp error
                     handleError "${thisFunction}" 123 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} could not be moved to remote error directory ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteDirectoryError} due to an error."
                  ;;
                  *) # Unknown error
                     handleError "${thisFunction}" 124 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} could not be moved to remote error directory ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteDirectoryError} due to an unknown error."
                  ;;
               esac
            ;;
            "delete")
               case "$(sftpRemoteFileDelete "${sftpRemoteFile}")" in # Returns 0, 1, 2 or 9
                  "0") # Delete failure
                     handleError "${thisFunction}" 125 "Check permissions for remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile}."
                  ;;
                  "1") # Delete success
                     # OK
                  ;;
                  "2") # File does not exist
                     # OK
                  ;;
                  "9") # Handled or sftp error
                     handleError "${thisFunction}" 126 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} could not be deleted due to an error."
                  ;;
                  *) # Unknown error
                     handleError "${thisFunction}" 127 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} could not be deleted due to an unknown error."
                  ;;
               esac
            ;;
         esac
         
         continue
      fi
      
      (( sftpRemoteFileCountOK+=1 ))
      
      case "${sftpOptionActionSuccess}" in
         "archive")
            sftpRemoteFileArchive="${sftpRemoteDirectoryArchive}/$(basename "${sftpRemoteFile}")"
            
            case "$(sftpRemoteFileMove "${sftpRemoteFile}" "${sftpRemoteFileArchive}")" in # Returns 0, 1 or 9
               "0") # Move failure
                  handleError "${thisFunction}" 128 "Check permissions for remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} and remote archive directory ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteDirectoryArchive}."
               ;;
               "1") # Move success
                  # OK
               ;;
               "9") # Handled or sftp error
                  handleError "${thisFunction}" 129 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} could not be moved to remote archive directory ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteDirectoryArchive} due to an error."
               ;;
               *) # Unknown error
                  handleError "${thisFunction}" 130 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} could not be moved to remote archive directory ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteDirectoryArchive} due to an unknown error."
               ;;
            esac
         ;;
         "delete")
            case "$(sftpRemoteFileDelete "${sftpRemoteFile}")" in # Returns 0, 1, 2 or 9
               "0") # Delete failure
                  handleError "${thisFunction}" 131 "Check permissions for remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile}."
               ;;
               "1") # Delete success
                  # OK
               ;;
               "2") # File does not exist
                  # OK
               ;;
               "9") # Handled or sftp error
                  handleError "${thisFunction}" 132 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} could not be deleted due to an error."
               ;;
               *) # Unknown error
                  handleError "${thisFunction}" 133 "Remote file ${sftpRemoteUser}@${sftpRemoteHost}:${sftpRemoteFile} could not be deleted due to an unknown error."
               ;;
            esac
         ;;
      esac
   done
   
   writeSep "light"
   
   handleInfo "${thisFunction}" "SFTP TRANSFER SUMMARY:"
   #handleInfo "${thisFunction}" "Total=${sftpRemoteFileListCount}"
   #handleInfo "${thisFunction}" "OK=${sftpRemoteFileCountOK}"
   #handleInfo "${thisFunction}" "Failed=${sftpRemoteFileCountKO}"
   #handleInfo "${thisFunction}" "Skipped=${sftpRemoteFileCountSkip}"
   handleInfo "${thisFunction}" "Total=${sftpRemoteFileListCount}, OK=${sftpRemoteFileCountOK}, Failed=${sftpRemoteFileCountKO}, Skipped=${sftpRemoteFileCountSkip}"
   
   if [ ${sftpRemoteFileCountKO} -eq 0 ]
   then
      echo 1
   else
      echo 0
   fi
   
   return 0
} # sftpRemoteFileListGet

################################################################################
## MAIN                                                                       ##
################################################################################

sftpScriptInitialize "${@}"

case "${sftpScriptAction}" in
   "start")
      sftpScriptActionStart
   ;;
   "status")
      sftpScriptActionStatus
   ;;
   *)
      exitHelp
   ;;
esac

################################################################################
## END OF SCRIPT                                                              ##
################################################################################

