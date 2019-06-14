/*status-check.p*/
{preDefs.i}

&IF {&USE_APPSRV} EQ YES &THEN
  /* AppServer Mods: Grab temp-table definitions for ALL repl tables */
  {{&CODE_DIR}/AppSrv/Replicator.i "NEW SHARED"} 
  DEFINE BUFFER ReplQueue  FOR ttReplqueue. /*Setup buffer for name ReplQueue*/
  
  /*Load all Repl Temp Tables via AppSrv */
  RUN {&CODE_DIR}/AppSrv/ASGetAllRepl.p 
                  (INPUT YES). /* Include Table & Field Xref */

&ENDIF

DEFINE NEW SHARED TEMP-TABLE ttRQcnt
  FIELD ttQThread  AS INTEGER LABEL "Thread"
  FIELD ttSrcDB    AS CHAR LABEL "SrcDB" FORMAT "X(12)"
  FIELD ttSrcTable AS CHAR LABEL "SrcTable" FORMAT "X(40)"
  FIELD ttCnt      AS INTEGER LABEL "RQ  Cnt".
  
DEF VAR strMsg         AS CHAR    NO-UNDO.  
DEF VAR vReplCnt       AS INT     NO-UNDO.
DEF VAR vLastCnt       AS INT     NO-UNDO.
DEF VAR vLastChkDate   AS DATE    NO-UNDO.
DEF VAR vLastChkTime   AS INT     NO-UNDO.
DEF VAR vLastAlarmDate AS DATE    NO-UNDO.
DEF VAR vLastAlarmTime AS INT     NO-UNDO.

DEF VAR intParamCnt    AS INTEGER   NO-UNDO. /* Parameter Counter */
DEF VAR strParam       AS CHARACTER NO-UNDO. /* Parameter Name */
DEF VAR vThread#       AS INTEGER   NO-UNDO.
DEF VAR vAllThreads    AS LOGICAL   NO-UNDO.
DEF VAR vThreadCnt     AS INT       NO-UNDO.

DEF VAR vReplRunning   AS LOGICAL   NO-UNDO.

DEFINE VARIABLE hdlSvr           AS HANDLE  NO-UNDO. /* AppSrv socket-lock */ 
DEFINE VARIABLE intPortLock      AS INTEGER NO-UNDO. /* AppSrv socket-lock */
DEFINE VARIABLE logPortConnected AS LOGICAL NO-UNDO. /* AppSrv socket-lock */

DEF STREAM mstream.
DEF STREAM istream.

DEF BUFFER bfrStatCtrl for {&ReplDb}ReplControl.

FUNCTION getProperty RETURNS CHARACTER
  ( INPUT istrPropName AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Pass a property to lookup and return the value of the property
    Notes:  
------------------------------------------------------------------------------*/
  FIND {&ReplDb}ReplProperties WHERE 
       {&ReplDb}ReplProperties.PropertyName EQ istrPropName NO-ERROR.
  IF AVAILABLE {&ReplDb}ReplProperties THEN
    RETURN {&ReplDb}ReplProperties.PropertyValue.
  ELSE 
    RETURN "".

END FUNCTION.

/* Go through the Session Startup Parameter List */
/* and see if there is a Thread # */
/* If not, then check ALL Threads */
ASSIGN vAllThreads = YES
       vThread#    = 1.  /* Default */

DO intParamCnt = 1 TO NUM-ENTRIES(SESSION:PARAMETER):
  ASSIGN strParam = ENTRY(intParamCnt,SESSION:PARAMETER).
  IF NUM-ENTRIES(strParam,"=") EQ 2 THEN DO:
    IF ENTRY(1,strParam,"=") = "Thread" THEN 
      ASSIGN vThread#    = INTEGER(ENTRY(2,strparam,"="))
             vAllThreads = NO.
  END. /* IF NUM-ENTRIES(...) EQ ... */
END. /* DO intParamCnt = ... */

/* All <or> selected thread to check */
DO vThreadCnt = 1 TO 15:  

  IF vAllThreads OR
    (vThread# = vThreadCnt) THEN DO:

    /* Only Check if thread is turned on */
    FIND FIRST bfrStatCtrl WHERE
                           bfrStatCtrl.GroupID  = "CONTROL"     AND
                           bfrStatCtrl.CodeID   = "REPLICATION" AND
                           bfrStatCtrl.CodeVal1 = STRING(vThreadCnt)
         NO-LOCK NO-ERROR.
    IF AVAILABLE bfrStatCtrl THEN
    DO:
     IF bfrStatCtrl.CodeVal3 EQ "0" THEN /* Repl is off */
        NEXT. /* SKip Checking for this Thread */
    END.
    ELSE NEXT. /* Skip Checking for this Thread it has No thread record */

    /* Only check if there is at least one table set to use this thread */ 
    IF CAN-FIND(FIRST {&ReplDb}ReplTableXref WHERE
      {&ReplDb}ReplTableXref.QThread = vThreadCnt) THEN DO:

      ASSIGN vReplRunning = FALSE
             vLastCnt     = 0
             vReplCnt     = 0.

      /* Get the last count information */
      RUN ip_replCnt (INPUT "GET", INPUT vThreadCnt, INPUT-OUTPUT vLastCnt, OUTPUT vLastChkDate, OUTPUT vLastChkTIME).

      &IF {&USE_APPSRV} EQ YES &THEN
        /*Open a socket-Server to use as a ReplProc Lock for AppServer*/
        DO:
          FIND FIRST {&ReplDb}ReplProperties 
               WHERE {&ReplDb}ReplProperties.PropertyName = "AppSrv_Lock_Port"
               NO-LOCK NO-ERROR.
          IF AVAILABLE ReplProperties THEN
             ASSIGN intPortLock = INTEGER({&ReplDb}ReplProperties.PropertyValue).
          ELSE
             ASSIGN intPortLock = 65000.
          ASSIGN intPortLock = intPortLock + vThreadCnt.
        END.
        CREATE SERVER-SOCKET hdlSvr.
        ASSIGN logPortConnected = hdlSvr:ENABLE-CONNECTIONS("-S " + STRING(intPortLock))
               NO-ERROR.
        IF logPortConnected = NO THEN ASSIGN vReplRunning = TRUE. 
        hdlSvr:DISABLE-CONNECTIONS().
        DELETE OBJECT hdlSvr. /* Release Socket-Lock */
      &ELSE
        DO FOR {&ReplDb}ReplControl TRANSACTION:
          FIND FIRST {&ReplDb}ReplControl WHERE {&ReplDb}ReplControl.GroupID  = "PROCESS"
                                 AND   {&ReplDb}ReplControl.CodeID   = "REPLICATION"
                                 AND   {&ReplDb}ReplControl.CodeVal1 = STRING(vThreadCnt)
                                 EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
  
          IF NOT AVAILABLE {&ReplDb}ReplControl THEN DO:
            IF LOCKED {&ReplDb}ReplControl THEN vReplRunning = TRUE.
          END.
          RELEASE {&ReplDb}ReplControl.
        END. /* DO FOR ReplControl: */
      &ENDIF

      /* Count the current queue */
      &IF {&USE_APPSRV} EQ YES &THEN
        /*Get RQ Count via AppSrv */
        RUN {&CODE_DIR}/AppSrv/ASGetRQcount.p 
             (INPUT vThreadCnt,
              /*INPUT "all",*/
              OUTPUT strMsg).
        ASSIGN vReplCnt = 0.
        FOR EACH ttRQcnt:
          ASSIGN vReplCnt = vReplCnt + ttCnt.
        END.      
      &ELSE
        FOR EACH {&ReplDb}replQueue NO-LOCK WHERE
          {&ReplDb}ReplQueue.Applied EQ FALSE AND
          {&ReplDb}ReplQueue.QThread EQ vThreadCnt:
            vReplCnt = vReplCnt + 1.
        END.
      &ENDIF
        
      /* Not running...Do we need to alarm? */
      IF NOT vReplRunning THEN DO:
       /* Have we alarmed today? */
  
       RUN ip_AlarmInfo (INPUT "GET", INPUT vThreadCnt, OUTPUT vLastAlarmDate, OUTPUT vLastAlarmTIME).
       IF vLastAlarmDate < TODAY THEN DO:
          RUN ip_sendmail (INPUT vThreadCnt).
          RUN ip_AlarmInfo (INPUT "PUT", INPUT vThreadCnt, OUTPUT vLastAlarmDate, OUTPUT vLastAlarmTIME).
       END.
	   ELSE DO:
	    OUTPUT STREAM mstream TO value(getProperty("LOG_DIRECTORY") + "/" + "blatAlert.LOG") APPEND.
        PUT STREAM mstream UNFORMATTED STRING(now) + " THREAD NOT-RUNNING (NO-EMAIL SENT PLEASE RESET ALARMS) Thread:" + STRING(vThreadCnt) + " ReplQueue Count = " + STRING(vReplCnt) SKIP.
        OUTPUT STREAM mstream CLOSE.
	   END.
      END.
	  ELSE DO:
        OUTPUT STREAM mstream TO value(getProperty("LOG_DIRECTORY") + "/" + "blatAlert.LOG") APPEND.
        PUT STREAM mstream UNFORMATTED STRING(now) + " STATUS ALL GOOD Thread:" + STRING(vThreadCnt) + " ReplQueue Count = " + STRING(vReplCnt) SKIP.
        OUTPUT STREAM mstream CLOSE.
      END.

      /* Store the date/time/cnt for the next run */
      RUN ip_replCnt (INPUT "PUT", INPUT vThreadCnt, INPUT-OUTPUT vReplCnt, OUTPUT vLastChkDate, OUTPUT vLastChkTIME).

    END. /* Replication turned on for this thread */

  END. /* Check ALL threads OR just one */

END. /* vThreadCnt = 1 to 5 */

OUTPUT STREAM mstream TO value(getProperty("LOG_DIRECTORY") + "/" + "blatAlert.LOG") APPEND.
PUT STREAM mstream UNFORMATTED "--------------------------------------------------------------------------" SKIP.
OUTPUT STREAM mstream CLOSE.

RETURN.

/* Procedure to put/get the last count/check date/check time of the queue */
PROCEDURE ip_ReplCnt:
  DEF INPUT PARAMETER        vmode  AS CHAR NO-UNDO.
  DEF INPUT PARAMETER        vthread AS INT NO-UNDO.
  DEF INPUT-OUTPUT PARAMETER vcnt   AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER       vdate  AS DATE NO-UNDO.
  DEF OUTPUT PARAMETER       vtime  AS INT  NO-UNDO.
  
  DEF BUFFER bReplControl FOR {&ReplDb}ReplControl.

  DO FOR bReplControl TRANSACTION:
     IF vmode = "PUT" THEN DO:
         FIND FIRST bReplControl WHERE bReplControl.GroupID  = "REPLICATION"
                                   AND bReplControl.CodeID   = "RECCNT" 
                                   AND bReplControl.CodeVal1 = STRING(vthread)
                                  EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAIL bReplControl THEN DO:
           CREATE bReplControl.
           ASSIGN bReplControl.GroupID  = "REPLICATION"
                  bReplControl.CodeID   = "RECCNT"
                  bReplControl.CodeVal1 = STRING(vthread).
         END.
         ASSIGN bReplControl.CodeVal2 = STRING(TODAY) + "," + STRING(TIME) 
                bReplControl.CodeVal3 = STRING(vCnt).
     END.  /*IF vmode = "PUT" THEN DO: */
     ELSE DO:
          FIND FIRST bReplControl WHERE bReplControl.GroupID  = "REPLICATION"
                                    AND bReplControl.CodeID   = "RECCNT"
                                    AND bReplControl.CodeVal1 = STRING(vthread)
                                    NO-LOCK NO-ERROR.
          ASSIGN vdate = IF AVAILABLE bReplControl THEN DATE(ENTRY(1,bReplControl.CodeVal2))  ELSE ?
                 vtime = IF AVAILABLE bReplControl THEN INT(ENTRY(2,bReplControl.CodeVal2))   ELSE 0
                 vcnt  = IF AVAILABLE bReplControl THEN INT(bReplControl.CodeVal3)            ELSE 0
				 NO-ERROR.
     END.  /*ELSE DO (GET) */
  END.  /*DO FOR bReplControl: */
END.    /*PROCEDURE ip_ReplCnt:*/

/* Procedure to put/get the last alarm date/alarm time of the queue */
PROCEDURE ip_AlarmInfo:
  DEF INPUT PARAMETER        vmode  AS CHAR NO-UNDO.
  DEF INPUT PARAMETER        vThread AS INT NO-UNDO.
  DEF OUTPUT PARAMETER       vdate  AS DATE NO-UNDO.
  DEF OUTPUT PARAMETER       vtime  AS INT  NO-UNDO.
  
  DEF BUFFER bReplControl FOR {&ReplDb}ReplControl.

  DO FOR bReplControl TRANSACTION:
     IF vmode = "PUT" THEN DO:
         FIND FIRST bReplControl WHERE bReplControl.GroupID  = "REPLICATION"
                                   AND bReplControl.CodeID   = "ALARM" 
                                   AND bReplControl.CodeVal1 = STRING(vThread)
                                  EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAIL bReplControl THEN DO:
           CREATE bReplControl.
           ASSIGN bReplControl.GroupID  = "REPLICATION"
                  bReplControl.CodeID   = "ALARM"
                  bReplControl.CodeVal1 = STRING(vThread).
         END.
         ASSIGN   bReplControl.CodeVal2 = STRING(TODAY) + "," + STRING(TIME).
     END.  /*IF vmode = "PUT" THEN DO: */
     ELSE IF vmode = "GET" THEN DO:
          FIND FIRST bReplControl WHERE bReplControl.GroupID  = "REPLICATION"
                                    AND bReplControl.CodeID   = "ALARM"
                                    AND bReplControl.CodeVal1 = STRING(vThread)
                                    NO-LOCK NO-ERROR.
          ASSIGN vdate = IF AVAILABLE bReplControl THEN DATE(ENTRY(1,bReplControl.CodeVal2)) ELSE 01/01/2000
                 vtime = IF AVAILABLE bReplControl THEN INT(ENTRY(2,bReplControl.CodeVal2))  ELSE 0.
     END.  /*ELSE DO (GET) */
  END.  /*DO FOR bReplControl: */
END.    /*PROCEDURE ip_AlarmInfo:*/


/*Procedure to send an alarm*/
PROCEDURE ip_SendMail:

  DEF INPUT PARAMETER ip_thread AS INT NO-UNDO.

  DEF VAR vSubject     AS CHAR NO-UNDO INIT "Pro2SQL Replication Alarm".
  DEF VAR vSendTo      AS CHAR NO-UNDO.
  DEF VAR vMailOptions AS CHAR NO-UNDO.
  DEF VAR vScriptDir   AS CHAR NO-UNDO.
  DEF VAR vTmpDir      AS CHAR NO-UNDO.
  DEF VAR vAlertList   AS CHAR NO-UNDO INIT "Pro2SQL_Alert.txt".
  DEF VAR vSendCmd     AS CHAR NO-UNDO.
  DEF VAR vFilename    AS CHAR NO-UNDO.
  DEF VAR tmp          AS CHAR NO-UNDO.
  DEF VAR strPro2SQLDIR  AS CHAR NO-UNDO.
  DEF VAR strDirMrk      AS CHAR NO-UNDO.
  DEF VAR vLogDir        AS CHAR NO-UNDO.
  
  /* Grab up a couple of ENV vars for the ouput and script dir */
  ASSIGN  strDirMrk     = IF OPSYS = "WIN32" THEN "~\" ELSE "/"
          vScriptDir    = OS-GetEnv( "SCRIPTDIR" )
          vTmpDir       = OS-GetEnv( "TMPDIR" )
          strPro2SQLDir = OS-GetEnv( "PRO2SQL" )
          vLogDir       = getProperty("LOG_DIRECTORY")
          strPro2SQLDir = UPPER(ENTRY(NUM-ENTRIES(strPro2SQLDir,strDirMrk),
                                                    strPro2SQLDir,strDirMrk)) 
      .
  IF vLogDir LE "" THEN ASSIGN vLogDir = "bprepl" + strDirMrk + "repl_log".

  IF (vScriptDir = '' OR vScriptDir = ?) OR (vTmpDir = '' OR vTmpDir = ?) THEN DO:
    OUTPUT STREAM mstream TO value("Alarm.LOG") APPEND.
    PUT STREAM mstream UNFORMATTED "Need to alarm but SCRIPTDIR and/or TMPDIR not set...aborting " string(today,"99/99/9999") " " STRING(time,"HH:MM:SS") SKIP.
    OUTPUT STREAM mstream CLOSE.
    RETURN.
  END.
  
  vAlertList = SEARCH(vScriptDir + strDirMrk + vAlertList).

  IF vAlertList <> ? THEN DO:  /*Found it..Load it */
     INPUT STREAM istream FROM VALUE(vAlertList) NO-ECHO.
     REPEAT:
            tmp = "".
            IMPORT STREAM istream tmp.
            IF tmp = "" OR tmp = ? THEN LEAVE.
            vSendTo = IF vSendTo = "" THEN tmp ELSE vSendTo + "," + tmp.
     END.
     INPUT STREAM istream CLOSE.
  END.  /*IF SEARCH(bAlertList) <> ? THEN DO:  /*Found it..Load it */ */
  
  /*Create the Body to a file */
  vFilename = vTmpDir + strDirMrk + "blat.tmp".
  OUTPUT STREAM mstream TO value(vFilename).
  PUT STREAM mstream  UNFORMATTED   
         "Pro2SQL replication queue for " + strPro2SQLDir + 
         " Thread # " STRING(ip_thread)
         " appears to be down as of " STRING(TODAY) " @ " STRING(TIME,"HH:MM:SS") SKIP
         "The current queue count is " STRING(vReplCnt) "." SKIP(1)
         "The last queue check was performed at " STRING(vLastChkDate) " @ " STRING(vLastChkTime,"HH:MM:SS") SKIP
         "The count at that time was " STRING(vLastCnt) " records." SKIP(1)
		 "NOTE: To Receive Additional Alarms Today You Must Reset The Alarms in the Pro2-Admin Tool" SKIP
		 "      [Tools]->[Reset Alarms]" SKIP.

  IF SEARCH(vAlertList) = ?  OR vSendTo = "" THEN DO:
      PUT  STREAM mstream  UNFORMATTED SKIP(2) 'Using default e-mail address.  Consider setting up "Pro2SQL_Alert.txt" alert file.' SKIP.
       vSendTo = "Pro2Support@bravepoint.com".
  END.
     
  OUTPUT STREAM mstream CLOSE.
  
  IF OPSYS = "win32" THEN DO:

    ASSIGN vFilename = REPLACE(vFilename,"~/","\")
           vSubject  = vSubject + " for : " + strPro2SQLDir.
    /*vSendCmd = vScriptDir + '\blat.exe ' + vFilename + ' -to ' + vSendTo + 
                ' -s "' + vSubject + '"' + ' -q'.*/
    vSendCmd = 'blat.exe ' + vFilename + ' -to ' + vSendTo + 
                ' -server {&COMPANY-MAIL-SERVER}  -f {&FROM-EMAL-ADDR} ' +
                ' -s "' + vSubject + '"' + ' -q'.
    OUTPUT STREAM mstream TO value(vLogDir + strDirMrk + "blatAlert.LOG") APPEND.
    PUT STREAM mstream UNFORMATTED vSendCmd SKIP.
    OUTPUT STREAM mstream CLOSE.
    OS-COMMAND SILENT VALUE( vSendCmd ).
  END.  /* opsys = "win32*/

  ELSE IF OPSYS = "unix" THEN DO:
      /*  We have to use Heirloom mailx with -a support for attachements
          There might be another mailx program on the server but it is likely
          that it does NOT support the -a flag. "Our" mailx is placed in the
          scripts directory. We do have executables for HPUX, AIX and Linux.
      */

      OS-COMMAND silent value
                ( 'echo "' + vFilename + '" | ' +
                  vScriptDir + '/mailx ' +
                  '-s "' + vSubject + '" ' +
                  vMailOptions + ' ' +
                  vSendTo + ' ' +
                  ' > /dev/null 2>&1' ).
  END.   /*ELSE IF OPSYS = "unix" THEN DO: */
END. /*PROCEDURE ip_SendMail*/

