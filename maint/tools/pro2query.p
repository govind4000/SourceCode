&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/* PSC: pro2query.p - Pro2Replication Query                                 */
/* COPYRIGHT HP Inc. ALL RIGHTS RESERVED. THIS IS AN UNPUBLISHED WORK.      */
/*                                                                          */
/* Category : B2                                                            */
/* - A      : Updates Std. MFG/PRO Database                                 */
/* - B      : Updates special Database                                      */
/* - C      : Non-Updating                                                  */
/* - 1      : Modified Std. MFG/PRO Program                                 */
/* - 2      : New Program                                                   */
/*                                                                          */
/*                                                                          */
/* %Z% SCCS-Id:%P% %I% %G% %U% tk                                           */
/* REVISION: eB   LAST MODIFIED: 23 Oct 2015  BY: HP/RDU                    */
/* ***************************  Definitions  ************************** */

DEFINE VARIABLE         vcSessionParameter   AS    CHARACTER      NO-UNDO.
DEFINE VARIABLE         vcPro2Database       AS    CHARACTER      NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getPro2Database) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPro2Database Procedure 
FUNCTION getPro2Database RETURNS CHARACTER
   () FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getThreadQueueInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getThreadQueueInfo Procedure 
FUNCTION getThreadQueueInfo RETURNS LOGICAL
   (INPUT                  ipiThread            AS    INTEGER,
    INPUT                  ipcDatabaseName      AS    CHARACTER,
    INPUT                  ipcTableName         AS    CHARACTER,
    INPUT                  iplGetQueueApplied   AS    LOGICAL,
    OUTPUT                 opiQueueNonApplied   AS    INT64,
    OUTPUT                 opiQueueApplied      AS    INT64) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getThreadStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getThreadStatus Procedure 
FUNCTION getThreadStatus RETURNS LOGICAL
   (INPUT                  ipiThread            AS    INTEGER,
    OUTPUT                 oplThreadEnabled     AS    LOGICAL,
    OUTPUT                 oplThreadRunning     AS    LOGICAL) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getThreadTableInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getThreadTableInfo Procedure 
FUNCTION getThreadTableInfo RETURNS LOGICAL
   (INPUT                  ipiThread            AS    INTEGER,
    OUTPUT                 opiTablesLinked      AS    INT64,
    OUTPUT                 opiTablesActive      AS    INT64,
    OUTPUT                 opiTablesSuspended   AS    INT64) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isLocked) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isLocked Procedure 
FUNCTION isLocked RETURNS LOGICAL
   (INPUT                  iphQuery       AS    HANDLE,
    INPUT                  iphBuffer      AS    HANDLE) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isPro2Database) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isPro2Database Procedure 
FUNCTION isPro2Database RETURNS LOGICAL
   (INPUT                  ipcDatabaseName   AS    CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sessionParameter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sessionParameter Procedure 
FUNCTION sessionParameter RETURNS CHARACTER
   () FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-showThreadStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD showThreadStatus Procedure 
FUNCTION showThreadStatus RETURNS LOGICAL
   (INPUT                  ipcMode                 AS    CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-showThreadTableStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD showThreadTableStatus Procedure 
FUNCTION showThreadTableStatus RETURNS LOGICAL
   (INPUT                  ipcMode                 AS    CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-threadEnabled) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD threadEnabled Procedure 
FUNCTION threadEnabled RETURNS LOGICAL
   (INPUT                  ipiThread            AS    INTEGER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-threadRunning) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD threadRunning Procedure 
FUNCTION threadRunning RETURNS LOGICAL
   (INPUT                  ipiThread            AS    INTEGER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 14.58
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

ON CLOSE OF THIS-PROCEDURE DO:
   IF THIS-PROCEDURE:PERSISTENT
   THEN DELETE PROCEDURE THIS-PROCEDURE.
   
   IF PROGRAM-NAME(2) EQ ?
   THEN QUIT.
END.

IF NUM-DBS EQ 0
THEN DO:
   MESSAGE "ERROR: No databases connected.".
   APPLY "CLOSE" TO THIS-PROCEDURE.
   RETURN "".
END.

ASSIGN vcSessionParameter = sessionParameter()
       vcPro2Database     = getPro2Database()
       .

IF vcPro2Database EQ "" OR
   vcPro2Database EQ ?
THEN DO:
   MESSAGE SUBSTITUTE("ERROR: Pro2Replication tables not found in &1.",
                      NUM-DBS).
   APPLY "CLOSE" TO THIS-PROCEDURE.
   RETURN "".
END.

CASE vcSessionParameter:
   WHEN "threadList"
   THEN showThreadStatus(vcSessionParameter).
   WHEN "threadStatus"
   THEN showThreadStatus(vcSessionParameter).
   WHEN "tableStatus"
   THEN showThreadStatus(vcSessionParameter).
   WHEN "pro2Status"         OR
   WHEN "pro2Status BOOLEAN"
   THEN showThreadStatus(vcSessionParameter).
   WHEN "tableList"
   THEN showThreadTableStatus(vcSessionParameter).
   OTHERWISE DO:
      MESSAGE "Usage:"                                      +  CHR(10) +
              "help        - Show this help"                +  CHR(10) + 
              "threadlist  - Show thread list"              +  CHR(10) + 
              "pro2status  - Show Pro2Replication Status"   +  CHR(10)
              .
   END.
END CASE.

APPLY "CLOSE" TO THIS-PROCEDURE.

RETURN "".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getPro2Database) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPro2Database Procedure 
FUNCTION getPro2Database RETURNS CHARACTER
   ():

   DEFINE VARIABLE         liCounter      AS    INTEGER        NO-UNDO.

   DO liCounter = 1 TO NUM-DBS:
      IF isPro2Database(LDBNAME(liCounter))
      THEN RETURN LDBNAME(liCounter).
   END.
   
   RETURN ?.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getThreadQueueInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getThreadQueueInfo Procedure 
FUNCTION getThreadQueueInfo RETURNS LOGICAL
   (INPUT                  ipiThread            AS    INTEGER,
    INPUT                  ipcDatabaseName      AS    CHARACTER,
    INPUT                  ipcTableName         AS    CHARACTER,
    INPUT                  iplGetQueueApplied   AS    LOGICAL,
    OUTPUT                 opiQueueNonApplied   AS    INT64,
    OUTPUT                 opiQueueApplied      AS    INT64):
    
   DEFINE VARIABLE         lhBuffer             AS    HANDLE         NO-UNDO.
   DEFINE VARIABLE         lhQuery              AS    HANDLE         NO-UNDO.
   
   DEFINE VARIABLE         lcTableName          AS    CHARACTER      NO-UNDO.
   DEFINE VARIABLE         lcQuery              AS    CHARACTER      NO-UNDO.
   
   ASSIGN ipcDatabaseName = (IF ipcDatabaseName EQ ""
                             THEN ?
                             ELSE ipcDatabaseName)
          ipcTableName    = (IF ipcTableName EQ ""
                             THEN ?
                             ELSE ipcTableName)
          ipcDatabaseName = (IF ipcTableName EQ ?
                             THEN ?
                             ELSE ipcDatabaseName)
          ipcTableName    = (IF ipcDatabaseName EQ ?
                             THEN ?
                             ELSE ipcTableName)
          .
                             
   
   ASSIGN lcTableName = SUBSTITUTE("&1.&2", vcPro2Database, "ReplQueue")
          lcQuery     = SUBSTITUTE("FOR EACH &1 "                       +
                                   "    WHERE &1.QThread  EQ &2 "       +
                                   (IF iplGetQueueApplied EQ ?
                                    THEN ""
                                    ELSE "AND &1.Applied  EQ &3 ")      +
                                   (IF ipcDatabaseName EQ ?
                                    THEN ""
                                    ELSE "AND &1.SrcDb    EQ '&4' "     +
                                         "AND &1.SrcTable EQ '&5' ")    + 
                                   "    NO-LOCK ",
                                   lcTableName,
                                   ipiThread,
                                   iplGetQueueApplied,
                                   ipcDatabaseName,
                                   ipcTableName).
   CREATE BUFFER lhBuffer FOR TABLE lcTableName.
   CREATE QUERY lhQuery.
   
   lhQuery:SET-BUFFERS(lhBuffer).
   lhQuery:QUERY-PREPARE(lcQuery).
   lhQuery:QUERY-OPEN().
   lhQuery:GET-FIRST().
   
   ASSIGN opiQueueNonApplied = 0
          opiQueueApplied    = 0
          .
          
   replicationTable:
   DO WHILE TRUE:
      IF lhQuery:QUERY-OFF-END
      THEN LEAVE replicationTable.
      
      ASSIGN opiQueueNonApplied = opiQueueNonApplied + INTEGER(lhBuffer::Applied NE TRUE)
             opiQueueApplied    = opiQueueApplied    + INTEGER(lhBuffer::Applied EQ TRUE)
             .
      lhQuery:GET-NEXT().
   END.
   
   ASSIGN opiQueueNonApplied = (IF iplGetQueueApplied EQ TRUE
                                THEN ?
                                ELSE opiQueueNonApplied)
          opiQueueApplied    = (IF iplGetQueueApplied EQ FALSE
                                THEN ?
                                ELSE opiQueueApplied)
          .   
   
   lhQuery:QUERY-CLOSE().
   lhBuffer:BUFFER-RELEASE().
   
   DELETE OBJECT lhBuffer NO-ERROR.
   DELETE OBJECT lhQuery NO-ERROR.

   RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getThreadStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getThreadStatus Procedure 
FUNCTION getThreadStatus RETURNS LOGICAL
   (INPUT                  ipiThread            AS    INTEGER,
    OUTPUT                 oplThreadEnabled     AS    LOGICAL,
    OUTPUT                 oplThreadRunning     AS    LOGICAL):
    
   ASSIGN oplThreadEnabled   = threadEnabled(ipiThread)
          oplThreadRunning   = threadRunning(ipiThread)
          .
   
   RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getThreadTableInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getThreadTableInfo Procedure 
FUNCTION getThreadTableInfo RETURNS LOGICAL
   (INPUT                  ipiThread            AS    INTEGER,
    OUTPUT                 opiTablesLinked      AS    INT64,
    OUTPUT                 opiTablesActive      AS    INT64,
    OUTPUT                 opiTablesSuspended   AS    INT64):
    
   DEFINE VARIABLE         lhBuffer          AS    HANDLE         NO-UNDO.
   DEFINE VARIABLE         lhQuery           AS    HANDLE         NO-UNDO.
   
   DEFINE VARIABLE         lcTableName       AS    CHARACTER      NO-UNDO.
   DEFINE VARIABLE         lcQuery           AS    CHARACTER      NO-UNDO.
   
   ASSIGN lcTableName = SUBSTITUTE("&1.&2", vcPro2Database, "ReplTableXref")
          lcQuery     = SUBSTITUTE("FOR EACH &1 "                       +
                                   "    WHERE &1.QThread  EQ &2 "       +
                                   "    NO-LOCK ",
                                   lcTableName,
                                   ipiThread).
   
   CREATE BUFFER lhBuffer FOR TABLE lcTableName.
   CREATE QUERY lhQuery.
   
   lhQuery:SET-BUFFERS(lhBuffer).
   lhQuery:QUERY-PREPARE(lcQuery).
   lhQuery:QUERY-OPEN().
   lhQuery:GET-FIRST().
   
   ASSIGN opiTablesLinked    = 0
          opiTablesActive    = 0
          opiTablesSuspended = 0
          .
          
   replicationTable:
   DO WHILE TRUE:
      IF lhQuery:QUERY-OFF-END
      THEN LEAVE replicationTable.
      
      ASSIGN opiTablesLinked    = opiTablesLinked    + 1
             opiTablesActive    = opiTablesActive    + INTEGER(lhBuffer::GenQrec EQ TRUE)
             opiTablesSuspended = opiTablesSuspended + INTEGER(lhBuffer::GenQrec EQ TRUE AND lhBuffer::ProcQRec NE TRUE)
             .
      lhQuery:GET-NEXT().
   END.
   
   lhQuery:QUERY-CLOSE().
   lhBuffer:BUFFER-RELEASE().
   
   DELETE OBJECT lhBuffer NO-ERROR.
   DELETE OBJECT lhQuery NO-ERROR.

   RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isLocked) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isLocked Procedure 
FUNCTION isLocked RETURNS LOGICAL
   (INPUT                  iphQuery       AS    HANDLE,
    INPUT                  iphBuffer      AS    HANDLE):

   checkLock:
   DO TRANSACTION:
      iphQuery:GET-CURRENT(EXCLUSIVE-LOCK, NO-WAIT).
      RETURN iphBuffer:LOCKED.
   END.
   
   RETURN ?.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isPro2Database) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isPro2Database Procedure 
FUNCTION isPro2Database RETURNS LOGICAL
   (INPUT                  ipcDatabaseName   AS    CHARACTER):

   DEFINE VARIABLE         llIsPro2Database AS    LOGICAL        NO-UNDO.
   
   DEFINE VARIABLE         lhBuffer          AS    HANDLE         NO-UNDO.
   DEFINE VARIABLE         lhQuery           AS    HANDLE         NO-UNDO.
   
   DEFINE VARIABLE         lcTableName       AS    CHARACTER      NO-UNDO.
   DEFINE VARIABLE         lcQuery           AS    CHARACTER      NO-UNDO.
   
   ASSIGN lcTableName = SUBSTITUTE("&1.&2", ipcDatabaseName, "_file")
          lcQuery     = SUBSTITUTE("FOR EACH &1 "                                +
                                   "    WHERE &1._file-name  EQ 'ReplControl' "  +
                                   "    NO-LOCK ",
                                   lcTableName).
                                   
   CREATE BUFFER lhBuffer FOR TABLE lcTableName.
   CREATE QUERY lhQuery.
   
   lhQuery:SET-BUFFERS(lhBuffer).
   lhQuery:QUERY-PREPARE(lcQuery).
   lhQuery:QUERY-OPEN().
   lhQuery:GET-FIRST().
          
   ASSIGN llIsPro2Database = lhQuery:QUERY-OFF-END EQ FALSE
          NO-ERROR.
   
   lhQuery:QUERY-CLOSE().
   lhBuffer:BUFFER-RELEASE().
   
   DELETE OBJECT lhBuffer NO-ERROR.
   DELETE OBJECT lhQuery NO-ERROR.
   
   RETURN llIsPro2Database.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sessionParameter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sessionParameter Procedure 
FUNCTION sessionParameter RETURNS CHARACTER
   ():
   
   DEFINE VARIABLE         lcSessionParameter   AS    CHARACTER      NO-UNDO.
   DEFINE VARIABLE         liCounter            AS    INTEGER        NO-UNDO.
   
   sessionParameterEntry:
   DO liCounter = 1 TO NUM-ENTRIES(SESSION:PARAMETER, " "):
      IF ENTRY(liCounter, SESSION:PARAMETER, " ") EQ ""
      THEN NEXT sessionParameterEntry.
      
      ASSIGN lcSessionParameter = SUBSTITUTE("&1 &2", 
                                             lcSessionParameter,
                                             ENTRY(liCounter, SESSION:PARAMETER, " ")). 
   END.
   
   RETURN TRIM(lcSessionParameter).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-showThreadStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION showThreadStatus Procedure 
FUNCTION showThreadStatus RETURNS LOGICAL
   (INPUT                  ipcMode                 AS    CHARACTER):
   
   DEFINE VARIABLE         lhBuffer                AS    HANDLE         NO-UNDO.
   DEFINE VARIABLE         lhQuery                 AS    HANDLE         NO-UNDO.
   
   DEFINE VARIABLE         lcTableName             AS    CHARACTER      NO-UNDO.
   DEFINE VARIABLE         lcQuery                 AS    CHARACTER      NO-UNDO.
   
   DEFINE VARIABLE         lcLine                  AS    CHARACTER      NO-UNDO.
   
   DEFINE VARIABLE         llThreadEnabled         AS    LOGICAL        NO-UNDO.
   DEFINE VARIABLE         llThreadRunning         AS    LOGICAL        NO-UNDO.

   DEFINE VARIABLE         liTablesLinked          AS    INT64          NO-UNDO.
   DEFINE VARIABLE         liTablesActive          AS    INT64          NO-UNDO.
   DEFINE VARIABLE         liTablesSuspended       AS    INT64          NO-UNDO.
   DEFINE VARIABLE         liQueueNonApplied       AS    INT64          NO-UNDO.
   DEFINE VARIABLE         liQueueApplied          AS    INT64          NO-UNDO.
   
   DEFINE VARIABLE         liTablesLinkedTotal     AS    INT64          NO-UNDO.
   DEFINE VARIABLE         liTablesActiveTotal     AS    INT64          NO-UNDO.
   DEFINE VARIABLE         liTablesSuspendedTotal  AS    INT64          NO-UNDO.
   DEFINE VARIABLE         liTablesDownTotal       AS    INT64          NO-UNDO.
   
   DEFINE VARIABLE         liQueueNonAppliedTotal  AS    INT64          NO-UNDO.
   DEFINE VARIABLE         liQueueAppliedTotal     AS    INT64          NO-UNDO.
   
   DEFINE VARIABLE         liThreadsLinked         AS    INTEGER        NO-UNDO.
   DEFINE VARIABLE         liThreadsUp             AS    INTEGER        NO-UNDO.
   DEFINE VARIABLE         liThreadsDown           AS    INTEGER        NO-UNDO.
   DEFINE VARIABLE         liThreadsInactive       AS    INTEGER        NO-UNDO.
   DEFINE VARIABLE         liThreadsSuspended      AS    INTEGER        NO-UNDO.
   
   DEFINE VARIABLE         lcStatus                AS    CHARACTER      NO-UNDO.
   
   ASSIGN lcTableName = SUBSTITUTE("&1.&2", vcPro2Database, "ReplControl")
          lcQuery     = SUBSTITUTE("FOR EACH &1 "                                 +
                                   /*
                                   "    WHERE &1.GroupId  EQ 'CONTROL'     AND "  +
                                   */
                                   "    WHERE &1.GroupId  EQ 'PROCESS'     AND "  +
                                   "          &1.CodeId   EQ 'REPLICATION' "      +
                                   "    NO-LOCK "                                 +
                                   "    BY INTEGER(&1.CodeVal1)",
                                   lcTableName).
   
   CASE ipcMode:
      WHEN "threadList"
      THEN DO:
         MESSAGE SUBSTITUTE("Pro2Replication Thread List in database ""&1"" as of &2",
                            vcPro2Database,
                            ISO-DATE(NOW)).
         ASSIGN lcLine = ""
                OVERLAY(lcLine, 027, 009) = "   Tables"
                OVERLAY(lcLine, 038, 009) = "   Tables"
                OVERLAY(lcLine, 049, 009) = "   Tables"
                OVERLAY(lcLine, 060, 011) = "      Queue"
                /*
                OVERLAY(lcLine, 067, 011) = "      Queue"
                */
                .
         MESSAGE lcLine.
         
         ASSIGN lcLine = ""
                OVERLAY(lcLine, 001, 006) = "Thread"
                OVERLAY(lcLine, 009, 007) = "Enabled"
                OVERLAY(lcLine, 018, 007) = "Running"
                OVERLAY(lcLine, 027, 009) = "   Linked"
                OVERLAY(lcLine, 038, 009) = "   Active"
                OVERLAY(lcLine, 049, 009) = "Suspended"
                OVERLAY(lcLine, 060, 011) = "Non-applied"
                /*
                OVERLAY(lcLine, 067, 011) = "    Applied"
                */
                .
         MESSAGE lcLine.
         
      END.
   END CASE.
      
   CREATE BUFFER lhBuffer FOR TABLE lcTableName.
   CREATE QUERY lhQuery.
   
   lhQuery:SET-BUFFERS(lhBuffer).
   lhQuery:QUERY-PREPARE(lcQuery).
   lhQuery:QUERY-OPEN().
   lhQuery:GET-FIRST().
   
   replicationControl:
   DO WHILE TRUE:
      IF lhQuery:QUERY-OFF-END
      THEN LEAVE replicationControl.
      
      getThreadStatus
         (INPUT  INTEGER(lhBuffer::CodeVal1),
          OUTPUT llThreadEnabled,
          OUTPUT llThreadRunning)
          .
      
      getThreadTableInfo
         (INPUT  INTEGER(lhBuffer::CodeVal1),
          OUTPUT liTablesLinked,
          OUTPUT liTablesActive,
          OUTPUT liTablesSuspended).
      
      CASE ipcMode:
         WHEN "threadList"
         THEN DO:
            getThreadQueueInfo
               (INPUT  INTEGER(lhBuffer::CodeVal1),
                INPUT  ?,                             /* Source Database */
                INPUT  ?,                             /* Source Table    */
                INPUT  FALSE,                         /* Queue Applied   */
                OUTPUT liQueueNonApplied,
                OUTPUT liQueueApplied).
         END.
      END CASE.
      
      ASSIGN liTablesLinkedTotal    = liTablesLinkedTotal    + liTablesLinked
             liTablesActiveTotal    = liTablesActiveTotal    + liTablesActive
             liTablesSuspendedTotal = liTablesSuspendedTotal + liTablesSuspended
             liQueueNonAppliedTotal = liQueueNonAppliedTotal + liQueueNonApplied
             liQueueAppliedTotal    = liQueueAppliedTotal    + liQueueApplied
             .

      IF liTablesLinked GT 0
      THEN DO:
         ASSIGN liThreadsLinked = liThreadsLinked + 1.
         
         IF llThreadRunning EQ TRUE
         THEN DO:
            ASSIGN liThreadsUp = liThreadsUp + 1.
            
            IF liTablesLinked GT liTablesActive
            THEN ASSIGN liThreadsInactive = liThreadsInactive + 1.
            
            IF liTablesSuspended GT 0
            THEN ASSIGN liThreadsSuspended = liThreadsSuspended + 1.
         END.
         ELSE DO:
            ASSIGN liThreadsDown          = liThreadsDown + 1
                   liTablesDownTotal      = liTablesDownTotal      + liTablesActive
                   liTablesActiveTotal    = liTablesActiveTotal    - liTablesActive
                   liTablesSuspendedTotal = liTablesSuspendedTotal - liTablesSuspended
                   .
         END.
      END.
      
      CASE ipcMode:
         WHEN "threadList"
         THEN DO:
            IF liTablesLinked    GT 0 OR
               liQueueNonApplied GT 0
            THEN DO:   
               ASSIGN lcLine = ""
                      OVERLAY(lcLine, 001, 006) = lhBuffer::CodeVal1
                      /*
                      OVERLAY(lcLine, 009, 007) = STRING(lhBuffer::CodeVal3 NE "0", "Yes/No")
                      */
                      OVERLAY(lcLine, 009, 007) = STRING(llThreadEnabled, "Yes/No")
                      OVERLAY(lcLine, 018, 007) = STRING(llThreadRunning, "Yes/No")
                      OVERLAY(lcLine, 027, 009) = SUBSTITUTE("&1", STRING(liTablesLinked   , ">,>>>,>>9"))
                      OVERLAY(lcLine, 038, 009) = SUBSTITUTE("&1", STRING(liTablesActive   , ">,>>>,>>9"))
                      OVERLAY(lcLine, 049, 009) = SUBSTITUTE("&1", STRING(liTablesSuspended, ">,>>>,>>9"))
                      OVERLAY(lcLine, 060, 011) = SUBSTITUTE("&1", STRING(liQueueNonApplied, ">>>,>>>,>>9"))
                      /*
                      OVERLAY(lcLine, 067, 011) = SUBSTITUTE("&1", STRING(liQueueApplied   , ">>>,>>>,>>9"))
                      */
                      .
               
               /*
               IF lhBuffer::CodeVal3 EQ "0" AND
               */
               IF llThreadEnabled    NE TRUE AND
                  liTablesLinked     GT 0
               THEN ASSIGN OVERLAY(lcLine, 009, 007) = "No*".
                                        
                /*
               IF lhBuffer::CodeVal3 NE "0" AND
               */
               IF llThreadEnabled    EQ TRUE AND
                  liTablesLinked     GT 0    AND
                  llThreadRunning    NE TRUE
               THEN ASSIGN OVERLAY(lcLine, 018, 007) = "No*".
               
               IF liTablesActive LT liTablesLinked
               THEN ASSIGN OVERLAY(lcLine, 047, 001) = "*".
               
               IF liTablesSuspended GT 0
               THEN ASSIGN OVERLAY(lcLine, 058, 001) = "*".
               
               MESSAGE lcLine.
            END.
         END. /* threadList */
      END CASE.
      
      lhQuery:GET-NEXT().
   END.
   
   lhQuery:QUERY-CLOSE().
   lhBuffer:BUFFER-RELEASE().
   
   DELETE OBJECT lhBuffer NO-ERROR.
   DELETE OBJECT lhQuery NO-ERROR.
   
   IF lcStatus           EQ ""               AND
      liThreadsDown      GT 0                AND
      liThreadsDown      EQ liThreadsLinked
   THEN ASSIGN lcStatus = "Down".
   
   IF lcStatus           EQ ""               AND
      liThreadsDown      GT 0
   THEN ASSIGN lcStatus = "Partially Down".
   
   IF lcStatus           EQ ""               AND
      liThreadsSuspended GT 0                AND
      liThreadsSuspended EQ liThreadsLinked
   THEN ASSIGN lcStatus = "Suspended".
   
   IF lcStatus           EQ ""               AND
      liThreadsSuspended GT 0
   THEN ASSIGN lcStatus = "Partially Suspended".
   
   IF lcStatus           EQ ""               AND
      liThreadsUp        GT 0                AND
      liThreadsUp        EQ liThreadsLinked
   THEN ASSIGN lcStatus = "Up".
   
   IF lcStatus EQ ""
   THEN ASSIGN lcStatus = "Unknown".
   
   CASE ipcMode:
      WHEN "threadStatus"
      THEN DO:
         MESSAGE SUBSTITUTE("Pro2Replication Thread Status in database ""&1"" as of &2",
                            vcPro2Database,
                            ISO-DATE(NOW)).
                            
         MESSAGE SUBSTITUTE("Total Replication Threads      : &1", liThreadsLinked).
         MESSAGE SUBSTITUTE("Replication Threads Up         : &1", liThreadsUp).
         MESSAGE SUBSTITUTE("Replication Threads Down       : &1", liThreadsDown).
      END.
      
      WHEN "tableStatus"
      THEN DO:
         MESSAGE SUBSTITUTE("Pro2Replication Table Status in database ""&1"" as of &2",
                            vcPro2Database,
                            ISO-DATE(NOW)).
                            
         MESSAGE SUBSTITUTE("Total Tables                  : &1", liTablesLinkedTotal).
         MESSAGE SUBSTITUTE("Table Replication Up          : &1", liTablesActiveTotal - liTablesSuspendedTotal).
         MESSAGE SUBSTITUTE("Table Replication Suspended   : &1", liTablesSuspendedTotal).
         MESSAGE SUBSTITUTE("Table Replication Deactivated : &1", liTablesLinkedTotal - liTablesActiveTotal - liTablesDownTotal).
         MESSAGE SUBSTITUTE("Table Replication Down        : &1", liTablesDownTotal).
      END.
      
      WHEN "pro2Status"
      THEN DO:
         MESSAGE SUBSTITUTE("Pro2Replication Status in database ""&1"" as of &2" + CHR(10) +
                            "Status=&3",
                            vcPro2Database,
                            ISO-DATE(NOW),
                            lcStatus).
      END.
      WHEN "pro2Status BOOLEAN"
      THEN DO:
         MESSAGE STRING(NOT lcStatus MATCHES "*Down*","1/0").
      END.
   END CASE.   
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-showThreadTableStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION showThreadTableStatus Procedure 
FUNCTION showThreadTableStatus RETURNS LOGICAL
   (INPUT                  ipcMode                 AS    CHARACTER):
    
   DEFINE VARIABLE         lhBuffer                AS    HANDLE         NO-UNDO.
   DEFINE VARIABLE         lhQuery                 AS    HANDLE         NO-UNDO.
   
   DEFINE VARIABLE         lcTableName             AS    CHARACTER      NO-UNDO.
   DEFINE VARIABLE         lcQuery                 AS    CHARACTER      NO-UNDO.
   
   DEFINE VARIABLE         lcLine                  AS    CHARACTER      NO-UNDO.
   
   DEFINE VARIABLE         llThreadEnabled         AS    LOGICAL        NO-UNDO.
   DEFINE VARIABLE         llThreadRunning         AS    LOGICAL        NO-UNDO.
   
   DEFINE VARIABLE         liQueueNonApplied       AS    INT64          NO-UNDO.
   DEFINE VARIABLE         liQueueApplied          AS    INT64          NO-UNDO.
   
   DEFINE VARIABLE         lcStatus                AS    CHARACTER      NO-UNDO.
   
   ASSIGN lcTableName = SUBSTITUTE("&1.&2", vcPro2Database, "ReplTableXref")
          lcQuery     = SUBSTITUTE("FOR EACH &1 "                       +
                                   "    NO-LOCK "                       +
                                   "    BREAK BY &1.QThread "           +
                                   "          BY &1.SrcDB "             +
                                   "          BY &1.SrcTable ",
                                   lcTableName).
   
   CASE ipcMode:
      WHEN "tableList"
      THEN DO:
         MESSAGE SUBSTITUTE("Pro2Replication Table List in database ""&1"" as of &2",
                            vcPro2Database,
                            ISO-DATE(NOW)).
         
         ASSIGN lcLine                    = ""
                OVERLAY(lcLine, 068, 011) = "      Queue"
                .
         MESSAGE lcLine.
         
         ASSIGN lcLine                    = ""
                OVERLAY(lcLine, 001, 006) = "Thread"
                OVERLAY(lcLine, 009, 012) = "Database"
                OVERLAY(lcLine, 023, 030) = "Table"
                OVERLAY(lcLine, 055, 025) = "Replication"
                OVERLAY(lcLine, 068, 011) = "Non-Applied"
                .
         MESSAGE lcLine.
      END.
   END CASE.
   
   CREATE BUFFER lhBuffer FOR TABLE lcTableName.
   CREATE QUERY lhQuery.
   
   lhQuery:SET-BUFFERS(lhBuffer).
   lhQuery:QUERY-PREPARE(lcQuery).
   lhQuery:QUERY-OPEN().
   lhQuery:GET-FIRST().
   
   replicationTable:
   DO WHILE TRUE:
      IF lhQuery:QUERY-OFF-END
      THEN LEAVE replicationTable.
      
      IF lhQuery:FIRST-OF(1)
      THEN DO:
         getThreadStatus
            (INPUT  lhBuffer::QThread,
             OUTPUT llThreadEnabled,
             OUTPUT llThreadRunning)
             .
      END.

      getThreadQueueInfo
         (INPUT  lhBuffer::QThread,
          INPUT  lhBuffer::SrcDB,
          INPUT  lhBuffer::SrcTable,
          INPUT  FALSE,
          OUTPUT liQueueNonApplied,
          OUTPUT liQueueApplied).
      
      ASSIGN lcStatus = "".
          
      IF lcStatus             EQ ""    AND
         llThreadEnabled      NE TRUE
      THEN ASSIGN lcStatus = "Disabled*".
      
      IF lcStatus             EQ ""    AND
         lhBuffer::GenQrec    NE TRUE
      THEN ASSIGN lcStatus = "Disabled".
      
      IF lcStatus             EQ ""    AND
         llThreadRunning      NE TRUE
      THEN ASSIGN lcStatus = "Down".
      
      IF lcStatus             EQ ""    AND
         lhBuffer::GenQrec    EQ TRUE  AND
         lhBuffer::ProcQRec   NE TRUE
      THEN ASSIGN lcStatus = "Suspended".
      
      IF lcStatus             EQ ""    AND
         lhBuffer::GenQrec    EQ TRUE
      THEN ASSIGN lcStatus = "Enabled".
      
      IF lcStatus             EQ ""
      THEN ASSIGN lcStatus = "Unknown".
      
      ASSIGN lcLine = ""
             OVERLAY(lcLine, 001, 006) = SUBSTITUTE("&1", lhBuffer::QThread)
             OVERLAY(lcLine, 009, 012) = SUBSTITUTE("&1", lhBuffer::SrcDB)
             OVERLAY(lcLine, 023, 030) = SUBSTITUTE("&1", lhBuffer::SrcTable)
             OVERLAY(lcLine, 055, 011) = SUBSTITUTE("&1", lcStatus)
             OVERLAY(lcLine, 068, 011) = SUBSTITUTE("&1", STRING(liQueueNonApplied, ">>>,>>>,>>9"))
             .
      MESSAGE lcLine.
      
      lhQuery:GET-NEXT().
   END.
   
   lhQuery:QUERY-CLOSE().
   lhBuffer:BUFFER-RELEASE().
   
   DELETE OBJECT lhBuffer NO-ERROR.
   DELETE OBJECT lhQuery NO-ERROR.

   RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-threadEnabled) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION threadEnabled Procedure 
FUNCTION threadEnabled RETURNS LOGICAL
   (INPUT                  ipiThread            AS    INTEGER):
    
   DEFINE VARIABLE         lhBuffer          AS    HANDLE         NO-UNDO.
   DEFINE VARIABLE         lhQuery           AS    HANDLE         NO-UNDO.
   
   DEFINE VARIABLE         lcTableName       AS    CHARACTER      NO-UNDO.
   DEFINE VARIABLE         lcQuery           AS    CHARACTER      NO-UNDO.
   
   DEFINE VARIABLE         llThreadEnabled   AS    LOGICAL        NO-UNDO.
   
   /* Extended Threads are controlled from thread 5 */
   ASSIGN ipiThread   = MIN(5, ipiThread).
   
   ASSIGN lcTableName = SUBSTITUTE("&1.&2", vcPro2Database, "ReplControl")
          lcQuery     = SUBSTITUTE("FOR EACH &1 "                                 +
                                   "    WHERE &1.GroupId  EQ 'CONTROL'     AND "  +
                                   "          &1.CodeId   EQ 'REPLICATION' AND "  +
                                   "          &1.CodeVal1 EQ '&2' "               +
                                   "    NO-LOCK ",
                                   lcTableName,
                                   ipiThread).
   
   CREATE BUFFER lhBuffer FOR TABLE lcTableName.
   CREATE QUERY lhQuery.
   
   lhQuery:SET-BUFFERS(lhBuffer).
   lhQuery:QUERY-PREPARE(lcQuery).
   lhQuery:QUERY-OPEN().
   lhQuery:GET-FIRST().
   
   ASSIGN llThreadEnabled = ?
          .
   
   replicationControl:
   DO WHILE TRUE:
      IF lhQuery:QUERY-OFF-END
      THEN LEAVE replicationControl.
      
      ASSIGN llThreadEnabled = lhBuffer::CodeVal3 NE "0".
      
      LEAVE replicationControl.
   END.
   
   lhQuery:QUERY-CLOSE().
   lhBuffer:BUFFER-RELEASE().
   
   DELETE OBJECT lhBuffer NO-ERROR.
   DELETE OBJECT lhQuery NO-ERROR.

   RETURN llThreadEnabled.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-threadRunning) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION threadRunning Procedure 
FUNCTION threadRunning RETURNS LOGICAL
   (INPUT                  ipiThread            AS    INTEGER):
    
   DEFINE VARIABLE         lhBuffer          AS    HANDLE         NO-UNDO.
   DEFINE VARIABLE         lhQuery           AS    HANDLE         NO-UNDO.
   
   DEFINE VARIABLE         lcTableName       AS    CHARACTER      NO-UNDO.
   DEFINE VARIABLE         lcQuery           AS    CHARACTER      NO-UNDO.
   
   DEFINE VARIABLE         llThreadRunning   AS    LOGICAL        NO-UNDO.
   
   ASSIGN lcTableName = SUBSTITUTE("&1.&2", vcPro2Database, "ReplControl")
          lcQuery     = SUBSTITUTE("FOR EACH &1 "                                 +
                                   "    WHERE &1.GroupId  EQ 'PROCESS'     AND "  +
                                   "          &1.CodeId   EQ 'REPLICATION' AND "  +
                                   "          &1.CodeVal1 EQ '&2' "               +
                                   "    NO-LOCK ",
                                   lcTableName,
                                   ipiThread).
   
   CREATE BUFFER lhBuffer FOR TABLE lcTableName.
   CREATE QUERY lhQuery.
   
   lhQuery:SET-BUFFERS(lhBuffer).
   lhQuery:QUERY-PREPARE(lcQuery).
   lhQuery:QUERY-OPEN().
   lhQuery:GET-FIRST().
   
   ASSIGN llThreadRunning = ?
          .
   
   replicationProcess:
   DO WHILE TRUE:
      IF lhQuery:QUERY-OFF-END
      THEN LEAVE replicationProcess.
      
      ASSIGN llThreadRunning = isLocked(lhQuery,lhBuffer).
      
      LEAVE replicationProcess.
   END.
   
   lhQuery:QUERY-CLOSE().
   lhBuffer:BUFFER-RELEASE().
   
   DELETE OBJECT lhBuffer NO-ERROR.
   DELETE OBJECT lhQuery NO-ERROR.

   RETURN llThreadRunning.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

