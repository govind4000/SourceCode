&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/* PSC: xxqldap.i - Query Enterprise Directory using LDAP                   */
/* COPYRIGHT Indigo  ALL RIGHTS RESERVED. THIS IS AN UNPUBLISHED WORK.      */
/*                                                                          */
/* Category : C2                                                            */
/* - A      : Updates Std. MFG/PRO Database                                 */
/* - B      : Updates special Database                                      */
/* - C      : Non-Updating                                                  */
/* - 1      : Modified Std. MFG/PRO Program                                 */
/* - 2      : New Program                                                   */
/*                                                                          */
/*                                                                          */
/* %Z% SCCS-Id:%P% %I% %G% %U% tk                                           */
/* REVISION: eB   LAST MODIFIED: 18 Jan 2012  BY: HP/RDU *SR5790*           */

/* ***************************  Definitions  ************************** */

&IF OPSYS EQ "UNIX"
&THEN
&SCOPED-DEFINE ldapExec "/opt/qadee/local/bin/admin/maint/src/ldap.pl"
&ELSE
&SCOPED-DEFINE ldapExec "AdFind.exe"
&ENDIF

&IF DEFINED(ldapReturnFields) EQ 0
&THEN
&SCOPED-DEFINE ldapReturnFields
&ENDIF

DEFINE TEMP-TABLE       ldapd_det                              NO-UNDO
       FIELD            ldapd_result_id   AS    INTEGER
       FIELD            ldapd_attribute   AS    CHARACTER
       FIELD            ldapd_value       AS    CHARACTER
       INDEX            ldapd_attribute   IS    UNIQUE
                        ldapd_attribute         ASCENDING
                        ldapd_result_id         ASCENDING
       INDEX            ldapd_result_id   IS    PRIMARY UNIQUE
                        ldapd_result_id         ASCENDING
                        ldapd_attribute         ASCENDING
                        .

DEFINE VARIABLE         ldapReturnFields  AS    CHARACTER      NO-UNDO.
{xxfilefunc.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapAttributeList Include 
FUNCTION ldapAttributeList RETURNS CHARACTER
   () FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapAttributeValue Include 
FUNCTION ldapAttributeValue RETURNS CHARACTER
   (INPUT                  ipiResultId       AS    INTEGER,
    INPUT                  ipcAttribute      AS    CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapCurrentResultId Include 
FUNCTION ldapCurrentResultId RETURNS INTEGER
   () FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapExec Include 
FUNCTION ldapExec RETURNS CHARACTER
   () FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapFirstResultId Include 
FUNCTION ldapFirstResultId RETURNS INTEGER
   () FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapFirstResultIdByValue Include 
FUNCTION ldapFirstResultIdByValue RETURNS INTEGER
   (INPUT                  ipcAttribute      AS    CHARACTER,
    INPUT                  ipcValue          AS    CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapFlushResults Include 
FUNCTION ldapFlushResults RETURNS LOGICAL
   () FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapInfoGet Include 
FUNCTION ldapInfoGet RETURNS CHARACTER
   (INPUT                  ipcInfoLDAP    AS    CHARACTER,
    INPUT                  ipcAttribute   AS    CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapInfoSet Include 
FUNCTION ldapInfoSet RETURNS CHARACTER
   (INPUT                  ipcInfoLDAP    AS    CHARACTER,
    INPUT                  ipcAttribute   AS    CHARACTER,
    INPUT                  ipcValue       AS    CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapInitializeProcedure Include 
FUNCTION ldapInitializeProcedure RETURNS CHARACTER
   () FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapLastResultIdByValue Include 
FUNCTION ldapLastResultIdByValue RETURNS INTEGER
   (INPUT                  ipcAttribute      AS    CHARACTER,
    INPUT                  ipcValue          AS    CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapNewResultId Include 
FUNCTION ldapNewResultId RETURNS INTEGER
   () FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapNextResultIdByValue Include 
FUNCTION ldapNextResultIdByValue RETURNS INTEGER
   (INPUT                  ipiResultId       AS    INTEGER,
    INPUT                  ipcAttribute      AS    CHARACTER,
    INPUT                  ipcValue          AS    CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapNumResults Include 
FUNCTION ldapNumResults RETURNS INTEGER
   () FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapNumResultsByValue Include 
FUNCTION ldapNumResultsByValue RETURNS INTEGER
   (INPUT                  ipcAttribute      AS    CHARACTER,
    INPUT                  ipcValue          AS    CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapPreviousResultIdByValue Include 
FUNCTION ldapPreviousResultIdByValue RETURNS INTEGER
   (INPUT                  ipiResultId       AS    INTEGER,
    INPUT                  ipcAttribute      AS    CHARACTER,
    INPUT                  ipcValue          AS    CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapPrevResultIdByValue Include 
FUNCTION ldapPrevResultIdByValue RETURNS INTEGER
   (INPUT                  ipiResultId       AS    INTEGER,
    INPUT                  ipcAttribute      AS    CHARACTER,
    INPUT                  ipcValue          AS    CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapQuery Include 
FUNCTION ldapQuery RETURNS INTEGER
   (INPUT                  ipcSearchFilter   AS    CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapResetProcedure Include 
FUNCTION ldapResetProcedure RETURNS CHARACTER
   () FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapResult Include 
FUNCTION ldapResult RETURNS CHARACTER
   (INPUT                  ipiResultId       AS    INTEGER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapResultAttribute Include 
FUNCTION ldapResultAttribute RETURNS CHARACTER
   (INPUT                  ipiResultId       AS    INTEGER,
    INPUT                  ipcAttributeList  AS    CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapResultId Include 
FUNCTION ldapResultId RETURNS INTEGER
   (INPUT                  ipcRequest        AS    CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapResultIdByValue Include 
FUNCTION ldapResultIdByValue RETURNS INTEGER
   (INPUT                  ipcRequest        AS    CHARACTER,
    INPUT                  ipiResultId       AS    INTEGER,
    INPUT                  ipcAttribute      AS    CHARACTER,
    INPUT                  ipcValue          AS    CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapSetAttribute Include 
FUNCTION ldapSetAttribute RETURNS LOGICAL
   (INPUT                  ipcLine           AS    CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapSetReturnFields Include 
FUNCTION ldapSetReturnFields RETURNS CHARACTER
   (INPUT                  ipcReturnFields   AS    CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ldapUniqueResultIdByValue Include 
FUNCTION ldapUniqueResultIdByValue RETURNS INTEGER
   (INPUT                  ipcAttribute      AS    CHARACTER,
    INPUT                  ipcValue          AS    CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 24.95
         WIDTH              = 61.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

ldapInitializeProcedure().

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapAttributeList Include 
FUNCTION ldapAttributeList RETURNS CHARACTER
   ():
    
   DEFINE VARIABLE         lcAttributeList   AS    CHARACTER      NO-UNDO.
   
   DEFINE BUFFER           ldapd_det         FOR   ldapd_det.
   
   FOR EACH ldapd_det
       NO-LOCK
       BREAK BY ldapd_det.ldapd_attribute:
       
      IF FIRST-OF(ldapd_det.ldapd_attribute)
      THEN ASSIGN lcAttributeList = lcAttributeList            +
                                    (IF lcAttributeList EQ ""
                                     THEN ""
                                     ELSE ",")                 +
                                    ldapd_det.ldapd_attribute.
   END.
   
   RETURN lcAttributeList.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapAttributeValue Include 
FUNCTION ldapAttributeValue RETURNS CHARACTER
   (INPUT                  ipiResultId       AS    INTEGER,
    INPUT                  ipcAttribute      AS    CHARACTER):
    
   DEFINE VARIABLE         lcAttribute       AS    CHARACTER      NO-UNDO.
   DEFINE VARIABLE         lcValue           AS    CHARACTER      NO-UNDO.
   
   DEFINE BUFFER           ldapd_det         FOR   ldapd_det.
   
   FIND ldapd_det
        WHERE ldapd_det.ldapd_result_id EQ ipiResultId   AND
              ldapd_det.ldapd_attribute EQ ipcAttribute
        NO-LOCK NO-ERROR.
   IF AVAILABLE(ldapd_det)
   THEN RETURN ldapd_det.ldapd_value.
          
   RETURN "".
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapCurrentResultId Include 
FUNCTION ldapCurrentResultId RETURNS INTEGER
   ():

   RETURN ldapResultId("CURRENT").
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapExec Include 
FUNCTION ldapExec RETURNS CHARACTER
   ():

   IF isFile({&ldapExec})     AND
      isReadable({&ldapExec})
   THEN RETURN getFullPathName({&ldapExec}).   
   
   RETURN ?.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapFirstResultId Include 
FUNCTION ldapFirstResultId RETURNS INTEGER
   ():

   RETURN ldapResultId("FIRST").
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapFirstResultIdByValue Include 
FUNCTION ldapFirstResultIdByValue RETURNS INTEGER
   (INPUT                  ipcAttribute      AS    CHARACTER,
    INPUT                  ipcValue          AS    CHARACTER):

  RETURN ldapResultIdByValue("FIRST",?,ipcAttribute,ipcValue).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapFlushResults Include 
FUNCTION ldapFlushResults RETURNS LOGICAL
   ():
   DEFINE BUFFER           ldapd_det         FOR   ldapd_det.
   
   EMPTY TEMP-TABLE ldapd_det.
   
   RETURN ldapNumResults() EQ 0.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapInfoGet Include 
FUNCTION ldapInfoGet RETURNS CHARACTER
   (INPUT                  ipcInfoLDAP    AS    CHARACTER,
    INPUT                  ipcAttribute   AS    CHARACTER):
   
   IF ipcInfoLDAP  EQ ?  OR
      ipcAttribute EQ "" OR
      ipcAttribute EQ ?
   THEN RETURN "".
   
   IF INDEX(ipcInfoLDAP,
            SUBSTITUTE("&1&2=",CHR(1),ipcAttribute)) NE 0
   THEN ASSIGN ipcInfoLDAP = SUBSTRING(ipcInfoLDAP,
                                       INDEX(ipcInfoLDAP,
                                             SUBSTITUTE("&1&2=",CHR(1),
                                                        ipcAttribute)) + 1).
   
   ASSIGN ipcInfoLDAP = ENTRY(1,ipcInfoLDAP,CHR(1)).
   
   IF NOT ipcInfoLDAP BEGINS SUBSTITUTE("&1=",ipcAttribute)
   THEN RETURN "".
   
   IF NOT ipcInfoLDAP MATCHES "*=*"
   THEN RETURN "".
   
   ASSIGN ipcInfoLDAP = SUBSTRING(ipcInfoLDAP,INDEX(ipcInfoLDAP,"=") + 1).
   
   RETURN ipcInfoLDAP.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapInfoSet Include 
FUNCTION ldapInfoSet RETURNS CHARACTER
   (INPUT                  ipcInfoLDAP    AS    CHARACTER,
    INPUT                  ipcAttribute   AS    CHARACTER,
    INPUT                  ipcValue       AS    CHARACTER):

   IF ipcInfoLDAP EQ ?
   THEN ASSIGN ipcInfoLDAP = "".

   ASSIGN ipcInfoLDAP = SUBSTITUTE("&1&2&3=&4",
                                   ipcInfoLDAP,
                                   FILL(CHR(1),INTEGER(ipcInfoLDAP NE "")),
                                   ipcAttribute,
                                   ipcValue).

   RETURN ipcInfoLDAP.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapInitializeProcedure Include 
FUNCTION ldapInitializeProcedure RETURNS CHARACTER
   ():
   
   ldapSetReturnFields("{&ldapReturnFields}").
   
   RETURN "".
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapLastResultIdByValue Include 
FUNCTION ldapLastResultIdByValue RETURNS INTEGER
   (INPUT                  ipcAttribute      AS    CHARACTER,
    INPUT                  ipcValue          AS    CHARACTER):

  RETURN ldapResultIdByValue("LAST",?,ipcAttribute,ipcValue).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapNewResultId Include 
FUNCTION ldapNewResultId RETURNS INTEGER
   ():

   RETURN ldapResultId("NEW").
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapNextResultIdByValue Include 
FUNCTION ldapNextResultIdByValue RETURNS INTEGER
   (INPUT                  ipiResultId       AS    INTEGER,
    INPUT                  ipcAttribute      AS    CHARACTER,
    INPUT                  ipcValue          AS    CHARACTER):

  RETURN ldapResultIdByValue("NEXT",ipiResultId,ipcAttribute,ipcValue).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapNumResults Include 
FUNCTION ldapNumResults RETURNS INTEGER
   ():

   RETURN ldapResultId("COUNT").
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapNumResultsByValue Include 
FUNCTION ldapNumResultsByValue RETURNS INTEGER
   (INPUT                  ipcAttribute      AS    CHARACTER,
    INPUT                  ipcValue          AS    CHARACTER):

  RETURN ldapResultIdByValue("COUNT",?,ipcAttribute,ipcValue).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapPreviousResultIdByValue Include 
FUNCTION ldapPreviousResultIdByValue RETURNS INTEGER
   (INPUT                  ipiResultId       AS    INTEGER,
    INPUT                  ipcAttribute      AS    CHARACTER,
    INPUT                  ipcValue          AS    CHARACTER):

  RETURN ldapResultIdByValue("PREVIOUS",ipiResultId,ipcAttribute,ipcValue).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapPrevResultIdByValue Include 
FUNCTION ldapPrevResultIdByValue RETURNS INTEGER
   (INPUT                  ipiResultId       AS    INTEGER,
    INPUT                  ipcAttribute      AS    CHARACTER,
    INPUT                  ipcValue          AS    CHARACTER):

  RETURN ldapResultIdByValue("PREVIOUS",ipiResultId,ipcAttribute,ipcValue).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapQuery Include 
FUNCTION ldapQuery RETURNS INTEGER
   (INPUT                  ipcSearchFilter   AS    CHARACTER):

   DEFINE VARIABLE         lcCommandTemplate AS    CHARACTER      NO-UNDO.
   DEFINE VARIABLE         lcCommand         AS    CHARACTER      NO-UNDO.
   
   DEFINE VARIABLE         lcLine            AS    CHARACTER      NO-UNDO.
   DEFINE VARIABLE         lcKey             AS    CHARACTER      NO-UNDO.
   
   DEFINE VARIABLE         llError           AS    LOGICAL        NO-UNDO.
   
   /* Clear previous results */
   ldapFlushResults().
   
   /* If the executable cannot be found return ? */
   IF ldapExec() EQ ?
   THEN RETURN ?.
   
   /* Note that we return all fields as ldapReturnFields is blank */                 
&IF OPSYS EQ "UNIX"
&THEN
   ASSIGN lcCommandTemplate = SUBSTITUTE('&1 "&&1" "&2"',
                                         ldapExec(),ldapReturnFields).
&ELSE
   ASSIGN lcCommandTemplate = SUBSTITUTE('&1 -h ldap.hp.com -p 389 -s subtree -b "o=hp.com" -simple -f "&&1" &2',
                                         ldapExec(),ldapReturnFields).
&ENDIF

   ASSIGN lcCommand = SUBSTITUTE(lcCommandTemplate, ipcSearchFilter)
          .
   
   INPUT THROUGH VALUE(lcCommand) NO-ECHO.
   processResultLine:
   REPEAT:
      IMPORT UNFORMATTED lcLine.
      
      IF ldapSetAttribute(lcLine) EQ ?
      THEN DO:
         ASSIGN llError = TRUE.
         
         ldapFlushResults().
         
         LEAVE processResultLine.
      END.
   END.
   INPUT CLOSE.
   
   /* Below will return -1 in case of error, otherwise the number of results */
   RETURN ldapNumResults() + INTEGER(llError EQ TRUE) * -1.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapResetProcedure Include 
FUNCTION ldapResetProcedure RETURNS CHARACTER
   ():

   ldapFlushResults().
   ldapInitializeProcedure().

   RETURN "".
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapResult Include 
FUNCTION ldapResult RETURNS CHARACTER
   (INPUT                  ipiResultId       AS    INTEGER):
   
   RETURN ldapResultAttribute(ipiResultId,"").
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapResultAttribute Include 
FUNCTION ldapResultAttribute RETURNS CHARACTER
   (INPUT                  ipiResultId       AS    INTEGER,
    INPUT                  ipcAttributeList  AS    CHARACTER):
   
   DEFINE VARIABLE         lcAttribute       AS    CHARACTER      NO-UNDO.
   DEFINE VARIABLE         liAttribute       AS    INTEGER        NO-UNDO.
   DEFINE VARIABLE         lcResult          AS    CHARACTER      NO-UNDO.
   
   
   IF ipcAttributeList EQ ? OR
      ipcAttributeList EQ ""
   THEN ASSIGN ipcAttributeList = ldapAttributeList().
   
   DO liAttribute = 1 TO NUM-ENTRIES(ipcAttributeList):
      ASSIGN lcAttribute = ENTRY(liAttribute,ipcAttributeList)
             lcResult    = lcResult             +
                           (IF lcResult EQ ""
                            THEN ""
                            ELSE CHR(10))       +
                            SUBSTITUTE("&1: &2",
                                       lcAttribute,
                                       ldapAttributeValue(ipiResultId, lcAttribute)).
   END.
   
   RETURN lcResult.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapResultId Include 
FUNCTION ldapResultId RETURNS INTEGER
   (INPUT                  ipcRequest        AS    CHARACTER):

   DEFINE BUFFER           ldapd_det         FOR   ldapd_det.

   FIND LAST ldapd_det
        USE-INDEX ldapd_result_id
        NO-LOCK NO-ERROR.
   
   CASE ipcRequest:
      WHEN "FIRST"
      THEN RETURN INTEGER(AVAILABLE(ldapd_det)).
      
      WHEN "NEW"
      THEN RETURN (IF AVAILABLE(ldapd_det)
                   THEN ldapd_det.ldapd_result_id
                   ELSE 0) + 1.

   END CASE.
   
   RETURN (IF AVAILABLE(ldapd_det)
           THEN ldapd_det.ldapd_result_id
           ELSE 0).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapResultIdByValue Include 
FUNCTION ldapResultIdByValue RETURNS INTEGER
   (INPUT                  ipcRequest        AS    CHARACTER,
    INPUT                  ipiResultId       AS    INTEGER,
    INPUT                  ipcAttribute      AS    CHARACTER,
    INPUT                  ipcValue          AS    CHARACTER):
   
   DEFINE BUFFER           ldapd_det         FOR   ldapd_det.

   IF ipiResultId EQ ?
   THEN ASSIGN ipiResultId = 0.
   
   ASSIGN ipiResultId = MAX(ipiResultId,0).
   
   CASE ipcRequest:
      WHEN "FIRST"
      THEN RETURN ldapResultIdByValue("NEXT",0,ipcAttribute,ipcValue).
      
      WHEN "LAST"
      THEN RETURN ldapResultIdByValue("PREVIOUS",ldapNumResults() + 1,ipcAttribute,ipcValue).
      
      WHEN "NEXT"
      THEN DO:
         FIND FIRST ldapd_det
              WHERE ldapd_det.ldapd_result_id GE ipiResultId + 1 AND
                    ldapd_det.ldapd_attribute EQ ipcAttribute    AND
                    ldapd_det.ldapd_value     EQ ipcValue 
              NO-LOCK NO-ERROR.
      END.
      
      WHEN "PREVIOUS" OR
      WHEN "PREV"
      THEN DO:
         FIND LAST ldapd_det
              WHERE ldapd_det.ldapd_result_id LE ipiResultId - 1 AND
                    ldapd_det.ldapd_attribute EQ ipcAttribute    AND
                    ldapd_det.ldapd_value     EQ ipcValue 
              NO-LOCK NO-ERROR.
      END.
      
      WHEN "UNIQUE"
      THEN DO:
         FIND ldapd_det
              WHERE ldapd_det.ldapd_attribute EQ ipcAttribute    AND
                    ldapd_det.ldapd_value     EQ ipcValue 
              NO-LOCK NO-ERROR.
      END.
      
      WHEN "COUNT"
      THEN DO:
         FOR EACH ldapd_det
             WHERE ldapd_det.ldapd_attribute EQ ipcAttribute    AND
                   ldapd_det.ldapd_value     EQ ipcValue 
             NO-LOCK:
            ACCUMULATE 1 (COUNT). 
         END.
         RETURN (ACCUM COUNT 1).
      END.
   END.
   
   RETURN (IF AMBIGUOUS(ldapd_det)
           THEN -1
           ELSE (IF AVAIL(ldapd_det)
                 THEN ldapd_det.ldapd_result_id
                 ELSE 0)).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapSetAttribute Include 
FUNCTION ldapSetAttribute RETURNS LOGICAL
   (INPUT                  ipcLine           AS    CHARACTER):
   
   DEFINE VARIABLE         liResultId        AS    INTEGER        NO-UNDO.
   DEFINE VARIABLE         lcAttribute       AS    CHARACTER      NO-UNDO.
   DEFINE VARIABLE         lcValue           AS    CHARACTER      NO-UNDO.
   
   DEFINE BUFFER           ldapd_det         FOR   ldapd_det.

   /* Return ? in case of failure */
   IF ipcLine MATCHES "*Server encountered an internal error*" OR
      ipcLine MATCHES "*Failed to connect to ldap.hp.com*"
   THEN RETURN ?.
   
   /* Convert result key to attribute */
   IF ipcLine BEGINS "dn:"
   THEN ASSIGN ipcLine = ">dn: " + SUBSTRING(ipcLine, 4).
   
   /* Only process attributes, return FALSE when no attribute */
   IF NOT ipcLine MATCHES ">*: *"
   THEN RETURN FALSE.
   
   /* Set attribute and attribute value */
   ASSIGN lcAttribute          = SUBSTRING(ENTRY(1,ipcLine,":"),2)
          lcValue              = ipcLine
          ENTRY(1,lcValue,":") = ""
          lcValue              = SUBSTRING(lcValue,3)
          .
   
   /* Get a new Result-id for each new result */       
   IF lcAttribute EQ "dn"
   THEN ASSIGN liResultId = ldapNewResultId().
   ELSE ASSIGN liResultId = ldapCurrentResultId().
             
   FIND ldapd_det
        WHERE ldapd_det.ldapd_result_id EQ liResultId  AND
              ldapd_det.ldapd_attribute EQ lcAttribute
        EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE(ldapd_det)
   THEN DO:
      CREATE ldapd_det.
      ASSIGN ldapd_det.ldapd_result_id = liResultId
             ldapd_det.ldapd_attribute = lcAttribute.
   END.
          
   ASSIGN ldapd_det.ldapd_value = ldapd_det.ldapd_value                          +
                                  TRIM(STRING(ldapd_det.ldapd_value EQ "","/:")) +
                                  lcValue.
            
   RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapSetReturnFields Include 
FUNCTION ldapSetReturnFields RETURNS CHARACTER
   (INPUT                  ipcReturnFields   AS    CHARACTER):
   
   ASSIGN ldapReturnFields = (IF ipcReturnFields EQ ?
                              THEN ""
                              ELSE ipcReturnFields).
   
   RETURN "".
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ldapUniqueResultIdByValue Include 
FUNCTION ldapUniqueResultIdByValue RETURNS INTEGER
   (INPUT                  ipcAttribute      AS    CHARACTER,
    INPUT                  ipcValue          AS    CHARACTER):

  RETURN ldapResultIdByValue("UNIQUE",?,ipcAttribute,ipcValue).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

