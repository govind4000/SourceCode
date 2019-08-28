&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/* PSC: xxpersistent.i - Functions for Persistent Procedures                */
/* COPYRIGHT Indigo  ALL RIGHTS RESERVED. THIS IS AN UNPUBLISHED WORK.      */
/*                                                                          */
/* Category : X9                                                            */
/* - A      : Updates Std. MFG/PRO Database                                 */
/* - B      : Updates special Database                                      */
/* - C      : Non-Updating                                                  */
/* - 1      : Modified Std. MFG/PRO Program                                 */
/* - 2      : New Program                                                   */
/*                                                                          */
/*                                                                          */
/* %Z% SCCS-Id:%P% %I% %G% %U% tk                                           */ 
/* REVISION: eB   LAST MODIFIED: 21 Jun 2006  BY: HP/RDU *SR4000*           */

/* ***************************  Definitions  ************************** */

&GLOBAL-DEFINE xxpersistent

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD deletePersistent Include 
FUNCTION deletePersistent RETURNS CHARACTER
  (INPUT                   iphProcedure   AS    HANDLE) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPersistent Include 
FUNCTION getPersistent RETURNS HANDLE
  (INPUT                   ipcFileName    AS    CHARACTER) FORWARD.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION deletePersistent Include 
FUNCTION deletePersistent RETURNS CHARACTER
  (INPUT                   iphProcedure   AS    HANDLE):
   
   IF VALID-HANDLE(iphProcedure)            AND
      CAN-QUERY(iphProcedure, "PERSISTENT") AND
      CAN-QUERY(iphProcedure, "TYPE")
   THEN DO:
      IF iphProcedure:TYPE       EQ "PROCEDURE" AND
         iphProcedure:PERSISTENT EQ TRUE
      THEN DO:
         DELETE PROCEDURE iphProcedure NO-ERROR.
      END.
   END.

   RETURN "".
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPersistent Include 
FUNCTION getPersistent RETURNS HANDLE
  (INPUT                   ipcFileName    AS    CHARACTER):

   DEFINE VARIABLE         lhProcedure    AS    HANDLE         NO-UNDO.
   
   IF ipcFileName EQ "" OR
      ipcFileName EQ ?
   THEN RETURN ?.

   ASSIGN lhProcedure = SESSION:FIRST-PROCEDURE.

   DO WHILE VALID-HANDLE(lhProcedure):
      IF lhProcedure:FILE-NAME  EQ ipcFileName AND
         lhProcedure:PERSISTENT EQ TRUE
      THEN RETURN lhProcedure.

      ASSIGN lhProcedure = lhProcedure:NEXT-SIBLING.
   END.

   IF NUM-ENTRIES(ipcFileName, ".") EQ 2     AND
      ENTRY(2, ipcFileName, ".")    NE "r"
   THEN DO:
      ASSIGN ENTRY(2, ipcFileName, ".") = "r".
      RETURN getPersistent(ipcFileName).
   END.

   RETURN ?.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

