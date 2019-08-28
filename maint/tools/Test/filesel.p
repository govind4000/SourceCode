/* PSC: dirsel.p - Select Directory Dialog                                  */
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
/* REVISION: eB   LAST MODIFIED: 14 Aug 2015  BY: HP/RDU                    */

&IF TRUE
&THEN
   &SCOPED-DEFINE definitionIn INPUT PARAMETER
   &SCOPED-DEFINE definitionOut OUTPUT PARAMETER
   &IF "{&OPSYS}" EQ "WIN32"
   &THEN
      &SCOPED-DEFINE dialogMode {&OPSYS}
   &ELSE
      &SCOPED-DEFINE dialogMode CUSTOM
   &ENDIF
   &SCOPED-DEFINE dialogMode CUSTOM
&ELSE
   &SCOPED-DEFINE definitionIn VARIABLE
   &SCOPED-DEFINE definitionOut VARIABLE
   &IF TRUE
   &THEN
      &SCOPED-DEFINE dialogMode CUSTOM
   &ELSE
      &SCOPED-DEFINE dialogMode {&OPSYS}
   &ENDIF
&ENDIF

DEFINE {&definitionIn}  ipcTitle             AS    CHARACTER      NO-UNDO.
DEFINE {&definitionIn}  ipcDirectoryInitial  AS    CHARACTER      NO-UNDO.
DEFINE VARIABLE         iplSave              AS    LOGICAL        NO-UNDO.
DEFINE {&definitionIn}  ipcFileSpec          AS    CHARACTER      NO-UNDO.
DEFINE {&definitionOut} opcFileName          AS    CHARACTER      NO-UNDO.

DEFINE VARIABLE         llReturnCode         AS    LOGICAL        NO-UNDO.

DEFINE VARIABLE cFilespec AS CHARACTER NO-UNDO.
DEFINE VARIABLE lReturn AS LOGICAL NO-UNDO.

ASSIGN ipcTitle            = (IF ipcTitle EQ "" OR
                                ipcTitle EQ ?
                              THEN SUBSTITUTE("&1 File", STRING(iplSave EQ TRUE, "Save/Open"))
                              ELSE ipcTitle)
       ipcDirectoryInitial = (IF ipcDirectoryInitial EQ "" OR
                                 ipcDirectoryInitial EQ ?
                              THEN SESSION:TEMP-DIRECTORY
                              ELSE ipcDirectoryInitial)
       iplSave             = FALSE
       iplSave             = iplSave EQ TRUE
       ipcFileSpec         = (IF ipcFileSpec EQ "" OR
                                 ipcFileSpec EQ ?
                              THEN ""
                              ELSE ipcFileSpec)
       .

&IF "{&definitionOut}" EQ "VARIABLE"
&THEN 
ASSIGN ipcFileSpec = "*.wrk"
       iplSave = FALSE.
&ENDIF

&IF "{&dialogMode}" EQ "CUSTOM"
&THEN
RUN adeedit/_dlggetf.p
    (INPUT ipcTitle,
     INPUT iplSave,            /* "NO" for open, "YES" for save */
     INPUT 0,                  /* Unused */
     INPUT-OUTPUT opcFileName, /* File to be opened or saved */
     OUTPUT llReturnCode).     /* Return code */
IF llReturnCode NE TRUE
THEN ASSIGN opcFileName = "".

&ELSE
   IF iplSave NE TRUE
   THEN DO:
      SYSTEM-DIALOG GET-FILE opcFileName
                    INITIAL-DIR ipcDirectoryInitial
                    FILTERS ipcFileSpec ipcFileSpec,
                            "All Types (*.*)" "*.*"
                            INITIAL-FILTER 1
                    MUST-EXIST
                    TITLE ipcTitle.
   END.
   ELSE DO:
      SYSTEM-DIALOG GET-FILE opcFileName
                    INITIAL-DIR ipcDirectoryInitial
                    FILTERS (IF ipcFileSpec NE "" 
                             THEN SUBSTITUTE("File Type (&1)",ipcFileSpec)
                             ELSE "") ipcFileSpec,
                            "All Types (*.*)" "*.*"
                            INITIAL-FILTER 1
                    SAVE-AS
                    ASK-OVERWRITE
                    TITLE ipcTitle.
   END.
&ENDIF

&IF "{&definitionOut}" EQ "VARIABLE"
&THEN 
MESSAGE SUBSTITUTE("&1 selected.", (IF opcFileName EQ "" 
                                    THEN "No file" 
                                    ELSE opcFileName))
        VIEW-AS ALERT-BOX INFORMATION.
&ENDIF
