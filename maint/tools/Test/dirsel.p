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
DEFINE {&definitionOut} opcDirectory         AS    CHARACTER      NO-UNDO.

DEFINE VARIABLE         lcTitle              AS    CHARACTER      NO-UNDO.
DEFINE VARIABLE         lcDirectory          AS    CHARACTER      NO-UNDO.

&IF "{&dialogMode}" EQ "CUSTOM"
&THEN
DEFINE VARIABLE         lcDirectorySelected  AS    CHARACTER      NO-UNDO.
DEFINE VARIABLE         lcDirectoryList      AS    CHARACTER      NO-UNDO.

DEFINE TEMP-TABLE       DirectoryList                             NO-UNDO
       FIELD            DirectoryOrder       AS    INTEGER
       FIELD            DirectoryName        AS    CHARACTER
       INDEX            DirectoryOrder       IS    PRIMARY
                        DirectoryOrder             ASCENDING
                        DirectoryName              ASCENDING.
DEFINE BUTTON           btnOK
       AUTO-GO
       LABEL "OK" 
       SIZE-CHARS 10 BY 1.

DEFINE BUTTON           btnCancel
       AUTO-ENDKEY
       LABEL "Cancel" 
       SIZE-CHARS 10 BY 1.

DEFINE FRAME frDirectory
       lcDirectory         AT ROW 2 COL 2 
                           FORMAT "X(1024)"
                           VIEW-AS FILL-IN
                           SIZE-CHARS 68 BY 1
       lcDirectoryList     AT ROW 3 COL 2
                           VIEW-AS SELECTION-LIST
                           SIZE-CHARS 68 BY 13
                           SINGLE 
                           SCROLLBAR-VERTICAL
       "Folder:"           AT ROW 16 COL 2
       lcDirectorySelected FORMAT "X(60)"
       btnOK               AT ROW 18 COL 2
       btnCancel
       WITH NO-LABELS VIEW-AS DIALOG-BOX THREE-D CENTERED SIZE-CHARS 72 BY 20
       TITLE " " + lcTitle + " ".

ON RETURN OF lcDirectoryList IN FRAME frDirectory
DO:
   RUN listDirectory (SELF:SCREEN-VALUE).
END.

ON MOUSE-SELECT-DBLCLICK OF lcDirectoryList IN FRAME frDirectory
DO:
   RUN listDirectory (SELF:SCREEN-VALUE).
END.

ON RETURN OF lcDirectory IN FRAME frDirectory
DO:
   RUN setDirectory (SELF:SCREEN-VALUE).
   RETURN NO-APPLY.
END.

ON GO OF lcDirectory IN FRAME frDirectory
DO:
   RUN setDirectory (SELF:SCREEN-VALUE).
   RETURN NO-APPLY.
END.

ON VALUE-CHANGED OF lcDirectoryList IN FRAME frDirectory
DO:
   RUN selectDirectory (SELF:SCREEN-VALUE).
END.

PROCEDURE selectDirectory:
   DEFINE INPUT  PARAMETER ipcDirectory   AS    CHARACTER      NO-UNDO.
   
   DO WITH FRAME frDirectory:
      ASSIGN FILE-INFO:FILENAME  = lcDirectory + (IF ipcDirectory NE ".."
                                                  THEN CHR(47) + ipcDirectory
                                                  ELSE "")
             lcDirectorySelected = FILE-INFO:FULL-PATHNAME
             .
         
      DISPLAY lcDirectory
              lcDirectorySelected
              .
      
      IF lcDirectorySelected:SCREEN-VALUE NE lcDirectorySelected
      THEN ASSIGN lcDirectorySelected:SCREEN-VALUE = "..." + 
                  SUBSTRING(lcDirectorySelected,LENGTH(lcDirectorySelected) - LENGTH(lcDirectorySelected:SCREEN-VALUE) + 4).
   END.
END PROCEDURE.

PROCEDURE setDirectory:
   DEFINE INPUT  PARAMETER ipcDirectory      AS    CHARACTER      NO-UNDO.
   
   DO WITH FRAME frDirectory:
      ASSIGN FILE-INFO:FILENAME = ipcDirectory.
      
      IF FILE-INFO:FULL-PATHNAME EQ ?
      THEN DO:
         MESSAGE SUBSTITUTE("Directory &1 does not exist.", ipcDirectory)
                 VIEW-AS ALERT-BOX WARNING.
         RETURN "".
      END.
      
      IF NOT INDEX(FILE-INFO:FILE-TYPE,"D") GT 0
      THEN DO:
         MESSAGE SUBSTITUTE("&1 is not a directory.", ipcDirectory)
                 VIEW-AS ALERT-BOX WARNING.
         RETURN "".
      END.
      
      ASSIGN lcDirectory = FILE-INFO:FULL-PATHNAME.
      
      RUN listDirectory(".").
   END.
END PROCEDURE.

PROCEDURE listDirectory:
   DEFINE INPUT  PARAMETER ipcDirectory      AS    CHARACTER      NO-UNDO.
   
   DEFINE VARIABLE         lcFile            AS    CHARACTER      NO-UNDO
                                                   EXTENT 3.
   DEFINE VARIABLE         lcDirectoryParent AS    CHARACTER      NO-UNDO.
   DEFINE VARIABLE         llHasParent       AS    LOGICAL        NO-UNDO.
   
   DEFINE VARIABLE         liCounter         AS    INTEGER        NO-UNDO.
   
   DEFINE VARIABLE         lhDirectoryList   AS    HANDLE         NO-UNDO.
   
   CREATE SELECTION-LIST lhDirectoryList.
   ASSIGN lhDirectoryList:SORT      = TRUE
          lhDirectoryList:HIDDEN    = TRUE
          lhDirectoryList:FRAME     = FRAME frDirectory:HANDLE
          lhDirectoryList:DELIMITER = CHR(1)
          .
      
   DO WITH FRAME frDirectory:
      ASSIGN lcDirectory = REPLACE(lcDirectory, CHR(92), CHR(47)).
      
      /*
      IF LOOKUP(TRIM(lcDirectory, CHR(47)), OS-DRIVES) GT 0
      THEN ASSIGN lcDirectory = ENTRY(LOOKUP(TRIM(lcDirectory, CHR(47)), OS-DRIVES), OS-DRIVES).
      */
      IF ipcDirectory EQ ".."
      THEN DO:
         ASSIGN lcDirectoryParent = ENTRY(NUM-ENTRIES(lcDirectory, CHR(47)), lcDirectory, CHR(47)).
         
         IF LOOKUP(TRIM(lcDirectory, CHR(47)), OS-DRIVES) GT 0
         THEN ASSIGN lcDirectoryParent = TRIM(lcDirectory, CHR(47))
                     lcDirectory       = ""
                     .
      END.
      
      IF lcDirectory  NE ""   OR
         ipcDirectory NE ".."
      THEN DO:
         ASSIGN FILE-INFO:FILENAME = (IF lcDirectory NE ""
                                      THEN lcDirectory + CHR(47)
                                      ELSE "") + 
                                     ipcDirectory + CHR(47) + "."
                lcDirectory        = FILE-INFO:FULL-PATHNAME
                .
      END.
      
      IF ipcDirectory EQ ".."
      THEN DO:
         IF LOOKUP(TRIM(REPLACE(lcDirectory, CHR(92), CHR(47)), CHR(47)), OS-DRIVES) GT 0
         THEN ASSIGN lcDirectoryParent = TRIM(REPLACE(lcDirectory, CHR(92), CHR(47)), CHR(47)).
      END.
      
      IF lcDirectory NE ""
      THEN DO:
         INPUT CLOSE.
         INPUT FROM OS-DIR(lcDirectory).
         osDirectory:
         REPEAT:
            IMPORT lcFile.
            
            IF lcFile[1] EQ "."
            THEN NEXT osDirectory.
            
            IF NOT INDEX(lcFile[3], "D") GT 0
            THEN NEXT osDirectory.
            
            IF lcFile[1] EQ ".."
            THEN DO:
               ASSIGN llHasParent = TRUE.
               NEXT osDirectory.
            END.
            
            lhDirectoryList:ADD-LAST(lcFile[1]).
         END.
         INPUT CLOSE.
      END.
      ELSE DO:
         osDrive:
         DO liCounter = 1 TO NUM-ENTRIES(OS-DRIVES):
            ASSIGN FILE-INFO:FILE-NAME = ENTRY(liCounter, OS-DRIVES) + "/.".
            
            IF NOT INDEX(FILE-INFO:FILE-TYPE, "D") GT 0
            THEN NEXT osDrive.
            
            IF NOT INDEX(FILE-INFO:FILE-TYPE, "R") GT 0
            THEN NEXT osDrive.
            
            lhDirectoryList:ADD-LAST(ENTRY(liCounter, OS-DRIVES)).
         END.
      END.
      
      ASSIGN lcDirectoryList:LIST-ITEMS = lhDirectoryList:LIST-ITEMS.
      DELETE OBJECT lhDirectoryList.
      
      IF llHasParent EQ TRUE
      THEN lcDirectoryList:HANDLE:ADD-FIRST("..").
      ELSE DO:
         IF OPSYS EQ "WIN32"
         THEN DO:
            IF lcDirectory NE ""
            THEN lcDirectoryList:HANDLE:ADD-FIRST("..").
         END.
      END.
      
      ASSIGN lcDirectoryList    :SCREEN-VALUE = ENTRY(MAX(1,LOOKUP(lcDirectoryParent, lcDirectoryList:LIST-ITEMS, CHR(1))), lcDirectoryList:LIST-ITEMS, CHR(1))
             lcDirectory        :SCREEN-VALUE = lcDirectory
             .
      
      RUN selectDirectory (lcDirectoryList:SCREEN-VALUE).
         
      ENABLE lcDirectory lcDirectoryList btnOK btnCancel.
      APPLY "ENTRY" TO lcDirectoryList.
   END.
END PROCEDURE.
&ENDIF

ASSIGN lcTitle     = (IF ipcTitle EQ "" OR
                         ipcTitle EQ ?
                      THEN "Browse For Folder"
                      ELSE ipcTitle)
       lcDirectory = (IF ipcDirectoryInitial EQ "" OR
                         ipcDirectoryInitial EQ ?
                      THEN SESSION:TEMP-DIRECTORY
                      ELSE ipcDirectoryInitial).

ASSIGN FILE-INFO:FILENAME = ipcDirectoryInitial.
IF NOT INDEX(FILE-INFO:FILE-TYPE,"D") GT 0
THEN ASSIGN lcDirectory = SESSION:TEMP-DIRECTORY.


&IF "{&dialogMode}" EQ "CUSTOM"
&THEN
selectDirectory:
DO ON END-KEY UNDO selectDirectory, LEAVE selectDirectory
   ON ERROR   UNDO selectDirectory, lEAVE selectDirectory
   WITH FRAME frDirectory:
   
   ASSIGN lcDirectoryList:DELIMITER = CHR(1).
   
   VIEW FRAME frDirectory.
   
   RUN listDirectory (".").
   
   IF NOT THIS-PROCEDURE:PERSISTENT
   THEN WAIT-FOR GO OF FRAME frDirectory.
   
   ASSIGN opcDirectory = lcDirectorySelected.
END.
HIDE FRAME frDirectory.
&ELSE
   SYSTEM-DIALOG GET-DIR opcDirectory
                 INITIAL-DIR lcDirectory
                 TITLE lcTitle.
&ENDIF

&IF "{&definitionOut}" EQ "VARIABLE"
&THEN 
MESSAGE SUBSTITUTE("&1 selected.", (IF opcDirectory EQ "" 
                                    THEN "No directory" 
                                    ELSE opcDirectory))
        VIEW-AS ALERT-BOX INFORMATION.
&ENDIF
