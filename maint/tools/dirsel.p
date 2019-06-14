DEFINE VARIABLE         lcTitle     AS    CHARACTER      NO-UNDO.

DEFINE VARIABLE         lcDirectorySelected  AS    CHARACTER      NO-UNDO.
DEFINE VARIABLE lcDirectory AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDirectoryList AS CHARACTER LABEL "Directories" NO-UNDO.
DEFINE VARIABLE vDirSeparator AS CHARACTER NO-UNDO.

DEFINE BUTTON           btnOK
       AUTO-GO
       LABEL "OK" 
       SIZE-CHARS 8 BY 1.
DEFINE BUTTON           btnCancel
       AUTO-ENDKEY
       LABEL "Cancel" 
       SIZE-CHARS 8 BY 1.

DEFINE FRAME frDirectory
       /*lcTitle             FORMAT "X(58)"*/
       lcDirectorySelected AT ROW 1 COL 1 FORMAT "X(58)"
       lcDirectoryList     AT ROW 2 COL 1
                           VIEW-AS SELECTION-LIST 
                           SIZE-CHARS 58 BY 9
                           SINGLE 
                           SCROLLBAR-VERTICAL
       SKIP 
       btnOK               AT ROW 11 COL 1
       btnCancel
       WITH NO-LABELS VIEW-AS DIALOG-BOX CENTERED SIZE-CHARS 60 BY 13
       TITLE lcTitle.

ON RETURN OF lcDirectoryList IN FRAME frDirectory
DO:
   ASSIGN FILE-INFO:FILENAME = lcDirectory + vDirSeparator + SELF:SCREEN-VALUE
          lcDirectory = FILE-INFO:PATHNAME.
   RUN list_directories.
END.

ON VALUE-CHANGED OF lcDirectoryList IN FRAME frDirectory
DO: 
MESSAGE                    lcDirectorySelected.
   DO WITH FRAME frDirectory:
      ASSIGN lcDirectorySelected:SCREEN-VALUE = lcDirectory.
      
   END.
END.

IF OPSYS = "UNIX" 
THEN vDirSeparator = CHR(47).
ELSE vDirSeparator = CHR(92).

ASSIGN lcTitle     = "Encrypted Source Directory"
       lcDirectory = SESSION:TEMP-DIRECTORY.


DO WITH FRAME frDirectory.
   VIEW FRAME frDirectory.
   
   RUN list_directories.
   
   ENABLE lcDirectoryList btnOK btnCancel.
   WAIT-FOR GO, END-ERROR, WINDOW-CLOSE OF FRAME frDirectory.
   
   ASSIGN FILE-INFO:FILENAME = lcDirectory + vDirSeparator + lcDirectoryList:SCREEN-VALUE
   lcDirectory = FILE-INFO:PATHNAME.
   MESSAGE "Directory selected =" lcDirectory VIEW-AS ALERT-BOX INFORMATION.
END.


PROCEDURE list_directories:
   DEFINE VARIABLE vDirName AS CHARACTER NO-UNDO.
   DEFINE VARIABLE vFiles AS CHARACTER EXTENT 3 NO-UNDO.
   DEFINE VARIABLE vFilesList AS CHARACTER NO-UNDO.
   
   DEFINE VARIABLE         liCounter      AS    INTEGER        NO-UNDO.
   
   INPUT FROM OS-DIR(lcDirectory).
   
   ASSIGN liCounter = 0.
   
   directoryItem:
   REPEAT:
      IMPORT vFiles.
      
      IF vFiles[1] EQ "."
      THEN NEXT directoryItem.
      
      IF NOT INDEX(vFiles[3], "D") GT 0
      THEN NEXT directoryItem.
      
      ASSIGN liCounter = liCounter + 1.
      
      ASSIGN vFilesList = vFilesList + (IF vFilesList NE "" 
                                             THEN "," 
                                             ELSE "") + vFiles[1].
   END.
   INPUT CLOSE.
   
   DO WITH FRAME frDirectory:
      ASSIGN lcDirectoryList:LIST-ITEMS        = vFilesList
             lcDirectoryList:SCREEN-VALUE      = ENTRY(1,lcDirectoryList:LIST-ITEMS)
             lcDirectorySelected:SCREEN-VALUE  = lcDirectory.
         
      APPLY "ENTRY" TO lcDirectoryList.
   END.
END PROCEDURE.

