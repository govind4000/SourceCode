/* PSC: xxencrypted.p                                                       */
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
/* REVISION: eB   LAST MODIFIED: 11 Aug 2015  BY: HP/RDU                    */

DEFINE VARIABLE         lcDirectorySource    AS    CHARACTER      NO-UNDO.
DEFINE VARIABLE         lcExtensionList      AS    CHARACTER      NO-UNDO.

DEFINE VARIABLE         lcLogFile            AS    CHARACTER      NO-UNDO.
DEFINE VARIABLE         llLogFile            AS    LOGICAL        NO-UNDO.
DEFINE VARIABLE         llMailLog            AS    LOGICAL        NO-UNDO.
DEFINE VARIABLE         lcEncryption         AS    CHARACTER      NO-UNDO.

DEFINE TEMP-TABLE       ttDir                                     NO-UNDO
       FIELD            DirectoryName        AS    CHARACTER
       .
DEFINE STREAM           stFileList.
DEFINE STREAM           stFile.
DEFINE STREAM           stLogFile.

&IF OPSYS EQ "UNIX"
&THEN
/*{xxmfghost.i}*/
&ENDIF

FUNCTION firstByte RETURNS INTEGER
   (INPUT                  ipcFileName       AS    CHARACTER):
   
   DEFINE VARIABLE         lmFileBuffer      AS    MEMPTR         NO-UNDO.
   DEFINE VARIABLE         liFirstByte       AS    INTEGER        NO-UNDO.

   SET-SIZE(lmFileBuffer) = 1.

   INPUT STREAM stFile FROM VALUE(ipcFileName) BINARY NO-CONVERT.
   IMPORT STREAM stFile lmFileBuffer.
   INPUT STREAM stFile CLOSE.
   
   ASSIGN liFirstByte = GET-BYTE(lmFileBuffer,1).

   SET-SIZE(lmFileBuffer) = 0.

   RETURN liFirstByte.
END FUNCTION.

FUNCTION isEncrypted RETURNS LOGICAL
   (INPUT                  ipcFileName       AS    CHARACTER):

   CASE firstByte(ipcFileName):
      WHEN 17 OR
      WHEN 19
      THEN RETURN TRUE.
   END CASE.

   RETURN FALSE.
END FUNCTION.

FUNCTION canDecrypt RETURNS LOGICAL
   (INPUT                  ipcFileName       AS    CHARACTER):

   CASE firstByte(ipcFileName):
      WHEN 17
      THEN RETURN TRUE.
      WHEN 19 
      THEN RETURN TRUE.
   END CASE.

   RETURN ?.
END FUNCTION.

PROCEDURE processFiles:
   DEFINE INPUT  PARAMETER ipcDirectory         AS    CHARACTER      NO-UNDO.
   
   DEFINE VARIABLE         lcFileNameShort      AS    CHARACTER      NO-UNDO.
   DEFINE VARIABLE         lcFileNameFull       AS    CHARACTER      NO-UNDO.
   DEFINE VARIABLE         lcFileType           AS    CHARACTER      NO-UNDO.
   DEFINE VARIABLE         lcExtension          AS    CHARACTER      NO-UNDO.

   DEFINE VARIABLE         llEncrypted          AS    LOGICAL        NO-UNDO.
   DEFINE VARIABLE         llCanDecrypt         AS    LOGICAL        NO-UNDO.

   DEFINE BUFFER           ttDir                FOR   ttDir.
   
   PAUSE 0 BEFORE-HIDE.

   IF ipcDirectory EQ "" OR
      ipcDirectory EQ ?
   THEN RETURN "*** Directory Name is invalid.".

   ASSIGN FILE-INFO:FILE-NAME = ipcDirectory.

   IF INDEX(FILE-INFO:FILE-TYPE, "D") EQ 0 OR
      INDEX(FILE-INFO:FILE-TYPE, "D") EQ ?
   THEN RETURN SUBSTITUTE("*** Directory &1 does not exist.", ipcDirectory).
   
   IF INDEX(FILE-INFO:FILE-TYPE, "R") EQ 0 OR
      INDEX(FILE-INFO:FILE-TYPE, "R") EQ ?
   THEN RETURN SUBSTITUTE("*** Directory &1 is not readable.", ipcDirectory).

   ASSIGN ipcDirectory = FILE-INFO:FULL-PATHNAME.

   INPUT STREAM stFileList FROM OS-DIR(ipcDirectory).
   
   processDirectory:
   REPEAT:
      IMPORT STREAM stFileList
             lcFileNameShort
             lcFileNameFull
             lcFileType.

      /* Sub-Directories are dealt with later */
      IF INDEX(lcFileType, "D") GT 0
      THEN DO:
         IF lcFileNameShort EQ "."  OR
            lcFileNameShort EQ ".."
         THEN NEXT processDirectory.

         CREATE ttDir.
         ASSIGN ttDir.DirectoryName = lcFileNameFull.
         NEXT processDirectory.
      END.

      /* Only process source files */
      ASSIGN lcExtension = IF NUM-ENTRIES(lcFileNameShort, ".") GT 1
                           THEN ENTRY(NUM-ENTRIES(lcFileNameShort, "."), lcFileNameShort, ".")
                           ELSE "".
      IF NOT CAN-DO(lcExtensionList, lcExtension)
      THEN NEXT processDirectory.

      RUN checkFile 
          (INPUT  lcFileNameFull,
           OUTPUT llEncrypted,
           OUTPUT llCanDecrypt).
      
      /*
      IF RETURN-VALUE EQ ""    AND
         llEncrypted  NE TRUE
      THEN NEXT processDirectory.
      */
      
      IF llLogFile EQ TRUE
      THEN DO:
         CASE lcEncryption:
            WHEN "X"
            THEN RUN createLogging (lcFileNameFull, STRING(llEncrypted,"***ENCRYPTED***/"), STRING(llCanDecrypt,"/UNKNOWN ENCRYPTION"), RETURN-VALUE).
            WHEN "S"
            THEN RUN createLogging (lcFileNameFull, STRING(llEncrypted,"/***UNENCRYPTED***"), STRING(llCanDecrypt,"/UNKNOWN ENCRYPTION"), RETURN-VALUE).
            WHEN "B"
            THEN RUN createLogging (lcFileNameFull, STRING(llEncrypted,"***ENCRYPTED***/***UNENCRYPED***"), STRING(llCanDecrypt,"/UNKNOWN ENCRYPTION"), RETURN-VALUE).
         END CASE.   
      END.

      DISPLAY lcFileNameFull                 FORMAT "X(70)"       LABEL "File"
              llEncrypted                    FORMAT "Yes/No"      LABEL "Enc"
              llCanDecrypt                   FORMAT "Yes/No"      LABEL "Dec"
              WITH FRAME frFiles DOWN.
      DOWN WITH FRAME frFiles.
         
      IF llCanDecrypt NE TRUE
      THEN NEXT processDirectory.

      /*RUN dcode (lcFileNameFull,TRUE,FALSE).*/
   END.
   INPUT STREAM stFileList CLOSE.

   RETURN "".
END PROCEDURE.

PROCEDURE checkFile:
   DEFINE INPUT  PARAMETER ipcFileName    AS    CHARACTER      NO-UNDO.
   DEFINE OUTPUT PARAMETER oplEncrypted   AS    LOGICAL        NO-UNDO.
   DEFINE OUTPUT PARAMETER oplCanDecrypt  AS    LOGICAL        NO-UNDO.

   ASSIGN oplEncrypted  = ?
          oplCanDecrypt = ?.

   IF ipcFileName EQ "" OR
      ipcFileName EQ ?
   THEN RETURN "*** File Name is invalid.".

   ASSIGN FILE-INFO:FILE-NAME = ipcFileName.
   
   IF INDEX(FILE-INFO:FILE-TYPE, "F") EQ 0 OR
      INDEX(FILE-INFO:FILE-TYPE, "F") EQ ?
   THEN RETURN SUBSTITUTE("*** File &1 does not exist.", ipcFileName).
   
   IF INDEX(FILE-INFO:FILE-TYPE, "R") EQ 0 OR
      INDEX(FILE-INFO:FILE-TYPE, "R") EQ ?
   THEN RETURN SUBSTITUTE("*** File &1 is not readable.", ipcFileName).

   HIDE MESSAGE NO-PAUSE.
   MESSAGE "Processing: " ipcFileName.

   ASSIGN oplEncrypted  = isEncrypted(ipcFileName)
          oplCanDecrypt = oplEncrypted            EQ FALSE OR
                          canDecrypt(ipcFileName) EQ TRUE
          .

   RETURN "".
END PROCEDURE.

PROCEDURE createLogging:
   DEFINE INPUT  PARAMETER ipcFileName    AS    CHARACTER      NO-UNDO.
   DEFINE INPUT  PARAMETER ipcEncrypted   AS    CHARACTER      NO-UNDO.
   DEFINE INPUT  PARAMETER ipcCanDecrypt  AS    CHARACTER      NO-UNDO.
   DEFINE INPUT  PARAMETER ipcMessage     AS    CHARACTER      NO-UNDO.

   OUTPUT STREAM stLogFile TO VALUE(lcLogfile) UNBUFFERED APPEND.
   
   PUT STREAM stLogFile UNFORMATTED 
              ipcFileName
              CHR(9)
              ipcEncrypted
              CHR(9)
              ipcCanDecrypt
              CHR(9)
              ipcMessage
              CHR(10).

   OUTPUT STREAM stLogFile CLOSE.
   
   RETURN "".
END PROCEDURE.

ASSIGN lcExtensionList = "p,w,i,t,v".

ASSIGN llLogFile            = TRUE
       llMailLog            = FALSE
       lcEncryption         = "X"
       .

REPEAT:
   UPDATE lcDirectorySource      FORMAT "X(60)"  LABEL "Start from"    COLON 15 SKIP
          lcExtensionList        FORMAT "X(60)"  LABEL "Extensions"    COLON 15 SKIP
          lcEncryption           FORMAT "!(1)"   LABEL "Flag Files"    COLON 15
                                 HELP "X=Encrypted,S=Readable or B=Both"
                                 VALIDATE(LOOKUP(lcEncryption,"X,S,B") GT 0, "Enter X, S or B")
          llLogFile              FORMAT "Yes/no" LABEL "Create Log"    COLON 15 SKIP
&IF OPSYS EQ "UNIX"
&THEN
          llMailLog              FORMAT "Yes/no" LABEL "Mail Log"      COLON 15 SKIP
&ENDIF
          WITH FRAME frInput SIDE-LABELS.
   
   ASSIGN llLogFile       = (llLogFile         EQ TRUE)
          llMailLog       = (llMailLog         EQ TRUE)
          lcLogFile       = "".


   IF llLogFile EQ TRUE
   THEN ASSIGN lcLogFile = "xxencrypted"                +
                           STRING(YEAR (TODAY), "9999") +
                           STRING(MONTH(TODAY), "99")   +
                           STRING(DAY  (TODAY), "99")   +
                           "_"                          +
                           REPLACE(STRING(TIME, "hh:mm:ss"), ":", "") +
                           ".log".
                           
   IF llLogFile EQ TRUE
   THEN DO:
      RUN createLogging ("Starting Scan from Directory      : " + lcDirectorySource,"","","").
      RUN createLogging ("Scanning for Files with Extension : " + lcExtensionList,"","","").
      RUN createLogging ("Process Started @ " + STRING(TIME, "hh:mm:ss"),"","","").
   END.
   
   DISPLAY lcLogFile FORMAT "X(70)"  NO-LABEL
           WITH FRAME frInput SIDE-LABELS.
    
   RUN processFiles(lcDirectorySource).

   IF llLogFile    EQ TRUE  AND
      RETURN-VALUE NE ""
   THEN RUN createLogging (lcDirectorySource,"","",RETURN-VALUE).
   
   scanSubDirectory:
   DO WHILE TRUE:
      FIND FIRST ttDir
           NO-LOCK NO-ERROR.
      IF NOT AVAIL(ttDir)
      THEN LEAVE ScanSubDirectory.

      RUN processFiles (ttDir.DirectoryName).
      
      IF llLogFile    EQ TRUE  AND
         RETURN-VALUE NE ""
      THEN RUN createLogging (ttDir.DirectoryName,"","",RETURN-VALUE).
   
      FIND CURRENT ttDir 
           EXCLUSIVE-LOCK.
      DELETE ttDir.
   END.
   
   IF llLogFile EQ TRUE
   THEN DO:
      RUN createLogging ("Process Finished @ " + STRING(TIME, "hh:mm:ss"),"","","").
      
&IF OPSYS EQ "UNIX"
&THEN
/*
      IF llMailLog EQ TRUE
      THEN DO:
         RUN xxsendmailmp.p
             ("",
              "xxdcrypt.p " + hostName(),
              "roger.duijckaerts@hp.com",
              "",                        
              "",                       
              lcLogFile,                       
              FALSE,                    
              lcLogFile,
              "",
              FALSE).
         OS-DELETE VALUE(lcLogFile).
      END.
*/
      &ENDIF
   END.
END.

RETURN "".
