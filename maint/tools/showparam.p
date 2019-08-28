DEFINE VARIABLE         lcType            AS    CHARACTER      NO-UNDO.

DEFINE VARIABLE         lcParameters      AS    CHARACTER      NO-UNDO.
DEFINE VARIABLE         lcStatus          AS    CHARACTER      NO-UNDO.

FUNCTION switchParameters RETURNS CHARACTER
   ():
   
   ASSIGN lcParameters = "".
   
   CASE lcType:
      WHEN ""   OR
      WHEN "DB"
      THEN DO:
         ASSIGN lcType       = "SESSION"
                lcParameters = SESSION:STARTUP-PARAMETERS
                .
      END.
      WHEN "SESSION"
      THEN DO:
         ASSIGN lcType       = "DB"
                lcParameters = DBPARAM("DICTDB")
                NO-ERROR.
      END.
   END CASE.
   
   ASSIGN lcParameters = (IF lcParameters EQ ?
                          THEN ""
                          ELSE lcParameters) 
          lcParameters = REPLACE(lcParameters,",",CHR(10)).
   
   RETURN "".
END FUNCTION.

ON "CTRL-X" ANYWHERE DO:
   OUTPUT TO VALUE(LC(lcType) + "_param.txt").
   PUT UNFORMATTED lcType.
   OUTPUT CLOSE.
   MESSAGE "File saved to " SESSION:TEMP-DIR + LC(lcType) + "_param.txt".
END.

switchParameters().

REPEAT:
   STATUS INPUT "Press " + KBLABEL("GO") + " to switch view; " +
                "Press CTRL-X to save".
                
   DISPLAY lcType
           lcParameters VIEW-AS EDITOR SIZE 78 BY 18 
           WITH FRAME X NO-LABELS.
           
   UPDATE lcParameters
          WITH FRAME X.
   switchParameters().
END.

