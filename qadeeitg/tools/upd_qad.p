DEFINE VARIABLE         lcEnvironment  AS    CHARACTER      NO-UNDO.
DEFINE VARIABLE         lcPropath      AS    CHARACTER      NO-UNDO.

ASSIGN lcEnvironment = ENTRY(NUM-ENTRIES(DBNAME,CHR(47)),DBNAME,CHR(47))
       lcEnvironment = ENTRY(2, lcEnvironment, "_").
              
FUNCTION runProgram RETURNS CHARACTER
   (INPUT                  ipcProgram         AS    CHARACTER):
  
   ASSIGN ipcProgram = SUBSTITUTE("/home/&1/tools/&2",
                                  OS-GETENV("USER"),
                                  ipcProgram).
  
   IF SEARCH(ipcProgram) EQ ?
   THEN DO:
      MESSAGE SUBSTITUTE("Program &1 not found.", ipcProgram)
              VIEW-AS ALERT-BOX.
      RETURN "".
   END.
   
   RUN VALUE(ipcProgram).
   
   RETURN "".
END FUNCTION.

ASSIGN lcPropath = PROPATH
       PROPATH   = PROPATH + "," +
                   SUBSTITUTE("/opt/qadee/ee2013/&1/qad/xrc/validation",
                              lcEnvironment)
                   .
runProgram("upd_qad_code_mstr_path.p").
runProgram("upd_qad_db_conn.p").

ASSIGN PROPATH = lcPropath.
