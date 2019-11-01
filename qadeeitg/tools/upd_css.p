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

runProgram("upd_css_control.p").
runProgram("upd_css_registry.p").
runProgram("upd_css_receiver.p").
