DEFINE VARIABLE         llLocked          AS    LOGICAL        NO-UNDO.
DEFINE BUFFER           bfReplControl     FOR   ReplControl.

for each ReplControl WHERE ReplControl.GroupID  = "PROCESS" AND
ReplControl.CodeID   = "REPLICATION"
NO-LOCK
BY ReplControl.GroupID 
BY ReplControl.CodeID
BY INTEGER(ReplControl.CodeVal1):

IF NOT CAN-FIND(FIRST {&ReplDb}ReplTableXref 
WHERE {&ReplDb}ReplTableXref.QThread = INTEGER(ReplControl.CodeVal1))
THEN NEXT.      
      
DO FOR bfReplControl:
FIND bfReplControl
WHERE ROWID(bfReplControl) EQ ROWID(ReplControl)
EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
ASSIGN llLocked = LOCKED(bfReplControl).
END.
DISPLAY ReplControl.CodeVal1 LABEL "Thread"
        llLocked             LABEL "Running"
        ReplControl.CodeVal3 EQ "0" LABEL "Replication On".
END.
