DEF BUFFER bf FOR ReplTableXref.
DEF BUFFER bfrStatCtrl for {&ReplDb}ReplControl.

FOR EACH ReplTableXref
/*
where qthread GE 6
WHERE {&ReplDb}ReplTableXref.QThread = INTEGER(ReplControl.CodeVal1)
*/
NO-LOCK BREAK by qthread:

DEF VAR i AS INT.

IF FIRST-OF(qthread)
THEN ASSIGN i = 0.
i = i + 1.
IF LAST-OF(qthread)
THEN DO:

FIND FIRST bfrStatCtrl WHERE
                           bfrStatCtrl.GroupID  = "CONTROL"     AND
                           bfrStatCtrl.CodeID   = "REPLICATION" AND
                           bfrStatCtrl.CodeVal1 = STRING( ReplTableXref.QThread)
NO-LOCK NO-ERROR.
MESSAGE "Thread" qthread "has" i "tables mapped" SKIP
        "Control record" STRING(AVAIL(bfrStatCtrl) ,"available/NOT available")
        VIEW-AS ALERT-BOX.

/*
IF NOT AVAIL(bfrStatCtrl) THEn DISP
qthread                   format "99"
ReplTableXref.srcDB ReplTableXref.srcTable.
*/

