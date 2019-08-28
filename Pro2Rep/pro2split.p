DEFINE VARIABLE i             AS INTEGER.
DEFINE VARIABLE j             AS INTEGER INIT ?. /* To restrict for first srcTable check init to ? */
DEFINE VARIABLE k             AS INTEGER.
DEFINE VARIABLE liQCnt        AS INTEGER.
DEFINE VARIABLE liHugeThread  AS INTEGER.
DEFINE VARIABLE liMaxThread   AS INTEGER.
DEFINE VARIABLE lcSkipThread  AS CHARACTER.
DEFINE VARIABLE lcAsgnThread  AS CHARACTER.
DEFINE VARIABLE lsrcTable     AS CHARACTER.
DEFINE VARIABLE liMinTime     AS INTEGER INIT 41.
DEFINE VARIABLE liMaxTime     AS INTEGER INIT 52.
DEFINE VARIABLE liLmtCount    AS INTEGER INIT 10000.
DEFINE VARIABLE liChkCount    AS INTEGER INIT 5000.
DEFINE VARIABLE liProcCount   AS INTEGER.
DEFINE BUFFER   bfreplqueue   FOR replqueue.
DEFINE BUFFER Tranreplqueue   FOR replqueue.
SELECT MAX(qThread) INTO liMaxThread FROM ReplTableXRef.
liHugeThread = 1.
lcSkipThread = "4,8" /*"4,8"*/.

mainLoop:
REPEAT:

   /* If we have a Thread with lot of tables and if we have a huge queue count for a single table and if we want to move all the other tables to sync fast, we can use lSrcTable so it will search for NE condition */
   IF j EQ ? OR i GT 10  /* Variable i have the no. of records assigned to other Threads in previous repeat loop - So in case there are no records to process for NE lSrcTable then we need to process for allTables */
   THEN lSrcTable = "evt" /* = "isdjc_dtl" || = isink_evt"*/.
  ELSE lSrcTable = "allTables".

   ASSIGN i = 0
          j = 0
          k = 0
          liQCnt = 0
          lcAsgnThread = "".
   SELECT count(*) INTO i FROM replqueue WHERE applied = NO AND qthread = liHugeThread.
   IF i LT 30000
   THEN DO:
      HIDE MESSAGE NO-PAUSE.
      MESSAGE "Only" i "records to process. Quitting...".
      RETURN.
   END.
   ELSE DO:
      HIDE MESSAGE NO-PAUSE.
      MESSAGE "Yet to process" i.
      PAUSE 1.
   END.

   IF INT(SUBSTRING(STRING(TIME,"HH:MM"),4,2)) GE liMinTime AND
      INT(SUBSTRING(STRING(TIME,"HH:MM"),4,2)) LT liMaxTime
   THEN DO:
      HIDE MESSAGE NO-PAUSE.
      MESSAGE "Waiting for alert mail to be sent without backlog.".
      PAUSE 59.
      NEXT mainLoop.
   END.

   incThread:
   DO j = 1 TO liMaxThread:
      IF j EQ liHugeThread OR (lcSkipThread NE "" AND LOOKUP(STRING(j),lcSkipThread) GT 0)
      THEN NEXT incThread.
      k = 0.
      SELECT count(*) INTO k FROM replqueue WHERE applied = NO AND qthread = j.
      IF k GT liChkCount
      THEN DO:
         HIDE MESSAGE NO-PAUSE.
         MESSAGE "Skipping Thread" j "we have already Queue count " k.
         PAUSE 1. 
         NEXT incThread.
      END.
      ELSE ASSIGN liQCnt = liQCnt + 1
                  lcAsgnThread = lcAsgnThread + ',' + STRING(j).
   END.
   IF lcAsgnThread NE ''
   THEN ASSIGN lcAsgnThread = STRING(liHugeThread) + lcAsgnThread    /* To make sure the processed records follow Seq while replicating */
               liQCnt = liQCnt + 1.
   ELSE DO: /* IF blank */
      HIDE MESSAGE NO-PAUSE.
      MESSAGE "Count of All Threads are more than" liChkCount.
      PAUSE 59.
      NEXT mainLoop.
   END.

   liProcCount = i / 2. /* No of records in huge Thread Div-by 2 */
   recCntLoop:
   DO WHILE liProcCount GT (liLmtCount * liQCnt):
      liProcCount = liProcCount / 2.
   END. /* recCntLoop: DO WHILE */

   ASSIGN i = 0
          j = 0
          k = 0
          .
   HIDE MESSAGE NO-PAUSE.
   MESSAGE "Assigning Queue to Threads" lcAsgnThread "from" liHugeThread.
   PAUSE 2.

   replLoop:
   FOR EACH bfreplqueue NO-LOCK
      WHERE bfreplqueue.qthread  EQ liHugeThread
        AND bfreplqueue.applied  EQ NO
        AND bfreplqueue.srctable NE lSrcTable
     BREAK BY bfreplqueue.eventdate
            BY bfreplqueue.eventtime:
      FIND FIRST tranreplqueue
           WHERE ROWID(tranreplqueue)  EQ ROWID(bfreplqueue)
             AND tranreplqueue.applied EQ NO
           EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF AVAILABLE tranreplqueue AND NOT LOCKED tranreplqueue
      THEN DO:
      ASSIGN j = j + 1
             j = IF j GT liQCnt THEN 1 ELSE j
             k = INT(ENTRY(j,lcAsgnThread))
             i = i + 1
             Tranreplqueue.qthread = k
             .
      END.
      ELSE NEXT replLoop.

      IF i MOD liProcCount EQ 0
      THEN LEAVE replLoop.

   END. /* FOR EACH bfreplqueue NO-LOCK */
   PAUSE 10.
END. /* mainLoop: REPEAT */
