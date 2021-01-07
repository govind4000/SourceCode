
DEFINE VARIABLE i             AS INTEGER.
DEFINE VARIABLE j             AS INTEGER INIT ?. /* To restrict for first srcTable check init to ? */
DEFINE VARIABLE k             AS INTEGER.
DEFINE VARIABLE liQCnt        AS INTEGER.
DEFINE VARIABLE liHugeThread  AS INTEGER.
DEFINE VARIABLE liMaxThread   AS INTEGER.
DEFINE VARIABLE lcSkipThread  AS CHARACTER.
DEFINE VARIABLE lcAsgnThread  AS CHARACTER.
DEFINE VARIABLE lsrcTable     AS CHARACTER.
DEFINE VARIABLE liMinTime     AS INTEGER INIT 51.
DEFINE VARIABLE liMaxTime     AS INTEGER INIT 52.
DEFINE VARIABLE liLmtCount    AS INTEGER INIT 100000.
DEFINE VARIABLE liChkCount    AS INTEGER INIT 1000.
DEFINE VARIABLE liProcCount   AS INTEGER.
DEFINE BUFFER   bfreplqueue   FOR replqueue.
DEFINE BUFFER Tranreplqueue   FOR replqueue.
DEFINE BUFFER   chkreplqueue  FOR replqueue.
SELECT MAX(qThread) INTO liMaxThread FROM ReplTableXRef.
liMaxThread = 15.
/*pause 10800.*/
topLoop:
REPEAT:
PAUSE 600.

liHugeThread = 8.
lcSkipThread = "1,4" /*"4,8"*/.

mainLoop:
REPEAT:

   /* If we have a Thread with lot of tables and if we have a huge queue count for a single table and if we want to move all the other tables to sync fast, we can use lSrcTable so it will se
arch for NE condition */
   IF j EQ ? OR i GT 10  /* Variable i have the no. of records assigned to other Threads in previous repeat loop - So in case there are no records to process for NE lSrcTable then we need to
 process for allTables */
   THEN lSrcTable = "evt" /* = "isdjc_dtl" || = isink_evt"*/.
  ELSE lSrcTable = "allTables".

   ASSIGN i = 0
          j = 0
          k = 0
          liQCnt = 0
          lcAsgnThread = "".
   IF liLmtCount GT 2000
   THEN PAUSE INT(liLmtCount / 300).
   SELECT count(*) INTO i FROM replqueue WHERE applied = NO AND qthread = liHugeThread.
   IF i LT 5000
   THEN DO:
      HIDE MESSAGE NO-PAUSE.
      MESSAGE "Only" i "records to process. Quitting...".
      IF liHugeThread EQ 4
      THEN DO:
         liHugeThread = 8.
         lcSkipThread = REPLACE(lcSkipThread,'8','4').
         MESSAGE "Changing to Thread 8 from 4".
         PAUSE 10.
         NEXT mainLoop.
      END.
      NEXT topLoop.
   END.
   ELSE DO:
      HIDE MESSAGE NO-PAUSE.
      MESSAGE "Yet to process" i.
      PAUSE 10.
   END.

   IF INT(SUBSTRING(STRING(TIME,"HH:MM"),4,2)) GE liMinTime AND
      INT(SUBSTRING(STRING(TIME,"HH:MM"),4,2)) LT liMaxTime
   THEN DO:
      HIDE MESSAGE NO-PAUSE.
      MESSAGE "Waiting for alert mail to be sent without backlog.".
      PAUSE 50.
      NEXT mainLoop.
   END.

   incThread:
   DO j = 1 TO liMaxThread:
      IF j EQ liHugeThread OR (lcSkipThread NE "" AND LOOKUP(STRING(j),lcSkipThread) GT 0)
      THEN NEXT incThread.
      message "J - " j.
      pause 5.
      k = 0.
      SELECT count(*) INTO k FROM replqueue WHERE applied = NO AND qthread = j.
      IF k GT liChkCount
      THEN DO:
         HIDE MESSAGE NO-PAUSE.
         MESSAGE "Skipping Thread" j "we have already Queue count " k.
         PAUSE 10.
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
      PAUSE 10.
      NEXT mainLoop.
   END.

   message lcAsgnThread.
   pause 10.
   
   /*IF lcAsgnThread NE (STRING(liHugeThread) + ',11,12,13,14,15')*/

   IF lcAsgnThread NE (STRING(liHugeThread) + ',2,3,5,6,7,9,10,11,12,13,14,15')
   THEN DO: /* To avoid processing records in diff order (it might cause 'D' Event to execute first and then 'W' event sync which will end up non reliability in Replication */
      HIDE MESSAGE NO-PAUSE.
      MESSAGE "Some Threads are still processing records, re-checking. Free Threads" lcAsgnThread.
      PAUSE 10.
      NEXT mainLoop.
   END.

    liProcCount = i . /* No of records in huge Thread */
   recCntLoop:
   DO WHILE liProcCount GT (liLmtCount * liQCnt):
      liProcCount = liProcCount / 2.
   END. /* recCntLoop: DO WHILE */

   ASSIGN i = 0
          j = 0
          k = 0
          .
   HIDE MESSAGE NO-PAUSE.
   MESSAGE "Assigning" liProcCount "records to Threads" lcAsgnThread "from" liHugeThread.
   PAUSE 10.

   replLoop:
   FOR EACH bfreplqueue NO-LOCK
      WHERE bfreplqueue.qthread  EQ liHugeThread
        AND bfreplqueue.applied  EQ NO
        AND bfreplqueue.srctable NE lSrcTable
     BREAK BY bfreplqueue.sequence
        /* BY bfreplqueue.eventdate
           BY bfreplqueue.eventtime */:
      FIND FIRST tranreplqueue
           WHERE ROWID(tranreplqueue)  EQ ROWID(bfreplqueue)
             AND tranreplqueue.applied EQ NO
           EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF AVAILABLE tranreplqueue AND NOT LOCKED tranreplqueue
      THEN DO:
         FIND FIRST chkreplqueue /* To avoid records processed in wrong seq */
              WHERE chkreplqueue.SrcRecord EQ tranreplqueue.SrcRecord
                AND chkreplqueue.qThread   GE 9
              NO-LOCK NO-ERROR.
         IF AVAILABLE chkreplqueue
         THEN ASSIGN Tranreplqueue.qthread = chkreplqueue.qThread
                     i = i + 1
                     .
         ELSE DO:
            IF CAN-FIND(FIRST chkreplqueue
                 WHERE chkreplqueue.applied   EQ NO
                   AND chkreplqueue.qThread   EQ liHugeThread
                   AND chkreplqueue.Seq       GT tranreplqueue.seq
                   AND chkreplqueue.Seq       LT (tranreplqueue.seq + 600)
                   /* To Avoid loop taking time, only checking next 1.2k rec (6 * 200 rec), seq can be fall to other threads also atleast 100 per thread */
                   AND chkreplqueue.SrcRecord EQ tranreplqueue.SrcRecord)
            THEN NEXT replLoop.
            ELSE ASSIGN j = j + 1
                        j = IF j GT liQCnt THEN 1 ELSE j
                        k = INT(ENTRY(j,lcAsgnThread))
                        Tranreplqueue.qthread = k
                        i = i + 1
                        .
         END.
      END.
      ELSE NEXT replLoop.

      IF i GT liProcCount
      THEN LEAVE replLoop.

   END. /* FOR EACH bfreplqueue NO-LOCK */
   PAUSE 10.
END. /* mainLoop: REPEAT */
return.
END. /* topLoop: REPEAT: */