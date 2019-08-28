DEFINE VARIABLE lcTable AS CHARACTER FORMAT "X(20)".



REPEAT:
  ASSIGN  lcTable = "".
  UPDATE  lcTable.
  
  IF CAN-FIND (FIRST ReplQueue
           WHERE ReplQueue.SrcTable       EQ lcTable  AND 
                          ReplQueue.eventDate    LT TODAY)
  THEN DO:
    FOR EACH  ReplQueue
        WHERE ReplQueue.SrcTable     EQ lcTable  AND 
              ReplQueue.eventDate    LT TODAY    AND
              ReplQueue.Applied      EQ FALSE
        NO-LOCK BREAK BY ReplQueue.eventDate:
 /*      DISPLAY   ReplQueue.SrcTable 
                 ReplQueue.SrcRecord format "x(30)" 
                 ReplQueue.EventDate 
                 ReplQueue.Eventtime 
                 ReplQueue.SrcTransID 
                 ReplQueue.Applied
                 .  */
      IF FIRST(ReplQueue.eventDate) 
      THEN DO:
        RUN Check_Process_ID(INPUT ReplQueue.SrcTransID).   
      END. /*if first(ReplQueue.eventDate)*/
                                  
    END.  /*for each replqueue*/            
  END. /* < today*/
  ELSE DO:
    FOR EACH  ReplQueue
        WHERE ReplQueue.SrcTable EQ lcTable AND
              ReplQueue.Applied  EQ FALSE
        NO-LOCK BREAK BY ReplQueue.eventDate:
/*       DISPLAY   ReplQueue.SrcTable 
                 ReplQueue.SrcRecord format "x(30)" 
                 ReplQueue.EventDate 
                 ReplQueue.Eventtime 
                 ReplQueue.SrcTransID 
                 ReplQueue.Applied
                 .    */  
        IF FIRST(ReplQueue.eventDate) 
        THEN DO:
          RUN Check_Process_ID(INPUT ReplQueue.SrcTransID).  
        END. /*if first(ReplQueue.eventDate)*/
    END. /*for each replqueue*/                              
  END. /*today*/
        
END. /*repeat*/ 

PROCEDURE Check_Process_ID:
  DEFINE INPUT PARAMETER lcTransNbr LIKE ReplQueue.SrcTransID.       
  FOR EACH _trans 
     WHERE _trans._trans-num EQ lcTransNbr
     NO-LOCK:
     /*DISPLAY _trans WITH 2 COL.*/
    FOR EACH _connect 
             WHERE _connect._connect-Usr EQ _trans._trans-usrnum 
             NO-LOCK :
        DISPLAY  _connect._connect-Usr 
                          _connect._connect-Pid 
                          .
    END. /*for each _connect*/
  END. /*for each _trans*/                     
END PROCEDURE.                                                      
                                  




