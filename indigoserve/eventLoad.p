
/*------------------------------------------------------------------------
    File        : eventLoad.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Jun 21 16:18:50 IST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/* ********************  Preprocessor Definitions  ******************** */
/* ***************************  Main Block  *************************** */


DEFINE VARIABLE         lcFilePath         AS    CHARACTER      NO-UNDO FORMAT "x(30)" init "/home/ygo/temp/bids.txt". 
DEFINE VARIABLE         lcTableName        AS    CHARACTER      NO-UNDO format "X(10)" init "isbid_evt".
DEFINE VARIABLE         llUpdate           AS    LOGICAL. 


DEFINE TEMP-TABLE    ttEventLoad                      NO-UNDO
       FIELD         ttEvents        AS    CHARACTER
       .
         
UPDATE lcFilePath
       lcTableName
       llUpdate
       .

INPUT FROM value(lcFilePath).          
REPEAT:
    CREATE ttEventLoad.
    IMPORT ttEventLoad.
END.
INPUT CLOSE.

OUTPUT TO VALUE("/home/ygo/temp/eventLoad.txt").

FOR EACH ttEventLoad  
         NO-LOCK:
         
    FIND FIRST istbld_det WHERE istbld_det.istbld_tbl      EQ lcTableName
                            AND istbld_det.istbld_evt_name EQ ttEventLoad.ttEvents NO-LOCK NO-ERROR.
    IF AVAILABLE istbld_det 
    THEN DO:
       put unformatted    "Avail     " lcTableName  " - "  istbld_det.istbld_evt_name skip.
    END.   
    ELSE DO:
       FIND FIRST istbld_det WHERE istbld_det.istbld_tbl      EQ lcTableName
                               AND istbld_det.istbld_evt_name MATCHES ttEventLoad.ttEvents NO-LOCK NO-ERROR.
       IF AVAILABLE istbld_det
       THEN DO: 
          put unformatted  "Matches   " lcTableName  " - "  istbld_det.istbld_evt_name skip.
       END.
       ELSE DO:
          if llUpdate then 
          do:
             create istbld_det.
             assign istbld_det.istbld_tbl = lcTableName
                    istbld_det.istbld_evt_name = ttEventLoad.ttEvents
                    .
             put unformatted  "Created " lcTableName  " - "  ttEventLoad.ttEvents skip.                    
          end.    
          else
          put unformatted  "Not Avail " lcTableName  " - "  ttEventLoad.ttEvents skip.
       END.
    END.
    
END.

/******
FOR EACH istbld_det 
         WHERE istbld_det.istbld_tbl EQ lcTableName
         NO-LOCK:
         
         FIND FIRST ttEventLoad WHERE ttEventLoad.ttEvents EQ istbld_det.istbld_evt_name NO-LOCK NO-ERROR.
         IF AVAILABLE ttEventLoad 
         THEN DO:
            put unformatted    "Avail    " istbld_det.istbld_tbl  "  - "  istbld_det.istbld_evt_name skip.
         END.   
         ELSE DO:
            put unformatted istbld_det.istbld_evt_name skip.
            FIND FIRST ttEventLoad WHERE ttEventLoad.ttEvents matches istbld_det.istbld_evt_name NO-LOCK NO-ERROR.
            IF AVAILABLE ttEventLoad 
            THEN DO:
               put unformatted  "Matches  " istbld_det.istbld_tbl  "  - "  istbld_det.istbld_evt_name skip.
            END.           
            ELSE DO:
               put unformatted  "Not avail" istbld_det.istbld_tbl  "  - "  istbld_det.istbld_evt_name skip.
               /*CREATE istbld_det.
               ASSIGN istbld_det.istbld_tbl = lcTableName
                      istbld_det.istbld_evt_name = ttEventLoad.ttEvents
                      .*/
            END.
         END.
END. /*FOR EACH istbld_det */
*****/

OUTPUT CLOSE.

         
         