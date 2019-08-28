/* PSC: xxbldstrct.p - Build Structure File                                 */
/* COPYRIGHT Indigo  ALL RIGHTS RESERVED. THIS IS AN UNPUBLISHED WORK.      */
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
/* REVISION: eB   LAST MODIFIED: 22 Jun 2008  BY: HP/RDU *SR5368*           */
/* REVISION: eB   LAST MODIFIED: 17 Apr 2014  BY: HP/RDU *SR6055*           */

DEFINE VARIABLE         lcLine            AS    CHARACTER      NO-UNDO.
DEFINE VARIABLE         lcDB              AS    CHARACTER      NO-UNDO.
DEFINE VARIABLE         lcType            AS    CHARACTER      NO-UNDO.
DEFINE VARIABLE         lcLocation        AS    CHARACTER      NO-UNDO.
DEFINE VARIABLE         liCounter         AS    INTEGER        NO-UNDO.
DEFINE VARIABLE         lnTotalSize       AS    DECIMAL        NO-UNDO.
DEFINE VARIABLE         lnExtentSize      AS    DECIMAL        NO-UNDO.
DEFINE VARIABLE         liNumExtents      AS    INTEGER        NO-UNDO.

DEFINE VARIABLE         lnDBSize          AS    DECIMAL        NO-UNDO.
DEFINE VARIABLE         lnDataSize        AS    DECIMAL        NO-UNDO.

DEFINE VARIABLE         vlBIKeepCurrent   AS    LOGICAL        NO-UNDO.
DEFINE VARIABLE         vcBILocation      AS    CHARACTER      NO-UNDO.
DEFINE VARIABLE         vlBIFixed         AS    LOGICAL        NO-UNDO.
DEFINE VARIABLE         vnBIFixedSize     AS    DECIMAL        NO-UNDO.
DEFINE VARIABLE         vlBIFixedAll      AS    LOGICAL        NO-UNDO.
DEFINE VARIABLE         viBIFixedNum      AS    INTEGER        NO-UNDO.

DEFINE VARIABLE         vlAdvancedMode    AS    LOGICAL        NO-UNDO.
DEFINE VARIABLE         vlAIKeepCurrent   AS    LOGICAL        NO-UNDO.
DEFINE VARIABLE         vlAIOn            AS    LOGICAL        NO-UNDO.
DEFINE VARIABLE         vcAILocation      AS    CHARACTER      NO-UNDO.
DEFINE VARIABLE         vlAIFixed         AS    LOGICAL        NO-UNDO.
DEFINE VARIABLE         vnAIFixedSize     AS    DECIMAL        NO-UNDO.
DEFINE VARIABLE         vlAIFixedAll      AS    LOGICAL        NO-UNDO.
DEFINE VARIABLE         viAIFixedNum      AS    INTEGER        NO-UNDO.

DEFINE VARIABLE         vnFillRate        AS    DECIMAL        NO-UNDO.
DEFINE VARIABLE         vlToScreen        AS    LOGICAL        NO-UNDO.
DEFINE VARIABLE         vlToFile          AS    LOGICAL        NO-UNDO.
DEFINE VARIABLE         vcFileName        AS    CHARACTER      NO-UNDO.

DEFINE STREAM           stStruct.

FORM lcLine FORMAT "X(80)"
     WITH FRAME frmScreen NO-LABELS NO-BOX DOWN.
FORM vlBIKeepCurrent COLON 25 
                     FORMAT "Yes/No"    
                     LABEL "Keep Current BI Settings"
     SKIP
     vcBILocation    COLON 25
                     FORMAT "X(255)"
                     VIEW-AS FILL-IN SIZE 50 BY 1
                     LABEL  "BI Location"
     SPACE(0) ">>" 
     SKIP
     vlBIFixed       COLON 25 
                     FORMAT "Fixed/Variable"    
                     LABEL "Fixed/Variable BI Size"
     vlBIFixedAll    COLON 55 
                     FORMAT "Yes/No"    
                     LABEL "All Fixed"    
     SKIP
     vnBIFixedSize   COLON 25 
                     FORMAT ">>>,>>>,>>9 KB"    
                     LABEL "Fixed BI Extent Size"
     viBIFixedNum    COLON 55 
                     FORMAT ">>9"    
                     LABEL "Nbr Fixed"
     SKIP(1)

     vlAIKeepCurrent COLON 25 
                     FORMAT "Yes/No"    
                     LABEL "Keep Current AI Settings"
     SKIP
     vlAIOn          COLON 25 
                     FORMAT "Yes/No"    
                     LABEL "AI On"
     SKIP
     vcAILocation    COLON 25
                     FORMAT "X(255)"
                     VIEW-AS FILL-IN SIZE 50 BY 1
                     LABEL  "AI Location"
     SPACE(0) ">>" 
     SKIP
     vlAIFixed       COLON 25 
                     FORMAT "Fixed/Variable"    
                     LABEL "Fixed/Variable AI Size"    
     vlAIFixedAll    COLON 55 
                     FORMAT "Yes/No"    
                     LABEL "All Fixed"    
     SKIP
     vnAIFixedSize   COLON 25 
                     FORMAT ">>>,>>>,>>9 KB"    
                     LABEL "Fixed AI Extent Size"
     viAIFixedNum    COLON 55 
                     FORMAT ">>9"    
                     LABEL "Nbr Fixed"
     SKIP(1)
     vnFillRate      COLON 25 
                     FORMAT ">>9%"           
                     LABEL "Minimum DB Fill Rate"
     SKIP
     vlToScreen      COLON 25
                     FORMAT "Yes/No"
                     LABEL  "Show on Screen"
     SKIP
     vlToFile        COLON 25
                     FORMAT "Yes/No"
                     LABEL  "Export to File"
     SKIP
     vcFileName      COLON 25
                     FORMAT "X(255)"
                     VIEW-AS FILL-IN SIZE 50 BY 1
                     LABEL  "File Name"
     SPACE(0) ">>" 
     WITH FRAME frmParams SIDE-LABELS WIDTH 80.


FUNCTION calcExtentMain RETURNS CHARACTER
   (INPUT                  ipnSize        AS    DECIMAL,
    OUTPUT                 opnExtentSize  AS    DECIMAL,
    OUTPUT                 opnNumExtents  AS    DECIMAL):

   DEFINE VARIABLE         lnSize         AS    DECIMAL        NO-UNDO.
   DEFINE VARIABLE         lnMultiplier   AS    DECIMAL        NO-UNDO.
   DEFINE VARIABLE         lnMinSize      AS    DECIMAL        NO-UNDO.
   DEFINE VARIABLE         lnMaxSize      AS    DECIMAL        NO-UNDO.
   DEFINE VARIABLE         lnFixedMax     AS    DECIMAL        NO-UNDO.
   DEFINE VARIABLE         lnFillRate     AS    DECIMAL        NO-UNDO.
   DEFINE VARIABLE         liCounter      AS    INTEGER        NO-UNDO.

   ASSIGN lnMultiplier = 10
          lnMinSize    = 5120            /* Approx 5 MB */
          lnMaxSize    = 5120 * 2 * 1000 /* Approx 10GB */
          lnFixedMax   = 5  
          lnFillRate   = vnFillRate / 100.
   
   IF ipnSize LE 0 OR
      ipnSize EQ ?
   THEN RETURN "".

   getSize:
   DO WHILE TRUE:

      DO liCounter = 1 TO 4:
         IF liCounter = 3
         THEN NEXT.
         ASSIGN lnSize = liCounter * 256 * lnMultiplier.

         IF lnSize LT lnMinSize
         THEN NEXT.

         IF lnSize GT lnMaxSize
         THEN LEAVE getSize.

         ASSIGN opnExtentSize = lnSize.

         IF (ipnSize / lnSize)                    LE 1 OR
            (lnSize * (lnFixedMax - 1)) - ipnSize GT 0
         THEN LEAVE getSize.
      END.

      ASSIGN lnMultiplier = lnMultiplier * 10.
   END.
   
   ASSIGN opnNumExtents = (IF opnExtentSize GT ipnSize
                           THEN 2
                           ELSE TRUNC(ipnSize / opnExtentSize, 0) + 2).
   
   DO WHILE ipnSize / (opnNumExtents * opnExtentSize) GT lnFillRate:
      ASSIGN opnNumExtents = opnNumExtents + 1.
   END.

   RETURN "".
END FUNCTION.

FUNCTION calcExtentSize RETURNS DECIMAL
   (INPUT                  ipnSize        AS    DECIMAL):
   DEFINE VARIABLE         lnExtentSize   AS    DECIMAL        NO-UNDO.
   DEFINE VARIABLE         lnNumExtents   AS    DECIMAL        NO-UNDO.

   calcExtentMain(ipnSize, OUTPUT lnExtentSize, OUTPUT lnNumExtents).

   RETURN lnExtentSize.
END FUNCTION.

FUNCTION calcNumExtents RETURNS DECIMAL
   (INPUT                  ipnSize        AS    DECIMAL):
   DEFINE VARIABLE         lnExtentSize   AS    DECIMAL        NO-UNDO.
   DEFINE VARIABLE         lnNumExtents   AS    DECIMAL        NO-UNDO.

   calcExtentMain(ipnSize, OUTPUT lnExtentSize, OUTPUT lnNumExtents).

   RETURN lnNumExtents.
END FUNCTION.

FUNCTION getSize RETURNS DECIMAL
   (INPUT                  ipcReturn      AS    CHARACTER,
    INPUT                  ipiAreaNumber  AS    INTEGER,
    INPUT                  ipiBlockSize   AS    INTEGER):
   
   DEFINE VARIABLE         lnHighWater    AS    DECIMAL        NO-UNDO.
   DEFINE BUFFER           bfStatus       FOR   DICTDB._AreaStatus.

   FIND bfStatus 
        WHERE bfStatus._Areastatus-AreaNum EQ ipiAreaNumber 
        NO-LOCK.

   ASSIGN lnHighWater = (IF bfStatus._AreaStatus-HiWater EQ ? 
                         THEN 0
                         ELSE bfStatus._AreaStatus-HiWater).
   CASE ipcReturn:
      WHEN "TotalGross" THEN RETURN 
      bfStatus._AreaStatus-TotBlocks 
      * DEC(ipiBlockSize) / 1024
      .
      WHEN "Total" THEN RETURN 
      (bfStatus._AreaStatus-TotBlocks - bfStatus._AreaStatus-Extents)
      * DEC(ipiBlockSize) / 1024
      .
      WHEN "Free" THEN RETURN 
      (bfStatus._AreaStatus-TotBlocks - bfStatus._AreaStatus-Extents - lnHighWater)
      * DEC(ipiBlockSize) / 1024
      .
      WHEN "Data" THEN RETURN 
      (bfStatus._AreaStatus-Extents + lnHighWater)
      * DEC(ipiBlockSize) / 1024
      .
   END CASE.
   RETURN 0.0.
END FUNCTION.
  
FUNCTION putLine RETURNS CHARACTER
   (INPUT                  ipcLine        AS    CHARACTER):

   IF vlToScreen EQ TRUE
   THEN DO WITH FRAME frmScreen:
      DISPLAY ipcLine @ lcLine.
      DOWN.
   END.
   IF vlToFile EQ TRUE
   THEN DO:
      PUT STREAM stStruct UNFORMATTED
          ipcLine 
          CHR(10).
   END.

   RETURN "".
END FUNCTION.

FUNCTION putHeaderLine RETURNS CHARACTER
   (INPUT                  ipcDBName      AS    CHARACTER):
   DEFINE VARIABLE         lcMonthList    AS    CHARACTER      NO-UNDO.

   ASSIGN lcMonthList = "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec".

   putLine(FILL("#",80)).
   putLine(SUBSTITUTE("# DB Structure File for &1", ipcDBName)).
   putLine(SUBSTITUTE("# Generated on &1-&2-&3 at &4",
                      STRING(DAY(TODAY),"99"),
                      ENTRY(MONTH(TODAY),lcMonthList),
                      STRING(YEAR(TODAY),"9999"),
                      STRING(TIME,"hh:mm:ss"))).
   putLine(FILL("#",80)).
   putLine("").
   RETURN "".
END FUNCTION.
     
FUNCTION putAreaHeaderLine RETURNS CHARACTER
   (INPUT                  ipcAreaType    AS    CHARACTER):

   putLine(SUBSTITUTE("# &1", ipcAreaType)).
   RETURN "".
END FUNCTION.

FUNCTION putAreaInfoLine RETURNS CHARACTER
   (INPUT                  ipcInfo        AS    CHARACTER,
    INPUT                  ipcValue       AS    CHARACTER):
   ASSIGN ipcInfo = FILL(" ", 19 - LENGTH(ipcInfo)) +
                    ipcInfo.
   putLine(SUBSTITUTE("#  &1 : &2",ipcInfo,ipcValue)).
   RETURN "".
END FUNCTION.

FUNCTION putFooterLine RETURNS CHARACTER
   ():

   putLine(FILL("#",80)).
   putAreaInfoLine("DB Extent Size", 
                   SUBSTITUTE("&1 KB",TRIM(STRING(lnDBSize,">>>,>>>,>>>,>>9")))).
   putAreaInfoLine("DB Data Size", 
                   SUBSTITUTE("&1 KB",TRIM(STRING(lnDataSize,">>>,>>>,>>>,>>9")))).
   putAreaInfoLine("DB Fill Rate", 
                   SUBSTITUTE("&1%",TRIM(STRING((lnDataSize / lnDBSize) * 100,">>9.99")))).
   putLine(FILL("#",80)).
   RETURN "".
END FUNCTION.


FUNCTION putAreaExtentLine RETURNS CHARACTER
   (INPUT                  ipcExtentType     AS    CHARACTER,
    INPUT                  ipcExtentName     AS    CHARACTER,
    INPUT                  ipiExtentNumber   AS    INTEGER,
    INPUT                  ipnExtentRPB      AS    DECIMAL,
    INPUT                  ipnExtentBPC      AS    DECIMAL,
    INPUT                  ipcExtentLocation AS    CHARACTER,
    INPUT                  ipiExtentSequence AS    INTEGER,
    INPUT                  iplFixed          AS    LOGICAL,
    INPUT                  ipnExtentSize     AS    DECIMAL):

   DEFINE VARIABLE         lcExtentSettings  AS    CHARACTER      NO-UNDO.
   DEFINE VARIABLE         lcExtent          AS    CHARACTER      NO-UNDO.
   DEFINE VARIABLE         lcExtentSize      AS    CHARACTER      NO-UNDO.


   ASSIGN lcExtentSettings = SUBSTITUTE("&1 ", ipcExtentType) 
                             +
                             (IF ipcExtentName EQ "" OR
                                 ipcExtentName EQ ?
                              THEN ""
                              ELSE SUBSTITUTE("""&1""", ipcExtentName))
                             +
                             (IF ipiExtentNumber EQ 0 OR
                                 ipiExtentNumber EQ ?
                              THEN ""
                              ELSE SUBSTITUTE(":&1", ipiExtentNumber))
                             +
                             (IF ipnExtentRPB EQ 0 OR
                                 ipnExtentRPB EQ ?
                              THEN ""
                              ELSE (IF ipiExtentNumber EQ 0 OR
                                       ipiExtentNumber EQ ?
                                    THEN SUBSTITUTE(":&1", ipnExtentRPB)
                                    ELSE SUBSTITUTE(",&1", ipnExtentRPB)))
                             +
                             (IF ipnExtentBPC EQ 0 OR
                                 ipnExtentBPC EQ ?
                              THEN ""
                              ELSE (IF ipiExtentNumber EQ 0 OR
                                       ipiExtentNumber EQ ?
                                    THEN SUBSTITUTE(";&1", ipnExtentBPC)
                                    ELSE SUBSTITUTE(";&1", ipnExtentBPC)))
          lcExtent         = ipcExtentLocation
                             +
                             (IF ipiExtentNumber   EQ 0 OR
                                 ipiExtentNumber   EQ ? OR
                                 ipiExtentSequence EQ 0 OR
                                 ipiExtentSequence EQ ?
                              THEN (IF ipiExtentSequence GT 0   AND
                                       ipcExtentType     NE "d" 
                                    THEN SUBSTITUTE(".&1&2", ipcExtentType, ipiExtentSequence)
                                    ELSE "")
                              ELSE SUBSTITUTE("_&1.&2&3", ipiExtentNumber, ipcExtentType,ipiExtentSequence))
          lcExtentSize     = (IF iplFixed      NE TRUE OR
                                 ipnExtentSize EQ 0    OR
                                 ipnExtentSize EQ ? 
                              THEN ""
                              ELSE SUBSTITUTE("&1", ipnExtentSize))
                             .

   ASSIGN lcExtentSettings = lcExtentSettings + 
                             FILL(" ", 24 - LENGTH(lcExtentSettings))

          lcExtent         = lcExtent + 
                             FILL(" ", 44 - LENGTH(lcExtent))
          lcExtentSize     = FILL(" ", 08 - LENGTH(lcExtentSize)) +
                             lcExtentSize 
                             .

   IF iplFixed EQ TRUE
   THEN putLine(SUBSTITUTE("&1 &2 f &3",
                           lcExtentSettings,
                           lcExtent,
                           lcExtentSize)).
   ELSE putLine(SUBSTITUTE("&1 &2",
                           lcExtentSettings,
                           lcExtent)).
   RETURN "".
END FUNCTION.

ASSIGN lcDB = ENTRY(NUM-ENTRIES(PDBNAME("DICTDB"), "/"), PDBNAME("DICTDB"), "/").

ASSIGN vlBIKeepCurrent = TRUE
       vlBIFixed       = TRUE
       vlBIFixedAll    = FALSE
       vnBIFixedSize   = 1024000.0
       viBIFixedNum    = 1
       
       vlAIKeepCurrent = TRUE
       vlAIOn          = FALSE
       vcAILocation    = ""
       vlAIFixed       = TRUE
       vlAIFixedAll    = TRUE
       vnAIFixedSize   = 1024000.0
       viAIFixedNum    = 10
       
       vnFillRate      = 80
       vlToScreen      = TRUE
       vlToFile        = FALSE
       vcFileName      = PDBNAME("DICTDB") + ".st".
       
FIND FIRST DICTDB._AreaExtent
     WHERE CAN-FIND(DICTDB._Area 
                    WHERE RECID(_Area)     EQ _AreaExtent._Area-Recid AND
                          _Area._Area-Type EQ 3)
     NO-LOCK NO-ERROR.
IF AVAIL(_AreaExtent)
THEN DO:
   ASSIGN vcBILocation  = _AreaExtent._Extent-Path
          ENTRY(NUM-ENTRIES(vcBILocation, "/"),vcBILocation, "/") = ""
          vcBILocation  = RIGHT-TRIM(vcBILocation, "/")
          .
   FIND FIRST DICTDB._AreaExtent
        WHERE _AreaExtent._Extent-Size GT 0 AND
              CAN-FIND(DICTDB._Area 
                       WHERE RECID(_Area)     EQ _AreaExtent._Area-Recid AND
                             _Area._Area-Type EQ 3)
        NO-LOCK NO-ERROR.
   ASSIGN vlBIFixed     = AVAIL(_AreaExtent)
          vnBIFixedSize = (IF AVAIL(_AreaExtent)
                           THEN _AreaExtent._Extent-Size
                           ELSE 1024000).

   FIND FIRST DICTDB._AreaExtent
        WHERE _AreaExtent._Extent-Size EQ 0 AND
              CAN-FIND(DICTDB._Area 
                       WHERE RECID(_Area)     EQ _AreaExtent._Area-Recid AND
                             _Area._Area-Type EQ 3)
        NO-LOCK NO-ERROR.
   ASSIGN vlBIFixedAll = NOT AVAIL(_AreaExtent).

   ASSIGN viBIFixedNum = 0.
   FOR EACH DICTDB._Area 
       WHERE _Area._Area-Type EQ 3
       NO-LOCK,
       EACH DICTDB._AreaExtent
       WHERE _AreaExtent._Area-Recid  EQ RECID(_Area) AND
             _AreaExtent._Extent-Size GT 0 
       NO-LOCK:
      ASSIGN viBIFixedNum = viBIFixedNum + 1.
   END.
END.

FIND FIRST DICTDB._AreaExtent
     WHERE CAN-FIND(DICTDB._Area 
                    WHERE RECID(_Area)     EQ _AreaExtent._Area-Recid AND
                          _Area._Area-Type EQ 7)
     NO-LOCK NO-ERROR.
IF AVAIL(_AreaExtent)
THEN DO:
   ASSIGN vlAIOn        = TRUE
          vcAILocation  = _AreaExtent._Extent-Path
          ENTRY(NUM-ENTRIES(vcAILocation, "/"),vcAILocation, "/") = ""
          vcAILocation  = RIGHT-TRIM(vcAILocation, "/")
          .
   FIND FIRST DICTDB._AreaExtent
        WHERE _AreaExtent._Extent-Size GT 0 AND
              CAN-FIND(DICTDB._Area 
                       WHERE RECID(_Area)     EQ _AreaExtent._Area-Recid AND
                             _Area._Area-Type EQ 7)
        NO-LOCK NO-ERROR.
   ASSIGN vlAIFixed     = AVAIL(_AreaExtent)
          vnAIFixedSize = (IF AVAIL(_AreaExtent)
                           THEN _AreaExtent._Extent-Size
                           ELSE 102400).

   FIND FIRST DICTDB._AreaExtent
        WHERE _AreaExtent._Extent-Size EQ 0 AND
              CAN-FIND(DICTDB._Area 
                       WHERE RECID(_Area)     EQ _AreaExtent._Area-Recid AND
                             _Area._Area-Type EQ 7)
        NO-LOCK NO-ERROR.
   ASSIGN vlAIFixedAll = NOT AVAIL(_AreaExtent).

   ASSIGN viAIFixedNum = 0.
   FOR EACH DICTDB._Area 
       WHERE _Area._Area-Type EQ 7
       NO-LOCK,
       EACH DICTDB._AreaExtent
       WHERE _AreaExtent._Area-Recid  EQ RECID(_Area) AND
             _AreaExtent._Extent-Size GT 0 
       NO-LOCK:
      ASSIGN viAIFixedNum = viAIFixedNum + 1.
   END.
END.

mainBlock:
REPEAT:
CLEAR FRAME frmScreen ALL NO-PAUSE.
PAUSE BEFORE-HIDE.
VIEW FRAME frmParams.
OUTPUT STREAM stStruct CLOSE.


DISPLAY vlBIKeepCurrent
        vcBILocation
        vlBIFixed      
        vlBIFixedAll   
        vnBIFixedSize  
        viBIFixedNum   
        
        vlAIKeepCurrent
        vcAILocation
        vlAIOn         
        vlAIFixed      
        vlAIFixedAll   
        vnAIFixedSize  
        viAIFixedNum   
        
        vnFillRate     
        
        vlToScreen     
        vlToFile       
        vcFileName
        WITH FRAME frmParams.

UPDATE vlBIKeepCurrent
       vcBILocation     WHEN vlAdvancedMode
       vlBIFixed        
       vlBIFixedAll     WHEN vlAdvancedMode
       vnBIFixedSize    WHEN vlAdvancedMode
       viBIFixedNum     WHEN vlAdvancedMode
       
       vlAIKeepCurrent  WHEN vlAdvancedMode
       vlAIOn           WHEN vlAdvancedMode
       vcAILocation     WHEN vlAdvancedMode    
       vlAIFixed        WHEN vlAdvancedMode
       vlAIFixedAll     WHEN vlAdvancedMode
       vnAIFixedSize    WHEN vlAdvancedMode
       viAIFixedNum     WHEN vlAdvancedMode
       
       vnFillRate     
       
       vlToScreen     
       vlToFile       
       vcFileName
       GO-ON("CTRL-X")
       WITH FRAME frmParams.

IF KEYLABEL(LASTKEY) EQ "CTRL-X"
THEN DO:
   ASSIGN vlAdvancedMode = NOT vlAdvancedMode EQ TRUE.
   NEXT mainBlock.
END.

IF (vlAdvancedMode NE TRUE AND
    vnFillRate     LT 80)  OR
   vnFillRate      EQ ?
THEN DO:
   MESSAGE "Fill Rate must be at least 80%"
           VIEW-AS ALERT-BOX.
   NEXT mainBlock.
END.

IF vlToFile    EQ TRUE AND
   (vcFileName EQ ?    OR
    vcFileName EQ "")
THEN NEXT mainBlock.

IF vlToScreen NE TRUE AND
   vlToFile   NE TRUE
THEN NEXT mainBlock.

IF vlToFile EQ TRUE 
THEN DO:
   PAUSE 0 BEFORE-HIDE.

   IF SEARCH(vcFileName) NE ?
   THEN OS-COPY VALUE(SEARCH(vcFileName)) 
                VALUE(SUBSTITUTE("&1.backup&2&3&4-&5",
                                 SEARCH(vcFileName),
                                 STRING(YEAR(TODAY),"9999"),
                                 STRING(MONTH(TODAY),"99"),
                                 STRING(DAY(TODAY),"99"),
                                 REPLACE(STRING(TIME,"hh:mm:ss"),":","")
                                 )).
   OUTPUT STREAM stStruct TO VALUE(vcFileName).
   OUTPUT STREAM stStruct CLOSE.
   
   OUTPUT STREAM stStruct TO VALUE(vcFileName).
END.

HIDE FRAME frmParams NO-PAUSE.

ASSIGN lnDBSize   = 0
       lnDataSize = 0.

putHeaderLine(lcDB).

FOR EACH DICTDB._Area
    WHERE _Area._Area-Number GT 1
    NO-LOCK 
    BREAK BY _Area._Area-Type
          BY _Area._Area-Number
    WITH FRAME frmScreen:

   CASE _Area._Area-Type:
      WHEN 3 
      THEN ASSIGN lcType = _Area._Area-Name.
      WHEN 6
      THEN DO:
         IF _Area._Area-Number EQ 6
         THEN ASSIGN lcType = _Area._Area-Name.
         ELSE ASSIGN lcType = (IF _Area._Area-Name MATCHES "*_IDX"
                               THEN "Index Area"
                               ELSE "Data Area").
                               
      END.
      WHEN 7
      THEN ASSIGN lcType = "After Image".
   END CASE.
   
   FIND FIRST DICTDB._AreaExtent
        WHERE _AreaExtent._Area-Recid EQ RECID(_Area)
        NO-LOCK.
                            
   ASSIGN lcLocation = _AreaExtent._Extent-Path
          ENTRY(NUM-ENTRIES(lcLocation, "/"),lcLocation, "/") = ""
          lcLocation = RIGHT-TRIM(lcLocation, "/").
                                   
   IF _Area._Area-Type NE 7            OR
      (FIRST-OF(_Area._Area-Type) AND
       vlAIKeepCurrent EQ TRUE)
   THEN DO:
      putAreaHeaderLine(lcType).

      CASE lcType:
         WHEN "Schema Area"           OR
         WHEN "Primary Recovery Area"
         THEN putAreaInfoLine("Located in", SUBSTITUTE("&1",lcLocation)).
         WHEN "After Image"
         THEN DO:
            IF vlAIKeepCurrent EQ TRUE
            THEN putAreaInfoLine("Located in", SUBSTITUTE("&1",lcLocation)).
         END.
      END CASE.
   
      CASE lcType:
         WHEN "Data Area" OR
         WHEN "Index Area" 
         THEN DO:
            putAreaInfoLine("Area Number", SUBSTITUTE("&1",_Area._Area-Number)).
            putAreaInfoLine("Area Name", SUBSTITUTE("&1",_Area._Area-Name)).
         END.
      END CASE. 

      CASE lcType:
         WHEN "Schema Area" OR
         WHEN "Data Area"   OR
         WHEN "Index Area"
         THEN DO:
            putAreaInfoLine("Storage", 
                            SUBSTITUTE("Type &1",TRIM(STRING(_Area._Area-ClusterSize EQ 1,"I/II")))).
            putAreaInfoLine("Records/Block", 
                            SUBSTITUTE("&1",EXP(2,_Area._Area-RecBits))).
            putAreaInfoLine("Blocks/Cluster", 
                            SUBSTITUTE("&1",_Area._Area-ClusterSize)).
         END.
      END CASE.
   END.

   CASE lcType:
      WHEN "Data Area"   OR
      WHEN "Index Area"
      THEN DO:
         ASSIGN lcLocation = lcLocation + "/" + lcDB
                lnTotalSize = getSize("Data",
                                      _Area._Area-Number,
                                      _Area._Area-BlockSize)
                lnExtentSize = calcExtentSize(lnTotalSize)
                liNumExtents = calcNumExtents(lnTotalSize)
                .
                                
         putAreaInfoLine("Extent Size", 
                         SUBSTITUTE("&1 KB",TRIM(STRING(lnExtentSize * liNumExtents,">>>,>>>,>>>,>>9")))).
         putAreaInfoLine("Data Size  ", 
                         SUBSTITUTE("&1 KB",TRIM(STRING(lnTotalSize,">>>,>>>,>>>,>>9")))).
         putAreaInfoLine("Fill Rate  ", 
                         SUBSTITUTE("&1%",TRIM(STRING((lnTotalSize / (lnExtentSize * liNumExtents)) * 100,">>9.99")))).

         DO liCounter = 1 TO liNumExtents:
            putAreaExtentLine("d",
                              _Area._Area-Name,
                              _Area._Area-Number,
                              EXP(2,_Area._Area-RecBits),
                              _Area._Area-ClusterSize,
                              lcLocation,
                              liCounter,
                              TRUE,
                              lnExtentSize).
         END.
         putAreaExtentLine("d",
                           _Area._Area-Name,
                           _Area._Area-Number,
                           EXP(2,_Area._Area-RecBits),
                           _Area._Area-ClusterSize,
                           lcLocation,
                           liNumExtents + 1,
                           FALSE,
                           ?).
         ASSIGN lnDBSize   = lnDBSize   + (lnExtentSize * liNumExtents)
                lnDataSize = lnDataSize + lnTotalSize
                .
      END.
      WHEN "Primary Recovery Area"
      THEN DO:
         IF vlBIKeepCurrent EQ TRUE
         THEN DO:
            FOR EACH DICTDB._AreaExtent
                WHERE _AreaExtent._Area-Recid EQ RECID(_Area)
                NO-LOCK:
               putAreaExtentLine("b",
                                 ?,
                                 ?,
                                 ?,
                                 ?,
                                 _AreaExtent._Extent-Path,
                                 ?,
                                 _AreaExtent._Extent-Size GT 0,
                                 DEC(_AreaExtent._Extent-Size)).
            END.
         END.
         ELSE DO:
            ASSIGN lcLocation = RIGHT-TRIM(vcBILocation, "/").

            IF vlBIFixed EQ TRUE
            THEN DO:
               ASSIGN lcLocation = lcLocation + "/" + lcDB.

               DO liCounter =  1 TO viBIFixedNum:
                  putAreaExtentLine("b",
                                    ?,
                                    ?,
                                    ?,
                                    ?,
                                    lcLocation,
                                    liCounter,
                                    TRUE,
                                    vnBIFixedSize).
               END.
            END.

            IF vlBIFixed    NE TRUE OR
               vlBIFixedAll NE TRUE
            THEN DO:
               putAreaExtentLine("b",
                                 ?,
                                 ?,
                                 ?,
                                 ?,
                                 lcLocation,
                                 (IF vlBIFixed EQ TRUE
                                  THEN viBIFixedNum + 1
                                  ELSE ?),
                                 FALSE,
                                 ?).
            END.
         END.
      END.
      WHEN "Schema Area"
      THEN DO:
         putAreaExtentLine("d",
                           _Area._Area-Name,
                           _Area._Area-Number,
                           EXP(2,_Area._Area-RecBits),
                           1, /*_Area._Area-ClusterSize,*/
                           lcLocation,
                           ?,
                           FALSE,
                           ?).
      END.
      WHEN "After Image"
      THEN DO:
         IF vlAIKeepCurrent EQ TRUE
         THEN putAreaExtentLine("a",
                                ?,
                                ?,
                                ?,
                                ?,
                                _AreaExtent._Extent-Path,
                                ?,
                                _AreaExtent._Extent-Size GT 0,
                                DEC(_AreaExtent._Extent-Size)).
      END.
   END CASE.                                   


   IF _Area._Area-Type NE 7      OR
      (LAST-OF(_Area._Area-Type) AND
       vlAIKeepCurrent EQ TRUE)
   THEN DO:
      putLine("").
      CLEAR FRAME frmScreen ALL.
   END.
END.
IF vlAIKeepCurrent NE TRUE AND
   vlAIOn          EQ TRUE
THEN DO:
   ASSIGN lcLocation = RIGHT-TRIM(vcAILocation, "/").

   putAreaHeaderLine("After Image").
   putAreaInfoLine("Located in", SUBSTITUTE("&1",lcLocation)).

   IF vlAIFixed EQ TRUE
   THEN DO:
      ASSIGN lcLocation = lcLocation + "/" + lcDB.

      DO liCounter =  1 TO viAIFixedNum:
         putAreaExtentLine("a",
                           ?,
                           ?,
                           ?,
                           ?,
                           lcLocation,
                           liCounter,
                           TRUE,
                           vnAIFixedSize).
      END.
   END.
   IF vlAIFixed    NE TRUE OR
      vlAIFixedAll NE TRUE
   THEN DO:
      putAreaExtentLine("a",
                        ?,
                        ?,
                        ?,
                        ?,
                        lcLocation,
                        (IF vlAIFixed EQ TRUE
                         THEN viAIFixedNum + 1
                         ELSE ?),
                        FALSE,
                        ?).
   END.
END.

putFooterLine().

OUTPUT STREAM stStruct CLOSE.

IF vlToScreen EQ TRUE
THEN
MESSAGE "Extent Size =" STRING(lnDBSize,">>>,>>>,>>9") "KB"     
        SKIP
        "Data Size   =" STRING(lnDataSize,">>>,>>>,>>9") "KB" 
        SKIP
        "Fill Rate   =" STRING((lnDataSize / lnDBSize) * 100, ">>>>>>>>>9.99%") 
        SKIP
        VIEW-AS ALERT-BOX TITLE "DB Values". 

END.
