/* 
  How much of the storage area is used up by allocated blocks (HWM)? 
  What is the true size of the Database? 
  How fast is my database growing? 
  iow: What is the real occupation? 

  Output to cut/paste 2 Excel macro
*/                      
Def Var treshold          As Integer.

Def Var v-prcnt_full      As Dec Format ">>9.99"     Label "% Full"     no-undo.
Def Var v-empty_blocks    As Dec Format ">>,>>>,>>9" Label "Empty"      no-undo.
Def Var v-total_blocks    As Dec Format ">>,>>>,>>9" Label "Total"      no-undo.
Def Var v-hiwater         As Dec Format ">>,>>>,>>9" Label "Hiwater"    no-undo.
Def Var v-mb_empty_blocks As Dec Format ">>,>>>,>>9" Label "MB Empty"   no-undo.
Def Var v-mb_total_blocks As Dec Format ">>,>>>,>>9" Label "MB Total"   no-undo.
Def Var v-mb_hiwater      As Dec Format ">>,>>>,>>9" Label "MB Hiwater" no-undo.
Def Var v-mb_used         As Dec Format ">>>,>>9.99" Label "MB Used"    no-undo.
Def Var v-mb_avail        As Dec Format ">>>,>>9.99" Label "MB Avail"   no-undo.
Def Var v-mb_tused        As Dec Format ">>>,>>9.99" Label "Total MB used" 
                                                      Initial 0.0       no-undo.
Def Var v-mb_tavail       As Dec Format ">>>,>>9.99" Label "Total MB avail" 
                                                      Initial 0.0       no-undo.
Def Var b_used            As Char Format "x(16)"      Label "   Graphic".    
DEFINE VARIABLE liExtentsFixed AS INTEGER NO-UNDO.
DEFINE VARIABLE liExtentLast AS INTEGER NO-UNDO.

ASSIGN treshold = Int(session:parameter) .

DISP LDBNAME("DICTDB") + " - " +
     PDBNAME("DICTDB")
     FORMAT "X(76)" WITH FRAME b WIDTH 80 NO-LABELS.
 
FOR EACH DICTDB._Area NO-LOCK 
    BY DICTDB._Area._Area-type 
    BY DICTDB._Area._Area-num:
  FIND DICTDB._Areastatus WHERE _Areastatus-Areanum = _Area._Area-number NO-LOCK.
  
  FOR EACH DICTDB._AreaExtent
      WHERE _AreaExtent._Area-Number EQ _Area._Area-number
      NO-LOCK:
     IF _AreaExtent._Extent-size NE 0
     THEN ASSIGN liExtentsFixed = _AreaExtent._Extent-number.
     IF _AreaExtent._Extent-path EQ _AreaStatus._AreaStatus-LastExtent
     THEN ASSIGN liExtentLast =  _AreaExtent._Extent-number.
  END.

  v-hiwater = _AreaStatus-Hiwater .
  v-mb_hiwater = v-hiwater / 1048576 * _Area-Blocksize.
  If v-hiwater = ? THEN v-hiwater = 0.0.

  v-empty_blocks = _AreaStatus-Totblocks - v-hiwater - _AreaStatus-Extents.
  v-mb_empty_blocks = v-empty_blocks / 1048576 * _Area-Blocksize.
  v-total_blocks = _AreaStatus-Totblocks - _AreaStatus-Extents.
  v-mb_total_blocks = v-total_blocks / 1048576 * 8192.

  v-prcnt_full = (1.0 - (v-empty_blocks / _AreaStatus-Totblocks)) * 100.0.
      
  v-mb_avail = v-empty_blocks / 1048576 * _Area-BlockSize.
  v-mb_tavail = v-mb_tavail + v-mb_avail.
            
  v-mb_used = v-hiwater / 1048576 * _Area-BlockSize.
  v-mb_tused = v-mb_tused + v-mb_used.

  IF v-prcnt_full > 0  THEN b_used = "   |     |     |".
  IF v-prcnt_full > 10 THEN b_used = "   |*    |     |".
  IF v-prcnt_full > 20 THEN b_used = "   |**   |     |".
  IF v-prcnt_full > 30 THEN b_used = "   |***  |     |".
  IF v-prcnt_full > 40 THEN b_used = "   |**** |     |".
  IF v-prcnt_full > 50 THEN b_used = "   |*****|     |".
  IF v-prcnt_full > 60 THEN b_used = "   |*****|*    |".
  IF v-prcnt_full > 70 THEN b_used = "   |*****|**   |".
  IF v-prcnt_full > 80 THEN b_used = "   |*****|***  |".
  IF v-prcnt_full > 90 THEN b_used = "   |*****|**** |".
  IF v-prcnt_full > 95 THEN b_used = "   !*****!*****!".

  /*
  Pause 0 before-hide.
  */
  
  IF v-prcnt_full > treshold AND v-mb_total_blocks > treshold AND
     ("{1}" NE "fullOnly" OR
      ("{1}" EQ "fullOnly" AND (liExtentLast GE liExtentsFixed OR v-prcnt_full >= 80)))
  THEN DO:
  DISPLAY
      _Area-name  Format "x(21)"
      v-mb_total_blocks
      v-mb_hiwater
      /*
      v-mb_empty_blocks
      */
      SUBSTITUTE("&3&1 (F&2)",
                 liExtentLast,liExtentsFixed,
                 STRING(liExtentLast GT liExtentsFixed,"V/F"))
      LABEL "In Extent" FORMAT "X(10)"                     
      v-prcnt_full 
      b_used  SKIP
      With Frame A Down /*No-Box No-Label Down */.
  IF liExtentLast GT liExtentsFixed
  THEN DISPLAY 100 @ v-prcnt_full WITH FRAME a.
  END.
End.
