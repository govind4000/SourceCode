
def var lcdb as char init "qaddb".
def var lctable as char format "x(20)" init "cmt_det".
def var lcRecid as character format "x(50)".


repeat :

update lcdb 
       lctable
       lcRecid
       .

For each ReplQueue where ReplQueue.SrcDB      = lcdb and 
                         ReplQueue.SrcTable   = lctable and
                         SrcRecord      = lcRecid :
                                   
   Disp   EventDate EventType EventTime Username QThread.
    update Applied.
End.

End.