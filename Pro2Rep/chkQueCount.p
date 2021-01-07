 def var i as int.
 def var j as int.

   for each replqueue no-lock
   where qthread = 6
   break by qthread by srctable
                              :

                if last-of(srctable) then do:
                    disp qthread i srctable j .
                              j = 0.
               end.
               if last-of(qthread) then do:
                    disp "Queue " qthread format "->>" " Total : "  i.
                    i = 0.
               end.




                       i = i + 1.
                       j = j + 1.
   end.
disp i.

