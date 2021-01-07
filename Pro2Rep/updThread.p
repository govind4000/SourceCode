def var i as int.

for each replqueue where srcdb = "qaddb" and
/*srctable = "Posting" */
/*srctable = "QPostingSaf"*/
/*srcTable = "QPostingLine"*/
/*srcTable = "PostingSaf" */
srcTable = "QPostingLine"
 and qthread =  06
:
assign qthread = 15.
i = i + 1.
if i = 50000 then leave.
end.

disp i.

