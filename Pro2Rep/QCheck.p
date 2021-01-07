def var i as int.
def var cnt as int.
form i cnt with frame a.
repeat:
do i = 1 to 15:
disp  i with frame a.
select /*srctable, eventtype,*/ count(*)  from replqueue where applied = no and qthread = i /*group by srctable, eventtype*/ with frame a down.

down 1 with frame a.

hide message.
end.
pause 5.
end.
