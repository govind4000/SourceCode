 def var lcUser as   char.
update lcUser format "x(10)".


input from value("/home/ygo/temp/usr_mstr_" + lcUser + ".txt").
repeat :
   create usr_mstr.
   import delimiter "|" usr_mstr.
end.
input close.

input from value("/home/ygo/temp/usr_" + lcUser + ".txt").
repeat :
   create usr.
   import delimiter "|" usr.
end.
input  close.

input from value("/home/ygo/temp/usrDomain_" + lcUser + ".txt").
repeat :
   create usrDomain.
   import delimiter "|" usrDomain.
end.
input  close.


input from value("/home/ygo/temp/usrRoleDomain_" + lcUser + ".txt").
repeat :
   create usrRoleDomain.
   import delimiter "|" usrRoleDomain.
end.
input  close.
      
input from value("/home/ygo/temp/udd_" + lcUser + ".txt").
repeat :
    create udd_det.
    import delimiter "|" udd_det.
end.
output close.
