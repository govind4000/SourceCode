def var lcUser as char.

update lcUser format "x(10)".

/**** Usr_mstr ****/
output to value("/home/ygo/temp/usr_mstr_" + lcUser + ".txt").

for each usr_mstr where usr_userid = lcUser :
   export delimiter "|" usr_mstr.
end.
output close.
   



/**** Usr ****/

output to value("/home/ygo/temp/usr_" + lcUser + ".txt").

for each usr where usrlogin = lcUser :
   export delimiter "|" usr.
end.
   
output close.

/**** UsrDomain ****/

output to value("/home/ygo/temp/usrDomain_" + lcUser + ".txt").
for each usr where usrlogin = lcUser:
for each UsrDomain where Usr.Usr_ID = UsrDomain.Usr_ID :
   export delimiter "|" usrDomain.
end.
end.
output close.

/**** UsrRoleDomain   ****/

output to value("/home/ygo/temp/usrRoleDomain_" + lcUser + ".txt").
for each usr where usrlogin = lcUser:
for each UsrDomain where Usr.Usr_ID = UsrDomain.Usr_ID :
for each UsrRoleDomain  where UsrRoleDomain.Domain_ID = UsrDomain.Domain_ID AND UsrRoleDomain.Usr_ID = UsrDomain.Usr_ID :
   export delimiter "|" UsrRoleDomain.
end.
end.
end.
output close.


output to value("/home/ygo/temp/udd_" + lcUser + ".txt").
for each udd_det where udd_userid = lcUser:
    export delimiter "|" udd_det.
end.
output close.
