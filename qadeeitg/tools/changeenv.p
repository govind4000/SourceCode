FUNCTION displayChange RETURNS CHARACTER
   (INPUT                  ipcTable       AS    CHARACTER,
    INPUT                  ipcKey         AS    CHARACTER,
    INPUT                  ipcField       AS    CHARACTER,
    INPUT                  ipcValueOld    AS    CHARACTER,
    INPUT                  ipcValueNew    AS    CHARACTER):

   RETURN ?.
END FUNCTION.

FUNCTION environmentChange RETURNS CHARACTER
   (INPUT                  ipcValueOld       AS    CHARACTER):
   DEFINE VARIABLE         lcValueNew        AS    CHARACTER      NO-UNDO.
   
   ASSIGN lcValueNew = REPLACE(ipcValueOld, "_prd_", "_itg_")
          .
          
   RETURN "".
END FUNCTION.

FUNCTION updateAppServer RETURNS CHARACTER
   ():
   DEFINE BUFFER           bf_aps_mstr    FOR   aps_mstr.
   
   aps_mstr:
   FOR EACH aps_mstr
       NO-LOCK
       TRANSACTION:
      
      IF environmentValue(aps_mstr.aps_appservice_name) EQ aps_mstr.aps_appservice_name
      THEN NEXT aps_mstr.
      
      displayChange("aps_mstr", 
                    aps_mstr.aps_name, 
                    "aps_appservice_name",
                    aps_mstr.aps_appservice_name,
                    environmentValue(aps_mstr.aps_appservice_name)).
                    
      FIND bf_aps_mstr
           WHERE ROWID(bf_aps_mstr) EQ ROWID(aps_mstr)
           EXCLUSIVE-LOCK NO-ERROR.
      ASSIGN bf_aps_mstr.aps_appservice_name = environmentValue(aps_mstr.aps_appservice_name)
             .
      
   END.
   
   RETURN "".
END FUNCTION.

updateAppServer().
/*
updateQXtendQXI(). /*qxi_name*/

qxi_tomcat_host
*/
