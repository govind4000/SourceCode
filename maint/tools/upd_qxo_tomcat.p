DEFINE VARIABLE         lcEnvironment  AS    CHARACTER      NO-UNDO.
DEFINE VARIABLE         lcHost         AS    CHARACTER      NO-UNDO.
DEFINE VARIABLE         lcPort         AS    CHARACTER      NO-UNDO.

ASSIGN lcEnvironment = ENTRY(NUM-ENTRIES(DBNAME,CHR(47)),DBNAME,CHR(47))
       lcEnvironment = ENTRY(2, lcEnvironment, "_").

CASE lcEnvironment:
   WHEN "prd"
   THEN ASSIGN lcHost = "qadee-pro.houston.hpicorp.net".
   OTHERWISE ASSIGN lcHost = "qadee-pro.houston.hpicorp.net".
END CASE.

CASE lcEnvironment:
   WHEN "prd"
   THEN ASSIGN lcPort = "20110".
   WHEN "itg"
   THEN ASSIGN lcPort = "20210".
   WHEN "qa"
   THEN ASSIGN lcPort = "20310".
END CASE.
         
FOR EACH mpm_subs_comm_param
    WHERE param_name MATCHES "TOMCAT*"
    TRANSACTION:

   CASE param_name:
      WHEN "TOMCATHOST"
      THEN ASSIGN parm_value = lcHost.
      WHEN "TOMCATPORT"
      THEN ASSIGN parm_value = lcPort.
   END CASE.

   DISPLAY SKIP(1)
           WITH 1 COL SIDE-LABELS WIDTH 80
           TITLE "QXO Update Tomcat".
   UPDATE mpm_subs_comm_param
          .
END.