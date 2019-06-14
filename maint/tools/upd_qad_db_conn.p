DEFINE NEW SHARED VARIABLE global_domain AS CHARACTER.

ASSIGN global_domain = "QAD".

FOR EACH xxdc_mstr
    TRANSACTION:

   DISPLAY SKIP(1)
           xxdc_pdbname         FORMAT "X(1024)" 
                                VIEW-AS FILL-IN SIZE 58 BY 1
           xxdc_ldbname         FORMAT "X(1024)" 
                                VIEW-AS FILL-IN SIZE 58 BY 1
           xxdc_dbtype          FORMAT "X(1024)" 
                                VIEW-AS FILL-IN SIZE 58 BY 1
           xxdc_network         FORMAT "X(1024)" 
                                VIEW-AS FILL-IN SIZE 58 BY 1
           xxdc_host            FORMAT "X(1024)" 
                                VIEW-AS FILL-IN SIZE 58 BY 1
           xxdc_service         FORMAT "X(1024)"
                                VIEW-AS FILL-IN SIZE 58 BY 1
           xxdc_trig_path       FORMAT "X(1024)"
                                VIEW-AS FILL-IN SIZE 58 BY 1
           xxdc_parm_file       FORMAT "X(1024)"
                                VIEW-AS FILL-IN SIZE 58 BY 1
           xxdc_parameters      FORMAT "X(1024)"
                                VIEW-AS FILL-IN SIZE 58 BY 1
           WITH 1 COL WIDTH 80
           TITLE "QAD Update DB Connect".
   DISPLAY xxdc_mstr.
   UPDATE xxdc_pdbname
          xxdc_ldbname
          xxdc_dbtype
          xxdc_network
          xxdc_host
          xxdc_service
          xxdc_trig_path
          xxdc_parm_file
          xxdc_parameters
          .
END.