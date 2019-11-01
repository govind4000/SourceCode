FOR EACH code_mstr
    WHERE code_value MATCHES "/opt*" OR
          code_cmmt  MATCHES "/opt*"
    TRANSACTION:

   DISPLAY SKIP(1)
           code_domain          FORMAT "X(60)"   COLON 15
           code_fldname         FORMAT "X(60)"   COLON 15
           code_group           FORMAT "X(60)"   COLON 15
           code_value           FORMAT "X(1024)" COLON 15
                                VIEW-AS FILL-IN SIZE 58 BY 1
           ">>"
           code_cmmt            FORMAT "X(1024)" COLON 15
                                VIEW-AS FILL-IN SIZE 58 BY 1
           ">>"
           WITH SIDE-LABELS WIDTH 80
           TITLE "QAD Update Path".
   UPDATE code_value
          code_cmmt.
END.