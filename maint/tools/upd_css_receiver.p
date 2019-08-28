FOR EACH wpro_control
    TRANSACTION:

   DISPLAY SKIP(1)
           user_directory       FORMAT "X(60)"   COLON 15
           wpro_receiver_url    FORMAT "X(1024)" COLON 15
                                LABEL "Receiver URL"
                                VIEW-AS FILL-IN SIZE 58 BY 1
           ">>"
           WITH SIDE-LABELS WIDTH 80
           TITLE "CSS Update Receiver".
   UPDATE wpro_receiver_url.
END.