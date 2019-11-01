FOR EACH wpro_registry
    WHERE wproKey EQ "QRAAppserverURL" OR
          wproKey EQ "CSSBaseURL"
    TRANSACTION:

   DISPLAY SKIP(1)
           wproKey      FORMAT "X(60)"   COLON 15
           wproKeyValue FORMAT "X(1024)" COLON 15
                        VIEW-AS FILL-IN SIZE 58 BY 1
           ">>"
           WITH SIDE-LABELS WIDTH 80
           TITLE "CSS Update Registry".
   UPDATE wproKeyValue.
END.