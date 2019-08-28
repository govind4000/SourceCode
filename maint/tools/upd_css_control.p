FOR EACH wpro_control
    TRANSACTION:

   DISPLAY wpro_control
           WITH SIDE-LABELS 1 COL WIDTH 80
           TITLE "CSS Update Control".
   UPDATE temp_directory
          user_directory
          debug_flag
          wpro_receiver_url
          .
   IF TRUE THEN NEXT.       
   UPDATE wpro_control.
          
   DISPLAY temp_directory
           VIEW-AS FILL-IN SIZE 40 BY 1
           image_path
           VIEW-AS FILL-IN SIZE 40 BY 1
           script_path
           VIEW-AS FILL-IN SIZE 40 BY 1
           style_path
           VIEW-AS FILL-IN SIZE 40 BY 1
           user_directory
           VIEW-AS FILL-IN SIZE 40 BY 1
           user_language
           VIEW-AS FILL-IN SIZE 40 BY 1
           database_description
           VIEW-AS FILL-IN SIZE 40 BY 1
           help_path
           VIEW-AS FILL-IN SIZE 40 BY 1
           wpro_receiver_url
           VIEW-AS FILL-IN SIZE 40 BY 1
           .
END.