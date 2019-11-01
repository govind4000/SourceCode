FOR EACH isisc_
TRANSACTION :
DISPLAY 
isisc__chr01         FORMAT "X(1024)" VIEW-AS FILL-IN SIZE 60 BY 1
isisc__chr02         FORMAT "X(1024)" VIEW-AS FILL-IN SIZE 60 BY 1
isisc__chr03         FORMAT "X(1024)" VIEW-AS FILL-IN SIZE 60 BY 1
isisc__chr04         FORMAT "X(1024)" VIEW-AS FILL-IN SIZE 60 BY 1
isisc_db_main        FORMAT "X(1024)" VIEW-AS FILL-IN SIZE 60 BY 1
isisc_db_rd          FORMAT "X(1024)" VIEW-AS FILL-IN SIZE 60 BY 1
isisc_failed_dir     FORMAT "X(1024)" VIEW-AS FILL-IN SIZE 60 BY 1
isisc_from_prs_dir   FORMAT "X(1024)" VIEW-AS FILL-IN SIZE 60 BY 1
isisc_inact_codes    FORMAT "X(1024)" VIEW-AS FILL-IN SIZE 60 BY 1
isisc_is_src_dir     FORMAT "X(1024)" VIEW-AS FILL-IN SIZE 60 BY 1
isisc_log_dir        FORMAT "X(1024)" VIEW-AS FILL-IN SIZE 60 BY 1
isisc_prc_dir        FORMAT "X(1024)" VIEW-AS FILL-IN SIZE 60 BY 1
isisc_rrm_base_url   FORMAT "X(1024)" VIEW-AS FILL-IN SIZE 60 BY 1
isisc_rrm_dir        FORMAT "X(1024)" VIEW-AS FILL-IN SIZE 60 BY 1
isisc_rrm_url        FORMAT "X(1024)" VIEW-AS FILL-IN SIZE 60 BY 1
isisc_script_dir     FORMAT "X(1024)" VIEW-AS FILL-IN SIZE 60 BY 1
isisc_success_dir    FORMAT "X(1024)" VIEW-AS FILL-IN SIZE 60 BY 1
isisc_ver_dir        FORMAT "X(1024)" VIEW-AS FILL-IN SIZE 60 BY 1
isisc_ws_src_dir     FORMAT "X(1024)" VIEW-AS FILL-IN SIZE 60 BY 1
WITH 1 COL WIDTH 320
.
PAUSE.
UPDATE isisc_ WITH 1 COL WIDTH 320.
