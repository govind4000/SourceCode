&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME winCompile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS winCompile 
/* PSC: compile.w - Compiler                                                */
/* COPYRIGHT Indigo  ALL RIGHTS RESERVED. THIS IS AN UNPUBLISHED WORK.      */
/*                                                                          */
/* Category : C2                                                            */
/* - A      : Updates Std. MFG/PRO Database                                 */
/* - B      : Updates special Database                                      */
/* - C      : Non-Updating                                                  */
/* - 1      : Modified Std. MFG/PRO Program                                 */
/* - 2      : New Program                                                   */
/*                                                                          */
/*                                                                          */
/* %Z% SCCS-Id:%P% %I% %G% %U% tk                                           */
/* REVISION: eB   LAST MODIFIED: 17 Sep 2015  BY: HP/RDU                    */

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE         vcPropath               AS    CHARACTER      NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frmCompile

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lcSrcCodeFormat liRcodeDestFormat ~
btnCompileList btnDestinationDirectory btnTopLevelList btnNoCompileList ~
btnProgOnlyList btnOraOnlyList lcCompileDatabases lcCompilePropath 
&Scoped-Define DISPLAYED-OBJECTS lblSrcCodeFormat lcSrcCodeFormat ~
liRcodeDestFormat lcCompileList lcDestinationDirectory lcTopLevelList ~
lcNoCompileList lcProgOnlyList lcOraOnlyList lcCompileDatabases ~
lcCompilePropath lblRcodeDestFormat lblCompileList lblDestinationDirectory ~
lblTopLevelList lblNoCompileList lblProgOnlyList lblOraOnlyList ~
lblDatabases lblCompilePropath 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD backupPropath winCompile 
FUNCTION backupPropath RETURNS LOGICAL
   () FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD connectDBS winCompile 
FUNCTION connectDBS RETURNS LOGICAL
   ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD disconnectDBS winCompile 
FUNCTION disconnectDBS RETURNS LOGICAL
   ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSessionParameters winCompile 
FUNCTION getSessionParameters RETURNS CHARACTER
   () FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD restorePropath winCompile 
FUNCTION restorePropath RETURNS LOGICAL
   () FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR winCompile AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mnuFile 
       MENU-ITEM mnuOpen        LABEL "&Open"         
       MENU-ITEM mnuSave        LABEL "&Save"         
       MENU-ITEM mnuSaveAs      LABEL "Save &As"      .

DEFINE MENU menuCompile MENUBAR
       SUB-MENU  mnuFile        LABEL "&File"         
       MENU-ITEM mnuCompile     LABEL "&Compile"      .


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCompileList 
     LABEL ".." 
     SIZE 4 BY 1
     FONT 0.

DEFINE BUTTON btnDestinationDirectory 
     LABEL ".." 
     SIZE 4 BY 1
     FONT 0.

DEFINE BUTTON btnNoCompileList 
     LABEL ".." 
     SIZE 4 BY 1
     FONT 0.

DEFINE BUTTON btnOraOnlyList 
     LABEL ".." 
     SIZE 4 BY 1
     FONT 0.

DEFINE BUTTON btnProgOnlyList 
     LABEL ".." 
     SIZE 4 BY 1
     FONT 0.

DEFINE BUTTON btnTopLevelList 
     LABEL ".." 
     SIZE 4 BY 1
     FONT 0.

DEFINE VARIABLE lcCompileDatabases AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 59 BY 5
     BGCOLOR 15 FONT 0 NO-UNDO.

DEFINE VARIABLE lcCompilePropath AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 59 BY 5
     BGCOLOR 15 FONT 0 NO-UNDO.

DEFINE VARIABLE lblCompileList AS CHARACTER FORMAT "X(18)":U INITIAL "Compile File List" 
      VIEW-AS TEXT 
     SIZE 18 BY 1
     FONT 0 NO-UNDO.

DEFINE VARIABLE lblCompilePropath AS CHARACTER FORMAT "X(18)":U INITIAL "Compile PROPATH" 
      VIEW-AS TEXT 
     SIZE 18 BY 1
     FONT 0 NO-UNDO.

DEFINE VARIABLE lblDatabases AS CHARACTER FORMAT "X(18)":U INITIAL "Compile Databases" 
      VIEW-AS TEXT 
     SIZE 18 BY 1
     FONT 0 NO-UNDO.

DEFINE VARIABLE lblDestinationDirectory AS CHARACTER FORMAT "X(18)":U INITIAL "Destination Path" 
      VIEW-AS TEXT 
     SIZE 18 BY 1
     FONT 0 NO-UNDO.

DEFINE VARIABLE lblNoCompileList AS CHARACTER FORMAT "X(18)":U INITIAL "No Compile List" 
      VIEW-AS TEXT 
     SIZE 18 BY 1
     FONT 0 NO-UNDO.

DEFINE VARIABLE lblOraOnlyList AS CHARACTER FORMAT "X(18)":U INITIAL "ORACLE Only List" 
      VIEW-AS TEXT 
     SIZE 18 BY 1
     FONT 0 NO-UNDO.

DEFINE VARIABLE lblProgOnlyList AS CHARACTER FORMAT "X(18)":U INITIAL "PROGRESS Only List" 
      VIEW-AS TEXT 
     SIZE 18 BY 1
     FONT 0 NO-UNDO.

DEFINE VARIABLE lblRcodeDestFormat AS CHARACTER FORMAT "X(18)":U INITIAL "R-Code Format" 
      VIEW-AS TEXT 
     SIZE 18 BY 1
     FONT 0 NO-UNDO.

DEFINE VARIABLE lblSrcCodeFormat AS CHARACTER FORMAT "X(18)":U INITIAL "Source Code Format" 
      VIEW-AS TEXT 
     SIZE 18 BY 1
     FONT 0 NO-UNDO.

DEFINE VARIABLE lblTopLevelList AS CHARACTER FORMAT "X(18)":U INITIAL "Top Level List" 
      VIEW-AS TEXT 
     SIZE 18 BY 1
     FONT 0 NO-UNDO.

DEFINE VARIABLE lcCompileList AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1
     BGCOLOR 15 FONT 0 NO-UNDO.

DEFINE VARIABLE lcDestinationDirectory AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1
     BGCOLOR 15 FONT 0 NO-UNDO.

DEFINE VARIABLE lcNoCompileList AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1
     BGCOLOR 15 FONT 0 NO-UNDO.

DEFINE VARIABLE lcOraOnlyList AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1
     BGCOLOR 15 FONT 0 NO-UNDO.

DEFINE VARIABLE lcProgOnlyList AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1
     BGCOLOR 15 FONT 0 NO-UNDO.

DEFINE VARIABLE lcTopLevelList AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1
     BGCOLOR 15 FONT 0 NO-UNDO.

DEFINE VARIABLE lcSrcCodeFormat AS CHARACTER INITIAL "twoletter" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Two Letter    ", "twoletter",
"Flat          ", "flat",
"None          ", "none"
     SIZE 55 BY 1
     FONT 0 NO-UNDO.

DEFINE VARIABLE liRcodeDestFormat AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Staggered     ", 1,
"Flat          ", 2,
"Same as Source", 3
     SIZE 55 BY 1
     FONT 0 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frmCompile
     lblSrcCodeFormat AT ROW 1 COL 1 NO-LABEL WIDGET-ID 56
     lcSrcCodeFormat AT ROW 1 COL 20 NO-LABEL WIDGET-ID 16
     liRcodeDestFormat AT ROW 2 COL 20 NO-LABEL WIDGET-ID 24
     lcCompileList AT ROW 3 COL 18 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     btnCompileList AT ROW 3 COL 75 WIDGET-ID 58
     lcDestinationDirectory AT ROW 4 COL 18 COLON-ALIGNED HELP
          "Select Destination Path" NO-LABEL WIDGET-ID 6
     btnDestinationDirectory AT ROW 4 COL 75 WIDGET-ID 60
     lcTopLevelList AT ROW 5 COL 18 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     btnTopLevelList AT ROW 5 COL 75 WIDGET-ID 62
     lcNoCompileList AT ROW 6 COL 18 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     btnNoCompileList AT ROW 6 COL 75 WIDGET-ID 64
     lcProgOnlyList AT ROW 7 COL 18 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     btnProgOnlyList AT ROW 7 COL 75 WIDGET-ID 66
     lcOraOnlyList AT ROW 8 COL 18 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     btnOraOnlyList AT ROW 8 COL 75 WIDGET-ID 68
     lcCompileDatabases AT ROW 9 COL 20 NO-LABEL WIDGET-ID 76
     lcCompilePropath AT ROW 14 COL 20 NO-LABEL WIDGET-ID 20
     lblRcodeDestFormat AT ROW 2 COL 1 NO-LABEL WIDGET-ID 50
     lblCompileList AT ROW 3 COL 1 NO-LABEL WIDGET-ID 40
     lblDestinationDirectory AT ROW 4 COL 1 NO-LABEL WIDGET-ID 42
     lblTopLevelList AT ROW 5 COL 1 NO-LABEL WIDGET-ID 52
     lblNoCompileList AT ROW 6 COL 1 NO-LABEL WIDGET-ID 44
     lblProgOnlyList AT ROW 7 COL 1 NO-LABEL WIDGET-ID 48
     lblOraOnlyList AT ROW 8 COL 1 NO-LABEL WIDGET-ID 46
     lblDatabases AT ROW 9 COL 1 NO-LABEL WIDGET-ID 70
     lblCompilePropath AT ROW 14 COL 1 NO-LABEL WIDGET-ID 54
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 20 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW winCompile ASSIGN
         HIDDEN             = YES
         TITLE              = "Compiler"
         HEIGHT             = 20
         WIDTH              = 80
         MAX-HEIGHT         = 39.15
         MAX-WIDTH          = 274.29
         VIRTUAL-HEIGHT     = 39.15
         VIRTUAL-WIDTH      = 274.29
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU menuCompile:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW winCompile
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME frmCompile
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN lblCompileList IN FRAME frmCompile
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lblCompilePropath IN FRAME frmCompile
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lblDatabases IN FRAME frmCompile
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lblDestinationDirectory IN FRAME frmCompile
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lblNoCompileList IN FRAME frmCompile
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lblOraOnlyList IN FRAME frmCompile
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lblProgOnlyList IN FRAME frmCompile
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lblRcodeDestFormat IN FRAME frmCompile
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lblSrcCodeFormat IN FRAME frmCompile
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lblTopLevelList IN FRAME frmCompile
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN lcCompileList IN FRAME frmCompile
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lcDestinationDirectory IN FRAME frmCompile
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lcNoCompileList IN FRAME frmCompile
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lcOraOnlyList IN FRAME frmCompile
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lcProgOnlyList IN FRAME frmCompile
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lcTopLevelList IN FRAME frmCompile
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(winCompile)
THEN winCompile:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME winCompile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL winCompile winCompile
ON END-ERROR OF winCompile /* Compiler */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL winCompile winCompile
ON WINDOW-CLOSE OF winCompile /* Compiler */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCompileList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCompileList winCompile
ON CHOOSE OF btnCompileList IN FRAME frmCompile /* .. */
DO:
   RUN filesel.p
       (INPUT  SUBSTITUTE("Select &1", lblCompileList),
        INPUT  ?,
        INPUT  "*.wrk",
        OUTPUT lcCompileList).
   RUN enable_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDestinationDirectory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDestinationDirectory winCompile
ON CHOOSE OF btnDestinationDirectory IN FRAME frmCompile /* .. */
DO:
   RUN dirsel.p
       (INPUT  SUBSTITUTE("Select &1", lblDestinationDirectory),
        INPUT  ".",
        OUTPUT lcDestinationDirectory).
   RUN enable_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNoCompileList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNoCompileList winCompile
ON CHOOSE OF btnNoCompileList IN FRAME frmCompile /* .. */
DO:
   RUN filesel.p
       (INPUT  SUBSTITUTE("Select &1", lblNoCompileList),
        INPUT  ?,
        INPUT  "*.wrk",
        OUTPUT lcNoCompileList).
   RUN enable_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOraOnlyList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOraOnlyList winCompile
ON CHOOSE OF btnOraOnlyList IN FRAME frmCompile /* .. */
DO:
   RUN filesel.p
       (INPUT  SUBSTITUTE("Select &1", lblOraOnlyList),
        INPUT  ?,
        INPUT  "*.wrk",
        OUTPUT lcOraOnlyList).
   RUN enable_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnProgOnlyList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnProgOnlyList winCompile
ON CHOOSE OF btnProgOnlyList IN FRAME frmCompile /* .. */
DO:
   RUN filesel.p
       (INPUT  SUBSTITUTE("Select &1", lblProgOnlyList),
        INPUT  ?,
        INPUT  "*.wrk",
        OUTPUT lcProgOnlyList).
   RUN enable_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTopLevelList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTopLevelList winCompile
ON CHOOSE OF btnTopLevelList IN FRAME frmCompile /* .. */
DO:
   RUN filesel.p
       (INPUT  SUBSTITUTE("Select &1", lblTopLevelList),
        INPUT  ?,
        INPUT  "*.wrk",
        OUTPUT lcTopLevelList).
   RUN enable_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mnuCompile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mnuCompile winCompile
ON CHOOSE OF MENU-ITEM mnuCompile /* Compile */
DO:
   RUN doCompile.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK winCompile 


/* ***************************  Main Block  *************************** */

{xxpersistent.i}

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
mainBlock:
DO ON ERROR   UNDO mainBlock, LEAVE mainBlock
   ON END-KEY UNDO mainBlock, LEAVE mainBlock:
   
   ON GO OF FRAME frmCompile DO:
      RUN doCompile.
   END.
  
   RUN enable_UI.
   
   IF NOT THIS-PROCEDURE:PERSISTENT
   THEN WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI winCompile  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(winCompile)
  THEN DELETE WIDGET winCompile.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doCompile winCompile 
PROCEDURE doCompile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE         llCompile      AS    LOGICAL        NO-UNDO.
   DEFINE VARIABLE         lhProcedure    AS    HANDLE         NO-UNDO.
   
   DO WITH FRAME frmCompile:
   ASSIGN lcSrcCodeFormat
          liRcodeDestFormat
          lcCompileList 
          lcDestinationDirectory
          lcTopLevelList
          lcNoCompileList
          lcProgOnlyList 
          lcOraOnlyList
          lcCompileDatabases
          lcCompilePropath.
   END.
   
   MESSAGE REPLACE(getSessionParameters(),",",CHR(10)) 
           VIEW-AS ALERT-BOX BUTTONS YES-NO TITLE "Start Compile"
           UPDATE llCompile.
   
   IF llCompile NE TRUE
   THEN RETURN "".
   
   IF connectDBS() NE TRUE
   THEN DO:
      HIDE MESSAGE NO-PAUSE.
      MESSAGE "ERROR: An error occurred when connecting databases.".
      RETURN "".
   END.
   backupPropath().
   
   RUN batchCompile.p NO-ERROR.
   
   restorePropath().
   disconnectDBS().
   
   /* Clean-up After QAD... again */
   deletePersistent(getPersistent("qdtaccess.p")).
   deletePersistent(getPersistent("qdtConfig.p")).
   deletePersistent(getPersistent("batchLogger.p")).
   deletePersistent(getPersistent("convert.p")).
   
   RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI winCompile  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY lblSrcCodeFormat lcSrcCodeFormat liRcodeDestFormat lcCompileList 
          lcDestinationDirectory lcTopLevelList lcNoCompileList lcProgOnlyList 
          lcOraOnlyList lcCompileDatabases lcCompilePropath lblRcodeDestFormat 
          lblCompileList lblDestinationDirectory lblTopLevelList 
          lblNoCompileList lblProgOnlyList lblOraOnlyList lblDatabases 
          lblCompilePropath 
      WITH FRAME frmCompile IN WINDOW winCompile.
  ENABLE lcSrcCodeFormat liRcodeDestFormat btnCompileList 
         btnDestinationDirectory btnTopLevelList btnNoCompileList 
         btnProgOnlyList btnOraOnlyList lcCompileDatabases lcCompilePropath 
      WITH FRAME frmCompile IN WINDOW winCompile.
  {&OPEN-BROWSERS-IN-QUERY-frmCompile}
  VIEW winCompile.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION backupPropath winCompile 
FUNCTION backupPropath RETURNS LOGICAL
   ():
   
   ASSIGN vcPropath = PROPATH.
   
   RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION connectDBS winCompile 
FUNCTION connectDBS RETURNS LOGICAL
   () :
   
   IF lcCompileDatabases EQ "" OR
      lcCompileDatabases EQ ?
   THEN RETURN TRUE.

   CONNECT VALUE(lcCompileDatabases) NO-ERROR.
   IF ERROR-STATUS:ERROR
   THEN DO:
      disconnectDBS().
      RETURN FALSE.
   END.
   
   RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION disconnectDBS winCompile 
FUNCTION disconnectDBS RETURNS LOGICAL
   () :
   
   DEFINE VARIABLE         liCounter      AS    INTEGER        NO-UNDO.

   DO liCounter = NUM-DBS TO 1 BY -1:
      DISCONNECT VALUE(LDBNAME(liCounter)) NO-ERROR.
   END.
   
   RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSessionParameters winCompile 
FUNCTION getSessionParameters RETURNS CHARACTER
   ():
   
   DEFINE VARIABLE         lcSessionParameters  AS    CHARACTER      NO-UNDO.
   
   ASSIGN lcSessionParameters = SUBSTITUTE("srcCodeFormat=&1"        , lcSrcCodeFormat)
                                + "," +
                                SUBSTITUTE("rcodeDestFormat=&1"      , liRcodeDestFormat)
                                + "," +
                                SUBSTITUTE("compileListFilename=&1"  , lcCompileList)
                                + "," +
                                SUBSTITUTE("destinationDirectory=&1" , lcDestinationDirectory)
                                + "," +
                                SUBSTITUTE("topLevelList=&1"         , lcTopLevelList)
                                + "," +
                                SUBSTITUTE("noCompileList=&1"        , lcNoCompileList)
                                + "," +
                                SUBSTITUTE("progOnlyList=&1"         , lcProgOnlyList)
                                + "," +
                                SUBSTITUTE("oraOnlyList=&1"          , lcOraOnlyList)
                                + "," +
                                SUBSTITUTE("compilePropath=&1"       , REPLACE(REPLACE(lcCompilePropath,",",":"),CHR(10),":"))
          .
   
   RETURN lcSessionParameters.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION restorePropath winCompile 
FUNCTION restorePropath RETURNS LOGICAL
   ():
   
   ASSIGN PROPATH = vcPropath.
   
   RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

