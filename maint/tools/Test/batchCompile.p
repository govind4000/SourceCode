/* batchCompile.p - Compiler sub program in batch mode                        */
/* Copyright 1986-2010 QAD Inc., Carpinteria, CA, USA.                        */
/* All rights reserved worldwide.  This is an unpublished work.               */
/* $Id::                                                                   :$ */
/*                                                                            */
/* Runs the batch compile as described below.                                 */
/*                                                                            */

/*------------------------------------------------------------------------

  File:        batchCompile.p

  Description: Compiler sub program - called by rcblcompile.p
  
  This program runs as the startup procedure of a batch session.  It is
  designed to run independent of the client session that started it for 
  the express purpose of being able to use a different code page for
  compiling.
  
  As such, it has some specific requirements.  The calling program will 
  have to create a batchCompile.pf file that contains all necessary 
  startup parameters.  These will include -b to specify a batch session, 
  connection parameters for all databases, -ininame to identify an ini 
  file which will specify the compile propath, other session-specific 
  parameters (e.g. -b -c, -s, -D, -TM, -TB, -B -yy -d), -cprcodeout utf-8, 
  -param as described in the next paragraph and a -p parameter that 
  specifies this program.

  The parameters described below are implicit.  They are actually populated
  via the -param startup parameter to the batch session.
  Input Parameters:
          piRcodeDestFormat      - destination directory
                                   1 - staggered QAD ERP default
                                   2 - Flat destination
                                   3 - Same as source
          pcCompileListFilename  - compile list file
          pcDestinationDirectory - destination directory name          
          pcTopLevelName         - Name of list file with top level programs
          pcNoCompileName        - Name of list file with programs that
                                   should not be compiled.
          pcProgOnlyName         - Name of list file with programs that
                                   should only be compiled if database is PROGRESS.
          pcOraOnlyName          - Name of list file with programs that
                                   should only be compiled if database is Oracle.
          pcSrcCodeFormat        - Name of the source code format. Valid values are
                                   twoletter, flat, and none.

  Output Parameters:
          <none>

  Note:  List file names if populated must have already been validated.  Really!
 ------------------------------------------------------------------------*/
/* REVISION: EE   LAST MODIFIED: 15 Sep 2015  BY: HP/RDU                    */

/* ***************************  Definitions  ************************** */

/* "Parameter" Definitions ---                                         */
define variable piRcodeDestFormat       as integer       no-undo.
define variable pcCompileListFilename   as character     no-undo.
define variable pcDestinationDirectory  as character     no-undo.
define variable pcCompilePropath        as character     no-undo.
define variable pcTopLevelName          as character     no-undo.
define variable pcNoCompileName         as character     no-undo.
define variable pcProgOnlyName          as character     no-undo.
define variable pcOraOnlyName           as character     no-undo.
define variable pcSrcCodeFormat         as character     no-undo.

/*********START ADDITION*****************************************************/
DEFINE VARIABLE         lcSessionParameters  AS    CHARACTER      NO-UNDO.
/*********END OF ADDITION****************************************************/

define stream st-complst.
define stream lastcomp.
/* e4gl-gen requires the shared stream webstream */
define new shared stream webstream.

/* Local Variable Definitions ---                                       */

{mfgini.i}
{retvals.i}
{mfguhelp.i}

{oserrors.i}
{convert.i}
{qdtEnv.i}        /* QDT constants & env variables */

define variable cNoCompileList      as character                no-undo.
define variable cProgressOnlyList   as character                no-undo.
define variable cOracleOnlyList     as character                no-undo.
define variable iProcessStatus      as integer                  no-undo.
define variable h-workflow          as handle                   no-undo.
define variable gcLanguageCode      as character                no-undo.
define variable gcSeparator         as character                no-undo
   initial &IF "{&OPSYS}" = "UNIX" &THEN "/"
           &ELSE "~\" &ENDIF.
define variable iWarningCount       as integer                  no-undo.
define variable iErrorCount         as integer                  no-undo.
define variable hConfig             as handle                   no-undo.
define variable hConvert            as handle                   no-undo.
define variable cDBType             as character                no-undo.

define variable gcTopLevelPrograms  as character                no-undo.
define variable gcCompileLanguages  as character                no-undo.
define variable tBatchCancelled     as logical                  no-undo.

define temp-table ttCompileError no-undo
   field num as integer
   field msg as character
   index num num.
         
/* ********************  Preprocessor Definitions  ******************** */
&scoped-define DEST_FORMAT_STAGGERED 1
&scoped-define DEST_FORMAT_FLAT      2
&scoped-define DEST_FORMAT_SOURCEDIR 3

/* ********************** Forward Declarations *********************** */
function loadPersistent returns logical           (input pcProgName as character,
                                                   output phProgHandle as handle) 
                                                  forward.
                                                  
function sessionParameters returns logical        () 
                                                  forward.

function verifyFile returns logical               (input pcFilename as character) 
                                                  forward.

function getDBType returns character              () 
                                                  forward.

function compilableFileCount returns integer      (input pcCompileList as character) 
                                                  forward.
   
function dotR returns character                   (input pcSourceFile as character) 
                                                  forward.
   
function delay returns logical                    () 
                                                  forward.

function dirOfFile returns character              (input pcFilename as character) 
                                                  forward.
   
function validTargetDirectory returns logical     (input  piRcodeDist   as integer,
                                                   input  pcDestDirPath as character,
                                                   input  pcSourceFile  as character,
                                                   input  pcSrcCodeFormat as character,
                                                   output pcFullDirName as character ) 
                                                  forward.

function preprocessSourceFile returns character   (input pcFilename as character) 
                                                  forward.

function postProcessSourceFile returns logical    (input pcSourceFile as character,
                                                   input pcCompileFile as character) 
                                                  forward.

function languageCodes         returns character  ()
                                                  forward.
/* *********************** Procedure Settings ************************ */

on close of this-procedure do:

   if this-procedure:persistent then delete procedure this-procedure.

end.

/****************************** Functions  *****************************/
function loadPersistent returns logical
   (input pcProgName as character,
    output phProgHandle as handle) :
/* ---------------------------------------------------------------------- */
/* Loads a named procedure persistently and returns the handle in the     */
/* output parameter                                                       */
/*                                                                        */
/*  Input parameters:                                                     */
/*       pcProgName   - name of program to load                           */
/*                                                                        */
/*  Output parameters:                                                    */
/*       phProgHandle - handle of successfully loaded program             */
/*                                                                        */
/*       Return type: logical - yes means success                         */
/*                              it won't return if unsuccessful           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
   run value(pcProgName) persistent set phProgHandle no-error.

   if not valid-handle(phProgHandle) then do:
      message
         substitute("Unable to find QDT file &1.",
                    pcProgName
                   ) skip
         "Please contact QAD Support for Assistance."
      view-as alert-box error.
      quit.
   end.

   return yes.
end function.

function sessionParameters returns logical () :
/*----------------------------------------------------------------------------
 Purpose: to get the session parameters
    NOTE: This function is looking for name-value pairs in the parameter 
          attribute of the session handle.
          It will populate global variables that are named like parameters.
------------------------------------------------------------------------------*/
   define variable i        as integer   no-undo.
   define variable cQADHome as character no-undo.
 
   do i = 1 to num-entries(lcSessionParameters):
      case entry(1, entry(i, lcSessionParameters), "="):
         when "rcodeDestFormat" then do:
            piRcodeDestFormat      = integer(entry(2, entry(i, lcSessionParameters), "=")).
         end.
         when "compileListFilename" then do:
            pcCompileListFilename  = entry(2, entry(i, lcSessionParameters), "=").
         end.
         when "destinationDirectory" then do:
            pcDestinationDirectory = entry(2, entry(i, lcSessionParameters), "=").
         end.
         when "topLevelList" then do:
            pcTopLevelName = entry(2, entry(i, lcSessionParameters), "=").
            if pcTopLevelName = "none" then pcTopLevelName = "".
         end.
         when "noCompileList" then do:
            pcNoCompileName = entry(2, entry(i, lcSessionParameters), "=").
            if pcNoCompileName = "none" then pcNoCompileName = "".
         end.
         when "progOnlyList" then do:
            pcProgOnlyName = entry(2, entry(i, lcSessionParameters), "=").
            if pcProgOnlyName = "none" then pcProgOnlyName = "".
         end.
         when "oraOnlyList" then do:
            pcOraOnlyName = entry(2, entry(i, lcSessionParameters), "=").
            if pcOraOnlyName = "none" then pcOraOnlyName = "".
         end.
         when "srcCodeFormat" then do:
             pcSrcCodeFormat = entry(2, entry(i, lcSessionParameters), "=").
         end.         
         when "compilePropath" then do:
            pcCompilePropath = replace(entry(2, entry(i, lcSessionParameters), "="),':',',').
            propath = pcCompilePropath + "," + propath.
         end.
      end case.
   end.

   if pcSrcCodeFormat = "twoletter" then do:
       run getQDTItem("base","{&ITEM-INSTALLDIR}", output cQADHome).
       propath = cQADHome + "{&DIRSEP}xrc{&DIRSEP}validation," + propath.
   end.   
   return yes.
end function.

function verifyFile returns logical 
   (input pcFilename as character) :
/*----------------------------------------------------------------------------
 Purpose: Function to make sure the file can be found before proceeding.
    NOTE: Strips any path information from pcFileName to check
          the lists.  Then looks for the full filename in the propath.
          If the file is not excluded by the lists and is present on the
          propath, returns true.
------------------------------------------------------------------------------*/
   define variable cJustFile as character no-undo.

   cJustFile = getFileName(pcFileName).
   if can-do(cNoCompileList, cJustFile) then return no.
   if cDBType = "ORACLE" then do:
      if can-do(cProgressOnlyList, cJustFile) then return no.
   end.
   else do:
      if can-do(cOracleOnlyList, cJustFile) then return no.
   end.
   return absPath(pcFilename) <> ? and (
          index(pcFilename, ".t") <> 0 or
          index(pcFilename, ".p") <> 0 or
          index(pcFilename, ".w") <> 0 or
	  /* pyv */
          index(pcFilename, ".html") <> 0 or
	  /* pyv */
          index(pcFilename,".cls") <> 0
          ).

end function.

function getDBType returns character () :
/*----------------------------------------------------------------------------
 Purpose: to determine the type (Progress or Oracle) of connected database(s). 
    NOTE: if any database is ORACLE, the type is set to ORACLE.
------------------------------------------------------------------------------*/   
   cDBType = "PROGRESS".
/*********START DELETION******************************************************
   for each _db:                         
      if _db._db-type = "ORACLE" then do:
         cDBType = "ORACLE".             
      end.                               
   end.                                  
**********END OF DELETION****************************************************/

   return cDBType.
end function.

function compilableFileCount returns integer 
   (input pcCompileList as character) :
/*----------------------------------------------------------------------------
 Purpose: to count valid, compilable files in the filelist
    NOTE: 
------------------------------------------------------------------------------*/
   define variable cCompFile    as character  no-undo.
   define variable iFileCount   as integer    no-undo.
   
   input stream st-complst from value(pcCompileList) no-echo.
    
   /* Log message */
   publish "LogLog"
      (input "Counting number of files to compile.").
                 
   iFileCount = 0.

   repeat:
/*********START ADDITION*****************************************************/
      ASSIGN cCompFile = "".
/*********END OF ADDITION****************************************************/
      import stream st-complst cCompFile.
      if verifyFile(cCompFile) then do:
         iFileCount = iFileCount + 1.
      end.
   end.
  
   /* Log message */
   publish "LogLog"
      (input substitute("Starting compile of &1 file(s).",
                        iFileCount
                       )
      ).
                     
   input close.   
   return iFileCount.
end function.

function dotR returns character 
   (input pcSourceFile as character) :
/*----------------------------------------------------------------------------
 Purpose: utility function to return the name of the .r file from a given source
    NOTE: 
------------------------------------------------------------------------------*/
   define variable iPeriodPosition as integer    no-undo.
   
   /* Make the name of the r-code file from the file name */
   /* by replacing the extension with ".r". */
   iPeriodPosition = r-index(pcSourceFile,".").

   return substring(pcSourceFile, 1, iPeriodPosition - 1) + ".r".
   
end function.

procedure batchCancelled:
/*----------------------------------------------------------------------------
 Purpose: utility to give other processes a chance to do something 
 			 (like maybe cancel)
    NOTE: 
------------------------------------------------------------------------------*/
   define output parameter pBatchCancelled as logical no-undo. 

   process events.   /* For silent cancel button - gives best interrupt */

   /* Check for cancel signal (U3) */
   /* Note: Five "process events" followed by wait-for .001 seconds */
   /*       Gives the best interrupt processing in the TTY mode     */
   
   process events.
   process events.
   process events.
   process events.   
   process events.
  
   pause .001.
   
   pBatchCancelled = absPath('{&QDT-CANCELFILE}') <> ?. 

end procedure.

function dirOfFile returns character
   (input pcFilename as character) :
/*----------------------------------------------------------------------------
 Purpose: utility to get the full directory name from a simple file name
 
 Parameters: pcFilename - simple file name
 
    NOTE: pcFilename must already be qualified as "valid" (i.e. in propath)
------------------------------------------------------------------------------*/
   define variable i as integer   no-undo.
   define variable cFullPath as character  no-undo.
   
   cFullPath = absPath(pcFilename).

   i = if r-index(cFullPath, "~\") <> 0 then r-index(cFullPath, "~\")
       else r-index(cFullPath, "/").
       
   return substring(cFullPath, 1, i - 1).
   
end function.

function validTargetDirectory returns logical
   (input  piRcodeDist   as integer,
    input  pcDestDirPath as character,
    input  pcSourceFile  as character,
    input  pcSrcCodeFormat as character,
    output pcFullDirName as character ) :
/*----------------------------------------------------------------------------
 Purpose: utility to get the target directory for the current file
 
 Parameters: piRcodeDist   - the type of rcode layout
           	 pcDestDirPath - the base path for target
             pcSourceFile  - the file being compiled
             pcSrcCodeFormat - the structure of the source code
             pcFullDirName - (output) the actual target directory for this file
 
    NOTE: returns no if there's a problem making the target directory, else yes
------------------------------------------------------------------------------*/
   define variable iOSErrorStatus      as integer    no-undo.
 
   /* If "MFGPRO" r-code distribution, ... */
   if piRcodeDist = {&DEST_FORMAT_STAGGERED} then do:
      if pcSrcCodeFormat = "flat" then do:
         /* If current file is NOT a top level pgm, or         */
         /* an mf* file, or a trigger (*.t), file, Create      */
         /* the two-letter subdirectory under the lang code    */
         /* as required.                                       */

         if lookup(dotR(pcSourceFile), gcTopLevelPrograms) = 0 and
            index(pcSourceFile, ".t")                      = 0 and
            substring(pcSourceFile, 1, 2)                  <> "mf" then do:

            /* Create the current file's target r-code directory by     */
            /* applying the language code directory layer (as required) */
            /* and then applying the first two letters of the file name */

            pcFullDirName = pcDestDirPath  + gcSeparator +
                            gcLanguageCode + gcSeparator +
                            substring(pcSourceFile, 1, 2).

            if absPath(pcFullDirName) = ? then do:
               if not createAllDirs(pcFullDirName) then do:
                  publish "LogLog"
                     (input substitute("Error creating two-letter directory: &1.",
                                       pcFullDirName
                                      )
                      ).
                   return no.
               end.
            end.
         end. /* Not a top level pgm or trigger. */
         else do: /* Must be a top-level, an "mf" program or a trigger */
            /* Applhelp.p, or other top level pgms */
            if lookup(dotR(pcSourceFile), gcTopLevelPrograms) <>  0 then
               pcFullDirName = pcDestDirPath.
               /* Not a top level pgm, trigger that goes in triggers
                  or mf that goes at the language level */
            else if index(pcSourceFile, ".t") <> 0 then
               pcFullDirName = pcDestDirPath + gcSeparator + "triggers".
            else if substring(pcSourceFile, 1, 2) = "mf" then
               pcFullDirName = pcDestDirPath + gcSeparator + gcLanguageCode.
         end.
      end. /* if pcSrcCodeFormat = "flat" */
      else do:  /* Source format is "twoletter" */
         /* For the two letter source format we need to append the relative
            path information from the compile list entry to the destination
            directory except for .cls files where the compile statement
            figures out the destination path for us.  */
         if substring(pcSourceFile,length(pcSourceFile) - 3) = ".cls" then do:
             pcFullDirName = pcDestDirPath.
         end.
         else do:
             pcFullDirName = getDirName(pcDestDirPath + gcSeparator + pcSourceFile).
             if absPath(pcFullDirName) = ? then do:
                if not createAllDirs(pcFullDirName) then do:
                   publish "LogLog"
                      (input substitute("Error creating directory: &1.",
                                        pcFullDirName
                                       )
                       ).
                    return no.
                end.
             end.
         end.
      end.
   end.
   else do: /* Flat Destination */
      pcFullDirName = if piRcodeDestFormat = {&DEST_FORMAT_FLAT} then pcDestDirPath
                      else dirOfFile(pcSourceFile).
   end.
   return yes.
   
end function.

function preprocessSourceFile returns character
   (input pcFilename as character) :
/*----------------------------------------------------------------------------
 Purpose: utility to handle unusual source
 
 Parameters: pcFilename - full-pathname of file being compiled
 
    NOTE: pcFilename must already be qualified as "valid" (i.e. in propath)
------------------------------------------------------------------------------*/
   define variable vOptions            as character  no-undo initial "".
   define variable cDotW               as character  no-undo.

   /* If file is html, generate .w from it and compile the .w. */
   if pcFilename matches "*.html" then do:
      assign
         vOptions            = ""
         cDotW               = replace(pcFilename, ".html", ".w")
         file-info:file-name = cDotW
      .
     
      run tty/webutil/e4gl-gen
         (input pcFilename,
          input-output cDotW,
          input-output vOptions) no-error.

      assign
         cDotW               = entry(1, cDotW).
         file-info:file-name = cDotW
      .
     
      if file-info:full-pathname = ? then do:
         publish "LogLog"
            (input "Error generating .w from .html file!").
      end.
      
      return cDotW.
   end.
   else
      return pcFilename.
end function.

function postProcessSourceFile returns logical 
   (input pcSourceFile as character,
    input pcCompileFile as character) :
/*----------------------------------------------------------------------------
 Purpose: Function to clean up
 
 Parameters: pcSourceFile - full-pathname of originally specified file
             pcCompileFile - full-pathname of file actually compiled
 
    NOTE: if not the same, we must have generated the compile file, so
          we have to delete it.
------------------------------------------------------------------------------*/
   if pcCompileFile <> pcSourceFile then do:
      os-delete value(pcCompileFile).
   end.
   return yes.
end function.

function languageCodes      returns character    
   () :
/*----------------------------------------------------------------------------
 Purpose: utility to get all available language codes using existing code
 
 Parameters: <none>
 
    NOTE: 
------------------------------------------------------------------------------*/
   define variable cmfglangcodes    as character no-undo.
   define variable cworkField       as character no-undo.
   

   file-info:file-name = "langxref.ini".

   /* Inform the user if we cannot locate the langxref.ini file. */
   if file-info:full-pathname = ? then do:
      return "". 
   end. 

   if file-info:file-type <> ? then do:
      if index(file-info:file-type,"F") <> 0 then do:
         input from value (file-info:full-pathname) no-echo.
         repeat:
            import unformatted cworkField.
            /* Bypass any blanks lines . */ 
            if length(cworkField) = 0 then next. 

            /* Ignore comments in ini file */
            if substring(cworkField, 1, 1) <> "#" then do:
               if cmfglangcodes = "" then
                  cmfglangcodes = entry(1,cworkField).
               else 
                  cmfglangcodes = cmfglangcodes + "," + entry(1,cworkField).
            end.
         end. /* Repeat */
         input close.
      end.
   end.
   
   return cmfglangcodes.
end function.

/****************************** Main Block *****************************/
main-block:
do on error   undo main-block, leave main-block
   on end-key undo main-block, leave main-block:

   /* Set up persistent handlers */
   loadPersistent("qdtaccess.p", hConfig).
   session:add-super-procedure(hConfig).
   run batchLogger.p persistent (input '/opt/shared/tmp/rotzooi.log').
   loadPersistent("convert.p", hConvert).
   session:add-super-procedure(hConvert).

/*********START ADDITION*****************************************************/
   IF SOURCE-PROCEDURE:GET-SIGNATURE("getSessionParameters") EQ "FUNCTION,CHARACTER,"
   THEN DO:
      ASSIGN lcSessionParameters = DYNAMIC-FUNCTION("getSessionParameters" IN SOURCE-PROCEDURE).
   END.
   ELSE ASSIGN lcSessionParameters = SESSION:PARAMETER.
/*********END OF ADDITION****************************************************/
   sessionParameters().
   run showProPath
      (input propath,
       input "compile").

   run loadlist (input pcTopLevelName,
                output gcTopLevelPrograms).
   run loadlist (input pcNoCompileName,
                output cNoCompileList).
   run loadlist (input pcProgOnlyName,
                output cProgressOnlyList).
   run loadlist (input pcOraOnlyName,
                output cOracleOnlyList).
   if gcTopLevelPrograms <> "" then do:
       publish "logDisp"
          (input substitute("Top level list= &1",gcTopLevelPrograms)).
   end.
   if cNoCompileList <> "" then do:
       publish "logDisp"
          (input substitute("No compile list = &1",cNoCompileList)).
   end.
   if cOracleOnlyList <> "" then do:
       publish "logDisp"
          (input substitute("Oracle only list= &1",cOracleOnlyList)).
   end.
   if cProgressOnlyList <> "" then do:
       publish "logDisp"
          (input substitute("Progress only list = &1",cProgressOnlyList)).
   end.

   /* Set up frequently used global values */
   /* Hard-code the language directory: all r-code now goes to "us" */
   assign
      gcLanguageCode     = "us"
      gcCompileLanguages = languageCodes().
   .
   

   publish "LogLog"
      (input substitute("Database type is &1.",
                        getDBType()
                       )
      ).

   /* Make sure that the translation database is connected */
   /* --> Need something here!!! */
   /* Get absolute path of compile list file */
   pcCompileListFilename = absPath(pcCompileListFilename).

   if pcCompileListFilename = ? then do:
      publish "LogLog"
         (input substitute("Compile List File &1 is not found in Propath",
                           file-info:file-name)).
      leave main-block.
   end.
   
   run createDestDir (
      input piRcodeDestFormat
      ).
   
   run createTriggerDir (
      input piRcodeDestFormat
      ).

   /* Display blank line in log file/monitor */
   publish "LogLog"
      (input " ").

   /* Loop through the comp-list-file and create a directory for the current    */
   /* file, as required, (using the first 2-letters of the filename)            */
   /* and compile the file (applhelp.p, mf*.p files and *.t's are special case) */

   run compileFiles (
      input compilableFileCount(pcCompileListFilename),
      input piRcodeDestFormat,
      input pcCompileListFilename,
      input pcSrcCodeFormat,
      output iErrorCount,
      output iWarningCount
      ). 
   
   /* Display Number of errors */   
   publish "LogLog"
      (input substitute("Compile contained &1 errors.",
                        iErrorCount
                       )
      ).
                   
   /* Display Number of warnings */   
   publish "LogLog"
      (input substitute("Compile contained &1 warnings.",
                        iWarningCount
                       )
      ).

   if iErrorCount <> 0 then do:
      output to batchCompile.e.
      put unformatted "***** Error".
      output close.               
   end.

end. /* Main block */

PROCEDURE compileFiles :
/*------------------------------------------------------------------------------
  Purpose:    Going through each entry of compile list file
              and compile the programs  
              
  Parameters:  piFileCount   - number of total files to compile
               piRcodeDist   - where to put R-code
               pcCompileList - name of filelist file
               pcSrcCodeFormat - the source code format
  					
               piError       - number of errors encountered
               piWarning     - number of warnings encountered
  Notes:       
------------------------------------------------------------------------------*/
   define input  parameter piFileCount     as integer    no-undo.
   define input  parameter piRcodeDist     as integer    no-undo.
   define input  parameter pcCompileList   as character  no-undo.
   define input  parameter pcSrcCodeFormat as character  no-undo.
   define output parameter piError         as integer    no-undo.
   define output parameter piWarning       as integer    no-undo.
   
   define variable i                   as integer    no-undo.
   define variable cCurrProg           as character  no-undo.
   define variable cCurrProgpath       as character  no-undo.
   define variable cFullDirName        as character  no-undo.
   define variable cDestDirPath        as character  no-undo.
   define variable iNumFiles           as integer    no-undo.
   define variable cCompFile           as character  no-undo.
   
   input stream st-complst from value(pcCompileList) no-echo.
   
   fileInList:
   repeat:
      
      run batchCancelled (output tBatchCancelled). 
      if tBatchCancelled then
         leave.
/*********START ADDITION*****************************************************/
      ASSIGN cCurrProg = "".
/*********END OF ADDITION****************************************************/
      /* Get next name in list */
      import stream st-complst cCurrProg.
      
      if verifyFile(cCurrProg) then do:
         assign
            iNumFiles = iNumFiles + 1
            /* Get the absolute path of the current program. */
            cCurrProgpath = absPath(cCurrProg)
            /* Get the absolute path of destination directory. */
            cDestDirPath  = absPath(pcDestinationDirectory)
         .

         /* Get the target directory for the current program file */
         if not validTargetDirectory(piRcodeDist,
                                     cDestDirPath,
                                     cCurrProg,
                                     pcSrcCodeFormat,
                                     output cFullDirName) then do:
            piError = piError + 1.
            next fileInList.
         end.

         publish "LogLog"
            (input substitute("Compiling &1 to &2",
                              cCurrProgpath,
                              cFullDirName
                             )
            ).

         /* This function returns the name of the file to compile.
            If the extension is not .html the returned name is the
            same as the passed name.  If the extension is .html, this
            function runs e4gl-gen.p which generates a compilable
            PROGRESS 4GL program from the html.  The extension is
            changed to .w and that's the name returned for compiling. */
         cCompFile = preprocessSourceFile(cCurrProgpath).
           
         compile value(cCompFile) no-attr-space
            languages (value(gcCompileLanguages))
            save into value(cFullDirName) no-error.

         if compiler:error then do:
            run reportCompilerMessages (
               input "error",
               input cCompFile
               ).
            piError = piError + 1.
         end.

         if compiler:warning then do:
            run reportCompilerMessages (
                input "warning",
                input cCompFile
               ).
            piWarning = piWarning + 1.
         end.
         
         /* This function deletes the generated .w file from
            above (preprocessSourceFiile) if one was generated. */
         postProcessSourceFile(cCurrProgpath, cCompFile).
         
         /* Write out file containing the most recent file name compiled */
         output stream lastcomp to "lastcomp.mfu".
         put stream lastcomp unformatted cCurrProg skip.
         output stream lastcomp close.

      end. /* if verifyFile */

   end.   /* do while loop */
        
   /* Display/log message if compile terminated */
   run batchCancelled (output tBatchCancelled). 
   if tBatchCancelled then do:
      publish "LogLog"
         (input "Compile Terminated.").
      if valid-handle(h-workflow) then do:
         run set-workflow-result in h-workflow (input "Cancelled").
      end.

   end.  /* if plVerboseCancel = TRUE... */
    
        
end PROCEDURE.

PROCEDURE createTriggerDir :
/*------------------------------------------------------------------------------
  Purpose:    Create directories to store triggers r code  
  Parameters:  piDestFormat - staggered, flat or source dir
  Notes:       
------------------------------------------------------------------------------*/
   define input  parameter piDestFormat as integer    no-undo.
   
   define variable iOSErrorStatus      as integer   no-undo.
   
    /* If r-code destination is "Staggered MFGPRO default",     */
   if piDestFormat = {&DEST_FORMAT_STAGGERED} then do:

      /* Create triggers directory */
      if absPath(pcDestinationDirectory + gcSeparator + "triggers") = ? then do:
         /* Log message */
         publish "LogLog"
            (input substitute("Creating triggers directory: &1",
                              file-info:file-name
                             )
            ).

         if not createAllDirs(pcDestinationDirectory + gcSeparator + "triggers") then do: 
            publish "LogLog"
               (input substitute("Error creating triggers subdirectory: &1",
                                 file-info:full-pathname
                                )
               ).
         end.
      end.
   end.

end procedure.

PROCEDURE createDestDir :
/*------------------------------------------------------------------------------
  Purpose:    Create directory to store r code  
  Parameters:  piDestFormat - staggered, flat or source dir
  Notes:       
------------------------------------------------------------------------------*/
   define input  parameter piDestFormat as integer    no-undo.
   
   define variable iOSErrorStatus      as integer   no-undo.
   
    /* If r-code destination is "Same as source", then nothign to create */
   if piDestFormat = {&DEST_FORMAT_SOURCEDIR} then
      return.

   /* Create destination directory */
   if absPath(pcDestinationDirectory) = ? then do:
      /* Log message */
      publish "LogLog"
         (input substitute("Creating destination directory: &1",
                           file-info:file-name
                          )
         ).

      if not createAllDirs(pcDestinationDirectory) then do: 
         publish "LogLog"
            (input substitute("Error creating destination directory: &1",
                              pcDestinationDirectory
                             )
            ).
      end.
   end.

   /* If r-code destination is "Staggered MFGPRO default",     */
   if piDestFormat = {&DEST_FORMAT_STAGGERED} then do:
      /* Create destination subdirectory */
      if absPath(pcDestinationDirectory + gcSeparator + "us") = ? then do:
         /* Log message */
         publish "LogLog"
            (input substitute("Creating destination subdirectory: &1",
                              file-info:file-name
                             )
            ).

         if not createAllDirs(pcDestinationDirectory + gcSeparator + "us") then do: 
            publish "LogLog"
               (input substitute("Error creating destination subdirectory: &1",
                                 pcDestinationDirectory + gcSeparator + "us"
                                )
               ).
         end.
      end.
   end.

end procedure.

PROCEDURE reportCompilerMessages :
/* -----------------------------------------------------------
  Purpose:     Logs and displays messages from the compiler
  Parameters:  pcMessageType - "Error" or "Warning"
               pcProcName    - file being compiled
  Notes:       
-------------------------------------------------------------*/
   define input  parameter pcMessageType as character  no-undo.
   define input  parameter pcProcName    as character  no-undo.
   
   define variable i as integer    no-undo.
   
   empty temp-table ttCompileError.
   do i = 1 to error-status:num-messages:
      create ttCompileError.
      assign
          ttCompileError.num = i
          ttCompileError.msg = "    " + error-status:get-message(i).
   end.
   
   publish "LogLog"
      (input substitute("Compilation &1 occurred in: &2",
                        pcMessageType,
                        pcProcName
                       )
      ).
   
   if pcMessageType = "error" then do:
      publish "LogLog"
         (input substitute("   Error occurred in: &1 at line &2 Column &3",
                           pcProcName,
                           compiler:error-row,
                           compiler:error-column
                          )
         ).
   end.
   
   for each ttCompileError:
      publish "LogLog"
         (input ttCompileError.msg).
   end.
end PROCEDURE.

PROCEDURE loadlist private:
/*------------------------------------------------------------------------------
  Purpose:     Load a list file into a variable.
  Parameters:  pcFilename - the file name of the list.
               pcList     - variable that holds the list.
  Notes:  The passed file name must have already been validated.  Name can be
          a full pathname or a relative pathname on the propath but it must
          have already been validated.  If the name is blank the routine will
          return an empty list.
------------------------------------------------------------------------------*/
   define input  parameter pcFilename as character no-undo.
   define output parameter pcList     as character no-undo.

   define variable cLine         as character no-undo.
   define variable cPattern      as character no-undo initial "**/".

   pcList = "".
    /* Read the file and build the comma separated list. */
   if pcFilename <> "" and
      absPath(pcFilename) <> ? then do:
      input from value(file-info:full-pathname).
   
      repeat:
         import unformatted cLine.
         if cLine begins cPattern then do:
             cLine = substring(cLine, 4).
         end.
         pcList = if pcList = "" then cLine
                    else pcList + "," + cLine.
      end. /* repeat */

      input close.
   end.
   
end procedure.


