/* convert.p - Persistent procedure library for conversions.              */
/* Copyright 2003-2011 QAD Inc., Santa Barbara, CA, USA.                  */
/* All rights reserved worldwide.  This is an unpublished work.           */
/*                                                                        */
/* $Id:: convert.p 7890 2012-08-23 17:09:42Z nus                        $ */
/*                                                                        */
/*  Input Parameters                                                      */
/*      <None>                                                            */
/*                                                                        */
/*  Output Parameters:                                                    */
/*      <None>                                                            */
/*                                                                        */ 
/*                                                                        */
/*                                                                        */
/*--------------------------- REVISION HISTORY ---------------------------*/
/* $Revision: 1.54 $   MODIFIED: 08/17/07  BY: Brian Smith     *R04B*     */
/*  Revision: 1.39     MODIFIED: 05/15/07  BY: Brian Smith     *R04B*     */
/*  Revision: 1.34     MODIFIED: 05/09/07  BY: Brian Smith     *R04B*     */
/*  Revision: 1.32     MODIFIED: 04/13/07  BY: Brian Smith     *R04B*     */
/*  Revision: 1.29     MODIFIED: 03/05/07  BY: Shane O'Riordan *R04B*     */
/*  Revision: 1.25     MODIFIED: 01/12/07  BY: Allan Doane     *R04B*     */
/*  Revision: 1.19     MODIFIED: 12/05/06  BY: Brian Smith     *R04B*     */
/*  Revision: 1.16     MODIFIED: 10/24/06  BY: Markus Barone   *R04B*     */
/*  Revision: 1.15     MODIFIED: 07/10/06  BY: Allan Doane     *R097*     */
/*  Revision: 1.13     MODIFIED: 01/06/06  BY: Brian Smith                */
/*  Revision: 1.10     MODIFIED: 06/24/05  BY: Brian Smith                */
/*  Revision: 1.8      MODIFIED: 06/24/05  BY: Brian Smith                */
/*  Revision: 1.1.4.9.2.41    MODIFIED: 06/20/05  BY: Brian Smith         */
/*  Revision: 1.1.4.9.2.40    MODIFIED: 04/13/05  BY: Brian Smith         */
/* Revision: 1.1.4.9.2.35     MODIFIED: 12/17/04  BY: Brian Smith         */
/* Revision: 1.1.4.9.2.30     MODIFIED: 10/26/04  BY: Brian Smith         */
/* Revision: 1.1.4.9.2.26     MODIFIED: 10/11/04  BY: Brian Smith         */
/* Revision: 1.1.4.9.2.25     MODIFIED: 09/15/04  BY: Brian Smith         */
/* Revision: 1.1.4.9.2.21     MODIFIED: 08/13/04  BY: Brian Smith         */
/* Revision: 1.1.4.9.2.18     MODIFIED: 08/12/04  BY: Brian Smith         */
/* Revision: 1.1.4.9.2.17     MODIFIED: 08/10/04  BY: Brian Smith *Q07Z*  */
/* Revision: 1.1.4.9.2.14     MODIFIED: 06/29/04  BY: Brian Smith *Q07Z*  */
/* Revision: 1.1.4.9.2.10     MODIFIED: 05/25/04  BY: Brian Smith         */
/* Revision: 1.1.4.9.2.6      MODIFIED: 06/25/03  BY: Brian Smith         */
/* Revision: 1.1.4.1           CREATED: 02/26/03  BY: Brian Smith         */
/*------------------------------------------------------------------------*/
/* REVISION: EE   LAST MODIFIED: 15 Sep 2015  BY: HP/RDU                    */

/* ************************ Input Parameters **************************** */

/* ************************ Local Variables ***************************** */

define variable cSourceDB        as character no-undo.
define variable cTargetDB        as character no-undo.
define variable cPhysTargetDB    as character no-undo.
define variable cAdminSourceDB   as character no-undo.
define variable cAdminTargetDB   as character no-undo.
define variable cDomainValue     as character no-undo initial "QAD".
define variable lInPlace         as logical   no-undo.
define variable cConvType        as character no-undo.
define variable cSourceDBVersion as character no-undo.
define variable cTargetDBVersion as character no-undo.
define variable cConvDBName      as character no-undo initial "qaddb".
define variable gcConvContainer  as character no-undo.

define variable cSourceTable     as character no-undo.
define variable cTargetTable     as character no-undo.
define variable cWhereClause     as character no-undo.

define variable hbSource         as handle  no-undo.
define variable hbTarget         as handle  no-undo.
define variable hqSource         as handle  no-undo.
define variable hqTarget         as handle  no-undo.
define variable hfTarget         as handle  no-undo.
define variable vhfTargetDomain  as handle  no-undo.
define variable vhfDummyDomain   as handle  no-undo.
define variable transcnt         as integer no-undo.
define variable idx              as integer no-undo.
define variable iStartTime       as integer no-undo.
define variable iStoptime        as integer no-undo.
define variable cTargetPrefix    as character no-undo.
define variable lGotCancel       as logical no-undo initial false.
define variable gl-sublen        as integer no-undo initial 0.
define variable iStackDepth      as integer no-undo.
define variable cReasonType      as character no-undo.
define variable cLastSectionProcessed as character no-undo.
define variable cCB              as character no-undo initial ?.
define variable hCB              as handle no-undo.
define variable ldoOIDGen        as logical no-undo initial no.
define variable cSeqDBName       as character no-undo.
define variable dRegID           as decimal no-undo initial ?.
define variable OIDSeqName       as character no-undo.

define variable hTempTable       as handle no-undo.

/* Variable used to hold the last result of getinfo.p */
define variable lGetInfoResult   as logical no-undo.

/* Global variable to supress duplicate record messages in copyRecord. */
define variable glSupress132    as logical   no-undo initial false.

define variable lvp_ugain_acct  as character no-undo.
define variable lvp_ugain_sub   as character no-undo.
define variable lvp_ugain_cc    as character no-undo.
define variable lvp_uloss_acct  as character no-undo.
define variable lvp_uloss_sub   as character no-undo.
define variable lvp_uloss_cc    as character no-undo.
define variable lvp_rgain_acct  as character no-undo.
define variable lvp_rgain_sub   as character no-undo.
define variable lvp_rgain_cc    as character no-undo.
define variable lvp_rloss_acct  as character no-undo.
define variable lvp_rloss_sub   as character no-undo.
define variable lvp_rloss_cc    as character no-undo.
define variable lvp_ex_rnd_acct as character no-undo.
define variable lvp_ex_rnd_sub  as character no-undo.
define variable lvp_ex_rnd_cc   as character no-undo.

define variable binary-key      as raw       no-undo.


&SCOPED-DEFINE TRANGROUP 10000


define temp-table tvprogs no-undo
   field convname as character
   field mandatory as logical
   field widgetname as character
   field doconv as logical
   field convui as character
   field tables as character
   index convname is unique primary mandatory convname.

define temp-table dumpprogs no-undo
   field dumpname as character
   field program as character
   field dodump as logical
   field tables as character
   index dumpname is unique primary dumpname.

define temp-table temp-table-list no-undo
   field ttname as character
   field tthandle as handle
   index ttname is unique primary ttname.

define temp-table tt-load-errors no-undo
   field num as integer
   field msg as character
   index num num.

define temp-table persis_var no-undo
   field var_name as character
   field var_val as character
   index var_name var_name.


/* ************************ Main Block ********************************** */

/* Set up local variables needed from ini file.  */
create widget-pool.

subscribe "logCancel" anywhere.
subscribe "logEnable" anywhere.

run initEnvVars.
run initTVProgs.

return.


/* ************************ Local Functions ***************************** */
{qdtEnv.i}
{gpcrpt.i} /* DECRYPT AND ENCRYPT FUNCTIONS 66 lines */
{qdtfunc.i}

function absPath returns character
   (input pcRelPath as character) :
/*----------------------------------------------------------------------------
 Purpose: utility to get the full pathname from a relative or unqualified
          reference.
    NOTE: Because file-info is a "global" object, this function has the
          side-effect of leaving it set, which means that the other attributes
          are available after the function call.
------------------------------------------------------------------------------*/
   
   file-info:file-name = pcRelPath.
   return file-info:full-pathname.
   
end function.

function touch returns logical
   (input pcFilename as character) :
/*----------------------------------------------------------------------------
 Purpose: utility function to "touch" a file (either create it or update the
 			 modified date)
    NOTE: In case the file exists, it opens the file with the append option, 
          so as not to blow away the data.
------------------------------------------------------------------------------*/

   define variable cFullPath as character  no-undo.

   cFullPath = absPath(pcFilename).
   if cFullPath = ? then do:
      cFullPath = absPath(".") +
                  ( if opsys = "unix" then "/" else "~\" ) +
                  pcFilename.
   end.

   /* append preserves file contents */
   output to value(cFullPath) append.
   output close.

   return yes.
end function.


function loadIniFile returns logical
   (input pcIniFileName as character,
    input phGenIni      as handle) :
/*----------------------------------------------------------------------------
 Purpose: Load an ini file and quit if not found.
    NOTE: Precondition: genini.p is already running persistently
------------------------------------------------------------------------------*/
   if absPath(pcIniFileName) = ? then do:
         publish "LogLog"
            (input substitute("***** Unable to locate &1, quitting.", pcIniFileName)).
         message "Unable to continue, cannot locate " + pcIniFileName + "." skip
                 "Please contact QAD Support for assistance."
           view-as alert-box error.
         run runquit.
   end.
   else do:
      if valid-handle(phGenIni) then
         run loadIni in phGenIni
            (input file-info:full-pathname).
   end.
   return yes.
end function.

function makeOrLoadIniFile returns logical
   (input pcIniFileName as character,
    input phGenIni      as handle) :
/*----------------------------------------------------------------------------
 Purpose: make an ini file not found load it otherwise.
    NOTE: Precondition: genini.p is already running persistently
------------------------------------------------------------------------------*/
   if absPath(pcIniFileName) = ? then do:
      run makeIni in phGenIni.
   end.
   else do:
      if valid-handle(phGenIni) then
         run loadIni in phGenIni
            (input file-info:full-pathname).
   end.
   return yes.
end function.

/* ---------------------------------------------------------------------- */
/* Return the result of the last getinfo.p execution.                     */
/*                                                                        */
/*  Input parameters: None                                                */
/*                                                                        */
/*       Return type: logical                                             */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION getGetInfoResult returns logical():

   return lGetInfoResult.

end FUNCTION.

/* ---------------------------------------------------------------------- */
/* Return true if QAD .net UI server is installed.                        */
/*                                                                        */
/*  Input parameters: filename (with full path) for qadui.tpl file.       */
/*                                                                        */
/*       Return type: logical                                             */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION getDotNetFlag returns logical(input cqaduitplFile as character):

   return if absPath(cqaduitplFile) = ? then false /* no qadui.tpl file found */
          else true.

end FUNCTION.

/* ---------------------------------------------------------------------- */
/* Removes any extension from the passed file name.                       */
/*                                                                        */
/*  Input parameters: pcPathName - The path to parse.                     */
/*                                                                        */
/*       Return type: character                                           */
/*  Note: Assumes only one ".".                                           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION removeExt returns character(input pcPathName as character):

   return entry(1, pcPathName, ".").

end FUNCTION.

/* ---------------------------------------------------------------------- */
/* Gets the file name from a full path descriptor.                        */
/*                                                                        */
/*  Input parameters: pcPathName - The path to parse.                     */
/*                                                                        */
/*       Return type: character                                           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION getFileName returns character(input pcPathName as character):

   define variable iSlashLoc as integer   no-undo.
   define variable cFileName as character no-undo.

   if absPath(pcPathName) <> ? then do:
      /* We found it.  If it's a directory, return nothing. */
      if index(file-info:file-type, "D") <> 0 then
         return "".
   end.
   /* It wasn't a directory. Just parse it. */
   iSlashLoc = r-index(pcPathName, "~\").

   if iSlashLoc = 0 then
      iSlashLoc = r-index(pcPathName, "/").

   /* Move one position past the last slash. */
   cFileName =  substring(pcPathName, iSlashLoc + 1).

   return cFileName.

end FUNCTION.

/* ---------------------------------------------------------------------- */
/* Gets the directory portion from a full path descriptor.                */
/*                                                                        */
/*  Input parameters: pcPathName - The path to parse.                     */
/*                                                                        */
/*       Return type: character                                           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION getDirName returns character(input pcPathName as character):

   define variable iSlashLoc as integer   no-undo.
   define variable cDirName  as character no-undo.

   if absPath(pcPathName) <> ? then do:
      /* We found it.  If it's a directory, return it. */
      if index(file-info:file-type, "D") <> 0 then
         return file-info:full-pathname.
   end.
   /* It wasn't a directory. Just parse it. */
   iSlashLoc = maximum(r-index(pcPathName,"~\"),r-index(pcPathName,"/")).

   /* If there's no slash, it's just a filename.  Return empty string. */
   if iSlashLoc = 0 then
      cDirName = "".
   else
      /* Return everything prior to the last slash. */
      cDirName =  substring(pcPathName, 1, iSlashLoc - 1).

   return cDirName.

end FUNCTION.

/* ---------------------------------------------------------------------- */
/* Checks to see if the passed string is a valid directory.  If not,      */
/* creates it if possible.  Returns true if it all succeeds.              */
/*                                                                        */
/*  Input parameters: pcPathName - The path to create or validate.        */
/*                                                                        */
/*       Return type: logical                                             */
/*                                                                        */
/* ---------------------------------------------------------------------- */
function checkDirectory returns logical (input pcDirName as character,
                                         output pcRetVal as character):

   define variable cRetVal       as character no-undo.
   define variable iCreateResult as integer   no-undo.
   
   file-info:file-name = pcDirName.
   if file-info:full-pathname <> ? then do:
      if index(file-information:file-type,"D") <> 0 and
          index(file-information:file-type,"W") <> 0 then return true.
      else do:
         cRetVal = substitute("***** &1 exists as a file or is not writeable: ",
                   pcDirName).
         return false.
      end.
   end.
   os-create-dir value(pcDirName).
   iCreateResult = os-error.
   if os-error = 0 then do:
      file-info:file-name = pcDirName.
      if file-info:full-pathname <> ? then do:
         if index(file-information:file-type,"D") <> 0 and
             index(file-information:file-type,"W") <> 0 then return true.
      end.
      cRetVal = substitute("***** Could not create writeable directory: &1",
                pcDirName).
      return false.
   end.
   else do:
      cRetVal = substitute("***** Create directory failed.  Error: &1 Directory: &2",
                           iCreateResult,
                           pcDirName).
      return false.
   end.

end function.


function createAllDirs returns logical
   (input pcFullPath as character ):
/*------------------------------------------------------------------------------
   Purpose:     This procedure is used to create the entire set of directories
                specified by the passed path.
                Returns true if everything is OK.  Logs errors.

   Parameters:  <input>
                pcFullPath  - full path to be created.

   Notes:       Path must always be fully qualified including drive on Windows.
------------------------------------------------------------------------------*/
   define variable cDriveLetter as character no-undo.
   define variable lOK          as logical   no-undo.
   define variable cRetVal      as character no-undo.
/*********START ADDITION*****************************************************/
   DEFINE VARIABLE         lcPath            AS    CHARACTER      NO-UNDO.
/*********END OF ADDITION****************************************************/
   
   /* First let's get all the slashes going the same way. */
   if '{&DIRSEP}' = '/' then
      pcFullPath = replace(pcFullPath, '~\', '{&DIRSEP}').
   else
      pcFullPath = replace(pcFullPath, '/', '{&DIRSEP}').

/*********START ADDITION*****************************************************/
   ASSIGN lcPath = pcFullPath.
/*********END OF ADDITION****************************************************/

   /* Save the start of the path. */
   if opsys = "win32" then do:
      cDriveLetter = substring(pcFullPath,1,2).
      pcFullPath = substring(pcFullPath, 4).
   end.
   else do:
      cDriveLetter = ''.
      pcFullPath = substring(pcFullPath, 2).
   end.

   lOK = true.
   do idx = 1 to num-entries(pcFullPath,'{&DIRSEP}') while lOK:
      cDriveLetter = cDriveLetter + '{&DIRSEP}' + entry(idx, pcFullPath,'{&DIRSEP}').
/*********START ADDITION*****************************************************/
      /* NO NEED FOR WRITE PERMISSIONS IN ALL SUBDIRECTORIES FROM ROOT UP */
      ASSIGN FILE-INFO:FILE-NAME = cDriveLetter.
      IF INDEX(FILE-INFO:FILE-TYPE, "D") GT 0
      THEN DO:
         IF INDEX(FILE-INFO:FILE-TYPE, "W") GT 0 OR
            cDriveLetter                    NE lcPath
         THEN NEXT.
      END.
/*********END OF ADDITION****************************************************/
      lOK = checkDirectory(cDriveLetter,
                           output cRetVal).
   end.   
   return lOK.

end function.

function copyFile returns logical
   (input pcSrcFileName as character,
    input pcDstFileName as character ):
/*------------------------------------------------------------------------------
   Purpose:     This procedure is used to copy the specifed file to 
                specified destination.
                If the copy operation failed, a message will be recorded to log
                file and the function returns false.

   Parameters:  <input>
                pcSrcFileName  - full path of the file to be copied
                pcDstFileName  - target path name

   Notes:
------------------------------------------------------------------------------*/
   define variable iErrorNo as integer   no-undo.
   define variable cPath    as character no-undo.
   define variable lOK      as logical   no-undo.

   cPath = getDirName(pcDstFileName).
   lOK = createAllDirs(cPath).
   if not lOK then do:
      publish "logLog"
         (input substitute("***** Error creating &1",cPath)).
      return lOK.
   end.
   os-copy value(pcSrcFileName) value(pcDstFileName).
   assign
      iErrorNo = os-error.

   if iErrorNo <> 0 then do:
      publish "LogLog"
         (input "***** Error occured when copying " +
              pcSrcFileName +
              " to " +
              pcDstFileName +
              ". System Error #" +
              string(iErrorNo)).
      return false.
   end.
   return true.

end function. /* copyFile */


/* ---------------------------------------------------------------------- */
/* Gets the drive letter portion from a full path descriptor.             */
/*                                                                        */
/*  Input parameters: pcPathName - The path to parse.                     */
/*                                                                        */
/*       Return type: character                                           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION getDriveLetter returns character(input pcPathName as character):

   define variable iSlashLoc    as integer   no-undo.
   define variable cDriveLetter as character no-undo.

   /* Assume UNIX */
   cDriveLetter = "".
   if OPSYS <> "UNIX" then do:
      if absPath(pcPathName) <> ? then do:
         /* We found it.  If it's a directory, return it. */
         cDriveLetter = substring(file-info:full-pathname, 1, 2).
      end.
   end.

   return cDriveLetter.

end FUNCTION.


/* ---------------------------------------------------------------------- */
/* Gets the logical name associated with the data in a database.          */
/*                                                                        */
/*  Input parameters: pcDBName - the connection name of the database.     */
/*                                                                        */
/*       Return type: character                                           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION getDBDataName returns character(input pcDBName as character):

   define variable cDataName as character no-undo.
   define variable hbDB      as handle    no-undo.

   assign
      cDataName = pcDBName.
   create buffer hbDB for table pcDBName + "._db" no-error.
   if valid-handle(hbDB) then do:
      hbDB:find-first("where _db-type <> " + quoter("PROGRESS"), no-lock) no-error.
      if hbDB:available then do:
         cDataName = hbDB:buffer-field("_db-name"):buffer-value.
      end.
      delete object hbDB.
   end.
   return cDataName.

end FUNCTION.


/* ---------------------------------------------------------------------- */
/* Gets the name of the target database used in the package.              */
/*                                                                        */
/*  Input parameters: <None>                                              */
/*                                                                        */
/*       Return type: character                                           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION getTrgDB returns character():

   return cTargetDB.

end FUNCTION.


/* ---------------------------------------------------------------------- */
/* Gets the base name of the physical target database                     */
/*                                                                        */
/*  Input parameters: <None>                                              */
/*                                                                        */
/*       Return type: character                                           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION getTargetBaseName returns character():

   return cPhysTargetDB.

end FUNCTION.


/* ---------------------------------------------------------------------- */
/* Sets the base name of the physical target database                     */
/*                                                                        */
/*  Input parameters: <None>                                              */
/*                                                                        */
/*       Return type: character                                           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
PROCEDURE setTargetBaseName:
   define input parameter cTarget as character no-undo.
   
   cPhysTargetDB = cTarget.

end PROCEDURE.


/* ---------------------------------------------------------------------- */
/* Returns the next OID sequence value.                                   */
/*                                                                        */
/*  Input parameters: <none>                                              */
/*                                                                        */
/*       Return type: Decimal                                             */
/*  NOTE: this function requires that cSeqDBName has be set up already    */
/* ---------------------------------------------------------------------- */
FUNCTION getNextOIDSeq returns integer ():

define variable cDBName as character no-undo.
define variable iRes    as integer   no-undo.

   return dynamic-next-value("qaddb_oid_sq01", cSeqDBName).

end FUNCTION.

/* ----------------------------------------------------------------------- */
/* Returns the current setting of the conversion Container variable        */
/* or looks it up in the xml temp table if it is not yet set.              */
/*                                                                         */
/*    input parameter: <none>                                              */
/*                                                                         */
/* ----------------------------------------------------------------------- */
function getConvContainer returns character ():
   define variable cContainerList as character no-undo.
    
   if gcConvContainer <> "" then do:
      return gcConvContainer.
   end.
   else do:
       run getContainerListByType
          ( input "{&TYPE-CONVERSION}",
            input getCurrentEnv(),
           output cContainerList).
       if num-entries(cContainerList) = 1 
       then do:          
          assign gcConvContainer = cContainerList.
          return cContainerList.
       end.
       else
          return "".
   end.
end function.


/* ---------------------------------------------------------------------- */
/* Returns the next OID value when qaddb_ctrl is not yet initialized.     */
/*                                                                        */
/*  Input parameters: <none>                                              */
/*                                                                        */
/*       Return type: Decimal                                             */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION getFirstOID returns decimal
   (input dd as decimal):

   define variable datevar as date    no-undo.
   define variable datedec as decimal no-undo.
   define variable nextOid as decimal no-undo.

   if not ldoOIDGen then return 0.0.

   assign
      datevar = today
      datedec = year(datevar) * 10000 + month(datevar) * 100 + day(datevar)
      nextOid = datedec * 10000000000 +
                getNextOIDSeq() + dd.
    return nextOid.

end FUNCTION.

function getRegid returns decimal 
   (input piSeed as integer):
/*------------------------------------------------------------------------
   function - getRegid
   Parameters: Input - piSeed the integer to be converted.

   Purpose: Converts an integer oidSeed value to a decimal.

   Duplicates code in mgdbpm1.i for use in mfgutil.
   NOTE: Duplicated from oidseed.p
--------------------------------------------------------------------------*/
   
   define variable regid as decimal no-undo.
   define variable i as integer no-undo.
   define variable j as integer no-undo.
   define variable k as integer no-undo.
   define variable l as decimal no-undo.

   do on error undo, return error {&GENERAL-APP-EXCEPT}:
      /* SET INITIAL VALUES */
      assign
         k = piSeed
         l = 1
         regid = 0.

      /* REVERSE THE DIGITS ENTERED AND DIVIDE BY 10 */
      /* RAISED TO THE NUMBER OF DIGITS              */
      do i = 1 to 9:
         assign
            j     = k modulo 10
            k     = (k - j) / 10
            regid = regid + ((j / l) / 10)
            l     = l * 10.

      end. /* DO I = 1 TO 9 */
   end. /* DO ON ERROR */

   return regid.
end function.

/* ---------------------------------------------------------------------- */
/* Returns the next OID value.                                            */
/*                                                                        */
/*  Input parameters: <none>                                              */
/*                                                                        */
/*       Return type: Decimal                                             */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION getNextOID returns decimal ():

   define variable datevar as date        no-undo.
   define variable datedec as decimal     no-undo.
   define variable nextOid as decimal     no-undo.
   define variable cSeed   as character   no-undo.

   define variable hbQADDB as handle      no-undo.


   if not ldoOIDGen then return 0.0.

   if dRegID = ? then do:
      create buffer hbQADDB for table "qaddb_ctrl" no-error.
      if valid-handle(hbQADDB) then do:
         hbQADDB:find-first() no-error.
         if hbQADDB:available then
            dRegID = hbQADDB:buffer-field("qaddb_oid_regid"):buffer-value.
         else do:
            /* Not there, so emulate as if it were. */
            run getQDTItem (getConvContainer(), '{&ITEM-OIDCODE}', output cSeed).
            dRegID = getRegID(integer(cSeed)).
         end.
         delete object hbQADDB.
      end.
      else do:
         publish "LogLog"
            (input "***** Unable to create buffer for qaddb_ctrl for OID creation.").
         return 0.0.
      end.
   end.
   assign
      datevar = today
      datedec = year(datevar) * 10000 + month(datevar) * 100 + day(datevar)
      nextOid = datedec * 10000000000 +
                getNextOIDSeq() + dRegID .
    return nextOid.

end FUNCTION.

/* ---------------------------------------------------------------------- */
/* Returns logical true if the named conversion is selected.              */
/*                                                                        */
/*  Input parameters:                                                     */
/*    pcConv - the name of the conversion (with [])                       */
/*                                                                        */
/* Output parameters:                                                     */
/*    <None>                                                              */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION IsConvSelected returns logical
   (input pcConvName as character):

   run initTVProgs.
   find first tvprogs where convname = pcConvName and doconv no-error.
   if available tvprogs then return true. else return false.

end FUNCTION.

/* ---------------------------------------------------------------------- */
/* Look up the passed domain, return true if it exists.                   */
/*                                                                        */
/*  Input parameters:                                                     */
/*    pcDomain - the domain to validate.                                  */
/*                                                                        */
/* Output parameters:                                                     */
/*    <None>                                                              */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION IsValidDomain returns logical
   (input pcDomain as character):

   define variable hDomainBuffer as handle  no-undo.
   define variable lFoundDomain  as logical no-undo.

   create buffer hDomainBuffer for table "dom_mstr" no-error.
   if valid-handle(hDomainBuffer) then do:

      /* Find the dom_mstr that matches the passed domain code. */
      hDomainBuffer:find-first("where dom_domain = " + quoter(pcDomain), no-lock) no-error.
      lFoundDomain = hDomainBuffer:available.
      delete object hDomainBuffer.

   end.

   return lFoundDomain.

end FUNCTION.

/* ---------------------------------------------------------------------- */
/* Return the dump name for the passed table and db.                      */
/*                                                                        */
/*       Return type: Character                                           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION getDumpName returns character
         (input pcTableName as character,
          input pcDBName as character):

   define variable hTable    as handle    no-undo.
   define variable cFullName as character no-undo.
   define variable cResult   as character no-undo.

   cResult = "".
   cFullName = sdbname(pcDBName) + "._file".
   create buffer hTable for table cFullName no-error.
   if not valid-handle(hTable) then do:
      publish "LogLog"
         (input substitute("***** Could not create buffer for &1!",
                           cFullName)).
   end.
   else do:
      hTable:find-first("where _file-name = " + quoter(pcTableName), no-lock) no-error.
      if hTable:available then
         cResult = hTable:buffer-field("_dump-name"):buffer-value.

      delete object hTable.
   end.

   return cResult.

end FUNCTION.

/* ---------------------------------------------------------------------- */
/* Remove an item from a list.                                            */
/*                                                                        */
/*       Return type: Character                                           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION removeItem returns character
         (input ii as integer,
          input list as character,
          input delim as character):

   define variable idx as integer no-undo.

   /* Remove the item from the list. */
   entry(ii, list) = "".
   /* And remove the extra delimiter. */
   idx = index(list, delim + delim).
   if idx <> 0 then
       substr(list, idx, 1) = ''.
   /* If item was the only item, this might happen. */
   if list = delim then
      list = "".
   return list.

end FUNCTION.

/* ---------------------------------------------------------------------- */
/* Returns reason type.                                                   */
/*                                                                        */
/*       Return type: Character                                           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION getRsnType returns character ():

   return cReasonType.

end FUNCTION.

/* ---------------------------------------------------------------------- */
/* Returns the next field from a standard progress data file line.        */
/* Also trims the field from the input line.                              */
/*                                                                        */
/*  Input parameter: pcDelimiter - The field separator character.         */
/*  Input-output parameter: pcInput - The input line being parsed.        */
/*                                                                        */
/*       Return type: Character                                           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION getNextField returns character
         (input pcDelimiter as character,
          input-output pcInput as character):

   define variable cField as character no-undo.
   define variable idx    as integer   no-undo.

   /* Indicate no more data if input is null. */
   if pcInput = "" then return ?.

   if pcInput begins '"' then do:
      idx = index(pcInput, '"', 2).
      do while substring(pcInput, idx + 1, 1) = '"'and idx <> 0:
         idx = index(pcInput, '"', idx + 2).
      end.
      if idx = 0 then do:
         publish "LogDisp"
            (input "***** Parsing error during load...").
         publish "LogDisp"
            (input "***** Remaining input line is:").
         publish "LogDisp"
            (input pcInput).
         pcInput = "".
         return error.
      end.
      cField = substring(pcInput, 2, idx - 2).
      pcInput = substring(pcInput, length(cField) + 4, -1).
   end.
   else do:
     cField = entry(1, pcInput, pcDelimiter).
     pcInput = substring(pcInput, length(cField) + 2, -1).
   end.

   /* Trim any extra white space. */
   left-trim(pcInput).

   return cField.

end FUNCTION.


/* ---------------------------------------------------------------------- */
/* Returns the value of cConvType.                                        */
/*                                                                        */
/*  Input parameters: <None>                                              */
/*                                                                        */
/*       Return type: character                                           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION getConvType returns character():

   return cConvType.

end FUNCTION.



/* ---------------------------------------------------------------------- */
/* Gets the name of the source database used in the package.              */
/*                                                                        */
/*  Input parameters: <None>                                              */
/*                                                                        */
/*       Return type: character                                           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION getSrcDB returns character():

   return cSourceDB.

end FUNCTION.


/* ---------------------------------------------------------------------- */
/* Returns table name for the fully qualified dump name passed.           */
/*                                                                        */
/*  Input parameters: pcDumpName - table name.                            */
/*                    pcLDBName  - logical database name.                 */
/*                                                                        */
/*       Return type: Character                                           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION getTableName returns character
         (input pcDumpName as character,
          input pcLDBName as character):

   define variable hbTable     as handle    no-undo.
   define variable hqTable     as handle    no-undo.
   define variable hfTableName as handle    no-undo.
   define variable cTableName  as character no-undo.

   cTableName = "".
   create buffer hbTable for table pcLDBName + "._file".
   hfTableName = hbTable:buffer-field("_file-name").

   create query hqTable.
   hqTable:set-buffers(hbTable).
   hqTable:query-prepare("for each _file no-lock where _dump-name = " + quoter(pcDumpName)).
   hqTable:query-open().
   hqTable:get-first().
   if hbTable:available then do:
      cTableName = hfTableName:buffer-value.
   end.
   hqTable:query-close().

   delete object hqTable.
   delete object hbTable.

   return cTableName.

end FUNCTION.

/* ---------------------------------------------------------------------- */
/* Returns the last section processed.                                    */
/*                                                                        */
/*  Input parameters: <None>.                                             */
/*                                                                        */
/*       Return type: Character                                           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION getTTSection returns character ():
   return cLastSectionProcessed.
end FUNCTION.

/* ---------------------------------------------------------------------- */
/* Returns the conversion database logical name.                          */
/*                                                                        */
/*  Input parameters: <None>.                                             */
/*                                                                        */
/*       Return type: character                                           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION getConvDB returns character ():
   return cConvDBName.
end FUNCTION.

/* ---------------------------------------------------------------------- */
/* Returns the value of iStackDepth.                                      */
/*                                                                        */
/*  Input parameters: <None>.                                             */
/*                                                                        */
/*       Return type: Integer                                             */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION getStackDepth returns integer ():
   return iStackDepth.
end FUNCTION.

/* ---------------------------------------------------------------------- */
/* Returns the value of gl-sublen.                                        */
/*                                                                        */
/*  Input parameters: <None>.                                             */
/*                                                                        */
/*       Return type: Integer                                             */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION getGLSubLen returns integer ():
   return gl-sublen.
end FUNCTION.

/* ---------------------------------------------------------------------- */
/* Returns account portion of pre-eas account field.                      */
/*                                                                        */
/*  Input parameters: p-concat-field - the pre-eas account field.         */
/*                                                                        */
/*       Return type: Character                                           */
/* Note: depends on gl-sublen being set.                                  */
/* ---------------------------------------------------------------------- */
FUNCTION parse_account returns character
   (input p-concat-field as character):
   return right-trim (substring(p-concat-field, 1,(8 - gl-sublen))).
end FUNCTION.

/* ---------------------------------------------------------------------- */
/* Returns subaccount portion of pre-eas account field.                   */
/*                                                                        */
/*  Input parameters: p-concat-field - the pre-eas account field.         */
/*                                                                        */
/*       Return type: Character                                           */
/* Note: depends on gl-sublen being set.                                  */
/* ---------------------------------------------------------------------- */
FUNCTION parse_subacct returns character
   (input p-concat-field as character):
   return
      right-trim (substring(p-concat-field, (9 - gl-sublen),gl-sublen)).
end FUNCTION.

/* ---------------------------------------------------------------------- */
/* Returns value from envvars.ini.                                        */
/*                                                                        */
/*  Input parameters: pcSection - Name of the section of the file.        */
/*                    pcAttribute - Name of the attribute in the section. */
/*                                                                        */
/*       Return type: Character                                           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION readAttrValue returns character(input pcSection as character,
                                         input pcAttribute as character):

   define variable hEnvVars as handle no-undo.
   define variable pcRetVal as character no-undo initial "".
 
   /* To avoid changing all of the calls to readAttrValue in the conversion programs
      this kludge changes them into look-ups in the conversion container.  */  
   if pcSection = "[environment]" then do:
      run getQDTItem(gcConvContainer, pcAttribute,   output pcRetVal).
   end.
   else do:

       /* Load the ini file into the temp table. */
       create temp-table hEnvVars.
       run getEnvVars
          (input "envvars.ini",
           input hEnvVars).
       /* Get the one being asked for. */
       run getOneIniVar
          (input hEnvVars:default-buffer-handle,
           input pcSection,
           input pcAttribute,
           output pcRetVal).
    
       /* Clean up. */
       delete object hEnvVars.
   end.
   
   return pcRetVal.

end FUNCTION.

/* ---------------------------------------------------------------------- */
/* Reads the domain value from the envvars.ini file.                      */
/*                                                                        */
/*  Input parameters: <None>                                              */
/*                                                                        */
/*       Return type: character                                           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION readDomainValue returns character ():

   define variable cDomain as character no-undo.

   run getQDTItem(gcConvContainer, "{&ITEM-CONVDOMAIN}",   output cDomain).
   return cDomain.

end FUNCTION.


/* ---------------------------------------------------------------------- */
/* Returns logical true if the table exists.                              */
/*                                                                        */
/*  Input parameters: pcTableName - Name of table to check.               */
/*                                                                        */
/*       Return type: Logical                                             */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION tableExists returns logical(input pcTableName as character):
   define variable retVal  as logical no-undo initial false.
   define variable hTable as handle no-undo.
/*   define variable cFullName as character no-undo.*/

   /* If the named database is not connected return false. */
   if not connected(entry(1,pcTableName,".")) then return (false).

   create buffer hTable for table pcTableName no-error.
   retval = valid-handle(hTable).
   delete object hTable no-error.

/***   
   /* Try to find the named table in the meta-schema. */
   cFullName = entry(1,pcTableName,".") + "._file".
   create buffer hTable for table cFullName no-error.
   if not valid-handle(hTable) then do:
      publish "LogLog"
         (input substitute("***** Could not create buffer for &1._file!",
                           entry(1,pcTableName,"."))).
   end.
   else do:
      hTable:find-first("where _file-name = " + quoter(entry(2,pcTableName,".")), no-lock) no-error.
      if hTable:available then
         retVal = true.
      delete object hTable.
   end.
***/
   return (retVal).

end FUNCTION.


/* ---------------------------------------------------------------------- */
/* Returns db logical name if the sequence exists.                        */
/*                                                                        */
/*  Input parameters: pcSequenceName - Name of sequence to check.         */
/*                                                                        */
/*       Return type: Character                                           */
/*                                                                        */
/*  Note:  The sequence name can be qualified with a database name.  If   */
/*         it is not then all of the connected databases will be checked. */
/* ---------------------------------------------------------------------- */
FUNCTION sequenceExists returns character(input pcSequenceName as character):
   define variable retVal  as logical no-undo initial false.
   define variable hTable as handle no-undo.
   define variable cFullName as character no-undo.
   define variable cDBName as character no-undo.
   define variable i as integer no-undo.
   define variable cRetDBName as character no-undo.

   retVal = no.
   cRetDBName = "".
   case num-entries(pcSequenceName, "."):
      when 1 then do:
         cDBName = ?.
      end.
      when 2 then do:
         cDBName = entry(1,pcSequenceName, ".").
         pcSequenceName = entry(2,pcSequenceName,".").
      end.
      otherwise
         return "".
   end case.

   if cDBName = ? then do:
      do i = 1 TO num-dbs while not retVal:

         /* Try to find the named sequence in the meta-schema. */
         cFullName = sdbname(i) + "._sequence".
         create buffer hTable for table cFullName no-error.
         if valid-handle(hTable) then do:
            hTable:find-first("where _seq-name = " + quoter(pcSequenceName), no-lock) no-error.
            if hTable:available then do:
               retVal = true.
               cRetDBName = sdbname(i).
            end.
            delete object hTable.
         end.
      end.
   end.
   else do:
      /* Try to find the named sequence in the meta-schema. */
      cFullName = sdbname(cDBName) + "._sequence".
      create buffer hTable for table cFullName no-error.
      if not valid-handle(hTable) then do:
         publish "LogLog"
            (input substitute("***** Could not create buffer for &1._sequence!",
                              sdbname(cDBName))).
         cRetDBName = "".
      end.
      else do:
         hTable:find-first("where _seq-name = " + quoter(pcSequenceName), no-lock) no-error.
         if hTable:available then do:
            retVal = true.
            cRetDBName = cDBName.
         end.
         delete object hTable.
      end.
   end.

   return cRetDBName.

end FUNCTION.


/* ---------------------------------------------------------------------- */
/* Returns logical true if the named database is connected.               */
/*                                                                        */
/*  Input parameters: pcDBName - logical database name.                   */
/*                                                                        */
/*       Return type: Logical                                             */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION isDBconnected returns logical(input cDBName as character):
   define variable i      as integer no-undo.
   define variable retVal as logical no-undo initial false.

   retval = no.
   if (ldbname(1) <> ?) then do:
      do i = 1 TO num-dbs while not retVal:
         retVal = (ldbname(i) = cDBName).
      end.
   end.

   return retVal.

end FUNCTION.

/* ---------------------------------------------------------------------- */
/* Returns logical true if the source database is connected.              */
/*                                                                        */
/*  Input parameters: <None>                                              */
/*                                                                        */
/*       Return type: Logical                                             */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION isSourceConnected returns logical():
/********
   define variable i      as integer no-undo.
   define variable retVal as logical no-undo initial false.

   retval = no.
   /* If we don't know the name, it's not connected. */
   if cSourceDB <> "" then
      if (ldbname(1) <> ?) then do:
         do i = 1 TO num-dbs while not retVal:
            retVal = (ldbname(i) = cSourceDB).
         end.
      end.

   return retVal.
*********/   
   return if cSourceDB = "" then no
          else isDBConnected(cSourceDB).

end FUNCTION.

/* ---------------------------------------------------------------------- */
/* Returns logical true if the target database is connected.              */
/*                                                                        */
/*  Input parameters: <None>                                              */
/*                                                                        */
/*       Return type: Logical                                             */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION isTargetConnected returns logical():
/*******
   define variable i      as integer no-undo.
   define variable retVal as logical no-undo initial false.

   retval = no.
   /* If we don't know the name, it's not connected. */
   if cTargetDB <> "" then
      if (ldbname(1) <> ?) then do:
         do i = 1 TO num-dbs while not retVal:
            retVal = (ldbname(i) = cTargetDB).
         end.
      end.

   return retVal.
*******/
   return if cTargetDB = "" then no
          else isDBConnected(cTargetDB).
end FUNCTION.

/* ---------------------------------------------------------------------- */
/* Returns logical true if the reference database is connected.           */
/*                                                                        */
/*  Input parameters: <None>                                              */
/*                                                                        */
/*       Return type: Logical                                             */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION isRefConnected returns logical():
define variable i as integer no-undo.

/******
define variable retVal as logical no-undo initial false.
if (ldbname(1) <> ?) then do:
   do i = 1 TO num-dbs while not retVal:
      retVal = (ldbname(i) = "reference").
   end.
end.

return retVal.
******/
   return isDBConnected("reference").
   
end FUNCTION.


/* ---------------------------------------------------------------------- */
/* Returns the version string of the connected source database.           */
/*                                                                        */
/*  Input parameters: <None>                                              */
/*                                                                        */
/*       Return type: Character                                           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION getSourceDBVersion returns character():

   run getQDTItem(getConvContainer(), "{&ITEM-SOURCEDBVER}",  output cSourceDBVersion).
   return cSourceDBVersion.

end FUNCTION.

/* ---------------------------------------------------------------------- */
/* Returns the version string of the connected target database.           */
/*                                                                        */
/*  Input parameters: <None>                                              */
/*                                                                        */
/*       Return type: Character                                           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION getTargetDBVersion returns character():

/******
if isTargetConnected() then do:
   return cTargetDBVersion.
end.
else do:
   return "".
end.
******/
   return if isTargetConnected() then cTargetDBVersion
          else "".

end FUNCTION.

/* ---------------------------------------------------------------------- */
/* Returns the buffer handle associated with the passed temp table name.  */
/*                                                                        */
/*  Input parameters: pcName - Name of the temp table.                    */
/*                                                                        */
/*       Return type: Character                                           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION getTTHandle returns handle( input pcTTName as character ):

define variable hTTHandle as handle no-undo.

find temp-table-list where ttname = pcTTName no-error.
if available temp-table-list then do:
   hTTHandle = tthandle.
end.

return hTTHandle.

end FUNCTION.

/* ----------------------------------------------------------------------- */
/* Returns a character value from a "persistent" variable                  */
/*                                                                         */
/*    input parameter: persistent variable name to get                     */
/*                                                                         */
/*    returns: value of persistent variable as character string            */
/*                                                                         */
/* ----------------------------------------------------------------------- */
FUNCTION get_persis_var returns character (input p_var_name as character):

   define variable v_var_val as character initial "" no-undo.

   for first persis_var
   where var_name = p_var_name:
      v_var_val = var_val.
   end.
   return v_var_val.

end FUNCTION. /* getPerVar */

/* ----------------------------------------------------------------------- */
/* Returns a logical value from a "persistent" variable                    */
/*                                                                         */
/*    input parameter: persistent variable name to get                     */
/*                                                                         */
/*    returns: value of persistent variable as logical type                */
/*    NOTE - only character string "yes" returns true!                     */
/*                                                                         */
/* ----------------------------------------------------------------------- */
FUNCTION get_persis_logical returns logical (input p_var_name as character):

   define variable v_var_val as character initial "" no-undo.

   for first persis_var
   where var_name = p_var_name:
      v_var_val = var_val.
   end.

   return (v_var_val = "yes").
   
end FUNCTION. /* getPerVar */

/* --------------------------------------------------------------------- */
/* Returns encrypted password for safe storage in plain text file        */
/*                                                                       */
/*    input parameters:                                                  */
/*           pcPlainText - plain text password                           */
/* --------------------------------------------------------------------- */
function encryptPassword returns character (input pcPlainText as character):
   define variable rawField as raw no-undo.
   define variable cEncrypted as character no-undo.

   /* put the string to raw field w/o terminating null */
   put-string(rawField, 1, length(pcPlainText, 'raw')) = pcPlainText.
   cEncrypted = base64-encode(rawField).
   return cEncrypted.
end function. /* encryptPassword */

/* --------------------------------------------------------------------- */
/* Returns decrypted password                                            */
/*                                                                       */
/*    input parameters:                                                  */
/*           pcEncrypted - encrypted password                            */
/* --------------------------------------------------------------------- */
function decryptPassword returns character (input pcEncrypted as character) : 
   define variable rawField as raw no-undo.

   if pcEncrypted = ? or pcEncrypted = '' then return ''.

   rawField = base64-decode(pcEncrypted).
   return get-string(rawField, 1).
end function. /* decryptPassword */

/* -------------------------------------------------------------------------- */
/* Returns true if a field exists for a table in a database                   */
/*                                                                            */
/* Parameter:                                                                 */
/*    input - table name                                                      */
/*    input - field name                                                      */
/*    input - database name                                                   */
/* -------------------------------------------------------------------------- */
FUNCTION doesFieldExist returns logical (input pcTableName as character,
                                         input pcFieldName as character,
                                         input pcDBName as character):

   define variable cQueryString  as character   no-undo.
   define variable hbFile        as handle      no-undo.
   define variable hbField       as handle      no-undo.
   define variable hqQuery       as handle      no-undo.
   define variable lReturnVal    as logical     no-undo.

   cQueryString = substitute("for each _File no-lock where _File-Name = &1,~
                              first _Field no-lock~
                              where _File-recid = recid(_File)~
                                and _Field-Name = &2",
                             quoter(pcTableName),
                             quoter(pcFieldName)).

   if pcDBName = "" then do: 
      create buffer hbFile  for table "_File".
      create buffer hbField for table "_Field".
   end.
   else do:
      create buffer hbFile  for table sdbname(pcDBName) + "._File".
      create buffer hbField for table sdbname(pcDBName) + "._Field".       
   end.

   /* Check if the field exists in the database for the table */
   create query hqQuery.
   hqQuery:set-buffers(hbFile, hbField).
   hqQuery:query-prepare(cQueryString).
   hqQuery:query-open().
   hqQuery:get-first().

   if hbField:available then 
      lReturnVal = true.
   else
      lReturnVal = false.

   /* Cleanup */
   hqQuery:query-close().
   delete object hqQuery.
   delete object hbFile.
   delete object hbField.

   return lReturnVal.

END FUNCTION. /* doesFieldExist */

/* -------------------------------------------------------------------------- */
/* Returns true if an index exists for a table in a database                  */
/*                                                                            */
/* Parameter:                                                                 */
/*    input - table name                                                      */
/*    input - index name                                                      */
/*    input - database name                                                   */
/* -------------------------------------------------------------------------- */
FUNCTION doesIndexExist returns logical (input pcTableName as character,
                                         input pcIndexName as character,
                                         input pcDBName as character):

   define variable cQueryString  as character   no-undo.
   define variable hbFile        as handle      no-undo.
   define variable hbIndex       as handle      no-undo.
   define variable hqQuery       as handle      no-undo.
   define variable lReturnVal    as logical     no-undo.

   cQueryString = substitute("for each _File no-lock where _File-Name = &1,~
                              first _Index no-lock~
                              where _File-recid = recid(_File)~
                                and _Index-Name = &2",
                             quoter(pcTableName),
                             quoter(pcIndexName)).

   if pcDBName = "" then do:
      create buffer hbFile  for table "_File".
      create buffer hbIndex for table "_Index".
   end.
   else do:
      create buffer hbFile  for table sdbname(pcDBName) + "._File".
      create buffer hbIndex for table sdbname(pcDBName) + "._Index".
   end.
   
   /* Check if the index exists in the database for the table */
   create query hqQuery.
   hqQuery:set-buffers(hbFile, hbIndex).
   hqQuery:query-prepare(cQueryString).
   hqQuery:query-open().
   hqQuery:get-first().

   if hbIndex:available then
      lReturnVal = true.
   else
      lReturnVal = false.

   /* Cleanup */
   hqQuery:query-close().
   delete object hqQuery.
   delete object hbFile.
   delete object hbIndex.

   return lReturnVal.

END FUNCTION. /* doesIndexExist */

/* ---------------------------------------------------------------------- */
/* The objective of this function is to return a value to set the         */
/* code_group field whenever the coversion creates a code_mstr record     */
/*                                                                        */
/* Description:                                                           */
/* Returns a value to be assigned to the code_mstr.code_group field.      */
/* The code group values are defined by seed data in the ctg_mstr table   */
/* and can have a value of either "SYSTEM" or "APP"                       */
/*                                                                        */
/* The logic to determine the value returned is as follows:               */
/*   - if a code_mstr record exists for the QAD domain and code_fldname   */
/*     combination, then use that code_group value.                       */
/*   - alternatively if no code_mstr record exists for the QAD domain     */
/*     and code_fldname combination then assign SYSTEM if domain is QAD   */
/*     and assign APP if domain is not QAD                                */
/*                                                                        */
/*  Input parameters:                                                     */
/*          pcDomain - the domain where the code_mstr will be created in. */
/*          pcCodeFldname - corresponds to the code_fldname in code_mstr  */
/*                                                                        */
/*  Output parameter:                                                     */
/*          Returns a character to be used as the code_group field        */
/*                                                                        */
/* ---------------------------------------------------------------------- */
FUNCTION getCodeGroup returns character
        (input pcDomain as character, input pcCodeFldname as character):
   
   define variable cCodeGroup   as character no-undo.
   define variable cQueryString as character no-undo.
   define variable hqCodeMstr   as handle no-undo.
   define variable hbCodeMstr   as handle no-undo.
   
   create buffer hbCodeMstr for table "code_mstr".
   create query hqCodeMstr.
   
   assign 
     cQueryString = "for each code_mstr no-lock " + 
                    "where code_mstr.code_domain = " + quoter("QAD") +
                    "  and code_mstr.code_fldname = " + quoter(pcCodeFldname) + 
                    "  and code_group <> " + quoter (""). 
   
   hqCodeMstr:set-buffers(hbCodeMstr).
   hqCodeMstr:query-prepare(cQueryString).
   hqCodeMstr:query-open().
   hqCodeMstr:get-first().
   
   if hbCodeMstr:available then do:
      assign cCodeGroup = hbCodeMstr:buffer-field("code_group"):buffer-value.
   end. 
   
   hqCodeMstr:query-close().
   delete object hqCodeMstr.
   delete object hbCodeMstr.

   if cCodeGroup <> "" then do:
       return cCodeGroup.
   end.
   else do:
      if pcDomain = "QAD" then do:
         return "SYSTEM".
      end.
      else do:
         return "APP".
      end.   
   end.

END FUNCTION. /* getCodeGroup */

/* -------------------------------------------------------------------------- */
/* Returns the _Field._width of a database field for table                    */
/*                                                                            */
/* Parameter:                                                                 */
/*    input - table name                                                      */
/*    input - field name                                                      */
/*    input - database name                                                   */
/* -------------------------------------------------------------------------- */
FUNCTION getFieldWidth returns integer (input pcTableName as character,
                                         input pcFieldName as character,
                                         input pcDBName as character):

   define variable cQueryString  as character   no-undo.
   define variable hbFile        as handle      no-undo.
   define variable hbField       as handle      no-undo.
   define variable hqQuery       as handle      no-undo.
   define variable iReturnVal    as integer     no-undo.

   cQueryString = substitute("for each _File no-lock where _File-Name = &1,~
                              first _Field no-lock~
                              where _File-recid = recid(_File)~
                                and _Field-Name = &2",
                             quoter(pcTableName),
                             quoter(pcFieldName)).

   if pcDBName = "" then do: 
      create buffer hbFile  for table "_File".
      create buffer hbField for table "_Field".
   end.
   else do:
      create buffer hbFile  for table sdbname(pcDBName) + "._File".
      create buffer hbField for table sdbname(pcDBName) + "._Field".       
   end.

   /* Check if the field exists in the database for the table */
   create query hqQuery.
   hqQuery:set-buffers(hbFile, hbField).
   hqQuery:query-prepare(cQueryString).
   hqQuery:query-open().
   hqQuery:get-first().

   if hbField:available then 
      iReturnVal = hbField:BUFFER-FIELD("_width"):BUFFER-VALUE.
   else
      iReturnVal = -1.

   /* Cleanup */
   hqQuery:query-close().
   delete object hqQuery.
   delete object hbFile.
   delete object hbField.

   return iReturnVal.

END FUNCTION. /* getFieldWidth */

/* ************************ Local Procedures **************************** */


PROCEDURE killMe:
/* ---------------------------------------------------------------------- */
/* Procedure to remove the super procedure chain from the procedure       */
/* referenced by the passed handle.                                       */
/*                                                                        */
/*  Input parameters:                                                     */
/*    phProc - the handle of the procedure to operate on.                 */
/*                                                                        */
/* Output parameters:                                                     */
/*    <None>.                                                             */
/*                                                                        */
/* Note: This procedure is recursive.                                     */
/* ---------------------------------------------------------------------- */
   define input parameter phProc as handle no-undo.

   define variable hConvert as handle    no-undo.
   define variable cSupers  as character no-undo.
   define variable idx      as integer   no-undo.

   if valid-handle(phProc) then do:
      cSupers = phProc:super-procedures.
      do idx = 1 to num-entries(cSupers):
         hConvert = widget-handle(entry(idx,cSupers)).
         run killMe
            (input hConvert).
         phProc:remove-super-procedure(hConvert).
      end.
      run destructor in phProc.
      if valid-handle(phProc) then
         delete procedure phProc.
   end.

end PROCEDURE.


PROCEDURE initEnvVars:
/* ---------------------------------------------------------------------- */
/* Procedure to initialize some globals from envvars.ini                  */
/*                                                                        */
/*  Input parameters:                                                     */
/*    <None>.                                                             */
/*                                                                        */
/* Output parameters:                                                     */
/*    <None>.                                                             */
/*                                                                        */
/* ---------------------------------------------------------------------- */
   define variable hEnvVars as handle    no-undo.
   define variable cTmp     as character no-undo.

   run getInPlace(output lInplace).
   run getQDTItem(gcConvContainer, "{&ITEM-CONVTYPE}",     output cConvType).
   run getQDTItem(gcConvContainer, "{&ITEM-SOURCEDB}",     output cSourceDB).
   run getQDTItem(gcConvContainer, "{&ITEM-TARGETDB}",     output cTargetDB).
   run getQDTItem(gcConvContainer, "{&ITEM-PHYSTARGETDB}", output cPhysTargetDB).
   run getQDTItem(gcConvContainer, "{&ITEM-CONVDOMAIN}",   output cDomainValue).
   run getQDTItem(gcConvContainer, "{&ITEM-SOURCEDBVER}",  output cSourceDBVersion).
   run getQDTItem(gcConvContainer, "{&ITEM-TARGETDBVER}",  output cTargetDBVersion).   
   
end PROCEDURE.


PROCEDURE writeAttrValue:
/* ---------------------------------------------------------------------- */
/* Writes value to envvars.ini.                                           */
/*                                                                        */
/*  Input parameters: pcSection - Name of the section of the file.        */
/*                    pcAttribute - Name of the attribute in the section. */
/*                    pcValue - Value to write.                           */
/*                                                                        */
/*                                                                        */
/* ---------------------------------------------------------------------- */
define input parameter pcSection   as character no-undo.
define input parameter pcAttribute as character no-undo.
define input parameter pcValue     as character no-undo.

   define variable hEnvVars as handle no-undo.

   /* Load the ini file into the temp table. */
   create temp-table hEnvVars.
   run getEnvVars
      (input "envvars.ini",
       input hEnvVars).
   /* Update the one specified. */
   run updateEnvVar
         (input hEnvVars:default-buffer-handle,
          input pcSection,
          input pcAttribute,
          input pcValue).
   /* Write out the temp table. */
   run setEnvVars
      (input "envvars.ini",
       input hEnvVars:default-buffer-handle).

   /* Clean up. */
   delete object hEnvVars.

end PROCEDURE.


PROCEDURE initTVProgs:
/* ---------------------------------------------------------------------- */
/* Procedure to initialize tvprogs temp table from selconv.ini.           */
/*                                                                        */
/*  Input parameters:                                                     */
/*    <None>.                                                             */
/*                                                                        */
/* Output parameters:                                                     */
/*    <None>.                                                             */
/*                                                                        */
/* ---------------------------------------------------------------------- */

   empty temp-table tvprogs.
   if absPath("selconv.ini") <> ? then do:
      input from value(file-info:full-pathname) no-echo.
      repeat:
         create tvprogs.
         import tvprogs.
      end.
      input close.
   end.

end PROCEDURE.

PROCEDURE setTTSection:
/* ---------------------------------------------------------------------- */
/* Sets the last section processed.                                       */
/*                                                                        */
/*  Input parameters: pcSection - the name of the section to display.     */
/*                                                                        */
/*  Output parameters: <None>                                             */
/*                                                                        */
/* Note: Used by routines that do not use convui.ini to define screens.   */
/*       Sets the title of dialog that is usually supplied by convui.ini. */
/* ---------------------------------------------------------------------- */
define input parameter pcSection as character no-undo.

   cLastSectionProcessed = pcSection.

end PROCEDURE.

PROCEDURE setRsnType:
/* ---------------------------------------------------------------------- */
/* Procedure to set the cReasonType variable.                             */
/*                                                                        */
/*  Input parameters:                                                     */
/*    pcReasonType - the reason type value.                               */
/*                                                                        */
/* Output parameters:                                                     */
/*    <None>.                                                             */
/*                                                                        */
/* ---------------------------------------------------------------------- */
   define input parameter pcReasonType as character no-undo.

   cReasonType = pcReasonType.

end PROCEDURE.


PROCEDURE setStackDepth:
/* ---------------------------------------------------------------------- */
/* Procedure to set the number of routines in the super procedure chain.  */
/*                                                                        */
/*  Input parameters:                                                     */
/*    piStackDepth - the number of routines.                              */
/*                                                                        */
/* Output parameters:                                                     */
/*    <None>.                                                             */
/*                                                                        */
/* ---------------------------------------------------------------------- */
   define input parameter piStackDepth as integer no-undo.

   iStackDepth = piStackDepth.

end PROCEDURE.

PROCEDURE setGLSubLen:
/* ---------------------------------------------------------------------- */
/* Procedure to set the value of gl-sublen if gl_ctrl is available.       */
/*                                                                        */
/*  Input parameters:                                                     */
/*    <None>.                                                             */
/*                                                                        */
/* Output parameters:                                                     */
/*    <None>.                                                             */
/*                                                                        */
/* ---------------------------------------------------------------------- */

   define variable hbGLCtrl as handle no-undo.
   define variable hfGLSubLen as handle no-undo.
   define variable cTableName as character no-undo.
   define variable cDBName    as character no-undo.
   define variable cDomain    as character no-undo.

   cDBName = getSrcDB().
   run getDomain
      (output cDomain).

   cTableName = cDBName + ".gl_ctrl".
   if not tableExists(cTableName) then do:
      cDBName = getTrgDB().
      cTableName = cDBName + ".gl_ctrl".
      if not tableExists(cTableName) then do:
         cTableName = "".
      end.
   end.
   if cTableName <> "" then do transaction:
      create buffer hbGLCtrl for table getDBDataName(cDBName) + ".gl_ctrl" no-error.
      hfGLSubLen = hbGLCtrl:buffer-field("gl_sub_len").
      hbGLCtrl:find-first("where gl_domain = " + quoter(cDomain), no-lock) no-error.
      if hbGLCtrl:available then do:
         gl-sublen = hfGLSubLen:buffer-value.
      end.
      delete object hbGLCtrl.
   end.
end.


PROCEDURE logCancel:
/* ---------------------------------------------------------------------- */
/* Procedure to handle the logCancel message.                             */
/*                                                                        */
/*  Input parameters:                                                     */
/*    <None>.                                                             */
/*                                                                        */
/* Output parameters:                                                     */
/*    <None>.                                                             */
/*                                                                        */
/* ---------------------------------------------------------------------- */

   lGotCancel = true.

end PROCEDURE.

PROCEDURE logEnable:
/* ---------------------------------------------------------------------- */
/* Procedure to handle the logEnable message.                             */
/*                                                                        */
/*  Input parameters:                                                     */
/*    <None>.                                                             */
/*                                                                        */
/* Output parameters:                                                     */
/*    <None>.                                                             */
/*                                                                        */
/* ---------------------------------------------------------------------- */

   lGotCancel = false.

end PROCEDURE.

PROCEDURE getTempTable:
/* ---------------------------------------------------------------------- */
/* Obtains the handle to an existing temp table by name.                  */
/*                                                                        */
/*  Input parameters:                                                     */
/*    pTempTableName - Name of a temp table.                              */
/*                                                                        */
/* Output parameters:                                                     */
/*    pcTTBuffer - Handle to the temp table.                              */
/*                                                                        */
/* ---------------------------------------------------------------------- */
define input parameter pTempTableName as character no-undo.
define output parameter pcTTHandle as handle no-undo.

/* Get the handle for the passed in TT name */
   find temp-table-list where ttname = pTempTableName no-error.
   if available temp-table-list then do:
      if valid-handle(temp-table-list.tthandle) then
          assign pcTTHandle = temp-table-list.tthandle.
   end.

end PROCEDURE.

PROCEDURE makeTempTable:
/* ---------------------------------------------------------------------- */
/* Create a persistent temp table like the one passed.                    */
/*                                                                        */
/*  Input parameters:                                                     */
/*    phTempTable - Handle to a temp table.                               */
/*                                                                        */
/* Output parameters:                                                     */
/*    pcTTBuffer - Handle to the buffer of the temp table.                */
/*                                                                        */
/* ---------------------------------------------------------------------- */
   define input parameter table-handle phTempTable.
   define output parameter pcTTBuffer as handle no-undo.

   define variable ttBuffHandle as handle no-undo.
   define variable hqTT as handle no-undo.

   define variable ttBuffHandle2 as handle no-undo.

   /* Get the default buffer handle of the temp table. */
   ttBuffHandle = phTempTable:default-buffer-handle.

   /* See if we already know about this table. */
   find temp-table-list where ttname = phTempTable:name no-error.
   if not available temp-table-list then do:
      /* Create the record for this table. */
      create temp-table-list.
      assign ttname = phTempTable:name.
   end.

   /* If there is already a valid temp-table handle, delete the object. */
   if valid-handle(temp-table-list.tthandle) then do:
       ttBuffHandle2 = temp-table-list.tthandle:default-buffer-handle.
       ttBuffHandle2:empty-temp-table().
      delete object temp-table-list.tthandle.
   end.

   /* Create the temp table like the one passed in. */
   create temp-table temp-table-list.tthandle.
   temp-table-list.tthandle:create-like(ttBuffHandle).
   temp-table-list.tthandle:temp-table-prepare(phTempTable:name).

   /* Copy the contents to the new temp table. */
   create query hqTT.
   hqTT:set-buffers(ttBuffHandle).
   hqTT:query-prepare("for each " + ttBuffHandle:name).
   hqTT:query-open().
   hqTT:get-first().
   do while not hqTT:query-off-end:
      temp-table-list.tthandle:default-buffer-handle:buffer-create().
      temp-table-list.tthandle:default-buffer-handle:buffer-copy(ttBuffHandle).
      hqTT:get-next().
   end.
   hqTT:query-close().
   delete object hqTT.

   /* Return the buffer handle of the new temp table. */
   pcTTBuffer = temp-table-list.tthandle:default-buffer-handle.

end PROCEDURE.

procedure createConvContainer :
/* ---------------------------------------------------------------------- */
/* Create the conversion container for the passed container.              */
/*                                                                        */
/*  Input parameters:                                                     */
/*    pcContainerName - name of the container.                            */
/*                                                                        */
/* Output parameters:                                                     */
/*    <None>                                                              */
/*  Note: this very specific routine is the mechanism to hook conversions */
/*        into container processing.  It presupposes that a conversion    */
/*        container occurs at a known place in the hierarchy.             */
/* ---------------------------------------------------------------------- */
   define input  parameter pcContainer as character  no-undo.

   define variable cDBServ        as character no-undo.
   define variable cContainerType as character no-undo.
   
   /* Create conversion container for current container and put name into system global */
   /* Get TYPE-DBSERVER container above current pcContainer. */
   cContainerType = getContainerAttribute(pcContainer, "type").
   lookie_loop:
   do while cContainerType <> "{&TYPE-DBSERVER}" and 
            cContainerType <> "{&TYPE-PRODUCT}":
      cDBServ = getParentContainer
         (input pcContainer).
      cContainerType = getContainerAttribute(cDBServ, "type").
      if cContainerType = "{&TYPE-PRODUCT}" then do:
         /* We went to the top without finding the dbserver so look for it as a child. */
         run getChildByType
            (input cDBServ,
             input "{&TYPE-DBSERVER}",
            output pcContainer).
         if pcContainer = "" then leave lookie_loop.
      end.
      else do:
         pcContainer = cDBServ.
      end.
   end.
   /* When we get here, pcContainer should be blank or the name of
	  the dbserver container.  If it's blank, that's bad.  We couldn't
	  find the dbserver container.  The theory is that can't happen
      but ... */

   publish "logLog"
      (input substitute("Conversion container parent is &1",
                        pcContainer)).   
   if pcContainer <> "" then do:
      /* Create the container in the TYPE-DBSERVER container */
      createContainer(substitute("&1-conversion",
                                 pcContainer), 
                                 "{&TYPE-CONVERSION}", 
                                 "Conversion Specification",
                                 pcContainer).
      /* Now make it public knowledge */
      run setConvContainer
         (input pcContainer).
   end.

end procedure.

procedure setConvContainer :
/* ---------------------------------------------------------------------- */
/* Set the name of the current container being worked on.                 */
/*                                                                        */
/*  Input parameters:                                                     */
/*    pcContainerName - name of the container.                            */
/*                                                                        */
/* Output parameters:                                                     */
/*    <None>                                                              */
/*  Note: this very specific routine is the mechanism to hook conversions */
/*        into container processing.  It presupposes that a conversion    */
/*        container occurs at a known place in the hierarchy.             */
/* ---------------------------------------------------------------------- */
   define input  parameter pcContainerName as character  no-undo.
   
   define variable cDBServ    as character   no-undo.
   
   if getContainerAttribute(pcContainerName, "type") = "{&TYPE-PRODUCT}" then do:
      /* Now find first dbserver */
      run getChildByType
         (input pcContainerName,
          input "{&TYPE-DBSERVER}",
          output cDBServ).
   end.
   else do:
      /* See if we've been given a dbserver container; that's okay, too. */
      if getContainerAttribute(pcContainerName, "type") = "{&TYPE-DBSERVER}" then do:
         cDBServ = pcContainerName.
      end.
   end.

   if cDBServ <> "" then do:
      /* Get conversion container from DBServer container. */
      run getChildByType
         (input cDBServ,
          input "{&TYPE-CONVERSION}",
          output gcConvContainer).
   end.
   else
      gcConvContainer = "".

   if gcConvContainer <> "" then do:
      run initEnvVars.
   end.
end procedure.

PROCEDURE SetConvDB:
/* ---------------------------------------------------------------------- */
/* Set the name of the conversion database.                               */
/*                                                                        */
/*  Input parameters:                                                     */
/*    pcDBName - logical name of the conversion database.                 */
/*                                                                        */
/* Output parameters:                                                     */
/*    <None>                                                              */
/*                                                                        */
/* ---------------------------------------------------------------------- */
define input parameter pcDBName as character no-undo.

cConvDBName = pcDBName.

end PROCEDURE.

PROCEDURE GetDomain:
/* ---------------------------------------------------------------------- */
/* Look up the default domain.                                            */
/*                                                                        */
/*  Input parameters:                                                     */
/*    <None>                                                              */
/*                                                                        */
/* Output parameters:                                                     */
/*    pcDomainCode   The domain code.                                     */
/*                                                                        */
/* ---------------------------------------------------------------------- */
   define output parameter pcDomainCode as character no-undo.

   define variable hDomainBuffer as handle no-undo.

   pcDomainCode = "".
   create buffer hDomainBuffer for table "target.dom_mstr" no-error.
   if valid-handle(hDomainBuffer) then do:

      /* Find the dom_mstr for the conversion db that is not the system domain. */
      hDomainBuffer:find-first("where dom_db = " +
                             quoter(cConvDBName) +
                             " and dom_domain <> " + quoter("QAD"), no-lock) no-error.
      if hDomainBuffer:available then
         pcDomainCode = hDomainBuffer:buffer-field("dom_domain"):buffer-value.

      delete object hDomainBuffer.
   end.
   return.

end PROCEDURE.

PROCEDURE GetDomainMap:
/* ---------------------------------------------------------------------- */
/* Look up the domain to substitute for the database name passed in.      */
/*                                                                        */
/*  Input parameters:                                                     */
/*    pcDBName The name of the database being replaced.                   */
/*                                                                        */
/* Output parameters:                                                     */
/*    pcDomainCode   The domain code to substitute.                       */
/*                                                                        */
/* ---------------------------------------------------------------------- */
   define input parameter pcDBName as character no-undo.
   define output parameter pcDomainCode as character no-undo.


   define variable hDomainBuffer as handle no-undo.
   define variable hqDomain as handle no-undo.
   define variable hfDomCode as handle no-undo.

   pcDomainCode = "".
   create buffer hDomainBuffer for table "dom_mstr".
   create query hqDomain.

   /* Make query ready for running. */
   hqDomain:set-buffers(hDomainBuffer).
   /* Find the dom_mstr for the conversion db that is not the system domain. */
   hqDomain:query-prepare("for each dom_mstr where dom_db = " +
                          quoter(pcDBName) +
                          " and dom_domain <> " + quoter("QAD")).
   hqDomain:query-open().
   hqDomain:get-first().

   if hDomainBuffer:available then do:
      hfDomCode = hDomainBuffer:buffer-field("dom_domain").
      pcDomainCode = hfDomCode:buffer-value.
   end.
   else do:
      /* szz 01/20/09 commenting out as temporary work-around to reduce log file size
      publish "LogDisp"
         (input substitute("***** Unable to find &1 in domain map.", pcDBName)).
      publish "LogDisp"
         (input substitute("***** Used the default domain &1 instead.", cConvDBName)).
      */
      pcDomainCode = cDomainValue.
   end.
   hqDomain:query-close().
   delete object hDomainBuffer.
   delete object hqDomain.

   return.

end PROCEDURE.

PROCEDURE ShowDomainMap:
/* ---------------------------------------------------------------------- */
/* Show the domain map temp table.                                        */
/*                                                                        */
/*  Input parameters:                                                     */
/*    <None>                                                              */
/*                                                                        */
/* Output parameters:                                                     */
/*    <None>                                                              */
/*                                                                        */
/* ---------------------------------------------------------------------- */

   define variable hDomMapBuffer as handle no-undo.
   define variable hqDomMap as handle no-undo.
   define variable hfDBName as handle no-undo.
   define variable hfDomCode as handle no-undo.

   hDomMapBuffer = getTTHandle
      (input "DomMap").

   create query hqDomMap.

   /* Make query ready for running. */
   hqDomMap:set-buffers(hDomMapBuffer).
   hqDomMap:query-prepare("for each " + hDomMapBuffer:name).
   hqDomMap:query-open().
   hqDomMap:get-first().

   hfDBName = hDomMapBuffer:buffer-field("dbname").
   hfDomCode = hDomMapBuffer:buffer-field("domcode").
   do while not hqDomMap:query-off-end:
      publish "LogDisp"
         (input "Name: " + hfDBName:buffer-value +
                " Code: " + hfDomCode:buffer-value).
      hqDomMap:get-next().
   end.
   hqDomMap:query-close().
   delete object hqDomMap.
   return.

end PROCEDURE.

PROCEDURE FindTblConv:
/* ---------------------------------------------------------------------- */
/* Find the procedures for the named table if one exists.                 */
/*                                                                        */
/*  Input parameters:                                                     */
/*    pcTable  The name of the source table.                              */
/*                                                                        */
/* Output parameters:                                                     */
/*    pcTblProcName  The list of procedure names for the table.           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
    define input  parameter pcTable       as character no-undo.
    define output parameter pcTblProcName as character no-undo.

    define variable idx         as integer no-undo.
    define variable hbExcept    as handle  no-undo.

    pcTblProcName = "".
    for each tvprogs where doconv:
       /* If this conversion has all, add the program associated with all */
       /* if the table is not in the exception list. "all" is used for the*/
       /* domain conversion list.                                         */
       idx = lookup("all", tvprogs.tables).
       if idx <> 0 then do:
          if pcTblProcName = "" then do:
             pcTblProcName = entry(idx + 1, tvprogs.tables).
          end.
          else do:
             pcTblProcName = pcTblProcName + "," + entry(idx + 1, tvprogs.tables).
          end.
       end.
       /* If this conversion has every, add the program associated with every*/
       /* if the table is not in the exception list. "every" is used for the*/
       /* oid conversion list.                                              */
       idx = lookup("every", tvprogs.tables).
       if idx <> 0 then do:
          if pcTblProcName = "" then do:
             pcTblProcName = entry(idx + 1, tvprogs.tables).
          end.
          else do:
             pcTblProcName = pcTblProcName + "," + entry(idx + 1, tvprogs.tables).
          end.
       end.
       /* If this conversion has this table, add the associated program.*/
       idx = lookup(pcTable, tvprogs.tables).
       if idx <> 0 then do:
          if pcTblProcName = "" then do:
             pcTblProcName = entry(idx + 1, tvprogs.tables).
          end.
          else do:
             pcTblProcName = pcTblProcName + "," + entry(idx + 1, tvprogs.tables).
          end.
       end.
    end.


end PROCEDURE.


PROCEDURE postFindTblConv:
/* ---------------------------------------------------------------------- */
/* Find the procedures for the named table if one exists.                 */
/*                                                                        */
/*  Input parameters:                                                     */
/*    pcTable  The name of the source table.                              */
/*                                                                        */
/* Output parameters:                                                     */
/*    pcTblProcName  The list of procedure names for the table.           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
    DEFINE INPUT PARAMETER pcTable AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER pcTblProcName AS CHARACTER NO-UNDO.

    define variable idx         as integer no-undo.

    pcTblProcName = "".
    for each tvprogs where doconv:
       /* If this conversion has every, add the program associated with every */
       /* if the table is not in the exception list. "every" is used for the*/
       /* oid conversion list.                                         */
/*   oidpop.p is the only conversion for "every".  It has to do its work during the
     buffer-copy phase, so we will eliminate the processing of "every" during the 
     conversion phase.  
       idx = lookup("every", tvprogs.tables).
       if idx <> 0 then do:
          if pcTblProcName = "" then do:
             pcTblProcName = entry(idx + 1, tvprogs.tables).
          end.
          else do:
             pcTblProcName = pcTblProcName + "," + entry(idx + 1, tvprogs.tables).
          end.
       end.
*/       
       /* If this conversion has this table, add the associated program.*/
       idx = lookup(pcTable, tvprogs.tables).
       if idx <> 0 then do:
          if pcTblProcName = "" then do:
             pcTblProcName = entry(idx + 1, tvprogs.tables).
          end.
          else do:
             pcTblProcName = pcTblProcName + "," + entry(idx + 1, tvprogs.tables).
          end.
       end.
    end.


end PROCEDURE.


PROCEDURE buildUITemp:
/*------------------------------------------------------------------------
   Procedure - buildUITemp
   Parameters: pcIniName - the name of the ini file.
               pcSection - the section of the ini to use.
               phUIWidgets - handle of temp table.
               phUIType

   Purpose: Builds temp table based on the ini and section passed.

--------------------------------------------------------------------------*/
define input parameter pcIniName as character no-undo.
define input parameter pcSection as character no-undo.
define input parameter phUIWidgets as handle no-undo.
define input parameter phUIType as handle no-undo.

define variable hTable as handle no-undo.
define variable hUIBuffer as handle no-undo.
define variable hTempTable as handle no-undo.
define variable hUITable as handle no-undo.
define variable hqUI as handle no-undo.
define variable hfField as handle no-undo.
define variable hGenIni as handle no-undo.

define variable cIniSection as character no-undo.
define variable cWidgetType as character no-undo.
define variable cItemName as character no-undo.
define variable cLabel as character no-undo.
define variable cDataType as character no-undo.
define variable iExtents as integer no-undo.
define variable cFormat as character no-undo.
define variable cInitialValue as character no-undo.
define variable cHelp as character no-undo.
define variable cValidation as character no-undo.
define variable cLookup as character no-undo.
define variable lOK as logical no-undo.
define variable fldidx as integer no-undo.
define variable hField as handle no-undo.
define variable iNumFields as integer no-undo.
define variable hfSection as handle no-undo.
define variable hfItemName as handle no-undo.
define variable hfWidgetType as handle no-undo.


run genini.p persistent set hGenIni.

loadIniFile(pcIniName, hGenIni).

create temp-table hTempTable.
run buildTempTable in hGenIni
   (input hTempTable).

delete procedure hGenini.

/* Get buffer object handle for temp-table */
hTable = hTempTable:default-buffer-handle.

phUIType:add-new-field("Section","character").
phUIType:add-new-field("ItemName","character").
phUIType:add-new-field("WidgetType","character").
phUIType:add-new-field("Lookup","character").
phUIType:add-new-field("EnvValue","character").
phUIType:TEMP-TABLE-PREPARE("convttwidgets").

hUIBuffer = phUIType:default-buffer-handle.
hfSection = hUIBuffer:buffer-field("Section").
hfItemName = hUIBuffer:buffer-field("ItemName").
hfWidgetType = hUIBuffer:buffer-field("WidgetType").

iNumFields = 0.
create query hqUI.
hqUI:set-buffers(hTable).
hqUI:query-prepare("for each " + hTable:name +
                   " where section = " + quoter(pcSection)).
hqUI:query-open().
hqUI:get-first().
do while not hqUI:query-off-end:
   iNumFields = iNumFields + 1.
   hfField = hTable:buffer-field("IniSection").
   cIniSection = hfField:buffer-value.
   hfField = hTable:buffer-field("WidgetType").
   cWidgetType = hfField:buffer-value.
   hfField = hTable:buffer-field("ItemName").
   cItemName = hfField:buffer-value.
   hfField = hTable:buffer-field("Label").
   cLabel = hfField:buffer-value.
   hfField = hTable:buffer-field("DataType").
   cDataType = hfField:buffer-value.
   hfField = hTable:buffer-field("Extents").
   iExtents = integer(hfField:buffer-value).
   hfField = hTable:buffer-field("Format").
   cFormat = hfField:buffer-value.
   hfField = hTable:buffer-field("InitialValue").
   cInitialValue = hfField:buffer-value.
   phUIWidgets:ADD-NEW-FIELD(cItemName, cDataType, iExtents,
                            cFormat, cInitialValue, cLabel).

   hUIBuffer:buffer-create().
   assign
      hfSection:buffer-value = cIniSection
      hfItemName:buffer-value = cItemName
      hfWidgetType:buffer-value = cWidgetType.

   hqUI:get-next().
end.
if iNumFields > 0 then do:
   cLastSectionProcessed = trim(pcSection, "[]").
   pcSection = replace(pcSection, ".", "").
   phUIWidgets:TEMP-TABLE-PREPARE(substring(replace(trim(pcSection, "[]"), " ", ""), 1, 32)).

   hUITable = phUIWidgets:default-buffer-handle.


   hqUI:get-first().
   do while not hqUI:query-off-end:
      hfField = hTable:buffer-field("Lookup").
      cLookup = hfField:buffer-value.
      hfField = hTable:buffer-field("Validation").
      cValidation = hfField:buffer-value.
      hfField = hTable:buffer-field("ItemName").
      cItemName = hfField:buffer-value.
      hfField = hUITable:buffer-field(cItemName).
      if cValidation <> "none" then do:
         hfField:validate-expression = cValidation.
      end.
      else do:
         hfField:validate-expression = "".
      end.
      if cLookup <> "none" then do:
         hfField:help = cLookup.
      end.
      else do:
         hfField:help = "".
      end.
      hqUI:get-next().
   end.
end.
else do:
   /* There was no section.  Build a dummy temp table to return. */
   phUIWidgets:ADD-NEW-FIELD("NoSection", "character").
   phUIWidgets:TEMP-TABLE-PREPARE("NoTable").
   hUITable = phUIWidgets:default-buffer-handle.
end.
hqUI:query-close().
delete object hqUI.
delete object hTempTable.


end PROCEDURE.



PROCEDURE buildTVProgs:
/*------------------------------------------------------------------------
   Procedure - buildTVProgs
   Parameters: <None>.

   Purpose: Builds the tvprogs temp table.

   Reads the convfunc.ini file, determines the release of the connected db.
   Allows the user to select the non-mandatory conversion functions
   appropriate to the release.  Runs the dialogs associcated with the
   selected conversion functions.  Updates envvars.ini with results of
   the function dialogs.  Builds the tables entries for each function
   selected.
--------------------------------------------------------------------------*/

DEFINE VARIABLE hbuild AS HANDLE NO-UNDO.
define variable hTable as handle no-undo.
define variable hTable1 as handle no-undo.
define variable hTable2 as handle no-undo.
define variable hTable3 as handle no-undo.
define variable hUITable as handle no-undo.
define variable hUIWidgets as handle no-undo.
define variable hfField as handle no-undo.
define variable hfSection as handle no-undo.
define variable hfMand as handle no-undo.
define variable hfUI as handle no-undo.
define variable hfTables as handle no-undo.
define variable hqUI as handle no-undo.
define variable hTempTable as handle no-undo.
define variable hqTempTable as handle no-undo.
define variable hGenIni as handle no-undo.
define variable idx as integer no-undo.
define variable hUIType as handle no-undo.
define variable hUIBuffer as handle no-undo.
define variable hfItemName as handle no-undo.
define variable hfItemValue as handle no-undo.
define variable hfWidgetType as handle no-undo.

define variable cItemName as character no-undo.
define variable cLabel as character no-undo.
define variable cDataType as character no-undo.
define variable iExtents as integer no-undo.
define variable cFormat as character no-undo.
define variable cInitialValue as character no-undo.
define variable cHelp as character no-undo.
define variable lAdvanced as logical no-undo.

define variable cRelease as character no-undo.
define variable cItemValue as character no-undo.

define variable lOK as logical no-undo.
define variable hWorkFlow as handle no-undo.

run wkflchk.p (output hWorkFlow).

if valid-handle(hWorkFlow) then do:
    run set-workflow-result in hWorkFlow
       (input 'Error').
end.

cRelease = getSourceDBVersion().

if cRelease = "" then do:
   publish "LogDisp"
      (input "***** Unknown version for source database.").
   publish "LogDisp"
      (input "***** Make sure that source database is connected.").
   publish "LogDone".
   publish "LogHide".
   return.
end.

/* See if we've already built a Conversion Selection list */
if absPath("selconv.ini") <> ? then do:
   /* We have one, so we use it. */
   run loadSelConv.
end.
else do:
   /* This is (probably) our first time through so build it */
   run makeTVTempTable.
   run buildTVProgsTT.
end.

/* Now we have a list of the applicable sections in tvprogs.  */
/* We now have to build a dialog to select the non-mandatory ones. */

create temp-table hUIType.
hUIType:add-new-field("Section","character").
hUIType:add-new-field("ItemName","character").
hUIType:add-new-field("WidgetType","character").
hUIType:add-new-field("Lookup","character").
hUIType:add-new-field("EnvValue","character").
hUIType:TEMP-TABLE-PREPARE("convttwidgets").

hUIBuffer = hUIType:default-buffer-handle.
hfItemName = hUIBuffer:buffer-field("ItemName").
hfWidgetType = hUIBuffer:buffer-field("WidgetType").

create temp-table hUIWidgets.

idx = 1.
for each tvprogs:
   cItemName = "Item" + string(idx).
   tvprogs.widgetname = cItemName.
   cLabel = trim(convname, "[]").
   cDataType = "Logical".
   iExtents = 0.
   cFormat = "Yes/No".
   cInitialValue = (if tvprogs.mandatory then "Yes" else "No").
   hUIWidgets:ADD-NEW-FIELD(cItemName, cDataType, iExtents,
                            cFormat, cInitialValue, cLabel).
   hUIBuffer:buffer-create().
   assign
      hfItemName:buffer-value = cItemName
      hfWidgetType:buffer-value = "Fill-In".
   idx = idx + 1.
end.
hUIWidgets:TEMP-TABLE-PREPARE("UIWidgets").

hUITable = hUIWidgets:default-buffer-handle.

cLastSectionProcessed = "Select Conversion Functions".


create query hqTempTable.

/* make query ready for running */
hqTempTable:set-buffers(hUITable).
hqTempTable:query-prepare("for each " + hUITable:name).
hqTempTable:query-open().

ladvanced = (readAttrValue
   (input "[environment]",
    input "advanced") = "yes").

RUN blddialg.p PERSISTENT SET hbuild.

do transaction:
    /* make query ready for running */

    hqTempTable:get-first().

    RUN create-field-list IN hbuild
        (input hUITable,
         input-output hUIType).

    RUN build-dialog IN hbuild.

    for each tvprogs where mandatory:
       if lAdvanced then do:
          run set-default-logical in hbuild
             (input tvprogs.widgetname,
              input no).
       end.
       else do:
          run disable-field in hbuild
             (input tvprogs.widgetname).
       end.
    end.

    run show-dialog in hbuild.
end.

/* Get the result of the dialog, if false the dialog was cancelled. */
run get-result in hbuild
   (output lOK).

run destructor in hbuild.

if lOK then do:
   /* Transfer the selections from the dialog to tvprogs.doconv. */
   hqTempTable:set-buffers(hUITable).
   hqTempTable:query-prepare("for each " + hUITable:name).
   hqTempTable:query-open().
   hqTempTable:get-first().
   if hUITable:available then do:

      do idx = 1 to hUITable:NUM-FIELDS:
         hfField = hUITable:BUFFER-FIELD(idx).
         find first tvprogs where tvprogs.widgetname = hfField:name no-error.
         if available tvprogs then do:
            tvprogs.doconv = hfField:buffer-value.
         end.
      end.
   end.
   else do:
      message "No record available from selection dialog." view-as alert-box error.
   end.
   /* Save tvprogs temp table if it has anything in it. */
   run saveSelConv.

   if valid-handle(hWorkFlow) then do:
      run set-workflow-result in hWorkFlow
         (input "Done").
   end.
end.
else do:
   if valid-handle(hWorkFlow) then do:
      run set-workflow-result in hWorkFlow
         (input "Cancelled").
   end.
   publish "LogDisp"
      (input "Conversion Function Selection cancelled by user.").
   publish "LogDone".
end.
hqTempTable:query-close().
delete object hqTempTable.
delete object hUIWidgets.
delete object hUIType.

end PROCEDURE.


procedure saveSelConv:
/*------------------------------------------------------------------------------
  Purpose:     Saves temp-table tvprogs into selconv.ini.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* Save tvprogs temp table if it has anything in it. */
   find first tvprogs no-error.
   if available tvprogs then do:
      output to selconv.ini.
      for each tvprogs where convname <> "":
         export tvprogs.
      end.
      output close.
   end.
   
end procedure.

procedure loadSelConv:
/*------------------------------------------------------------------------------
  Purpose:     Loads selconv.ini into temp-table tvprogs
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   if absPath("selconv.ini") <> ? then do:
      empty temp-table tvprogs.
      input from value(file-info:full-pathname) no-echo.
      repeat:
         create tvprogs.
         import tvprogs.
      end.
      input close.

      for each tvprogs where tvprogs.convname = "":
         delete tvprogs.
      end.
   end.

end procedure.


procedure makeTVTempTable:
/*------------------------------------------------------------------------------
  Purpose:    Localize the construction of the convfunc tvlist
  Parameters:  <none>
  Notes:      this uses global variables to store the temptable handle
------------------------------------------------------------------------------*/
   define variable hGenIni       as handle      no-undo.

   run genini.p persistent set hGenIni.

   publish "logLog"
      (input "Reading convfunc.ini...").
   loadIniFile("convfunc.ini", hGenIni).

   create temp-table hTempTable.

   run moveTempTable in hGenIni
      (input "tvlist",
       input hTempTable).

   delete procedure hGenIni.

end procedure.

procedure buildTVProgsTT:
/*------------------------------------------------------------------------------
  Purpose:    Localize the construction of a query on hTempTable to build
              tvprogs
  Parameters:  <none>
  Notes:      
------------------------------------------------------------------------------*/
   define variable hqUI          as handle      no-undo.
   define variable hTable        as handle      no-undo.
   define variable hTable1       as handle      no-undo.
   define variable hTable2       as handle      no-undo.
   define variable hTable3       as handle      no-undo.

   define variable hfField       as handle      no-undo.
   define variable hfSection     as handle      no-undo.
   define variable hfMand        as handle      no-undo.
   define variable hfUI          as handle      no-undo.
   define variable hfTables      as handle      no-undo.

   define variable cItemValue    as character   no-undo.
   define variable idx           as integer     no-undo.
   define variable cRelease      as character   no-undo.
   define variable iCount        as integer     no-undo. 

   cRelease = getSourceDBVersion().
   publish "logLog"
      (input substitute("Building tvprogs for &1...",
                        cRelease)).
   
   /* create buffer hTable for table hTempTable:name. */
   hTable = hTempTable:default-buffer-handle.
   create buffer hTable1 for table hTable buffer-name "Mand".
   create buffer hTable2 for table hTable buffer-name "ConvUI".
   create buffer hTable3 for table hTable buffer-name "Tables".

   empty temp-table tvprogs.
   iCount = 0.

   create query hqUI.
   hqUI:set-buffers(hTable, hTable1, hTable3, hTable2).
   hqUI:query-prepare(
      "for each tvlist where ItemName = " + quoter("Releases") +
      ", first Mand where Mand.Section = tvlist.Section and " +
                         "Mand.ItemName = " + quoter("Mandatory") +
      ", first Tables outer-join where Tables.Section = tvlist.Section" +
                                 " and Tables.ItemName = " + quoter("Tables") +
      ", first ConvUI outer-join where ConvUI.Section = tvlist.Section" +
                                 " and ConvUI.ItemName = " + quoter("ConvUI") ).
   hqUI:query-open().
   hqUI:get-first().

   hfSection   = hTable:buffer-field("Section").
   hfField     = hTable:buffer-field("ItemValue").
   hfMand      = hTable1:buffer-field("ItemValue").
   hfUI        = hTable2:buffer-field("ItemValue").
   hfTables    = hTable3:buffer-field("ItemValue").
   
   do while not hqUI:query-off-end:
      cItemValue = hfField:buffer-value.
      idx = lookup(cRelease, cItemValue).
      if idx <> 0 then do:
         /* Found release, add section to list. */
         create tvprogs.
         iCount = iCount + 1.
         assign 
            convname = hfSection:buffer-value
            mandatory = (hfMand:buffer-value = "yes")
            doconv = (hfMand:buffer-value = "yes")
            convui = (if hfUI:buffer-value = ? then "" else "[" + hfUI:buffer-value + "]")
            tables = (if hfTables:buffer-value = ? then "" else hfTables:buffer-value).
            
         publish "LogLog"
            (input substitute("Adding &1 Conversion.",
                              convname)).
      end.
      hqUI:get-next().
   end.

   publish "logLog"
      (input substitute("Created &1 tvprog records...",
                        iCount)).
   
   /* All done.  Now do housekeeping. */
   hqUI:query-close().
   delete object hqUI.
   delete object hTable1.
   delete object hTable2.
   delete object hTable3.
   delete object hTempTable.

end procedure.

PROCEDURE getUserInput:
/*------------------------------------------------------------------------
   Procedure - getUserInput
   Parameters: <None>.

   Purpose: Get input from the user for each selected conversion where
            convui is specified.

--------------------------------------------------------------------------*/

   define variable h-workflow as handle    no-undo.
   define variable c-result   as character no-undo.


   run wkflchk.p (output h-workflow).

   /* Set local result to Done so we can start. */
   c-result = "Done".

   for each tvprogs where doconv and tvprogs.convui <> "":
      if c-result = "Done" then do:
         run getinfo.p
            (input "convui.ini",
             input tvprogs.convui,
             input "envvars.ini").
         if valid-handle(h-workflow) then
            run get-workflow-result in h-workflow
               (output c-result).
      end.
   end.

   if c-result <> "Done" then
      publish "LogDone".

end PROCEDURE.


PROCEDURE loadSpecialDumps:
/*------------------------------------------------------------------------
   Procedure - loadSpecialDumps
   Parameters: <None>.

   Purpose: Loads the tables that were dumped by the special dump programs
            for this release.

   Note:  Currently this routine just does the cu_mstr and exr_rate loads
          for the appropriate releases.  This may be generalized later.
--------------------------------------------------------------------------*/

   define variable lv_destDB as character no-undo.
   define variable tvtblname as character no-undo.
   define variable dumpdir   as character no-undo.
   define variable putit     as character no-undo.
   define variable cDomain   as character no-undo.
   define variable hCU_MSTR  as handle    no-undo.
   define variable htXR_RATE as handle    no-undo.
   define variable cSrcVer   as character no-undo.

   /* Get the source version.  */
   cSrcVer = getSourceDBVersion().

   /* Return if greater than 86. */
   if cSrcVer > "86" then return.

   /* Get the conversion domain.  */
   run getDomainValue
      (output cDomain).

   /* Get the account, sub and cost center values. */
   lvp_ugain_acct = trim(readAttrValue("[environment]", "EURO00")).
   lvp_ugain_sub  = trim(readAttrValue("[environment]", "EURO01")).
   lvp_ugain_cc   = trim(readAttrValue("[environment]", "EURO02")).
   lvp_uloss_acct = trim(readAttrValue("[environment]", "EURO03")).
   lvp_uloss_sub  = trim(readAttrValue("[environment]", "EURO04")).
   lvp_uloss_cc   = trim(readAttrValue("[environment]", "EURO05")).
   lvp_rgain_acct = trim(readAttrValue("[environment]", "EURO06")).
   lvp_rgain_sub  = trim(readAttrValue("[environment]", "EURO07")).
   lvp_rgain_cc   = trim(readAttrValue("[environment]", "EURO08")).
   lvp_rloss_acct = trim(readAttrValue("[environment]", "EURO09")).
   lvp_rloss_sub  = trim(readAttrValue("[environment]", "EURO10")).
   lvp_rloss_cc   = trim(readAttrValue("[environment]", "EURO11")).

   lvp_ex_rnd_acct = trim(readAttrValue("[environment]", "EURO12")).
   lvp_ex_rnd_sub  = trim(readAttrValue("[environment]", "EURO13")).
   lvp_ex_rnd_cc   = trim(readAttrValue("[environment]", "EURO14")).


   /* Set the table to load to cu_mstr. */
   assign
      tvtblname = "cu_mstr".

   /* Get the dump directory.  */
   dumpdir = readAttrValue (input "[environment]",
                            input "dumpdir").

   /* Build the complete path to the file to load. */
   if dumpdir = "" then
      putit = tvtblname + ".dxx".
   else do:
      if opsys = "unix" then
         putit = dumpdir + "/"  + tvtblname + ".dxx".
      else
         putit = dumpdir + "~\" + tvtblname + ".dxx".
   end.

   /* Find the file. */
   if absPath(putit) = ? then do:
      publish "LogDisp"
         (input substitute("***** Unable to find &1 for load of cu_mstr.",
                           putit)).
      publish "LogDisp"
         (input "      cu_mstr not loaded.").
      publish "LogDone".
   end.
   else do:
      /* File was found.  Create the buffer. */
      create buffer hCU_MSTR for table "cu_mstr" no-error.
      if not valid-handle(hCU_MSTR) then do:
         publish "LogDisp"
            (input "***** Unable to create buffer for load of cu_mstr.").
         publish "LogDisp"
            (input "      cu_mstr not loaded.").
         publish "LogDone".
      end.
      else do:
         /* Buffer created OK.  Disable triggers and load.  */
         hCU_MSTR:disable-load-triggers(false).
         hCU_MSTR:disable-dump-triggers().

         /* Delete all existing cu_mstr records, if any. */
         hCU_MSTR:find-first("", exclusive-lock) no-error.
         do while hCU_MSTR:available:
            hCU_MSTR:buffer-delete().
            hCU_MSTR:find-first("", exclusive-lock) no-error.
         end.

         run loadTable
            (input hCU_MSTR,
             input putit,
             input cDomain,
             input this-procedure,
             input "doCuMstr").

         delete object hCU_MSTR.
      end. /* else of not valid-handle(hCU_MSTR) */
   end. /* else of file-info:full-pathname = ? */

   /* Now set the table to exr_rate. */
   assign
      tvtblname = "exr_rate".

   /* Build the complete path to the file to load. */
   if dumpdir = "" then
      putit = tvtblname + ".dxx".
   else do:
      if opsys = "unix" then
         putit = dumpdir + "/"  + tvtblname + ".dxx".
      else
         putit = dumpdir + "~\" + tvtblname + ".dxx".
   end.

   /* Find the file. */
   if absPath(putit) = ? then do:
      publish "LogDisp"
         (input substitute("***** Unable to find &1 for load of exr_rate.",
                           putit)).
      publish "LogDisp"
         (input "      exr_rate not loaded.").
      publish "LogDone".
   end.
   else do:
      /* File was found.  Create the buffer. */
      create buffer htXR_RATE for table "exr_rate" no-error.
      if not valid-handle(htXR_RATE) then do:
         publish "LogDisp"
            (input "***** Unable to create buffer for load of exr_rate.").
         publish "LogDisp"
            (input "      exr_rate not loaded.").
         publish "LogDone".
      end.
      else do:
         /* Buffer created OK.  Disable triggers and load.  */
         htXR_RATE:disable-load-triggers(false).
         htXR_RATE:disable-dump-triggers().

         /* Delete all existing exr_rate records, if any. */
         htXR_RATE:find-first("", exclusive-lock) no-error.
         do while htXR_RATE:available:
            htXR_RATE:buffer-delete().
            htXR_RATE:find-first("", exclusive-lock) no-error.
         end.

         run loadTable
            (input htXR_RATE,
             input putit,
             input cDomain,
             input this-procedure,
             input "doExrRate").

         delete object htXR_RATE.
      end. /* else of not valid-handle(htXR_RATE) */
   end. /* else of file-info:full-pathname = ? */


/*
   /* Now set the table to sad_det. */
   assign
      tvtblname = "sad_det".

   /* Build the complete path to the file to load. */
   if dumpdir = "" then
      putit = tvtblname + ".dxx".
   else do:
      if opsys = "unix" then
         putit = dumpdir + "/"  + tvtblname + ".dxx".
      else
         putit = dumpdir + "~\" + tvtblname + ".dxx".
   end.

   /* Find the file. */
   file-info:file-name = putit.
   if file-info:full-pathname = ? then do:
      publish "LogDisp"
         (input substitute("***** Unable to find &1 for load of sad_det.",
                           putit)).
      publish "LogDisp"
         (input "      sad_det not loaded.").
      publish "LogDone".
   end.
   else do:
      /* File was found.  Create the buffer. */
      create buffer hSAD_DET for table "sad_det" no-error.
      if not valid-handle(hSAD_DET) then do:
         publish "LogDisp"
            (input "***** Unable to create buffer for load of sad_det.").
         publish "LogDisp"
            (input "      sad_det not loaded.").
         publish "LogDone".
      end.
      else do:
         /* Buffer created OK.  Disable triggers and load.  */
         hSAD_DET:disable-load-triggers(false).
         hSAD_DET:disable-dump-triggers().

         /* Delete all existing exr_rate records, if any. */
         hSAD_DET:find-first("", exclusive-lock) no-error.
         do while hSAD_DET:available:
            hSAD_DET:buffer-delete().
            hSAD_DET:find-first("", exclusive-lock) no-error.
         end.

         run loadTable
            (input hSAD_DET,
             input putit,
             input cDomain,
             input this-procedure,
             input "doSadDet").

         delete object hSAD_DET.
      end. /* else of not valid-handle(hSAD_DET) */
   end. /* else of file-info:full-pathname = ? */
*/

end PROCEDURE.


PROCEDURE doCuMstr:
/*------------------------------------------------------------------------
   Procedure - doCuMstr
   Input Parameters: phCuMstr - handle to the buffer for cu_mstr.

   Purpose: Does extra buffer manipulation when loading cu_mstr.dxx.

--------------------------------------------------------------------------*/

   define input parameter phCuMstr as handle no-undo.

   define variable dNextOID as decimal no-undo.

   assign
      dNextOID = getNextOID()
      phCuMstr:buffer-field("oid_cu_mstr"):buffer-value = dNextOID

      /* Fill in the values for the various accounts that did not
         previously exist. Use the user input values.               */
      phCuMstr:buffer-field("cu__qadc02"):buffer-value = lvp_ugain_acct
         when phCuMstr:buffer-field("cu__qadc02"):buffer-value = ""
      phCuMstr:buffer-field("cu__qadc03"):buffer-value = lvp_ugain_sub
         when phCuMstr:buffer-field("cu__qadc03"):buffer-value = ""
      phCuMstr:buffer-field("cu__qadc04"):buffer-value = lvp_ugain_cc
         when phCuMstr:buffer-field("cu__qadc04"):buffer-value = ""

      phCuMstr:buffer-field("cu__qadc05"):buffer-value = lvp_uloss_acct
         when phCuMstr:buffer-field("cu__qadc05"):buffer-value = ""
      phCuMstr:buffer-field("cu__qadc06"):buffer-value = lvp_uloss_sub
         when phCuMstr:buffer-field("cu__qadc06"):buffer-value = ""
      phCuMstr:buffer-field("cu__qadc07"):buffer-value = lvp_uloss_cc
         when phCuMstr:buffer-field("cu__qadc07"):buffer-value = ""

      phCuMstr:buffer-field("cu__qadc08"):buffer-value = lvp_rgain_acct
         when phCuMstr:buffer-field("cu__qadc08"):buffer-value = ""
      phCuMstr:buffer-field("cu__qadc09"):buffer-value = lvp_rgain_sub
         when phCuMstr:buffer-field("cu__qadc09"):buffer-value = ""
      phCuMstr:buffer-field("cu__qadc10"):buffer-value = lvp_rgain_cc
         when phCuMstr:buffer-field("cu__qadc10"):buffer-value = ""

      phCuMstr:buffer-field("cu__qadc11"):buffer-value = lvp_rloss_acct
         when phCuMstr:buffer-field("cu__qadc11"):buffer-value = ""
      phCuMstr:buffer-field("cu__qadc12"):buffer-value = lvp_rloss_sub
         when phCuMstr:buffer-field("cu__qadc12"):buffer-value = ""
      phCuMstr:buffer-field("cu__qadc13"):buffer-value = lvp_rloss_cc
         when phCuMstr:buffer-field("cu__qadc13"):buffer-value = ""

      phCuMstr:buffer-field("cu__qadc14"):buffer-value = lvp_ex_rnd_acct
         when phCuMstr:buffer-field("cu__qadc14"):buffer-value = ""
      phCuMstr:buffer-field("cu__qadc15"):buffer-value = lvp_ex_rnd_sub
         when phCuMstr:buffer-field("cu__qadc15"):buffer-value = ""
      phCuMstr:buffer-field("cu__qadc16"):buffer-value = lvp_ex_rnd_cc
         when phCuMstr:buffer-field("cu__qadc16"):buffer-value = ""

      no-error.
      if error-status:error then run checkErrors.

end PROCEDURE.

PROCEDURE doExrRate:
/*------------------------------------------------------------------------
   Procedure - doExrRate
   Input Parameters: ptExrRate - handle to the buffer for exr_rate.

   Purpose: Does extra buffer manipulation when loading exr_rate.dxx.

--------------------------------------------------------------------------*/

   define input parameter phtxrRate as handle no-undo.

   define variable dNextOID as decimal no-undo.

   assign
      dNextOID = getNextOID()
      phtxrRate:buffer-field("oid_exr_rate"):buffer-value = dNextOID no-error.
   if error-status:error then run checkErrors.


end PROCEDURE.


PROCEDURE doSadDet:
/*------------------------------------------------------------------------
   Procedure - doSadDet
   Input Parameters: phSadDet - handle to the buffer for sad_det.

   Purpose: Does extra buffer manipulation when loading sad_det.dxx.

--------------------------------------------------------------------------*/

   define input parameter phSadDet as handle no-undo.



end PROCEDURE.

PROCEDURE loadTable:
/*------------------------------------------------------------------------
   Procedure - loadTable
   Parameters: <None>.

   Purpose: Loads a table from an ASCII file.

--------------------------------------------------------------------------*/
   define input parameter phTable    as handle    no-undo.
   define input parameter pcDmpFile  as character no-undo.
   define input parameter pcDomain   as character no-undo.
   define input parameter phCallBack as handle    no-undo.
   define input parameter pcCallBack as character no-undo.

   define variable hfDomain  as handle    no-undo.
   define variable hDataLoad as handle    no-undo.
   define variable cIdxName  as character no-undo.
   define variable hTT       as handle    no-undo.
   define variable hTTBuffer as handle    no-undo.
   define variable iRecsUpd  as integer   no-undo.
   define variable iRecsAdd  as integer   no-undo.
   define variable iFileSize as integer   no-undo.
   define variable hTail     as handle    no-undo.
   define variable cDotDDir  as character no-undo.

   if lGotCancel then return.

   iFileSize = ?.

   run readtail.p persistent set hTail
      (input pcDmpfile).
   run readTrailer in hTail.
   run getExpectedRecs in hTail
      (output iFileSize).
   run destructor in hTail.

   if iFileSize = 0 then return.

   run setCBHandle
      (input phCallBack).

   run setCBName
      (input pcCallBack).

   hfDomain = phTable:buffer-field(entry(1, phTable:name, "_") + "_domain") no-error.
   cDotDDir = getDirName(pcDmpFile).
   run dataload.p persistent set hDataLoad
      (input (if valid-handle(hfDomain) then pcDomain else ""),
       input  cDotDDir).

   /* Find the unique indexes of this table */
   run findIndexes in hDataLoad
      ( input phTable,
       output cIdxName).

   if cIdxName <> "" then do:
      /* Create a temp-table for the table */
      create temp-table hTT.

      hTT:create-like(phTable, entry(1, cIdxName)).
      hTT:temp-table-prepare("TempData").

      hTTBuffer = hTT:default-buffer-handle.

      run readData in hDataLoad
         (input hTTBuffer,
          input pcDmpFile,
          input phTable:name,
          input iFileSize).

      run loadData in hDataLoad
         ( input hTTBuffer,
           input phTable,
          output iRecsUpd,
          output iRecsAdd).
/*
      publish "SaveTrail"
         (input iRecsUpd,
          input phTable:name).
*/
      if iRecsUpd > 0 then
         publish "LogDisp"
            (input substitute("&1 records updated from &2.",
                              iRecsUpd,
                              phTable:name)).
      if iRecsAdd > 0 then
         publish "LogDisp"
            (input substitute("&1 records added in &2.",
                            iRecsAdd,
                            phTable:name)).
      delete object hTT.

   end.

   run destructor in hDataLoad.

   run setCBHandle
      (input ?).

   run setCBName
      (input ?).

end PROCEDURE.


PROCEDURE setCBHandle:
/*------------------------------------------------------------------------
   Procedure - setCBHandle
   Input Parameters: phCB - handle to the procedure containing the call
                            back routine.

   Purpose: Set the global variable defining the call back.

   Note: Each routine that sets this value is responisble for resetting it
         when done.
--------------------------------------------------------------------------*/

   define input parameter phCB as handle no-undo.

   hCB = phCB.

end PROCEDURE.


PROCEDURE setCBName:
/*------------------------------------------------------------------------
   Procedure - setCBName
   Input Parameters: pcCB - Name of the internal procedure that implements
                            the call back routine.

   Purpose: Set the global variable defining the call back.

   Note: Each routine that sets this value is responisble for resetting it
         when done.
--------------------------------------------------------------------------*/

   define input parameter pcCB as character no-undo.

   cCB = pcCB.

end PROCEDURE.


PROCEDURE getCBHandle:
/*------------------------------------------------------------------------
   Procedure - getCBHandle
   Output Parameters: phCB - handle to the procedure containing the call
                             back routine.

   Purpose: Get the global variable defining the call back.

--------------------------------------------------------------------------*/

   define output parameter phCB as handle no-undo.

   phCB = hCB.

end PROCEDURE.


PROCEDURE getCBName:
/*------------------------------------------------------------------------
   Procedure - GetCBName
   Output Parameters: pcCB - Name of the internal procedure that implements
                             the call back routine.

   Purpose: Get the global variable defining the call back.

--------------------------------------------------------------------------*/

   define output parameter pcCB as character no-undo.

   pcCB = cCB.

end PROCEDURE.


PROCEDURE runSpecialDumps:
/*------------------------------------------------------------------------
   Procedure - runSpecialDumps
   Parameters: <None>.

   Purpose: Runs the special dump programs for this release.

   Reads the specdump.ini file, determines the release of the connected db.
   Builds the tables entries for each dump function.  Determines if the
   necessary tables are connected and if so runs the dump.
--------------------------------------------------------------------------*/

define variable hbuild    as handle no-undo.
define variable hReleases as handle no-undo.
define variable hProgram as handle no-undo.
define variable hTables as handle no-undo.
define variable hfSection as handle no-undo.
define variable hfReleases as handle no-undo.
define variable hfTables as handle no-undo.
define variable hfProgram as handle no-undo.
define variable hqDumps as handle no-undo.
define variable hTempTable as handle no-undo.
define variable hGenIni as handle no-undo.
define variable idx as integer no-undo.

define variable cItemValue as character no-undo.

define variable lOK as logical no-undo.

define variable cSrcRelease  as character no-undo.
define variable cSrcDBName   as character no-undo.

run consrcdb.p
   (output cSrcDBName).
   
cSrcRelease = getSourceDBVersion().

run initselcv.p.

run genini.p persistent set hGenIni.

loadIniFile("specdump.ini", hGenIni).

create temp-table hTempTable.

run moveTempTable in hGenIni
   (input "specdump",
    input hTempTable).

delete procedure hGenIni.

hReleases = hTempTable:default-buffer-handle.
create buffer hTables for table hReleases buffer-name "Tables".
create buffer hProgram for table hReleases buffer-name "Program".

empty temp-table dumpprogs.

create query hqDumps.
hqDumps:set-buffers(hReleases, hProgram, hTables).
hqDumps:query-prepare(
   "for each specdump where ItemName = " + quoter("Releases") +
   ", first Program outer-join where Program.Section = specdump.Section" +
                              " and Program.ItemName = " + quoter("Program") +
   ", first Tables outer-join where Tables.Section = specdump.Section" +
                              " and Tables.ItemName = " + quoter("Tables") ).
hqDumps:query-open().
hqDumps:get-first().

hfSection = hReleases:buffer-field("Section").
hfReleases = hReleases:buffer-field("ItemValue").
hfProgram = hProgram:buffer-field("ItemValue").
hfTables = hTables:buffer-field("ItemValue").
do while not hqDumps:query-off-end:
   cItemValue = hfReleases:buffer-value.
   idx = lookup(cSrcRelease, cItemValue).
   if idx <> 0 then do:
      /* Found release, add section to list. */
      create dumpprogs.
      assign dumpname = hfSection:buffer-value
             program = hfProgram:buffer-value
             dodump = yes
             tables = (if hfTables:buffer-value = ? then "" else hfTables:buffer-value).
   end.
   hqDumps:get-next().
end.

hqDumps:query-close().

delete object hqDumps.
delete object hProgram.
delete object hTables.
delete object hTempTable.

publish "LogEnable".

for each dumpprogs while not lGotCancel:
   do idx = 1 to num-entries(dumpprogs.tables):
      if not tableExists(cSrcDBName + "." + entry(idx, dumpprogs.tables)) then do:
         /* Have to make an exception for European Accounting         */
         /* process svpea.p and admin tables that need to be dumped   */
         /* even though tables not present (yet).  These dumps have   */
         /* the name of the associated conversion in the table field. */
         if index(dumpprogs.tables, "[") = 0 then do:
            publish "LogLog"
               (input substitute("Skipping &1 dump, &2 not present.",
                                 dumpprogs.dumpname,
                                 entry(idx, dumpprogs.tables))).
            dumpprogs.dodump = no.
         end.
         else do:
            /* If user did not select the associated conversion then  */
            /* don't do the dump.                                     */
            find first tvprogs where tvprogs.convname = dumpprogs.tables
               no-error.
            if not available(tvprogs) or not tvprogs.doconv then do:
               dumpprogs.dodump = no.
               publish "LogLog"
                  (input substitute("Skipping &1 dump, &2 not selected.",
                                    dumpprogs.dumpname,
                                    dumpprogs.tables)).
            end.
         end.
      end.
   end.
   if dumpprogs.dodump and not lGotCancel then do:
      if absPath(dumpprogs.program) <> ? then do:
         publish "LogDisp"
            (input substitute("Running &1 special dump.",
                              dumpprogs.program)).
         run value(dumpprogs.program).
      end.
      else do:
         publish "LogDisp"
            (input dumpprogs.program + " was not found.").
      end.
   end.
end.

end PROCEDURE.

PROCEDURE deleteEnvVar:
/*----------------------------------------------------------------------------
    Purpose: Deletes the DT2 config records in the envars temp-table.

 Input Parameters:  pcEnvIni  - name of the ini file.


      Notes:
 -----------------------------------------------------------------------------*/
   define input parameter pcEnvIni as character no-undo.

  /* Now delete the ini file since it is no longer needed. */
   if absPath(pcEnvIni) <> ? and file-info:full-pathname <> "" then do:
       os-delete value (file-info:full-pathname).
   end.

end PROCEDURE.

PROCEDURE getEnvVars:
/*------------------------------------------------------------------------
   Procedure - getEnvVars
   Input Parameters: pcEnvIni    - the name of the temp table.
                     phTempTable - handle to the temp table to build.

   Purpose: Reads the envvars.ini file into a temp table.
--------------------------------------------------------------------------*/
   define input parameter pcEnvIni as character no-undo.
   define input parameter phTempTable as handle no-undo.

   define variable hGenIni as handle no-undo.

   run genini.p persistent set hGenIni.

   makeOrLoadIniFile(pcEnvIni, hGenIni).

   run moveTempTable in hGenIni
      (input "envvars",
       input phTempTable).

   delete procedure hGenIni.

   return.

end PROCEDURE.

PROCEDURE setEnvVars:
/*------------------------------------------------------------------------
   Procedure - setEnvVars
   Input Parameters: pcEnvIni - name of environment file.
                     phTempTable - handle to the temp table buffer to write.

   Purpose: Writes the temp table into envvars.ini file.
--------------------------------------------------------------------------*/
   define input parameter pcEnvIni as character no-undo.
   define input parameter phTempTable as handle no-undo.

   define variable hGenIni as handle no-undo.

   run genini.p persistent set hGenIni.

   run writeIni in hGenIni
      (input phTempTable,
       input pcEnvIni).

   delete procedure hGenIni.
   return.

end PROCEDURE.


PROCEDURE updateEnvVar:
/*----------------------------------------------------------------------------
    Purpose: Update a record to the conv temp-table.  If it doesn't exist add it.

 Input Parameters:  hTable     - the handle to the buffer.
                    c-section  - name of the section.
                    c-name     - name of the value.
                    c-value    - value field.

      Notes:
 -----------------------------------------------------------------------------*/
   define input parameter hTable as handle no-undo.
   define input parameter c-section as character no-undo.
   define input parameter c-name as character no-undo.
   define input parameter c-value as character no-undo.

   define variable hfSection as handle no-undo.
   define variable hfItemName as handle no-undo.
   define variable hfItemValue as handle no-undo.
   define variable hfWholeLine as handle no-undo.

   hfSection = hTable:buffer-field("Section").
   hfItemName = hTable:buffer-field("ItemName").
   hfItemValue = hTable:buffer-field("ItemValue").
   hfWholeLine = hTable:buffer-field("WholeLine").

   hTable:find-first("where Section = " + quoter(c-section) +
                     " and ItemName = " + quoter(c-name)) no-error.
   if hTable:available then do:
      assign
         hfItemValue:buffer-value = c-value
         hfWholeLine:buffer-value = c-name + "=" + c-value.
   end.
   else do:
      run addEnvVar
         (input hTable,
          input c-section,
          input c-name,
          input c-value).
   end.
   /* Update the conversion container. */
   if c-section = "[environment]" then do:
      run setQDTItem(gcConvContainer, c-name, c-value).
   end.

end PROCEDURE.

PROCEDURE showOneEnvVar:
/*----------------------------------------------------------------------------
    Purpose: Show the value of an environment variable in the log window.

 Input Parameters:  hTable     - handle to the buffer.
                    c-section  - name of the section.
                    c-name     - name of the value.
                    c-value    - value field.

      Notes:
 -----------------------------------------------------------------------------*/
   define input parameter hTable as handle no-undo.
   define input parameter c-section as character no-undo.
   define input parameter c-name as character no-undo.
   define input parameter c-value as character no-undo.

   define variable hqVar as handle no-undo.
   define variable hfSection as handle no-undo.
   define variable hfItemName as handle no-undo.
   define variable hfItemValue as handle no-undo.

   hfSection = hTable:buffer-field("Section").
   hfItemName = hTable:buffer-field("ItemName").
   hfItemValue = hTable:buffer-field("ItemValue").

   create query hqVar.
   hqVar:set-buffers(hTable).
   hqVar:query-prepare("for each " + hTable:name +
                       " where Section = " + quoter(c-section) +
                       " and ItemName = " + quoter(c-name)).
   hqVar:query-open().
   hqVar:get-first().
   if hTable:available then do:
      publish "LogMsg"
          (input hfSection:buffer-value + " " + hfItemName:buffer-value +
                 "=" + hfItemValue:buffer-value).
   end.
   else do:
      publish "LogMsg"
          (input c-section + " " + c-name +
                 " was not found.").
   end.
   hqVar:query-close().
   delete object hqVar.

end PROCEDURE.

PROCEDURE showEnvVars:
/*----------------------------------------------------------------------------
    Purpose: Display the envvars.ini temp-table in the log window.

 Input Parameters:  hTable - the handle of the buffer.

      Notes:
 -----------------------------------------------------------------------------*/
   define input parameter hTable as handle no-undo.

   define variable hqVar as handle no-undo.
   define variable hfSection as handle no-undo.
   define variable hfItemName as handle no-undo.
   define variable hfItemValue as handle no-undo.

   hfSection = hTable:buffer-field("Section").
   hfItemName = hTable:buffer-field("ItemName").
   hfItemValue = hTable:buffer-field("ItemValue").

   create query hqVar.
   hqVar:set-buffers(hTable).
   hqVar:query-prepare("for each " + hTable:name).
   hqVar:query-open().
   hqVar:get-first().
   do while hTable:available :
      publish "LogMsg"
          (input hfSection:buffer-value + " " + hfItemName:buffer-value +
                 "=" + hfItemValue:buffer-value).
      hqVar:get-next().
   end.
   hqVar:query-close().
   delete object hqVar.

end PROCEDURE.

PROCEDURE getOneIniVar:
/*----------------------------------------------------------------------------
    Purpose: Read a value from the ini file.

 Input Parameters:  hTable     - handle to the buffer.
                    c-section  - name of the section.
                    c-name     - name of the value.
 Output Parameters: c-value    - value field.

      Notes:
 -----------------------------------------------------------------------------*/
   define input parameter hTable as handle no-undo.
   define input parameter c-section as character no-undo.
   define input parameter c-name as character no-undo.
   define output parameter c-value as character no-undo.

   define variable hqVar as handle no-undo.
   define variable hfSection as handle no-undo.
   define variable hfItemName as handle no-undo.
   define variable hfItemValue as handle no-undo.

   hfSection = hTable:buffer-field("Section").
   hfItemName = hTable:buffer-field("ItemName").
   hfItemValue = hTable:buffer-field("ItemValue").

   create query hqVar.
   hqVar:set-buffers(hTable).
   hqVar:query-prepare("for each " + hTable:name +
                       " where Section = " + quoter(c-section) +
                       " and ItemName = " + quoter(c-name)).
   hqVar:query-open().
   hqVar:get-first().
   if hTable:available then do:
      c-value = hfItemValue:buffer-value.
   end.
   hqVar:query-close().
   delete object hqVar.

end PROCEDURE.

PROCEDURE addEnvVar:
/*----------------------------------------------------------------------------
    Purpose: Add a record to a temp-table.  Figure out the correct
             sequence number combination.

 Input Parameters:  hTable     - handle to the buffer.
                    c-section  - name of the section.
                    c-name - name of the program.
                    c-value    - value field.

      Notes:
 -----------------------------------------------------------------------------*/
   define input parameter hTable as handle no-undo.
   define input parameter c-section as character no-undo.
   define input parameter c-name as character no-undo.
   define input parameter c-value as character no-undo.

   define variable i-new-seqnum as integer no-undo.
   define variable i-section-num as integer no-undo.
   define variable hfSection as handle no-undo.
   define variable hfItemName as handle no-undo.
   define variable hfItemValue as handle no-undo.
   define variable hfLineNum as handle no-undo.

   hfSection = hTable:buffer-field("Section").
   hfItemName = hTable:buffer-field("ItemName").
   hfItemValue = hTable:buffer-field("ItemValue").
   hfLineNum = hTable:buffer-field("LineNum").

   hTable:find-first("where WholeLine = " + quoter(c-section) +
                     " and Comment") no-error.
   if not hTable:available then do:
      /* Create a new section, this one doesn't exist. */
      hTable:find-last("use-index LineNum") no-error.
      if hTable:available then do:
         i-new-seqnum = hfLineNum:buffer-value + 1.
         i-section-num = i-new-seqnum.
      end.
      else do:
         i-new-seqnum = 1.
         i-section-num = 1.
      end.

      run insertEnvVar
         (input hTable,
          input c-section,
          input "",
          input "",
          input c-section,
          input yes,
          input i-new-seqnum,
          input i-section-num).
   end.
   else do:
      i-section-num = hTable:buffer-field("SecLineNum"):buffer-value.
   end.

   hTable:find-last("where Section = " + quoter(c-section) +
                    " and not Comment use-index LineNum") no-error.
   if hTable:available then do:
      i-new-seqnum = hfLineNum:buffer-value + 1.
   end.
   else do:
      i-new-seqnum = i-section-num + 1.
   end.

   run insertEnvVar
      (input hTable,
       input c-section,
       input c-name,
       input c-value,
       input c-name + '=' + c-value,
       input no,
       input i-new-seqnum,
       input i-section-num).

   return.

end PROCEDURE.

PROCEDURE insertEnvVar:
/*----------------------------------------------------------------------------
    Purpose: Add a new record to ini table.

 Input Parameters: hTable     - handle of the buffer
                   c-section  - name of the section.
                   c-name     - name of the item.
                   c-value    - value.
                   c-whole-line - the raw input line of the ini file.
                   l-comment  - logical, yes if line is a comment.
                   i-seqnum   - integer, the new sequence number.
                   i-section  - integer, the section number.

      Notes:
 -----------------------------------------------------------------------------*/
   define input parameter hTable as handle no-undo.
   define input parameter c-section as character no-undo.
   define input parameter c-name as character no-undo.
   define input parameter c-value as character no-undo.
   define input parameter c-whole-line as character no-undo.
   define input parameter l-comment as logical no-undo.
   define input parameter i-seqnum as integer no-undo.
   define input parameter i-section as integer no-undo.

   define variable hfSection as handle no-undo.
   define variable hfItemName as handle no-undo.
   define variable hfItemValue as handle no-undo.
   define variable hfLineNum as handle no-undo.
   define variable hfSecLineNum as handle no-undo.
   define variable hfComment as handle no-undo.
   define variable hfWholeLine as handle no-undo.

   hfSection = hTable:buffer-field("Section").
   hfItemName = hTable:buffer-field("ItemName").
   hfItemValue = hTable:buffer-field("ItemValue").
   hfLineNum = hTable:buffer-field("LineNum").
   hfSecLineNum = hTable:buffer-field("SecLineNum").
   hfComment = hTable:buffer-field("Comment").
   hfWholeLine = hTable:buffer-field("WholeLine").

   /* The destination may be an already existing blank or comment line. */
   /* If it is, just replace it.  Otherwise, create a new line.         */
   hTable:find-first
      ("where SecLineNum = " + quoter(i-section) +
       " and LineNum = " + quoter(i-seqnum) +
       " and Comment") no-error.
   if not hTable:available then do:
      hTable:buffer-create().
   end.

   assign
      hfSection:buffer-value    = c-section
      hfItemName:buffer-value   = c-name
      hfItemValue:buffer-value  = c-value
      hfWholeLine:buffer-value  = c-whole-line
      hfComment:buffer-value    = l-comment
      hfLineNum:buffer-value    = i-seqnum
      hfSecLineNum:buffer-value = i-section.

   return.

end PROCEDURE.

PROCEDURE setSourceDBVersion:
/*------------------------------------------------------------------------
   Procedure - setSourceDBVersion
   Parameters: input - pcVersion

   Purpose: Sets the version of the source database.
--------------------------------------------------------------------------*/
   define input parameter pcSourceDBVersion as character no-undo.

   cSourceDBVersion = pcSourceDBVersion.
   run setQDTItem(getConvContainer(), "{&ITEM-SOURCEDBVER}",  cSourceDBVersion).

end PROCEDURE.


PROCEDURE setTargetDBVersion:
/*------------------------------------------------------------------------
   Procedure - setTargetDBVersion
   Parameters: input - pcVersion

   Purpose: Sets the version of the source database.
--------------------------------------------------------------------------*/
   define input parameter pcTargetDBVersion as character no-undo.

   cTargetDBVersion = pcTargetDBVersion.
   run setQDTItem(gcConvContainer, "{&ITEM-TARGETDBVER}",  cTargetDBVersion).

end PROCEDURE.

PROCEDURE getDBDefaults:
/*------------------------------------------------------------------------
   Procedure - getDBDefaults
   Parameters: input - cDBName
               input-output - p_PName
                              p_LName
                              p_Type
                              p_Multi_User
                              p_Network
                              p_Host_Name
                              p_Service_Name
                              p_UserId
                              p_Password
                              p_Trig_Loc
                              p_Parm_File
                              p_Unix_Parms

   Purpose: Reads the ini file with the conversion db values and returns them.
--------------------------------------------------------------------------*/
   define input        parameter cDBName        as character no-undo.
   define input-output parameter p_PName        as character no-undo.
   define input-output parameter p_LName        as character no-undo.
   define input-output parameter p_Type         as character no-undo.
   define input-output parameter p_Multi_User   as logical   no-undo.
   define input-output parameter p_Network      as character no-undo.
   define input-output parameter p_Host_Name    as character no-undo.
   define input-output parameter p_Service_Name as character no-undo.
   define input-output parameter p_UserId       as character no-undo.
   define input-output parameter p_Password     as character no-undo.
   define input-output parameter p_Trig_Loc     as character no-undo.
   define input-output parameter p_Parm_File    as character no-undo.
   define input-output parameter p_Unix_Parms   as character no-undo.

   define variable hGenIni as handle no-undo.
   define variable hTempTable as handle no-undo.
   define variable hTable as handle no-undo.
   define variable hqArgs as handle no-undo.
   define variable hfSection as handle no-undo.
   define variable hfItemName as handle no-undo.
   define variable hfItemValue as handle no-undo.
   define variable hfArgs as handle no-undo.
   define variable cItemName as character no-undo.

   if cDBName = "" then return.

   run genini.p persistent set hGenIni.

   makeOrLoadIniFile(cDBName + ".ini", hGenIni).

   create temp-table hTempTable.

   run moveTempTable in hGenIni
      (input cDBName,
       input hTempTable).

   /* Get buffer object handle for temp-table */

   hTable = hTempTable:default-buffer-handle.

   create query hqArgs.
   hqArgs:set-buffers(hTable).
   hqArgs:query-prepare("for each " + hTable:name + " where not Comment").
   hqArgs:query-open().
   hqArgs:get-first().
   hfSection = hTable:buffer-field("Section").
   hfItemName = hTable:buffer-field("ItemName").
   hfItemValue = hTable:buffer-field("ItemValue").
   do while not hqArgs:query-off-end:
      cItemName = hfItemName:buffer-value.
      case cItemName:
         when "Database" then do:
            p_PName = hfItemValue:buffer-value.
         end.
         when "LogicalDB" then do:
            p_LName = hfItemValue:buffer-value.
         end.
         when "DatabaseType" then do:
            p_Type = hfItemValue:buffer-value.
         end.
         when "MultiUser" then do:
            p_Multi_User = (hfItemValue:buffer-value = "yes").
         end.
         when "Network" then do:
            p_Network = hfItemValue:buffer-value.
         end.
         when "Host" then do:
            p_Host_Name = hfItemValue:buffer-value.
         end.
         when "Service" then do:
            p_Service_Name = hfItemValue:buffer-value.
         end.
         when "UserID" then do:
            P_UserId = hfItemValue:buffer-value.
         end.
         when "Password" then do:
            if hfItemValue:buffer-value = "" then
               p_Password = hfItemValue:buffer-value.
            else do:
               p_Password = decryptString
                  (input hfItemValue:buffer-value).
            end.
         end.
         when "TrigLoc" then do:
            p_Trig_Loc = hfItemValue:buffer-value.
         end.
         when "ParmFile" then do:
            p_Parm_File = hfItemValue:buffer-value.
         end.
         when "OtherParms" then do:
            p_Unix_Parms = hfItemValue:buffer-value.
         end.
      end case.
      hqArgs:get-next().
   end.
   hqArgs:query-close().
   delete procedure hGenIni.
   delete object hTempTable.
   delete object hqArgs.

   return.

end PROCEDURE.

PROCEDURE setDBDefaults:
/*------------------------------------------------------------------------
   Procedure - setDBDefaults
   Parameters: input - cDBName
               input-output - p_PName
                              p_LName
                              p_Type
                              p_Multi_User
                              p_Network
                              p_Host_Name
                              p_Service_Name
                              p_UserId
                              p_Password
                              p_Trig_Loc
                              p_Parm_File
                              p_Unix_Parms

   Purpose: Updates the temp table with the values passed and then writes
            the temp-table.
--------------------------------------------------------------------------*/
   define input        parameter cDBName        as character no-undo.
   define input-output parameter p_PName        as character no-undo.
   define input-output parameter p_LName        as character no-undo.
   define input-output parameter p_Type         as character no-undo.
   define input-output parameter p_Multi_User   as logical   no-undo.
   define input-output parameter p_Network      as character no-undo.
   define input-output parameter p_Host_Name    as character no-undo.
   define input-output parameter p_Service_Name as character no-undo.
   define input-output parameter p_UserId       as character no-undo.
   define input-output parameter p_Password     as character no-undo.
   define input-output parameter p_Trig_Loc     as character no-undo.
   define input-output parameter p_Parm_File    as character no-undo.
   define input-output parameter p_Unix_Parms   as character no-undo.

   define variable hGenIni     as handle    no-undo.
   define variable hTempTable  as handle    no-undo.
   define variable hTable      as handle    no-undo.
   define variable hqArgs      as handle    no-undo.
   define variable hfSection   as handle    no-undo.
   define variable hfItemName  as handle    no-undo.
   define variable hfItemValue as handle    no-undo.
   define variable hfArgs      as handle    no-undo.
   define variable cItemName   as character no-undo.
   define variable idx         as integer   no-undo.
   define variable cPassTemp   as character no-undo.

   if cDBName = "" then return.

   run genini.p persistent set hGenIni.

   makeOrLoadIniFile(cDBName + ".ini", hGenIni).

   create temp-table hTempTable.
   run moveTempTable in hGenIni
      (input cDBName,
       input hTempTable).

   /* Get buffer object handle for temp-table */

   hTable = hTempTable:default-buffer-handle.

   if not hTempTable:has-records then do:
      publish "LogLog"
         (input substitute("Creating new connection file: &1.ini.",
                           cDBName)).
      run createMT
         (input cDBName,
          input hTable).
   end.
   hfSection = hTable:buffer-field("Section").
   hfItemName = hTable:buffer-field("ItemName").
   hfItemValue = hTable:buffer-field("ItemValue").
   create query hqArgs.
   hqArgs:set-buffers(hTable).
   hqArgs:query-prepare("for each " + hTable:name + " where not Comment").
   hqArgs:query-open().
   hqArgs:get-first().
   do while hTable:available:
      cItemName = hfItemName:buffer-value.
      case cItemName :
         when "Database" then do:
            hfItemValue:buffer-value = p_PName.
         end.
         when "LogicalDB" then do:
            hfItemValue:buffer-value = p_LName.
         end.
         when "DatabaseType" then do:
            hfItemValue:buffer-value = p_Type.
         end.
         when "MultiUser" then do:
            hfItemValue:buffer-value = (if p_Multi_User then "Yes" else "No").
         end.
         when "Network" then do:
            hfItemValue:buffer-value = (if p_Network = "(None)" then "" else p_Network).
         end.
         when "Host" then do:
            hfItemValue:buffer-value = p_Host_Name.
         end.
         when "Service" then do:
            hfItemValue:buffer-value = p_Service_Name.
         end.
         when "UserID" then do:
            hfItemValue:buffer-value = P_UserId.
         end.
         when "Password" then do:
            if p_Password = "" then
               hfItemValue:buffer-value = p_Password.
            else do:
               cPassTemp = encryptString
                  (input p_Password).
                if cPassTemp = ? then cPassTemp = "?".
                hfItemValue:buffer-value = cPassTemp.
            end.
         end.
         when "TrigLoc" then do:
            hfItemValue:buffer-value = p_Trig_Loc.
         end.
         when "ParmFile" then do:
            hfItemValue:buffer-value = p_Parm_File.
         end.
         when "OtherParms" then do:
            hfItemValue:buffer-value = p_Unix_Parms.
         end.
      end case.
      hTable:buffer-field("WholeLine"):buffer-value =
         hfItemName:buffer-value + "=" + hfItemValue:buffer-value.
      hqArgs:get-next().
   end.

   hqArgs:query-close().

   run writeIni in hGenIni
      (input hTempTable:default-buffer-handle,
       input cDBName + ".ini").

   delete procedure hGenIni.
   delete object hTempTable.
   delete object hqArgs.

   return.

end PROCEDURE.

PROCEDURE OIDSetup :
/*------------------------------------------------------------------------
   Procedure - OIDSetup
   Parameters: <None>

   Purpose: Prepares for OID generation by setting the name of the
            procedure that will get the OID value.
   NOTE: This no longer loads a persistant procedure.  All external
         functionality has been brought inside convert.p. (Thanks to 
         being able to rely on dynamic features).  Now it just enables
         OID generation.
--------------------------------------------------------------------------*/
   define variable i                 as integer   no-undo.
   define variable j                 as integer   no-undo.
   define variable cNonProgressDBIdx as character no-undo.

   /* If target is connected use it. */
   if isTargetConnected() then do:
      cSeqDBName = cTargetDB.
      ldoOIDGen  = yes.
   end.
   else do:
      /* Otherwise, use the first database that owns the sequence */
      cNonProgressDBIdx = ''.
      do i = 1 to num-dbs:
         if sdbname(i) <> ldbname(i) then do:
            cNonProgressDBIdx = cNonProgressDBIdx + ',' + string(i).
         end.
      end.
      cNonProgressDBIdx = substring(cNonProgressDBIdx, 2, -1). 

      /* check sequence existence using schema holder's logical name,
       * access the sequence value using non-Progress db logical name
       */ 
      if num-entries(cNonProgressDBIdx) > 0 then do:
         do i = 1 to num-entries(cNonProgressDBIdx):
            j = integer(entry(i, cNonProgressDBIdx)).
            if sequenceExists(substitute("&1.qaddb_oid_sq01",
                                         sdbname(j)))  <> "" then do:
               cSeqDBName = ldbname(j).
               ldoOIDGen  = yes.
               leave.
            end.
         end.
      end.
      else do:
         dbLoop:
         do i = 1 to num-dbs:
            if sequenceExists(substitute("&1.qaddb_oid_sq01",
                                         ldbname(i)))  <> "" then do:
               cSeqDBName = ldbname(i).
               ldoOIDGen  = yes.
               leave dbLoop.
            end.
         end.
      end.
   end.

end PROCEDURE.


PROCEDURE OIDUnload :
/*------------------------------------------------------------------------
   Procedure - OIDUnload
   Parameters: <None>

   Purpose: Unloads the OID generation procedure if it is loaded.
   NOTE: This no longer unloads a procedure.  It simply "turns off"
         the ability to get oids.
--------------------------------------------------------------------------*/
define variable hConvert as handle    no-undo.
define variable idx      as integer   no-undo.
define variable cSupers  as character no-undo.

   assign
      ldoOIDGen   = no
      cSeqDBName  = ?.

end PROCEDURE.


PROCEDURE createMT :
/*------------------------------------------------------------------------
   Procedure - creatMT
   Parameters: Input - pcDBName - the logical name of the database.

   Purpose: Creates an empty temp table for the database connection info.
--------------------------------------------------------------------------*/
   define input parameter pcDBName as character no-undo.
   define input parameter hTable as handle no-undo.

   define variable cItemName    as character no-undo.
   define variable iline        as integer   no-undo.
   define variable hfSection    as handle    no-undo.
   define variable hfItemName   as handle    no-undo.
   define variable hfItemValue  as handle    no-undo.
   define variable hfComment    as handle    no-undo.
   define variable hfWholeLine  as handle    no-undo.
   define variable hfLineNum    as handle    no-undo.
   define variable hfSecLineNum as handle    no-undo.
   define variable cParmList    as character no-undo
      initial "Database,LogicalDB,DatabaseType,MultiUser,Network,Host,Service,UserID,Password,TrigLoc,ParmFile,OtherParms".

   assign
      hfSection   = hTable:buffer-field("Section")
      hfItemName  = hTable:buffer-field("ItemName")
      hfItemValue = hTable:buffer-field("ItemValue")
      hfComment   = hTable:buffer-field("Comment")
      hfWholeLine = hTable:buffer-field("WholeLine")
      hfLineNum   = hTable:buffer-field("LineNum")
      hfSecLineNum = hTable:buffer-field("SecLineNum").

   iline = 1.
   do transaction:
      hTable:buffer-create().
      assign
         hfSection:buffer-value    = "[" + pcDBName + "]"
         hfItemName:buffer-value   = ""
         hfItemValue:buffer-value  = ""
         hfComment:buffer-value    = yes
         hfWholeLine:buffer-value  = "[" + pcDBName + "]"
         hfLineNum:buffer-value    = iLine
         hfSecLineNum:buffer-value = iLine.
      hTable:buffer-release().
   end.

   do idx = 1 to num-entries(cParmList):
      assign
         iLine = iLine + 1
         cItemName = entry(idx,cParmList).
      hTable:buffer-create().
      assign
         hfSection:buffer-value    = "[" + pcDBName + "]"
         hfItemName:buffer-value   = cItemName
         hfItemValue:buffer-value  = ""
         hfComment:buffer-value    = no
         hfWholeLine:buffer-value  = cItemName + "="
         hfLineNum:buffer-value    = iLine
         hfSecLineNum:buffer-value = 1.
      hTable:buffer-release().

   end.

end PROCEDURE.


PROCEDURE getDBVer :
/*------------------------------------------------------------------------
   Procedure - getDBVer
   Parameters: Input - pcDBName - the logical name of the database.
              Output - pcDBVer    - the database version.

   Purpose: Returns the version and type of the named database.
--------------------------------------------------------------------------*/
   define input parameter pcDBName as character no-undo.
   define output parameter pcDBVer as character no-undo.

   define variable hDBVers as handle no-undo.
   define variable hTable as handle no-undo.
   define variable hqDBVers as handle no-undo.
   define variable hfItemName as handle no-undo.
   define variable hfItemValue as handle no-undo.

   define variable hbSource as handle no-undo.
   define variable hbField  as handle no-undo.
   define variable cSection as character no-undo.
   define variable lTableFound as logical no-undo.
   define variable cTablename as character no-undo.
   define variable cFieldname as character no-undo.
   define variable cIndexname as character no-undo.
   define variable cLogDBName  as character no-undo.

   /* Read the ini file and build the temp-table. */
   create temp-table hDBVers.
   run readDBVers
      (input hDBVers).

   hTable = hDBVers:default-buffer-handle.
   cLogDBName = sdbname(pcDBName).

   create query hqDBVers.

   create buffer hbSource for table cLogDBName + "._file" no-error.

   if valid-handle(hbSource) then do:
      hbSource:find-first("where _file-name = " + quoter("gl_ctrl"), no-lock) no-error.
      if hbSource:available then
         cSection = "[qaddb]".
      else do:
         hbSource:find-first("where _file-name = " + quoter("drl_mstr"), no-lock) no-error.
         if hbSource:available then
            cSection = "[admin]".
         else
            cSection = "[help]".
      end.
   end.
   hqDBVers:set-buffers(hTable).
   hqDBVers:query-prepare("for each " + hTable:name +
                          " where not Comment and section = " +
                          quoter(cSection)).
   hqDBVers:query-open().
   hqDBVers:get-first().
   hfItemValue = hTable:buffer-field("ItemValue").
   hfItemName = hTable:buffer-field("ItemName").
   lTableFound = true.
   do while lTableFound and not hqDBVers:query-off-end :

      /* This may be the one, set the version. */
      pcDBVer = hfItemName:buffer-value.
      if hfItemValue:buffer-value = "" then do:
         if pcDBVer = "73" then do:
            create buffer hbField for table cLogDBName + "._field" no-error.
            if valid-handle(hbField) then do:
               hbField:find-first("where _field-name = " +
                                  quoter("gl_frtacc_acct"), no-lock) no-error.
               if hbField:available then do:
                  lTableFound =
                     (hbField:buffer-field("_valexp"):buffer-value =
                      "~{gpglac.v gl_frtacc_acct~}").
               end.
               delete object hbField.
            end.
         end.
         else do:
            /* If the table name is blank and the DB version isn't 7.3 then   */
            /* we're at the end and the database must be the current version. */
            /* So we set the end condition and just let the loop end with the */
            /* DB set to the latest version. */
            lTableFound = false.
         end.
      end. /* if hfItemValue:buffer-value */
      else if INDEX(string(hfItemValue:buffer-value), "table__") > 0 then do:
         /* Check if its a field or index */
         if INDEX(string(hfItemValue:buffer-value), "__field__") > 0 then do:
            run parseDBVersIniTableData(input hfItemValue:buffer-value,
                                        output cTablename,
                                        output cFieldname).

            if doesFieldExist (input cTableName, input cFieldName, input "") = false then do:
               lTableFound = false.
            end.
         end. /* if INDEX(pcDbVersStringValue, "__Field__") */
         /* if its not a field i.e. _field_, then assume its an index */
         else if INDEX(string(hfItemValue:buffer-value), "__index__") > 0 then do:
            run parseDBVersIniTableData(input hfItemValue:buffer-value,
                                        output cTablename,
                                        output cIndexname).

            if doesIndexExist (input cTableName, input cIndexName, input "") = false then do:
               lTableFound = false.
	        end.
         end. /* else if INDEX(string(hfItemValue:buffer-value), "__index__") */
         else do:
            publish "LogLog" (INPUT "Invalid string format in dbvers.ini: " + string(hfItemValue:buffer-value)).
            publish "LogLog" (INPUT "Allowed formats are:").
            publish "LogLog" (INPUT "    table__<tablename>__field__<fieldname>").
            publish "LogLog" (INPUT " or table__<tablename>__index__<indexname>").
         end.
      end. /* else if INDEX(string(hfItemValue:buffer-value), "table__") */
      else do:
         hbSource:find-first("where _file-name = " +
                             quoter(hfItemValue:buffer-value), no-lock) no-error.
         if not hbSource:available then do:
            lTableFound = false.
         end.
      end. /* else do */
      hqDBVers:get-next().
   end.
   hqDBVers:query-close().
   if valid-handle(hbSource) then
      delete object hbSource.
   delete object hqDBVers.
   delete object hDBVers.

   return.

end PROCEDURE.


PROCEDURE readDBVers :
/*------------------------------------------------------------------------
   Procedure - readDBVers
   Input Parameters: hDBVers -  Table handle of temp table to build.

   Purpose: Reads the dbvers.ini file and build the temp-table of versions
            and table names used to determine the version of a database.
--------------------------------------------------------------------------*/
   define input parameter hDBVers as handle no-undo.

   define variable hGenIni as handle no-undo.

   run genini.p persistent set hGenIni.

   loadIniFile("dbvers.ini", hGenIni).

   run moveTempTable in hGenIni
      (input "MFGDBVers",
       input hDBVers).

   delete procedure hGenIni.

   return.

end PROCEDURE.

PROCEDURE setTargetDB:
/* ---------------------------------------------------------------------- */
/* Sets the name of the target database for use in the package.           */
/*                                                                        */
/*  Input parameters: pcLTargetDB - the logical name of the target db.    */
/*                                                                        */
/* Output parameters: <None>                                              */
/*                                                                        */
/* ---------------------------------------------------------------------- */
   define input parameter pcLTargetDB as character no-undo.

   assign
      cTargetDB     = pcLTargetDB.

end PROCEDURE.


PROCEDURE setSourceDB:
/* ---------------------------------------------------------------------- */
/* Sets the name of the source database for use in the package.           */
/*                                                                        */
/*  Input parameters: pcDBName - the name of the source database.       */
/*                                                                        */
/* Output parameters: <None>                                              */
/*                                                                        */
/* ---------------------------------------------------------------------- */
   define input parameter pcLDBName as character no-undo.

   assign
      cSourceDB     = pcLDBName.

end PROCEDURE.


PROCEDURE setAdminTargetDB:
/* ---------------------------------------------------------------------- */
/* Sets the name of the admin target database for use in the package.     */
/*                                                                        */
/*  Input parameters: pcTargetDB - the name of the target database.       */
/*                                                                        */
/* Output parameters: <None>                                              */
/*                                                                        */
/* ---------------------------------------------------------------------- */
   define input parameter pcTargetDB as character no-undo.

   cAdminTargetDB = pcTargetDB.

end PROCEDURE.


PROCEDURE setAdminSourceDB:
/* ---------------------------------------------------------------------- */
/* Sets the name of the admin source database for use in the package.     */
/*                                                                        */
/*  Input parameters: pcDBName - the name of the source database.       */
/*                                                                        */
/* Output parameters: <None>                                              */
/*                                                                        */
/* ---------------------------------------------------------------------- */
   define input parameter pcDBName as character no-undo.

   cAdminSourceDB = pcDBName.

end PROCEDURE.


PROCEDURE setDomainValue:
/* ---------------------------------------------------------------------- */
/* Sets the value for the domain to be inserted in the new records.       */
/*                                                                        */
/*  Input parameters: pcDomainValue - The name of the domain.             */
/*                                                                        */
/* Output parameters: <None>                                              */
/*                                                                        */
/* ---------------------------------------------------------------------- */
   define input parameter pcDomainValue as character no-undo.

   if pcDomainValue <> cDomainValue then do:
      cDomainValue = pcDomainValue.
      run setQDTItem(gcConvContainer, "{&ITEM-CONVDOMAIN}",   cDomainValue).
/**
      run writeAttrValue
         (input "[environment]",
          input "CONVDOMAIN",
          input cDomainValue).
**/
   end.
end PROCEDURE.


PROCEDURE setConvType:
/* ---------------------------------------------------------------------- */
/* Sets the value for the conversion type (Oracle or Progress)            */
/*                                                                        */
/*  Input parameters: pcConvType - The type of conversion.                */
/*                                                                        */
/* Output parameters: <None>                                              */
/*                                                                        */
/* ---------------------------------------------------------------------- */
   define input parameter pcConvType as character no-undo.

   cConvType = pcConvType.

end PROCEDURE.


PROCEDURE getTargetDB:
/* ---------------------------------------------------------------------- */
/* Gets the name of the target database used in the package.              */
/*                                                                        */
/*  Input parameters: <None>                                              */
/*                                                                        */
/* Output parameters: pcTargetDB - the name of the target database.       */
/*                                                                        */
/* ---------------------------------------------------------------------- */
   define output parameter pcTargetDB as character no-undo.

   pcTargetDB = cTargetDB.

end PROCEDURE.


PROCEDURE getSourceDB:
/* ---------------------------------------------------------------------- */
/* Gets the name of the source database used in the package.              */
/*                                                                        */
/*  Input parameters: <None>                                              */
/*                                                                        */
/* Output parameters: pcDBName - the name of the source database.       */
/*                                                                        */
/* ---------------------------------------------------------------------- */
   define output parameter pcDBName as character no-undo.

   pcDBName = cSourceDB.

end PROCEDURE.


PROCEDURE getAdminTargetDB:
/* ---------------------------------------------------------------------- */
/* Gets the name of the admin target database used in the package.        */
/*                                                                        */
/*  Input parameters: <None>                                              */
/*                                                                        */
/* Output parameters: pcTargetDB - the name of the target database.       */
/*                                                                        */
/* ---------------------------------------------------------------------- */
   define output parameter pcAdminTargetDB as character no-undo.

   pcAdminTargetDB = cAdminTargetDB.

end PROCEDURE.


PROCEDURE getAdminSourceDB:
/* ---------------------------------------------------------------------- */
/* Gets the name of the admin source database used in the package.        */
/*                                                                        */
/*  Input parameters: <None>                                              */
/*                                                                        */
/* Output parameters: pcDBName - the name of the source database.       */
/*                                                                        */
/* ---------------------------------------------------------------------- */
   define output parameter pcDBName as character no-undo.

   pcDBName = cAdminSourceDB.

end PROCEDURE.


PROCEDURE getDomainValue:
/* ---------------------------------------------------------------------- */
/* Gets the value for the domain to be inserted in the new records.       */
/*                                                                        */
/*  Input parameters: <None>                                              */
/*                                                                        */
/* Output parameters: pcDomainValue - The name of the domain.             */
/*                                                                        */
/* ---------------------------------------------------------------------- */
   define output parameter pcDomainValue as character no-undo.

   if cDomainValue = "QAD" or
      cDomainValue = "" then do:
      /* Domain has not been set yet.                        */
      pcDomainValue = readDomainValue().

      if pcDomainValue = "QAD" or
         pcDomainValue = "" then do:
         /* Domain has still not been set yet.                        */
         /* We'll let this go now.  If the domain conversion is selected,
            it will be set.  If it's not, then the records being
            converted will already have a value and will not need
            to add the domain value.
         publish "LogDisp"
            (input "Conversion domain has not been selected.").
         publish "LogDisp"
            (input "Use Conversion/User Input of Conversion Values menu to").
         publish "LogDisp"
            (input "select conversion domain.").
         publish "LogDisp"
            (input "Cannot continue...").
         publish "LogDone".
         run runquit.
         */
      end.
      else do:
         run setDomainValue
            (input pcDomainValue).
      end.
   end.
   pcDomainValue = cDomainValue.

end PROCEDURE.


PROCEDURE getInPlace:
/* ---------------------------------------------------------------------- */
/* Gets the value for the In Place logical.                               */
/*                                                                        */
/*  Input parameters: <None>                                              */
/*                                                                        */
/* Output parameters: plInPlace - True if conversion is in place.         */
/*                                                                        */
/* ---------------------------------------------------------------------- */
   define output parameter plInPlace as logical no-undo.
   
   define variable cTmp   as character   no-undo.

   run getQDTItem(gcConvContainer, "{&ITEM-DBCONVERSIONTYPE}",  output cTmp).
   plInPlace = (cTmp = "{&ITEM-INPLACE}").

end PROCEDURE.


PROCEDURE getTargetBuffer:
/* ---------------------------------------------------------------------- */
/* Gets the handle for the target buffer.                            */
/*                                                                        */
/*  Input parameters: <None>                                              */
/*                                                                        */
/* Output parameters: phBuff - the handle for the target buffer.     */
/*                                                                        */
/* ---------------------------------------------------------------------- */
   define output parameter phBuff as handle no-undo.

   phBuff = hbTarget.

end PROCEDURE.


PROCEDURE getSourceBuffer:
/* ---------------------------------------------------------------------- */
/* Gets the handle for the source buffer.                                 */
/*                                                                        */
/*  Input parameters: <None>                                              */
/*                                                                        */
/* Output parameters: phBuff - the handle for the source buffer.          */
/*                                                                        */
/* ---------------------------------------------------------------------- */
   define output parameter phBuff as handle no-undo.

   phBuff = hbSource.

end PROCEDURE.


PROCEDURE copyTableSetup:
/* ---------------------------------------------------------------------- */
/* Sets up to iterate through a table.                                    */
/*                                                                        */
/*  Input parameters:                                                     */
/*    pcSourceTable  The name of the source table                         */
/*    pcTargetTable  The name of the target table                         */
/*    pcWhereClause  Where clause for query.                              */
/*                                                                        */
/* Output parameters: <none>                                              */
/*                                                                        */
/* Uses the package variables to define the database and table names.     */
/* ---------------------------------------------------------------------- */
   define  input parameter pcSourceTable as character no-undo.
   define  input parameter pcTargetTable   as character no-undo.
   define  input parameter pcWhereClause as character no-undo.

   define variable cTargetFileDB as character no-undo.
   define variable cSourceFileDB as character no-undo.

   cTargetFileDB = getDBDataName(getTrgDB()).
   cSourceFileDB = getDBDataName(getSrcDB()).

   cSourceTable = pcSourceTable.
   cTargetTable = pcTargetTable.
   cWhereClause = pcWhereClause.

   /* create dynamic objects based on input parameter */
   create buffer hbSource for table cSourceFileDB + "." + pcSourceTable.
   create buffer hbTarget for table cTargetFileDB + "." + pcTargetTable.

   cTargetPrefix = entry(1, pcTargetTable, "_").
   vhfTargetDomain = hbTarget:buffer-field(cTargetPrefix + "_domain") no-error.
   if not valid-handle(vhfTargetDomain) then
      vhfTargetDomain = vhfDummyDomain.
   
   hbSource:DISABLE-DUMP-TRIGGERS().
   hbTarget:DISABLE-LOAD-TRIGGERS(FALSE).
   
   create query hqSource.
   if pcWhereClause = "" then do:
      run buildWhere
         ( input hbSource,
          output pcWhereClause).
   end.

   /* make query ready for running */
   hqSource:set-buffers(hbSource).
   hqSource:query-prepare("for each " + pcSourceTable + " " + pcWhereClause ).
   hqSource:query-open().
   hqSource:get-first().

   return.

end PROCEDURE.

PROCEDURE convertTableSetup:
/* ---------------------------------------------------------------------- */
/* Sets up to iterate through a table in post process loop.               */
/*                                                                        */
/*  Input parameters:                                                     */
/*    pcTargetTable  The name of the target table.                        */
/*    pcWhereClause  Where clause for query.                              */
/*                                                                        */
/* Output parameters: <none>                                              */
/*                                                                        */
/* Uses the package variables to define the database and table names.     */
/* ---------------------------------------------------------------------- */
   define  input parameter pcTargetTable   as character no-undo.
   define  input parameter pcWhereClause as character no-undo.

   define variable cTargetFileDB as character no-undo.

   cTargetFileDB = getDBDataName(getTrgDB()).

   cTargetTable = pcTargetTable.
   cWhereClause = pcWhereClause.

   /* create dynamic objects based on input parameter */
   create buffer hbTarget for table cTargetFileDB + "." + pcTargetTable.
   cTargetPrefix = entry(1, pcTargetTable, "_").
   hbTarget:DISABLE-DUMP-TRIGGERS().
   hbTarget:DISABLE-LOAD-TRIGGERS(FALSE).

   create query hqTarget.

   if pcWhereClause = "" then do:
      run buildWhere
         ( input hbTarget,
          output pcWhereClause).
   end.

   /* make query ready for running */
   hqTarget:set-buffers(hbTarget).
   hqTarget:query-prepare("for each " + pcTargetTable + " " + pcWhereClause).
/*   message hqSource:index-information
      view-as alert-box information. */
   hqTarget:query-open().
   hqTarget:get-first().

   return.

end PROCEDURE.


PROCEDURE buildWhere:
/* ---------------------------------------------------------------------- */
/* Build the where clause for the primary unique key for the passed       */
/* buffer.                                                                */
/*                                                                        */
/*  Input parameters:                                                     */
/*    pbSource       Handle to the input buffer.                          */
/* Output parameters:                                                     */
/*    pcWhereClause  Where clause for primary unique key.                 */
/*                                                                        */
/* Uses the package variables to define the database and table names.     */
/* ---------------------------------------------------------------------- */
   define  input parameter pbSource as handle no-undo.
   define output parameter pcWhereClause as character no-undo.

   define variable cIndexInfo as character no-undo.
   define variable iIdx as integer no-undo.

   ASSIGN
      iIdx      = 0
      cIndexInfo = "".
 
   DO WHILE cIndexInfo <> ?:
      ASSIGN
        iIdx         = iIdx + 1
        cIndexInfo   = pbSource:INDEX-INFORMATION(iIdx).

      IF ENTRY(3,cIndexInfo) = "1" and
         ENTRY(2,cIndexInfo) = "1" then do:
         pcWhereClause = "use-index " + ENTRY(1, cIndexInfo).
         return.
      end.
          
    END. /* do while true */


end PROCEDURE.


PROCEDURE copyTable:
/* ---------------------------------------------------------------------- */
/* Copies a table from the source to the target and adds the domain  */
/* value specified.                                                       */
/*                                                                        */
/*  Input parameters:                                                     */
/*                                                                        */
/* Output parameters:                                                     */
/*    piRecCount     Count of records processed.                          */
/*    piElaspedTime  Elasped milliseconds during copy.                    */
/*    plError        True if anything goes wrong.                         */
/*                                                                        */
/* Uses the package variables to define the database and table names.     */
/* ---------------------------------------------------------------------- */
   DEFINE OUTPUT PARAMETER piRecCount    AS INTEGER   NO-UNDO.
   DEFINE OUTPUT PARAMETER piElapsedTime AS INTEGER   NO-UNDO.
   DEFINE OUTPUT PARAMETER plError       AS LOGICAL   NO-UNDO.

   define variable lError as logical no-undo.
   define variable lOK    as logical no-undo.

   run getInplace(output lInPlace).
   hbSource:DISABLE-DUMP-TRIGGERS().
   hbTarget:DISABLE-LOAD-TRIGGERS(FALSE).

   /* Reset the record counter. */
   transcnt = 0.
   piRecCount = 0.
   
   ETIME(YES).
   DO while hbSource:AVAILABLE and not lGotCancel:

      /* iterate through the query, copying data to the target database */
      /* Set up a transaction scope for the updates. */
      repeat transaction while hbSource:AVAILABLE and not lGotCancel on error undo, leave:

         /* Update {&TRANGROUP} records, or as many as are left, within one transaction. */
         inner:
         do while transcnt < {&TRANGROUP} and not lGotCancel on endkey undo, leave:
            onerec:
            DO ON ERROR UNDO, LEAVE onerec:
                if not lInPlace then do:
                   RUN copyRecord IN TARGET-PROCEDURE
                      ( INPUT hbSource,
                        INPUT hbTarget,
                        OUTPUT lOK).
                   if lOK then do:
                      assign lOK = hbTarget:buffer-release() no-error.
                      if error-status:error or not lOK then do:
                         run checkErrors.
                         lOK = false.
                      end.
                   end.
                   IF ( NOT lOK ) THEN DO:
                      assign lError = hbTarget:buffer-delete() no-error.
                   end.
                end.
            END.
            process events.
            /* Get the next record to process from the query. */
            hqSource:get-next().
            transcnt = transcnt + 1.
            piRecCount = piRecCount + 1.
            if hqSource:query-off-end or lGotCancel then do:
               /* Oops, were done. (In a good way.) */
               leave inner.
            end. /* if hqSource:query-off-end */

         end. /* do while transcnt < {&TRANGROUP} */

         PUBLISH "LogMsg"
            (INPUT cTargetTable + " - " + string(piRecCount) + " records.").
         transcnt = 0.
      end. /* repeat transaction */
   END.
   PUBLISH "LogDisp"
       (INPUT cTargetTable + " - " + string(piRecCount) + " records.").

   if not plError then plError = lGotCancel.

   /* delete all dynamic objects */
   hqSource:query-close().
   delete object hqSource.
   delete object hbTarget.
   delete object hbSource.

   piElapsedTime = ETIME.

end PROCEDURE.

PROCEDURE convertTable:
/* ---------------------------------------------------------------------- */
/* Performs the post processing of tables that depend on contents of      */
/* other tables.                                                          */
/*                                                                        */
/*  Input parameters:                                                     */
/*                                                                        */
/* Output parameters:                                                     */
/*    piRecCount     Count of records processed.                          */
/*    piElaspedTime  Elasped milliseconds during copy.                    */
/*    plError        True if anything goes wrong.                         */
/*                                                                        */
/* Uses the package variables to define the database and table names.     */
/* ---------------------------------------------------------------------- */
   DEFINE OUTPUT PARAMETER piRecCount    AS INTEGER   NO-UNDO.
   DEFINE OUTPUT PARAMETER piElapsedTime AS INTEGER   NO-UNDO.
   DEFINE OUTPUT PARAMETER plError       AS LOGICAL   NO-UNDO.

   define variable lOK     as logical no-undo.
   define variable lBulkOK as logical no-undo.

   if not valid-handle(hbTarget) then
      return.

   hbTarget:DISABLE-DUMP-TRIGGERS().
   hbTarget:DISABLE-LOAD-TRIGGERS(FALSE).

   /* Reset the record counter. */
   transcnt = 0.
   piRecCount = 0.

   ETIME(YES).

   DO while hbTarget:AVAILABLE and not lGotCancel:

      /* iterate through the query, processing records in the target database */
      /* Set up a transaction scope for the updates. */
      repeat transaction while hbTarget:available and not lGotCancel:

         /* Update {&TRANGROUP} records, or as many as are left, within one transaction. */
         inner:
         do while transcnt < {&TRANGROUP} and not lGotCancel:
            if transcnt = 0 then
               hbTarget:find-current(exclusive-lock).

            DO ON ERROR UNDO, LEAVE:
                RUN convertRecord IN TARGET-PROCEDURE
                    ( INPUT hbTarget,
                     OUTPUT lOK).
                IF ( NOT lOK ) THEN DO:
                    RUN checkErrors.
                end.
            END.
            process events.
            /* Get the next record to process from the query. */
            if hqTarget:get-next(exclusive-lock) then do:
            end.
            else do:
/*               run checkErrors.
               publish "LogDisp"
                  (input substitute("Get-next failed.  Count is &1",
                                     piRecCount)). */
            end.
            transcnt = transcnt + 1.
            piRecCount = piRecCount + 1.
            if hqTarget:query-off-end or lGotCancel then do:
               /* Oops, were done. (In a good way.) */
               leave inner.
            end. /* if hqSource:query-off-end */

         end. /* do while trancnt < {&TRANGROUP} */

         PUBLISH "LogMsg"
             (INPUT cTargetTable + " - " + string(piRecCount) + " records.").
         transcnt = 0.
      end. /* repeat */
   END.
   piElapsedTime = ETIME.
   publish "LogLog"
       (input substitute("&1 &2 records &3 seconds.",
                         cTargetTable,
                         piRecCount,
                         piElapsedTime / 1000) ).

   /* delete all dynamic objects */
   hqTarget:query-close().
   delete object hqTarget.
   delete object hbTarget.


end PROCEDURE.

PROCEDURE searchTable:
/* ---------------------------------------------------------------------- */
/* Find any records in the target table that meet the where clause   */
/* value specified.  Returns the time and count.                         */
/*                                                                        */
/*  Input parameters:                                                     */
/*    pcSourceTable  The name of the source table                         */
/*    pcTargetTable    The name of the target table                    */
/*                                                                        */
/* Output parameters: <None>                                              */
/*                                                                        */
/* Uses the package variables to define the database and table names.     */
/* ---------------------------------------------------------------------- */
   define  input parameter pcTargetTable as character no-undo.
   define  input parameter pcWhereClause as character no-undo.
   DEFINE OUTPUT PARAMETER piRecCount    AS INTEGER   NO-UNDO.
   DEFINE OUTPUT PARAMETER piElapsedTime AS INTEGER   NO-UNDO.

   define variable hbTarget      as handle  no-undo.
   define variable hqTarget      as handle  no-undo.
   define variable hfTarget      as handle  no-undo.
   define variable transcnt    as integer no-undo.
   define variable idx         as integer no-undo.
   define variable iStartTime  as integer no-undo.
   define variable iStoptime   as integer no-undo.
   define variable cTargetPrefix as character no-undo.
   define variable cTargetFileDB as character no-undo.


   cTargetFileDB = getDBDataName(getTrgDB()).

   /* create dynamic objects based on input parameter */
   create buffer hbTarget for table cTargetFileDB + "." + pcTargetTable.
   create query hqTarget.

   /* Reset the record counter. */
   transcnt = 0.
   piRecCount = 0.
   ETIME(YES).
   cTargetPrefix = entry(1, pcTargetTable, "_").
   hfTarget = hbTarget:buffer-field(cTargetPrefix + "_domain") no-error.
   if valid-handle(hfTarget) then do:

      /* make query ready for running */
      hqTarget:set-buffers(hbTarget).
      hqTarget:query-prepare("for each " + pcTargetTable + " " + pcWhereClause).
      hqTarget:query-open().
      hqTarget:get-first().

      IF hbTarget:AVAILABLE THEN DO:

         piRecCount = piRecCount + 1.
         /* Get the next record to process from the query. */
         hqTarget:get-next().
         if hqTarget:query-off-end then do:
            /* Oops, were done. (In a good way.) */
            LEAVE.
         end. /* if hqSource:query-off-end */
      END.

      /* delete all dynamic objects */
      hqTarget:query-close().
      delete object hqTarget.
      delete object hbTarget.
   END.
   piElapsedTime = ETIME.

end PROCEDURE.


PROCEDURE SrcProc:
/* ---------------------------------------------------------------------- */
/* The standard source buffer procedure.  Does nothing.                   */
/*                                                                        */
/*  Input parameters:                                                     */
/*    phbSource   The handle to the source buffer.                        */
/*                                                                        */
/* Output parameters:                                                     */
/*    plOK        Logical set false on error.                             */
/*                                                                        */
/* ---------------------------------------------------------------------- */
    DEFINE INPUT PARAMETER phbSource AS HANDLE NO-UNDO.
    DEFINE OUTPUT PARAMETER plOK AS LOGICAL NO-UNDO.

    plOK = YES.

end PROCEDURE.


PROCEDURE destructor:
/* ---------------------------------------------------------------------- */
/* The standard destructor procedure.  Does nothing but serve as backstop */
/*                                                                        */
/*  Input parameters: <None>                                              */
/*                                                                        */
/* Output parameters: <None>                                              */
/*                                                                        */
/* ---------------------------------------------------------------------- */

   return.

end PROCEDURE.


PROCEDURE showconv:
/* ---------------------------------------------------------------------- */
/* Display all of the conversions that will be run.                       */
/*                                                                        */
/*  Input parameters: <None>                                              */
/*                                                                        */
/* Output parameters: <None>                                              */
/*                                                                        */
/* ---------------------------------------------------------------------- */

   publish "LogLog"
      (input "The following conversions are being performed:").
   for each tvprogs where doconv:
      publish "LogLog"
         (input trim(convname,"[]")).
   end.

end PROCEDURE.


PROCEDURE showrec:
/* ---------------------------------------------------------------------- */
/* Display the record in the buffer.                                      */
/*                                                                        */
/*  Input parameters:                                                     */
/*    phbSource   The handle to the source buffer.                        */
/*                                                                        */
/* Output parameters: <None>                                              */
/*                                                                        */
/* ---------------------------------------------------------------------- */
    define input parameter phbSource as handle no-undo.

    define variable idx  as integer no-undo.
    define variable idx1 as integer no-undo.

    do idx = 1 to phbSource:num-fields:
       publish "LogDisp"
          (input substitute("&1.&2:&3:&4",
                            phbSource:dbname,
                            phbSource:name,
                            phbSource:buffer-field(idx):name,
                            phbSource:buffer-field(idx):buffer-value)).
    end.

end PROCEDURE.

PROCEDURE copyRecord:
/* ---------------------------------------------------------------------- */
/* The standard copy buffer procedure.  Does a buffer copy.               */
/*                                                                        */
/*  Input parameters:                                                     */
/*    phbSource   The handle to the source buffer.                        */
/*    phbTarget     The handle to the target buffer.                      */
/*                                                                        */
/* Output parameters:                                                     */
/*    plOK        Logical set false on error.                             */
/*                                                                        */
/* ---------------------------------------------------------------------- */
    define input parameter phbSource as handle no-undo.
    define input parameter phbTarget as handle no-undo.
    define output parameter plOK as logical no-undo.

    glSupress132 = true.
    assign plOK = phbTarget:buffer-copy(phbSource) no-error.
    if error-status:error or not plOK then do:
       run checkErrors.
       plOK = false.
    end.
    glSupress132 = false.


end PROCEDURE.

PROCEDURE convertRecord:
/* ---------------------------------------------------------------------- */
/* The standard target buffer procedure.  Does nothing.                   */
/*                                                                        */
/*  Input parameters:                                                     */
/*    phbTarget     The handle to the target buffer.                   */
/*                                                                        */
/* Output parameters:                                                     */
/*    plOK        Logical set false on error.                             */
/*                                                                        */
/* ---------------------------------------------------------------------- */
    DEFINE INPUT PARAMETER phbTarget AS HANDLE NO-UNDO.
    DEFINE OUTPUT PARAMETER plOK AS LOGICAL NO-UNDO.

    plOK = yes.

end PROCEDURE.

PROCEDURE checkErrors:
/* ---------------------------------------------------------------------- */
/* Display errors and log them.                                           */
/*                                                                        */
/*  Input parameters: <None>                                              */
/*                                                                        */
/* Output parameters: <None>                                              */
/*                                                                        */
/* ---------------------------------------------------------------------- */
   define variable i as integer no-undo.

   define variable hbDest as handle no-undo.

   if error-status:error then do:
      if error-status:num-messages = 0 then do:
         create tt-load-errors.
         assign
            tt-load-errors.num = 1
            tt-load-errors.msg = "    Error status but no message, probably no error.".
         for each tt-load-errors:
            publish "LogLog"
               (input " " + tt-load-errors.msg).
         end.
      end.
      else do:
         DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
            if glSupress132 then
                if error-status:get-number (i) = 132 then next.
            create tt-load-errors.
            assign
                tt-load-errors.num = i
                tt-load-errors.msg = "    " + ERROR-STATUS:GET-MESSAGE(i).
         END.

         find first tt-load-errors no-error.
         
         if available tt-load-errors then
            /* Identify caller */
            publish "LogDisp"
               (input substitute("***** Error detected in &1",
                                 program-name(2))).

         /* Now publish error messages */
         for each tt-load-errors:
            publish "LogDisp"
               (input "***** " + tt-load-errors.msg).
         END.
      end.
      empty temp-table tt-load-errors.
   end.
   else do:
      publish "LogDisp"
         (input "***** CheckErrors called with no error, possibly invalid data.").
      run getTargetBuffer
         (output hbDest).
      if valid-handle(hbDest) then do:
         publish "LogDisp"
            (input substitute("Processing table: &1",
                              hbDest:name)).
         run showrec
            (input hbDest).
      end.
      run getSourceBuffer
         (output hbDest).
      if valid-handle(hbDest) then do:
         publish "LogDisp"
            (input substitute("Processing table: &1",
                              hbDest:name)).
         run showrec
            (input hbDest).
      end.
   end.

end PROCEDURE.


PROCEDURE makeRef:
/* ---------------------------------------------------------------------- */
/* Create the alias for the reference database.                           */
/*                                                                        */
/*  Input parameters: <None>                                              */
/*                                                                        */
/* Output parameters: <None>                                              */
/*                                                                        */
/* ---------------------------------------------------------------------- */

   create alias reference for database value(ldbname("target")) no-error.
   if error-status:error then do:
      run checkErrors.
      publish "LogDisp"
         (input "***** Unable to create alias reference for target DB, cannot continue.").
      publish "LogDone".
      publish "LogHide".
      message "***** Unable to continue without reference DB." skip
              "***** Quitting MFG/UTIL."
         view-as alert-box error.
      run runquit.
   end.

end PROCEDURE.

PROCEDURE exitPrep:
/* ---------------------------------------------------------------------- */
/* Do any necessary cleanup and state preservation.                       */
/*                                                                        */
/*  Input parameters: <None>                                              */
/*                                                                        */
/* Output parameters: <None>                                              */
/*                                                                        */
/* ---------------------------------------------------------------------- */
   define variable hEnvVars            as handle    no-undo.
   define variable loccConvType        as character no-undo.
   define variable loccSourceDB        as character no-undo.
   define variable loccTargetDB        as character no-undo.
   define variable loccPhysTargetDB    as character no-undo.
   define variable loccSourceDBVersion as character no-undo.
   define variable loccTargetDBVersion as character no-undo.
   define variable loccDomainValue     as character no-undo.
   define variable cTmp                as character no-undo.
   define variable ttBuffHandle2       as handle no-undo.

   /* Get the values from the container */
   run getQDTItem(gcConvContainer, "{&ITEM-CONVTYPE}",     output loccConvType).
   run getQDTItem(gcConvContainer, "{&ITEM-TARGETDB}",     output loccTargetDB).
   run getQDTItem(gcConvContainer, "{&ITEM-PHYSTARGETDB}", output loccPhysTargetDB).
   run getQDTItem(gcConvContainer, "{&ITEM-CONVDOMAIN}",   output loccDomainValue).
   run getQDTItem(gcConvContainer, "{&ITEM-SOURCEDBVER}",  output loccSourceDBVersion).
   run getQDTItem(gcConvContainer, "{&ITEM-TARGETDBVER}",  output loccTargetDBVersion).


   /* Update container if current values don't match container values */
   if loccSourceDB <> cSourceDB then
      run setQDTItem(gcConvContainer, "{&ITEM-SOURCEDB}",     cSourceDB).
   if loccTargetDB <> cTargetDB then
      run setQDTItem(gcConvContainer, "{&ITEM-TARGETDB}",     cTargetDB).
   if loccPhysTargetDB <> cPhysTargetDB then
      run setQDTItem(gcConvContainer, "{&ITEM-PHYSTARGETDB}", cPhysTargetDB).
   if loccDomainValue <> cDomainValue then
      run setQDTItem(gcConvContainer, "{&ITEM-CONVDOMAIN}",   cDomainValue).
   if loccSourceDBVersion <> cSourceDBVersion then
   run setQDTItem(gcConvContainer, "{&ITEM-SOURCEDBVER}",  cSourceDBVersion).
   if loccTargetDBVersion <> cTargetDBVersion then
      run setQDTItem(gcConvContainer, "{&ITEM-TARGETDBVER}",  cTargetDBVersion).
   if loccConvType <> cConvType then
      run setQDTItem(gcConvContainer, "{&ITEM-CONVTYPE}",     cConvType).
   run setQDTConfig.

   /* Now clean up temp table handles */
   for each temp-table-list:
      /* If there is a valid temp-table handle, delete the object. */
      if valid-handle(temp-table-list.tthandle) then do:
         ttBuffHandle2 = temp-table-list.tthandle:default-buffer-handle.
         ttBuffHandle2:empty-temp-table().
         delete object temp-table-list.tthandle.
      end.

   end.

end PROCEDURE.


PROCEDURE init_persis_var:
/* --------------------------------------------------------------------- */
/* Initialise (reset to none) temp-table which holds "persistent" vars   */
/*                                                                       */
/*   No parameters                                                       */
/* --------------------------------------------------------------------- */

   for each persis_var:
      delete persis_var.
   end.

end PROCEDURE.


PROCEDURE set_persis_var:
/* --------------------------------------------------------------------- */
/* set a persistent variable                                             */
/*                                                                       */
/*    input parameters: p_var_name : name of variable                    */
/*                      p_var_val  : value of variable (as string)       */
/* --------------------------------------------------------------------- */
   define input parameter p_var_name as character no-undo.
   define input parameter p_var_val as character no-undo.

   if p_var_val = "true" then
      p_var_val = "yes".
   else if p_var_val = "false" then
      p_var_val = "no".

   find first persis_var
      where persis_var.var_name = p_var_name no-error.
   if not available persis_var then
      create persis_var.
   assign
      persis_var.var_name = p_var_name
      persis_var.var_val = p_var_val.

end PROCEDURE.


PROCEDURE luassocpf:
/* --------------------------------------------------------------------- */
/* look up pf file that is associated with the DB Set DBSetName          */
/*                                                                       */
/*    input parameters:                                                  */
/*           cDbSetName - name of DBSet for which we want the pf file    */
/*   output parameters:                                                  */
/*           pfFile - the pfFile associated with cDbSetName              */
/* --------------------------------------------------------------------- */
define input  parameter cDbSetName as character no-undo.
define output parameter pfFile as character no-undo.

define variable pfFilesList as character no-undo.
define variable pfFileTmp as character no-undo.
define variable pfFileJustFile as character no-undo.
define variable icnt as integer no-undo.

run lupffile.p (input "",  output pfFilesList).
repeat icnt = 1 to NUM-ENTRIES(pfFilesList):
     pfFileTmp = entry(icnt, pfFilesList, ",").
     pfFileJustFile = getFileName(pfFileTmp).
     if cDbSetName + ".pf" = pfFileJustFile  then do:
        pfFile = pfFileTmp. /* we found the right pf file */
     end.
end. /* repeat */
end PROCEDURE.

PROCEDURE setGetInfoResult:
/* --------------------------------------------------------------------- */
/* Set the result of the last run of getinfo.p.                          */
/*                                                                       */
/*    input parameters:                                                  */
/*          plGetInfoResult - logical from getinfo.p.                    */
/* --------------------------------------------------------------------- */
define input parameter plGetInfoResult as logical no-undo.

lGetInfoResult = plGetInfoResult.

end PROCEDURE.

PROCEDURE valEmptyDir:
/* ---------------------------------------------------------------------- */
/* Procedure to validate that an existing directory is empty.             */
/*                                                                        */
/*  Input parameters:                                                     */
/*    pc-dirname - the directory to use.                                  */
/*                                                                        */
/* Output parameters:                                                     */
/*    pl-mt-dir - Set to true if the passed directory is empty.           */
/*                                                                        */
/* ---------------------------------------------------------------------- */
define input  parameter pc-dirname      as character no-undo.
define output parameter pl-mt-dir       as logical   no-undo.

define variable cDirNameOut      as character no-undo.
define variable c-cur-file-name  as character no-undo.
define variable c-cur-fullpath   as character no-undo.
define variable c-cur-attributes as character no-undo.

pl-mt-dir = true.
file-info:file-name = pc-dirname.
if file-info:full-pathname <> ? then do:
   input from OS-DIR(pc-dirname) no-echo. 
   repeat:
      import c-cur-file-name c-cur-fullpath c-cur-attributes.
      if INDEX(c-cur-attributes, 'F') > 0 THEN DO:
          pl-mt-dir = false.
      END. /* if INDEX(c-cur-attributes,'F') > 0 THEN DO:*/
   END. /* repeat: */
   input close.
end.

end PROCEDURE.

PROCEDURE valDataDir:
/* ---------------------------------------------------------------------- */
/* Procedure to validate that an existing directory has data files.       */
/*                                                                        */
/*  Input parameters:                                                     */
/*    pc-dirname - the directory to use.                                  */
/*                                                                        */
/* Output parameters:                                                     */
/*    pl-data-dir - Set to true if the passed directory has .d files.     */
/*                                                                        */
/* ---------------------------------------------------------------------- */
define input  parameter pc-dirname      as character no-undo.
define output parameter pl-data-dir     as logical   no-undo.

define variable cDirNameOut      as character no-undo.
define variable c-cur-file-name  as character no-undo.
define variable c-cur-fullpath   as character no-undo.
define variable c-cur-attributes as character no-undo.

pl-data-dir = false.
file-info:file-name = pc-dirname.
if file-info:full-pathname <> ? then do:
   input from OS-DIR(pc-dirname) no-echo. 
   repeat:
      import c-cur-file-name c-cur-fullpath c-cur-attributes.
      if INDEX(c-cur-attributes, 'F') > 0 and
         c-cur-file-name matches "*~.d" THEN DO:
          pl-data-dir = true.
          leave.
      END. /* if INDEX(c-cur-attributes,'F') > 0 THEN DO:*/
   END. /* repeat: */
   input close.
end.

end PROCEDURE.

PROCEDURE valNoDir:
/* ---------------------------------------------------------------------- */
/* Procedure to validate that a directory does not exist.                 */
/*                                                                        */
/*  Input parameters:                                                     */
/*    pc-dirname - the directory to use.                                  */
/*                                                                        */
/* Output parameters:                                                     */
/*    pl-no-dir - Set to true if the passed directory name did not        */
/*                exist and was successfully created.                     */
/*                                                                        */
/* Creates the directory if it does not exist.                            */
/* ---------------------------------------------------------------------- */
define input  parameter pc-dirname      as character no-undo.
define output parameter pl-no-dir       as logical   no-undo.

define variable cDirNameOut as character no-undo.
define variable lEmptyDir   as logical   no-undo.

pl-no-dir = true.
file-info:file-name = pc-dirname.
if file-info:full-pathname <> ? then do:
    run valEmptyDir
        (input pc-dirname,
        output lEmptyDir).
    if not lEmptyDir then do:
        message substitute("Directory &1 already exists,",
                       file-info:full-pathname) skip
            "and is not empty.  You must specify a directory that does" skip
            "not exist or is empty."
                view-as alert-box error.
        pl-no-dir = false.
        return.
    end.
end.

/* Create the directory.  Returns the name of the created directory. */
run create-dir
    (input pc-dirname,
    output cDirNameOut).
    
/* If we could not create the directory, return false. */
if pc-dirname <> cDirNameOut then do:
    pl-no-dir = false.
    message substitute("Directory &1 could not be created.",
                       file-info:full-pathname) skip
            "Specify a different directory that does not exist."
                view-as alert-box error.
end.

end PROCEDURE.

PROCEDURE valDir:
/* ---------------------------------------------------------------------- */
/* Procedure to validate a directory.                                     */
/*                                                                        */
/*  Input parameters:                                                     */
/*    pc-dirname - the directory to use.                                  */
/*                                                                        */
/* Output parameters:                                                     */
/*    pc-full-pathname - the full path of the validated directory.        */
/*                       Set to "" if invalid.                            */
/* ---------------------------------------------------------------------- */
define input  parameter pc-data-dir      as character no-undo.
define output parameter pc-full-pathname as character no-undo.

define variable ll-baddir        as logical  no-undo.
define variable ll-create-dir    as logical  no-undo.

ll-baddir = true.

if pc-data-dir <> "" then do:
    file-info:file-name = pc-data-dir.
    if file-info:full-pathname <> ? then do:
        if index(file-info:file-type,'D') <> 0 then do:
            if index(file-info:file-type, 'W') <> 0 then do:
                /* It's a writable directory, OK */
                ll-baddir = false.
                pc-full-pathname = file-info:full-pathname.
            end.
        end.
    end.
    else do:
        ll-create-dir = yes.
        message 
                pc-data-dir + " does not exist, create it?" VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO update ll-create-dir.
        if ll-create-dir then do:
            run create-dir
               ( input pc-data-dir,
                output pc-data-dir).
            if pc-data-dir <> "" then do:
                ll-baddir = false.
                publish "LogLog"
                   (input "Created directory " + pc-data-dir).
                pc-full-pathname = pc-data-dir.
            end.
        end.
    end.
end.

if ll-baddir then do:
    message
        pc-data-dir + " could not be created " +
        "or is not writable."
        view-as alert-box.
    publish "LogLog"
       (input pc-data-dir + " could not be created " +
        "or is not writable.").
    pc-full-pathname = "".
end.

end PROCEDURE.

PROCEDURE create-dir:
/* ---------------------------------------------------------------------- */
/* Procedure to create a directory.                                       */
/*                                                                        */
/*  Input parameters:                                                     */
/*    pc-dirname - the directory to create.                               */
/*                                                                        */
/* Output parameters:                                                     */
/*    pc-full-pathname - the full path of the created directory.          */
/*                                                                        */
/* ---------------------------------------------------------------------- */
define  input parameter pc-dirname       as character no-undo.
define output parameter pc-full-pathname as character no-undo.

define variable lc-new-dir-name as character no-undo.
define variable lc-existing-path as character no-undo.
define variable li-dirsep-idx as integer no-undo.

/* If the directory already exists, just return the full name. */
pc-full-pathname = ''.
file-info:file-name = pc-dirname.
if file-info:full-pathname <> ? and
   index(file-info:file-type,'D') <> 0 and
   index(file-info:file-type,'W') <> 0 then do:
    pc-full-pathname = file-info:full-pathname.
    return.
end.

os-create-dir value(pc-dirname).
file-info:file-name = pc-dirname.
if file-info:full-pathname <> ? and
   index(file-info:file-type,'D') <> 0 and
   index(file-info:file-type,'W') <> 0 then do:
   pc-full-pathname = file-info:full-pathname.
   publish "logLog"
      (input substitute("Created &1",
                         pc-dirname)).
end.
                      
if pc-full-pathname = '' then do:
    li-dirsep-idx = r-index(pc-dirname, '{&DIRSEP}').
    
    if li-dirsep-idx <> 0 then do:
        lc-new-dir-name = substr(pc-dirname, li-dirsep-idx + 1).
        lc-existing-path = substr(pc-dirname, 1, li-dirsep-idx - 1).
        run create-dir
           (input  lc-existing-path,
            output pc-full-pathname).
        if pc-full-pathname <> '' then do:
            os-create-dir value(pc-dirname).
            file-info:file-name = pc-dirname.
            if file-info:full-pathname <> ? and
               index(file-info:file-type,'D') <> 0 and
               index(file-info:file-type,'W') <> 0 then do:
                pc-full-pathname = file-info:full-pathname.
                publish "logLog"
                   (input substitute("Created &1",
                                     pc-dirname)).
            end.
        end.
    end.
end.

return.

end PROCEDURE.

procedure add2list:
/* ---------------------------------------------------------------------- */
/* Procedure to add entry to "|" separated list.                          */
/*                                                                        */
/*   Input-Output Parameters:                                             */
/*       cList: list to add entry to.                                     */
/*   Input Parameters:                                                    */
/*       cLine: line to add to list.                                      */
/*                                                                        */
/*------------------------------------------------------------------------*/
define input-output parameter cList as character.
define input parameter cLine as character.

if cList = "" then do:
  cList = cLine.
end.
else do:
  cList = cList + "|" + cLine.
end.

return.

end procedure.

procedure getIconInfo:

define  input parameter pcContainer               as character no-undo.
define output parameter pcIconFolder              as character no-undo.
define output parameter plOverWritePfFiles        as logical   no-undo.
define output parameter plOverWriteScriptFiles    as logical   no-undo.

define variable cIconFullPath as character no-undo.
define variable cIconHome     as character no-undo.
define variable cProduct      as character no-undo.
define variable cProdName     as character no-undo.
define variable cComponent    as character no-undo.
define variable cCompname     as character no-undo.
define variable cOWPF         as character no-undo.
define variable cOWScp        as character no-undo.

   run getQDTItem (pcContainer, '{&ITEM-WRITEPF}', output cOWPF).
   if cOWPF = "" then cOWPF = "true".
   run getQDTItem (pcContainer, '{&ITEM-WRITESCRIPTS}', output cOWScp).
   if cOWScp = "" then cOWScp = "true".
   run getCurrentProduct (input pcContainer, output cProduct, output cComponent).
   run getQDTItem (cProduct, "{&ITEM-DESC}", output cProdName).
   /* Remove any slashes from description to make folder name. */
   pcIconFolder = replace(cProdName,"/","").
   pcIconFolder = replace(pcIconFolder,"~\","").
          
   plOverWritePfFiles =
      (if lookup(trim(cOWPF), "true,yes") > 0
          then true else false).
   plOverWriteScriptFiles =
      (if lookup(trim(cOWScp), "true,yes") > 0
          then true else false).
                  
if opsys <> "unix" then do:
   LOAD "Software\Microsoft\Windows\CurrentVersion\Explorer"
        BASE-KEY "HKEY_LOCAL_MACHINE".
    
   USE "Software\Microsoft\Windows\CurrentVersion\Explorer".

   GET-KEY-VALUE SECTION "Shell Folders" KEY "Common Programs" VALUE cIconHome.

   USE "".
   
   cIconFullPath = cIconHome + "{&DIRSEP}{&PROD-QAD}".
   file-info:file-name = cIconFullPath.
   if file-info:file-type = ? then do:
      os-create-dir value(cIconFullPath).
      file-info:file-name = cIconFullPath.
   end.
   if file-info:file-type = ? then do:
      pcIconFolder = "".
      return.
   end.

   cIconFullPath = cIconFullPath + "{&DIRSEP}" + pcIconFolder.
   file-info:file-name = cIconFullPath.
   if file-info:file-type = ? then do:
      os-create-dir value(cIconFullPath).
      file-info:file-name = cIconFullPath.
   end.
   if file-info:file-type = ? then do:
      pcIconFolder = "".
      return.
   end.
   else
      pcIconFolder = cIconFullPath.

   cIconFullPath = cIconFullPath + "{&DIRSEP}" + getCurrentEnv().
   file-info:file-name = cIconFullPath.
   if file-info:file-type = ? then do:
      os-create-dir value(cIconFullPath).
      file-info:file-name = cIconFullPath.
   end.
   if file-info:file-type = ? then do:
      pcIconFolder = "".
      return.
   end.
   else
      pcIconFolder = cIconFullPath.
end.


end procedure.

/* Write the file to signal that we're done. 
   This is currently not used for anything.  */
procedure runquit:
define variable cLogDirectory as character no-undo.
   /* Get the log directory.*/
   run getQDTItem('{&PROD-GLOBAL}','{&ITEM-LOGDIR}',output cLogDirectory).
   output to value(cLogDirectory + "{&DIRSEP}QDTAdminDone.txt").
   put unformatted "Done" skip.
   output close.
   quit.
end procedure.

procedure parseDBVersIniTableData:
/* ---------------------------------------------------------------------- */
/* Procedure to parse specially formatted records in dbvers.ini           */
/* The format can be a table and field pair or table and index pair       */ 
/*                                                                        */
/* Format of table/field pair: table__<tablename>__field__<fieldname>     */
/* e.g. table__dom_mstr__field__dom_domain                                */
/*                                                                        */
/* Format of table/index pair: table__<tablename>__index__<indexname>     */
/* e.g. table__dom_mstr__index__dom_name                                  */
/*                                                                        */
/* Note: the formatting uses double underscores by design as some field   */
/*       and/or index names may contain _field_ in their name             */
/*                                                                        */
/*   Input Parameters:                                                    */
/*       ipcString: A string of characters using the above format of      */
/*                  table__<tablename>__field__<fieldname>                */  
/*               or table__<tablename>__index__<indexname>                */
/*                                                                        */
/*   Output Parameters:                                                   */
/*       opcTablename       : A database table name                       */
/*       opcFieldOrIndexname: A field or index name                       */
/*                                                                        */
/*------------------------------------------------------------------------*/
   define input  parameter ipcString          as character no-undo.
   define output parameter opcTablename       as character no-undo.
   define output parameter opcFieldOrIndexname as character no-undo.
  
   define variable iPos1 as integer no-undo.
   define variable iPos2 as integer no-undo.
   
   assign 
      iPos1               = Index(ipcString, "table__") + length("table__")
      /* Luckily the length of _field_ and _index_ are        */
      /* the same otherwise we would have to check beforehand */
      /* and then handle each case separately                 */
      iPos2               = Index(ipcString, "__field__")
      opcTablename        = substring(ipcString, iPos1, iPos2 - iPos1)
      opcFieldOrIndexname = substring(ipcString, iPos2 + length("__field__")).
   
end procedure.


