/****************************************************************************
 **  Program Name: replTrigInsert.p
 **        Author: Terry Mays
 **       Created: 09/24/2009
 **   Description: Standalone trigger insertion routine... Source Schema.
 **                You MUST set the CURRENT WORKING DB in the dictionary to the 
 **                database to activate triggers on. Also set the vDB varriable
 **                equal to the source DB name.
 **                You can do this by setting the vDB varriable below to the 
 **                source databas to activate triggers on or set the SOURCEDB 
 **                value in predefs.i You also need to   
 **
 **       History: MTM - 1/25/2012 4.01a - Tweaked for Pro2 v4
 **                Property names changed in v4, added support for TRIGGER_EXTENSION
 ** -----------------------------------------------------------------------
 ****************************************************************************/


/* CAUTION!!!!!!  SEE NOTE ABOVE CONCERNING WORKING DATABASE */

DEFINE VARIABLE vDB                 AS CHARACTER NO-UNDO init "qadcust".  /* see notes above!!! */
DEFINE VARIABLE vlogFrozen          AS LOGICAL NO-UNDO.
DEFINE VARIABLE vHasTrigger         AS LOGICAL NO-UNDO.
DEFINE VARIABLE vWriteDir           AS CHARACTER NO-UNDO.
DEFINE VARIABLE vDeleteDir          AS CHARACTER NO-UNDO.
DEFINE VARIABLE vTrigExtension      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lResp               AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE vtables AS CHARACTER NO-UNDO INIT "xxbco_mstr,xxbcp_mstr,xxccm_mstr,xxdc_mstr,xxdrpinv_mstr,xxdrprl_ctrl,xxdrptr_hist,xxemtmc_mstr,xxemtmcd_det,xxemtmi_mstr,xxemtms_mstr,xxemtmu_mstr,xxexp_hist,xxlgd_det,xxlgx_mstr,xxlop_ctrl,xxmtp_mstr,xxordt_mstr,xxordu_mstr,xxovp_det,xxpc_hist,xxplr_det,xxqi_ctrl,xxrb_mstr,xxrbd_det,xxrr_mstr,xxrrc_ctrl,xxrrd_det,xxscpd_det,xxso_mstr,xxsrvc_ctrl,xxst_mstr,xxstrc_ctrl,xxwwcl_ctrl,xxwwd_mstr".


DO FOR replProperties.
  /* MTM - 1/25/2012 - 4.10a */
  FIND FIRST replProperties WHERE replProperties.PropertyName = "SCHEMA_DEL_DIRECTORY" NO-LOCK NO-ERROR.
  IF AVAILABLE replProperties THEN DO:
     ASSIGN vDeleteDir = replProperties.PropertyValue.
     IF vDeleteDir = "." THEN vDeleteDir = "".
     IF vDeleteDir <> "" AND SUBSTRING(vDeleteDir,LENGTH(vDeleteDir),1) <> "/" THEN vDeleteDir = vDeleteDir + "/".
  END. /*find SCHEMA_DELETE_PATH*/
  ELSE DO:
      MESSAGE "Missing Property value for SCHEMA_DEL_DIRECTORY. Can not continue."
         VIEW-AS ALERT-BOX BUTTONS OK.
      RETURN.
  END.
  /* MTM - 1/25/2012 - 4.10a */
  FIND FIRST replProperties WHERE replProperties.PropertyName = "SCHEMA_WRI_DIRECTORY" NO-LOCK NO-ERROR.
  IF AVAILABLE replProperties THEN DO:
      ASSIGN vWriteDir = replProperties.PropertyValue.
      IF vWriteDir = "." THEN vWriteDir = "".     
      /* If the Path is not blank then insure the last character is a */
      IF vWriteDir <> "" AND SUBSTRING(vwritedir,LENGTH(vwritedir),1) <> "/" THEN vWriteDir = vWriteDir + "/".
  END. /*find SCHEMA_WRITE_PATH*/
  ELSE DO:
      MESSAGE "Missing Property value for SCHEMA_WRI_DIRECTORY. Can not continue."
         VIEW-AS ALERT-BOX BUTTONS OK.
      RETURN.
  END.
  /* MTM - 1/25/2012 - 4.10a */
  FIND FIRST replProperties WHERE replProperties.PropertyName = "TRIGGER_EXTENSION" NO-LOCK NO-ERROR.
  IF AVAILABLE replProperties THEN DO:
     ASSIGN vTrigExtension = replProperties.PropertyValue.
     IF vTrigExtension = "" THEN vTrigExtension = ".p".     
     /* If the Path is not blank then insure the last character is a */
  END. /*find TRIGGER_EXTENSION*/
  ELSE DO:
     MESSAGE "Missing Property value for TRIGGER_EXTENSION.. Using default of .p."
             VIEW-AS ALERT-BOX BUTTONS OK.
     vTrigExtension = ".p".
  END.
END.  /*DO FOR replProperties. */

FOR EACH _file:
    /* IF table is not on our list, go to next table */
    IF LOOKUP (_file._file-name, vtables) = 0 THEN NEXT.
  
    /* Unfreeze the table if frozen....We'll put it back..no worries...*/
    vLogFrozen= FALSE.
    IF _file._frozen 
        THEN ASSIGN vLogFrozen= TRUE
                    _file._frozen = FALSE.
    /* Do we already have a repl delete trigger for this table? */
    vHasTrigger = False.
    FOR EACH _file-trig OF _file WHERE _file-trig._event EQ "REPLICATION-DELETE":
        vHasTrigger = TRUE.
    END.
    /* No repl trigger...Add it */
    IF NOT vHasTrigger THEN DO:
       CREATE _file-trig.
       ASSIGN _file-trig._file-recid = RECID(_file)
              _file-trig._event      = "REPLICATION-DELETE"
              _file-trig._proc-name   = vDeleteDir + "d" + vDB + "_" + _file._file-name + vTrigExtension. 
    END.
  
    /* Do we already have a repl write trigger for this table? */
    vHasTrigger = False.
    FOR EACH _file-trig OF _file WHERE _file-trig._event EQ "REPLICATION-WRITE":
        vHasTrigger = TRUE.
    END.
  
    IF NOT vHasTrigger THEN DO:
       CREATE _file-trig.
       ASSIGN _file-trig._file-recid = RECID(_file)
              _file-trig._event      = "REPLICATION-WRITE"
             _file-trig._proc-name   = vWriteDir + "w" + vDB + "_" + _file._file-name + vTrigExtension. 
    END.
  
  
    /* If the table was previously frozen then re-freeze it!! */
    IF vlogFrozen 
      THEN ASSIGN _file._frozen = TRUE
                  vLogFrozen= FALSE.
  
END.  /*FOR EACH _file */




MESSAGE  "Replication Triggers have been inserted for" LDBNAME("DICTDB") 
           VIEW-AS ALERT-BOX.
