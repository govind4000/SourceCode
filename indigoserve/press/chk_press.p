DEFINE VARIABLE  li_cntr  AS    INTEGER     NO-UNDO.
DEFINE VARIABLE  lc_PID   AS    CHARACTER   FORMAT "x(60)"     NO-UNDO.
DEFINE VARIABLE  ld_sdate AS    DATE        FORMAT "99/99/99"  NO-UNDO.

define variable liisutil as integer.
define variable liisjam  as integer.
define variable liisfail as integer.
define variable liissub  as integer.
define variable liisprt  as integer.
define variable liiscfc  as integer.
define variable liisink  as integer.
define variable liisblk  as integer.
define variable liispip  as integer.
define variable liisbid  as integer.
define variable liisdi  as integer.
define variable liisic  as integer.

define variable llisutil as logical.
define variable llisjam  as logical.
define variable llisfail as logical.
define variable llissub  as logical.
define variable llisprt  as logical.
define variable lliscfc  as logical.
define variable llisink  as logical.
define variable llisblk  as logical.
define variable llispip  as logical.
define variable llisbid  as logical.
define variable llisdi  as logical.
define variable llisic  as logical.



assign LC_PID = "44000280"   
    ld_sdate = 10/01/2015.


repeat :


UPDATE LC_PID 
       ld_sdate.     

assign llisutil = false
llisjam = false
llisfail = false
llissub = false
llisprt = false
lliscfc = false
llisink = false
llisblk = false
llispip = false
llisbid = false
.

assign liisutil = 0
liisjam = 0
liisfail = 0
liissub = 0
liisprt = 0
liiscfc = 0
liisink = 0
liisblk = 0
liispip = 0
liisbid = 0
.




DO li_cntr =1 TO NUM-ENTRIES(LC_PID):

 FOR EACH isutil_mstr WHERE isutil_prs_id = ENTRY(li_cntr,lc_PID)
     AND isutil_date >= ld_sdate no-lock:
       Assign llisutil = true
              liisutil = liisutil + 1.

 END.
  
 FOR EACH isjam_evt WHERE isjam_prs_id = ENTRY(li_cntr,lc_PID) 
     AND isjam_date >= ld_sdate no-lock:
       Assign llisjam = true
              liisjam = liisjam + 1.

 END.   

 FOR EACH isfail_evt WHERE isfail_prs_id = ENTRY(li_cntr,lc_PID) 
     AND isfail_date >= ld_sdate no-lock:
       Assign llisfail = true
              liisfail = liisfail + 1.

 END.
  
 FOR EACH issub_evt WHERE issub_prs_id = ENTRY(li_cntr,lc_PID) 
     AND issub_date >= ld_sdate no-lock:
       Assign llissub = true
              liissub = liissub + 1.

 END. 
   
 FOR EACH isprt_evt WHERE isprt_prs_id = ENTRY(li_cntr,lc_PID) 
     AND isprt_date >= ld_sdate no-lock:

    FOR EACH isprtc_dtl WHERE isprtc_id = isprt_id
         no-LOCK :
       Assign llisprt = true
              liisprt = liisprt + 1.

    END.
 END.
 FOR EACH iscf_evt WHERE iscf_prs_id = ENTRY(li_cntr,lc_PID) 
     AND iscf_date >= ld_sdate no-LOCK:

    FOR EACH iscfc_dtl WHERE iscfc_id = iscf_id
         no-LOCK :
       Assign lliscfc = true
              liiscfc = liiscfc + 1.

    END.

 END.
 FOR EACH isink_evt WHERE isink_prs_id = ENTRY(li_cntr,lc_PID) 
     AND isink_date >= ld_sdate no-lock:
       Assign llisink = true
              liisink = liisink + 1.

 END.
 FOR EACH isblk_evt WHERE isblk_prs_id = ENTRY(li_cntr,lc_PID) 
     AND isblk_date >= ld_sdate no-lock:
       Assign llisblk = true
              liisblk = liisblk + 1.

 END.
 FOR EACH ispip_evt WHERE ispip_prs_id = ENTRY(li_cntr,lc_PID) 
     AND ispip_date >= ld_sdate no-lock:
       Assign llispip = true
              liispip = liispip + 1.

 END.
 FOR EACH isbid_evt WHERE isbid_prs_id = ENTRY(li_cntr,lc_PID) 
     AND isbid_date >= ld_sdate no-lock:
       Assign llisbid = true
              liisbid = liisbid + 1.

 END.
 
 FOR EACH isdi_mstr WHERE isdi_prs_id = ENTRY(li_cntr,lc_PID) 
     AND isdi_date >= ld_sdate:
       Assign llisdi = true
              liisdi = liisdi + 1.

 END.

 FOR EACH isic_mstr WHERE isic_prs_id = ENTRY(li_cntr,lc_PID) 
     AND isic_ctr_date >= ld_sdate:
       Assign llisic = true
              liisic = liisic + 1.
 END.

 
/*
 FOR EACH iseh_hist WHERE iseh_prs_id = ENTRY(li_cntr,lc_PID) 
     AND iseh_date >= ld_sdate:
        DELETE iseh_hist.
 END.
*/
/*
 FOR EACH isch_hist WHERE isch_prs_id = ENTRY(li_cntr,lc_PID) 
     AND isch_date >= ld_sdate:
        DELETE isch_hist.
 END.
 FOR EACH ispcd_det WHERE ispcd_prs_id = ENTRY(li_cntr,lc_PID) 
     AND ispcd_date >= ld_sdate:
        DELETE ispcd_det.
 END.
 FOR EACH isdj_mstr WHERE isdj_prs_id = ENTRY(li_cntr,lc_PID) 
     AND isdj_date >= ld_sdate:
        DELETE isdj_mstr.
 END.
 FOR EACH isic_mstr WHERE isic_prs_id = ENTRY(li_cntr,lc_PID) 
     AND isic_ctr_date >= ld_sdate:
        DELETE isic_mstr.
 END.*/

END.


disp llisutil
     llisjam
     llisfail
     llissub
     llisprt
     lliscfc
     llisink
     llisblk
     llispip
     llisbid
     llisdi
     llisic
     .


END. /*repeat */