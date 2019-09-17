DEFINE VARIABLE  li_cntr  AS    INTEGER     NO-UNDO.
DEFINE VARIABLE  lc_PID   AS    CHARACTER   FORMAT "x(60)"     NO-UNDO.
DEFINE VARIABLE  ld_sdate AS    DATE        FORMAT "99/99/99"  NO-UNDO.

assign LC_PID = "43000135"   
    ld_sdate = 09/01/2017.

UPDATE LC_PID 
       ld_sdate.     


DO li_cntr =1 TO NUM-ENTRIES(LC_PID):

 FOR EACH isutil_mstr WHERE isutil_prs_id = ENTRY(li_cntr,lc_PID)
     AND isutil_date >= ld_sdate:
       DELETE isutil_mstr.
 END.
  
 FOR EACH isjam_evt WHERE isjam_prs_id = ENTRY(li_cntr,lc_PID) 
     AND isjam_date >= ld_sdate:
       DELETE isjam_evt.
 END.   

 FOR EACH isfail_evt WHERE isfail_prs_id = ENTRY(li_cntr,lc_PID) 
     AND isfail_date >= ld_sdate:
       DELETE isfail_evt.
 END.
  
 FOR EACH issub_evt WHERE issub_prs_id = ENTRY(li_cntr,lc_PID) 
     AND issub_date >= ld_sdate:
        DELETE issub_evt.
 END. 
   
 FOR EACH isprt_evt WHERE isprt_prs_id = ENTRY(li_cntr,lc_PID) 
     AND isprt_date >= ld_sdate:

    FOR EACH isprtc_dtl WHERE isprtc_id = isprt_id
         EXCLUSIVE-LOCK :
       DELETE isprtc_dtl .
    END.

       DELETE isprt_evt.
 END.

 FOR EACH iscf_evt WHERE iscf_prs_id = ENTRY(li_cntr,lc_PID) 
     AND iscf_date >= ld_sdate EXCLUSIVE-LOCK:

    FOR EACH iscfc_dtl WHERE iscfc_id = iscf_id
         EXCLUSIVE-LOCK :
       DELETE iscfc_dtl .
    END.
       DELETE iscf_evt.
 END.

 FOR EACH isink_evt WHERE isink_prs_id = ENTRY(li_cntr,lc_PID) 
     AND isink_date >= ld_sdate:
        DELETE isink_evt.
 END.

 FOR EACH isblk_evt WHERE isblk_prs_id = ENTRY(li_cntr,lc_PID) 
     AND isblk_date >= ld_sdate:
        DELETE isblk_evt.
 END.

 FOR EACH ispip_evt WHERE ispip_prs_id = ENTRY(li_cntr,lc_PID) 
     AND ispip_date >= ld_sdate:
        DELETE ispip_evt.
 END.

 FOR EACH isbid_evt WHERE isbid_prs_id = ENTRY(li_cntr,lc_PID) 
     AND isbid_date >= ld_sdate:
        DELETE isbid_evt. 
 END.

/*
 FOR EACH iseh_hist WHERE iseh_prs_id = ENTRY(li_cntr,lc_PID) 
     AND iseh_date >= ld_sdate:
        DELETE iseh_hist.
 END.
*/

 FOR EACH isch_hist WHERE isch_prs_id = ENTRY(li_cntr,lc_PID) 
     AND isch_date >= ld_sdate:
        DELETE isch_hist.
 END.



 FOR EACH ispcd_det WHERE ispcd_prs_id = ENTRY(li_cntr,lc_PID) 
     AND ispcd_date >= ld_sdate:
        DELETE ispcd_det.
 END.

 FOR EACH isdi_mstr WHERE isdi_prs_id = ENTRY(li_cntr,lc_PID) 
     AND isdi_date >= ld_sdate:

    FOR EACH isdic_dtl WHERE isdic_prs_id = isdi_prs_id
                         AND isdic_eng = isdi_eng
                         AND isdic_date = isdi_date
         EXCLUSIVE-LOCK :
       DELETE isdic_dtl .
    END.

    DELETE isdi_mstr.

 END.

 FOR EACH isdj_mstr WHERE isdj_prs_id = ENTRY(li_cntr,lc_PID) 
     AND isdj_date >= ld_sdate:

    FOR EACH isdjc_dtl WHERE isdjc_id = isdj_id
         EXCLUSIVE-LOCK :
       DELETE isdjc_dtl .
    END.

    DELETE isdj_mstr.

 END.

 FOR EACH isic_mstr WHERE isic_prs_id = ENTRY(li_cntr,lc_PID) 
     AND isic_ctr_date >= ld_sdate:

    FOR EACH isicd_dtl WHERE isicd_prs_id = isic_prs_id
                         AND isicd_year = isic_year 
                         AND isicd_month = isic_month
         EXCLUSIVE-LOCK :
       DELETE isicd_dtl .
    END.
   
    DELETE isic_mstr.

 END.

END.
