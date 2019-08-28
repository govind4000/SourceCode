def var lcPress like isprs_id init '51000151'.


repeat :

update lcPress.


for each isprs_mstr 
    where isprs_id = lcPress no-lock,
     each isprsd_det where isprsd_prs_id = isprs_id :
    
 find first iseu_mstr where iseu_nbr = isprsd_eu_nbr no-lock no-error. 
 find last isprt_evt where isprt_prs_id = isprs_id no-lock no-error.
   
   disp isprs_id isprs_stat isprsd_eu_nbr 
        iseu_cm_nbr when avail iseu_mstr        isprsd_start isprsd_end
        isprt_date when avail isprt_evt label "isprt_date" .

end. /*isprs_mstr*/   

end.  /*repeat*/