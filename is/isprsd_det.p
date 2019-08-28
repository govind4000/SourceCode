def var lcPress like isprsd_prs_id init '47100459'.
def var llUpdate as logical label "Update".
def var llDuplicate as logical label "Duplicate".
def var lldel as logical label "Del".


repeat :

update lcPress llUpdate llDuplicate.


for each isprsd_det where isprsd_prs_id = lcPress 
    :
 
    disp isprsd_eu_nbr isprsd_start_date.
    disp isprsd_end_date.
    disp isprsd_mfg_start_date.

    if llUpdate then
      update isprsd_start_date isprsd_end_date isprsd_mfg_start_date.
    
    if llDuplicate
    then do:
      update lldel.
      if lldel then delete isprsd_det.
    end.
   
end. /*isprsd_det*/   

end.  /*repeat*/


/*Check ISB*/
/*************************************
for each isb_mstr where isb_serial = '41000198' no-lock :
   disp isb_serial isb_eu_nbr isb_ins_date isb_domain.

for each ftch_hist no-lock where  ftch_serial= "50000209" and ftch_eff_date >= (today - 31) /*and (ftch_trans = "4" or
ftch_trans = "2")*/  :
disp ftch_trans ftch_eff_date ftch_eng_code ftch_serial ftch_trnbr ftch_eff_date
ftch_prior_value
.

*************************************/

