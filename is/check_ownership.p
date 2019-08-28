def var lcPressId as char init "48000523".
def var lcEnduser as char.
def var ldStart  as date.
def var ldEnd   as date.
def var liCount as int.

update lcPressId.
 for each isprsd_det no-lock where isprsd_prs_id = lcPressId break by isprsd_prs_id :

   
    if first-of(isprsd_prs_id)
    then do:
      assign lcEndUser = ""
             ldStart = ?
             ldEnd = ?
             liCount = 0.
    end.

    if isprsd_eu_nbr = lcEndUser then liCount = liCount + 1.
     disp isprsd_prs_id isprsd_eu_nbr lcEndUser liCount.
    assign lcEndUser = isprsd_eu_nbr.

    if last-of(isprsd_prs_id)
    then do:
      if liCount gt 0 then
       disp isprsd_prs_id liCount.
    end.
 end.