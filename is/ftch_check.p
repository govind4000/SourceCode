 FOR EACH ftch_hist
     WHERE ftch_hist.ftch_domain     EQ "US"     AND 
               ftch_hist.ftch_serial     EQ "43000265"          AND
                         ftch_hist.ftch_eff_date   GE TODAY - 30  AND
                                   ftch_hist.ftch_eff_date   LE TODAY + 30
                                       
                                           BREAK BY ftch_hist.ftch_serial
                                                     BY ftch_hist.ftch_eff_date:
          disp ftch_domain ftch_hist.ftch_serial ftch_hist.ftch_eng_code ftch_hist.ftch_trans 
          ftch_ent_date ftch_eff_date.