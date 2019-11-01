DEF VAR lhApp AS HANDLE.

CREATE SERVER lhApp.
MESSAGE 
lhApp:CONNECT
("-H qadee-itg.austin.hpicorp.net  -AppService is_itg_main_as -S 20002 -ssl",
"","")
/*
("-H qadee-itg.austin.hpicorp.net  -AppService qad_itg_qadui_as -S 20002 -ssl",
"","")
  */
/*
("-H g2u2391c.austin.hpicorp.net -AppService is_itg_main_as -S 20002",
"","")

("-AppService qad_itg_qadui_as -DirectConnect -H qadee-itg.austin.hpicorp.net -S 24562 -ssl",
"","")
*/
