DEF VAR lhApp AS HANDLE.

CREATE SERVER lhApp.
MESSAGE 
lhApp:CONNECT
("-H qadee-pro.houston.hpicorp.net  -AppService is_prd_main_as -S 20002 -ssl",
"","")
/*
("-H qadee-prd.austin.hpicorp.net  -AppService qad_prd_qadui_as -S 20002 -ssl",
"","")
  */
/*
("-H g2u2391c.austin.hpicorp.net -AppService is_prd_main_as -S 20002",
"","")

("-AppService qad_prd_qadui_as -DirectConnect -H qadee-prd.houston.hpicorp.net -S 24562 -ssl",
"","")
*/
