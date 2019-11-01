{xxqldap.i &ldapReturnFields = "cn uid employeeNumber l c telephoneNumber hpStatus hpTerminationDate ntUserDomainId hpBusinessGroup hpBusinessGroupCode hpBusinessRegion hpBusinessRegionAcronym hpBusinessUnit hpBusinessUnitAcronym hpJobFamily hpJobFunction manager managerEmployeeNumber"}

PAUSE 0 BEFORE-HIDE.
DEFINE STREAM stOut.

OUTPUT STREAM stOut TO ldap.xls UNBUFFERED.

PUT STREAM stOut UNFORMATTED
    "usr_userid"                CHR(9)
    "usr_name"                  CHR(9)
    "usr_mail_address"          CHR(9)
    "usr_remark"                CHR(9)
    "usr_active"                CHR(9)
    "cn"                        CHR(9)
    "uid"                       CHR(9)
    "employeeNumber"            CHR(9)
    "telephoneNumber"           CHR(9)
    "l"                         CHR(9)
    "c"                         CHR(9)
    "hpStatus"                  CHR(9)
    "hpTerminationDate"         CHR(9)
    "ntUserDomainId"            CHR(9)
    "hpBusinessGroup"           CHR(9)
    "hpBusinessGroupCode"       CHR(9)
    "hpBusinessRegion"          CHR(9)
    "hpBusinessRegionAcronym"   CHR(9)
    "hpBusinessUnit"            CHR(9)
    "hpBusinessUnitAcronym"     CHR(9)
    "hpJobFamily"               CHR(9)
    "hpJobFunction"             CHR(9)
    "manager"                   CHR(9)
    "managerEmployeeNumber"     CHR(9)
    CHR(10)
    .
FOR EACH usr_mstr
    WHERE /*usr_mstr.usr_active EQ TRUE AND */
          usr_mstr.usr_name   NE ""
    NO-LOCK:
   ldapQuery(SUBSTITUTE("mail=&1",usr_mstr.usr_mail_address)).
  DISPLAY ldapAttributeValue(1,"uid") @ usr_mstr.usr_mail_address
          WITH FRAME frProgress 1 DOWN.
  PUT STREAM stOut UNFORMATTED
      usr_mstr.usr_userid        CHR(9)
      usr_mstr.usr_name          CHR(9)
      usr_mstr.usr_mail_address  CHR(9)
      usr_mstr.usr_remark        CHR(9)
      usr_mstr.usr_active        CHR(9)
      ldapAttributeValue(1,"cn") CHR(9)
      ldapAttributeValue(1,"uid") CHR(9)
      ldapAttributeValue(1,"employeeNumber") CHR(9)
      ldapAttributeValue(1,"telephoneNumber") CHR(9)
      ldapAttributeValue(1,"l") CHR(9)
      ldapAttributeValue(1,"c") CHR(9)
      ldapAttributeValue(1,"hpStatus") CHR(9)
      ldapAttributeValue(1,"hpTerminationDate") CHR(9)
      ldapAttributeValue(1,"ntUserDomainId") CHR(9)
      ldapAttributeValue(1,"hpBusinessGroup") CHR(9)
      ldapAttributeValue(1,"hpBusinessGroupCode") CHR(9)
      ldapAttributeValue(1,"hpBusinessRegion") CHR(9)
      ldapAttributeValue(1,"hpBusinessRegionAcronym") CHR(9)
      ldapAttributeValue(1,"hpBusinessUnit") CHR(9)
      ldapAttributeValue(1,"hpBusinessUnitAcronym") CHR(9)
      ldapAttributeValue(1,"hpJobFamily") CHR(9)
      ldapAttributeValue(1,"hpJobFunction") CHR(9)
      ldapAttributeValue(1,"manager") CHR(9)
      ldapAttributeValue(1,"managerEmployeeNumber")
      CHR(10).
END.
OUTPUT STREAM stOut CLOSE.

