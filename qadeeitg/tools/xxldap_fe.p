DEFINE VARIABLE lcPropath AS CHARACTER NO-UNDO.

ASSIGN lcPropath = PROPATH.
PROPATH="/opt/qadee/local/bin/admin/maint/src," + PROPATH.

RUN /home/qadeeitg/tools/xxldap.p.

ASSIGN PROPATH = lcPropath.
