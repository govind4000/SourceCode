DEF VAR x AS CHAR.
DEF VAR i AS INTEGER.

DO i = 1 TO NUM-DBS:
x = LDBNAME (i).
CREATE ALIAS "DICTDB" FOR DATABASE VALUE (x).
DISPLAY x LABEL "Database name: ".
RUN getTableDF1.p.
END