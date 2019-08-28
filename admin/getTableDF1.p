/* PROGRAM2.P: get all the tables/fields name */

FOR EACH dictdb._field, EACH dictdb._file
WHERE RECID (_file) = _field._fil BREAK BY (_file-name):

IF SUBSTRING (_file-name,1,1) = "_" THEN NEXT.
ELSE DO:
IF FIRST-OF (_file-name) THEN
DISPLAY _file-name FORMAT "X(20)" _field-name.
ELSE DISPLAY _field-name.
END.
END.