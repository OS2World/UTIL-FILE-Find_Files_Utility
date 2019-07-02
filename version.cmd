/*----------------------------------------------------------
   Returns the version number of ffind.

           Author:       Peter Moylan
           Last revised: 14 April 2019

   Usage:
           ver = version()

           (Run this from the ffind top-level directory)

------------------------------------------------------------*/

DEFFile = "DEF\FFV.def"

DO FOREVER
    IF lines(DEFFile) != 1 THEN LEAVE
    parse value linein(DEFFile) with kwd'='val
    kwd = STRIP(kwd)
    IF kwd = "version" THEN LEAVE
END

/* Extra the part of val inside double quotes. */

PARSE VALUE val WITH v1 '"' version '"' v2

CALL STREAM DEFFile, 'C', 'CLOSE'
RETURN version

exit

