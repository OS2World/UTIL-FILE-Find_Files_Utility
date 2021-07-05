/*----------------------------------------------------------
   Appends a build level to FFIND.EXE

           Author:       Peter Moylan
           Last revised: 14 April 2019

   Usage:
           bldlvl ver

           where ver is the version string

------------------------------------------------------------*/

parse arg ver
projHost = "PJM3"
timestamp = LEFT(DATE() TIME(),25)LEFT(projHost,10)
signature0 = "@#Peter Moylan:"ver"#@##1## "timestamp"::EN:AU:::@@"
outfile = "level.txt"
"DEL "outfile" 1> nul 2> nul"
CALL LINEOUT outfile, signature0||"File find utility for OS/2 and eCS"
CALL STREAM outfile,'C','CLOSE'
"@copy findr.exe /B + level.txt findr.exe /B >nul"

"@DEL "outfile

exit

