/* zipping up a findr distribution */

'del findr*.zip'
'xc =p findr.prj'
'\apps\lxlite\lxlite findr.exe'

/* Create the sym and xqs files. */

'call PerlEnv.cmd'
perl 'D:\Apps\scripts\makexqs.pl' findr.map
say "findr.sym and findr.xqs should now exist"

/* zip up the source files. */

del src.zip
'Imports Findr | zip -j -u src.zip -@'

/* Version and build level. */

ver = version()
call bldlvl ver

mkdir temp
cd temp
'copy ..\README'
'copy ..\findr.exe'
'copy ..\findr.ico'
mkdir build
'copy ..\makezip.cmd build'
'copy ..\bldlvl.cmd build'
'copy ..\version.cmd build'

/* Add in the source files. */

'mkdir source'
'cd source'
'copy D:\Dev1\General\doc\gpl.txt'
'copy ..\..\src.zip'
'unzip src.zip'
'del src.zip'
CALL SortSrc
'cd ..'

/* zip it all up. */

'zip -r ..\findr_'ver'.zip .'
'cd ..'
'call deltree /Y temp >nul'           /* deltree3.zip from Hobbes */

EXIT 0

/****************************************************************/
/*                  SORTING THE SOURCE FILES                    */
/****************************************************************/

SortSrc: PROCEDURE

    /* Moves the files in the current directory to              */
    /* subdirectories that more accurately reflect the original */
    /* source directories. This is not a perfect sort, since    */
    /* the files came from several top-level directories, but   */
    /* it gives a more readable result.                         */

    rc = SysMkDir('DEF')
    rc = SysMkDir('SRC')
    rc = SysFileTree('*', tree, 'O')
    DO j = 1 TO tree.0
        fname = tree.j
        tail = TRANSLATE(RIGHT(fname, 4))
        IF tail = '.DEF' THEN
            '@move 'fname' DEF 1>nul'
        ELSE IF tail = '.MOD' THEN
            '@move 'fname' SRC 1>nul'
    END
    RETURN


