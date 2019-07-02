/* zipping up a ffind distribution */

'del ffind*.zip'
'xc =p ffind.prj'
'\apps\lxlite\lxlite ffind.exe'

ver = version()
call bldlvl ver

mkdir temp
cd temp
'copy ..\README'
'copy ..\ffind.exe'
'copy ..\ffind.ico'
'copy ..\makezip.cmd'
'copy ..\bldlvl.cmd'
'copy ..\version.cmd'

/* Add in the source files. */

'mkdir source'
'cd source'
'copy D:\Dev1\General\doc\gpl.txt'
'mkdir def'
'copy ..\..\def\*.def def'
'mkdir mod'
'copy ..\..\src\*.mod mod'
'cd ..'

/* zip it all up. */

'zip -r ..\ffind_'ver'.zip .'
'cd ..'
'call deltree /Y temp >nul'           /* deltree3.zip from Hobbes */

