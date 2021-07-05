(**************************************************************************)
(*                                                                        *)
(*  Utility to search drives for files                                    *)
(*  Copyright (C) 2021   Peter Moylan                                     *)
(*                                                                        *)
(*  This program is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 3 of the License, or     *)
(*  (at your option) any later version.                                   *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>. *)
(*                                                                        *)
(*  To contact author:   http://www.pmoylan.org   peter@pmoylan.org       *)
(*                                                                        *)
(**************************************************************************)

(************************************************************************)
(* FAILURE CASES:                                                       *)
(*  All known failure cases now work.                                   *)
(************************************************************************)

MODULE findr;

        (********************************************************)
        (*                                                      *)
        (*                 File find utility                    *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            24 February 2019                *)
        (*  Last edited:        4 July 2021                     *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

IMPORT OS2, TextIO, Strings;

FROM SYSTEM IMPORT
    (* type *)  ADDRESS, LOC,
    (* proc *)  ADR, ADDADR;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM FRV IMPORT
    (* const*)  version;

FROM FWildCard IMPORT
    (* proc *)  FSubstringMatch;

FROM ProgramArgs IMPORT
    (* proc *)  ArgChan, IsArgPresent;

FROM CtrlC IMPORT
    (* proc *)  SetBreakHandler;

FROM IOChan IMPORT
    (* type *)  ChanId;

FROM STextIO IMPORT
    (* proc *)  WriteChar, WriteString, WriteLn;

FROM TimeConv IMPORT
    (* proc *)  time;

(************************************************************************)

CONST
    Nul = CHR(0);  LF = CHR(10);  CR = CHR(13);
    testing = FALSE;
    (*TestArgs = "d:\dev*\*\src\progname.*";*)
    (*TestArgs = "c:\*.exe";*)
    (*TestArgs = "d:\*\*.mod";*)
    (*TestArgs = 'd:\*.mod "Maj*M?jor"';*)
    TestArgs = "D:\*.mod 'time ()'";

TYPE
    CharSet = SET OF CHAR;
    FilenameIndex = [0..259];
    FilenameString = ARRAY FilenameIndex OF CHAR;

    Qptr = POINTER TO
                RECORD
                    next: Qptr;
                    dir: FilenameString;
                END (*RECORD*);

    Queue = RECORD
                head, tail: Qptr;
            END (*RECORD*);

    (* This next list type is used only for testing. *)

    HandleList = POINTER TO
                    RECORD
                        next: HandleList;
                        this: CARDINAL;
                        count: CARDINAL;
                    END (*RECORD*);

VAR
    (* Default parameter string if not supplied. *)

    DefaultArgs: FilenameString;

    (* List of handles that were used, while testing. *)

    HList: HandleList;

    (* Flag to trigger program termination. *)

    Shutdown: BOOLEAN;

(************************************************************************)
(*                             MISCELLANEOUS                            *)
(************************************************************************)

PROCEDURE EVAL (dummy: ARRAY OF LOC);

    (* A do-nothing procedure - we use it for evaluating a function and *)
    (* ignoring the result.                                             *)

    BEGIN
    END EVAL;

(************************************************************************)

PROCEDURE ["C"] CtrlCHandler(): BOOLEAN;

    (* Called when Ctrl/C detected.  Sets a flag to abort search.  *)

    BEGIN
        Shutdown := TRUE;
        RETURN TRUE;
    END CtrlCHandler;

(************************************************************************)

PROCEDURE WriteCard (N: CARDINAL);

    (* Writes N in decimal to standard output. *)

    BEGIN
        IF N > 9 THEN
            WriteCard (N DIV 10);
            N := N MOD 10;
        END (*IF*);
        WriteChar (CHR(ORD('0') + N));
    END WriteCard;

(************************************************************************)

PROCEDURE WriteHexG (N, bits: CARDINAL);

    (* Writes N in hex to standard output.  There are no guarantees if  *)
    (* bits is not a multiple of 4.                                     *)

    BEGIN
        IF bits > 4 THEN
            WriteHexG (N DIV 16, bits-4);
            N := N MOD 16;
        END (*IF*);
        IF N > 9 THEN
            WriteChar (CHR(ORD('A')+N-10));
        ELSE
            WriteChar (CHR(ORD('0')+N));
        END (*IF*);
    END WriteHexG;

(************************************************************************)

PROCEDURE WriteHex32 (N: CARDINAL);

    (* Writes N in hex to standard output. *)

    BEGIN
        WriteHexG (N, 32);
    END WriteHex32;

(************************************************************************)

PROCEDURE AddToHandleList (handle: CARDINAL);

    VAR p, prev: HandleList;

    BEGIN
        prev := NIL;
        p := HList;
        LOOP
            IF p = NIL THEN
                EXIT (*LOOP*);
            ELSIF p^.this = handle THEN
                INC (p^.count);
                RETURN;
            ELSE
                prev := p;
                p := p^.next;
            END (*IF*);
        END (*LOOP*);

        (* Fell out of loop without finding a match. *)

        NEW (p);
        p^.count := 1;
        p^.this := handle;
        p^.next := NIL;
        IF prev = NIL THEN
            HList := p;
        ELSE
            prev^.next := p;
        END (*IF*);

    END AddToHandleList;

(************************************************************************)
(*                        COMMAND LINE PARAMETERS                       *)
(************************************************************************)

PROCEDURE GetParameters (VAR (*OUT*) mask: FilenameString;
                            VAR (*OUT*) text: ARRAY OF CHAR);

    (* Picks up program arguments from the command line.  *)

    CONST maxstringindex = 1023;

    VAR j: CARDINAL;
        argstring: ARRAY [0..maxstringindex] OF CHAR;

    (********************************************************************)

    PROCEDURE SkipBlanks;

        BEGIN
            LOOP
                IF argstring[j] <> ' ' THEN EXIT(*LOOP*) END(*IF*);
                IF j = maxstringindex THEN
                    argstring[j] := CHR(0);  EXIT (*LOOP*);
                ELSE
                    INC (j);
                END (*IF*);
            END (*LOOP*);
        END SkipBlanks;

    (********************************************************************)

    PROCEDURE GetString (Stoppers: CharSet;  VAR (*OUT*) result: ARRAY OF CHAR);

        VAR k: CARDINAL;

        BEGIN
            k := 0;
            WHILE (j <= maxstringindex) AND NOT (argstring[j] IN Stoppers) DO
                result[k] := argstring[j];
                INC(j);  INC(k);
            END (*WHILE*);
            result[k] := Nul;
        END GetString;

    (********************************************************************)

    PROCEDURE GetTextArg (VAR (*OUT*) result: ARRAY OF CHAR);

        (* Gets a string optionally enclosed in quotation marks. *)

        VAR Stoppers: CharSet;  ch, quotechar: CHAR;

        BEGIN
            SkipBlanks;
            Stoppers := CharSet {Nul, ' ', CR, LF};
            quotechar := Nul;
            ch := argstring[j];
            IF (ch = '"') OR (ch = "'") THEN
                INCL (Stoppers, ch);  quotechar := ch;  INC(j);
                EXCL (Stoppers, ' ');
            END (*IF*);
            GetString (Stoppers, result);
            IF quotechar <> Nul THEN
                IF argstring[j] = quotechar THEN
                    INC (j);
                END (*IF*);
            END (*IF*);
        END GetTextArg;

    (********************************************************************)

    VAR args: ChanId;

    BEGIN
        mask[0] := Nul;  text[0] := Nul;
        args := ArgChan();
        IF IsArgPresent() THEN
            TextIO.ReadString (args, argstring);
        END (*IF*);
        IF argstring[0] = Nul THEN
            Strings.Assign (DefaultArgs, argstring);
        END (*IF*);
        j := 0;  SkipBlanks;
        GetTextArg (mask);
        SkipBlanks;
        GetTextArg (text);

    END GetParameters;

(************************************************************************)
(*                     SEARCHING FOR SUBDIRECTORIES                     *)
(*                                                                      *)
(* NOTE: The obvious recursive approach to walking through a directory  *)
(* tree can cause us to run out of file handles.  To get around that,   *)
(* we scan only one directory at a time, and keep all pending           *)
(* directories on a queue.                                              *)
(*                                                                      *)
(************************************************************************)

PROCEDURE CollectSubdirectories (VAR (*IN*) dir: FilenameString;
                                            mask: FilenameString) : Queue;

    (* Returns a list of all subdirectories of dir matching mask.   *)
    (* Non-recursive: we don't go deeper into the tree.             *)

    CONST
        DirSearch = 1035H;
        NBlocks = 64;
        ResultSize = NBlocks * SIZE(OS2.FILEFINDBUF3);

    VAR k, ulrc, numberofresults: CARDINAL;
        found, skip: BOOLEAN;
        p: Qptr;
        Q: Queue;
        pres0: ADDRESS;
        pres: POINTER TO OS2.FILEFINDBUF3;
        handle: OS2.HDIR;
        name, filespec: FilenameString;

    BEGIN
        Q.head := NIL;  Q.tail := NIL;
        ALLOCATE (pres0, ResultSize);

        Strings.Assign (dir, filespec);
        Strings.Append ('\', filespec);
        Strings.Append (mask, filespec);
        handle := OS2.HDIR_CREATE;
        numberofresults := NBlocks;
        REPEAT
            ulrc := OS2.DosFindFirst (filespec, handle, DirSearch,
                            pres0, ResultSize, numberofresults, OS2.FIL_STANDARD);
        UNTIL ulrc <> OS2.ERROR_INTERRUPT;
        AddToHandleList (handle);
        found := ulrc = 0;
        IF (NOT found) AND (ulrc <> 18) THEN
            WriteString ("DosFindFirst error ");  WriteCard (ulrc);
            WriteString (" while looking for ");  WriteString (filespec);
            WriteLn;
        END (*IF*);

        WHILE found AND NOT Shutdown DO

            pres := pres0;

            FOR k := 0 TO numberofresults-1 DO

                Strings.Assign (pres^.achName, name);
                pres := ADDADR (pres, pres^.oNextEntryOffset);

                (* Ignore the '.' and '..' entries. *)

                skip := (name[0] = '.') AND
                            ((name[1] = Nul) OR
                                    ((name[1] = '.') AND (name[2] = Nul)));
                IF NOT skip THEN
                    Strings.Assign (dir, filespec);
                    Strings.Append ('\', filespec);
                    Strings.Append (name, filespec);
                    NEW (p);
                    p^.next := NIL;
                    p^.dir := filespec;
                    IF Q.head = NIL THEN
                        Q.head := p;
                    ELSE
                        Q.tail^.next := p;
                    END (*IF*);
                    Q.tail := p;
                END (*IF*);
            END (*FOR*);

            (* Start the next search. *)

            numberofresults := NBlocks;
            REPEAT
                ulrc := OS2.DosFindNext (handle, pres0, ResultSize, numberofresults);
            UNTIL ulrc <> OS2.ERROR_INTERRUPT;
            found := ulrc = 0;
            IF (NOT found) AND (ulrc <> OS2.ERROR_NO_MORE_FILES) THEN
                WriteString ("DosFindNext error ");  WriteCard (ulrc);
                WriteString (" while looking for ");  WriteString (filespec);
                WriteLn;
            END (*IF*);

        END (*WHILE still collecting the subdirectories*);

        OS2.DosFindClose (handle);

        DEALLOCATE (pres0, ResultSize);
        RETURN Q;

    END CollectSubdirectories;

(************************************************************************)

PROCEDURE MakeDirectoryList (VAR (*IN*) dirspec: FilenameString) : Queue;

    (* Converts dirspec, which might contain wildcards, to a list of directories.  *)

    VAR p, qptr, next: Qptr;
        Q, Q1, Q2: Queue;
        slashpos, wildpos, qpos: CARDINAL;
        havetail, foundwild, foundq: BOOLEAN;
        part2: FilenameString;

    BEGIN
        Q.head := NIL;  Q.tail := NIL;
        IF Shutdown THEN
            RETURN Q;
        END (*IF*);

        (* Work out which of '*' and '?' (if any) comes first. *)

        Strings.FindNext ('*', dirspec, 0, foundwild, wildpos);
        Strings.FindNext ('?', dirspec, 0, foundq, qpos);
        IF foundq THEN
            IF foundwild THEN
                IF qpos < wildpos THEN wildpos := qpos END(*IF*);
            ELSE
                foundwild := TRUE;  wildpos := qpos;
            END (*IF*);
        END (*IF*);

        IF foundwild THEN

            (* Break dirspec at the first '\' after the first wildcard.  *)

            Strings.FindNext ('\', dirspec, wildpos+1, havetail, slashpos);
            IF havetail THEN

                (* This is the most complicated case.  We have to remove a      *)
                (* trailing part, expand the first part, and then put back the  *)
                (* trailing part.  And the trailing part might also contain     *)
                (* wildcards, so further expansions might be needed.            *)

                part2 := dirspec;
                dirspec[slashpos] := Nul;
                IF slashpos > 0 THEN
                    Strings.Delete (part2, 0, slashpos);
                END (*IF*);

                Q1 := MakeDirectoryList (dirspec);

                IF Shutdown THEN
                    WHILE Q1.head <> NIL DO
                        p := Q1.head;
                        Q1.head := p^.next;
                        DISPOSE (p);
                    END (*WHILE*);
                    Q1.tail := NIL;
                END (*IF*);

                (* For each entry in Q1, put the tail back and re-expand. *)

                qptr := Q1.head;
                WHILE qptr <> NIL DO
                    dirspec := qptr^.dir;
                    Strings.Append (part2, dirspec);
                    next := qptr^.next;
                    DISPOSE (qptr);
                    qptr := next;
                    Q2 := MakeDirectoryList (dirspec);
                    IF Q.head = NIL THEN
                        Q.head := Q2.head;
                    ELSE
                        Q.tail^.next := Q2.head;
                    END (*IF*);
                    Q.tail := Q2.tail;
                END (*WHILE*);

            ELSE
                (* We have to break down dirspec differently.  The wild part is *)
                (* the last element in the directory path.                      *)

                Strings.FindPrev ('\', dirspec, wildpos-1, havetail, slashpos);
                IF havetail THEN
                    part2 := dirspec;
                    dirspec[slashpos] := Nul;
                    Strings.Delete (part2, 0, slashpos+1);
                    Q := CollectSubdirectories (dirspec, part2);
                ELSE
                    WriteString ("UNEXPECTED: No '\' in ");
                    WriteString (dirspec);  WriteLn;
                END (*IF*);

            END (*IF*);

        ELSE

            (* The simplest case - no wildcards. *)

            NEW (p);
            p^.dir := dirspec;
            p^.next := NIL;
            Q.head := p;  Q.tail := p;

        END (*IF*);

        RETURN Q;

    END MakeDirectoryList;

(************************************************************************)
(*                         SEARCHING FOR FILES                          *)
(*                                                                      *)
(* NOTE: The obvious recursive approach to walking through a directory  *)
(* tree can cause us to run out of file handles.  To get around that,   *)
(* we scan only one directory at a time, and keep all pending           *)
(* directories on a queue.                                              *)
(*                                                                      *)
(************************************************************************)

PROCEDURE SearchForFiles (VAR (*INOUT*) Q: Queue;
                            VAR (*IN*) mask: FilenameString;
                              VAR (*IN*) text: ARRAY OF CHAR;
                                VAR (*OUT*) Nchecked: CARDINAL): CARDINAL;

    (* Searches all directories on Q for files matching mask, including *)
    (* any subdirectory trees that we encounter.  Returns number of     *)
    (* matches.  Parameter Nchecked returns the number of non-directory *)
    (* files that were checked.  Assumption: all directories on Q are   *)
    (* fully specified, without wildcards.  On return, Q is empty.      *)

    CONST
        NonDirSearch = 25H;
        DirSearch = 1035H;
        NBlocks = 64;
        ResultSize = NBlocks * SIZE(OS2.FILEFINDBUF3);

    VAR k, count, ulrc, numberofresults: CARDINAL;
        found, textsearch: BOOLEAN;
        p: Qptr;
        pres0: ADDRESS;
        pres: POINTER TO OS2.FILEFINDBUF3;
        subQ: Queue;
        handle: OS2.HDIR;
        dirname, filespec: FilenameString;

    BEGIN
        textsearch := text[0] <> Nul;
        ALLOCATE (pres0, ResultSize);
        Nchecked := 0;
        count := 0;
        WHILE (Q.head <> NIL) AND NOT Shutdown DO

            (* Take the first element from the queue. *)

            p := Q.head;
            dirname := p^.dir;
            Q.head := p^.next;
            DISPOSE (p);
            IF Q.head = NIL THEN Q.tail := NIL END(*IF*);

            (* It is possible for a non-existent directory to get on    *)
            (* the queue, and of course we don't want to waste our time *)
            (* on that case.  For simplicity, though, we skip this test *)
            (* if dirname is just a drive specification.                *)

            IF dirname[2] = Nul THEN
                ulrc := 0;
            ELSE
                handle := OS2.HDIR_CREATE;
                numberofresults := 1;
                REPEAT
                    ulrc := OS2.DosFindFirst (dirname, handle, DirSearch,
                                            pres0, ResultSize, numberofresults, OS2.FIL_STANDARD);
                UNTIL ulrc <> OS2.ERROR_INTERRUPT;
                AddToHandleList (handle);
                OS2.DosFindClose (handle);
            END (*IF*);

            IF ulrc = 0 THEN

                (* Directory exists. *)

                Strings.Assign (dirname, filespec);
                Strings.Append ('\', filespec);
                Strings.Append (mask, filespec);

                (************************************)
                (*  FIND FILES IN THIS DIRECTORY    *)
                (************************************)

                (* First go through the non-directory files in this directory. *)

                handle := OS2.HDIR_CREATE;
                numberofresults := NBlocks;
                REPEAT
                    ulrc := OS2.DosFindFirst (filespec, handle, NonDirSearch,
                                        pres0, ResultSize, numberofresults, OS2.FIL_STANDARD);
                UNTIL ulrc <> OS2.ERROR_INTERRUPT;
                AddToHandleList (handle);
                found := ulrc = 0;
                IF (NOT found) AND (ulrc <> 18) THEN
                    WriteString ("DosFindFirst error ");  WriteCard (ulrc);
                    WriteString (" while looking for ");  WriteString (filespec);
                    WriteLn;
                END (*IF*);

                WHILE found AND NOT Shutdown DO

                    INC (Nchecked, numberofresults);
                    pres := pres0;

                    FOR k := 0 TO numberofresults-1 DO
                        filespec := dirname;  Strings.Append ('\', filespec);
                        Strings.Append (pres^.achName, filespec);
                        IF textsearch THEN
                            found := FSubstringMatch (filespec, text);
                        END (*IF*);
                        IF found THEN

                            INC (count);

                            (* Write out one result. *)

                            WriteString (filespec);  WriteLn;

                        END (*IF*);
                        pres := ADDADR (pres, pres^.oNextEntryOffset);
                    END (*FOR*);
                    numberofresults := NBlocks;
                    REPEAT
                        ulrc := OS2.DosFindNext (handle, pres0, ResultSize, numberofresults);
                    UNTIL ulrc <> OS2.ERROR_INTERRUPT;
                    found := ulrc = 0;
                    IF (NOT found) AND (ulrc <> 18) THEN
                        WriteString ("DosFindNext error ");  WriteCard (ulrc);
                        WriteString (" while looking for ");  WriteString (filespec);
                        WriteLn;
                    END (*IF*);
                END (*WHILE*);
                OS2.DosFindClose (handle);

                (***********************************************)
                (*  ADD SUBDIRECTORIES TO THE DIRECTORY QUEUE  *)
                (***********************************************)

                (* Put the subdirectories onto the queue. *)

                subQ := CollectSubdirectories (dirname, '*');

                (* In order to report the results in the desired order, put the *)
                (* subdirectory queue at the HEAD of the main queue.            *)

                IF subQ.head <> NIL THEN
                    subQ.tail^.next := Q.head;
                    Q.head := subQ.head;
                    IF Q.tail = NIL THEN Q.tail := subQ.tail END (*IF*);
                END (*IF*);

            END (*IF directory exists*);

        END (*WHILE main queue is not empty*);

        (* Special case: Q might be nonempty if Shutdown = TRUE. *)

        WHILE Q.head <> NIL DO
            p := Q.head;
            Q.head := p^.next;
            DISPOSE (p);
        END (*WHILE*);
        Q.tail := NIL;

        DEALLOCATE (pres0, ResultSize);
        RETURN count;

    END SearchForFiles;

(************************************************************************)
(*                   THE OVERALL 'FIND' OPERATION                       *)
(************************************************************************)

PROCEDURE FindAll (VAR (*IN*) mask: FilenameString;
                               VAR (*IN*) texttofind: ARRAY OF CHAR;
                                VAR (*OUT*) Nchecked: CARDINAL): CARDINAL;

    (* List all files matching mask, and containing the specified text. *)
    (* If texttofind is the empty string, we just look for the file     *)
    (* match and not the text match.  On return Nchecked is the total   *)
    (* number of non-directory files checked, and the function return   *)
    (* value is the total number of matches.                            *)

    VAR pos, count: CARDINAL;
        found: BOOLEAN;
        Q: Queue;
        dirspec: FilenameString;

    BEGIN
        (* Split off the "directory" part of mask. *)

        Strings.FindPrev ('\', mask, HIGH(mask), found, pos);
        IF found THEN
            dirspec := mask;
            dirspec[pos] := Nul;
            Strings.Delete (mask, 0, pos+1);
        ELSE
            dirspec[0] := Nul;
        END (*IF*);

        IF mask[0] = Nul THEN
            WriteString ("You haven't specified anything to search for.");
            WriteLn;
            RETURN 0;
        END (*IF*);

        WriteString ("Searching ");  WriteString (dirspec);
        WriteString (" for ");  WriteString (mask);
        IF texttofind[0] <> Nul THEN
            WriteString (' and text "');  WriteString (texttofind);
            WriteString ('"');
        END (*IF*);
        WriteLn;

        Q := MakeDirectoryList (dirspec);
        count := SearchForFiles (Q, mask, texttofind, Nchecked);
        RETURN count;

    END FindAll;

(************************************************************************)
(*                     CLEAN UP A FILE SPECIFICATION                    *)
(************************************************************************)

PROCEDURE NormaliseFilespec (VAR (*INOUT*) filespec: FilenameString);

    (* Makes sure that the filespec is an absolute one including drive. *)
    (* Defaults to current drive and current directory if needed to     *)
    (* deal with a relative specification.  Also changes all '/' to '\'.*)

    VAR j, disknum, map: CARDINAL;
        CurrentDir: FilenameString;

    BEGIN
        FOR j := 0 TO LENGTH(filespec)-1 DO
            IF filespec[j] = '/' THEN filespec[j] := '\' END(*IF*);
        END (*FOR*);

        (* Insert drive letter if needed. *)

        IF filespec[1] = ':' THEN
            IF filespec[0] > 'Z' THEN DEC(filespec[0], ORD('a')-ORD('A')) END(*IF*);
        ELSE
            OS2.DosQueryCurrentDisk (disknum, map);
            Strings.Insert ('C:', 0, filespec);
            filespec[0] := CHR(ORD('A') - 1 + disknum);
        END (*IF*);

        (* Change relative to absolute path if needed. *)

        IF filespec[2] <> '\' THEN
            disknum := ORD(filespec[0]) - ORD('A') + 1;
            j := SIZE(FilenameString);
            OS2.DosQueryCurrentDir (disknum, CurrentDir, j);
            Strings.Insert ('\', 2, filespec);
            IF CurrentDir[0] <> Nul THEN
                Strings.Insert ('\', 3, filespec);
                Strings.Insert (CurrentDir, 3, filespec);
            END (*IF*);
        END (*IF*);

    END NormaliseFilespec;

(************************************************************************)
(*                          INCREASE FILE HANDLES                       *)
(************************************************************************)

(*
PROCEDURE IncreaseFileHandles;

    (* Adds some more file handles to the process.      *)
    (* Probably not needed now that I reuse handles.    *)

    VAR cbReqCount: OS2.LONG;  cbCurMaxFH: OS2.ULONG;

    BEGIN
        cbReqCount := 128;
        OS2.DosSetRelMaxFH (cbReqCount, cbCurMaxFH);
    END IncreaseFileHandles;
*)

(************************************************************************)
(*                          EXPLAIN PARAMETERS                          *)
(************************************************************************)

PROCEDURE ExplainUsage;

    (* Puts an explanatory message to standard output. *)

    BEGIN
        WriteString ("Usage: findr mask [text]");  WriteLn;
        WriteString ("where");  WriteLn;
        WriteString ("    mask is a filename string that may include wildcards '*' and '?'");  WriteLn;
        WriteString ("    text is an optional text string to search for,");  WriteLn;
        WriteString ("        which may also contain wildcards");  WriteLn;
        WriteString ("If either argument contains spaces then it must be delimited");  WriteLn;
        WriteString ("   by single or double quote marks");  WriteLn;
        WriteString ("Example 1:    findr c:\*.prj");  WriteLn;
        WriteString ("Example 2:    findr d:\dev*\*\src\progname.* 'version string'");  WriteLn;
    END ExplainUsage;

(************************************************************************)
(*                             MAIN PROGRAM                             *)
(************************************************************************)

PROCEDURE DoTheJob;

    VAR mask: FilenameString;
        text: ARRAY [0..511] OF CHAR;
        Nchecked, count, t1, t2: CARDINAL;

    BEGIN
        WriteString ("findr version ");  WriteString(version);
        WriteLn;
        GetParameters (mask, text);
        IF mask[0] = Nul THEN
            ExplainUsage;
        ELSE
            NormaliseFilespec (mask);
            t1 := time();
            count := FindAll (mask, text, Nchecked);
            t2 := time();
            IF text[0] <> Nul THEN
                WriteCard (Nchecked);
                WriteString (" files checked, ");
            END (*IF*);
            WriteCard (count);
            WriteString (" files matched, ");
            WriteCard (t2-t1);
            WriteString (" seconds.");
            WriteLn;
        END (*IF*);
    END DoTheJob;

(************************************************************************)

PROCEDURE DumpHandleList;

    (* Dumps the list of handles used by this program. *)

    VAR next: HandleList;

    BEGIN
        WriteString ("Handles used by this program");  WriteLn;
        WHILE HList <> NIL DO
            next := HList^.next;
            WriteHex32 (HList^.this);  WriteString ("  ");
            WriteCard (HList^.count);  WriteString (" times");  WriteLn;
            DISPOSE (HList);
            HList := next;
        END (*WHILE*);
        WriteLn;
    END DumpHandleList;

(************************************************************************)

BEGIN
    Shutdown := FALSE;
    HList := NIL;
    EVAL (SetBreakHandler(CtrlCHandler));
    IF testing THEN
        DefaultArgs := TestArgs;
    ELSE
        DefaultArgs := "";
    END (*IF*);
    DoTheJob;
FINALLY
    IF HList <> NIL THEN
        DumpHandleList;
    END (*IF*);
    IF Shutdown THEN
        WriteString ("Search terminated by Ctrl/C");  WriteLn;
    END (*IF*);
END findr.

