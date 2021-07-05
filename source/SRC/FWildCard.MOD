(**************************************************************************)
(*                                                                        *)
(*  PMOS/2 software library                                               *)
(*  Copyright (C) 2019   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE FWildCard;

        (********************************************************)
        (*                                                      *)
        (*            Wildcard search within a file             *)
        (*                                                      *)
        (*       This version does left-to-right matching,      *)
        (*      i.e. it doesn't work inwards from both ends     *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            19 November 2019                *)
        (*  Last edited:        23 December 2019                *)
        (*  Status:             Seems to be working             *)
        (*                                                      *)
        (********************************************************)


IMPORT WildCard, Strings;

FROM Arith64 IMPORT
    (* const*)  Zero64,
    (* type *)  CARD64,
    (* proc *)  Add64, ToAPI64;

FROM FileOps IMPORT
    (* type *)  ChanId,
    (* proc *)  GetFileSize, OpenOldFile, CloseFile, SetPosition, ReadRaw;

FROM MiscFuncs IMPORT
    (* type *)  CharArrayPointer;

FROM LowLevel IMPORT
    (* proc *)  Copy;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0);
    BufSize = 8191;

TYPE
    BufArray = ARRAY [0..BufSize] OF CHAR;

    (* Note that the buffer array is longer by one character from what  *)
    (* we read into it.  This allows a terminating Nul that will stop   *)
    (* string searches from running off the end of the array.           *)

    Buffer = RECORD
                pval: POINTER TO BufArray;
                offset: CARD64;
                amount: CARDINAL;
             END (*RECORD*);

    (* Notation: input[j..] means a string that begins at pval^[j] in   *)
    (* a buffer, and continues to the end of the input file, only some  *)
    (* of which is in the buffer.                                       *)

    SearchFunc = PROCEDURE (VAR ARRAY OF CHAR, VAR ARRAY OF CHAR): BOOLEAN;

(************************************************************************)
(*                      RELOADING THE FILE BUFFER                       *)
(************************************************************************)

PROCEDURE ReloadBuffer (cid: ChanId;  VAR (*INOUT*) buff: Buffer);

    (* Steps forward in the file to reload buff.  *)

    BEGIN
        Add64 (buff.offset, BufSize);
        SetPosition (cid, ToAPI64(buff.offset));
        ReadRaw (cid, buff.pval^, BufSize, buff.amount);
        buff.pval^[buff.amount] := Nul;
    END ReloadBuffer;

(************************************************************************)

PROCEDURE Clone (VAR (*IN*) buff: Buffer): Buffer;

    (* Returns a copy of its argument, including duplication of the     *)
    (* data.  Copies are needed in situations where we might need to    *)
    (* back up to an earlier point in the data.                         *)

    VAR clone: Buffer;

    BEGIN
        clone := buff;
        IF buff.pval <> NIL THEN
            NEW (clone.pval);

            (* Copy one byte more than the obvious, for the trailing Nul. *)

            Copy (buff.pval, clone.pval, buff.amount + 1);

        END (*IF*);
        RETURN clone;
    END Clone;

(************************************************************************)
(*         THE CASE WHERE ALL OF THE INPUT FITS IN THE BUFFER           *)
(************************************************************************)

PROCEDURE InMemorySearch (f: SearchFunc;
                        buf: Buffer;  VAR (*IN*) template: ARRAY OF CHAR;
                                        jt, kt: CARDINAL): BOOLEAN;

    VAR pnewtemplate: CharArrayPointer;
        success: BOOLEAN;

    BEGIN
        ALLOCATE (pnewtemplate, kt+2);
        Strings.Assign (template, pnewtemplate^);
        pnewtemplate^[kt+1] := Nul;
        IF jt > 0 THEN
            Strings.Delete (pnewtemplate^, 0, jt);
        END (*IF*);
        success := f(buf.pval^, pnewtemplate^);
        DEALLOCATE (pnewtemplate, kt+2);
        RETURN success;
    END InMemorySearch;

(************************************************************************)
(*                   THE ACTUAL MATCHING PROCEDURES                     *)
(************************************************************************)

PROCEDURE Match (cid: ChanId;  buf: Buffer;
                          j: CARDINAL;
                          VAR (*IN*) template: ARRAY OF CHAR;
                          jt, kt: CARDINAL): BOOLEAN;
                                                                 FORWARD;

    (* Returns TRUE if input from buf[j] to end of file matches template[jt..kt].          *)

(************************************************************************)

PROCEDURE SubMatch (cid: ChanId;  buf: Buffer;
                                j: CARDINAL;
                                    VAR (*IN*) template: ARRAY OF CHAR;
                                                jt, kt: CARDINAL): BOOLEAN;
                                                                    FORWARD;

    (* Succeeds iff a substring of input matches template.      *)

(************************************************************************)

PROCEDURE HeadMatch (cid: ChanId;  buf: Buffer;
                                j: CARDINAL;
                                    VAR (*IN*) template: ARRAY OF CHAR;
                                                jt, kt: CARDINAL): BOOLEAN;

    (* Executive overview: succeeds if a LEADING substring of input     *)
    (* matches template.                                                *)

    (* The caller guarantees that the template is nonempty and that     *)
    (* template[jt] <> '*'.                                             *)

    (* Returns TRUE if input[j..k] matches template[jt..kt], where      *)
    (* j <= k and k does not necessarily correspond to end of file.     *)

    (* Might return with buf contents altered.                          *)

    BEGIN
        (* Check for special cases. *)

        IF jt > kt THEN
            (* Empty template, which matches a leading empty string. *)
            RETURN TRUE;
        ELSIF template[jt] = '*' THEN
            RETURN SubMatch (cid, buf, j, template, jt+1, kt);
        END (*IF*);

        LOOP
            (* Step through the template until we hit a '*' or we can   *)
            (* return with a definite success or failure.               *)

            IF jt > kt THEN

                (* Template all used up, so we have matched a leading substring. *)

                RETURN TRUE;

            ELSIF template[jt] = '*' THEN

                EXIT (*LOOP*);

            END (*IF*);

            IF j >= buf.amount THEN
                ReloadBuffer (cid, buf);
                j := 0;
            END (*IF*);

            IF buf.amount = 0 THEN

                (* Some template left but no input left. *)

                RETURN FALSE;

            ELSIF (template[jt] <> '?') AND (CAP(buf.pval^[j]) <> CAP(template[jt])) THEN

                RETURN FALSE;

            END (*IF*);

            (* If we have passed all of the above, we have matched a character. *)

            INC (j);  INC (jt);

        END (*LOOP*);

        (* If we reach this point, then jt <= kt and template[jt] = '*'.  *)

        IF jt = kt THEN
            RETURN TRUE;
        ELSE
            (* jt < kt and template[jt+1] <> '*'.  *)

            RETURN SubMatch (cid, buf, j, template, jt+1, kt);

        END (*IF*);

    END HeadMatch;

(************************************************************************)

PROCEDURE SubMatch (cid: ChanId;  buf: Buffer;
                                j: CARDINAL;
                                    VAR (*IN*) template: ARRAY OF CHAR;
                                                jt, kt: CARDINAL): BOOLEAN;

    (* Succeeds iff a substring of input matches template, i.e. if      *)
    (* input[m..k] matches template[jt..kt], where j <= m <= k.         *)

    (* Might return with buf contents altered.                          *)

    VAR newbuf: Buffer;  success: BOOLEAN;

    BEGIN
        newbuf.pval := NIL;
        IF j >= buf.amount THEN
            ReloadBuffer (cid, buf);
            IF buf.amount < BufSize THEN
                RETURN InMemorySearch (WildCard.SubstringMatch,
                                                buf, template, jt, kt);
            END (*IF*);
            j := 0;
        END (*IF*);

        IF jt > kt THEN
            (* Template is empty.  This matches an empty substring  *)
            (* of input.                                            *)
            RETURN TRUE;
        ELSIF buf.amount = 0 THEN
            (* Input is empty.  Match iff template = '*'. *)
            RETURN (kt = jt) AND (template[jt] = '*');
        END (*IF*);

        (* We now have a nonempty template and buffer. *)

        IF template[jt] = '*' THEN
            INC (jt);
        END (*IF*);
        IF template[kt] = '*' THEN
            IF kt = 0 THEN
                (* Empty template, so a match as above. *)
                RETURN TRUE;
            ELSE
                DEC (kt);
            END (*IF*);
        END (*IF*);

        (* Now template[jt] <> '*'.  *)

        WHILE buf.amount > 0 DO

            IF j >= buf.amount THEN
                ReloadBuffer (cid, buf);
                IF buf.amount < BufSize THEN

                    (* Switch to an in-memory search. *)

                    RETURN InMemorySearch (WildCard.SubstringMatch,
                                                    buf, template, jt, kt);

                END (*IF*);
                j := 0;
            END (*IF*);

            success := (template[jt] = '?') OR (CAP(buf.pval^[j]) = CAP(template[jt]));
            IF success THEN
                newbuf := Clone(buf);
                success := HeadMatch (cid, newbuf, j+1, template, jt+1, kt);
                IF newbuf.pval <> NIL THEN
                    DISPOSE (newbuf.pval);
                END (*IF*);
            END (*IF*);
            IF success THEN
                RETURN TRUE;
            ELSE
                INC (j);
            END (*IF*);

        END (*WHILE*);

        (* On loop exit, input is exhausted but template is still       *)
        (* nonempty and does not start with '*', so failure.            *)

        RETURN FALSE;

    END SubMatch;

(************************************************************************)

PROCEDURE TailMatch (cid: ChanId;  buf: Buffer;
                                j: CARDINAL;
                                    VAR (*IN*) template: ARRAY OF CHAR;
                                                jt, kt: CARDINAL): BOOLEAN;

    (* Executive overview: succeeds if a TRAILING substring of input    *)
    (* matches template.                                                *)

    (* The caller guarantees that the template is nonempty and that     *)
    (* template[jt] <> '*'.                                             *)

    (* Returns TRUE if input[m..] matches template[jt..kt], where       *)
    (* m >= j.                                                          *)

    (* Might return with buf contents altered.                          *)

    VAR newbuf: Buffer;  success: BOOLEAN;

    BEGIN
        (* Check for special cases. *)

        IF jt > kt THEN
            (* Empty template, which will always match anything. *)
            RETURN TRUE;
        ELSIF template[kt] = '*' THEN
            RETURN SubMatch (cid, buf, j, template, jt, kt-1);
        END (*IF*);

        newbuf.pval := NIL;
        LOOP
            IF j >= buf.amount THEN
                ReloadBuffer (cid, buf);
                j := 0;
            END (*IF*);

            IF buf.amount = 0 THEN
                RETURN FALSE;
            ELSE
                success := (template[jt] = '?') OR (CAP(buf.pval^[j]) = CAP(template[jt]));
                IF success THEN
                    newbuf := Clone(buf);
                    success := Match (cid, newbuf, j+1, template, jt+1, kt);
                    IF newbuf.pval <> NIL THEN
                        DISPOSE (newbuf.pval);
                    END (*IF*)
                END (*IF*);
            END (*IF*);
            IF success THEN
                RETURN TRUE;
            ELSE
                INC (j);
            END (*IF*);
        END (*LOOP*);

    END TailMatch;

(************************************************************************)

PROCEDURE Match (cid: ChanId;  buf: Buffer;
                          j: CARDINAL;
                          VAR (*IN*) template: ARRAY OF CHAR;
                          jt, kt: CARDINAL): BOOLEAN;

    (* Returns TRUE if input[j..] matches template[jt..kt].  *)

    (* Might return with buf contents altered.               *)

    BEGIN
        IF j >= buf.amount THEN

            (* Get a new buffer-full of input. *)

            ReloadBuffer (cid, buf);
            IF buf.amount < BufSize THEN
                RETURN InMemorySearch (WildCard.WildMatch,
                                                buf, template, jt, kt);
            END (*IF*);
            j := 0;
        END (*IF*);

        IF buf.amount = 0 THEN

            (* Empty input; the only thing that can match is an *)
            (* empty or all-star template.                      *)

            IF jt = kt THEN RETURN template[jt] = '*'
            ELSE RETURN jt > kt
            END (*IF*);

        ELSIF jt > kt THEN

            (* Empty template, non-empty input. *)

            RETURN FALSE;

        END (*IF*);

        (* Having disposed of the "empty" cases, we're now comparing    *)
        (* input[j..] with template[jt..kt].                           *)

        LOOP
            (* From the left, buf.pval^[j] and template[jt] are the    *)
            (* first characters we haven't yet tested for a match.      *)

            IF j >= buf.amount THEN

                (* Get a new buffer-full of input. *)

                ReloadBuffer (cid, buf);
                j := 0;
                IF (buf.amount < BufSize) THEN
                    RETURN InMemorySearch (WildCard.WildMatch,
                                            buf, template, jt, kt);
                END (*IF*);

            END (*IF*);

            IF jt > kt THEN

                (* No more template left; match iff we've also  *)
                (* exhausted the input.                         *)

                RETURN buf.amount = 0;

            ELSIF template[jt] = '*' THEN

                EXIT (*LOOP*);

            ELSIF buf.amount = 0 THEN

                (* Input exhausted, first unmatched template char *)
                (* is not '*', so we have a definite mismatch.    *)

                RETURN FALSE;

            ELSIF (template[jt] <> '?') AND
                        (CAP(buf.pval^[j]) <> CAP(template[jt])) THEN

                RETURN FALSE;

            END (*IF*);

            INC (j);  INC (jt);

        END (*LOOP*);

        (* If we reach here, template[jt] = '*'.  *)

        INC (jt);

        IF jt > kt THEN

            (* No more template left, so effective template = '*'.  *)

            RETURN TRUE;

        ELSE
            (* Now jt <= kt and template[jt] <> '*'.  We don't have to  *)
            (* clone buf because we are outside the main loop.          *)

            RETURN TailMatch (cid, buf, j, template, jt, kt);

        END (*IF*);

    END Match;

(************************************************************************)
(*                    TIDYING UP THE INPUT PARAMETERS                   *)
(************************************************************************)

PROCEDURE CleanTemplate (VAR (*INOUT*) template: ARRAY OF CHAR;
                         jt: CARDINAL;  VAR (*INOUT*) kt: CARDINAL);

    (* Reduces sequences of '*' characters to a single one. *)

    CONST Nul = CHR(0);

    VAR js, jd: CARDINAL;

    BEGIN
        jd := jt;
        IF kt > jt THEN
            FOR js := jt TO kt-1 DO
                IF (template[js] <> '*') OR (template[js+1] <> '*') THEN
                    template[jd] := template[js];
                    INC (jd);
                END (*IF*);
            END (*FOR*);
            IF jd <> kt THEN
                template[jd] := template[kt];
                template[jd+1] := Nul;
                kt := jd;
            END (*IF*);
        END (*IF*);
    END CleanTemplate;

(************************************************************************)

PROCEDURE TidyTemplate (VAR (*IN*) template: ARRAY OF CHAR;
                                            VAR (*OUT*) jt, kt: CARDINAL);

    (* Sets the subscript limits for template, also removes redundant   *)
    (* wildcards in template.  This means that the other procedures in  *)
    (* this module may assume that an '**' never occurs.                *)

    BEGIN
        jt := 0;  kt := LENGTH (template);

        (* Deal with the "empty" case in such a way that will not   *)
        (* let kt go negative.                                      *)

        IF kt = 0 THEN
            jt := 1;
        ELSE
            DEC (kt);
        END (*IF*);

        CleanTemplate (template, jt, kt);

    END TidyTemplate;

(************************************************************************)
(*                  THE EXTERNALLY CALLABLE PROCEDURES                  *)
(************************************************************************)

PROCEDURE FWildMatch (VAR (*IN*) infile, template: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE if template and input are equal, with the extra     *)
    (* rules:                                                           *)
    (*   1. Character case is not significant.                          *)
    (*   2. A '?' in template matches any single character.             *)
    (*   3. A '*' in template matches any string of zero or more        *)
    (*      characters.                                                 *)

    VAR jt, kt: CARDINAL;
        size: CARD64;
        match: BOOLEAN;
        cid: ChanId;
        Buf: Buffer;

    BEGIN
        size := GetFileSize (infile);

        (* Read the beginning of the file into Buf.  *)

        NEW (Buf.pval);
        Buf.offset := Zero64;
        cid := OpenOldFile (infile, FALSE, FALSE);
        ReadRaw (cid, Buf.pval^, BufSize, Buf.amount);
        IF Buf.amount < BufSize THEN
            Buf.pval^[Buf.amount] := Nul;
        END (*IF*);

        IF (size.high = 0) AND (size.low <=BufSize) THEN
            (* Everything fits into a single buffer. *)
            match := WildCard.WildMatch (Buf.pval^, template);
        ELSE
            TidyTemplate (template, jt, kt);
            match := Match (cid, Buf, 0, template, jt, kt);
        END (*IF*);

        IF Buf.pval <> NIL THEN
            DISPOSE (Buf.pval);
        END (*IF*);
        CloseFile (cid);
        RETURN match;

    END FWildMatch;

(************************************************************************)

PROCEDURE FSubstringMatch (VAR (*IN*) infile, template: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE if template matches a substring of input, with the  *)
    (* extra rules:                                                     *)
    (*   1. Character case is not significant.                          *)
    (*   2. A '?' in template matches any single character.             *)
    (*   3. A '*' in template matches any string of zero or more        *)
    (*      characters.                                                 *)

    VAR jt, kt: CARDINAL;
        size: CARD64;
        match: BOOLEAN;
        cid: ChanId;
        Buf: Buffer;

    BEGIN
        size := GetFileSize (infile);

        (* Read the beginning of the file into Buf.  *)

        NEW (Buf.pval);
        Buf.offset := Zero64;
        cid := OpenOldFile (infile, FALSE, FALSE);
        ReadRaw (cid, Buf.pval^, BufSize, Buf.amount);
        IF Buf.amount < BufSize THEN
            Buf.pval^[Buf.amount] := Nul;
        END (*IF*);

        IF (size.high = 0) AND (size.low <=BufSize) THEN
            (* Everything fits into a single buffer. *)
            match := WildCard.SubstringMatch (Buf.pval^, template);
        ELSE
            TidyTemplate (template, jt, kt);
            match := SubMatch (cid, Buf, 0, template, jt, kt);
        END (*IF*);

        IF Buf.pval <> NIL THEN
            DISPOSE (Buf.pval);
        END (*IF*);
        CloseFile (cid);
        RETURN match;

    END FSubstringMatch;

(************************************************************************)

END FWildCard.

