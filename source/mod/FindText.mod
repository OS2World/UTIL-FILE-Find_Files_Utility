(**************************************************************************)
(*                                                                        *)
(*  Search for text string inside a file                                  *)
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

IMPLEMENTATION MODULE FindText;

        (********************************************************)
        (*                                                      *)
        (*              Find text inside a file                 *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            3 March 2019                    *)
        (*  Last edited:        5 March 2019                    *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT
    (* type *)  ADDRESS,
    (* proc *)  ADR, ADDADR, MOVE;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId,
    (* proc *)  OpenOldFile, ReadRaw, CloseFile;

(********************************************************************************)
(*                                 MAIN FUNCTION                                *)
(********************************************************************************)

PROCEDURE FindInFile (filename, text: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff the text string occurs in the file. *)

    (* Remark: we cannot use standard string search functions because some of   *)
    (* the files could be binary files, and for example zero bytes would        *)
    (* prematurely terminate a search.                                          *)

    (* Note also that the buffer size is slightly larger than what we read from *)
    (* disk.  This is to handle the case where the string being sought would    *)
    (* otherwise straddle a block boundary.                                     *)

    CONST
        BlockSize = 8192;

    VAR cid: ChanId;
        buffersize, textlength, NumberRead, j, k0, k, klimit, N: CARDINAL;
        ch: CHAR;
        found: BOOLEAN;
        pbuf: POINTER TO ARRAY [0..2*BlockSize - 1] OF CHAR;
        pblock: ADDRESS;

    BEGIN
        found := FALSE;
        cid := OpenOldFile (filename, FALSE, TRUE);
        IF cid = NoSuchChannel THEN
            RETURN FALSE;
        END (*IF*);
        textlength := LENGTH(text);
        N := textlength - 1;
        buffersize := BlockSize + N;
        ALLOCATE (pbuf, buffersize);
        ReadRaw (cid, pbuf^, N, NumberRead);
        pblock := ADDADR (pbuf, N);

        REPEAT
            ReadRaw (cid, pblock^, BlockSize, NumberRead);
            IF NumberRead > 0 THEN
                k := 0;  ch := text[0];
                klimit := buffersize;
                IF NumberRead < BlockSize THEN
                    DEC (klimit, BlockSize - NumberRead);
                END (*IF*);
                WHILE (NOT found) AND (k < klimit) DO

                    (* Scan for match with first character of text. *)

                    WHILE (k < klimit) AND (pbuf^[k] <> ch) DO
                        INC (k);
                    END (*WHILE*);
                    k0 := k;

                    IF pbuf^[k] = ch THEN

                        (* Check for match with remainder of text. *)

                        INC(k);  j := 1;
                        WHILE (k < klimit) AND (j < textlength)
                                        AND (pbuf^[k] = text[j]) DO
                            INC (j);  INC(k);
                        END (*WHILE*);
                        found := j >= textlength;

                    END (*IF*);

                    IF NOT found THEN
                        k := k0 + 1;        (* place to resume the search *)
                    END (*IF*);

                END (*WHILE*);

                (* Move the last little bit of the data up to the   *)
                (* beginning of the search buffer.                  *)

                IF NOT found THEN
                    MOVE (ADR(pbuf^[klimit - N]), pbuf, N);
                END (*IF*);

            END (*IF NumberRead > 0 *);

        UNTIL found OR (NumberRead = 0);

        DEALLOCATE (pbuf, buffersize);
        CloseFile (cid);
        RETURN found;

    END FindInFile;

(********************************************************************************)

END FindText.

