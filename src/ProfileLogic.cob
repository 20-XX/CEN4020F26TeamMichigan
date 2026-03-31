IDENTIFICATION DIVISION.
    PROGRAM-ID. PROFILELOGIC.

ENVIRONMENT DIVISION.
    INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT PROFILE-FILE ASSIGN TO 'data/Profiles.dat'
                ORGANIZATION IS LINE SEQUENTIAL.
            SELECT PROFILE-TEMP ASSIGN TO 'data/Profiles.tmp'
                ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
    FILE SECTION.
        FD PROFILE-FILE.
        01 PROFILE-RECORD.
            05 PR-USERNAME    PIC X(20).
            05 PR-FIRST-NAME    PIC X(20).
            05 PR-LAST-NAME    PIC X(20).
            05 PR-UNIVERSITY    PIC X(40).
            05 PR-MAJOR    PIC X(30).
            05 PR-GRAD-YEAR    PIC 9(4).
            05 PR-ABOUT    PIC X(200).
            05 PR-EXP-COUNT    PIC 9.
            05 PR-EXPERIENCE OCCURS 3 TIMES.
                10 PR-EXP-TITLE    PIC X(30).
                10 PR-EXP-COMPANY    PIC X(30).
                10 PR-EXP-DATES    PIC X(20).
                10 PR-EXP-DESC    PIC X(100).
            05 PR-EDU-COUNT    PIC 9.
            05 PR-EDUCATION OCCURS 3 TIMES.
                10 PR-EDU-DEGREE    PIC X(30).
                10 PR-EDU-SCHOOL    PIC X(40).
                10 PR-EDU-YEARS    PIC X(15).
        FD PROFILE-TEMP.
        01 PROFILE-TEMP-RECORD    PIC X(800).
    LOCAL-STORAGE SECTION.
        77 PR-EOF    PIC X VALUE "N".
        77 PROFILE-FOUND    PIC X VALUE "N".
        01 I    PIC 9(2).
        01 LS-SEARCH-FULLNAME    PIC X(50).
        01 LS-SEARCH-FIRST-NAME    PIC X(20).
        01 LS-SEARCH-LAST-NAME    PIC X(20).
        01 LS-SEARCH-SPACE-LOC    PIC 9(2) VALUE 0.
        01 LS-PROFILE-RECORD.
            05 LS-PR-USERNAME    PIC X(20).
            05 LS-PR-FIRST-NAME    PIC X(20).
            05 LS-PR-LAST-NAME    PIC X(20).
            05 LS-PR-UNIVERSITY    PIC X(40).
            05 LS-PR-MAJOR    PIC X(30).
            05 LS-PR-GRAD-YEAR    PIC 9(4).
            05 LS-PR-ABOUT    PIC X(200).
            05 LS-PR-EXP-COUNT    PIC 9.
            05 LS-PR-EXPERIENCE OCCURS 3 TIMES.
                10 LS-PR-EXP-TITLE    PIC X(30).
                10 LS-PR-EXP-COMPANY    PIC X(30).
                10 LS-PR-EXP-DATES    PIC X(20).
                10 LS-PR-EXP-DESC    PIC X(100).
            05 LS-PR-EDU-COUNT    PIC 9.
            05 LS-PR-EDU-EDUCATION OCCURS 3 TIMES.
                10 LS-PR-EDU-DEGREE    PIC X(30).
                10 LS-PR-EDU-SCHOOL    PIC X(40).
                10 LS-PR-EDU-YEARS    PIC X(15).
    LINKAGE SECTION.
        01 LNK-PARAMETERS.
            05 LNK-OPERATION    PIC X(3).
            05 LNK-RETURN-CODE    PIC X.
            05 LNK-SEARCH-USERNAME    PIC X(20).
            05 LNK-SEARCH-FULLNAME    PIC X(50).
            05 LNK-RECORD.
                10 LNK-USERNAME    PIC X(20).
                10 LNK-FIRST-NAME    PIC X(20).
                10 LNK-LAST-NAME    PIC X(20).
                10 LNK-UNIVERSITY    PIC X(40).
                10 LNK-MAJOR    PIC X(30).
                10 LNK-GRAD-YEAR    PIC 9(4).
                10 LNK-ABOUT    PIC X(200).
                10 LNK-EXP-COUNT    PIC 9.
                10 LNK-EXPERIENCE OCCURS 3 TIMES.
                    15 LNK-EXP-TITLE    PIC X(30).
                    15 LNK-EXP-COMPANY    PIC X(30).
                    15 LNK-EXP-DATES    PIC X(20).
                    15 LNK-EXP-DESC    PIC X(100).
                10 LNK-EDU-COUNT    PIC 9.
                10 LNK-EDU-EDUCATION OCCURS 3 TIMES.
                    15 LNK-EDU-DEGREE    PIC X(30).
                    15 LNK-EDU-SCHOOL    PIC X(40).
                    15 LNK-EDU-YEARS    PIC X(15).
PROCEDURE DIVISION USING LNK-PARAMETERS.
    MOVE "N" TO LNK-RETURN-CODE
    MOVE LNK-RECORD TO LS-PROFILE-RECORD
    EVALUATE FUNCTION TRIM(LNK-OPERATION)
        WHEN "SP"
            PERFORM SAVE-PROFILE
        WHEN "GCP"
            PERFORM GET-CURRENT-PROFILE
            IF PROFILE-FOUND = "Y"
                MOVE "Y" TO LNK-RETURN-CODE
            ELSE
                MOVE "N" TO LNK-RETURN-CODE
            END-IF
        WHEN "GFP"
            MOVE LNK-SEARCH-FULLNAME TO LS-SEARCH-FULLNAME
            PERFORM PARSE-SEARCH-NAME
            PERFORM GET-FULLNAME-PROFILE
            IF PROFILE-FOUND = "Y"
                MOVE "Y" TO LNK-RETURN-CODE
            ELSE
                MOVE "N" TO LNK-RETURN-CODE
            END-IF
        WHEN OTHER
            MOVE "E" TO LNK-RETURN-CODE
    END-EVALUATE
    GOBACK.

    SAVE-PROFILE.
        OPEN INPUT PROFILE-FILE
        OPEN OUTPUT PROFILE-TEMP

        PERFORM UNTIL PR-EOF = "Y"
            READ PROFILE-FILE
                AT END
                    MOVE "Y" TO PR-EOF
                NOT AT END
                    IF FUNCTION TRIM(PR-USERNAME) = FUNCTION TRIM(LNK-SEARCH-USERNAME)
                        MOVE "Y" TO PROFILE-FOUND
                    ELSE
                        MOVE PROFILE-RECORD TO PROFILE-TEMP-RECORD
                        WRITE PROFILE-TEMP-RECORD
                    END-IF
            END-READ
        END-PERFORM

        MOVE LS-PROFILE-RECORD TO PROFILE-RECORD
        MOVE PROFILE-RECORD TO PROFILE-TEMP-RECORD
        WRITE PROFILE-TEMP-RECORD

        CLOSE PROFILE-FILE
        CLOSE PROFILE-TEMP

        CALL 'SYSTEM' USING 'mv data/Profiles.tmp data/Profiles.dat'

        MOVE "Y" TO LNK-RETURN-CODE.

    GET-CURRENT-PROFILE.
        OPEN INPUT PROFILE-FILE

        PERFORM UNTIL PR-EOF = "Y" OR PROFILE-FOUND = "Y"
            READ PROFILE-FILE
                AT END
                    MOVE "Y" TO PR-EOF
                NOT AT END
                    IF FUNCTION TRIM(PR-USERNAME) =
                        FUNCTION TRIM(LNK-SEARCH-USERNAME)
                        MOVE "Y" TO PROFILE-FOUND
                        MOVE PROFILE-RECORD TO LNK-RECORD
                    END-IF
            END-READ
        END-PERFORM

        CLOSE PROFILE-FILE.

    PARSE-SEARCH-NAME.
        PERFORM VARYING I FROM 1 BY 1 UNTIL I > FUNCTION LENGTH(LS-SEARCH-FULLNAME) OR LS-SEARCH-SPACE-LOC > 0
            IF LS-SEARCH-FULLNAME(I:1) = " "
                MOVE I TO LS-SEARCH-SPACE-LOC
            END-IF
        END-PERFORM

        IF LS-SEARCH-SPACE-LOC > 0
            MOVE LS-SEARCH-FULLNAME(1:LS-SEARCH-SPACE-LOC - 1) TO LS-SEARCH-FIRST-NAME
            MOVE LS-SEARCH-FULLNAME(LS-SEARCH-SPACE-LOC + 1:) TO LS-SEARCH-LAST-NAME
        ELSE
            MOVE LS-SEARCH-FULLNAME TO LS-SEARCH-FIRST-NAME
        END-IF.

    GET-FULLNAME-PROFILE.
        OPEN INPUT PROFILE-FILE

        PERFORM UNTIL PR-EOF = "Y" OR PROFILE-FOUND = "Y"
            READ PROFILE-FILE
                AT END
                    MOVE "Y" TO PR-EOF
                NOT AT END
                    IF FUNCTION TRIM(PR-FIRST-NAME) = FUNCTION TRIM(LS-SEARCH-FIRST-NAME)
                        AND FUNCTION TRIM(PR-LAST-NAME) = FUNCTION TRIM(LS-SEARCH-LAST-NAME)
                        MOVE "Y" TO PROFILE-FOUND
                        MOVE PROFILE-RECORD TO LNK-RECORD
                    END-IF
            END-READ
        END-PERFORM

        CLOSE PROFILE-FILE.
