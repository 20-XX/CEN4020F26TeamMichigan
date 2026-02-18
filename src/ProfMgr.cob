IDENTIFICATION DIVISION.
    PROGRAM-ID. PROFMGR.

ENVIRONMENT DIVISION.
    INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT PROFILE-FILE ASSIGN TO 'Profilestest.dat'
                ORGANIZATION IS LINE SEQUENTIAL.
            SELECT PROFILE-TEMP ASSIGN TO 'Profiles.tmp'
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
    WORKING-STORAGE SECTION.
        77 PR-EOF    PIC X VALUE "N".
        77 PROFILE-FOUND    PIC X VALUE "N".
        01 WS-PROFILE-RECORD.
            05 WS-PR-USERNAME    PIC X(20).
            05 WS-PR-FIRST-NAME    PIC X(20).
            05 WS-PR-LAST-NAME    PIC X(20).
            05 WS-PR-UNIVERSITY    PIC X(40).
            05 WS-PR-MAJOR    PIC X(30).
            05 WS-PR-GRAD-YEAR    PIC 9(4).
            05 WS-PR-ABOUT    PIC X(200).
            05 WS-PR-EXP-COUNT    PIC 9.
            05 WS-PR-EXPERIENCE OCCURS 3 TIMES.
                10 WS-PR-EXP-TITLE    PIC X(30).
                10 WS-PR-EXP-COMPANY    PIC X(30).
                10 WS-PR-EXP-DATES    PIC X(20).
                10 WS-PR-EXP-DESC    PIC X(100).
            05 WS-PR-EDU-COUNT    PIC 9.
            05 WS-PR-EDU-EDUCATION OCCURS 3 TIMES.
                10 WS-PR-EDU-DEGREE    PIC X(30).
                10 WS-PR-EDU-SCHOOL    PIC X(40).
                10 WS-PR-EDU-YEARS   PIC X(15).
    LINKAGE SECTION.
        01 LNK-OPERATION    PIC X(2).
        01 LNK-RETURN-CODE    PIC X.
        01 LNK-RECORD.
            05 LNK-USERNAME    PIC X(20).
            05 LNK-FIRST-NAME    PIC X(20).
            05 LNK-LAST-NAME    PIC X(20).
            05 LNK-UNIVERSITY    PIC X(40).
            05 LNK-MAJOR    PIC X(30).
            05 LNK-GRAD-YEAR    PIC 9(4).
            05 LNK-ABOUT    PIC X(200).
            05 LNK-EXP-COUNT    PIC 9.
            05 LNK-EXPERIENCE OCCURS 3 TIMES.
                10 LNK-EXP-TITLE    PIC X(30).
                10 LNK-EXP-COMPANY    PIC X(30).
                10 LNK-EXP-DATES    PIC X(20).
                10 LNK-EXP-DESC    PIC X(100).
            05 LNK-EDU-COUNT    PIC 9.
            05 LNK-EDU-EDUCATION OCCURS 3 TIMES.
                10 LNK-EDU-DEGREE    PIC X(30).
                10 LNK-EDU-SCHOOL    PIC X(40).
                10 LNK-EDU-YEARS    PIC X(15).
PROCEDURE DIVISION USING LNK-OPERATION, LNK-RETURN-CODE, LNK-RECORD.
    MOVE "N" TO LNK-RETURN-CODE
    MOVE LNK-RECORD TO WS-PROFILE-RECORD
    EVALUATE LNK-OPERATION
        WHEN "SV"
            PERFORM SAVE-PROFILE
        WHEN OTHER
            MOVE "E" TO LNK-RETURN-CODE
    END-EVALUATE
    GOBACK.

    SAVE-PROFILE.
        MOVE "N" TO PR-EOF
        MOVE "N" TO PROFILE-FOUND
        OPEN INPUT PROFILE-FILE
        PERFORM UNTIL PROFILE-FOUND = "Y" OR PR-EOF = "Y"
            READ PROFILE-FILE INTO PROFILE-RECORD
                AT END
                    MOVE "Y" TO PR-EOF
                NOT AT END
                    IF PR-USERNAME = WS-PR-USERNAME
                        MOVE "Y" TO PROFILE-FOUND
                    END-IF
            END-READ
        END-PERFORM
        CLOSE PROFILE-FILE

        IF PROFILE-FOUND = "Y"
            MOVE "N" TO PR-EOF
            OPEN INPUT PROFILE-FILE
            OPEN OUTPUT PROFILE-TEMP
            PERFORM UNTIL PR-EOF = "Y"
                READ PROFILE-FILE INTO PROFILE-RECORD
                    AT END
                        MOVE "Y" TO PR-EOF
                    NOT AT END
                        IF PR-USERNAME = WS-PR-USERNAME
                            MOVE WS-PROFILE-RECORD TO PROFILE-RECORD
                        END-IF
                        MOVE PROFILE-RECORD TO PROFILE-TEMP-RECORD
                        WRITE PROFILE-TEMP-RECORD
                END-READ
            END-PERFORM
            CLOSE PROFILE-FILE
            CLOSE PROFILE-TEMP
            CALL 'SYSTEM' USING 'mv Profiles.tmp Profilestest.dat'
        ELSE
            OPEN EXTEND PROFILE-FILE
            MOVE WS-PROFILE-RECORD TO PROFILE-RECORD
            WRITE PROFILE-RECORD
            CLOSE PROFILE-FILE
        END-IF
        MOVE "Y" TO LNK-RETURN-CODE.
