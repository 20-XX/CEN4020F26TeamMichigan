IDENTIFICATION DIVISION.
    PROGRAM-ID. PROFMGR.

ENVIRONMENT DIVISION.
    INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT PROFILE-FILE ASSIGN TO 'Profiles.dat'
                ORGANIZATION IS LINE SEQUENTIAL.
            SELECT PROFILE-TEMP FILE ASSIGN TO 'Profiles.tmp'
                ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
    FILE SECTION.
        FD PROFILE-FILE.
        01 PROFILE-RECORD.
            05 PR-USERNAME        PIC X(20).
            05 PR-FIRST-NAME      PIC X(20).
            05 PR-LAST-NAME       PIC X(20).
            05 PR-UNIVERSITY      PIC X(40).
            05 PR-MAJOR           PIC X(30).
            05 PR-GRAD-YEAR       PIC 9(4).
            05 PR-ABOUT           PIC X(200).
            05 PR-EXP-COUNT       PIC 9.
            05 PR-EXPERIENCE OCCURS 3 TIMES.
                10 PR-EXP-TITLE   PIC X(30).
                10 PR-EXP-COMPANY PIC X(30).
                10 PR-EXP-DATES   PIC X(20).
                10 PR-EXP-DESC    PIC X(100).
            05 PR-EDU-COUNT       PIC 9.
            05 PR-EDUCATION OCCURS 3 TIMES.
                10 PR-EDU-DEGREE  PIC X(30).
                10 PR-EDU-SCHOOL  PIC X(40).
                10 PR-EDU-YEARS   PIC X(15).
        FD PROFILE-TEMP.
        01 PROFILE-TEMP-RECORD PIC X(800).
    WORKING-STORAGE SECTION.
        77 PR-EOF              PIC X VALUE "N".
        77 PROFILE-FOUND       PIC X VALUE "N".
        01 WS-PROFILE-RECORD.
            05 WS-PR-USERNAME        PIC X(20).
            05 WS-PR-FIRST-NAME      PIC X(20).
            05 WS-PR-LAST-NAME       PIC X(20).
            05 WS-PR-UNIVERSITY      PIC X(40).
            05 WS-PR-MAJOR           PIC X(30).
            05 WS-PR-GRAD-YEAR       PIC 9(4).
            05 WS-PR-ABOUT           PIC X(200).
            05 WS-PR-EXP-COUNT       PIC 9.
            05 WS-PR-EXPERIENCE OCCURS 3 TIMES.
                10 WS-PR-EXP-TITLE   PIC X(30).
                10 WS-PR-EXP-COMPANY PIC X(30).
                10 WS-PR-EXP-DATES   PIC X(20).
                10 WS-PR-EXP-DESC    PIC X(100).
            05 WS-PR-EDU-COUNT       PIC 9.
            05 WS-PR-EDU-EDUCATION OCCURS 3 TIMES.
                10 WS-PR-EDU-DEGREE  PIC X(30).
                10 WS-PR-EDU-SCHOOL  PIC X(40).
                10 WS-PR-EDU-YEARS   PIC X(15).
