      *****************************************************************
      * INCOLLEGE-DATA.cpy
      * Common data structures shared across InCollege modules
      *****************************************************************
      
      * File Control Definitions
       01 FILE-CONTROL-DATA.
           05 INPUT-FILE-NAME     PIC X(30) VALUE "InCollege-Input.txt".
           05 OUTPUT-FILE-NAME    PIC X(30) VALUE "InCollege-Output.txt".
           05 ACCOUNT-FILE-NAME   PIC X(30) VALUE "Accounts.dat".
           05 PROFILE-FILE-NAME   PIC X(30) VALUE "Profiles.dat".
           05 PROFILE-TEMP-NAME   PIC X(30) VALUE "Profiles.tmp".

      * Account Record Structure
       01 ACCOUNT-RECORD.
           05 ACC-USERNAME        PIC X(20).
           05 ACC-PASSWORD        PIC X(12).

      * Profile Record Structure
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

      * Working Storage Profile Record
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

      * Common Flags
       01 COMMON-FLAGS.
           05 RETURN-CODE         PIC 9(2) VALUE 0.
           05 SUCCESS-FLAG        PIC X VALUE "N".
           05 EOF-FLAG            PIC X VALUE "N".

      * Common Working Variables
       01 COMMON-WORK-AREAS.
           05 WS-USERNAME         PIC X(20).
           05 WS-PASSWORD         PIC X(50).
           05 WS-HASHED-PASSWORD  PIC X(12).
           05 WS-OUT-LINE         PIC X(100).
           05 I                   PIC 9(2).
