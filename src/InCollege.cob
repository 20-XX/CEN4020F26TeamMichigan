IDENTIFICATION DIVISION.
    PROGRAM-ID. INCOLLEGE.

ENVIRONMENT DIVISION.
    INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT INPUT-FILE ASSIGN TO "InCollege-Input.txt"
                ORGANIZATION IS LINE SEQUENTIAL.
            SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
                ORGANIZATION IS LINE SEQUENTIAL.
            SELECT BROWSE-JOBS-FILE ASSIGN TO 'data/Jobs.dat'
                ORGANIZATION IS LINE SEQUENTIAL.
            SELECT APPLY-FILE ASSIGN TO 'data/Applications.dat'
                ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
    FILE SECTION.
        FD INPUT-FILE.
            01 INPUT-RECORD    PIC X(100).
        FD OUTPUT-FILE.
            01 OUTPUT-RECORD    PIC X(200).

        FD BROWSE-JOBS-FILE.
            01 BJ-FILE-RECORD.
                05 BJ-FILE-JOB-ID           PIC 9(5).
                05 BJ-FILE-JOB-POSTED-BY    PIC X(20).
                05 BJ-FILE-JOB-TITLE        PIC X(50).
                05 BJ-FILE-JOB-DESCRIPTION  PIC X(200).
                05 BJ-FILE-JOB-EMPLOYER     PIC X(50).
                05 BJ-FILE-JOB-LOCATION     PIC X(50).
                05 BJ-FILE-JOB-SALARY       PIC X(30).

        FD APPLY-FILE.
            01 APP-RECORD.
                05 APP-USERNAME         PIC X(20).
                05 APP-JOB-ID           PIC 9(5).
                05 APP-JOB-TITLE        PIC X(50).
                05 APP-JOB-EMPLOYER     PIC X(50).
                05 APP-JOB-LOCATION     PIC X(50).

    WORKING-STORAGE SECTION.
        77 EOF-FLAG    PIC X VALUE "N".
        77 MENU-CHOICE    PIC X.
        77 WS-ACCEPT-CHOICE    PIC X VALUE SPACES.

        01 WS-USERNAME    PIC X(20).
        01 WS-PASSWORD    PIC X(50).
        01 WS-YEAR-INPUT    PIC X(4).
        01 WS-OUT-LINE    PIC X(100).

        01 WS-PROFILE-RECORD.
            05 WS-PR-USERNAME   PIC X(20).
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
                10 WS-PR-EDU-YEARS    PIC X(15).

        01 WS-PEND-REQUEST.
            05 WS-PEND-SENDER-USER    PIC X(20).
            05 WS-PEND-SENDER-FIRST    PIC X(20).
            05 WS-PEND-SENDER-LAST    PIC X(20).
            05 WS-PEND-RECEIVER-USER    PIC X(20).
            05 WS-PEND-RECEIVER-FIRST    PIC X(20).
            05 WS-PEND-RECEIVER-LAST    PIC X(20).
        01 WS-JOB-RECORD.
            05 WS-JOB-ID          PIC 9(5).
            05 WS-JOB-POSTED-BY    PIC X(20).
            05 WS-JOB-TITLE    PIC X(50).
            05 WS-JOB-DESCRIPTION    PIC X(200).
            05 WS-JOB-EMPLOYER    PIC X(50).
            05 WS-JOB-LOCATION    PIC X(50).
            05 WS-JOB-SALARY    PIC X(30).
        01 I    PIC 9(2).
        01 IDX  PIC 9(3).

      *> --- Epic 7: Browse, Apply, View Applications variables ---
        77 WS-BROWSE-EOF        PIC X VALUE "N".
        77 WS-APPLY-EOF         PIC X VALUE "N".
        77 WS-BROWSE-JOB-COUNT  PIC 9(2) VALUE 0.
        77 WS-BROWSE-IDX        PIC 9(2) VALUE 0.
        77 WS-BROWSE-CHOICE     PIC 9(2) VALUE 0.
        77 WS-BROWSE-CHOICE-STR PIC X(3).
        77 WS-DETAIL-CHOICE     PIC X.
        77 WS-ALREADY-APPLIED   PIC X VALUE "N".
        77 WS-APP-TOTAL         PIC 9(3) VALUE 0.

        01 WS-BROWSE-JOBS OCCURS 20 TIMES.
            05 BJ-JOB-IDX       PIC 9(2).
            05 BJ-REAL-ID       PIC 9(5).
            05 BJ-JOB-TITLE     PIC X(50).
            05 BJ-JOB-EMPLOYER  PIC X(50).
            05 BJ-JOB-LOCATION  PIC X(50).
            05 BJ-JOB-DESCRIPTION PIC X(200).
            05 BJ-JOB-SALARY    PIC X(30).

        01 WS-MSG-RECORD.
            05 WS-MSG-SENDER    PIC X(20).
            05 WS-MSG-RECIPIENT    PIC X(20).
            05 WS-MSG-TIMESTAMP    PIC X(21).
            05 WS-MSG-CONTENT    PIC X(100).

        01 ACCT-LINK-PARAMETERS.
            05 ACCT-LNK-OPERATION    PIC X(2).
            05 ACCT-LNK-USERNAME    PIC X(20).
            05 ACCT-LNK-PASSWORD    PIC X(50).
            05 ACCT-LNK-RETURN-CODE    PIC X.
        01 PROF-LINK-PARAMETERS.
            05 PROF-LNK-OPERATION    PIC X(3).
            05 PROF-LNK-RETURN-CODE    PIC X.
            05 PROF-LNK-SEARCH-USERNAME    PIC X(20).
            05 PROF-LNK-SEARCH-FULLNAME    PIC X(50).
            05 PROF-LNK-RECORD.
                10 PROF-LNK-USERNAME    PIC X(20).
                10 PROF-LNK-FIRST-NAME    PIC X(20).
                10 PROF-LNK-LAST-NAME    PIC X(20).
                10 PROF-LNK-UNIVERSITY    PIC X(40).
                10 PROF-LNK-MAJOR    PIC X(30).
                10 PROF-LNK-GRAD-YEAR    PIC 9(4).
                10 PROF-LNK-ABOUT    PIC X(200).
                10 PROF-LNK-EXP-COUNT    PIC 9.
                10 PROF-LNK-EXPERIENCE OCCURS 3 TIMES.
                    15 PROF-LNK-EXP-TITLE    PIC X(30).
                    15 PROF-LNK-EXP-COMPANY    PIC X(30).
                    15 PROF-LNK-EXP-DATES    PIC X(20).
                    15 PROF-LNK-EXP-DESC    PIC X(100).
                10 PROF-LNK-EDU-COUNT    PIC 9.
                10 PROF-LNK-EDU-EDUCATION OCCURS 3 TIMES.
                    15 PROF-LNK-EDU-DEGREE    PIC X(30).
                    15 PROF-LNK-EDU-SCHOOL    PIC X(40).
                    15 PROF-LNK-EDU-YEARS    PIC X(15).
        01 CONN-LINK-PARAMETERS.
            05 CONN-LNK-OPERATION    PIC X(3).
            05 CONN-LNK-RETURN-CODE    PIC X.
            05 CONN-LNK-SEARCH-USERNAME    PIC X(20).
            05 CONN-LNK-PEND-REQUEST.
                10 CONN-LNK-SENDER-USER    PIC X(20).
                10 CONN-LNK-SENDER-FIRST    PIC X(20).
                10 CONN-LNK-SENDER-LAST    PIC X(20).
                10 CONN-LNK-RECEIVER-USER    PIC X(20).
                10 CONN-LNK-RECEIVER-FIRST    PIC X(20).
                10 CONN-LNK-RECEIVER-LAST    PIC X(20).
            05 CONN-LNK-NEW-CONN.
                10 CONN-LNK-NEW-CONN-USER1    PIC X(20).
                10 CONN-LNK-NEW-CONN-USER1-FIRST    PIC X(20).
                10 CONN-LNK-NEW-CONN-USER1-LAST    PIC X(20).
                10 CONN-LNK-NEW-CONN-USER2    PIC X(20).
                10 CONN-LNK-NEW-CONN-USER2-FIRST    PIC X(20).
                10 CONN-LNK-NEW-CONN-USER2-LAST    PIC X(20).
            05 CONN-LNK-ALL-PENDING-REQUESTS.
                10 CONN-LNK-NUM-PEND-REQUESTS    PIC 9.
                10 CONN-LNK-PEND-REQUESTS OCCURS 4 TIMES.
                    15 CONN-LNK-REQUEST-SENDER-USER    PIC X(20).
                    15 CONN-LNK-REQUEST-SENDER-FIRST    PIC X(20).
                    15 CONN-LNK-REQUEST-SENDER-LAST    PIC X(20).
                    15 CONN-LNK-REQUEST-RECEIVER-USER    PIC X(20).
                    15 CONN-LNK-REQUEST-RECEIVER-FIRST    PIC X(20).
                    15 CONN-LNK-REQUEST-RECEIVER-LAST    PIC X(20).
            05 CONN-LNK-ALL-CONNECTIONS.
                10 CONN-LNK-NUM-CONNECTIONS    PIC 9.
                10 CONN-LNK-CONNECTIONS OCCURS 4 TIMES.
                    15 CONN-LNK-CONN-USER1    PIC X(20).
                    15 CONN-LNK-CONN-USER1-FIRST    PIC X(20).
                    15 CONN-LNK-CONN-USER1-LAST    PIC X(20).
                    15 CONN-LNK-CONN-USER2    PIC X(20).
                    15 CONN-LNK-CONN-USER2-FIRST    PIC X(20).
                    15 CONN-LNK-CONN-USER2-LAST    PIC X(20).
        01 JOB-LINK-PARAMETERS.
            05 JOB-LNK-OPERATION    PIC X(3).
            05 JOB-LNK-RETURN-CODE    PIC X.
            05 JOB-LNK-RETURN-ID    PIC 9(5).
            05 JOB-LNK-JOB-RECORD.
                10 JOB-LNK-JOB-ID          PIC 9(5).
                10 JOB-LNK-JOB-POSTED-BY    PIC X(20).
                10 JOB-LNK-JOB-TITLE    PIC X(50).
                10 JOB-LNK-JOB-DESCRIPTION    PIC X(200).
                10 JOB-LNK-JOB-EMPLOYER    PIC X(50).
                10 JOB-LNK-JOB-LOCATION    PIC X(50).
                10 JOB-LNK-JOB-SALARY    PIC X(30).

        01 MSG-LINK-PARAMETERS.
            05 MSG-LNK-OPERATION    PIC X(3).
            05 MSG-LNK-RETURN-CODE    PIC X.
            05 MSG-LNK-RECORD.
                10 MSG-LNK-SENDER    PIC X(20).
                10 MSG-LNK-RECIPIENT    PIC X(20).
                10 MSG-LNK-TIMESTAMP    PIC X(21).
                10 MSG-LNK-CONTENT    PIC X(100).
           05 MSG-LNK-USERNAME        PIC X(20).
           05 MSG-LNK-NUM-RECORDS     PIC 9(3).
           05 MSG-LNK-RECORDS OCCURS 100 TIMES.
               10 MSG-LNK-REC-SENDER     PIC X(20).
               10 MSG-LNK-REC-RECIPIENT  PIC X(20).
               10 MSG-LNK-REC-TIMESTAMP  PIC X(21).
               10 MSG-LNK-REC-CONTENT    PIC X(100).


PROCEDURE DIVISION.
    OPEN INPUT INPUT-FILE
    OPEN OUTPUT OUTPUT-FILE

    PERFORM UNTIL EOF-FLAG = "Y"
        PERFORM MAIN-MENU
    END-PERFORM

    CLOSE INPUT-FILE
    CLOSE OUTPUT-FILE
    STOP RUN.

    MAIN-MENU.
        IF EOF-FLAG = "Y"
            EXIT PARAGRAPH
        END-IF

        MOVE "Welcome to InCollege!" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        MOVE "Log In" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        MOVE "Create New Account" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        MOVE "Enter your choice:" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE

        PERFORM READ-INPUT

        IF EOF-FLAG = "Y"
            EXIT PARAGRAPH
        END-IF

        MOVE INPUT-RECORD(1:1) TO MENU-CHOICE

        IF MENU-CHOICE = "1"
            PERFORM LOGIN-HANDLER
        ELSE
            IF MENU-CHOICE = "2"
                PERFORM CREATE-ACCOUNT-HANDLER
            END-IF
        END-IF

        EXIT PARAGRAPH.

    CREATE-ACCOUNT-HANDLER.
        MOVE "LA" TO ACCT-LNK-OPERATION
        CALL 'ACCOUNTLOGIC' USING ACCT-LINK-PARAMETERS
        IF ACCT-LNK-RETURN-CODE = "N"
            MOVE "All permitted accounts have been created, please come back later" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            EXIT PARAGRAPH
        END-IF

        MOVE "Please enter a username:" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        PERFORM READ-INPUT

        IF EOF-FLAG = "Y"
            EXIT PARAGRAPH
        END-IF
        MOVE INPUT-RECORD TO WS-USERNAME
        MOVE "CU" TO ACCT-LNK-OPERATION
        MOVE WS-USERNAME TO ACCT-LNK-USERNAME

        CALL 'ACCOUNTLOGIC' USING ACCT-LINK-PARAMETERS

        IF ACCT-LNK-RETURN-CODE = "Y"
            MOVE "Username already exists" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            EXIT PARAGRAPH
        END-IF

        MOVE "Please enter a password:" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        PERFORM READ-INPUT

        IF EOF-FLAG = "Y"
            EXIT PARAGRAPH
        END-IF

        MOVE INPUT-RECORD TO WS-PASSWORD
        MOVE "VP" TO ACCT-LNK-OPERATION
        MOVE WS-PASSWORD TO ACCT-LNK-PASSWORD

        CALL 'ACCOUNTLOGIC' USING ACCT-LINK-PARAMETERS

        IF ACCT-LNK-RETURN-CODE = "Y"
             MOVE "AA" TO ACCT-LNK-OPERATION
             CALL 'ACCOUNTLOGIC' USING ACCT-LINK-PARAMETERS
             MOVE "Account successfully created" TO WS-OUT-LINE
             PERFORM DISPLAY-LINE
        ELSE
            MOVE "Password does not meet requirements" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            EXIT PARAGRAPH
        END-IF

        EXIT PARAGRAPH.

    LOGIN-HANDLER.
        MOVE "Please enter your username:" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        PERFORM READ-INPUT
        IF EOF-FLAG = "Y"
            EXIT PARAGRAPH
        END-IF
        MOVE INPUT-RECORD TO WS-USERNAME
        MOVE WS-USERNAME TO ACCT-LNK-USERNAME

        MOVE "Please enter your password:" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        PERFORM READ-INPUT
        IF EOF-FLAG = "Y"
            EXIT PARAGRAPH
        END-IF
        MOVE INPUT-RECORD TO WS-PASSWORD
        MOVE WS-PASSWORD TO ACCT-LNK-PASSWORD
        MOVE "AL" TO ACCT-LNK-OPERATION

        CALL 'ACCOUNTLOGIC' USING ACCT-LNK-OPERATION, ACCT-LNK-USERNAME, ACCT-LNK-PASSWORD, ACCT-LNK-RETURN-CODE

        IF ACCT-LNK-RETURN-CODE = "Y"
            MOVE "You have successfully logged in" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            PERFORM POST-LOGIN
        ELSE
            MOVE "Incorrect username/password" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
        END-IF

        EXIT PARAGRAPH.

    POST-LOGIN.
        STRING "Welcome, " DELIMITED BY SIZE
            FUNCTION TRIM(WS-USERNAME) DELIMITED BY SIZE
            "!" DELIMITED BY SIZE
            INTO WS-OUT-LINE
        END-STRING
        PERFORM DISPLAY-LINE

        PERFORM UNTIL MENU-CHOICE = "8" OR EOF-FLAG = "Y"
            MOVE "1. Search for a job" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            MOVE "2. Find someone you know" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            MOVE "3. Learn a new skill" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            MOVE "4. Create/Edit My Profile" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            MOVE "5. View My Profile" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            MOVE "6. View My Pending Connection Requests" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            MOVE "7. View My Network" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            MOVE "8. Messages" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            MOVE "9. Exit" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE

            MOVE "Enter your choice:" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE

            PERFORM READ-INPUT
            IF EOF-FLAG = "Y"
                EXIT PERFORM
            END-IF
            MOVE INPUT-RECORD(1:1) TO MENU-CHOICE

            EVALUATE MENU-CHOICE
                WHEN "1"
                    PERFORM JOB-SEARCH-MENU
                WHEN "2"
                    PERFORM USER-PROFILE-SEARCH
                WHEN "3"
                    PERFORM SKILL-MENU
                WHEN "4"
                    PERFORM PROFILE-MENU
                WHEN "5"
                    PERFORM VIEW-PROFILE
                WHEN "6"
                    PERFORM VIEW-PENDING-REQUESTS
                WHEN "7"
                    PERFORM VIEW-MY-NETWORK
                WHEN "8"
                    PERFORM MESSAGE-MENU
                WHEN "9"
                    EXIT PARAGRAPH
            END-EVALUATE
        END-PERFORM

        EXIT PARAGRAPH.

    SKILL-MENU.
        IF EOF-FLAG = "Y"
            EXIT PARAGRAPH
        END-IF
        MOVE "Learn a New Skill:" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        MOVE "1. Programming" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        MOVE "2. Networking" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        MOVE "3. Cybersecurity" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        MOVE "4. Databases" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        MOVE "5. Cloud Computing" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        MOVE "6. Go Back" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        MOVE "Enter your choice:" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE

        PERFORM READ-INPUT
        MOVE INPUT-RECORD(1:1) TO MENU-CHOICE

        IF MENU-CHOICE = "6"
            PERFORM POST-LOGIN
        ELSE
            MOVE "This skill is under construction." TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            PERFORM SKILL-MENU
        END-IF.
    PROFILE-MENU.
        IF EOF-FLAG = "Y"
            EXIT PARAGRAPH
        END-IF
        MOVE "1. Create or Edit Profile" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        MOVE "2. Return to Menu" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE

        PERFORM READ-INPUT
        MOVE INPUT-RECORD(1:1) TO MENU-CHOICE

        IF MENU-CHOICE = "1"
            PERFORM CREATE-EDIT-PROFILE
        END-IF.

    CREATE-EDIT-PROFILE.
        MOVE WS-USERNAME TO WS-PR-USERNAME
        PERFORM INITIALIZE-PROFILE-RECORD

        PERFORM PROMPT-REQUIRED-FIELDS
        PERFORM PROMPT-OPTIONAL-FIELDS

        MOVE "SP" TO PROF-LNK-OPERATION
        MOVE WS-PROFILE-RECORD TO PROF-LNK-RECORD
        CALL 'PROFILELOGIC' USING PROF-LINK-PARAMETERS
        IF PROF-LNK-RETURN-CODE = "Y"
             MOVE "Profile saved successfully." TO WS-OUT-LINE
             PERFORM DISPLAY-LINE
        ELSE
             MOVE "Error saving profile." TO WS-OUT-LINE
             PERFORM DISPLAY-LINE
        END-IF

        PERFORM PROFILE-MENU

        EXIT PARAGRAPH.
    INITIALIZE-PROFILE-RECORD.
        MOVE SPACES TO WS-PR-FIRST-NAME
        MOVE SPACES TO WS-PR-LAST-NAME
        MOVE SPACES TO WS-PR-UNIVERSITY
        MOVE SPACES TO WS-PR-MAJOR
        MOVE 0 TO WS-PR-GRAD-YEAR
        MOVE SPACES TO WS-PR-ABOUT
        MOVE 0 TO WS-PR-EXP-COUNT
        MOVE 0 TO WS-PR-EDU-COUNT

        PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
            MOVE SPACES TO WS-PR-EXP-TITLE(I)
            MOVE SPACES TO WS-PR-EXP-COMPANY(I)
            MOVE SPACES TO WS-PR-EXP-DATES(I)
            MOVE SPACES TO WS-PR-EXP-DESC(I)
            MOVE SPACES TO WS-PR-EDU-DEGREE(I)
            MOVE SPACES TO WS-PR-EDU-SCHOOL(I)
            MOVE SPACES TO WS-PR-EDU-YEARS(I)
        END-PERFORM.
    PROMPT-REQUIRED-FIELDS.
        PERFORM UNTIL WS-PR-FIRST-NAME NOT = SPACES
            MOVE "Enter First Name:" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            PERFORM READ-INPUT
            MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-PR-FIRST-NAME
        END-PERFORM

        PERFORM UNTIL WS-PR-LAST-NAME NOT = SPACES
            MOVE "Enter Last Name:" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            PERFORM READ-INPUT
            MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-PR-LAST-NAME
        END-PERFORM

        PERFORM UNTIL WS-PR-UNIVERSITY NOT = SPACES
            MOVE "Enter University/College Attended:" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            PERFORM READ-INPUT
            MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-PR-UNIVERSITY
        END-PERFORM

        PERFORM UNTIL WS-PR-MAJOR NOT = SPACES
            MOVE "Enter Major:" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            PERFORM READ-INPUT
            MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-PR-MAJOR
        END-PERFORM

        PERFORM UNTIL WS-PR-GRAD-YEAR >= 1900 AND WS-PR-GRAD-YEAR <= 2100
            MOVE "Enter Graduation Year (YYYY):" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            PERFORM READ-INPUT
            MOVE INPUT-RECORD(1:4) TO WS-YEAR-INPUT

            IF WS-YEAR-INPUT IS NUMERIC
                MOVE WS-YEAR-INPUT TO WS-PR-GRAD-YEAR
            ELSE
                MOVE 0 TO WS-PR-GRAD-YEAR
                MOVE "Invalid Year. Please enter valid 4 digit year (YYYY)." TO WS-OUT-LINE
                PERFORM DISPLAY-LINE
            END-IF
        END-PERFORM.

    PROMPT-OPTIONAL-FIELDS.
        MOVE "Enter About Me (optional):" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        PERFORM READ-INPUT
        MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-PR-ABOUT

        PERFORM ADD-EXPERIENCE
        PERFORM ADD-EDUCATION.

    ADD-EXPERIENCE.
        PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
            MOVE "Add experience entry? (Y/N)" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            PERFORM READ-INPUT

            IF INPUT-RECORD(1:1) NOT = "Y"
                EXIT PERFORM
            END-IF

            ADD 1 TO WS-PR-EXP-COUNT

            MOVE "Title:" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            PERFORM READ-INPUT
            MOVE INPUT-RECORD TO WS-PR-EXP-TITLE(I)

            MOVE "Company:" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            PERFORM READ-INPUT
            MOVE INPUT-RECORD TO WS-PR-EXP-COMPANY(I)

            MOVE "Dates:" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            PERFORM READ-INPUT
            MOVE INPUT-RECORD TO WS-PR-EXP-DATES(I)

            MOVE "Description (optional):" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            PERFORM READ-INPUT
            MOVE INPUT-RECORD TO WS-PR-EXP-DESC(I)
        END-PERFORM.

    ADD-EDUCATION.
        PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
            MOVE "Add education entry? (Y/N)" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            PERFORM READ-INPUT

            IF INPUT-RECORD(1:1) NOT = "Y"
                EXIT PERFORM
            END-IF

            ADD 1 TO WS-PR-EDU-COUNT

            MOVE "Degree:" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            PERFORM READ-INPUT
            MOVE INPUT-RECORD TO WS-PR-EDU-DEGREE(I)

            MOVE "University/College:" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            PERFORM READ-INPUT
            MOVE INPUT-RECORD TO WS-PR-EDU-SCHOOL(I)

            MOVE "Years Attended (e.g., 2023-2025):" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            PERFORM READ-INPUT
            MOVE INPUT-RECORD TO WS-PR-EDU-YEARS(I)
        END-PERFORM.

    VIEW-PROFILE.
        MOVE WS-USERNAME TO PROF-LNK-SEARCH-USERNAME
        MOVE "GCP" TO PROF-LNK-OPERATION
        CALL 'PROFILELOGIC' USING PROF-LINK-PARAMETERS

        IF PROF-LNK-RETURN-CODE = "Y"
            MOVE PROF-LNK-RECORD TO WS-PROFILE-RECORD
            MOVE "----- Your Profile -----" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            PERFORM DISPLAY-PROFILE
        ELSE
            MOVE "No profile found. Please create one first." TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
        END-IF

        EXIT PARAGRAPH.

    DISPLAY-PROFILE.
        STRING "Name: " DELIMITED BY SIZE
            FUNCTION TRIM(WS-PR-FIRST-NAME) DELIMITED BY SIZE
            " " DELIMITED BY SIZE
            FUNCTION TRIM(WS-PR-LAST-NAME) DELIMITED BY SIZE
            INTO WS-OUT-LINE
        END-STRING
        PERFORM DISPLAY-LINE

        STRING "University: " DELIMITED BY SIZE
            FUNCTION TRIM(WS-PR-UNIVERSITY) DELIMITED BY SIZE
            INTO WS-OUT-LINE
        END-STRING
        PERFORM DISPLAY-LINE

        STRING "Major: " DELIMITED BY SIZE
            FUNCTION TRIM(WS-PR-MAJOR) DELIMITED BY SIZE
            INTO WS-OUT-LINE
        END-STRING
        PERFORM DISPLAY-LINE

        STRING "Graduation Year: " DELIMITED BY SIZE
            WS-PR-GRAD-YEAR DELIMITED BY SIZE
            INTO WS-OUT-LINE
        END-STRING
        PERFORM DISPLAY-LINE

        IF FUNCTION TRIM(WS-PR-ABOUT) NOT = SPACES
            MOVE "About Me:" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            MOVE FUNCTION TRIM(WS-PR-ABOUT) TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
        END-IF

        IF WS-PR-EXP-COUNT > 0
            MOVE "Experience:" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-PR-EXP-COUNT
                STRING "Title: " DELIMITED BY SIZE
                       FUNCTION TRIM(WS-PR-EXP-TITLE(I)) DELIMITED BY SIZE
                       " | Company: " DELIMITED BY SIZE
                       FUNCTION TRIM(WS-PR-EXP-COMPANY(I)) DELIMITED BY SIZE
                       " | Dates: " DELIMITED BY SIZE
                       FUNCTION TRIM(WS-PR-EXP-DATES(I)) DELIMITED BY SIZE
                       INTO WS-OUT-LINE
                END-STRING
                PERFORM DISPLAY-LINE
                IF FUNCTION TRIM(WS-PR-EXP-DESC(I)) NOT = SPACES
                    STRING "Description: " DELIMITED BY SIZE
                           FUNCTION TRIM(WS-PR-EXP-DESC(I)) DELIMITED BY SIZE
                           INTO WS-OUT-LINE
                    END-STRING
                    PERFORM DISPLAY-LINE
                END-IF
            END-PERFORM
        END-IF

        IF WS-PR-EDU-COUNT > 0
            MOVE "Education:" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-PR-EDU-COUNT
                STRING "Degree: " DELIMITED BY SIZE
                       FUNCTION TRIM(WS-PR-EDU-DEGREE(I)) DELIMITED BY SIZE
                       " | School: " DELIMITED BY SIZE
                       FUNCTION TRIM(WS-PR-EDU-SCHOOL(I)) DELIMITED BY SIZE
                       " | Years: " DELIMITED BY SIZE
                       FUNCTION TRIM(WS-PR-EDU-YEARS(I)) DELIMITED BY SIZE
                       INTO WS-OUT-LINE
                END-STRING
                PERFORM DISPLAY-LINE
            END-PERFORM
        END-IF.

    USER-PROFILE-SEARCH.
        MOVE "Enter the full name of the person you are looking for:" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        PERFORM READ-INPUT

        IF EOF-FLAG = "Y"
            EXIT PARAGRAPH
        END-IF

        MOVE FUNCTION TRIM(INPUT-RECORD) TO PROF-LNK-SEARCH-FULLNAME
        MOVE "GFP" TO PROF-LNK-OPERATION
        CALL 'PROFILELOGIC' USING PROF-LINK-PARAMETERS

        IF PROF-LNK-RETURN-CODE = "Y"
            MOVE PROF-LNK-RECORD TO WS-PROFILE-RECORD
            MOVE "----- Found User Profile -----" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            PERFORM DISPLAY-PROFILE
            MOVE "1. Send Connection Request" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            MOVE "2. Back to Main Menu" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE

            MOVE SPACES TO MENU-CHOICE
            PERFORM UNTIL MENU-CHOICE = "1" OR MENU-CHOICE = "2" OR EOF-FLAG = "Y"
                PERFORM READ-INPUT
                IF EOF-FLAG = "Y"
                    EXIT PERFORM
                END-IF
                MOVE INPUT-RECORD(1:1) TO MENU-CHOICE
                IF MENU-CHOICE NOT = "1" AND MENU-CHOICE NOT = "2"
                    MOVE "Invalid choice." TO WS-OUT-LINE
                    PERFORM DISPLAY-LINE
                    MOVE "Enter your choice:" TO WS-OUT-LINE
                    PERFORM DISPLAY-LINE
                END-IF
            END-PERFORM
            IF MENU-CHOICE = "1"
                PERFORM SEND-CONNECTION-REQUEST
            END-IF
        ELSE
            MOVE "No one by that name could be found." TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
        END-IF.

    SEND-CONNECTION-REQUEST.
        MOVE WS-USERNAME TO PROF-LNK-SEARCH-USERNAME
        MOVE "GCP" TO PROF-LNK-OPERATION
        CALL 'PROFILELOGIC' USING PROF-LINK-PARAMETERS
        IF PROF-LNK-RETURN-CODE = "Y"
            MOVE PROF-LNK-USERNAME TO WS-PEND-SENDER-USER
            MOVE PROF-LNK-FIRST-NAME TO WS-PEND-SENDER-FIRST
            MOVE PROF-LNK-LAST-NAME TO WS-PEND-SENDER-LAST
        ELSE
            MOVE "You must create a profile before sending connection requests." TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            EXIT PARAGRAPH
        END-IF

        MOVE FUNCTION TRIM(WS-PR-USERNAME) TO WS-PEND-RECEIVER-USER
        MOVE FUNCTION TRIM(WS-PR-FIRST-NAME) TO WS-PEND-RECEIVER-FIRST
        MOVE FUNCTION TRIM(WS-PR-LAST-NAME) TO WS-PEND-RECEIVER-LAST

        IF WS-PEND-RECEIVER-USER = FUNCTION TRIM(WS-USERNAME)
            MOVE "You cannot send a connection request to yourself." TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            EXIT PARAGRAPH
        END-IF

        MOVE WS-PEND-REQUEST TO CONN-LNK-PEND-REQUEST
        MOVE "CIC" TO CONN-LNK-OPERATION
        CALL 'CONNECTIONLOGIC' USING CONN-LINK-PARAMETERS
        IF CONN-LNK-RETURN-CODE = "Y"
            STRING "You are already connected with " DELIMITED BY SIZE
            FUNCTION TRIM(WS-PEND-RECEIVER-FIRST) DELIMITED BY SIZE
            " " DELIMITED BY SIZE
            FUNCTION TRIM(WS-PEND-RECEIVER-LAST) DELIMITED BY SIZE
            "." DELIMITED BY SIZE
            INTO WS-OUT-LINE
            END-STRING
            PERFORM DISPLAY-LINE
            EXIT PARAGRAPH
        END-IF

        MOVE "CIS" TO CONN-LNK-OPERATION
        CALL 'CONNECTIONLOGIC' USING CONN-LINK-PARAMETERS
        IF CONN-LNK-RETURN-CODE = "Y"
            STRING "You have already sent a connection request to " DELIMITED BY SIZE
            FUNCTION TRIM(WS-PEND-RECEIVER-FIRST) DELIMITED BY SIZE
            " " DELIMITED BY SIZE
            FUNCTION TRIM(WS-PEND-RECEIVER-LAST) DELIMITED BY SIZE
            "." DELIMITED BY SIZE
            INTO WS-OUT-LINE
            END-STRING
            PERFORM DISPLAY-LINE
            EXIT PARAGRAPH
        END-IF

        MOVE "CIR" TO CONN-LNK-OPERATION
        CALL 'CONNECTIONLOGIC' USING CONN-LINK-PARAMETERS
        IF CONN-LNK-RETURN-CODE = "Y"
            MOVE "This user has already sent you a connection request." TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            EXIT PARAGRAPH
        END-IF

        MOVE "APR" TO CONN-LNK-OPERATION
        CALL 'CONNECTIONLOGIC' USING CONN-LINK-PARAMETERS
        IF CONN-LNK-RETURN-CODE = "Y"
            STRING "Connection request sent to " DELIMITED BY SIZE
            FUNCTION TRIM(WS-PEND-RECEIVER-FIRST) DELIMITED BY SIZE
            " " DELIMITED BY SIZE
            FUNCTION TRIM(WS-PEND-RECEIVER-LAST) DELIMITED BY SIZE
            "." DELIMITED BY SIZE
            INTO WS-OUT-LINE
            END-STRING
            PERFORM DISPLAY-LINE
        ELSE
            MOVE "Failed to send connection request." TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
        END-IF
        EXIT PARAGRAPH.

    VIEW-PENDING-REQUESTS.
        MOVE WS-USERNAME TO CONN-LNK-SEARCH-USERNAME
        MOVE "GAP" TO CONN-LNK-OPERATION
        CALL 'CONNECTIONLOGIC' USING CONN-LINK-PARAMETERS

        MOVE "----- Pending Connection Requests -----" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE

        IF CONN-LNK-RETURN-CODE = "Y"
            PERFORM VARYING I FROM 1 BY 1 UNTIL I > CONN-LNK-NUM-PEND-REQUESTS
                STRING FUNCTION TRIM(CONN-LNK-REQUEST-SENDER-FIRST(I)) DELIMITED BY SIZE
                       " " DELIMITED BY SIZE
                       FUNCTION TRIM(CONN-LNK-REQUEST-SENDER-LAST(I)) DELIMITED BY SIZE
                       " wants to connect." DELIMITED BY SIZE
                       INTO WS-OUT-LINE
                END-STRING
                PERFORM DISPLAY-LINE
                MOVE "Accept (A) or Reject (R)?" TO WS-OUT-LINE
                PERFORM DISPLAY-LINE
                PERFORM READ-INPUT
                MOVE INPUT-RECORD(1:1) TO WS-ACCEPT-CHOICE
                IF WS-ACCEPT-CHOICE = "A" OR WS-ACCEPT-CHOICE = "a"
                    MOVE CONN-LNK-PEND-REQUESTS(I) TO CONN-LNK-NEW-CONN
                    MOVE "ANC" TO CONN-LNK-OPERATION
                    CALL "CONNECTIONLOGIC" USING CONN-LINK-PARAMETERS
                    IF CONN-LNK-RETURN-CODE = "Y"
                        STRING "You are now connected with " DELIMITED BY SIZE
                               FUNCTION TRIM(CONN-LNK-REQUEST-SENDER-FIRST(I)) DELIMITED BY SIZE
                               " " DELIMITED BY SIZE
                               FUNCTION TRIM(CONN-LNK-REQUEST-SENDER-LAST(I)) DELIMITED BY SIZE
                               "." DELIMITED BY SIZE
                               INTO WS-OUT-LINE
                        END-STRING
                        PERFORM DISPLAY-LINE
                    ELSE
                        MOVE "Error accepting connection request." TO WS-OUT-LINE
                        PERFORM DISPLAY-LINE
                    END-IF
                ELSE
                    STRING "Connection request from " DELIMITED BY SIZE
                           FUNCTION TRIM(CONN-LNK-REQUEST-SENDER-FIRST(I)) DELIMITED BY SIZE
                           " " DELIMITED BY SIZE
                           FUNCTION TRIM(CONN-LNK-REQUEST-SENDER-LAST(I)) DELIMITED BY SIZE
                           " rejected." DELIMITED BY SIZE
                           INTO WS-OUT-LINE
                    END-STRING
                    PERFORM DISPLAY-LINE
                END-IF
            END-PERFORM
        ELSE
            MOVE "You have no pending connection requests at this time." TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
        END-IF

        MOVE "-----------------------------------" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE.

    VIEW-MY-NETWORK.
        MOVE WS-USERNAME TO CONN-LNK-SEARCH-USERNAME
        MOVE "GAC" TO CONN-LNK-OPERATION
        CALL 'CONNECTIONLOGIC' USING CONN-LINK-PARAMETERS

        MOVE "----- Your Network -----" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE

        IF CONN-LNK-RETURN-CODE = "Y"
            PERFORM VARYING I FROM 1 BY 1 UNTIL I > CONN-LNK-NUM-CONNECTIONS
                IF CONN-LNK-CONN-USER1(I) = FUNCTION TRIM(WS-USERNAME)
                    STRING FUNCTION TRIM(CONN-LNK-CONN-USER2-FIRST(I)) DELIMITED BY SIZE
                           " " DELIMITED BY SIZE
                           FUNCTION TRIM(CONN-LNK-CONN-USER2-LAST(I)) DELIMITED BY SIZE
                           INTO WS-OUT-LINE
                    END-STRING
                    MOVE CONN-LNK-CONN-USER2(I) TO PROF-LNK-SEARCH-USERNAME
                ELSE
                    STRING FUNCTION TRIM(CONN-LNK-CONN-USER1-FIRST(I)) DELIMITED BY SIZE
                           " " DELIMITED BY SIZE
                           FUNCTION TRIM(CONN-LNK-CONN-USER1-LAST(I)) DELIMITED BY SIZE
                           INTO WS-OUT-LINE
                    END-STRING
                    MOVE CONN-LNK-CONN-USER1(I) TO PROF-LNK-SEARCH-USERNAME
                END-IF
                PERFORM DISPLAY-LINE
                MOVE "GCP" TO PROF-LNK-OPERATION
                CALL 'PROFILELOGIC' USING PROF-LINK-PARAMETERS
                STRING "  University: " DELIMITED BY SIZE
                       FUNCTION TRIM(PROF-LNK-UNIVERSITY) DELIMITED BY SIZE
                       INTO WS-OUT-LINE
                END-STRING
                PERFORM DISPLAY-LINE
                STRING "  Major: " DELIMITED BY SIZE
                       FUNCTION TRIM(PROF-LNK-MAJOR) DELIMITED BY SIZE
                       INTO WS-OUT-LINE
                END-STRING
                PERFORM DISPLAY-LINE
            END-PERFORM
        ELSE
            MOVE "You have no connections yet." TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
        END-IF

        MOVE "-----------------------------------" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        EXIT PARAGRAPH.

    JOB-SEARCH-MENU.
        IF EOF-FLAG = "Y"
            EXIT PARAGRAPH
        END-IF
        MOVE "--- Job Search/Internship Menu ---" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        MOVE "1. Post a Job/Internship" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        MOVE "2. Browse Jobs/Internships" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        MOVE "3. View My Applications" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        MOVE "4. Back to Main Menu" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        MOVE "Enter your choice:" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        PERFORM READ-INPUT
        IF EOF-FLAG = "Y"
            EXIT PARAGRAPH
        END-IF
        MOVE INPUT-RECORD(1:1) TO MENU-CHOICE

        EVALUATE MENU-CHOICE
            WHEN "1"
                PERFORM POST-JOB
                PERFORM JOB-SEARCH-MENU
            WHEN "2"
                PERFORM BROWSE-JOBS
                PERFORM JOB-SEARCH-MENU
            WHEN "3"
                PERFORM VIEW-MY-APPLICATIONS
                PERFORM JOB-SEARCH-MENU
            WHEN "4"
                CONTINUE
            WHEN OTHER
                MOVE "Invalid choice." TO WS-OUT-LINE
                PERFORM DISPLAY-LINE
                PERFORM JOB-SEARCH-MENU
        END-EVALUATE

        EXIT PARAGRAPH.

       COPY "src/Applyjob.cob".
       COPY "src/Viewapplications.cob".

    POST-JOB.
        MOVE "--- Post a Job/Internship ---" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE

        MOVE SPACES TO WS-JOB-TITLE
        PERFORM UNTIL WS-JOB-TITLE NOT = SPACES
            MOVE "Job Title (required):" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            PERFORM READ-INPUT
            IF EOF-FLAG = "Y"
                EXIT PARAGRAPH
            END-IF
            MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-JOB-TITLE
            IF WS-JOB-TITLE = SPACES
                MOVE "Job Title is required. Please enter a value." TO WS-OUT-LINE
                PERFORM DISPLAY-LINE
            END-IF
        END-PERFORM

        MOVE SPACES TO WS-JOB-DESCRIPTION
        PERFORM UNTIL WS-JOB-DESCRIPTION NOT = SPACES
            MOVE "Description (required):" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            PERFORM READ-INPUT
            IF EOF-FLAG = "Y"
                EXIT PARAGRAPH
            END-IF
            MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-JOB-DESCRIPTION
            IF WS-JOB-DESCRIPTION = SPACES
                MOVE "Description is required. Please enter a value." TO WS-OUT-LINE
                PERFORM DISPLAY-LINE
            END-IF
        END-PERFORM

        MOVE SPACES TO WS-JOB-EMPLOYER
        PERFORM UNTIL WS-JOB-EMPLOYER NOT = SPACES
            MOVE "Employer (required):" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            PERFORM READ-INPUT
            IF EOF-FLAG = "Y"
                EXIT PARAGRAPH
            END-IF
            MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-JOB-EMPLOYER
            IF WS-JOB-EMPLOYER = SPACES
                MOVE "Employer is required. Please enter a value." TO WS-OUT-LINE
                PERFORM DISPLAY-LINE
            END-IF
        END-PERFORM

        MOVE SPACES TO WS-JOB-LOCATION
        PERFORM UNTIL WS-JOB-LOCATION NOT = SPACES
            MOVE "Location (required):" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            PERFORM READ-INPUT
            IF EOF-FLAG = "Y"
                EXIT PARAGRAPH
            END-IF
            MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-JOB-LOCATION
            IF WS-JOB-LOCATION = SPACES
                MOVE "Location is required. Please enter a value." TO WS-OUT-LINE
                PERFORM DISPLAY-LINE
            END-IF
        END-PERFORM

        MOVE "Salary (optional, press Enter to skip):" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        PERFORM READ-INPUT
        IF EOF-FLAG = "Y"
            EXIT PARAGRAPH
        END-IF
        MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-JOB-SALARY
        MOVE 0 TO WS-JOB-ID
        MOVE WS-USERNAME TO WS-JOB-POSTED-BY

        MOVE WS-JOB-RECORD TO JOB-LNK-JOB-RECORD
        MOVE "ANJ" TO JOB-LNK-OPERATION
        CALL 'JOBLOGIC' USING JOB-LINK-PARAMETERS
        IF JOB-LNK-RETURN-CODE = "Y"
            STRING "Job posting #" DELIMITED BY SIZE
                   JOB-LNK-RETURN-ID DELIMITED BY SIZE
                   " created successfully." DELIMITED BY SIZE
                   INTO WS-OUT-LINE
            END-STRING
            PERFORM DISPLAY-LINE
        ELSE
            MOVE "Error posting job/internship." TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
        END-IF
        EXIT PARAGRAPH.

    MESSAGE-MENU.
        IF EOF-FLAG = "Y"
            EXIT PARAGRAPH
        END-IF

        MOVE "--- Messages Menu ---" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE

        MOVE "1. Send a New Message" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        MOVE "2. View My Messages" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE
        MOVE "3. Back to Main Menu" TO WS-OUT-LINE
        PERFORM DISPLAY-LINE

        PERFORM READ-INPUT
        MOVE INPUT-RECORD(1:1) TO MENU-CHOICE

        EVALUATE MENU-CHOICE
            WHEN "1"
                MOVE "Enter recepient's username (must be a connection):" TO WS-OUT-LINE
                PERFORM DISPLAY-LINE
                PERFORM READ-INPUT
                IF EOF-FLAG = "Y"
                    EXIT PARAGRAPH
                END-IF
                MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-MSG-RECIPIENT
                MOVE WS-USERNAME TO WS-MSG-SENDER
                MOVE WS-USERNAME TO CONN-LNK-SENDER-USER
                MOVE WS-MSG-RECIPIENT TO CONN-LNK-RECEIVER-USER
                MOVE "CIC" TO CONN-LNK-OPERATION
                CALL 'CONNECTIONLOGIC' USING CONN-LINK-PARAMETERS
                IF CONN-LNK-RETURN-CODE = "Y"
                    MOVE "Enter your message (max 100 chars):" TO WS-OUT-LINE
                    PERFORM DISPLAY-LINE
                    PERFORM READ-INPUT
                    IF EOF-FLAG = "Y"
                        EXIT PARAGRAPH
                    END-IF
                    MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-MSG-CONTENT
                    MOVE FUNCTION CURRENT-DATE TO WS-MSG-TIMESTAMP
                    MOVE WS-MSG-RECORD TO MSG-LNK-RECORD
                    MOVE "SNM" TO MSG-LNK-OPERATION
                    CALL 'MESSAGELOGIC' USING MSG-LINK-PARAMETERS
                    IF MSG-LNK-RETURN-CODE = "Y"
                        MOVE "Message sent successfully." TO WS-OUT-LINE
                        PERFORM DISPLAY-LINE
                        PERFORM MESSAGE-MENU
                    ELSE
                        MOVE "Error sending message." TO WS-OUT-LINE
                        PERFORM DISPLAY-LINE
                        PERFORM MESSAGE-MENU
                    END-IF
                ELSE
                    MOVE "You can only send messages to users you're connected with." TO WS-OUT-LINE
                    PERFORM DISPLAY-LINE
                    PERFORM MESSAGE-MENU
                END-IF
            WHEN "2"
                MOVE FUNCTION TRIM(WS-USERNAME) TO MSG-LNK-USERNAME
                MOVE "VWM" TO  MSG-LNK-OPERATION
                CALL 'MESSAGELOGIC' USING MSG-LINK-PARAMETERS

                IF MSG-LNK-NUM-RECORDS = 0
                   MOVE "You have no messages at this time." TO WS-OUT-LINE
                   PERFORM DISPLAY-LINE
                ELSE
                   PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > MSG-LNK-NUM-RECORDS
                       STRING "From: " DELIMITED BY SIZE
                           FUNCTION TRIM(MSG-LNK-REC-SENDER (IDX)) DELIMITED BY SIZE
                           INTO WS-OUT-LINE
                       END-STRING
                       PERFORM DISPLAY-LINE

                       STRING "Message: " DELIMITED BY SIZE
                           FUNCTION TRIM(MSG-LNK-REC-CONTENT (IDX)) DELIMITED BY SIZE
                           INTO WS-OUT-LINE
                       END-STRING
                       PERFORM DISPLAY-LINE

                       IF LENGTH FUNCTION TRIM(MSG-LNK-REC-TIMESTAMP (IDX)) > 0
                           STRING "(Optional: Sent: " DELIMITED BY SIZE
                               FUNCTION TRIM(MSG-LNK-REC-TIMESTAMP (IDX)) DELIMITED BY SIZE
                               ")" DELIMITED BY SIZE
                               INTO WS-OUT-LINE
                           END-STRING
                           PERFORM DISPLAY-LINE
                       END-IF

                       MOVE "---------------------" TO WS-OUT-LINE
                       PERFORM DISPLAY-LINE
                   END-PERFORM
                END-IF

                PERFORM MESSAGE-MENU

            WHEN "3"
                CONTINUE
            WHEN OTHER
                MOVE "Invalid choice." TO WS-OUT-LINE
                PERFORM DISPLAY-LINE
                PERFORM MESSAGE-MENU
        END-EVALUATE

        EXIT PARAGRAPH.

    READ-INPUT.
        READ INPUT-FILE
            AT END
                MOVE "Y" TO EOF-FLAG
            NOT AT END
                DISPLAY INPUT-RECORD
                MOVE INPUT-RECORD TO OUTPUT-RECORD
                WRITE OUTPUT-RECORD
        END-READ.

    DISPLAY-LINE.
        DISPLAY WS-OUT-LINE
        MOVE WS-OUT-LINE TO OUTPUT-RECORD
        WRITE OUTPUT-RECORD
        MOVE SPACES TO WS-OUT-LINE.
