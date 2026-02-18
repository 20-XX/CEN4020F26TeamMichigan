IDENTIFICATION DIVISION.
    PROGRAM-ID. INCOLLEGEDRIVER.

ENVIRONMENT DIVISION.
    INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT INPUT-FILE ASSIGN TO "InCollege-Input.txt"
                ORGANIZATION IS LINE SEQUENTIAL.
            SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
                ORGANIZATION IS LINE SEQUENTIAL.
            SELECT ACCOUNT-FILE ASSIGN TO "Accounts.dat"
                ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
    FILE SECTION.

        FD INPUT-FILE.
            01 INPUT-RECORD    PIC X(100).

        FD OUTPUT-FILE.
            01 OUTPUT-RECORD    PIC X(200).

        FD ACCOUNT-FILE.
            01 ACCOUNT-RECORD.
                05 ACC-USERNAME    PIC X(20).
                05 ACC-PASSWORD    PIC X(12).

    WORKING-STORAGE SECTION.

        77 EOF-FLAG    PIC X VALUE "N".
        77 ACC-EOF    PIC X VALUE "N".
        77 ACCOUNT-COUNT    PIC 9 VALUE 0.
        77 MENU-CHOICE    PIC X.
        77 LOGIN-SUCCESS    PIC X VALUE "N".

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
        01 I    PIC 9(2).

        01 ACCT-LINK-PARAMETERS.
            05 ACCT-LNK-OPERATION    PIC X(2).
            05 ACCT-LNK-USERNAME    PIC X(20).
            05 ACCT-LNK-PASSWORD    PIC X(50).
            05 ACCT-LNK-RETURN-CODE    PIC X.
        01 PROF-LINK-PARAMETERS.
            05 PROF-LNK-OPERATION    PIC X(2).
            05 PROF-LNK-RETURN-CODE    PIC X.
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

PROCEDURE DIVISION.
    OPEN INPUT INPUT-FILE
    OPEN OUTPUT OUTPUT-FILE
    OPEN INPUT ACCOUNT-FILE

    PERFORM LOAD-ACCOUNTS

    CLOSE ACCOUNT-FILE

    PERFORM UNTIL EOF-FLAG = "Y"
        PERFORM MAIN-MENU
    END-PERFORM

    CLOSE INPUT-FILE
    CLOSE OUTPUT-FILE
    STOP RUN.

    LOAD-ACCOUNTS.
        MOVE "N" TO ACC-EOF
        PERFORM UNTIL ACC-EOF = "Y"
            READ ACCOUNT-FILE
                AT END
                    MOVE "Y" TO ACC-EOF
                NOT AT END
                    ADD 1 TO ACCOUNT-COUNT
            END-READ
        END-PERFORM.

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
        IF ACCOUNT-COUNT >= 5
            MOVE "All permitted accounts have been created, please come back later"
                TO WS-OUT-LINE
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

        CALL 'ACCTMGR' USING ACCT-LNK-OPERATION, ACCT-LNK-USERNAME, ACCT-LNK-PASSWORD, ACCT-LNK-RETURN-CODE

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

        CALL 'ACCTMGR' USING ACCT-LNK-OPERATION, ACCT-LNK-USERNAME, ACCT-LNK-PASSWORD, ACCT-LNK-RETURN-CODE

        IF ACCT-LNK-RETURN-CODE = "Y"
             MOVE "AA" TO ACCT-LNK-OPERATION
             CALL 'ACCTMGR' USING ACCT-LNK-OPERATION, ACCT-LNK-USERNAME, ACCT-LNK-PASSWORD, ACCT-LNK-RETURN-CODE
             ADD 1 TO ACCOUNT-COUNT
             MOVE "Account successfully created" TO WS-OUT-LINE
             PERFORM DISPLAY-LINE
        ELSE
            MOVE "Password does not meet requirements" TO WS-OUT-LINE
            PERFORM DISPLAY-LINE
            EXIT PARAGRAPH
        END-IF

        EXIT PARAGRAPH.

    LOGIN-HANDLER.
        MOVE "N" TO LOGIN-SUCCESS

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

        CALL 'ACCTMGR' USING ACCT-LNK-OPERATION, ACCT-LNK-USERNAME, ACCT-LNK-PASSWORD, ACCT-LNK-RETURN-CODE

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
        MOVE SPACES TO WS-OUT-LINE
        STRING "Welcome, " DELIMITED BY SIZE
            FUNCTION TRIM(WS-USERNAME) DELIMITED BY SIZE
            "!" DELIMITED BY SIZE
            INTO WS-OUT-LINE
        END-STRING
        PERFORM DISPLAY-LINE

        MOVE "N" TO MENU-CHOICE
        PERFORM UNTIL MENU-CHOICE = "6" OR EOF-FLAG = "Y"
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
            MOVE "6. Logout" TO WS-OUT-LINE
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
                    MOVE "Job search/internship is under construction."
                        TO WS-OUT-LINE
                    PERFORM DISPLAY-LINE
                WHEN "2"
                    MOVE "Find someone you know is under construction."
                        TO WS-OUT-LINE
                    PERFORM DISPLAY-LINE
                WHEN "3"
                    PERFORM SKILL-MENU
                WHEN "4"
                    PERFORM PROFILE-MENU
                WHEN "5"
                    MOVE "Profile viewing is under construction."
                        TO WS-OUT-LINE
                    PERFORM DISPLAY-LINE
                WHEN "6"
                    CONTINUE
            END-EVALUATE
        END-PERFORM

        EXIT PARAGRAPH.

    SKILL-MENU.
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

        MOVE "SV" TO PROF-LNK-OPERATION
        MOVE WS-PROFILE-RECORD TO PROF-LNK-RECORD
        CALL 'PROFMGR' USING PROF-LNK-OPERATION, PROF-LNK-RETURN-CODE, PROF-LNK-RECORD
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
        WRITE OUTPUT-RECORD.
