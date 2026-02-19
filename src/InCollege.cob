       IDENTIFICATION DIVISION.
       PROGRAM-ID. INCOLLEGE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ACCOUNT-FILE ASSIGN TO "Accounts.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PROFILE-FILE ASSIGN TO "Profiles.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PROFILE-TEMP ASSIGN TO "Profiles.tmp"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PENDING-FILE ASSIGN TO "PendingRequests.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS PEND-FS.


       DATA DIVISION.
       FILE SECTION.

       FD INPUT-FILE.
       01 INPUT-RECORD            PIC X(100).

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD           PIC X(200).

       FD ACCOUNT-FILE.
       01 ACCOUNT-RECORD.
           05 ACC-USERNAME        PIC X(20).
           05 ACC-PASSWORD        PIC X(12).
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

       FD PENDING-FILE.
       01 PENDING-RECORD.
           05 PEND-SENDER-USER     PIC X(20).
           05 PEND-SENDER-FIRST    PIC X(20).
           05 PEND-SENDER-LAST     PIC X(20).
           05 PEND-RECEIVER-USER   PIC X(20).
           05 PEND-RECEIVER-FIRST  PIC X(20).
           05 PEND-RECEIVER-LAST   PIC X(20).


       WORKING-STORAGE SECTION.

       77 EOF-FLAG                PIC X VALUE "N".
       77 ACC-EOF                 PIC X VALUE "N".
       77 ACCOUNT-COUNT           PIC 9 VALUE 0.
       77 MENU-CHOICE             PIC X.
       77 LOGIN-SUCCESS           PIC X VALUE "N".
       77 USER-FOUND              PIC X VALUE "N".
       77 PASSWORD-VALID          PIC X VALUE "N".
       77 CNT-UPPER PIC 9(3) VALUE 0.
       77 CNT-DIGIT PIC 9(3) VALUE 0.
       77 CNT-SPECIAL PIC 9(3) VALUE 0.
       77 PROFILE-EOF PIC X VALUE "N".
       77 PROFILE-FOUND PIC X VALUE "N".

       77 PEND-EOF     PIC X VALUE "N".
       77 PEND-FOUND   PIC X VALUE "N".
       77 PEND-FS      PIC XX VALUE "00".


       01 WS-USERNAME             PIC X(20).
       01 WS-PASSWORD             PIC X(50).
       01 WS-OUT-LINE             PIC X(100).

       01 PASSWORD-FLAGS.
           05 HAS-UPPER           PIC X VALUE "N".
           05 HAS-DIGIT           PIC X VALUE "N".
           05 HAS-SPECIAL         PIC X VALUE "N".

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

       01 WS-PEND-RECORD.
           05 WS-PEND-SENDER-USER      PIC X(20).
           05 WS-PEND-SENDER-FIRST     PIC X(20).
           05 WS-PEND-SENDER-LAST      PIC X(20).
           05 WS-PEND-RECEIVER-USER    PIC X(20).
           05 WS-PEND-RECEIVER-FIRST   PIC X(20).
           05 WS-PEND-RECEIVER-LAST    PIC X(20).

       01 I                       PIC 9(2).


       01 HASH-VALUE              PIC 9(10) VALUE 0.
       01 HASH-CHAR               PIC 9(3).
       01 WS-HASHED-PASSWORD      PIC X(12).
       01 TEMP-HASH               PIC 9(10).

       01 WS-YEAR-INPUT           PIC X(4).

       01 WS-SEARCH-FULLNAME      PIC X(50).
       01 WS-SEARCH-FIRST-NAME    PIC X(20).
       01 WS-SEARCH-LAST-NAME     PIC X(20).
       01 WS-SEARCH-SPACE-LOC     PIC 9(2) VALUE 0.
       01 WS-SEARCH-USER-FOUND    PIC X(1) VALUE "N".


       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE
           OPEN INPUT ACCOUNT-FILE
           OPEN INPUT PROFILE-FILE
           OPEN INPUT PENDING-FILE

           IF PEND-FS NOT = "00"
               OPEN OUTPUT PENDING-FILE
               CLOSE PENDING-FILE
               OPEN INPUT PENDING-FILE
           END-IF


           PERFORM LOAD-ACCOUNTS

           PERFORM UNTIL EOF-FLAG = "Y"
               PERFORM MAIN-MENU
           END-PERFORM


           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           CLOSE ACCOUNT-FILE
           CLOSE PROFILE-FILE
           CLOSE PENDING-FILE

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
       HASH-PASSWORD.
           MOVE 0 TO HASH-VALUE
           MOVE SPACES TO WS-HASHED-PASSWORD

           PERFORM VARYING I FROM 1 BY 1
                UNTIL I > FUNCTION LENGTH(
                    FUNCTION TRIM(WS-PASSWORD))
                COMPUTE HASH-CHAR = FUNCTION ORD(WS-PASSWORD(I:1))
                COMPUTE TEMP-HASH = HASH-VALUE * 31
                COMPUTE HASH-VALUE = FUNCTION MOD(TEMP-HASH + HASH-CHAR, 999999999)
           END-PERFORM

           STRING HASH-VALUE DELIMITED BY SIZE
               INTO WS-HASHED-PASSWORD
           END-STRING.

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
               PERFORM LOGIN
           ELSE
               IF MENU-CHOICE = "2"
                   PERFORM CREATE-ACCOUNT
               END-IF
           END-IF

           EXIT PARAGRAPH.

       CREATE-ACCOUNT.
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


           PERFORM CHECK-USERNAME

           IF USER-FOUND = "Y"
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

           PERFORM VALIDATE-PASSWORD

           IF FUNCTION LENGTH(FUNCTION TRIM(WS-PASSWORD)) < 8
               OR FUNCTION LENGTH(FUNCTION TRIM(WS-PASSWORD)) > 12
               MOVE "Password does not meet requirements" TO WS-OUT-LINE
               PERFORM DISPLAY-LINE
               EXIT PARAGRAPH
           END-IF

           IF HAS-UPPER = "Y" AND HAS-DIGIT = "Y" AND HAS-SPECIAL = "Y"
               PERFORM HASH-PASSWORD
               CLOSE ACCOUNT-FILE
               OPEN EXTEND ACCOUNT-FILE
               MOVE WS-USERNAME TO ACC-USERNAME
               MOVE WS-HASHED-PASSWORD TO ACC-PASSWORD
               WRITE ACCOUNT-RECORD
               CLOSE ACCOUNT-FILE
               OPEN INPUT ACCOUNT-FILE
               ADD 1 TO ACCOUNT-COUNT
               MOVE "Account successfully created" TO WS-OUT-LINE
               PERFORM DISPLAY-LINE
           ELSE
               MOVE "Password does not meet requirements" TO WS-OUT-LINE
               PERFORM DISPLAY-LINE
           END-IF

           EXIT PARAGRAPH.

       CHECK-USERNAME.
           MOVE "N" TO USER-FOUND
           MOVE "N" TO ACC-EOF
           CLOSE ACCOUNT-FILE
           OPEN INPUT ACCOUNT-FILE

           PERFORM UNTIL ACC-EOF = "Y"
               READ ACCOUNT-FILE
                   AT END
                       MOVE "Y" TO ACC-EOF
                   NOT AT END
                       IF WS-USERNAME = ACC-USERNAME
                           MOVE "Y" TO USER-FOUND
                       END-IF
               END-READ
           END-PERFORM.
       VALIDATE-PASSWORD.

           MOVE "N" TO HAS-UPPER HAS-DIGIT HAS-SPECIAL PASSWORD-VALID
           MOVE 0 TO CNT-UPPER CNT-DIGIT CNT-SPECIAL

           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-PASSWORD)) TO I

           IF I < 8 OR I > 12
               MOVE "N" TO PASSWORD-VALID
               EXIT PARAGRAPH
           END-IF

           INSPECT WS-PASSWORD TALLYING CNT-UPPER FOR ALL "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
           IF CNT-UPPER > 0
               MOVE "Y" TO HAS-UPPER
           END-IF

           INSPECT WS-PASSWORD TALLYING CNT-DIGIT FOR ALL "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
           IF CNT-DIGIT > 0
               MOVE "Y" TO HAS-DIGIT
           END-IF

           INSPECT WS-PASSWORD TALLYING CNT-SPECIAL FOR ALL "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "-" "_" "+" "=" "~" "`" "[" "]" "{" "}" "|" "\" ":" ";" "'" '"' "<" ">" "," "." "?" "/"
           IF CNT-SPECIAL > 0
               MOVE "Y" TO HAS-SPECIAL
           END-IF

           IF HAS-UPPER = "Y" AND HAS-DIGIT = "Y" AND HAS-SPECIAL = "Y"
               MOVE "Y" TO PASSWORD-VALID
           END-IF.


       LOGIN.
           MOVE "N" TO LOGIN-SUCCESS

           MOVE "Please enter your username:" TO WS-OUT-LINE
           PERFORM DISPLAY-LINE
           PERFORM READ-INPUT
           IF EOF-FLAG = "Y"
               EXIT PARAGRAPH
           END-IF
           MOVE INPUT-RECORD TO WS-USERNAME

           MOVE "Please enter your password:" TO WS-OUT-LINE
           PERFORM DISPLAY-LINE
           PERFORM READ-INPUT
           IF EOF-FLAG = "Y"
               EXIT PARAGRAPH
           END-IF
           MOVE INPUT-RECORD TO WS-PASSWORD

           PERFORM HASH-PASSWORD

           MOVE "N" TO ACC-EOF
           CLOSE ACCOUNT-FILE
           OPEN INPUT ACCOUNT-FILE

           PERFORM UNTIL ACC-EOF = "Y"
               READ ACCOUNT-FILE
                   AT END
                       MOVE "Y" TO ACC-EOF
                   NOT AT END
                       IF FUNCTION TRIM(ACC-USERNAME) =
                           FUNCTION TRIM(WS-USERNAME)
                           IF FUNCTION TRIM(ACC-PASSWORD) =
                               FUNCTION TRIM(WS-HASHED-PASSWORD)
                               MOVE "Y" TO LOGIN-SUCCESS
                           END-IF
                       END-IF
               END-READ
           END-PERFORM

           IF LOGIN-SUCCESS = "Y"
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
           PERFORM UNTIL MENU-CHOICE = "7" OR EOF-FLAG = "Y"
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
               MOVE "7. Logout" TO WS-OUT-LINE
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

           PERFORM SAVE-PROFILE

           MOVE "Profile saved successfully." TO WS-OUT-LINE
           PERFORM DISPLAY-LINE

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
      SAVE-PROFILE.
           MOVE "N" TO PROFILE-FOUND
           MOVE "N" TO PROFILE-EOF

           CLOSE PROFILE-FILE
           OPEN INPUT PROFILE-FILE
           OPEN OUTPUT PROFILE-TEMP

           PERFORM UNTIL PROFILE-EOF = "Y"
               READ PROFILE-FILE
                   AT END
                       MOVE "Y" TO PROFILE-EOF
                   NOT AT END
                       IF PR-USERNAME = WS-USERNAME
                           MOVE "Y" TO PROFILE-FOUND
                       ELSE
                           MOVE PROFILE-RECORD TO PROFILE-TEMP-RECORD
                           WRITE PROFILE-TEMP-RECORD
                       END-IF
               END-READ
           END-PERFORM


           MOVE WS-PROFILE-RECORD TO PROFILE-RECORD
           MOVE PROFILE-RECORD TO PROFILE-TEMP-RECORD
           WRITE PROFILE-TEMP-RECORD

           CLOSE PROFILE-FILE
           CLOSE PROFILE-TEMP

           CALL 'SYSTEM' USING "mv Profiles.tmp Profiles.dat"

           OPEN INPUT PROFILE-FILE.


       PROMPT-REQUIRED-FIELDS.
           MOVE SPACES TO WS-PR-FIRST-NAME

           PERFORM UNTIL WS-PR-FIRST-NAME NOT = SPACES
               MOVE "Enter First Name:" TO WS-OUT-LINE
               PERFORM DISPLAY-LINE
               PERFORM READ-INPUT
               MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-PR-FIRST-NAME
           END-PERFORM

           MOVE SPACES TO WS-PR-LAST-NAME

           PERFORM UNTIL WS-PR-LAST-NAME NOT = SPACES
               MOVE "Enter Last Name:" TO WS-OUT-LINE
               PERFORM DISPLAY-LINE
               PERFORM READ-INPUT
               MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-PR-LAST-NAME
           END-PERFORM

           MOVE SPACES TO WS-PR-UNIVERSITY

           PERFORM UNTIL WS-PR-UNIVERSITY NOT = SPACES
                MOVE "Enter University/College Attended:" TO WS-OUT-LINE
                PERFORM DISPLAY-LINE
                PERFORM READ-INPUT
                MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-PR-UNIVERSITY
            END-PERFORM

            MOVE SPACES TO WS-PR-MAJOR

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
           MOVE 0 TO WS-PR-EXP-COUNT

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
           MOVE 0 TO WS-PR-EDU-COUNT

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
           MOVE "N" TO PROFILE-FOUND
           MOVE "N" TO PROFILE-EOF

           CLOSE PROFILE-FILE
           OPEN INPUT PROFILE-FILE

           PERFORM UNTIL PROFILE-EOF = "Y" OR PROFILE-FOUND = "Y"
               READ PROFILE-FILE
                   AT END
                       MOVE "Y" TO PROFILE-EOF
                   NOT AT END
                       IF FUNCTION TRIM(PR-USERNAME) =
                           FUNCTION TRIM(WS-USERNAME)
                           MOVE "Y" TO PROFILE-FOUND
                       END-IF
               END-READ
           END-PERFORM

           IF PROFILE-FOUND = "Y"
               MOVE "----- Your Profile -----" TO WS-OUT-LINE
               PERFORM DISPLAY-LINE

               MOVE SPACES TO WS-OUT-LINE
               STRING "Name: " DELIMITED BY SIZE
                   FUNCTION TRIM(PR-FIRST-NAME) DELIMITED BY SIZE
                   " " DELIMITED BY SIZE
                   FUNCTION TRIM(PR-LAST-NAME) DELIMITED BY SIZE
                   INTO WS-OUT-LINE
               END-STRING
               PERFORM DISPLAY-LINE

               MOVE SPACES TO WS-OUT-LINE
               STRING "University: " DELIMITED BY SIZE
                   FUNCTION TRIM(PR-UNIVERSITY) DELIMITED BY SIZE
                   INTO WS-OUT-LINE
               END-STRING
               PERFORM DISPLAY-LINE

               MOVE SPACES TO WS-OUT-LINE
               STRING "Major: " DELIMITED BY SIZE
                   FUNCTION TRIM(PR-MAJOR) DELIMITED BY SIZE
                   INTO WS-OUT-LINE
               END-STRING
               PERFORM DISPLAY-LINE

               MOVE SPACES TO WS-OUT-LINE
               STRING "Graduation Year: " DELIMITED BY SIZE
                   PR-GRAD-YEAR DELIMITED BY SIZE
                   INTO WS-OUT-LINE
               END-STRING
               PERFORM DISPLAY-LINE

               IF FUNCTION TRIM(PR-ABOUT) NOT = SPACES
                   MOVE "About Me:" TO WS-OUT-LINE
                   PERFORM DISPLAY-LINE
                   MOVE FUNCTION TRIM(PR-ABOUT) TO WS-OUT-LINE
                   PERFORM DISPLAY-LINE
               END-IF

               IF PR-EXP-COUNT > 0
                   MOVE "Experience:" TO WS-OUT-LINE
                   PERFORM DISPLAY-LINE
                   PERFORM VARYING I FROM 1 BY 1 UNTIL I > PR-EXP-COUNT
                       MOVE SPACES TO WS-OUT-LINE
                       STRING "Title: " DELIMITED BY SIZE
                              FUNCTION TRIM(PR-EXP-TITLE(I)) DELIMITED BY SIZE
                              " | Company: " DELIMITED BY SIZE
                              FUNCTION TRIM(PR-EXP-COMPANY(I)) DELIMITED BY SIZE
                              " | Dates: " DELIMITED BY SIZE
                              FUNCTION TRIM(PR-EXP-DATES(I)) DELIMITED BY SIZE
                              INTO WS-OUT-LINE
                       END-STRING
                       PERFORM DISPLAY-LINE
                       IF FUNCTION TRIM(PR-EXP-DESC(I)) NOT = SPACES
                           MOVE SPACES TO WS-OUT-LINE
                           STRING "Description: " DELIMITED BY SIZE
                                  FUNCTION TRIM(PR-EXP-DESC(I)) DELIMITED BY SIZE
                                  INTO WS-OUT-LINE
                           END-STRING
                           PERFORM DISPLAY-LINE
                       END-IF
                   END-PERFORM
               END-IF

               IF PR-EDU-COUNT > 0
                   MOVE "Education:" TO WS-OUT-LINE
                   PERFORM DISPLAY-LINE
                   PERFORM VARYING I FROM 1 BY 1 UNTIL I > PR-EDU-COUNT
                       MOVE SPACES TO WS-OUT-LINE
                       STRING "Degree: " DELIMITED BY SIZE
                              FUNCTION TRIM(PR-EDU-DEGREE(I)) DELIMITED BY SIZE
                              " | School: " DELIMITED BY SIZE
                              FUNCTION TRIM(PR-EDU-SCHOOL(I)) DELIMITED BY SIZE
                              " | Years: " DELIMITED BY SIZE
                              FUNCTION TRIM(PR-EDU-YEARS(I)) DELIMITED BY SIZE
                              INTO WS-OUT-LINE
                       END-STRING
                       PERFORM DISPLAY-LINE
                   END-PERFORM
               END-IF
           ELSE
               MOVE "No profile found. Please create one first." TO WS-OUT-LINE
               PERFORM DISPLAY-LINE
           END-IF

           EXIT PARAGRAPH.


       USER-PROFILE-SEARCH.
           MOVE "N" TO WS-SEARCH-USER-FOUND
           MOVE "Enter the full name of the person you are looking for:" TO WS-OUT-LINE
           PERFORM DISPLAY-LINE
           PERFORM READ-INPUT

           IF EOF-FLAG = "Y"
               EXIT PARAGRAPH
           END-IF

           MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-SEARCH-FULLNAME

           PERFORM PARSE-ENTERED-SEARCH

           MOVE "N" TO PROFILE-EOF
           CLOSE PROFILE-FILE
           OPEN INPUT PROFILE-FILE

           PERFORM UNTIL PROFILE-EOF = "Y" OR WS-SEARCH-USER-FOUND = "Y"
                   READ PROFILE-FILE
                       AT END
                           MOVE "Y" TO PROFILE-EOF
                       NOT AT END
                           IF FUNCTION TRIM(PR-FIRST-NAME) = WS-SEARCH-FIRST-NAME AND
                               FUNCTION TRIM(PR-LAST-NAME) = WS-SEARCH-LAST-NAME
                               MOVE "Y" TO WS-SEARCH-USER-FOUND
                           END-IF
                   END-READ
           END-PERFORM

                IF WS-SEARCH-USER-FOUND = "Y"
                    MOVE "----- Found User Profile -----" TO WS-OUT-LINE
                    PERFORM DISPLAY-LINE

                    MOVE SPACES TO WS-OUT-LINE
                    STRING "Name: " DELIMITED BY SIZE
                        FUNCTION TRIM(PR-FIRST-NAME) DELIMITED BY SIZE
                        " " DELIMITED BY SIZE
                        FUNCTION TRIM(PR-LAST-NAME) DELIMITED BY SIZE
                        INTO WS-OUT-LINE
                    END-STRING
                    PERFORM DISPLAY-LINE

                    MOVE SPACES TO WS-OUT-LINE
                    STRING "University: " DELIMITED BY SIZE
                        FUNCTION TRIM(PR-UNIVERSITY) DELIMITED BY SIZE
                        INTO WS-OUT-LINE
                    END-STRING
                    PERFORM DISPLAY-LINE

                    MOVE SPACES TO WS-OUT-LINE
                    STRING "Major: " DELIMITED BY SIZE
                        FUNCTION TRIM(PR-MAJOR) DELIMITED BY SIZE
                        INTO WS-OUT-LINE
                    END-STRING
                    PERFORM DISPLAY-LINE

                    MOVE SPACES TO WS-OUT-LINE
                    STRING "Graduation Year: " DELIMITED BY SIZE
                        PR-GRAD-YEAR DELIMITED BY SIZE
                        INTO WS-OUT-LINE
                    END-STRING
                    PERFORM DISPLAY-LINE

                     IF FUNCTION TRIM(PR-ABOUT) NOT = SPACES
                        MOVE "About Me:" TO WS-OUT-LINE
                        PERFORM DISPLAY-LINE
                        MOVE FUNCTION TRIM(PR-ABOUT) TO WS-OUT-LINE
                        PERFORM DISPLAY-LINE
                    END-IF

                    IF PR-EXP-COUNT > 0
                        MOVE "Experience:" TO WS-OUT-LINE
                        PERFORM DISPLAY-LINE
                        PERFORM VARYING I FROM 1 BY 1 UNTIL I > PR-EXP-COUNT
                            MOVE SPACES TO WS-OUT-LINE
                            STRING "Title: " DELIMITED BY SIZE
                                   FUNCTION TRIM(PR-EXP-TITLE(I)) DELIMITED BY SIZE
                                   " | Company: " DELIMITED BY SIZE
                                   FUNCTION TRIM(PR-EXP-COMPANY(I)) DELIMITED BY SIZE
                                   " | Dates: " DELIMITED BY SIZE
                                   FUNCTION TRIM(PR-EXP-DATES(I)) DELIMITED BY SIZE
                                   INTO WS-OUT-LINE
                            END-STRING
                            PERFORM DISPLAY-LINE
                            IF FUNCTION TRIM(PR-EXP-DESC(I)) NOT = SPACES
                                MOVE SPACES TO WS-OUT-LINE
                                STRING "Description: " DELIMITED BY SIZE
                                       FUNCTION TRIM(PR-EXP-DESC(I)) DELIMITED BY SIZE
                                       INTO WS-OUT-LINE
                                END-STRING
                                PERFORM DISPLAY-LINE
                            END-IF
                        END-PERFORM
                    END-IF

                    IF PR-EDU-COUNT > 0
                        MOVE "Education:" TO WS-OUT-LINE
                        PERFORM DISPLAY-LINE
                        PERFORM VARYING I FROM 1 BY 1 UNTIL I > PR-EDU-COUNT
                            MOVE SPACES TO WS-OUT-LINE
                            STRING "Degree: " DELIMITED BY SIZE
                                   FUNCTION TRIM(PR-EDU-DEGREE(I)) DELIMITED BY SIZE
                                   " | School: " DELIMITED BY SIZE
                                   FUNCTION TRIM(PR-EDU-SCHOOL(I)) DELIMITED BY SIZE
                                   " | Years: " DELIMITED BY SIZE
                                   FUNCTION TRIM(PR-EDU-YEARS(I)) DELIMITED BY SIZE
                                   INTO WS-OUT-LINE
                            END-STRING
                            PERFORM DISPLAY-LINE
                        END-PERFORM
                    END-IF

                MOVE "1. Send Connection Request" TO WS-OUT-LINE
                PERFORM DISPLAY-LINE
                MOVE "2. Back to Main Menu" TO WS-OUT-LINE
                PERFORM DISPLAY-LINE
                MOVE "Enter your choice:" TO WS-OUT-LINE
                PERFORM DISPLAY-LINE


                MOVE SPACES TO MENU-CHOICE
                PERFORM UNTIL MENU-CHOICE = "1" OR MENU-CHOICE = "2" OR EOF-FLAG = "Y"
                   PERFORM READ-INPUT

                   IF EOF-FLAG = "Y"
                       EXIT PARAGRAPH
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
                END-IF

           EXIT PARAGRAPH.

       PARSE-ENTERED-SEARCH.

              MOVE SPACES TO WS-SEARCH-FIRST-NAME
              MOVE SPACES TO WS-SEARCH-LAST-NAME
              MOVE 0 TO WS-SEARCH-SPACE-LOC

              PERFORM VARYING I FROM 1 BY 1 UNTIL I > FUNCTION LENGTH(WS-SEARCH-FULLNAME) OR WS-SEARCH-SPACE-LOC > 0
                  IF WS-SEARCH-FULLNAME(I:1) = " "
                      MOVE I TO WS-SEARCH-SPACE-LOC
                  END-IF
              END-PERFORM

              IF WS-SEARCH-SPACE-LOC > 0
                  MOVE WS-SEARCH-FULLNAME(1:WS-SEARCH-SPACE-LOC - 1) TO WS-SEARCH-FIRST-NAME
                  MOVE WS-SEARCH-FULLNAME(WS-SEARCH-SPACE-LOC + 1:) TO WS-SEARCH-LAST-NAME
              ELSE
                  MOVE WS-SEARCH-FULLNAME TO WS-SEARCH-FIRST-NAME
              END-IF.

           EXIT PARAGRAPH.

       SEND-CONNECTION-REQUEST.
           MOVE "N" TO PEND-FOUND
           MOVE "N" TO PEND-EOF

           MOVE FUNCTION TRIM(PR-USERNAME) TO WS-PEND-RECEIVER-USER
           MOVE FUNCTION TRIM(PR-FIRST-NAME) TO WS-PEND-RECEIVER-FIRST
           MOVE FUNCTION TRIM(PR-LAST-NAME) TO WS-PEND-RECEIVER-LAST


           MOVE SPACES TO WS-PEND-SENDER-FIRST WS-PEND-SENDER-LAST
           MOVE "N" TO PROFILE-FOUND PROFILE-EOF
           CLOSE PROFILE-FILE
           OPEN INPUT PROFILE-FILE
           PERFORM UNTIL PROFILE-EOF = "Y" OR PROFILE-FOUND = "Y"
               READ PROFILE-FILE
                   AT END
                       MOVE "Y" TO PROFILE-EOF
                   NOT AT END
                       IF FUNCTION TRIM(PR-USERNAME) = FUNCTION TRIM(WS-USERNAME)
                           MOVE FUNCTION TRIM(PR-FIRST-NAME) TO WS-PEND-SENDER-FIRST
                           MOVE FUNCTION TRIM(PR-LAST-NAME) TO WS-PEND-SENDER-LAST
                           MOVE "Y" TO PROFILE-FOUND
                       END-IF
               END-READ
           END-PERFORM
           CLOSE PROFILE-FILE
           OPEN INPUT PROFILE-FILE

           CLOSE PENDING-FILE


           IF FUNCTION TRIM(WS-PEND-RECEIVER-USER) = FUNCTION TRIM(WS-USERNAME)
               MOVE "You cannot send a connection request to yourself." TO WS-OUT-LINE
               PERFORM DISPLAY-LINE
               EXIT PARAGRAPH
           END-IF


           OPEN INPUT PENDING-FILE
           PERFORM UNTIL PEND-EOF = "Y"
               READ PENDING-FILE
                   AT END
                       MOVE "Y" TO PEND-EOF
                   NOT AT END
                       IF FUNCTION TRIM(PEND-SENDER-USER) = FUNCTION TRIM(WS-USERNAME)
                           AND FUNCTION TRIM(PEND-RECEIVER-USER) = FUNCTION TRIM(WS-PEND-RECEIVER-USER)
                               STRING "You have already sent a connection request to " DELIMITED BY SIZE
                                   FUNCTION TRIM(WS-PEND-RECEIVER-FIRST) DELIMITED BY SIZE
                                  " " DELIMITED BY SIZE
                                  FUNCTION TRIM(WS-PEND-RECEIVER-LAST) DELIMITED BY SIZE
                                  "." DELIMITED BY SIZE
                                  INTO WS-OUT-LINE

                                  END-STRING
                           PERFORM DISPLAY-LINE
                           MOVE "Y" TO PEND-FOUND
                       ELSE
                           IF FUNCTION TRIM(PEND-SENDER-USER) = FUNCTION TRIM(WS-PEND-RECEIVER-USER)
                              AND FUNCTION TRIM(PEND-RECEIVER-USER) = FUNCTION TRIM(WS-USERNAME)
                               MOVE "This user has already sent you a connection request" TO WS-OUT-LINE
                               PERFORM DISPLAY-LINE
                               MOVE "Y" TO PEND-FOUND
                           END-IF
                       END-IF
               END-READ
           END-PERFORM

           IF PEND-FOUND = "N"
               CLOSE PENDING-FILE
               OPEN EXTEND PENDING-FILE
               MOVE FUNCTION TRIM(WS-USERNAME) TO PEND-SENDER-USER
               MOVE FUNCTION TRIM(WS-PEND-SENDER-FIRST) TO PEND-SENDER-FIRST
               MOVE FUNCTION TRIM(WS-PEND-SENDER-LAST) TO PEND-SENDER-LAST
               MOVE FUNCTION TRIM(WS-PEND-RECEIVER-USER) TO PEND-RECEIVER-USER
               MOVE FUNCTION TRIM(WS-PEND-RECEIVER-FIRST) TO PEND-RECEIVER-FIRST
               MOVE FUNCTION TRIM(WS-PEND-RECEIVER-LAST) TO PEND-RECEIVER-LAST
               WRITE PENDING-RECORD
               CLOSE PENDING-FILE
               OPEN INPUT PENDING-FILE

               STRING "Connection request sent to " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-PEND-RECEIVER-FIRST) DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-PEND-RECEIVER-LAST) DELIMITED BY SIZE
                      "." DELIMITED BY SIZE
                      INTO WS-OUT-LINE
               END-STRING
               PERFORM DISPLAY-LINE
           END-IF

           EXIT PARAGRAPH.


           VIEW-PENDING-REQUESTS.
               MOVE "----- Pending Connection Requests -----" TO WS-OUT-LINE
               PERFORM DISPLAY-LINE
               MOVE "N" TO PEND-FOUND
               MOVE "N" TO PEND-EOF

               CLOSE PENDING-FILE
               OPEN INPUT PENDING-FILE

               PERFORM UNTIL PEND-EOF = "Y"
                   READ PENDING-FILE
                       AT END
                           MOVE "Y" TO PEND-EOF
                       NOT AT END
                           IF FUNCTION TRIM(PEND-RECEIVER-USER) = FUNCTION TRIM(WS-USERNAME)
                               MOVE SPACES TO WS-OUT-LINE
                               STRING FUNCTION TRIM(PEND-SENDER-FIRST) DELIMITED BY SIZE
                                      " " DELIMITED BY SIZE
                                      FUNCTION TRIM(PEND-SENDER-LAST) DELIMITED BY SIZE
                                      INTO WS-OUT-LINE
                               END-STRING
                               PERFORM DISPLAY-LINE
                               MOVE "Y" TO PEND-FOUND
                           END-IF
                   END-READ
               END-PERFORM

               IF PEND-FOUND = "N"
                   MOVE "You have no pending connection requests at this time." TO WS-OUT-LINE
                   PERFORM DISPLAY-LINE
               END-IF

               MOVE "-----------------------------------" TO WS-OUT-LINE
               PERFORM DISPLAY-LINE

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
           WRITE OUTPUT-RECORD.

