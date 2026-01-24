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

       WORKING-STORAGE SECTION.

       77 EOF-FLAG                PIC X VALUE "N".
       77 ACC-EOF                 PIC X VALUE "N".
       77 ACCOUNT-COUNT           PIC 9 VALUE 0.
       77 MENU-CHOICE             PIC X.
       77 LOGIN-SUCCESS           PIC X VALUE "N".
       77 USER-FOUND              PIC X VALUE "N".

       01 WS-USERNAME             PIC X(20).
       01 WS-PASSWORD             PIC X(12).
       01 WS-OUT-LINE             PIC X(200).

       01 PASSWORD-FLAGS.
           05 HAS-UPPER           PIC X VALUE "N".
           05 HAS-DIGIT           PIC X VALUE "N".
           05 HAS-SPECIAL         PIC X VALUE "N".

       01 I                       PIC 9(2).

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE
           OPEN INPUT ACCOUNT-FILE

           PERFORM LOAD-ACCOUNTS
           PERFORM MAIN-MENU

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           CLOSE ACCOUNT-FILE
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
           MOVE "Welcome to InCollege!" TO WS-OUT-LINE
           PERFORM DISPLAY-LINE
           MOVE "Log In" TO WS-OUT-LINE
           PERFORM DISPLAY-LINE
           MOVE "Create New Account" TO WS-OUT-LINE
           PERFORM DISPLAY-LINE
           MOVE "Enter your choice:" TO WS-OUT-LINE
           PERFORM DISPLAY-LINE

           PERFORM READ-INPUT
           MOVE INPUT-RECORD(1:1) TO MENU-CHOICE

           IF MENU-CHOICE = "1"
               PERFORM LOGIN
           ELSE
               IF MENU-CHOICE = "2"
                   PERFORM CREATE-ACCOUNT
               ELSE
                   PERFORM MAIN-MENU
               END-IF
           END-IF.

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
           MOVE INPUT-RECORD TO WS-PASSWORD

           PERFORM VALIDATE-PASSWORD

           IF HAS-UPPER = "Y" AND HAS-DIGIT = "Y" AND HAS-SPECIAL = "Y"
               CLOSE ACCOUNT-FILE
               OPEN EXTEND ACCOUNT-FILE
               MOVE WS-USERNAME TO ACC-USERNAME
               MOVE WS-PASSWORD TO ACC-PASSWORD
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

           PERFORM MAIN-MENU.

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
           MOVE "N" TO HAS-UPPER HAS-DIGIT HAS-SPECIAL

           IF LENGTH OF WS-PASSWORD < 8 OR LENGTH OF WS-PASSWORD > 12
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH OF WS-PASSWORD
               IF WS-PASSWORD(I:1) >= "A"
                  AND WS-PASSWORD(I:1) <= "Z"
                   MOVE "Y" TO HAS-UPPER
               ELSE
                   IF WS-PASSWORD(I:1) >= "0"
                      AND WS-PASSWORD(I:1) <= "9"
                       MOVE "Y" TO HAS-DIGIT
                   ELSE
                       IF WS-PASSWORD(I:1) < "a"
                          OR WS-PASSWORD(I:1) > "z"
                           MOVE "Y" TO HAS-SPECIAL
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

       LOGIN.
           MOVE "N" TO LOGIN-SUCCESS

           PERFORM UNTIL LOGIN-SUCCESS = "Y"
               MOVE "Please enter your username:" TO WS-OUT-LINE
               PERFORM DISPLAY-LINE
               PERFORM READ-INPUT
               MOVE INPUT-RECORD TO WS-USERNAME

               MOVE "Please enter your password:" TO WS-OUT-LINE
               PERFORM DISPLAY-LINE
               PERFORM READ-INPUT
               MOVE INPUT-RECORD TO WS-PASSWORD

               MOVE "N" TO ACC-EOF
               CLOSE ACCOUNT-FILE
               OPEN INPUT ACCOUNT-FILE

               PERFORM UNTIL ACC-EOF = "Y"
                   READ ACCOUNT-FILE
                       AT END
                           MOVE "Y" TO ACC-EOF
                       NOT AT END
                           IF WS-USERNAME = ACC-USERNAME
                              AND WS-PASSWORD = ACC-PASSWORD
                               MOVE "Y" TO LOGIN-SUCCESS
                           END-IF
                   END-READ
               END-PERFORM

               IF LOGIN-SUCCESS = "Y"
                   MOVE "You have successfully logged in" TO WS-OUT-LINE
                   PERFORM DISPLAY-LINE
                   PERFORM POST-LOGIN
               ELSE
                   MOVE "Incorrect username/password, please try again"
                       TO WS-OUT-LINE
                   PERFORM DISPLAY-LINE
               END-IF
           END-PERFORM.

       POST-LOGIN.
           MOVE "1. Search for a job" TO WS-OUT-LINE
           PERFORM DISPLAY-LINE
           MOVE "2. Find someone you know" TO WS-OUT-LINE
           PERFORM DISPLAY-LINE
           MOVE "3. Learn a new skill" TO WS-OUT-LINE
           PERFORM DISPLAY-LINE
           MOVE "4. Logout" TO WS-OUT-LINE
           PERFORM DISPLAY-LINE
           MOVE "Enter your choice:" TO WS-OUT-LINE
           PERFORM DISPLAY-LINE

           PERFORM READ-INPUT
           MOVE INPUT-RECORD(1:1) TO MENU-CHOICE

           EVALUATE MENU-CHOICE
               WHEN "1"
                   MOVE "Job search/internship is under construction" TO WS-OUT-LINE
                   PERFORM DISPLAY-LINE
                   PERFORM POST-LOGIN
               WHEN "2"
                   MOVE "Find someone you know is under construction" TO WS-OUT-LINE
                   PERFORM DISPLAY-LINE
                   PERFORM POST-LOGIN
               WHEN "3"
                   PERFORM SKILL-MENU
               WHEN "4"
                   EXIT PARAGRAPH
               WHEN OTHER
                   PERFORM POST-LOGIN
           END-EVALUATE.

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
