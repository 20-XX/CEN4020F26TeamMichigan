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

       01 WS-USERNAME             PIC X(20).
       01 WS-PASSWORD             PIC X(50).
       01 WS-OUT-LINE             PIC X(100).

       01 I                       PIC 9(2).

       01 LINK-PARAMETERS.
           05 LNK-OPERATION         PIC X(2).
           05 LNK-USERNAME          PIC X(20).
           05 LNK-PASSWORD          PIC X(50).
           05 LNK-RETURN-CODE       PIC X.

       PROCEDURE DIVISION.
       MAIN.
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
           MOVE "CU" TO LNK-OPERATION
           MOVE WS-USERNAME TO LNK-USERNAME

           CALL 'ACCTMGR' USING LNK-OPERATION, LNK-USERNAME, LNK-PASSWORD, LNK-RETURN-CODE

           IF LNK-RETURN-CODE = "Y"
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
           MOVE "VP" TO LNK-OPERATION
           MOVE WS-PASSWORD TO LNK-PASSWORD

           CALL 'ACCTMGR' USING LNK-OPERATION, LNK-USERNAME, LNK-PASSWORD, LNK-RETURN-CODE

           IF LNK-RETURN-CODE = "Y"
                MOVE "AA" TO LNK-OPERATION
                CALL 'ACCTMGR' USING LNK-OPERATION, LNK-USERNAME, LNK-PASSWORD, LNK-RETURN-CODE
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
           MOVE WS-USERNAME TO LNK-USERNAME

           MOVE "Please enter your password:" TO WS-OUT-LINE
           PERFORM DISPLAY-LINE
           PERFORM READ-INPUT
           IF EOF-FLAG = "Y"
               EXIT PARAGRAPH
           END-IF
           MOVE INPUT-RECORD TO WS-PASSWORD
           MOVE WS-PASSWORD TO LNK-PASSWORD
           MOVE "AL" TO LNK-OPERATION

           CALL 'ACCTMGR' USING LNK-OPERATION, LNK-USERNAME, LNK-PASSWORD, LNK-RETURN-CODE

           IF LNK-RETURN-CODE = "Y"
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
                       MOVE "Profile creation and editing is under construction."
                           TO WS-OUT-LINE
                       PERFORM DISPLAY-LINE
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
