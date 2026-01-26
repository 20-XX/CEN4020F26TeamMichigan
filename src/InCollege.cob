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
       77 PASSWORD-VALID          PIC X VALUE "N".
       77 CNT-UPPER PIC 9(3) VALUE 0.
       77 CNT-DIGIT PIC 9(3) VALUE 0.
       77 CNT-SPECIAL PIC 9(3) VALUE 0.


       01 WS-USERNAME             PIC X(20).
       01 WS-PASSWORD             PIC X(50).
       01 WS-OUT-LINE             PIC X(100).

       01 PASSWORD-FLAGS.
           05 HAS-UPPER           PIC X VALUE "N".
           05 HAS-DIGIT           PIC X VALUE "N".
           05 HAS-SPECIAL         PIC X VALUE "N".

       01 I                       PIC 9(2).


       01 HASH-VALUE              PIC 9(10) VALUE 0.
       01 HASH-CHAR               PIC 9(3).
       01 WS-HASHED-PASSWORD      PIC X(12).
       01 TEMP-HASH               PIC 9(10).


       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE
           OPEN INPUT ACCOUNT-FILE

           PERFORM LOAD-ACCOUNTS

           PERFORM UNTIL EOF-FLAG = "Y"
               PERFORM MAIN-MENU
           END-PERFORM


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
           PERFORM UNTIL MENU-CHOICE = "4" OR EOF-FLAG = "Y"
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
