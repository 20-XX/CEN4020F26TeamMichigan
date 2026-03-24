BROWSE-JOBS.
           MOVE "--- Available Job Listings ---" TO WS-OUT-LINE
           PERFORM DISPLAY-LINE

           OPEN INPUT BROWSE-JOBS-FILE
           MOVE 0 TO WS-BROWSE-JOB-COUNT
           MOVE "N" TO WS-BROWSE-EOF

           PERFORM UNTIL WS-BROWSE-EOF = "Y"
               READ BROWSE-JOBS-FILE
                   AT END
                       MOVE "Y" TO WS-BROWSE-EOF
                   NOT AT END
                       ADD 1 TO WS-BROWSE-JOB-COUNT
                       MOVE WS-BROWSE-JOB-COUNT TO
                           BJ-JOB-IDX(WS-BROWSE-JOB-COUNT)
                       MOVE BJ-FILE-JOB-ID TO
                           BJ-REAL-ID(WS-BROWSE-JOB-COUNT)
                       MOVE BJ-FILE-JOB-TITLE TO
                           BJ-JOB-TITLE(WS-BROWSE-JOB-COUNT)
                       MOVE BJ-FILE-JOB-EMPLOYER TO
                           BJ-JOB-EMPLOYER(WS-BROWSE-JOB-COUNT)
                       MOVE BJ-FILE-JOB-LOCATION TO
                           BJ-JOB-LOCATION(WS-BROWSE-JOB-COUNT)
                       MOVE BJ-FILE-JOB-DESCRIPTION TO
                           BJ-JOB-DESCRIPTION(WS-BROWSE-JOB-COUNT)
                       MOVE BJ-FILE-JOB-SALARY TO
                           BJ-JOB-SALARY(WS-BROWSE-JOB-COUNT)
               END-READ
           END-PERFORM
           CLOSE BROWSE-JOBS-FILE

           IF WS-BROWSE-JOB-COUNT = 0
               MOVE "No job listings are currently available."
                   TO WS-OUT-LINE
               PERFORM DISPLAY-LINE
               MOVE "-----------------------------" TO WS-OUT-LINE
               PERFORM DISPLAY-LINE
               MOVE "Enter job number to view details, or 0 to go back:"
                   TO WS-OUT-LINE
               PERFORM DISPLAY-LINE
               PERFORM READ-INPUT
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING WS-BROWSE-IDX FROM 1 BY 1
               UNTIL WS-BROWSE-IDX > WS-BROWSE-JOB-COUNT
               STRING FUNCTION TRIM(BJ-JOB-TITLE(WS-BROWSE-IDX))
                       DELIMITED BY SIZE
                   " at " DELIMITED BY SIZE
                   FUNCTION TRIM(BJ-JOB-EMPLOYER(WS-BROWSE-IDX))
                       DELIMITED BY SIZE
                   " (" DELIMITED BY SIZE
                   FUNCTION TRIM(BJ-JOB-LOCATION(WS-BROWSE-IDX))
                       DELIMITED BY SIZE
                   ")" DELIMITED BY SIZE
                   INTO WS-OUT-LINE
               END-STRING
               PERFORM DISPLAY-LINE
           END-PERFORM

           MOVE "-----------------------------" TO WS-OUT-LINE
           PERFORM DISPLAY-LINE
           MOVE "Enter job number to view details, or 0 to go back:"
               TO WS-OUT-LINE
           PERFORM DISPLAY-LINE
           PERFORM READ-INPUT

           IF EOF-FLAG = "Y"
               EXIT PARAGRAPH
           END-IF

           MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-BROWSE-CHOICE-STR
           MOVE 0 TO WS-BROWSE-CHOICE
           IF WS-BROWSE-CHOICE-STR(1:1) >= "0" AND
              WS-BROWSE-CHOICE-STR(1:1) <= "9"
               MOVE WS-BROWSE-CHOICE-STR(1:1) TO WS-BROWSE-CHOICE
           END-IF
           
           IF WS-BROWSE-CHOICE = 0
               EXIT PARAGRAPH
           END-IF

           IF WS-BROWSE-CHOICE < 1 OR
              WS-BROWSE-CHOICE > WS-BROWSE-JOB-COUNT
               MOVE "Invalid job number." TO WS-OUT-LINE
               PERFORM DISPLAY-LINE
               PERFORM BROWSE-JOBS
               EXIT PARAGRAPH
           END-IF

           PERFORM VIEW-JOB-DETAILS

           EXIT PARAGRAPH.

       VIEW-JOB-DETAILS.
           MOVE "--- Job Details ---" TO WS-OUT-LINE
           PERFORM DISPLAY-LINE

           STRING "Title: " DELIMITED BY SIZE
               FUNCTION TRIM(BJ-JOB-TITLE(WS-BROWSE-CHOICE))
                   DELIMITED BY SIZE
               INTO WS-OUT-LINE
           END-STRING
           PERFORM DISPLAY-LINE

           STRING "Description: " DELIMITED BY SIZE
               FUNCTION TRIM(BJ-JOB-DESCRIPTION(WS-BROWSE-CHOICE))
                   DELIMITED BY SIZE
               INTO WS-OUT-LINE
           END-STRING
           PERFORM DISPLAY-LINE

           STRING "Employer: " DELIMITED BY SIZE
               FUNCTION TRIM(BJ-JOB-EMPLOYER(WS-BROWSE-CHOICE))
                   DELIMITED BY SIZE
               INTO WS-OUT-LINE
           END-STRING
           PERFORM DISPLAY-LINE

           STRING "Location: " DELIMITED BY SIZE
               FUNCTION TRIM(BJ-JOB-LOCATION(WS-BROWSE-CHOICE))
                   DELIMITED BY SIZE
               INTO WS-OUT-LINE
           END-STRING
           PERFORM DISPLAY-LINE

           IF FUNCTION TRIM(BJ-JOB-SALARY(WS-BROWSE-CHOICE))
               NOT = SPACES
               STRING "Salary: " DELIMITED BY SIZE
                   FUNCTION TRIM(BJ-JOB-SALARY(WS-BROWSE-CHOICE))
                       DELIMITED BY SIZE
                   INTO WS-OUT-LINE
               END-STRING
               PERFORM DISPLAY-LINE
           END-IF

           MOVE "-------------------" TO WS-OUT-LINE
           PERFORM DISPLAY-LINE
           MOVE "1. Apply for this Job" TO WS-OUT-LINE
           PERFORM DISPLAY-LINE
           MOVE "2. Back to Job List" TO WS-OUT-LINE
           PERFORM DISPLAY-LINE
           MOVE "Enter your choice:" TO WS-OUT-LINE
           PERFORM DISPLAY-LINE
           PERFORM READ-INPUT

           IF EOF-FLAG = "Y"
               EXIT PARAGRAPH
           END-IF

           MOVE INPUT-RECORD(1:1) TO WS-DETAIL-CHOICE

           IF WS-DETAIL-CHOICE = "1"
               PERFORM APPLY-FOR-JOB
           END-IF

           PERFORM BROWSE-JOBS

           EXIT PARAGRAPH.

       APPLY-FOR-JOB.
           MOVE "N" TO WS-ALREADY-APPLIED

           OPEN INPUT APPLY-FILE
           MOVE "N" TO WS-APPLY-EOF

           PERFORM UNTIL WS-APPLY-EOF = "Y"
               READ APPLY-FILE
                   AT END
                       MOVE "Y" TO WS-APPLY-EOF
                   NOT AT END
                       IF FUNCTION TRIM(APP-USERNAME) =
                          FUNCTION TRIM(WS-USERNAME)
                         AND APP-JOB-ID =
                          BJ-REAL-ID(WS-BROWSE-CHOICE)
                           MOVE "Y" TO WS-ALREADY-APPLIED
                       END-IF
               END-READ
           END-PERFORM
           CLOSE APPLY-FILE

           IF WS-ALREADY-APPLIED = "Y"
               STRING "You have already applied for "
                       DELIMITED BY SIZE
                   FUNCTION TRIM(BJ-JOB-TITLE(WS-BROWSE-CHOICE))
                       DELIMITED BY SIZE
                   " at " DELIMITED BY SIZE
                   FUNCTION TRIM(BJ-JOB-EMPLOYER(WS-BROWSE-CHOICE))
                       DELIMITED BY SIZE
                   "." DELIMITED BY SIZE
                   INTO WS-OUT-LINE
               END-STRING
               PERFORM DISPLAY-LINE
               EXIT PARAGRAPH
           END-IF

           MOVE FUNCTION TRIM(WS-USERNAME) TO APP-USERNAME
           MOVE BJ-REAL-ID(WS-BROWSE-CHOICE) TO APP-JOB-ID
           MOVE BJ-JOB-TITLE(WS-BROWSE-CHOICE) TO APP-JOB-TITLE
           MOVE BJ-JOB-EMPLOYER(WS-BROWSE-CHOICE) TO APP-JOB-EMPLOYER
           MOVE BJ-JOB-LOCATION(WS-BROWSE-CHOICE) TO APP-JOB-LOCATION

           OPEN EXTEND APPLY-FILE
           WRITE APP-RECORD
           CLOSE APPLY-FILE

           STRING "Your application for " DELIMITED BY SIZE
               FUNCTION TRIM(BJ-JOB-TITLE(WS-BROWSE-CHOICE))
                   DELIMITED BY SIZE
               " at " DELIMITED BY SIZE
               FUNCTION TRIM(BJ-JOB-EMPLOYER(WS-BROWSE-CHOICE))
                   DELIMITED BY SIZE
               " has been submitted." DELIMITED BY SIZE
               INTO WS-OUT-LINE
           END-STRING
           PERFORM DISPLAY-LINE

           EXIT PARAGRAPH.
