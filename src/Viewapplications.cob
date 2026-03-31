VIEW-MY-APPLICATIONS.
           MOVE "--- Your Job Applications ---" TO WS-OUT-LINE
           PERFORM DISPLAY-LINE

           STRING "Application Summary for " DELIMITED BY SIZE
               FUNCTION TRIM(WS-USERNAME) DELIMITED BY SIZE
               INTO WS-OUT-LINE
           END-STRING
           PERFORM DISPLAY-LINE

           MOVE "------------------------------" TO WS-OUT-LINE
           PERFORM DISPLAY-LINE

           OPEN INPUT APPLY-FILE
           MOVE "N" TO WS-APPLY-EOF
           MOVE 0 TO WS-APP-TOTAL

           PERFORM UNTIL WS-APPLY-EOF = "Y"
               READ APPLY-FILE
                   AT END
                       MOVE "Y" TO WS-APPLY-EOF
                   NOT AT END
                       IF FUNCTION TRIM(APP-USERNAME) =
                          FUNCTION TRIM(WS-USERNAME)
                           ADD 1 TO WS-APP-TOTAL

                           STRING "Job Title: " DELIMITED BY SIZE
                               FUNCTION TRIM(APP-JOB-TITLE)
                                   DELIMITED BY SIZE
                               INTO WS-OUT-LINE
                           END-STRING
                           PERFORM DISPLAY-LINE

                           STRING "Employer: " DELIMITED BY SIZE
                               FUNCTION TRIM(APP-JOB-EMPLOYER)
                                   DELIMITED BY SIZE
                               INTO WS-OUT-LINE
                           END-STRING
                           PERFORM DISPLAY-LINE

                           STRING "Location: " DELIMITED BY SIZE
                               FUNCTION TRIM(APP-JOB-LOCATION)
                                   DELIMITED BY SIZE
                               INTO WS-OUT-LINE
                           END-STRING
                           PERFORM DISPLAY-LINE

                           MOVE "---" TO WS-OUT-LINE
                           PERFORM DISPLAY-LINE
                       END-IF
               END-READ
           END-PERFORM
           CLOSE APPLY-FILE

           IF WS-APP-TOTAL = 0
               MOVE "You have not applied to any jobs yet."
                   TO WS-OUT-LINE
               PERFORM DISPLAY-LINE
           END-IF

           MOVE "------------------------------" TO WS-OUT-LINE
           PERFORM DISPLAY-LINE

           STRING "Total Applications: " DELIMITED BY SIZE
               WS-APP-TOTAL DELIMITED BY SIZE
               INTO WS-OUT-LINE
           END-STRING
           PERFORM DISPLAY-LINE

           MOVE "------------------------------" TO WS-OUT-LINE
           PERFORM DISPLAY-LINE

           EXIT PARAGRAPH.
           