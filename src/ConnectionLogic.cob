IDENTIFICATION DIVISION.
    PROGRAM-ID. CONNECTIONLOGIC.

ENVIRONMENT DIVISION.
    INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT PENDING-FILE ASSIGN TO "PendingRequests.dat"
                ORGANIZATION IS LINE SEQUENTIAL.
            SELECT PEND-TEMP ASSIGN TO "PendingRequests.tmp"
                ORGANIZATION IS LINE SEQUENTIAL.
            SELECT CONNECTIONS-FILE ASSIGN TO "Connections.dat"
                ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
    FILE SECTION.
        FD PENDING-FILE.
        01 PENDING-RECORD.
            05 PEND-SENDER-USER    PIC X(20).
            05 PEND-SENDER-FIRST    PIC X(20).
            05 PEND-SENDER-LAST    PIC X(20).
            05 PEND-RECEIVER-USER    PIC X(20).
            05 PEND-RECEIVER-FIRST    PIC X(20).
            05 PEND-RECEIVER-LAST    PIC X(20).
        FD PEND-TEMP.
        01 PEND-TEMP-RECORD.
            05 PT-SENDER-USER    PIC X(20).
            05 PT-SENDER-FIRST    PIC X(20).
            05 PT-SENDER-LAST    PIC X(20).
            05 PT-RECEIVER-USER    PIC X(20).
            05 PT-RECEIVER-FIRST    PIC X(20).
            05 PT-RECEIVER-LAST    PIC X(20).
        FD CONNECTIONS-FILE.
        01 CONNECTION-RECORD.
            05 CONN-USER1    PIC X(20).
            05 CONN-USER1-FIRST    PIC X(20).
            05 CONN-USER1-LAST    PIC X(20).
            05 CONN-USER2    PIC X(20).
            05 CONN-USER2-FIRST    PIC X(20).
            05 CONN-USER2-LAST    PIC X(20).
    LOCAL-STORAGE SECTION.
        77 PEND-EOF    PIC X VALUE "N".
        77 PEND-FOUND    PIC X VALUE "N".
        77 CONN-EOF    PIC X VALUE "N".
        77 CONN-FOUND    PIC X VALUE "N".
        01 LS-PEND-RECORD.
            05 LS-PEND-SENDER-USER    PIC X(20).
            05 LS-PEND-SENDER-FIRST    PIC X(20).
            05 LS-PEND-SENDER-LAST    PIC X(20).
            05 LS-PEND-RECEIVER-USER    PIC X(20).
            05 LS-PEND-RECEIVER-FIRST    PIC X(20).
            05 LS-PEND-RECEIVER-LAST    PIC X(20).
     LINKAGE SECTION.
        01 LNK-OPERATION    PIC X(3).
        01 LNK-RETURN-CODE    PIC X.
        01 LNK-RETURN-MSG    PIC X(100).
        01 LNK-PEND-RECORD.
             05 LNK-SENDER-USER    PIC X(20).
             05 LNK-SENDER-FIRST    PIC X(20).
             05 LNK-SENDER-LAST    PIC X(20).
             05 LNK-RECEIVER-USER    PIC X(20).
             05 LNK-RECEIVER-FIRST    PIC X(20).
             05 LNK-RECEIVER-LAST    PIC X(20).
PROCEDURE DIVISION USING LNK-OPERATION, LNK-RETURN-CODE, LNK-RETURN-MSG, LNK-PEND-RECORD.
    MOVE "N" TO LNK-RETURN-CODE
    MOVE SPACES TO LNK-RETURN-MSG
    MOVE LNK-PEND-RECORD TO LS-PEND-RECORD
    EVALUATE LNK-OPERATION
        WHEN "APR"
            PERFORM ADD-PENDING-REQUEST
        WHEN OTHER
            MOVE "N" TO LNK-RETURN-CODE
    END-EVALUATE
    GOBACK.

    ADD-PENDING-REQUEST.
        OPEN INPUT PENDING-FILE
        PERFORM UNTIL PEND-FOUND = "Y" OR PEND-EOF = "Y"
            READ PENDING-FILE INTO PENDING-RECORD
                AT END
                    MOVE "Y" TO PEND-EOF
                NOT AT END
                    IF FUNCTION TRIM(PEND-SENDER-USER) = FUNCTION TRIM(LS-PEND-SENDER-USER) AND
                       FUNCTION TRIM(PEND-RECEIVER-USER) = FUNCTION TRIM(LS-PEND-RECEIVER-USER)
                           MOVE "Y" TO PEND-FOUND
                           STRING "You have already sent a connection request to " DELIMITED BY SIZE
                           FUNCTION TRIM(LS-PEND-RECEIVER-FIRST) DELIMITED BY SIZE
                           " " DELIMITED BY SIZE
                           FUNCTION TRIM(LS-PEND-RECEIVER-LAST) DELIMITED BY SIZE
                           "." DELIMITED BY SIZE
                           INTO LNK-RETURN-MSG
                    ELSE
                        IF FUNCTION TRIM(PEND-SENDER-USER) = FUNCTION TRIM(LS-PEND-RECEIVER-USER) AND
                           FUNCTION TRIM(PEND-RECEIVER-USER) = FUNCTION TRIM(LS-PEND-SENDER-USER)
                               MOVE "Y" TO PEND-FOUND
                               MOVE "This user has already sent you a connection request." TO LNK-RETURN-MSG
                        END-IF
                    END-IF
            END-READ
        END-PERFORM

        CLOSE PENDING-FILE

        IF PEND-FOUND = "Y"
            MOVE "N" TO LNK-RETURN-CODE
        ELSE
            OPEN EXTEND PENDING-FILE
            MOVE LNK-PEND-RECORD TO PENDING-RECORD
            WRITE PENDING-RECORD
            CLOSE PENDING-FILE
            MOVE "Y" TO LNK-RETURN-CODE
            MOVE "Connection request sent successfully." TO LNK-RETURN-MSG
        END-IF

        EXIT PARAGRAPH.
