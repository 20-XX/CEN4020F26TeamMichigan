IDENTIFICATION DIVISION.
    PROGRAM-ID. CONNECTIONLOGIC.

ENVIRONMENT DIVISION.
    INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT PENDING-FILE ASSIGN TO "data/PendingRequests.dat"
                ORGANIZATION IS LINE SEQUENTIAL.
            SELECT PEND-TEMP ASSIGN TO "data/PendingRequests.tmp"
                ORGANIZATION IS LINE SEQUENTIAL.
            SELECT CONNECTIONS-FILE ASSIGN TO "data/Connections.dat"
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
        01 LS-PEND-REQUEST.
            05 LS-PEND-SENDER-USER    PIC X(20).
            05 LS-PEND-SENDER-FIRST    PIC X(20).
            05 LS-PEND-SENDER-LAST    PIC X(20).
            05 LS-PEND-RECEIVER-USER    PIC X(20).
            05 LS-PEND-RECEIVER-FIRST    PIC X(20).
            05 LS-PEND-RECEIVER-LAST    PIC X(20).
        01 LS-NEW-CONN.
            05 LS-NEW-CONN-USER1    PIC X(20).
            05 LS-NEW-CONN-USER1-FIRST    PIC X(20).
            05 LS-NEW-CONN-USER1-LAST    PIC X(20).
            05 LS-NEW-CONN-USER2    PIC X(20).
            05 LS-NEW-CONN-USER2-FIRST    PIC X(20).
            05 LS-NEW-CONN-USER2-LAST    PIC X(20).
        01 LS-ALL-PENDING-REQUESTS.
            05 LS-NUM-PEND-REQUESTS    PIC 9 VALUE 0.
            05 LS-PEND-REQUESTS OCCURS 4 TIMES.
                10 LS-REQUEST-SENDER-USER    PIC X(20).
                10 LS-REQUEST-SENDER-FIRST    PIC X(20).
                10 LS-REQUEST-SENDER-LAST    PIC X(20).
                10 LS-REQUEST-RECEIVER-USER    PIC X(20).
                10 LS-REQUEST-RECEIVER-FIRST    PIC X(20).
                10 LS-REQUEST-RECEIVER-LAST    PIC X(20).
        01 LS-ALL-CONNECTIONS.
            05 LS-NUM-CONNECTIONS    PIC 9 VALUE 0.
            05 LS-CONNECTIONS OCCURS 4 TIMES.
                10 LS-CONN-USER1   PIC X(20).
                10 LS-CONN-USER1-FIRST   PIC X(20).
                10 LS-CONN-USER1-LAST   PIC X(20).
                10 LS-CONN-USER2   PIC X(20).
                10 LS-CONN-USER2-FIRST   PIC X(20).
                10 LS-CONN-USER2-LAST   PIC X(20).
     LINKAGE SECTION.
        01 LNK-PARAMETERS.
            05 LNK-OPERATION    PIC X(3).
            05 LNK-RETURN-CODE    PIC X.
            05 LNK-SEARCH-USERNAME    PIC X(20).
            05 LNK-PEND-REQUEST.
                 10 LNK-PEND-SENDER-USER    PIC X(20).
                 10 LNK-PEND-SENDER-FIRST    PIC X(20).
                 10 LNK-PEND-SENDER-LAST    PIC X(20).
                 10 LNK-PEND-RECEIVER-USER    PIC X(20).
                 10 LNK-PEND-RECEIVER-FIRST    PIC X(20).
                 10 LNK-PEND-RECEIVER-LAST    PIC X(20).
            05 LNK-NEW-CONN.
                 10 LNK-NEW-CONN-USER1    PIC X(20).
                 10 LNK-NEW-CONN-USER1-FIRST    PIC X(20).
                 10 LNK-NEW-CONN-USER1-LAST    PIC X(20).
                 10 LNK-NEW-CONN-USER2    PIC X(20).
                 10 LNK-NEW-CONN-USER2-FIRST    PIC X(20).
                 10 LNK-NEW-CONN-USER2-LAST    PIC X(20).
            05 LNK-ALL-PENDING-REQUESTS.
                10 LNK-NUM-PEND-REQUESTS    PIC 9 VALUE 0.
                10 LNK-PEND-REQUESTS OCCURS 4 TIMES.
                   15 LNK-REQUEST-SENDER-USER    PIC X(20).
                   15 LNK-REQUEST-SENDER-FIRST    PIC X(20).
                   15 LNK-REQUEST-SENDER-LAST    PIC X(20).
                   15 LNK-REQUEST-RECEIVER-USER    PIC X(20).
                   15 LNK-REQUEST-RECEIVER-FIRST    PIC X(20).
                   15 LNK-REQUEST-RECEIVER-LAST    PIC X(20).
            05 LNK-ALL-CONNECTIONS.
                10 LNK-NUM-CONNECTIONS    PIC 9 VALUE 0.
                10 LNK-CONNECTIONS OCCURS 4 TIMES.
                    15 LNK-CONN-USER1   PIC X(20).
                    15 LNK-CONN-USER1-FIRST   PIC X(20).
                    15 LNK-CONN-USER1-LAST   PIC X(20).
                    15 LNK-CONN-USER2   PIC X(20).
                    15 LNK-CONN-USER2-FIRST   PIC X(20).
                    15 LNK-CONN-USER2-LAST   PIC X(20).
PROCEDURE DIVISION USING LNK-PARAMETERS.
    MOVE "N" TO LNK-RETURN-CODE
    MOVE LNK-PEND-REQUEST TO LS-PEND-REQUEST
    MOVE LNK-NEW-CONN TO LS-NEW-CONN
    EVALUATE FUNCTION TRIM(LNK-OPERATION)
        WHEN "CIC"
            PERFORM CHECK-IF-CONNECTED
            IF CONN-FOUND = "Y"
                MOVE "Y" TO LNK-RETURN-CODE
            END-IF
        WHEN "CIS"
            PERFORM CHECK-IF-SENT
            IF PEND-FOUND = "Y"
                MOVE "Y" TO LNK-RETURN-CODE
            END-IF
        WHEN "CIR"
            PERFORM CHECK-IF-RECEIVED
            IF PEND-FOUND = "Y"
                MOVE "Y" TO LNK-RETURN-CODE
            END-IF
        WHEN "APR"
            PERFORM ADD-PENDING-REQUEST
        WHEN "GAP"
            PERFORM GET-ALL-PENDING
            IF LS-NUM-PEND-REQUESTS > 0
                MOVE "Y" TO LNK-RETURN-CODE
                MOVE LS-ALL-PENDING-REQUESTS TO LNK-ALL-PENDING-REQUESTS
            END-IF
        WHEN "ANC"
            PERFORM ACCEPT-NEW-CONNECTION
        WHEN "GAC"
            PERFORM GET-ALL-CONNECTIONS
            IF LS-NUM-CONNECTIONS > 0
                MOVE "Y" TO LNK-RETURN-CODE
                MOVE LS-ALL-CONNECTIONS TO LNK-ALL-CONNECTIONS
            END-IF
        WHEN OTHER
            MOVE "N" TO LNK-RETURN-CODE
    END-EVALUATE
    GOBACK.

    CHECK-IF-CONNECTED.
        OPEN INPUT CONNECTIONS-FILE
        PERFORM UNTIL CONN-FOUND = "Y" OR CONN-EOF = "Y"
            READ CONNECTIONS-FILE INTO CONNECTION-RECORD
                AT END
                    MOVE "Y" TO CONN-EOF
                NOT AT END
                    IF (FUNCTION TRIM(CONN-USER1) = FUNCTION TRIM(LS-PEND-SENDER-USER)
                        AND FUNCTION TRIM(CONN-USER2) = FUNCTION TRIM(LS-PEND-RECEIVER-USER))
                        OR (FUNCTION TRIM(CONN-USER1) = FUNCTION TRIM(LS-PEND-RECEIVER-USER)
                        AND FUNCTION TRIM(CONN-USER2) = FUNCTION TRIM(LS-PEND-SENDER-USER))
                            MOVE "Y" TO CONN-FOUND
                    END-IF
            END-READ
        END-PERFORM
        CLOSE CONNECTIONS-FILE
        EXIT PARAGRAPH.

    CHECK-IF-SENT.
        OPEN INPUT PENDING-FILE
        PERFORM UNTIL PEND-FOUND = "Y" OR PEND-EOF = "Y"
            READ PENDING-FILE INTO PENDING-RECORD
                AT END
                    MOVE "Y" TO PEND-EOF
                NOT AT END
                    IF FUNCTION TRIM(PEND-SENDER-USER) = FUNCTION TRIM(LS-PEND-SENDER-USER) AND
                       FUNCTION TRIM(PEND-RECEIVER-USER) = FUNCTION TRIM(LS-PEND-RECEIVER-USER)
                           MOVE "Y" TO PEND-FOUND
                    END-IF
            END-READ
        END-PERFORM
        CLOSE PENDING-FILE
        EXIT PARAGRAPH.

    CHECK-IF-RECEIVED.
        OPEN INPUT PENDING-FILE
        PERFORM UNTIL PEND-FOUND = "Y" OR PEND-EOF = "Y"
            READ PENDING-FILE INTO PENDING-RECORD
                AT END
                    MOVE "Y" TO PEND-EOF
                NOT AT END
                    IF FUNCTION TRIM(PEND-SENDER-USER) = FUNCTION TRIM(LS-PEND-RECEIVER-USER) AND
                       FUNCTION TRIM(PEND-RECEIVER-USER) = FUNCTION TRIM(LS-PEND-SENDER-USER)
                           MOVE "Y" TO PEND-FOUND
                    END-IF
            END-READ
        END-PERFORM
        CLOSE PENDING-FILE
        EXIT PARAGRAPH.

    ADD-PENDING-REQUEST.
        OPEN EXTEND PENDING-FILE
        MOVE LS-PEND-REQUEST TO PENDING-RECORD
        WRITE PENDING-RECORD
        CLOSE PENDING-FILE
        MOVE "Y" TO LNK-RETURN-CODE
        EXIT PARAGRAPH.

    GET-ALL-PENDING.
        MOVE "N" TO PEND-EOF
        MOVE 0 TO LS-NUM-PEND-REQUESTS
        OPEN INPUT PENDING-FILE
        OPEN OUTPUT PEND-TEMP
        PERFORM UNTIL PEND-EOF = "Y"
            READ PENDING-FILE INTO PENDING-RECORD
                AT END
                    MOVE "Y" TO PEND-EOF
                NOT AT END
                    IF FUNCTION TRIM(PEND-RECEIVER-USER) = FUNCTION TRIM(LNK-SEARCH-USERNAME)
                        ADD 1 TO LS-NUM-PEND-REQUESTS
                        MOVE PENDING-RECORD TO LS-PEND-REQUESTS(LS-NUM-PEND-REQUESTS)
                    ELSE
                        MOVE PENDING-RECORD TO PEND-TEMP-RECORD
                        WRITE PEND-TEMP-RECORD
                    END-IF
            END-READ
        END-PERFORM
        CLOSE PENDING-FILE
        CLOSE PEND-TEMP
        CALL 'SYSTEM' USING 'mv data/PendingRequests.tmp data/PendingRequests.dat'
        EXIT PARAGRAPH.

    ACCEPT-NEW-CONNECTION.
        OPEN EXTEND CONNECTIONS-FILE
        MOVE LS-NEW-CONN TO CONNECTION-RECORD
        WRITE CONNECTION-RECORD
        CLOSE CONNECTIONS-FILE
        MOVE "Y" TO LNK-RETURN-CODE
        EXIT PARAGRAPH.

    GET-ALL-CONNECTIONS.
        OPEN INPUT CONNECTIONS-FILE
        PERFORM UNTIL CONN-EOF = "Y"
            READ CONNECTIONS-FILE INTO CONNECTION-RECORD
                AT END
                    MOVE "Y" TO CONN-EOF
                NOT AT END
                    IF FUNCTION TRIM(CONN-USER1) = FUNCTION TRIM(LNK-SEARCH-USERNAME) OR
                       FUNCTION TRIM(CONN-USER2) = FUNCTION TRIM(LNK-SEARCH-USERNAME)
                           ADD 1 TO LS-NUM-CONNECTIONS
                           MOVE CONNECTION-RECORD TO LS-CONNECTIONS(LS-NUM-CONNECTIONS)
                    END-IF
            END-READ
        END-PERFORM
        CLOSE CONNECTIONS-FILE
        EXIT PARAGRAPH.
