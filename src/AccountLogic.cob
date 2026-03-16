IDENTIFICATION DIVISION.
    PROGRAM-ID. ACCOUNTLOGIC.

ENVIRONMENT DIVISION.
    INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT ACCOUNT-FILE ASSIGN TO 'data/Accounts.dat'
                ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
    FILE SECTION.
        FD  ACCOUNT-FILE.
            01  ACCOUNT-RECORD.
                05 ACC-USERNAME    PIC X(20).
                05 ACC-PASSWORD    PIC X(12).
    LOCAL-STORAGE SECTION.
        77 ACC-EOF    PIC X VALUE "N".
        77 ACC-COUNT    PIC 9 VALUE 0.
        77 USER-FOUND    PIC X VALUE "N".
        77 PASSWORD-VALID    PIC X VALUE "N".
        77 CNT-UPPER    PIC 9(3) VALUE 0.
        77 CNT-DIGIT    PIC 9(3) VALUE 0.
        77 CNT-SPECIAL    PIC 9(3) VALUE 0.
        01 HASH-VALUE    PIC 9(10) VALUE 0.
        01 HASH-CHAR    PIC 9(3).
        01 LS-HASHED-PASSWORD    PIC X(12).
        01 TEMP-HASH    PIC 9(10).
        01 I    PIC 9(2).
        01 LOGIN-SUCCESS    PIC X VALUE "N".
        01 LS-USERNAME    PIC X(20).
        01 LS-PASSWORD    PIC X(50).
    LINKAGE SECTION.
        01 LNK-PARAMETERS.
            05 LNK-OPERATION    PIC X(2).
            05 LNK-USERNAME    PIC X(20).
            05 LNK-PASSWORD    PIC X(50).
            05 LNK-RETURN-CODE    PIC X.

PROCEDURE DIVISION USING LNK-PARAMETERS.
    MOVE "N" TO LNK-RETURN-CODE
    MOVE LNK-USERNAME TO LS-USERNAME
    MOVE LNK-PASSWORD TO LS-PASSWORD

    EVALUATE LNK-OPERATION
        WHEN "LA"
            PERFORM LOAD-ACCOUNTS
            IF ACC-COUNT < 5
                MOVE "Y" TO LNK-RETURN-CODE
            END-IF
        WHEN "CU"
            PERFORM CHECK-USERNAME
            IF USER-FOUND = "Y"
                MOVE "Y" TO LNK-RETURN-CODE
            END-IF
        WHEN "VP"
            PERFORM VALIDATE-PASSWORD
            IF PASSWORD-VALID = "Y"
                MOVE "Y" TO LNK-RETURN-CODE
            END-IF
         WHEN "AA"
              PERFORM ADD-ACCOUNT
         WHEN "AL"
              PERFORM ATTEMPT-LOGIN
              IF LOGIN-SUCCESS = "Y"
                  MOVE "Y" TO LNK-RETURN-CODE
              END-IF
        WHEN OTHER
            MOVE "E" TO LNK-RETURN-CODE
    END-EVALUATE
    GOBACK.

    LOAD-ACCOUNTS.
        OPEN INPUT ACCOUNT-FILE
            PERFORM UNTIL ACC-EOF = "Y"
                READ ACCOUNT-FILE INTO ACCOUNT-RECORD
                    AT END
                        MOVE "Y" TO ACC-EOF
                    NOT AT END
                        ADD 1 TO ACC-COUNT
                END-READ
            END-PERFORM
        CLOSE ACCOUNT-FILE.

    CHECK-USERNAME.
        OPEN INPUT ACCOUNT-FILE
            PERFORM UNTIL USER-FOUND = "Y" OR ACC-EOF = "Y"
                READ ACCOUNT-FILE INTO ACCOUNT-RECORD
                    AT END
                        MOVE "Y" TO ACC-EOF
                    NOT AT END
                        IF ACC-USERNAME = LS-USERNAME
                            MOVE "Y" TO USER-FOUND
                        END-IF
                END-READ
            END-PERFORM
        CLOSE ACCOUNT-FILE.

    VALIDATE-PASSWORD.
        IF FUNCTION LENGTH(FUNCTION TRIM(LS-PASSWORD)) < 8 OR FUNCTION LENGTH(FUNCTION TRIM(LS-PASSWORD)) > 12
            EXIT PARAGRAPH
        END-IF

        INSPECT LS-PASSWORD TALLYING CNT-UPPER FOR ALL "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
        INSPECT LS-PASSWORD TALLYING CNT-DIGIT FOR ALL "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
        INSPECT LS-PASSWORD TALLYING CNT-SPECIAL FOR ALL "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "-" "_" "+" "=" "~" "`" "[" "]" "{" "}" "|" "\" ":" ";" "'" '"' "<" ">" "," "." "?" "/"

        IF CNT-UPPER > 0 AND CNT-DIGIT > 0 AND CNT-SPECIAL > 0
            MOVE "Y" TO PASSWORD-VALID
        END-IF.

    HASH-PASSWORD.
        PERFORM VARYING I FROM 1 BY 1
            UNTIL I > FUNCTION LENGTH(FUNCTION TRIM(LS-PASSWORD))
                COMPUTE HASH-CHAR = FUNCTION ORD(LS-PASSWORD(I:1))
                COMPUTE TEMP-HASH = HASH-VALUE * 31
                COMPUTE HASH-VALUE = FUNCTION MOD(TEMP-HASH + HASH-CHAR, 999999999)
        END-PERFORM

        STRING HASH-VALUE DELIMITED BY SIZE
            INTO LS-HASHED-PASSWORD
        END-STRING.

    ADD-ACCOUNT.
        PERFORM HASH-PASSWORD
        OPEN EXTEND ACCOUNT-FILE
            MOVE LNK-USERNAME TO ACC-USERNAME
            MOVE LS-HASHED-PASSWORD TO ACC-PASSWORD
            WRITE ACCOUNT-RECORD
        CLOSE ACCOUNT-FILE.

    ATTEMPT-LOGIN.
        PERFORM HASH-PASSWORD
        OPEN INPUT ACCOUNT-FILE
            PERFORM UNTIL LOGIN-SUCCESS = "Y" OR ACC-EOF = "Y"
                READ ACCOUNT-FILE INTO ACCOUNT-RECORD
                    AT END
                        MOVE "Y" TO ACC-EOF
                    NOT AT END
                        IF FUNCTION TRIM(ACC-USERNAME) = FUNCTION TRIM(LS-USERNAME)
                            IF FUNCTION TRIM(ACC-PASSWORD) = FUNCTION TRIM(LS-HASHED-PASSWORD)
                                MOVE "Y" TO LOGIN-SUCCESS
                            END-IF
                        END-IF
                END-READ
            END-PERFORM
        CLOSE ACCOUNT-FILE.
