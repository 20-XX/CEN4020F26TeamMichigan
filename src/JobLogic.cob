IDENTIFICATION DIVISION.
    PROGRAM-ID. JOBLOGIC.

ENVIRONMENT DIVISION.
    INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT JOBS-FILE ASSIGN TO 'data/Jobs.dat'
                ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
    FILE SECTION.
        FD  JOBS-FILE.
            01  JOB-RECORD.
                05 JOB-ID          PIC 9(5).
                05 JOB-POSTED-BY    PIC X(20).
                05 JOB-TITLE    PIC X(50).
                05 JOB-DESCRIPTION    PIC X(200).
                05 JOB-EMPLOYER    PIC X(50).
                05 JOB-LOCATION    PIC X(50).
                05 JOB-SALARY    PIC X(30).
    LOCAL-STORAGE SECTION.
        77 JOBS-EOF    PIC X VALUE "N".
        77 JOB-COUNT    PIC 9(5) VALUE 0.
        77 NEXT-JOB-ID    PIC 9(5) VALUE 1.
        01 LS-JOB-RECORD.
            05 LS-JOB-ID          PIC 9(5).
            05 LS-JOB-POSTED-BY    PIC X(20).
            05 LS-JOB-TITLE    PIC X(50).
            05 LS-JOB-DESCRIPTION    PIC X(200).
            05 LS-JOB-EMPLOYER    PIC X(50).
            05 LS-JOB-LOCATION    PIC X(50).
            05 LS-JOB-SALARY    PIC X(30).
    LINKAGE SECTION.
        01 LNK-PARAMETERS.
            05 LNK-OPERATION    PIC X(3).
            05 LNK-RETURN-CODE    PIC X.
            05 LNK-JOB-RETURN-ID    PIC 9(5).
            05 LNK-JOB-RECORD.
                10 LNK-JOB-ID          PIC 9(5).
                10 LNK-JOB-POSTED-BY    PIC X(20).
                10 LNK-JOB-TITLE    PIC X(50).
                10 LNK-JOB-DESCRIPTION    PIC X(200).
                10 LNK-JOB-EMPLOYER    PIC X(50).
                10 LNK-JOB-LOCATION    PIC X(50).
                10 LNK-JOB-SALARY    PIC X(30).

PROCEDURE DIVISION USING LNK-PARAMETERS.
    MOVE "N" TO LNK-RETURN-CODE
    MOVE LNK-JOB-RECORD TO LS-JOB-RECORD
    EVALUATE FUNCTION TRIM(LNK-OPERATION)
        WHEN "ANJ"
            PERFORM ADD-NEW-JOB
            IF LNK-RETURN-CODE = "Y"
                MOVE LS-JOB-ID TO LNK-JOB-RETURN-ID
            END-IF
         WHEN OTHER
            MOVE "N" TO LNK-RETURN-CODE
    END-EVALUATE
    GOBACK.

    ADD-NEW-JOB.
        OPEN INPUT JOBS-FILE
        PERFORM UNTIL JOBS-EOF = "Y"
            READ JOBS-FILE
                AT END
                    MOVE "Y" TO JOBS-EOF
                NOT AT END
                    ADD 1 TO JOB-COUNT
            END-READ
        END-PERFORM
        CLOSE JOBS-FILE

        COMPUTE NEXT-JOB-ID = JOB-COUNT + 1
        MOVE NEXT-JOB-ID TO LS-JOB-ID

        MOVE LS-JOB-RECORD TO JOB-RECORD
        OPEN EXTEND JOBS-FILE
        WRITE JOB-RECORD
        CLOSE JOBS-FILE
        MOVE "Y" TO LNK-RETURN-CODE
        EXIT PARAGRAPH.
