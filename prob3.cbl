       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROB3.
       AUTHOR. COLE DOMBROWSKI.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

            SELECT PAYROLL-INPUT-FILE ASSIGN TO 'payroll.dat'
            ORGANIZATION IS LINE SEQUENTIAL.

            SELECT PAYROLL-OUTPUT-FILE ASSIGN TO 'payroll.doc'
            ORGANIZATION IS LINE SEQUENTIAL.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD  PAYROLL-INPUT-FILE RECORDING MODE IS F.
       01  FILLER                   PIC X(80).

       FD  PAYROLL-OUTPUT-FILE RECORDING MODE IS F.
       01  PRINT-A-SINGLE-LINE      PIC X(132).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 WORKING-VARIABLES.
           05 EOF-PAYROLL-WS        PIC X(3)        VALUE 'NO '.
           05 PAYCHECK-WS           PIC 9(5)V99.

       01 PAYROLL-INPUT-RECORD.
           05 SSN-IN                PIC X(9).
           05 RATE-IN               PIC 9(2)V99.
           05 HOURS-IN              PIC 99.
           05 NAME-IN               PIC X(20).

       01 PAYROLL-OUTPUT-RECORD.
           05 FILLER                PIC X(3)        VALUE SPACE.
           05 SSN-OUT               PIC X(9).
           05 FILLER                PIC X(3)        VALUE SPACE.
           05 RATE-OUT              PIC $$$$,$$9.99.
           05 FILLER                PIC X(3)        VALUE SPACE.
           05 HOURS-OUT             PIC 99.
           05 FILLER                PIC X(3)        VALUE SPACE.
           05 PAYCHECK-OUT          PIC $$$$,$$9.99.
           05 FILLER                PIC X(3)        VALUE SPACE.
           05 NAME-OUT              PIC X(20).
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.

       100-MAINLINE.
           PERFORM 200-OPEN
           PERFORM 300-PROCESS UNTIL EOF-PAYROLL-WS = 'YES'
           PERFORM 900-CLOSE
           STOP RUN.

       200-OPEN.
           OPEN INPUT PAYROLL-INPUT-FILE OUTPUT PAYROLL-OUTPUT-FILE
           PERFORM 250-READ-ONE-RECORD.

       250-READ-ONE-RECORD.
           READ PAYROLL-INPUT-FILE INTO  PAYROLL-INPUT-RECORD
               AT END MOVE 'YES' TO EOF-PAYROLL-WS
           END-READ.

       300-PROCESS.
           MOVE SSN-IN         TO SSN-OUT
           MOVE NAME-IN        TO NAME-OUT
           MOVE HOURS-IN       TO HOURS-OUT
           MOVE RATE-IN        TO RATE-OUT
           COMPUTE PAYCHECK-WS = RATE-IN * HOURS-IN
           MOVE PAYCHECK-WS    TO PAYCHECK-OUT

           MOVE  PAYROLL-OUTPUT-RECORD TO PRINT-A-SINGLE-LINE
           WRITE PRINT-A-SINGLE-LINE AFTER ADVANCING 1 LINE

           PERFORM 250-READ-ONE-RECORD.

       900-CLOSE.
           CLOSE PAYROLL-INPUT-FILE PAYROLL-OUTPUT-FILE.
      *-----------------------------------------------------------------
