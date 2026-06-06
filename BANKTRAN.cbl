           IDENTIFICATION DIVISION.
           PROGRAM-ID. BANKTRAN.
           ENVIRONMENT DIVISION.
           DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 WS-BALANCE PIC 9(07)V99 VALUE 10000.00.
           01 WS-TRAN-TYPE PIC X(04) VALUE "RTGS".
           01 WS-FINAL-BALANCE PIC 9(07)V99.
           01 WS-CHARGE PIC 9(04)V99 VALUE 0.
           PROCEDURE DIVISION.
           MAIN-PARA.
               DISPLAY "ACCOUNT BALANCE : " WS-BALANCE.
               EVALUATE TRUE
                   WHEN WS-TRAN-TYPE="ATM"
                       MOVE 10.00 TO WS-CHARGE
                       COMPUTE WS-FINAL-BALANCE=WS-BALANCE + WS-CHARGE
                   WHEN WS-TRAN-TYPE="UPI"
                       MOVE 0.00 TO WS-CHARGE
                       COMPUTE WS-FINAL-BALANCE=WS-BALANCE + WS-CHARGE
                   WHEN WS-TRAN-TYPE="NEFT"
                       MOVE 5.00 TO WS-CHARGE
                       COMPUTE WS-FINAL-BALANCE=WS-BALANCE + WS-CHARGE
                   WHEN WS-TRAN-TYPE="RTGS"
                       MOVE 25.00 TO WS-CHARGE
                       COMPUTE WS-FINAL-BALANCE=WS-BALANCE + WS-CHARGE
                   WHEN OTHER
                   MOVE 50.00 TO WS-CHARGE
                       COMPUTE WS-FINAL-BALANCE=WS-BALANCE + WS-CHARGE
               END-EVALUATE.
               DISPLAY "TRANSACTION TYPE : " WS-TRAN-TYPE.
               DISPLAY "CHARGES : " WS-CHARGE.
               DISPLAY "FINAL BALANCE : " WS-FINAL-BALANCE.
               STOP RUN.
                       