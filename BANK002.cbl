           IDENTIFICATION DIVISION.
           PROGRAM-ID. BANK002.
           ENVIRONMENT DIVISION.
           DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 WS-BALANCE PIC 9(7)V99 VALUE 120000.
           01 WS-TRAN-AMT PIC 9(07)V99 VALUE 110000.
           01 WS-TRANS-TYPE PIC X(04) VALUE 'RTGS'.
           01 WS-CUTOMER-TYPE PIC X(01) VALUE 'R'.
           01 WS-CHARGE PIC 9(04)V99 VALUE ZERO.
           01 WS-FINAL-BALANCE PIC 9(7)V99.
           01 WS-PENALTY PIC 9(04)V99 VALUE 200.
           01 WS-PROCESSING-FEE PIC 9(04)V99 VALUE 50.

           PROCEDURE DISVISON.
           MAIN-PARA.
               IF WS-TRAN-AMT > WS-BALANCE
                   DISPLAY 'INSUFFICIENT FUNDS
               ELSE
                   EVALUATE WS-TRANS-TYPE
                       WHEN 'UPI'
                           MOVE 0 TO WS-CHARGE
                       WHEN 'NEFT'
                           MOVE 5 TO WS-CHARGE
                       WHEN 'RTGS'
                           MOVE 25 TO WS-CHARGE
                       WHEN 'IMPS'
                           MOVE 15 TO WS-CHARGE
                       WHEN OTHER
                           MOVE 50 TO WS-CHARGE
                   END-EVALUATE

                   EVALUATE TRUE
                       WHEN WS-CUSTOMER-TYPE = 'S'
                           MOVE 0 TO WS-CAHRGE
                       WHEN OTHER
                            CONTINUE
                   END-EVALUATE
                   IF WS-TRN-AMT > 100000
                       COMPUTE WS-CHARGE = WS-CHARGE + WS-PROCESSING-FEE
                       DISPLAY 'ADDITIONAL CHARGE OF 50 APPLIED FOR TRANSACTIONS ABOVE 100000'
                   END-IF
                   COMPUTE  WS-FINAL-BALANCE =
                                WS-BALANCE - WS-TRAN-AMT - WS-CHARGE

                   IF WS-FINAL-BALANCE < 5000
                       COMPUTE WS-FINAL-BALANCE = WS-FINAL-BALANCE - WS-PENALTY
                       DISPLAY 'PENALTY APPLIED FOR LOW BALANCE. PENALTY AMOUNT: ' WS-PENALTY'
                   END-IF
                   DISPLAY 'TRANSANCTION CHARGE: ' WS-CHARGE
                   DISPLAY 'PROCESSING FEE: ' WS-PROCESSING-FEE
                   DISPLAY 'FINAL BALANCE: ' WS-FINAL-BALANCE
               END-IF.
               STOP RUN.