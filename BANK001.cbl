           IDENTIFICATION DIVISION.
           PROGRAM-ID. BANK001.
      
           DATA DIVISION.
      
           WORKING-STORAGE SECTION.
      
           01 WS-ACCOUNT-BALANCE      PIC 9(7)V99 VALUE 20000.
           01 WS-WITHDRAW-AMOUNT      PIC 9(7)V99 VALUE 5000.
           01 WS-TRANS-TYPE           PIC X(04) VALUE 'IMPS'.
           01 WS-CHARGE              PIC 9(3)V99 VALUE ZERO.
           01 WS-FINAL-BALANCE       PIC 9(7)V99.
      *>ADDED: New field for customer type (P=Premium, R=Regular)
           01 WS-CUSTOMER-TYPE       PIC X(01) VALUE 'P'.
           01 WS-PENALTY             PIC 9(04)V99 VALUE 0.

      
           PROCEDURE DIVISION.
      
           MAIN-PARA.
      
               IF WS-WITHDRAW-AMOUNT > WS-ACCOUNT-BALANCE
      
                   DISPLAY 'INSUFFICIENT FUNDS'
      
               ELSE
      
                   EVALUATE WS-TRANS-TYPE
      
                       WHEN 'ATM '
                            MOVE 10 TO WS-CHARGE
      
                       WHEN 'UPI '
                            MOVE 0 TO WS-CHARGE
      
                       WHEN 'NEFT'
                            MOVE 5 TO WS-CHARGE
      
                       WHEN 'RTGS'
                            MOVE 25 TO WS-CHARGE
      *>ADDED: New transaction type IMPS with charge of 15
                       WHEN 'IMPS'
                           MOVE 15 TO WS-CHARGE      
                       WHEN OTHER
                            MOVE 50 TO WS-CHARGE
      
                   END-EVALUATE
      *>ADDED: Premium customers should not pay charges
                   IF WS-CUSTOMER-TYPE = 'P'
                       MOVE 0 TO WS-CHARGE
                   END-IF
      
                   COMPUTE WS-FINAL-BALANCE =
                           WS-ACCOUNT-BALANCE
                         - WS-WITHDRAW-AMOUNT
                         - WS-CHARGE
      *>CHANGED: Check if balance FALLS BELOW 10000 (< not <=)
                   IF WS-FINAL-BALANCE < 10000
      *>CHANGED: Fixed typo T0 to TO
                       MOVE 250 TO WS-PENALTY
                       COMPUTE WS-FINAL-BALANCE = WS-FINAL-BALANCE - WS-PENALTY
                       DISPLAY "PENALTY APPLIED = " WS-PENALTY
                   END-IF
                   DISPLAY 'CHARGE APPLIED =' WS-CHARGE
                   DISPLAY 'FINAL BALANCE = '
                           WS-FINAL-BALANCE
      
               END-IF
      
               STOP RUN.