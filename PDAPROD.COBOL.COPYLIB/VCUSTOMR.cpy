      ******************************************************************
      * CUSTOMER RECORD -- VSAM KSDS                                   *
      ******************************************************************
       01  CUSTOMER-RECORD.
           05  CUSTOMER-KEY.
               10 CUSTOMER-PREFIX      PIC 9(05).
               10 CUSTOMER-PRE REDEFINES
                  CUSTOMER-PREFIX      PIC X(05).
               10 CUSTOMER-ID          PIC X(32).
           05 CUSTOMER-PASSWORD        PIC X(16).
           05 CUSTOMER-LAST-ORDER-AMT  PIC S9(07)V99  COMP-3.
           05 CUSTOMER-TOTAL-ORDER-COUNT
                                       PIC 9(05).
           05 CUSTOMER-TOTAL-DOLLAR-AMT-GRP.
              10 CUSTOMER-TOTAL-DOLLAR-AMT
                                       PIC S9(09)V99  COMP-3.
           05 CUSTOMER-NAME-ADDRESS-INFO.
              10 CUSTOMER-NAME         PIC X(64).
              10 CUSTOMER-ADDRESS      PIC X(128).
              10 CUSTOMER-CITY         PIC X(32).
              10 CUSTOMER-STATE        PIC X(32).
              10 CUSTOMER-POSTAL-CODE  PIC X(12).
           05 CUSTOMER-SHIP-TO-INFO.
              10 CUSTOMER-SHIP-TO-NAME PIC X(64).
              10 CUSTOMER-SHIP-TO-ADDRESS
                                       PIC X(128).
              10 CUSTOMER-SHIP-TO-CITY PIC X(32).
              10 CUSTOMER-SHIP-TO-STATE
                                       PIC X(32).
              10 CUSTOMER-SHIP-TO-POSTAL-CODE
                                       PIC X(12).
           05 CUSTOMER-EMAIL-ADDRESS   PIC X(128).