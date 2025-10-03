      ******************************************************************
      * AFFILIATE CUSTOMER RECORD -- VSAM KSDS                         *
      ******************************************************************
       01  AFF-CUSTOMER-RECORD.
           05  AFF-CUSTOMER-KEY.
               10 AFF-CUSTOMER-PREFIX  PIC 9(05).
               10 AFF-CUSTOMER-ID      PIC X(32).
           05 AFF-CUSTOMER-PASSWORD    PIC X(16).
           05 AFF-CUSTOMER-LAST-ORDER-AMT
                                       PIC S9(07)V99  COMP-3.
           05 AFF-CUSTOMER-TOT-ORDER-COUNT
                                       PIC 9(05).
           05 AFF-CUSTOMER-TOTAL-DOLLAR-AMT
                                       PIC S9(09)V99  COMP-3.
           05 AFF-CUSTOMER-NAME-ADDRESS-INFO.
              10 AFF-CUSTOMER-NAME     PIC X(64).
              10 AFF-CUSTOMER-ADDRESS  PIC X(128).
              10 AFF-CUSTOMER-CITY     PIC X(32).
              10 AFF-CUSTOMER-STATE    PIC X(32).
              10 AFF-CUSTOMER-POSTAL-CODE
                                       PIC X(12).
           05 AFF-CUSTOMER-SHIP-TO-INFO.
              10 AFF-CUSTOMER-SHIP-TO-NAME
                                       PIC X(64).
              10 AFF-CUSTOMER-SHIP-TO-ADDRESS
                                       PIC X(128).
              10 AFF-CUSTOMER-SHIP-TO-CITY
                                       PIC X(32).
              10 AFF-CUSTOMER-SHIP-TO-STATE
                                       PIC X(32).
              10 AFF-CUSTOMER-SHIP-TO-POST-CODE
                                       PIC X(12).
           05 AFF-CUSTOMER-EMAIL-ADDRESS
                                       PIC X(128).