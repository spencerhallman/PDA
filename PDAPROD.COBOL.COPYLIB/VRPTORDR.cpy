       01  REPORT-ORDER-RECORD.
           05  REPORT-ORDER-KEY.
               10 REPORT-ORDER-PREFIX          PIC X(05).
               10 REPORT-ORDER-NUMBER          PIC X(10).
           05  REPORT-ORDER-PURCHASE-NUMBER    PIC 9(13) COMP-3.
           05  REPORT-ORDER-DATE-YYMMDD        PIC X(6).
           05  REPORT-ORDER-STATUS             PIC X(32).
           05  REPORT-ORDER-TOTAL-AMOUNT       PIC S9(07)V99 COMP-3.
           05  REPORT-ORDER-NEXT-ITEM-SEQ      PIC 9(05) COMP-3.
           05  REPORT-ORDER-CUSTOMER-KEY.
               10 REPORT-ORDER-CUSTOMER-PREFIX PIC 9(05).
               10 REPORT-ORDER-CUSTOMER-ID     PIC X(32).
           05  REPORT-ORDER-PURCHASE-TYPE-KEY.
               10 REPORT-ORDER-PURCHASE-TYPE-PRE
                                               PIC 9(05).
               10 REPORT-ORDER-PURCHASE-TYPE   PIC 9(03).
           05  REPORT-ORDER-SHIPPER-NUMBER     PIC 9(10).