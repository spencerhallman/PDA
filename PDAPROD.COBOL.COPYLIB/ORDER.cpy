      ******************************************************************
      * IMS ORDER ROOT SEGMENT                                         *
      * SEGMENT     : ORDER                                            *
      * DATABASE    : ORDER1DB                                         *
      * ORGANIZATION: HIDAM                                            *
      ******************************************************************
       01  ORDER-SEGMENT.
           05  ORDER-KEY.
               10 ORDER-PREFIX         PIC 9(05).
               10 ORDER-NUMBER         PIC 9(10).
           05  ORDER-PURCHASE-NUMBER   PIC 9(13)       COMP-3.
           05  ORDER-DATE-YYMMDD       PIC X(6).
           05  ORDER-STATUS            PIC X(32).
           05  ORDER-TOTAL-AMOUNT      PIC S9(07)V99   COMP-3.
           05  ORDER-NEXT-ITEM-SEQUENCE
                                       PIC 9(05)       COMP-3.
           05  ORDER-CUSTOMER-KEY.
               10 ORDER-CUSTOMER-PREFIX
                                       PIC 9(05).
               10 ORDER-CUSTOMER-ID    PIC X(32).
           05  ORDER-PURCHASE-TYPE-KEY.
               10 ORDER-PURCHASE-TYPE-PREFIX
                                       PIC 9(05).
               10 ORDER-PURCHASE-TYPE  PIC 9(03).
           05  ORDER-SHIPPER-NUMBER    PIC 9(10).