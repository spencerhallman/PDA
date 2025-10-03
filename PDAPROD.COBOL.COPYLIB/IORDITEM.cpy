      ******************************************************************
      * IMS ORDER ITEM SEGMENT                                         *
      * PARENT      : ORDER (ROOT SEGMENT)                             *
      * SEGMENT     : ORDITEM                                          *
      * DATABASE    : ORDER1DB                                         *
      * ORGANIZATION: HIDAM                                            *
      ******************************************************************
       01  ORDER-ITEM-SEGMENT.
           05  ORDER-ITEM-KEY.
               10 ORDER-ITEM-PREFIX    PIC 9(05).
               10 ORDER-ITEM-SEQUENCE  PIC 9(05).
           05  ORDER-ITEM-QUANTITY     PIC 9(09)       COMP-3.
           05  ORDER-ITEM-UNIT-PRICE   PIC S9(08)V99   COMP-3.
           05  ORDER-ITEM-ITEM-KEY.
               10 ORDER-ITEM-ITEM-PREFIX
                                       PIC 9(05).
               10 ORDER-ITEM-ITEM-NUMBER
                                       PIC X(32).
           05  ORDER-ITEM-SUPPLIER-KEY.
               10 ORDER-ITEM-SUPPLIER-PREFIX
                                       PIC 9(05).
               10 ORDER-ITEM-SUPPLIER-ID
                                       PIC X(32).