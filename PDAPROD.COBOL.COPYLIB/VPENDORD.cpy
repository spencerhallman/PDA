      ******************************************************************
      * PENDING ORDER RECORD  -- VSAM KSDS                             *
      ******************************************************************
       01  PENDING-ORDER-RECORD.
           05  PENDING-ORDER-KEY.
               10 PENDING-ORDER-PREFIX PIC 9(05).
               10 PENDING-ORDER-PRE REDEFINES
                  PENDING-ORDER-PREFIX PIC X(05).
               10 PENDING-ORDER-SEQUENCE
                                       PIC 9(05).
           05 PENDING-ORDER-QUANTITY   PIC 9(09)       COMP-3.
           05 PENDING-ORDER-ITEM-KEY.
              10 PENDING-ORDER-ITEM-PREFIX
                                       PIC 9(05).
              10 PENDING-ORDER-ITEM-PRE REDEFINES
                 PENDING-ORDER-ITEM-PREFIX
                                       PIC X(05).
              10 PENDING-ORDER-ITEM-NUMBER
                                       PIC X(32).
           05 PENDING-ORDER-SUPPLIER-KEY.
              10 PENDING-ORDER-SUPPLIER-PREFIX
                                       PIC 9(05).
              10 PENDING-ORDER-SUPPLIER-PRE REDEFINES
                 PENDING-ORDER-SUPPLIER-PREFIX
                                       PIC X(05).
              10 PENDING-ORDER-SUPPLIER-ID
                                       PIC X(32).