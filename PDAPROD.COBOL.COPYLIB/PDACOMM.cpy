      ******************************************************************
      * PRODUCT DEMONSTRATION APPLICATION (PDA)                        *
      * CICS COMMAREA DEFINITION                                       *
      * LENGTH = 2000                                                  *
      ******************************************************************
         03  PDA-COMMAREA.
             05  PC-COMMAREA-LTH       PIC S9(04)      COMP.
             05  PC-PREV-PGRMID        PIC X(08).
             05  PC-NEXT-PGRMID        PIC X(08).
             05  PC-USERID-ID          PIC X(08).
             05  PC-USERID-NUMBER      PIC 9(05).
             05  PC-PREV-MENU-SEL      PIC X.
             05  PC-CUSTOMER-ID        PIC X(32).
             05  PC-ITEM-CATEGORY      PIC X(32).
             05  PC-ITEM-SUB-CATEGORY  PIC X(32).
             05  PC-SELECTED-ITEM      PIC X(32).
             05  PC-ORDER-NUMBER       PIC X(10).
             05  PC-ORIGINATING-PGRMID PIC X(08).
      *
             05  PC-ACTIVE-SCENARIOS-GRP
                                       PIC X(250).
             05  PC-ACTIVE-SCENARIOS-ARRAY
                                       REDEFINES
                                       PC-ACTIVE-SCENARIOS-GRP.
                 10 PC-ACTIVE-SCENARIO OCCURS 250 TIMES
                                       PIC X.
      *
             05  PC-PDA008-ORIGINATING-PGRMID
                                       PIC X(08).
             05  FILLER                PIC X(564).
             05  PC-PROGRAM-WORKAREA   PIC X(1000).