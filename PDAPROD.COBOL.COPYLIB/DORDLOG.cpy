      ******************************************************************
      * DCLGEN TABLE(ORDER_LOG)                                        *
      *        LIBRARY(PDAPROD.COBOL.COPYLIB(DORDLOG))                 *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(ORDER-LOG-)                                       *
      *        STRUCTURE(ORDER-LOG)                                    *
      *        QUOTE                                                   *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE ORDER_LOG TABLE
           ( PREFIX                         CHAR(5) NOT NULL,
             NUMBER                         CHAR(10) NOT NULL,
             PURCHASE_NUMBER                INTEGER NOT NULL,
             DATE_YYMMDD                    CHAR(6) NOT NULL,
             STATUS                         CHAR(32) NOT NULL,
             TOTAL_AMOUNT                   DECIMAL(9, 2) NOT NULL,
             CUSTOMER_PREFIX                CHAR(5) NOT NULL,
             CUSTOMER_ID                    CHAR(32) NOT NULL,
             PURCHASE_TYPE_PRE              CHAR(5) NOT NULL,
             PURCHASE_TYPE                  CHAR(3) NOT NULL,
             SHIPPER_NUMBER                 INTEGER NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE ORDER_LOG                          *
      ******************************************************************
       01  ORDER-LOG.
         05  ORDER-LOG-KEY.
           10  ORDER-LOG-PREFIX            PIC X(5).
           10  ORDER-LOG-NUMBER            PIC X(10).
         05  ORDER-LOG-PURCHASE-NUMBER     PIC S9(9) COMP.
         05  ORDER-LOG-DATE-YYMMDD         PIC X(6).
         05  ORDER-LOG-STATUS              PIC X(32).
         05  ORDER-LOG-TOTAL-AMOUNT        PIC S9(7)V9(2) COMP-3.
         05  ORDER-LOG-CUSTOMER-PREFIX     PIC X(5).
         05  ORDER-LOG-CUSTOMER-ID         PIC X(32).
         05  ORDER-LOG-PURCHASE-TYPE-PRE   PIC X(5).
         05  ORDER-LOG-PURCHASE-TYPE       PIC X(3).
         05  ORDER-LOG-SHIPPER-NUMBER      PIC S9(9) COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************