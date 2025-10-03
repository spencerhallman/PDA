      ******************************************************************
      * DCLGEN TABLE(PURCHASE_TYPE)                                    *
      *        LIBRARY(PDADEMO.COBOL.COPYLIB(DPURTYP))                 *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(PURCHASE_TYPE)                                *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE PURCHASE_TYPE TABLE
           ( PREFIX                         CHAR(5) NOT NULL,
             TYPE                           CHAR(3) NOT NULL,
             DESCRIPTION                    CHAR(32) NOT NULL,
             LAST_ORDER_AMT                 DECIMAL(9, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE PURCHASE_TYPE                      *
      ******************************************************************
       01  PURCHASE-TYPE.
           05  PURCHASE-TYPE-KEY.
               10 PURCHASE-TYPE-PREFIX PIC X(5).
               10 PURCHASE-TYPE-TYPE   PIC X(3).
           05  PURCHASE-TYPE-DESCRIPTION
                                       PIC X(32).
           05  PURCHASE-TYPE-LAST-ORDER-AMT
                                       PIC S9(07)V99  COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************