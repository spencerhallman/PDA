      ******************************************************************
      * DCLGEN TABLE(ITEM_SUPPLIER)                                    *
      *        LIBRARY(PDADEMO.COBOL.COPYLIB(DITMSUP))                 *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(ITEM_SUPPLIER)                                *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE ITEM_SUPPLIER TABLE
           ( ITEM_PREFIX                    CHAR(5) NOT NULL,
             ITEM_NUMBER                    CHAR(32) NOT NULL,
             SUPPLIER_PREFIX                CHAR(5) NOT NULL,
             SUPPLIER_ID                    CHAR(32) NOT NULL,
             QUANTITY_ON_HAND               INTEGER NOT NULL,
             UNIT_PRICE                     DECIMAL(10, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE ITEM_SUPPLIER                      *
      ******************************************************************
       01  ITEM-SUPPLIER.
           05  ITEM-SUPPLIER-ITEM-KEY.
               10 ITEM-SUPPLIER-ITEM-PREFIX
                                       PIC X(5).
               10 ITEM-SUPPLIER-ITEM-NUMBER
                                       PIC X(32).
           05  ITEM-SUPPLIER-SUPPLIER-KEY.
               10 ITEM-SUPPLIER-SUPPLIER-PREFIX
                                       PIC X(5).
               10 ITEM-SUPPLIER-SUPPLIER-ID
                                       PIC X(32).
           05  ITEM-SUPPLIER-QUANTITY-ON-HAND
                                       PIC S9(9)      USAGE COMP.
           05  ITEM-SUPPLIER-UNIT-PRICE
                                       PIC S9(8)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************