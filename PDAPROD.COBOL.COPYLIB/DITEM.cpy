      ******************************************************************
      * DCLGEN TABLE(ITEM)                                             *
      *        LIBRARY(PDADEMO.COBOL.COPYLIB(DITEM))                   *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(ITEM)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE ITEM TABLE
           ( PREFIX                         CHAR(5) NOT NULL,
             NUMBER                         CHAR(32) NOT NULL,
             CATEGORY_NAME                  CHAR(32) NOT NULL,
             SUB_CATEGORY_NAME              CHAR(32) NOT NULL,
             NAME                           CHAR(50) NOT NULL,
             LENGTH                         DECIMAL(10, 2) NOT NULL,
             DIAMETER                       DECIMAL(10, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE ITEM                               *
      ******************************************************************
       01  ITEM.
           05  ITEM-KEY.
               10 ITEM-PREFIX          PIC X(5).
               10 ITEM-NUMBER          PIC X(32).
           05  ITEM-CATEGORY-NAME      PIC X(32).
           05  ITEM-SUB-CATEGORY-NAME  PIC X(32).
           05  ITEM-NAME               PIC X(50).
           05  ITEM-LENGTH             PIC S9(8)V9(2) USAGE COMP-3.
           05  ITEM-DIAMETER           PIC S9(8)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************