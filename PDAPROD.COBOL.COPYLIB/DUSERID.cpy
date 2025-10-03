      ******************************************************************
      * DCLGEN TABLE(USERID)                                           *
      *        LIBRARY(PDADEMO.COBOL.COPYLIB(DUSERID))                 *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(USERID)                                       *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE USERID TABLE
           ( ID                             CHAR(8)   NOT NULL,
             NUMBER                         INTEGER   NOT NULL,
             LAST_ACCESSED                  DATE      NOT NULL,
             ACTIVE_SCENARIOS               CHAR(250) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE USERID                             *
      ******************************************************************
       01  USERID.
           05  USERID-KEY.
               10 USERID-ID            PIC X(8).
           05 USERID-NUMBER            PIC S9(9) USAGE COMP.
           05 USERID-LAST-ACCESSED     PIC X(10).
           05 USERID-ACTIVE-SCENARIOS  PIC X(250).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************