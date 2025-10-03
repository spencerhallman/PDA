      ******************************************************************
      * DCLGEN TABLE(PDAPROD.USERID1)                                  *
      *        LIBRARY(PDAPROD.COBOL.COPYLIB(DUSERID1))                *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(USERID1)                                      *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE USERID1 TABLE
           ( ID                             CHAR(8) NOT NULL,
             NUMBER                         INTEGER NOT NULL,
             LAST_ACCESSED                  DATE NOT NULL,
             ACTIVE_SCENARIOS               CHAR(250) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE USERID1                            *
      ******************************************************************
       01  USERID1.
           05  USERID1-KEY.
               10 USERID1-ID       PIC X(8).
           05 USERID1-NUMBER       PIC S9(9) USAGE COMP.
           05 USERID1-LAST-ACCESSED
                                   PIC X(10).
           05 USERID1-ACTIVE-SCENARIOS
                                   PIC X(250).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************