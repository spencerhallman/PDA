      ******************************************************************
      * DCLGEN TABLE(SUPPLIER)                                         *
      *        LIBRARY(PDADEMO.COBOL.COPYLIB(DSUPPLR))                 *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(SUPPLIER)                                     *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE SUPPLIER TABLE
           ( PREFIX                         CHAR(5) NOT NULL,
             SUPPLIER_ID                    CHAR(32) NOT NULL,
             PASSWORD                       CHAR(32) NOT NULL,
             NAME                           CHAR(64) NOT NULL,
             ADDRESS                        CHAR(128) NOT NULL,
             CITY                           CHAR(32) NOT NULL,
             STATE                          CHAR(32) NOT NULL,
             POSTAL_CODE                    CHAR(12) NOT NULL,
             EMAIL_ADDRESS                  CHAR(128) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE SUPPLIER                           *
      ******************************************************************
       01  SUPPLIER.
           05  SUPPLIER-KEY.
               10 SUPPLIER-PREFIX      PIC X(5).
               10 SUPPLIER-SUPPLIER-ID PIC X(32).
           05 SUPPLIER-PASSWORD        PIC X(32).
           05 SUPPLIER-NAME            PIC X(64).
           05 SUPPLIER-ADDRESS         PIC X(128).
           05 SUPPLIER-CITY            PIC X(32).
           05 SUPPLIER-STATE           PIC X(32).
           05 SUPPLIER-POSTAL-CODE     PIC X(12).
           05 SUPPLIER-EMAIL-ADDRESS   PIC X(128).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************