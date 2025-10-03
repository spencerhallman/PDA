      ******************************************************************
      * DCLGEN TABLE(PDAPROD.AFFILIATE_SUPPLIER)                       *
      *        LIBRARY(PDAPROD.COBOL.COPYLIB(DAFFSUPP))                *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        NAMES(AFF-SUPP-)                                        *
      *        STRUCTURE(AFFILIATE-SUPPLIER)                           *
      *        QUOTE                                                   *
      *        COLSUFFIX(YES)                                          *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE AFFILIATE_SUPPLIER TABLE
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
      * COBOL DECLARATION FOR TABLE AFFILIATE_SUPPLIER                 *
      ******************************************************************
       01  AFFILIATE-SUPPLIER.
           05  AFF-SUPP-KEY.
               10  AFF-SUPP-PREFIX           PIC X(5).
               10  AFF-SUPP-SUPPLIER-ID      PIC X(32).
           05  AFF-SUPP-PASSWORD             PIC X(32).
           05  AFF-SUPP-NAME                 PIC X(64).
           05  AFF-SUPP-ADDRESS              PIC X(128).
           05  AFF-SUPP-CITY                 PIC X(32).
           05  AFF-SUPP-STATE                PIC X(32).
           05  AFF-SUPP-POSTAL-CODE          PIC X(12).
           05  AFF-SUPP-EMAIL-ADDRESS        PIC X(128).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************