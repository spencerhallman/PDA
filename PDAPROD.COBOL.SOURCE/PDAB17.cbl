       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDAB17.

      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDAB17                                            *
      *                                                               *
      * FUNCTION:   PROGRAM PDAB17 IS THE BATCH MQSERIES CUSTOMER     *
      *             ORDER QUERY PROGRAM. THE PROGRAM IS DESIGNED      *
      *             TO RUN INDEFINITELY (UNLIMITED MQSERIES WAIT      *
      *             INTERVAL) TO PROCESS MESSAGES ON THE CUSTOMER     *
      *             ORDER QUERY QUEUE AS THEY ARRIVE.                 *
      *                                                               *
      *             THE PROGRAM PROVIDES MQSERIES BATCH FUNCTIONALITY *
      *             FOR VARIOUS PRODUCT LINES.                        *
      *                                                               *
      *             THE MQSERIES MESSAGE FROM THE APPLICATION QUEUE   *
      *             IS RETRIEVED AND THE CUSTOMER IDENTIFICATION      *
      *             FROM THE MQSERIES MESSAGE IS USED TO PROCESS ALL  *
      *             ORDER INFORMATION FOR THE SELECTED CUSTOMER.      *
      *                                                               *
      *             CUSTOMER ORDER RELATED INFORMATION IS THEN PLACED *
      *             ON A MQSERIES RESPONSE QUEUE TO BE PROCESSED BY   *
      *             THE INITIATING PROGRAM (PDAB16).                  *
      *                                                               *
      *                                                               *
      * FILES   :   PARAMETER FILE   -  SEQUENTIAL (INPUT)            *
      *             ORDER DATABASE   -  IMS/DLI    (READ-ONLY)        *
      *                                                               *
      *                                                               *
      * PROGRAMS INITIATED:    NONE                                   *
      *                        CUSTOMER ORDER PROCESSING PROGRAM,     *
      *                        PDAB17 WAITS FOR MESSAGES TO ARRIVE ON *
      *                        THE CUSTOMER REQUEST QUEUE, FROM PDAB16*
      *                                                               *
      *****************************************************************
      *             PROGRAM CHANGE LOG                                *
      *             -------------------                               *
      *                                                               *
      *  DATE       UPDATED BY            CHANGE DESCRIPTION          *
      *  ---------- --------------------  --------------------------  *
      *                                                               *
      *  MM/DD/YYYY XXXXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXX  *
      *                                                               *
      *****************************************************************

       ENVIRONMENT DIVISION.
                                                                        00380000
       INPUT-OUTPUT SECTION.                                            00390000
       FILE-CONTROL.                                                    00410000
                                                                        00540000
           SELECT INPUT-PARAMETERS   ASSIGN TO IPARAMS.                 00550000
                                                                        00540000
                                                                        00540000
       DATA DIVISION.
       FILE SECTION.                                                    00630000
                                                                        00640000
       FD INPUT-PARAMETERS                                              00650000
           LABEL RECORDS ARE STANDARD                                   00660000
           RECORDING MODE IS F                                          00670000
           RECORD CONTAINS 80 CHARACTERS                                00680000
           BLOCK CONTAINS 0 CHARACTERS.                                 00690000
                                                                        00700000
       01  INPUT-PARAMETER-RECORD      PIC X(80).                       00710000
           EJECT                                                        01220000
                                                                        00730000
                                                                        00730000
       WORKING-STORAGE SECTION.

      *****************************************************************
      *    77 LEVEL DATA ITEMS HERE  (SUBSCRIPTS, INDEXES ETC.)       *
      *****************************************************************
       77  WS-SUB1                     PIC S9(04)  COMP   VALUE +0.
       77  WS-SUB2                     PIC S9(04)  COMP   VALUE +0.
       77  WS-RETURN-CODE              PIC  9(04)  COMP   VALUE  0.
       77  WS-QUEUE-NAME-LTH           PIC S9(04)  COMP   VALUE +48.
       77  WS-ORDER-MAX                PIC S9(04)  COMP   VALUE +14.
       77  WS-MAX-PARAMETERS           PIC S9(04)  COMP   VALUE +500.
       77  WS-USERID-PARM-COUNT        PIC S9(04)  COMP   VALUE +0.
       77  WS-PARAMETER-RECORDS-IN     PIC S9(05)  COMP-3 VALUE +0.

      *****************************************************************
      *    SWITCHES                                                   *
      *****************************************************************
       01  WS-SWITCHES.

           05  WS-ERROR-FOUND-SW       PIC 9(01)             VALUE 0.
               88  NO-ERROR-FOUND                            VALUE 0.
               88  ERROR-FOUND                               VALUE 1.
               88  FATAL-ERROR-FOUND                         VALUE 9.

           05  WS-PROCESS-COMPLETE-SW  PIC X(01)             VALUE 'N'.
               88  PROCESS-COMPLETE                          VALUE 'Y'.
               88  NOT-PROCESS-COMPLETE                      VALUE 'N'.

           05  WS-END-OF-PARM-FILE-SW  PIC X(01)             VALUE 'N'.
               88  END-OF-PARM-FILE                          VALUE 'Y'.
               88  NOT-END-OF-PARM-FILE                      VALUE 'N'.

           05  WS-PARM-ERROR-FOUND-SW  PIC X(01)             VALUE 'N'.
               88  PARM-ERROR-FOUND                          VALUE 'Y'.
               88  NOT-PARM-ERROR-FOUND                      VALUE 'N'.

           05  WS-MORE-MESSAGES-SW     PIC X(01)             VALUE 'Y'.
               88  MORE-MESSAGES                             VALUE 'Y'.
               88  NO-MORE-MESSAGES                          VALUE 'N'.

           05  WS-SKIP-ERROR-CHECK-SW  PIC 9(01)             VALUE 0.
               88  NOT-SKIP-ERROR-CHECK                      VALUE 0.
               88  SKIP-ERROR-CHECK                          VALUE 1.

           05  WS-ERROR-IS-FORMATTED-SW PIC 9(01)            VALUE 0.
               88  ERROR-IS-FORMATTED                        VALUE 1.


           EJECT
      *****************************************************************
      *    MISCELLANEOUS WORK FIELDS                                  *
      *****************************************************************

       01  WS-MISCELLANEOUS-FIELDS.
           05  WMF-ABSTIME             PIC S9(15)  VALUE +0    COMP-3.
           05  WMF-DATE-MMDDYY         PIC X(08)   VALUE SPACES.
           05  WMF-TIME-HHMMSS         PIC X(08)   VALUE SPACES.
           05  WMF-MESSAGE-AREA        PIC X(80)   VALUE SPACES.
           05  WMF-USERID              PIC X(08)   VALUE SPACES.
           05  WMF-CUSTOMER-ID         PIC X(32)   VALUE 'ARROW'.
           05  WMF-PSBNAME             PIC X(08)   VALUE 'PDAB17'.

           05  WMF-DATE-1.
               10  WMF-DATE-1-YY       PIC X(02)   VALUE SPACES.
               10  WMF-DATE-1-MM       PIC X(02)   VALUE SPACES.
               10  WMF-DATE-1-DD       PIC X(02)   VALUE SPACES.

           05  WMF-DATE-2.
               10  WMF-DATE-2-MM       PIC X(02)   VALUE SPACES.
               10  FILLER              PIC X(01)   VALUE '/'.
               10  WMF-DATE-2-DD       PIC X(02)   VALUE SPACES.
               10  FILLER              PIC X(01)   VALUE '/'.
               10  WMF-DATE-2-YY       PIC X(02)   VALUE SPACES.

           05  WMF-ORDER-KEY           PIC X(15)   VALUE SPACES.
           05  WMF-ORDER-KEY-R         REDEFINES   WMF-ORDER-KEY.
               10  WMF-ORDER-PREFIX    PIC 9(05).
               10  WMF-ORDER-NUMBER    PIC X(10).

           05  WMF-TOTAL-ORDERS        PIC 9(05)    VALUE 0   COMP-3.
           05  WMF-TOTAL-DOLLAR-AMOUNT PIC 9(09)V99 VALUE 0   COMP-3.
           05  WMF-TOTAL-FEES          PIC 9(09)V99 VALUE 0   COMP-3.
           05  WMF-AVG-DOLLAR-AMOUNT   PIC 9(09)V99 VALUE 0   COMP-3.
           05  WMF-LAST-ORDER-DATE     PIC X(06)    VALUE SPACES.
           05  WMF-LAST-ORDER-AMOUNT   PIC 9(07)V99 VALUE 0   COMP-3.
           05  WMF-LAST-ORDER-NUMBER   PIC X(10)    VALUE SPACES.

           05  WMF-HOLD-DATE-CCYYMMDD  PIC X(08)    VALUE SPACES.

           05  WMF-DATE-CCYYMMDD.
               10  WMF-DATE-CC         PIC 9(02)    VALUE ZEROES.
               10  WMF-DATE-YYMMDD.
                   15  WMF-DATE-YY     PIC 9(02)    VALUE ZEROES.
                   15  WMF-DATE-MM     PIC 9(02)    VALUE ZEROES.
                   15  WMF-DATE-DD     PIC 9(02)    VALUE ZEROES.

           05  WMF-ACTIVE-SCENARIOS    PIC X(250)   VALUE SPACES.
           05  WMF-ACTIVE-SCENARIOS-R  REDEFINES WMF-ACTIVE-SCENARIOS
                                       OCCURS 250 TIMES
                                       PIC X(01).

           05  WMF-SAVE-MSGID          PIC X(24)    VALUE SPACES.
           05  WMF-SPECIAL-MSGID       PIC X(24)    VALUE
               'MQ-PDAB17-CUSTOMER-2033 '.

      *** CREDIT RATING VALUES
      *** A=APPROVED  R=REJECTED  U=UNAVAILABLE
      ***
           05  WMF-FINAL-CREDIT-RATING PIC X(01)    VALUE SPACES.

           05  WMF-CREDIT-RATINGS      PIC X(03)    VALUE 'UUU'.
           05  WMF-CREDIT-RATINGS-R    REDEFINES WMF-CREDIT-RATINGS
                                       OCCURS 3 TIMES
                                       PIC X(01).


      *****************************************************************
      *  THIS AREA CONTAINS THE DATA FROM THE FUNCTION CURRENT-DATE   *
      *****************************************************************

       01  WS-CURRENT-DATE-TIME.
           03  WS-CDT-DATE.
               05  WS-CDT-D-YEAR       PIC 9(4)  VALUE ZEROES.
               05  WS-CDT-D-MONTH      PIC 99    VALUE ZEROES.
               05  WS-CDT-D-DAY        PIC 99    VALUE ZEROES.
           03  WS-CDT-TIME.
               05  WS-CDT-T-HOURS      PIC 99    VALUE ZEROES.
               05  WS-CDT-T-MINUTES    PIC 99    VALUE ZEROES.
               05  WS-CDT-T-SECONDS    PIC 99    VALUE ZEROES.
               05  WS-CDT-T-HUNDRETHS  PIC 99    VALUE ZEROES.
           03  WS-CDT-GMT-INDICATOR    PIC X     VALUE SPACES.
               88  AHEAD-OF-GMT                  VALUE '+'.
               88  BEHIND-GMT                    VALUE '-'.
               88  GMT-NOT-AVAILABLE             VALUE '0'.
           03  WS-CDT-GMT-TIME-DIFFERENTIAL.
               05  WS-CDT-GMT-HOURS    PIC 99    VALUE ZEROES.
               05  WS-CDT-GMT-MINUTES  PIC 99    VALUE ZEROES.
           EJECT

      *****************************************************************
      *  FORMATTED PRINT / DISPLAY LINES                              *
      *****************************************************************

       01  WS-DISPLAY-LINES.
           05  WDL-LINE-01.
               10  WDL-LITERAL-01       PIC X(25) VALUE SPACES.
               10  FILLER               PIC X(03) VALUE ' : '.
               10  WDL-FIELD-01         PIC X(17) VALUE SPACES.
               10  WDL-NUM-01           REDEFINES WDL-FIELD-01
                                        PIC ZZ,ZZZ,ZZZ,ZZ9.99.
               10  WDL-NUM-02           REDEFINES WDL-FIELD-01
                                        PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.
               10  WDL-ALPHANUM-01      REDEFINES WDL-FIELD-01.
                   15 FILLER            PIC X(11).
                   15 WDL-ALPHANUM-LEN06
                                        PIC X(06).
               10  WDL-ALPHANUM-02      REDEFINES WDL-FIELD-01.
                   15 FILLER            PIC X(07).
                   15 WDL-ALPHANUM-LEN10
                                        PIC X(10).
               10  FILLER               PIC X(36) VALUE SPACES.
           EJECT

      *****************************************************************
      *    FILE LAYOUTS                                               *
      *****************************************************************
      *****************************************************************
      *    PARAMETER RECORD LAYOUTS                                   *
      *****************************************************************

       01  WS-PARAMETER-RECORD.
           05  WPR-RECORD-TYPE         PIC X(01).
               88  WPR-USERID          VALUE 'U'.
           05  FILLER                  PIC X(01).
           05  WPR-RECORD-DATA         PIC X(78).
           05  WPR-RECORD-DATA-USERID  REDEFINES WPR-RECORD-DATA.
               10  WPR-USERID-VALUE    PIC X(08).
               10  FILLER              PIC X(70).


      *****************************************************************
      *    PARAMETER RECORD ARRAY                                     *
      *****************************************************************
       01  WS-PARAMETER-RECORD-ARRAY.
           05  WPRA-RECORD             OCCURS 500 TIMES
                                       PIC X(80).

      *****************************************************************
      *    IMS / DLI DEFINITIONS                                      *
      *****************************************************************
                                                                        03330000
      ***************************************************************** 03060000
      *    IMS FUNCTION DEFINITIONS                                   * 03070000
      ***************************************************************** 03080000
                                                                        03330000
       01  IMS-CALL-FUNCTIONS.                                          03330000
           05 ICF-GHU                  PIC X(04)   VALUE 'GHU'.         03330000
           05 ICF-GU                   PIC X(04)   VALUE 'GU'.          03330000
           05 ICF-GN                   PIC X(04)   VALUE 'GN'.          03330000
           05 ICF-ISRT                 PIC X(04)   VALUE 'ISRT'.        03330000
           05 ICF-REPL                 PIC X(04)   VALUE 'REPL'.        03330000
           05 ICF-DLET                 PIC X(04)   VALUE 'DLET'.        03330000
                                                                        03330000
      ***************************************************************** 03060000
      *    IMS SEGMENT SEARCH ARGUMENTS (SSA)                         * 03070000
      ***************************************************************** 03080000
                                                                        03090000
       01  ORDER-SSA-QUAL.                                              03100000
           03  FILLER                  PIC X(8)  VALUE 'ORDER'.         03110000
           03  FILLER                  PIC X     VALUE '('.             03120000
           03  FILLER                  PIC X(8)  VALUE 'ORDKEY'.        03130000
           03  OSQ-REL-OPER            PIC XX    VALUE ' ='.            03140000
           03  OSQ-ORDER-KEY.                                           03150000
               05  OSQ-ORDER-PREFIX    PIC 9(5)  VALUE ZEROES.          03160000
               05  OSQ-ORDER-NUMBER    PIC 9(10) VALUE ZEROES.          03170000
           03  FILLER                  PIC X     VALUE ')'.             03180000
                                                                        03190000
       01  ORDER-SSA-UNQUAL.                                            03200000
           03  FILLER                  PIC X(8)  VALUE 'ORDER'.         03210000
           03  FILLER                  PIC X     VALUE SPACES.          03220000
                                                                        03230000
       01  ORDER-ITEM-SSA-UNQUAL.                                       03240000
           03  FILLER                  PIC X(8)  VALUE 'ORDITEM'.       03250000
           03  FILLER                  PIC X     VALUE SPACES.          03260000
           EJECT                                                        03270000
                                                                        03330000
      ***************************************************************** 03280000
      *    IMS SEGMENT I/O AREAS                                      * 03290000
      ***************************************************************** 03300000
      *****************************************************************
      *    ORDER DATABASE ROOT SEGMENT (ORDER1DB)                     *
      *****************************************************************
           COPY IORDER.
           EJECT


      *****************************************************************
      *    MQSERIES MISCELLANEOUS APPLICATION FIELDS / VARIABLES      *
      *****************************************************************

       01  MQS-MISCELLANEOUS.
           05  MQS-HCONN               PIC S9(9)  BINARY  VALUE +0.
           05  MQS-HOBJECT             PIC S9(9)  BINARY  VALUE +0.
           05  MQS-HOBJECT-REQUEST-Q   PIC S9(9)  BINARY  VALUE +0.
           05  MQS-HOBJECT-RESPONSE-Q  PIC S9(9)  BINARY  VALUE +0.
           05  MQS-HOBJECT-LOG-01-Q    PIC S9(9)  BINARY  VALUE +0.
           05  MQS-HOBJECT-REMOTE-Q    PIC S9(9)  BINARY  VALUE +0.
           05  MQS-HOBJECT-TRANSMIT-Q  PIC S9(9)  BINARY  VALUE +0.
           05  MQS-HOBJECT-DYNAMIC-Q   PIC S9(9)  BINARY  VALUE +0.
           05  MQS-HOBJECT-REPORT-Q    PIC S9(9)  BINARY  VALUE +0.
           05  MQS-HOBJECT-TRANSACTION-Q
                                       PIC S9(9)  BINARY  VALUE +0.
           05  MQS-HOBJECT-CREDIT-AUTH-RESP-Q
                                       PIC S9(9)  BINARY  VALUE +0.
           05  MQS-HOBJECT-BUREAU-REQ-Q
                                       PIC S9(9)  BINARY  VALUE +0.
           05  MQS-HOBJECT-BUREAU-RESP-Q
                                       PIC S9(9)  BINARY  VALUE +0.

           05  MQS-OPTIONS             PIC S9(9)  BINARY  VALUE +0.
           05  MQS-OBJECTTYPE          PIC S9(9)  BINARY  VALUE +0.
           05  MQS-BUFFERLENGTH        PIC S9(9)  BINARY  VALUE +0.
           05  MQS-DATALENGTH          PIC S9(9)  BINARY  VALUE +0.
           05  MQS-COMPCODE            PIC S9(9)  BINARY  VALUE +0.
           05  MQS-REASONCODE          PIC S9(9)  BINARY  VALUE +0.
           05  MQS-QMANAGER-NAME       PIC X(48)          VALUE 'MMQM'.
           05  MQS-OBJECTNAME          PIC X(48)          VALUE SPACES.
           05  MQS-MSGID               PIC X(24)          VALUE SPACES.
           05  MQS-OBJECTTYPE-DESC     PIC X(15)          VALUE SPACES.

           05  MQS-CUSTOMER-QUEUE-ORIG.
               10 MQS-USERID           PIC X(08)          VALUE SPACES.
               10 FILLER               PIC X(40)          VALUE
               '.BATCH.CUSTOMER.QUEUE'.
           05  MQS-CUSTOMER-QUEUE-R    REDEFINES
                                       MQS-CUSTOMER-QUEUE-ORIG.
               10 MQS-A-BYTE-01        PIC X(01)
                                       OCCURS 48 TIMES.

           05  MQS-CUSTOMER-QUEUE-COMPRESS
                                       PIC X(48)          VALUE SPACES.
           05  MQS-CUSTOMER-QUEUE-COMPRESS-R
                                       REDEFINES
                                       MQS-CUSTOMER-QUEUE-COMPRESS.
               10 MQS-A-BYTE-02        PIC X(01)
                                       OCCURS 48 TIMES.


           05  MQS-CUSTOMER-RESPONSE-QUEUE
                                       PIC X(48)          VALUE
               'PDAPROD.BATCH.CUSTOMER.RESPONSE.QUEUE'.
           05  MQS-CUSTOMER-RESPONSE-QALIAS
                                       PIC X(48)          VALUE
               'PDAPROD.BATCH.CUSTOMER.RESPONSE.QUEUE.ALIAS'.

           05  MQS-MODEL-TEMP-DYNAMIC-QUEUE
                                       PIC X(48)          VALUE
               'PDAPROD.H01AC013.MODEL.TEMP.DYNAMIC'.

           05  MQS-TEMP-DYNAMIC-QUEUE-PREFIX
                                       PIC X(48)          VALUE
               'PDAPROD.H01AC013.*'.

           05  MQS-REPORT-TEMP-DYNAMIC-PREFIX
                                       PIC X(48)          VALUE
               'PDAPROD.H01AC013.REPORT.*'.

           05  MQS-REPORT-TEMP-DYNAMIC-QUEUE
                                       PIC X(48)          VALUE SPACES.

           05  MQS-TRANS-LOG-QUEUE-01  PIC X(48)          VALUE
               'PDAPROD.H01AC013.TRANS.LOG.QUEUE.01'.

           05  MQS-TRANS-LOG-QUEUE-99  PIC X(48)          VALUE
               'PDAPROD.H01AC013.TRANS.LOG.QUEUE.99'.

           05  MQS-XMIT-CW01-TO-CW09-QUEUE
                                       PIC X(48)          VALUE
               'PDAPROD.H01AC013.XMITQ.CW01.TO.CW09'.

           05  MQS-REMOTE-CW01-TO-CW09-QUEUE
                                       PIC X(48)          VALUE
               'PDAPROD.H01AC013.QREMOTE.CW01.TO.CW09'.

           05  MQS-TRANSACTION-QUEUE   PIC X(48)          VALUE
               'PDAPROD.H01AC013.TRANSACTION.QUEUE'.

           05  MQS-BUREAU-REQ-QUEUE
                                       PIC X(48)          VALUE
               'PDAPROD.QREMOTE.CW01.TO.CW09.CREDIT.AUTH'.

           05  MQS-BUREAU-RESP-QUEUE
                                       PIC X(48)          VALUE
               'PDAPROD.QLOCAL.CW09.TO.CW01.CREDIT.AUTH'.

           05  MQS-CREDIT-AUTH-RESP-QUEUE
                                       PIC X(48)          VALUE
               'PDAPROD.H01AC013.CREDIT.AUTH.RESP.QUEUE'.


      *****************************************************************
      *    MQSERIES INQUIRE FUNCTION WORK AREAS (MQINQ)               *
      *****************************************************************

       01  MQS-MQINQ-AREA.
           05  MQS-SELECTOR-COUNT      PIC S9(9)  BINARY  VALUE +0.
           05  MQS-INTATTR-COUNT       PIC S9(9)  BINARY  VALUE +0.
           05  MQS-CHARATTR-LENGTH     PIC S9(9)  BINARY  VALUE +0.

           05  MQS-SELECTOR-TABLE.
               10  MQS-SELECTORS       PIC S9(9)  BINARY
                                       OCCURS 1 TIMES.

           05  MQS-INTATTR-TABLE.
               10  MQS-INTATTRS        PIC S9(9)  BINARY
                                       OCCURS 1 TIMES.

           05  MQS-CHARATTRS           PIC X(01)          VALUE SPACES.


      *****************************************************************
      *    MQSERIES GENERAL INPUT / OUTPUT BUFFER USED FOR MESSAGES   *
      *****************************************************************

       01  MQS-BUFFER                  PIC X(1000)        VALUE SPACES.

       01  MQS-BUFFER-SHORT            PIC X(50).


      *****************************************************************
      *    CUSTOMER ORDER QUERY REQUEST MESSAGE                       *
      *****************************************************************

       01  MQS-CUSTOMER-MESSAGE.
           05  MQS-CUSTOMER-USERID     PIC X(08).
           05  MQS-CUSTOMER-ID         PIC X(32).
           05  MQS-CUSTOMER-ORDER-FEE  PIC 9(7)V99.
           05  MQS-CUSTOMER-ORDER-FEE-R
                                       REDEFINES MQS-CUSTOMER-ORDER-FEE
                                       PIC X(09).
           05  MQS-CUSTOMER-SCENARIOS  PIC X(250).
           05  FILLER                  PIC X(01).


      *****************************************************************
      *    MQSERIES MESSAGE SENT TO THE RESULTS QUEUE                 *
      *    (CUSTOMER ORDER QUERY INFORMATION)                         *
      *****************************************************************

       01  MQS-RESULTS-MESSAGE.
           05  MQS-RETURN-CODE         PIC 9(01).
               88  MQS-NO-ERROR                           VALUE 0.
               88  MQS-ERROR                              VALUE 1.
               88  MQS-FATAL-ERROR                        VALUE 9.
           05  MQS-TOTAL-ORDERS        PIC 9(05).
           05  MQS-TOTAL-DOLLAR-AMOUNT
                                       PIC 9(09)V99.
           05  MQS-AVG-DOLLAR-AMOUNT
                                       PIC 9(09)V99.
           05  MQS-LAST-ORDER-DATE     PIC X(06).
           05  MQS-LAST-ORDER-AMOUNT
                                       PIC 9(07)V99.
           05  MQS-LAST-ORDER-NUMBER
                                       PIC X(10).
           05  MQS-ORDER-DETAIL        OCCURS 14 TIMES.
               10  MQS-ORDER-NUMBER
                                       PIC X(10).
               10  MQS-ORDER-AMOUNT
                                       PIC 9(07)V99.
           05  MQS-SCREEN-MESSAGE      PIC X(79).
           05  MQS-ERROR-INFORMATION.
               10  MQS-PDA-ERROR-TYPE  PIC X(04).
               10  MQS-PDA-ERROR-LINE-01
                                       PIC X(78).
               10  MQS-PDA-ERROR-LINE-02
                                       PIC X(78).
           05  FILLER                  PIC X(442).


      *****************************************************************
      *    CUSTOMER ORDER PAYMENT MESSAGE                             *
      *****************************************************************

       01  MQS-CUSTOMER-PAYMENT-MESSAGE.
           05  MQS-CUSTOMER-PAYMENT-ID PIC X(32)       VALUE SPACES.
           05  MQS-CUSTOMER-PAYMENT-AMT
                                       PIC 9(3)V99     VALUE ZEROES.
           05  MQS-CUSTOMER-PAYMENT-DESC
                                       PIC X(50)       VALUE SPACES.

      *****
      ** FIXED CUSTOMER MESSAGES TO POPULATE TRANSACTION QUEUES
      *****
       01  MQS-CUSTOMER-PAYMENT-TABLE.
           05  MQS-PAYMENTS.
               10  FILLER              PIC X(32)       VALUE
                   'DAMON HARDWARE '.
               10  FILLER              PIC 9(3)V99     VALUE 325.87.
               10  FILLER              PIC X(50)       VALUE
                   'CHECK # 10034687 CUSTOMER PAYMENT TRANSACTION'.
               10  FILLER              PIC X(32)       VALUE
                   'POWERS '.
               10  FILLER              PIC 9(3)V99     VALUE 198.29.
               10  FILLER              PIC X(50)       VALUE
                   'VISA CUSTOMER PAYMENT TRANSACTION'.
               10  FILLER              PIC X(32)       VALUE
                   'SPIRALOCK '.
               10  FILLER              PIC 9(3)V99     VALUE 763.01.
               10  FILLER              PIC X(50)       VALUE
                   'CASH CUSTOMER PAYMENT TRANSACTION'.

           05  MQS-PAYMENTS-R          REDEFINES MQS-PAYMENTS
                                       PIC X(87)
                                       OCCURS 3 TIMES.


      *****************************************************************
      *    CUSTOMER CREDIT AUTHORIZATION REQUEST / RESPONSE MESSAGE   *
      *****************************************************************

       01  MQS-CREDIT-AUTH-REQ-MESSAGE.
           05  MQS-CREDIT-RETURN-CODE  PIC 9(01).
               88  MQS-CREDIT-NO-ERROR                    VALUE 0.
               88  MQS-CREDIT-ERROR                       VALUE 1.
               88  MQS-CREDIT-FATAL-ERROR                 VALUE 9.
           05  MQS-CREDIT-CUSTOMER-ID  PIC X(32).
           05  MQS-CREDIT-BUREAU       PIC X(15).
           05  MQS-CREDIT-AUTH         PIC X(01).
               88  MQS-CREDIT-APPROVED                    VALUE 'A'.
               88  MQS-CREDIT-REJECTED                    VALUE 'R'.
               88  MQS-CREDIT-UNAVAILABLE                 VALUE 'U'.
           05  MQS-CREDIT-SCREEN-MESSAGE
                                       PIC X(79).
           05  MQS-CREDIT-ERROR-INFORMATION.
               10  MQS-CREDIT-ERROR-TYPE
                                       PIC X(04).
               10  MQS-CREDIT-ERROR-LINE-01
                                       PIC X(78).
               10  MQS-CREDIT-ERROR-LINE-02
                                       PIC X(78).
           05  FILLER                  PIC X(12).
           EJECT

      *****************************************************************
      *    MQSERIES DEFINITIONS                                       *
      *****************************************************************

       01  MQS-OBJECT-DESCRIPTOR.
           COPY CMQODV.
           EJECT

       01  MQS-MESSAGE-DESCRIPTOR.
           COPY CMQMD2V.
           EJECT

       01  MQS-PUT-MESSAGE-OPTIONS.
           COPY CMQPMOV.
           EJECT

       01  MQS-GET-MESSAGE-OPTIONS.
           COPY CMQGMOV.
           EJECT

       01  MQS-CONSTANTS.
           COPY CMQV.
           EJECT

      *****************************************************************
      *    GENERAL ERROR PROCESSING WORK AREAS                        *
      *****************************************************************
      ******************************************************************
      * PRODUCT DEMONSTRATION APPLICATION (PDA)                        *
      *                                                                *
      * ERROR WORK AREA DEFINITIONS FOR: GEN, IMS-DLI, DB2, MQSERIES   *
      *                                                                *
      ******************************************************************

       01  WS-PDA-ERROR-GENERAL.

           05  WS-PDA-ERROR-TYPE       PIC X(04)       VALUE SPACES.
               88  PDA-GENERAL-ERROR                   VALUE 'GEN'.
               88  PDA-DB2-ERROR                       VALUE 'DB2'.
               88  PDA-IMS-ERROR                       VALUE 'IMS'.
               88  PDA-MQSERIES-ERROR                  VALUE 'MQS'.


      ******************************************************************
      *    PDA FORMATTED ERROR LINES                                   *
      ******************************************************************

       01  WS-PDA-ERROR-AREA.
           05  WPEA-ERROR-01           PIC X(80)       VALUE ALL '*'.
           05  WPEA-ERROR-02.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-03.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE
               '   PRODUCT DEMONSTRATION APPLICATION (PDA) ERROR '.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-04.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-05           PIC X(80)       VALUE ALL '*'.
           05  WPEA-ERROR-06.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-07.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 WPEA-ERROR-07-TEXT   PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-08.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 WPEA-ERROR-08-TEXT   PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-09.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-10           PIC X(80)       VALUE ALL '*'.


      ******************************************************************
      *    PDA GENERAL ERROR LINES                                     *
      ******************************************************************

       01  WS-PDA-GEN-ERROR-01.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(07)       VALUE
               'ERROR: '.
           05  FILLER                  PIC X(10)       VALUE
               'PROGRAM = '.
           05  WPGE-PROGRAM-ID         PIC X(08)       VALUE SPACES.
           05  FILLER                  PIC X(14)       VALUE
               ', PARAGRAPH = '.
           05  WPGE-PARAGRAPH          PIC X(06).
           05  FILLER                  PIC X(32)       VALUE SPACES.

       01  WS-PDA-GEN-ERROR-02.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  WPGE-DESCRIPTION        PIC X(78)       VALUE SPACES.


      ******************************************************************
      *    PDA IMS-DLI ERROR LINES                                     *
      ******************************************************************

       01  WS-PDA-IMS-ERROR-01.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(15)       VALUE
               'IMS-DLI ERROR: '.
           05  FILLER                  PIC X(08)       VALUE
               'PROGRAM='.
           05  WPIE-PROGRAM-ID         PIC X(08)       VALUE SPACES.
           05  FILLER                  PIC X(12)       VALUE
               ', PARAGRAPH='.
           05  WPIE-PARAGRAPH          PIC X(06)       VALUE SPACES.
           05  FILLER                  PIC X(09)       VALUE
               ', STATUS='.
           05  WPIE-STATUS-CODE        PIC X(2)        VALUE SPACES.
           05  FILLER                  PIC X(12)       VALUE
               ', FUNCTION='.
           05  WPIE-FUNCTION-CODE      PIC X(4)        VALUE SPACES.

       01  WS-PDA-IMS-ERROR-02.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(08)       VALUE
               'SEGMENT='.
           05  WPIE-SEGMENT-NAME       PIC X(8)        VALUE SPACES.
           05  FILLER                  PIC X(11)       VALUE
               ', DATABASE='.
           05  WPIE-DATABASE-NAME      PIC X(8)        VALUE SPACES.
           05  FILLER                  PIC X(10)       VALUE
               ', COMMAND='.
           05  WPIE-COMMAND            PIC X(32)       VALUE SPACES.


      ******************************************************************
      *    PDA DB2 ERROR LINES                                         *
      ******************************************************************

       01  WS-PDA-DB2-ERROR-01.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(11)       VALUE
               'DB2 ERROR: '.
           05  FILLER                  PIC X(10)       VALUE
               'PROGRAM = '.
           05  WPDE-PROGRAM-ID         PIC X(08)       VALUE SPACES.
           05  FILLER                  PIC X(12)       VALUE
               ', SQLCODE = '.
           05  WPDE-DB2-SQLCODE        PIC ZZZZZZ9-.
           05  FILLER                  PIC X(28)       VALUE SPACES.

       01  WS-PDA-DB2-ERROR-02.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(11)       VALUE
               'FUNCTION = '.
           05  WPDE-FUNCTION           PIC X(30)       VALUE SPACES.
           05  WPDE-FUNCTION-R         REDEFINES WPDE-FUNCTION.
               10  WPDE-FUNCTION-1     PIC X(15).
               10  WPDE-FUNCTION-2     PIC X(15).
           05  FILLER                  PIC X(14)       VALUE
               ', PARAGRAPH = '.
           05  WPDE-PARAGRAPH          PIC X(06)       VALUE SPACES.
           05  FILLER                  PIC X(16)       VALUE SPACES.


      ******************************************************************
      *    PDA MQSERIES ERROR LINES                                    *
      ******************************************************************

       01  WS-PDA-MQSERIES-ERROR-01.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(16)       VALUE
               'MQSERIES ERROR: '.
           05  FILLER                  PIC X(10)       VALUE
               'PROGRAM = '.
           05  WPME-PROGRAM-ID         PIC X(08)       VALUE SPACES.
           05  FILLER                  PIC X(16)       VALUE
               ', REASON CODE = '.
           05  WPME-REASON-CODE        PIC ZZZZZZZZ9.
           05  FILLER                  PIC X(18)       VALUE SPACES.

       01  WS-PDA-MQSERIES-ERROR-02.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(11)       VALUE
               'FUNCTION = '.
           05  WPME-FUNCTION           PIC X(30)       VALUE SPACES.
           05  WPME-FUNCTION-R         REDEFINES WPME-FUNCTION.
               10  WPME-FUNCTION-1     PIC X(15).
               10  WPME-FUNCTION-2     PIC X(15).
           05  FILLER                  PIC X(14)       VALUE
               ', PARAGRAPH = '.
           05  WPME-PARAGRAPH          PIC X(06)       VALUE SPACES.
           05  FILLER                  PIC X(17)       VALUE SPACES.

      *****************************************************************
      *    MESSAGES   (ERROR AND INFORMATIONAL)                       *
      *****************************************************************

       01  WS-PDAB17-MESSAGES.

           05  WPM-BLANK               PIC X(01)       VALUE     ' '.
           05  WPM-ALL-ASTERISK        PIC X(80)       VALUE ALL '*'.

           05  WPM-BEGIN-PROGRAM.
               10 FILLER               PIC X(78)   VALUE
                  '***** BEGIN PROGRAM PDAB17 *****'.

           05  WPM-END-PROGRAM.
               10 FILLER               PIC X(78)   VALUE
                  '***** END PROGRAM PDAB17 *****'.

           05  WPM-PROGRAM-ERROR.
               10 FILLER               PIC X(29)   VALUE
                  'ERROR RETURNED FROM PROGRAM: '.
               10 WPM-PROGRAM-NAME     PIC X(09)   VALUE SPACES.
               10 FILLER               PIC X(15)   VALUE
                  ',RETURN CODE = '.
               10 WPM-RETURN-CODE      PIC X(10)   VALUE SPACES.
               10 FILLER               PIC X(15)   VALUE SPACES.

           05  WPM-NO-MSG-AVAILABLE.
               10 FILLER               PIC X(78)   VALUE
                  'INQUIRY FAILED, NO RESPONSE FROM QUERY, PLEASE RE-SUB
      -           'MIT OR CONTACT SYSTEMS'.

           05  WPM-PARAMETER-FILE-EMPTY.
               10 FILLER               PIC X(78)   VALUE
                  'INPUT PARAMETER FILE (IPARAMS) IS EMPTY - PARAMETERS
      -           'ARE REQUIRED'.

           05  WPM-MAX-PARAMETERS-EXCEEDED.
               10 FILLER               PIC X(48)   VALUE
                  'MAX NUMBER OF INPUT PARAMETER RECORDS EXCEEDED, '.
               10 FILLER               PIC X(14)   VALUE
                  'MAX ALLOWED = '.
               10 WPM-MAX-PARAMETERS   PIC ZZZZ9.
               10 FILLER               PIC X(11)   VALUE SPACES.

           05  WPM-PARM-INVALID-RECORD-TYPE.
               10 FILLER               PIC X(78)   VALUE
                  'POSITION 1 - RECORD TYPE MUST BE U'.

           05  WPM-RECORD-NUMBER-MSG.
               10 FILLER               PIC X(16)   VALUE
                  'RECORD NUMBER = '.
               10 WPM-RECORD-NUMBER    PIC 9(05)   VALUE ZEROES.
               10 FILLER               PIC X(59)   VALUE SPACES.

           05  WPM-USERID-PARM-REQUIRED.
               10 FILLER               PIC X(78)   VALUE
                  'USER ID INPUT PARAMETER RECORD IS REQUIRED '.

           05  WPM-USERID-PARM-TOO-MANY.
               10 FILLER               PIC X(78)   VALUE
                  'ONLY 1 USER ID INPUT PARAMETER RECORD IS ALLOWED '.

           05  WPM-INVALID-USERID.
               10 FILLER               PIC X(78)   VALUE
                  'POSITION 3 - 10, USER ID IS REQUIRED '.

           05  WPM-BEGIN-REQUEST.
               10 FILLER               PIC X(78)   VALUE
                  '***** BEGIN NEW QUERY REQUEST *****'.

           05  WPM-END-REQUEST.
               10 FILLER               PIC X(78)   VALUE
                  '***** END QUERY REQUEST *****'.

           05  WPM-DATE-TIME-MSG.
               10 FILLER               PIC X(06)   VALUE '***** '.
               10 FILLER               PIC X(06)   VALUE 'DATE: '.
               10 WPM-DATE-TIME-MONTH  PIC X(02)   VALUE SPACES.
               10 FILLER               PIC X(01)   VALUE '-'.
               10 WPM-DATE-TIME-DAY    PIC X(02)   VALUE SPACES.
               10 FILLER               PIC X(01)   VALUE '-'.
               10 WPM-DATE-TIME-YEAR   PIC X(04)   VALUE SPACES.
               10 FILLER               PIC X(05)   VALUE SPACES.
               10 FILLER               PIC X(06)   VALUE 'TIME: '.
               10 WPM-DATE-TIME-HH     PIC X(02)   VALUE SPACES.
               10 FILLER               PIC X(01)   VALUE ':'.
               10 WPM-DATE-TIME-MM     PIC X(02)   VALUE SPACES.
               10 FILLER               PIC X(01)   VALUE ':'.
               10 WPM-DATE-TIME-SS     PIC X(02)   VALUE SPACES.
               10 FILLER               PIC X(37)   VALUE SPACES.
           EJECT

      *****************************************************************
      *    L I N K A G E     S E C T I O N                            *
      *****************************************************************

       LINKAGE SECTION.
                                                                        03780000
      ****************************************************************  03790000
      *****  I-O PCB                                                    03800000
      ****************************************************************  03810000
                                                                        03820000
       01  IO-PCB.                                                      03830000
           05  FILLER                  PIC X(10) VALUE SPACES.          03840000
           05  IO-STATUS               PIC XX    VALUE SPACES.          03850000
           05  FILLER                  PIC X(20) VALUE SPACES.          03860000


           COPY PCBORDER.
           EJECT

      *****************************************************************
      *    P R O C E D U R E    D I V I S I O N                       *
      *****************************************************************

       PROCEDURE DIVISION.
           ENTRY 'DLITCBL'  USING  IO-PCB
                                   ORDER-PCB.

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00000-MAINLINE                                *
      *                                                               *
      *    FUNCTION :  PROGRAM ENTRY, CONTROL HIGH LEVEL PROCESSING   *
      *                FOR THE PRODUCT DEMONSTRATION APPLICATION      *
      *                CUSTOMER ORDER PROCESSING PROGRAM              *
      *                                                               *
      *    CALLED BY:  NONE                                           *
      *                                                               *
      *****************************************************************

       P00000-MAINLINE.
           DISPLAY 'P00000-MAINLINE'.

           DISPLAY WPM-BLANK.
           DISPLAY WPM-ALL-ASTERISK.
           DISPLAY WPM-BEGIN-PROGRAM.
           DISPLAY WPM-ALL-ASTERISK.


           PERFORM  P00050-INITIALIZE                                   TAGGED
               THRU P00050-INITIALIZE-EXIT.                             CODE
                                                                        TESTING
                                                                        03/13/01
           IF NO-ERROR-FOUND
               PERFORM  P00500-MAIN-PROCESS
                   THRU P00500-MAIN-PROCESS-EXIT.
                                                                        TESTING
                                                                        03/13/01
           PERFORM  P00300-END-OF-JOB
               THRU P00300-END-OF-JOB-EXIT.


           DISPLAY WPM-BLANK.
           DISPLAY WPM-ALL-ASTERISK.
           DISPLAY WPM-END-PROGRAM.
           DISPLAY WPM-ALL-ASTERISK.

           GOBACK.

           DISPLAY 'P00000-MAINLINE-EXIT'.
       P00000-MAINLINE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00050-INITIALIZE                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INITIALIZE RELEVANT WORK FIELDS     *
      *                AND VARIABLES, PERFORM ONE TIME TASKS          *
      *                                                               *
      *    CALLED BY:  P00000-MAINLINE                                *
      *                                                               *
      *****************************************************************

       P00050-INITIALIZE.
           DISPLAY 'P00050-INITIALIZE'.

      *****************************************************************
      *    INITIALIZE SWITCHES, SUBSCRIPTS, ETC.                      *
      *****************************************************************

           MOVE ZEROES                 TO WS-ERROR-FOUND-SW.

           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW
                                          WS-END-OF-PARM-FILE-SW
                                          WS-PARM-ERROR-FOUND-SW.

           MOVE ZEROES                 TO WS-SKIP-ERROR-CHECK-SW
                                          WS-ERROR-IS-FORMATTED-SW.

           MOVE 'Y'                    TO WS-MORE-MESSAGES-SW.
           MOVE SPACES                 TO WS-PDA-ERROR-TYPE.
                                                                        00010000
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME.          00020001

      *****************************************************************
      *    INITIALIZE THE QUERY RESULTS MESSAGE I/O AREA TO DEFAULTS  *
      *****************************************************************

           PERFORM P00060-INIT-MQS-MESSAGE
              THRU P00060-INIT-MQS-MESSAGE-EXIT.


      *****************************************************************
      *    CONNECT TO THE MQSERIES QUEUE MANAGER                      *
      *****************************************************************

           PERFORM  P07000-MQS-CONNECT
               THRU P07000-MQS-CONNECT-EXIT.

           IF ERROR-FOUND
               GO TO P00050-INITIALIZE-EXIT.

      *****************************************************************
      *    OPEN THE MQSERIES QUEUES TO BE UTILIZED                    *
      *****************************************************************

           PERFORM P00100-OPEN-MAIN-QUEUES
              THRU P00100-OPEN-MAIN-QUEUES-EXIT.


      *****************************************************************
      *    OPEN PARAMETER FILE, DO 1ST READ, EOF IS AN ERROR          *
      *****************************************************************

           OPEN INPUT    INPUT-PARAMETERS.                              00020001

           PERFORM  P80000-READ-PARAMETERS
               THRU P80000-READ-PARAMETERS-EXIT.

           IF END-OF-PARM-FILE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB17'           TO WPGE-PROGRAM-ID
               MOVE 'P00050'           TO WPGE-PARAGRAPH
               MOVE WPM-PARAMETER-FILE-EMPTY
                                       TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


           DISPLAY 'P00050-INITIALIZE-EXIT'.
       P00050-INITIALIZE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00060-INIT-MQS-MESSAGE                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INITIALIZE THE MQSERIES OUTPUT      *
      *                MESSAGE AREA TO DEFAULT VALUES                 *
      *                                                               *
      *    CALLED BY:  P00050-INITIALIZE                              *
      *                                                               *
      *****************************************************************

       P00060-INIT-MQS-MESSAGE.
           DISPLAY 'P00060-INIT-MQS-MESSAGE'.

           MOVE SPACES                 TO MQS-RESULTS-MESSAGE.
           MOVE ZEROES                 TO MQS-RETURN-CODE
                                          MQS-TOTAL-ORDERS
                                          MQS-TOTAL-DOLLAR-AMOUNT
                                          MQS-AVG-DOLLAR-AMOUNT
                                          MQS-LAST-ORDER-AMOUNT.

           PERFORM P00065-INIT-MQS-ORDERS
              THRU P00065-INIT-MQS-ORDERS-EXIT
                  VARYING WS-SUB1 FROM +1 BY +1
                      UNTIL WS-SUB1 > WS-ORDER-MAX.

           DISPLAY 'P00060-INIT-MQS-MESSAGE-EXIT'.
       P00060-INIT-MQS-MESSAGE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00065-INIT-MQS-ORDERS                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INITIALIZE THE MQSERIES OUTPUT      *
      *                MESSAGE ORDER DETAIL AREA                      *
      *                                                               *
      *    CALLED BY:  P00060-INIT-MQS-MESSAGE                        *
      *                                                               *
      *****************************************************************

       P00065-INIT-MQS-ORDERS.

           MOVE SPACES                 TO MQS-ORDER-NUMBER (WS-SUB1).
           MOVE ZEROES                 TO MQS-ORDER-AMOUNT (WS-SUB1).

       P00065-INIT-MQS-ORDERS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00100-OPEN-MAIN-QUEUES                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO OPEN THE PRIMARY  MQSERIES QUEUES   *
      *                TO BE USED BY THE PROGRAM                      *
      *                                                               *
      *    CALLED BY:  P00050-INITIALIZE                              *
      *                                                               *
      *****************************************************************

       P00100-OPEN-MAIN-QUEUES.
           DISPLAY 'P00100-OPEN-MAIN-QUEUES'.

      *****************************************************************
      *    INITIALIZE MQSERIES PARAMETERS AND VARIABLES               *
      *****************************************************************

           MOVE ZEROES                 TO MQS-HOBJECT
                                          MQS-HOBJECT-REQUEST-Q
                                          MQS-HOBJECT-RESPONSE-Q
                                          MQS-COMPCODE
                                          MQS-REASONCODE.

           MOVE MQOD-CURRENT-VERSION   TO MQOD-VERSION.
           MOVE MQOT-Q                 TO MQOD-OBJECTTYPE.
           MOVE 'QUEUE'                TO MQS-OBJECTTYPE-DESC.


      *****************************************************************
      *    OPEN THE CUSTOMER RESPONSE QUEUE - MAIN OUTPUT OF TRANS    *
      *    (THE OPEN IS ISSUED FOR AN ALIAS OF THE QUEUE)             *
      *****************************************************************

           MOVE MQS-CUSTOMER-RESPONSE-QALIAS
                                       TO MQOD-OBJECTNAME.
           COMPUTE MQS-OPTIONS         =  MQOO-OUTPUT            +
                                          MQOO-PASS-ALL-CONTEXT  +
                                          MQOO-FAIL-IF-QUIESCING.

           PERFORM P07100-MQS-OPEN
              THRU P07100-MQS-OPEN-EXIT.

           MOVE MQS-HOBJECT            TO MQS-HOBJECT-RESPONSE-Q.

           DISPLAY 'P00100-OPEN-MAIN-QUEUES-EXIT'.
       P00100-OPEN-MAIN-QUEUES-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00300-END-OF-JOB                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PERFORM NORMAL END OF PROGRAM       *
      *                OPERATIONS, I.E. CLOSE FILES, ETC.             *
      *                                                               *
      *    CALLED BY:  P00000-MAINLINE                                *
      *                                                               *
      *****************************************************************

       P00300-END-OF-JOB.
           DISPLAY 'P00300-END-OF-JOB'.

      *****************************************************************
      *    DISCONNECT FROM THE MQSERIES QUEUE MANAGER                 *
      *****************************************************************

           IF MQS-HCONN                > ZEROES
               PERFORM  P07030-MQS-DISCONNECT
                   THRU P07030-MQS-DISCONNECT-EXIT.

           DISPLAY 'P00300-END-OF-JOB-EXIT'.
       P00300-END-OF-JOB-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00500-MAIN-PROCESS                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL PDAB17 HIGH LEVEL PROCESSES *
      *                                                               *
      *    CALLED BY:  P00000-MAINLINE                                *
      *                                                               *
      *****************************************************************

       P00500-MAIN-PROCESS.
           DISPLAY 'P00500-MAIN-PROCESS'.

      *****************************************************************
      *    PERFORM INPUT PARAMETER PROCESS -- IF ERROR FOUND, EXIT    *
      *****************************************************************

           PERFORM  P00600-PARAMETER-PROCESS                            TAGGED
               THRU P00600-PARAMETER-PROCESS-EXIT.                      CODE
                                                                        TESTING
           IF ERROR-FOUND                                               TESTING
               GO TO P00500-MAIN-PROCESS-EXIT.
                                                                        TESTING
                                                                        TESTING
      *****************************************************************
      *    OPEN ORDER QUERY REQUEST QUEUE                             *
      *****************************************************************

           PERFORM  P01000-OPEN-REQUEST-Q                               TAGGED
               THRU P01000-OPEN-REQUEST-Q-EXIT.                         CODE

           IF ERROR-FOUND                                               TESTING
               GO TO P00500-MAIN-PROCESS-EXIT.


      *****************************************************************
      *    INITIATE THE MQSERIES CUSTOMER ORDER QUERY                 *
      *    (REPETITIVE LOOP, UNLIMITED WAIT FOR INCOMING MESSAGE ON Q)*
      *****************************************************************

           PERFORM  P02000-ORDER-QUERY
               THRU P02000-ORDER-QUERY-EXIT
                   UNTIL NO-MORE-MESSAGES.

           DISPLAY 'P00500-MAIN-PROCESS-EXIT'.
       P00500-MAIN-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00600-PARAMETER-PROCESS                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ THE PARAMETER INPUT RECORDS,   *
      *                STORE PARAMETERS IN AN ARRAY, EDIT THE         *
      *                PARAMETER CONTENT                              *
      *                                                               *
      *    CALLED BY:  P00500-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P00600-PARAMETER-PROCESS.
           DISPLAY 'P00600-PARAMETER-PROCESS'.

      *****************************************************************
      *    PROCESS PARAMETERS UNTIL END OF FILE                       *
      *****************************************************************

           MOVE ZEROES                 TO WS-SUB1
                                          WS-USERID-PARM-COUNT.

           PERFORM  P00630-LOAD-PARM-ARRAY                              TAGGED
               THRU P00630-LOAD-PARM-ARRAY-EXIT                         CODE
                   UNTIL END-OF-PARM-FILE.                              TESTING

           CLOSE  INPUT-PARAMETERS.                                     00020001
                                                                        TESTING
           IF ERROR-FOUND                                               TESTING
               GO TO P00600-PARAMETER-PROCESS-EXIT.

      *****************************************************************
      *    PERFORM PARAMETER RECORD EDITS                             *
      *****************************************************************

           PERFORM  P00660-EDIT-PARMS
               THRU P00660-EDIT-PARMS-EXIT
                   VARYING WS-SUB1 FROM +1 BY +1
                       UNTIL WS-SUB1 > WS-PARAMETER-RECORDS-IN.
                                                                        TESTING
           IF ERROR-FOUND                                               TESTING
               GO TO P00600-PARAMETER-PROCESS-EXIT.


      *****************************************************************
      *    IF NO USER ID SPECIFICATION RECORD, ERROR - TERMINATE      *
      *****************************************************************

           IF WS-USERID-PARM-COUNT     > ZEROES                         00020001
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB17'           TO WPGE-PROGRAM-ID
               MOVE 'P00600'           TO WPGE-PARAGRAPH
               MOVE WPM-USERID-PARM-REQUIRED
                                       TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

           DISPLAY 'P00600-PARAMETER-PROCESS-EXIT'.
       P00600-PARAMETER-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00630-LOAD-PARM-ARRAY                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ PARAMETER RECORDS AND STORE THE*
      *                PARAMETERS IN AN ARRAY FOR LATER PROCESSING    *
      *                                                               *
      *    CALLED BY:  P00600-PARAMETER-PROCESS                       *
      *                                                               *
      *****************************************************************

       P00630-LOAD-PARM-ARRAY.
           DISPLAY 'P00630-LOAD-PARM-ARRAY'.

      *****************************************************************
      *    CHECK FOR MAXIMUM PARAMETER RECORDS ALLOWED                *
      *****************************************************************

           ADD +1                      TO WS-SUB1.

           IF WS-SUB1                  >  WS-MAX-PARAMETERS             00020001
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB17'           TO WPGE-PROGRAM-ID
               MOVE 'P00630'           TO WPGE-PARAGRAPH
               MOVE WS-MAX-PARAMETERS  TO WPM-MAX-PARAMETERS
               MOVE WPM-MAX-PARAMETERS-EXCEEDED
                                       TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

           MOVE WS-PARAMETER-RECORD    TO WPRA-RECORD (WS-SUB1).


      *****************************************************************
      *    READ NEXT PARAMETER RECORD                                 *
      *****************************************************************

           PERFORM  P80000-READ-PARAMETERS
               THRU P80000-READ-PARAMETERS-EXIT.

           DISPLAY 'P00630-LOAD-PARM-ARRAY-EXIT'.
       P00630-LOAD-PARM-ARRAY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00660-EDIT-PARMS                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO EDIT THE PARAMETER RECORD SYNTAX     *
      *                                                               *
      *    CALLED BY:  P00600-PARAMETER-PROCESS                       *
      *                                                               *
      *****************************************************************

       P00660-EDIT-PARMS.
           DISPLAY 'P00660-EDIT-PARMS'.

           MOVE 'N'                    TO WS-PARM-ERROR-FOUND-SW.
           MOVE WPRA-RECORD (WS-SUB1)  TO WS-PARAMETER-RECORD.

      *****************************************************************
      *    EDIT THE RECORD TYPE -  U = USERID SPECIFICATION           *
      *****************************************************************

           IF WPR-USERID
               NEXT SENTENCE
           ELSE                                                         00020001
               MOVE WPM-PARM-INVALID-RECORD-TYPE
                                       TO WMF-MESSAGE-AREA
               PERFORM  P00700-PARM-ERROR
                   THRU P00700-PARM-ERROR-EXIT.

      *****************************************************************
      *    FOR ACTION U= USER ID SPECIFICATION, ONLY 1 USER ID PARM   *
      *    RECORD IS ALLOWED, USERID MUST BE NON-BLANK                *
      *****************************************************************

           IF WPR-USERID
               ADD +1                  TO WS-USERID-PARM-COUNT
               IF  WS-USERID-PARM-COUNT > +1
                   MOVE WPM-USERID-PARM-TOO-MANY
                                       TO WMF-MESSAGE-AREA
                   PERFORM  P00700-PARM-ERROR
                       THRU P00700-PARM-ERROR-EXIT
               ELSE
               IF  WPR-USERID-VALUE     > SPACES
                   MOVE WPR-USERID-VALUE
                                       TO WMF-USERID
               ELSE
                   MOVE WPM-INVALID-USERID
                                       TO WMF-MESSAGE-AREA
                   PERFORM  P00700-PARM-ERROR
                       THRU P00700-PARM-ERROR-EXIT
           ELSE                                                         00020001
                   NEXT SENTENCE.


      *****************************************************************
      *    IF ERROR IN THIS PARM RECORD -- FINISH DISPLAY OF ERROR    *
      *****************************************************************

           IF PARM-ERROR-FOUND
               DISPLAY WPEA-ERROR-01
               DISPLAY ' '.

           DISPLAY 'P00660-EDIT-PARMS-EXIT'.
       P00660-EDIT-PARMS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00700-PARM-ERROR                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS / DISPLAY PARM RECORD ERRORS*
      *                                                               *
      *    CALLED BY:  P00660-EDIT-PARMS                              *
      *                                                               *
      *****************************************************************

       P00700-PARM-ERROR.

           MOVE 1                      TO WS-ERROR-FOUND-SW.

      *****************************************************************
      *    IF ERROR ALREADY ENCOUNTERED FOR THIS RECORD, JUST ADD THE *
      *    SINGLE LINE MESSAGE TO THE DISPLAY -- EXIT                 *
      *****************************************************************

           IF PARM-ERROR-FOUND
               DISPLAY WMF-MESSAGE-AREA
               GO TO P00700-PARM-ERROR-EXIT.

      *****************************************************************
      *    IF 1ST ERROR FOR THIS RECORD, DISPLAY THE ALL ASTERISK     *
      *    SINGLE LINE MESSAGE TO THE DISPLAY -- EXIT                 *
      *****************************************************************

           MOVE 'Y'                    TO WS-PARM-ERROR-FOUND-SW.
           DISPLAY ' '.
           DISPLAY WPEA-ERROR-01.
           MOVE WS-SUB1                TO WPM-RECORD-NUMBER.
           DISPLAY WPM-RECORD-NUMBER-MSG.
           DISPLAY 'PARAMETER RECORD IN ERROR FOLLOWS: '.
           DISPLAY WS-PARAMETER-RECORD.
           DISPLAY WMF-MESSAGE-AREA.

       P00700-PARM-ERROR-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01000-OPEN-REQUEST-Q                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO FORMAT THE REQUEST QUEUE NAME       *
      *                USING THE USERID SPECIFIED IN THE PARAMETER    *
      *                FILE                                           *
      *                                                               *
      *                AND                                            *
      *                                                               *
      *                OPEN THE USER SPECIFIC REQUEST QUEUE           *
      *                                                               *
      *    CALLED BY:  P00500-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P01000-OPEN-REQUEST-Q.
           DISPLAY 'P01000-OPEN-REQUEST-Q'.

      *****************************************************************
      *    QUERY REQUEST QUEUE NAME IS USERID SPECIFIC, PREFIXED BY   *
      *    USERID. FORMAT QUEUE NAME USING PARAMETER INPUT USERID.    *
      *****************************************************************

           MOVE WMF-USERID             TO MQS-USERID.
           MOVE SPACES                 TO MQS-CUSTOMER-QUEUE-COMPRESS.
           MOVE ZEROES                 TO WS-SUB2.

           PERFORM  P01030-FORMAT-Q-NAME
               THRU P01030-FORMAT-Q-NAME-EXIT
                   VARYING WS-SUB1 FROM +1 BY +1
                       UNTIL WS-SUB1 > WS-QUEUE-NAME-LTH.

           DISPLAY 'MQS-CUSTOMER-QUEUE-COMPRESS = '.
           DISPLAY  MQS-CUSTOMER-QUEUE-COMPRESS.


      *****************************************************************
      *    OPEN THE MQSERIES CUSTOMER QUEUE FOR INPUT                 *
      *****************************************************************
           MOVE MQOD-CURRENT-VERSION   TO MQOD-VERSION.

           MOVE MQOT-Q                 TO MQOD-OBJECTTYPE.
           MOVE 'QUEUE'                TO MQS-OBJECTTYPE-DESC.
           MOVE MQS-CUSTOMER-QUEUE-COMPRESS
                                       TO MQOD-OBJECTNAME.
           COMPUTE MQS-OPTIONS         =  MQOO-INPUT-SHARED     +
                                          MQOO-SAVE-ALL-CONTEXT +
                                          MQOO-FAIL-IF-QUIESCING.
           MOVE ZEROES                 TO MQS-HOBJECT
                                          MQS-COMPCODE
                                          MQS-REASONCODE.

           PERFORM P07100-MQS-OPEN
              THRU P07100-MQS-OPEN-EXIT.

           MOVE MQS-HOBJECT            TO MQS-HOBJECT-REQUEST-Q.


           DISPLAY 'P01000-OPEN-REQUEST-Q-EXIT'.
       P01000-OPEN-REQUEST-Q-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01030-FORMAT-Q-NAME                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO COMPRESS ANY BLANKS FROM THE        *
      *                CUSTOMER REQUEST QUEUE NAME. SITUATION OCCURS  *
      *                IF THE USERID PREFIX IS LESS THAN 8 CHARACTERS.*
      *                                                               *
      *    CALLED BY:  P01000-OPEN-REQUEST-Q                          *
      *                                                               *
      *****************************************************************

       P01030-FORMAT-Q-NAME.

      *****************************************************************
      *    MOVE ONLY NON-BLANK CHARACTERS FROM SOURCE TO TARGET       *
      *****************************************************************

           IF MQS-A-BYTE-01 (WS-SUB1)  NOT EQUAL TO SPACES
               ADD +1                  TO WS-SUB2
               MOVE MQS-A-BYTE-01 (WS-SUB1)
                                       TO MQS-A-BYTE-02 (WS-SUB2)
           ELSE
               NEXT SENTENCE.

       P01030-FORMAT-Q-NAME-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P02000-ORDER-QUERY                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE CUSTOMER ORDER INQUIRY  *
      *                PROCESS. READ CUSTOMER QUERY REQUEST QUEUE FOR *
      *                A REQUEST MESSAGE (THE GET EITHER OBTAINS      *
      *                MESSAGES ALREADY ON THE QUEUE OR WAITS         *
      *                INDEFINITELY FOR A MESSAGE TO ARRIVE).         *
      *                                                               *
      *                PROCESS QUERY, RETURN                          *
      *                RESPONSE ON QUERY RESPONSE / RESULTS QUEUE.    *
      *                                                               *
      *                PERFORM CREDIT CHECK OPERATIONS                *
      *                                                               *
      *    CALLED BY:  P00500-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P02000-ORDER-QUERY.
           DISPLAY 'P02000-ORDER-QUERY'.

      *****************************************************************
      *    PROCESS THE CUSTOMER ORDER REQUEST QUEUE                   *
      *****************************************************************

           PERFORM  P02100-ORDER-INQ-REQUEST
               THRU P02100-ORDER-INQ-REQUEST-EXIT.


      *****************************************************************
      *    PLACE RESULTS FROM THE ORDER INQUIRY REQUEST ON RESPONSE Q *
      *****************************************************************

           PERFORM  P05000-ORDER-INQ-RESPONSE
               THRU P05000-ORDER-INQ-RESPONSE-EXIT.

           IF ERROR-FOUND
               GO TO P02000-ORDER-QUERY-EXIT.


      *****************************************************************
      *    PROCESS CREDIT BUREAU AUTHORIZATION RESPONSE MESSAGES      *
      *****************************************************************

           PERFORM  P06000-CREDIT-BUREAU-RESP
               THRU P06000-CREDIT-BUREAU-RESP-EXIT.

           IF ERROR-FOUND
               GO TO P02000-ORDER-QUERY-EXIT.


      *****************************************************************
      *    WRITE THE CREDIT AUTHORIZATION RESPONSE MESSAGE TO BE      *
      *    PROCESSED BY PROGRAM PDAB16 (CUSTOMER ORDER QUERY REQUEST  *
      *    PROGRAM)                                                   *
      *****************************************************************

           PERFORM  P06200-SEND-CREDIT-AUTH
               THRU P06200-SEND-CREDIT-AUTH-EXIT.


      *****************************************************************
      *    CLOSE THE MQSERIES QUEUES                                  *
      *****************************************************************

           PERFORM  P06900-CLOSE-THE-QUEUES
               THRU P06900-CLOSE-THE-QUEUES-EXIT.

           DISPLAY WPM-BLANK.
           DISPLAY WPM-ALL-ASTERISK.
           DISPLAY WPM-END-REQUEST.
           DISPLAY WPM-DATE-TIME-MSG.
           DISPLAY WPM-ALL-ASTERISK.

           DISPLAY 'P02000-ORDER-QUERY-EXIT'.
       P02000-ORDER-QUERY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P02100-ORDER-INQ-REQUEST                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS INCOMING ORDER QUERY        *
      *                REQUESTS.                                      *
      *                                                               *
      *                FORMAT REQUEST Q NAME BASED ON USERID          *
      *                FROM THE PARAMETER INPUT RECORD                *
      *                                                               *
      *    CALLED BY:  P02000-ORDER-QUERY                             *
      *                                                               *
      *****************************************************************

       P02100-ORDER-INQ-REQUEST.
           DISPLAY 'P02100-ORDER-INQ-REQUEST'.

      *****************************************************************
      *    ATTEMPT READ OF THE REQUEST QUEUE,                         *
      *    (UNLIMITED WAIT ISSUED IF NO MESSAGES CURRENTLY AVAILABLE) *
      *****************************************************************

           MOVE MQMD-CURRENT-VERSION   TO MQMD-VERSION.
           MOVE MQRO-NONE              TO MQMD-REPORT.
           MOVE MQPER-NOT-PERSISTENT   TO MQMD-PERSISTENCE.
           MOVE MQMI-NONE              TO MQMD-MSGID.
           MOVE MQCI-NONE              TO MQMD-CORRELID.
           MOVE MQENC-NATIVE           TO MQMD-ENCODING.
           MOVE MQCCSI-Q-MGR           TO MQMD-CODEDCHARSETID.

           MOVE MQGMO-CURRENT-VERSION  TO MQGMO-VERSION.
           COMPUTE MQGMO-OPTIONS       =  MQGMO-WAIT              +
                                          MQGMO-CONVERT           +
                                          MQGMO-FAIL-IF-QUIESCING +
                                          MQGMO-NO-SYNCPOINT.
           MOVE MQWI-UNLIMITED         TO MQGMO-WAITINTERVAL.
           MOVE MQMO-NONE              TO MQGMO-MATCHOPTIONS.
           MOVE LENGTH OF MQS-CUSTOMER-MESSAGE
                                       TO MQS-BUFFERLENGTH.
           MOVE MQS-HOBJECT-REQUEST-Q  TO MQS-HOBJECT.

           PERFORM P07400-MQS-GET
              THRU P07400-MQS-GET-EXIT.

      *****************************************************************
      *    A MESSAGE HAS ARRIVED !!!!!!!!!!!!!!!!!!!!!!!!             *
      *    PERFORM INITIAL DISPLAYS, RE-INITIALIZATION, ETC.          *
      *****************************************************************
                                                                        00010000
           MOVE FUNCTION CURRENT-DATE  TO WS-CURRENT-DATE-TIME.         00020001
           MOVE WS-CDT-D-MONTH         TO WPM-DATE-TIME-MONTH.          00020001
           MOVE WS-CDT-D-DAY           TO WPM-DATE-TIME-DAY.            00020001
           MOVE WS-CDT-D-YEAR          TO WPM-DATE-TIME-YEAR.           00020001
           MOVE WS-CDT-T-HOURS         TO WPM-DATE-TIME-HH.             00020001
           MOVE WS-CDT-T-MINUTES       TO WPM-DATE-TIME-MM.             00020001
           MOVE WS-CDT-T-SECONDS       TO WPM-DATE-TIME-SS.             00020001


           DISPLAY WPM-BLANK.
           DISPLAY WPM-ALL-ASTERISK.
           DISPLAY WPM-BEGIN-REQUEST.
           DISPLAY WPM-DATE-TIME-MSG.
           DISPLAY WPM-ALL-ASTERISK.

           MOVE MQS-BUFFER             TO MQS-CUSTOMER-MESSAGE.
           MOVE MQMD-MSGID             TO WMF-SAVE-MSGID.
           MOVE MQS-CUSTOMER-SCENARIOS TO WMF-ACTIVE-SCENARIOS.

           DISPLAY 'WMF-ACTIVE-SCENARIOS = '.
           DISPLAY  WMF-ACTIVE-SCENARIOS.

      *****************************************************************
      *    WHEN A NEW MESSAGE ARRIVES PERFORM PROGRAM VARIABLE        *
      *    RE-INITIALIZATION AND OPEN ALL REQUIRED MQSERIES OBJECTS   *
      *****************************************************************

           PERFORM  P02200-RE-INITIALIZE
               THRU P02200-RE-INITIALIZE-EXIT.

           PERFORM  P02500-OPEN-OTHER-QUEUES
               THRU P02500-OPEN-OTHER-QUEUES-EXIT.


      *****************************************************************
      **** PERFORM CUSTOMER PAYMENT TRANSACTION PROCESSING ************
      *    WRITES MESSAGES TO TEMPORARY DYNAMIC, AND TRANSACTION QUES *
      *    (USED BY SCENARIO 16, 2033 - MESSAGE NOT AVAILABLE AND     *
      *     USED BY SCENARIO 17, ABEND ASRA)                          *
      *****************************************************************

           PERFORM  P02800-PAYMENT-TRANS
               THRU P02800-PAYMENT-TRANS-EXIT.


      *****************************************************************
      *    EXTRACT ORDER INFORMATION FOR THE SELECTED CUSTOMER        *
      *****************************************************************

           PERFORM  P03000-ORDER-PROCESS
               THRU P03000-ORDER-PROCESS-EXIT.


           DISPLAY 'P02100-ORDER-INQ-REQUEST-EXIT'.
       P02100-ORDER-INQ-REQUEST-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P02200-RE-INITIALIZE                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RE-INITIALIZE RELEVANT WORK FIELDS  *
      *                AND VARIABLES WHEN A NEW MESSAGE ARRIVES ON    *
      *                THE INPUT QUEUE.                               *
      *                                                               *
      *    CALLED BY:  P02100-ORDER-INQ-REQUEST                       *
      *                                                               *
      *****************************************************************

       P02200-RE-INITIALIZE.
           DISPLAY 'P02200-RE-INITIALIZE'.

      *****************************************************************
      *    INITIALIZE SWITCHES, SUBSCRIPTS, ETC.                      *
      *****************************************************************

           MOVE ZEROES                 TO WS-ERROR-FOUND-SW.
           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.
           MOVE 'Y'                    TO WS-MORE-MESSAGES-SW.
           MOVE ZEROES                 TO WS-SKIP-ERROR-CHECK-SW
                                          WS-ERROR-IS-FORMATTED-SW.

           MOVE SPACES                 TO WMF-MESSAGE-AREA
                                          WMF-ORDER-KEY
                                          WS-PDA-ERROR-TYPE.
                                                                        00010000
           MOVE ZEROES                 TO WMF-TOTAL-ORDERS
                                          WMF-TOTAL-DOLLAR-AMOUNT
                                          WMF-TOTAL-FEES
                                          WMF-AVG-DOLLAR-AMOUNT
                                          WMF-LAST-ORDER-AMOUNT
                                          WMF-DATE-CCYYMMDD.

           MOVE SPACES                 TO WMF-LAST-ORDER-DATE
                                          WMF-LAST-ORDER-NUMBER         00010000
                                          WMF-HOLD-DATE-CCYYMMDD.       00010000

           MOVE 'UUU'                  TO WMF-CREDIT-RATINGS.

      *****************************************************************
      *    INITIALIZE THE QUERY RESULTS MESSAGE I/O AREA TO DEFAULTS  *
      *****************************************************************

           PERFORM P02260-INIT-MQS-MESSAGE
              THRU P02260-INIT-MQS-MESSAGE-EXIT.


           DISPLAY 'P02200-RE-INITIALIZE-EXIT'.
       P02200-RE-INITIALIZE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P02260-INIT-MQS-MESSAGE                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INITIALIZE THE MQSERIES OUTPUT      *
      *                MESSAGE AREA TO DEFAULT VALUES                 *
      *                                                               *
      *    CALLED BY:  P02200-RE-INITIALIZE                            *
      *                                                               *
      *****************************************************************

       P02260-INIT-MQS-MESSAGE.
           DISPLAY 'P02260-INIT-MQS-MESSAGE'.

           MOVE SPACES                 TO MQS-RESULTS-MESSAGE.
           MOVE ZEROES                 TO MQS-RETURN-CODE
                                          MQS-TOTAL-ORDERS
                                          MQS-TOTAL-DOLLAR-AMOUNT
                                          MQS-AVG-DOLLAR-AMOUNT
                                          MQS-LAST-ORDER-AMOUNT.

           PERFORM P02265-INIT-MQS-ORDERS
              THRU P02265-INIT-MQS-ORDERS-EXIT
                  VARYING WS-SUB1 FROM +1 BY +1
                      UNTIL WS-SUB1 > WS-ORDER-MAX.

           DISPLAY 'P02260-INIT-MQS-MESSAGE-EXIT'.
       P02260-INIT-MQS-MESSAGE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P02265-INIT-MQS-ORDERS                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INITIALIZE THE MQSERIES OUTPUT      *
      *                MESSAGE ORDER DETAIL AREA                      *
      *                                                               *
      *    CALLED BY:  P02260-INIT-MQS-MESSAGE                        *
      *                                                               *
      *****************************************************************

       P02265-INIT-MQS-ORDERS.

           MOVE SPACES                 TO MQS-ORDER-NUMBER (WS-SUB1).
           MOVE ZEROES                 TO MQS-ORDER-AMOUNT (WS-SUB1).

       P02265-INIT-MQS-ORDERS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P02500-OPEN-OTHER-QUEUES                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO OPEN ALL THE REMAINING MQSERIES     *
      *                QUEUES TO BE USED BY THE PROGRAM               *
      *                                                               *
      *    CALLED BY:  P02100-ORDER-INQ-REQUEST                       *
      *                                                               *
      *****************************************************************

       P02500-OPEN-OTHER-QUEUES.
           DISPLAY 'P02500-OPEN-OTHER-QUEUES'.

      *****************************************************************
      *    INITIALIZE MQSERIES PARAMETERS AND VARIABLES               *
      *****************************************************************

           MOVE ZEROES                 TO MQS-HOBJECT
                                          MQS-HOBJECT-LOG-01-Q
                                          MQS-HOBJECT-REMOTE-Q
                                          MQS-HOBJECT-TRANSMIT-Q
                                          MQS-HOBJECT-DYNAMIC-Q
                                          MQS-HOBJECT-REPORT-Q
                                          MQS-HOBJECT-TRANSACTION-Q
                                         MQS-HOBJECT-CREDIT-AUTH-RESP-Q
                                          MQS-HOBJECT-BUREAU-REQ-Q
                                          MQS-HOBJECT-BUREAU-RESP-Q
                                          MQS-COMPCODE
                                          MQS-REASONCODE.

           MOVE MQOD-CURRENT-VERSION   TO MQOD-VERSION.
           MOVE MQOT-Q                 TO MQOD-OBJECTTYPE.
           MOVE 'QUEUE'                TO MQS-OBJECTTYPE-DESC.


      *****************************************************************
      *    OPEN TEMPORARY DYNAMIC QUEUE USING THE MODEL QUEUE         *
      *****************************************************************

           MOVE MQS-MODEL-TEMP-DYNAMIC-QUEUE
                                       TO MQOD-OBJECTNAME.
           MOVE MQS-TEMP-DYNAMIC-QUEUE-PREFIX
                                       TO MQOD-DYNAMICQNAME.
           COMPUTE MQS-OPTIONS         =  MQOO-INPUT-SHARED     +
                                          MQOO-OUTPUT           +
                                          MQOO-PASS-ALL-CONTEXT +
                                          MQOO-FAIL-IF-QUIESCING.

           PERFORM P07100-MQS-OPEN
              THRU P07100-MQS-OPEN-EXIT.

           MOVE MQS-HOBJECT            TO MQS-HOBJECT-DYNAMIC-Q.


      *****************************************************************
      *    OPEN REPORT TEMPORARY DYNAMIC QUEUE USING THE MODEL QUEUE  *
      *****************************************************************

           MOVE MQS-MODEL-TEMP-DYNAMIC-QUEUE
                                       TO MQOD-OBJECTNAME.
           MOVE MQS-REPORT-TEMP-DYNAMIC-PREFIX
                                       TO MQOD-DYNAMICQNAME.

           COMPUTE MQS-OPTIONS         =  MQOO-INPUT-SHARED     +
                                          MQOO-OUTPUT           +
                                          MQOO-PASS-ALL-CONTEXT +
                                          MQOO-FAIL-IF-QUIESCING.

           PERFORM P07100-MQS-OPEN
              THRU P07100-MQS-OPEN-EXIT.

           MOVE MQS-HOBJECT            TO MQS-HOBJECT-REPORT-Q.
           MOVE MQOD-OBJECTNAME        TO
                                       MQS-REPORT-TEMP-DYNAMIC-QUEUE.


      *****************************************************************
      *    OPEN TRANSACTION LOG QUEUE 01                              *
      *    (USED FOR SCENARIO TO PUT MESSAGE TO A PUT INHIBITED QUEUE)*
      *****************************************************************

           MOVE MQS-TRANS-LOG-QUEUE-01 TO MQOD-OBJECTNAME.
           COMPUTE MQS-OPTIONS         =  MQOO-OUTPUT            +
                                          MQOO-PASS-ALL-CONTEXT  +
                                          MQOO-FAIL-IF-QUIESCING.

           PERFORM P07100-MQS-OPEN
              THRU P07100-MQS-OPEN-EXIT.

           MOVE MQS-HOBJECT            TO MQS-HOBJECT-LOG-01-Q.


      *****************************************************************
      *    OPEN LOCAL TRANSMISSION QUEUE USED TO TRANSMIT MESSAGES    *
      *    TO A REMOTE QUEUE DEFINITION (CW01 TO CW09)                *
      *****************************************************************
      *    1. FIRST OPEN QUEUE AS INPUT                               *
      *    2. ISSUE TOKEN READ AGAINST QUEUE TO PURGE ALL EXPIRED     *
      *       MESSAGES ON THE QUEUE, THIS PROGRAM PUTS MESSAGES       *
      *       ON THE QUEUE WITH EXPIRATIONS TO SATISFY SCENARIO 16,   *
      *       NO MESSAGE AVAILABLE ON QUEUE, REASONCODE = 2033.       *
      *       (PREVENTS MESSAGE ACCUMULATION ON THE QUEUE)            *
      *    3. CLOSE THE QUEUE                                         *
      *    4. OPEN THE QUEUE AS OUTPUT (USED WHEN MESSAGE PUT ON      *
      *       QREMOTE OBJECT (LOCAL DEFINITION OF REMOTE QUEUE)       *
      *****************************************************************

           MOVE MQS-XMIT-CW01-TO-CW09-QUEUE
                                       TO MQOD-OBJECTNAME.
           COMPUTE MQS-OPTIONS         =  MQOO-INPUT-SHARED      +
                                          MQOO-FAIL-IF-QUIESCING.

           PERFORM P07100-MQS-OPEN
              THRU P07100-MQS-OPEN-EXIT.
           MOVE MQS-HOBJECT            TO MQS-HOBJECT-TRANSMIT-Q.

           PERFORM P02530-CLEAR-XMIT-QUEUE
              THRU P02530-CLEAR-XMIT-QUEUE-EXIT.

           MOVE MQS-XMIT-CW01-TO-CW09-QUEUE
                                       TO MQOD-OBJECTNAME.
           COMPUTE MQS-OPTIONS         =  MQOO-OUTPUT            +
                                          MQOO-FAIL-IF-QUIESCING.

           PERFORM P07100-MQS-OPEN
              THRU P07100-MQS-OPEN-EXIT.
           MOVE MQS-HOBJECT            TO MQS-HOBJECT-TRANSMIT-Q.


      *****************************************************************
      *    OPEN REMOTE QUEUE OBJECT, LOCAL DEFINITION OF REMOTE QUEUE,*
      *    QREMOTE OBJECT, (CW01 TO CW09)
      *****************************************************************

           MOVE MQS-REMOTE-CW01-TO-CW09-QUEUE
                                       TO MQOD-OBJECTNAME.
           COMPUTE MQS-OPTIONS         =  MQOO-OUTPUT            +
                                          MQOO-PASS-ALL-CONTEXT  +
                                          MQOO-FAIL-IF-QUIESCING.

           PERFORM P07100-MQS-OPEN
              THRU P07100-MQS-OPEN-EXIT.

           MOVE MQS-HOBJECT            TO MQS-HOBJECT-REMOTE-Q.


      *****************************************************************
      *    OPEN THE TRANSACTION QUEUE FOR INPUT, OUTPUT, INQUIRE      *
      *    USED PRIMARILY FOR SCENARIOS:                              *
      *    16 - PRODUCE NO MESSAGE AVAILABLE ON QUEUE CONDITION       *
      *****************************************************************

           MOVE MQS-TRANSACTION-QUEUE  TO MQOD-OBJECTNAME.
           COMPUTE MQS-OPTIONS         =  MQOO-INPUT-SHARED      +
                                          MQOO-OUTPUT            +
                                          MQOO-INQUIRE           +
                                          MQOO-PASS-ALL-CONTEXT  +
                                          MQOO-FAIL-IF-QUIESCING.

           PERFORM P07100-MQS-OPEN
              THRU P07100-MQS-OPEN-EXIT.

           MOVE MQS-HOBJECT            TO MQS-HOBJECT-TRANSACTION-Q.


      *****************************************************************
      *    OPEN THE CUSTOMER CREDIT AUTHORIZATION RESPONSE QUEUE.     *
      *    USED TO RETURN CREDIT CHECK RESULTS TO PROGRAM PDAB16      *
      *****************************************************************

           MOVE MQS-CREDIT-AUTH-RESP-QUEUE
                                       TO MQOD-OBJECTNAME.
           COMPUTE MQS-OPTIONS         =  MQOO-OUTPUT            +
                                          MQOO-PASS-ALL-CONTEXT  +
                                          MQOO-FAIL-IF-QUIESCING.

           PERFORM P07100-MQS-OPEN
              THRU P07100-MQS-OPEN-EXIT.

           MOVE MQS-HOBJECT          TO MQS-HOBJECT-CREDIT-AUTH-RESP-Q.


      *****************************************************************
      *    OPEN THE CREDIT BUREAU RESPONSE QUEUE FOR INPUT            *
      *****************************************************************

           MOVE MQS-BUREAU-RESP-QUEUE  TO MQOD-OBJECTNAME.
           COMPUTE MQS-OPTIONS         =  MQOO-INPUT-SHARED     +
                                          MQOO-SAVE-ALL-CONTEXT +
                                          MQOO-FAIL-IF-QUIESCING.
           MOVE ZEROES                 TO MQS-HOBJECT
                                          MQS-COMPCODE
                                          MQS-REASONCODE.

           PERFORM P07100-MQS-OPEN
              THRU P07100-MQS-OPEN-EXIT.

           MOVE MQS-HOBJECT            TO MQS-HOBJECT-BUREAU-RESP-Q.


      *****************************************************************
      *    OPEN THE CREDIT BUREAU REQUEST QUEUE FOR OUTPUT            *
      *****************************************************************

           MOVE MQS-BUREAU-REQ-QUEUE   TO MQOD-OBJECTNAME.
           COMPUTE MQS-OPTIONS         =  MQOO-OUTPUT            +
                                          MQOO-PASS-ALL-CONTEXT  +
                                          MQOO-FAIL-IF-QUIESCING.

           PERFORM P07100-MQS-OPEN
              THRU P07100-MQS-OPEN-EXIT.

           MOVE MQS-HOBJECT            TO MQS-HOBJECT-BUREAU-REQ-Q.


      *****************************************************************
      *    OPEN TRANSACTION LOG QUEUE 99, ONLY IF SCENARIO ACTIVE     *
      *    (USED FOR SCENARIO TO OPEN QUEUE THAT DOES NOT EXIST)      *
      *****************************************************************

           IF WMF-ACTIVE-SCENARIOS-R (14) = 'Y'
               PERFORM P02560-OPEN-LOG-99
                  THRU P02560-OPEN-LOG-99-EXIT.


           DISPLAY 'P02500-OPEN-OTHER-QUEUES-EXIT'.
       P02500-OPEN-OTHER-QUEUES-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P02530-CLEAR-XMIT-QUEUE                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO ATTEMPT RETRIEVAL OF MESSAGES FROM  *
      *                THE TRANSMISSION QUEUE (CWO01 TO CW09) TO      *
      *                CLEAR ANY EXPIRED MESSAGES. THIS PROGRAM       *
      *                PURPOSELY PUTS MESSAGES WITH EXPIRATIONS TO    *
      *                SATISFY SCENARIO NUMBER 16.                    *
      *                                                               *
      *                THIS ROUTINE PREVENTS MESSAGE ACCUMULATION     *
      *                                                               *
      *    CALLED BY:  P02500-OPEN-OTHER-QUEUES                       *
      *                                                               *
      *****************************************************************

       P02530-CLEAR-XMIT-QUEUE.
           DISPLAY 'P02530-CLEAR-XMIT-QUEUE'.


      *****************************************************************
      *    READ TRANSMISSION QUEUE (CW01 TO CW09) TO REMOVE           *
      *    ALL EXPIRED MESSAGES FROM THE QUEUE                        *
      *    (PREVENTS MESSAGE ACCUMULATION)                            *
      *****************************************************************

           MOVE MQMD-CURRENT-VERSION   TO MQMD-VERSION.
           MOVE MQRO-NONE              TO MQMD-REPORT.
           MOVE MQPER-NOT-PERSISTENT   TO MQMD-PERSISTENCE.
           MOVE MQMI-NONE              TO MQMD-MSGID.
           MOVE MQCI-NONE              TO MQMD-CORRELID.
           MOVE MQENC-NATIVE           TO MQMD-ENCODING.
           MOVE MQCCSI-Q-MGR           TO MQMD-CODEDCHARSETID.


           MOVE MQGMO-CURRENT-VERSION  TO MQGMO-VERSION.
           COMPUTE MQGMO-OPTIONS       =  MQGMO-NO-WAIT           +
                                          MQGMO-CONVERT           +
                                          MQGMO-FAIL-IF-QUIESCING +
                                          MQGMO-NO-SYNCPOINT.
           MOVE ZEROES                 TO MQGMO-WAITINTERVAL.
           MOVE MQMO-NONE              TO MQGMO-MATCHOPTIONS.
           MOVE LENGTH OF MQS-BUFFER   TO MQS-BUFFERLENGTH.
           MOVE MQS-HOBJECT-TRANSMIT-Q TO MQS-HOBJECT.
           MOVE 'QUEUE'                TO MQS-OBJECTTYPE-DESC.

           PERFORM P07400-MQS-GET
              THRU P07400-MQS-GET-EXIT.


      *****************************************************************
      *    CHECK FOR APPROPRIATE REASONCODE SUCCESS OR NO MESSAGE     *
      *    AVAILABLE ARE ACCEPTABLE                                   *
      *****************************************************************

           IF (MQS-COMPCODE            =  MQCC-OK)    OR
              (MQS-REASONCODE          =  MQRC-NO-MSG-AVAILABLE)
               NEXT SENTENCE
           ELSE
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB17'           TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQGET'            TO WPME-FUNCTION-1
               MOVE MQS-OBJECTTYPE-DESC
                                       TO WPME-FUNCTION-2
               MOVE 'P02530'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    CLOSE THE QUEUE, WILL BE OPENED AS OUTPUT ABOVE            *
      *****************************************************************

           COMPUTE MQS-OPTIONS         =  MQCO-NONE.
           MOVE MQS-HOBJECT-TRANSMIT-Q TO MQS-HOBJECT.

           PERFORM P07300-MQS-CLOSE
              THRU P07300-MQS-CLOSE-EXIT.

           DISPLAY 'P02530-CLEAR-XMIT-QUEUE-EXIT'.
       P02530-CLEAR-XMIT-QUEUE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P02560-OPEN-LOG-99                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO OPEN THE MQSERIES TRANSACTION LOG   *
      *                (USED FOR SCENARIO TO OPEN QUEUE THAT DOES     *
      *                NOT EXIST)                                     *
      *                                                               *
      *    CALLED BY:  P02500-OPEN-OTHER-QUEUES                       *
      *                                                               *
      *****************************************************************

       P02560-OPEN-LOG-99.
           DISPLAY 'P02560-OPEN-LOG-99'.

      *****************************************************************
      *    OPEN TRANSACTION LOG QUEUE 99                              *
      *    (USED FOR SCENARIO TO OPEN QUEUE THAT DOES NOT EXIST)      *
      *****************************************************************

           MOVE MQS-TRANS-LOG-QUEUE-99 TO MQOD-OBJECTNAME.
           COMPUTE MQS-OPTIONS         =  MQOO-OUTPUT            +
                                          MQOO-PASS-ALL-CONTEXT  +
                                          MQOO-FAIL-IF-QUIESCING.


      *****************************************************************
      *    OPEN SHOULD FAIL WITH REASONCODE 2085,                     *
      *    MQRC-UNKNOWN-OBJECT-NAME, PROGRAM WILL TERMINATE           *
      *****************************************************************

           PERFORM P07100-MQS-OPEN
              THRU P07100-MQS-OPEN-EXIT.

           DISPLAY 'P02560-OPEN-LOG-99-EXIT'.
       P02560-OPEN-LOG-99-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P02800-PAYMENT-TRANS                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS CUSTOMER PAYMENT            *
      *                TRANSACTIONS. WRITES CUSTOMER PAYMENT MQSERIES *
      *                MESSAGES TO:                                   *
      *                1. TEMPORARY DYNAMIC QUEUE                     *
      *                   (USED BY SCENARIO 17 ABEND 0C7  LATER)      *
      *                2. TRANSACTION QUEUE (PERMANENT QUEUE)         *
      *                   (USED BY SCENARIO 16 -2033 NO MSG AVAILABLE)*
      *                                                               *
      *    CALLED BY:  P02100-ORDER-INQ-REQUEST                       *
      *                                                               *
      *****************************************************************

       P02800-PAYMENT-TRANS.
           DISPLAY 'P02800-PAYMENT-TRANS'.

      *****************************************************************
      *    WRITE A CUSTOMER PAYMENT TO THE TEMPORARY DYNAMIC QUEUE    *
      *****************************************************************

           MOVE 'QUEUE'                TO MQS-OBJECTTYPE-DESC.

           MOVE MQMD-CURRENT-VERSION   TO MQMD-VERSION.

           COMPUTE MQMD-REPORT         =  MQRO-EXCEPTION-WITH-DATA   +
                                          MQRO-EXPIRATION-WITH-DATA  +
                                          MQRO-PASS-MSG-ID.

           MOVE MQS-REPORT-TEMP-DYNAMIC-QUEUE
                                       TO MQMD-REPLYTOQ.

           MOVE MQPER-NOT-PERSISTENT   TO MQMD-PERSISTENCE.
           MOVE MQCI-NONE              TO MQMD-CORRELID.
           MOVE 9                      TO MQMD-PRIORITY.
           MOVE MQENC-NATIVE           TO MQMD-ENCODING.
           MOVE MQCCSI-Q-MGR           TO MQMD-CODEDCHARSETID.
           MOVE 5000                   TO MQMD-EXPIRY.


           MOVE MQPMO-CURRENT-VERSION  TO MQPMO-VERSION.
           MOVE MQS-HOBJECT-REQUEST-Q  TO MQPMO-CONTEXT.

           COMPUTE MQPMO-OPTIONS       =  MQPMO-NO-SYNCPOINT      +
                                          MQPMO-PASS-ALL-CONTEXT  +
                                          MQPMO-FAIL-IF-QUIESCING.

           MOVE LENGTH OF MQS-CUSTOMER-PAYMENT-MESSAGE
                                       TO MQS-BUFFERLENGTH.
           MOVE MQS-PAYMENTS-R (1)     TO MQS-CUSTOMER-PAYMENT-MESSAGE.
           MOVE MQS-CUSTOMER-ID        TO MQS-CUSTOMER-PAYMENT-ID.
           MOVE MQS-CUSTOMER-PAYMENT-MESSAGE
                                       TO MQS-BUFFER.
           MOVE MQS-HOBJECT-DYNAMIC-Q  TO MQS-HOBJECT.

           PERFORM P07200-MQS-PUT
              THRU P07200-MQS-PUT-EXIT.


      *****************************************************************
      *    VERIFY MESSAGES EXIST ON THE PERMANENT TRANSACTION QUEUE,  *
      *    IF NONE EXIST, LOAD PRE-DETERMINED MESSAGES TO QUEUE       *
      *    (USED FOR DEMONSTRATION PURPOSES - SCENARIO 16             *
      *****************************************************************
      *****************************************************************
      *    USE MQSERIES INQUIRE FUNCTION (MQINQ) TO GET THE CURRENT   *
      *    QUEUE DEPTH (I.E. NUMBER OF MESSAGES ON THE QUEUE          *
      *****************************************************************

           MOVE 1                      TO MQS-SELECTOR-COUNT
                                          MQS-INTATTR-COUNT.
           MOVE MQIA-CURRENT-Q-DEPTH   TO MQS-SELECTORS (1).
           MOVE ZEROES                 TO MQS-INTATTRS (1)
                                          MQS-CHARATTR-LENGTH.

           CALL 'MQINQ'       USING    MQS-HCONN,
                                       MQS-HOBJECT-TRANSACTION-Q,
                                       MQS-SELECTOR-COUNT,
                                       MQS-SELECTOR-TABLE,
                                       MQS-INTATTR-COUNT,
                                       MQS-INTATTR-TABLE,
                                       MQS-CHARATTR-LENGTH,
                                       MQS-CHARATTRS,
                                       MQS-COMPCODE,
                                       MQS-REASONCODE.

           IF MQS-COMPCODE             =  MQCC-OK
               NEXT SENTENCE
           ELSE
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB17'           TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQINQ'            TO WPME-FUNCTION-1
               MOVE MQS-OBJECTTYPE-DESC
                                       TO WPME-FUNCTION-2
               MOVE 'P02800'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    IF NO MESSAGES ON THE TRANSACTION QUEUE, WRITE SOME        *
      *****************************************************************

           IF MQS-INTATTRS (1)         > ZEROES
               NEXT SENTENCE
           ELSE
               MOVE MQMD-CURRENT-VERSION
                                       TO MQMD-VERSION
               COMPUTE MQMD-REPORT     =  MQRO-EXCEPTION-WITH-DATA  +
                                          MQRO-EXPIRATION-WITH-DATA +
                                          MQRO-PASS-MSG-ID
               MOVE MQS-REPORT-TEMP-DYNAMIC-QUEUE
                                       TO MQMD-REPLYTOQ

               MOVE MQPER-NOT-PERSISTENT
                                       TO MQMD-PERSISTENCE
               MOVE MQCI-NONE          TO MQMD-CORRELID
               MOVE 9                  TO MQMD-PRIORITY
               MOVE MQENC-NATIVE       TO MQMD-ENCODING
               MOVE MQCCSI-Q-MGR       TO MQMD-CODEDCHARSETID
               MOVE MQEI-UNLIMITED     TO MQMD-EXPIRY

               MOVE MQPMO-CURRENT-VERSION
                                       TO MQPMO-VERSION
               MOVE MQS-HOBJECT-REQUEST-Q
                                       TO MQPMO-CONTEXT

               COMPUTE MQPMO-OPTIONS   =  MQPMO-NO-SYNCPOINT      +
                                          MQPMO-PASS-ALL-CONTEXT  +
                                          MQPMO-FAIL-IF-QUIESCING

               PERFORM  P02815-WRITE-PAYMENT-TRAN
                   THRU P02815-WRITE-PAYMENT-TRAN-EXIT
                       VARYING WS-SUB1 FROM +1 BY +1
                           UNTIL WS-SUB1 > +3.


      *****************************************************************
      *    PROCESS CUSTOMER SPECIAL REQUEST MESSAGES FIRST            *
      *    (USED FOR SCENARIO TO ATTEMPT RETRIEVAL OF MESSAGES WITH   *
      *     A BOGUS MESSAGE ID TO PRODUCE RETURN CODE 2033,NO MESSAGE *
      *     AVAILABLE ON QUEUE)                                       *
      *****************************************************************

           IF WMF-ACTIVE-SCENARIOS-R (16) = 'Y'
               PERFORM  P02830-GET-SPECIAL-MSG
                   THRU P02830-GET-SPECIAL-MSG-EXIT.

           DISPLAY 'P02800-PAYMENT-TRANS-EXIT'.
       P02800-PAYMENT-TRANS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P02815-WRITE-PAYMENT-TRAN                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO WRITE PAYMENT MESSAGES TO           *
      *                THE TRANSACTION QUEUE FOR DEMONSTRATION        *
      *                PURPOSES. USED WITH SCENARIO 16 - 2033 NO      *
      *                MESSAGE AVAILABLE ON QUEUE                     *
      *                                                               *
      *    CALLED BY:  P02800-PAYMENT-TRANS                           *
      *                                                               *
      *****************************************************************

       P02815-WRITE-PAYMENT-TRAN.
           DISPLAY 'P02815-WRITE-PAYMENT-TRAN'.


           MOVE LENGTH OF MQS-CUSTOMER-PAYMENT-MESSAGE
                                       TO MQS-BUFFERLENGTH.
           MOVE MQS-PAYMENTS-R (WS-SUB1)
                                       TO MQS-CUSTOMER-PAYMENT-MESSAGE.
           MOVE MQS-CUSTOMER-PAYMENT-MESSAGE
                                       TO MQS-BUFFER.
           MOVE MQS-HOBJECT-TRANSACTION-Q
                                       TO MQS-HOBJECT.

           PERFORM P07200-MQS-PUT
              THRU P07200-MQS-PUT-EXIT.


           DISPLAY 'P02815-WRITE-PAYMENT-TRAN-EXIT'.
       P02815-WRITE-PAYMENT-TRAN-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P02830-GET-SPECIAL-MSG                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO ATTEMPT RETRIEVAL OF MESSAGES FROM  *
      *                THE TRANSACTION QUE USING BOGUS MESSAGE ID TO  *
      *                PRODUCE A RETURN CODE OF 2033, NO MESSAGE      *
      *                AVAILABLE ON QUEUE FOR SCENARIO PROCESSING     *
      *                                                               *
      *    CALLED BY:  P02800-PAYMENT-TRANS                           *
      *                                                               *
      *****************************************************************

       P02830-GET-SPECIAL-MSG.
           DISPLAY 'P02830-GET-SPECIAL-MSG'.

      *****************************************************************
      *    PUT A CUSTOMER REQUEST MESSAGE ON THE REMOTE QUEUE         *
      *    (DONE FOR DEMO PURPOSES ONLY TO SHOW A MESSAGE ON THE      *
      *     TRANSMISSION QUEUE WHEN XPEDITER DUMP REPORTS ARE SHOWN,  *
      *     CURRENTLY DONE FOR SCENARIO 16, 2033 MQSERIES RETURN CODE)*
      *****************************************************************

           MOVE MQMD-CURRENT-VERSION   TO MQMD-VERSION.
           MOVE MQRO-NONE              TO MQMD-REPORT.
           MOVE SPACES                 TO MQMD-REPLYTOQMGR
                                          MQMD-REPLYTOQ.
           MOVE MQPER-NOT-PERSISTENT   TO MQMD-PERSISTENCE.
           MOVE MQCI-NONE              TO MQMD-CORRELID.
           MOVE MQPRI-PRIORITY-AS-Q-DEF
                                       TO MQMD-PRIORITY.
           MOVE MQENC-NATIVE           TO MQMD-ENCODING.
           MOVE MQCCSI-Q-MGR           TO MQMD-CODEDCHARSETID.
           MOVE 60000                  TO MQMD-EXPIRY.


           MOVE MQPMO-CURRENT-VERSION  TO MQPMO-VERSION.
           MOVE MQS-HOBJECT-REQUEST-Q  TO MQPMO-CONTEXT.

           COMPUTE MQPMO-OPTIONS       =  MQPMO-NO-SYNCPOINT      +
                                          MQPMO-PASS-ALL-CONTEXT  +
                                          MQPMO-FAIL-IF-QUIESCING.
           MOVE LENGTH OF MQS-CUSTOMER-MESSAGE
                                       TO MQS-BUFFERLENGTH.
           MOVE MQS-CUSTOMER-MESSAGE   TO MQS-BUFFER.
           MOVE MQS-HOBJECT-REMOTE-Q   TO MQS-HOBJECT.
           MOVE 'QREMOTE'              TO MQS-OBJECTTYPE-DESC.

           PERFORM P07200-MQS-PUT
              THRU P07200-MQS-PUT-EXIT.


      *****************************************************************
      *    ATTEMPT TO READ TRANSACTION QUEUE USING BOGUS MSG ID       *
      *    (PRODUCES A 2033 -- NO MESSAGE AVAILABLE, AND TERMINATE    *
      *     PROGRAM WITH A STORAGE DUMP)                              *
      *****************************************************************

           MOVE MQMD-CURRENT-VERSION   TO MQMD-VERSION.
           MOVE MQRO-NONE              TO MQMD-REPORT.
           MOVE MQPER-NOT-PERSISTENT   TO MQMD-PERSISTENCE.
           MOVE WMF-SPECIAL-MSGID      TO MQMD-MSGID.
           MOVE MQCI-NONE              TO MQMD-CORRELID.
           MOVE MQENC-NATIVE           TO MQMD-ENCODING.
           MOVE MQCCSI-Q-MGR           TO MQMD-CODEDCHARSETID.


           MOVE MQGMO-CURRENT-VERSION  TO MQGMO-VERSION.
           COMPUTE MQGMO-OPTIONS       =  MQGMO-WAIT              +
                                          MQGMO-CONVERT           +
                                          MQGMO-FAIL-IF-QUIESCING +
                                          MQGMO-NO-SYNCPOINT.
           MOVE 1000                   TO MQGMO-WAITINTERVAL.
           MOVE MQMO-MATCH-MSG-ID      TO MQGMO-MATCHOPTIONS.
           MOVE LENGTH OF MQS-CUSTOMER-MESSAGE
                                       TO MQS-BUFFERLENGTH.
           MOVE MQS-HOBJECT-TRANSACTION-Q
                                       TO MQS-HOBJECT.
           MOVE 'QUEUE'                TO MQS-OBJECTTYPE-DESC.

           PERFORM P07400-MQS-GET
              THRU P07400-MQS-GET-EXIT.


      *****************************************************************
      *    TERMINATE PROGRAM WITH DUMP FOR THE NO MSG AVAILABLE(2033) *
      *****************************************************************

           MOVE 'MQS'                  TO WS-PDA-ERROR-TYPE.
           MOVE 'PDAB17'               TO WPME-PROGRAM-ID.
           MOVE MQS-REASONCODE         TO WPME-REASON-CODE.
           MOVE 'MQGET'                TO WPME-FUNCTION-1.
           MOVE MQS-OBJECTTYPE-DESC    TO WPME-FUNCTION-2.
           MOVE 'P02830'               TO WPME-PARAGRAPH.
           PERFORM  P99500-PDA-ERROR
               THRU P99500-PDA-ERROR-EXIT.

           DISPLAY 'P02830-GET-SPECIAL-MSG-EXIT'.
       P02830-GET-SPECIAL-MSG-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03000-ORDER-PROCESS                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PERFORM THE ORDER INFORMATION       *
      *                EXTRACTION PROCESS FROM THE IMS/DLI ORDER      *
      *                DATABASE FOR THE SELECTED CUSTOMER ID          *
      *                                                               *
      *    CALLED BY:  P02100-ORDER-INQ-REQUEST                       *
      *                                                               *
      *****************************************************************

       P03000-ORDER-PROCESS.
           DISPLAY 'P03000-ORDER-PROCESS'.

      *****************************************************************
      *    PROCESS THE ORDER DATABASE FOR THE CUSTOMER ID SUPPLIED    *
      *    IN THE MQSERIES INPUT MESSAGE                               *
      *****************************************************************

           PERFORM  P03500-EXTRACT-ORDERS
               THRU P03500-EXTRACT-ORDERS-EXIT.


           DISPLAY 'P03000-ORDER-PROCESS-EXIT'.
       P03000-ORDER-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03500-EXTRACT-ORDERS                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE AND FORMAT THE ORDER       *
      *                INFORMATION FOR THE SELECTED CUSTOMER ID       *
      *                FROM THE ORDER DATABASE                        *
      *                                                               *
      *    CALLED BY:  P03000-ORDER-PROCESS                           *
      *                                                               *
      *****************************************************************

       P03500-EXTRACT-ORDERS.
           DISPLAY 'P03500-EXTRACT-ORDERS'.

           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.
           MOVE ZEROES                 TO WS-SUB1.

      *****************************************************************
      *    ESTABLISH DATABASE POSITION AT FIRST ORDER,                *
      *    (IF NOT FOUND, TERMINATE THE PROCESSING LOOP)              *
      *****************************************************************

           MOVE ZEROES                 TO WMF-ORDER-PREFIX.
           MOVE LOW-VALUES             TO WMF-ORDER-NUMBER.
           MOVE 'GE'                   TO OSQ-REL-OPER.

           PERFORM  P80100-GU-ORDER
               THRU P80100-GU-ORDER-EXIT.

           IF OP-STATUS                =  'GE' OR 'GB'
               GO TO P03500-EXTRACT-ORDERS-EXIT.


      *****************************************************************
      *    FORMAT / RETRIEVE THE REMAINDER OF ORDERS FOR CUSTOMER     *
      *    UNTIL NO MORE                                               *
      *****************************************************************

           PERFORM  P03600-SELECT-ORDERS
               THRU P03600-SELECT-ORDERS-EXIT
                   UNTIL PROCESS-COMPLETE.


      *****************************************************************
      *    AT END OF ORDERS, PERFORM TOTAL CALCULATIONS, MOVE INFO    *
      *    TO RETURN MESSAGE AREA                                     *
      *****************************************************************

           MOVE WMF-TOTAL-ORDERS       TO MQS-TOTAL-ORDERS.
           MOVE WMF-TOTAL-DOLLAR-AMOUNT
                                       TO MQS-TOTAL-DOLLAR-AMOUNT.
           MOVE WMF-LAST-ORDER-DATE    TO MQS-LAST-ORDER-DATE.
           MOVE WMF-LAST-ORDER-AMOUNT  TO MQS-LAST-ORDER-AMOUNT.
           MOVE WMF-LAST-ORDER-NUMBER  TO MQS-LAST-ORDER-NUMBER.

           IF WMF-TOTAL-ORDERS         > ZEROES
               COMPUTE WMF-AVG-DOLLAR-AMOUNT ROUNDED =
                   WMF-TOTAL-DOLLAR-AMOUNT / WMF-TOTAL-ORDERS.

           MOVE WMF-AVG-DOLLAR-AMOUNT  TO MQS-AVG-DOLLAR-AMOUNT.


           DISPLAY 'P03500-EXTRACT-ORDERS-EXIT'.
       P03500-EXTRACT-ORDERS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03600-SELECT-ORDERS                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS ALL ORDERS,                 *
      *                SELECTING ONLY THOSE MATCHING ON CUSTOMER ID   *
      *                                                               *
      *    CALLED BY:  P03500-EXTRACT-ORDERS                          *
      *                                                               *
      *****************************************************************

       P03600-SELECT-ORDERS.
           DISPLAY 'P03600-SELECT-ORDERS'.

      *****************************************************************
      *    SELECT ORDERS WITH MATCHING CUSTOMER ID                    *
      *****************************************************************

           IF ORDER-CUSTOMER-ID    =  MQS-CUSTOMER-ID
               PERFORM  P03640-FORMAT-ORDER
                   THRU P03640-FORMAT-ORDER-EXIT.


      *****************************************************************
      *    READ THE NEXT ORDER IN THE DATABASE, IF NO MORE END LOOP   *
      *****************************************************************

           PERFORM  P80130-GN-ORDER
               THRU P80130-GN-ORDER-EXIT.

           IF OP-STATUS                =  'GE' OR 'GB'
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
               GO TO P03600-SELECT-ORDERS-EXIT.

           DISPLAY 'P03600-SELECT-ORDERS-EXIT'.
       P03600-SELECT-ORDERS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03640-FORMAT-ORDER                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO FORMAT / CAPTURE ORDER INFORMATION  *
      *                FOR ORDERS MATCHING THE SELECTION CRITERIA     *
      *                OF CUSTOMER ID                                 *
      *                                                               *
      *    CALLED BY:  P03600-SELECT-ORDERS                           *
      *                                                               *
      *****************************************************************

       P03640-FORMAT-ORDER.
           DISPLAY 'P03640-FORMAT-ORDER'.

      *****************************************************************
      *    A MAXIMUM OF 14 ORDERS WILL BE CAPTURED (SCREEN SIZE),     *
      *    IF WE HAVE 14 CONSIDER THE PROCESS COMPLETE, EXIT          *
      *****************************************************************

           ADD +1                      TO WS-SUB1.

           IF WS-SUB1                  >  WS-ORDER-MAX
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
               GO TO P03640-FORMAT-ORDER-EXIT.


      *****************************************************************
      *    ADD TO RUNNING TOTALS, STORE ORDER INFORMATION             *
      *****************************************************************

           ADD +1                      TO WMF-TOTAL-ORDERS.

           COMPUTE WMF-TOTAL-DOLLAR-AMOUNT  =
                   WMF-TOTAL-DOLLAR-AMOUNT  +  ORDER-TOTAL-AMOUNT.


           MOVE ORDER-DATE-YYMMDD      TO WMF-DATE-YYMMDD.
           IF WMF-DATE-YY              >  50
               MOVE 19                 TO WMF-DATE-CC
           ELSE
               MOVE 20                 TO WMF-DATE-CC.

           IF WMF-DATE-CCYYMMDD  NOT   <  WMF-HOLD-DATE-CCYYMMDD
               MOVE ORDER-DATE-YYMMDD  TO WMF-LAST-ORDER-DATE
               MOVE ORDER-TOTAL-AMOUNT TO WMF-LAST-ORDER-AMOUNT
               MOVE ORDER-NUMBER       TO WMF-LAST-ORDER-NUMBER
               MOVE WMF-DATE-CCYYMMDD  TO WMF-HOLD-DATE-CCYYMMDD.


           MOVE ORDER-NUMBER           TO MQS-ORDER-NUMBER (WS-SUB1).
           MOVE ORDER-TOTAL-AMOUNT     TO MQS-ORDER-AMOUNT (WS-SUB1).


           DISPLAY 'P03640-FORMAT-ORDER-EXIT'.
       P03640-FORMAT-ORDER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05000-ORDER-INQ-RESPONSE                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO WRITE THE MQSERIES RESPONSE MESSAGE *
      *                TO THE CUSTOMER RESPONSE QUEUE, MESSAGE WILL   *
      *                BE PROCESSED BY THE CALLING APPLICATION        *
      *                                                               *
      *    CALLED BY:  P02000-ORDER-QUERY                             *
      *                                                               *
      *****************************************************************

       P05000-ORDER-INQ-RESPONSE.
           DISPLAY 'P05000-ORDER-INQ-RESPONSE'.

      *****************************************************************
      *    WRITE THE CUSTOMER ORDER INQUIRY RESPONSE MESSAGE          *
      *****************************************************************

           MOVE MQMD-CURRENT-VERSION   TO MQMD-VERSION.
           MOVE MQRO-NONE              TO MQMD-REPORT.
           MOVE MQFMT-STRING           TO MQMD-FORMAT.
           MOVE SPACES                 TO MQMD-REPLYTOQMGR
                                          MQMD-REPLYTOQ.
           MOVE MQPER-NOT-PERSISTENT   TO MQMD-PERSISTENCE.
           MOVE MQCI-NONE              TO MQMD-CORRELID.
           MOVE MQPRI-PRIORITY-AS-Q-DEF
                                       TO MQMD-PRIORITY.
           MOVE MQENC-NATIVE           TO MQMD-ENCODING.
           MOVE MQCCSI-Q-MGR           TO MQMD-CODEDCHARSETID.
           MOVE 5000                   TO MQMD-EXPIRY.
           MOVE WMF-SAVE-MSGID         TO MQMD-MSGID.


           MOVE MQPMO-CURRENT-VERSION  TO MQPMO-VERSION.
           MOVE MQS-HOBJECT-REQUEST-Q  TO MQPMO-CONTEXT.

           COMPUTE MQPMO-OPTIONS       =  MQPMO-NO-SYNCPOINT      +
                                          MQPMO-PASS-ALL-CONTEXT  +
                                          MQPMO-FAIL-IF-QUIESCING.
           MOVE LENGTH OF MQS-RESULTS-MESSAGE
                                       TO MQS-BUFFERLENGTH.
           MOVE MQS-RESULTS-MESSAGE    TO MQS-BUFFER.
           MOVE MQS-HOBJECT-RESPONSE-Q TO MQS-HOBJECT.

           PERFORM P07200-MQS-PUT
              THRU P07200-MQS-PUT-EXIT.


      *****************************************************************
      *    WRITE THE CUSTOMER CREDIT AUTHORIZATION REQUEST MESSAGE    *
      *    (ONLY WRITE IF NO ERRORS FOUND DURING ORDER PROCESS   )    *
      *****************************************************************

           IF NO-ERROR-FOUND
               PERFORM  P05030-CREDIT-AUTH-REQ
                   THRU P05030-CREDIT-AUTH-REQ-EXIT.


      *****************************************************************
      *    WRITE THE TRANSACTION LOG MESSAGE AS MIRROR IMAGE OF THE   *
      *    ORDER QUERY RESULT MESSAGE                                 *
      *    (USED FOR SCENARIO TO ATTEMPT WRITE TO A PUT INHIBITED QUE)*
      *****************************************************************

           IF (NO-ERROR-FOUND) AND (WMF-ACTIVE-SCENARIOS-R (15) = 'Y')
               PERFORM P05100-TRANS-LOG-PROCESS
                  THRU P05100-TRANS-LOG-PROCESS-EXIT.


      *****************************************************************
      *    IF SCENARIO ACTIVE ALLOW TRUNCATED MESSAGE TO BE RECEIVED  *
      *    (RESULTS WILL BE USED LATER TO PRODUCE AN ABEND 0C7)       *
      *****************************************************************

           IF WMF-ACTIVE-SCENARIOS-R (17) = 'Y'
               PERFORM  P05200-GET-SHORT-MSG
                   THRU P05200-GET-SHORT-MSG-EXIT.


      *****************************************************************
      *    CALCULATE CUSTOMER TOTAL ORDER FEES                        *
      *    (IF SCENARIO TO PROCESS A TRUNCATED MESSAGE IS ACTIVE,     *
      *    THIS CALCULATION WILL PRODUCE A 0C7 ABEND AS THE           *
      *    MESSAGE ORDER FEE FIELD WILL CONTAIN INVALID NUMERICS)     *
      *****************************************************************

           COMPUTE WMF-TOTAL-FEES ROUNDED =
               WMF-TOTAL-ORDERS * MQS-CUSTOMER-ORDER-FEE.

           DISPLAY 'P05000-ORDER-INQ-RESPONSE-EXIT'.
       P05000-ORDER-INQ-RESPONSE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05030-CREDIT-AUTH-REQ                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO WRITE THE MQSERIES CREDIT AUTH      *
      *                REQUEST MESSAGE. THE MESSAGE IS PROCESSED      *
      *                DIRECTLY BY THE BATCH MODULE PDAB05, BUREAU    *
      *                CREDIT AUTHORIZATION MODULE                    *
      *                                                               *
      *    CALLED BY:  P05000-ORDER-INQ-REQUEST                       *
      *                                                               *
      *****************************************************************

       P05030-CREDIT-AUTH-REQ.
           DISPLAY 'P05030-CREDIT-AUTH-REQ'.

      *****************************************************************
      *    WRITE THE CUSTOMER CREDIT AUTHORIZATION REQUEST MESSAGE    *
      *****************************************************************

           MOVE MQMD-CURRENT-VERSION   TO MQMD-VERSION.
           MOVE MQRO-NONE              TO MQMD-REPORT.
           MOVE MQPER-NOT-PERSISTENT   TO MQMD-PERSISTENCE.
           MOVE MQCI-NONE              TO MQMD-CORRELID.
           MOVE MQPRI-PRIORITY-AS-Q-DEF
                                       TO MQMD-PRIORITY.
           MOVE MQENC-NATIVE           TO MQMD-ENCODING.
           MOVE MQCCSI-Q-MGR           TO MQMD-CODEDCHARSETID.
           MOVE 5000                   TO MQMD-EXPIRY.
           MOVE WMF-SAVE-MSGID         TO MQMD-MSGID.


           MOVE MQPMO-CURRENT-VERSION  TO MQPMO-VERSION.
           MOVE ZEROES                 TO MQPMO-CONTEXT.

           COMPUTE MQPMO-OPTIONS       =  MQPMO-NO-SYNCPOINT      +
                                          MQPMO-DEFAULT-CONTEXT   +
                                          MQPMO-FAIL-IF-QUIESCING.


           MOVE SPACES                 TO MQS-CREDIT-AUTH-REQ-MESSAGE.
           MOVE ZEROES                 TO MQS-CREDIT-RETURN-CODE.
           MOVE MQS-CUSTOMER-ID        TO MQS-CREDIT-CUSTOMER-ID.
           MOVE LENGTH OF MQS-CREDIT-AUTH-REQ-MESSAGE
                                       TO MQS-BUFFERLENGTH.
           MOVE MQS-CREDIT-AUTH-REQ-MESSAGE
                                       TO MQS-BUFFER.
           MOVE MQS-HOBJECT-BUREAU-REQ-Q
                                       TO MQS-HOBJECT.
           MOVE 'QUEUE'                TO MQS-OBJECTTYPE-DESC.

           PERFORM P07200-MQS-PUT
              THRU P07200-MQS-PUT-EXIT.


           DISPLAY 'P05030-CREDIT-AUTH-REQ-EXIT'.
       P05030-CREDIT-AUTH-REQ-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05100-TRANS-LOG-PROCESS                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO WRITE THE MQSERIES TRANSACTION      *
      *                LOG MESSAGES FOR THE ORDER QUERY PROCESS       *
      *                (USED FOR SCENARIO TO ATTEMPT WRITE TO A PUT  *
      *                INHIBITED QUEUE)                               *
      *                                                               *
      *    CALLED BY:  P05000-ORDER-INQ-RESPONSE                      *
      *                                                               *
      *****************************************************************

       P05100-TRANS-LOG-PROCESS.
           DISPLAY 'P05100-TRANS-LOG-PROCESS'.

           MOVE MQMD-CURRENT-VERSION   TO MQMD-VERSION.
           MOVE MQRO-NONE              TO MQMD-REPORT.
           MOVE SPACES                 TO MQMD-REPLYTOQMGR
                                          MQMD-REPLYTOQ.
           MOVE MQPER-PERSISTENT       TO MQMD-PERSISTENCE.
           MOVE MQCI-NONE              TO MQMD-CORRELID.
           MOVE MQPRI-PRIORITY-AS-Q-DEF
                                       TO MQMD-PRIORITY.
           MOVE MQENC-NATIVE           TO MQMD-ENCODING.
           MOVE MQCCSI-Q-MGR           TO MQMD-CODEDCHARSETID.
           MOVE 5000                   TO MQMD-EXPIRY.


           MOVE MQPMO-CURRENT-VERSION  TO MQPMO-VERSION.
           MOVE MQS-HOBJECT-REQUEST-Q  TO MQPMO-CONTEXT.

           COMPUTE MQPMO-OPTIONS       =  MQPMO-NO-SYNCPOINT      +
                                          MQPMO-PASS-ALL-CONTEXT  +
                                          MQPMO-FAIL-IF-QUIESCING.
           MOVE LENGTH OF MQS-RESULTS-MESSAGE
                                       TO MQS-BUFFERLENGTH.
           MOVE MQS-RESULTS-MESSAGE    TO MQS-BUFFER.
           MOVE MQS-HOBJECT-LOG-01-Q   TO MQS-HOBJECT.
           MOVE 'QUEUE'                TO MQS-OBJECTTYPE-DESC.

           PERFORM P07200-MQS-PUT
              THRU P07200-MQS-PUT-EXIT.


      *****************************************************************
      *    TERMINATE PROGRAM WITH A STORAGE DUMP                      *
      *****************************************************************

           MOVE 'MQS'                  TO WS-PDA-ERROR-TYPE
           MOVE 'PDAB17'               TO WPME-PROGRAM-ID
           MOVE MQS-REASONCODE         TO WPME-REASON-CODE
           MOVE 'MQPUT'                TO WPME-FUNCTION-1
           MOVE MQS-OBJECTTYPE-DESC    TO WPME-FUNCTION-2
           MOVE 'P05100'               TO WPME-PARAGRAPH
           PERFORM  P99500-PDA-ERROR
               THRU P99500-PDA-ERROR-EXIT.


           DISPLAY 'P05100-TRANS-LOG-PROCESS-EXIT'.
       P05100-TRANS-LOG-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05200-GET-SHORT-MSG                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ THE CUSTOMER PAYMENT MESSAGE   *
      *                (FROM TRANSACTION Q) FORCING MSG TRUNCATION.   *
      *                PROCESS ONLY EXECUTED WHEN SPECIFIC SCENARIO   *
      *                IS ACTIVE. RESULTS USED LATER IN PROGRAM       *
      *                TO PRODUCE A 0C7 ABNORMAL TERMINATION          *
      *                                                               *
      *                USED IN SCENARIO 17 (ABEND 0C7), TRUNCATED     *
      *                MESSAGE RENDERS MQS-CUSTOMER-ORDER-FEE AS AN   *
      *                INVALID NUMERIC FIELD                          *
      *                                                               *
      *    CALLED BY:  P05000-ORDER-INQ-RESPONSE                      *
      *                                                               *
      *****************************************************************

       P05200-GET-SHORT-MSG.
           DISPLAY 'P05200-GET-SHORT-MSG'.

      *****************************************************************
      *    SET MESSAGE OPTIONS FOR TRUNCATED MESSAGE                  *
      *****************************************************************

           MOVE MQMD-CURRENT-VERSION   TO MQMD-VERSION.
           MOVE MQRO-NONE              TO MQMD-REPORT.
           MOVE MQPER-NOT-PERSISTENT   TO MQMD-PERSISTENCE.
           MOVE MQMI-NONE              TO MQMD-MSGID.
           MOVE MQCI-NONE              TO MQMD-CORRELID.
           MOVE MQENC-NATIVE           TO MQMD-ENCODING.
           MOVE MQCCSI-Q-MGR           TO MQMD-CODEDCHARSETID.


           MOVE MQGMO-CURRENT-VERSION  TO MQGMO-VERSION.
           COMPUTE MQGMO-OPTIONS       =  MQGMO-WAIT                 +
                                          MQGMO-ACCEPT-TRUNCATED-MSG +
                                          MQGMO-CONVERT              +
                                          MQGMO-FAIL-IF-QUIESCING.
           MOVE 5000                   TO MQGMO-WAITINTERVAL.
           MOVE MQS-HOBJECT-DYNAMIC-Q  TO MQS-HOBJECT.
           MOVE SPACES                 TO MQS-BUFFER.

           MOVE LENGTH OF MQS-BUFFER-SHORT
                                       TO MQS-BUFFERLENGTH.

           MOVE 1                      TO WS-SKIP-ERROR-CHECK-SW.
           PERFORM P07400-MQS-GET
              THRU P07400-MQS-GET-EXIT.
           MOVE ZEROES                 TO WS-SKIP-ERROR-CHECK-SW.


      *****************************************************************
      *    CHECK FOR APPROPRIATE REASONCODE TO SATISFY THE INTENDED   *
      *    SCENARIO ERROR, OTHERWISE CONSIDER FATAL ERROR-- TERMINATE *
      *****************************************************************

           IF MQS-REASONCODE           =  MQRC-TRUNCATED-MSG-ACCEPTED
               MOVE MQS-BUFFER         TO MQS-CUSTOMER-MESSAGE
           ELSE
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB17'           TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQGET'            TO WPME-FUNCTION-1
               MOVE MQS-OBJECTTYPE-DESC
                                       TO WPME-FUNCTION-2
               MOVE 'P05200'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

           DISPLAY 'P05200-GET-SHORT-MSG-EXIT'.
       P05200-GET-SHORT-MSG-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06000-CREDIT-BUREAU-RESP                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS THE CREDIT BUREAU           *
      *                AUTHORIZATION CHECK RESPONSES. 3 RESPONSE      *
      *                MESSAGES ARE EXPECTED, 1 EACH FROM EACH OF THE *
      *                CREDIT BUREAUS (TRW, EQUIFAX, EXPERIAN).       *
      *                                                               *
      *                ROUTINE ALSO WRITES THE CREDIT AUTHORIZATION   *
      *                MESSAGE TO BE PROCESSED BY PROGRAM PDAB16      *
      *                                                               *
      *    CALLED BY:  P02000-ORDER-QUERY                             *
      *                                                               *
      *****************************************************************

       P06000-CREDIT-BUREAU-RESP.
           DISPLAY 'P06000-CREDIT-BUREAU-RESP'.

      *****************************************************************
      *    PROCESS LOOP TO OBTAIN 3 CREDIT RESPONSE MESSAGES          *
      *****************************************************************

           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.
           MOVE ZEROES                 TO WS-SUB1.
           MOVE 'UUU'                  TO WMF-CREDIT-RATINGS.

           PERFORM  P06100-GET-BUREAU-RESP
               THRU P06100-GET-BUREAU-RESP-EXIT
                   UNTIL PROCESS-COMPLETE.


      *****************************************************************
      *    INTERROGATE THE CREDIT BUREAU RESPONSES                    *
      *****************************************************************
      *****************************************************************
      *    IF ANY BUREAUS UNAVAILABLE -- FINAL RATING IS UNAVAILABLE  *
      *****************************************************************

           IF WMF-CREDIT-RATINGS-R (1) = 'U'      OR
              WMF-CREDIT-RATINGS-R (2) = 'U'      OR
              WMF-CREDIT-RATINGS-R (3) = 'U'
               MOVE 'U'                TO WMF-FINAL-CREDIT-RATING
               GO TO P06000-CREDIT-BUREAU-RESP-EXIT.

      *****************************************************************
      *    IF ANY BUREAU REJECTIONS -- FINAL RATING IS REJECTED       *
      *****************************************************************

           IF WMF-CREDIT-RATINGS-R (1) = 'R'      OR
              WMF-CREDIT-RATINGS-R (2) = 'R'      OR
              WMF-CREDIT-RATINGS-R (3) = 'R'
               MOVE 'R'                TO WMF-FINAL-CREDIT-RATING
               GO TO P06000-CREDIT-BUREAU-RESP-EXIT.

      *****************************************************************
      *    OTHERWISE CREDIT IS ACCEPTED                               *
      *****************************************************************

           MOVE 'A'                    TO WMF-FINAL-CREDIT-RATING.

           DISPLAY 'P06000-CREDIT-BUREAU-RESP-EXIT'.
       P06000-CREDIT-BUREAU-RESP-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06100-GET-BUREAU-RESP                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO OBTAIN THE CREDIT BUREAU            *
      *                AUTHORIZATION CHECK RESPONSES. 3 RESPONSE      *
      *                MESSAGES ARE EXPECTED, 1 EACH FROM EACH OF THE *
      *                CREDIT BUREAUS (TRW, EQUIFAX, EXPERIAN).       *
      *                THE GETS ARE DONE WITH A WAIT INTERVAL TO      *
      *                ALLOW AMPLE TIME FOR THE BUREAU MESSAGES TO    *
      *                ARRIVE.                                        *
      *                                                               *
      *    CALLED BY:  P06100-CREDIT-BUREAU-RESP                      *
      *                                                               *
      *****************************************************************

       P06100-GET-BUREAU-RESP.
           DISPLAY 'P06100-GET-BUREAU-RESP'.

      *****************************************************************
      *    CHECK FOR MAXIMUM MESSAGES PROCESSED                       *
      *****************************************************************
           ADD +1                      TO WS-SUB1.

           IF WS-SUB1                  > +3
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
               GO TO P06100-GET-BUREAU-RESP-EXIT.


      *****************************************************************
      *    READ A CREDIT BUREAU RESPONSE MESSAGE FROM PDAB05 (ON CW09)*
      *    (RESULTS FROM THE CREDIT AUTHORIZATION REQUEST MESSAGE)    *
      *****************************************************************

           MOVE MQMD-CURRENT-VERSION   TO MQMD-VERSION.
           MOVE MQRO-NONE              TO MQMD-REPORT.
           MOVE MQPER-NOT-PERSISTENT   TO MQMD-PERSISTENCE.
           MOVE WMF-SAVE-MSGID         TO MQMD-MSGID.
           MOVE MQCI-NONE              TO MQMD-CORRELID.
           MOVE MQENC-NATIVE           TO MQMD-ENCODING.
           MOVE MQCCSI-Q-MGR           TO MQMD-CODEDCHARSETID.


           MOVE MQGMO-CURRENT-VERSION  TO MQGMO-VERSION.
           COMPUTE MQGMO-OPTIONS       =  MQGMO-WAIT              +
                                          MQGMO-CONVERT           +
                                          MQGMO-FAIL-IF-QUIESCING +
                                          MQGMO-NO-SYNCPOINT.
           MOVE 50000                  TO MQGMO-WAITINTERVAL.
           MOVE MQMO-MATCH-MSG-ID      TO MQGMO-MATCHOPTIONS.
           MOVE LENGTH OF MQS-CREDIT-AUTH-REQ-MESSAGE
                                       TO MQS-BUFFERLENGTH.
           MOVE MQS-HOBJECT-BUREAU-RESP-Q
                                       TO MQS-HOBJECT.


           MOVE 1                      TO WS-SKIP-ERROR-CHECK-SW.
           PERFORM P07400-MQS-GET
              THRU P07400-MQS-GET-EXIT.
           MOVE ZEROES                 TO WS-SKIP-ERROR-CHECK-SW.


      *****************************************************************
      *    CHECK FOR MQSERIES ERROR, IF ERROR ENCOUNTERED FORMAT      *
      *    ERROR MESSAGE, CALL ERROR ROUTINE TO TERMINATE             *
      *****************************************************************

           IF MQS-COMPCODE             =  MQCC-OK
               NEXT SENTENCE
           ELSE
           IF MQS-REASONCODE           =  MQRC-NO-MSG-AVAILABLE
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
               GO TO P06100-GET-BUREAU-RESP-EXIT
           ELSE
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB17'           TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQGET'            TO WPME-FUNCTION-1
               MOVE MQS-OBJECTTYPE-DESC
                                       TO WPME-FUNCTION-2
               MOVE 'P06100'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    CHECK FOR ERROR ENCOUNTERED IN BUREAU RESPONSE PROGRAM     *
      *    (BATCH PROGRAM PDAB05 ON CW09)                             *
      *****************************************************************

           MOVE MQS-BUFFER             TO MQS-CREDIT-AUTH-REQ-MESSAGE.

           IF MQS-CREDIT-FATAL-ERROR
               MOVE 1                  TO WS-ERROR-IS-FORMATTED-SW
               MOVE MQS-CREDIT-ERROR-TYPE                               00638300
                                       TO WS-PDA-ERROR-TYPE             00638300
               MOVE MQS-CREDIT-ERROR-LINE-01                            00638300
                                       TO WPEA-ERROR-07-TEXT            00638300
               MOVE MQS-CREDIT-ERROR-LINE-02                            00638300
                                       TO WPEA-ERROR-08-TEXT            00638300
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    SAVE THE CREDIT RATING FOR LATER INTERROGATION             *
      *****************************************************************

           IF MQS-CREDIT-NO-ERROR
               MOVE MQS-CREDIT-AUTH  TO WMF-CREDIT-RATINGS-R (WS-SUB1).


           DISPLAY 'P06100-GET-BUREAU-RESP-EXIT'.
       P06100-GET-BUREAU-RESP-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06200-SEND-CREDIT-AUTH                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO WRITE THE MQSERIES RESPONSE MESSAGE *
      *                TO THE CREDIT AUTHORIZATION RESPONSE QUEUE,    *
      *                RESPONSE WILL BE PROCESSED BY THE CALLING      *
      *                APPLICATION                                    *
      *                                                               *
      *    CALLED BY:  P02000-ORDER-QUERY                             *
      *                                                               *
      *****************************************************************

       P06200-SEND-CREDIT-AUTH.
           DISPLAY 'P06200-SEND-CREDIT-AUTH'.

      *****************************************************************
      *    WRITE THE CREDIT AUTHORIZATION RESPONSE MESSAGE            *
      *****************************************************************

           MOVE MQMD-CURRENT-VERSION   TO MQMD-VERSION.
           MOVE MQRO-NONE              TO MQMD-REPORT.
           MOVE MQFMT-STRING           TO MQMD-FORMAT.
           MOVE SPACES                 TO MQMD-REPLYTOQMGR
                                          MQMD-REPLYTOQ.
           MOVE MQPER-NOT-PERSISTENT   TO MQMD-PERSISTENCE.
           MOVE MQCI-NONE              TO MQMD-CORRELID.
           MOVE MQPRI-PRIORITY-AS-Q-DEF
                                       TO MQMD-PRIORITY.
           MOVE MQENC-NATIVE           TO MQMD-ENCODING.
           MOVE MQCCSI-Q-MGR           TO MQMD-CODEDCHARSETID.
           MOVE 5000                   TO MQMD-EXPIRY.
           MOVE WMF-SAVE-MSGID         TO MQMD-MSGID.


           MOVE MQPMO-CURRENT-VERSION  TO MQPMO-VERSION.
           MOVE ZEROES                 TO MQPMO-CONTEXT.

           COMPUTE MQPMO-OPTIONS       =  MQPMO-NO-SYNCPOINT      +
                                          MQPMO-DEFAULT-CONTEXT   +
                                          MQPMO-FAIL-IF-QUIESCING.


           MOVE SPACES                 TO MQS-CREDIT-AUTH-REQ-MESSAGE.
           MOVE ZEROES                 TO MQS-CREDIT-RETURN-CODE.
           MOVE WMF-CUSTOMER-ID        TO MQS-CREDIT-CUSTOMER-ID.
           MOVE ALL '*'                TO MQS-CREDIT-BUREAU.
           MOVE WMF-FINAL-CREDIT-RATING
                                       TO MQS-CREDIT-AUTH.

           MOVE LENGTH OF MQS-CREDIT-AUTH-REQ-MESSAGE
                                       TO MQS-BUFFERLENGTH.
           MOVE MQS-CREDIT-AUTH-REQ-MESSAGE
                                       TO MQS-BUFFER.
           MOVE MQS-HOBJECT-CREDIT-AUTH-RESP-Q
                                       TO MQS-HOBJECT.

           PERFORM P07200-MQS-PUT
              THRU P07200-MQS-PUT-EXIT.

           DISPLAY 'P06200-SEND-CREDIT-AUTH-EXIT'.
       P06200-SEND-CREDIT-AUTH-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06900-CLOSE-THE-QUEUES                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CLOSE RELEVANT MQSERIES QUEUES      *
      *                                                               *
      *    CALLED BY:  P02000-ORDER-QUERY                             *
      *                                                               *
      *****************************************************************

       P06900-CLOSE-THE-QUEUES.
           DISPLAY 'P06900-CLOSE-THE-QUEUES'.

      *****************************************************************
      *    CLOSE THE MQSERIES TEMPORARY DYNAMIC QUEUE                 *
      *****************************************************************

           IF MQS-HOBJECT-DYNAMIC-Q    >  ZEROES
               COMPUTE MQS-OPTIONS     =  MQCO-NONE
               MOVE MQS-HOBJECT-DYNAMIC-Q
                                       TO MQS-HOBJECT
               PERFORM P07300-MQS-CLOSE
                  THRU P07300-MQS-CLOSE-EXIT.


      *****************************************************************
      *    CLOSE THE MQSERIES REPORT TEMPORARY DYNAMIC QUEUE          *
      *****************************************************************

           IF MQS-HOBJECT-REPORT-Q     >  ZEROES
               COMPUTE MQS-OPTIONS     =  MQCO-NONE
               MOVE MQS-HOBJECT-REPORT-Q
                                       TO MQS-HOBJECT
               PERFORM P07300-MQS-CLOSE
                  THRU P07300-MQS-CLOSE-EXIT.


      *****************************************************************
      *    CLOSE THE MQSERIES TRANSACTION LOG QUEUE 01                 *
      *****************************************************************

           IF MQS-HOBJECT-LOG-01-Q     >  ZEROES
               COMPUTE MQS-OPTIONS     =  MQCO-NONE
               MOVE MQS-HOBJECT-LOG-01-Q
                                       TO MQS-HOBJECT
               PERFORM P07300-MQS-CLOSE
                  THRU P07300-MQS-CLOSE-EXIT.


      *****************************************************************
      *    CLOSE THE LOCAL TRANSMISSION QUEUE, USED FOR TRANSMISSION   *
      *    OF MESSAGES TO A REMOTE QUEUE (CW01 TO CW09)                *
      *****************************************************************

           IF MQS-HOBJECT-TRANSMIT-Q   >  ZEROES
               COMPUTE MQS-OPTIONS     =  MQCO-NONE
               MOVE MQS-HOBJECT-TRANSMIT-Q
                                       TO MQS-HOBJECT
               PERFORM P07300-MQS-CLOSE
                  THRU P07300-MQS-CLOSE-EXIT.


      *****************************************************************
      *    CLOSE THE REMOTE QUEUE (QREMOTE, LOCAL DEFINITION OF A     *
      *    REMOTE QUEUE (CW01 TO CW09)                                *
      *****************************************************************

           IF MQS-HOBJECT-REMOTE-Q     >  ZEROES
               COMPUTE MQS-OPTIONS     =  MQCO-NONE
               MOVE MQS-HOBJECT-REMOTE-Q
                                       TO MQS-HOBJECT
               PERFORM P07300-MQS-CLOSE
                  THRU P07300-MQS-CLOSE-EXIT.


      *****************************************************************
      *    CLOSE THE MQSERIES TRANSACTION QUEUE (PERMANENT)            *
      *****************************************************************

           IF MQS-HOBJECT-TRANSACTION-Q >  ZEROES
               COMPUTE MQS-OPTIONS     =  MQCO-NONE
               MOVE MQS-HOBJECT-TRANSACTION-Q
                                       TO MQS-HOBJECT
               PERFORM P07300-MQS-CLOSE
                  THRU P07300-MQS-CLOSE-EXIT.


      *****************************************************************
      *    CLOSE THE MQSERIES CREDIT AUTHORIZATION RESPONSE QUEUE      *
      *****************************************************************

           IF MQS-HOBJECT-CREDIT-AUTH-RESP-Q  >  ZEROES
               COMPUTE MQS-OPTIONS     =  MQCO-NONE
               MOVE MQS-HOBJECT-CREDIT-AUTH-RESP-Q
                                       TO MQS-HOBJECT
               PERFORM P07300-MQS-CLOSE
                  THRU P07300-MQS-CLOSE-EXIT.


      *****************************************************************
      *    CLOSE THE MQSERIES CREDIT BUREAU RESPONSE QUEUE             *
      *****************************************************************

           IF MQS-HOBJECT-BUREAU-RESP-Q   >  ZEROES
               COMPUTE MQS-OPTIONS     =  MQCO-NONE
               MOVE MQS-HOBJECT-BUREAU-RESP-Q
                                       TO MQS-HOBJECT
               PERFORM P07300-MQS-CLOSE
                  THRU P07300-MQS-CLOSE-EXIT.


      *****************************************************************
      *    CLOSE THE MQSERIES CREDIT BUREAU REQUEST  QUEUE             *
      *****************************************************************

           IF MQS-HOBJECT-BUREAU-REQ-Q    >  ZEROES
               COMPUTE MQS-OPTIONS     =  MQCO-NONE
               MOVE MQS-HOBJECT-BUREAU-REQ-Q
                                       TO MQS-HOBJECT
               PERFORM P07300-MQS-CLOSE
                  THRU P07300-MQS-CLOSE-EXIT.


           DISPLAY 'P06900-CLOSE-THE-QUEUES-EXIT'.
       P06900-CLOSE-THE-QUEUES-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07000-MQS-CONNECT                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONNECT TO THE MQSERIES QUEUE       *
      *                MANAGER                                        *
      *                                                               *
      *    CALLED BY:  P00050-INITIALIZE                              *
      *                                                               *
      *****************************************************************

       P07000-MQS-CONNECT.
           DISPLAY 'P07000-MQS-CONNECT'.

           CALL 'MQCONN'      USING    MQS-QMANAGER-NAME
                                       MQS-HCONN
                                       MQS-COMPCODE
                                       MQS-REASONCODE.


      *****************************************************************
      *    CHECK FOR MQSERIES ERROR, IF ERROR ENCOUNTERED FORMAT      *
      *    ERROR MESSAGE, CALL ERROR ROUTINE TO TERMINATE             *
      *****************************************************************

           IF MQS-COMPCODE             =  MQCC-OK
               NEXT SENTENCE
           ELSE
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB17'           TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQCONN'           TO WPME-FUNCTION-1
               MOVE 'QUEUE MANAGER'    TO WPME-FUNCTION-2
               MOVE 'P07000'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


           DISPLAY 'P07000-MQS-CONNECT-EXIT'.
       P07000-MQS-CONNECT-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07030-MQS-DISCONNECT                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DISCONNECT FROM THE MQSERIES QUEUE  *
      *                MANAGER                                        *
      *                                                               *
      *    CALLED BY:  P00100-END-OF-JOB                              *
      *                                                               *
      *****************************************************************

       P07030-MQS-DISCONNECT.
           DISPLAY 'P07030-MQS-DISCONNECT'.

           CALL 'MQDISC'      USING    MQS-HCONN
                                       MQS-COMPCODE
                                       MQS-REASONCODE.


      *****************************************************************
      *    CHECK FOR MQSERIES ERROR, IF ERROR ENCOUNTERED FORMAT      *
      *    ERROR MESSAGE, CALL ERROR ROUTINE TO TERMINATE             *
      *****************************************************************

           IF MQS-COMPCODE             =  MQCC-OK
               MOVE ZEROES             TO MQS-HCONN
           ELSE
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB17'           TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQDISC'           TO WPME-FUNCTION-1
               MOVE 'QUEUE MANAGER'    TO WPME-FUNCTION-2
               MOVE 'P07030'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


           DISPLAY 'P07030-MQS-DISCONNECT-EXIT'.
       P07030-MQS-DISCONNECT-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07100-MQS-OPEN                                *
      *                                                               *
      *    FUNCTION :  ROUTINE TO OPEN A MQSERIES OBJECT. ALL         *
      *                OPTIONS AND PARAMETERS ARE SET BY THE CALLING  *
      *                PARAGRAPH AND VARY ACCORDING TO THE OBJECT     *
      *                TYPE BEING OPENED.                             *
      *                                                               *
      *    CALLED BY:  P05100-ORDER-INQ-REQUEST                       *
      *                P05500-ORDER-INQ-RESPONSE                      *
      *                P05800-CREDIT RESULTS                          *
      *                                                               *
      *****************************************************************

       P07100-MQS-OPEN.
           DISPLAY 'P07100-MQS-OPEN'.

           CALL 'MQOPEN'      USING    MQS-HCONN
                                       MQOD
                                       MQS-OPTIONS
                                       MQS-HOBJECT
                                       MQS-COMPCODE
                                       MQS-REASONCODE.


      *****************************************************************
      *    CHECK FOR MQSERIES ERROR, IF ERROR ENCOUNTERED FORMAT      *
      *    ERROR MESSAGE, CALL ERROR ROUTINE TO TERMINATE             *
      *****************************************************************

           IF MQS-COMPCODE             =  MQCC-OK
               NEXT SENTENCE
           ELSE
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB17'           TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQOPEN'           TO WPME-FUNCTION-1
               MOVE MQS-OBJECTTYPE-DESC
                                       TO WPME-FUNCTION-2
               MOVE 'P07100'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


           DISPLAY 'P07100-MQS-OPEN-EXIT'.
       P07100-MQS-OPEN-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07200-MQS-PUT                                 *
      *                                                               *
      *    FUNCTION :  ROUTINE TO WRITE A MESSAGE TO THE OPEN QUEUE   *
      *                OPTIONS AND PARAMETERS ARE SET BY THE CALLING  *
      *                PARAGRAPH AND VARY ACCORDING TO THE SPECIFIC   *
      *                MESSAGE PUT OPERATION                          *
      *                                                               *
      *    CALLED BY:  P05100-ORDER-INQ-REQUEST                       *
      *                                                               *
      *****************************************************************

       P07200-MQS-PUT.
           DISPLAY 'P07200-MQS-PUT'.

           CALL 'MQPUT'       USING    MQS-HCONN
                                       MQS-HOBJECT
                                       MQMD
                                       MQPMO
                                       MQS-BUFFERLENGTH
                                       MQS-BUFFER
                                       MQS-COMPCODE
                                       MQS-REASONCODE.


      *****************************************************************
      *    CHECK FOR MQSERIES ERROR, IF ERROR ENCOUNTERED FORMAT      *
      *    ERROR MESSAGE, CALL ERROR ROUTINE TO TERMINATE             *
      *****************************************************************

           IF MQS-COMPCODE             =  MQCC-OK
               NEXT SENTENCE
           ELSE
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB17'           TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQPUT'            TO WPME-FUNCTION-1
               MOVE MQS-OBJECTTYPE-DESC
                                       TO WPME-FUNCTION-2
               MOVE 'P07200'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


           DISPLAY 'P07200-MQS-PUT-EXIT'.
       P07200-MQS-PUT-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07300-MQS-CLOSE                               *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CLOSE A MQSERIES OBJECT. ALL        *
      *                OPTIONS AND PARAMETERS ARE SET BY THE CALLING  *
      *                PARAGRAPH AND VARY ACCORDING TO THE OBJECT     *
      *                TYPE BEING CLOSED.                              *
      *                                                               *
      *    CALLED BY:  P05100-ORDER-INQ-REQUEST                       *
      *                P05500-ORDER-INQ-RESPONSE                      *
      *                P05800-CREDIT-RESULTS                          *
      *                P06900-CLOSE-THE-QUEUES                        *
      *                                                               *
      *****************************************************************

       P07300-MQS-CLOSE.
           DISPLAY 'P07300-MQS-CLOSE'.

           CALL 'MQCLOSE'     USING    MQS-HCONN
                                       MQS-HOBJECT
                                       MQS-OPTIONS
                                       MQS-COMPCODE
                                       MQS-REASONCODE.


      *****************************************************************
      *    CHECK FOR MQSERIES ERROR, IF ERROR ENCOUNTERED FORMAT      *
      *    ERROR MESSAGE, CALL ERROR ROUTINE TO TERMINATE             *
      *****************************************************************

           IF MQS-COMPCODE             =  MQCC-OK
               NEXT SENTENCE
           ELSE
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB17'           TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQCLOSE'          TO WPME-FUNCTION-1
               MOVE MQS-OBJECTTYPE-DESC
                                       TO WPME-FUNCTION-2
               MOVE 'P07300'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


           DISPLAY 'P07300-MQS-CLOSE-EXIT'.
       P07300-MQS-CLOSE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07400-MQS-GET                                 *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ A MESSAGE FROM THE OPEN QUEUE. *
      *                OPTIONS AND PARAMETERS ARE SET BY THE CALLING  *
      *                PARAGRAPH AND VARY ACCORDING TO THE SPECIFIC   *
      *                MESSAGE GET OPERATION                          *
      *                                                               *
      *    CALLED BY:  P05500-ORDER-INQ-RESPONSE                      *
      *                P05800-CREDIT-RESULTS                          *
      *                                                               *
      *****************************************************************

       P07400-MQS-GET.
           DISPLAY 'P07400-MQS-GET'.

           CALL 'MQGET'       USING    MQS-HCONN
                                       MQS-HOBJECT
                                       MQMD
                                       MQGMO
                                       MQS-BUFFERLENGTH
                                       MQS-BUFFER
                                       MQS-DATALENGTH
                                       MQS-COMPCODE
                                       MQS-REASONCODE.


      *****************************************************************
      *    CHECK FOR MQSERIES ERROR, IF ERROR ENCOUNTERED FORMAT      *
      *    ERROR MESSAGE, CALL ERROR ROUTINE TO TERMINATE             *
      *****************************************************************

           IF SKIP-ERROR-CHECK
               GO TO P07400-MQS-GET-EXIT.


           IF MQS-COMPCODE             =  MQCC-OK
               NEXT SENTENCE
           ELSE
           IF MQS-REASONCODE           =  MQRC-NO-MSG-AVAILABLE
               GO TO P07400-MQS-GET-EXIT
           ELSE
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB17'           TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQGET'            TO WPME-FUNCTION-1
               MOVE MQS-OBJECTTYPE-DESC
                                       TO WPME-FUNCTION-2
               MOVE 'P07400'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


           DISPLAY 'P07400-MQS-GET-EXIT'.
       P07400-MQS-GET-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80000-READ-PARAMETERS                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ THE INPUT PARAMETER FILE       *
      *                                                               *
      *    CALLED BY:  P00050-INITIALIZE                              *
      *                P00630-LOAD-PARM-ARRAY                         *
      *                                                               *
      *****************************************************************

       P80000-READ-PARAMETERS.
           DISPLAY 'P80000-READ-PARAMETERS'.

           READ INPUT-PARAMETERS INTO WS-PARAMETER-RECORD
               AT END
                   MOVE 'Y' TO WS-END-OF-PARM-FILE-SW
                   GO TO P80000-READ-PARAMETERS-EXIT.

           ADD +1                      TO WS-PARAMETER-RECORDS-IN.

           DISPLAY ' '.
           DISPLAY WPEA-ERROR-01.
           MOVE WS-PARAMETER-RECORDS-IN TO WPM-RECORD-NUMBER.
           DISPLAY WPM-RECORD-NUMBER-MSG.
           DISPLAY WS-PARAMETER-RECORD.
           DISPLAY WPEA-ERROR-01.

           DISPLAY 'P80000-READ-PARAMETERS-EXIT'.
       P80000-READ-PARAMETERS-EXIT.
           EXIT.
           EJECT

      ***************************************************************** 13370000
      *                                                               * 13380000
      *    PARAGRAPH:  P80130-GU-ORDER                                * 13390000
      *                                                               * 13400000
      *    FUNCTION :  ROUTINE TO READ AN ORDER ROOT SEGMENT          * 13410000
      *                                                               * 13430000
      *    CALLED BY:  P03500-EXTRACT-ORDERS                          * 13440000
      *                                                               * 13450000
      ***************************************************************** 13460000
                                                                        13470000
       P80100-GU-ORDER.                                                 13480000
           DISPLAY 'P80100-GU-ORDER'.                                   13480000
                                                                        13490000
           CALL 'CBLTDLI' USING                                         13500000
                          ICF-GU                                        13510000
                          ORDER-PCB                                     13520000
                          ORDER-SEGMENT                                 13530000
                          ORDER-SSA-QUAL                                13540000
           END-CALL.                                                    13550000
                                                                        13580000
           IF OP-STATUS                = SPACES OR 'GE' OR 'GB'         13590000
               NEXT SENTENCE                                            13600000
           ELSE                                                         13610000
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE             13620000
               MOVE 'PDAB17'           TO WPIE-PROGRAM-ID               13630000
               MOVE 'P80100'           TO WPIE-PARAGRAPH                13640000
               MOVE OP-STATUS          TO WPIE-STATUS-CODE              13650000
               MOVE 'GU'               TO WPIE-FUNCTION-CODE            13660000
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME             13670000
               MOVE 'ORDER1DB'         TO WPIE-DATABASE-NAME            13680000
               MOVE 'GU ORDER SEGMENT' TO WPIE-COMMAND                  13690000
               PERFORM  P99500-PDA-ERROR                                13700000
                   THRU P99500-PDA-ERROR-EXIT                           13700000
           END-IF.                                                      13710000
                                                                        13720000
           DISPLAY 'P80100-GU-ORDER-EXIT'.                              13480000
       P80100-GU-ORDER-EXIT.                                            13730000
           EXIT.                                                        13740000
           EJECT                                                        13750000

      ***************************************************************** 13370000
      *                                                               * 13380000
      *    PARAGRAPH:  P80130-GN-ORDER                                * 13390000
      *                                                               * 13400000
      *    FUNCTION :  ROUTINE TO READ ORDER ROOT SEGMENTS            * 13410000
      *                SEQUENTIALLY                                   * 13420000
      *                                                               * 13430000
      *    CALLED BY:  P03500-EXTRACT-ORDERS                          * 13440000
      *                                                               * 13450000
      ***************************************************************** 13460000
                                                                        13470000
       P80130-GN-ORDER.                                                 13480000
           DISPLAY 'P80130-GN-ORDER'.                                   13480000
                                                                        13490000
           CALL 'CBLTDLI' USING                                         13500000
                          ICF-GN                                        13510000
                          ORDER-PCB                                     13520000
                          ORDER-SEGMENT                                 13530000
                          ORDER-SSA-UNQUAL                              13540000
           END-CALL.                                                    13550000
                                                                        13580000
                                                                        13580000
           IF OP-STATUS                = SPACES OR 'GE' OR 'GB'         13590000
               NEXT SENTENCE                                            13600000
           ELSE                                                         13610000
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE             13620000
               MOVE 'PDAB17'           TO WPIE-PROGRAM-ID               13630000
               MOVE 'P80130'           TO WPIE-PARAGRAPH                13640000
               MOVE OP-STATUS          TO WPIE-STATUS-CODE              13650000
               MOVE 'GN'               TO WPIE-FUNCTION-CODE            13660000
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME             13670000
               MOVE 'ORDER1DB'         TO WPIE-DATABASE-NAME            13680000
               MOVE 'GN ORDER SEGMENT' TO WPIE-COMMAND                  13690000
               PERFORM  P99500-PDA-ERROR                                13700000
                   THRU P99500-PDA-ERROR-EXIT                           13700000
           END-IF.                                                      13710000
                                                                        13720000
           DISPLAY 'P80130-GN-ORDER-EXIT'.                              13480000
       P80130-GN-ORDER-EXIT.                                            13730000
           EXIT.                                                        13740000
           EJECT                                                        13750000

      *****************************************************************
      *                                                               *
      *    P R O D U C T    D E M O N S T R A T I O N     A P P L     *
      *                                                               *
      *             E R R O R    R O U T I N E S                      *
      *                                                               *
      *                                                               *
      *****************************************************************

KCS305                                                                  KCS32005
KCS305***************************************************************** KCS32005
KCS305*                                                               * KCS32005
KCS305*    PARAGRAPH:  P99400-ERROR-ROUTINE                           * KCS32005
KCS305*                                                               * KCS32005
KCS305*    FUNCTION :  ROUTINE TO FORMAT AND DISPLAY NON-FATAL ERRORS * KCS32005
KCS305*                                                               * KCS32005
KCS305*                ERROR TEXT IS DISPLAYED                        * KCS32005
KCS305*                TO THE USER INDICATING THE NATURE OF THE ERROR * KCS32005
KCS305*                                                               * KCS32005
KCS305*                CONTROL IS RETURNED TO CALLING ROUTINE         * KCS32005
KCS305*                                                               * KCS32005
KCS305*    CALLED BY:  GLOBAL                                         * KCS32005
KCS305*                                                               * KCS32005
KCS305***************************************************************** KCS32005
KCS305                                                                  KCS32005
KCS305 P99400-ERROR-ROUTINE.                                            KCS32005
KCS305     DISPLAY 'P99400-ERROR-ROUTINE'.                              KCS32005
KCS305                                                                  KCS32005
KCS305     MOVE 1                      TO WS-ERROR-FOUND-SW.            KCS32005
KCS305                                                                  KCS32005
KCS305     DISPLAY ' '.                                                 KCS32005
KCS305     DISPLAY WPEA-ERROR-01.                                       KCS32005
KCS305     DISPLAY WPEA-ERROR-02.                                       KCS32005
KCS305     DISPLAY WPEA-ERROR-03.                                       KCS32005
KCS305     DISPLAY WPEA-ERROR-04.                                       KCS32005
KCS305     DISPLAY WPEA-ERROR-05.                                       KCS32005
KCS305     DISPLAY WPEA-ERROR-06.                                       KCS32005
KCS305                                                                  00636300
KCS305     MOVE WMF-MESSAGE-AREA       TO WPEA-ERROR-07-TEXT.           00638300
KCS305     DISPLAY WPEA-ERROR-07.                                       KCS32005
KCS305                                                                  00638600
KCS305     DISPLAY WPEA-ERROR-08.                                       KCS32005
KCS305     DISPLAY WPEA-ERROR-09.                                       KCS32005
KCS305     DISPLAY WPEA-ERROR-10.                                       KCS32005
KCS305     DISPLAY ' '.                                                 KCS32005
KCS305                                                                  KCS32005
KCS305     DISPLAY 'P99400-ERROR-ROUTINE-EXIT'.                         KCS32005
KCS305 P99400-ERROR-ROUTINE-EXIT.                                       KCS32005
KCS305     EXIT.                                                        KCS32005
KCS305     EJECT                                                        KCS32005

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P99500-PDA-ERROR                               *
      *                                                               *
      *    FUNCTION :  ROUTINE TO HANDLE FATAL / TERMINATING GENERAL, *
      *                DB2, IMS-DLI, MQSERIES ERRORS                  *
      *                                                               *
      *                ERROR TEXT IS DISPLAYED                        *
      *                TO THE USER INDICATING THE NATURE OF THE ERROR *
      *                                                               *
      *                PROGRAM IS ABNORMALLY TERMINATED               *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************

       P99500-PDA-ERROR.

           MOVE 9                      TO WS-ERROR-FOUND-SW.

           DISPLAY ' '.
           DISPLAY WPEA-ERROR-01.
           DISPLAY WPEA-ERROR-02.
           DISPLAY WPEA-ERROR-03.
           DISPLAY WPEA-ERROR-04.
           DISPLAY WPEA-ERROR-05.
           DISPLAY WPEA-ERROR-06.

      *****************************************************************
      *      FORMAT AND SEND ERROR TEXT                               *
      *****************************************************************
                                                                        00636300
           IF ERROR-IS-FORMATTED                                        00636300
               NEXT SENTENCE                                            00636300
           ELSE                                                         00636300
           IF PDA-DB2-ERROR                                             00636400
               MOVE WS-PDA-DB2-ERROR-01                                 00636500
                                       TO WPEA-ERROR-07-TEXT            00636600
               MOVE WS-PDA-DB2-ERROR-02                                 00636700
                                       TO WPEA-ERROR-08-TEXT            00636800
           ELSE                                                         00636900
           IF PDA-IMS-ERROR                                             00637000
               MOVE WS-PDA-IMS-ERROR-01                                 00637100
                                       TO WPEA-ERROR-07-TEXT            00637200
               MOVE WS-PDA-IMS-ERROR-02                                 00637300
                                       TO WPEA-ERROR-08-TEXT            00637400
           ELSE                                                         00637500
           IF PDA-MQSERIES-ERROR                                        00637602
               MOVE WS-PDA-MQSERIES-ERROR-01                            00637702
                                       TO WPEA-ERROR-07-TEXT            00637802
               MOVE WS-PDA-MQSERIES-ERROR-02                            00637902
                                       TO WPEA-ERROR-08-TEXT            00638002
           ELSE                                                         00638102
               MOVE WS-PDA-GEN-ERROR-01                                 00638200
                                       TO WPEA-ERROR-07-TEXT            00638300
               MOVE WS-PDA-GEN-ERROR-02                                 00638400
                                       TO WPEA-ERROR-08-TEXT.           00638500
                                                                        00638600
           DISPLAY WPEA-ERROR-07.
           DISPLAY WPEA-ERROR-08.
                                                                        00638600
           DISPLAY WPEA-ERROR-09.
           DISPLAY WPEA-ERROR-10.
           DISPLAY ' '.


           MOVE 99                     TO WS-RETURN-CODE.
           CALL 'ILBOABN0'          USING WS-RETURN-CODE.
           MOVE WS-RETURN-CODE         TO RETURN-CODE.

           GOBACK.

       P99500-PDA-ERROR-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P99999-ERROR                                   *
      *                                                               *
      *    FUNCTION :  GENERIC ERROR ROUTINE TO HANDLE GENERAL ERRORS *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************

RTN    P99999-ERROR.                                                     RTN
NOT                                                                      NOT
USED       MOVE 'GEN'                  TO WS-PDA-ERROR-TYPE.             USED
AS OF      MOVE 'PDAB17'               TO WPGE-PROGRAM-ID.               AS OF
JAN        MOVE 99                     TO WS-RETURN-CODE.                JAN
2001       MOVE 'ERROR'                TO WPGE-DESCRIPTION.              2001
           MOVE 'P99999'               TO WPGE-PARAGRAPH.
LLR                                                                      LLR
           PERFORM  P99500-PDA-ERROR
               THRU P99500-PDA-ERROR-EXIT.

       P99999-ERROR-EXIT.
           EXIT.
           EJECT
                                                                        00631800