       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDAB16.

      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDAB16                                            *
      *                                                               *
      * FUNCTION:   PROGRAM PDAB16 IS THE BATCH MQSERIES CUSTOMER     *
      *             ORDER INQUIRY REQUEST FUNCTION. PROGRAM ALLOWS    *
      *             USERS TO QUERY THE ORDER DATABASE FOR ALL         *
      *             ORDER INFORMATION RELATING TO A SPECIFIC          *
      *             CUSTOMER IDENTIFICATION.                          *
      *                                                               *
      *             THE PROGRAM PROVIDES MQSERIES BATCH FUNCTIONALITY *
      *             FOR VARIOUS PRODUCT LINES. THE QUERY REQUEST IS   *
      *             ISSUED VIA A MQSERIES MESSAGE (PROCESSED BY       *
      *             PROGRAM PDAB17, CUSTOMER ORDER PROCESSING PROGRAM)*
      *             WITH THE QUERY RESULTS RETURNED VIA AN MQSERIES   *
      *             MESSAGE IN A RESPONSE QUEUE.                      *
      *                                                               *
      *                                                               *
      * FILES   :   PARAMETER FILE   -  SEQUENTIAL (INPUT)            *
      *             CUSTOMER FILE    -  VSAM KSDS  (INPUT)            *
      *                                                               *
      *                                                               *
      * PROGRAMS INITIATED:                                           *
      *             PDAB17     CUSTOMER ORDER PROCESSING PROGRAM,     *
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
           SELECT VSAM-CUSTOMER      ASSIGN TO VCUSTOMR                 00550000
                                     ORGANIZATION IS INDEXED            00560000
                                     ACCESS IS DYNAMIC                  00570000
                                     RECORD KEY IS CUSTOMER-KEY         00580000
                                     FILE STATUS IS WMF-CUSTOMR-STATUS. 00590000
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
                                                                        00730000
                                                                        00730000
       FD  VSAM-CUSTOMER                                                01180000
           RECORD CONTAINS 733 CHARACTERS.                              01190000
                                                                        01200000
           COPY VCUSTOMR.                                               01210000
           EJECT                                                        01220000
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

           05  WS-ERROR-FOUND-SW       PIC X(01)             VALUE 'N'.
               88  ERROR-FOUND                               VALUE 'Y'.
               88  NO-ERROR-FOUND                            VALUE 'N'.

           05  WS-PROCESS-COMPLETE-SW  PIC X(01)             VALUE 'N'.
               88  PROCESS-COMPLETE                          VALUE 'Y'.
               88  NOT-PROCESS-COMPLETE                      VALUE 'N'.

           05  WS-END-OF-PARM-FILE-SW  PIC X(01)             VALUE 'N'.
               88  END-OF-PARM-FILE                          VALUE 'Y'.
               88  NOT-END-OF-PARM-FILE                      VALUE 'N'.

           05  WS-PARM-ERROR-FOUND-SW  PIC X(01)             VALUE 'N'.
               88  PARM-ERROR-FOUND                          VALUE 'Y'.
               88  NOT-PARM-ERROR-FOUND                      VALUE 'N'.

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
           05  WMF-PER-ORDER-FEE       PIC 9(7)V99 VALUE 6.75  COMP-3.
           05  WMF-CUSTOMR-STATUS      PIC X(02)   VALUE SPACES.

           05  WMF-ACTIVE-SCENARIOS    PIC X(250)  VALUE SPACES.
           05  WMF-ACTIVE-SCENARIOS-R  REDEFINES WMF-ACTIVE-SCENARIOS
                                       OCCURS 250 TIMES
                                       PIC X(01).

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
      *  FORMATTTED PRINT / DISPLAY LINES                             *
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
                   15 FILLER            PIC X(09).
                   15 WDL-ALPHANUM-LEN08
                                        PIC X(08).
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
               88  WPR-SCENARIO        VALUE 'S'.
               88  WPR-USERID          VALUE 'U'.
           05  FILLER                  PIC X(01).
           05  WPR-RECORD-DATA         PIC X(78).
           05  WPR-RECORD-DATA-USERID  REDEFINES WPR-RECORD-DATA.
               10  WPR-USERID-VALUE    PIC X(08).
               10  FILLER              PIC X(70).
           05  WPR-RECORD-DATA-SCENARIO
                                       REDEFINES WPR-RECORD-DATA.
               10  WPR-SCENARIO-NUMBER PIC X(03).
               10  WPR-SCENARIO-NUMBER-R
                                       REDEFINES WPR-SCENARIO-NUMBER
                                       PIC 9(03).
               10  FILLER              PIC X(75).


      *****************************************************************
      *    PARAMETER RECORD ARRAY                                     *
      *****************************************************************
       01  WS-PARAMETER-RECORD-ARRAY.
           05  WPRA-RECORD             OCCURS 500 TIMES
                                       PIC X(80).

      *****************************************************************
      *    VSAM CUSTOMER RECORD LAYOUT                                *
      *****************************************************************

      *****COPY VCUSTOMR.
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
      *    MQSERIES MISCELLANEOUS APPLICATION FIELDS / VARIABLES      *
      *****************************************************************

       01  MQS-MISCELLANEOUS.
           05  MQS-HCONN               PIC S9(9)  BINARY  VALUE +0.
           05  MQS-HOBJECT             PIC S9(9)  BINARY  VALUE +0.
           05  MQS-HOBJECT-CREDIT-AUTH-RESP-Q
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

           05  MQS-CUSTOMER-INIT-QUEUE PIC X(48)          VALUE
               'PDAPROD.BATCH.CUSTOMER.INIT.QUEUE'.

           05  MQS-CREDIT-AUTH-RESP-QUEUE
                                       PIC X(48)          VALUE
               'PDAPROD.H01AC013.CREDIT.AUTH.RESP.QUEUE'.


      *****************************************************************
      *    MQSERIES MESSAGE PUT ON CUSTOMER QUEUE TO TRIGGER PDAB17   *
      *****************************************************************

       01  MQS-BUFFER-OUT              PIC X(300)         VALUE SPACES.

       01  MQS-CUSTOMER-MESSAGE        REDEFINES MQS-BUFFER-OUT.
           05  MQS-CUSTOMER-USERID     PIC X(08).
           05  MQS-CUSTOMER-ID         PIC X(32).
           05  MQS-CUSTOMER-ORDER-FEE  PIC 9(7)V99.
           05  MQS-CUSTOMER-SCENARIOS  PIC X(250).
           05  FILLER                  PIC X(01).


      *****************************************************************
      *    MQSERIES MESSAGE RECEIVED ON RESULTS QUEUE                 *
      *    (CUSTOMER ORDER QUERY INFORMATION)                         *
      *****************************************************************

       01  MQS-BUFFER-IN               PIC X(1000)        VALUE SPACES.

       01  MQS-RESULTS-MESSAGE         REDEFINES MQS-BUFFER-IN.
           05  MQS-RETURN-CODE         PIC X(01).
               88  MQS-NO-ERROR                           VALUE '0'.
               88  MQS-ERROR                              VALUE '1'.
               88  MQS-FATAL-ERROR                        VALUE '9'.
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
           EJECT

      *****************************************************************
      *    CUSTOMER CREDIT AUTHORIZATION REQUEST / RESPONSE MESSAGE   *
      *****************************************************************

       01  MQS-CREDIT-AUTH-REQ-MESSAGE REDEFINES MQS-BUFFER-IN.
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

       01  WS-PDAB16-MESSAGES.

           05  WPM-BLANK               PIC X(01)       VALUE     ' '.
           05  WPM-ALL-ASTERISK        PIC X(80)       VALUE ALL '*'.

           05  WPM-BEGIN-PROGRAM.
               10 FILLER               PIC X(78)   VALUE
                  '***** BEGIN PROGRAM PDAB16 *****'.

           05  WPM-END-PROGRAM.
               10 FILLER               PIC X(78)   VALUE
                  '***** END PROGRAM PDAB16 *****'.

           05  WPM-VSAM-ERROR.
               10 FILLER               PIC X(21)   VALUE
                  'VSAM ERROR ON FILE - '.
               10 WPM-VSAM-ERROR-FILE  PIC X(09)   VALUE SPACES.
               10 FILLER               PIC X(15)   VALUE
                  ',FILE STATUS = '.
               10 WPM-VSAM-ERROR-STATUS
                                       PIC X(02)   VALUE SPACES.
               10 FILLER               PIC X(12)   VALUE
                  ', COMMAND = '.
               10 WPM-VSAM-ERROR-COMMAND
                                       PIC X(19)   VALUE SPACES.

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
                  'POSITION 1 - RECORD TYPE MUST BE U OR S'.

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

           05  WPM-INVALID-SCENARIO-NUMBER.
               10 FILLER               PIC X(78)   VALUE
                  'POSITION 3 - 5, SCENARIO NUMBER MUST BE NUMERIC, VALU
      -           'E 1 THRU 250'.

           EJECT

      *****************************************************************
      *    L I N K A G E     S E C T I O N                            *
      *****************************************************************

       LINKAGE SECTION.

           EJECT

      *****************************************************************
      *    P R O C E D U R E    D I V I S I O N                       *
      *****************************************************************

       PROCEDURE DIVISION.


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00000-MAINLINE                                *
      *                                                               *
      *    FUNCTION :  PROGRAM ENTRY, CONTROL HIGH LEVEL PROCESSING   *
      *                FOR THE PRODUCT DEMONSTRATION APPLICATION      *
      *                CUSTOMER ORDER INQUIRY                         *
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
           PERFORM  P00100-END-OF-JOB
               THRU P00100-END-OF-JOB-EXIT.


           DISPLAY WPM-BLANK.
           DISPLAY WPM-ALL-ASTERISK.
           DISPLAY WPM-END-PROGRAM.
           DISPLAY WPM-ALL-ASTERISK.

           GOBACK.

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

           MOVE 'N'                    TO WS-ERROR-FOUND-SW
                                          WS-END-OF-PARM-FILE-SW
                                          WS-PROCESS-COMPLETE-SW.
                                                                        00010000
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME.          00020001


      *****************************************************************
      *    OPEN FILES, VERIFY SUCCESSFUL VSAM FILE OPENS              *
      *****************************************************************

           OPEN INPUT    INPUT-PARAMETERS                               00020001
                         VSAM-CUSTOMER.                                 00020001

           IF WMF-CUSTOMR-STATUS = '00'                                 00020001
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB16'           TO WPGE-PROGRAM-ID
               MOVE 'P00050'           TO WPGE-PARAGRAPH
               MOVE 'VCUSTOMR'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-CUSTOMR-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'OPEN'             TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    PERFORM 1ST READ ON PARAMETER FILE -- EOF IS AN ERROR      *
      *****************************************************************

           PERFORM  P80000-READ-PARAMETERS
               THRU P80000-READ-PARAMETERS-EXIT.

           IF END-OF-PARM-FILE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB16'           TO WPGE-PROGRAM-ID
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
      *    PARAGRAPH:  P00100-END-OF-JOB                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PERFORM NORMAL END OF PROGRAM       *
      *                OPERATIONS, I.E. CLOSE FILES, ETC.             *
      *                                                               *
      *    CALLED BY:  P00000-MAINLINE                                *
      *                                                               *
      *****************************************************************

       P00100-END-OF-JOB.
           DISPLAY 'P00100-END-OF-JOB'.

      *****************************************************************
      *    CLOSE FILES, VERIFY SUCCESSFUL VSAM FILE CLOSURES          *
      *****************************************************************

           CLOSE  INPUT-PARAMETERS                                      00020001
                  VSAM-CUSTOMER.                                        00020001


           IF WMF-CUSTOMR-STATUS = '00'                                 00020001
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB16'           TO WPGE-PROGRAM-ID
               MOVE 'P00100'           TO WPGE-PARAGRAPH
               MOVE 'VCUSTOMR'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-CUSTOMR-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'CLOSE'            TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    DISCONNECT FROM THE MQSERIES QUEUE MANAGER                 *
      *****************************************************************

           IF MQS-HCONN                > ZEROES
               PERFORM  P07030-MQS-DISCONNECT
                   THRU P07030-MQS-DISCONNECT-EXIT.

           DISPLAY 'P00100-END-OF-JOB-EXIT'.
       P00100-END-OF-JOB-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00500-MAIN-PROCESS                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL PDAB16 HIGH LEVEL PROCESSES *
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


      *****************************************************************
      *    PROCESS THE SELECTED CUSTOMER FOR THE QUERY                *
      *****************************************************************

           PERFORM  P01000-CUSTOMER-PROCESS
               THRU P01000-CUSTOMER-PROCESS-EXIT.

           IF ERROR-FOUND
               GO TO P00500-MAIN-PROCESS-EXIT.


      *****************************************************************
      *    INITIATE THE MQSERIES CUSTOMER ORDER QUERY                 *
      *****************************************************************

           PERFORM  P05000-ORDER-QUERY
               THRU P05000-ORDER-QUERY-EXIT.

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
      *                PARAMETER CONTENT                               *
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
               MOVE WPM-USERID-PARM-REQUIRED
                                       TO WMF-MESSAGE-AREA
               PERFORM  P99400-ERROR-ROUTINE
                   THRU P99400-ERROR-ROUTINE-EXIT.

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
               MOVE 'PDAB16'           TO WPGE-PROGRAM-ID
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
      *    S = SCENARIO NUMBER SPECIFICATION                          *
      *****************************************************************

           IF WPR-SCENARIO      OR
              WPR-USERID
               NEXT SENTENCE
           ELSE                                                         00020001
               MOVE WPM-PARM-INVALID-RECORD-TYPE
                                       TO WMF-MESSAGE-AREA
               PERFORM  P00700-PARM-ERROR
                   THRU P00700-PARM-ERROR-EXIT.

      *****************************************************************
      *    FOR ACTION S= SCENARIO,                                    *
      *    A 3 POSITION NUMERIC SCENARIO NUMBER IS REQUIRED           *
      *****************************************************************

           IF WPR-SCENARIO
               IF (WPR-SCENARIO-NUMBER NUMERIC)    AND
                  (WPR-SCENARIO-NUMBER-R > 0)      AND
                  (WPR-SCENARIO-NUMBER-R < 251)
                   MOVE 'Y'            TO WMF-ACTIVE-SCENARIOS-R
                                             (WPR-SCENARIO-NUMBER-R)
               ELSE
                   MOVE WPM-INVALID-SCENARIO-NUMBER
                                       TO WMF-MESSAGE-AREA
                   PERFORM  P00700-PARM-ERROR
                       THRU P00700-PARM-ERROR-EXIT
           ELSE                                                         00020001
                   NEXT SENTENCE.


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

           MOVE 'Y'                    TO WS-ERROR-FOUND-SW.

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
      *    PARAGRAPH:  P01000-CUSTOMER-PROCESS                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO VALIDATE THE CUSTOMER FOR THE QUERY.*
      *                -ARROW- HAS BEEN ARBITRARILY SELECTED FOR THE  *
      *                QUERY.                                         *
      *                                                               *
      *    CALLED BY:  P00500-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P01000-CUSTOMER-PROCESS.
           DISPLAY 'P01000-CUSTOMER-PROCESS'.

      *****************************************************************
      *    READ CUSTOMER USING THE APPROPRIATE KEY (VSAM READ)        *
      *****************************************************************

           MOVE ZEROES                 TO CUSTOMER-PREFIX.
           MOVE WMF-CUSTOMER-ID        TO CUSTOMER-ID.
                                                                        13580000
           PERFORM  P80600-READ-CUSTOMER
               THRU P80600-READ-CUSTOMER-EXIT.

      *****************************************************************
      *    READ SUCCESSFUL (00) IS THE ONLY ACCEPTABLE STATUS,        *
      *    OTHERWISE FORMAT ERROR AND TERMINATE PROGRAM               *
      *****************************************************************

           IF WMF-CUSTOMR-STATUS = '00'                                 00020001
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB16'           TO WPGE-PROGRAM-ID
               MOVE 'P01000'           TO WPGE-PARAGRAPH
               MOVE 'VCUSTOMR'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-CUSTOMR-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'READ'             TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

           DISPLAY 'P01000-CUSTOMER-PROCESS-EXIT'.
       P01000-CUSTOMER-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05000-ORDER-QUERY                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE CUSTOMER ORDER INQUIRY  *
      *                PROCESS. PROGRAM REQUESTS CUSTOMER ORDER       *
      *                INFORMATION ON ONE MQSERIES QUEUE AND THEN     *
      *                PROCESSES THE RESPONSE FROM PROGRAM PDAB17     *
      *                ON ANOTHER QUEUE                               *
      *                                                               *
      *    CALLED BY:  P00500-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P05000-ORDER-QUERY.
           DISPLAY 'P05000-ORDER-QUERY'.

      *****************************************************************
      *    CONNECT TO THE MQSERIES QUEUE MANAGER                      *
      *****************************************************************

           PERFORM  P07000-MQS-CONNECT
               THRU P07000-MQS-CONNECT-EXIT.

           IF ERROR-FOUND
               GO TO P05000-ORDER-QUERY-EXIT.

      *****************************************************************
      *    REQUEST CUSTOMER ORDER INFORMATION VIA MQSERIES MESSAGE    *
      *    (PDAB17 WILL PROCESS THE MESSAGE AND PERFORM THE QUERY)    *
      *****************************************************************

           PERFORM  P05100-ORDER-INQ-REQUEST
               THRU P05100-ORDER-INQ-REQUEST-EXIT.


      *****************************************************************
      *    PROCESS RESULTS FROM THE ORDER INQUIRY REQUEST             *
      *    (MODULE PDAB17 PLACES QUERY RESULTS ON ANOTHER QUEUE)      *
      *****************************************************************

           PERFORM  P05500-ORDER-INQ-RESPONSE
               THRU P05500-ORDER-INQ-RESPONSE-EXIT.

           IF ERROR-FOUND
               GO TO P05000-ORDER-QUERY-EXIT.

      *****************************************************************
      *    PROCESS RESULTS FROM THE CUSTOMER CREDIT CHECK REQUEST     *
      *    (MODULE PDAB17 PLACES CREDIT REQUEST ON ANOTHER QUEUE)     *
      *****************************************************************

           PERFORM  P05800-CREDIT-RESULTS
               THRU P05800-CREDIT-RESULTS-EXIT.


           DISPLAY 'P05000-ORDER-QUERY-EXIT'.
       P05000-ORDER-QUERY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05100-ORDER-INQ-REQUEST                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO TRIGGER THE CUSTOMER ORDER INQUIRY  *
      *                PROCESSING PROGRAM (PDAB17) VIA A WRITE TO     *
      *                THE MQSERIES QUEUE. THE RESULTS ARE RETRIEVED  *
      *                FROM THE MQSERIES RESPONSE QUEUE.              *
      *                                                               *
      *    CALLED BY:  P05000-ORDER-QUERY                             *
      *                                                               *
      *****************************************************************

       P05100-ORDER-INQ-REQUEST.
           DISPLAY 'P05100-ORDER-INQ-REQUEST'.

      *****************************************************************
      *    QUERY REQUEST QUEUE NAME IS USERID SPECIFIC, PREFIXED BY   *
      *    USERID. FORMAT QUEUE NAME USING PARAMETER INPUT USERID.    *
      *****************************************************************

           MOVE WMF-USERID             TO MQS-USERID.
           MOVE SPACES                 TO MQS-CUSTOMER-QUEUE-COMPRESS.
           MOVE ZEROES                 TO WS-SUB2.

           PERFORM  P05130-FORMAT-Q-NAME
               THRU P05130-FORMAT-Q-NAME-EXIT
                   VARYING WS-SUB1 FROM +1 BY +1
                       UNTIL WS-SUB1 > WS-QUEUE-NAME-LTH.

           DISPLAY 'MQS-CUSTOMER-QUEUE-COMPRESS = '.
           DISPLAY  MQS-CUSTOMER-QUEUE-COMPRESS.

      *****************************************************************
      *    OPEN THE MQSERIES CUSTOMER QUEUE FOR OUTPUT                *
      *****************************************************************
           MOVE MQOD-CURRENT-VERSION   TO MQOD-VERSION.

           MOVE MQOT-Q                 TO MQOD-OBJECTTYPE.
           MOVE 'QUEUE'                TO MQS-OBJECTTYPE-DESC.
           MOVE MQS-CUSTOMER-QUEUE-COMPRESS
                                       TO MQOD-OBJECTNAME.
           COMPUTE MQS-OPTIONS         =  MQOO-OUTPUT           +
                                          MQOO-PASS-ALL-CONTEXT +
                                          MQOO-FAIL-IF-QUIESCING.
           MOVE ZEROES                 TO MQS-HOBJECT
                                          MQS-COMPCODE
                                          MQS-REASONCODE.

           PERFORM P07100-MQS-OPEN
              THRU P07100-MQS-OPEN-EXIT.


      *****************************************************************
      *    FORMAT AND WRITE THE MQSERIES MSG TO THE CUSTOMER QUEUE    *
      *    (MESSAGE TRIGGERS THE CUSTOMER PROCESSING PROGRAM (PDAB17) *
      *****************************************************************

           MOVE MQMD-CURRENT-VERSION   TO MQMD-VERSION.
           MOVE MQRO-NONE              TO MQMD-REPORT.
           MOVE MQPER-NOT-PERSISTENT   TO MQMD-PERSISTENCE.
           MOVE MQMI-NONE              TO MQMD-MSGID.
           MOVE MQCI-NONE              TO MQMD-CORRELID.
           MOVE MQPRI-PRIORITY-AS-Q-DEF
                                       TO MQMD-PRIORITY.
           MOVE MQENC-NATIVE           TO MQMD-ENCODING.
           MOVE MQCCSI-Q-MGR           TO MQMD-CODEDCHARSETID.
           MOVE 5000                   TO MQMD-EXPIRY.


           MOVE MQPMO-CURRENT-VERSION  TO MQPMO-VERSION.
           COMPUTE MQPMO-OPTIONS       =  MQPMO-NO-SYNCPOINT    +
                                          MQPMO-DEFAULT-CONTEXT +
                                          MQPMO-FAIL-IF-QUIESCING.
           MOVE LENGTH OF MQS-BUFFER-OUT
                                       TO MQS-BUFFERLENGTH.
           MOVE WMF-USERID             TO MQS-CUSTOMER-USERID.
           MOVE WMF-CUSTOMER-ID        TO MQS-CUSTOMER-ID.
           MOVE WMF-PER-ORDER-FEE      TO MQS-CUSTOMER-ORDER-FEE.
           MOVE WMF-ACTIVE-SCENARIOS   TO MQS-CUSTOMER-SCENARIOS.


           PERFORM P07200-MQS-PUT
              THRU P07200-MQS-PUT-EXIT.

           MOVE MQMD-MSGID             TO MQS-MSGID.


      *****************************************************************
      *    CLOSE THE MQSERIES CUSTOMER QUEUE                           *
      *****************************************************************

           COMPUTE MQS-OPTIONS         =  MQCO-NONE.

           PERFORM P07300-MQS-CLOSE
              THRU P07300-MQS-CLOSE-EXIT.


           DISPLAY 'P05100-ORDER-INQ-REQUEST-EXIT'.
       P05100-ORDER-INQ-REQUEST-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05130-FORMAT-Q-NAME                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO COMPRESS ANY BLANKS FROM THE        *
      *                CUSTOMER REQUEST QUEUE NAME. SITUATION OCCURS  *
      *                IF THE USERID PREFIX IS LESS THAN 8 CHARACTERS.*
      *                                                               *
      *    CALLED BY:  P05100-ORDER-INQ-REQUEST-EXIT                  *
      *                                                               *
      *****************************************************************

       P05130-FORMAT-Q-NAME.

      *****************************************************************
      *    MOVE ONLY NON-BLANK CHARACTERS FROM SOURCE TO TARGET       *
      *****************************************************************

           IF MQS-A-BYTE-01 (WS-SUB1)  NOT EQUAL TO SPACES
               ADD +1                  TO WS-SUB2
               MOVE MQS-A-BYTE-01 (WS-SUB1)
                                       TO MQS-A-BYTE-02 (WS-SUB2)
           ELSE
               NEXT SENTENCE.

       P05130-FORMAT-Q-NAME-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05500-ORDER-INQ-RESPONSE                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS THE RESULTS OF THE ORDER    *
      *                INQUIRY REQUEST. EITHER ALL ORDER INFORMATON   *
      *                FOR THE SELECTED CUSTOMER OR ERROR INFORMATON  *
      *                IS RETURNED IN THE CUSTOMER RESPONSE QUEUE.    *
      *                                                               *
      *    CALLED BY:  P05000-ORDER-QUERY                             *
      *                                                               *
      *****************************************************************

       P05500-ORDER-INQ-RESPONSE.
           DISPLAY 'P05500-ORDER-INQ-RESPONSE'.

           MOVE MQOD-CURRENT-VERSION   TO MQOD-VERSION.

      *****************************************************************
      *    OPEN THE MQSERIES CUSTOMER RESPONSE QUEUE FOR INPUT        *
      *****************************************************************

           MOVE MQOT-Q                 TO MQOD-OBJECTTYPE.
           MOVE 'QUEUE'                TO MQS-OBJECTTYPE-DESC.
           MOVE MQS-CUSTOMER-RESPONSE-QALIAS
                                       TO MQOD-OBJECTNAME.
           COMPUTE MQS-OPTIONS         =  MQOO-INPUT-SHARED      +
                                          MQOO-SAVE-ALL-CONTEXT  +
                                          MQOO-FAIL-IF-QUIESCING.
           MOVE ZEROES                 TO MQS-HOBJECT
                                          MQS-COMPCODE
                                          MQS-REASONCODE.

           PERFORM P07100-MQS-OPEN
              THRU P07100-MQS-OPEN-EXIT.


      *****************************************************************
      *    READ THE RESPONSE QUEUE MESSAGE FROM PROGRAM PDAB17        *
      *    (RESULTS FROM THE ORDER INQUIRY REQUEST MESSAGE)           *
      *****************************************************************

           MOVE MQMD-CURRENT-VERSION   TO MQMD-VERSION.
           MOVE MQRO-NONE              TO MQMD-REPORT.
           MOVE MQPER-NOT-PERSISTENT   TO MQMD-PERSISTENCE.
           MOVE MQS-MSGID              TO MQMD-MSGID.
           MOVE MQCI-NONE              TO MQMD-CORRELID.
           MOVE MQENC-NATIVE           TO MQMD-ENCODING.
           MOVE MQCCSI-Q-MGR           TO MQMD-CODEDCHARSETID.


           MOVE MQGMO-CURRENT-VERSION  TO MQGMO-VERSION.
           COMPUTE MQGMO-OPTIONS       =  MQGMO-WAIT              +
                                          MQGMO-CONVERT           +
                                          MQGMO-FAIL-IF-QUIESCING +
                                          MQGMO-NO-SYNCPOINT.
           MOVE 120000                 TO MQGMO-WAITINTERVAL.
           MOVE MQMO-MATCH-MSG-ID      TO MQGMO-MATCHOPTIONS.
           MOVE LENGTH OF MQS-BUFFER-IN
                                       TO MQS-BUFFERLENGTH.

           PERFORM P07400-MQS-GET
              THRU P07400-MQS-GET-EXIT.

           IF MQS-REASONCODE           =  MQRC-NO-MSG-AVAILABLE
               MOVE WPM-NO-MSG-AVAILABLE
                                       TO WMF-MESSAGE-AREA
               PERFORM  P99400-ERROR-ROUTINE
                   THRU P99400-ERROR-ROUTINE-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    CLOSE THE MQSERIES CUSTOMER RESPONSE QUEUE                  *
      *****************************************************************

           COMPUTE MQS-OPTIONS         =  MQCO-NONE.

           PERFORM P07300-MQS-CLOSE
              THRU P07300-MQS-CLOSE-EXIT.


      *****************************************************************
      *    PROCESS THE RESULTS OF THE CUSTOMER ORDER INQUIRY           *
      *    (EITHER SUCCESSFUL QUERY, OR ERROR ENCOUNTERED)             *
      *****************************************************************

           IF NO-ERROR-FOUND
               PERFORM P06000-PROCESS-RESULTS
                  THRU P06000-PROCESS-RESULTS-EXIT.


           DISPLAY 'P05500-ORDER-INQ-RESPONSE-EXIT'.
       P05500-ORDER-INQ-RESPONSE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05800-CREDIT-RESULTS                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS THE RESULTS OF THE CUSTOMER *
      *                CREDIT AUTHORIZATION REQUEST. PROGRAM          *
      *                PDAB17 PLACES THE CREDIT REQUEST ON THE        *
      *                MQSERIES QUEUE.                                *
      *                                                               *
      *    CALLED BY:  P05000-ORDER-QUERY                             *
      *                                                               *
      *****************************************************************

       P05800-CREDIT-RESULTS.
           DISPLAY 'P05800-CREDIT-RESULTS'.

           MOVE MQOD-CURRENT-VERSION   TO MQOD-VERSION.

      *****************************************************************
      *    OPEN THE MQSERIES CUSTOMER CREDIT AUTH RESPONSE Q FOR INPUT*
      *****************************************************************

           MOVE MQOT-Q                 TO MQOD-OBJECTTYPE.
           MOVE 'QUEUE'                TO MQS-OBJECTTYPE-DESC.
           MOVE MQS-CREDIT-AUTH-RESP-QUEUE
                                       TO MQOD-OBJECTNAME.
           COMPUTE MQS-OPTIONS         =  MQOO-INPUT-SHARED      +
                                          MQOO-SAVE-ALL-CONTEXT  +
                                          MQOO-FAIL-IF-QUIESCING.
           MOVE ZEROES                 TO MQS-HOBJECT
                                          MQS-COMPCODE
                                          MQS-REASONCODE.

           PERFORM P07100-MQS-OPEN
              THRU P07100-MQS-OPEN-EXIT.


      *****************************************************************
      *    READ THE RESPONSE QUEUE MESSAGE FROM PROGRAM PDAB17        *
      *    (RESULTS FROM THE CUSTOMER CREDIT AUTH REQUEST MESSAGE)    *
      *****************************************************************

           MOVE MQMD-CURRENT-VERSION   TO MQMD-VERSION.
           MOVE MQRO-NONE              TO MQMD-REPORT.
           MOVE MQPER-NOT-PERSISTENT   TO MQMD-PERSISTENCE.
           MOVE MQS-MSGID              TO MQMD-MSGID.
           MOVE MQCI-NONE              TO MQMD-CORRELID.
           MOVE MQENC-NATIVE           TO MQMD-ENCODING.
           MOVE MQCCSI-Q-MGR           TO MQMD-CODEDCHARSETID.


           MOVE MQGMO-CURRENT-VERSION  TO MQGMO-VERSION.
           COMPUTE MQGMO-OPTIONS       =  MQGMO-WAIT              +
                                          MQGMO-CONVERT           +
                                          MQGMO-FAIL-IF-QUIESCING +
                                          MQGMO-NO-SYNCPOINT.
           MOVE 120000                 TO MQGMO-WAITINTERVAL.
           MOVE MQMO-MATCH-MSG-ID      TO MQGMO-MATCHOPTIONS.
           MOVE LENGTH OF MQS-BUFFER-IN
                                       TO MQS-BUFFERLENGTH.

           PERFORM P07400-MQS-GET
              THRU P07400-MQS-GET-EXIT.

           IF MQS-REASONCODE           =  MQRC-NO-MSG-AVAILABLE
               MOVE SPACES             TO MQS-BUFFER-IN
               MOVE ZEROES             TO MQS-CREDIT-RETURN-CODE
               MOVE 'U'                TO MQS-CREDIT-AUTH
           ELSE
               NEXT SENTENCE.

      *****************************************************************
      *    CLOSE THE MQSERIES CUSTOMER CREDIT AUTH QUEUE               *
      *****************************************************************

           COMPUTE MQS-OPTIONS         =  MQCO-NONE.

           PERFORM P07300-MQS-CLOSE
              THRU P07300-MQS-CLOSE-EXIT.


      *****************************************************************
      *    PROCESS THE RESULTS OF THE CUSTOMER CREDIT AUTHORIZATION   *
      *    (EITHER SUCCESSFUL QUERY, OR ERROR ENCOUNTERED)            *
      *****************************************************************

           IF NO-ERROR-FOUND
               PERFORM P06400-PROCESS-CR-RESULTS
                  THRU P06400-PROCESS-CR-RESULTS-EXIT.


           DISPLAY 'P05800-CREDIT-RESULTS-EXIT'.
       P05800-CREDIT-RESULTS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06000-PROCESS-RESULTS                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INTERROGATE THE RESULTS OF THE      *
      *                MQSERIES ORDER QUERY.                          *
      *                                                               *
      *                IF SUCCESSFUL, DISPLAY CUSTOMER ORDER DATA     *
      *                                                               *
      *                IF ERROR, DISPLAY ERROR INFO TEXT AND          *
      *                TERMINATE THE PROGRAM                          *
      *                                                               *
      *    CALLED BY:  P05500-ORDER-INQ-RESPONSE                      *
      *                                                               *
      *****************************************************************

       P06000-PROCESS-RESULTS.
           DISPLAY 'P06000-PROCESS-RESULTS'.

      *****************************************************************
      *    IF FATAL ERROR ENCOUNTERED, FORMAT ERROR INFO INTO COMMON  *
      *    PDA ERROR ROUTINE, TERMINATE PROGRAM                       *
      *****************************************************************

           IF MQS-FATAL-ERROR
               MOVE MQS-PDA-ERROR-TYPE TO WS-PDA-ERROR-TYPE
               IF PDA-DB2-ERROR
                   MOVE MQS-PDA-ERROR-LINE-01
                                       TO WS-PDA-DB2-ERROR-01
                   MOVE MQS-PDA-ERROR-LINE-02
                                       TO WS-PDA-DB2-ERROR-02
                   PERFORM P99500-PDA-ERROR
                      THRU P99500-PDA-ERROR-EXIT
               ELSE
               IF PDA-IMS-ERROR
                   MOVE MQS-PDA-ERROR-LINE-01
                                       TO WS-PDA-IMS-ERROR-01
                   MOVE MQS-PDA-ERROR-LINE-02
                                       TO WS-PDA-IMS-ERROR-02
                   PERFORM P99500-PDA-ERROR
                      THRU P99500-PDA-ERROR-EXIT
               ELSE
               IF PDA-MQSERIES-ERROR
                   MOVE MQS-PDA-ERROR-LINE-01
                                       TO WS-PDA-MQSERIES-ERROR-01
                   MOVE MQS-PDA-ERROR-LINE-02
                                       TO WS-PDA-MQSERIES-ERROR-02
                   PERFORM P99500-PDA-ERROR
                      THRU P99500-PDA-ERROR-EXIT
               ELSE
                   MOVE MQS-PDA-ERROR-LINE-01
                                       TO WS-PDA-GEN-ERROR-01
                   MOVE MQS-PDA-ERROR-LINE-02
                                       TO WS-PDA-GEN-ERROR-02
                   PERFORM P99500-PDA-ERROR
                      THRU P99500-PDA-ERROR-EXIT
           ELSE
                   NEXT SENTENCE.


      *****************************************************************
      *    OTHERWISE PRINT / DISPLAY CUSTOMER ORDER INFORMATION       *
      *****************************************************************

           DISPLAY WPM-ALL-ASTERISK.
           DISPLAY '***** BEGIN CUSTOMER ORDER QUERY RESULTS *****'.
           DISPLAY WPM-ALL-ASTERISK.
           DISPLAY ' '.

           MOVE 'TOTAL ORDERS '        TO WDL-LITERAL-01.
           MOVE SPACES                 TO WDL-FIELD-01.
           MOVE MQS-TOTAL-ORDERS       TO WDL-NUM-02.
           DISPLAY WDL-LINE-01.

           MOVE 'TOTAL DOLLAR AMOUNT ' TO WDL-LITERAL-01.
           MOVE SPACES                 TO WDL-FIELD-01.
           MOVE MQS-TOTAL-DOLLAR-AMOUNT
                                       TO WDL-NUM-01.
           DISPLAY WDL-LINE-01.

           MOVE 'AVG. DOLLAR AMOUNT '  TO WDL-LITERAL-01.
           MOVE SPACES                 TO WDL-FIELD-01.
           MOVE MQS-AVG-DOLLAR-AMOUNT  TO WDL-NUM-01.
           DISPLAY WDL-LINE-01.
           DISPLAY ' '.


           IF MQS-LAST-ORDER-NUMBER    >  SPACES
               MOVE 'LAST ORDER DATE ' TO WDL-LITERAL-01
               MOVE SPACES             TO WDL-FIELD-01
               MOVE MQS-LAST-ORDER-DATE
                                       TO WMF-DATE-1
               MOVE WMF-DATE-1-MM      TO WMF-DATE-2-MM
               MOVE WMF-DATE-1-DD      TO WMF-DATE-2-DD
               MOVE WMF-DATE-1-YY      TO WMF-DATE-2-YY
               MOVE WMF-DATE-2         TO WDL-ALPHANUM-LEN08
               DISPLAY WDL-LINE-01

               MOVE 'LAST ORDER AMOUNT '
                                       TO WDL-LITERAL-01
               MOVE SPACES             TO WDL-FIELD-01
               MOVE MQS-LAST-ORDER-AMOUNT
                                       TO WDL-NUM-01
               DISPLAY WDL-LINE-01

               MOVE 'LAST ORDER NUMBER '
                                       TO WDL-LITERAL-01
               MOVE SPACES             TO WDL-FIELD-01
               MOVE MQS-LAST-ORDER-NUMBER
                                       TO WDL-ALPHANUM-LEN10
               DISPLAY WDL-LINE-01
               DISPLAY ' '.


      *****************************************************************
      *    PROCESS THE ORDER DETAIL                                   *
      *****************************************************************

           PERFORM  P06100-FMT-ORDER-DETAIL
               THRU P06100-FMT-ORDER-DETAIL-EXIT
                   VARYING WS-SUB1 FROM +1 BY +1
                       UNTIL WS-SUB1 >  WS-ORDER-MAX.


           IF MQS-SCREEN-MESSAGE       >  SPACES
               DISPLAY 'MESSAGE RETURNED: '
               DISPLAY MQS-SCREEN-MESSAGE.

           DISPLAY ' '.
           DISPLAY WPM-ALL-ASTERISK.
           DISPLAY '***** END CUSTOMER ORDER QUERY RESULTS *****'.
           DISPLAY WPM-ALL-ASTERISK.

           DISPLAY 'P06000-PROCESS-RESULTS-EXIT'.
       P06000-PROCESS-RESULTS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06100-FMT-ORDER-DETAIL                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO FORMAT AND DISPLAY THE ORDER DETAIL *
      *                FROM THE MQSERIES MESSAGE AREA.                *
      *                                                               *
      *    CALLED BY:  P06000-PROCESS-RESULTS                         *
      *                                                               *
      *****************************************************************

       P06100-FMT-ORDER-DETAIL.

           IF MQS-ORDER-NUMBER (WS-SUB1)  >  SPACES

               DISPLAY ' '
               MOVE 'ORDER NUMBER '    TO WDL-LITERAL-01
               MOVE SPACES             TO WDL-FIELD-01
               MOVE MQS-ORDER-NUMBER (WS-SUB1)
                                       TO WDL-ALPHANUM-LEN10
               DISPLAY WDL-LINE-01

               MOVE 'ORDER AMOUNT '    TO WDL-LITERAL-01
               MOVE SPACES             TO WDL-FIELD-01
               MOVE MQS-ORDER-AMOUNT (WS-SUB1)
                                       TO WDL-NUM-01
               DISPLAY WDL-LINE-01.


       P06100-FMT-ORDER-DETAIL-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06400-PROCESS-CR-RESULTS                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INTERROGATE THE RESULTS OF THE      *
      *                MQSERIES CREDIT AUTHORIZATION QUERY.           *
      *                                                               *
      *                IF SUCCESSFUL, FORMAT / DISPLAY CUSTOMER       *
      *                CREDIT STATUS                                  *
      *                                                               *
      *                IF ERROR, FORMAT / DISPLAY ERROR TEXT AND      *
      *                TERMINATE THE PROGRAM                          *
      *                                                               *
      *    CALLED BY:  P05800-CREDIT-RESULTS                          *
      *                                                               *
      *****************************************************************

       P06400-PROCESS-CR-RESULTS.
           DISPLAY 'P06400-PROCESS-CR-RESULTS'.

      *****************************************************************
      *    IF FATAL ERROR ENCOUNTERED, FORMAT ERROR INFO INTO COMMON  *
      *    PDA ERROR ROUTINE, TERMINATE PROGRAM                       *
      *****************************************************************

           IF MQS-CREDIT-FATAL-ERROR
               MOVE MQS-CREDIT-ERROR-TYPE
                                       TO WS-PDA-ERROR-TYPE
               IF PDA-DB2-ERROR
                   MOVE MQS-CREDIT-ERROR-LINE-01
                                       TO WS-PDA-DB2-ERROR-01
                   MOVE MQS-CREDIT-ERROR-LINE-02
                                       TO WS-PDA-DB2-ERROR-02
                   PERFORM P99500-PDA-ERROR
                      THRU P99500-PDA-ERROR-EXIT
               ELSE
               IF PDA-IMS-ERROR
                   MOVE MQS-CREDIT-ERROR-LINE-01
                                       TO WS-PDA-IMS-ERROR-01
                   MOVE MQS-CREDIT-ERROR-LINE-02
                                       TO WS-PDA-IMS-ERROR-02
                   PERFORM P99500-PDA-ERROR
                      THRU P99500-PDA-ERROR-EXIT
               ELSE
               IF PDA-MQSERIES-ERROR
                   MOVE MQS-CREDIT-ERROR-LINE-01
                                       TO WS-PDA-MQSERIES-ERROR-01
                   MOVE MQS-CREDIT-ERROR-LINE-02
                                       TO WS-PDA-MQSERIES-ERROR-02
                   PERFORM P99500-PDA-ERROR
                      THRU P99500-PDA-ERROR-EXIT
               ELSE
                   MOVE MQS-CREDIT-ERROR-LINE-01
                                       TO WS-PDA-GEN-ERROR-01
                   MOVE MQS-CREDIT-ERROR-LINE-02
                                       TO WS-PDA-GEN-ERROR-02
                   PERFORM P99500-PDA-ERROR
                      THRU P99500-PDA-ERROR-EXIT
           ELSE
                   NEXT SENTENCE.


      *****************************************************************
      *    OTHERWISE FORMAT CREDIT STATUS FOR PRINT / DISPLAY         *
      *****************************************************************

           DISPLAY WPM-ALL-ASTERISK.
           DISPLAY '***** BEGIN CUSTOMER CREDIT RESULTS *****'.
           DISPLAY WPM-ALL-ASTERISK.
           DISPLAY ' '.

           MOVE 'CREDIT STATUS '       TO WDL-LITERAL-01.

           IF MQS-CREDIT-APPROVED
               MOVE 'APPROVED'         TO WDL-FIELD-01
           ELSE
           IF MQS-CREDIT-REJECTED
               MOVE 'REJECTED'         TO WDL-FIELD-01
           ELSE
               MOVE 'UNAVAILABLE'      TO WDL-FIELD-01.

           DISPLAY WDL-LINE-01.
           DISPLAY ' '.

           DISPLAY WPM-ALL-ASTERISK.
           DISPLAY '***** END CUSTOMER CREDIT RESULTS *****'.
           DISPLAY WPM-ALL-ASTERISK.


           DISPLAY 'P06400-PROCESS-CR-RESULTS-EXIT'.
       P06400-PROCESS-CR-RESULTS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07000-MQS-CONNECT                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONNECT TO THE MQSERIES QUEUE       *
      *                MANAGER                                        *
      *                                                               *
      *    CALLED BY:  P05000-ORDER-QUERY                             *
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
               MOVE 'PDAB16'           TO WPME-PROGRAM-ID
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
               MOVE 'PDAB16'           TO WPME-PROGRAM-ID
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
               MOVE 'PDAB16'           TO WPME-PROGRAM-ID
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
                                       MQS-BUFFER-OUT
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
               MOVE 'PDAB16'           TO WPME-PROGRAM-ID
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
               MOVE 'PDAB16'           TO WPME-PROGRAM-ID
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
                                       MQS-BUFFER-IN
                                       MQS-DATALENGTH
                                       MQS-COMPCODE
                                       MQS-REASONCODE.


      *****************************************************************
      *    CHECK FOR MQSERIES ERROR, IF ERROR ENCOUNTERED FORMAT      *
      *    ERROR MESSAGE, CALL ERROR ROUTINE TO TERMINATE             *
      *****************************************************************

           IF MQS-COMPCODE             =  MQCC-OK
               NEXT SENTENCE
           ELSE
           IF MQS-REASONCODE           =  MQRC-NO-MSG-AVAILABLE
               GO TO P07400-MQS-GET-EXIT
           ELSE
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB16'           TO WPME-PROGRAM-ID
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

       P80000-READ-PARAMETERS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80600-READ-CUSTOMER                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ THE CUSTOMER VSAM FILE         *
      *                                                               *
      *    CALLED BY:  P01000-CUSTOMER-PROCESS                        *
      *                                                               *
      *****************************************************************

       P80600-READ-CUSTOMER.
           DISPLAY 'P80600-READ-CUSTOMER'.

           READ VSAM-CUSTOMER.

      *****************************************************************
      *    READ SUCCESSFUL (00) OR NOT FOUND (23) ARE ACCETABLE,      *
      *    OTHERWISE FORMAT ERROR AND TERMINATE PROGRAM               *
      *****************************************************************

           IF WMF-CUSTOMR-STATUS = '00' OR '23'                         00020001
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB16'           TO WPGE-PROGRAM-ID
               MOVE 'P80600'           TO WPGE-PARAGRAPH
               MOVE 'VCUSTOMR'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-CUSTOMR-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'READ'             TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


           DISPLAY 'P80600-READ-CUSTOMER-EXIT'.
       P80600-READ-CUSTOMER-EXIT.
           EXIT.
           EJECT

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
KCS305                                                                  KCS32005
KCS305     MOVE 'Y'                    TO WS-ERROR-FOUND-SW.            KCS32005
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

           MOVE 'Y'                    TO WS-ERROR-FOUND-SW.

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
AS OF      MOVE 'PDAB16'               TO WPGE-PROGRAM-ID.               AS OF
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