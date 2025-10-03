       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA017.

      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA017                                            *
      * TRANS   :   PD17                                              *
      * MAPSET  :   NONE                                              *
      *                                                               *
      * FUNCTION:   PROGRAM PDA017 IS THE CUSTOMER ORDER INQUIRY      *
      *             PROCESSING PROGRAM. THE PROGRAM IS INVOKED VIA    *
      *             THE MQSERIES / CICS TRIGGER MECHANISM. CICS       *
      *             PROGRAM PDA016 (CUSTOMER ORDER INQUIRY SCREEN)    *
      *             CREATES A MQSERIES MESSAGE TO REQUEST THE         *
      *             CUSTOMER ORDER INQUIRY FOR A SPECIFIC CUSTOMER    *
      *             ID. THE MQSERIES MESSAGE CAUSES A MQSERIES        *
      *             TRIGGER MESSAGE TO BE SPAWNED (VIA THE CKTI       *
      *             TRIGGER MONITOR) WHICH THEN STARTS THIS CICS      *
      *             FUNCTION (TRANSACTION PD17).                      *
      *                                                               *
      *             THE MQSERIES MESSAGE FROM THE APPLICATION QUEUE   *
      *             IS RETRIEVED AND THE CUSTOMER IDENTIFICATION      *
      *             FROM THE MQSERIES MESSAGE IS USED TO PROCESS ALL  *
      *             ORDER INFORMATION FOR THE SELECTED CUSTOMER.      *
      *                                                               *
      *             CUSTOMER ORDER RELATED INFORMATION IS THEN PLACED *
      *             ON A MQSERIES RESPONSE QUEUE TO BE PROCESSED BY   *
      *             THE INITIATING PROGRAM.                           *
      *                                                               *
      *                                                               *
      * FILES   :   ORDER DATABASE   -  IMS/DLI   (READ-ONLY)         *
      *     ******** USERID TABLE ACCESS MOVED TO PDASP2 **********   *
      *             USERID TABLE     -  DB2       (READ-ONLY)         *
      *     ******** JS 8/6/02 ************************************   *
      *                                                               *
      *                                                               *
      * TRANSACTIONS GENERATED:                                       *
      *             NONE                                              *
      *                                                               *
      *                                                               *
      * PFKEYS  :   NONE                                              *
      *                                                               *
      *                                                               *
      *****************************************************************
      *             PROGRAM CHANGE LOG                                *
      *             -------------------                               *
      *                                                               *
      *  DATE       UPDATED BY            CHANGE DESCRIPTION          *
      *  --------   --------------------  --------------------------  *
      *  XX/XX/XX   XXXXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXX  *
      *                                                               *
      *  09/23/03   PAUL BARON            MIGRATE TO MQSERIES V5.3.1, *
      *                                   USE HIGHER VERSION MQSERIES *
      *                                   COPYBOOKS FOR MQ STRUCTURES *
      *                                                               *
      *  04/30/03   PAUL BARON            CHANGED PROGRAM TO WRITE    *
      *                                   A MQSERIES MESSAGE TO       *
      *                                   REQUEST A CREDIT AUTHORIZATN*
      *                                   CHECK FOR THE CUSTOMER BEING*
      *                                   PROCESSED BY THE ORDER      *
      *                                   INQUIRY REQUEST.            *
      *                                                               *
      *  04/15/03   PAUL BARON            CHANGED PROCESSING FOR      *
      *                                   SCENARIO 16 (MQ RETURN CODE *
      *                                   2033), TO OPERATE ON        *
      *                                   PERMANENT MQSERIES QUEUES   *
      *                                   AS OPPOSED TO TEMPORARY     *
      *                                   DYNAMIC MQSERIES QUEUES.    *
      *                                   (FOR XPEDITER, SHERYL KING) *
      *                                                               *
      *  08/06/02   JS                    ADDED DB2 SP - PDASP2       *
      *                                                               *
      *****************************************************************

       ENVIRONMENT DIVISION.
       DATA DIVISION.
           EJECT
       WORKING-STORAGE SECTION.

      *****************************************************************
      *    77 LEVEL DATA ITEMS HERE  (SUBSCRIPTS, INDEXES ETC.)       *
      *****************************************************************
       77  WS-SUB1                     PIC S9(04)   COMP  VALUE +0.
       77  WS-ORDERS-MAX               PIC S9(04)   COMP  VALUE +14.
       77  WS-RESPONSE-CODE            PIC S9(08)   COMP  VALUE +0.

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

           EJECT
      *****************************************************************
      *    MISCELLANEOUS WORK FIELDS                                  *
      *****************************************************************

       01  WS-MISCELLANEOUS-FIELDS.
           05  WMF-PSB-NAME            PIC X(08)   VALUE 'PDA017'.
           05  WMF-STARTCODE           PIC X(02)   VALUE SPACES.
           05  WMF-MESSAGE-AREA        PIC X(79)   VALUE SPACES.

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

           05  WMF-ACTIVE-SCENARIOS-GRP
                                       PIC X(250)   VALUE SPACES.
           05  WMF-ACTIVE-SCENARIO     REDEFINES
                                       WMF-ACTIVE-SCENARIOS-GRP
                                       PIC X(01)
                                       OCCURS 250 TIMES.

           05  WMF-CURRENT-SCENARIO    PIC 9(03)    VALUE ZEROES.

           05  WMF-DUMP-CODE           PIC X(04)    VALUE SPACES.
           05  WMF-DUMP-CODE-R         REDEFINES WMF-DUMP-CODE
                                       PIC 9(04).

           05  WMF-SAVE-MSGID          PIC X(24)    VALUE SPACES.
           05  WMF-SPECIAL-MSGID       PIC X(24)    VALUE
               'MQ-PDA017-CUSTOMER-2033 '.

       01  PDASP2-USERID               PIC X(08).
       01  PDASP2-NUMBER               PIC S9(9) COMP.
       01  PDASP2-ACTIVE-SCENARIOS     PIC X(250).
       01  PDASP2-STATUS               PIC X(04).

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
      *    CICS DEFINITIONS                                           *
      *****************************************************************
      *    NONE REQUIRED                                              *
      *****************************************************************
           EJECT

      *****************************************************************
      *    IMS / DLI DEFINITIONS                                      *
      *****************************************************************

      *****************************************************************
      *    ORDER DATABASE ROOT SEGMENT (ORDER1DB)                     *
      *****************************************************************
           COPY IORDER.
           EJECT

      *****************************************************************
      *    DB2  DEFINITIONS                                           *
      *****************************************************************

      *****************************************************************
      *         SQL COMMUNICATIONS AREA                               *
      *****************************************************************

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
           EJECT

      *****************************************************************
      *         USER IDENTIFICATION TABLE   -- DCLGEN DUSERID         *
      *****************************************************************
      *!
      *!   EXEC SQL
      *!       INCLUDE DUSERID
      *!   END-EXEC.
      *!   EJECT

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
           05  MQS-HOBJECT-CREDIT-AUTH-REQ-Q
                                       PIC S9(9)  BINARY  VALUE +0.
           05  MQS-OPTIONS             PIC S9(9)  BINARY  VALUE +0.
           05  MQS-OBJECTTYPE          PIC S9(9)  BINARY  VALUE +0.
           05  MQS-BUFFERLENGTH        PIC S9(9)  BINARY  VALUE +0.
           05  MQS-DATALENGTH          PIC S9(9)  BINARY  VALUE +0.
           05  MQS-COMPCODE            PIC S9(9)  BINARY  VALUE +0.
           05  MQS-REASONCODE          PIC S9(9)  BINARY  VALUE +0.
           05  MQS-OBJECTNAME          PIC X(48)          VALUE SPACES.
           05  MQS-MSGID               PIC X(24)          VALUE SPACES.
           05  MQS-OBJECTTYPE-DESC     PIC X(15)          VALUE SPACES.


           05  MQS-CUSTOMER-QUEUE      PIC X(48)          VALUE
               'PDAPROD.H01AC013.CUSTOMER.QUEUE'.
           05  MQS-CUSTOMER-QALIAS     PIC X(48)          VALUE
               'PDAPROD.H01AC013.CUSTOMER.QUEUE.ALIAS'.

           05  MQS-CUSTOMER-RESPONSE-QUEUE
                                       PIC X(48)          VALUE
               'PDAPROD.H01AC013.CUSTOMER.RESPONSE.QUEUE'.
           05  MQS-CUSTOMER-RESPONSE-QALIAS
                                       PIC X(48)          VALUE
               'PDAPROD.H01AC013.CUSTOMER.RESPONSE.QUEUE.ALIAS'.

           05  MQS-CUSTOMER-INIT-QUEUE PIC X(48)          VALUE
               'PDAPROD.H01AC013.CUSTOMER.INIT.QUEUE'.

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

           05  MQS-CREDIT-AUTH-REQ-QUEUE
                                       PIC X(48)          VALUE
               'PDAPROD.H01AC013.CREDIT.AUTH.REQ.QUEUE'.


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
      *    MQSERIES STRUCTURE DEFINITIONS                             *
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

       01  MQS-TRIGGER-MESSAGE-STRUCTURE.
           COPY CMQTMV.
           EJECT

       01  MQS-CONSTANTS.
           COPY CMQV.
           EJECT

      *****************************************************************
      *    MESSAGES   (ERROR AND INFORMATIONAL)                       *
      *****************************************************************

           COPY PDAMSGS.
           EJECT

      *****************************************************************
      *    GENERAL ERROR PROCESSING WORK AREAS (CICS, IMS-DLI, DB2)   *
      *****************************************************************

           COPY PDAERRWS.
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
      *                CUSTOMER ORDER PROCESSING PROGRAM              *
      *                                                               *
      *    CALLED BY:  NONE                                           *
      *                                                               *
      *****************************************************************

       P00000-MAINLINE.


           EXEC CICS HANDLE CONDITION
                ERROR(P99100-GENERAL-ERROR)
           END-EXEC.


           PERFORM  P00050-INITIALIZE                                   TAGGED
               THRU P00050-INITIALIZE-EXIT.                             CODE
                                                                        TESTING
                                                                        03/13/01
           PERFORM  P00700-MAIN-PROCESS
               THRU P00700-MAIN-PROCESS-EXIT.


           PERFORM  P00400-CICS-RETURN
               THRU P00400-CICS-RETURN-EXIT.

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

      *****************************************************************
      *    INITIALIZE SWITCHES, SUBSCRIPTS, ETC.                      *
      *****************************************************************

           MOVE ZEROES                 TO WS-ERROR-FOUND-SW.
           MOVE SPACES                 TO WS-PDA-ERROR-TYPE.
           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.
           MOVE ZEROES                 TO WMF-CURRENT-SCENARIO.
           MOVE FUNCTION CURRENT-DATE  TO WS-CURRENT-DATE-TIME.         00020001


      *****************************************************************
      *    INITIALIZE THE QUERY RESULTS MESSAGE I/O AREA TO DEFAULTS  *
      *****************************************************************

           PERFORM P00060-INIT-MQS-MESSAGE
              THRU P00060-INIT-MQS-MESSAGE-EXIT.


      *****************************************************************
      *    OPEN THE MQSERIES QUEUES TO BE UTILIZED                    *
      *****************************************************************

           PERFORM P00300-OPEN-MAIN-QUEUES
              THRU P00300-OPEN-MAIN-QUEUES-EXIT.


      *****************************************************************
      *    VERIFY THE TRANSACTION WAS INVOKED FROM A VALID SOURCE     *
      *    (TRANS MUST HAVE BEEN STARTED WITH DATA - CKTI TRIGGER)    *
      *****************************************************************

           EXEC CICS ASSIGN
                     STARTCODE (WMF-STARTCODE)
                     NOHANDLE
                     RESP      (WS-RESPONSE-CODE)
           END-EXEC.


           IF WS-RESPONSE-CODE  = DFHRESP(NORMAL)
               IF WMF-STARTCODE = 'SD'
                   NEXT SENTENCE
               ELSE
                   MOVE PM050-INVALID-TRANS-REQUEST
                                       TO WMF-MESSAGE-AREA
                   PERFORM  P70000-ERROR-ROUTINE
                       THRU P70000-ERROR-ROUTINE-EXIT
                   MOVE 9              TO WS-ERROR-FOUND-SW
                   MOVE 'CICS'         TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA017'       TO WPCE-PROGRAM-ID
                   MOVE WS-RESPONSE-CODE
                                       TO WPCE-RESPONSE-CODE
                   MOVE 'CICS ASSIGN STARTCODE'
                                       TO WPCE-COMMAND
                   MOVE 'P00050'       TO WPCE-PARAGRAPH
                   PERFORM  P99500-PDA-ERROR
                       THRU P99500-PDA-ERROR-EXIT
           ELSE
                   MOVE 9              TO WS-ERROR-FOUND-SW
                   MOVE 'CICS'         TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA017'       TO WPCE-PROGRAM-ID
                   MOVE WS-RESPONSE-CODE
                                       TO WPCE-RESPONSE-CODE
                   MOVE 'CICS ASSIGN STARTCODE'
                                       TO WPCE-COMMAND
                   MOVE 'P00050'       TO WPCE-PARAGRAPH
                   PERFORM  P99500-PDA-ERROR
                       THRU P99500-PDA-ERROR-EXIT.


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

           MOVE SPACES                 TO MQS-RESULTS-MESSAGE.
           MOVE ZEROES                 TO MQS-RETURN-CODE
                                          MQS-TOTAL-ORDERS
                                          MQS-TOTAL-DOLLAR-AMOUNT
                                          MQS-AVG-DOLLAR-AMOUNT
                                          MQS-LAST-ORDER-AMOUNT.

           PERFORM P00065-INIT-MQS-ORDERS
              THRU P00065-INIT-MQS-ORDERS-EXIT
                  VARYING WS-SUB1 FROM +1 BY +1
                      UNTIL WS-SUB1 > WS-ORDERS-MAX.

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
      *    PARAGRAPH:  P00300-OPEN-MAIN-QUEUES                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO OPEN ALL RELEVANT MQSERIES QUEUES   *
      *                TO BE USED BY THE TRANSACTION                  *
      *                                                               *
      *    CALLED BY:  P00050-INITIALIZE                              *
      *                                                               *
      *****************************************************************

       P00300-OPEN-MAIN-QUEUES.

      *****************************************************************
      *    INITIALIZE MQSERIES PARAMETERS AND VARIABLES               *
      *****************************************************************

           MOVE ZEROES                 TO MQS-HOBJECT
                                          MQS-HOBJECT-REQUEST-Q
                                          MQS-HOBJECT-RESPONSE-Q
                                          MQS-COMPCODE
                                          MQS-REASONCODE.

           MOVE MQOD-CURRENT-VERSION   TO MQOD-VERSION.
           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
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


      *****************************************************************
      *    OPEN THE CUSTOMER REQUEST QUEUE -- MAIN INPUT TO TRANS     *
      *    (THE OPEN IS ISSUED FOR AN ALIAS OF THE QUEUE)             *
      *****************************************************************

           MOVE MQS-CUSTOMER-QALIAS    TO MQOD-OBJECTNAME.
           COMPUTE MQS-OPTIONS         =  MQOO-INPUT-SHARED      +
                                          MQOO-SAVE-ALL-CONTEXT  +
                                          MQOO-FAIL-IF-QUIESCING.

           PERFORM P07100-MQS-OPEN
              THRU P07100-MQS-OPEN-EXIT.

           MOVE MQS-HOBJECT            TO MQS-HOBJECT-REQUEST-Q.


       P00300-OPEN-MAIN-QUEUES-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00400-CICS-RETURN                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETURN CONTROL TO CICS              *
      *                                                               *
      *    CALLED BY:  P00000-MAINLINE                                *
      *                                                               *
      *****************************************************************

       P00400-CICS-RETURN.


           EXEC CICS RETURN
                     NOHANDLE
                     RESP          (WS-RESPONSE-CODE)
           END-EXEC.


      *****************************************************************
      *    IF ERROR, FORMAT ERROR INFORMATION AND TERMINATE           *
      *****************************************************************

           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               NEXT SENTENCE
           ELSE
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
               MOVE 'PDA017'           TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
               MOVE 'CICS RETURN'      TO WPCE-COMMAND
               MOVE 'P00400'           TO WPCE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P00400-CICS-RETURN-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00700-MAIN-PROCESS                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE CUSTOMER ORDER QUERY    *
      *                HIGH LEVEL PROCESSES                           *
      *                                                               *
      *    CALLED BY:  P00000-MAINLINE                                *
      *                                                               *
      *****************************************************************

       P00700-MAIN-PROCESS.

      *****************************************************************
      *    PROCESS THE CUSTOMER ORDER QUERY REQUEST                   *
      *****************************************************************

           PERFORM  P00800-PROCESS-REQUEST
               THRU P00800-PROCESS-REQUEST-EXIT.


      *****************************************************************
      *    SEND QUERY RESPONSE VIA MQSERIES MESSAGE                   *
      *****************************************************************

           PERFORM  P06000-SEND-RESPONSE
               THRU P06000-SEND-RESPONSE-EXIT.


      *****************************************************************
      *    CLOSE THE MQSERIES QUEUES / OBJECTS                         *
      *****************************************************************

           PERFORM  P06900-CLOSE-THE-QUEUES
               THRU P06900-CLOSE-THE-QUEUES-EXIT.


       P00700-MAIN-PROCESS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00800-PROCESS-REQUEST                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE CUSTOMER ORDER QUERY    *
      *                PROCESS                                        *
      *                                                               *
      *    CALLED BY:  P00700-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P00800-PROCESS-REQUEST.

      *****************************************************************
      *    RETRIEVE THE INCOMING QUERY REQUEST FROM THE MQSERIES      *
      *    APPLICATION INPUT QUEUE                                    *
      *****************************************************************

           PERFORM  P01000-GET-REQUEST
               THRU P01000-GET-REQUEST-EXIT.

           IF ERROR-FOUND
               GO TO P00800-PROCESS-REQUEST-EXIT.


      *****************************************************************
      *    OPEN THE REMAINING (SECONDARY) MQSERIES QUEUES             *
      *****************************************************************

           PERFORM  P01500-OPEN-OTHER-QUEUES
               THRU P01500-OPEN-OTHER-QUEUES-EXIT.


      *****************************************************************
      **** PERFORM CUSTOMER PAYMENT TRANSACTION PROCESSING ************
      *    WRITES MESSAGES TO TEMPORARY DYNAMIC, AND TRANSACTION QUES *
      *    (USED BY SCENARIO 16, 2033 - MESSAGE NOT AVAILABLE AND     *
      *     USED BY SCENARIO 17, ABEND ASRA)                          *
      *****************************************************************

           PERFORM  P01800-PAYMENT-TRANS
               THRU P01800-PAYMENT-TRANS-EXIT.


      *****************************************************************
      *    EXTRACT ORDER INFORMATION FOR THE SELECTED CUSTOMER        *
      *****************************************************************

           PERFORM  P03000-ORDER-PROCESS
               THRU P03000-ORDER-PROCESS-EXIT.


       P00800-PROCESS-REQUEST-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01000-GET-REQUEST                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE  AND VALIDATE THE CUSTOMER *
      *                ORDER QUERY REQUEST FROM THE MQSERIES CUSTOMER *
      *                APPLICATION QUEUE                              *
      *                                                               *
      *    CALLED BY:  P00800-PROCESS-REQUEST                         *
      *                                                               *
      *****************************************************************

       P01000-GET-REQUEST.

      *****************************************************************
      *    READ THE PRIMARY CUSTOMER REQUEST FROM CUSTOMER INPUT QUEUE*
      *****************************************************************

           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
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
           MOVE LENGTH OF MQS-CUSTOMER-MESSAGE
                                       TO MQS-BUFFERLENGTH.
           MOVE MQS-HOBJECT-REQUEST-Q  TO MQS-HOBJECT.

           PERFORM P07400-MQS-GET
              THRU P07400-MQS-GET-EXIT.

           MOVE MQS-BUFFER             TO MQS-CUSTOMER-MESSAGE.
           MOVE MQMD-MSGID             TO WMF-SAVE-MSGID.


      *****************************************************************
      *    VERIFY THE USERID IS VALID FOR THE PRODUCT DEMO APPLICATION*
      *****************************************************************

           PERFORM P01100-VERIFY-USERID
              THRU P01100-VERIFY-USERID-EXIT.


       P01000-GET-REQUEST-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01100-VERIFY-USERID                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO VALIDATE THE USERID FOR PRODUCT     *
      *                DEMONSTRATION APPLICATION USAGE AND OBTAIN     *
      *                USER SPECIFIC INFORMATION FOR LATER USAGE      *
      *                                                               *
      *    CALLED BY:  P01000-GET-REQUEST                             *
      *                                                               *
      *****************************************************************

       P01100-VERIFY-USERID.

      *****************************************************************
      *    USERID MUST EXIST IN THE DB2 USERID TABLE                  *
      *****************************************************************

           MOVE MQS-CUSTOMER-USERID     TO PDASP2-USERID.
           MOVE ZEROS                   TO PDASP2-NUMBER.
           MOVE SPACES                  TO PDASP2-ACTIVE-SCENARIOS
                                           PDASP2-STATUS.

           EXEC SQL
               CALL PDASP2 (:PDASP2-USERID,
                            :PDASP2-NUMBER, :PDASP2-ACTIVE-SCENARIOS,
                            :PDASP2-STATUS)
           END-EXEC.
      *!   EXEC SQL
      *!       CALL PDASP2 (:PDASP2-USERID, :PDASP2-NUMBER)
      *!   END-EXEC.


      *****************************************************************
      *    ZERO RETURN CODE (SUCCESS) IS ONLY ACCEPTABLE CODE         *
      *****************************************************************

           IF PDASP2-STATUS            =  ZEROES
               MOVE PDASP2-ACTIVE-SCENARIOS
                                       TO WMF-ACTIVE-SCENARIOS-GRP
           ELSE
           IF PDASP2-STATUS            =  '0100'
               MOVE 'USER'             TO MQS-PDA-ERROR-TYPE
               MOVE PM033-USERID-NOT-FOUND
                                       TO WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
           ELSE
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA017'           TO WPDE-PROGRAM-ID
               MOVE PDASP2-STATUS      TO WPDE-DB2-SQLCODE
               MOVE 'SELECT ID FROM USERID'
                                       TO WPDE-FUNCTION
               MOVE 'P01100'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P01100-VERIFY-USERID-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01500-OPEN-OTHER-QUEUES                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO OPEN ALL THE REMAINING MQSERIES     *
      *                QUEUES TO BE USED BY THE TRANSACTION           *
      *                                                               *
      *    CALLED BY:  P00800-PROCESS-REQUEST                         *
      *                                                               *
      *****************************************************************

       P01500-OPEN-OTHER-QUEUES.

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
                                          MQS-HOBJECT-CREDIT-AUTH-REQ-Q
                                          MQS-COMPCODE
                                          MQS-REASONCODE.

           MOVE MQOD-CURRENT-VERSION   TO MQOD-VERSION.
           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
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
      *    (USED FOR SCENARIO TO PUT MESSAGE TO A PUT INHIBITED QUE)  *
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

           MOVE 16                     TO WMF-CURRENT-SCENARIO.
           PERFORM P01530-CLEAR-XMIT-QUEUE
              THRU P01530-CLEAR-XMIT-QUEUE-EXIT.
           MOVE ZEROES                 TO WMF-CURRENT-SCENARIO.


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
      *    OPEN THE CUSTOMER CREDIT AUTHORIZATION REQUEST QUEUE.      *
      *    USED TO REQUEST CREDIT CHECKS FROM CREDIT BUREAUS          *
      *****************************************************************

           MOVE MQS-CREDIT-AUTH-REQ-QUEUE
                                       TO MQOD-OBJECTNAME.
           COMPUTE MQS-OPTIONS         =  MQOO-OUTPUT            +
                                          MQOO-PASS-ALL-CONTEXT  +
                                          MQOO-FAIL-IF-QUIESCING.

           PERFORM P07100-MQS-OPEN
              THRU P07100-MQS-OPEN-EXIT.

           MOVE MQS-HOBJECT           TO MQS-HOBJECT-CREDIT-AUTH-REQ-Q.


      *****************************************************************
      *    OPEN TRANSACTION LOG QUEUE 99, ONLY IF SCENARIO ACTIVE     *
      *    (USED FOR SCENARIO TO OPEN QUEUE THAT DOES NOT EXIST)      *
      *****************************************************************

           IF WMF-ACTIVE-SCENARIO (14) = 'Y'
               MOVE 14                 TO WMF-CURRENT-SCENARIO
               PERFORM P01560-OPEN-LOG-99
                  THRU P01560-OPEN-LOG-99-EXIT
               MOVE ZEROES             TO WMF-CURRENT-SCENARIO.


       P01500-OPEN-OTHER-QUEUES-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01530-CLEAR-XMIT-QUEUE                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO ATTEMPT RETRIEVAL OF MESSAGES FROM  *
      *                THE TRANSMISSION QUEUE (CWO01 TO CW09) TO      *
      *                CLEAR ANY EXPIRED MESSAGES. THIS PROGRAM       *
      *                PURPOSELY PUTS MESSAGES WITH EXPIRATIONS TO    *
      *                SATISFY SCENARIO NUMBER 16.                    *
      *                                                               *
      *                THIS ROUTINE PREVENTS MESSAGE ACCUMULATION     *
      *                                                               *
      *    CALLED BY:  P01500-OPEN-OTHER-QUEUES                       *
      *                                                               *
      *****************************************************************

       P01530-CLEAR-XMIT-QUEUE.


      *****************************************************************
      *    READ TRANSMISSION QUEUE (CW01 TO CW09) TO REMOVE           *
      *    ALL EXPIRED MESSAGES FROM THE QUEUE                        *
      *    (PREVENTS MESSAGE ACCUMULATION)                            *
      *****************************************************************

           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
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
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA017'           TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQGET'            TO WPME-FUNCTION-1
               MOVE MQS-OBJECTTYPE-DESC
                                       TO WPME-FUNCTION-2
               MOVE 'P01530'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    CLOSE THE QUEUE, WILL BE OPENED AS OUTPUT ABOVE            *
      *****************************************************************

           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
           COMPUTE MQS-OPTIONS         =  MQCO-NONE.
           MOVE MQS-HOBJECT-TRANSMIT-Q TO MQS-HOBJECT.

           PERFORM P07300-MQS-CLOSE
              THRU P07300-MQS-CLOSE-EXIT.


       P01530-CLEAR-XMIT-QUEUE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01560-OPEN-LOG-99                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO OPEN THE MQSERIES TRANSACTION LOG   *
      *                (USED FOR SCENARIO TO OPEN QUEUE THAT DOES     *
      *                NOT EXIST)                                     *
      *                                                               *
      *    CALLED BY:  P01500-OPEN-OTHER-QUEUES                       *
      *                                                               *
      *****************************************************************

       P01560-OPEN-LOG-99.

      *****************************************************************
      *    OPEN TRANSACTION LOG QUEUE 99                              *
      *    (USED FOR SCENARIO TO OPEN QUEUE THAT DOES NOT EXIST)      *
      *****************************************************************

           MOVE MQS-TRANS-LOG-QUEUE-99 TO MQOD-OBJECTNAME.
           COMPUTE MQS-OPTIONS         =  MQOO-OUTPUT            +
                                          MQOO-PASS-ALL-CONTEXT  +
                                          MQOO-FAIL-IF-QUIESCING.

           PERFORM P07100-MQS-OPEN
              THRU P07100-MQS-OPEN-EXIT.


      *****************************************************************
      *    CHECK FOR APPROPRIATE REASONCODE TO SATISFY THE INTENDED   *
      *    SCENARIO ERROR, OTHERWISE CONSIDER FATAL ERROR-- TERMINATE *
      *****************************************************************

           IF MQS-REASONCODE           =  MQRC-UNKNOWN-OBJECT-NAME
               MOVE MQS-REASONCODE     TO WMF-DUMP-CODE-R
               PERFORM  P70500-TRANS-DUMP-ROUTINE
                   THRU P70500-TRANS-DUMP-ROUTINE-EXIT
           ELSE
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA017'           TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQOPEN'           TO WPME-FUNCTION-1
               MOVE MQS-OBJECTTYPE-DESC
                                       TO WPME-FUNCTION-2
               MOVE 'P01560'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P01560-OPEN-LOG-99-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01800-PAYMENT-TRANS                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS CUSTOMER PAYMENT            *
      *                TRANSACTIONS. WRITES CUSTOMER PAYMENT MQSERIES *
      *                MESSAGES TO:                                   *
      *                1. TEMPORARY DYNAMIC QUEUE                     *
      *                   (USED BY SCENARIO 17 ABEND ASRA LATER)      *
      *                2. TRANSACTION QUEUE (PERMANENT QUEUE)         *
      *                   (USED BY SCENARIO 16 -2033 NO MSG AVAILABLE)*
      *                                                               *
      *    CALLED BY:  P00800-PROCESS-REQUEST                         *
      *                                                               *
      *****************************************************************

       P01800-PAYMENT-TRANS.

      *****************************************************************
      *    WRITE A CUSTOMER PAYMENT TO THE TEMPORARY DYNAMIC QUEUE    *
      *****************************************************************

           MOVE 'QUEUE'                TO MQS-OBJECTTYPE-DESC.

           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
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

           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
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
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA017'           TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQINQ'            TO WPME-FUNCTION-1
               MOVE MQS-OBJECTTYPE-DESC
                                       TO WPME-FUNCTION-2
               MOVE 'P01800'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    IF NO MESSAGES ON THE TRANSACTION QUEUE, WRITE SOME        *
      *****************************************************************

           IF MQS-INTATTRS (1)         > ZEROES
               NEXT SENTENCE
           ELSE
               MOVE MQHC-DEF-HCONN     TO MQS-HCONN
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

               PERFORM  P01815-WRITE-PAYMENT-TRAN
                   THRU P01815-WRITE-PAYMENT-TRAN-EXIT
                       VARYING WS-SUB1 FROM +1 BY +1
                           UNTIL WS-SUB1 > +3.


      *****************************************************************
      *    PROCESS CUSTOMER SPECIAL REQUEST MESSAGES FIRST            *
      *    (USED FOR SCENARIO TO ATTEMPT RETRIEVAL OF MESSAGES WITH   *
      *     A BOGUS MESSAGE ID TO PRODUCE RETURN CODE 2033,NO MESSAGE *
      *     AVAILABLE ON QUEUE)                                       *
      *****************************************************************

           IF WMF-ACTIVE-SCENARIO (16) = 'Y'
               MOVE 16                 TO WMF-CURRENT-SCENARIO
               PERFORM  P01830-GET-SPECIAL-MSG
                   THRU P01830-GET-SPECIAL-MSG-EXIT
               MOVE ZEROES             TO WMF-CURRENT-SCENARIO.


       P01800-PAYMENT-TRANS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01815-WRITE-PAYMENT-TRAN                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO WRITE PAYMENT MESSAGES TO           *
      *                THE TRANSACTION QUEUE FOR DEMONSTRATION        *
      *                PURPOSES. USED WITH SCENARIO 16 - 2033 NO      *
      *                MESSAGE AVAILABLE ON QUEUE                     *
      *                                                               *
      *    CALLED BY:  P01800-PAYMENT-TRANS                           *
      *                                                               *
      *****************************************************************

       P01815-WRITE-PAYMENT-TRAN.


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


       P01815-WRITE-PAYMENT-TRAN-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01830-GET-SPECIAL-MSG                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO ATTEMPT RETRIEVAL OF MESSAGES FROM  *
      *                THE TRANSACTION QUE USING BOGUS MESSAGE ID TO  *
      *                PRODUCE A RETURN CODE OF 2033, NO MESSAGE      *
      *                AVAILABLE ON QUEUE FOR SCENARIO PROCESSING     *
      *                                                               *
      *    CALLED BY:  P01800-PAYMENT-TRANS                           *
      *                                                               *
      *****************************************************************

       P01830-GET-SPECIAL-MSG.

      *****************************************************************
      *    PUT A CUSTOMER REQUEST MESSAGE ON THE REMOTE QUEUE         *
      *    (DONE FOR DEMO PURPOSES ONLY TO SHOW A MESSAGE ON THE      *
      *     TRANSMISSION QUEUE WHEN XPEDITER DUMP REPORTS ARE SHOWN,  *
      *     CURRENTLY DONE FOR SCENARIO 16, 2033 MQSERIES RETURN CODE)*
      *****************************************************************

           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
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
      *    (PRODUCES A 2033 -- NO MESSAGE AVAILABLE, AND TRANS DUMP)  *
      *****************************************************************

           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
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
      *    CHECK FOR APPROPRIATE REASONCODE TO SATISFY THE INTENDED   *
      *    SCENARIO ERROR, OTHERWISE CONSIDER FATAL ERROR-- TERMINATE *
      *****************************************************************

           IF MQS-REASONCODE           =  MQRC-NO-MSG-AVAILABLE
               MOVE MQS-REASONCODE     TO WMF-DUMP-CODE-R
               PERFORM P70500-TRANS-DUMP-ROUTINE
                  THRU P70500-TRANS-DUMP-ROUTINE-EXIT
           ELSE
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA017'           TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQGET'            TO WPME-FUNCTION-1
               MOVE MQS-OBJECTTYPE-DESC
                                       TO WPME-FUNCTION-2
               MOVE 'P01830'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P01830-GET-SPECIAL-MSG-EXIT.
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
      *    CALLED BY:  P00800-PROCESS-REQUEST                         *
      *                                                               *
      *****************************************************************

       P03000-ORDER-PROCESS.

      *****************************************************************
      *    SCHEDULE THE PSB TO BE USED (PDA017)                       *
      *****************************************************************

           PERFORM  P03100-SCHEDULE-PSB
               THRU P03100-SCHEDULE-PSB-EXIT.


      *****************************************************************
      *    PROCESS THE ORDER DATABASE FOR THE CUSTOMER ID SUPPLIED    *
      *    IN THE MQSERIES INPUT MESSAGE                               *
      *****************************************************************

           PERFORM  P03500-EXTRACT-ORDERS
               THRU P03500-EXTRACT-ORDERS-EXIT.


      *****************************************************************
      *    TERMINATE THE PSB                                          *
      *****************************************************************

           PERFORM  P03200-TERMINATE-PSB
               THRU P03200-TERMINATE-PSB-EXIT.


       P03000-ORDER-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03100-SCHEDULE-PSB                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO SCHEDULE THE IMS-DLI PSB USED       *
      *                BY THE MODULE TO ACCESS THE ORDER DATABASE     *
      *                                                               *
      *    CALLED BY:  P03000-ORDER-PROCESS                           *
      *                                                               *
      *****************************************************************

       P03100-SCHEDULE-PSB.


           EXEC DLI  SCHEDULE
                     PSB((WMF-PSB-NAME))
                     NODHABEND
           END-EXEC.


      *****************************************************************
      *    CHECK FOR PSB SCHEDULING ERROR                             *
      *****************************************************************

           IF DIBSTAT    =  SPACES
               NEXT SENTENCE
           ELSE
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA017'           TO WPIE-PROGRAM-ID
               MOVE 'P03100'           TO WPIE-PARAGRAPH
               MOVE DIBSTAT            TO WPIE-STATUS-CODE
               MOVE 'SCHD'             TO WPIE-FUNCTION-CODE
               MOVE SPACES             TO WPIE-SEGMENT-NAME
               MOVE SPACES             TO WPIE-DATABASE-NAME
               MOVE 'PSB SCHEDULING ERROR'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P03100-SCHEDULE-PSB-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03200-TERMINATE-PSB                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO TERMINATE THE IMS-DLI PSB USED      *
      *                BY THE MODULE TO ACCESS THE ORDER DATABASE     *
      *                                                               *
      *    CALLED BY:  P03000-ORDER-PROCESS                           *
      *                                                               *
      *****************************************************************

       P03200-TERMINATE-PSB.


           EXEC DLI  TERMINATE
           END-EXEC.


      *****************************************************************
      *    CHECK FOR PSB TERMINATION ERROR                            *
      *****************************************************************

           IF DIBSTAT    =  SPACES
               NEXT SENTENCE
           ELSE
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA017'           TO WPIE-PROGRAM-ID
               MOVE 'P03200'           TO WPIE-PARAGRAPH
               MOVE DIBSTAT            TO WPIE-STATUS-CODE
               MOVE 'TERM'             TO WPIE-FUNCTION-CODE
               MOVE SPACES             TO WPIE-SEGMENT-NAME
               MOVE SPACES             TO WPIE-DATABASE-NAME
               MOVE 'PSB TERMINATION ERROR'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P03200-TERMINATE-PSB-EXIT.
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

           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.
           MOVE ZEROES                 TO WS-SUB1.

      *****************************************************************
      *    ESTABLISH DATABASE POSITION AT FIRST ORDER FOR THE USERID  *
      *    (IF NOT FOUND, TERMINATE THE PROCESSING LOOP)              *
      *****************************************************************

      *    MOVE USERID-NUMBER          TO WMF-ORDER-PREFIX.
           MOVE PDASP2-NUMBER          TO WMF-ORDER-PREFIX.
           MOVE LOW-VALUES             TO WMF-ORDER-NUMBER.

           PERFORM  P03900-GU-ORDER
               THRU P03900-GU-ORDER-EXIT.

           IF (DIBSTAT            NOT  =  SPACES)   OR
      *       (ORDER-PREFIX       NOT  =  USERID-NUMBER)
              (ORDER-PREFIX       NOT  =  PDASP2-NUMBER)
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


       P03500-EXTRACT-ORDERS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03600-SELECT-ORDERS                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS ALL ORDERS FOR THE USER     *
      *                PREFIX, SELECTING ONLY THOSE MATCHING ON       *
      *                CUSTOMER ID.                                   *
      *                                                               *
      *    CALLED BY:  P03500-EXTRACT-ORDERS                          *
      *                                                               *
      *****************************************************************

       P03600-SELECT-ORDERS.

      *****************************************************************
      *    SELECT ORDERS WITH MATCHING USER PREFIX AND CUSTOMER ID    *
      *****************************************************************

      *!   IF ORDER-PREFIX             =  USERID-NUMBER
           IF ORDER-PREFIX             =  PDASP2-NUMBER
               IF ORDER-CUSTOMER-ID    =  MQS-CUSTOMER-ID
                   PERFORM  P03640-FORMAT-ORDER
                       THRU P03640-FORMAT-ORDER-EXIT
               ELSE
                   NEXT SENTENCE
           ELSE
                   MOVE 'Y'            TO WS-PROCESS-COMPLETE-SW
                   GO TO P03600-SELECT-ORDERS-EXIT.


      *****************************************************************
      *    READ THE NEXT ORDER IN THE DATABASE                        *
      *****************************************************************

           EXEC DLI GN USING
                    PCB         (1)
                    SEGMENT     (ORDER)
                    INTO        (ORDER-SEGMENT)
                    SEGLENGTH   (123)
                    WHERE       (ORDKEY>=WMF-ORDER-KEY)
                    FIELDLENGTH (15)
           END-EXEC.


      *****************************************************************
      *    CHECK STATUS CODE FOR SUCCESS, NOT FOUND, END OF DATABASE, *
      *    ALL OTHERS ARE AN ERROR                                    *
      *****************************************************************

           IF DIBSTAT    =  SPACES
               NEXT SENTENCE
           ELSE
           IF DIBSTAT    =  'GE' OR 'GB'
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
           ELSE
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA017'           TO WPIE-PROGRAM-ID
               MOVE 'P03600'           TO WPIE-PARAGRAPH
               MOVE DIBSTAT            TO WPIE-STATUS-CODE
               MOVE 'GN'               TO WPIE-FUNCTION-CODE
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME
               MOVE 'ORDER1DB'         TO WPIE-DATABASE-NAME
               MOVE 'GN ORDER ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


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

      *****************************************************************
      *    A MAXIMUM OF 14 ORDERS WILL BE CAPTURED (SCREEN SIZE),     *
      *    IF WE HAVE 14 CONSIDER THE PROCESS COMPLETE, EXIT          *
      *****************************************************************

           ADD +1                      TO WS-SUB1.

           IF WS-SUB1                  >  WS-ORDERS-MAX
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


       P03640-FORMAT-ORDER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03900-GU-ORDER                                *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE ORDER ROOT SEGMENT     *
      *                USING THE SUPPLIED CUSTOMER PREFIX             *
      *                (FROM MQSERIES MESSAGE)                        *
      *                                                               *
      *    CALLED BY:  P03500-EXTRACT-ORDERS                          *
      *                                                               *
      *****************************************************************

       P03900-GU-ORDER.


           EXEC DLI GU USING
                    PCB         (1)
                    SEGMENT     (ORDER)
                    INTO        (ORDER-SEGMENT)
                    SEGLENGTH   (123)
                    WHERE       (ORDKEY>=WMF-ORDER-KEY)
                    FIELDLENGTH (15)
           END-EXEC.


      *****************************************************************
      *    CHECK STATUS CODE FOR SUCCESS, NOT FOUND, END OF DATABASE, *
      *    ALL OTHERS ARE AN ERROR                                    *
      *****************************************************************

           IF DIBSTAT    =  SPACES OR 'GE' OR 'GB'
               NEXT SENTENCE
           ELSE
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA017'           TO WPIE-PROGRAM-ID
               MOVE 'P03900'           TO WPIE-PARAGRAPH
               MOVE DIBSTAT            TO WPIE-STATUS-CODE
               MOVE 'GU'               TO WPIE-FUNCTION-CODE
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME
               MOVE 'ORDER1DB'         TO WPIE-DATABASE-NAME
               MOVE 'GU ORDER ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P03900-GU-ORDER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06000-SEND-RESPONSE                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO WRITE THE MQSERIES RESPONSE MESSAGE *
      *                TO THE CUSTOMER RESPONSE QUEUE, MESSAGE WILL   *
      *                BE PROCESSED BY THE CALLING APPLICATION        *
      *                                                               *
      *    CALLED BY:  P00700-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P06000-SEND-RESPONSE.

      *****************************************************************
      *    WRITE THE CUSTOMER ORDER INQUIRY RESPONSE MESSAGE          *
      *****************************************************************

           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
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
               PERFORM  P06030-CREDIT-AUTH-REQ
                   THRU P06030-CREDIT-AUTH-REQ-EXIT.


      *****************************************************************
      *    WRITE THE TRANSACTION LOG MESSAGE AS MIRROR IMAGE OF THE   *
      *    ORDER QUERY RESULT MESSAGE                                 *
      *    (USED FOR SCENARIO TO ATTEMPT WRITE TO A PUT INHIBITED QUE)*
      *****************************************************************

           IF (NO-ERROR-FOUND) AND (WMF-ACTIVE-SCENARIO (15) = 'Y')
               MOVE 15                 TO WMF-CURRENT-SCENARIO
               PERFORM P06100-TRANS-LOG-PROCESS
                  THRU P06100-TRANS-LOG-PROCESS-EXIT
               MOVE ZEROES             TO WMF-CURRENT-SCENARIO.


      *****************************************************************
      *    IF SCENARIO ACTIVE ALLOW TRUNCATED MESSAGE TO BE RECEIVED  *
      *    (RESULTS WILL BE USED LATER TO PRODUCE CICS ABEND ASRA)    *
      *****************************************************************

           IF WMF-ACTIVE-SCENARIO (17) = 'Y'
               MOVE 17                 TO WMF-CURRENT-SCENARIO
               PERFORM  P06200-GET-SHORT-MSG
                   THRU P06200-GET-SHORT-MSG-EXIT
               MOVE ZEROES             TO WMF-CURRENT-SCENARIO.


      *****************************************************************
      *    CALCULATE CUSTOMER TOTAL ORDER FEES                        *
      *    (IF SCENARIO TO PROCESS A TRUNCATED MESSAGE IS ACTIVE,     *
      *    THIS CALCULATION WILL PRODUCE A CICS ABEND ASRA AS THE     *
      *    MESSAGE ORDER FEE FIELD WILL CONTAIN INVALID NUMERICS)     *
      *****************************************************************

           COMPUTE WMF-TOTAL-FEES ROUNDED =
               WMF-TOTAL-ORDERS * MQS-CUSTOMER-ORDER-FEE.

       P06000-SEND-RESPONSE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06030-CREDIT-AUTH-REQ                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO WRITE THE MQSERIES CREDIT AUTH      *
      *                REQUEST MESSAGE. THE MESSAGE TRIGGERS THE      *
      *                CREDIT AUTH PROCESSING PROGRAM (CICS PROGRAM   *
      *                PDA018).                                       *
      *                                                               *
      *    CALLED BY:  P06000-SEND-RESPONSE.                          *
      *                                                               *
      *****************************************************************

       P06030-CREDIT-AUTH-REQ.

      *****************************************************************
      *    WRITE THE CUSTOMER CREDIT AUTHORIZATION REQUEST MESSAGE    *
      *    (ONLY WRITE IF NO ERRORS FOUND DURING ORDER PROCESS   )    *
      *****************************************************************

           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
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


           MOVE SPACES                 TO MQS-CREDIT-AUTH-REQ-MESSAGE.
           MOVE ZEROES                 TO MQS-CREDIT-RETURN-CODE.
           MOVE MQS-CUSTOMER-ID        TO MQS-CREDIT-CUSTOMER-ID.
           MOVE LENGTH OF MQS-CREDIT-AUTH-REQ-MESSAGE
                                       TO MQS-BUFFERLENGTH.
           MOVE MQS-CREDIT-AUTH-REQ-MESSAGE
                                       TO MQS-BUFFER.
           MOVE MQS-HOBJECT-CREDIT-AUTH-REQ-Q
                                       TO MQS-HOBJECT.

           PERFORM P07200-MQS-PUT
              THRU P07200-MQS-PUT-EXIT.


       P06030-CREDIT-AUTH-REQ-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06100-TRANS-LOG-PROCESS                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO WRITE THE MQSERIES TRANSACTION      *
      *                LOG MESSAGES FOR THE ORDER QUERY PROCESS       *
      *                (USED FOR SCENARIO TO ATTEMPT WRITE TO A PUT  *
      *                INHIBITED QUEUE)                               *
      *                                                               *
      *    CALLED BY:  P06000-SEND-RESPONSE                           *
      *                                                               *
      *****************************************************************

       P06100-TRANS-LOG-PROCESS.

           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
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
      *    CHECK FOR APPROPRIATE REASONCODE TO SATISFY THE INTENDED   *
      *    SCENARIO ERROR, OTHERWISE CONSIDER FATAL ERROR-- TERMINATE *
      *****************************************************************

           IF MQS-REASONCODE           =  MQRC-PUT-INHIBITED
               MOVE MQS-REASONCODE     TO WMF-DUMP-CODE-R
               PERFORM P70500-TRANS-DUMP-ROUTINE
                  THRU P70500-TRANS-DUMP-ROUTINE-EXIT
           ELSE
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA017'           TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQPUT'            TO WPME-FUNCTION-1
               MOVE MQS-OBJECTTYPE-DESC
                                       TO WPME-FUNCTION-2
               MOVE 'P06100'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P06100-TRANS-LOG-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06200-GET-SHORT-MSG                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ THE CUSTOMER PAYMENT MESSAGE   *
      *                (FROM TRANSACTION Q) FORCING MSG TRUNCATION.   *
      *                PROCESS ONLY EXECUTED WHEN SPECIFIC SCENARIO   *
      *                IS ACTIVE. RESULTS USED LATER IN PROGRAM       *
      *                TO PRODUCE A CICS ABEND ASRA.                  *
      *                                                               *
      *                USED IN SCENARIO 17 (ABEND ASRA), TRUNCATED    *
      *                MESSAGE RENDERS MQS-CUSTOMER-ORDER-FEE AS AN   *
      *                INVALID NUMERIC FIELD                          *
      *                                                               *
      *    CALLED BY:  P06000-SEND RESPONSE                           *
      *                                                               *
      *****************************************************************

       P06200-GET-SHORT-MSG.

      *****************************************************************
      *    SET MESSAGE OPTIONS FOR TRUNCATED MESSAGE                  *
      *****************************************************************

           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
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

           PERFORM P07400-MQS-GET
              THRU P07400-MQS-GET-EXIT.


      *****************************************************************
      *    CHECK FOR APPROPRIATE REASONCODE TO SATISFY THE INTENDED   *
      *    SCENARIO ERROR, OTHERWISE CONSIDER FATAL ERROR-- TERMINATE *
      *****************************************************************

           IF MQS-REASONCODE           =  MQRC-TRUNCATED-MSG-ACCEPTED
               MOVE MQS-BUFFER         TO MQS-CUSTOMER-MESSAGE
           ELSE
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA017'           TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQGET'            TO WPME-FUNCTION-1
               MOVE MQS-OBJECTTYPE-DESC
                                       TO WPME-FUNCTION-2
               MOVE 'P06200'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P06200-GET-SHORT-MSG-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06900-CLOSE-THE-QUEUES                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CLOSE ANY OPEN MQSERIES QUEUES      *
      *                                                               *
      *    CALLED BY:  P00700-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P06900-CLOSE-THE-QUEUES.

      *****************************************************************
      *    CLOSE THE MQSERIES CUSTOMER REQUEST QUEUE                   *
      *****************************************************************

           IF MQS-HOBJECT-REQUEST-Q    >  ZEROES
               MOVE MQHC-DEF-HCONN     TO MQS-HCONN
               COMPUTE MQS-OPTIONS     =  MQCO-NONE
               MOVE MQS-HOBJECT-REQUEST-Q
                                       TO MQS-HOBJECT

               PERFORM P07300-MQS-CLOSE
                  THRU P07300-MQS-CLOSE-EXIT.


      *****************************************************************
      *    CLOSE THE MQSERIES CUSTOMER RESPONSE QUEUE                  *
      *****************************************************************

           IF MQS-HOBJECT-RESPONSE-Q   >  ZEROES
               MOVE MQHC-DEF-HCONN     TO MQS-HCONN
               COMPUTE MQS-OPTIONS     =  MQCO-NONE
               MOVE MQS-HOBJECT-RESPONSE-Q
                                       TO MQS-HOBJECT

               PERFORM P07300-MQS-CLOSE
                  THRU P07300-MQS-CLOSE-EXIT.


      *****************************************************************
      *    CLOSE THE MQSERIES TEMPORARY DYNAMIC QUEUE                 *
      *****************************************************************

           IF MQS-HOBJECT-DYNAMIC-Q    >  ZEROES
               MOVE MQHC-DEF-HCONN     TO MQS-HCONN
               COMPUTE MQS-OPTIONS     =  MQCO-NONE
               MOVE MQS-HOBJECT-DYNAMIC-Q
                                       TO MQS-HOBJECT

               PERFORM P07300-MQS-CLOSE
                  THRU P07300-MQS-CLOSE-EXIT.


      *****************************************************************
      *    CLOSE THE MQSERIES REPORT TEMPORARY DYNAMIC QUEUE          *
      *****************************************************************

           IF MQS-HOBJECT-REPORT-Q     >  ZEROES
               MOVE MQHC-DEF-HCONN     TO MQS-HCONN
               COMPUTE MQS-OPTIONS     =  MQCO-NONE
               MOVE MQS-HOBJECT-REPORT-Q
                                       TO MQS-HOBJECT

               PERFORM P07300-MQS-CLOSE
                  THRU P07300-MQS-CLOSE-EXIT.


      *****************************************************************
      *    CLOSE THE MQSERIES TRANSACTION LOG QUEUE 01                 *
      *****************************************************************

           IF MQS-HOBJECT-LOG-01-Q     >  ZEROES
               MOVE MQHC-DEF-HCONN     TO MQS-HCONN
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
               MOVE MQHC-DEF-HCONN     TO MQS-HCONN
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
               MOVE MQHC-DEF-HCONN     TO MQS-HCONN
               COMPUTE MQS-OPTIONS     =  MQCO-NONE
               MOVE MQS-HOBJECT-REMOTE-Q
                                       TO MQS-HOBJECT

               PERFORM P07300-MQS-CLOSE
                  THRU P07300-MQS-CLOSE-EXIT.


      *****************************************************************
      *    CLOSE THE MQSERIES TRANSACTION QUEUE (PERMANENT)            *
      *****************************************************************

           IF MQS-HOBJECT-TRANSACTION-Q >  ZEROES
               MOVE MQHC-DEF-HCONN     TO MQS-HCONN
               COMPUTE MQS-OPTIONS     =  MQCO-NONE
               MOVE MQS-HOBJECT-TRANSACTION-Q
                                       TO MQS-HOBJECT

               PERFORM P07300-MQS-CLOSE
                  THRU P07300-MQS-CLOSE-EXIT.


      *****************************************************************
      *    CLOSE THE MQSERIES CREDIT AUTHORIZATION REQUEST QUEUE       *
      *****************************************************************

           IF MQS-HOBJECT-CREDIT-AUTH-REQ-Q  >  ZEROES
               MOVE MQHC-DEF-HCONN     TO MQS-HCONN
               COMPUTE MQS-OPTIONS     =  MQCO-NONE
               MOVE MQS-HOBJECT-CREDIT-AUTH-REQ-Q
                                       TO MQS-HOBJECT

               PERFORM P07300-MQS-CLOSE
                  THRU P07300-MQS-CLOSE-EXIT.


       P06900-CLOSE-THE-QUEUES-EXIT.
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
      *    CALLED BY:  P00300-OPEN-MAIN-QUEUES                        *
      *                P01500-OPEN-OTHER-QUEUES                       *
      *                                                               *
      *****************************************************************

       P07100-MQS-OPEN.

           CALL 'MQOPEN'      USING    MQS-HCONN
                                       MQOD
                                       MQS-OPTIONS
                                       MQS-HOBJECT
                                       MQS-COMPCODE
                                       MQS-REASONCODE.


      *****************************************************************
      *    CHECK FOR MQSERIES ERROR, IF ERROR ENCOUNTERED FORMAT      *
      *    ERROR MESSAGE, CALL ERROR ROUTINE TO TERMINATE             *
      *    (ALLOW PASSTHRU OF SPECIAL SCENARIO PROCESSING)            *
      *****************************************************************

           IF WMF-CURRENT-SCENARIO    =  14
               GO TO P07100-MQS-OPEN-EXIT.


           IF MQS-COMPCODE             =  MQCC-OK
               NEXT SENTENCE
           ELSE
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA017'           TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQOPEN'           TO WPME-FUNCTION-1
               MOVE MQS-OBJECTTYPE-DESC
                                       TO WPME-FUNCTION-2
               MOVE 'P07100'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


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
      *    CALLED BY:  P06000-SEND-RESPONSE                           *
      *                                                               *
      *****************************************************************

       P07200-MQS-PUT.

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
      *    (ALLOW PASSTHRU OF SPECIAL SCENARIO PROCESSING)            *
      *****************************************************************

           IF WMF-CURRENT-SCENARIO     =  15
               GO TO P07200-MQS-PUT-EXIT.


           IF MQS-COMPCODE             =  MQCC-OK
               NEXT SENTENCE
           ELSE
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA017'           TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQPUT'            TO WPME-FUNCTION-1
               MOVE MQS-OBJECTTYPE-DESC
                                       TO WPME-FUNCTION-2
               MOVE 'P07200'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


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
      *    CALLED BY:  P06900-CLOSE-THE-QUEUES                        *
      *                                                               *
      *****************************************************************

       P07300-MQS-CLOSE.

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
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA017'           TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQCLOSE'          TO WPME-FUNCTION-1
               MOVE MQS-OBJECTTYPE-DESC
                                       TO WPME-FUNCTION-2
               MOVE 'P07300'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


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
      *    CALLED BY:  P01000-GET-REQUEST                             *
      *                                                               *
      *****************************************************************

       P07400-MQS-GET.

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
      *    (ALLOW PASSTHRU OF SPECIAL SCENARIO PROCESSING)            *
      *****************************************************************

           IF WMF-CURRENT-SCENARIO     =  16  OR  17
               GO TO P07400-MQS-GET-EXIT.


           IF MQS-COMPCODE             =  MQCC-OK
               NEXT SENTENCE
           ELSE
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA017'           TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQGET'            TO WPME-FUNCTION-1
               MOVE MQS-OBJECTTYPE-DESC
                                       TO WPME-FUNCTION-2
               MOVE 'P07400'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P07400-MQS-GET-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P70000-ERROR-ROUTINE                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO HANDLE NON-FATAL ERROR MESSAGE      *
      *                PROCESSING                                     *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************

       P70000-ERROR-ROUTINE.

           MOVE 1                      TO WS-ERROR-FOUND-SW
                                          MQS-RETURN-CODE.

           IF MQS-SCREEN-MESSAGE       >  SPACES
               NEXT SENTENCE
           ELSE
               MOVE WMF-MESSAGE-AREA   TO MQS-SCREEN-MESSAGE.


       P70000-ERROR-ROUTINE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P70500-TRANS-DUMP-ROUTINE                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO TAKE A CICS TRANSACTION DUMP WITH   *
      *                A PRE-DETERMINED TRANSACTION DUMP CODE         *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************

       P70500-TRANS-DUMP-ROUTINE.

                                                                        00636300
           EXEC CICS DUMP                                                   0063
                     TRANSACTION                                            0063
                     DUMPCODE  (WMF-DUMP-CODE)                              0063
                     NOHANDLE
                     RESP      (WS-RESPONSE-CODE)
           END-EXEC.


      *****************************************************************
      *    IF ERROR, FORMAT ERROR INFORMATION AND TERMINATE           *
      *****************************************************************

           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               NEXT SENTENCE
           ELSE
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
               MOVE 'PDA017'           TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
               MOVE 'CICS DUMP TRANSACTION'
                                       TO WPCE-COMMAND
               MOVE 'P70500'           TO WPCE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.
                                                                        00640801

       P70500-TRANS-DUMP-ROUTINE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P99100-GENERAL-ERROR                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CATCH ANY CICS ERROR(S) NOT         *
      *                SPECIFICALLY PROCESSED BY A CICS HANDLE        *
      *                CONDITION                                      *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************

       P99100-GENERAL-ERROR.


           MOVE 9                      TO WS-ERROR-FOUND-SW
           MOVE 'CICS'                 TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA017'               TO WPCE-PROGRAM-ID.
           MOVE EIBRESP                TO WPCE-RESPONSE-CODE.
           MOVE 'UNHANDLED CICS ERROR' TO WPCE-COMMAND.
           MOVE 'P99100'               TO WPCE-PARAGRAPH.
           PERFORM  P99500-PDA-ERROR
               THRU P99500-PDA-ERROR-EXIT.


       P99100-GENERAL-ERROR-EXIT.
           EXIT.
           EJECT
                                                                        00631700
                                                                        00631800
      ***************************************************************** 00631900
      *                                                               * 00632000
      *    P R O D U C T    D E M O N S T R A T I O N     A P P L     * 00632100
      *                                                               * 00632200
      *             E R R O R    R O U T I N E S                      * 00632300
      *                                                               * 00632400
      *                                                               * 00632500
      ***************************************************************** 00632600
                                                                        00632700
      ***************************************************************** 00632800
      *                                                               * 00632900
      *    PARAGRAPH:  P99500-PDA-ERROR                               * 00633000
      *                                                               * 00633100
      *    FUNCTION :  ROUTINE TO HANDLE FATAL / TERMINATING CICS,    * 00633200
      *                DB2, IMS-DLI, MQSERIES ERRORS                  * 00633302
      *                                                               * 00633400
      *                THE MQSERIES MESSAGE AREA IS FORMATTED WITH    * 00633500
      *                ERROR INFORMATION AND A MQSERIES RESPONSE      * 00633600
      *                MESSAGE IS SENT TO THE CALLING APPLICATION.    * 00633600
      *                                                               * 00633400
      *                A CICS TRANSACTION DUMP IS ALSO GENERATED.     * 00633600
      *                                                               * 00633700
      *    CALLED BY:  GLOBAL                                         * 00633800
      *                                                               * 00633900
      ***************************************************************** 00634000
                                                                        00634100
       P99500-PDA-ERROR.                                                00634200
                                                                        00634300
      ***************************************************************** 00634400
      *      SUSPEND ANY HANDLE CONDITIONS IN EFFECT                  * 00634500
      ***************************************************************** 00634600
                                                                        00634700
           EXEC CICS PUSH HANDLE                                        00634800
           END-EXEC.                                                    00634900
                                                                        00635000
                                                                        00635100
      ***************************************************************** 00635200
      *      ROLLBACK ANY TRANSACTION UPDATES                         * 00635300
      ***************************************************************** 00635400
                                                                        00635500
           EXEC CICS SYNCPOINT ROLLBACK                                 00635600
           END-EXEC.                                                    00635700
                                                                        00635800
                                                                        00635900
      ***************************************************************** 00636000
      *      FORMAT APPROPRIATE ERROR TEXT (APPLIES TO FATAL ERRORS)  * 00636100
      ***************************************************************** 00636200
                                                                        00636300
           MOVE WS-ERROR-FOUND-SW      TO MQS-RETURN-CODE.              00636300
           MOVE WS-PDA-ERROR-TYPE      TO MQS-PDA-ERROR-TYPE.           00636300
                                                                        00636300
           IF PDA-DB2-ERROR                                             00636400
               MOVE WS-PDA-DB2-ERROR-01                                 00636500
                                       TO MQS-PDA-ERROR-LINE-01         00636600
               MOVE WS-PDA-DB2-ERROR-02                                 00636700
                                       TO MQS-PDA-ERROR-LINE-02         00636600
           ELSE                                                         00636900
           IF PDA-IMS-ERROR                                             00637000
               MOVE WS-PDA-IMS-ERROR-01                                 00637100
                                       TO MQS-PDA-ERROR-LINE-01         00636600
               MOVE WS-PDA-IMS-ERROR-02                                 00637300
                                       TO MQS-PDA-ERROR-LINE-02         00636600
           ELSE                                                         00637500
           IF PDA-MQSERIES-ERROR                                        00637602
               MOVE WS-PDA-MQSERIES-ERROR-01                            00637702
                                       TO MQS-PDA-ERROR-LINE-01         00636600
               MOVE WS-PDA-MQSERIES-ERROR-02                            00637902
                                       TO MQS-PDA-ERROR-LINE-02         00636600
           ELSE                                                         00638102
               MOVE WS-PDA-CICS-ERROR-01                                00638200
                                       TO MQS-PDA-ERROR-LINE-01         00636600
               MOVE WS-PDA-CICS-ERROR-02                                00638400
                                       TO MQS-PDA-ERROR-LINE-02.        00636600
                                                                        00635900
                                                                        00635900
      ***************************************************************** 00636000
      *      TAKE A TRANSACTION DUMP (FOR FATAL ERRORS ONLY)          * 00636100
      ***************************************************************** 00636200
                                                                        00636300
           IF FATAL-ERROR-FOUND                                         00636300
               EXEC CICS DUMP                                               0063
                         TRANSACTION                                        0063
                         DUMPCODE('PDER')                                   0063
               END-EXEC.                                                    0063
                                                                        00635900
                                                                        00635900
      ***************************************************************** 00636000
      *      IF POSSIBLE,                                             * 00636100
      *      SEND A MQSERIES RESPONSE TO THE CALLING APPLICATION      * 00636100
      *      (MESSAGE CONTAINS ALL ERROR RELATED TEXT)                * 00636100
      ***************************************************************** 00636200
                                                                        00640701
           IF MQS-HOBJECT-RESPONSE-Q   > ZEROES                         00640701
               PERFORM  P99600-PDA-ERROR-MQMSG                          00640801
                   THRU P99600-PDA-ERROR-MQMSG-EXIT.                    00640801
                                                                        00640801
                                                                        00640801
      ***************************************************************** 00640901
      * RETURN CONTROL TO CICS                                        * 00641001
      ***************************************************************** 00641101
                                                                        00641201
           EXEC CICS RETURN                                             00641301
           END-EXEC.                                                    00641401
                                                                        00641501
                                                                        00641601
           GOBACK.                                                      00641701
                                                                        00641801
       P99500-PDA-ERROR-EXIT.                                           00641901
           EXIT.                                                        00642001
           EJECT                                                        00650000

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P99600-PDA-ERROR-MQMSG                         *
      *                                                               *
      *    FUNCTION :  ROUTINE SEND A MQSERIES RESPONSE TO THE        *
      *                CALLING APPLICATION IN AN ERROR SITUATION.     *
      *                                                               *
      *    CALLED BY:  P99500-PDA-ERROR                               *
      *                                                               *
      *****************************************************************

       P99600-PDA-ERROR-MQMSG.
                                                                        00640801
      ***************************************************************** 00640901
      * SET MQSERIES PUT MESSAGE OPTIONS, WRITE MESSAGE               * 00641001
      ***************************************************************** 00641101

           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
           MOVE MQMD-CURRENT-VERSION   TO MQMD-VERSION.
           MOVE MQRO-NONE              TO MQMD-REPORT.
           MOVE SPACES                 TO MQMD-REPLYTOQMGR
                                          MQMD-REPLYTOQ.
           MOVE MQPER-NOT-PERSISTENT   TO MQMD-PERSISTENCE.
           MOVE MQCI-NONE              TO MQMD-CORRELID.

           IF WMF-SAVE-MSGID           NOT = SPACES
               MOVE WMF-SAVE-MSGID     TO MQMD-MSGID.

           MOVE MQPRI-PRIORITY-AS-Q-DEF
                                       TO MQMD-PRIORITY.
           MOVE MQENC-NATIVE           TO MQMD-ENCODING.
           MOVE MQCCSI-Q-MGR           TO MQMD-CODEDCHARSETID.
           MOVE 5000                   TO MQMD-EXPIRY.


           MOVE MQPMO-CURRENT-VERSION  TO MQPMO-VERSION.
           COMPUTE MQPMO-OPTIONS       =  MQPMO-NO-SYNCPOINT +
                                          MQPMO-FAIL-IF-QUIESCING.
           MOVE LENGTH OF MQS-RESULTS-MESSAGE
                                       TO MQS-BUFFERLENGTH.
           MOVE MQS-RESULTS-MESSAGE    TO MQS-BUFFER.


           CALL 'MQPUT'       USING    MQS-HCONN
                                       MQS-HOBJECT-RESPONSE-Q
                                       MQMD
                                       MQPMO
                                       MQS-BUFFERLENGTH
                                       MQS-BUFFER
                                       MQS-COMPCODE
                                       MQS-REASONCODE.
                                                                        00640801
      ***************************************************************** 00640901
      * CLOSE THE MQSERIES RESPONSE QUEUE                             * 00641001
      ***************************************************************** 00641101

           MOVE MQHC-DEF-HCONN     TO MQS-HCONN.
           COMPUTE MQS-OPTIONS     =  MQCO-NONE.

           CALL 'MQCLOSE' USING    MQS-HCONN
                                   MQS-HOBJECT-RESPONSE-Q
                                   MQS-OPTIONS
                                   MQS-COMPCODE
                                   MQS-REASONCODE.


       P99600-PDA-ERROR-MQMSG-EXIT.
           EXIT.
           EJECT