       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA016.

      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA016                                            *
      * TRANS   :   PD16                                              *
      * MAPSET  :   PDA016M                                           *
      *                                                               *
      * FUNCTION:   PROGRAM PDA016 IS THE CUSTOMER ORDER INQUIRY      *
      *             FUNCTION. THE FUNCTION ALLOWS ANY THIRD PARTY     *
      *             CUSTOMER TO QUERY THE ORDER DATABASE FOR ALL      *
      *             ORDER INFORMATION RELATING TO THE SPECIFIC        *
      *             CUSTOMER IDENTIFICATION.                          *
      *                                                               *
      *             THE PROGRAM PROVIDES MQSERIES FUNCTIONALITY FOR   *
      *             THE VARIOUS PRODUCT LINES. THE QUERY REQUEST IS   *
      *             ISSUED VIA A MQSERIES MESSAGE (PROCESSED BY       *
      *             PROGRAM PDA017, CUSTOMER ORDER PROCESSING PROGRAM)*
      *             WITH THE QUERY RESULTS RETURNED VIA AN MQSERIES   *
      *             MESSAGE IN A RESPONSE QUEUE.                      *
      *                                                               *
      *             FUNCTION IS INVOKED FROM THE MAIN MENU (PDA001)   *
      *                                                               *
      * FILES   :   CUSTOMER         -  VSAM KSDS (READ-ONLY)         *
      *                                                               *
      *                                                               *
      * TRANSACTIONS GENERATED:                                       *
      *             PD17       CUSTOMER ORDER PROCESSING PROGRAM      *
      *                        (VIA MQSERIES MESSAGE TRIGGER)         *
      *             PD01       MAIN MENU                              *
      *                                                               *
      *                                                               *
      * PFKEYS  :   PF12  =    EXIT, RETURN TO MAIN MENU              *
      *                                                               *
      *                                                               *
      *****************************************************************
      *             PROGRAM CHANGE LOG                                *
      *             -------------------                               *
      *                                                               *
      *  DATE       UPDATED BY            CHANGE DESCRIPTION          *
      *  --------   --------------------  --------------------------  *
      *                                                               *
      *  09/23/03   PAUL BARON            MIGRATE TO MQSERIES V5.3.1, *
      *                                   USE HIGHER VERSION MQSERIES *
      *                                   COPYBOOKS FOR MQ STRUCTURES *
      *                                                               *
      *  05/19/03   PAUL BARON            ADDED PROCESSING FOR CREDIT *
      *                                   STATUS, OBTAINED FROM       *
      *                                   MQSERIES MESSAGE PRODUCED   *
      *                                   BY PROGRAM PDA018. CUSTOMER *
      *                                   CREDIT STATUS IS ALSO ADDED *
      *                                   TO THE SCREEN DISPLAY.      *
      *                                                               *
      *                                                               *
      *  08/02/02   PAUL BARON            ADDED PROCESSING FOR        *
      *                                   SCENARIOS 18 AND 19,CURRENCY*
      *                                   UNIT DISPLAY. THE TOTAL     *
      *                                   DOLLAR AMOUNT AND AVERAGE   *
      *                                   DOLLAR AMOUNT FIELD HEADINGS*
      *                                   ARE CHANGED APPROPRIATELY   *
      *                                   IF EITHER SCENARIO 18 OR 19 *
      *                                   ARE ACTIVE. SEE PARAGRAPH   *
      *                                   P03900-CHECK-CURRENCY       *
      *                                                               *
      *  XX/XX/XX   XXXXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXX  *
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
       77  WS-SCREEN-MAX               PIC S9(04)   COMP  VALUE +14.
       77  WS-MESSAGE-LTH              PIC S9(04)   COMP  VALUE +79.
       77  WS-CKTI-TRANS-LTH           PIC S9(04)   COMP  VALUE +0.
       77  WS-RESPONSE-CODE            PIC S9(08)   COMP  VALUE +0.

      *****************************************************************
      *    SWITCHES                                                   *
      *****************************************************************
       01  WS-SWITCHES.

           05  WS-TRANS-INTENT-SW      PIC X(01)             VALUE 'I'.
               88  INQUIRY-TRANS                             VALUE 'I'.
               88  UPDATE-TRANS                              VALUE 'U'.

           05  WS-ERROR-FOUND-SW       PIC X(01)             VALUE 'N'.
               88  ERROR-FOUND                               VALUE 'Y'.
               88  NO-ERROR-FOUND                            VALUE 'N'.

           EJECT
      *****************************************************************
      *    MISCELLANEOUS WORK FIELDS                                  *
      *****************************************************************

       01  WS-MISCELLANEOUS-FIELDS.
           05  WMF-ABSTIME             PIC S9(15)  VALUE +0    COMP-3.
           05  WMF-DATE-MMDDYY         PIC X(08)   VALUE SPACES.
           05  WMF-TIME-HHMMSS         PIC X(08)   VALUE SPACES.
           05  WMF-MESSAGE-AREA        PIC X(79)   VALUE SPACES.
           05  WMF-PER-ORDER-FEE       PIC 9(7)V99 VALUE 6.75  COMP-3.

           05  WMF-UNDERSCORE-LOWVALUE.
               10  FILLER              PIC X(01)   VALUE '_'.
               10  FILLER              PIC X(01)   VALUE LOW-VALUES.
           05  WMF-UNDERSCORE-LOWVALUE-R
                                       REDEFINES
                                       WMF-UNDERSCORE-LOWVALUE
                                       PIC X(02).

           05  WMF-SPACES-LOWVALUE.
               10  FILLER              PIC X(01)   VALUE SPACES.
               10  FILLER              PIC X(01)   VALUE LOW-VALUES.
           05  WMF-SPACES-LOWVALUE-R   REDEFINES
                                       WMF-SPACES-LOWVALUE
                                       PIC X(02).

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
      *    CICS DEFINITIONS                                           *
      *****************************************************************

      *****************************************************************
      *         CICS ATTRIBUTE VALUES                                 *
      *****************************************************************

           COPY DFHBMSCA.
           EJECT
      *****************************************************************
      *         CICS ATTENTION IDENDIFIER VALUES                      *
      *****************************************************************

           COPY DFHAID.
           EJECT
      *****************************************************************
      *         MAP DSECTS -- MAINT MENU PDA016M                      *
      *****************************************************************

           COPY PDA016M.

      *****************************************************************
      *    MAP DSECT REDEFINITION                                     *
      *****************************************************************

       01  PDA016I-R                   REDEFINES PDA016I.
           05  FILLER                  PIC X(325).
           05  M-ORDER-DETAIL          OCCURS 14 TIMES.
               10  FILLER              PIC X(05).
               10  M-ORDER-NUMBER      PIC X(10).
               10  M-ORDER-NUMBER-R    REDEFINES M-ORDER-NUMBER
                                       PIC 9(10).
               10  FILLER              PIC X(05).
               10  M-ORDER-AMOUNT      PIC X(12).
               10  M-ORDER-AMOUNT-R    REDEFINES M-ORDER-AMOUNT
                                       PIC Z,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(84).
           EJECT

      *****************************************************************
      *    IMS / DLI DEFINITIONS                                      *
      *****************************************************************
      *****************************************************************
      *    NO IMS / DLI PROCESSING IN THE PROGRAM                     *
      *****************************************************************

      *****************************************************************
      *    FILE LAYOUTS                                               *
      *****************************************************************

           COPY VCUSTOMR.
           EJECT

      *****************************************************************
      *    DB2  DEFINITIONS                                           *
      *****************************************************************
      *****************************************************************
      *    NO DB2 PROCESSING IN THE PROGRAM                           *
      *****************************************************************
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

           05  MQS-CREDIT-AUTH-RESP-QUEUE
                                       PIC X(48)          VALUE
               'PDAPROD.H01AC013.CREDIT.AUTH.RESP.QUEUE'.

           05  MQS-STARTCKTI-TRANS     PIC X(52)          VALUE
               'CKQC STARTCKTI  PDAPROD.H01AC013.CUSTOMER.INIT.QUEUE'.

      *****************************************************************
      *    MQSERIES MESSAGE PUT ON CUSTOMER QUEUE TO TRIGGER PDA017   *
      *****************************************************************

       01  MQS-BUFFER-OUT              PIC X(50)          VALUE SPACES.

       01  MQS-CUSTOMER-MESSAGE        REDEFINES MQS-BUFFER-OUT.
           05  MQS-CUSTOMER-USERID     PIC X(08).
           05  MQS-CUSTOMER-ID         PIC X(32).
           05  MQS-CUSTOMER-ORDER-FEE  PIC 9(7)V99.
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

       01  DFHCOMMAREA.
           COPY PDACOMM.

           05  PC-PROGRAM-WORKAREA-R   REDEFINES
                                       PC-PROGRAM-WORKAREA.
               10 PC-PDA016-CUSTOMER-ID
                                       PIC X(32).
               10 FILLER               PIC X(968).
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


           EXEC CICS HANDLE CONDITION
                ERROR(P99100-GENERAL-ERROR)
           END-EXEC.


           PERFORM  P00050-INITIALIZE                                   TAGGED
               THRU P00050-INITIALIZE-EXIT.                             CODE
                                                                        TESTING
                                                                        03/13/01
           PERFORM  P00100-MAIN-PROCESS
               THRU P00100-MAIN-PROCESS-EXIT.


           PERFORM  P00200-CICS-RETURN
               THRU P00200-CICS-RETURN-EXIT.

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
      *    VERIFY THE COMMAREA IS PRESENT AND CORRECT LENGTH          *
      *****************************************************************

           IF EIBCALEN                 > ZEROES
               IF EIBCALEN             = PC-COMMAREA-LTH
                   NEXT SENTENCE
               ELSE
                   MOVE 'CICS'         TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA016'       TO WPCE-PROGRAM-ID
                   MOVE ZEROES         TO WPCE-RESPONSE-CODE
                   MOVE 'COMMAREA LENGTH NOT CORRECT'
                                       TO WPCE-COMMAND
                   MOVE 'P00050'       TO WPCE-PARAGRAPH
                   PERFORM  P99500-PDA-ERROR
                       THRU P99500-PDA-ERROR-EXIT
           ELSE
               MOVE PM019-ENTER-APPLICATION
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P80400-SEND-MESSAGE
                   THRU P80400-SEND-MESSAGE-EXIT
               GO TO P00050-INITIALIZE-EXIT.

      *****************************************************************
      *    INITIALIZE SWITCHES, SUBSCRIPTS, ETC.                      *
      *****************************************************************

           MOVE 'I'                    TO WS-TRANS-INTENT-SW.
           MOVE 'N'                    TO WS-ERROR-FOUND-SW.
                                                                        00010000
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME.          00020001

      *****************************************************************
      *    OBTAIN CURRENT DATE AND TIME FOR DISPLAY                   *
      *****************************************************************

           EXEC CICS ASKTIME
                     ABSTIME (WMF-ABSTIME)
           END-EXEC.


           EXEC CICS FORMATTIME
                     ABSTIME (WMF-ABSTIME)
                     MMDDYY  (WMF-DATE-MMDDYY)
                     DATESEP ('/')
                     TIME    (WMF-TIME-HHMMSS)
                     TIMESEP
                     NOHANDLE
                     RESP    (WS-RESPONSE-CODE)
           END-EXEC.

           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               NEXT SENTENCE
           ELSE
               MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
               MOVE 'PDA016'           TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
               MOVE 'CICS FORMATTIME ABSTIME'
                                       TO WPCE-COMMAND
               MOVE 'P00050'           TO WPCE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P00050-INITIALIZE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00100-MAIN-PROCESS                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL PROGRAM INQUIRY OR          *
      *                EDIT / UPDATE PROCESSES                        *
      *                                                               *
      *    CALLED BY:  P00000-MAINLINE                                *
      *                                                               *
      *****************************************************************

       P00100-MAIN-PROCESS.

      *****************************************************************
      *    DETERMINE TRANSACTION INTENT, INQUIRY OR EDIT / UPDATE     *
      *****************************************************************

           PERFORM  P00500-CHK-TRANS-INTENT
               THRU P00500-CHK-TRANS-INTENT-EXIT.


      *****************************************************************
      *    EITHER SEND INITIAL SCREEN OR PERFORM SCREEN EDIT PROCESS  *
      *****************************************************************

           IF INQUIRY-TRANS
               PERFORM  P01000-1ST-TIME-PROCESS
                   THRU P01000-1ST-TIME-PROCESS-EXIT
           ELSE
               PERFORM  P03000-PROCESS-TRANS
                   THRU P03000-PROCESS-TRANS-EXIT.


       P00100-MAIN-PROCESS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00200-CICS-RETURN                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETURN CONTROL TO CICS WITH THE     *
      *                NEXT TRANSACTION ID OPTION                     *
      *                                                               *
      *    CALLED BY:  P00000-MAINLINE                                *
      *                                                               *
      *****************************************************************

       P00200-CICS-RETURN.


           EXEC CICS RETURN
                     TRANSID       ('PD16')
                     COMMAREA      (PDA-COMMAREA)
                     LENGTH        (PC-COMMAREA-LTH)
                     NOHANDLE
                     RESP          (WS-RESPONSE-CODE)
           END-EXEC.


      *****************************************************************
      *    IF ERROR, FORMAT ERROR INFORMATION AND TERMINATE           *
      *****************************************************************

           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               NEXT SENTENCE
           ELSE
               MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
               MOVE 'PDA016'           TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
               MOVE 'CICS RETURN TRANSID'
                                       TO WPCE-COMMAND
               MOVE 'P00200'           TO WPCE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P00200-CICS-RETURN-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00500-CHK-TRANS-INTENT                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DETERMINE INQUIRY MODE (1ST TIME    *
      *                THRU) OR EDIT / UPDATE MODE                    *
      *                                                               *
      *    CALLED BY:  P00100-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P00500-CHK-TRANS-INTENT.

      *****************************************************************
      *    IF PREVIOUS PROGRAM IS NOT CUSTOMER ORDER INQUIRY          *
      *    SET INQUIRY MODE OTHERWISE SET EDIT / UPDATE MODE          *
      *****************************************************************

           IF PC-PREV-PGRMID           =  'PDA016'
               MOVE 'U'                TO WS-TRANS-INTENT-SW
           ELSE
               MOVE 'I'                TO WS-TRANS-INTENT-SW.

       P00500-CHK-TRANS-INTENT-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01000-1ST-TIME-PROCESS                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL PROCESSING TO SEND THE      *
      *                INITIAL PDA CUSTOMER ORDER INQUIRY SCREEN      *
      *                                                               *
      *    CALLED BY:  P00100-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P01000-1ST-TIME-PROCESS.

      *****************************************************************
      *    INITIALIZE COMMAREA AND MAP                                *
      *****************************************************************

           MOVE 'PDA016'               TO PC-PREV-PGRMID.
           MOVE SPACES                 TO PC-PROGRAM-WORKAREA.
           MOVE LOW-VALUES             TO PDA016I.
           MOVE WMF-DATE-MMDDYY        TO PDADATEO.
           MOVE EIBTRMID               TO PDATERMO.
           MOVE WMF-TIME-HHMMSS        TO PDATIMEO.

           PERFORM  P03900-CHECK-CURRENCY
               THRU P03900-CHECK-CURRENCY-EXIT.

      *****************************************************************
      *    FORMAT AND SEND THE FULL MAP -- LITERALS AND DATA          *
      *****************************************************************

           MOVE -1                     TO PDACUSTL.

           PERFORM  P80000-SEND-FULL-MAP
               THRU P80000-SEND-FULL-MAP-EXIT.

       P01000-1ST-TIME-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03000-PROCESS-TRANS                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE PROGRAM EDIT PROCESS    *
      *                                                               *
      *    CALLED BY:  P00100-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P03000-PROCESS-TRANS.

           MOVE 'PDA016'               TO PC-PREV-PGRMID.

      *****************************************************************
      *    RECEIVE THE INPUT MAP                                      *
      *****************************************************************

           PERFORM  P80200-RECEIVE-MAP
               THRU P80200-RECEIVE-MAP-EXIT.

           MOVE WMF-DATE-MMDDYY        TO PDADATEO.
           MOVE EIBTRMID               TO PDATERMO.
           MOVE WMF-TIME-HHMMSS        TO PDATIMEO.
           MOVE SPACES                 TO PDAMSGO.

           PERFORM  P03900-CHECK-CURRENCY
               THRU P03900-CHECK-CURRENCY-EXIT.


      *****************************************************************
      *    PERFORM THE SCREEN EDIT PROCESS (PFKEY AND DATA VALIDATION)*
      *****************************************************************

           PERFORM  P03100-EDIT-SCREEN
               THRU P03100-EDIT-SCREEN-EXIT.


      *****************************************************************
      *    IF NO EDIT ERRORS FOUND, AND THE CUSTOMER ID IS THE SAME   *
      *    AS THE PREVIOUS ENTRY,  PERFORM THE ORDER QUERY            *
      *****************************************************************

           IF (NO-ERROR-FOUND)
               IF (PDACUSTI  =  PC-PDA016-CUSTOMER-ID)
                   PERFORM  P05000-ORDER-QUERY
                       THRU P05000-ORDER-QUERY-EXIT
               ELSE
                   MOVE CUSTOMER-ID    TO PC-PDA016-CUSTOMER-ID
                   PERFORM  P80600-CLEAR-SCREEN
                       THRU P80600-CLEAR-SCREEN-EXIT
                   MOVE PM048-PROCEED  TO PDAMSGO
           ELSE
                   NEXT SENTENCE.


      *****************************************************************
      *    DISPLAY THE SCREEN, EXIT                                   *
      *****************************************************************

           MOVE -1                     TO PDACUSTL.

           PERFORM  P80100-SEND-MAP-DATAONLY
               THRU P80100-SEND-MAP-DATAONLY-EXIT.


       P03000-PROCESS-TRANS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE SCREEN EDIT PROCESS     *
      *                                                               *
      *    CALLED BY:  P03000-PROCESS-TRANS                           *
      *                                                               *
      *****************************************************************

       P03100-EDIT-SCREEN.

           INSPECT PDACUSTI
               CONVERTING  WMF-UNDERSCORE-LOWVALUE-R TO SPACES.

      *****************************************************************
      *    EDIT THE OPERATOR PROGRAM FUNCTION KEY SELECTION (PFKEY)   *
      *****************************************************************

           PERFORM  P03200-EDIT-PFKEY
               THRU P03200-EDIT-PFKEY-EXIT.

           IF ERROR-FOUND
               GO TO P03100-EDIT-SCREEN-EXIT.


      *****************************************************************
      *    EDIT THE OPERATOR ENTERED CUSTOMER ID (REQUIRED ENTRY)     *
      *****************************************************************

           PERFORM  P03300-EDIT-CUST-ID
               THRU P03300-EDIT-CUST-ID-EXIT.


       P03100-EDIT-SCREEN-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03200-EDIT-PFKEY                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO VALIDATE PROGRAM FUNCTION KEY USAGE *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03200-EDIT-PFKEY.

      *****************************************************************
      *    VALID KEYS ARE: ENTER, PF12, CLEAR                         *
      *****************************************************************

           IF EIBAID                   =  DFHENTER  OR  DFHPF12  OR     KCS418
                                          DFHCLEAR                      ADDED
               NEXT SENTENCE                                            88 LEVEL
           ELSE                                                         EDIT
               MOVE PM001-INVALID-PFKEY
                                       TO WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P03200-EDIT-PFKEY-EXIT.


      *****************************************************************
      *    PF12 FROM THIS SCREEN RETURNS USER TO THE MAIN MENU        *
      *****************************************************************

           IF EIBAID                   =  DFHPF12
               MOVE 'PDA001'           TO PC-NEXT-PGRMID
               PERFORM  P80300-XFER-CONTROL
                   THRU P80300-XFER-CONTROL-EXIT.

      *****************************************************************
      *    ALLOW USER TO EXIT APPLICATION WITH CLEAR KEY              *
      *    (SEND MESSAGE, ERASE SCREEN)                               *
      *****************************************************************

           IF EIBAID                   =   DFHCLEAR
               MOVE PM002-EXIT-APPLICATION
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P80400-SEND-MESSAGE
                   THRU P80400-SEND-MESSAGE-EXIT
               GO TO P03200-EDIT-PFKEY-EXIT.


       P03200-EDIT-PFKEY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03300-EDIT-CUST-ID                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO VALIDATE THE CUSTOMER IDENTIFICATION*
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03300-EDIT-CUST-ID.

           MOVE SPACES                 TO NAMEO.

      *****************************************************************
      *    CUSTOMER ID IS A REQUIRED ENTRY                            *
      *****************************************************************

           IF PDACUSTI                 > SPACES
               NEXT SENTENCE
           ELSE
               MOVE DFHUNINT           TO PDACUSTA
               MOVE PM009-ENTER-CUST-ID
                                       TO WMF-MESSAGE-AREA
               MOVE SPACES             TO PC-PDA016-CUSTOMER-ID
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P03300-EDIT-CUST-ID-EXIT.


      *****************************************************************
      *    ATTEMPT A CUSTOMER FILE READ (KEY EQUAL OR GREATER) BASED  *
      *    ON FULL OR PARTIAL KEY ENTERED                             *
      *****************************************************************

           PERFORM P04000-POPULATE-CUST
               THRU P04000-POPULATE-CUST-EXIT.


       P03300-EDIT-CUST-ID-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03900-CHECK-CURRENCY                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DETERMINE THE APPROPRIATE CURRENCY  *
      *                FOR DISPLAY BASED ON WHICH CURRENCY SCENARIOS  *
      *                ARE ACTIVE (IF ANY). THE U.S. DOLLAR IS THE    *
      *                DEFAULT.                                       *
      *                                                               *
      *    CALLED BY:  P01000-1ST-TIME-PROCESS                        *
      *                P03000-PROCESS-TRANS                           *
      *                                                               *
      *****************************************************************

       P03900-CHECK-CURRENCY.

           IF PC-ACTIVE-SCENARIO (18) = 'Y'
               MOVE 'TOTAL EURO AMOUNT    :'
                                       TO TOTAMTHO
               MOVE 'AVERAGE EURO AMOUNT  :'
                                       TO AVGAMTHO
           ELSE
           IF PC-ACTIVE-SCENARIO (19) = 'Y'
               MOVE 'TOTAL POUND AMOUNT   :'
                                       TO TOTAMTHO
               MOVE 'AVERAGE POUND AMOUNT :'
                                       TO AVGAMTHO
           ELSE
               MOVE 'TOTAL DOLLAR AMOUNT  :'
                                       TO TOTAMTHO
               MOVE 'AVERAGE DOLLAR AMOUNT:'
                                       TO AVGAMTHO.

       P03900-CHECK-CURRENCY-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P04000-POPULATE-CUST                           *
      *                                                               *
      *    FUNCTION :  READS VSAM AND POPULATES SCREEN FIELDS         *
      *                                                               *
      *    CALLED BY:  P03300-EDIT-CUST-ID                            *
      *                                                               *
      *****************************************************************

       P04000-POPULATE-CUST.

           MOVE PDACUSTI               TO CUSTOMER-ID.
           MOVE PC-USERID-NUMBER       TO CUSTOMER-PREFIX.


           PERFORM P09000-READ-CUSTOMER
              THRU P09000-READ-CUSTOMER-EXIT.

           IF ERROR-FOUND
               MOVE SPACES             TO PC-PDA016-CUSTOMER-ID
           ELSE
               MOVE CUSTOMER-ID        TO PDACUSTO
               MOVE CUSTOMER-NAME      TO NAMEO.


       P04000-POPULATE-CUST-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05000-ORDER-QUERY                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE CUSTOMER ORDER INQUIRY  *
      *                PROCESS. PROGRAM REQUESTS CUSTOMER ORDER       *
      *                INFORMATION ON ONE MQSERIES QUEUE AND THEN     *
      *                PROCESSES THE RESPONSE FROM PROGRAM PDA017     *
      *                ON ANOTHER QUEUE                               *
      *                                                               *
      *    CALLED BY:  P03000-PROCESS-TRANS                           *
      *                                                               *
      *****************************************************************

       P05000-ORDER-QUERY.

      *****************************************************************
      *    ATTEMPT TO START AN INSTANCE OF THE CKTI TRIGGER MONITOR   *
      *    ON THE CUSTOMER INITIATION QUEUE                           *
      *****************************************************************

      *****PERFORM  P05030-STARTCKTI-INSTANCE
      *****    THRU P05030-STARTCKTI-INSTANCE-EXIT.


      *****************************************************************
      *    REQUEST CUSTOMER ORDER INFORMATION VIA MQSERIES MESSAGE    *
      *    (TRIGGER CICS MODULE PDA017 TO PERFORM THE QUERY)          *
      *****************************************************************

           PERFORM  P05100-ORDER-INQ-REQUEST
               THRU P05100-ORDER-INQ-REQUEST-EXIT.


      *****************************************************************
      *    PROCESS RESULTS FROM THE ORDER INQUIRY REQUEST             *
      *    (CICS MODULE PDA017 PLACES QUERY RESULTS ON ANOTHER QUEUE) *
      *****************************************************************

           PERFORM  P05500-ORDER-INQ-RESPONSE
               THRU P05500-ORDER-INQ-RESPONSE-EXIT.


      *****************************************************************
      *    PROCESS RESULTS FROM THE CUSTOMER CREDIT CHECK REQUEST     *
      *    (CICS MODULE PDA017 PLACES REQUEST ON ANOTHER QUEUE)       *
      *****************************************************************

           PERFORM  P05800-CREDIT-RESULTS
               THRU P05800-CREDIT-RESULTS-EXIT.


       P05000-ORDER-QUERY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05030-STARTCKTI-INSTANCE                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO ATTEMPT TO START A CKTI TRIGGER     *
      *                MONITOR INSTANCE ON THE CUSTOMER INITIATION    *
      *                QUEUE. CKTI IS THE CICS TRIGGER MONITOR TO     *
      *                FACILITATE THE AUTOMATIC INITIATION OF         *
      *                TRANSACTION PD17 (CUSTOMER ORDER INQUIRY       *
      *                PROCESSING PROGRAM) WHEN A MQSERIES MESSAGE    *
      *                IS PLACED ON THE CUSTOMER QUEUE.               *
      *                                                               *
      *    CALLED BY:  P05000-ORDER-QUERY                             *
      *                                                               *
      *****************************************************************

       P05030-STARTCKTI-INSTANCE.


           MOVE LENGTH OF MQS-STARTCKTI-TRANS
                                       TO WS-CKTI-TRANS-LTH.


           EXEC CICS LINK
                     PROGRAM     ('CSQCSSQ')
                     INPUTMSG    (MQS-STARTCKTI-TRANS)
                     INPUTMSGLEN (WS-CKTI-TRANS-LTH)
                     NOHANDLE
                     RESP        (WS-RESPONSE-CODE)
           END-EXEC.


      *****************************************************************
      *    IF ERROR ON LINK, FORMAT ERROR INFORMATION AND TERMINATE   *
      *****************************************************************

           IF  WS-RESPONSE-CODE        =  DFHRESP(NORMAL)
               NEXT SENTENCE
           ELSE
               MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
               MOVE 'PDA016'           TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
               MOVE 'LINK TO PROGRAM CSQCSSQ'
                                       TO WPCE-COMMAND
               MOVE 'P05030'           TO WPCE-PARAGRAPH
               PERFORM P99500-PDA-ERROR
                  THRU P99500-PDA-ERROR-EXIT.


       P05030-STARTCKTI-INSTANCE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05100-ORDER-INQ-REQUEST                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO TRIGGER THE CUSTOMER ORDER INQUIRY  *
      *                PROCESSING PROGRAM (PDA017) VIA A WRITE TO     *
      *                THE MQSERIES QUEUE. THE RESULTS ARE RETRIEVED  *
      *                FROM THE MQSERIES RESPONSE QUEUE.              *
      *                                                               *
      *    CALLED BY:  P05000-ORDER-QUERY                             *
      *                                                               *
      *****************************************************************

       P05100-ORDER-INQ-REQUEST.

           MOVE MQOD-CURRENT-VERSION   TO MQOD-VERSION.

      *****************************************************************
      *    OPEN THE MQSERIES CUSTOMER QUEUE FOR OUTPUT                *
      *****************************************************************

           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
           MOVE MQOT-Q                 TO MQOD-OBJECTTYPE.
           MOVE 'QUEUE'                TO MQS-OBJECTTYPE-DESC.
           MOVE MQS-CUSTOMER-QALIAS    TO MQOD-OBJECTNAME.
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
      *    (MESSAGE TRIGGERS THE CUSTOMER PROCESSING PROGRAM (PDA017) *
      *****************************************************************

           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
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
           MOVE PC-USERID-ID           TO MQS-CUSTOMER-USERID.
           MOVE PDACUSTI               TO MQS-CUSTOMER-ID.
           MOVE WMF-PER-ORDER-FEE      TO MQS-CUSTOMER-ORDER-FEE.


           PERFORM P07200-MQS-PUT
              THRU P07200-MQS-PUT-EXIT.

           MOVE MQMD-MSGID             TO MQS-MSGID.


      *****************************************************************
      *    CLOSE THE MQSERIES CUSTOMER QUEUE                           *
      *****************************************************************

           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
           COMPUTE MQS-OPTIONS         =  MQCO-NONE.

           PERFORM P07300-MQS-CLOSE
              THRU P07300-MQS-CLOSE-EXIT.


       P05100-ORDER-INQ-REQUEST-EXIT.
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

           MOVE MQOD-CURRENT-VERSION   TO MQOD-VERSION.

      *****************************************************************
      *    OPEN THE MQSERIES CUSTOMER RESPONSE QUEUE FOR INPUT        *
      *****************************************************************

           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
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
      *    READ THE RESPONSE QUEUE MESSAGE FROM PROGRAM PDA017        *
      *    (RESULTS FROM THE ORDER INQUIRY REQUEST MESSAGE)           *
      *****************************************************************

           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
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
               MOVE PM049-NO-MSG-AVAILABLE
                                       TO WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    CLOSE THE MQSERIES CUSTOMER RESPONSE QUEUE                  *
      *****************************************************************

           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
           COMPUTE MQS-OPTIONS         =  MQCO-NONE.

           PERFORM P07300-MQS-CLOSE
              THRU P07300-MQS-CLOSE-EXIT.


      *****************************************************************
      *    PROCESS THE RESULTS OF THE CUSTOMER ORDER INQUIRY           *
      *    (EITHER SUCCESSFUL QUERY, OR ERROR ENCOUNTERED)             *
      *****************************************************************

           IF NO-ERROR-FOUND
               PERFORM P08000-PROCESS-RESULTS
                  THRU P08000-PROCESS-RESULTS-EXIT.


       P05500-ORDER-INQ-RESPONSE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05800-CREDIT-RESULTS                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS THE RESULTS OF THE CUSTOMER *
      *                CREDIT AUTHORIZATION REQUEST. CICS PROGRAM     *
      *                PDA017 PLACES THE CREDIT REQUEST ON THE        *
      *                MQSERIES QUEUE. CICS PROGRAM PDA018 PLACES THE *
      *                CREDIT RESULTS ON THE MQSERIES QUEUE.          *
      *                                                               *
      *    CALLED BY:  P05000-ORDER-QUERY                             *
      *                                                               *
      *****************************************************************

       P05800-CREDIT-RESULTS.

           MOVE MQOD-CURRENT-VERSION   TO MQOD-VERSION.

      *****************************************************************
      *    OPEN THE MQSERIES CUSTOMER CREDIT AUTH RESPONSE Q FOR INPUT*
      *****************************************************************

           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
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
      *    READ THE RESPONSE QUEUE MESSAGE FROM PROGRAM PDA018        *
      *    (RESULTS FROM THE CUSTOMER CREDIT AUTH REQUEST MESSAGE)    *
      *****************************************************************

           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
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

           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
           COMPUTE MQS-OPTIONS         =  MQCO-NONE.

           PERFORM P07300-MQS-CLOSE
              THRU P07300-MQS-CLOSE-EXIT.


      *****************************************************************
      *    PROCESS THE RESULTS OF THE CUSTOMER CREDIT AUTHORIZATION   *
      *    (EITHER SUCCESSFUL QUERY, OR ERROR ENCOUNTERED)            *
      *****************************************************************

           IF NO-ERROR-FOUND
               PERFORM P08400-PROCESS-CR-RESULTS
                  THRU P08400-PROCESS-CR-RESULTS-EXIT.


       P05800-CREDIT-RESULTS-EXIT.
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
      *****************************************************************

           IF MQS-COMPCODE             =  MQCC-OK
               NEXT SENTENCE
           ELSE
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA016'           TO WPME-PROGRAM-ID
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
      *    CALLED BY:  P05100-ORDER-INQ-REQUEST                       *
      *                P05500-ORDER-INQ-RESPONSE                      *
      *                                                               *
      *****************************************************************

       P07200-MQS-PUT.

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
               MOVE 'PDA016'           TO WPME-PROGRAM-ID
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
      *    CALLED BY:  P05100-ORDER-INQ-REQUEST                       *
      *                P05500-ORDER-INQ-RESPONSE                      *
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
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA016'           TO WPME-PROGRAM-ID
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
      *    CALLED BY:  P05500-ORDER-INQ-RESPONSE                      *
      *                                                               *
      *****************************************************************

       P07400-MQS-GET.

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
               MOVE 'PDA016'           TO WPME-PROGRAM-ID
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
      *    PARAGRAPH:  P08000-PROCESS-RESULTS                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INTERROGATE THE RESULTS OF THE      *
      *                MQSERIES ORDER QUERY.                          *
      *                                                               *
      *                IF SUCCESSFUL, FORMAT SCREEN WITH CUSTOMER     *
      *                ORDER DATA                                     *
      *                                                               *
      *                IF ERROR, FORMAT SCREEN WITH ERROR TEXT AND    *
      *                TERMINATE THE TASK                             *
      *                                                               *
      *    CALLED BY:  P05500-ORDER-INQ-RESPONSE                      *
      *                                                               *
      *****************************************************************

       P08000-PROCESS-RESULTS.

      *****************************************************************
      *    IF FATAL ERROR ENCOUNTERED, FORMAT ERROR INFO INTO COMMON  *
      *    PDA ERROR ROUTINE, TERMINATE TASK                          *
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
                                       TO WS-PDA-CICS-ERROR-01
                   MOVE MQS-PDA-ERROR-LINE-02
                                       TO WS-PDA-CICS-ERROR-02
                   PERFORM P99500-PDA-ERROR
                      THRU P99500-PDA-ERROR-EXIT
           ELSE
                   NEXT SENTENCE.


      *****************************************************************
      *    OTHERWISE FORMAT SCREEN WITH CUSTOMER ORDER INFORMATION    *
      *****************************************************************

           PERFORM  P80600-CLEAR-SCREEN
               THRU P80600-CLEAR-SCREEN-EXIT.

           MOVE MQS-TOTAL-ORDERS       TO TOTORDO.
           MOVE MQS-TOTAL-DOLLAR-AMOUNT
                                       TO TOTAMTO.
           MOVE MQS-AVG-DOLLAR-AMOUNT  TO AVGAMTO.

           IF MQS-LAST-ORDER-NUMBER    >  SPACES
               MOVE MQS-LAST-ORDER-DATE
                                       TO WMF-DATE-1
               MOVE WMF-DATE-1-MM      TO WMF-DATE-2-MM
               MOVE WMF-DATE-1-DD      TO WMF-DATE-2-DD
               MOVE WMF-DATE-1-YY      TO WMF-DATE-2-YY
               MOVE WMF-DATE-2         TO LORDDTEO
               MOVE MQS-LAST-ORDER-AMOUNT
                                       TO LORDAMTO
               MOVE MQS-LAST-ORDER-NUMBER
                                       TO LORDNBRO.


           PERFORM  P08100-FMT-ORDER-DETAIL
               THRU P08100-FMT-ORDER-DETAIL-EXIT
                   VARYING WS-SUB1 FROM +1 BY +1
                       UNTIL WS-SUB1 >  WS-SCREEN-MAX.


           IF MQS-SCREEN-MESSAGE       >  SPACES
               MOVE MQS-SCREEN-MESSAGE TO PDAMSGO
           ELSE
               MOVE PM038-INQUIRY-COMPLETE
                                       TO PDAMSGO.


       P08000-PROCESS-RESULTS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P08100-FMT-ORDER-DETAIL                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO FORMAT THE ORDER DETAIL FROM THE    *
      *                MQSERIES MESSAGE AREA TO THE SCREEN.           *
      *                                                               *
      *    CALLED BY:  P08000-PROCESS-RESULTS                         *
      *                                                               *
      *****************************************************************

       P08100-FMT-ORDER-DETAIL.

           IF MQS-ORDER-NUMBER (WS-SUB1)  >  SPACES
               MOVE MQS-ORDER-NUMBER (WS-SUB1)
                                       TO M-ORDER-NUMBER   (WS-SUB1)
               MOVE MQS-ORDER-AMOUNT (WS-SUB1)
                                       TO M-ORDER-AMOUNT-R (WS-SUB1).


       P08100-FMT-ORDER-DETAIL-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P08400-PROCESS-CR-RESULTS                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INTERROGATE THE RESULTS OF THE      *
      *                MQSERIES CREDIT AUTHORIZATION QUERY.           *
      *                                                               *
      *                IF SUCCESSFUL, FORMAT SCREEN WITH CUSTOMER     *
      *                CREDIT STATUS                                  *
      *                                                               *
      *                IF ERROR, FORMAT SCREEN WITH ERROR TEXT AND    *
      *                TERMINATE THE TASK                             *
      *                                                               *
      *    CALLED BY:  P05800-CREDIT-RESULTS                          *
      *                                                               *
      *****************************************************************

       P08400-PROCESS-CR-RESULTS.

      *****************************************************************
      *    IF FATAL ERROR ENCOUNTERED, FORMAT ERROR INFO INTO COMMON  *
      *    PDA ERROR ROUTINE, TERMINATE TASK                          *
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
                                       TO WS-PDA-CICS-ERROR-01
                   MOVE MQS-CREDIT-ERROR-LINE-02
                                       TO WS-PDA-CICS-ERROR-02
                   PERFORM P99500-PDA-ERROR
                      THRU P99500-PDA-ERROR-EXIT
           ELSE
                   NEXT SENTENCE.


      *****************************************************************
      *    OTHERWISE FORMAT CREDIT STATUS TO THE SCREEN               *
      *****************************************************************

           IF MQS-CREDIT-APPROVED
               MOVE 'APPROVED'         TO CRSTATO
           ELSE
           IF MQS-CREDIT-REJECTED
               MOVE 'REJECTED'         TO CRSTATO
           ELSE
               MOVE 'UNAVAILABLE'      TO CRSTATO.


       P08400-PROCESS-CR-RESULTS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P09000-READ-CUSTOMER                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO ACCESS THE CUSTOMER VSAM FILE       *
      *                                                               *
      *    CALLED BY:  P04000-POPULATE-CUST                           *
      *                                                               *
      *****************************************************************

       P09000-READ-CUSTOMER.

           EXEC CICS READ
                     DATASET('PDACUST')
                     INTO   (CUSTOMER-RECORD)
                     RIDFLD (CUSTOMER-KEY)
                     GTEQ
                     RESP   (WS-RESPONSE-CODE)
           END-EXEC.


           IF (WS-RESPONSE-CODE        =  DFHRESP(NORMAL))  AND
              (CUSTOMER-PREFIX         =  PC-USERID-NUMBER)
               NEXT SENTENCE
           ELSE
           IF (WS-RESPONSE-CODE        =  DFHRESP(NOTFND))  OR          HWB418
              (WS-RESPONSE-CODE        =  DFHRESP(NORMAL)   AND         HWB418
               CUSTOMER-PREFIX     NOT =  PC-USERID-NUMBER)             HWB418
               MOVE DFHUNINT           TO PDACUSTA
               MOVE PM008-CUST-NOT-FOUND
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
           ELSE
               MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
               MOVE 'PDA016'           TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
               MOVE 'READ CUSTOMER MAST'
                                       TO WPCE-COMMAND
               MOVE 'P09000'           TO WPCE-PARAGRAPH
               PERFORM P99500-PDA-ERROR
                  THRU P99500-PDA-ERROR-EXIT.


       P09000-READ-CUSTOMER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P70000-ERROR-ROUTINE                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO HANDLE THE SCREEN ERROR MESSAGE     *
      *                PROCESSING                                     *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************

       P70000-ERROR-ROUTINE.

           MOVE 'Y'                    TO WS-ERROR-FOUND-SW.

           IF PDAMSGO                  >  SPACES
               NEXT SENTENCE
           ELSE
               MOVE WMF-MESSAGE-AREA   TO PDAMSGO.

       P70000-ERROR-ROUTINE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80000-SEND-FULL-MAP                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DISPLAY THE INITIAL SCREEN          *
      *                                                               *
      *    CALLED BY:  P01000-1ST-TIME-PROCESS                        *
      *                                                               *
      *****************************************************************

       P80000-SEND-FULL-MAP.

           EXEC CICS SEND
                     MAP           ('PDA016')
                     MAPSET        ('PDA016M')
                     FROM          (PDA016O)
                     ERASE
                     FREEKB
                     CURSOR
                     NOHANDLE
                     RESP          (WS-RESPONSE-CODE)
           END-EXEC.


      *****************************************************************
      *    IF ERROR, FORMAT ERROR INFORMATION AND TERMINATE           *
      *****************************************************************

           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               NEXT SENTENCE
           ELSE
               MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
               MOVE 'PDA016'           TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
               MOVE 'CICS SEND MAP'    TO WPCE-COMMAND
               MOVE 'P80000'           TO WPCE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P80000-SEND-FULL-MAP-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80100-SEND-MAP-DATAONLY                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DISPLAY THE MAIN MENU SENDING DATA  *
      *                ONLY (NO LITERALS)                             *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************

       P80100-SEND-MAP-DATAONLY.

      *****************************************************************
      *    IF ERROR FOUND, CLEAR THE DETAIL PORTION OF THE SCREEN     *
      *****************************************************************

           IF ERROR-FOUND
               PERFORM   P80600-CLEAR-SCREEN
                   THRU  P80600-CLEAR-SCREEN-EXIT.


      *****************************************************************
      *    SEND THE MAP DATA ONLY, DO NOT ERASE SCREEN                *
      *****************************************************************

           EXEC CICS SEND
                     MAP           ('PDA016')
                     MAPSET        ('PDA016M')
                     FROM          (PDA016O)
                     DATAONLY
                     FREEKB
                     CURSOR
                     NOHANDLE
                     RESP          (WS-RESPONSE-CODE)
           END-EXEC.


      *****************************************************************
      *    IF ERROR, FORMAT ERROR INFORMATION AND TERMINATE           *
      *****************************************************************

           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               NEXT SENTENCE
           ELSE
               MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
               MOVE 'PDA016'           TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
               MOVE 'CICS SEND MAP'    TO WPCE-COMMAND
               MOVE 'P80100'           TO WPCE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P80100-SEND-MAP-DATAONLY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80200-RECEIVE-MAP                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RECEIVE / FORMAT THE INPUT MAP DATA *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************

       P80200-RECEIVE-MAP.

           EXEC CICS RECEIVE
                     MAP           ('PDA016')
                     MAPSET        ('PDA016M')
                     INTO          (PDA016I)
                     NOHANDLE
                     RESP          (WS-RESPONSE-CODE)
           END-EXEC.


      *****************************************************************
      *    IF ERROR, FORMAT ERROR INFORMATION AND TERMINATE           *
      *****************************************************************

           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)        OR
              WS-RESPONSE-CODE = DFHRESP(MAPFAIL)
               NEXT SENTENCE
           ELSE
               MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
               MOVE 'PDA016'           TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
               MOVE 'CICS RECEIVE MAP' TO WPCE-COMMAND
               MOVE 'P80200'           TO WPCE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P80200-RECEIVE-MAP-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80300-XFER-CONTROL                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO TRANSFER CONTROL TO THE             *
      *                APPROPRIATE CICS FUNCTION BASED ON THE MENU    *
      *                SELECTION ENTERED                              *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************

       P80300-XFER-CONTROL.

           EXEC CICS XCTL
                     PROGRAM       (PC-NEXT-PGRMID)
                     COMMAREA      (PDA-COMMAREA)
                     LENGTH        (PC-COMMAREA-LTH)
                     NOHANDLE
                     RESP          (WS-RESPONSE-CODE)
           END-EXEC.


      *****************************************************************
      *    IF ERROR, FORMAT ERROR INFORMATION AND TERMINATE           *
      *****************************************************************

           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               NEXT SENTENCE
           ELSE
               MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
               MOVE 'PDA016'           TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
               MOVE 'CICS XCTL --- '   TO WPCE-COMMAND-1
               MOVE PC-NEXT-PGRMID     TO WPCE-COMMAND-2
               MOVE 'P80300'           TO WPCE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P80300-XFER-CONTROL-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80400-SEND-MESSAGE                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO SEND A ONE LINE MESSAGE TO THE      *
      *                TERMINAL                                       *
      *                                                               *
      *    CALLED BY:  P03200-EDIT-PFKEY                              *
      *                                                               *
      *****************************************************************

       P80400-SEND-MESSAGE.

      *****************************************************************
      *    SEND THE MESSAGE LINE, IF ERROR FORMAT ERROR AND TERMINATE *
      *****************************************************************

           EXEC CICS SEND
                     FROM          (WMF-MESSAGE-AREA)
                     LENGTH        (WS-MESSAGE-LTH)
                     ERASE
                     NOHANDLE
                     RESP          (WS-RESPONSE-CODE)
           END-EXEC.



           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               NEXT SENTENCE
           ELSE
               MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
               MOVE 'PDA016'           TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
               MOVE 'CICS SEND'        TO WPCE-COMMAND
               MOVE 'P80400'           TO WPCE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    CURSOR AT FIRST POSITION ON SCREEN, IF ERROR TERMINATE     *
      *****************************************************************

           EXEC CICS SEND
                     CONTROL
                     CURSOR        (0)
                     NOHANDLE
                     RESP          (WS-RESPONSE-CODE)
           END-EXEC.



           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               NEXT SENTENCE
           ELSE
               MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
               MOVE 'PDA016'           TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
               MOVE 'CICS SEND CONTROL'
                                       TO WPCE-COMMAND
               MOVE 'P80400'           TO WPCE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    RETURN TO CICS (NO TRANSID OPTION)                         *
      *****************************************************************

           EXEC CICS RETURN
           END-EXEC.

           GOBACK.

       P80400-SEND-MESSAGE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80600-CLEAR-SCREEN                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RE-INITIALIZE THE SCREEN DETAIL     *
      *                FIELDS TO DEFAULT VALUES                       *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************

       P80600-CLEAR-SCREEN.

           MOVE SPACES                 TO  CRSTATI
                                           TOTORDI
                                           TOTAMTI
                                           AVGAMTI
                                           LORDDTEI
                                           LORDAMTI
                                           LORDNBRI.


           PERFORM  P80620-CLEAR-DETAIL
               THRU P80620-CLEAR-DETAIL-EXIT
                   VARYING WS-SUB1 FROM +1 BY +1
                       UNTIL WS-SUB1 > WS-SCREEN-MAX.


       P80600-CLEAR-SCREEN-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80620-CLEAR-DETAIL                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RE-INITIALIZE THE SCREEN DETAIL     *
      *                FIELDS TO DEFAULT VALUES                       *
      *                ORDER DETAIL LINE OCCURANCES                   *
      *                                                               *
      *    CALLED BY:  P80600-CLEAR-SCREEN                            *
      *                                                               *
      *****************************************************************

       P80620-CLEAR-DETAIL.

           MOVE SPACES                 TO  M-ORDER-NUMBER (WS-SUB1)
                                           M-ORDER-AMOUNT (WS-SUB1).

       P80620-CLEAR-DETAIL-EXIT.
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


           MOVE 'CICS'                 TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA016'               TO WPCE-PROGRAM-ID.
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
      *                AN ERROR SCREEN CONTAINING TEXT IS SENT        * 00633500
      *                TO THE USER INDICATING THE NATURE OF THE ERROR * 00633600
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
      *      FORMAT AND SEND ERROR TEXT                               * 00636100
      ***************************************************************** 00636200
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
               MOVE WS-PDA-CICS-ERROR-01                                00638200
                                       TO WPEA-ERROR-07-TEXT            00638300
               MOVE WS-PDA-CICS-ERROR-02                                00638400
                                       TO WPEA-ERROR-08-TEXT.           00638500
                                                                        00638600
                                                                        00638700
                                                                        00638800
           EXEC CICS DUMP                                               00638901
                     TRANSACTION                                        00639001
                     DUMPCODE('PDER')                                   00639101
           END-EXEC.                                                    00639200
                                                                        00639301
                                                                        00639401
           EXEC CICS SEND                                               00639501
                     FROM    (WS-PDA-ERROR-AREA)                        00639601
                     LENGTH  (WS-PDA-ERROR-LENGTH)                      00639701
                     ERASE                                              00639801
           END-EXEC.                                                    00639901
                                                                        00640001
                                                                        00640101
                                                                        00640201
           EXEC CICS SEND                                               00640301
                     CONTROL                                            00640401
                     CURSOR  (0)                                        00640501
           END-EXEC.                                                    00640601
                                                                        00640701
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
      *    PARAGRAPH:  P99999-ERROR                                   *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CATCH ANY ERROR(S) NOT              *
      *                SPECIFICALLY PROCESSED BY A CICS HANDLE        *
      *                CONDITION                                      *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************

RTN    P99999-ERROR.                                                     RTN
NOT                                                                      NOT
USED       MOVE 'CICS'                 TO WS-PDA-ERROR-TYPE.             USED
AS OF      MOVE 'PDA016'               TO WPCE-PROGRAM-ID.               AS OF
JAN        MOVE EIBRESP                TO WPCE-RESPONSE-CODE.            JAN
2001       MOVE 'ERROR'                TO WPCE-COMMAND.                  2001
           MOVE 'P99999'               TO WPCE-PARAGRAPH.
LLR                                                                      LLR
           PERFORM  P99500-PDA-ERROR
               THRU P99500-PDA-ERROR-EXIT.

       P99999-ERROR-EXIT.
           EXIT.
           EJECT