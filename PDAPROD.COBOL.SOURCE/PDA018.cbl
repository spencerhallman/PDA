       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA018.

      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA018                                            *
      * TRANS   :   PD18                                              *
      * MAPSET  :   NONE                                              *
      *                                                               *
      * FUNCTION:   PROGRAM PDA018 IS THE CUSTOMER CREDIT             *
      *             AUTHORIZATION PROCESSING PROGRAM.                 *
      *             THE PROGRAM IS INVOKED VIA THE MQSERIES / CICS    *
      *             TRIGGER MECHANISM (CKTI). CICS PROGRAM            *
      *             PDA017 (CUSTOMER ORDER PROCESSING PROGRAM)        *
      *             CREATES A MQSERIES MESSAGE TO REQUEST A CREDIT    *
      *             AUTHORIZATION CHECK FOR A SPECIFIC CUSTOMER ID.   *
      *             THE MQSERIES MESSAGE CAUSES A MQSERIES            *
      *             TRIGGER MESSAGE TO BE SPAWNED (VIA THE CKTI       *
      *             TRIGGER MONITOR) WHICH THEN STARTS THIS CICS      *
      *             FUNCTION (TRANSACTION PD18).                      *
      *                                                               *
      *             THE MQSERIES MESSAGE FROM THE APPLICATION QUEUE   *
      *             IS RETRIEVED AND A CREDIT AUTHORIZATION REQUEST   *
      *             IS MADE TO THE CREDIT BUREAU CREDIT PROGRAM WHICH *
      *             OBTAINS CREDIT AUTHORIZATION INFORMATION FROM THE *
      *             3 MAJOR CREDIT BUREAUS (EQUIFAX, TRW, EXPERIAN).  *
      *             (BATCH PROGRAM PDAB05)                            *
      *                                                               *
      *             THIS PROGRAM THEN WAITS FOR CREDIT AUTHORIZATION  *
      *             MQSERIES RESPONSE MESSAGES (FROM BATCH PROGRAM    *
      *             PDAB05) FOR EACH OF THE CREDIT BUREAUS. ONCE      *
      *             RECEIVED A FINAL RESPONSE MQSERIES MESSAGE IS     *
      *             PLACED ON THE CREDIT AUTHORIZATION RESPONSE QUEUE *
      *             FOR PROCESSING BY THE ORIGINAL CALLING USER       *
      *             INTERFACE (CICS PROGRAM, GUI INTERFACE, ETC.)     *
      *                                                               *
      *                                                               *
      * FILES   :   NONE
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
      *****************************************************************

       ENVIRONMENT DIVISION.
       DATA DIVISION.
           EJECT
       WORKING-STORAGE SECTION.

      *****************************************************************
      *    77 LEVEL DATA ITEMS HERE  (SUBSCRIPTS, INDEXES ETC.)       *
      *****************************************************************
       77  WS-SUB1                     PIC S9(04)   COMP  VALUE +0.
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
           05  WMF-STARTCODE           PIC X(02)   VALUE SPACES.
           05  WMF-MESSAGE-AREA        PIC X(79)   VALUE SPACES.

           05  WMF-DUMP-CODE           PIC X(04)    VALUE SPACES.
           05  WMF-DUMP-CODE-R         REDEFINES WMF-DUMP-CODE
                                       PIC 9(04).

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
      *    CICS DEFINITIONS                                           *
      *****************************************************************
      *    NONE REQUIRED                                              *
      *****************************************************************

      *****************************************************************
      *    IMS / DLI DEFINITIONS                                      *
      *****************************************************************
      *    NONE REQUIRED                                              *
      *****************************************************************

      *****************************************************************
      *    DB2  DEFINITIONS                                           *
      *****************************************************************
      *    NONE REQUIRED                                              *
      *****************************************************************

      *****************************************************************
      *    MQSERIES MISCELLANEOUS APPLICATION FIELDS / VARIABLES      *
      *****************************************************************

       01  MQS-MISCELLANEOUS.
           05  MQS-HCONN               PIC S9(9)  BINARY  VALUE +0.
           05  MQS-HOBJECT             PIC S9(9)  BINARY  VALUE +0.
           05  MQS-HOBJECT-CREDIT-AUTH-REQ-Q
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
           05  MQS-OBJECTNAME          PIC X(48)          VALUE SPACES.
           05  MQS-MSGID               PIC X(24)          VALUE SPACES.
           05  MQS-OBJECTTYPE-DESC     PIC X(15)          VALUE SPACES.


           05  MQS-CREDIT-AUTH-REQ-QUEUE
                                       PIC X(48)          VALUE
               'PDAPROD.H01AC013.CREDIT.AUTH.REQ.QUEUE'.

           05  MQS-CREDIT-AUTH-RESP-QUEUE
                                       PIC X(48)          VALUE
               'PDAPROD.H01AC013.CREDIT.AUTH.RESP.QUEUE'.

           05  MQS-BUREAU-REQ-QUEUE    PIC X(48)          VALUE
               'PDAPROD.QREMOTE.CW01.TO.CW09.CREDIT.AUTH'.

           05  MQS-BUREAU-RESP-QUEUE   PIC X(48)          VALUE
               'PDAPROD.QLOCAL.CW09.TO.CW01.CREDIT.AUTH'.


      *****************************************************************
      *    MQSERIES GENERAL INPUT / OUTPUT BUFFER USED FOR MESSAGES   *
      *****************************************************************

       01  MQS-BUFFER                  PIC X(1000)        VALUE SPACES.

       01  MQS-SAVE-BUFFER             PIC X(1000)        VALUE SPACES.


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

           MOVE ZEROES                 TO WS-ERROR-FOUND-SW
                                          WS-SKIP-ERROR-CHECK-SW
                                          WS-ERROR-IS-FORMATTED-SW.

           MOVE SPACES                 TO WS-PDA-ERROR-TYPE.
           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.
           MOVE FUNCTION CURRENT-DATE  TO WS-CURRENT-DATE-TIME.         00020001

           MOVE 'UUU'                  TO WMF-CREDIT-RATINGS.

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
                   MOVE 'PDA018'       TO WPCE-PROGRAM-ID
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
                   MOVE 'PDA018'       TO WPCE-PROGRAM-ID
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

           MOVE ZEROES               TO MQS-HOBJECT
                                        MQS-HOBJECT-CREDIT-AUTH-REQ-Q
                                        MQS-HOBJECT-CREDIT-AUTH-RESP-Q
                                        MQS-HOBJECT-BUREAU-REQ-Q
                                        MQS-HOBJECT-BUREAU-RESP-Q
                                        MQS-COMPCODE
                                        MQS-REASONCODE.

           MOVE MQOD-CURRENT-VERSION   TO MQOD-VERSION.
           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
           MOVE MQOT-Q                 TO MQOD-OBJECTTYPE.
           MOVE 'QUEUE'                TO MQS-OBJECTTYPE-DESC.


      *****************************************************************
      *    OPEN THE CREDIT AUTHORIZATION RESPONSE QUEUE (OUTPUT)      *
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
      *    OPEN THE CREDIT AUTHORIZATION REQUEST QUEUE (INPUT)        *
      *****************************************************************

           MOVE MQS-CREDIT-AUTH-REQ-QUEUE
                                       TO MQOD-OBJECTNAME.
           COMPUTE MQS-OPTIONS         =  MQOO-INPUT-SHARED      +
                                          MQOO-SAVE-ALL-CONTEXT  +
                                          MQOO-FAIL-IF-QUIESCING.

           PERFORM P07100-MQS-OPEN
              THRU P07100-MQS-OPEN-EXIT.

           MOVE MQS-HOBJECT          TO MQS-HOBJECT-CREDIT-AUTH-REQ-Q.


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
               MOVE 'PDA018'           TO WPCE-PROGRAM-ID
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
      *    FUNCTION :  ROUTINE TO CONTROL THE CREDIT AUTHORIZATION    *
      *                HIGH LEVEL PROCESSES                           *
      *                                                               *
      *    CALLED BY:  P00000-MAINLINE                                *
      *                                                               *
      *****************************************************************

       P00700-MAIN-PROCESS.

      *****************************************************************
      *    PROCESS THE CUSTOMER CREDIT AUTHORIZATION REQUEST          *
      *****************************************************************

           PERFORM  P00800-PROCESS-REQUEST
               THRU P00800-PROCESS-REQUEST-EXIT.


      *****************************************************************
      *    WAIT FOR / SEND CREDIT AUTHORIZATION RESPONSES             *
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
      *    FUNCTION :  ROUTINE TO CONTROL THE CUSTOMER CREDIT         *
      *                AUTHORIZATION PROCESS                          *
      *                                                               *
      *    CALLED BY:  P00700-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P00800-PROCESS-REQUEST.

      *****************************************************************
      *    RETRIEVE THE INCOMING CREDIT AUTHORIZATION REQUEST FROM    *
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
      *    INITIATE THE BATCH CREDIT AUTHORIZATION / CREDIT BUREAU    *
      *    PROCESSES (PDAB05 BATCH PROGRAM VIA MQSERIES TRIGGER)      *
      *****************************************************************

           PERFORM  P03000-CREDIT-AUTH
               THRU P03000-CREDIT-AUTH-EXIT.


       P00800-PROCESS-REQUEST-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01000-GET-REQUEST                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE  AND VALIDATE THE CUSTOMER *
      *                CREDIT AUTH REQUEST FROM THE MQSERIES CUSTOMER *
      *                CREDIT AUTH REQUEST QUEUE                      *
      *                                                               *
      *    CALLED BY:  P00800-PROCESS-REQUEST                         *
      *                                                               *
      *****************************************************************

       P01000-GET-REQUEST.

      *****************************************************************
      *    READ THE PRIMARY CREDIT AUTH REQUEST FROM THE  INPUT QUEUE *
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
           MOVE LENGTH OF MQS-CREDIT-AUTH-REQ-MESSAGE
                                       TO MQS-BUFFERLENGTH.
           MOVE MQS-HOBJECT-CREDIT-AUTH-REQ-Q
                                       TO MQS-HOBJECT.

           PERFORM P07400-MQS-GET
              THRU P07400-MQS-GET-EXIT.

           MOVE MQS-BUFFER             TO MQS-CREDIT-AUTH-REQ-MESSAGE
                                          MQS-SAVE-BUFFER.
           MOVE MQMD-MSGID             TO MQS-MSGID.


       P01000-GET-REQUEST-EXIT.
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
                                          MQS-HOBJECT-BUREAU-REQ-Q
                                          MQS-HOBJECT-BUREAU-RESP-Q
                                          MQS-COMPCODE
                                          MQS-REASONCODE.

           MOVE MQOD-CURRENT-VERSION   TO MQOD-VERSION.
           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
           MOVE MQOT-Q                 TO MQOD-OBJECTTYPE.
           MOVE 'QUEUE'                TO MQS-OBJECTTYPE-DESC.


      *****************************************************************
      *    OPEN THE CREDIT BUREAU REQUEST QUEUE (OUTPUT)              *
      *    (QREMOTE DEFINITION FOR A REMOTE QUEUE ON CW09)            *
      *****************************************************************

           MOVE MQS-BUREAU-REQ-QUEUE   TO MQOD-OBJECTNAME.
           COMPUTE MQS-OPTIONS         =  MQOO-OUTPUT            +
                                          MQOO-PASS-ALL-CONTEXT  +
                                          MQOO-FAIL-IF-QUIESCING.

           PERFORM P07100-MQS-OPEN
              THRU P07100-MQS-OPEN-EXIT.

           MOVE MQS-HOBJECT            TO MQS-HOBJECT-BUREAU-REQ-Q.


      *****************************************************************
      *    OPEN THE CREDIT BUREAU RESPONSE QUEUE       (INPUT)        *
      *****************************************************************

           MOVE MQS-BUREAU-RESP-QUEUE  TO MQOD-OBJECTNAME.
           COMPUTE MQS-OPTIONS         =  MQOO-INPUT-SHARED      +
                                          MQOO-SAVE-ALL-CONTEXT  +
                                          MQOO-FAIL-IF-QUIESCING.

           PERFORM P07100-MQS-OPEN
              THRU P07100-MQS-OPEN-EXIT.

           MOVE MQS-HOBJECT          TO MQS-HOBJECT-BUREAU-RESP-Q.


       P01500-OPEN-OTHER-QUEUES-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03000-CREDIT-AUTH                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INITIATE THE CREDIT BUREAU          *
      *                AUTHORIZATION CHECK VIA PLACING A MQSERIES     *
      *                MESSAGE ON THE CREDIT BUREAU REQUEST QUEUE.    *
      *                THE QUEUE IS A REMOTE ON CW09 AND THE MESSAGE  *
      *                TRIGGERS BATCH PROGRAM PDAB05 TO PROCCES THE   *
      *                CREDIT REQUEST. THE PROGRAM THEN WAITS FOR     *
      *                RESPONSES ON THE CREDIT BUREAU RESPONSE QUEUE. *
      *                                                               *
      *    CALLED BY:  P00800-PROCESS-REQUEST                         *
      *                                                               *
      *****************************************************************

       P03000-CREDIT-AUTH.

      *****************************************************************
      *    FORMAT AND PUT MQSERIES MESSAGE ON THE CREDIT BUREAU       *
      *    REQUEST Q (REMOTE Q ON CW09, TRIGGERS BATCH PROGRAM PDAB05)*
      *****************************************************************

           PERFORM  P03100-CREDIT-BUREAU-REQ
               THRU P03100-CREDIT-BUREAU-REQ-EXIT.

           IF ERROR-FOUND
               GO TO P03000-CREDIT-AUTH-EXIT.


      *****************************************************************
      *    PROCESS THE CREDIT BUREAU RESPONSE MESSAGES                *
      *****************************************************************

           PERFORM  P03500-CREDIT-BUREAU-RESP
               THRU P03500-CREDIT-BUREAU-RESP-EXIT.


       P03000-CREDIT-AUTH-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03100-CREDIT-BUREAU-REQ                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INITIATE THE CREDIT BUREAU          *
      *                AUTHORIZATION CHECK VIA PLACING A MQSERIES     *
      *                MESSAGE ON THE CREDIT BUREAU REQUEST QUEUE.    *
      *                THE QUEUE IS A REMOTE ON CW09 AND THE MESSAGE  *
      *                TRIGGERS BATCH PROGRAM PDAB05 TO PROCESS THE   *
      *                CREDIT REQUEST.                                *
      *                                                               *
      *    CALLED BY:  P03000-CREDIT-AUTH                             *
      *                                                               *
      *****************************************************************

       P03100-CREDIT-BUREAU-REQ.

      *****************************************************************
      *    WRITE THE MQSERIES REQUEST MSG TO CREDIT BUREAU REQUEST QUE*
      *    (MESSAGE TRIGGERS BATCH PROGRAM PDAB05 ON CW09             *
      *****************************************************************

           MOVE MQHC-DEF-HCONN         TO MQS-HCONN.
           MOVE MQMD-CURRENT-VERSION   TO MQMD-VERSION.
           MOVE MQRO-NONE              TO MQMD-REPORT.
           MOVE MQPER-NOT-PERSISTENT   TO MQMD-PERSISTENCE.
           MOVE MQS-MSGID              TO MQMD-MSGID.
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

           MOVE LENGTH OF MQS-CREDIT-AUTH-REQ-MESSAGE
                                       TO MQS-BUFFERLENGTH.
           MOVE MQS-CREDIT-AUTH-REQ-MESSAGE
                                       TO MQS-BUFFER.
           MOVE MQS-HOBJECT-BUREAU-REQ-Q
                                       TO MQS-HOBJECT.
           MOVE 'QUEUE'                TO MQS-OBJECTTYPE-DESC.

           PERFORM P07200-MQS-PUT
              THRU P07200-MQS-PUT-EXIT.


       P03100-CREDIT-BUREAU-REQ-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03500-CREDIT-BUREAU-RESP                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS THE CREDIT BUREAU           *
      *                AUTHORIZATION CHECK RESPONSES. 3 RESPONSE      *
      *                MESSAGES ARE EXPECTED, 1 EACH FROM EACH OF THE *
      *                CREDIT BUREAUS (TRW, EQUIFAX, EXPERIAN).       *
      *                                                               *
      *    CALLED BY:  P03000-CREDIT-AUTH                             *
      *                                                               *
      *****************************************************************

       P03500-CREDIT-BUREAU-RESP.

      *****************************************************************
      *    PROCESS LOOP TO OBTAIN 3 CREDIT RESPONSE MESSAGES          *
      *****************************************************************

           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.
           MOVE ZEROES                 TO WS-SUB1.

           PERFORM  P03600-GET-BUREAU-RESP
               THRU P03600-GET-BUREAU-RESP-EXIT
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
               GO TO P03500-CREDIT-BUREAU-RESP-EXIT.

      *****************************************************************
      *    IF ANY BUREAU REJECTIONS -- FINAL RATING IS REJECTED       *
      *****************************************************************

           IF WMF-CREDIT-RATINGS-R (1) = 'R'      OR
              WMF-CREDIT-RATINGS-R (2) = 'R'      OR
              WMF-CREDIT-RATINGS-R (3) = 'R'
               MOVE 'R'                TO WMF-FINAL-CREDIT-RATING
               GO TO P03500-CREDIT-BUREAU-RESP-EXIT.

      *****************************************************************
      *    OTHERWISE CREDIT IS ACCEPTED                               *
      *****************************************************************

           MOVE 'A'                    TO WMF-FINAL-CREDIT-RATING.


       P03500-CREDIT-BUREAU-RESP-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03600-GET-BUREAU-RESP                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO OBTAIN THE CREDIT BUREAU            *
      *                AUTHORIZATION CHECK RESPONSES. 3 RESPONSE      *
      *                MESSAGES ARE EXPECTED, 1 EACH FROM EACH OF THE *
      *                CREDIT BUREAUS (TRW, EQUIFAX, EXPERIAN).       *
      *                THE GETS ARE DONE WITH A WAIT INTERVAL TO      *
      *                ALLOW AMPLE TIME FOR THE BUREAU MESSAGES TO    *
      *                ARRIVE.                                        *
      *                                                               *
      *    CALLED BY:  P03500-CREDIT-BUREAU-RESP                      *
      *                                                               *
      *****************************************************************

       P03600-GET-BUREAU-RESP.

      *****************************************************************
      *    CHECK FOR MAXIMUM MESSAGES PROCESSED                       *
      *****************************************************************
           ADD +1                      TO WS-SUB1.

           IF WS-SUB1                  > +3
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
               GO TO P03600-GET-BUREAU-RESP-EXIT.


      *****************************************************************
      *    READ A CREDIT BUREAU RESPONSE MESSAGE FROM PDAB05 (ON CW09)*
      *    (RESULTS FROM THE CREDIT AUTHORIZATION REQUEST MESSAGE)    *
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
               GO TO P03600-GET-BUREAU-RESP-EXIT
           ELSE
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA018'           TO WPME-PROGRAM-ID
               MOVE MQS-REASONCODE     TO WPME-REASON-CODE
               MOVE 'MQGET'            TO WPME-FUNCTION-1
               MOVE MQS-OBJECTTYPE-DESC
                                       TO WPME-FUNCTION-2
               MOVE 'P03600'           TO WPME-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    CHECK FOR ERROR ENCOUNTERED IN BUREAU RESPONSE PROGRAM     *
      *    (BATCH PROGRAM PDAB05 ON CW09)                             *
      *****************************************************************

           MOVE MQS-BUFFER             TO MQS-CREDIT-AUTH-REQ-MESSAGE.

           IF MQS-CREDIT-FATAL-ERROR
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 1                  TO WS-ERROR-IS-FORMATTED-SW
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    SAVE THE CREDIT RATING FOR LATER INTERROGATION             *
      *****************************************************************

           IF MQS-CREDIT-NO-ERROR
               MOVE MQS-CREDIT-AUTH  TO WMF-CREDIT-RATINGS-R (WS-SUB1).


       P03600-GET-BUREAU-RESP-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06000-SEND-RESPONSE                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO WRITE THE MQSERIES RESPONSE MESSAGE *
      *                TO THE CREDIT AUTHORIZATION RESPONSE QUEUE,    *
      *                RESPONSE WILL BE PROCESSED BY THE CALLING      *
      *                APPLICATION                                    *
      *                                                               *
      *    CALLED BY:  P00700-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P06000-SEND-RESPONSE.

      *****************************************************************
      *    WRITE THE CREDIT AUTHORIZATION RESPONSE MESSAGE            *
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
           MOVE MQS-MSGID              TO MQMD-MSGID.


           MOVE MQPMO-CURRENT-VERSION  TO MQPMO-VERSION.
           MOVE MQS-HOBJECT-CREDIT-AUTH-REQ-Q
                                       TO MQPMO-CONTEXT.

           COMPUTE MQPMO-OPTIONS       =  MQPMO-NO-SYNCPOINT      +
                                          MQPMO-PASS-ALL-CONTEXT  +
                                          MQPMO-FAIL-IF-QUIESCING.


           MOVE MQS-SAVE-BUFFER        TO MQS-CREDIT-AUTH-REQ-MESSAGE.

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


       P06000-SEND-RESPONSE-EXIT.
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
      *    CLOSE THE MQSERIES CREDIT AUTHORIZATION REQUEST QUEUE      *
      *****************************************************************

           IF MQS-HOBJECT-CREDIT-AUTH-REQ-Q  >  ZEROES
               MOVE MQHC-DEF-HCONN     TO MQS-HCONN
               COMPUTE MQS-OPTIONS     =  MQCO-NONE
               MOVE MQS-HOBJECT-CREDIT-AUTH-REQ-Q
                                       TO MQS-HOBJECT
               PERFORM P07300-MQS-CLOSE
                  THRU P07300-MQS-CLOSE-EXIT.


      *****************************************************************
      *    CLOSE THE MQSERIES CREDIT AUTHORIZATION RESPONSE QUEUE     *
      *****************************************************************

           IF MQS-HOBJECT-CREDIT-AUTH-RESP-Q  >  ZEROES
               MOVE MQHC-DEF-HCONN     TO MQS-HCONN
               COMPUTE MQS-OPTIONS     =  MQCO-NONE
               MOVE MQS-HOBJECT-CREDIT-AUTH-RESP-Q
                                       TO MQS-HOBJECT
               PERFORM P07300-MQS-CLOSE
                  THRU P07300-MQS-CLOSE-EXIT.


      *****************************************************************
      *    CLOSE THE MQSERIES CREDIT BUREAU REQUEST QUEUE             *
      *****************************************************************

           IF MQS-HOBJECT-BUREAU-REQ-Q >  ZEROES
               MOVE MQHC-DEF-HCONN     TO MQS-HCONN
               COMPUTE MQS-OPTIONS     =  MQCO-NONE
               MOVE MQS-HOBJECT-BUREAU-REQ-Q
                                       TO MQS-HOBJECT
               PERFORM P07300-MQS-CLOSE
                  THRU P07300-MQS-CLOSE-EXIT.


      *****************************************************************
      *    CLOSE THE MQSERIES CREDIT BUREAU RESPONSE QUEUE            *
      *****************************************************************

           IF MQS-HOBJECT-BUREAU-RESP-Q >  ZEROES
               MOVE MQHC-DEF-HCONN     TO MQS-HCONN
               COMPUTE MQS-OPTIONS     =  MQCO-NONE
               MOVE MQS-HOBJECT-BUREAU-RESP-Q
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
      *****************************************************************

           IF MQS-COMPCODE             =  MQCC-OK
               NEXT SENTENCE
           ELSE
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA018'           TO WPME-PROGRAM-ID
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
      *    FUNCTION :  ROUTINE TO WRITE A MESSAGE TO THE OPEN QUEUE,  *
      *                OPTIONS AND PARAMETERS ARE SET BY THE CALLING  *
      *                PARAGRAPH AND VARY ACCORDING TO THE SPECIFIC   *
      *                MESSAGE PUT OPERATION                          *
      *                                                               *
      *    CALLED BY:  P03100-CREDIT-BUREAU-REQ                       *
      *                P06000-SEND-RESPONSE                           *
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
      *****************************************************************

           IF MQS-COMPCODE             =  MQCC-OK
               NEXT SENTENCE
           ELSE
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA018'           TO WPME-PROGRAM-ID
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
               MOVE 'PDA018'           TO WPME-PROGRAM-ID
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
      *                P03600-GET-BUREAU-RESP                         *
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
      *    (ALLOW PASSTHRU OF SPECIAL CONDITION PROCESSING)           *
      *****************************************************************

           IF SKIP-ERROR-CHECK
               GO TO P07400-MQS-GET-EXIT.


           IF MQS-COMPCODE             =  MQCC-OK
               NEXT SENTENCE
           ELSE
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'MQS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA018'           TO WPME-PROGRAM-ID
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
                                          MQS-CREDIT-RETURN-CODE.

           IF MQS-CREDIT-SCREEN-MESSAGE > SPACES
               NEXT SENTENCE
           ELSE
               MOVE WMF-MESSAGE-AREA   TO MQS-CREDIT-SCREEN-MESSAGE.


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
               MOVE 'PDA018'           TO WPCE-PROGRAM-ID
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
           MOVE 'PDA018'               TO WPCE-PROGRAM-ID.
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
      *      (CHECK IF ERROR INFO IS ALREADY FORMATTED - EXTERNAL     * 00636100
      *       SOURCE, SUCH AS RETURN FROM ANOTHER PROGRAM)            * 00636100
      ***************************************************************** 00636200
                                                                        00636300
           IF ERROR-IS-FORMATTED                                        00636300
               NEXT SENTENCE                                            00636300
           ELSE                                                         00636300
               MOVE WS-ERROR-FOUND-SW  TO MQS-CREDIT-RETURN-CODE        00636300
               MOVE WS-PDA-ERROR-TYPE  TO MQS-CREDIT-ERROR-TYPE         00636300
                                                                        00636300
               IF PDA-DB2-ERROR                                         00636400
                   MOVE WS-PDA-DB2-ERROR-01                             00636500
                                       TO MQS-CREDIT-ERROR-LINE-01      00636600
                   MOVE WS-PDA-DB2-ERROR-02                             00636700
                                       TO MQS-CREDIT-ERROR-LINE-02      00636600
               ELSE                                                     00636900
               IF PDA-IMS-ERROR                                         00637000
                   MOVE WS-PDA-IMS-ERROR-01                             00637100
                                       TO MQS-CREDIT-ERROR-LINE-01      00636600
                   MOVE WS-PDA-IMS-ERROR-02                             00637300
                                       TO MQS-CREDIT-ERROR-LINE-02      00636600
               ELSE                                                     00637500
               IF PDA-MQSERIES-ERROR                                    00637602
                   MOVE WS-PDA-MQSERIES-ERROR-01                        00637702
                                       TO MQS-CREDIT-ERROR-LINE-01      00636600
                   MOVE WS-PDA-MQSERIES-ERROR-02                        00637902
                                       TO MQS-CREDIT-ERROR-LINE-02      00636600
               ELSE                                                     00638102
                   MOVE WS-PDA-CICS-ERROR-01                            00638200
                                       TO MQS-CREDIT-ERROR-LINE-01      00636600
                   MOVE WS-PDA-CICS-ERROR-02                            00638400
                                       TO MQS-CREDIT-ERROR-LINE-02.     00636600
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
           IF MQS-HOBJECT-CREDIT-AUTH-RESP-Q > ZEROES                   00640701
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

           IF MQS-MSGID                NOT = SPACES
               MOVE MQS-MSGID          TO MQMD-MSGID.

           MOVE MQCI-NONE              TO MQMD-CORRELID.
           MOVE MQPRI-PRIORITY-AS-Q-DEF
                                       TO MQMD-PRIORITY.
           MOVE MQENC-NATIVE           TO MQMD-ENCODING.
           MOVE MQCCSI-Q-MGR           TO MQMD-CODEDCHARSETID.
           MOVE 5000                   TO MQMD-EXPIRY.


           MOVE MQPMO-CURRENT-VERSION  TO MQPMO-VERSION.
           COMPUTE MQPMO-OPTIONS       =  MQPMO-NO-SYNCPOINT +
                                          MQPMO-FAIL-IF-QUIESCING.

           MOVE LENGTH OF MQS-CREDIT-AUTH-REQ-MESSAGE
                                       TO MQS-BUFFERLENGTH.
           MOVE MQS-CREDIT-AUTH-REQ-MESSAGE
                                       TO MQS-BUFFER.
           MOVE MQS-HOBJECT-CREDIT-AUTH-RESP-Q
                                       TO MQS-HOBJECT.


           CALL 'MQPUT'       USING    MQS-HCONN
                                       MQS-HOBJECT
                                       MQMD
                                       MQPMO
                                       MQS-BUFFERLENGTH
                                       MQS-BUFFER
                                       MQS-COMPCODE
                                       MQS-REASONCODE.
                                                                        00640801
      ***************************************************************** 00640901
      * CLOSE THE MQSERIES CREDIT AUTHORIZATION RESPONSE QUEUE        * 00641001
      ***************************************************************** 00641101

           MOVE MQHC-DEF-HCONN     TO MQS-HCONN.
           COMPUTE MQS-OPTIONS     =  MQCO-NONE.

           CALL 'MQCLOSE' USING    MQS-HCONN
                                   MQS-HOBJECT
                                   MQS-OPTIONS
                                   MQS-COMPCODE
                                   MQS-REASONCODE.


       P99600-PDA-ERROR-MQMSG-EXIT.
           EXIT.
           EJECT