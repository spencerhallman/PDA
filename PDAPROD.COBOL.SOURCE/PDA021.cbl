       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA021.

      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA021                                            *
      * TRANS   :   PD21                                              *
      * MAPSET  :   NONE                                              *
      *                                                               *
      * FUNCTION:   PROGRAM PDA021 IS AN ADDITIONAL VERSION OF THE    *
      *             CUSTOMER ORDER INQUIRY PROCESSING PROGRAM.        *
      *             THE ORIGINAL VERSION IS PROGRAM PDA017 WHICH IS   *
      *             THE MQSERIES / CICS TRIGGER MECHANISM BASED       *
      *             VERSION.                                          *
      *             PDA021 CONTAINS THE SAME APPLICATION FUNCTIONALITY*
      *             AS PDA017 BUT IS INITIATED VIA THE JAVA CONNECTOR *
      *             ARCHITECTURE (JCA) USING THE CICS TRANSACTION     *
      *             GATEWAY.                                          *
      *                                                               *
      *             ALL MQSERIES FUNCTIONALITY RESIDING IN PDA017 HAS *
      *             BEEN REMOVED FROM THIS VERSION.                   *
      *                                                               *
      *             PDA021 USES THE JCA EXTERNAL CALL INTERFACE (ECI) *
      *             WHICH UTILIZES THE CICS COMMAREA AS THE           *
      *             COMMUNICATION LINK / COMMON DATA AREA BETWEEN THE *
      *             JAVA CLIENT AND MAINFRAME CICS APPLICATION.       *
      *             CUSTOMER QUERY INPUT PARAMETERS AND QUERY RESULTS *
      *             ARE PASSED VIA THE COMMAREA DATA STRUCTURE.       *
      *                                                               *
      *                                                               *
      * FILES   :   ORDER DATABASE   -  IMS/DLI   (READ-ONLY)         *
      *             USERID TABLE     -  DB2       (READ-ONLY)         *
      *                                                               *
      * TRANSACTIONS GENERATED:                                       *
      *             NONE                                              *
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
       77  WS-COMMAREA-LTH             PIC S9(04)   COMP  VALUE +700.
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
           05  WMF-CUSTOMER-USERID     PIC X(08).
           05  WMF-CUSTOMER-ID         PIC X(32).
           05  WMF-PSB-NAME            PIC X(08)   VALUE 'PDA021'.
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

           EXEC SQL
               INCLUDE DUSERID
           END-EXEC.
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

      *****************************************************************
      *    DFHCOMMAREA  -- LENGTH = 700                               *
      *****************************************************************
      *****************************************************************
      *    CUSTOMER ORDER QUERY REQUEST / RESPONSE INFORMATION        *
      *****************************************************************

       01  DFHCOMMAREA.
         03 COM-CUSTOMER-REQUEST-IN.
           05  COM-CUSTOMER-USERID     PIC X(08).
           05  COM-CUSTOMER-ID         PIC X(32).

         03 COM-CUSTOMER-RESPONSE-OUT.
           05  COM-RETURN-CODE         PIC 9(01).
               88  COM-NO-ERROR                           VALUE 0.
               88  COM-ERROR                              VALUE 1.
               88  COM-FATAL-ERROR                        VALUE 9.
           05  COM-TOTAL-ORDERS        PIC 9(05).
           05  COM-TOTAL-DOLLAR-AMOUNT
                                       PIC 9(09)V99.
           05  COM-AVG-DOLLAR-AMOUNT
                                       PIC 9(09)V99.
           05  COM-LAST-ORDER-DATE     PIC X(06).
           05  COM-LAST-ORDER-AMOUNT
                                       PIC 9(07)V99.
           05  COM-LAST-ORDER-NUMBER
                                       PIC X(10).
           05  COM-ORDER-DETAIL        OCCURS 14 TIMES.
               10  COM-ORDER-NUMBER
                                       PIC X(10).
               10  COM-ORDER-AMOUNT
                                       PIC 9(07)V99.
           05  COM-SCREEN-MESSAGE      PIC X(79).
           05  COM-ERROR-INFORMATION.
               10  COM-PDA-ERROR-TYPE  PIC X(04).
               10  COM-PDA-ERROR-LINE-01
                                       PIC X(78).
               10  COM-PDA-ERROR-LINE-02
                                       PIC X(78).
           05  COM-FILLER              PIC X(102).
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
           MOVE FUNCTION CURRENT-DATE  TO WS-CURRENT-DATE-TIME.         00020001

           MOVE FUNCTION UPPER-CASE(COM-CUSTOMER-USERID)                00020001
                                       TO WMF-CUSTOMER-USERID.
           MOVE FUNCTION UPPER-CASE(COM-CUSTOMER-ID)                    00020001
                                       TO WMF-CUSTOMER-ID.

      *****************************************************************
      *    INITIALIZE THE COMMAREA CUSTOMER ORDER INFO RESULTS AREA   *
      *****************************************************************

           MOVE SPACES                 TO COM-CUSTOMER-RESPONSE-OUT.
           MOVE ZEROES                 TO COM-RETURN-CODE
                                          COM-TOTAL-ORDERS
                                          COM-TOTAL-DOLLAR-AMOUNT
                                          COM-AVG-DOLLAR-AMOUNT
                                          COM-LAST-ORDER-AMOUNT.

           PERFORM P00065-INIT-COM-ORDERS
              THRU P00065-INIT-COM-ORDERS-EXIT
                  VARYING WS-SUB1 FROM +1 BY +1
                      UNTIL WS-SUB1 > WS-ORDERS-MAX.

       P00050-INITIALIZE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00065-INIT-COM-ORDERS                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INITIALIZE THE COMMAREA             *
      *                ORDER DETAIL AREA                              *
      *                                                               *
      *    CALLED BY:  P00050-INITIALIZE                              *
      *                                                               *
      *****************************************************************

       P00065-INIT-COM-ORDERS.

           MOVE SPACES                 TO COM-ORDER-NUMBER (WS-SUB1).
           MOVE ZEROES                 TO COM-ORDER-AMOUNT (WS-SUB1).

       P00065-INIT-COM-ORDERS-EXIT.
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
               MOVE 'PDA021'           TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
               MOVE 'CICS RETURN'      TO WPCE-COMMAND
               MOVE 'P00400'           TO WPCE-PARAGRAPH
               EXEC CICS ABEND                                              0063
                         ABCODE('PDAR')                                     0063
               END-EXEC.                                                    0063


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
      *    VERIFY THE USERID IS VALID FOR THE PRODUCT DEMO APPLICATION*
      *****************************************************************

           PERFORM P01100-VERIFY-USERID
              THRU P01100-VERIFY-USERID-EXIT.


      *****************************************************************
      *    EXTRACT ORDER INFORMATION FOR THE SELECTED CUSTOMER        *
      *****************************************************************

           IF NO-ERROR-FOUND
               PERFORM  P03000-ORDER-PROCESS
                   THRU P03000-ORDER-PROCESS-EXIT.


       P00800-PROCESS-REQUEST-EXIT.
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
      *    CALLED BY:  P00800-PROCESS-REQUEST                         *
      *                                                               *
      *****************************************************************

       P01100-VERIFY-USERID.

      *****************************************************************
      *    USERID MUST EXIST IN THE DB2 USERID TABLE                  *
      *****************************************************************

           EXEC SQL SELECT    ID,
                              NUMBER,
                              ACTIVE_SCENARIOS

                    INTO      :USERID-ID,
                              :USERID-NUMBER,
                              :USERID-ACTIVE-SCENARIOS

                    FROM      USERID

                    WHERE     ID = :WMF-CUSTOMER-USERID
           END-EXEC.


      *****************************************************************
      *    ZERO RETURN CODE (SUCCESS) IS ONLY ACCEPTABLE CODE         *
      *****************************************************************

           IF SQLCODE                  =  ZEROES
               NEXT SENTENCE
           ELSE
           IF SQLCODE                  =  +100
               MOVE 'USER'             TO COM-PDA-ERROR-TYPE
               MOVE PM033-USERID-NOT-FOUND
                                       TO WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
           ELSE
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA021'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
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
      *    SCHEDULE THE PSB TO BE USED (PDA021)                       *
      *****************************************************************

           PERFORM  P03100-SCHEDULE-PSB
               THRU P03100-SCHEDULE-PSB-EXIT.


      *****************************************************************
      *    PROCESS THE ORDER DATABASE FOR THE CUSTOMER ID SUPPLIED    *
      *    IN THE COMMAREA                                             *
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
               MOVE 'PDA021'           TO WPIE-PROGRAM-ID
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
               MOVE 'PDA021'           TO WPIE-PROGRAM-ID
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

           MOVE USERID-NUMBER          TO WMF-ORDER-PREFIX.
           MOVE LOW-VALUES             TO WMF-ORDER-NUMBER.

           PERFORM  P03900-GU-ORDER
               THRU P03900-GU-ORDER-EXIT.

           IF (DIBSTAT            NOT  =  SPACES)   OR
              (ORDER-PREFIX       NOT  =  USERID-NUMBER)
               GO TO P03500-EXTRACT-ORDERS-EXIT.


      *****************************************************************
      *    FORMAT / RETRIEVE THE REMAINDER OF ORDERS FOR CUSTOMER     *
      *    UNTIL NO MORE                                               *
      *****************************************************************

           IF WMF-CUSTOMER-ID     > SPACES
               PERFORM  P03600-SELECT-ORDERS
                   THRU P03600-SELECT-ORDERS-EXIT
                       UNTIL PROCESS-COMPLETE.


      *****************************************************************
      *    AT END OF ORDERS, PERFORM TOTAL CALCULATIONS, MOVE INFO    *
      *    TO RETURN MESSAGE AREA                                     *
      *****************************************************************

           MOVE WMF-TOTAL-ORDERS       TO COM-TOTAL-ORDERS.
           MOVE WMF-TOTAL-DOLLAR-AMOUNT
                                       TO COM-TOTAL-DOLLAR-AMOUNT.
           MOVE WMF-LAST-ORDER-DATE    TO COM-LAST-ORDER-DATE.
           MOVE WMF-LAST-ORDER-AMOUNT  TO COM-LAST-ORDER-AMOUNT.
           MOVE WMF-LAST-ORDER-NUMBER  TO COM-LAST-ORDER-NUMBER.

           IF WMF-TOTAL-ORDERS         > ZEROES
               COMPUTE WMF-AVG-DOLLAR-AMOUNT ROUNDED =
                   WMF-TOTAL-DOLLAR-AMOUNT / WMF-TOTAL-ORDERS.

           MOVE WMF-AVG-DOLLAR-AMOUNT  TO COM-AVG-DOLLAR-AMOUNT.


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

           IF ORDER-PREFIX             =  USERID-NUMBER
               IF ORDER-CUSTOMER-ID    =  WMF-CUSTOMER-ID
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
               MOVE 'PDA021'           TO WPIE-PROGRAM-ID
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


           MOVE ORDER-NUMBER           TO COM-ORDER-NUMBER (WS-SUB1).
           MOVE ORDER-TOTAL-AMOUNT     TO COM-ORDER-AMOUNT (WS-SUB1).


       P03640-FORMAT-ORDER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03900-GU-ORDER                                *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE ORDER ROOT SEGMENT     *
      *                USING THE SUPPLIED CUSTOMER PREFIX             *
      *                (FROM COMMAREA)                                *
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
               MOVE 'PDA021'           TO WPIE-PROGRAM-ID
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
                                          COM-RETURN-CODE.

           IF COM-SCREEN-MESSAGE       >  SPACES
               NEXT SENTENCE
           ELSE
               MOVE WMF-MESSAGE-AREA   TO COM-SCREEN-MESSAGE.


       P70000-ERROR-ROUTINE-EXIT.
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
           MOVE 'PDA021'               TO WPCE-PROGRAM-ID.
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
      *                FORMATTED ERROR TEXT IS RETURNED VIA THE       * 00633500
      *                COMMAREA                                       * 00633600
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
           MOVE WS-ERROR-FOUND-SW      TO COM-RETURN-CODE.              00636300
           MOVE WS-PDA-ERROR-TYPE      TO COM-PDA-ERROR-TYPE.           00636300
                                                                        00636300
           IF PDA-DB2-ERROR                                             00636400
               MOVE WS-PDA-DB2-ERROR-01                                 00636500
                                       TO COM-PDA-ERROR-LINE-01         00636600
               MOVE WS-PDA-DB2-ERROR-02                                 00636700
                                       TO COM-PDA-ERROR-LINE-02         00636600
           ELSE                                                         00636900
           IF PDA-IMS-ERROR                                             00637000
               MOVE WS-PDA-IMS-ERROR-01                                 00637100
                                       TO COM-PDA-ERROR-LINE-01         00636600
               MOVE WS-PDA-IMS-ERROR-02                                 00637300
                                       TO COM-PDA-ERROR-LINE-02         00636600
           ELSE                                                         00637500
           IF PDA-MQSERIES-ERROR                                        00637602
               MOVE WS-PDA-MQSERIES-ERROR-01                            00637702
                                       TO COM-PDA-ERROR-LINE-01         00636600
               MOVE WS-PDA-MQSERIES-ERROR-02                            00637902
                                       TO COM-PDA-ERROR-LINE-02         00636600
           ELSE                                                         00638102
               MOVE WS-PDA-CICS-ERROR-01                                00638200
                                       TO COM-PDA-ERROR-LINE-01         00636600
               MOVE WS-PDA-CICS-ERROR-02                                00638400
                                       TO COM-PDA-ERROR-LINE-02.        00636600
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
                                                                        00640801
      ***************************************************************** 00640901
      * RETURN CONTROL TO CICS                                        * 00641001
      ***************************************************************** 00641101
                                                                        00641201
           EXEC CICS RETURN
           END-EXEC.
                                                                        00641601
           GOBACK.                                                      00641701
                                                                        00641801
       P99500-PDA-ERROR-EXIT.                                           00641901
           EXIT.                                                        00642001
           EJECT                                                        00650000