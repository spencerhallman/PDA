       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA013.
      *
      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA013                                            *
      * TRANS   :   PD13                                              *
      * MAPSET  :   PDA013M                                           *
      *                                                               *
      * FUNCTION:   PROGRAM PDA013 IS THE DATABASE REFRESH PROCESS.   *
      *             IT WILL REMOVE ALL ENTRIES FOR THE USERID FROM    *
      *             THE FOLLOWING FILES:                              *
      *                  1)  CUSTOMER FILE               (VSAM)       *
      *                  2)  PENDING ORDER FILE          (VSAM)       *
      *                  3)  ORDER DATABASE              (IMS-DLI)    *
      *                  4)  ITEM TABLE                  (DB2)        *
      *                  5)  SUPPLIER TABLE              (DB2)        *
      *                  6)  ITEM SUPPLIER TABLE         (DB2)        *
      *                  7)  PURCHASE TYPES TABLE        (DB2)        *
      *             IT WILL THEN RELOAD THE FOLLOWING FILES USING THE *
      *             ZERO USERID RECORDS AS A BASE:                    *
      *                  1)  CUSTOMER FILE               (VSAM)       *
      *                  2)  ITEM TABLE                  (DB2)        *
      *                  3)  SUPPLIER TABLE              (DB2)        *
      *                  4)  ITEM SUPPLIER TABLE         (DB2)        *
      *                  5)  PURCHASE TYPES TABLE        (DB2)        *
      *             IT WILL THEN RESET THE ZERO RECORD FOR THE ORDER  *
      *             NUMBER ON THE FOLLOWING FILE:                     *
      *                  1)  ORDER DATABASE              (IMS-DLI)    *
      *                                                               *
      * FILES   :   CUSTOMER FILE         -  VSAM KSDS     (UPDATE)   *
      *             PENDING ORDER FILE    -  VSAM KSDS     (UPDATE)   *
      *             ORDER DATABASE        -  IMS-DLI       (UPDATE)   *
      *             ITEM TABLE            -  DB2           (UPDATE)   *
      *             SUPPLIER TABLE        -  DB2           (UPDATE)   *
      *             ITEM SUPPLIER TABLE   -  DB2           (UPDATE)   *
      *             PURCHASE TYPES TABLE  -  DB2           (UPDATE)   *
      *                                                               *
      *                                                               *
      * TRANSACTIONS GENERATED:                                       *
      *             PD03       MAINTENANCE MENU                       *
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
      *                                                               *
      *****************************************************************

       ENVIRONMENT DIVISION.
       DATA DIVISION.
           EJECT
       WORKING-STORAGE SECTION.

      *****************************************************************
      *    77 LEVEL DATA ITEMS HERE  (SUBSCRIPTS, INDEXES ETC.)       *
      *****************************************************************
       77  WS-SUB1                     PIC S9(4) COMP VALUE +0.
       77  WS-SUB-MAX                  PIC S9(4) COMP VALUE +3.
       77  WS-SUB-MAX-PLUS-ONE         PIC S9(4) COMP VALUE +4.
       77  WS-MESSAGE-LTH              PIC S9(4) COMP VALUE +79.
       77  WS-RESPONSE-CODE            PIC S9(8) COMP VALUE +0.

      *****************************************************************
      *    SWITCHES                                                   *
      *****************************************************************

       01  WS-SWITCHES.
           05  WS-MENU-SELECTION-SW    PIC X     VALUE ' '.
           05  WS-TRANS-INTENT-SW      PIC X     VALUE 'I'.
               88  INQUIRY-TRANS                 VALUE 'I'.
               88  UPDATE-TRANS                  VALUE 'U'.
           05  WS-END-OF-PROCESS-SW    PIC X     VALUE 'N'.
               88  END-OF-PROCESS                VALUE 'Y'.
               88  NOT-END-OF-PROCESS            VALUE 'N'.
           05  WS-TOP-OF-DATA-SW       PIC X     VALUE 'N'.
               88  TOP-OF-DATA                   VALUE 'Y'.
               88  NOT-TOP-OF-DATA               VALUE 'N'.
           05  WS-BOTTOM-OF-DATA-SW    PIC X     VALUE 'N'.
               88  BOTTOM-OF-DATA                VALUE 'Y'.
               88  NOT-BOTTOM-OF-DATA            VALUE 'N'.
           05  WS-ERROR-FOUND-SW       PIC X     VALUE 'N'.
               88  ERROR-FOUND                   VALUE 'Y'.
               88  NO-ERROR-FOUND                VALUE 'N'.
           05  WS-SELECTION-SW         PIC X     VALUE 'N'.
               88  SELECTION-MADE                VALUE 'Y'.
               88  NO-SELECTION-MADE             VALUE 'N'.
           05  WS-ZERO-RECORD-SW       PIC X     VALUE 'N'.
               88  ZERO-RECORD-FOUND             VALUE 'Y'.
           EJECT
      *****************************************************************
      *    MISCELLANEOUS WORK FIELDS                                  *
      *****************************************************************

       01  WS-MISCELLANEOUS-FIELDS.
           05  WMF-USERID              PIC X(8)  VALUE SPACES.
           05  WMF-ABSTIME             PIC S9(15) VALUE +0      COMP-3.
           05  WMF-DATE-MMDDYY         PIC X(08) VALUE SPACES.
           05  WMF-TIME-HHMMSS         PIC X(08) VALUE SPACES.
           05  WMF-MESSAGE-AREA        PIC X(79) VALUE SPACES.
           05  WMF-SELECTION-COUNT     PIC S9(4) VALUE +0       COMP.
           05  WMF-SEL-SUB             PIC S9(4) VALUE +0       COMP.
           05  WMF-PSB-NAME            PIC X(8)  VALUE 'PDA013'.
           05  WMF-USER-PREFIX         PIC X(5)  VALUE SPACES.
           05  WMF-CUSTOMER-KEY.
               07  WMF-CUSTOMER-PREFIX PIC X(5)  VALUE SPACES.
               07  FILLER              PIC X(31) VALUE SPACES.
               07  WMF-CUSTOMER-SUFFIX PIC X     VALUE SPACES.
           05  WMF-PENDING-KEY.
               07  WMF-PENDING-PREFIX  PIC X(5)  VALUE SPACES.
               07  FILLER              PIC X(5)  VALUE SPACES.
           05  WMF-ORDER-KEY.
               07  WMF-ORDER-PREFIX    PIC X(5)  VALUE SPACES.
               07  FILLER              PIC X(10) VALUE SPACES.
           05  WMF-EMAIL-ADDRESS.
               07  WMF-EMAIL-PREFIX    PIC X(5)  VALUE SPACES.
               07  FILLER              PIC X(123) VALUE SPACES.

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

           EJECT
      *****************************************************************
      *    IMS / DLI DEFINITIONS                                      *
      *****************************************************************

           COPY IORDER.
           EJECT
      *****************************************************************
      *    VSAM FILE DEFINITIONS                                      *
      *****************************************************************

           COPY VCUSTOMR.
           EJECT

           COPY VPENDORD.
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
           EXEC SQL
              INCLUDE DITMSUP
           END-EXEC.
           EJECT
           EXEC SQL
              INCLUDE DITEM
           END-EXEC.
           EJECT
           EXEC SQL
              INCLUDE DSUPPLR
           END-EXEC.
           EJECT
           EXEC SQL
              INCLUDE DPURTYP
           END-EXEC.
           EJECT
           EXEC SQL
               DECLARE ITEM_SUPPLIER CURSOR FOR
                   SELECT  *
                   FROM    ITEM_SUPPLIER
                   WHERE   ITEM_PREFIX        = '00000'
                   ORDER BY ITEM_NUMBER
           END-EXEC.


           EXEC SQL
               DECLARE ITEM CURSOR FOR
                   SELECT  *
                   FROM    ITEM
                   WHERE   ITEM.PREFIX        = '00000'
                   ORDER BY ITEM.NUMBER
           END-EXEC.


           EXEC SQL
               DECLARE SUPPLIER CURSOR FOR
                   SELECT  *
                   FROM    SUPPLIER
                   WHERE   SUPPLIER.PREFIX    = '00000'
                   ORDER BY SUPPLIER_ID
           END-EXEC.


           EXEC SQL
               DECLARE PURCHASE_TYPE CURSOR FOR
                   SELECT  *
                   FROM    PURCHASE_TYPE
                   WHERE   PURCHASE_TYPE.PREFIX = '00000'
                   ORDER BY TYPE
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
      *    PRODUCT DEMONSTRATION APPLICATION (PDA) COMMAREA LAYOUT    *
      *                                                               *
      *    ESTABLISH THE INITIAL COMMAREA FOR THE APPLICATION         *
      *                                                               *
      *    ALL OTHER PROGRAMS SHOULD DEFINE / USE THE COMMAREA FROM   *
      *    THE LINKAGE SECTION, AS IT WILL BE ESTABLISHED             *
      *****************************************************************

      *****************************************************************
      *    L I N K A G E     S E C T I O N                            *
      *****************************************************************

       LINKAGE SECTION.

       01  DFHCOMMAREA.
           COPY PDACOMM.

      *****************************************************************
      *    P R O C E D U R E    D I V I S I O N                       *
      *****************************************************************

       PROCEDURE DIVISION.


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00000-MAINLINE                                *
      *                                                               *
      *    FUNCTION :  PROGRAM ENTRY, CONTROL HIGH LEVEL PROCESSING   *
      *                FOR THE PRODUCT DEMONSTRATION APPLICATION MAIN *
      *                MENU.                                          *
      *                                                               *
      *    CALLED BY:  NONE                                           *
      *                                                               *
      *****************************************************************

       P00000-MAINLINE.

           EXEC CICS
               HANDLE CONDITION
                   DISABLED(P90010-DISABLED)
                   DUPREC(P90020-DUPREC)
                   ENDFILE(P90030-ENDFILE)
                   FILENOTFOUND(P90040-FILENOTFOUND)
                   ILLOGIC(P90050-ILLOGIC)
                   INVREQ(P90060-INVREQ)
                   IOERR(P90070-IOERR)
                   ISCINVREQ(P90080-ISCINVREQ)
                   LENGERR(P90090-LENGERR)
                   NOSPACE(P90100-NOSPACE)
                   NOTAUTH(P90110-NOTAUTH)
                   NOTFND(P90120-NOTFND)
                   NOTOPEN(P90130-NOTOPEN)
                   ERROR(P99100-GENERAL-ERROR)
           END-EXEC.

           PERFORM P00050-INITIALIZE THRU P00050-EXIT.                  TAGGED
                                                                        CODE
           PERFORM P10000-MAIN-PROCESS THRU P10000-EXIT.                TESTING
                                                                        03/13/01
           GOBACK.

       P00000-EXIT.
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

           IF EIBCALEN > ZEROES
               IF EIBCALEN NOT = PC-COMMAREA-LTH
                   MOVE 'CICS' TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA013' TO WPCE-PROGRAM-ID
                   MOVE ZEROES TO WPCE-RESPONSE-CODE
                   MOVE 'COMMAREA LENGTH NOT CORRECT' TO WPCE-COMMAND
                   MOVE 'P00050' TO WPCE-PARAGRAPH
                   PERFORM P99500-PDA-ERROR THRU P99500-EXIT
               END-IF
           ELSE
               MOVE PM019-ENTER-APPLICATION TO WMF-MESSAGE-AREA
               PERFORM P80400-SEND-MESSAGE THRU P80400-EXIT
               GO TO P00050-EXIT
           END-IF.

           MOVE SPACES TO WS-MENU-SELECTION-SW.
           MOVE 'I' TO WS-TRANS-INTENT-SW.
           MOVE 'N' TO WS-ERROR-FOUND-SW.
           MOVE 'N' TO WS-TOP-OF-DATA-SW.
           MOVE 'N' TO WS-BOTTOM-OF-DATA-SW.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME.          00020001

      *****************************************************************
      *    OBTAIN CURRENT DATE AND TIME FOR DISPLAY                   *
      *****************************************************************

           EXEC CICS
               ASKTIME
                   ABSTIME(WMF-ABSTIME)
           END-EXEC.

           EXEC CICS
               FORMATTIME
                   ABSTIME(WMF-ABSTIME)
                   MMDDYY(WMF-DATE-MMDDYY)
                   DATESEP('/')
                   TIME(WMF-TIME-HHMMSS)
                   TIMESEP
                   NOHANDLE
                   RESP(WS-RESPONSE-CODE)
           END-EXEC.

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               MOVE 'CICS' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA013' TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE TO WPCE-RESPONSE-CODE
               MOVE 'CICS FORMATTIME ABSTIME' TO WPCE-COMMAND
               MOVE 'P00050' TO WPCE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P00050-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P10000-MAIN-PROCESS                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE REFRESH PROCESS         *
      *                                                               *
      *    CALLED BY:  P00000-MAINLINE                                *
      *                                                               *
      *****************************************************************

       P10000-MAIN-PROCESS.

           MOVE PC-USERID-NUMBER TO WMF-USER-PREFIX
                                    WMF-CUSTOMER-PREFIX
                                    WMF-PENDING-PREFIX
                                    WMF-ORDER-PREFIX.
           MOVE 'CICS' TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA013' TO WPCE-PROGRAM-ID.

           EXEC CICS
               HANDLE CONDITION
                   NOTFND(P10100-NOTFND)
           END-EXEC.

           MOVE 'CLEAR CUSTOMER FILE' TO WPCE-COMMAND.
           MOVE 'P10100' TO WPCE-PARAGRAPH.
           MOVE 'N' TO WS-END-OF-PROCESS-SW.

           PERFORM P10100-CLEAR-CUSTOMER THRU P10100-EXIT
               UNTIL END-OF-PROCESS.

           EXEC CICS
               HANDLE CONDITION
                   NOTFND(P10200-NOTFND)
           END-EXEC.

           MOVE 'CLEAR PENDING ORDER FILE' TO WPCE-COMMAND.
           MOVE 'P10200' TO WPCE-PARAGRAPH.
           MOVE 'N' TO WS-END-OF-PROCESS-SW.

           PERFORM P10200-CLEAR-PENDING THRU P10200-EXIT
               UNTIL END-OF-PROCESS.

           PERFORM P10300-CLEAR-ORDER THRU P10300-EXIT.

           PERFORM P10500-CLEAR-ITEM THRU P10500-EXIT.

           PERFORM P10600-CLEAR-SUPPLIER THRU P10600-EXIT.

           PERFORM P10400-CLEAR-ITEM-SUPPLIER THRU P10400-EXIT.

           PERFORM P10700-CLEAR-PURCHASE-TYPE THRU P10700-EXIT.

           MOVE '00000' TO WMF-CUSTOMER-PREFIX
                           WMF-PENDING-PREFIX.

           EXEC CICS
               HANDLE CONDITION
                   NOTFND(P11100-NOTFND)
           END-EXEC.

           MOVE 'RELOAD CUSTOMER FILE' TO WPCE-COMMAND.
           MOVE 'P11100' TO WPCE-PARAGRAPH.
           MOVE 'N' TO WS-END-OF-PROCESS-SW.

           PERFORM P11100-RELOAD-CUSTOMER THRU P11100-EXIT
               UNTIL END-OF-PROCESS.

           PERFORM P11200-RELOAD-ORDER THRU P11200-EXIT.

           PERFORM P11300-RELOAD-ITEM THRU P11300-EXIT.

           PERFORM P11400-RELOAD-SUPPLIER THRU P11400-EXIT.

           PERFORM P11500-RELOAD-ITEM-SUPPLIER THRU P11500-EXIT.

           PERFORM P11600-RELOAD-PURCHASE-TYPE THRU P11600-EXIT.

       P10000-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P10100-CLEAR-CUSTOMER                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CLEAR THE CUSTOMER VSAM FILE        *
      *                                                               *
      *    CALLED BY:  P10000-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P10100-CLEAR-CUSTOMER.

           MOVE WMF-CUSTOMER-KEY TO CUSTOMER-KEY.

           EXEC CICS
               READ
                   FILE('PDACUST')
                   INTO(CUSTOMER-RECORD)
                   RIDFLD(CUSTOMER-KEY)
                   GTEQ
           END-EXEC.

           IF WMF-CUSTOMER-PREFIX NOT = CUSTOMER-PREFIX
               MOVE 'Y' TO WS-END-OF-PROCESS-SW
               GO TO P10100-EXIT
           END-IF.

           EXEC CICS
               DELETE
                   FILE('PDACUST')
                   RIDFLD(CUSTOMER-KEY)
           END-EXEC.

           GO TO P10100-EXIT.


       P10100-NOTFND.

           MOVE 'Y' TO WS-END-OF-PROCESS-SW.

       P10100-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P10200-CLEAR-PENDING                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CLEAR THE PENDING ORDER VSAM FILE   *
      *                                                               *
      *    CALLED BY:  P10000-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P10200-CLEAR-PENDING.

           MOVE WMF-PENDING-KEY TO PENDING-ORDER-KEY.

           EXEC CICS
               READ
                   FILE('PDAPEND')
                   INTO(PENDING-ORDER-RECORD)
                   RIDFLD(PENDING-ORDER-KEY)
                   GTEQ
           END-EXEC.

           IF WMF-PENDING-PREFIX NOT = PENDING-ORDER-PREFIX
               MOVE 'Y' TO WS-END-OF-PROCESS-SW
               GO TO P10200-EXIT
           END-IF.

           EXEC CICS
               DELETE
                   FILE('PDAPEND')
                   RIDFLD(PENDING-ORDER-KEY)
           END-EXEC.

           GO TO P10200-EXIT.


       P10200-NOTFND.

           MOVE 'Y' TO WS-END-OF-PROCESS-SW.

       P10200-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P10300-CLEAR-ORDER                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CLEAR THE ORDER DATABASE            *
      *                                                               *
      *    CALLED BY:  P10000-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P10300-CLEAR-ORDER.

           EXEC DLI
               SCHEDULE
                   PSB((WMF-PSB-NAME))
                   NODHABEND
           END-EXEC.

      *****************************************************************
      *    CHECK FOR PSB SCHEDULING ERROR                             *
      *****************************************************************

           IF DIBSTAT NOT = SPACES
               MOVE 'Y' TO WS-ERROR-FOUND-SW
               MOVE 'IMS' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA013' TO WPIE-PROGRAM-ID
               MOVE 'P10400' TO WPIE-PARAGRAPH
               MOVE 'SCHD' TO WPIE-FUNCTION-CODE
               MOVE SPACES TO WPIE-SEGMENT-NAME
                              WPIE-DATABASE-NAME
               MOVE DIBSTAT TO WPIE-STATUS-CODE
               MOVE 'PSB SCHEDULING ERROR' TO WPIE-COMMAND
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

           MOVE 'N' TO WS-END-OF-PROCESS-SW.

           PERFORM P10310-CLEAR-ORDER THRU P10310-EXIT
               UNTIL END-OF-PROCESS.

           EXEC DLI
               TERMINATE
           END-EXEC.

      *****************************************************************
      *    CHECK FOR PSB TERMINATION ERROR                            *
      *****************************************************************

           IF DIBSTAT NOT = SPACES
               MOVE 'Y' TO WS-ERROR-FOUND-SW
               MOVE 'IMS' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA013' TO WPIE-PROGRAM-ID
               MOVE 'P10400' TO WPIE-PARAGRAPH
               MOVE 'TERM' TO WPIE-FUNCTION-CODE
               MOVE SPACES TO WPIE-SEGMENT-NAME
                              WPIE-DATABASE-NAME
               MOVE DIBSTAT TO WPIE-STATUS-CODE
               MOVE 'PSB TERMINATION ERROR' TO WPIE-COMMAND
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P10300-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P10310-CLEAR-ORDER                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CLEAR THE ORDER DATABASE            *
      *                                                               *
      *    CALLED BY:  P10300-CLEAR-ORDER                             *
      *                                                               *
      *****************************************************************

       P10310-CLEAR-ORDER.

           EXEC DLI
               GU USING
                   PCB(1)
                   SEGMENT(ORDER)
                   INTO(ORDER-SEGMENT)
                   SEGLENGTH(123)
                   WHERE(ORDKEY>=WMF-ORDER-KEY)
                   FIELDLENGTH(15)
           END-EXEC.

      *****************************************************************
      *    CHECK STATUS CODE FOR SUCCESS, NOT FOUND, ALL OTHERS ARE   *
      *    AN ERROR                                                   *
      *****************************************************************

           EVALUATE TRUE
               WHEN DIBSTAT = SPACES
                   EXIT
               WHEN DIBSTAT = 'GE'
                   MOVE 'Y' TO WS-END-OF-PROCESS-SW
                   GO TO P10310-EXIT
               WHEN OTHER
                   MOVE 'Y' TO WS-ERROR-FOUND-SW
                   MOVE 'IMS' TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA013' TO WPIE-PROGRAM-ID
                   MOVE 'P10310' TO WPIE-PARAGRAPH
                   MOVE 'GU' TO WPIE-FUNCTION-CODE
                   MOVE 'ORDER' TO WPIE-SEGMENT-NAME
                   MOVE 'ORDER1DB' TO WPIE-DATABASE-NAME
                   MOVE DIBSTAT TO WPIE-STATUS-CODE
                   MOVE 'GU ORDER ROOT SEGMENT' TO WPIE-COMMAND
                   PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-EVALUATE.

           IF WMF-ORDER-PREFIX NOT = ORDER-PREFIX
               MOVE 'Y' TO WS-END-OF-PROCESS-SW
               GO TO P10310-EXIT
           END-IF.

           EXEC DLI
               DLET USING
                   PCB(1)
                   SEGMENT(ORDER)
                   FROM(ORDER-SEGMENT)
                   SEGLENGTH(123)
           END-EXEC.

      *****************************************************************
      *    CHECK STATUS CODE FOR SUCCESS, NOT FOUND, ALL OTHERS ARE   *
      *    AN ERROR                                                   *
      *****************************************************************

           IF DIBSTAT NOT = SPACES
               MOVE 'Y' TO WS-ERROR-FOUND-SW
               MOVE 'IMS' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA013' TO WPIE-PROGRAM-ID
               MOVE 'P10310' TO WPIE-PARAGRAPH
               MOVE 'DLET' TO WPIE-FUNCTION-CODE
               MOVE 'ORDER' TO WPIE-SEGMENT-NAME
               MOVE 'ORDER1DB' TO WPIE-DATABASE-NAME
               MOVE DIBSTAT TO WPIE-STATUS-CODE
               MOVE 'DLET ORDER ROOT SEGMENT' TO WPIE-COMMAND
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P10310-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P10400-CLEAR-ITEM-SUPPLIER                     *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CLEAR THE ITEM SUPPLIER TABLE       *
      *                                                               *
      *    CALLED BY:  P10000-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P10400-CLEAR-ITEM-SUPPLIER.

           EXEC SQL
               DELETE
               FROM    ITEM_SUPPLIER
               WHERE   ITEM_PREFIX = :WMF-USER-PREFIX
           END-EXEC.

           IF SQLCODE NOT = +0
               IF SQLCODE = +100
                   MOVE 'Y' TO WS-END-OF-PROCESS-SW
               ELSE
                   MOVE 'DB2' TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA013' TO WPDE-PROGRAM-ID
                   MOVE SQLCODE TO WPDE-DB2-SQLCODE
                   MOVE 'DELETE ITEM SUPPLIER TABLE ENTRIES' TO
                       WPDE-FUNCTION
                   MOVE 'P10400' TO WPDE-PARAGRAPH
                   PERFORM P99500-PDA-ERROR THRU P99500-EXIT
               END-IF
           END-IF.

       P10400-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P10500-CLEAR-ITEM                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CLEAR THE ITEM TABLE                *
      *                                                               *
      *    CALLED BY:  P10000-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P10500-CLEAR-ITEM.

           EXEC SQL
               DELETE
               FROM    ITEM
               WHERE   PREFIX = :WMF-USER-PREFIX
           END-EXEC.

           IF SQLCODE NOT = +0
               IF SQLCODE = +100
                   MOVE 'Y' TO WS-END-OF-PROCESS-SW
               ELSE
                   MOVE 'DB2' TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA013' TO WPDE-PROGRAM-ID
                   MOVE SQLCODE TO WPDE-DB2-SQLCODE
                   MOVE 'DELETE ITEM TABLE ENTRIES' TO WPDE-FUNCTION
                   MOVE 'P10500' TO WPDE-PARAGRAPH
                   PERFORM P99500-PDA-ERROR THRU P99500-EXIT
               END-IF
           END-IF.

       P10500-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P10600-CLEAR-SUPPLIER                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CLEAR THE SUPPLIER TABLE            *
      *                                                               *
      *    CALLED BY:  P10000-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P10600-CLEAR-SUPPLIER.

           EXEC SQL
               DELETE
               FROM    SUPPLIER
               WHERE   PREFIX = :WMF-USER-PREFIX
           END-EXEC.

           IF SQLCODE NOT = +0
               IF SQLCODE = +100
                   MOVE 'Y' TO WS-END-OF-PROCESS-SW
               ELSE
                   MOVE 'DB2' TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA013' TO WPDE-PROGRAM-ID
                   MOVE SQLCODE TO WPDE-DB2-SQLCODE
                   MOVE 'DELETE SUPPLIER TABLE ENTRIES' TO WPDE-FUNCTION
                   MOVE 'P10600' TO WPDE-PARAGRAPH
                   PERFORM P99500-PDA-ERROR THRU P99500-EXIT
               END-IF
           END-IF.

       P10600-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P10700-CLEAR-PURCHASE-TYPE                     *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CLEAR THE PURCHASE TYPE TABLE       *
      *                                                               *
      *    CALLED BY:  P10000-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P10700-CLEAR-PURCHASE-TYPE.

           EXEC SQL
               DELETE
               FROM    PURCHASE_TYPE
               WHERE   PREFIX = :WMF-USER-PREFIX
           END-EXEC.

           IF SQLCODE NOT = +0
               IF SQLCODE = +100
                   MOVE 'Y' TO WS-END-OF-PROCESS-SW
               ELSE
                   MOVE 'DB2' TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA013' TO WPDE-PROGRAM-ID
                   MOVE SQLCODE TO WPDE-DB2-SQLCODE
                   MOVE 'DELETE PURCHASE TYPE TABLE ENTRIES' TO
                       WPDE-FUNCTION
                   MOVE 'P10700' TO WPDE-PARAGRAPH
                   PERFORM P99500-PDA-ERROR THRU P99500-EXIT
               END-IF
           END-IF.

       P10700-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P11100-RELOAD-CUSTOMER                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RELOAD THE CUSTOMER VSAM FILE       *
      *                                                               *
      *    CALLED BY:  P10000-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P11100-RELOAD-CUSTOMER.

           MOVE WMF-CUSTOMER-KEY TO CUSTOMER-KEY.

           EXEC CICS
               READ
                   FILE('PDACUST')
                   INTO(CUSTOMER-RECORD)
                   RIDFLD(CUSTOMER-KEY)
                   GTEQ
           END-EXEC.

           IF WMF-CUSTOMER-PREFIX NOT = CUSTOMER-PREFIX
               MOVE 'Y' TO WS-END-OF-PROCESS-SW
               GO TO P11100-EXIT
           END-IF.

           MOVE CUSTOMER-KEY TO WMF-CUSTOMER-KEY.
           MOVE HIGH-VALUES TO WMF-CUSTOMER-SUFFIX.
           MOVE WMF-USER-PREFIX TO CUSTOMER-PREFIX.

           EXEC CICS
               WRITE
                   FILE('PDACUST')
                   FROM(CUSTOMER-RECORD)
                   RIDFLD(CUSTOMER-KEY)
           END-EXEC.

           GO TO P11100-EXIT.


       P11100-NOTFND.

           MOVE 'Y' TO WS-END-OF-PROCESS-SW.

       P11100-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P11200-RELOAD-ORDER                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RELOAD THE ORDER DATABASE           *
      *                                                               *
      *    CALLED BY:  P10000-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P11200-RELOAD-ORDER.

           EXEC DLI
               SCHEDULE
                   PSB((WMF-PSB-NAME))
                   NODHABEND
           END-EXEC.

      *****************************************************************
      *    CHECK FOR PSB SCHEDULING ERROR                             *
      *****************************************************************

           IF DIBSTAT NOT = SPACES
               MOVE 'Y' TO WS-ERROR-FOUND-SW
               MOVE 'IMS' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA013' TO WPIE-PROGRAM-ID
               MOVE 'P11200' TO WPIE-PARAGRAPH
               MOVE 'SCHD' TO WPIE-FUNCTION-CODE
               MOVE SPACES TO WPIE-SEGMENT-NAME
                              WPIE-DATABASE-NAME
               MOVE DIBSTAT TO WPIE-STATUS-CODE
               MOVE 'PSB SCHEDULING ERROR' TO WPIE-COMMAND
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

           MOVE SPACES TO ORDER-SEGMENT.
           MOVE WMF-USER-PREFIX TO ORDER-PREFIX.
           MOVE 0 TO ORDER-NUMBER.
           MOVE 5000 TO ORDER-PURCHASE-NUMBER.

           EXEC DLI
               ISRT USING
                   PCB(1)
                   SEGMENT(ORDER)
                   FROM(ORDER-SEGMENT)
                   SEGLENGTH(123)
           END-EXEC.

      *****************************************************************
      *    CHECK STATUS CODE FOR SUCCESS, ALL OTHERS ARE AN ERROR     *
      *****************************************************************

           IF DIBSTAT NOT = SPACES
               MOVE 'Y' TO WS-ERROR-FOUND-SW
               MOVE 'IMS' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA013' TO WPIE-PROGRAM-ID
               MOVE 'P11200' TO WPIE-PARAGRAPH
               MOVE 'ISRT' TO WPIE-FUNCTION-CODE
               MOVE 'ORDER' TO WPIE-SEGMENT-NAME
               MOVE 'ORDER1DB' TO WPIE-DATABASE-NAME
               MOVE DIBSTAT TO WPIE-STATUS-CODE
               MOVE 'ISRT ORDER ROOT SEGMENT' TO WPIE-COMMAND
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

           EXEC DLI
               TERMINATE
           END-EXEC.

      *****************************************************************
      *    CHECK FOR PSB TERMINATION ERROR                            *
      *****************************************************************

           IF DIBSTAT NOT = SPACES
               MOVE 'Y' TO WS-ERROR-FOUND-SW
               MOVE 'IMS' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA013' TO WPIE-PROGRAM-ID
               MOVE 'P11200' TO WPIE-PARAGRAPH
               MOVE 'TERM' TO WPIE-FUNCTION-CODE
               MOVE SPACES TO WPIE-SEGMENT-NAME
                              WPIE-DATABASE-NAME
               MOVE DIBSTAT TO WPIE-STATUS-CODE
               MOVE 'PSB TERMINATION ERROR' TO WPIE-COMMAND
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P11200-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P11300-RELOAD-ITEM                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RELOAD THE ITEM TABLE               *
      *                                                               *
      *    CALLED BY:  P10000-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P11300-RELOAD-ITEM.

           EXEC SQL
               OPEN ITEM
           END-EXEC.

           IF SQLCODE NOT = +0
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA013' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'OPEN ITEM CURSOR' TO WPDE-FUNCTION
               MOVE 'P11300' TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

           MOVE 'N' TO WS-END-OF-PROCESS-SW.

           PERFORM P11310-LOAD-ITEM THRU P11310-EXIT
               UNTIL END-OF-PROCESS.

           EXEC SQL
               CLOSE ITEM
           END-EXEC.

           IF SQLCODE NOT = +0
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA013' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'CLOSE ITEM CURSOR' TO WPDE-FUNCTION
               MOVE 'P11300' TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P11300-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P11310-LOAD-ITEM                               *
      *                                                               *
      *    FUNCTION :  ROUTINE TO LOAD THE ITEM TABLE                 *
      *                                                               *
      *    CALLED BY:  P11300-RELOAD-ITEM                             *
      *                                                               *
      *****************************************************************

       P11310-LOAD-ITEM.

           EXEC SQL
               FETCH  ITEM
               INTO   :ITEM-PREFIX,
                      :ITEM-NUMBER,
                      :ITEM-CATEGORY-NAME,
                      :ITEM-SUB-CATEGORY-NAME,
                      :ITEM-NAME,
                      :ITEM-LENGTH,
                      :ITEM-DIAMETER
           END-EXEC.

           IF SQLCODE NOT = +0
               IF SQLCODE = +100
                   MOVE 'Y' TO WS-END-OF-PROCESS-SW
                   GO TO P11310-EXIT
               ELSE
                   MOVE 'DB2' TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA013' TO WPDE-PROGRAM-ID
                   MOVE SQLCODE TO WPDE-DB2-SQLCODE
                   MOVE 'FETCH ITEM CURSOR' TO WPDE-FUNCTION
                   MOVE 'P11310' TO WPDE-PARAGRAPH
                   PERFORM P99500-PDA-ERROR THRU P99500-EXIT
               END-IF
           END-IF.

           MOVE WMF-USER-PREFIX TO ITEM-PREFIX.

           EXEC SQL
               INSERT
               INTO   ITEM
                     (PREFIX,
                      NUMBER,
                      CATEGORY_NAME,
                      SUB_CATEGORY_NAME,
                      NAME,
                      LENGTH,
                      DIAMETER)
               VALUES
                     (:ITEM-PREFIX,
                      :ITEM-NUMBER,
                      :ITEM-CATEGORY-NAME,
                      :ITEM-SUB-CATEGORY-NAME,
                      :ITEM-NAME,
                      :ITEM-LENGTH,
                      :ITEM-DIAMETER)
           END-EXEC.

           IF SQLCODE NOT = +0
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA013' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'INSERT ITEM TABLE' TO WPDE-FUNCTION
               MOVE 'P11310' TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P11310-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P11400-RELOAD-SUPPLIER                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RELOAD THE SUPPLIER TABLE           *
      *                                                               *
      *    CALLED BY:  P10000-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P11400-RELOAD-SUPPLIER.

           EXEC SQL
               OPEN SUPPLIER
           END-EXEC.

           IF SQLCODE NOT = +0
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA013' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'OPEN SUPPLIER CURSOR' TO WPDE-FUNCTION
               MOVE 'P11400' TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

           MOVE 'N' TO WS-END-OF-PROCESS-SW.

           PERFORM P11410-LOAD-SUPPLIER THRU P11410-EXIT
               UNTIL END-OF-PROCESS.

           EXEC SQL
               CLOSE SUPPLIER
           END-EXEC.

           IF SQLCODE NOT = +0
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA013' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'CLOSE SUPPLIER CURSOR' TO WPDE-FUNCTION
               MOVE 'P11400' TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P11400-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P11410-LOAD-SUPPLIER                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO LOAD THE SUPPLIER TABLE             *
      *                                                               *
      *    CALLED BY:  P11400-LOAD-SUPPLIER                           *
      *                                                               *
      *****************************************************************

       P11410-LOAD-SUPPLIER.

           EXEC SQL
               FETCH  SUPPLIER
               INTO   :SUPPLIER-PREFIX,
                      :SUPPLIER-SUPPLIER-ID,
                      :SUPPLIER-PASSWORD,
                      :SUPPLIER-NAME,
                      :SUPPLIER-ADDRESS,
                      :SUPPLIER-CITY,
                      :SUPPLIER-STATE,
                      :SUPPLIER-POSTAL-CODE,
                      :SUPPLIER-EMAIL-ADDRESS
           END-EXEC.

           IF SQLCODE NOT = +0
               IF SQLCODE = +100
                   MOVE 'Y' TO WS-END-OF-PROCESS-SW
                   GO TO P11410-EXIT
               ELSE
                   MOVE 'DB2' TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA013' TO WPDE-PROGRAM-ID
                   MOVE SQLCODE TO WPDE-DB2-SQLCODE
                   MOVE 'FETCH SUPPLIER CURSOR' TO WPDE-FUNCTION
                   MOVE 'P11410' TO WPDE-PARAGRAPH
                   PERFORM P99500-PDA-ERROR THRU P99500-EXIT
               END-IF
           END-IF.

           MOVE WMF-USER-PREFIX TO SUPPLIER-PREFIX.
           MOVE SUPPLIER-EMAIL-ADDRESS TO WMF-EMAIL-ADDRESS.
           MOVE WMF-USER-PREFIX TO WMF-EMAIL-PREFIX.
           MOVE WMF-EMAIL-ADDRESS TO SUPPLIER-EMAIL-ADDRESS.

           EXEC SQL
               INSERT
               INTO   SUPPLIER
                     (PREFIX,
                      SUPPLIER_ID,
                      PASSWORD,
                      NAME,
                      ADDRESS,
                      CITY,
                      STATE,
                      POSTAL_CODE,
                      EMAIL_ADDRESS)
               VALUES
                     (:SUPPLIER-PREFIX,
                      :SUPPLIER-SUPPLIER-ID,
                      :SUPPLIER-PASSWORD,
                      :SUPPLIER-NAME,
                      :SUPPLIER-ADDRESS,
                      :SUPPLIER-CITY,
                      :SUPPLIER-STATE,
                      :SUPPLIER-POSTAL-CODE,
                      :SUPPLIER-EMAIL-ADDRESS)
           END-EXEC.

           IF SQLCODE NOT = +0
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA013' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'INSERT SUPPLIER TABLE' TO WPDE-FUNCTION
               MOVE 'P11410' TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P11410-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P11500-RELOAD-ITEM-SUPPLIER                    *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RELOAD THE ITEM SUPPLIER TABLE      *
      *                                                               *
      *    CALLED BY:  P10000-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P11500-RELOAD-ITEM-SUPPLIER.

           EXEC SQL
               OPEN ITEM_SUPPLIER
           END-EXEC.

           IF SQLCODE NOT = +0
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA013' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'OPEN ITEM-SUPPLIER CURSOR' TO WPDE-FUNCTION
               MOVE 'P11500' TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

           MOVE 'N' TO WS-END-OF-PROCESS-SW.

           PERFORM P11510-LOAD-ITEM-SUPPLIER THRU P11510-EXIT
               UNTIL END-OF-PROCESS.

           EXEC SQL
               CLOSE ITEM_SUPPLIER
           END-EXEC.

           IF SQLCODE NOT = +0
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA013' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'CLOSE ITEM-SUPPLIER CURSOR' TO WPDE-FUNCTION
               MOVE 'P11500' TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P11500-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P11510-LOAD-ITEM-SUPPLIER                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO LOAD THE ITEM SUPPLIER TABLE        *
      *                                                               *
      *    CALLED BY:  P11500-RELOAD-ITEM-SUPPLIER                    *
      *                                                               *
      *****************************************************************

       P11510-LOAD-ITEM-SUPPLIER.

           EXEC SQL
               FETCH  ITEM_SUPPLIER
               INTO   :ITEM-SUPPLIER-ITEM-PREFIX,
                      :ITEM-SUPPLIER-ITEM-NUMBER,
                      :ITEM-SUPPLIER-SUPPLIER-PREFIX,
                      :ITEM-SUPPLIER-SUPPLIER-ID,
                      :ITEM-SUPPLIER-QUANTITY-ON-HAND,
                      :ITEM-SUPPLIER-UNIT-PRICE
           END-EXEC.

           IF SQLCODE NOT = +0
               IF SQLCODE = +100
                   MOVE 'Y' TO WS-END-OF-PROCESS-SW
                   GO TO P11510-EXIT
               ELSE
                   MOVE 'DB2' TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA013' TO WPDE-PROGRAM-ID
                   MOVE SQLCODE TO WPDE-DB2-SQLCODE
                   MOVE 'FETCH ITEM-SUPPLIER CURSOR' TO WPDE-FUNCTION
                   MOVE 'P11510' TO WPDE-PARAGRAPH
                   PERFORM P99500-PDA-ERROR THRU P99500-EXIT
               END-IF
           END-IF.

           MOVE WMF-USER-PREFIX TO ITEM-SUPPLIER-ITEM-PREFIX
                                   ITEM-SUPPLIER-SUPPLIER-PREFIX.

           EXEC SQL
               INSERT
               INTO   ITEM_SUPPLIER
                     (ITEM_PREFIX,
                      ITEM_NUMBER,
                      SUPPLIER_PREFIX,
                      SUPPLIER_ID,
                      QUANTITY_ON_HAND,
                      UNIT_PRICE)
               VALUES
                     (:ITEM-SUPPLIER-ITEM-PREFIX,
                      :ITEM-SUPPLIER-ITEM-NUMBER,
                      :ITEM-SUPPLIER-SUPPLIER-PREFIX,
                      :ITEM-SUPPLIER-SUPPLIER-ID,
                      :ITEM-SUPPLIER-QUANTITY-ON-HAND,
                      :ITEM-SUPPLIER-UNIT-PRICE)
           END-EXEC.

           IF SQLCODE NOT = +0
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA013' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'INSERT ITEM-SUPPLIER TABLE' TO WPDE-FUNCTION
               MOVE 'P11510' TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P11510-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P11600-RELOAD-PURCHASE-TYPE                    *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RELOAD THE PURCHASE TYPE TABLE      *
      *                                                               *
      *    CALLED BY:  P10000-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P11600-RELOAD-PURCHASE-TYPE.

           EXEC SQL
               OPEN PURCHASE_TYPE
           END-EXEC.

           IF SQLCODE NOT = +0
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA013' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'OPEN PURCHASE-TYPE CURSOR' TO WPDE-FUNCTION
               MOVE 'P11600' TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

           MOVE 'N' TO WS-END-OF-PROCESS-SW.

           PERFORM P11610-LOAD-PURCHASE-TYPE THRU P11610-EXIT
               UNTIL END-OF-PROCESS.

           EXEC SQL
               CLOSE PURCHASE_TYPE
           END-EXEC.

           IF SQLCODE NOT = +0
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA013' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'CLOSE PURCHASE-TYPE CURSOR' TO WPDE-FUNCTION
               MOVE 'P11600' TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P11600-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P11610-LOAD-PURCHASE-TYPE                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO LOAD THE PURCHASE TYPE TABLE        *
      *                                                               *
      *    CALLED BY:  P11600-RELOAD-PURCHASE-TYPE                    *
      *                                                               *
      *****************************************************************

       P11610-LOAD-PURCHASE-TYPE.

           EXEC SQL
               FETCH  PURCHASE_TYPE
               INTO   :PURCHASE-TYPE-PREFIX,
                      :PURCHASE-TYPE-TYPE,
                      :PURCHASE-TYPE-DESCRIPTION
           END-EXEC.

           IF SQLCODE NOT = +0
               IF SQLCODE = +100
                   MOVE 'Y' TO WS-END-OF-PROCESS-SW
                   GO TO P11610-EXIT
               ELSE
                   MOVE 'DB2' TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA013' TO WPDE-PROGRAM-ID
                   MOVE SQLCODE TO WPDE-DB2-SQLCODE
                   MOVE 'FETCH PURCHASE-TYPE CURSOR' TO WPDE-FUNCTION
                   MOVE 'P11610' TO WPDE-PARAGRAPH
                   PERFORM P99500-PDA-ERROR THRU P99500-EXIT
               END-IF
           END-IF.

           MOVE WMF-USER-PREFIX TO PURCHASE-TYPE-PREFIX.

           EXEC SQL
               INSERT
               INTO   PURCHASE_TYPE
                     (PREFIX,
                      TYPE,
                      DESCRIPTION)
               VALUES
                     (:PURCHASE-TYPE-PREFIX,
                      :PURCHASE-TYPE-TYPE,
                      :PURCHASE-TYPE-DESCRIPTION)
           END-EXEC.

           IF SQLCODE NOT = +0
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA013' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'INSERT PURCHASE-TYPE TABLE' TO WPDE-FUNCTION
               MOVE 'P11610' TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P11610-EXIT.
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

           EXEC CICS
               XCTL
                   PROGRAM(PC-NEXT-PGRMID)
                   COMMAREA(PDA-COMMAREA)
                   LENGTH(PC-COMMAREA-LTH)
                   NOHANDLE
                   RESP(WS-RESPONSE-CODE)
           END-EXEC.

      *****************************************************************
      *    IF ERROR, FORMAT ERROR INFORMATION AND TERMINATE           *
      *****************************************************************

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               MOVE 'CICS' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA013' TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE TO WPCE-RESPONSE-CODE
               MOVE 'CICS XCTL --- ' TO WPCE-COMMAND-1
               MOVE PC-NEXT-PGRMID TO WPCE-COMMAND-2
               MOVE 'P80300' TO WPCE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P80300-EXIT.
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

           EXEC CICS
               SEND
                   FROM(WMF-MESSAGE-AREA)
                   LENGTH(WS-MESSAGE-LTH)
                   ERASE
                   NOHANDLE
                   RESP(WS-RESPONSE-CODE)
           END-EXEC.

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               MOVE 'CICS' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA013' TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE TO WPCE-RESPONSE-CODE
               MOVE 'CICS SEND' TO WPCE-COMMAND
               MOVE 'P80400' TO WPCE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

      *****************************************************************
      *    CURSOR AT FIRST POSITION ON SCREEN, IF ERROR TERMINATE     *
      *****************************************************************

           EXEC CICS
               SEND
                   CONTROL
                   CURSOR(0)
                   NOHANDLE
                   RESP(WS-RESPONSE-CODE)
           END-EXEC.

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               MOVE 'CICS' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA013' TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE TO WPCE-RESPONSE-CODE
               MOVE 'CICS SEND CONTROL' TO WPCE-COMMAND
               MOVE 'P80400' TO WPCE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

      *****************************************************************
      *    RETURN TO CICS (NO TRANSID OPTION)                         *
      *****************************************************************

           EXEC CICS
               RETURN
           END-EXEC.

           GOBACK.

       P80400-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P90...                                         *
      *                                                               *
      *    FUNCTION :  ROUTINES TO DISPLAY CICS ERROR MESSAGES BASED  *
      *                UPON CICS HANDLE CONDITIONS.                   *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************

       P90010-DISABLED.

           MOVE EIBRESP TO WPCE-RESPONSE-CODE.
           MOVE 'DISABLED' TO WPCE-COMMAND-2.

           PERFORM P99500-PDA-ERROR THRU P99500-EXIT.


       P90020-DUPREC.

           MOVE EIBRESP TO WPCE-RESPONSE-CODE.
           MOVE 'DUPREC' TO WPCE-COMMAND-2.

           PERFORM P99500-PDA-ERROR THRU P99500-EXIT.


       P90030-ENDFILE.

           MOVE EIBRESP TO WPCE-RESPONSE-CODE.
           MOVE 'ENDFILE' TO WPCE-COMMAND-2.

           PERFORM P99500-PDA-ERROR THRU P99500-EXIT.


       P90040-FILENOTFOUND.

           MOVE EIBRESP TO WPCE-RESPONSE-CODE.
           MOVE 'FILENOTFOUND' TO WPCE-COMMAND-2.

           PERFORM P99500-PDA-ERROR THRU P99500-EXIT.


       P90050-ILLOGIC.

           MOVE EIBRESP TO WPCE-RESPONSE-CODE.
           MOVE 'ILLOGIC' TO WPCE-COMMAND-2.

           PERFORM P99500-PDA-ERROR THRU P99500-EXIT.


       P90060-INVREQ.

           MOVE EIBRESP TO WPCE-RESPONSE-CODE.
           MOVE 'INVREQ' TO WPCE-COMMAND-2.

           PERFORM P99500-PDA-ERROR THRU P99500-EXIT.


       P90070-IOERR.

           MOVE EIBRESP TO WPCE-RESPONSE-CODE.
           MOVE 'IOERR' TO WPCE-COMMAND-2.

           PERFORM P99500-PDA-ERROR THRU P99500-EXIT.


       P90080-ISCINVREQ.

           MOVE EIBRESP TO WPCE-RESPONSE-CODE.
           MOVE 'ISCINVREQ' TO WPCE-COMMAND-2.

           PERFORM P99500-PDA-ERROR THRU P99500-EXIT.


       P90090-LENGERR.

           MOVE EIBRESP TO WPCE-RESPONSE-CODE.
           MOVE 'LENGERR' TO WPCE-COMMAND-2.

           PERFORM P99500-PDA-ERROR THRU P99500-EXIT.


       P90100-NOSPACE.

           MOVE EIBRESP TO WPCE-RESPONSE-CODE.
           MOVE 'NOSPACE' TO WPCE-COMMAND-2.

           PERFORM P99500-PDA-ERROR THRU P99500-EXIT.


       P90110-NOTAUTH.

           MOVE EIBRESP TO WPCE-RESPONSE-CODE.
           MOVE 'NOTAUTH' TO WPCE-COMMAND-2.

           PERFORM P99500-PDA-ERROR THRU P99500-EXIT.


       P90120-NOTFND.

           MOVE EIBRESP TO WPCE-RESPONSE-CODE.
           MOVE 'NOTFND' TO WPCE-COMMAND-2.

           PERFORM P99500-PDA-ERROR THRU P99500-EXIT.


       P90130-NOTOPEN.

           MOVE EIBRESP TO WPCE-RESPONSE-CODE.
           MOVE 'NOTOPEN' TO WPCE-COMMAND-2.

           PERFORM P99500-PDA-ERROR THRU P99500-EXIT.
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

           MOVE 'CICS' TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA013' TO WPCE-PROGRAM-ID.
           MOVE EIBRESP TO WPCE-RESPONSE-CODE.
           MOVE 'UNHANDLED CICS ERROR' TO WPCE-COMMAND.
           MOVE 'P99100' TO WPCE-PARAGRAPH.

           PERFORM P99500-PDA-ERROR THRU P99500-EXIT.

       P99100-EXIT.
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

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P99500-PDA-ERROR                               *
      *                                                               *
      *    FUNCTION :  ROUTINE TO HANDLE FATAL / TERMINATING CICS,    *
      *                DB2, IMS-DLI ERRORS                            *
      *                                                               *
      *                AN ERROR SCREEN CONTAINING TEXT IS SENT TO     *
      *                THE USER INDICATING THE NATURE OF THE ERROR    *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************

       P99500-PDA-ERROR.

      *****************************************************************
      *      SUSPEND ANY HANDLE CONDITIONS IN EFFECT                  *
      *****************************************************************

           EXEC CICS
               PUSH HANDLE
           END-EXEC.

      *****************************************************************
      *      ROLLBACK ANY TRANSACTION UPDATES                         *
      *****************************************************************

           EXEC CICS
               SYNCPOINT ROLLBACK
           END-EXEC.

      *****************************************************************
      *      FORMAT AND SEND ERROR TEXT                               *
      *****************************************************************

           EVALUATE TRUE
               WHEN PDA-DB2-ERROR
                   MOVE WS-PDA-DB2-ERROR-01 TO WPEA-ERROR-07-TEXT
                   MOVE WS-PDA-DB2-ERROR-02 TO WPEA-ERROR-08-TEXT
               WHEN PDA-IMS-ERROR
                   MOVE WS-PDA-IMS-ERROR-01 TO WPEA-ERROR-07-TEXT
                   MOVE WS-PDA-IMS-ERROR-02 TO WPEA-ERROR-08-TEXT
               WHEN OTHER
                   MOVE WS-PDA-CICS-ERROR-01 TO WPEA-ERROR-07-TEXT
                   MOVE WS-PDA-CICS-ERROR-02 TO WPEA-ERROR-08-TEXT
           END-EVALUATE.

           EXEC CICS
               DUMP
                   TRANSACTION
                   DUMPCODE('PDER')
           END-EXEC.

           EXEC CICS
               SEND
                   FROM(WS-PDA-ERROR-AREA)
                   LENGTH(WS-PDA-ERROR-LENGTH)
                   ERASE
           END-EXEC.

           EXEC CICS
               SEND
                   CONTROL
                   CURSOR(0)
           END-EXEC.

           EXEC CICS
               ABEND
                   ABCODE('PD13')
           END-EXEC.

       P99500-EXIT.
           EXIT.
           EJECT
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
USED       MOVE 'CICS' TO WS-PDA-ERROR-TYPE.                             USED
AS OF      MOVE 'PDA013' TO WPCE-PROGRAM-ID.                             AS OF
JAN        MOVE EIBRESP TO WPCE-RESPONSE-CODE.                           JAN
2001       MOVE 'ERROR' TO WPCE-COMMAND.                                 2001
           MOVE 'P99999' TO WPCE-PARAGRAPH.
LLR                                                                      LLR
           PERFORM P99500-PDA-ERROR THRU P99500-EXIT.

       P99999-ERROR-EXIT.
           EXIT.
           EJECT