       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA008.
      *
      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA008                                            *
      * TRANS   :   PD08                                              *
      * MAPSET  :   PDA008M                                           *
      *                                                               *
      * FUNCTION:   PROGRAM PDA008 IS THE PENDING ORDER SCREEN WHICH  *
      *             DISPLAYS A SCROLLABLE LIST OF ALL ITEMS SELECTED  *
      *             BY THE USER DURING THE ORDER ADD PROCESS.         *
      *             PENDING ORDERS REPRESENT A HOLDING/WORKING AREA   *
      *             FOR ITEMS BEFORE THE ACTUAL ORDER PLACEMENT       *
      *             PROCESS OCCURS.  THE USER MAY CHANGE ITEM ORDER   *
      *             INFORMATION, DELETE ITEMS, AND CHECK ITEM         *
      *             AVAILABILITY BASED ON ORDER QUANTITY AND THE      *
      *             SUPPLIER'S STOCK ON HAND.                         *
      *                                                               *
      * FILES   :   ITEM               -  DB2        (READ-ONLY)      *
      *             ITEM_SUPPLIER      -  DB2        (READ-ONLY)      *
      *             PENDING_ORDER_FILE -  VSAM KSDS  (UPDATE)         *
      *                                                               *
      *                                                               *
      * TRANSACTIONS GENERATED:                                       *
      *             PD04       CUSTOMER IDENTIFICATION (VIA PF3=PREV) *
      *             PD07       ITEM DETAIL             (VIA PF3=PREV) *
      *             PD09       PROCESS ORDER                          *
      *             PD05       BROWSE CATEGORIES                      *
      *             PD02       ORDER MENU                             *
      *             PD01       MAIN MENU                              *
      *                                                               *
      *                                                               *
      * PFKEYS  :   PF03  =    PREVIOUS- EITHER PDA002 - ORDER MENU   *
      *                        OR PDA007 - ITEM DETAIL DEPENDING ON   *
      *                        THE ORIGINATING PROGRAM                *
      *             PF04  =    PROCEED TO PDA009, PROCESS ORDER       *
      *             PF07  =    SCROLL BACKWARD                        *
      *             PF08  =    SCROLL FORWARD                         *
      *             PF10  =    PROCEED TO PDA005, BROWSE CATEGORIES   *
      *             PF11  =    EXIT, RETURN TO ORDER MENU             *
      *             PF12  =    EXIT, RETURN TO MAIN MENU              *
      *                                                               *
      *                                                               *
      *****************************************************************
      *             PROGRAM CHANGE LOG                                *
      *             -------------------                               *
      *                                                               *
      *  DATE       UPDATED BY            CHANGE DESCRIPTION          *
      *  --------   --------------------  --------------------------  *
      *  04/17/01   PAUL BARON            ADDED PF3 - PREVIOUS        *
      *                                   FUNCTIONALITY               *
      *                                                               *
      *                                   WHEN SCENARIO 8 ACTIVE,     *
      *                                   ABEND TASK WITH DUMP INSTEAD*
      *                                   OF DISPLAYING THE ERROR     *
      *                                   SCREEN                      *
      *                                                               *
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
       77  WS-SUB2                     PIC S9(4) COMP VALUE +0.
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
           05  WS-ORDER-FOUND-SW       PIC X     VALUE 'N'.
               88  ORDER-FOUND                   VALUE 'Y'.
           05  EIBAID-SW               PIC X     VALUE ' '.
               88  CLEAR-PKEY                    VALUE '_'.
               88  ENTER-PKEY                    VALUE ''''.
               88  PREVIOUS-PKEY                 VALUE '3'.
               88  PROCESS-ORDER-PKEY            VALUE '4'.
               88  BACKWARD-PKEY                 VALUE '7'.
               88  FORWARD-PKEY                  VALUE '8'.
               88  BROWSE-CATEGORIES-PKEY        VALUE ':'.
               88  ORDER-MENU-PKEY               VALUE '#'.
               88  MAIN-MENU-PKEY                VALUE '@'.
               88  VALID-PKEY-ENTERED            VALUE '_' '@' '4' '7'
                                                       '3'
                                                       '8' ':' '#' ''''.
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
           05  WMF-QTY-LEN             PIC 9     VALUE 0.
           05  WMF-UNDERSCORE-LOWVALUE.
               10  FILLER              PIC X     VALUE '_'.
               10  FILLER              PIC X     VALUE LOW-VALUES.
           05  WMF-UNDERSCORE-LOWVALUE-R
                                      REDEFINES WMF-UNDERSCORE-LOWVALUE
                                       PIC XX.
           05  WMF-SPACES-LOWVALUE.
               10  FILLER              PIC X     VALUE SPACES.
               10  FILLER              PIC X     VALUE LOW-VALUES.
           05  WMF-SPACES-LOWVALUE-R   REDEFINES WMF-SPACES-LOWVALUE
                                       PIC XX.
           05  WMF-ITEM-PREFIX         PIC X(5)  VALUE SPACES.
           05  WMF-ITEM-NUMBER         PIC X(32) VALUE SPACES.
           05  WMF-ITEM-SEQ            PIC 9(5)  VALUE ZEROES.
           05  WMF-QTY                 OCCURS 3 TIMES
                                       PIC 9(9).
           05  WMF-EXT-PRICE           PIC S9(11)V99 VALUE +0.
           05  WMF-TOTAL-COST          PIC S9(11)V99 VALUE +0.
           05  WS-PDA008-WORKAREA.
               07  WPW-FIRST-SUPPLIER  PIC X(32) VALUE SPACES.
               07  WPW-LAST-SUPPLIER   PIC X(32) VALUE SPACES.
               07  WPW-PAGE-NUMBER     PIC 9(5)  VALUE ZEROES.
               07  WPW-MORE-SW         PIC X     VALUE SPACES.
               07  WPW-REFRESH-SW      PIC X     VALUE SPACES.

       01  PDASP1-PREFIX               PIC X(5)  VALUE SPACES.
       01  PDASP1-TOTAL-COST           PIC S9(15)V99 VALUE +0   COMP-3.
       01  PDASP1-STATUS               PIC X(4)  VALUE SPACES.

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
      *         MAP DSECTS -- BROWSE ITEMS BY CATEGORY - PDA008M      *
      *****************************************************************

           COPY PDA008M.
       01  FILLER                      REDEFINES PDA008O.
           03  FILLER                  PIC X(51).
           03  P8-SCREEN-AREA          OCCURS 3 TIMES.
               05  ACT-LEN             PIC S9(4)                COMP.
               05  ACT-ATTR            PIC X.
               05  FILLER              PIC XX.
               05  SCRN-ACTION         PIC X.
               05  QTY-LEN             PIC S9(4)                COMP.
               05  QTY-ATTR            PIC X.
               05  FILLER              PIC XX.
               05  SCRN-QUANTITY       PIC Z(9).
               05  FILLER              REDEFINES SCRN-QUANTITY.
                   07  SCRN-QTY        OCCURS 9 TIMES
                                       PIC X.
               05  FILLER              REDEFINES SCRN-QUANTITY.
                   07  SCRN-QTY-1      PIC 9.
               05  FILLER              REDEFINES SCRN-QUANTITY.
                   07  SCRN-QTY-2      PIC 99.
               05  FILLER              REDEFINES SCRN-QUANTITY.
                   07  SCRN-QTY-3      PIC 9(3).
               05  FILLER              REDEFINES SCRN-QUANTITY.
                   07  SCRN-QTY-4      PIC 9(4).
               05  FILLER              REDEFINES SCRN-QUANTITY.
                   07  SCRN-QTY-5      PIC 9(5).
               05  FILLER              REDEFINES SCRN-QUANTITY.
                   07  SCRN-QTY-6      PIC 9(6).
               05  FILLER              REDEFINES SCRN-QUANTITY.
                   07  SCRN-QTY-7      PIC 9(7).
               05  FILLER              REDEFINES SCRN-QUANTITY.
                   07  SCRN-QTY-8      PIC 9(8).
               05  FILLER              REDEFINES SCRN-QUANTITY.
                   07  SCRN-QTY-9      PIC 9(9).
               05  ITEM-LEN            PIC S9(4)                COMP.
               05  ITEM-ATTR           PIC X.
               05  FILLER              PIC XX.
               05  SCRN-ITEM           PIC X(32).
               05  SUPPLIER-LEN        PIC S9(4)                COMP.
               05  SUPPLIER-ATTR       PIC X.
               05  FILLER              PIC XX.
               05  SCRN-ITEM-NAME      PIC X(50).
               05  SUPPID-LEN          PIC S9(4)                COMP.
               05  SUPPID-ATTR         PIC X.
               05  FILLER              PIC XX.
               05  SCRN-SUPPLIER-ID    PIC X(32).
               05  ORDSEQ-LEN          PIC S9(4)                COMP.
               05  ORDSEQ-ATTR         PIC X.
               05  FILLER              PIC XX.
               05  SCRN-ORDER-SEQ      PIC X(5).
               05  HPRICE-LEN          PIC S9(4)                COMP.
               05  HPRICE-ATTR         PIC X.
               05  FILLER              PIC XX.
               05  SCRN-HID-PRICE      PIC 9(8)V99.
               05  PRICE-LEN           PIC S9(4)                COMP.
               05  PRICE-ATTR          PIC X.
               05  FILLER              PIC XX.
               05  SCRN-PRICE          PIC ZZ,ZZZ,ZZZ.ZZ.
               05  XPRICE-LEN          PIC S9(4)                COMP.
               05  XPRICE-ATTR         PIC X.
               05  FILLER              PIC XX.
               05  SCRN-EXT-PRICE      PIC ZZ,ZZZ,ZZZ.ZZ.
               05  STOCK-LEN           PIC S9(4)                COMP.
               05  STOCK-ATTR          PIC X.
               05  FILLER              PIC XX.
               05  SCRN-STOCK          PIC X(12).
           EJECT
      *****************************************************************
      *    IMS / DLI DEFINITIONS                                      *
      *****************************************************************
           EJECT
      *****************************************************************
      *    VSAM FILE DEFINITIONS                                      *
      *****************************************************************

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
              INCLUDE DITEM
           END-EXEC.
           EJECT
           EXEC SQL
              INCLUDE DITMSUP
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
      *    P R O G R A M     W O R K A R E A                          *
      *****************************************************************

      *01  WS-PDA008-WORKAREA.
      *    05  WPW-FIRST-SUPPLIER      PIC X(32) VALUE SPACES.
      *    05  WPW-LAST-SUPPLIER       PIC X(32) VALUE SPACES.

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


      *****************************************************************
      *    ALLOW USER TO EXIT APPLICATION WITH CLEAR KEY              *
      *    (SEND MESSAGE, ERASE SCREEN)                               *
      *****************************************************************

           IF EIBAID = DFHCLEAR
               MOVE PM002-EXIT-APPLICATION TO WMF-MESSAGE-AREA
               PERFORM P80400-SEND-MESSAGE THRU P80400-EXIT
           END-IF.

           PERFORM P00050-INITIALIZE THRU P00050-EXIT.                  TAGGED
                                                                        CODE
           PERFORM P00100-MAIN-PROCESS THRU P00100-EXIT.                TESTING
                                                                        03/13/01
           PERFORM P00200-CICS-RETURN THRU P00200-EXIT.

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
                   MOVE 'PDA008' TO WPCE-PROGRAM-ID
                   MOVE ZEROES TO WPCE-RESPONSE-CODE
                   MOVE 'COMMAREA LENGTH NOT CORRECT' TO WPCE-COMMAND
                   MOVE 'P00500' TO WPCE-PARAGRAPH
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
KGT412     MOVE 'N' TO WS-TOP-OF-DATA-SW.
KGT412     MOVE 'N' TO WS-BOTTOM-OF-DATA-SW.
                                                                        00010000
KGT412     MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME.          00020001

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
               MOVE 'PDA008' TO WPCE-PROGRAM-ID
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

           PERFORM P00500-CHK-TRANS-INTENT THRU P00500-EXIT.

      *****************************************************************
      *    EITHER SEND INITIAL MENU SCREEN OR PERFORM SCREEN EDIT     *
      *    PROCESS                                                    *
      *****************************************************************

           IF INQUIRY-TRANS
               PERFORM P01000-FIRST-TIME THRU P01000-EXIT
           ELSE
               PERFORM P03000-EDIT-PROCESS THRU P03000-EXIT
           END-IF.

           MOVE WS-PDA008-WORKAREA TO PC-PROGRAM-WORKAREA.

       P00100-EXIT.
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

           EXEC CICS
               RETURN
                   TRANSID('PD08')
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
               MOVE 'PDA008' TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE TO WPCE-RESPONSE-CODE
               MOVE 'CICS RETURN TRANSID' TO WPCE-COMMAND
               MOVE 'P00200' TO WPCE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P00200-EXIT.
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
      *    IF PREVIOUS PROGRAM IS NOT PDA008, SET INQUIRY MODE        *
      *    OTHERWISE SET EDIT / UPDATE MODE                           *
      *****************************************************************

           IF PC-PREV-PGRMID = 'PDA008'
               MOVE 'U' TO WS-TRANS-INTENT-SW
           ELSE
               MOVE 'I' TO WS-TRANS-INTENT-SW
           END-IF.

       P00500-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01000-FIRST-TIME                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL PROCESSING TO SEND THE      *
      *                INITIAL SCREEN                                 *
      *                                                               *
      *    CALLED BY:  P00100-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P01000-FIRST-TIME.

      *****************************************************************
      *    INITIALIZE COMMAREA AND MAP                                *
      *****************************************************************

           MOVE LOW-VALUES TO PDA008I.
           MOVE WMF-DATE-MMDDYY TO PDADATEO.
           MOVE EIBTRMID TO PDATERMO.
           MOVE WMF-TIME-HHMMSS TO PDATIMEO.

      *****************************************************************
      *    FORMAT AND SEND THE FULL MAP -- LITERALS AND DATA          *
      *****************************************************************

           MOVE PC-USERID-NUMBER TO PENDING-ORDER-PREFIX.
           MOVE ZEROES TO PENDING-ORDER-SEQUENCE.
           MOVE SPACES TO WS-PDA008-WORKAREA.
           MOVE 0 TO WPW-PAGE-NUMBER
                     SCRN-PRICE(1)
                     SCRN-PRICE(2)
                     SCRN-PRICE(3)
                     SCRN-EXT-PRICE(1)
                     SCRN-EXT-PRICE(2)
                     SCRN-EXT-PRICE(3)
                     SCRN-HID-PRICE(1)
                     SCRN-HID-PRICE(2)
                     SCRN-HID-PRICE(3).
           MOVE 'N' TO WPW-MORE-SW
                       WS-END-OF-PROCESS-SW.

           PERFORM P05200-SCROLL-FORWARD THRU P05200-EXIT.

           EXEC SQL
               CALL PDASP1 (:PDASP1-PREFIX,
                            :PDASP1-TOTAL-COST,
                            :PDASP1-STATUS)
           END-EXEC.

           IF PDASP1-STATUS = '0000'
               MOVE PDASP1-TOTAL-COST TO TOTLCSTO
           ELSE
               MOVE 0 TO TOTLCSTO
           END-IF.

           PERFORM P79000-DISPLAY-SCREEN THRU P79000-EXIT.

      *****************************************************************
      *    SAVE PENDING ORDER SCREEN (PDA008) ORIGINATING PROGRAM,    *
      *    EITHER PDA002 (ORDER MENU) OR PDA007 (ITEM DETAIL)         *
      *****************************************************************

           IF PC-PREV-PGRMID           = 'PDA004' OR 'PDA007'
               IF PC-PREV-PGRMID       = 'PDA004'
                   MOVE 'PDA002'       TO PC-PDA008-ORIGINATING-PGRMID
               ELSE
                   MOVE PC-PREV-PGRMID TO PC-PDA008-ORIGINATING-PGRMID
           ELSE
               NEXT SENTENCE.

           MOVE 'PDA008' TO PC-PREV-PGRMID.

       P01000-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE PROGRAM EDIT PROCESS    *
      *                                                               *
      *    CALLED BY:  P00100-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P03000-EDIT-PROCESS.

           MOVE 'PDA008' TO PC-PREV-PGRMID.
           MOVE PC-PROGRAM-WORKAREA TO WS-PDA008-WORKAREA.

      *****************************************************************
      *    RECEIVE THE INPUT MAP                                      *
      *****************************************************************

           PERFORM P80200-RECEIVE-MAP THRU P80200-EXIT.

           MOVE WMF-DATE-MMDDYY TO PDADATEO.                            JLC419
           MOVE EIBTRMID TO PDATERMO.                                   UPDATE
           MOVE WMF-TIME-HHMMSS TO PDATIMEO.                            SCREEN
           MOVE SPACES TO PDAMSGO.

      *****************************************************************
      *    PERFORM THE SCREEN EDIT PROCESS (PFKEY AND DATA VALIDATION)*
      *****************************************************************

           PERFORM P03100-EDIT-SCREEN THRU P03100-EXIT.

           PERFORM P79000-DISPLAY-SCREEN THRU P79000-EXIT.

       P03000-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE SCREEN EDIT PROCESS     *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************

       P03100-EDIT-SCREEN.

           PERFORM P03110-INSPECT-SELECTIONS THRU P03110-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
                   UNTIL WS-SUB1 > WS-SUB-MAX.

KCS330     IF ERROR-FOUND
KCS330         GO TO P03100-EXIT
KCS330     END-IF.

      *****************************************************************
      *    EDIT THE OPERATOR PROGRAM FUNCTION KEY SELECTION (PFKEY)   *
      *****************************************************************

           PERFORM P03120-EDIT-PFKEY THRU P03120-EXIT.

           IF ERROR-FOUND
               GO TO P03100-EXIT
           END-IF.

           IF BACKWARD-PKEY OR
               FORWARD-PKEY
                   IF WPW-REFRESH-SW NOT = 'Y'
                       GO TO P03100-EXIT
                   END-IF
           END-IF.

           IF NO-SELECTION-MADE
               IF WPW-REFRESH-SW = 'Y'
                   MOVE PC-USERID-NUMBER TO PENDING-ORDER-PREFIX
                   MOVE SCRN-ORDER-SEQ(1) TO PENDING-ORDER-SEQUENCE
                   SUBTRACT 1 FROM WPW-PAGE-NUMBER
                   MOVE 'Y' TO WPW-MORE-SW
                   MOVE 'N' TO WPW-REFRESH-SW
                               WS-END-OF-PROCESS-SW
                   PERFORM P05200-SCROLL-FORWARD THRU P05200-EXIT
               END-IF
               IF WPW-REFRESH-SW = 'Y'
                   MOVE PC-USERID-NUMBER TO PENDING-ORDER-PREFIX
                   MOVE ZEROES TO PENDING-ORDER-SEQUENCE
                                  WPW-PAGE-NUMBER
                   MOVE 'Y' TO WPW-MORE-SW
                   MOVE 'N' TO WPW-REFRESH-SW
                               WS-END-OF-PROCESS-SW
                               WS-ERROR-FOUND-SW
                   PERFORM P05200-SCROLL-FORWARD THRU P05200-EXIT
               END-IF
               MOVE -1 TO ACTION1L
               MOVE PM025-MAKE-SELECTION TO PDAMSGO
               GO TO P03100-EXIT
           END-IF.

           IF NOT ENTER-PKEY
               GO TO P03100-EXIT
           END-IF.

      *****************************************************************
      *    EDIT THE OPERATOR ENTERED SELECTIONS                       *
      *****************************************************************

           PERFORM P03130-EDIT-SELECTIONS THRU P03130-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
                   UNTIL WS-SUB1 > WS-SUB-MAX.

           IF ERROR-FOUND
               GO TO P03100-EXIT
           END-IF.

      *****************************************************************
      *    PROCESS THE OPERATOR ENTERED SELECTIONS                    *
      *****************************************************************

           PERFORM P03140-PROCESS-SELECTION THRU P03140-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
                   UNTIL WS-SUB1 > WS-SUB-MAX.

           MOVE PM043-UPDATE-COMPLETE TO PDAMSGO.
           MOVE PENDING-ORDER-PREFIX TO PDASP1-PREFIX.

           IF PC-ACTIVE-SCENARIO(8) = 'Y'
               MOVE '0008' TO PDASP1-STATUS
           END-IF.

           EXEC SQL
               CALL PDASP1 (:PDASP1-PREFIX,
                            :PDASP1-TOTAL-COST,
                            :PDASP1-STATUS)
           END-EXEC.

           IF PDASP1-STATUS = '0000'
               MOVE PDASP1-TOTAL-COST TO TOTLCSTO
           ELSE
               MOVE 0 TO TOTLCSTO
               IF PC-ACTIVE-SCENARIO(8) = 'Y'
                   MOVE 'DB2' TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA008' TO WPDE-PROGRAM-ID
                   MOVE -303 TO WPDE-DB2-SQLCODE
                   MOVE 'CALL PDASP1' TO WPDE-FUNCTION
                   MOVE 'P03100' TO WPDE-PARAGRAPH

                   EXEC CICS
                        DUMP
                        TRANSACTION
                        DUMPCODE('DB2E')
                        COMPLETE
                   END-EXEC

                   EXEC CICS
                        ABEND
                        ABCODE('DB2E')
                        NODUMP
                   END-EXEC
               END-IF
           END-IF.

       P03100-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03110-INSPECT-SELECTIONS                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INSPECT ALL 3 SELECTION CODE FLDS   *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03110-INSPECT-SELECTIONS.

           IF SCRN-ACTION(WS-SUB1) = SPACES OR
               SCRN-ACTION(WS-SUB1) = LOW-VALUES OR
               SCRN-ACTION(WS-SUB1) = '_'
                   MOVE '_' TO SCRN-ACTION(WS-SUB1)
           ELSE
               MOVE 'Y' TO WS-SELECTION-SW
               IF WMF-SEL-SUB = +0
                   MOVE WS-SUB1 TO WMF-SEL-SUB
               END-IF
           END-IF.

       P03110-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03120-EDIT-PFKEY                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO VALIDATE PROGRAM FUNCTION KEY USAGE *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03120-EDIT-PFKEY.

      *****************************************************************
      *    VALID KEYS ARE:ENTER, PF3, PF4, PF7, PF8, PF10, PF11, PF12,*
      *                    CLEAR                                      *
      *****************************************************************

           MOVE EIBAID TO EIBAID-SW.

           IF NOT VALID-PKEY-ENTERED
               MOVE -1 TO ACTION1L
               MOVE PM001-INVALID-PFKEY TO WMF-MESSAGE-AREA
               PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT
               GO TO P03120-EXIT
           END-IF.

      *****************************************************************
      *    IF SELECTION ENTERED AND PFKEY HIT, DISPLAY ERROR MESSAGE  *
      *****************************************************************

           IF SELECTION-MADE
               IF NOT ENTER-PKEY
                   MOVE -1 TO ACT-LEN(WMF-SEL-SUB)
                   MOVE PM003-ACTION-VS-PFKEY-CONFLICT TO
                       WMF-MESSAGE-AREA
                   PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT
               END-IF
               GO TO P03120-EXIT
           END-IF.

      *****************************************************************
      *    PF3  FROM THIS SCREEN RETURNS USER TO PREVIOUS SCREEN,     *
      *    EITHER ORD MENU (PDA002) OR ITEM DETAIL (PDA007) DEPENDING *
      *    ON WHICH SCREEN ORIGINALLY INVOKED THIS PROGRAM            *
      *****************************************************************

           IF PREVIOUS-PKEY                                             JLC417
               MOVE PC-PDA008-ORIGINATING-PGRMID                        ADDED
                                       TO PC-NEXT-PGRMID                PF3
               MOVE SPACES             TO PC-PDA008-ORIGINATING-PGRMID  FUNCTION
               PERFORM P80300-XFER-CONTROL THRU P80300-EXIT             JLC417
           END-IF.                                                      JLC417

      *****************************************************************
      *    PF04 FROM THIS SCREEN TO PROCESS ORDER                     *
      *****************************************************************

           IF PROCESS-ORDER-PKEY
               PERFORM P03121-EDIT-PENDING-ORDER THRU P03121-EXIT
               IF ORDER-FOUND
                   MOVE 'PDA009' TO PC-NEXT-PGRMID
                   PERFORM P80300-XFER-CONTROL THRU P80300-EXIT
               ELSE
                   MOVE -1 TO ACTION1L
                   MOVE PM032-NO-PENDING-ORDER TO WMF-MESSAGE-AREA
                   PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT
                   GO TO P03120-EXIT
               END-IF
           END-IF.

      *****************************************************************
      *    PF10 FROM THIS SCREEN RETURNS USER TO BROWSE CATEGORIES    *
      *****************************************************************

           IF BROWSE-CATEGORIES-PKEY
               MOVE 'PDA005' TO PC-NEXT-PGRMID
               MOVE SPACES             TO PC-PDA008-ORIGINATING-PGRMID
               PERFORM P80300-XFER-CONTROL THRU P80300-EXIT
           END-IF.

      *****************************************************************
      *    PF11 FROM THIS SCREEN RETURNS USER TO THE ORDER MENU       *
      *****************************************************************

           IF ORDER-MENU-PKEY
               MOVE 'PDA002' TO PC-NEXT-PGRMID
               MOVE SPACES             TO PC-PDA008-ORIGINATING-PGRMID
               PERFORM P80300-XFER-CONTROL THRU P80300-EXIT
           END-IF.

      *****************************************************************
      *    PF12 FROM THIS SCREEN RETURNS USER TO THE MAIN MENU        *
      *****************************************************************

           IF MAIN-MENU-PKEY
               MOVE 'PDA001' TO PC-NEXT-PGRMID
               MOVE SPACES             TO PC-PDA008-ORIGINATING-PGRMID
               PERFORM P80300-XFER-CONTROL THRU P80300-EXIT
           END-IF.

      *****************************************************************
      *    ALLOW USER TO EXIT APPLICATION WITH CLEAR KEY              *
      *    (SEND MESSAGE, ERASE SCREEN)                               *
      *****************************************************************

           IF CLEAR-PKEY
               MOVE PM002-EXIT-APPLICATION TO WMF-MESSAGE-AREA
               PERFORM P80400-SEND-MESSAGE THRU P80400-EXIT
               GO TO P03120-EXIT
           END-IF.

      *****************************************************************
      *    CHECK IF SCREEN NEEDS TO BE REFRESHED AT THIS POINT, THIS  *
      *    OVERRIDES ANY SCROLLING THAT IS REQUESTED                  *
      *****************************************************************

           IF WPW-REFRESH-SW = 'Y'
               GO TO P03120-EXIT
           END-IF.

      *****************************************************************
      *    PF07 FROM THIS SCREEN SCROLLS BACKWARDS                    *
      *****************************************************************

           IF BACKWARD-PKEY
               MOVE PC-USERID-NUMBER TO PENDING-ORDER-PREFIX
               MOVE SCRN-ORDER-SEQ(1) TO PENDING-ORDER-SEQUENCE
               MOVE 'N' TO WS-END-OF-PROCESS-SW
               PERFORM P06200-SCROLL-BACKWARD THRU P06200-EXIT
           END-IF.

      *****************************************************************
      *    PF08 FROM THIS SCREEN SCROLLS FORWARDS                     *
      *****************************************************************

           IF FORWARD-PKEY
               MOVE PC-USERID-NUMBER TO PENDING-ORDER-PREFIX
               MOVE SCRN-ORDER-SEQ(3) TO PENDING-ORDER-SEQUENCE
               ADD 1 TO PENDING-ORDER-SEQUENCE
               MOVE 'N' TO WS-END-OF-PROCESS-SW
               PERFORM P05200-SCROLL-FORWARD THRU P05200-EXIT
           END-IF.

       P03120-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03121-EDIT-PENDING-ORDER                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO SEE IF A PENDING ORDER EXISTS.      *
      *                                                               *
      *    CALLED BY:  P03120-EDIT-PFKEY                              *
      *                                                               *
      *****************************************************************

       P03121-EDIT-PENDING-ORDER.

           MOVE 'CICS' TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA008' TO WPCE-PROGRAM-ID.
           MOVE 'CICS READ' TO WPCE-COMMAND.
           MOVE 'P03121' TO WPCE-PARAGRAPH.
           MOVE PC-USERID-NUMBER TO PENDING-ORDER-PREFIX.
           MOVE 1 TO PENDING-ORDER-SEQUENCE.

           EXEC CICS
               HANDLE CONDITION
                   NOTFND(P03121-NOTFND)
           END-EXEC.

           EXEC CICS
               READ
                   FILE('PDAPEND')
                   INTO(PENDING-ORDER-RECORD)
                   RIDFLD(PENDING-ORDER-KEY)
                   GTEQ
           END-EXEC.

           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               IF PC-USERID-NUMBER = PENDING-ORDER-PREFIX
                   MOVE 'Y' TO WS-ORDER-FOUND-SW
               END-IF
               GO TO P03121-EXIT
           ELSE
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.


       P03121-NOTFND.

           MOVE 'N' TO WS-ORDER-FOUND-SW.


       P03121-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03130-EDIT-SELECTIONS                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO EDIT ALL 3 SELECTION CODE FLDS      *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03130-EDIT-SELECTIONS.

           IF SCRN-ACTION(WS-SUB1) = '_' OR
               SCRN-ACTION(WS-SUB1) = 'D'
                   GO TO P03130-EXIT
           END-IF.

           IF SCRN-ACTION(WS-SUB1) NOT = 'C'
               MOVE -1 TO ACT-LEN(WS-SUB1)
               MOVE DFHDFHI TO ACT-ATTR(WS-SUB1)
               MOVE PM020-INVALID-ACTION-CODE TO WMF-MESSAGE-AREA
               PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT
               GO TO P03130-EXIT
           END-IF.

           INSPECT SCRN-QUANTITY(WS-SUB1)
               REPLACING ALL '_' BY ' '.

KCS411     INSPECT SCRN-QUANTITY(WS-SUB1)
ADD 0          REPLACING LEADING ' ' BY '0'.
FILL
           PERFORM VARYING WS-SUB2 FROM +1 BY +1
               UNTIL WS-SUB2 > +9
                   IF SCRN-QTY(WS-SUB1 WS-SUB2) > SPACES
                       MOVE WS-SUB2 TO WMF-QTY-LEN
                   END-IF
           END-PERFORM.

           EVALUATE TRUE
               WHEN WMF-QTY-LEN = +9
                   IF SCRN-QTY-9(WS-SUB1) NUMERIC
                       MOVE SCRN-QTY-9(WS-SUB1) TO WMF-QTY(WS-SUB1)
                   ELSE
                       MOVE -1 TO QTY-LEN(WS-SUB1)
                       MOVE DFHDFHI TO QTY-ATTR(WS-SUB1)
                       MOVE PM016-QUANTITY-INVALID TO WMF-MESSAGE-AREA
                       PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT
                       MOVE +0 TO WMF-QTY(WS-SUB1)
                   END-IF
               WHEN WMF-QTY-LEN = +8
                   IF SCRN-QTY-8(WS-SUB1) NUMERIC
                       MOVE SCRN-QTY-8(WS-SUB1) TO WMF-QTY(WS-SUB1)
                   ELSE
                       MOVE -1 TO QTY-LEN(WS-SUB1)
                       MOVE DFHDFHI TO QTY-ATTR(WS-SUB1)
                       MOVE PM016-QUANTITY-INVALID TO WMF-MESSAGE-AREA
                       PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT
                       MOVE +0 TO WMF-QTY(WS-SUB1)
                   END-IF
               WHEN WMF-QTY-LEN = +7
                   IF SCRN-QTY-7(WS-SUB1) NUMERIC
                       MOVE SCRN-QTY-7(WS-SUB1) TO WMF-QTY(WS-SUB1)
                   ELSE
                       MOVE -1 TO QTY-LEN(WS-SUB1)
                       MOVE DFHDFHI TO QTY-ATTR(WS-SUB1)
                       MOVE PM016-QUANTITY-INVALID TO WMF-MESSAGE-AREA
                       PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT
                       MOVE +0 TO WMF-QTY(WS-SUB1)
                   END-IF
               WHEN WMF-QTY-LEN = +6
                   IF SCRN-QTY-6(WS-SUB1) NUMERIC
                       MOVE SCRN-QTY-6(WS-SUB1) TO WMF-QTY(WS-SUB1)
                   ELSE
                       MOVE -1 TO QTY-LEN(WS-SUB1)
                       MOVE DFHDFHI TO QTY-ATTR(WS-SUB1)
                       MOVE PM016-QUANTITY-INVALID TO WMF-MESSAGE-AREA
                       PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT
                       MOVE +0 TO WMF-QTY(WS-SUB1)
                   END-IF
               WHEN WMF-QTY-LEN = +5
                   IF SCRN-QTY-5(WS-SUB1) NUMERIC
                       MOVE SCRN-QTY-5(WS-SUB1) TO WMF-QTY(WS-SUB1)
                   ELSE
                       MOVE -1 TO QTY-LEN(WS-SUB1)
                       MOVE DFHDFHI TO QTY-ATTR(WS-SUB1)
                       MOVE PM016-QUANTITY-INVALID TO WMF-MESSAGE-AREA
                       PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT
                       MOVE +0 TO WMF-QTY(WS-SUB1)
                   END-IF
               WHEN WMF-QTY-LEN = +4
                   IF SCRN-QTY-4(WS-SUB1) NUMERIC
                       MOVE SCRN-QTY-4(WS-SUB1) TO WMF-QTY(WS-SUB1)
                   ELSE
                       MOVE -1 TO QTY-LEN(WS-SUB1)
                       MOVE DFHDFHI TO QTY-ATTR(WS-SUB1)
                       MOVE PM016-QUANTITY-INVALID TO WMF-MESSAGE-AREA
                       PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT
                       MOVE +0 TO WMF-QTY(WS-SUB1)
                   END-IF
               WHEN WMF-QTY-LEN = +3
                   IF SCRN-QTY-3(WS-SUB1) NUMERIC
                       MOVE SCRN-QTY-3(WS-SUB1) TO WMF-QTY(WS-SUB1)
                   ELSE
                       MOVE -1 TO QTY-LEN(WS-SUB1)
                       MOVE DFHDFHI TO QTY-ATTR(WS-SUB1)
                       MOVE PM016-QUANTITY-INVALID TO WMF-MESSAGE-AREA
                       PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT
                       MOVE +0 TO WMF-QTY(WS-SUB1)
                   END-IF
               WHEN WMF-QTY-LEN = +2
                   IF SCRN-QTY-2(WS-SUB1) NUMERIC
                       MOVE SCRN-QTY-2(WS-SUB1) TO WMF-QTY(WS-SUB1)
                   ELSE
                       MOVE -1 TO QTY-LEN(WS-SUB1)
                       MOVE DFHDFHI TO QTY-ATTR(WS-SUB1)
                       MOVE PM016-QUANTITY-INVALID TO WMF-MESSAGE-AREA
                       PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT
                       MOVE +0 TO WMF-QTY(WS-SUB1)
                   END-IF
               WHEN WMF-QTY-LEN = +1
                   IF SCRN-QTY-1(WS-SUB1) NUMERIC
                       MOVE SCRN-QTY-1(WS-SUB1) TO WMF-QTY(WS-SUB1)
                   ELSE
                       MOVE -1 TO QTY-LEN(WS-SUB1)
                       MOVE DFHDFHI TO QTY-ATTR(WS-SUB1)
                       MOVE PM016-QUANTITY-INVALID TO WMF-MESSAGE-AREA
                       PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT
                       MOVE +0 TO WMF-QTY(WS-SUB1)
                   END-IF
               WHEN OTHER
                   MOVE -1 TO QTY-LEN(WS-SUB1)
                   MOVE DFHDFHI TO QTY-ATTR(WS-SUB1)
                   MOVE PM016-QUANTITY-INVALID TO WMF-MESSAGE-AREA
                   PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT
           END-EVALUATE.

           IF WMF-QTY(WS-SUB1) = +0
               MOVE -1 TO QTY-LEN(WS-SUB1)
               MOVE DFHDFHI TO QTY-ATTR(WS-SUB1)
               MOVE PM016-QUANTITY-INVALID TO WMF-MESSAGE-AREA
               PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT
           ELSE
               MOVE WMF-QTY(WS-SUB1) TO SCRN-QTY-9(WS-SUB1)
           END-IF.

       P03130-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03140-PROCESS-SELECTION                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS ITEM SELECTIONS             *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03140-PROCESS-SELECTION.

           IF SCRN-ACTION(WS-SUB1) NOT = 'C' AND
               SCRN-ACTION(WS-SUB1) NOT = 'D'
                   GO TO P03140-EXIT
           END-IF.

           MOVE PC-USERID-NUMBER TO PENDING-ORDER-PREFIX
           MOVE SCRN-ORDER-SEQ(WS-SUB1) TO PENDING-ORDER-SEQUENCE.

           PERFORM P03141-READ-PENDING-ORDER THRU P03141-EXIT.

           IF SCRN-ACTION(WS-SUB1) = 'C'
               MOVE WMF-QTY(WS-SUB1) TO PENDING-ORDER-QUANTITY
                                        SCRN-QUANTITY(WS-SUB1)
               COMPUTE WMF-EXT-PRICE = WMF-QTY(WS-SUB1) *
                                       SCRN-HID-PRICE(WS-SUB1)
               MOVE WMF-EXT-PRICE TO SCRN-EXT-PRICE(WS-SUB1)
               PERFORM P03142-REWRITE-PENDING-ORDER THRU P03142-EXIT
           ELSE
               MOVE 'Y' TO WPW-REFRESH-SW
               MOVE SPACES TO SCRN-ACTION(WS-SUB1)
                              SCRN-ITEM(WS-SUB1)
                              SCRN-ITEM-NAME(WS-SUB1)
                              SCRN-SUPPLIER-ID(WS-SUB1)
                              SCRN-STOCK(WS-SUB1)
               MOVE ZEROES TO SCRN-QUANTITY(WS-SUB1)
                              SCRN-PRICE(WS-SUB1)
                              SCRN-EXT-PRICE(WS-SUB1)
                              SCRN-HID-PRICE(WS-SUB1)
               MOVE 'ITEM DELETED' TO SCRN-ITEM(WS-SUB1)
               PERFORM P03143-DELETE-PENDING-ORDER THRU P03143-EXIT
           END-IF.

           MOVE '_' TO SCRN-ACTION(WS-SUB1).

       P03140-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03141-READ-PENDING-ORDER                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ THE PENDING ORDER FILE FOR     *
      *                UPDATE.                                        *
      *                                                               *
      *    CALLED BY:  P03140-PROCESS-SELECTION                       *
      *                                                               *
      *****************************************************************

       P03141-READ-PENDING-ORDER.

           MOVE 'CICS' TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA008' TO WPCE-PROGRAM-ID.
           MOVE 'CICS READ' TO WPCE-COMMAND.
           MOVE 'P03141' TO WPCE-PARAGRAPH.

           EXEC CICS
               READ
                   FILE('PDAPEND')
                   INTO(PENDING-ORDER-RECORD)
                   RIDFLD(PENDING-ORDER-KEY)
                   UPDATE
           END-EXEC.

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P03141-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03142-REWRITE-PENDING-ORDER                   *
      *                                                               *
      *    FUNCTION :  ROUTINE TO REWRITE THE PENDING ORDER FILE.     *
      *                                                               *
      *    CALLED BY:  P03140-PROCESS-SELECTION                       *
      *                                                               *
      *****************************************************************

       P03142-REWRITE-PENDING-ORDER.

           MOVE 'CICS' TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA008' TO WPCE-PROGRAM-ID.
           MOVE 'CICS REWRITE' TO WPCE-COMMAND.
           MOVE 'P03142' TO WPCE-PARAGRAPH.

           EXEC CICS
               REWRITE
                   FILE('PDAPEND')
                   FROM(PENDING-ORDER-RECORD)
           END-EXEC.

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P03142-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03143-DELETE-PENDING-ORDER                    *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DELETE THE PENDING ORDER FILE.      *
      *                                                               *
      *    CALLED BY:  P03140-PROCESS-SELECTION                       *
      *                                                               *
      *****************************************************************

       P03143-DELETE-PENDING-ORDER.

           MOVE 'CICS' TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA008' TO WPCE-PROGRAM-ID.
           MOVE 'CICS DELETE' TO WPCE-COMMAND.
           MOVE 'P03143' TO WPCE-PARAGRAPH.

           EXEC CICS
               DELETE
                   FILE('PDAPEND')
           END-EXEC.

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P03143-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05200-SCROLL-FORWARD                          *
      *                                                               *
      *    FUNCTION :  PERFORMS STARTBR AND FORMATS ITEM LINES        *
      *                                                               *
      *    CALLED BY:  P00100-FIRST-TIME                              *
      *                P03120-EDIT-PFKEY                              *
      *                                                               *
      *****************************************************************

       P05200-SCROLL-FORWARD.

      *****************************************************************
      * IF FIRST-TIME-PROCESSING, KEYS NEED NOT BE CHECKED            *
      *****************************************************************

           IF NOT INQUIRY-TRANS
               IF WPW-MORE-SW = 'N'
                   MOVE PM013-BOTTOM-MSG TO PDAMSGO
                   GO TO P05200-EXIT
               ELSE
                   PERFORM P78000-CLEAR-SCREEN THRU P78000-EXIT
                       VARYING WS-SUB1 FROM 1 BY 1
                           UNTIL WS-SUB1 > WS-SUB-MAX
               END-IF
           END-IF.

           PERFORM P05210-STARTBR-PENDING-ORDER THRU P05210-EXIT.

           IF ERROR-FOUND
               GO TO P05200-EXIT
           END-IF.

           ADD 1 TO WPW-PAGE-NUMBER.

           MOVE 'N' TO WPW-MORE-SW.

           EXEC CICS
               HANDLE CONDITION
                   ENDFILE(P05200-ENDFILE)
           END-EXEC.

           PERFORM P05220-FORMAT-FORWARD-LINE THRU P05220-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
                   UNTIL END-OF-PROCESS.

           EXEC CICS
               HANDLE CONDITION
                   ENDFILE(P90030-ENDFILE)
           END-EXEC.

           PERFORM P05230-ENDBR-PENDING-ORDER THRU P05230-EXIT.

           GO TO P05200-EXIT.


       P05200-ENDFILE.

           MOVE 'Y' TO WS-END-OF-PROCESS-SW
                       WS-BOTTOM-OF-DATA-SW.
           MOVE PM013-BOTTOM-MSG TO PDAMSGO.

           PERFORM P05230-ENDBR-PENDING-ORDER THRU P05230-EXIT.

       P05200-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05210-STARTBR-PENDING-ORDER                   *
      *                                                               *
      *    FUNCTION :  PERFORMS A START BROWSE ON THE PENDING ORDER   *
      *                FILE.                                          *
      *                                                               *
      *    CALLED BY:  P05200-SCROLL-FORWARD                          *
      *                                                               *
      *****************************************************************

       P05210-STARTBR-PENDING-ORDER.

           MOVE 'CICS' TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA008' TO WPCE-PROGRAM-ID.
           MOVE 'CICS STARTBR' TO WPCE-COMMAND.
           MOVE 'P05210' TO WPCE-PARAGRAPH.

           EXEC CICS
               HANDLE CONDITION
                   ENDFILE(P05210-ERROR)
                   NOTFND(P05210-ERROR)
           END-EXEC.

           EXEC CICS
               STARTBR
                   FILE('PDAPEND')
                   RIDFLD(PENDING-ORDER-KEY)
                   GTEQ
           END-EXEC.

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

           GO TO P05210-EXIT.


       P05210-ERROR.

           MOVE 'Y' TO WPW-REFRESH-SW
                       WS-ERROR-FOUND-SW.


       P05210-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05220-FORMAT-FORWARD-LINE                     *
      *                                                               *
      *    FUNCTION :  FORMATS LINE OF LIST                           *
      *                                                               *
      *    CALLED BY:  P05200-SCROLL-FORWARD                          *
      *                                                               *
      *****************************************************************

       P05220-FORMAT-FORWARD-LINE.

           PERFORM P05221-READNEXT-PENDING-ORDER THRU P05221-EXIT.

           IF PENDING-ORDER-PREFIX NOT = PC-USERID-NUMBER
               IF WS-SUB1 = 1
                   MOVE 'Y' TO WPW-REFRESH-SW
               ELSE
                   MOVE 'Y' TO WS-END-OF-PROCESS-SW
                               WS-BOTTOM-OF-DATA-SW
                   MOVE PM013-BOTTOM-MSG TO PDAMSGO
               END-IF
               GO TO P05220-EXIT
           END-IF.

           IF PENDING-ORDER-SEQUENCE = 0
               SUBTRACT 1 FROM WS-SUB1
               GO TO P05220-EXIT
           END-IF.

           IF WS-SUB1 > WS-SUB-MAX
               MOVE 'Y' TO WS-END-OF-PROCESS-SW
                           WPW-MORE-SW
               GO TO P05220-EXIT
           END-IF.

           MOVE '_' TO SCRN-ACTION(WS-SUB1).
           MOVE PENDING-ORDER-PREFIX TO ITEM-PREFIX
                                        ITEM-SUPPLIER-ITEM-PREFIX
                                        PDASP1-PREFIX.
           MOVE PENDING-ORDER-QUANTITY TO SCRN-QUANTITY(WS-SUB1).
           MOVE PENDING-ORDER-ITEM-NUMBER TO SCRN-ITEM(WS-SUB1)
                                             ITEM-NUMBER
                                             ITEM-SUPPLIER-ITEM-NUMBER.

           EXEC SQL
               SELECT  NAME
               INTO    :ITEM-NAME
               FROM    ITEM
               WHERE   PREFIX             = :ITEM-PREFIX AND
                       NUMBER             = :ITEM-NUMBER
           END-EXEC.

           IF SQLCODE NOT = +0
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA008' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'SELECT ITEM' TO WPDE-FUNCTION
               MOVE 'P05220' TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

           MOVE ITEM-NAME TO SCRN-ITEM-NAME(WS-SUB1).
           MOVE PENDING-ORDER-SUPPLIER-ID TO SCRN-SUPPLIER-ID(WS-SUB1)
                                             ITEM-SUPPLIER-SUPPLIER-ID.
           MOVE PENDING-ORDER-SEQUENCE TO SCRN-ORDER-SEQ(WS-SUB1).

           EXEC SQL
               SELECT  UNIT_PRICE
               INTO    :ITEM-SUPPLIER-UNIT-PRICE
               FROM    ITEM_SUPPLIER
               WHERE   ITEM_PREFIX    = :ITEM-SUPPLIER-ITEM-PREFIX AND
                       ITEM_NUMBER    = :ITEM-SUPPLIER-ITEM-NUMBER AND
                       SUPPLIER_ID    = :ITEM-SUPPLIER-SUPPLIER-ID
           END-EXEC.

           IF SQLCODE NOT = +0
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA008' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'SELECT ITEM-SUPPLIER' TO WPDE-FUNCTION
               MOVE 'P05220' TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.
           MOVE ITEM-SUPPLIER-UNIT-PRICE TO SCRN-PRICE(WS-SUB1)
                                            SCRN-HID-PRICE(WS-SUB1).

           COMPUTE WMF-EXT-PRICE = ITEM-SUPPLIER-UNIT-PRICE *
                                   PENDING-ORDER-QUANTITY.

           MOVE WMF-EXT-PRICE TO SCRN-EXT-PRICE(WS-SUB1).
           MOVE 'IN STOCK' TO SCRN-STOCK(WS-SUB1).

       P05220-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05221-READNEXT-PENDING-ORDER                  *
      *                                                               *
      *    FUNCTION :  READ NEXT RECORD FROM THE PENDING ORDER FILE.  *
      *                                                               *
      *    CALLED BY:  P05220-FORMAT-FORWARD-LINE                     *
      *                                                               *
      *****************************************************************

       P05221-READNEXT-PENDING-ORDER.

           MOVE 'CICS' TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA008' TO WPCE-PROGRAM-ID.
           MOVE 'CICS READNEXT' TO WPCE-COMMAND.
           MOVE 'P05221' TO WPCE-PARAGRAPH.

           EXEC CICS
               READNEXT
                   FILE('PDAPEND')
                   INTO(PENDING-ORDER-RECORD)
                   RIDFLD(PENDING-ORDER-KEY)
           END-EXEC.

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P05221-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05230-ENDBR-PENDING-ORDER                     *
      *                                                               *
      *    FUNCTION :  PERFORMS END BROWSE ON THE PENDING ORDER FILE. *
      *                                                               *
      *    CALLED BY:  P05200-SCROLL-FORWARD                          *
      *                                                               *
      *****************************************************************

       P05230-ENDBR-PENDING-ORDER.

           MOVE 'CICS' TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA008' TO WPCE-PROGRAM-ID.
           MOVE 'CICS ENDBR' TO WPCE-COMMAND.
           MOVE 'P05230' TO WPCE-PARAGRAPH.

           EXEC CICS
               ENDBR
                   FILE('PDAPEND')
           END-EXEC.

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P05230-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06200-SCROLL-BACKWARD                         *
      *                                                               *
      *    FUNCTION :  PERFORMS FETCH AND FORMATS ITEM LINES          *
      *                                                               *
      *    CALLED BY:  P00100-FIRST-TIME                              *
      *                                                               *
      *****************************************************************

       P06200-SCROLL-BACKWARD.

           IF WPW-PAGE-NUMBER = 1
               MOVE PM014-TOP-MSG TO PDAMSGO
               GO TO P06200-EXIT
           END-IF.

           MOVE 'Y' TO WPW-MORE-SW.

           COMPUTE WPW-PAGE-NUMBER = WPW-PAGE-NUMBER - 1.

           PERFORM P06210-STARTBR-PENDING-ORDER THRU P06210-EXIT.

           IF ERROR-FOUND
               GO TO P06200-EXIT
           END-IF.

           EXEC CICS
               HANDLE CONDITION
                   ENDFILE(P06200-ERROR)
                   NOTFND(P06200-ERROR)
           END-EXEC.

           PERFORM P06220-FORMAT-BACKWARD-LINE THRU P06220-EXIT
               VARYING WS-SUB1 FROM 3 BY -1
                   UNTIL END-OF-PROCESS.

           EXEC CICS
               HANDLE CONDITION
                   ENDFILE(P90030-ENDFILE)
                   NOTFND(P90120-NOTFND)
           END-EXEC.

           PERFORM P06230-ENDBR-PENDING-ORDER THRU P06230-EXIT.

           GO TO P06200-EXIT.


       P06200-ERROR.

           PERFORM P06230-ENDBR-PENDING-ORDER THRU P06230-EXIT.

           MOVE 0 TO WPW-PAGE-NUMBER.
           MOVE '00000' TO SCRN-ORDER-SEQ(3).
           MOVE '8' TO EIBAID-SW.
           MOVE 'Y' TO WPW-MORE-SW.


       P06200-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06220-STARTBR-PENDING-ORDER                   *
      *                                                               *
      *    FUNCTION :  PERFORMS START BROWSE ON THE PENDING ORDER     *
      *                FILE AND THE FIRST READ PREVIOUS TO REVERSE    *
      *                THE READ DIRECTION.                            *
      *                                                               *
      *    CALLED BY:  P06200-SCROLL-BACKWARD                         *
      *                                                               *
      *****************************************************************

       P06210-STARTBR-PENDING-ORDER.

           MOVE 'CICS' TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA008' TO WPCE-PROGRAM-ID.
           MOVE 'CICS STARTBR' TO WPCE-COMMAND.
           MOVE 'P06210' TO WPCE-PARAGRAPH.

           EXEC CICS
               STARTBR
                   FILE('PDAPEND')
                   RIDFLD(PENDING-ORDER-KEY)
                   GTEQ
           END-EXEC.

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

           MOVE 'CICS' TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA008' TO WPCE-PROGRAM-ID.
           MOVE 'CICS READNEXT' TO WPCE-COMMAND.
           MOVE 'P06210' TO WPCE-PARAGRAPH.

           EXEC CICS
               READNEXT
                   FILE('PDAPEND')
                   INTO(PENDING-ORDER-RECORD)
                   RIDFLD(PENDING-ORDER-KEY)
           END-EXEC.

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

           MOVE 'CICS' TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA008' TO WPCE-PROGRAM-ID.
           MOVE 'CICS READPREV' TO WPCE-COMMAND.
           MOVE 'P06210' TO WPCE-PARAGRAPH.

           EXEC CICS
               READPREV
                   FILE('PDAPEND')
                   INTO(PENDING-ORDER-RECORD)
                   RIDFLD(PENDING-ORDER-KEY)
           END-EXEC.

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P06210-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06220-FORMAT-BACKWARD-LINE                    *
      *                                                               *
      *    FUNCTION :  FORMATS LINE OF LIST                           *
      *                                                               *
      *    CALLED BY:  P06200-SCROLL-BACKWARDWARD                     *
      *                                                               *
      *****************************************************************

       P06220-FORMAT-BACKWARD-LINE.

           IF WS-SUB1 < 1
               MOVE 'Y' TO WS-END-OF-PROCESS-SW
               GO TO P06220-EXIT
           END-IF.

           PERFORM P06221-READPREV-PENDING-ORDER THRU P06221-EXIT.

           IF PENDING-ORDER-PREFIX NOT = PC-USERID-NUMBER OR
               PENDING-ORDER-SEQUENCE = 0
                   MOVE 0 TO WPW-PAGE-NUMBER
                   MOVE '00000' TO SCRN-ORDER-SEQ(3)
                   MOVE '8' TO EIBAID-SW
                   MOVE 'Y' TO WPW-MORE-SW
                               WS-END-OF-PROCESS-SW
           END-IF.

           IF ERROR-FOUND OR
               END-OF-PROCESS
                   GO TO P06220-EXIT
           END-IF.

           MOVE '_' TO SCRN-ACTION(WS-SUB1).
           MOVE PENDING-ORDER-PREFIX TO ITEM-PREFIX
                                        ITEM-SUPPLIER-ITEM-PREFIX
                                        PDASP1-PREFIX.
           MOVE PENDING-ORDER-QUANTITY TO SCRN-QUANTITY(WS-SUB1).
           MOVE PENDING-ORDER-ITEM-NUMBER TO SCRN-ITEM(WS-SUB1)
                                             ITEM-NUMBER
                                             ITEM-SUPPLIER-ITEM-NUMBER.

           EXEC SQL
               SELECT  NAME
               INTO    :ITEM-NAME
               FROM    ITEM
               WHERE   PREFIX             = :ITEM-PREFIX AND
                       NUMBER             = :ITEM-NUMBER
           END-EXEC.

           IF SQLCODE NOT = +0
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA008' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'SELECT ITEM' TO WPDE-FUNCTION
               MOVE 'P06220' TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

           MOVE ITEM-NAME TO SCRN-ITEM-NAME(WS-SUB1).
           MOVE PENDING-ORDER-SUPPLIER-ID TO SCRN-SUPPLIER-ID(WS-SUB1)
                                             ITEM-SUPPLIER-SUPPLIER-ID.
           MOVE PENDING-ORDER-SEQUENCE TO SCRN-ORDER-SEQ(WS-SUB1).

           EXEC SQL
               SELECT  UNIT_PRICE
               INTO    :ITEM-SUPPLIER-UNIT-PRICE
               FROM    ITEM_SUPPLIER
               WHERE   ITEM_PREFIX    = :ITEM-SUPPLIER-ITEM-PREFIX AND
                       ITEM_NUMBER    = :ITEM-SUPPLIER-ITEM-NUMBER AND
                       SUPPLIER_ID    = :ITEM-SUPPLIER-SUPPLIER-ID
           END-EXEC.

           IF SQLCODE NOT = +0
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA008' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'SELECT ITEM-SUPPLIER' TO WPDE-FUNCTION
               MOVE 'P06220' TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

           MOVE ITEM-SUPPLIER-UNIT-PRICE TO SCRN-PRICE(WS-SUB1)
                                            SCRN-HID-PRICE(WS-SUB1).

           COMPUTE WMF-EXT-PRICE = ITEM-SUPPLIER-UNIT-PRICE *
                                   PENDING-ORDER-QUANTITY.

           MOVE WMF-EXT-PRICE TO SCRN-EXT-PRICE(WS-SUB1).
           MOVE 'IN STOCK' TO SCRN-STOCK(WS-SUB1).

       P06220-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06221-READPREV-PENDING-ORDER                  *
      *                                                               *
      *    FUNCTION :  READ PREVIOUS RECORD FROM THE PENDING ORDER    *
      *                FILE.                                          *
      *                                                               *
      *    CALLED BY:  P06220-FORMAT-BACKWARD-LINE                    *
      *                                                               *
      *****************************************************************

       P06221-READPREV-PENDING-ORDER.

           MOVE 'CICS' TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA008' TO WPCE-PROGRAM-ID.
           MOVE 'CICS READPREV' TO WPCE-COMMAND.
           MOVE 'P06221' TO WPCE-PARAGRAPH.

           EXEC CICS
               READPREV
                   FILE('PDAPEND')
                   INTO(PENDING-ORDER-RECORD)
                   RIDFLD(PENDING-ORDER-KEY)
           END-EXEC.

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P06221-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06230-ENDBR-PENDING-ORDER                     *
      *                                                               *
      *    FUNCTION :  PERFORMS END BROWSE ON THE PENDING ORDER FILE. *
      *                                                               *
      *    CALLED BY:  P06200-SCROLL-BACKWARD                         *
      *                                                               *
      *****************************************************************

       P06230-ENDBR-PENDING-ORDER.

           MOVE 'CICS' TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA008' TO WPCE-PROGRAM-ID.
           MOVE 'CICS ENDBR' TO WPCE-COMMAND.
           MOVE 'P06230' TO WPCE-PARAGRAPH.

           EXEC CICS
               ENDBR
                   FILE('PDAPEND')
           END-EXEC.

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P06230-EXIT.
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

           MOVE 'Y' TO WS-ERROR-FOUND-SW.

           IF PDAMSGO NOT > SPACES
               MOVE WMF-MESSAGE-AREA TO PDAMSGO
           END-IF.

       P70000-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P78000-CLEAR-SCREEN                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CLEAR DETAIL LINES BEFORE POPULATING*
      *                AGAIN                                          *
      *                                                               *
      *    CALLED BY:  P05200-SCROLL-FORWARD                          *
      *                                                               *
      *                                                               *
      *****************************************************************

       P78000-CLEAR-SCREEN.

      *****************************************************************
      *    MAKE FINAL ADJUSTMENTS TO ATTRIBUTES AND FIELDS            *
      *****************************************************************

           MOVE '_' TO SCRN-ACTION(WS-SUB1).
           MOVE SPACES TO SCRN-ITEM-NAME(WS-SUB1)
                          SCRN-ITEM(WS-SUB1)
                          SCRN-SUPPLIER-ID(WS-SUB1)
                          SCRN-ORDER-SEQ(WS-SUB1)
                          SCRN-STOCK(WS-SUB1).
           MOVE ZEROES TO SCRN-QUANTITY(WS-SUB1)
                          SCRN-PRICE(WS-SUB1)
                          SCRN-EXT-PRICE(WS-SUB1)
                          SCRN-HID-PRICE(WS-SUB1).

       P78000-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79000-DISPLAY-SCREEN                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE SCREEN DISPLAY          *
      *                PROCESSING                                     *
      *                                                               *
      *    CALLED BY:  P01000-1ST-TIME-PROCESS                        *
      *                P02000-PROCESS-TRANS                           *
      *                                                               *
      *****************************************************************

       P79000-DISPLAY-SCREEN.

      *****************************************************************
      *    SET CURSOR AND SAVE FIRST ITEM NUMBER IN COMM AREA         *
      *****************************************************************

JKD312     IF NO-ERROR-FOUND
JKD312         MOVE -1 TO ACTION1L
JKD312     END-IF.

      *****************************************************************
      *    MAKE FINAL ADJUSTMENTS TO ATTRIBUTES AND FIELDS            *
      *****************************************************************

           PERFORM P79100-SET-MAP-FIELDS THRU P79100-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
                   UNTIL WS-SUB1 > WS-SUB-MAX.

      *****************************************************************
      *    SEND FULL MAP IF 1ST TIME, OTHERWISE SEND DATAONLY         *
      *****************************************************************

           IF PC-PREV-PGRMID = 'PDA008'
               PERFORM P80100-SEND-MAP-DATAONLY THRU P80100-EXIT
           ELSE
               PERFORM P80000-SEND-FULL-MAP THRU P80000-EXIT
           END-IF.

       P79000-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79100-SET-MAP-FIELDS                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO SET CERTAIN MAP FIELDS AND          *
      *                ATTRIBUTES BASED ON DATA PRESENT IN THE MAP    *
      *                                                               *
      *    CALLED BY:  P79000-DISPLAY-SCREEN                          *
      *                                                               *
      *****************************************************************

       P79100-SET-MAP-FIELDS.

      *****************************************************************
      *    IF ITEM PRESENT, MAKE SELECTION CODE ENTERABLE AND         *
      *    INITIALIZE TO UNDERSCORE IF SELECTION CODE NOT PRESENT,    *
      *    OTHERWISE PROTECT SELECTION CODE AND SET VALUE TO SPACES   *
      *                                                               *
      *    SAVE LAST ITEM IN COMM AREA FOR NEXT TIME                  *
      *****************************************************************

           IF SCRN-SUPPLIER-ID(WS-SUB1) > SPACES
               IF NO-ERROR-FOUND
                   MOVE -1 TO ACT-LEN(WS-SUB1)
               END-IF
               MOVE DFHBMFSE TO ACT-ATTR(WS-SUB1)
                                QTY-ATTR(WS-SUB1)
               INSPECT SCRN-ACTION(WS-SUB1)
                   CONVERTING WMF-SPACES-LOWVALUE-R TO '__'
           ELSE
               MOVE 0 TO ACT-LEN(WS-SUB1)
               MOVE SPACES TO SCRN-ACTION(WS-SUB1)
               MOVE DFHBMASF TO ACT-ATTR(WS-SUB1)
                                QTY-ATTR(WS-SUB1)
               MOVE ZEROES TO SCRN-QUANTITY(WS-SUB1)
           END-IF.

       P79100-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80000-SEND-FULL-MAP                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DISPLAY THE INITIAL MAIN MENU       *
      *                                                               *
      *    CALLED BY:  P01000-FIRST-TIME                              *
      *                                                               *
      *****************************************************************

       P80000-SEND-FULL-MAP.

           EXEC CICS
               SEND
                   MAP('PDA008')
                   MAPSET('PDA008M')
                   FROM(PDA008O)
                   ERASE
                   FREEKB
                   CURSOR
                   NOHANDLE
                   RESP(WS-RESPONSE-CODE)
           END-EXEC.

      *****************************************************************
      *    IF ERROR, FORMAT ERROR INFORMATION AND TERMINATE           *
      *****************************************************************

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               MOVE 'CICS' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA008' TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE TO WPCE-RESPONSE-CODE
               MOVE 'CICS SEND MAP' TO WPCE-COMMAND
               MOVE 'P80000' TO WPCE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P80000-EXIT.
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
      *    RESET ENTERABLE FIELDS BACK TO DEFAULT (IF NECESSARY)      *
      *****************************************************************

      *    INSPECT PDACUSTI
      *        CONVERTING  WMF-SPACES-LOWVALUE-R TO '__'.

      *****************************************************************
      *    SEND THE MAP DATA ONLY, DO NOT ERASE SCREEN                *
      *****************************************************************

           EXEC CICS
               SEND
                   MAP('PDA008')
                   MAPSET('PDA008M')
                   FROM(PDA008O)
                   DATAONLY
                   FREEKB
                   CURSOR
                   NOHANDLE
                   RESP(WS-RESPONSE-CODE)
           END-EXEC.

      *****************************************************************
      *    IF ERROR, FORMAT ERROR INFORMATION AND TERMINATE           *
      *****************************************************************

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               MOVE 'CICS' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA008' TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE TO WPCE-RESPONSE-CODE
               MOVE 'CICS SEND MAP' TO WPCE-COMMAND
               MOVE 'P80100' TO WPCE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P80100-EXIT.
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

           EXEC CICS
               RECEIVE
                   MAP('PDA008')
                   MAPSET('PDA008M')
                   INTO(PDA008I)
                   NOHANDLE
                   RESP(WS-RESPONSE-CODE)
           END-EXEC.


      *****************************************************************
      *    IF ERROR, FORMAT ERROR INFORMATION AND TERMINATE           *
      *****************************************************************

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL) AND
               WS-RESPONSE-CODE NOT = DFHRESP(MAPFAIL)
                   MOVE 'CICS' TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA008' TO WPCE-PROGRAM-ID
                   MOVE WS-RESPONSE-CODE TO WPCE-RESPONSE-CODE
                   MOVE 'CICS RECEIVE MAP' TO WPCE-COMMAND
                   MOVE 'P80200' TO WPCE-PARAGRAPH
                   PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P80200-EXIT.
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
               MOVE 'PDA008' TO WPCE-PROGRAM-ID
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
               MOVE 'PDA008' TO WPCE-PROGRAM-ID
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
               MOVE 'PDA008' TO WPCE-PROGRAM-ID
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
           MOVE 'PDA008' TO WPCE-PROGRAM-ID.
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

      *****************************************************************
      * RETURN CONTROL TO CICS                                        *
      *****************************************************************

           EXEC CICS
               RETURN
           END-EXEC.

           GOBACK.

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
AS OF      MOVE 'PDA008' TO WPCE-PROGRAM-ID.                             AS OF
JAN        MOVE EIBRESP TO WPCE-RESPONSE-CODE.                           JAN
2001       MOVE 'ERROR' TO WPCE-COMMAND.                                 2001
           MOVE 'P99999' TO WPCE-PARAGRAPH.
LLR                                                                      LLR
           PERFORM P99500-PDA-ERROR THRU P99500-EXIT.

       P99999-ERROR-EXIT.
           EXIT.
           EJECT