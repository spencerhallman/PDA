       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA007.
      *
      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA007                                            *
      * TRANS   :   PD07                                              *
      * MAPSET  :   PDA007M                                           *
      *                                                               *
      * FUNCTION:   PROGRAM PDA007 IS THE ITEM DETAIL SCREEN WHICH    *
      *             CONTAINS A SCROLLABLE LIST OF ALL SUPPLIERS OF    *
      *             THE SELECTED ITEM.  THE USER MAY SELECT ONE OR    *
      *             MORE SUPPLIERS BY ENTERING AN ORDER QUANTITY FOR  *
      *             THE DESIRED SUPPLIER.  A PENDING ORDER LINE ITEM  *
      *             IS ADDED TO THE PENDING ORDER FILE FOR EACH ITEM  *
      *             SELECTED.                                         *
      *                                                               *
      * FILES   :   ITEM               -  DB2        (READ-ONLY)      *
      *             ITEM_SUPPLIER      -  DB2        (READ-ONLY)      *
      *             SUPPLIER           -  DB2        (READ-ONLY)      *
      *             PENDING_ORDER_FILE -  VSAM KSDS  (UPDATE)         *
      *                                                               *
      *                                                               *
      * TRANSACTIONS GENERATED:                                       *
      *             PD06       BROWSE ITEMS BY CATEGORY               *
      *             PD08       PENDING ORDER                          *
      *             PD02       ORDER MENU                             *
      *             PD01       MAIN MENU                              *
      *                                                               *
      *                                                               *
      * PFKEYS  :   PF03  =    EXIT, RETURN TO PDA006, BROWSE ITEMS   *
      *             PF07  =    SCROLL BACKWARD                        *
      *             PF08  =    SCROLL FORWARD                         *
      *             PF10  =    PROCEED TO PDA008, PENDING ORDERS      *
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
       77  WS-SUB-MAX                  PIC S9(4) COMP VALUE +5.
       77  WS-SUB-MAX-PLUS-ONE         PIC S9(4) COMP VALUE +6.
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
           05  EIBAID-SW               PIC X     VALUE ' '.
               88  CLEAR-PKEY                    VALUE '_'.
               88  ENTER-PKEY                    VALUE ''''.
               88  PREVIOUS-PKEY                 VALUE '3'.
               88  BACKWARD-PKEY                 VALUE '7'.
               88  FORWARD-PKEY                  VALUE '8'.
               88  PENDING-ORDER-PKEY            VALUE ':'.
               88  ORDER-MENU-PKEY               VALUE '#'.
               88  MAIN-MENU-PKEY                VALUE '@'.
               88  VALID-PKEY-ENTERED            VALUE '_' '@' '3' '7'
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
           05  WS-QUANTITIES.
               07  FILLER              PIC 9(9)  VALUE ZEROES.
               07  FILLER              PIC 9(9)  VALUE ZEROES.
               07  FILLER              PIC 9(9)  VALUE ZEROES.
               07  FILLER              PIC 9(9)  VALUE ZEROES.
               07  FILLER              PIC 9(9)  VALUE ZEROES.
           05  FILLER                  REDEFINES WS-QUANTITIES.
               07  WS-QTY              OCCURS 5 TIMES
                                       PIC 9(9).
           05  WS-PDA007-WORKAREA.
               07  WPW-FIRST-SUPPLIER  PIC X(32) VALUE SPACES.
               07  WPW-LAST-SUPPLIER   PIC X(32) VALUE SPACES.
               07  WPW-ROW-COUNT       PIC S9(9) VALUE +0       COMP.
               07  WPW-PAGE-NUMBER     PIC 9(5)  VALUE ZEROES.
               07  WPW-MORE-SW         PIC X     VALUE SPACES.
               07  WPW-SUPPLIER        OCCURS 5 TIMES
                                       PIC X(32).

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
      *         MAP DSECTS -- BROWSE ITEMS BY CATEGORY - PDA007M      *
      *****************************************************************

           COPY PDA007M.
       01  FILLER                      REDEFINES PDA007O.
           03  FILLER                  PIC X(179).
           03  P7-SCREEN-AREA          OCCURS 5 TIMES.
               05  QTY-LEN             PIC S9(4)                COMP.
               05  QTY-ATTR            PIC X.
               05  FILLER              PIC XX.
               05  SCRN-QUANTITY       PIC X(9).
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
               05  SUPPLIER-LEN        PIC S9(4)                COMP.
               05  SUPPLIER-ATTR       PIC X.
               05  FILLER              PIC XX.
               05  SCRN-SUPPLIER       PIC X(32).
               05  PRICE-LEN           PIC S9(4)                COMP.
               05  PRICE-ATTR          PIC X.
               05  FILLER              PIC XX.
               05  SCRN-PRICE          PIC ZZ,ZZZ,ZZZ.ZZ.
               05  SUPPLIER-NAME-LEN   PIC S9(4)                COMP.
               05  SUPPLIER-NAME-ATTR  PIC X.
               05  FILLER              PIC XX.
               05  SCRN-SUPPLIER-NAME  PIC X(50).
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
           EXEC SQL
              INCLUDE DSUPPLR
           END-EXEC.
           EJECT
           EXEC SQL
               DECLARE ITEMFORW CURSOR FOR
                   SELECT  NUMBER,
                           CATEGORY_NAME,
                           SUB_CATEGORY_NAME,
                           ITEM.NAME,
                           LENGTH,
                           DIAMETER,
                           ITEM_SUPPLIER.SUPPLIER_ID,
                           UNIT_PRICE,
                           SUPPLIER.NAME
                   FROM    ITEM,
                           ITEM_SUPPLIER,
                           SUPPLIER
                   WHERE   (ITEM.PREFIX       = :WMF-ITEM-PREFIX AND
                           ITEM_PREFIX        = :WMF-ITEM-PREFIX AND
                           SUPPLIER.PREFIX    = :WMF-ITEM-PREFIX)
                           AND
                           (ITEM.NUMBER       = :WMF-ITEM-NUMBER AND
                           ITEM_NUMBER        = :WMF-ITEM-NUMBER)
                           AND
                           ITEM_SUPPLIER.SUPPLIER_ID
                                              = SUPPLIER.SUPPLIER_ID
                           AND
                           ITEM_SUPPLIER.SUPPLIER_ID
                                            > :WPW-LAST-SUPPLIER
                   ORDER BY SUPPLIER_ID
           END-EXEC.

           EXEC SQL
               DECLARE ITEMBACK CURSOR FOR
                   SELECT  NUMBER,
                           CATEGORY_NAME,
                           SUB_CATEGORY_NAME,
                           ITEM.NAME,
                           LENGTH,
                           DIAMETER,
                           ITEM_SUPPLIER.SUPPLIER_ID,
                           UNIT_PRICE,
                           SUPPLIER.NAME
                   FROM    ITEM,
                           ITEM_SUPPLIER,
                           SUPPLIER
                   WHERE   (ITEM.PREFIX       = :WMF-ITEM-PREFIX AND
                           ITEM_PREFIX        = :WMF-ITEM-PREFIX AND
                           SUPPLIER.PREFIX    = :WMF-ITEM-PREFIX)
                           AND
                           (ITEM.NUMBER       = :WMF-ITEM-NUMBER AND
                           ITEM_NUMBER        = :WMF-ITEM-NUMBER)
                           AND
                           ITEM_SUPPLIER.SUPPLIER_ID
                                              = SUPPLIER.SUPPLIER_ID
                           AND
                           ITEM_SUPPLIER.SUPPLIER_ID
                                              < :WPW-FIRST-SUPPLIER
                   ORDER BY SUPPLIER_ID DESC
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
      *    P R O G R A M     W O R K A R E A                          *
      *****************************************************************

      *01  WS-PDA007-WORKAREA.
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
                   MOVE 'PDA007' TO WPCE-PROGRAM-ID
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
           MOVE 'N' TO WS-TOP-OF-DATA-SW.
           MOVE 'N' TO WS-BOTTOM-OF-DATA-SW.
                                                                        00010000
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
               MOVE 'PDA007' TO WPCE-PROGRAM-ID
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

           MOVE WS-PDA007-WORKAREA TO PC-PROGRAM-WORKAREA.

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
                   TRANSID('PD07')
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
               MOVE 'PDA007' TO WPCE-PROGRAM-ID
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
      *    IF PREVIOUS PROGRAM IS NOT PDA007, SET INQUIRY MODE        *
      *    OTHERWISE SET EDIT / UPDATE MODE                           *
      *****************************************************************

           IF PC-PREV-PGRMID = 'PDA007'
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

           MOVE LOW-VALUES TO PDA007I.
           MOVE WMF-DATE-MMDDYY TO PDADATEO.
           MOVE EIBTRMID TO PDATERMO.
           MOVE WMF-TIME-HHMMSS TO PDATIMEO.

      *****************************************************************
      *    FORMAT AND SEND THE FULL MAP -- LITERALS AND DATA          *
      *****************************************************************

           MOVE PC-USERID-NUMBER TO WMF-ITEM-PREFIX.
           MOVE PC-SELECTED-ITEM TO WMF-ITEM-NUMBER
                                    ITEMNOO.
           MOVE 0 TO LENGTHO
                     DIAMTRO.
           MOVE SPACES TO WS-PDA007-WORKAREA.
           MOVE 0 TO WPW-PAGE-NUMBER.
           MOVE 'N' TO WPW-MORE-SW
                       WS-END-OF-PROCESS-SW.

           EXEC SQL
               SELECT  COUNT(*)
               INTO    :WPW-ROW-COUNT
               FROM    ITEM,
                       ITEM_SUPPLIER,
                       SUPPLIER
               WHERE   (ITEM.PREFIX       = :WMF-ITEM-PREFIX AND
                       ITEM_PREFIX        = :WMF-ITEM-PREFIX AND
                       SUPPLIER.PREFIX    = :WMF-ITEM-PREFIX)
                       AND
                       (ITEM.NUMBER       = :WMF-ITEM-NUMBER AND
                       ITEM_NUMBER        = :WMF-ITEM-NUMBER)
                       AND
                       ITEM_SUPPLIER.SUPPLIER_ID
                                          = SUPPLIER.SUPPLIER_ID
                       AND
                       ITEM_SUPPLIER.SUPPLIER_ID
                                        > :WPW-LAST-SUPPLIER
           END-EXEC.

           IF SQLCODE NOT = +0
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA007' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'SELECT COUNT' TO WPDE-FUNCTION
               MOVE 'P01000' TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

           PERFORM P05200-SCROLL-FORWARD THRU P05200-EXIT.

           PERFORM P79000-DISPLAY-SCREEN THRU P79000-EXIT.

           MOVE 'PDA007' TO PC-PREV-PGRMID.

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

           MOVE 'PDA007' TO PC-PREV-PGRMID.
           MOVE PC-PROGRAM-WORKAREA TO WS-PDA007-WORKAREA.

      *****************************************************************
      *    RECEIVE THE INPUT MAP                                      *
      *****************************************************************

           PERFORM P80200-RECEIVE-MAP THRU P80200-EXIT.

           MOVE WMF-DATE-MMDDYY TO PDADATEO.
           MOVE EIBTRMID TO PDATERMO.
           MOVE WMF-TIME-HHMMSS TO PDATIMEO.
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

      *****************************************************************
      *    EDIT THE OPERATOR PROGRAM FUNCTION KEY SELECTION (PFKEY)   *
      *****************************************************************

           PERFORM P03120-EDIT-PFKEY THRU P03120-EXIT.

           IF ERROR-FOUND OR
               NOT ENTER-PKEY
                   GO TO P03100-EXIT
           END-IF.

           IF NO-SELECTION-MADE
               MOVE -1 TO QTY1L
               MOVE PM017-ENTER-QUANTITY TO PDAMSGO
               GO TO P03100-EXIT
           END-IF.

      *****************************************************************
      *    EDIT THE OPERATOR ENTERED DATA                             *
      *****************************************************************

           PERFORM P03130-EDIT-QUANTITY THRU P03130-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
                   UNTIL WS-SUB1 > WS-SUB-MAX.

           IF ERROR-FOUND
               MOVE PM016-QUANTITY-INVALID TO PDAMSGO
               GO TO P03100-EXIT
           END-IF.


           PERFORM P03150-PROCESS-SELECTION THRU P03150-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
                   UNTIL WS-SUB1 > WS-SUB-MAX.


           MOVE PM018-ADDED-TO-ORDER TO PDAMSGO.

       P03100-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03110-INSPECT-SELECTIONS                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INSPECT ALL 5 QUANTITY FIELDS       *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03110-INSPECT-SELECTIONS.

           INSPECT SCRN-QUANTITY(WS-SUB1)
               CONVERTING WMF-SPACES-LOWVALUE-R TO '__'.

           IF SCRN-QUANTITY(WS-SUB1) NOT = '_________'
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
      *    VALID KEYS ARE: ENTER, PF3, PF7, PF8, PF10, PF11, PF12,    *
      *                    CLEAR                                      *
      *****************************************************************

           MOVE EIBAID TO EIBAID-SW.

           IF NOT VALID-PKEY-ENTERED
               MOVE -1 TO QTY1L
               MOVE PM001-INVALID-PFKEY TO WMF-MESSAGE-AREA
               PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT
               GO TO P03120-EXIT
           END-IF.

      *****************************************************************
      *    IF SELECTION ENTERED AND PFKEY HIT, DISPLAY ERROR MESSAGE  *
      *****************************************************************

           IF SELECTION-MADE
               IF NOT ENTER-PKEY
                   MOVE -1 TO QTY-LEN(WMF-SEL-SUB)
                   MOVE PM045-QTY-VS-PFKEY-CONFLICT TO WMF-MESSAGE-AREA
                   PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT
                   GO TO P03120-EXIT
               END-IF
           END-IF.

      *****************************************************************
      *    PF03 FROM THIS SCREEN RETURNS USER TO BROWSE CAT SCREEN    *
      *****************************************************************

           IF PREVIOUS-PKEY
               MOVE 'PDA006' TO PC-NEXT-PGRMID
               MOVE SPACES TO PC-SELECTED-ITEM
               PERFORM P80300-XFER-CONTROL THRU P80300-EXIT
           END-IF.

      *****************************************************************
      *    PF11 FROM THIS SCREEN RETURNS USER TO THE ORDER MENU       *
      *****************************************************************

           IF ORDER-MENU-PKEY
               MOVE 'PDA002' TO PC-NEXT-PGRMID
               PERFORM P80300-XFER-CONTROL THRU P80300-EXIT
           END-IF.

      *****************************************************************
      *    PF12 FROM THIS SCREEN RETURNS USER TO THE MAIN MENU        *
      *****************************************************************

           IF MAIN-MENU-PKEY
               MOVE 'PDA001' TO PC-NEXT-PGRMID
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
      *    PF07 FROM THIS SCREEN SCROLLS BACKWARDS                    *
      *****************************************************************

           IF BACKWARD-PKEY
               MOVE PC-USERID-NUMBER TO WMF-ITEM-PREFIX
               MOVE PC-SELECTED-ITEM TO WMF-ITEM-NUMBER
               MOVE 'N' TO WS-END-OF-PROCESS-SW
               PERFORM P06200-SCROLL-BACKWARD THRU P06200-EXIT
           END-IF.

      *****************************************************************
      *    PF08 FROM THIS SCREEN SCROLLS FORWARDS                     *
      *****************************************************************

           IF FORWARD-PKEY
               MOVE PC-USERID-NUMBER TO WMF-ITEM-PREFIX
               MOVE PC-SELECTED-ITEM TO WMF-ITEM-NUMBER
               MOVE 'N' TO WS-END-OF-PROCESS-SW
               PERFORM P05200-SCROLL-FORWARD THRU P05200-EXIT
           END-IF.

      *****************************************************************
      *    PF10 FROM THIS SCREEN SENDS USER TO PENDING ORDER SCREEN   *
      *****************************************************************

           IF PENDING-ORDER-PKEY
               PERFORM P03121-PENDING-ORDER THRU P03121-EXIT
           END-IF.

       P03120-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03121-PENDING-ORDER                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DETERMINE IF A PENDING ORDER DOES   *
      *                EXIST.  IF SO, PASS CONTROL TO PROGRAM PDA008. *
      *                                                               *
      *    CALLED BY:  P03120-EDIT-PFKEY                              *
      *                                                               *
      *****************************************************************

       P03121-PENDING-ORDER.

           EXEC CICS
               HANDLE CONDITION
                   ENDFILE(P03121-ERROR)
                   NOTFND(P03121-ERROR)
                   ERROR(P03121-ERROR)
           END-EXEC.

           MOVE PC-USERID-NUMBER TO PENDING-ORDER-PREFIX.
           MOVE 1 TO PENDING-ORDER-SEQUENCE.

           EXEC CICS
               READ
                   FILE('PDAPEND')
                   INTO(PENDING-ORDER-RECORD)
                   RIDFLD(PENDING-ORDER-KEY)
                   GTEQ
           END-EXEC.

           IF PC-USERID-NUMBER = PENDING-ORDER-PREFIX
               MOVE 'PDA008' TO PC-NEXT-PGRMID
               PERFORM P80300-XFER-CONTROL THRU P80300-EXIT
           END-IF.


       P03121-ERROR.

           MOVE -1 TO QTY1L.
           MOVE PM032-NO-PENDING-ORDER TO WMF-MESSAGE-AREA.

           PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT.


       P03121-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03130-EDIT-QUANTITY                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO EDIT ALL 5 QUANTITY FIELDS          *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03130-EDIT-QUANTITY.

           IF SCRN-QUANTITY(WS-SUB1) = '_________'
               GO TO P03130-EXIT
           ELSE
               INSPECT SCRN-QUANTITY(WS-SUB1)
                   REPLACING ALL '_' BY ' '
               INSPECT SCRN-QUANTITY(WS-SUB1)
                   REPLACING LEADING ' ' BY '0'
           END-IF.

           PERFORM VARYING WS-SUB2 FROM +1 BY +1
               UNTIL WS-SUB2 > +9
                   IF SCRN-QTY(WS-SUB1 WS-SUB2) > SPACES
                       MOVE WS-SUB2 TO WMF-QTY-LEN
                   END-IF
           END-PERFORM.

           EVALUATE TRUE
               WHEN WMF-QTY-LEN = 9
                   IF SCRN-QTY-9(WS-SUB1) NUMERIC
                       MOVE SCRN-QTY-9(WS-SUB1) TO WS-QTY(WS-SUB1)
                   ELSE
                       MOVE 'Y' TO WS-ERROR-FOUND-SW
                       MOVE -1 TO QTY-LEN(WS-SUB1)
                       MOVE DFHDFHI TO QTY-ATTR(WS-SUB1)
                   END-IF
               WHEN WMF-QTY-LEN = 8
                   IF SCRN-QTY-8(WS-SUB1) NUMERIC
                       MOVE SCRN-QTY-8(WS-SUB1) TO WS-QTY(WS-SUB1)
                   ELSE
                       MOVE 'Y' TO WS-ERROR-FOUND-SW
                       MOVE -1 TO QTY-LEN(WS-SUB1)
                       MOVE DFHDFHI TO QTY-ATTR(WS-SUB1)
                   END-IF
               WHEN WMF-QTY-LEN = 7
                   IF SCRN-QTY-7(WS-SUB1) NUMERIC
                       MOVE SCRN-QTY-7(WS-SUB1) TO WS-QTY(WS-SUB1)
                   ELSE
                       MOVE 'Y' TO WS-ERROR-FOUND-SW
                       MOVE -1 TO QTY-LEN(WS-SUB1)
                       MOVE DFHDFHI TO QTY-ATTR(WS-SUB1)
                   END-IF
               WHEN WMF-QTY-LEN = 6
                   IF SCRN-QTY-6(WS-SUB1) NUMERIC
                       MOVE SCRN-QTY-6(WS-SUB1) TO WS-QTY(WS-SUB1)
                   ELSE
                       MOVE 'Y' TO WS-ERROR-FOUND-SW
                       MOVE -1 TO QTY-LEN(WS-SUB1)
                       MOVE DFHDFHI TO QTY-ATTR(WS-SUB1)
                   END-IF
               WHEN WMF-QTY-LEN = 5
                   IF SCRN-QTY-5(WS-SUB1) NUMERIC
                       MOVE SCRN-QTY-5(WS-SUB1) TO WS-QTY(WS-SUB1)
                   ELSE
                       MOVE 'Y' TO WS-ERROR-FOUND-SW
                       MOVE -1 TO QTY-LEN(WS-SUB1)
                       MOVE DFHDFHI TO QTY-ATTR(WS-SUB1)
                   END-IF
               WHEN WMF-QTY-LEN = 4
                   IF SCRN-QTY-4(WS-SUB1) NUMERIC
                       MOVE SCRN-QTY-4(WS-SUB1) TO WS-QTY(WS-SUB1)
                   ELSE
                       MOVE 'Y' TO WS-ERROR-FOUND-SW
                       MOVE -1 TO QTY-LEN(WS-SUB1)
                       MOVE DFHDFHI TO QTY-ATTR(WS-SUB1)
                   END-IF
               WHEN WMF-QTY-LEN = 3
                   IF SCRN-QTY-3(WS-SUB1) NUMERIC
                       MOVE SCRN-QTY-3(WS-SUB1) TO WS-QTY(WS-SUB1)
                   ELSE
                       MOVE 'Y' TO WS-ERROR-FOUND-SW
                       MOVE -1 TO QTY-LEN(WS-SUB1)
                       MOVE DFHDFHI TO QTY-ATTR(WS-SUB1)
                   END-IF
               WHEN WMF-QTY-LEN = 2
                   IF SCRN-QTY-2(WS-SUB1) NUMERIC
                       MOVE SCRN-QTY-2(WS-SUB1) TO WS-QTY(WS-SUB1)
                   ELSE
                       MOVE 'Y' TO WS-ERROR-FOUND-SW
                       MOVE -1 TO QTY-LEN(WS-SUB1)
                       MOVE DFHDFHI TO QTY-ATTR(WS-SUB1)
                   END-IF
               WHEN WMF-QTY-LEN = 1
                   IF SCRN-QTY-1(WS-SUB1) NUMERIC
                       MOVE SCRN-QTY-1(WS-SUB1) TO WS-QTY(WS-SUB1)
                   ELSE
                       MOVE 'Y' TO WS-ERROR-FOUND-SW
                       MOVE -1 TO QTY-LEN(WS-SUB1)
                       MOVE DFHDFHI TO QTY-ATTR(WS-SUB1)
                   END-IF
               WHEN OTHER
                   MOVE 'Y' TO WS-ERROR-FOUND-SW
                   MOVE -1 TO QTY-LEN(WS-SUB1)
                   MOVE DFHDFHI TO QTY-ATTR(WS-SUB1)
           END-EVALUATE.

           IF WS-QTY(WS-SUB1) = +0
               MOVE 'Y' TO WS-ERROR-FOUND-SW
               MOVE -1 TO QTY-LEN(WS-SUB1)
               MOVE DFHDFHI TO QTY-ATTR(WS-SUB1)
           ELSE
               MOVE WS-QTY(WS-SUB1) TO SCRN-QTY-9(WS-SUB1)
           END-IF.

       P03130-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03140-READ-ZERO-RECORD                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ THE ZERO RECORD FROM THE       *
      *                PENDING ORDER FILE.                            *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03140-READ-ZERO-RECORD.

           MOVE 'CICS' TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA007' TO WPCE-PROGRAM-ID.
           MOVE 'CICS READ' TO WPCE-COMMAND.
           MOVE 'P03140' TO WPCE-PARAGRAPH.
           MOVE PC-USERID-NUMBER TO PENDING-ORDER-PREFIX.
           MOVE 0 TO PENDING-ORDER-SEQUENCE.

           EXEC CICS
               HANDLE CONDITION
                   NOTFND(P03140-NOTFND)
           END-EXEC.

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

           MOVE 'Y' TO WS-ZERO-RECORD-SW.
           MOVE PENDING-ORDER-QUANTITY TO WMF-ITEM-SEQ.

           EXEC CICS
               HANDLE CONDITION
                   NOTFND(P90120-NOTFND)
           END-EXEC.

           GO TO P03140-EXIT.


       P03140-NOTFND.

           MOVE 'N' TO WS-ZERO-RECORD-SW.
           MOVE 0 TO WMF-ITEM-SEQ.

       P03140-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03150-PROCESS-SELECTION                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS ITEM SELECTIONS             *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03150-PROCESS-SELECTION.

           IF WS-QTY(WS-SUB1) = +0
               GO TO P03150-EXIT
           END-IF.


      *****************************************************************
      *    GET NEXT AVAILABLE ITEM SEQUENCE FROM CONTROL RECORD,      *
      *    EITHER CREATE OR UPDATE CONTROL RECORD                     *
      *****************************************************************

           PERFORM P03140-READ-ZERO-RECORD THRU P03140-EXIT.

           IF NOT ZERO-RECORD-FOUND
               PERFORM P03170-CREATE-ZERO-RECORD THRU P03170-EXIT
               PERFORM P03140-READ-ZERO-RECORD THRU P03140-EXIT
           END-IF.

           ADD 1 TO WMF-ITEM-SEQ.

           PERFORM P03160-UPDATE-ZERO-RECORD THRU P03160-EXIT.


      *****************************************************************
      *    ADD THE PENDING ORDER ITEM                                 *
      *****************************************************************

           MOVE SPACES TO PENDING-ORDER-RECORD.
           MOVE PC-USERID-NUMBER TO PENDING-ORDER-PREFIX
                                    PENDING-ORDER-ITEM-PREFIX
                                    PENDING-ORDER-SUPPLIER-PREFIX.

           MOVE WMF-ITEM-SEQ TO PENDING-ORDER-SEQUENCE.
           MOVE WS-QTY(WS-SUB1) TO PENDING-ORDER-QUANTITY.
           MOVE PC-SELECTED-ITEM TO PENDING-ORDER-ITEM-NUMBER.
           MOVE WPW-SUPPLIER(WS-SUB1) TO PENDING-ORDER-SUPPLIER-ID.
           MOVE '_________' TO SCRN-QUANTITY(WS-SUB1).

           IF PC-ACTIVE-SCENARIO(4) = 'Y'
               EXEC CICS
                   PUSH HANDLE
               END-EXEC
               EXEC CICS
                   WRITE
                       FILE('PDAPEND1')
                       FROM(PENDING-ORDER-RECORD)
                       RIDFLD(PENDING-ORDER-KEY)
               END-EXEC
           END-IF.

           PERFORM P03151-WRITE-PENDING-ORDER THRU P03151-EXIT.

       P03150-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03151-WRITE-PENDING-ORDER                     *
      *                                                               *
      *    FUNCTION :  ROUTINE TO WRITE SELECTED ITEMS TO THE PENDING *
      *                ORDER FILE.                                    *
      *                                                               *
      *    CALLED BY:  P03150-PROCESS-SELECTION                       *
      *                                                               *
      *****************************************************************

       P03151-WRITE-PENDING-ORDER.

           MOVE 'CICS' TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA007' TO WPCE-PROGRAM-ID.
           MOVE 'CICS WRITE' TO WPCE-COMMAND.
           MOVE 'P03151' TO WPCE-PARAGRAPH.

           EXEC CICS
               WRITE
                   FILE('PDAPEND')
                   FROM(PENDING-ORDER-RECORD)
                   RIDFLD(PENDING-ORDER-KEY)
           END-EXEC.

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P03151-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03160-UPDATE-ZERO-RECORD                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO UPDATE THE ZERO RECORD ON THE       *
      *                PENDING ORDER FILE.                            *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03160-UPDATE-ZERO-RECORD.

           MOVE 'CICS' TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA007' TO WPCE-PROGRAM-ID.
           MOVE 'P03160' TO WPCE-PARAGRAPH.
           MOVE 'CICS REWRITE' TO WPCE-COMMAND.

           MOVE WMF-ITEM-SEQ TO PENDING-ORDER-QUANTITY.

           EXEC CICS
               REWRITE
                   FILE('PDAPEND')
                   FROM(PENDING-ORDER-RECORD)
           END-EXEC.

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P03160-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03170-CREATE-ZERO-RECORD                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CREATE THE ZERO RECORD ON THE       *
      *                PENDING ORDER FILE.                            *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03170-CREATE-ZERO-RECORD.

           MOVE 'CICS' TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA007' TO WPCE-PROGRAM-ID.
           MOVE 'CICS WRITE' TO WPCE-COMMAND.
           MOVE 'P03170' TO WPCE-PARAGRAPH.
           MOVE SPACES TO PENDING-ORDER-RECORD.
           MOVE PC-USERID-NUMBER TO PENDING-ORDER-PREFIX.
           MOVE 0 TO PENDING-ORDER-SEQUENCE.
           MOVE ZEROES       TO PENDING-ORDER-QUANTITY.

           EXEC CICS
               WRITE
                   FILE('PDAPEND')
                   FROM(PENDING-ORDER-RECORD)
                   RIDFLD(PENDING-ORDER-KEY)
           END-EXEC.

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P03170-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05200-SCROLL-FORWARD                          *
      *                                                               *
      *    FUNCTION :  PERFORMS FETCH AND FORMATS ITEM LINES          *
      *                                                               *
      *    CALLED BY:  P00100-FIRST-TIME                              *
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

           PERFORM P05210-OPEN-FORWARD-CURSOR THRU P05210-EXIT.

           IF ERROR-FOUND
               GO TO P05200-EXIT
           END-IF.

           ADD 1 TO WPW-PAGE-NUMBER.

           MOVE 'N' TO WPW-MORE-SW.
           MOVE SPACES TO WPW-SUPPLIER(1)
                          WPW-SUPPLIER(2)
                          WPW-SUPPLIER(3)
                          WPW-SUPPLIER(4)
                          WPW-SUPPLIER(5).

           PERFORM P05220-FORMAT-FORWARD-LINE THRU P05220-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
                   UNTIL END-OF-PROCESS.

           PERFORM P05230-CLOSE-FORWARD-CURSOR THRU P05230-EXIT.

       P05200-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05210-OPEN-FORWARD-CURSOR                     *
      *                                                               *
      *    FUNCTION :  OPENS CURSOR USED TO CREATE ITEM LIST          *
      *                                                               *
      *    CALLED BY:  P05200-SCROLL-FORWARD                          *
      *                                                               *
      *****************************************************************

       P05210-OPEN-FORWARD-CURSOR.

           EXEC SQL
               OPEN ITEMFORW
           END-EXEC.

           IF SQLCODE NOT = +0
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA007' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'OPEN FORWARD CURSOR' TO WPDE-FUNCTION
               MOVE 'P05210' TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

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

           PERFORM P05221-FETCH-FORWARD-ROW THRU P05221-EXIT.

           IF ERROR-FOUND OR
               END-OF-PROCESS
                   GO TO P05220-EXIT
           END-IF.

           IF WS-SUB1 > WS-SUB-MAX
               MOVE 'Y' TO WS-END-OF-PROCESS-SW
                           WPW-MORE-SW
               GO TO P05220-EXIT
           END-IF.

           IF WS-SUB1 = 1
               MOVE ITEM-SUPPLIER-SUPPLIER-ID TO WPW-FIRST-SUPPLIER
               MOVE ITEM-NUMBER TO ITEMNOO
               MOVE ITEM-NAME TO ITEMNMO
               MOVE ITEM-LENGTH TO LENGTHO
               MOVE ITEM-DIAMETER TO DIAMTRO
           END-IF.

           MOVE ALL '_' TO SCRN-QUANTITY(WS-SUB1).
           MOVE ITEM-SUPPLIER-SUPPLIER-ID TO WPW-LAST-SUPPLIER
                                             WPW-SUPPLIER(WS-SUB1)
                                             SCRN-SUPPLIER(WS-SUB1).
           MOVE ITEM-SUPPLIER-UNIT-PRICE TO SCRN-PRICE(WS-SUB1).
           MOVE SUPPLIER-NAME TO SCRN-SUPPLIER-NAME(WS-SUB1).

       P05220-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05221-FETCH-FORWARD-ROW                       *
      *                                                               *
      *    FUNCTION :  FETCH ROW FROM FORWARD CURSOR                  *
      *                                                               *
      *    CALLED BY:  P05220-FORMAT-FORWARD-LINE                     *
      *                                                               *
      *****************************************************************

       P05221-FETCH-FORWARD-ROW.

           EXEC SQL
               FETCH  ITEMFORW
               INTO   :ITEM-NUMBER,
                      :ITEM-CATEGORY-NAME,
                      :ITEM-SUB-CATEGORY-NAME,
                      :ITEM-NAME,
                      :ITEM-LENGTH,
                      :ITEM-DIAMETER,
                      :ITEM-SUPPLIER-SUPPLIER-ID,
                      :ITEM-SUPPLIER-UNIT-PRICE,
                      :SUPPLIER-NAME
           END-EXEC.

           IF SQLCODE NOT = +0
               IF SQLCODE = +100
                   MOVE 'Y' TO WS-END-OF-PROCESS-SW
                   IF WS-SUB1 <  WS-SUB-MAX-PLUS-ONE
                       MOVE 'Y' TO WS-BOTTOM-OF-DATA-SW
                       MOVE PM013-BOTTOM-MSG TO PDAMSGO
                   END-IF
               ELSE
                   MOVE 'DB2' TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA007' TO WPDE-PROGRAM-ID
                   MOVE SQLCODE TO WPDE-DB2-SQLCODE
                   MOVE 'FETCH FORWARD CURSOR' TO WPDE-FUNCTION
                   MOVE 'P05221' TO WPDE-PARAGRAPH
                   PERFORM P99500-PDA-ERROR THRU P99500-EXIT
               END-IF
           END-IF.

       P05221-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05230-CLOSE-FORWARD-CURSOR                    *
      *                                                               *
      *    FUNCTION :  CLOSES CURSOR USED TO CREATE ITEM LIST         *
      *                                                               *
      *    CALLED BY:  P05200-SCROLL-FORWARD                          *
      *                                                               *
      *****************************************************************

       P05230-CLOSE-FORWARD-CURSOR.

           EXEC SQL
               CLOSE ITEMFORW
           END-EXEC.

           IF SQLCODE NOT = +0
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA007' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'CLOSE FORWARD CURSOR' TO WPDE-FUNCTION
               MOVE 'P05230' TO WPDE-PARAGRAPH
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
               IF WPW-ROW-COUNT > +5
                   MOVE 'Y' TO WPW-MORE-SW
               ELSE
                   MOVE 'N' TO WPW-MORE-SW
               END-IF
               MOVE PM014-TOP-MSG TO PDAMSGO
               GO TO P06200-EXIT
           END-IF.

           MOVE 'Y' TO WPW-MORE-SW.

           COMPUTE WPW-PAGE-NUMBER = WPW-PAGE-NUMBER - 1.

           PERFORM P06210-OPEN-BACKWARD-CURSOR THRU P06210-EXIT.

           IF ERROR-FOUND
               GO TO P06200-EXIT
           END-IF.

           PERFORM P06220-FORMAT-BACKWARD-LINE THRU P06220-EXIT
               VARYING WS-SUB1 FROM 5 BY -1
                   UNTIL END-OF-PROCESS.

           PERFORM P06230-CLOSE-BACKWARD-CURSOR THRU P06230-EXIT.

       P06200-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06210-OPEN-BACKWARD-CURSOR                    *
      *                                                               *
      *    FUNCTION :  OPENS CURSOR USED TO CREATE ITEM LIST          *
      *                                                               *
      *    CALLED BY:  P06200-SCROLL-BACKWARD                         *
      *                                                               *
      *****************************************************************

       P06210-OPEN-BACKWARD-CURSOR.

           EXEC SQL
               OPEN ITEMBACK
           END-EXEC.

           IF SQLCODE NOT = +0
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA007' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'OPEN BACKWARD CURSOR' TO WPDE-FUNCTION
               MOVE 'P06210' TO WPDE-PARAGRAPH
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

           PERFORM P06221-FETCH-BACKWARD-ROW THRU P06221-EXIT.

           IF ERROR-FOUND OR
               END-OF-PROCESS
                   GO TO P06220-EXIT
           END-IF.

           IF WS-SUB1 = 5
               MOVE ITEM-SUPPLIER-SUPPLIER-ID TO WPW-LAST-SUPPLIER
               MOVE ITEM-NUMBER TO ITEMNOO
               MOVE ITEM-NAME TO ITEMNMO
               MOVE ITEM-LENGTH TO LENGTHO
               MOVE ITEM-DIAMETER TO DIAMTRO
           END-IF.

           MOVE ALL '_' TO SCRN-QUANTITY(WS-SUB1).
           MOVE ITEM-SUPPLIER-SUPPLIER-ID TO WPW-FIRST-SUPPLIER
                                             WPW-SUPPLIER(WS-SUB1)
                                             SCRN-SUPPLIER(WS-SUB1).
           MOVE ITEM-SUPPLIER-UNIT-PRICE TO SCRN-PRICE(WS-SUB1).
           MOVE SUPPLIER-NAME TO SCRN-SUPPLIER-NAME(WS-SUB1).

       P06220-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06221-FETCH-BACKWARD-ROW                      *
      *                                                               *
      *    FUNCTION :  FETCH ROW FROM BACKWARD CURSOR                 *
      *                                                               *
      *    CALLED BY:  P06220-FORMAT-BACKWARD-LINE                    *
      *                                                               *
      *****************************************************************

       P06221-FETCH-BACKWARD-ROW.

           EXEC SQL
               FETCH  ITEMBACK
               INTO   :ITEM-NUMBER,
                      :ITEM-CATEGORY-NAME,
                      :ITEM-SUB-CATEGORY-NAME,
                      :ITEM-NAME,
                      :ITEM-LENGTH,
                      :ITEM-DIAMETER,
                      :ITEM-SUPPLIER-SUPPLIER-ID,
                      :ITEM-SUPPLIER-UNIT-PRICE,
                      :SUPPLIER-NAME
           END-EXEC.

           IF SQLCODE NOT = +0
               IF SQLCODE = +100
                   MOVE 'Y' TO WS-END-OF-PROCESS-SW
                   IF WS-SUB1 <  WS-SUB-MAX-PLUS-ONE
                       MOVE 'Y' TO WS-BOTTOM-OF-DATA-SW
                       MOVE PM013-BOTTOM-MSG TO PDAMSGO
                   END-IF
               ELSE
                   MOVE 'DB2' TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA007' TO WPDE-PROGRAM-ID
                   MOVE SQLCODE TO WPDE-DB2-SQLCODE
                   MOVE 'FETCH BACKWARD CURSOR' TO WPDE-FUNCTION
                   MOVE 'P06221' TO WPDE-PARAGRAPH
                   PERFORM P99500-PDA-ERROR THRU P99500-EXIT
               END-IF
           END-IF.

       P06221-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06230-CLOSE-BACKWARD-CURSOR                   *
      *                                                               *
      *    FUNCTION :  CLOSES CURSOR USED TO CREATE ITEM LIST         *
      *                                                               *
      *    CALLED BY:  P06200-SCROLL-BACKWARD                         *
      *                                                               *
      *****************************************************************

       P06230-CLOSE-BACKWARD-CURSOR.

           EXEC SQL
               CLOSE ITEMBACK
           END-EXEC.

           IF SQLCODE NOT = +0
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA007' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'CLOSE BACKWARD CURSOR' TO WPDE-FUNCTION
               MOVE 'P06230' TO WPDE-PARAGRAPH
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

           IF WS-SUB1 = 1
               MOVE SPACES TO ITEMNOO
                              ITEMNMO
               MOVE ZEROES TO LENGTHO
                              DIAMTRO
           END-IF.

           MOVE ALL '_' TO SCRN-QUANTITY(WS-SUB1).
           MOVE SPACES TO SCRN-SUPPLIER(WS-SUB1).
           MOVE ZEROES TO SCRN-PRICE(WS-SUB1).
           MOVE SPACES TO SCRN-SUPPLIER-NAME(WS-SUB1).

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

           IF NO-ERROR-FOUND
               MOVE -1 TO QTY1L
           END-IF.

      *****************************************************************
      *    MAKE FINAL ADJUSTMENTS TO ATTRIBUTES AND FIELDS            *
      *****************************************************************

           PERFORM P79100-SET-MAP-FIELDS THRU P79100-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
                   UNTIL WS-SUB1 > WS-SUB-MAX.

      *****************************************************************
      *    SEND FULL MAP IF 1ST TIME, OTHERWISE SEND DATAONLY         *
      *****************************************************************

           IF PC-PREV-PGRMID = 'PDA007'
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

           IF WPW-SUPPLIER(WS-SUB1) > SPACES
               MOVE DFHBMFSE TO QTY-ATTR(WS-SUB1)
               INSPECT SCRN-QUANTITY(WS-SUB1)
                   CONVERTING WMF-SPACES-LOWVALUE-R TO '__'
           ELSE
               MOVE DFHBMASF TO QTY-ATTR(WS-SUB1)
               MOVE SPACES TO SCRN-QUANTITY(WS-SUB1)
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
                   MAP('PDA007')
                   MAPSET('PDA007M')
                   FROM(PDA007O)
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
               MOVE 'PDA007' TO WPCE-PROGRAM-ID
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
                   MAP('PDA007')
                   MAPSET('PDA007M')
                   FROM(PDA007O)
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
               MOVE 'PDA007' TO WPCE-PROGRAM-ID
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
                   MAP('PDA007')
                   MAPSET('PDA007M')
                   INTO(PDA007I)
                   NOHANDLE
                   RESP(WS-RESPONSE-CODE)
           END-EXEC.


      *****************************************************************
      *    IF ERROR, FORMAT ERROR INFORMATION AND TERMINATE           *
      *****************************************************************

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL) AND
               WS-RESPONSE-CODE NOT = DFHRESP(MAPFAIL)
                   MOVE 'CICS' TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA007' TO WPCE-PROGRAM-ID
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
               MOVE 'PDA007' TO WPCE-PROGRAM-ID
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
               MOVE 'PDA007' TO WPCE-PROGRAM-ID
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
               MOVE 'PDA007' TO WPCE-PROGRAM-ID
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
           MOVE 'PDA007' TO WPCE-PROGRAM-ID.
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
AS OF      MOVE 'PDA007' TO WPCE-PROGRAM-ID.                             AS OF
JAN        MOVE EIBRESP TO WPCE-RESPONSE-CODE.                           JAN
2001       MOVE 'ERROR' TO WPCE-COMMAND.                                 2001
           MOVE 'P99999' TO WPCE-PARAGRAPH.
LLR                                                                      LLR
           PERFORM P99500-PDA-ERROR THRU P99500-EXIT.

       P99999-ERROR-EXIT.
           EXIT.
           EJECT