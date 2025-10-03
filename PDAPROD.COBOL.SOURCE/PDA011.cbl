       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA011.
      *
      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA011                                            *
      * TRANS   :   PD11                                              *
      * MAPSET  :   PDA011M                                           *
      *                                                               *
      * FUNCTION:   PROGRAM PDA011 IS THE VIEW ORDER ITEMS WHICH      *
      *             DISPLAYS A SCROLLABLE LIST OF ALL MERCHANDISE     *
      *             ITEMS FOR A SELECTED ORDER.  ITEM INFORMATION     *
      *             INCLUDES ORDER QUANTITY, ITEM NAME, SUPPLIER,     *
      *             SUPPLIER UNIT PRICE, AND EXTENDED PRICE WHICH IS  *
      *             CALCULATED AS ORDER QUANTITY TIMES UNIT PRICE.    *
      *                                                               *
      * FILES   :   ORDER_DATABASE     -  IMS-DLI    (READ-ONLY)      *
      *             ITEM               -  DB2        (READ-ONLY)      *
      *             ITEM_SUPPLIER      -  DB2        (READ-ONLY)      *
      *                                                               *
      *                                                               *
      * TRANSACTIONS GENERATED:                                       *
      *             PD10       ORDER INQUIRY/MAINTENANCE              *
      *             PD02       ORDER MENU                             *
      *             PD01       MAIN MENU                              *
      *                                                               *
      *                                                               *
      * PFKEYS  :   PF03  =    EXIT, RETURN TO ORDER INQUIRY/MAINT    *
      *             PF07  =    SCROLL BACKWARD                        *
      *             PF08  =    SCROLL FORWARD                         *
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
           05  EIBAID-SW               PIC X     VALUE ' '.
               88  CLEAR-PKEY                    VALUE '_'.
               88  ENTER-PKEY                    VALUE ''''.
               88  PREVIOUS-PKEY                 VALUE '3'.
               88  BACKWARD-PKEY                 VALUE '7'.
               88  FORWARD-PKEY                  VALUE '8'.
               88  ORDER-MENU-PKEY               VALUE '#'.
               88  MAIN-MENU-PKEY                VALUE '@'.
               88  VALID-PKEY-ENTERED            VALUE '_' '@' '3' '7'
                                                       '8' '#' ''''.
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
           05  WMF-PSB-NAME            PIC X(8)  VALUE 'PDA011'.
           05  WMF-SHIFT-KEYS          PIC X(100) VALUE SPACES.
           05  WS-PDA011-WORKAREA.
               07  WPW-PAGE-NUMBER     PIC S99   VALUE +0.
               07  WPW-MORE-SW         PIC X     VALUE SPACES.
               07  WPW-PAGE-KEYS.
                   09  WPW-CURRENT-KEY.
                       11  WPW-CK-PREF PIC 9(5)  VALUE ZEROES.
                       11  WPW-CK-NUMB PIC 9(10) VALUE ZEROES.
                       11  WPW-CK-SEQ  PIC 9(5)  VALUE ZEROES.
                   09  WPW-REST-KEYS   PIC X(100) VALUE SPACES.

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
      *         MAP DSECTS -- BROWSE ITEMS BY CATEGORY - PDA011M      *
      *****************************************************************

           COPY PDA011M.
       01  FILLER                      REDEFINES PDA011O.
           03  FILLER                  PIC X(66).
           03  P11-SCREEN-AREA         OCCURS 3 TIMES.
               05  QTY-LEN             PIC S9(4)                COMP.
               05  QTY-ATTR            PIC X.
               05  FILLER              PIC XX.
               05  SCRN-QUANTITY       PIC ZZZZZZZZZ.
               05  ITEM-LEN            PIC S9(4)                COMP.
               05  ITEM-ATTR           PIC X.
               05  FILLER              PIC XX.
               05  SCRN-ITEM-NUMBER    PIC X(32).
               05  SEQ-LEN             PIC S9(4)                COMP.
               05  SEQ-ATTR            PIC X.
               05  FILLER              PIC XX.
               05  SCRN-SEQUENCE       PIC X(5).
               05  NAME-LEN            PIC S9(4)                COMP.
               05  NAME-ATTR           PIC X.
               05  FILLER              PIC XX.
               05  SCRN-ITEM-NAME      PIC X(50).
               05  SUPPLR-LEN          PIC S9(4)                COMP.
               05  SUPPLR-ATTR         PIC X.
               05  FILLER              PIC XX.
               05  SCRN-SUPPLIER-ID    PIC X(32).
               05  UPRICE-LEN          PIC S9(4)                COMP.
               05  UPRICE-ATTR         PIC X.
               05  FILLER              PIC XX.
               05  SCRN-UNIT-PRICE     PIC ZZ,ZZZ,ZZZ.ZZ.
               05  XPRICE-LEN          PIC S9(4)                COMP.
               05  XPRICE-ATTR         PIC X.
               05  FILLER              PIC XX.
               05  SCRN-EXT-PRICE      PIC ZZ,ZZZ,ZZZ.ZZ.
           EJECT
      *****************************************************************
      *    IMS / DLI DEFINITIONS                                      *
      *****************************************************************

           COPY IORDER.
           EJECT

           COPY IORDITEM.
           EJECT
      *****************************************************************
      *    VSAM FILE DEFINITIONS                                      *
      *****************************************************************
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
              INCLUDE DSUPPLR
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

      *01  WS-PDA011-WORKAREA.
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
                   MOVE 'PDA011' TO WPCE-PROGRAM-ID
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
               MOVE 'PDA011' TO WPCE-PROGRAM-ID
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

           MOVE WS-PDA011-WORKAREA TO PC-PROGRAM-WORKAREA.

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
                   TRANSID('PD11')
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
               MOVE 'PDA011' TO WPCE-PROGRAM-ID
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
      *    IF PREVIOUS PROGRAM IS NOT PDA011, SET INQUIRY MODE        *
      *    OTHERWISE SET EDIT / UPDATE MODE                           *
      *****************************************************************

           IF PC-PREV-PGRMID = 'PDA011'
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

           MOVE LOW-VALUES TO PDA011I.
           MOVE WMF-DATE-MMDDYY TO PDADATEO.
           MOVE EIBTRMID TO PDATERMO.
           MOVE WMF-TIME-HHMMSS TO PDATIMEO.

      *****************************************************************
      *    FORMAT AND SEND THE FULL MAP -- LITERALS AND DATA          *
      *****************************************************************

           MOVE SPACES TO WS-PDA011-WORKAREA.
           MOVE +1 TO WPW-PAGE-NUMBER.
           MOVE 'N' TO WPW-MORE-SW
                       WS-END-OF-PROCESS-SW.
           MOVE PC-USERID-NUMBER TO WPW-CK-PREF.
           MOVE PC-ORDER-NUMBER TO WPW-CK-NUMB.
           MOVE ZEROES TO WPW-CK-SEQ.

           PERFORM P05000-BUILD-SCREEN THRU P05000-EXIT.

           PERFORM P79000-DISPLAY-SCREEN THRU P79000-EXIT.

           MOVE 'PDA011' TO PC-PREV-PGRMID.

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

           MOVE 'PDA011' TO PC-PREV-PGRMID.
           MOVE PC-PROGRAM-WORKAREA TO WS-PDA011-WORKAREA.

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

      *****************************************************************
      *    EDIT THE OPERATOR PROGRAM FUNCTION KEY SELECTION (PFKEY)   *
      *****************************************************************

           PERFORM P03120-EDIT-PFKEY THRU P03120-EXIT.

       P03100-EXIT.
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
      *    VALID KEYS ARE: ENTER, PF3, PF7, PF8, PF11, PF12, CLEAR    *
      *****************************************************************

           MOVE EIBAID TO EIBAID-SW.

           IF NOT VALID-PKEY-ENTERED
               MOVE -1 TO ORDNBRL
               MOVE PM001-INVALID-PFKEY TO WMF-MESSAGE-AREA
               PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT
               GO TO P03120-EXIT
           END-IF.

      *****************************************************************
      *    IF SELECTION ENTERED AND PFKEY HIT, DISPLAY ERROR MESSAGE  *
      *****************************************************************

           IF SELECTION-MADE
               IF NOT ENTER-PKEY
                   MOVE -1 TO ORDNBRL
                   MOVE PM003-ACTION-VS-PFKEY-CONFLICT TO
                       WMF-MESSAGE-AREA
                   PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT
               END-IF
               GO TO P03120-EXIT
           END-IF.

      *****************************************************************
      *    IF ENTER KEY PRESSED, DISPLAY ERROR MESSAGE                *
      *****************************************************************

           IF ENTER-PKEY
               MOVE -1 TO ORDNBRL
               MOVE PM031-USE-PFKEY TO WMF-MESSAGE-AREA
               PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT
               GO TO P03120-EXIT
           END-IF.

      *****************************************************************
      *    PF03 FROM THIS SCREEN RETURNS USER TO THE ORDER INQUIRY/   *
      *    MAINTENANCE SCREEN.                                        *
      *****************************************************************

           IF PREVIOUS-PKEY
               MOVE 'PDA010' TO PC-NEXT-PGRMID
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
      *    PF07 FROM THIS SCREEN SCROLLS BACKWARDS                    *
      *****************************************************************

           IF BACKWARD-PKEY
               IF WPW-PAGE-NUMBER = 1
                   MOVE PM014-TOP-MSG TO PDAMSGO
               ELSE
                   MOVE 'Y' TO WPW-MORE-SW
                   COMPUTE WPW-PAGE-NUMBER = WPW-PAGE-NUMBER - 1
                   MOVE WPW-REST-KEYS TO WMF-SHIFT-KEYS
                   MOVE WMF-SHIFT-KEYS TO WPW-PAGE-KEYS
                   IF WPW-PAGE-KEYS = SPACES
                       MOVE PC-USERID-NUMBER TO WPW-CK-PREF
                       MOVE PC-ORDER-NUMBER TO WPW-CK-NUMB
                       MOVE ZEROES TO WPW-CK-SEQ
                   END-IF
                   MOVE 'N' TO WS-END-OF-PROCESS-SW
                   PERFORM P05000-BUILD-SCREEN THRU P05000-EXIT
               END-IF
           END-IF.

      *****************************************************************
      *    PF08 FROM THIS SCREEN SCROLLS FORWARDS                     *
      *****************************************************************

           IF FORWARD-PKEY
               IF WPW-MORE-SW = 'N'
                   MOVE PM013-BOTTOM-MSG TO PDAMSGO
               ELSE
                   COMPUTE WPW-PAGE-NUMBER = WPW-PAGE-NUMBER + 1
                   MOVE WPW-PAGE-KEYS TO WMF-SHIFT-KEYS
                   MOVE WMF-SHIFT-KEYS TO WPW-REST-KEYS
                   MOVE SCRN-SEQUENCE(3) TO WPW-CK-SEQ
                   MOVE 'N' TO WS-END-OF-PROCESS-SW
                   PERFORM P05000-BUILD-SCREEN THRU P05000-EXIT
               END-IF
           END-IF.

       P03120-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05000-BUILD-SCREEN                            *
      *                                                               *
      *    FUNCTION :  READS ORDER DATABASE AND FORMATS ITEM LINES    *
      *                                                               *
      *    CALLED BY:  P00100-FIRST-TIME                              *
      *                P03120-EDIT-PFKEY                              *
      *                                                               *
      *****************************************************************

       P05000-BUILD-SCREEN.

           IF NOT INQUIRY-TRANS
               PERFORM P78000-CLEAR-SCREEN THRU P78000-EXIT
                   VARYING WS-SUB1 FROM 1 BY 1
                       UNTIL WS-SUB1 > WS-SUB-MAX
           END-IF.

           PERFORM P05100-SCHEDULE-PSB THRU P05100-EXIT.

           MOVE PC-ORDER-NUMBER TO ORDNBRO.
           MOVE 'N' TO WPW-MORE-SW.
           MOVE WPW-CK-PREF TO ORDER-PREFIX
                               ORDER-ITEM-PREFIX.
           MOVE WPW-CK-NUMB TO ORDER-NUMBER.
           MOVE WPW-CK-SEQ TO ORDER-ITEM-SEQUENCE.

           PERFORM P05200-FORMAT-LINE THRU P05200-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
                   UNTIL END-OF-PROCESS.

           PERFORM P05300-TERMINATE-PSB THRU P05300-EXIT.

       P05000-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05100-SCHEDULE-PSB                            *
      *                                                               *
      *    FUNCTION :  PERFORMS A SCHEDULE PSB ON THE ORDER DATABASE. *
      *                                                               *
      *    CALLED BY:  P05000-BUILD-SCREEN                            *
      *                                                               *
      *****************************************************************

       P05100-SCHEDULE-PSB.

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
               MOVE 'PDA011' TO WPIE-PROGRAM-ID
               MOVE 'P05100' TO WPIE-PARAGRAPH
               MOVE 'SCHD' TO WPIE-FUNCTION-CODE
               MOVE SPACES TO WPIE-SEGMENT-NAME
                              WPIE-DATABASE-NAME
               MOVE DIBSTAT TO WPIE-STATUS-CODE
               MOVE 'PSB SCHEDULING ERROR' TO WPIE-COMMAND
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P05100-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05200-FORMAT-LINE                             *
      *                                                               *
      *    FUNCTION :  FORMATS LINE OF LIST                           *
      *                                                               *
      *    CALLED BY:  P05000-BUILD-SCREEN                            *
      *                                                               *
      *****************************************************************

       P05200-FORMAT-LINE.

           PERFORM P05210-GU-ORDER-DATABASE THRU P05210-EXIT.

           IF ORDER-PREFIX NOT = PC-USERID-NUMBER OR
               DIBSTAT = 'GE'
                   MOVE 'Y' TO WS-END-OF-PROCESS-SW
                               WS-BOTTOM-OF-DATA-SW
                   MOVE PM013-BOTTOM-MSG TO PDAMSGO
           END-IF.

           IF ERROR-FOUND OR
               END-OF-PROCESS
                   GO TO P05200-EXIT
           END-IF.

           IF WS-SUB1 > WS-SUB-MAX
               MOVE 'Y' TO WS-END-OF-PROCESS-SW
                           WPW-MORE-SW
               GO TO P05200-EXIT
           END-IF.

           MOVE ORDER-ITEM-QUANTITY TO SCRN-QUANTITY(WS-SUB1).
           MOVE ORDER-ITEM-ITEM-NUMBER TO SCRN-ITEM-NUMBER(WS-SUB1).
           MOVE ORDER-ITEM-SEQUENCE TO SCRN-SEQUENCE(WS-SUB1).
           MOVE ORDER-ITEM-ITEM-KEY TO ITEM-KEY.

           EXEC SQL
               SELECT  NAME
               INTO    :ITEM-NAME
               FROM    ITEM
               WHERE   PREFIX             = :ITEM-PREFIX AND
                       NUMBER             = :ITEM-NUMBER
           END-EXEC.

           IF SQLCODE NOT = +0
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA011' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'SELECT ITEM' TO WPDE-FUNCTION
               MOVE 'P05200' TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

           MOVE ITEM-NAME TO SCRN-ITEM-NAME(WS-SUB1).
           MOVE ORDER-ITEM-SUPPLIER-ID TO SCRN-SUPPLIER-ID(WS-SUB1).
           MOVE ORDER-ITEM-UNIT-PRICE TO SCRN-UNIT-PRICE(WS-SUB1).

           COMPUTE ORDER-ITEM-UNIT-PRICE = ORDER-ITEM-UNIT-PRICE *
                                           ORDER-ITEM-QUANTITY.

           MOVE ORDER-ITEM-UNIT-PRICE TO SCRN-EXT-PRICE(WS-SUB1).

       P05200-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05210-GU-ORDER-DATABASE                       *
      *                                                               *
      *    FUNCTION :  GET NEXT SEGMENT FROM THE ORDER DATABASE.      *
      *                                                               *
      *    CALLED BY:  P05200-FORMAT-LINE                             *
      *                                                               *
      *****************************************************************

       P05210-GU-ORDER-DATABASE.

           EXEC DLI
               GU USING
                   PCB(1)
                   SEGMENT(ORDER)
                   WHERE(ORDKEY=ORDER-KEY)
                   FIELDLENGTH(15)
                   SEGMENT(ORDITEM)
                   INTO(ORDER-ITEM-SEGMENT)
                   SEGLENGTH(95)
                   WHERE(ITEMKEY>ORDER-ITEM-KEY)
                   FIELDLENGTH(10)
           END-EXEC.

      *****************************************************************
      *    CHECK STATUS CODE FOR SUCCESS, END OF DATABASE, ALL OTHERS *
      *    ARE AN ERROR.                                              *
      *****************************************************************

           IF DIBSTAT NOT = SPACES
               IF DIBSTAT = 'GE'
                   MOVE 'Y' TO WS-END-OF-PROCESS-SW
               ELSE
                   MOVE 'Y' TO WS-ERROR-FOUND-SW
                   MOVE 'IMS' TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA011' TO WPIE-PROGRAM-ID
                   MOVE 'P05210' TO WPIE-PARAGRAPH
                   MOVE 'GU' TO WPIE-FUNCTION-CODE
                   MOVE 'ORDER' TO WPIE-SEGMENT-NAME
                   MOVE 'ORDER1DB' TO WPIE-DATABASE-NAME
                   MOVE DIBSTAT TO WPIE-STATUS-CODE
                   MOVE 'GU ORDER ROOT SEGMENT' TO WPIE-COMMAND
                   PERFORM P99500-PDA-ERROR THRU P99500-EXIT
               END-IF
           END-IF.

       P05210-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05300-TERMINATE-PSB                           *
      *                                                               *
      *    FUNCTION :  PERFORMS TERMINATE PSB ON THE ORDER DATABASE.  *
      *                                                               *
      *    CALLED BY:  P05000-BUILD-SCREEN                            *
      *                                                               *
      *****************************************************************

       P05300-TERMINATE-PSB.

           EXEC DLI
               TERMINATE
           END-EXEC.

      *****************************************************************
      *    CHECK FOR PSB TERMINATION ERROR                            *
      *****************************************************************

           IF DIBSTAT NOT = SPACES
               MOVE 'Y' TO WS-ERROR-FOUND-SW
               MOVE 'IMS' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA011' TO WPIE-PROGRAM-ID
               MOVE 'P05300' TO WPIE-PARAGRAPH
               MOVE 'TERM' TO WPIE-FUNCTION-CODE
               MOVE SPACES TO WPIE-SEGMENT-NAME
                              WPIE-DATABASE-NAME
               MOVE DIBSTAT TO WPIE-STATUS-CODE
               MOVE 'PSB TERMINATION ERROR' TO WPIE-COMMAND
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P05300-EXIT.
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

           MOVE SPACES TO SCRN-ITEM-NUMBER(WS-SUB1)
                          SCRN-ITEM-NAME(WS-SUB1)
                          SCRN-SEQUENCE(WS-SUB1)
                          SCRN-SUPPLIER-ID(WS-SUB1).
           MOVE ZEROES TO SCRN-QUANTITY(WS-SUB1)
                          SCRN-UNIT-PRICE(WS-SUB1)
                          SCRN-EXT-PRICE(WS-SUB1).

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

           MOVE -1 TO ORDNBRL.

      *****************************************************************
      *    SEND FULL MAP IF 1ST TIME, OTHERWISE SEND DATAONLY         *
      *****************************************************************

           IF PC-PREV-PGRMID = 'PDA011'
               PERFORM P80100-SEND-MAP-DATAONLY THRU P80100-EXIT
           ELSE
               PERFORM P80000-SEND-FULL-MAP THRU P80000-EXIT
           END-IF.

       P79000-EXIT.
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
                   MAP('PDA011')
                   MAPSET('PDA011M')
                   FROM(PDA011O)
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
               MOVE 'PDA011' TO WPCE-PROGRAM-ID
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
                   MAP('PDA011')
                   MAPSET('PDA011M')
                   FROM(PDA011O)
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
               MOVE 'PDA011' TO WPCE-PROGRAM-ID
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
                   MAP('PDA011')
                   MAPSET('PDA011M')
                   INTO(PDA011I)
                   NOHANDLE
                   RESP(WS-RESPONSE-CODE)
           END-EXEC.


      *****************************************************************
      *    IF ERROR, FORMAT ERROR INFORMATION AND TERMINATE           *
      *****************************************************************

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL) AND
               WS-RESPONSE-CODE NOT = DFHRESP(MAPFAIL)
                   MOVE 'CICS' TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA011' TO WPCE-PROGRAM-ID
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
               MOVE 'PDA011' TO WPCE-PROGRAM-ID
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
               MOVE 'PDA011' TO WPCE-PROGRAM-ID
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
               MOVE 'PDA011' TO WPCE-PROGRAM-ID
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
           MOVE 'PDA011' TO WPCE-PROGRAM-ID.
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
AS OF      MOVE 'PDA011' TO WPCE-PROGRAM-ID.                             AS OF
JAN        MOVE EIBRESP TO WPCE-RESPONSE-CODE.                           JAN
2001       MOVE 'ERROR' TO WPCE-COMMAND.                                 2001
           MOVE 'P99999' TO WPCE-PARAGRAPH.
LLR                                                                      LLR
           PERFORM P99500-PDA-ERROR THRU P99500-EXIT.

       P99999-ERROR-EXIT.
           EXIT.
           EJECT