       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA024.
      *
      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA024                                            *
      * TRANS   :   PD24                                              *
      * MAPSET  :   PDA024M                                           *
      *                                                               *
      * FUNCTION:   PROGRAM PDA024 IS THE SPECIAL SCENARIO SELECTION  *
      *             SCREEN WHICH CONTAINS A SCROLLABLE LIST OF THE    *
      *             AVAILABLE SPECIAL SCENARIOS.  THE USER MAY CHOOSE *
      *             TO ACTIVATE OR DEACTIVATE ANY OF THE SCENARIOS    *
      *             FOR THEIR USE.                                    *
      *                                                               *
      * FILES   :   USERID             -  DB2        (UPDATE)         *
      *                                                               *
      *                                                               *
      * TRANSACTIONS GENERATED:                                       *
      *             PD01       MAIN MENU                              *
      *             PD03       MAINTENANCE MENU                       *
      *                                                               *
      *                                                               *
      * PFKEYS  :   PF03  =    EXIT, RETURN TO PDA003, MAINT MENU     *
      *             PF07  =    SCROLL BACKWARD                        *
      *             PF08  =    SCROLL FORWARD                         *
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
               88  MAIN-MENU-PKEY                VALUE '@'.
               88  VALID-PKEY-ENTERED            VALUE '_' '@' '3' '7'
                                                       '8' ''''.
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
           05  WS-PDA024-WORKAREA.
               07  WPW-PAGE-NUMBER     PIC 9(3)  VALUE ZEROES.
               07  WPW-MORE-SW         PIC X     VALUE SPACES.
           05  WS-SCENARIO             PIC 9(5)  VALUE ZEROES.

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
      *         CICS ATTENTION IDENTIFIER VALUES                      *
      *****************************************************************

           COPY DFHAID.
           EJECT
      *****************************************************************
      *         MAP DSECTS -- SCENARIO SELECTION - PDA024M            *
      *****************************************************************

           COPY PDA024M.
       01  FILLER                      REDEFINES PDA024O.
           03  FILLER                  PIC X(51).
           03  FILLER                  OCCURS 5 TIMES.
               05  ACT-LEN             PIC S9(4)                COMP.
               05  ACT-ATTR            PIC X.
               05  FILLER              PIC XX.
               05  SCRN-ACTION         PIC X.
               05  SCENARIO-LEN        PIC S9(4)                COMP.
               05  SCENARIO-ATTR       PIC X.
               05  FILLER              PIC XX.
               05  SCRN-SCENARIO       PIC X(5).
               05  STATUS-LEN          PIC S9(4)                COMP.
               05  STATUS-ATTR         PIC X.
               05  FILLER              PIC XX.
               05  SCRN-STATUS         PIC X(8).
               05  PROGRAM-LEN         PIC S9(4)                COMP.
               05  PROGRAM-ATTR        PIC X.
               05  FILLER              PIC XX.
               05  SCRN-PROGRAM        PIC X(6).
               05  NAME-LEN            PIC S9(4)                COMP.
               05  NAME-ATTR           PIC X.
               05  FILLER              PIC XX.
               05  SCRN-NAME           PIC X(50).
           EJECT
      *****************************************************************
      *    IMS / DLI DEFINITIONS                                      *
      *****************************************************************

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
              INCLUDE DUSERID
           END-EXEC.
           EJECT
      *****************************************************************
      *    SPECIAL SCENARIOS                                          *
      *****************************************************************

           COPY PDASCNWS.
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
               MOVE 'PDA024' TO WPCE-PROGRAM-ID
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

           MOVE WS-PDA024-WORKAREA TO PC-PROGRAM-WORKAREA.

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
                   TRANSID('PD24')
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
               MOVE 'PDA024' TO WPCE-PROGRAM-ID
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
      *    VERIFY THE COMMAREA IS PRESENT AND CORRECT LENGTH          *
      *****************************************************************

           IF EIBCALEN > ZEROES
               IF EIBCALEN NOT = PC-COMMAREA-LTH
                   MOVE 'CICS' TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA024' TO WPCE-PROGRAM-ID
                   MOVE ZEROES TO WPCE-RESPONSE-CODE
                   MOVE 'COMMAREA LENGTH NOT CORRECT' TO WPCE-COMMAND
                   MOVE 'P00500' TO WPCE-PARAGRAPH
                   PERFORM P99500-PDA-ERROR THRU P99500-EXIT
               END-IF
           ELSE
               MOVE PM019-ENTER-APPLICATION TO WMF-MESSAGE-AREA
               PERFORM P80400-SEND-MESSAGE THRU P80400-EXIT
               GO TO P00500-EXIT
           END-IF.

      *****************************************************************
      *    IF PREVIOUS PROGRAM IS NOT PDA024, SET INQUIRY MODE        *
      *    OTHERWISE SET EDIT / UPDATE MODE                           *
      *****************************************************************

           IF PC-PREV-PGRMID = 'PDA024'
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

           MOVE LOW-VALUES TO PDA024I.
           MOVE WMF-DATE-MMDDYY TO PDADATEO.
           MOVE EIBTRMID TO PDATERMO.
           MOVE WMF-TIME-HHMMSS TO PDATIMEO.

      *****************************************************************
      *    FORMAT AND SEND THE FULL MAP -- LITERALS AND DATA          *
      *****************************************************************

           MOVE SPACES TO WS-PDA024-WORKAREA.
           MOVE 1 TO WPW-PAGE-NUMBER.
           MOVE 'N' TO WPW-MORE-SW
                       WS-END-OF-PROCESS-SW.

           PERFORM P04000-BUILD-SCREEN THRU P04000-EXIT.

           PERFORM P79000-DISPLAY-SCREEN THRU P79000-EXIT.

           MOVE 'PDA024' TO PC-PREV-PGRMID.

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

           MOVE 'PDA024' TO PC-PREV-PGRMID.
           MOVE PC-PROGRAM-WORKAREA TO WS-PDA024-WORKAREA.

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

           PERFORM P03110-INSPECT-ACTIONS THRU P03110-EXIT
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
               MOVE -1 TO ACT1L
               MOVE PM025-MAKE-SELECTION TO PDAMSGO
               GO TO P03100-EXIT
           END-IF.

      *****************************************************************
      *    EDIT THE OPERATOR ENTERED DATA                             *
      *****************************************************************

           PERFORM P03130-EDIT-ACTIONS THRU P03130-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
                   UNTIL WS-SUB1 > WS-SUB-MAX.

           IF ERROR-FOUND
               GO TO P03100-EXIT
           END-IF.

           PERFORM P05000-GET-USERID THRU P05000-EXIT.

           PERFORM P03140-PROCESS-ACTIONS THRU P03140-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
                   UNTIL WS-SUB1 > WS-SUB-MAX.

           PERFORM P03150-UPDATE-USERID THRU P03150-EXIT.

           MOVE PM047-SCENARIOS-PROCESSED TO PDAMSGO.

       P03100-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03110-INSPECT-ACTIONS                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INSPECT ALL 5 ACTION FIELDS         *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03110-INSPECT-ACTIONS.

           INSPECT SCRN-ACTION(WS-SUB1)
               CONVERTING WMF-SPACES-LOWVALUE-R TO '__'.

           IF SCRN-ACTION(WS-SUB1) NOT = '_'
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
      *    VALID KEYS ARE: ENTER, PF3, PF7, PF8, PF12, CLEAR          *
      *****************************************************************

           MOVE EIBAID TO EIBAID-SW.

           IF NOT VALID-PKEY-ENTERED
               MOVE -1 TO ACT1L
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
                   GO TO P03120-EXIT
               END-IF
           END-IF.

      *****************************************************************
      *    PF03 FROM THIS SCREEN RETURNS USER TO MAINT MENU SCREEN    *
      *****************************************************************

           IF PREVIOUS-PKEY
               MOVE 'PDA003' TO PC-NEXT-PGRMID
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
               IF WPW-PAGE-NUMBER = 1
                   MOVE PM014-TOP-MSG TO PDAMSGO
               ELSE
                   MOVE 'N' TO WS-END-OF-PROCESS-SW
                   SUBTRACT 1 FROM WPW-PAGE-NUMBER
                   PERFORM P04000-BUILD-SCREEN THRU P04000-EXIT
               END-IF
           END-IF.

      *****************************************************************
      *    PF08 FROM THIS SCREEN SCROLLS FORWARDS                     *
      *****************************************************************

           IF FORWARD-PKEY
               IF WPW-MORE-SW = 'N'
                   MOVE PM013-BOTTOM-MSG TO PDAMSGO
               ELSE
                   MOVE 'N' TO WS-END-OF-PROCESS-SW
                   ADD 1 TO WPW-PAGE-NUMBER
                   PERFORM P04000-BUILD-SCREEN THRU P04000-EXIT
               END-IF
           END-IF.

       P03120-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03130-EDIT-ACTIONS                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO EDIT ALL 5 ACTION FIELDS            *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03130-EDIT-ACTIONS.

           EVALUATE TRUE
               WHEN SCRN-ACTION(WS-SUB1) = LOW-VALUES
               WHEN SCRN-ACTION(WS-SUB1) = ' '
               WHEN SCRN-ACTION(WS-SUB1) = '_'
                   EXIT
               WHEN SCRN-ACTION(WS-SUB1) = 'A'
               WHEN SCRN-ACTION(WS-SUB1) = 'D'
                   EXIT
               WHEN OTHER
                   MOVE -1 TO ACT-LEN(WS-SUB1)
                   MOVE DFHDFHI TO ACT-ATTR(WS-SUB1)
                   MOVE PM020-INVALID-ACTION-CODE TO WMF-MESSAGE-AREA
                   PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT
           END-EVALUATE.

       P03130-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03140-PROCESS-ACTIONS                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS ACTIONS                     *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03140-PROCESS-ACTIONS.

           MOVE SCRN-SCENARIO(WS-SUB1) TO WS-SCENARIO.

           IF SCRN-ACTION(WS-SUB1) = 'A'
               MOVE 'Y' TO PC-ACTIVE-SCENARIO(WS-SCENARIO)
               MOVE ' ACTIVE ' TO SCRN-STATUS(WS-SUB1)
           END-IF.

           IF SCRN-ACTION(WS-SUB1) = 'D'
               MOVE 'INACTIVE' TO SCRN-STATUS(WS-SUB1)
               MOVE SPACES TO PC-ACTIVE-SCENARIO(WS-SCENARIO)
           END-IF.

           MOVE '_' TO SCRN-ACTION(WS-SUB1).

       P03140-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03150-UPDATE-USERID                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO UPDATE THE USERID RECORD            *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03150-UPDATE-USERID.

           MOVE PC-USERID-ID TO USERID-ID.
           MOVE PC-ACTIVE-SCENARIOS-GRP TO USERID-ACTIVE-SCENARIOS.

           EXEC SQL
               UPDATE  USERID
               SET     ACTIVE_SCENARIOS = :USERID-ACTIVE-SCENARIOS
               WHERE   ID = :USERID-ID
           END-EXEC.

           IF SQLCODE NOT = +0
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA024' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'UPDATE USERID' TO WPDE-FUNCTION
               MOVE 'P03150' TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P03150-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P04000-BUILD-SCREEN                            *
      *                                                               *
      *    FUNCTION :  BUILDS THE SCREEN                              *
      *                                                               *
      *    CALLED BY:  P00100-FIRST-TIME                              *
      *                P03120-EDIT-PFKEY                              *
      *                                                               *
      *****************************************************************

       P04000-BUILD-SCREEN.

      *****************************************************************
      * IF FIRST-TIME-PROCESSING, KEYS NEED NOT BE CHECKED            *
      *****************************************************************

           IF NOT INQUIRY-TRANS
               PERFORM P78000-CLEAR-SCREEN THRU P78000-EXIT
                   VARYING WS-SUB1 FROM 1 BY 1
                       UNTIL WS-SUB1 > WS-SUB-MAX
           END-IF.

           PERFORM P05000-GET-USERID THRU P05000-EXIT.

           MOVE 'N' TO WPW-MORE-SW.

           COMPUTE PDA-SWA-SUB = ((WPW-PAGE-NUMBER - 1) * 4).

           PERFORM P04100-LOAD-SCENARIOS THRU P04100-EXIT
               VARYING WS-SUB1 FROM 1 BY 1
                   UNTIL END-OF-PROCESS.

       P04000-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P04100-LOAD-SCENARIOS                          *
      *                                                               *
      *    FUNCTION :  LOADS THE SCENARIOS TO THE SCREEN              *
      *                                                               *
      *    CALLED BY:  P04000-BUILD-SCREEN                            *
      *                                                               *
      *****************************************************************

       P04100-LOAD-SCENARIOS.

           ADD 1 TO PDA-SWA-SUB.

           IF PDA-SWA-SUB > PDA-SWA-MAX-ENTRIES OR
               PDA-SWA-SL-SCENARIO(PDA-SWA-SUB) = SPACES
                   MOVE 'Y' TO WS-END-OF-PROCESS-SW
                   MOVE PM013-BOTTOM-MSG TO PDAMSGO
                   GO TO P04100-EXIT
           END-IF.

           IF WS-SUB1 > WS-SUB-MAX
               IF PDA-SWA-SL-SCENARIO(PDA-SWA-SUB) > SPACES
                   MOVE 'Y' TO WPW-MORE-SW
               END-IF
               MOVE 'Y' TO WS-END-OF-PROCESS-SW
               GO TO P04100-EXIT
           END-IF.

           MOVE PDA-SWA-SL-SCENARIO(PDA-SWA-SUB) TO WS-SCENARIO
                                                 SCRN-SCENARIO(WS-SUB1).
           MOVE PDA-SWA-SL-PROGRAM(PDA-SWA-SUB) TO
                                                  SCRN-PROGRAM(WS-SUB1).
           MOVE PDA-SWA-SL-NAME(PDA-SWA-SUB) TO SCRN-NAME(WS-SUB1).

           IF PC-ACTIVE-SCENARIO(WS-SCENARIO) = 'Y'
               MOVE ' ACTIVE ' TO SCRN-STATUS(WS-SUB1)
           ELSE
               MOVE 'INACTIVE' TO SCRN-STATUS(WS-SUB1)
           END-IF.

       P04100-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05000-GET-USERID                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO GET THE USERID RECORD               *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                P04000-BUILD-SCREEN                            *
      *                                                               *
      *****************************************************************

       P05000-GET-USERID.

           MOVE PC-USERID-ID TO USERID-ID.

           EXEC SQL
               SELECT  ACTIVE_SCENARIOS
               INTO    :USERID-ACTIVE-SCENARIOS
               FROM    USERID
               WHERE   ID = :USERID-ID
           END-EXEC.

           IF SQLCODE NOT = +0
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA024' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'SELECT USERID' TO WPDE-FUNCTION
               MOVE 'P05000' TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

           MOVE USERID-ACTIVE-SCENARIOS TO PC-ACTIVE-SCENARIOS-GRP.

       P05000-EXIT.
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
      *    CALLED BY:  P04000-BULD-SCREEN                             *
      *                                                               *
      *                                                               *
      *****************************************************************

       P78000-CLEAR-SCREEN.

           MOVE '_' TO SCRN-ACTION(WS-SUB1).
           MOVE SPACES TO SCRN-SCENARIO(WS-SUB1)
                          SCRN-STATUS(WS-SUB1)
                          SCRN-PROGRAM(WS-SUB1)
                          SCRN-NAME(WS-SUB1).

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
               MOVE -1 TO ACT1L
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

           IF PC-PREV-PGRMID = 'PDA024'
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

           IF SCRN-SCENARIO(WS-SUB1) > SPACES
               MOVE DFHBMFSE TO ACT-ATTR(WS-SUB1)
               INSPECT SCRN-ACTION(WS-SUB1)
                   CONVERTING WMF-SPACES-LOWVALUE-R TO '__'
           ELSE
               MOVE DFHBMASF TO ACT-ATTR(WS-SUB1)
               MOVE SPACES TO SCRN-ACTION(WS-SUB1)
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
                   MAP('PDA024')
                   MAPSET('PDA024M')
                   FROM(PDA024O)
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
               MOVE 'PDA024' TO WPCE-PROGRAM-ID
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
                   MAP('PDA024')
                   MAPSET('PDA024M')
                   FROM(PDA024O)
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
               MOVE 'PDA024' TO WPCE-PROGRAM-ID
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
                   MAP('PDA024')
                   MAPSET('PDA024M')
                   INTO(PDA024I)
                   NOHANDLE
                   RESP(WS-RESPONSE-CODE)
           END-EXEC.


      *****************************************************************
      *    IF ERROR, FORMAT ERROR INFORMATION AND TERMINATE           *
      *****************************************************************

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL) AND
               WS-RESPONSE-CODE NOT = DFHRESP(MAPFAIL)
                   MOVE 'CICS' TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA024' TO WPCE-PROGRAM-ID
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
               MOVE 'PDA024' TO WPCE-PROGRAM-ID
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
               MOVE 'PDA024' TO WPCE-PROGRAM-ID
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
               MOVE 'PDA024' TO WPCE-PROGRAM-ID
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
           MOVE 'PDA024' TO WPCE-PROGRAM-ID.
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
AS OF      MOVE 'PDA024' TO WPCE-PROGRAM-ID.                             AS OF
JAN        MOVE EIBRESP TO WPCE-RESPONSE-CODE.                           JAN
2001       MOVE 'ERROR' TO WPCE-COMMAND.                                 2001
           MOVE 'P99999' TO WPCE-PARAGRAPH.
LLR                                                                      LLR
           PERFORM P99500-PDA-ERROR THRU P99500-EXIT.

       P99999-ERROR-EXIT.
           EXIT.
           EJECT