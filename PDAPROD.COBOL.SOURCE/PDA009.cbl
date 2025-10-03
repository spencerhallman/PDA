       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA009.
      *
      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA009                                            *
      * TRANS   :   PD09                                              *
      * MAPSET  :   PDA009M                                           *
      *                                                               *
      * FUNCTION:   PROGRAM PDA009 IS THE PROCESS ORDER SCREEN WHICH  *
      *             FACILITATES THE ACTUAL PLACEMENT OF THE ORDER.    *
      *             ALL ITEMS CURRENTLY RESIDING ON THE PENDING ORDER *
      *             FILE WILL BE INCLUDED UNDER ONE ORDER NUMBER WHEN *
      *             THE USER CHOOSES THE SUBMIT ORDER FUNCTION FROM   *
      *             THIS SCREEN(PF4=SUBMIT ORDER).                    *
      *                                                               *
      * FILES   :   ITEM_SUPPLIER           -  DB2       (READ-ONLY)  *
      *             PURCHASE_TYPES_TABLE    -  DB2       (UPDATE)     *
      *             PENDING_ORDER_FILE      -  VSAM KSDS (UPDATE)     *
      *             CUSTOMER_FILE           -  VSAM KSDS (UPDATE)     *
      *             ORDER_DATABASE          -  IMS-DLI   (UPDATE)     *
      *                                                               *
      *                                                               *
      * TRANSACTIONS GENERATED:                                       *
      *             PD08       PENDING ORDER                          *
      *             PD02       ORDER MENU                             *
      *             PD01       MAIN MENU                              *
      *                                                               *
      *                                                               *
      * PFKEYS  :   PF04  =    SUBMIT ORDER                           *
      *             PF05  =    CANCEL ORDER                           *
      *             PF10  =    PROCEED TO PDA008, PENDING ORDER       *
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
      *  08/02/02   PAUL BARON            UPDATE CUSTOMER FILE (VSAM) *
      *                                   AND PURCHASE TYPE TABLE(DB2)*
      *                                   LAST ORDER AMOUNT FIELDS    *
      *                                   WHEN A NEW ORDER IS ADDED.  *
      *                                   UK CURRENCY DEMO REQUIREMENT*
      *                                   TO ADD THE FIELDS TO THE    *
      *                                   FILES, AND UPDATE.          *
      *                                                               *
      *  01/11/02   PAUL BARON            UPDATE ORDER ROOT CONTROL   *
      *                                   SEGMENT (ZERO KEY) WITH     *
      *                                   LAST ORDER NUMBER RIGHT     *
      *                                   AFTER GET TO PREVENT        *
      *                                   DUPLICATE ORDER (II STATUS) *
      *                                                               *
      *  04/17/01   PAUL BARON            CLEAR COMMAREA FIELD:       *
      *                                   PC-PDA008-ORIGINATING-PGRMID*
      *                                   WHEN TRANSFERRING CONTROL   *
      *                                   TO ANYTHING BUT PENDING     *
      *                                   ORDER                       *
      *                                                               *
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
           05  EIBAID-SW               PIC X     VALUE ' '.
               88  CLEAR-PKEY                    VALUE '_'.
               88  ENTER-PKEY                    VALUE ''''.
               88  SUBMIT-ORDER-PKEY             VALUE '4'.
               88  CANCEL-ORDER-PKEY             VALUE '5'.
               88  PENDING-ORDER-PKEY            VALUE ':'.
               88  ORDER-MENU-PKEY               VALUE '#'.
               88  MAIN-MENU-PKEY                VALUE '@'.
               88  VALID-PKEY-ENTERED            VALUE '_' '@' '4' '5'
                                                       ':' '#' ''''.
           EJECT
      *****************************************************************
      *    MISCELLANEOUS WORK FIELDS                                  *
      *****************************************************************

       01  WS-MISCELLANEOUS-FIELDS.
           05  WMF-USERID              PIC X(8)  VALUE SPACES.
           05  WMF-ABSTIME             PIC S9(15) VALUE +0      COMP-3.
           05  WMF-DATE-MMDDYY         PIC X(08) VALUE SPACES.
           05  FILLER                  REDEFINES WMF-DATE-MMDDYY.
               07  WMF-DATE-MMDDYY-MM  PIC XX.
               07  FILLER              PIC X.
               07  WMF-DATE-MMDDYY-DD  PIC XX.
               07  FILLER              PIC X.
               07  WMF-DATE-MMDDYY-YY  PIC XX.
           05  WMD-DATE-YYMMDD.
               07  WMF-DATE-YYMMDD-YY  PIC XX.
               07  WMF-DATE-YYMMDD-MM  PIC XX.
               07  WMF-DATE-YYMMDD-DD  PIC XX.
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
           05  WMF-PSB-NAME            PIC X(8)  VALUE 'PDA009'.
           05  WMF-ITEM-PREFIX         PIC X(5)  VALUE SPACES.
           05  WMF-ITEM-NUMBER         PIC X(32) VALUE SPACES.
           05  WMF-ITEM-SEQ            PIC 9(5)  VALUE ZEROES.
           05  WMF-CNT                 PIC S9(5) VALUE +0.
           05  WMF-ORDER-SEQUENCE      PIC S9(5) VALUE +0.
           05  WMF-ORDER-NUMBER        PIC 9(10) VALUE ZEROES.
           05  WMF-PURCHASE-TYPE       PIC 9(3)  VALUE ZEROES.
           05  WMF-PURCHASE-TYPE-EDIT  PIC X(3)  VALUE SPACES.
           05  FILLER                  REDEFINES WMF-PURCHASE-TYPE-EDIT.
               07  WMF-PURCHASE-TYPE-2 PIC 99.
               07  WMF-PURCHASE-FILL-2 PIC X.
           05  FILLER                  REDEFINES WMF-PURCHASE-TYPE-EDIT.
               07  WMF-PURCHASE-TYPE-1 PIC 9.
               07  WMF-PURCHASE-FILL-1 PIC XX.
           05  WMF-PO-NUMBER           PIC 9(13) VALUE ZEROES.
           05  WMF-PO-NUMBER-EDIT      PIC X(13) VALUE SPACES.
           05  FILLER                  REDEFINES WMF-PO-NUMBER-EDIT.
               07  WMF-PO-NUMBER-12    PIC 9(12).
               07  WMF-PO-FILLER-12    PIC X.
           05  FILLER                  REDEFINES WMF-PO-NUMBER-EDIT.
               07  WMF-PO-NUMBER-11    PIC 9(11).
               07  WMF-PO-FILLER-11    PIC XX.
           05  FILLER                  REDEFINES WMF-PO-NUMBER-EDIT.
               07  WMF-PO-NUMBER-10    PIC 9(10).
               07  WMF-PO-FILLER-10    PIC X(3).
           05  FILLER                  REDEFINES WMF-PO-NUMBER-EDIT.
               07  WMF-PO-NUMBER-09    PIC 9(9).
               07  WMF-PO-FILLER-09    PIC X(4).
           05  FILLER                  REDEFINES WMF-PO-NUMBER-EDIT.
               07  WMF-PO-NUMBER-08    PIC 9(8).
               07  WMF-PO-FILLER-08    PIC X(5).
           05  FILLER                  REDEFINES WMF-PO-NUMBER-EDIT.
               07  WMF-PO-NUMBER-07    PIC 9(7).
               07  WMF-PO-FILLER-07    PIC X(6).
           05  FILLER                  REDEFINES WMF-PO-NUMBER-EDIT.
               07  WMF-PO-NUMBER-06    PIC 9(6).
               07  WMF-PO-FILLER-06    PIC X(7).
           05  FILLER                  REDEFINES WMF-PO-NUMBER-EDIT.
               07  WMF-PO-NUMBER-05    PIC 9(5).
               07  WMF-PO-FILLER-05    PIC X(8).
           05  FILLER                  REDEFINES WMF-PO-NUMBER-EDIT.
               07  WMF-PO-NUMBER-04    PIC 9(4).
               07  WMF-PO-FILLER-04    PIC X(9).
           05  FILLER                  REDEFINES WMF-PO-NUMBER-EDIT.
               07  WMF-PO-NUMBER-03    PIC 9(3).
               07  WMF-PO-FILLER-03    PIC X(10).
           05  FILLER                  REDEFINES WMF-PO-NUMBER-EDIT.
               07  WMF-PO-NUMBER-02    PIC 99.
               07  WMF-PO-FILLER-02    PIC X(11).
           05  FILLER                  REDEFINES WMF-PO-NUMBER-EDIT.
               07  WMF-PO-NUMBER-01    PIC 9.
               07  WMF-PO-FILLER-01    PIC X(12).
           05  WS-PDA009-WORKAREA.
               07  WPW-ORDER-CONFIRM   PIC X(24) VALUE SPACES.
               07  WPW-ORDER-MSG       PIC X(18) VALUE SPACES.
               07  WPW-ORDER-NUMBER    PIC X(10) VALUE SPACES.

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
      *         MAP DSECTS -- BROWSE ITEMS BY CATEGORY - PDA009M      *
      *****************************************************************

           COPY PDA009M.
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

           COPY VPENDORD.
           EJECT

           COPY VCUSTOMR.
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
              INCLUDE DPURTYP
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

      *01  WS-PDA009-WORKAREA.
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

FTJ403     IF EIBAID = DFHCLEAR
ADD            MOVE PM002-EXIT-APPLICATION TO WMF-MESSAGE-AREA
CLEAR          PERFORM P80400-SEND-MESSAGE THRU P80400-EXIT
FUNC       END-IF.

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
                   MOVE 'PDA009' TO WPCE-PROGRAM-ID
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

JXW419     IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
ADD            MOVE 'CICS' TO WS-PDA-ERROR-TYPE
ERROR          MOVE 'PDA009' TO WPCE-PROGRAM-ID
CHECK          MOVE WS-RESPONSE-CODE TO WPCE-RESPONSE-CODE
FOR            MOVE 'CICS FORMATTIME ABSTIME' TO WPCE-COMMAND
DATE           MOVE 'P00050' TO WPCE-PARAGRAPH
TIME           PERFORM P99500-PDA-ERROR THRU P99500-EXIT
JXW419     END-IF.

           MOVE WMF-DATE-MMDDYY-YY TO WMF-DATE-YYMMDD-YY.
           MOVE WMF-DATE-MMDDYY-MM TO WMF-DATE-YYMMDD-MM.
           MOVE WMF-DATE-MMDDYY-DD TO WMF-DATE-YYMMDD-DD.

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

           MOVE WS-PDA009-WORKAREA TO PC-PROGRAM-WORKAREA.

       P00100-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00200-CICS-RETURN                             *
      *                                                               *
      *    FUNCTION :  ROUTINETO RETURN CONTROL TO CICS WITH THE      *
      *                NEXT TRANSACTION ID OPTION                     *
      *                                                               *
      *    CALLED BY:  P00000-MAINLINE                                *
      *                                                               *
      *****************************************************************

       P00200-CICS-RETURN.

           EXEC CICS
               RETURN
                   TRANSID('PD09')
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
               MOVE 'PDA009' TO WPCE-PROGRAM-ID
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
      *    IF PREVIOUS PROGRAM IS NOT PDA009, SET INQUIRY MODE        *
      *    OTHERWISE SET EDIT / UPDATE MODE                           *
      *****************************************************************

           IF PC-PREV-PGRMID = 'PDA009'
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
      *    FUNCTION :  ROUTINE TO CONTROL PROCESSINGTO SEND THE       *
      *                INITIAL SCREEN                                 *
      *                                                               *
      *    CALLED BY:  P00100-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P01000-FIRST-TIME.

      *****************************************************************
      *    INITIALIZE COMMAREA AND MAP                                *
      *****************************************************************

           MOVE LOW-VALUES TO PDA009I.
           MOVE WMF-DATE-MMDDYY TO PDADATEO.
           MOVE EIBTRMID TO PDATERMO.
           MOVE WMF-TIME-HHMMSS TO PDATIMEO.

      *****************************************************************
      *    FORMAT AND SEND THE FULL MAP -- LITERALS AND DATA          *
      *****************************************************************

           MOVE PC-USERID-NUMBER TO CUSTOMER-PREFIX
                                    PDASP1-PREFIX.
           MOVE PC-CUSTOMER-ID TO CUSTOMER-ID.
           MOVE SPACES TO WS-PDA009-WORKAREA.

           PERFORM P01100-READ-CUSTOMER THRU P01100-EXIT.

           MOVE CUSTOMER-ID TO PDACUSTO.
           MOVE CUSTOMER-NAME TO NAMEO.
           MOVE CUSTOMER-ADDRESS TO ADDRO.
           MOVE CUSTOMER-CITY TO CITYO.
           MOVE CUSTOMER-STATE TO STATEO.
           MOVE CUSTOMER-POSTAL-CODE TO ZIPO.
           MOVE CUSTOMER-EMAIL-ADDRESS TO EMAILO.
           MOVE CUSTOMER-SHIP-TO-NAME TO SHNAMEO.
           MOVE CUSTOMER-SHIP-TO-ADDRESS TO SHADDRO.
           MOVE CUSTOMER-SHIP-TO-CITY TO SHCITYO.
           MOVE CUSTOMER-SHIP-TO-STATE TO SHSTATEO.
           MOVE CUSTOMER-SHIP-TO-POSTAL-CODE TO SHZIPO.

           EXEC SQL
               CALL PDASP1 (:PDASP1-PREFIX,
                            :PDASP1-TOTAL-COST,
                            :PDASP1-STATUS)
           END-EXEC.

           IF PDASP1-STATUS = '0000'
               MOVE PDASP1-TOTAL-COST TO TOTCOSTO
           ELSE
               MOVE 0 TO TOTCOSTO
           END-IF.

           PERFORM P79000-DISPLAY-SCREEN THRU P79000-EXIT.

           MOVE 'PDA009' TO PC-PREV-PGRMID.

       P01000-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01100-READ-CUSTOMER                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ THE CUSTOMER FILE.             *
      *                                                               *
      *    CALLED BY:  P01000-FIRST-TIME                              *
      *                                                               *
      *****************************************************************

       P01100-READ-CUSTOMER.

           MOVE 'CICS' TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA009' TO WPCE-PROGRAM-ID.
           MOVE 'CICS READ' TO WPCE-COMMAND.
           MOVE 'P01100' TO WPCE-PARAGRAPH.

           EXEC CICS
               READ
                   FILE('PDACUST')
                   INTO(CUSTOMER-RECORD)
                   RIDFLD(CUSTOMER-KEY)
           END-EXEC.

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P01100-EXIT.
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

           MOVE 'PDA009' TO PC-PREV-PGRMID.
           MOVE PC-PROGRAM-WORKAREA TO WS-PDA009-WORKAREA.

      *****************************************************************
      *    RECEIVE THE INPUT MAP                                      *
      *****************************************************************

           PERFORM P80200-RECEIVE-MAP THRU P80200-EXIT.

           MOVE WMF-DATE-MMDDYY TO PDADATEO.                            DFH416
           MOVE EIBTRMID TO PDATERMO.                                   REFRESH
           MOVE WMF-TIME-HHMMSS TO PDATIMEO.                            DAT/TIME
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

           PERFORM P03110-EDIT-PFKEY THRU P03110-EXIT.

           IF ERROR-FOUND
               GO TO P03100-EXIT
           END-IF.

      *****************************************************************
      *    PF05 FROM THIS SCREEN TO CANCEL ORDER                      *
      *****************************************************************

           IF CANCEL-ORDER-PKEY
               IF WPW-ORDER-CONFIRM = PM030-ORDER-CANCELLED
                   PERFORM P03150-READ-PENDING-CNTL THRU P03150-EXIT
                   EXEC CICS
                       HANDLE CONDITION
                           NOTFND(P03190-EXIT)
                   END-EXEC
                   MOVE -1 TO WMF-ORDER-SEQUENCE
                   PERFORM P03190-CANCEL-ORDER THRU P03190-EXIT
                       UNTIL END-OF-PROCESS
                   MOVE PM030-ORDER-CANCELLED TO WS-PDA009-WORKAREA
                   MOVE 'PDA002' TO PC-NEXT-PGRMID
                   MOVE WS-PDA009-WORKAREA TO PC-PROGRAM-WORKAREA
                   MOVE SPACES   TO PC-PDA008-ORIGINATING-PGRMID
                   PERFORM P80300-XFER-CONTROL THRU P80300-EXIT
               ELSE
                   MOVE PM030-ORDER-CANCELLED TO WS-PDA009-WORKAREA
                   MOVE PM035-CONFIRM-CANCELL TO PDAMSGO
                   GO TO P03100-EXIT
               END-IF
           END-IF.

      *****************************************************************
      *    EDIT THE OPERATOR ENTERED DATA                             *
      *****************************************************************

           PERFORM P03120-EDIT-INPUT THRU P03120-EXIT.

           IF ERROR-FOUND
               GO TO P03100-EXIT
           END-IF.

      *****************************************************************
      *    PF04 FROM THIS SCREEN TO SUBMIT ORDER                      *
      *****************************************************************

           IF SUBMIT-ORDER-PKEY
               IF WPW-ORDER-CONFIRM = PM029-ORDER-PROCESSED
                   PERFORM P03130-SCHEDULE-PSB THRU P03130-EXIT
                   PERFORM P03140-GET-ORDER-CNTL THRU P03140-EXIT
                   PERFORM P03170-UPDATE-ORDER-CNTL THRU P03170-EXIT
                   PERFORM P03150-READ-PENDING-CNTL THRU P03150-EXIT
                   MOVE -1 TO WMF-ORDER-SEQUENCE
                   EXEC CICS
                       HANDLE CONDITION
                           NOTFND(P03160-EXIT)
                   END-EXEC
                   PERFORM P03160-SUBMIT-ORDER THRU P03160-EXIT
                       UNTIL END-OF-PROCESS
                   PERFORM P03180-TERMINATE-PSB THRU P03180-EXIT
                   MOVE 'N' TO WS-END-OF-PROCESS-SW
                   MOVE -1 TO WMF-ORDER-SEQUENCE
                   EXEC CICS
                       HANDLE CONDITION
                           NOTFND(P03190-EXIT)
                   END-EXEC
                   PERFORM P03190-CANCEL-ORDER THRU P03190-EXIT
                       UNTIL END-OF-PROCESS
                   MOVE PM029-ORDER-PROCESSED TO WS-PDA009-WORKAREA
                   MOVE ', ORDER NUMBER IS' TO WPW-ORDER-MSG
                   MOVE WMF-ORDER-NUMBER TO WPW-ORDER-NUMBER
                   MOVE 'PDA002' TO PC-NEXT-PGRMID
                   MOVE WS-PDA009-WORKAREA TO PC-PROGRAM-WORKAREA
                   MOVE SPACES   TO PC-PDA008-ORIGINATING-PGRMID
                   PERFORM P80300-XFER-CONTROL THRU P80300-EXIT
               ELSE
                   MOVE PM029-ORDER-PROCESSED TO WS-PDA009-WORKAREA
                   MOVE PM034-CONFIRM-PROCESS TO PDAMSGO
                   GO TO P03100-EXIT
               END-IF
           END-IF.

           MOVE PM031-USE-PFKEY TO PDAMSGO.

       P03100-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03110-EDIT-PFKEY                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO VALIDATE PROGRAM FUNCTION KEY USAGE *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03110-EDIT-PFKEY.

      *****************************************************************
      *    VALID KEYS ARE: ENTER, PF4, PF5, PF10, PF11, PF12, CLEAR   *
      *****************************************************************

           MOVE EIBAID TO EIBAID-SW.

           IF NOT VALID-PKEY-ENTERED                                    JLC406
               MOVE -1 TO PURTYPEL                                      ADD 88
               MOVE PM001-INVALID-PFKEY TO WMF-MESSAGE-AREA             LEVEL
               PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT            EDIT
               GO TO P03110-EXIT                                        CHECK
           END-IF.                                                      JLC406

      *****************************************************************
      *    PF10 FROM THIS SCREEN RETURNS USER TO THE PENDING ORDER    *
      *    SCREEN                                                     *
      *****************************************************************

           IF PENDING-ORDER-PKEY
               MOVE 'PDA008' TO PC-NEXT-PGRMID
               PERFORM P80300-XFER-CONTROL THRU P80300-EXIT
           END-IF.

      *****************************************************************
      *    PF11 FROM THIS SCREEN RETURNS USER TO THE ORDER MENU       *
      *****************************************************************

           IF ORDER-MENU-PKEY
               MOVE 'PDA002' TO PC-NEXT-PGRMID
               MOVE SPACES   TO PC-PDA008-ORIGINATING-PGRMID
               PERFORM P80300-XFER-CONTROL THRU P80300-EXIT
           END-IF.

      *****************************************************************
      *    PF12 FROM THIS SCREEN RETURNS USER TO THE MAIN MENU        *
      *****************************************************************

           IF MAIN-MENU-PKEY
               MOVE 'PDA001' TO PC-NEXT-PGRMID
               MOVE SPACES   TO PC-PDA008-ORIGINATING-PGRMID
               PERFORM P80300-XFER-CONTROL THRU P80300-EXIT
           END-IF.

       P03110-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03120-EDIT-INPUT                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO EDIT THE INPUT FIELDS               *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03120-EDIT-INPUT.

           IF ENTER-PKEY
               MOVE SPACES TO WS-PDA009-WORKAREA
                              PC-PROGRAM-WORKAREA
           END-IF.

           MOVE SPACES TO PURDESCO.

           MOVE PURTYPEI TO WMF-PURCHASE-TYPE-EDIT.

           INSPECT WMF-PURCHASE-TYPE-EDIT
               REPLACING LEADING SPACES BY ZEROES.

           EVALUATE TRUE
               WHEN PURTYPEI = LOW-VALUES
               WHEN PURTYPEI = SPACES
                   MOVE -1 TO PURTYPEL
                   MOVE DFHDFHI TO PURTYPEA
                   MOVE PM027-ENTER-PURCHASE-TYPE TO WMF-MESSAGE-AREA
                   PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT
                   GO TO P03120-EXIT
               WHEN WMF-PURCHASE-TYPE-EDIT NUMERIC
                   MOVE WMF-PURCHASE-TYPE-EDIT TO WMF-PURCHASE-TYPE
               WHEN WMF-PURCHASE-TYPE-2 NUMERIC
                   IF WMF-PURCHASE-FILL-2 = SPACES
                       MOVE WMF-PURCHASE-TYPE-2 TO WMF-PURCHASE-TYPE
                       MOVE WMF-PURCHASE-TYPE TO PURTYPEO
                   END-IF
               WHEN WMF-PURCHASE-TYPE-1 NUMERIC
                   IF WMF-PURCHASE-FILL-1 = SPACES
                       MOVE WMF-PURCHASE-TYPE-1 TO WMF-PURCHASE-TYPE
                       MOVE WMF-PURCHASE-TYPE TO PURTYPEO
                   END-IF
           END-EVALUATE.

           MOVE PC-USERID-NUMBER TO PURCHASE-TYPE-PREFIX.
           MOVE WMF-PURCHASE-TYPE TO PURCHASE-TYPE-TYPE.

           EXEC SQL
               SELECT  DESCRIPTION
               INTO    :PURCHASE-TYPE-DESCRIPTION
               FROM    PURCHASE_TYPE
               WHERE   PREFIX       = :PURCHASE-TYPE-PREFIX AND
                       TYPE         = :PURCHASE-TYPE-TYPE
           END-EXEC.

           EVALUATE TRUE
               WHEN SQLCODE = +0
                   MOVE WMF-PURCHASE-TYPE TO PURTYPEO
                   MOVE PURCHASE-TYPE-DESCRIPTION TO PURDESCO
               WHEN SQLCODE = +100
                   MOVE -1 TO PURTYPEL
                   MOVE DFHDFHI TO PURTYPEA
                   MOVE PM028-INVALID-PURCHASE-TYPE TO WMF-MESSAGE-AREA
                   PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT
               WHEN OTHER
                   MOVE 'DB2' TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA009' TO WPDE-PROGRAM-ID
                   MOVE SQLCODE TO WPDE-DB2-SQLCODE
                   MOVE 'SELECT' TO WPDE-FUNCTION
                   MOVE 'P03120' TO WPDE-PARAGRAPH
                   PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-EVALUATE.

           MOVE PONBRI TO WMF-PO-NUMBER-EDIT.

           INSPECT WMF-PO-NUMBER-EDIT
               REPLACING LEADING SPACES BY ZEROES.

           EVALUATE TRUE
               WHEN PONBRI = LOW-VALUES
               WHEN PONBRI = SPACES
                   MOVE -1 TO PONBRL
                   MOVE DFHDFHI TO PONBRA
                   MOVE PM046-INVALID-P-O-NUMBER TO WMF-MESSAGE-AREA
                   PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT
               WHEN WMF-PO-NUMBER-EDIT NUMERIC
                   MOVE WMF-PO-NUMBER-EDIT TO WMF-PO-NUMBER
               WHEN WMF-PO-NUMBER-12 NUMERIC
                   IF WMF-PO-FILLER-12 = SPACES
                       MOVE WMF-PO-NUMBER-12 TO WMF-PO-NUMBER
                   END-IF
               WHEN WMF-PO-NUMBER-11 NUMERIC
                   IF WMF-PO-FILLER-11 = SPACES
                       MOVE WMF-PO-NUMBER-11 TO WMF-PO-NUMBER
                   END-IF
               WHEN WMF-PO-NUMBER-10 NUMERIC
                   IF WMF-PO-FILLER-10 = SPACES
                       MOVE WMF-PO-NUMBER-10 TO WMF-PO-NUMBER
                   END-IF
               WHEN WMF-PO-NUMBER-09 NUMERIC
                   IF WMF-PO-FILLER-09 = SPACES
                       MOVE WMF-PO-NUMBER-09 TO WMF-PO-NUMBER
                   END-IF
               WHEN WMF-PO-NUMBER-08 NUMERIC
                   IF WMF-PO-FILLER-08 = SPACES
                       MOVE WMF-PO-NUMBER-08 TO WMF-PO-NUMBER
                   END-IF
               WHEN WMF-PO-NUMBER-07 NUMERIC
                   IF WMF-PO-FILLER-07 = SPACES
                       MOVE WMF-PO-NUMBER-07 TO WMF-PO-NUMBER
                   END-IF
               WHEN WMF-PO-NUMBER-06 NUMERIC
                   IF WMF-PO-FILLER-06 = SPACES
                       MOVE WMF-PO-NUMBER-06 TO WMF-PO-NUMBER
                   END-IF
               WHEN WMF-PO-NUMBER-05 NUMERIC
                   IF WMF-PO-FILLER-05 = SPACES
                       MOVE WMF-PO-NUMBER-05 TO WMF-PO-NUMBER
                   END-IF
               WHEN WMF-PO-NUMBER-04 NUMERIC
                   IF WMF-PO-FILLER-04 = SPACES
                       MOVE WMF-PO-NUMBER-04 TO WMF-PO-NUMBER
                   END-IF
               WHEN WMF-PO-NUMBER-03 NUMERIC
                   IF WMF-PO-FILLER-03 = SPACES
                       MOVE WMF-PO-NUMBER-03 TO WMF-PO-NUMBER
                   END-IF
               WHEN WMF-PO-NUMBER-02 NUMERIC
                   IF WMF-PO-FILLER-02 = SPACES
                       MOVE WMF-PO-NUMBER-02 TO WMF-PO-NUMBER
                   END-IF
               WHEN WMF-PO-NUMBER-01 NUMERIC
                   IF WMF-PO-FILLER-01 = SPACES
                       MOVE WMF-PO-NUMBER-01 TO WMF-PO-NUMBER
                   END-IF
           END-EVALUATE.

           IF WMF-PO-NUMBER > 0
               MOVE WMF-PO-NUMBER TO PONBRO
           ELSE
               MOVE -1 TO PONBRL
               MOVE DFHDFHI TO PONBRA
               MOVE PM046-INVALID-P-O-NUMBER TO WMF-MESSAGE-AREA
               PERFORM P70000-ERROR-ROUTINE THRU P70000-EXIT
           END-IF.

       P03120-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03130-SCHEDULE-PSB                            *
      *                                                               *
      *    FUNCTION :  SCHEDULE THE PSB SO THAT DLI CAN BE EXECUTED.  *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03130-SCHEDULE-PSB.

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
               MOVE 'PDA009' TO WPIE-PROGRAM-ID
               MOVE 'P03130' TO WPIE-PARAGRAPH
               MOVE 'SCHD' TO WPIE-FUNCTION-CODE
               MOVE SPACES TO WPIE-SEGMENT-NAME
                              WPIE-DATABASE-NAME
               MOVE DIBSTAT TO WPIE-STATUS-CODE
               MOVE 'PSB SCHEDULING ERROR' TO WPIE-COMMAND
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P03130-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03140-GET-ORDER-CNTL                          *
      *                                                               *
      *    FUNCTION :  READ THE ZERO RECORD FROM THE ORDER DATABASE.  *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03140-GET-ORDER-CNTL.

           MOVE PC-USERID-NUMBER TO ORDER-PREFIX.
           MOVE 0 TO ORDER-NUMBER.

           EXEC DLI
               GU USING
                   PCB(1)
                   SEGMENT(ORDER)
                   INTO(ORDER-SEGMENT)
                   SEGLENGTH(123)
                   WHERE(ORDKEY=ORDER-KEY)
                   FIELDLENGTH(15)
           END-EXEC.

      *****************************************************************
      *    CHECK STATUS CODE FOR SUCCESS, ALL OTHERS ARE AN ERROR.    *
      *****************************************************************

           IF DIBSTAT = SPACES
               MOVE ORDER-PURCHASE-NUMBER TO WMF-ORDER-NUMBER
               ADD 1 TO WMF-ORDER-NUMBER
           ELSE
               MOVE 'Y' TO WS-ERROR-FOUND-SW
               MOVE 'IMS' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA009' TO WPIE-PROGRAM-ID
               MOVE 'P03140' TO WPIE-PARAGRAPH
               MOVE 'GU' TO WPIE-FUNCTION-CODE
               MOVE 'ORDER' TO WPIE-SEGMENT-NAME
               MOVE 'ORDER1DB' TO WPIE-DATABASE-NAME
               MOVE DIBSTAT TO WPIE-STATUS-CODE
               MOVE 'GU ORDER ROOT SEGMENT' TO WPIE-COMMAND
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P03140-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03150-READ-PENDING-CNTL                       *
      *                                                               *
      *    FUNCTION :  READ THE ZERO RECORD FROM THE PENDING ORDER    *
      *                FILE TO OBTAIN MAX NUMBER OF ITEMS.            *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03150-READ-PENDING-CNTL.

           MOVE 'CICS' TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA009' TO WPCE-PROGRAM-ID.
           MOVE 'CICS READ' TO WPCE-COMMAND.
           MOVE 'P03150' TO WPCE-PARAGRAPH.
           MOVE PC-USERID-NUMBER TO PENDING-ORDER-PREFIX.
           MOVE 0 TO PENDING-ORDER-SEQUENCE.

           EXEC CICS
               READ
                   FILE('PDAPEND')
                   INTO(PENDING-ORDER-RECORD)
                   RIDFLD(PENDING-ORDER-KEY)
                   GTEQ
           END-EXEC.

           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               MOVE PENDING-ORDER-QUANTITY TO WMF-CNT
           ELSE
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P03150-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03160-SUBMIT-ORDER                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO SUBMIT THE ORDER                    *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03160-SUBMIT-ORDER.

           ADD 1 TO WMF-ORDER-SEQUENCE.

           IF WMF-ORDER-SEQUENCE > WMF-CNT
               PERFORM P03164-UPDATE-ORDER-ROOT THRU P03164-EXIT
               MOVE 'Y' TO WS-END-OF-PROCESS-SW
               GO TO P03160-EXIT
           END-IF.

           MOVE WMF-ORDER-SEQUENCE TO PENDING-ORDER-SEQUENCE.

           PERFORM P03161-READ-PENDING-ORDER THRU P03161-EXIT.

           IF PENDING-ORDER-SEQUENCE = 0
               PERFORM P03162-BUILD-ORDER-ROOT THRU P03162-EXIT
               GO TO P03160-EXIT
           END-IF.

           PERFORM P03163-BUILD-ORDER-ITEM THRU P03163-EXIT.

      *****************************************************************
      *    UPDATE CUSTOMER FILE AND PURCHASE TYPE TABLE LAST ORDER    *
      *    AMOUNT FIELDS                                              *
      *****************************************************************

           MOVE PC-USERID-NUMBER TO CUSTOMER-PREFIX.
           MOVE PC-CUSTOMER-ID TO CUSTOMER-ID.
           PERFORM P04000-UPDATE-CUSTOMER  THRU P04000-EXIT.

           MOVE PC-USERID-NUMBER   TO PURCHASE-TYPE-PREFIX.
           MOVE WMF-PURCHASE-TYPE  TO PURCHASE-TYPE-TYPE.
           MOVE ORDER-TOTAL-AMOUNT TO PURCHASE-TYPE-LAST-ORDER-AMT.
           PERFORM P04100-UPDATE-PURCHASE-TYPE
                                           THRU P04100-EXIT.

       P03160-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03161-READ-PENDING-ORDER                      *
      *                                                               *
      *    FUNCTION :  READ RECORD FROM THE PENDING ORDER FILE.       *
      *                                                               *
      *    CALLED BY:  P03160-SUBMIT-ORDER                            *
      *                                                               *
      *****************************************************************

       P03161-READ-PENDING-ORDER.

           MOVE 'CICS' TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA009' TO WPCE-PROGRAM-ID.
           MOVE 'CICS READ' TO WPCE-COMMAND.
           MOVE 'P03161' TO WPCE-PARAGRAPH.

           EXEC CICS
               READ
                   FILE('PDAPEND')
                   INTO(PENDING-ORDER-RECORD)
                   RIDFLD(PENDING-ORDER-KEY)
           END-EXEC.

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P03161-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03162-BUILD-ORDER-ROOT                        *
      *                                                               *
      *    FUNCTION :  BUILD AND INSERT THE ORDER ROOT SEGMENT.       *
      *                                                               *
      *    CALLED BY:  P03160-SUBMIT-ORDER                            *
      *                                                               *
      *****************************************************************

       P03162-BUILD-ORDER-ROOT.

           MOVE SPACES TO ORDER-SEGMENT.
           MOVE PC-USERID-NUMBER TO ORDER-PREFIX.
           MOVE WMF-ORDER-NUMBER TO ORDER-NUMBER.
           MOVE WMF-PO-NUMBER TO ORDER-PURCHASE-NUMBER.
           MOVE WMD-DATE-YYMMDD TO ORDER-DATE-YYMMDD.
           MOVE 'IN PROGRESS' TO ORDER-STATUS.
           MOVE PC-USERID-NUMBER TO PDASP1-PREFIX.

           EXEC SQL
               CALL PDASP1 (:PDASP1-PREFIX,
                            :PDASP1-TOTAL-COST,
                            :PDASP1-STATUS)
           END-EXEC.

           IF PDASP1-STATUS = '0000'
               MOVE PDASP1-TOTAL-COST TO ORDER-TOTAL-AMOUNT
           ELSE
               MOVE 0 TO ORDER-TOTAL-AMOUNT
           END-IF.

           MOVE +0 TO ORDER-NEXT-ITEM-SEQUENCE.
           MOVE PC-USERID-NUMBER TO ORDER-CUSTOMER-PREFIX.
           MOVE PC-CUSTOMER-ID TO ORDER-CUSTOMER-ID.
           MOVE PC-USERID-NUMBER TO ORDER-PURCHASE-TYPE-PREFIX.
           MOVE WMF-PURCHASE-TYPE TO ORDER-PURCHASE-TYPE.
           MOVE ZEROES TO ORDER-SHIPPER-NUMBER.

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
               MOVE 'PDA009' TO WPIE-PROGRAM-ID
               MOVE 'P03162' TO WPIE-PARAGRAPH
               MOVE 'ISRT' TO WPIE-FUNCTION-CODE
               MOVE 'ORDER' TO WPIE-SEGMENT-NAME
               MOVE 'ORDER1DB' TO WPIE-DATABASE-NAME
               MOVE DIBSTAT TO WPIE-STATUS-CODE
               MOVE 'ISRT ORDER ROOT SEGMENT' TO WPIE-COMMAND
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P03162-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03163-BUILD-ORDER-ITEM                        *
      *                                                               *
      *    FUNCTION :  BUILE AND INSERT THE ORDER ITEM SEGMENT.       *
      *                                                               *
      *    CALLED BY:  P03160-SUBMIT-ORDER                            *
      *                                                               *
      *****************************************************************

       P03163-BUILD-ORDER-ITEM.

           MOVE SPACES TO ORDER-ITEM-SEGMENT.
           MOVE PC-USERID-NUMBER TO ORDER-ITEM-PREFIX.

           ADD 1 TO WMF-ITEM-SEQ.

           MOVE WMF-ITEM-SEQ TO ORDER-ITEM-SEQUENCE.
           MOVE PENDING-ORDER-QUANTITY TO ORDER-ITEM-QUANTITY.
           MOVE PENDING-ORDER-PREFIX TO ITEM-SUPPLIER-ITEM-PREFIX.
           MOVE PENDING-ORDER-ITEM-NUMBER TO ITEM-SUPPLIER-ITEM-NUMBER.
           MOVE PENDING-ORDER-SUPPLIER-ID TO ITEM-SUPPLIER-SUPPLIER-ID.

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
               MOVE 'PDA009' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'SELECT ITEM-SUPPLIER' TO WPDE-FUNCTION
               MOVE 'P03163' TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

           MOVE ITEM-SUPPLIER-UNIT-PRICE TO ORDER-ITEM-UNIT-PRICE.
           MOVE PENDING-ORDER-ITEM-KEY TO ORDER-ITEM-ITEM-KEY.
           MOVE PENDING-ORDER-SUPPLIER-KEY TO ORDER-ITEM-SUPPLIER-KEY.

           EXEC DLI
               ISRT USING
                   PCB(1)
                   SEGMENT(ORDITEM)
                   FROM(ORDER-ITEM-SEGMENT)
                   SEGLENGTH(95)
           END-EXEC.

      *****************************************************************
      *    CHECK STATUS CODE FOR SUCCESS, ALL OTHERS ARE AN ERROR     *
      *****************************************************************

           IF DIBSTAT NOT = SPACES
               MOVE 'Y' TO WS-ERROR-FOUND-SW
               MOVE 'IMS' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA009' TO WPIE-PROGRAM-ID
               MOVE 'P03163' TO WPIE-PARAGRAPH
               MOVE 'ISRT' TO WPIE-FUNCTION-CODE
               MOVE 'ORDER' TO WPIE-SEGMENT-NAME
               MOVE 'ORDER1DB' TO WPIE-DATABASE-NAME
               MOVE DIBSTAT TO WPIE-STATUS-CODE
               MOVE 'ISRT ORDER ITEM SEGMENT' TO WPIE-COMMAND
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P03163-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03164-UPDATE-ORDER-ROOT                       *
      *                                                               *
      *    FUNCTION :  UPDATE THE ORDER ROOT RECORD WITH THE NEXT     *
      *                ITEM SEQUENCE.                                 *
      *                                                               *
      *    CALLED BY:  P03160-SUBMIT-ORDER                            *
      *                                                               *
      *****************************************************************

       P03164-UPDATE-ORDER-ROOT.

           MOVE PC-USERID-NUMBER TO ORDER-PREFIX.
           MOVE WMF-ORDER-NUMBER TO ORDER-NUMBER.

           EXEC DLI
               GU USING
                   PCB(1)
                   SEGMENT(ORDER)
                   INTO(ORDER-SEGMENT)
                   SEGLENGTH(123)
                   WHERE(ORDKEY=ORDER-KEY)
                   FIELDLENGTH(15)
           END-EXEC.

      *****************************************************************
      *    CHECK STATUS CODE FOR SUCCESS, ALL OTHERS ARE AN ERROR.    *
      *****************************************************************

           IF DIBSTAT = SPACES
               ADD 1 TO WMF-ITEM-SEQ
               MOVE WMF-ITEM-SEQ TO ORDER-NEXT-ITEM-SEQUENCE
           ELSE
               MOVE 'Y' TO WS-ERROR-FOUND-SW
               MOVE 'IMS' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA009' TO WPIE-PROGRAM-ID
               MOVE 'P03164' TO WPIE-PARAGRAPH
               MOVE 'GU' TO WPIE-FUNCTION-CODE
               MOVE 'ORDER' TO WPIE-SEGMENT-NAME
               MOVE 'ORDER1DB' TO WPIE-DATABASE-NAME
               MOVE DIBSTAT TO WPIE-STATUS-CODE
               MOVE 'GU ORDER ROOT SEGMENT' TO WPIE-COMMAND
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

      *****************************************************************
      *    REPLACE ORDER CONTROL SEGMENT.                             *
      *****************************************************************

           EXEC DLI
               REPL USING
                   PCB(1)
                   SEGMENT(ORDER)
                   FROM(ORDER-SEGMENT)
                   SEGLENGTH(123)
           END-EXEC.

      *****************************************************************
      *    CHECK STATUS CODE FOR SUCCESS, ALL OTHERS ARE AN ERROR.    *
      *****************************************************************

           IF DIBSTAT NOT = SPACES
               MOVE 'Y' TO WS-ERROR-FOUND-SW
               MOVE 'IMS' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA009' TO WPIE-PROGRAM-ID
               MOVE 'P03164' TO WPIE-PARAGRAPH
               MOVE 'REPL' TO WPIE-FUNCTION-CODE
               MOVE 'ORDER' TO WPIE-SEGMENT-NAME
               MOVE 'ORDER1DB' TO WPIE-DATABASE-NAME
               MOVE DIBSTAT TO WPIE-STATUS-CODE
               MOVE 'REPL ORDER ROOT SEGMENT' TO WPIE-COMMAND
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P03164-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03170-UPDATE-ORDER-CNTL                       *
      *                                                               *
      *    FUNCTION :  UPDATE THE ORDER CONTROL RECORD WITH THE NEW   *
      *                ORDER NUMBER.                                  *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03170-UPDATE-ORDER-CNTL.

           MOVE PC-USERID-NUMBER TO ORDER-PREFIX.
           MOVE 0 TO ORDER-NUMBER.

           EXEC DLI
               GU USING
                   PCB(1)
                   SEGMENT(ORDER)
                   INTO(ORDER-SEGMENT)
                   SEGLENGTH(123)
                   WHERE(ORDKEY=ORDER-KEY)
                   FIELDLENGTH(15)
           END-EXEC.

      *****************************************************************
      *    CHECK STATUS CODE FOR SUCCESS, ALL OTHERS ARE AN ERROR.    *
      *****************************************************************

           IF DIBSTAT = SPACES
               MOVE WMF-ORDER-NUMBER TO ORDER-PURCHASE-NUMBER
           ELSE
               MOVE 'Y' TO WS-ERROR-FOUND-SW
               MOVE 'IMS' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA009' TO WPIE-PROGRAM-ID
               MOVE 'P03170' TO WPIE-PARAGRAPH
               MOVE 'GU' TO WPIE-FUNCTION-CODE
               MOVE 'ORDER' TO WPIE-SEGMENT-NAME
               MOVE 'ORDER1DB' TO WPIE-DATABASE-NAME
               MOVE DIBSTAT TO WPIE-STATUS-CODE
               MOVE 'GU ORDER ROOT SEGMENT' TO WPIE-COMMAND
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

      *****************************************************************
      *    REPLACE ORDER CONTROL SEGMENT.                             *
      *****************************************************************

           EXEC DLI
               REPL USING
                   PCB(1)
                   SEGMENT(ORDER)
                   FROM(ORDER-SEGMENT)
                   SEGLENGTH(123)
           END-EXEC.

      *****************************************************************
      *    CHECK STATUS CODE FOR SUCCESS, ALL OTHERS ARE AN ERROR.    *
      *****************************************************************

           IF DIBSTAT NOT = SPACES
               MOVE 'Y' TO WS-ERROR-FOUND-SW
               MOVE 'IMS' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA009' TO WPIE-PROGRAM-ID
               MOVE 'P03170' TO WPIE-PARAGRAPH
               MOVE 'REPL' TO WPIE-FUNCTION-CODE
               MOVE 'ORDER' TO WPIE-SEGMENT-NAME
               MOVE 'ORDER1DB' TO WPIE-DATABASE-NAME
               MOVE DIBSTAT TO WPIE-STATUS-CODE
               MOVE 'REPL ORDER ROOT SEGMENT' TO WPIE-COMMAND
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P03170-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03180-TERMINATE-PSB                           *
      *                                                               *
      *    FUNCTION :  TERMINATE THE PSB.                             *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03180-TERMINATE-PSB.

           EXEC DLI
               TERMINATE
           END-EXEC.

      *****************************************************************
      *    CHECK FOR PSB SCHEDULING ERROR                             *
      *****************************************************************

           IF DIBSTAT NOT = SPACES
               MOVE 'Y' TO WS-ERROR-FOUND-SW
               MOVE 'IMS' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA009' TO WPIE-PROGRAM-ID
               MOVE 'P03180' TO WPIE-PARAGRAPH
               MOVE 'TERM' TO WPIE-FUNCTION-CODE
               MOVE SPACES TO WPIE-SEGMENT-NAME
                              WPIE-DATABASE-NAME
               MOVE DIBSTAT TO WPIE-STATUS-CODE
               MOVE 'PSB TERMINATION ERROR' TO WPIE-COMMAND
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P03180-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03190-CANCEL-ORDER                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CANCEL THE ORDER                    *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03190-CANCEL-ORDER.

           ADD 1 TO WMF-ORDER-SEQUENCE.

           IF WMF-ORDER-SEQUENCE > WMF-CNT
               MOVE 'Y' TO WS-END-OF-PROCESS-SW
               GO TO P03190-EXIT
           END-IF.

           MOVE WMF-ORDER-SEQUENCE TO PENDING-ORDER-SEQUENCE.

           PERFORM P03191-READUPD-PENDING-ORDER THRU P03191-EXIT.

           PERFORM P03192-DELETE-PENDING-ORDER THRU P03192-EXIT.

       P03190-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03191-READUPD-PENDING-ORDER                   *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ THE PENDING ORDER FILE FOR     *
      *                UPDATE.                                        *
      *                                                               *
      *    CALLED BY:  P03190-CANCEL-ORDER                            *
      *                                                               *
      *****************************************************************

       P03191-READUPD-PENDING-ORDER.

           MOVE 'CICS' TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA009' TO WPCE-PROGRAM-ID.
           MOVE 'CICS READ' TO WPCE-COMMAND.
           MOVE 'P03191' TO WPCE-PARAGRAPH.

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

       P03191-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03192-DELETE-PENDING-ORDER                    *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DELETE THE PENDING ORDER FILE.      *
      *                                                               *
      *    CALLED BY:  P03190-CANCEL-ORDER                            *
      *                                                               *
      *****************************************************************

       P03192-DELETE-PENDING-ORDER.

           MOVE 'CICS' TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA009' TO WPCE-PROGRAM-ID.
           MOVE 'CICS DELETE' TO WPCE-COMMAND.
           MOVE 'P03192' TO WPCE-PARAGRAPH.

           EXEC CICS
               DELETE
                   FILE('PDAPEND')
           END-EXEC.

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P03192-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P04000-UPDATE-CUSTOMER                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO UPDATE THE CUSTOMER RECORD LAST     *
      *                ORDER AMOUNT WHEN AN ORDER IS ADDED            *
      *                                                               *
      *    CALLED BY:  P03160-SUBMIT-ORDER                            *
      *                                                               *
      *****************************************************************

       P04000-UPDATE-CUSTOMER.

           MOVE 'CICS' TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA009' TO WPCE-PROGRAM-ID.
           MOVE 'CICS READ' TO WPCE-COMMAND.
           MOVE 'P04000' TO WPCE-PARAGRAPH.


      *****************************************************************
      *    READ CUSTOMER RECORD FOR UPDATE                            *
      *****************************************************************

           EXEC CICS
               READ
                   FILE('PDACUST')
                   INTO(CUSTOMER-RECORD)
                   RIDFLD(CUSTOMER-KEY)
                   UPDATE
           END-EXEC.

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.


      *****************************************************************
      *    UPDATE LAST ORDER AMOUNT AND REWRITE RECORD                *
      *****************************************************************

           MOVE 'CICS REWRITE' TO WPCE-COMMAND.
           MOVE ORDER-TOTAL-AMOUNT     TO CUSTOMER-LAST-ORDER-AMT.

           EXEC CICS
               REWRITE
                   FILE('PDACUST')
                   FROM(CUSTOMER-RECORD)
           END-EXEC.

           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT
           END-IF.

       P04000-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P04100-UPDATE-PURCHASE-TYPE                    *
      *                                                               *
      *    FUNCTION :  ROUTINE TO UPDATE THE PURCHASE TYPE   LAST     *
      *                ORDER AMOUNT WHEN AN ORDER IS ADDED            *
      *                                                               *
      *    CALLED BY:  P03160-SUBMIT-ORDER                            *
      *                                                               *
      *****************************************************************

       P04100-UPDATE-PURCHASE-TYPE.

      *****************************************************************
      *    UPDATE THE PURCHASE TYPE ROW WITH THE ORDER AMOUNT         *
      *****************************************************************

           EXEC SQL UPDATE  PURCHASE_TYPE
               SET   LAST_ORDER_AMT = :PURCHASE-TYPE-LAST-ORDER-AMT

               WHERE PREFIX         = :PURCHASE-TYPE-PREFIX AND
                     TYPE           = :PURCHASE-TYPE-TYPE
           END-EXEC.


           IF SQLCODE = +0
               NEXT SENTENCE
           ELSE
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE
               MOVE 'PDA009' TO WPDE-PROGRAM-ID
               MOVE SQLCODE TO WPDE-DB2-SQLCODE
               MOVE 'UPDATE' TO WPDE-FUNCTION
               MOVE 'P04100' TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR THRU P99500-EXIT.


       P04100-EXIT.
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
      *    CALLED BY:  P75200-SCROLL-FORWARD                          *
      *                                                               *
      *                                                               *
      *****************************************************************

       P78000-CLEAR-SCREEN.

      *****************************************************************
      *    MAKE FINAL ADJUSTMENTS TO ATTRIBUTES AND FIELDS            *
      *****************************************************************


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

           IF PONBRL NOT = -1
               MOVE -1 TO PURTYPEL
           END-IF.

      *****************************************************************
      *    SEND FULL MAP IF 1ST TIME, OTHERWISE SEND DATAONLY         *
      *****************************************************************

           IF PC-PREV-PGRMID = 'PDA009'
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
                   MAP('PDA009')
                   MAPSET('PDA009M')
                   FROM(PDA009O)
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
               MOVE 'PDA009' TO WPCE-PROGRAM-ID
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
                   MAP('PDA009')
                   MAPSET('PDA009M')
                   FROM(PDA009O)
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
               MOVE 'PDA009' TO WPCE-PROGRAM-ID
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
                   MAP('PDA009')
                   MAPSET('PDA009M')
                   INTO(PDA009I)
                   NOHANDLE
                   RESP(WS-RESPONSE-CODE)
           END-EXEC.


      *****************************************************************
      *    IF ERROR, FORMAT ERROR INFORMATION AND TERMINATE           *
      *****************************************************************

VLB404     IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL) AND
ADD            WS-RESPONSE-CODE NOT = DFHRESP(MAPFAIL)
ERROR              MOVE 'CICS' TO WS-PDA-ERROR-TYPE
HANDLE             MOVE 'PDA009' TO WPCE-PROGRAM-ID
LOGIC              MOVE WS-RESPONSE-CODE TO WPCE-RESPONSE-CODE
VLB404             MOVE 'CICS RECEIVE MAP' TO WPCE-COMMAND
VLB404             MOVE 'P80200' TO WPCE-PARAGRAPH
VLB404             PERFORM P99500-PDA-ERROR THRU P99500-EXIT
VLB404     END-IF.

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
               MOVE 'PDA009' TO WPCE-PROGRAM-ID
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
               MOVE 'PDA009' TO WPCE-PROGRAM-ID
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
               MOVE 'PDA009' TO WPCE-PROGRAM-ID
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
           MOVE 'PDA009' TO WPCE-PROGRAM-ID.
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
AS OF      MOVE 'PDA009' TO WPCE-PROGRAM-ID.                             AS OF
JAN        MOVE EIBRESP TO WPCE-RESPONSE-CODE.                           JAN
2001       MOVE 'ERROR' TO WPCE-COMMAND.                                 2001
           MOVE 'P99999' TO WPCE-PARAGRAPH.
LLR                                                                      LLR
           PERFORM P99500-PDA-ERROR THRU P99500-EXIT.

       P99999-ERROR-EXIT.
           EXIT.
           EJECT