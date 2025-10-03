       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA002.

      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA002                                            *
      * TRANS   :   PD01                                              *
      * MAPSET  :   PDA002M                                           *
      *                                                               *
      * FUNCTION:   PROGRAM PDA002 IS THE PRODUCT DEMONSTRATION       *
      *             APPLICATION ORDER MENU PROGRAM. THE ORDER MENU    *
      *             CONTROLS ALL PROCESSING RELATED TO ORDER ADD,     *
      *             MAINTENANCE, INQUIRY, BROWSE, AND ORDER           *
      *             PLACEMENT FUNCTIONALITIES.                        *
      *                                                               *
      * FILES   :   NONE                                              *
      *                                                               *
      *                                                               *
      * TRANSACTIONS GENERATED:                                       *
      *             PD04       CUSTOMER IDENTIFICATION                *
      *             PD10       ORDER INQUIRY MAINTENANCE              *
      *             PD12       BROWSE SUBMITTED ORDERS                *
      *             PD01       MAIN MENU                              *
      *                                                               *
      *                                                               *
      * PFKEYS  :   PF12  =    EXIT MENU, RETURN TO MAIN MENU         *
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
       77  WS-MESSAGE-LTH              PIC S9(04)   COMP  VALUE +79.
       77  WS-RESPONSE-CODE            PIC S9(08)   COMP  VALUE +0.

      *****************************************************************
      *    SWITCHES                                                   *
      *****************************************************************
       01  WS-SWITCHES.

           05  WS-MENU-SELECTION-SW    PIC X(01)             VALUE ' '.
               88  SELECTION-IS-ADD-ORDER                    VALUE '1'.
               88  SELECTION-IS-ORDER-INQUIRY                VALUE '2'.
               88  SELECTION-IS-BROWSE-ORDERS                VALUE '3'.
               88  SELECTION-IS-PENDING-ORDER                VALUE '4'.
               88  SELECTION-IS-VALID                        VALUE '1'
                                                                   '2'
                                                                   '3'
                                                                   '4'.

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

           05  WMF-ABSTIME             PIC S9(15)  VALUE +0  COMP-3.
           05  WMF-DATE-MMDDYY         PIC X(08)   VALUE SPACES.
           05  WMF-TIME-HHMMSS         PIC X(08)   VALUE SPACES.
           05  WMF-MESSAGE-AREA        PIC X(79)   VALUE SPACES.

           05  WMF-UNDERSCORE-LOWVALUE.
               10  FILLER              PIC X(01)   VALUE '_'.
               10  FILLER              PIC X(01)   VALUE LOW-VALUES.
           05  WMF-UNDERSCORE-LOWVALUE-R
                                       REDEFINES
                                       WMF-UNDERSCORE-LOWVALUE
                                       PIC X(02).
           05  WMF-PROGRAM-AREA        PIC X(79) VALUE SPACES.


           05  WMF-SPACES-LOWVALUE.
               10  FILLER              PIC X(01)   VALUE SPACES.
               10  FILLER              PIC X(01)   VALUE LOW-VALUES.
           05  WMF-SPACES-LOWVALUE-R   REDEFINES
                                       WMF-SPACES-LOWVALUE
                                       PIC X(02).

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
      *         MAP DSECTS -- MAIN MENU PDA002M                       *
      *****************************************************************

           COPY PDA002M.
           EJECT

      *****************************************************************
      *    IMS / DLI DEFINITIONS                                      *
      *****************************************************************

      *****************************************************************
      *    XXXXXXXXXXXXXXXXXXXXXXXXXX                                 *
      *****************************************************************

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
      *    MAIN MENU USES COMMAREA FROM WORKING STORAGE IN ORDER TO   *
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


           EXEC CICS HANDLE CONDITION
                ERROR(P99100-GENERAL-ERROR)
           END-EXEC.


      *****************************************************************
      *    ALLOW USER TO EXIT APPLICATION WITH CLEAR KEY              *
      *    (SEND MESSAGE, ERASE SCREEN)                               *
      *****************************************************************

           IF EIBAID  = DFHCLEAR
               MOVE PM002-EXIT-APPLICATION
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P80400-SEND-MESSAGE
                   THRU P80400-SEND-MESSAGE-EXIT.


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
                   MOVE 'PDA002'       TO WPCE-PROGRAM-ID
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

           MOVE SPACES                 TO WS-MENU-SELECTION-SW.
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
               MOVE 'PDA002'           TO WPCE-PROGRAM-ID
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
      *    EITHER SEND INITIAL MENU SCREEN OR PERFORM SCREEN EDIT     *
      *    PROCESS                                                    *
      *****************************************************************

           IF INQUIRY-TRANS
               PERFORM  P01000-MENU-PROCESS
                   THRU P01000-MENU-PROCESS-EXIT
           ELSE
               PERFORM  P03000-EDIT-PROCESS
                   THRU P03000-EDIT-PROCESS-EXIT.


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
                     TRANSID       ('PD02')
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
               MOVE 'PDA002'           TO WPCE-PROGRAM-ID
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
      *    IF PREVIOUS PROGRAM IS NOT ORDER MENU, SET INQUIRY MODE    *
      *    OTHERWISE SET EDIT / UPDATE MODE                           *
      *****************************************************************

           IF PC-PREV-PGRMID           =  'PDA002'
               MOVE 'U'                TO WS-TRANS-INTENT-SW
           ELSE
               MOVE 'I'                TO WS-TRANS-INTENT-SW.


       P00500-CHK-TRANS-INTENT-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01000-MENU-PROCESS                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL PROCESSING TO SEND THE      *
      *                INITIAL PDA MAIN MENU SCREEN                   *
      *                                                               *
      *    CALLED BY:  P00100-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P01000-MENU-PROCESS.

      *****************************************************************
      *    CHECK COMMAREA FOR RETURN FROM ORDER PROCESSING.           *
      *****************************************************************

           IF PC-PREV-PGRMID           =  'PDA009'
               MOVE PC-PROGRAM-WORKAREA
                                       TO WMF-PROGRAM-AREA.

      *****************************************************************
      *    INITIALIZE COMMAREA AND MAP                                *
      *****************************************************************

           IF PC-PREV-PGRMID           =  'PDA001'
               MOVE SPACES             TO PC-CUSTOMER-ID.

           MOVE 'PDA002'               TO PC-PREV-PGRMID.
           MOVE SPACES                 TO PC-PREV-MENU-SEL
                                          PC-ITEM-CATEGORY
                                          PC-ITEM-SUB-CATEGORY
                                          PC-SELECTED-ITEM.
           MOVE LOW-VALUES             TO PDA002I.

           MOVE WMF-DATE-MMDDYY        TO PDADATEO.
           MOVE EIBTRMID               TO PDATERMO.
           MOVE WMF-TIME-HHMMSS        TO PDATIMEO.


      *****************************************************************
      *    FORMAT AND SEND THE FULL MAP -- LITERALS AND DATA          *
      *****************************************************************

           MOVE WMF-PROGRAM-AREA       TO PDAMSGO.
           MOVE -1                     TO MENUSELL.

           PERFORM  P80000-SEND-FULL-MAP
               THRU P80000-SEND-FULL-MAP-EXIT.

       P01000-MENU-PROCESS-EXIT.
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

           MOVE 'PDA002'               TO PC-PREV-PGRMID.

      *****************************************************************
      *    RECEIVE THE INPUT MAP                                      *
      *****************************************************************

           PERFORM  P80200-RECEIVE-MAP
               THRU P80200-RECEIVE-MAP-EXIT.

           MOVE WMF-DATE-MMDDYY        TO PDADATEO.
           MOVE EIBTRMID               TO PDATERMO.
           MOVE WMF-TIME-HHMMSS        TO PDATIMEO.


      *****************************************************************
      *    PERFORM THE SCREEN EDIT PROCESS (PFKEY AND DATA VALIDATION)*
      *****************************************************************

           PERFORM  P03100-EDIT-SCREEN
               THRU P03100-EDIT-SCREEN-EXIT.


      *****************************************************************
      *    IF ANY ERRORS ENCOUNTERED, RETURN THE MAIN MENU SCREEN     *
      *    OTHERWISE TRANSFER CONTROL TO THE APPROPRIATE SCREEN BASED *
      *    ON MENU SELECTION                                          *
      *****************************************************************

           IF ERROR-FOUND
               PERFORM  P80100-SEND-MAP-DATAONLY
                   THRU P80100-SEND-MAP-DATAONLY-EXIT
           ELSE
               PERFORM  P80300-XFER-CONTROL
                   THRU P80300-XFER-CONTROL-EXIT.


       P03000-EDIT-PROCESS-EXIT.
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

           INSPECT MENUSELI
               CONVERTING  WMF-UNDERSCORE-LOWVALUE-R TO SPACES.

      *****************************************************************
      *    EDIT THE OPERATOR PROGRAM FUNCTION KEY SELECTION (PFKEY)   *
      *****************************************************************

           PERFORM  P03200-EDIT-PFKEY
               THRU P03200-EDIT-PFKEY-EXIT.

           IF ERROR-FOUND
               GO TO P03100-EDIT-SCREEN-EXIT.


      *****************************************************************
      *    EDIT THE OPERATOR ENTERED MENU SELECTION                   *
      *****************************************************************

           PERFORM  P03300-EDIT-SELECTION
               THRU P03300-EDIT-SELECTION-EXIT.

           IF ERROR-FOUND
               GO TO P03100-EDIT-SCREEN-EXIT.

      *****************************************************************
      *    IF NO ERRORS -- DETERMINE NEXT PROGRAM TO SCHEDULE BASED   *
      *    ON THE MENU SELECTION                                      *
      *****************************************************************

           IF SELECTION-IS-ORDER-INQUIRY
               MOVE 'PDA010'           TO PC-NEXT-PGRMID
               MOVE SPACES             TO PC-ORIGINATING-PGRMID
           ELSE
               IF SELECTION-IS-BROWSE-ORDERS
                   MOVE 'PDA012'       TO PC-NEXT-PGRMID
                   MOVE 'PDA002'       TO PC-ORIGINATING-PGRMID
               ELSE
                   MOVE WS-MENU-SELECTION-SW
                                       TO PC-PREV-MENU-SEL
                   MOVE 'PDA004'       TO PC-NEXT-PGRMID.


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
      *    VALID KEYS ARE: ENTER, PF12                                *
      *****************************************************************

           IF EIBAID  = DFHCLEAR OR DFHENTER  OR  DFHPF12
               NEXT SENTENCE
           ELSE
               MOVE -1                 TO  MENUSELL
               MOVE PM001-INVALID-PFKEY
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P03200-EDIT-PFKEY-EXIT.


      *****************************************************************
      *    PFKEY USAGE AND ACTION / SELECTION CODE ENTRY NOT ALLOWED  *
      *    (MUTUALLY EXCLUSIVE OPERATIONS)                            *
      *****************************************************************

           IF (EIBAID  NOT = DFHENTER)    AND
              (MENUSELI    > SPACES)
               MOVE -1                 TO  MENUSELL
               MOVE PM003-ACTION-VS-PFKEY-CONFLICT
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P03200-EDIT-PFKEY-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    PF12 FROM ORDER MENU RETURNS USER TO THE MAIN MENU         *
      *****************************************************************

           IF EIBAID  = DFHPF12
               MOVE 'PDA001'           TO PC-NEXT-PGRMID
               PERFORM  P80300-XFER-CONTROL
                   THRU P80300-XFER-CONTROL-EXIT.


       P03200-EDIT-PFKEY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03300-EDIT-SELECTION                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO VALIDATE THE MAIN MENU SELECTION    *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P03300-EDIT-SELECTION.

      *****************************************************************
      *    MENU SELECTION MUST BE NUMERIC, AND VALUE OF 1 OR 2        *
      *****************************************************************

           MOVE MENUSELI               TO WS-MENU-SELECTION-SW.

           IF SELECTION-IS-VALID
               NEXT SENTENCE
           ELSE
               MOVE -1                 TO  MENUSELL
               MOVE DFHUNINT           TO  MENUSELA
               MOVE PM004-INVALID-MENU-SELECTION
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P03300-EDIT-SELECTION-EXIT.


       P03300-EDIT-SELECTION-EXIT.
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
      *    FUNCTION :  ROUTINE TO DISPLAY THE INITIAL MAIN MENU       *
      *                                                               *
      *    CALLED BY:  P01000-MENU-PROCESS                            *
      *                                                               *
      *****************************************************************

       P80000-SEND-FULL-MAP.

           EXEC CICS SEND
                     MAP           ('PDA002')
                     MAPSET        ('PDA002M')
                     FROM          (PDA002O)
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
               MOVE 'PDA002'           TO WPCE-PROGRAM-ID
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
      *    RESET ENTERABLE FIELDS BACK TO DEFAULT (IF NECESSARY)      *
      *****************************************************************

           INSPECT MENUSELI
               CONVERTING  WMF-SPACES-LOWVALUE-R TO '__'.


      *****************************************************************
      *    SEND THE MAP DATA ONLY, DO NOT ERASE SCREEN                *
      *****************************************************************

           EXEC CICS SEND
                     MAP           ('PDA002')
                     MAPSET        ('PDA002M')
                     FROM          (PDA002O)
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
               MOVE 'PDA002'           TO WPCE-PROGRAM-ID
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
                     MAP           ('PDA002')
                     MAPSET        ('PDA002M')
                     INTO          (PDA002I)
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
               MOVE 'PDA002'           TO WPCE-PROGRAM-ID
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
               MOVE 'PDA002'           TO WPCE-PROGRAM-ID
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
               MOVE 'PDA002'           TO WPCE-PROGRAM-ID
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
               MOVE 'PDA002'           TO WPCE-PROGRAM-ID
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
           MOVE 'PDA002'               TO WPCE-PROGRAM-ID.
           MOVE EIBRESP                TO WPCE-RESPONSE-CODE.
           MOVE 'UNHANDLED CICS ERROR' TO WPCE-COMMAND.
           MOVE 'P99100'               TO WPCE-PARAGRAPH.
           PERFORM  P99500-PDA-ERROR
               THRU P99500-PDA-ERROR-EXIT.


       P99100-GENERAL-ERROR-EXIT.
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
      *                AN ERROR SCREEN CONTAINING TEXT IS SENT        *
      *                TO THE USER INDICATING THE NATURE OF THE ERROR *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************

       P99500-PDA-ERROR.

      *****************************************************************
      *      SUSPEND ANY HANDLE CONDITIONS IN EFFECT                  *
      *****************************************************************

           EXEC CICS PUSH HANDLE
           END-EXEC.


      *****************************************************************
      *      ROLLBACK ANY TRANSACTION UPDATES                         *
      *****************************************************************

           EXEC CICS SYNCPOINT ROLLBACK
           END-EXEC.


      *****************************************************************
      *      FORMAT AND SEND ERROR TEXT                               *
      *****************************************************************

           IF PDA-DB2-ERROR
               MOVE WS-PDA-DB2-ERROR-01
                                       TO WPEA-ERROR-07-TEXT
               MOVE WS-PDA-DB2-ERROR-02
                                       TO WPEA-ERROR-08-TEXT
           ELSE
           IF PDA-IMS-ERROR
               MOVE WS-PDA-IMS-ERROR-01
                                       TO WPEA-ERROR-07-TEXT
               MOVE WS-PDA-IMS-ERROR-02
                                       TO WPEA-ERROR-08-TEXT
           ELSE
               MOVE WS-PDA-CICS-ERROR-01
                                       TO WPEA-ERROR-07-TEXT
               MOVE WS-PDA-CICS-ERROR-02
                                       TO WPEA-ERROR-08-TEXT.


           EXEC CICS DUMP
                     TRANSACTION
                     DUMPCODE('PDER')
           END-EXEC.



           EXEC CICS SEND
                     FROM    (WS-PDA-ERROR-AREA)
                     LENGTH  (WS-PDA-ERROR-LENGTH)
                     ERASE
           END-EXEC.



           EXEC CICS SEND
                     CONTROL
                     CURSOR  (0)
           END-EXEC.


      *****************************************************************
      * RETURN CONTROL TO CICS                                        *
      *****************************************************************

           EXEC CICS RETURN
           END-EXEC.


           GOBACK.

       P99500-PDA-ERROR-EXIT.
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
USED       MOVE 'CICS'                 TO WS-PDA-ERROR-TYPE.             USED
AS OF      MOVE 'PDA002'               TO WPCE-PROGRAM-ID.               AS OF
JAN        MOVE EIBRESP                TO WPCE-RESPONSE-CODE.            JAN
2001       MOVE 'ERROR'                TO WPCE-COMMAND.                  2001
           MOVE 'P99999'               TO WPCE-PARAGRAPH.
LLR                                                                      LLR
           PERFORM  P99500-PDA-ERROR
               THRU P99500-PDA-ERROR-EXIT.

       P99999-ERROR-EXIT.
           EXIT.
           EJECT