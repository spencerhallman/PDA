       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA001.

      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA001                                            *
      * TRANS   :   PD01                                              *
      * MAPSET  :   PDA001M                                           *
      *                                                               *
      * FUNCTION:   PROGRAM PDA001 IS THE PRODUCT DEMONSTRATION       *
      *             APPLICATION MAIN MENU PROGRAM. THE MAIN MENU      *
      *             CONTROLS THE NAVIGATION TO THE FOLLOWING          *
      *             COMPONENTS OF THE PRODUCT DEMONSTRATION           *
      *             APPLICATION:                                      *
      *             ORDERS      (ORDER ADD, INQUIRY, MAINTENANCE)     *
      *                                                               *
      *             MAINTENANCE (GENERAL MAINTENANCE, UTILITY         *
      *                          FUNCTIONS)                           *
      *                                                               *
      *             CUSTOMER ORDER INQUIRY                            *
      *                         (INQUIRY TO PROVIDE MQSERIES          *
      *                          FUNCTIONALITY)                       *
      *                                                               *
      *                                                               *
      * FILES   :   USERID_TABLE (DB2)    (INPUT / OUTPUT)            *
      *                                                               *
      *                                                               *
      * TRANSACTIONS GENERATED:                                       *
      *             PD02       ORDER MENU                             *
      *             PD03       MAINTENANCE MENU                       *
      *             PD13       BASE DATA REFRESH                      *
      *             PD16       CUSTOMER ORDER INQUIRY                 *
      *                                                               *
      *                                                               *
      * PFKEYS  :   PF3   =    EXIT APPLICATION, RETURN TO CICS       *
      *                                                               *
      *                                                               *
      *****************************************************************
      *             PROGRAM CHANGE LOG                                *
      *             -------------------                               *
      *                                                               *
      *  DATE       UPDATED BY            CHANGE DESCRIPTION          *
      *  --------   --------------------  --------------------------  *
      *  08/10/01   PAUL BARON            ADDED CUSTOMER ORDER INQUIRY*
      *                                   MENU SELECTION FOR MQSERIES *
      *                                   FUNCTIONALITY               *
      *                                                               *
      *  02/21/02   PAUL BARON            ADDED DB2 TABLE LOCK FOR    *
      *                                   USERID TABLE ON A NEW USER  *
      *                                   ADD. P04200-ADD-USERID      *
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
       77  WS-PDA-COMMAREA-LTH         PIC S9(04)   COMP  VALUE +2000.
       77  WS-RESPONSE-CODE            PIC S9(08)   COMP  VALUE +0.

      *****************************************************************
      *    SWITCHES                                                   *
      *****************************************************************
       01  WS-SWITCHES.

           05  WS-MENU-SELECTION-SW    PIC X(01)             VALUE ' '.
               88  SELECTION-IS-ORDERS                       VALUE '1'.
               88  SELECTION-IS-MAINTENANCE                  VALUE '2'.
               88  SELECTION-IS-CUSTOMER-INQUIRY             VALUE '3'.
               88  SELECTION-IS-VALID                        VALUE '1'
                                                                   '2'
                                                                   '3'.

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
           05  WMF-USERID              PIC X(08)   VALUE SPACES.
           05  WMF-MESSAGE-AREA        PIC X(79)   VALUE SPACES.

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

           05  WMF-USERID-NUMBER       PIC S9(09)  VALUE +0  COMP.
           05  WMF-NULL-IND            PIC S9(04)  VALUE +0  COMP.


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
      *         MAP DSECTS -- MAIN MENU PDA001M                       *
      *****************************************************************

           COPY PDA001M.
           EJECT

      *****************************************************************
      *    IMS / DLI DEFINITIONS                                      *
      *****************************************************************
      *****************************************************************
      *    NO IMS / DLI USED IN THE MODULE                            *
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
      *****************************************************************
      *         USER IDENTIFICATION TABLE        -- DCLGEN DUSERID    *
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
      *    PRODUCT DEMONSTRATION APPLICATION (PDA) COMMAREA LAYOUT    *
      *                                                               *
      *    MAIN MENU USES COMMAREA FROM WORKING STORAGE IN ORDER TO   *
      *    ESTABLISH THE INITIAL COMMAREA FOR THE APPLICATION         *
      *                                                               *
      *    ALL OTHER PROGRAMS SHOULD DEFINE / USE THE COMMAREA FROM   *
      *    THE LINKAGE SECTION, AS IT WILL BE ESTABLISHED             *
      *****************************************************************

       01  WS-PDA-COMMAREA.
           COPY PDACOMM.
           EJECT

      *****************************************************************
      *    L I N K A G E     S E C T I O N                            *
      *****************************************************************

       LINKAGE SECTION.

       01  DFHCOMMAREA.

           05  FILLER                  PIC X(2000).
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

           MOVE SPACES                 TO WS-MENU-SELECTION-SW.
           MOVE 'I'                    TO WS-TRANS-INTENT-SW.
           MOVE 'N'                    TO WS-ERROR-FOUND-SW.
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
               MOVE 'PDA001'           TO WPCE-PROGRAM-ID
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
                     TRANSID       ('PD01')
                     COMMAREA      (WS-PDA-COMMAREA)
                     LENGTH        (WS-PDA-COMMAREA-LTH)
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
               MOVE 'PDA001'           TO WPCE-PROGRAM-ID
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
      *    IF NO COMMAREA --- 1ST TIME THRU, SET INQUIRY --- EXIT     *
      *****************************************************************

           IF EIBCALEN                 =  ZEROES
               MOVE 'I'                TO WS-TRANS-INTENT-SW
               GO TO P00500-CHK-TRANS-INTENT-EXIT.


      *****************************************************************
      *    IF PREVIOUS PROGRAM IS NOT MAIN MENU, SET INQUIRY MODE     *
      *    OTHERWISE SET EDIT / UPDATE MODE                           *
      *****************************************************************

           MOVE DFHCOMMAREA            TO WS-PDA-COMMAREA.

           IF PC-PREV-PGRMID           =  'PDA001'
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
      *    INITIALIZE COMMAREA AND MAP                                *
      *****************************************************************

           MOVE SPACES                 TO WS-PDA-COMMAREA.
           MOVE WS-PDA-COMMAREA-LTH    TO PC-COMMAREA-LTH.
           MOVE 'PDA001'               TO PC-PREV-PGRMID.
           MOVE LOW-VALUES             TO PDA001I.

           MOVE WMF-DATE-MMDDYY        TO PDADATEO.
           MOVE EIBTRMID               TO PDATERMO.
           MOVE WMF-TIME-HHMMSS        TO PDATIMEO.


      *****************************************************************
      *    FORMAT AND SEND THE FULL MAP -- LITERALS AND DATA          *
      *****************************************************************

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

           MOVE 'PDA001'               TO PC-PREV-PGRMID.

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
      *    IF ANY ERRORS ENCOUNTERED, RETURN THE MAIN MENU SCREEN,    *
      *    AND EXIT THE PROCESS                                       *
      *****************************************************************

           IF ERROR-FOUND
               PERFORM  P80100-SEND-MAP-DATAONLY
                   THRU P80100-SEND-MAP-DATAONLY-EXIT
               GO TO P03000-EDIT-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    TRANSFER CONTROL TO NEXT PROGRAM BASED ON MENU SELECTION   *
      *****************************************************************

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
      *    PERFORM USERID VERIFICATION AGAINST THE USERID DB2 TABLE   *
      *****************************************************************

           PERFORM  P04000-VERIFY-USERID
               THRU P04000-VERIFY-USERID-EXIT.

           IF ERROR-FOUND
               GO TO P03100-EDIT-SCREEN-EXIT.

      *****************************************************************
      *    IF NO ERRORS -- DETERMINE NEXT PROGRAM TO SCHEDULE BASED   *
      *    ON THE MENU SELECTION                                      *
      *****************************************************************

           IF SELECTION-IS-ORDERS
               MOVE 'PDA002'           TO PC-NEXT-PGRMID
           ELSE
           IF SELECTION-IS-MAINTENANCE
               MOVE 'PDA003'           TO PC-NEXT-PGRMID
           ELSE
               MOVE 'PDA016'           TO PC-NEXT-PGRMID.

           MOVE USERID-ACTIVE-SCENARIOS TO PC-ACTIVE-SCENARIOS-GRP.


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
      *    VALID KEYS ARE: ENTER, CLEAR, PF3                          *
      *****************************************************************

           IF EIBAID  = DFHENTER  OR  DFHCLEAR  OR  DFHPF3
               NEXT SENTENCE
           ELSE
               MOVE -1                 TO  MENUSELL
               MOVE PM001-INVALID-PFKEY
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P03200-EDIT-PFKEY-EXIT.


      *****************************************************************
      *    ALLOW USER TO EXIT APPLICATION WITH CLEAR KEY              *
      *    (SEND MESSAGE, ERASE SCREEN)                               *
      *****************************************************************

           IF EIBAID  = DFHCLEAR
               MOVE PM002-EXIT-APPLICATION
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P80400-SEND-MESSAGE
                   THRU P80400-SEND-MESSAGE-EXIT
               GO TO P03200-EDIT-PFKEY-EXIT
           ELSE
               NEXT SENTENCE.

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
      *    PF3 FROM THE MAIN MENU EXITS THE APPLICATION               *
      *    (SEND MESSAGE, ERASE SCREEN)                               *
      *****************************************************************

           IF EIBAID  = DFHPF3
               MOVE PM002-EXIT-APPLICATION
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P80400-SEND-MESSAGE
                   THRU P80400-SEND-MESSAGE-EXIT
               GO TO P03200-EDIT-PFKEY-EXIT
           ELSE
               NEXT SENTENCE.


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
      *    PARAGRAPH:  P04000-VERIFY-USERID                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PERFORM USERID VERIFICATION:        *
      *                                                               *
      *                1) CHECK IF USERID (CICS SIGNON ID)            *
      *                   ALREADY EXISTS                              *
      *                                                               *
      *                2) IF USERID EXISTS, UPDATE THE DATE ACCESSED  *
      *                                                               *
      *                3) IF USERID DOES NOT EXIST, OBTAIN THE NEXT   *
      *                   USERID UNIQUE IDENTIFIER, INSERT THE USERID *
      *                   ROW, LINK TO THE BASE DATA REFRESH MODULE   *
      *                   (PDA013) TO LOAD DATA FOR THE NEW USER.     *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************

       P04000-VERIFY-USERID.

      *****************************************************************
      *    OBTAIN THE USERID (CICS SIGN ON ID)                        *
      *****************************************************************

           EXEC CICS ASSIGN
                     USERID        (WMF-USERID)
                     NOHANDLE
                     RESP          (WS-RESPONSE-CODE)
           END-EXEC.


           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               NEXT SENTENCE
           ELSE
               MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
               MOVE 'PDA001'           TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
               MOVE 'CICS ASSIGN USERID'
                                       TO WPCE-COMMAND
               MOVE 'P04000'           TO WPCE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    CHECK IF USERID EXISTS IN THE USERID DB2 TABLE             *
      *                                                               *
      *    1) IF EXISTS, UPDATE USERID TABLE LAST ACCESSED DATE       *
      *    2) IF NOT EXISTS, PERFORM USERID ADD PROCESS               *
      *****************************************************************

           EXEC SQL SELECT    ID,
                              NUMBER,
                              ACTIVE_SCENARIOS

                    INTO      :USERID-ID,
                              :USERID-NUMBER,
                              :USERID-ACTIVE-SCENARIOS

                    FROM      USERID

                    WHERE     ID = :WMF-USERID
           END-EXEC.


           IF SQLCODE                  =  ZEROES
               PERFORM  P04100-UPDATE-USERID
                   THRU P04100-UPDATE-USERID-EXIT
               MOVE USERID-ID          TO PC-USERID-ID
               MOVE USERID-NUMBER      TO PC-USERID-NUMBER
           ELSE
           IF SQLCODE                  =  +100
               PERFORM  P04200-ADD-USERID
                   THRU P04200-ADD-USERID-EXIT
               MOVE SPACES             TO USERID-ACTIVE-SCENARIOS
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA001'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'SELECT ID FROM USERID'
                                       TO WPDE-FUNCTION
               MOVE 'P04000'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P04000-VERIFY-USERID-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P04100-UPDATE-USERID                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO UPDATE THE USERID DB2 TABLE         *
      *                LAST ACCESSED DATE COLUMN FOR THE CICS SIGNED  *
      *                ON USER                                        *
      *                                                               *
      *    CALLED BY:  P04000-VERIFY-USERID                           *
      *                                                               *
      *****************************************************************

       P04100-UPDATE-USERID.

      *****************************************************************
      *    UPDATE THE USERID LAST ACCESSED DATE COLUMN WITH THE       *
      *    CURRENT DATE                                               *
      *****************************************************************

           EXEC SQL UPDATE  USERID
                    SET     LAST_ACCESSED  =  CURRENT DATE

                    WHERE   ID             =  :WMF-USERID
           END-EXEC.


      *****************************************************************
      *    RETURN CODE OTHER THAN ZEROES IS AN ERROR                  *
      *****************************************************************

           IF SQLCODE                  = ZEROES
               NEXT SENTENCE
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA001'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'UPDATE USERID - LAST_ACCESSED'
                                       TO WPDE-FUNCTION
               MOVE 'P04100'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P04100-UPDATE-USERID-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P04200-ADD-USERID                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO ADD A USERID TO THE USERID DB2      *
      *                                                               *
      *                1) DETERMINE NEXT AVAILABLE UNIQUE IDENTIFIER  *
      *                   FROM USERID TABLE (MAX COLUMN FUNCTION)     *
      *                                                               *
      *                2) INSERT USERID ROW                           *
      *                                                               *
      *                3) LOAD NEW USER BASE DATA INTO ALL FILES      *
      *                   (ACHIEVED VIA LINK TO MODULE PDA013)        *
      *                                                               *
      *    CALLED BY:  P04000-VERIFY-USERID                           *
      *                                                               *
      *****************************************************************

       P04200-ADD-USERID.

      *****************************************************************
      *    LOCK THE USERID TABLE IN SHARE MODE FOR NEW USER ADD       *
      *****************************************************************

           EXEC SQL LOCK TABLE USERID IN SHARE MODE
           END-EXEC.

           IF SQLCODE                  =  ZEROES
               NEXT SENTENCE
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA001'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'LOCK TABLE USERID'
                                       TO WPDE-FUNCTION
               MOVE 'P04200'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    OBTAIN THE MAXIMUM USERID NUMBER CURRENTLY ON FILE         *
      *****************************************************************

           EXEC SQL SELECT  MAX(NUMBER)
                    INTO    :WMF-USERID-NUMBER    :WMF-NULL-IND
                    FROM    USERID
           END-EXEC.


      *****************************************************************
      *    IF NULL VALUE RETURNED SET USERID NUMBER TO 1              *
      *    IF VALUE RETURNED, INCREMENT BY 1                          *
      *    IF VALUE RETURNED IS 99998, ERROR (CANNOT ADD NEW USERS)   *
      *****************************************************************

           IF SQLCODE                  =  ZEROES
               IF WMF-NULL-IND         <  ZEROES
                   MOVE +1             TO WMF-USERID-NUMBER
               ELSE
               IF WMF-USERID-NUMBER    =  +99998
                   MOVE -1             TO  MENUSELL
                   MOVE PM005-SYSTEM-AT-MAXIMUM-USERS
                                       TO  WMF-MESSAGE-AREA
                   PERFORM  P70000-ERROR-ROUTINE
                       THRU P70000-ERROR-ROUTINE-EXIT
                   GO TO P04200-ADD-USERID-EXIT
               ELSE
                   ADD +1              TO  WMF-USERID-NUMBER
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA001'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'SELECT MAX(NUMBER) FROM USERID'
                                       TO WPDE-FUNCTION
               MOVE 'P04200'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    ADD (INSERT) THE USERID ROW INTO THE TABLE                 *
      *****************************************************************

           EXEC SQL INSERT  INTO  USERID
                            (ID,
                             NUMBER,
                             LAST_ACCESSED,
                             ACTIVE_SCENARIOS)
                    VALUES  (:WMF-USERID,
                             :WMF-USERID-NUMBER,
                             CURRENT DATE,
                             ' ')
           END-EXEC.


      *****************************************************************
      *    RETURN CODE OTHER THAN ZERO IS AN ERROR                    *
      *****************************************************************

           IF SQLCODE                  =  ZEROES
               NEXT SENTENCE
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA001'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'INSERT INTO USERID'
                                       TO WPDE-FUNCTION
               MOVE 'P04200'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    PROCEED TO LOAD ALL BASE DATA FOR THE NEW USERID           *
      *****************************************************************

           PERFORM  P04300-LOAD-USERID-DATA
               THRU P04300-LOAD-USERID-DATA-EXIT.


       P04200-ADD-USERID-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P04300-LOAD-USERID-DATA                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO POPULATE THE PRODUCT DEMONSTRATION  *
      *                APPLICATION FILES WITH A BASE SET OF DATA FOR  *
      *                A SPECIFIC USERID                              *
      *                                                               *
      *                THE DATA POPULATION IS ACCOMPLISHED VIA        *
      *                MODULE PDA013 (SECONDARY CICS TRANSACTION)     *
      *                                                               *
      *                                                               *
      *    CALLED BY:  P04200-ADD-USERID                              *
      *                                                               *
      *****************************************************************

       P04300-LOAD-USERID-DATA.

      *****************************************************************
      *    SEND MESSAGE TO USER TO INDICATE THE DATA LOAD IN PROGRESS *
      *****************************************************************

           MOVE PM007-LOADING-USER-DATA
                                       TO PDAMSGO.

           PERFORM  P80100-SEND-MAP-DATAONLY
               THRU P80100-SEND-MAP-DATAONLY-EXIT.


      *****************************************************************
      *    LINK TO MODULE PDA013 TO LOAD THE BASE SET OF USER DATA    *
      *****************************************************************

           MOVE WMF-USERID             TO PC-USERID-ID.
           MOVE WMF-USERID-NUMBER      TO PC-USERID-NUMBER.


           EXEC CICS LINK
                     PROGRAM       ('PDA013')
                     COMMAREA      (WS-PDA-COMMAREA)
                     LENGTH        (WS-PDA-COMMAREA-LTH)
                     NOHANDLE
                     RESP          (WS-RESPONSE-CODE)
           END-EXEC.



           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               NEXT SENTENCE
           ELSE
               MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
               MOVE 'PDA001'           TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
               MOVE 'CICS LINK PDA013' TO WPCE-COMMAND
               MOVE 'P04300'           TO WPCE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P04300-LOAD-USERID-DATA-EXIT.
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
                     MAP           ('PDA001')
                     MAPSET        ('PDA001M')
                     FROM          (PDA001O)
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
               MOVE 'PDA001'           TO WPCE-PROGRAM-ID
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
                     MAP           ('PDA001')
                     MAPSET        ('PDA001M')
                     FROM          (PDA001O)
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
               MOVE 'PDA001'           TO WPCE-PROGRAM-ID
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
                     MAP           ('PDA001')
                     MAPSET        ('PDA001M')
                     INTO          (PDA001I)
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
               MOVE 'PDA001'           TO WPCE-PROGRAM-ID
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
                     COMMAREA      (WS-PDA-COMMAREA)
                     LENGTH        (WS-PDA-COMMAREA-LTH)
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
               MOVE 'PDA001'           TO WPCE-PROGRAM-ID
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
               MOVE 'PDA001'           TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
               MOVE 'CICS SEND'        TO WPCE-COMMAND
               MOVE 'P80400'           TO WPCE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    CURSOR AT FIRST POSITION ON SCREEN, IF ERROR TERMINATE     *
      *****************************************************************

PWB501**** EXEC CICS SEND
PWB501****           CONTROL
PWB501****           CURSOR        (0)
PWB501****           NOHANDLE
PWB501****           RESP          (WS-RESPONSE-CODE)
PWB501**** END-EXEC.



PWB501**** IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
PWB501****     NEXT SENTENCE
PWB501**** ELSE
PWB501****     MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
PWB501****     MOVE 'PDA001'           TO WPCE-PROGRAM-ID
PWB501****     MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
PWB501****     MOVE 'CICS SEND CONTROL'
PWB501****                             TO WPCE-COMMAND
PWB501****     MOVE 'P80400'           TO WPCE-PARAGRAPH
PWB501****     PERFORM  P99500-PDA-ERROR
PWB501****         THRU P99500-PDA-ERROR-EXIT.

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
           MOVE 'PDA001'               TO WPCE-PROGRAM-ID.
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



PWB501**** EXEC CICS SEND
PWB501****           CONTROL
PWB501****           CURSOR  (0)
PWB501**** END-EXEC.


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
AS OF      MOVE 'PDA001'               TO WPCE-PROGRAM-ID.               AS OF
JAN        MOVE EIBRESP                TO WPCE-RESPONSE-CODE.            JAN
2001       MOVE 'ERROR'                TO WPCE-COMMAND.                  2001
           MOVE 'P99999'               TO WPCE-PARAGRAPH.
LLR                                                                      LLR
           PERFORM  P99500-PDA-ERROR
               THRU P99500-PDA-ERROR-EXIT.

       P99999-ERROR-EXIT.
           EXIT.
           EJECT