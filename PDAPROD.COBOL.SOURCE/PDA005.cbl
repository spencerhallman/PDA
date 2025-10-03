       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA005.

      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA005                                            *
      * TRANS   :   PD05                                              *
      * MAPSET  :   PDA005M                                           *
      *                                                               *
      * FUNCTION:   PROGRAM PDA005 IS THE PRODUCT DEMONSTRATION       *
      *             APPLICATION BROWSE CATEGORIES PROGRAM. THE BROWSE *
      *             CATEGORIES SCREEN CONTAINS A SCROLLABLE LIST      *
      *             OF ALL THE VALID CATEGORY / SUB-CATEGORY          *
      *             COMBINATIONS FOR THE APPLICATION.                 *
      *                                                               *
      *             THE FUNCTION IS PART OF THE ORDER ADD PROCESS.    *
      *             THE USER MAY SELECT (VIA SELECTION CODE) ANY      *
      *             CATEGORY / SUB-CATEGORY DESIRED AND ADVANCE TO    *
      *             THE NEXT LOGICAL SCREEN IN THE ORDER ADD PROCESS. *
      *                                                               *
      *                                                               *
      *                                                               *
      * FILES   :   NONE    (CATEGORIES ARE STORED IN A STATIC ARRAY) *
      *                                                               *
      *                                                               *
      * TRANSACTIONS GENERATED:                                       *
      *             PD01       MAIN MENU                              *
      *             PD04       CUSTOMER IDENTIFICATION                *
      *             PD06       BROWSE ITEMS BY CATEGORY               *
      *                                                               *
      *                                                               *
      * PFKEYS  :   PF3   =    PREVIOUS   (CUSTOMER IDENT. PD04)      *
      *             PF7   =    SCROLL BACKWARD                        *
      *             PF8   =    SCROLL FORWARD                         *
      *             PF12  =    MAIN MENU                              *
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
       77  WS-SUB1                     PIC S9(04)   COMP    VALUE +0.
       77  WS-RESPONSE-CODE            PIC S9(08)   COMP    VALUE +0.
       77  WS-MESSAGE-LTH              PIC S9(04)   COMP    VALUE +79.
       77  WS-MAP-SUB                  PIC S9(04)   COMP    VALUE +0.
       77  WS-CAT-SUB                  PIC S9(04)   COMP    VALUE +0.
       77  WS-SUBCAT-SUB               PIC S9(04)   COMP    VALUE +0.
       77  WS-MAP-LINES-MAX            PIC S9(04)   COMP    VALUE +15.
       77  WS-COUNT                    PIC S9(04)   COMP    VALUE +0.

      *****************************************************************
      *    SWITCHES                                                   *
      *****************************************************************
       01  WS-SWITCHES.

           05  WS-CATEGORY-SELECTION-SW
                                       PIC X(01)             VALUE ' '.
               88  CATEGORY-SELECTION-IS-VALID               VALUE 'S'.

           05  WS-ERROR-FOUND-SW       PIC X(01)             VALUE 'N'.
               88  ERROR-FOUND                               VALUE 'Y'.
               88  NO-ERROR-FOUND                            VALUE 'N'.

           05  WS-SELECTION-DATA-ENTERED-SW
                                       PIC X(01)             VALUE 'N'.
               88  SELECTION-DATA-ENTERED                    VALUE 'Y'.
               88  NO-SELECTION-DATA-ENTERED                 VALUE 'N'.

           05  WS-TOP-OF-DATA-SW       PIC X(01)             VALUE 'N'.
               88  TOP-OF-DATA                               VALUE 'Y'.
               88  NOT-TOP-OF-DATA                           VALUE 'N'.

           05  WS-BOTTOM-OF-DATA-SW    PIC X(01)             VALUE 'N'.
               88  BOTTOM-OF-DATA                            VALUE 'Y'.
               88  NOT-BOTTOM-OF-DATA                        VALUE 'N'.

           05  WS-CATEGORY-ARRAY-LOADED-SW
                                       PIC X(01)             VALUE 'N'.
               88  CATEGORY-ARRAY-LOADED                     VALUE 'Y'.
               88  NOT-CATEGORY-ARRAY-LOADED                 VALUE 'N'.

           EJECT
      *****************************************************************
      *    MISCELLANEOUS WORK FIELDS                                  *
      *****************************************************************
       01  WS-MISCELLANEOUS-FIELDS.

           05  WMF-ABSTIME             PIC S9(15)  VALUE +0  COMP-3.
           05  WMF-DATE-MMDDYY         PIC X(08)   VALUE SPACES.
           05  WMF-TIME-HHMMSS         PIC X(08)   VALUE SPACES.
           05  WMF-MESSAGE-AREA        PIC X(79)   VALUE SPACES.
           05  WMF-HOLD-CATEGORY       PIC X(32)   VALUE SPACES.

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

           05  WMF-ASKIP-DARK-FSET-1   PIC S9(04)  VALUE +125 COMP.
           05  WMF-ASKIP-DARK-FSET-R   REDEFINES WMF-ASKIP-DARK-FSET-1.
               10  FILLER              PIC X(01).
               10  WMF-ASKIP-DARK-FSET PIC X(01).

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
      *         MAP DSECTS -- MAIN MENU PDA005M                       *
      *****************************************************************

           COPY PDA005M.
           EJECT


      *****************************************************************
      *         MAP DSECTS -- REDEFINES                               *
      *****************************************************************

       01  PDA005I-R                   REDEFINES PDA005I.
           05  FILLER                  PIC X(51).

           05  M-CATEGORY-DETAIL       OCCURS  15  TIMES.

               10  M-SELECTION-LTH     PIC S9(04)  COMP.
               10  M-SELECTION-ATTR    PIC X(01).
               10  FILLER              PIC X(02).
               10  M-SELECTION         PIC X(01).

               10  M-CATEGORY-LTH      PIC S9(04)  COMP.
               10  M-CATEGORY-ATTR     PIC X(01).
               10  FILLER              PIC X(02).
               10  M-CATEGORY          PIC X(32).

               10  M-SUB-CATEGORY-LTH  PIC S9(04)  COMP.
               10  M-SUB-CATEGORY-ATTR PIC X(01).
               10  FILLER              PIC X(02).
               10  M-SUB-CATEGORY      PIC X(32).

           05  FILLER                  PIC X(84).
           EJECT

      *****************************************************************
      *    IMS / DLI DEFINITIONS                                      *
      *****************************************************************
      *****************************************************************
      *    NO IMS / DLI USED IN MODULE                                *
      *****************************************************************

           EJECT

      *****************************************************************
      *    DB2  DEFINITIONS                                           *
      *****************************************************************
      *****************************************************************
      *    NO DB2 USED IN MODULE                                      *
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
      *    PROGRAM WORK AREA PORTION OF THE PDA COMMAREA              *
      *****************************************************************

       01  WS-PDA-COMMAREA-WORKAREA.

           05  WPCW-ORIGINATING-PGRMID PIC X(08).

           05  WPCW-FIRST-SCREEN-KEY.
               10  WPCW-FIRST-CAT-SUB  PIC S9(04)      COMP.

           05  WPCW-LAST-SCREEN-KEY.
               10  WPCW-LAST-CAT-SUB   PIC S9(04)      COMP.
           05  FILLER                  PIC X(988).
           EJECT

      *****************************************************************
      *    PDA STANDARD CATEGORY / SUB-CATEGORY FOR THE APPLICATION   *
      *****************************************************************

           COPY PDACATGY.
           EJECT

      *****************************************************************
      *    PROGRAM INTERNAL USE ARRAYS CATEGORY, SUB-CATEGORY         *
      *****************************************************************

       01  WS-PDA-CATEGORY-ARRAY.
           05  WPCA-CATEGORY-MAX       PIC S9(05)   COMP-3  VALUE +100.
           05  WPCA-CATEGORY-COUNT     PIC S9(05)   COMP-3.
           05  WPCA-CATEGORY-GRP       OCCURS 1 TO 100 TIMES
                                       DEPENDING ON
                                         WPCA-CATEGORY-COUNT
                                           INDEXED BY WPCA-CAT-IX.
               10  WPCA-CATEGORY       PIC X(32).
               10  WPCA-SUB-CATEGORY   PIC X(32).

           EJECT

      *****************************************************************
      *    D E M O N S T R A T I O N    P U R P O S E S   O N L Y     *
      *    ARRAY NOT USED IN APPLICATION                              *
      *    PROGRAM INTERNAL USE ARRAY  SUB-CATEGORY                   *
      *****************************************************************

       01  WS-SUB-CATEGORY-ARRAY.
           05  WSCA-MAX-ENTRIES        PIC S9(05)   COMP-3  VALUE +100.
           05  WSCA-SUB-CATEGORY-COUNT PIC S9(05)   COMP-3.
           05  WSCA-SUB-CATEGORY-GRP   OCCURS 1 TO 100 TIMES
                                       DEPENDING ON
                                         WSCA-SUB-CATEGORY-COUNT
                                           INDEXED BY WSCA-SUBCAT-IX.
               10  WSCA-SUB-CATEGORY   PIC X(32).

           EJECT

      *****************************************************************
      *    L I N K A G E     S E C T I O N                            *
      *****************************************************************

       LINKAGE SECTION.

       01  DFHCOMMAREA.
           COPY PDACOMM.
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
      *                BROWSE CATEGORIES SCREEN                       *
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

      *****************************************************************
      *    VERIFY THE COMMAREA IS PRESENT AND CORRECT LENGTH          *
      *****************************************************************

           IF EIBCALEN                 > ZEROES
               IF EIBCALEN             = PC-COMMAREA-LTH
                   NEXT SENTENCE
               ELSE
                   MOVE 'CICS'         TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA005'       TO WPCE-PROGRAM-ID
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


      *****************************************************************
      *    INITIALZE VARIABLES, WORK AREAS, ETC.                      *
      *****************************************************************

           MOVE SPACES                 TO WS-CATEGORY-SELECTION-SW.
           MOVE 'N'                    TO WS-ERROR-FOUND-SW.
           MOVE 'N'                    TO WS-SELECTION-DATA-ENTERED-SW.
           MOVE 'N'                    TO WS-TOP-OF-DATA-SW.
           MOVE 'N'                    TO WS-BOTTOM-OF-DATA-SW.
           MOVE 'N'                    TO WS-CATEGORY-ARRAY-LOADED-SW.
                                                                        00010000
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME.          00020001


           IF PC-PREV-PGRMID           =  'PDA005'
               MOVE PC-PROGRAM-WORKAREA
                                       TO WS-PDA-COMMAREA-WORKAREA
           ELSE
               MOVE SPACES             TO PC-PROGRAM-WORKAREA
                                          WS-PDA-COMMAREA-WORKAREA.


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
               MOVE 'PDA005'           TO WPCE-PROGRAM-ID
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
      *    IF 1ST TIME THRU, PERFORM INITIAL SCREEN BUILD PROCESSES,  *
      *    OTHERWISE PROCESS EDIT / UPDATE TRANSACTION                *
      *****************************************************************

           IF PC-PREV-PGRMID           NOT = 'PDA005'

               PERFORM  P01000-1ST-TIME-PROCESS
                   THRU P01000-1ST-TIME-PROCESS-EXIT

           ELSE

               PERFORM  P02000-PROCESS-TRANS
                   THRU P02000-PROCESS-TRANS-EXIT.


      *****************************************************************
      *    SAVE THE PROGRAM WORKAREA IN COMMAREA                      *
      *****************************************************************

           MOVE WS-PDA-COMMAREA-WORKAREA
                                       TO  PC-PROGRAM-WORKAREA.

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
                     TRANSID       ('PD05')
                     COMMAREA      (DFHCOMMAREA)
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
               MOVE 'PDA005'           TO WPCE-PROGRAM-ID
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
      *    PARAGRAPH:  P01000-1ST-TIME-PROCESS                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL PROGRAM INITIAL INQUIRY     *
      *                PROCESSES                                      *
      *                                                               *
      *    CALLED BY:  P00100-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P01000-1ST-TIME-PROCESS.

           MOVE LOW-VALUES             TO PDA005I.

      *****************************************************************
      *    SET INITIAL INQUIRY TO START FROM THE BEGINNING OF THE     *
      *    CATEGORY / SUB-CATEGORY ARRAY (USE SCROLL FORWARD ROUTINE) *
      *****************************************************************

           MOVE PC-PREV-PGRMID         TO WPCW-ORIGINATING-PGRMID.

           MOVE +1                     TO WPCW-FIRST-CAT-SUB.
           MOVE ZEROES                 TO WPCW-LAST-CAT-SUB.

           PERFORM  P07000-SCROLL-FORWARD
               THRU P07000-SCROLL-FORWARD-EXIT.


      *****************************************************************
      *    DISPLAY THE INITIAL SCREEN, UPDATE COMMAREA VARIABLES      *
      *****************************************************************


           PERFORM  P79000-DISPLAY-SCREEN
               THRU P79000-DISPLAY-SCREEN-EXIT.

           MOVE 'PDA005'               TO  PC-PREV-PGRMID.


       P01000-1ST-TIME-PROCESS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P02000-PROCESS-TRANS                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL EDIT / UPDATE / PFKEY       *
      *                REQUESTS FOR THE SCREEN                        *
      *                                                               *
      *    CALLED BY:  P00100-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P02000-PROCESS-TRANS.

           MOVE 'PDA005'               TO  PC-PREV-PGRMID.

      *****************************************************************
      *    RECEIVE THE INPUT MAP, CONVERT UNDERSCORES AND LOW-VALUES  *
      *    TO SPACES, UPDATE SCREEN HEADER INFORMATION                *
      *****************************************************************

           PERFORM  P80200-RECEIVE-MAP
               THRU P80200-RECEIVE-MAP-EXIT.

           MOVE SPACES                 TO  PDAMSGO.

           PERFORM  P02100-CONVERT-FIELDS
               THRU P02100-CONVERT-FIELDS-EXIT
                   VARYING WS-MAP-SUB  FROM +1  BY  +1
                       UNTIL WS-MAP-SUB  >  WS-MAP-LINES-MAX.


      *****************************************************************
      *    PROCEED WITH THE TRANSACTION EDIT PROCESSES                *
      *****************************************************************

           PERFORM  P03000-EDIT-PROCESS
               THRU P03000-EDIT-PROCESS-EXIT.


      *****************************************************************
      *    DISPLAY THE OUTPUT SCREEN                                  *
      *****************************************************************

           PERFORM  P79000-DISPLAY-SCREEN
               THRU P79000-DISPLAY-SCREEN-EXIT.

       P02000-PROCESS-TRANS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P02100-CONVERT-FIELDS                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONVERT SCREEN ENTERABLE FIELDS     *
      *                FROM UNDERSCORES OR LOW VALUES TO SPACES       *
      *                                                               *
      *    CALLED BY:  P02000-PROCESS-TRANS                           *
      *                                                               *
      *****************************************************************

       P02100-CONVERT-FIELDS.


           INSPECT M-SELECTION  (WS-MAP-SUB)
               CONVERTING  WMF-UNDERSCORE-LOWVALUE-R TO SPACES.


       P02100-CONVERT-FIELDS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE PROGRAM EDIT PROCESS    *
      *                                                               *
      *    CALLED BY:  P02000-PROCESS-TRANS                           *
      *                                                               *
      *****************************************************************

       P03000-EDIT-PROCESS.


      *****************************************************************
      *    EDIT THE TRANSACTION INTENT, SELECTION / ACTION CODES      *
      *    VERSUS PFKEYS                                              *
      *****************************************************************

           PERFORM  P03500-EDIT-TRANS-INTENT
               THRU P03500-EDIT-TRANS-INTENT-EXIT.

           IF ERROR-FOUND
               GO TO P03000-EDIT-PROCESS-EXIT.


      *****************************************************************
      *    IF ENTER KEY PROCESS SCREEN, ELSE PROCESS PFKEY FUNCTION   *
      *****************************************************************

           IF EIBAID    =    DFHENTER
               PERFORM  P05000-PROCESS-SCREEN
                   THRU P05000-PROCESS-SCREEN-EXIT
           ELSE
               PERFORM  P04000-PFKEY-PROCESS
                   THRU P04000-PFKEY-PROCESS-EXIT.


       P03000-EDIT-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03500-EDIT-TRANS-INTENT                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO EDIT PFKEY SELECTION, AND PFKEY     *
      *                SELECTION VERSUS SELECTION CODE ENTRY          *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************

       P03500-EDIT-TRANS-INTENT.

      *****************************************************************
      *    VALID KEYS ARE: ENTER, CLEAR, PF3, PF7, PF8, PF12          *
      *****************************************************************

           IF EIBAID  = DFHENTER  OR  DFHCLEAR  OR  DFHPF3  OR
                        DFHPF7    OR  DFHPF8    OR  DFHPF12
               NEXT SENTENCE
           ELSE
               MOVE -1                 TO  M-SELECTION-LTH (1)
               MOVE PM001-INVALID-PFKEY
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P03500-EDIT-TRANS-INTENT-EXIT.


      *****************************************************************
      *    DETERMINE IF ANY SELECTION CODES HAVE BEEN ENTERED         *
      *    (SELECTION CODES AND PFKEYS ARE MUTUALLY EXCLUSIVE)        *
      *****************************************************************

           IF EIBAID                   NOT = DFHCLEAR
               MOVE ZEROES             TO WS-COUNT
               PERFORM  P03600-CHK-SELECTION-DATA
                   THRU P03600-CHK-SELECTION-DATA-EXIT
                       VARYING WS-MAP-SUB  FROM  +1  BY  +1
                           UNTIL WS-MAP-SUB  >  WS-MAP-LINES-MAX
           ELSE
               NEXT SENTENCE.


           IF EIBAID                   =    DFHENTER  OR  DFHCLEAR
               NEXT SENTENCE
           ELSE
           IF SELECTION-DATA-ENTERED
               MOVE PM003-ACTION-VS-PFKEY-CONFLICT
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               PERFORM  P03700-HILITE-SELECTIONS
                   THRU P03700-HILITE-SELECTIONS-EXIT
                       VARYING WS-MAP-SUB FROM +1 BY +1
                           UNTIL WS-MAP-SUB > WS-MAP-LINES-MAX
               GO TO P03500-EDIT-TRANS-INTENT-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    ONE SELECTION CODE MUST BE ENTERED, MORE THAN 1 IS ERROR   *
      *****************************************************************

           IF EIBAID                   =  DFHENTER
               IF NO-SELECTION-DATA-ENTERED
                   MOVE -1             TO  M-SELECTION-LTH (1)
                   MOVE PM010-ENTER-SELECTION
                                       TO  WMF-MESSAGE-AREA
                   PERFORM  P70000-ERROR-ROUTINE
                       THRU P70000-ERROR-ROUTINE-EXIT
                   GO TO P03500-EDIT-TRANS-INTENT-EXIT
               ELSE
               IF WS-COUNT             >  +1
                   MOVE PM011-ONE-SELECTION
                                       TO  WMF-MESSAGE-AREA
                   PERFORM  P70000-ERROR-ROUTINE
                       THRU P70000-ERROR-ROUTINE-EXIT
                   PERFORM  P03700-HILITE-SELECTIONS
                       THRU P03700-HILITE-SELECTIONS-EXIT
                           VARYING WS-MAP-SUB FROM +1 BY +1
                               UNTIL WS-MAP-SUB > WS-MAP-LINES-MAX
                   GO TO P03500-EDIT-TRANS-INTENT-EXIT
               ELSE
                   NEXT SENTENCE
           ELSE
               NEXT SENTENCE.


       P03500-EDIT-TRANS-INTENT-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03600-CHK-SELECTION-DATA                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DETERMINE IF ANY SELECTION CODES    *
      *                HAVE BEEN ENTERED                              *
      *                                                               *
      *    CALLED BY:  P03500-EDIT-TRANS-INTENT                       *
      *                                                               *
      *****************************************************************

       P03600-CHK-SELECTION-DATA.


           IF M-SELECTION (WS-MAP-SUB) >  SPACES
               MOVE 'Y'                TO WS-SELECTION-DATA-ENTERED-SW
               ADD  +1                 TO WS-COUNT
           ELSE
               NEXT SENTENCE.


       P03600-CHK-SELECTION-DATA-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03700-HILITE-SELECTIONS                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO HIGHLIGHT THE SELECTION CODES AND   *
      *                PLACE THE CURSOR AT THE 1ST ENTERED SELECTION  *
      *                CODE IN AN ERROR SITUATION                     *
      *                                                               *
      *    CALLED BY:  P03500-EDIT-TRANS-INTENT                       *
      *                                                               *
      *****************************************************************

       P03700-HILITE-SELECTIONS.


           IF M-SELECTION  (WS-MAP-SUB) >  SPACES
               MOVE -1                 TO M-SELECTION-LTH  (WS-MAP-SUB)
               MOVE DFHUNIMD           TO M-SELECTION-ATTR (WS-MAP-SUB)
           ELSE
               NEXT SENTENCE.


       P03700-HILITE-SELECTIONS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P04000-PFKEY-PROCESS                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE PROCESSING WHEN A       *
      *                PROGRAM FUNCTION KEY (PFKEY) IS UTILIZED       *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************

       P04000-PFKEY-PROCESS.

      *****************************************************************
      *    ALLOW USER TO EXIT APPLICATION WITH CLEAR KEY              *
      *    (SEND MESSAGE, ERASE SCREEN)                               *
      *****************************************************************

           IF EIBAID  = DFHCLEAR
               MOVE PM002-EXIT-APPLICATION
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P80400-SEND-MESSAGE
                   THRU P80400-SEND-MESSAGE-EXIT
               GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    PF3 (PREVIOUS), TRANSFER TO CUSTOMER IDENT (PDA004) IF:    *
      *    1) ORIGINAL MENU SELECTION WAS ORDER ADD (SELECTION 1)     *
      *    2) ORIGINAL MENU SELECTION WAS PENDING ORDER (SELECTION 4) *
      *    3) PREVIOUS PROGRAM WAS EITHER CUSTOMER IDENT (PDA004) OR  *
      *       OR PENDING ORDER (PDA008)                               *
      *                                                               *
      *    OTHERWISE TRANSFER TO THE ORIGINATING PROGRAM              *
      *****************************************************************

           IF EIBAID  = DFHPF3
               IF (PC-PREV-MENU-SEL        = '1' OR '4')    OR
                  (WPCW-ORIGINATING-PGRMID = 'PDA004' OR 'PDA008')
                   MOVE 'PDA004'       TO  PC-NEXT-PGRMID
                   PERFORM  P80300-XFER-CONTROL
                       THRU P80300-XFER-CONTROL-EXIT
                   GO TO P04000-PFKEY-PROCESS-EXIT
               ELSE
                   MOVE WPCW-ORIGINATING-PGRMID
                                       TO  PC-NEXT-PGRMID
                   PERFORM  P80300-XFER-CONTROL
                       THRU P80300-XFER-CONTROL-EXIT
                   GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF PF7, PERFORM SCROLL BACKWARD PROCESS                    *
      *****************************************************************

           IF EIBAID  = DFHPF7
               PERFORM  P06000-SCROLL-BACKWARD
                   THRU P06000-SCROLL-BACKWARD-EXIT
               GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF PF8, PERFORM SCROLL FORWARD PROCESS                     *
      *****************************************************************

           IF EIBAID  = DFHPF8
               PERFORM  P07000-SCROLL-FORWARD
                   THRU P07000-SCROLL-FORWARD-EXIT
               GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF PF12, RETURN TO THE MAIN MENU                           *
      *****************************************************************

           IF EIBAID  = DFHPF12
               MOVE 'PDA001'       TO  PC-NEXT-PGRMID
               PERFORM  P80300-XFER-CONTROL
                   THRU P80300-XFER-CONTROL-EXIT
               GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


       P04000-PFKEY-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05000-PROCESS-SCREEN                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE PROCESSING WHEN A       *
      *                CATEGORY / SUB-CATEGORY LINE IS SELECTED       *
      *                BY THE USER                                    *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************

       P05000-PROCESS-SCREEN.

      *****************************************************************
      *    VERIFY SELECTION CODE  ENTERED IS VALID (VALUE MUST BE -S-)*
      *****************************************************************

           PERFORM  P05100-FIND-SELECTION
               THRU P05100-FIND-SELECTION-EXIT
                   VARYING WS-MAP-SUB FROM +1 BY +1
                       UNTIL M-SELECTION (WS-MAP-SUB) > SPACES.


           IF M-SELECTION (WS-MAP-SUB)    = 'S'
               NEXT SENTENCE
           ELSE
               MOVE -1                 TO M-SELECTION-LTH (WS-MAP-SUB)
               MOVE DFHUNIMD           TO M-SELECTION-ATTR (WS-MAP-SUB)
               MOVE PM012-INVALID-SEL-CODE
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P05000-PROCESS-SCREEN-EXIT.


      *****************************************************************
      *    CAPTURE CATEGORY / SUB-CATEGORY INFO FOR THE COMMAREA,     *
      *    TRANSFER CONTROL TO BROWSE ITEMS BY CATEGORY (PDA006)      *
      *****************************************************************

           MOVE M-CATEGORY     (WS-MAP-SUB)
                                       TO PC-ITEM-CATEGORY.
           MOVE M-SUB-CATEGORY (WS-MAP-SUB)
                                       TO PC-ITEM-SUB-CATEGORY.
           MOVE 'PDA006'               TO PC-NEXT-PGRMID.

           PERFORM  P80300-XFER-CONTROL
               THRU P80300-XFER-CONTROL-EXIT.


       P05000-PROCESS-SCREEN-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05100-FIND-SELECTION                          *
      *                                                               *
      *    FUNCTION :  DUMMY ROUTINE TO POSITION SUBSCRIPT AT THE     *
      *                FIRST NON-BLANK SELECTION CODE ON THE SCREEN   *
      *                                                               *
      *    CALLED BY:  P05000-PROCESS-SCREEN                          *
      *                                                               *
      *****************************************************************

       P05100-FIND-SELECTION.



       P05100-FIND-SELECTION-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06000-SCROLL-BACKWARD                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE SCREEN SCROLL BACKWARD  *
      *                PROCESS                                        *
      *                                                               *
      *    CALLED BY:  P04000-PFKEY-PROCESS                           *
      *                                                               *
      *****************************************************************

       P06000-SCROLL-BACKWARD.

           MOVE 'N'                    TO WS-TOP-OF-DATA-SW.

      *****************************************************************
      *    LOAD THE WORK CATEGORY ARRAY FROM THE STANDARD COPYBOOK    *
      *    CATEGORY ARRAY (BEING DONE FOR DEMONSTRATION PURPOSES ONLY)*
      *****************************************************************

           IF CATEGORY-ARRAY-LOADED
               NEXT SENTENCE
           ELSE
               PERFORM  P08000-LOAD-WORK-ARRAY
                   THRU P08000-LOAD-WORK-ARRAY-EXIT.


      *****************************************************************
      *    DETERMINE THE SCROLL BACKWARD START KEY, INITIALIZE        *
      *    VARIABLES AND MAP DISPLAY                                  *
      *****************************************************************

           PERFORM  P06050-GET-BACKWARD-KEY
               THRU P06050-GET-BACKWARD-KEY-EXIT.

           MOVE ZEROES                 TO WPCW-FIRST-CAT-SUB
                                          WPCW-LAST-CAT-SUB.

           PERFORM  P79200-CLEAR-MAP-FIELDS
               THRU P79200-CLEAR-MAP-FIELDS-EXIT.


      *****************************************************************
      *    IF NOT AT TOP OF DATA (I.E. MORE DATA TO DISPLAY),         *
      *    BUILD THE SCREEN DISPLAY FROM THE WORK CATEGORY ARRAY      *
      *    (BUILD FROM END OF THE MAP UP AND FROM THE PREDETERMINED   *
      *     STARTING POINT IN THE CATEGORY ARRAY UP -- BOTTOM TO TOP) *
      *****************************************************************

           IF NOT-TOP-OF-DATA
               MOVE WS-MAP-LINES-MAX   TO WS-MAP-SUB

               PERFORM  P06100-BUILD-SCREEN
                   THRU P06100-BUILD-SCREEN-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF TOP OF DATA ENCOUNTERED, I.E. NOT ENOUGH DATA TO FILL   *
      *    THE SCREEN, RE-INITIALIZE VARIABLES AND PERFORM THE        *
      *    STANDARD SCROLL FORWARD PROCESS FORM THE BEGINNING OF THE  *
      *    CATEGORY ARRAY AND THE TOP OF THE SCREEN                   *
      *****************************************************************

           IF TOP-OF-DATA

               MOVE +1                 TO WPCW-FIRST-CAT-SUB
               MOVE ZEROES             TO WPCW-LAST-CAT-SUB
               PERFORM  P07000-SCROLL-FORWARD
                   THRU P07000-SCROLL-FORWARD-EXIT

               MOVE PM014-TOP-MSG      TO WMF-MESSAGE-AREA
               PERFORM  P70100-MESSAGE-ROUTINE
                   THRU P70100-MESSAGE-ROUTINE-EXIT

           ELSE
               NEXT SENTENCE.


       P06000-SCROLL-BACKWARD-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06050-GET-BACKWARD-KEY                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DETERMINE THE STARTING KEY FOR      *
      *                A SCROLL BACKWARD FUNCTION                     *
      *                                                               *
      *    CALLED BY:  P06000-SCROLL-BACKWARD                         *
      *                                                               *
      *****************************************************************

       P06050-GET-BACKWARD-KEY.

      *****************************************************************
      *    IF AN ENTRY WAS DISPLAYED ON FIRST LINE OF SCREEN, USE IT  *
      *    AS THE STARTING POINT                                      *
      *****************************************************************

           IF WPCW-FIRST-CAT-SUB       >  ZEROES
               MOVE WPCW-FIRST-CAT-SUB TO WS-CAT-SUB
           ELSE

      *****************************************************************
      *    OTHERWISE SET TOP OF DATA INDICATOR WHICH WILL FORCE A     *
      *    SCROLL FORWARD FROM THE BEGINNING OF THE ARRAY             *
      *****************************************************************

               MOVE 'Y'                TO WS-TOP-OF-DATA-SW
               GO TO P06050-GET-BACKWARD-KEY-EXIT.


       P06050-GET-BACKWARD-KEY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06100-BUILD-SCREEN                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE SCREEN FORMATTING       *
      *                PROCESS TO POPULATE THE SCREEN WITH CATEGORY   *
      *                AND SUB-CATEGORY INFORMATION                   *
      *                                                               *
      *    CALLED BY:  P06000-SCROLL-BACKWARD                         *
      *                                                               *
      *****************************************************************

       P06100-BUILD-SCREEN.

      *****************************************************************
      *    PROCEED TO PROCESS / BUILD THE SCREEN CATEGORY INFORMATION *
      *****************************************************************

           PERFORM  P06200-BUILD-CATEGORY
               THRU P06200-BUILD-CATEGORY-EXIT
                   VARYING WS-CAT-SUB FROM WS-CAT-SUB BY -1
                       UNTIL WS-CAT-SUB  <  1.


      *****************************************************************
      *    IF SCREEN NOT FULL, SET TOP OF DATA INDICATOR TO FORCE     *
      *    A SCROLL FORWARD FUNCTION                                  *
      *****************************************************************

           IF WPCW-FIRST-CAT-SUB       >  ZEROES
               NEXT SENTENCE
           ELSE
               MOVE 'Y'                TO WS-TOP-OF-DATA-SW.


       P06100-BUILD-SCREEN-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06200-BUILD-CATEGORY                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS THE CATEGORY / SUB-CATEGORY *
      *                AND FORMAT THE INFORMATION TO THE SCREEN       *
      *                                                               *
      *    CALLED BY:  P06100-BUILD-SCREEN                            *
      *                                                               *
      *****************************************************************

       P06200-BUILD-CATEGORY.

      *****************************************************************
      *    SAVE KEY INFORMATION FOR FIRST AND LAST ENTRIES DISPLAYED  *
      *    ON THE SCREEN                                              *
      *****************************************************************

           SET WPCA-CAT-IX             TO  WS-CAT-SUB.

           IF WS-MAP-SUB               =   +1
               MOVE WS-CAT-SUB         TO  WPCW-FIRST-CAT-SUB
           ELSE
           IF WS-MAP-SUB               =   WS-MAP-LINES-MAX
               MOVE WS-CAT-SUB         TO  WPCW-LAST-CAT-SUB
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    FORMAT SCREEN CATEGORY / SUB-CATEGORY INFORMATION           *
      *****************************************************************

           MOVE WPCA-CATEGORY     (WPCA-CAT-IX)
                                       TO M-CATEGORY     (WS-MAP-SUB).
           MOVE WPCA-SUB-CATEGORY (WPCA-CAT-IX)
                                       TO M-SUB-CATEGORY (WS-MAP-SUB).


      *****************************************************************
      *    IF SCREEN IS FULL, SET VARIABLES TO TERMINATE PROCESS       *
      *****************************************************************

           COMPUTE WS-MAP-SUB          =   WS-MAP-SUB -  +1.

           IF WS-MAP-SUB               <   +1
               MOVE +1                 TO  WS-CAT-SUB
               GO TO P06200-BUILD-CATEGORY-EXIT
           ELSE
               NEXT SENTENCE.


       P06200-BUILD-CATEGORY-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07000-SCROLL-FORWARD                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE SCREEN SCROLL FORWARD   *
      *                PROCESS                                        *
      *                                                               *
      *    CALLED BY:  P01000-1ST-TIME-PROCESS                        *
      *                P04000-PFKEY-PROCESS                           *
      *                                                               *
      *****************************************************************

       P07000-SCROLL-FORWARD.

           MOVE 'N'                    TO WS-BOTTOM-OF-DATA-SW.

      *****************************************************************
      *    LOAD THE WORK CATEGORY ARRAY FROM THE STANDARD COPYBOOK    *
      *    CATEGORY ARRAY (BEING DONE FOR DEMONSTRATION PURPOSES ONLY)*
      *****************************************************************

           IF CATEGORY-ARRAY-LOADED
               NEXT SENTENCE
           ELSE
               PERFORM  P08000-LOAD-WORK-ARRAY
                   THRU P08000-LOAD-WORK-ARRAY-EXIT.


      *****************************************************************
      *    DETERMINE THE SCROLL FORWARD START KEY, CLEAR SCREEN       *
      *****************************************************************

           PERFORM  P07050-GET-FORWARD-KEY
               THRU P07050-GET-FORWARD-KEY-EXIT.

           MOVE ZEROES                 TO WPCW-FIRST-CAT-SUB
                                          WPCW-LAST-CAT-SUB.

           PERFORM  P79200-CLEAR-MAP-FIELDS
               THRU P79200-CLEAR-MAP-FIELDS-EXIT.


      *****************************************************************
      *    BUILD THE SCREEN DISPLAY FROM THE WORK CATEGORY ARRAY,     *
      *    IF END OF DATA ENCOUNTERED  -- FORMAT MESSAGE              *
      *****************************************************************

           MOVE ZEROES                 TO WS-MAP-SUB.

           PERFORM  P07100-BUILD-SCREEN
               THRU P07100-BUILD-SCREEN-EXIT.


           IF BOTTOM-OF-DATA
               MOVE PM013-BOTTOM-MSG   TO WMF-MESSAGE-AREA
               PERFORM  P70100-MESSAGE-ROUTINE
                   THRU P70100-MESSAGE-ROUTINE-EXIT
           ELSE
               NEXT SENTENCE.


       P07000-SCROLL-FORWARD-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07050-GET-FORWARD-KEY                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DETERMINE THE STARTING KEY FOR      *
      *                A SCROLL FORWARD FUNCTION                      *
      *                                                               *
      *    CALLED BY:  P07000-SCROLL-FORWARD                          *
      *                                                               *
      *****************************************************************

       P07050-GET-FORWARD-KEY.

      *****************************************************************
      *    IF AN ENTRY WAS DISPLAYED ON LAST LINE OF SCREEN, USE IT   *
      *    AS THE STARTING POINT                                      *
      *****************************************************************

           IF WPCW-LAST-CAT-SUB        >  ZEROES
               MOVE WPCW-LAST-CAT-SUB  TO WS-CAT-SUB
           ELSE

      *****************************************************************
      *    IF NO LAST LINE SCREEN ENTRY, USE SCREEN FIRST LINE ENTRY  *
      *    (IF PRESENT) AS THE STARTING POINT                         *
      *****************************************************************

           IF WPCW-FIRST-CAT-SUB       >  ZEROES
               MOVE WPCW-FIRST-CAT-SUB TO WS-CAT-SUB
           ELSE

      *****************************************************************
      *    OTHERWISE START AT THE BEGINNING OF THE CATEGORY ARRAY     *
      *****************************************************************

               MOVE +1                 TO WS-CAT-SUB.

       P07050-GET-FORWARD-KEY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07100-BUILD-SCREEN                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE SCREEN FORMAT / BUILD   *
      *                PROCESSES TO DISPLAY CATEGORY AND SUB-CATEGORY *
      *                INFORMATION                                    *
      *                                                               *
      *    CALLED BY:  P07000-SCROLL-FORWARD                          *
      *                                                               *
      *****************************************************************

       P07100-BUILD-SCREEN.


      *****************************************************************
      *    PROCESS THE CATEGORIES FROM THE PRE-DETERMINED START POINT *
      *****************************************************************

           PERFORM  P07200-BUILD-CATEGORY
               THRU P07200-BUILD-CATEGORY-EXIT
                   VARYING WS-CAT-SUB FROM WS-CAT-SUB BY +1
                       UNTIL WS-CAT-SUB  >  WPCA-CATEGORY-COUNT.


      *****************************************************************
      *    CHECK FOR BOTTOM OF DATA (NO DATA ON LAST LINE)            *
      *****************************************************************

           IF M-CATEGORY (WS-MAP-LINES-MAX)  >  SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'Y'                TO WS-BOTTOM-OF-DATA-SW.


       P07100-BUILD-SCREEN-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07200-BUILD-CATEGORY                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS / FORMAT THE CATEGORY /     *
      *                SUB-CATEGORY INFORMATION TO THE SCREEN         *
      *                                                               *
      *    CALLED BY:  P07100-BUILD-SCREEN                            *
      *                                                               *
      *****************************************************************

       P07200-BUILD-CATEGORY.


           IF PC-ACTIVE-SCENARIO(9)   =   'Y'
               MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME
           END-IF.

           SET WPCA-CAT-IX             TO WS-CAT-SUB.

      *****************************************************************
      *    CHECK IF MAP IS FULL, SAVE KEY INFORMATION FOR FIRST AND   *
      *    LAST ENTRIES DISPLAYED ON THE SCREEN                       *
      *****************************************************************

           ADD +1                      TO  WS-MAP-SUB.

           IF WS-MAP-SUB               >   WS-MAP-LINES-MAX
               MOVE WPCA-CATEGORY-MAX  TO  WS-CAT-SUB
               GO TO P07200-BUILD-CATEGORY-EXIT
           ELSE
           IF WS-MAP-SUB               =   +1
               MOVE WS-CAT-SUB         TO  WPCW-FIRST-CAT-SUB
           ELSE
           IF WS-MAP-SUB               =   WS-MAP-LINES-MAX
               MOVE WS-CAT-SUB         TO  WPCW-LAST-CAT-SUB
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    FORMAT SCREEN CATEGORY / SUB-CATEGORY INFORMATION           *
      *****************************************************************

           MOVE WPCA-CATEGORY     (WPCA-CAT-IX)
                                       TO M-CATEGORY     (WS-MAP-SUB).
           MOVE WPCA-SUB-CATEGORY (WPCA-CAT-IX)
                                       TO M-SUB-CATEGORY (WS-MAP-SUB).


       P07200-BUILD-CATEGORY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P08000-LOAD-WORK-ARRAY                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO LOAD A WORK VERSION OF THE          *
      *                CATEGORY / SUB-CATEGORY ARRAY FROM THE         *
      *                STANDARD COPYBOOK CATEGORY ARRAY               *
      *                (BEING DONE FOR DEMONSTRATION PURPOSES ONLY)   *
      *                                                               *
      *    CALLED BY:  P06000-SCROLL-BACKWARD                         *
      *                P07000-SCROLL-FORWARD                          *
      *                                                               *
      *****************************************************************

       P08000-LOAD-WORK-ARRAY.


      *****************************************************************
      *    COUNT THE CATEGORY / SUB-CATEGORY COMBINATIONS             *
      *****************************************************************

           MOVE ZEROES                 TO WS-COUNT.

           PERFORM  P08020-COUNT-CATEGORY
               THRU P08020-COUNT-CATEGORY-EXIT
                   VARYING WS-CAT-SUB FROM +1 BY +1
                       UNTIL WS-CAT-SUB > PDA-CATEGORY-MAX.


           IF WS-COUNT                 >  WPCA-CATEGORY-MAX
               MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
               MOVE 'PDA005'           TO WPCE-PROGRAM-ID
               MOVE ZEROES             TO WPCE-RESPONSE-CODE
               MOVE 'INTERNAL ERROR-ARRAY OVERFLOW'
                                       TO WPCE-COMMAND
               MOVE 'P08000'           TO WPCE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    INITIALIZE WORK ARRAY, PROCESS ALL THE CATEGORIES          *
      *****************************************************************

           MOVE WS-COUNT               TO WPCA-CATEGORY-COUNT.
           SET  WPCA-CAT-IX            TO 1.

           PERFORM  P08050-LOAD-CATEGORY
               THRU P08050-LOAD-CATEGORY-EXIT
                   VARYING WS-CAT-SUB FROM +1 BY +1
                       UNTIL WS-CAT-SUB > PDA-CATEGORY-MAX.



           MOVE 'Y'                    TO WS-CATEGORY-ARRAY-LOADED-SW.


       P08000-LOAD-WORK-ARRAY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P08020-COUNT-CATEGORY                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO COUNT THE TOTAL NUMBER OF           *
      *                CATEGORY / SUB-CATEGORY COMBINATIONS           *
      *                TO BE LOADED INTO THE WORK ARRAY               *
      *                (BEING DONE FOR DEMONSTRATION PURPOSES ONLY)   *
      *                                                               *
      *    CALLED BY:  P08000-LOAD-WORK-ARRAY                         *
      *                                                               *
      *****************************************************************

       P08020-COUNT-CATEGORY.


           COMPUTE WS-COUNT            =
                   WS-COUNT + PCAR-SUB-CATEGORY-COUNT (WS-CAT-SUB).


       P08020-COUNT-CATEGORY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P08050-LOAD-CATEGORY                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO LOAD CATEGORY INFORMATION INTO THE  *
      *                WORK ARRAY, AND CONTROL THE SUB-CATEGORY       *
      *                PROCESSING                                     *
      *                                                               *
      *    CALLED BY:  P08000-LOAD-WORK-ARRAY                         *
      *                                                               *
      *****************************************************************

       P08050-LOAD-CATEGORY.


      *****************************************************************
      *    D E M O N S T R A T I O N    P U R P O S E S   O N L Y     *
      *    THIS ARRAY NOT USED IN THE APPLICATION                     *
      *****************************************************************

           MOVE PCAR-SUB-CATEGORY-COUNT (WS-CAT-SUB)
                                       TO WSCA-SUB-CATEGORY-COUNT.
           SET  WSCA-SUBCAT-IX         TO 1.


      *****************************************************************
      *    LOAD THE SUB-CATEGORY INFORMATION OF THE ARRAY             *
      *****************************************************************

           PERFORM  P08100-LOAD-SUB-CATEGORY
               THRU P08100-LOAD-SUB-CATEGORY-EXIT
                   VARYING WS-SUBCAT-SUB FROM +1 BY +1
                       UNTIL WS-SUBCAT-SUB > PCAR-SUB-CATEGORY-COUNT
                                                     (WS-CAT-SUB).


       P08050-LOAD-CATEGORY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P08100-LOAD-SUB-CATEGORY                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO LOAD THE SUB-CATEGORY PORTION OF    *
      *                THE WORK ARRAY                                 *
      *                                                               *
      *    CALLED BY:  P08050-LOAD-CATEGORY                           *
      *                                                               *
      *****************************************************************

       P08100-LOAD-SUB-CATEGORY.

      *****************************************************************
      *    FORMAT THE CATEGORY AND SUB-CATEGORY INFORMATION           *
      *****************************************************************

           MOVE PCAR-CATEGORY (WS-CAT-SUB)
                                       TO WPCA-CATEGORY (WPCA-CAT-IX).
           MOVE PCAR-SUB-CATEGORY      (WS-CAT-SUB,  WS-SUBCAT-SUB)
               TO WPCA-SUB-CATEGORY    (WPCA-CAT-IX).


           IF WPCA-CAT-IX  <  WPCA-CATEGORY-COUNT
               SET WPCA-CAT-IX             UP BY 1.


      *****************************************************************
      *    D E M O N S T R A T I O N    P U R P O S E S   O N L Y     *
      *    THIS ARRAY NOT USED IN THE APPLICATION                     *
      *****************************************************************

           MOVE PCAR-SUB-CATEGORY      (WS-CAT-SUB,  WS-SUBCAT-SUB)
               TO WSCA-SUB-CATEGORY    (WSCA-SUBCAT-IX).

           IF WSCA-SUBCAT-IX  <  PCAR-SUB-CATEGORY-COUNT (WS-CAT-SUB)
               SET  WSCA-SUBCAT-IX     UP BY 1.


       P08100-LOAD-SUB-CATEGORY-EXIT.
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
      *    PARAGRAPH:  P70100-MESSAGE-ROUTINE                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO FORMAT A MESSAGE TO THE SCREEN      *
      *                IN A NON-ERROR SITUATION                       *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************

       P70100-MESSAGE-ROUTINE.

           IF PDAMSGO                  >  SPACES
               NEXT SENTENCE
           ELSE
               MOVE WMF-MESSAGE-AREA   TO PDAMSGO.


       P70100-MESSAGE-ROUTINE-EXIT.
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

           MOVE SPACES                 TO WMF-HOLD-CATEGORY.

           MOVE WMF-DATE-MMDDYY        TO PDADATEO.
           MOVE EIBTRMID               TO PDATERMO.
           MOVE WMF-TIME-HHMMSS        TO PDATIMEO.


      *****************************************************************
      *    MAKE FINAL ADJUSTMENTS TO ATTRIBUTES AND FIELDS            *
      *****************************************************************

           PERFORM  P79100-SET-MAP-FIELDS
               THRU P79100-SET-MAP-FIELDS-EXIT
                   VARYING WS-MAP-SUB FROM +1 BY +1
                       UNTIL WS-MAP-SUB > WS-MAP-LINES-MAX.


      *****************************************************************
      *    POSITION CURSOR AT APPROPRIATE LOCATION                    *
      *****************************************************************

           IF PC-PREV-PGRMID           =  'PDA005'
               IF NO-ERROR-FOUND
                   MOVE -1             TO M-SELECTION-LTH (1)
               ELSE
                   NEXT SENTENCE
           ELSE
                   MOVE -1             TO M-SELECTION-LTH (1).


      *****************************************************************
      *    SEND FULL MAP IF 1ST TIME, OTHERWISE SEND DATAONLY         *
      *****************************************************************

           IF PC-PREV-PGRMID  =  'PDA005'
               PERFORM  P80100-SEND-MAP-DATAONLY
                   THRU P80100-SEND-MAP-DATAONLY-EXIT
           ELSE
               PERFORM  P80000-SEND-FULL-MAP
                   THRU P80000-SEND-FULL-MAP-EXIT.


       P79000-DISPLAY-SCREEN-EXIT.
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
      *    IF CATEGORY PRESENT, MAKE SELECTION CODE ENTERABLE AND     *
      *    INITIALIZE TO UNDERSCORE IF SELECTION CODE NOT PRESENT,    *
      *    OTHERWISE PROTECT SELECTION CODE AND SET VALUE TO SPACES   *
      *****************************************************************

           IF M-CATEGORY (WS-MAP-SUB)  >  SPACES
               MOVE DFHBMFSE           TO M-SELECTION-ATTR (WS-MAP-SUB)
               INSPECT M-SELECTION  (WS-MAP-SUB)
                   CONVERTING  WMF-SPACES-LOWVALUE-R TO '__'
           ELSE
               MOVE DFHBMASF           TO M-SELECTION-ATTR(WS-MAP-SUB)
               MOVE SPACES             TO M-SELECTION     (WS-MAP-SUB).


      *****************************************************************
      *    MAKE CATEGORY VIEWABLE ONLY ON A CHANGE IN CATEGORY NAME,  *
      *    OTHERWISE MAKE CATEGORY NON-DISPLAY                        *
      *****************************************************************

           IF M-CATEGORY (WS-MAP-SUB)  >  SPACES
               IF M-CATEGORY (WS-MAP-SUB)  =  WMF-HOLD-CATEGORY
                   MOVE WMF-ASKIP-DARK-FSET
                                       TO M-CATEGORY-ATTR (WS-MAP-SUB)
               ELSE
                   MOVE DFHBMASF       TO M-CATEGORY-ATTR (WS-MAP-SUB)
           ELSE
               NEXT SENTENCE.


           MOVE M-CATEGORY (WS-MAP-SUB) TO WMF-HOLD-CATEGORY.


       P79100-SET-MAP-FIELDS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79200-CLEAR-MAP-FIELDS                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INITIALIZE THE MAP TO DEFAULT       *
      *                VALUES                                         *
      *                                                               *
      *    CALLED BY:  P06000-SCROLL-BACKWARD                         *
      *                P07000-SCROLL-FORWARD                          *
      *                                                               *
      *****************************************************************

       P79200-CLEAR-MAP-FIELDS.

           MOVE SPACES                 TO PDAMSGO.

           PERFORM  P79300-CLEAR-MAP-DETAIL
               THRU P79300-CLEAR-MAP-DETAIL-EXIT
                   VARYING WS-MAP-SUB FROM +1 BY +1
                       UNTIL WS-MAP-SUB  >  WS-MAP-LINES-MAX.


       P79200-CLEAR-MAP-FIELDS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79300-CLEAR-MAP-DETAIL                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INITIALIZE THE MAP DETAIL           *
      *                OCCURRENCES TO DEFAULT VALUES                  *
      *                                                               *
      *    CALLED BY:  P79200-CLEAR-MAP-FIELDS                        *
      *                                                               *
      *****************************************************************

       P79300-CLEAR-MAP-DETAIL.


           MOVE SPACES                 TO M-SELECTION     (WS-MAP-SUB)
                                          M-CATEGORY      (WS-MAP-SUB)
                                          M-SUB-CATEGORY  (WS-MAP-SUB).


       P79300-CLEAR-MAP-DETAIL-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80000-SEND-FULL-MAP                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DISPLAY THE INITIAL SCREEN          *
      *                                                               *
      *    CALLED BY:  P01000-MENU-PROCESS                            *
      *                                                               *
      *****************************************************************

       P80000-SEND-FULL-MAP.

           EXEC CICS SEND
                     MAP           ('PDA005')
                     MAPSET        ('PDA005M')
                     FROM          (PDA005O)
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
               MOVE 'PDA005'           TO WPCE-PROGRAM-ID
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
      *    FUNCTION :  ROUTINE TO DISPLAY THE SCREEN SENDING DATA     *
      *                ONLY (NO LITERALS)                             *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************

       P80100-SEND-MAP-DATAONLY.

      *****************************************************************
      *    SEND THE MAP DATA ONLY, DO NOT ERASE SCREEN                *
      *****************************************************************

           EXEC CICS SEND
                     MAP           ('PDA005')
                     MAPSET        ('PDA005M')
                     FROM          (PDA005O)
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
               MOVE 'PDA005'           TO WPCE-PROGRAM-ID
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
                     MAP           ('PDA005')
                     MAPSET        ('PDA005M')
                     INTO          (PDA005I)
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
               MOVE 'PDA005'           TO WPCE-PROGRAM-ID
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
      *                APPROPRIATE CICS FUNCTION BASED ON PFKEY OR    *
      *                SELECTION CODE ENTERED                         *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************

       P80300-XFER-CONTROL.

           EXEC CICS XCTL
                     PROGRAM       (PC-NEXT-PGRMID)
                     COMMAREA      (DFHCOMMAREA)
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
               MOVE 'PDA005'           TO WPCE-PROGRAM-ID
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
               MOVE 'PDA005'           TO WPCE-PROGRAM-ID
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
               MOVE 'PDA005'           TO WPCE-PROGRAM-ID
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
           MOVE 'PDA005'               TO WPCE-PROGRAM-ID.
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
AS OF      MOVE 'PDA005'               TO WPCE-PROGRAM-ID.               AS OF
JAN        MOVE EIBRESP                TO WPCE-RESPONSE-CODE.            JAN
2001       MOVE 'ERROR'                TO WPCE-COMMAND.                  2001
           MOVE 'P99999'               TO WPCE-PARAGRAPH.
LLR                                                                      LLR
           PERFORM  P99500-PDA-ERROR
               THRU P99500-PDA-ERROR-EXIT.

       P99999-ERROR-EXIT.
           EXIT.
           EJECT