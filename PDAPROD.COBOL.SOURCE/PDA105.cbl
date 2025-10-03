       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA105.

      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA105                                            *
      * TRANS   :   PDA10501                                          *
      *                                                               *
      * FUNCTION:   PROGRAM PDA105 IS THE IMS/DC PRODUCT DEMONSTRATION*
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
      *             PDA10101   MAIN MENU                              *
      *             PDA10401   CUSTOMER IDENTIFICATION                *
      *             PDA10601   BROWSE ITEMS BY CATEGORY               *
      *                                                               *
      *                                                               *
      * PFKEYS  :   PF3   =    PREVIOUS   (CUSTOMER IDENT. PDA10401)  *
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
       77  WS-SUB2                     PIC S9(04)   COMP    VALUE +0.
       77  WS-SCR-SUB                  PIC S9(04)   COMP    VALUE +0.
       77  WS-CAT-SUB                  PIC S9(04)   COMP    VALUE +0.
       77  WS-SUBCAT-SUB               PIC S9(04)   COMP    VALUE +0.
       77  WS-SCR-LINES-MAX            PIC S9(04)   COMP    VALUE +14.
       77  WS-COUNT                    PIC S9(04)   COMP    VALUE +0.

      *****************************************************************
      *    SWITCHES                                                   *
      *****************************************************************
       01  WS-SWITCHES.

           05  WS-MORE-MSGS-SW         PIC X(01)             VALUE 'Y'.
               88  MORE-MSGS                                 VALUE 'Y'.
               88  NO-MORE-MSGS                              VALUE 'N'.

           05  WS-TRANS-INTENT-SW      PIC X(01)             VALUE 'I'.
               88  INQUIRY-TRANS                             VALUE 'I'.
               88  UPDATE-TRANS                              VALUE 'U'.

           05  WS-ERROR-FOUND-SW       PIC X(01)             VALUE 'N'.
               88  ERROR-FOUND                               VALUE 'Y'.
               88  NO-ERROR-FOUND                            VALUE 'N'.

           05  WS-CATEGORY-SELECTION-SW
                                       PIC X(01)             VALUE ' '.
               88  CATEGORY-SELECTION-IS-VALID               VALUE 'S'.

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

           05  WMF-MODNAME             PIC X(08)   VALUE 'PDA105O'.
           05  WMF-MODNAME-ERROR       PIC X(08)   VALUE 'PDAERRO'.
           05  WMF-MASTER-LTERM-NAME   PIC X(08)   VALUE 'SMASTER'.
           05  WMF-IO-PCB-LTERM-NAME   PIC X(08)   VALUE SPACES.
           05  WMF-NEXT-TRANID         PIC X(08)   VALUE SPACES.
           05  WMF-NEXT-TRANID-R       REDEFINES   WMF-NEXT-TRANID.
               10  FILLER              PIC X(06).
               10  WMF-NEXT-TRANID-SEQ PIC X(02).
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
      *  IMS / DC FIELD ATTRIBUTE VALUES                              *
      *****************************************************************

       01  WS-ATTRIBUTE-VALUES.
      *    POSITION CURSOR  DEC=192, HEX=C0, BITS=11000000
           05  WS-CURSOR-ATTR          PIC X(01) VALUE '{'.

      *    HIGH INTEN,MOD   DEC=201, HEX=C9, BITS=11001001
           05  WS-HI-INTENSITY-ATTR    PIC X(01) VALUE 'I'.

      *    UNPROT,MOD       DEC=193, HEX=C1, BITS=11000001
           05  WS-UNPROT-MOD-ATTR      PIC X(01) VALUE 'A'.

      *    PROT,MOD         DEC=225, HEX=E1, BITS=11100001
           05  WS-PROT-MOD-ATTR        PIC X(01) VALUE '÷'.

      *    PROT,NODISP,MOD  DEC=229, HEX=E5, BITS=11100101
           05  WS-PROT-NODISP-MOD-ATTR PIC X(01) VALUE 'V'.
           EJECT

      *****************************************************************
      *    IMS  DEFINITIONS                                           *
      *****************************************************************

      *****************************************************************
      *         IMS FUNCTION CODE VALUES                              *
      *****************************************************************

           COPY IMSFUNC.
           EJECT
      *****************************************************************
      *         IMS/DC MESSAGE I/O AREAS                              *
      *****************************************************************
      *****************************************************************
      *  COMMON INPUT / OUTPUT MESSAGE STORAGE AREA                   *
      *  PREFIX: CIOM                                                 *
      *****************************************************************

       01  CIOM-MESSAGE.
           05 CIOM-MSG-LL              PIC S9(04)      COMP.
           05 CIOM-MSG-ZZ              PIC X(02).
           05 CIOM-MSG-TRANCODE        PIC X(08).
           05 FILLER                   PIC X(01).
           05 CIOM-MSG-SOURCE          PIC X(03).
           05 FILLER                   PIC X(01).
           05 CIOM-MSG-PFKEY           PIC X(02).
           05 CIOM-MSG-USERID-INFO.
              10  CIOM-USERID-ID       PIC X(08).
              10  CIOM-USERID-NUMBER   PIC 9(05).
           05 CIOM-PREV-PGRMID         PIC X(08).
           05 CIOM-SAVE-AREA           PIC X(79).
           05 CIOM-THE-REST            PIC X(1801).
           EJECT

      *****************************************************************
      *  PDA BROWSE CATEGORIES IN/OUT MESSAGE STORAGE AREA            *
      *  PREFIX: PDA105                                               *
      *****************************************************************

       01  PDA105-MESSAGE              REDEFINES CIOM-MESSAGE.
           05 PDA105-MSG-LL            PIC S9(04)      COMP.
           05 PDA105-MSG-ZZ            PIC X(02).
           05 PDA105-MSG-TRANCODE      PIC X(08).
           05 FILLER                   PIC X(01).
           05 PDA105-MSG-SOURCE        PIC X(03).
           05 FILLER                   PIC X(01).
           05 PDA105-PFKEY             PIC X(02).
           05 PDA105-MSG-USERID-INFO.
              10  PDA105-USERID-ID     PIC X(08).
              10  PDA105-USERID-NUMBER PIC 9(05).
           05 PDA105-PREV-PGRMID       PIC X(08).
           05 PDA105-SAVAREA           PIC X(79).
           05 PDA105-SAVAREA-R         REDEFINES PDA105-SAVAREA.
              10 PDA105-SAVAREA-ORDER-MENU-SEL
                                       PIC X(01).
              10 PDA105-SAVAREA-CUSID  PIC X(32).
              10 PDA105-ORIGINATING-PGRMID
                                       PIC X(08).
              10 PDA105-FIRST-CAT-SUB  PIC 9(03).
              10 PDA105-LAST-CAT-SUB   PIC 9(03).
              10 PDA105-SELECTED-CAT   PIC 9(03).
              10 PDA105-SELECTED-SUBCAT
                                       PIC 9(03).
           05 PDA105-CATEGORY-LINES    OCCURS 14 TIMES.
              10 PDA105-SELCODE-ATTR.
                  15 PDA105-SELCODE-ATTR1
                                       PIC X(01).
                  15 PDA105-SELCODE-ATTR2
                                       PIC X(01).
              10 PDA105-SELCODE        PIC X(01).
              10 PDA105-ITEM-CAT-ATTR.
                  15 PDA105-ITEM-CAT-ATTR1
                                       PIC X(01).
                  15 PDA105-ITEM-CAT-ATTR2
                                       PIC X(01).
              10 PDA105-ITEM-CAT       PIC X(32).
              10 PDA105-ITEM-SUBCAT    PIC X(32).
           05 PDA105-SCREEN-MESSAGE    PIC X(79).
           05 PDA105-SMESSAGE          PIC X(79).
           EJECT

      *****************************************************************
      *  PDA ERROR SCREEN OUTPUT MESSAGE STORAGE AREA                 *
      *  PREFIX: PDAERR                                               *
      *****************************************************************

       01  PDAERR-MESSAGE.
           05 PDAERR-MSG-LL            PIC S9(04)      COMP.
           05 PDAERR-MSG-ZZ            PIC X(02).
           05 PDAERR-MSGLIN01          PIC X(79).
           05 PDAERR-MSGLIN02          PIC X(79).
           05 PDAERR-MSGLIN03          PIC X(79).
           05 PDAERR-MSGLIN04          PIC X(79).
           05 PDAERR-MSGLIN05          PIC X(79).
           05 PDAERR-MSGLIN06          PIC X(79).
           05 PDAERR-MSGLIN07          PIC X(79).
           05 PDAERR-MSGLIN08          PIC X(79).
           05 PDAERR-MSGLIN09          PIC X(79).
           05 PDAERR-MSGLIN10          PIC X(79).
           05 PDAERR-SMESSAGE          PIC X(79).
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

      *****************************************************************
      *    IMS   I / O PCB MASK                                       *
      *****************************************************************

       01  IO-PCB.
           05  IO-PCB-LTERM-NAME       PIC X(08).
           05  FILLER                  PIC X(02).
           05  IO-PCB-STATUS           PIC X(02).
           05  IO-PCB-DATE             PIC S9(07)      COMP-3.
           05  IO-PCB-TIME             PIC S9(07)      COMP-3.
           05  IO-PCB-MSG-SEQ-NO       PIC S9(05)      COMP.
           05  IO-PCB-MOD-NAME         PIC X(08).
           05  IO-PCB-USERID           PIC X(08).
           05  IO-PCB-GROUP-NAME       PIC X(08).
           05  IO-PCB-TIME-STAMP       PIC X(12).
           05  IO-PCB-USERID-IND       PIC X(01).
           05  FILLER                  PIC X(03).

      *****************************************************************
      *    IMS ALTERNATE I / O PCB MASK (EXPRESS=YES, MODIFY=YES)     *
      *****************************************************************

       01  ALT-IO-PCB1.
           05  ALT-IO-PCB1-LTERM-NAME  PIC X(08).
           05  FILLER                  PIC X(02).
           05  ALT-IO-PCB1-STATUS      PIC X(02).


      *****************************************************************
      *    IMS ALTERNATE I / O PCB MASK (EXPRESS=NO, MODIFY=YES)      *
      *****************************************************************

       01  ALT-IO-PCB2.
           05  ALT-IO-PCB2-LTERM-NAME  PIC X(08).
           05  FILLER                  PIC X(02).
           05  ALT-IO-PCB2-STATUS      PIC X(02).
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

           ENTRY 'DLITCBL'   USING  IO-PCB
                                    ALT-IO-PCB1
                                    ALT-IO-PCB2.


      *****************************************************************
      *    PROCESS INCOMING IMS MESSSAGES UNTIL NO MORE               *
      *****************************************************************

           MOVE 'Y'                    TO WS-MORE-MSGS-SW.
                                                                        03/13/01
           PERFORM  P00100-MAIN-PROCESS
               THRU P00100-MAIN-PROCESS-EXIT
                  UNTIL NO-MORE-MSGS.


           GOBACK.

       P00000-MAINLINE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00100-MAIN-PROCESS                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL PROGRAM INITIALIZATION,     *
      *                IMS MESSAGE PROCESSES, INQUIRY/EDIT/UPDATE     *
      *                OPERATIONS                                     *
      *                                                               *
      *    CALLED BY:  P00000-MAINLINE                                *
      *                                                               *
      *****************************************************************

       P00100-MAIN-PROCESS.

           PERFORM  P00200-INITIALIZE                                   TAGGED
               THRU P00200-INITIALIZE-EXIT.                             CODE


      *****************************************************************
      *    PROCESS THE INCOMING IMS/DC MESSAGE                        *
      *****************************************************************

           PERFORM  P01000-PROCESS-MSG
               THRU P01000-PROCESS-MSG-EXIT.


       P00100-MAIN-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00200-INITIALIZE                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INITIALIZE RELEVANT WORK FIELDS     *
      *                AND VARIABLES, PERFORM ONE TIME TASKS          *
      *                FOR EACH IMS MESSAGE PROCESSED                 *
      *                                                               *
      *    CALLED BY:  P00100-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P00200-INITIALIZE.

      *****************************************************************
      *    INITIALIZE SWITCHES / SUBSCRIPTS                           *
      *****************************************************************

           MOVE 'I'                    TO WS-TRANS-INTENT-SW.
           MOVE 'N'                    TO WS-ERROR-FOUND-SW.
           MOVE SPACES                 TO WS-CATEGORY-SELECTION-SW.

           MOVE 'N'                    TO WS-SELECTION-DATA-ENTERED-SW
                                          WS-TOP-OF-DATA-SW
                                          WS-BOTTOM-OF-DATA-SW
                                          WS-CATEGORY-ARRAY-LOADED-SW.

           MOVE SPACES                 TO WMF-IO-PCB-LTERM-NAME
                                          WMF-NEXT-TRANID
                                          WMF-DATE-MMDDYY
                                          WMF-TIME-HHMMSS
                                          WMF-MESSAGE-AREA
                                          WMF-HOLD-CATEGORY.

      *****************************************************************
      *    OBTAIN CURRENT SYSTEM DATE / TIME                          *
      *****************************************************************

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME.          00020001


      *****************************************************************
      *    INITIALIZE MESSAGE AREAS, WORK AREAS ETC.                  *
      *****************************************************************

           MOVE LOW-VALUES            TO CIOM-MESSAGE.                  00020001


       P00200-INITIALIZE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01000-PROCESS-MSG                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ THE INCOMING IMS MESSAGE,      *
      *                PERFORM THE SCREEN EDIT PROCESS                *
      *                                                               *
      *    CALLED BY:  P00100-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P01000-PROCESS-MSG.

      *****************************************************************
      *    RETRIEVE THE IMS / DC MESSAGE                              *
      *****************************************************************

           CALL 'CBLTDLI'    USING     GU
                                       IO-PCB
                                       CIOM-MESSAGE.

           MOVE IO-PCB-LTERM-NAME      TO WMF-IO-PCB-LTERM-NAME.


           IF IO-PCB-STATUS            = SPACES
               NEXT SENTENCE
           ELSE
           IF IO-PCB-STATUS            = 'QC'
               MOVE 'N'                TO WS-MORE-MSGS-SW
               GO TO P01000-PROCESS-MSG-EXIT
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA105'           TO WPIE-PROGRAM-ID
               MOVE IO-PCB-STATUS      TO WPIE-STATUS-CODE
               MOVE 'GU'               TO WPIE-FUNCTION-CODE
               MOVE 'P01000'           TO WPIE-PARAGRAPH
               MOVE 'GU IO-PCB FOR IMS/DC MESSAGE'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    VERIFY TRANSACTION ENTRY INTO THIS PROGRAM WAS VIA A       *
      *    LEGITIMATE SOURCE AND FORMAT                               *
      *    (TRANCODE AND SOURCE OF MESSAGE FROM A VALID APPL SCREEN)  *
      *****************************************************************

           IF CIOM-MSG-TRANCODE        = 'PDA10501'    AND
              CIOM-MSG-SOURCE          = 'PDA'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA105'           TO WPIE-PROGRAM-ID
               MOVE 'P01000'           TO WPIE-PARAGRAPH
               MOVE 'USE /FOR PDAMNUO FOR MAIN MENU'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    VERIFY USER HAS PERFORMED IMS SIGNON BY CHECKING VALUES    *
      *    IN THE IMS IO-PCB                                          *
      *    (IO-PCB-USERID-IND OF -U- INDICATES VALUE IN IO-PCB-USERID *
      *     IS A LEGITIMATE USERID VIA IMS SIGNON)                    *
      *****************************************************************

           IF (IO-PCB-USERID           = SPACES OR LOW-VALUES)    OR
              (IO-PCB-USERID-IND  NOT  = 'U')
               MOVE PM051-USER-SIGNON-REQUIRED
                                       TO  PDA105-SCREEN-MESSAGE
               PERFORM  P80000-INSERT-MSG
                   THRU P80000-INSERT-MSG-EXIT
               GO TO P01000-PROCESS-MSG-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    PROCESS THE INCOMING MESSAGE                               *
      *****************************************************************
                                                                        03/13/01
           PERFORM  P01500-MAIN-PROCESS
               THRU P01500-MAIN-PROCESS-EXIT.


       P01000-PROCESS-MSG-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01500-MAIN-PROCESS                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL PROGRAM INQUIRY OR          *
      *                EDIT / UPDATE PROCESSES                        *
      *                                                               *
      *    CALLED BY:  P01000-PROCESS-MSG                             *
      *                                                               *
      *****************************************************************

       P01500-MAIN-PROCESS.

      *****************************************************************
      *    IF 1ST TIME THRU, PERFORM INITIAL SCREEN BUILD PROCESSES,  *
      *    OTHERWISE PROCESS EDIT / UPDATE TRANSACTION                *
      *****************************************************************

           IF PDA105-PREV-PGRMID       NOT = 'PDA105'
               PERFORM  P01800-1ST-TIME-PROCESS
                   THRU P01800-1ST-TIME-PROCESS-EXIT
           ELSE
               PERFORM  P02000-PROCESS-TRANS
                   THRU P02000-PROCESS-TRANS-EXIT.


       P01500-MAIN-PROCESS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01800-1ST-TIME-PROCESS                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL PROGRAM INITIAL INQUIRY     *
      *                PROCESSES                                      *
      *                                                               *
      *    CALLED BY:  P01500-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P01800-1ST-TIME-PROCESS.

           MOVE LOW-VALUES             TO CIOM-THE-REST.

      *****************************************************************
      *    SET INITIAL INQUIRY TO START FROM THE BEGINNING OF THE     *
      *    CATEGORY / SUB-CATEGORY ARRAY (USE SCROLL FORWARD ROUTINE) *
      *****************************************************************

           MOVE PDA105-PREV-PGRMID     TO PDA105-ORIGINATING-PGRMID.

           MOVE +1                     TO PDA105-FIRST-CAT-SUB.
           MOVE ZEROES                 TO PDA105-LAST-CAT-SUB
                                          PDA105-SELECTED-CAT
                                          PDA105-SELECTED-SUBCAT.

           PERFORM  P07000-SCROLL-FORWARD
               THRU P07000-SCROLL-FORWARD-EXIT.


      *****************************************************************
      *    DISPLAY THE INITIAL SCREEN, SAVE CONTROL VARIABLES         *
      *****************************************************************

           PERFORM  P79000-DISPLAY-SCREEN
               THRU P79000-DISPLAY-SCREEN-EXIT.

           MOVE 'PDA105'               TO  PDA105-PREV-PGRMID.


       P01800-1ST-TIME-PROCESS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P02000-PROCESS-TRANS                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL EDIT / UPDATE / PFKEY       *
      *                REQUESTS FOR THE SCREEN                        *
      *                                                               *
      *    CALLED BY:  P01500-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P02000-PROCESS-TRANS.

           MOVE 'PDA105'               TO  PDA105-PREV-PGRMID.

      *****************************************************************
      *    CONVERT UNDERSCORES AND LOW-VALUES TO SPACES               *
      *****************************************************************

           MOVE SPACES                 TO  PDA105-SCREEN-MESSAGE.

           PERFORM  P02100-CONVERT-FIELDS
               THRU P02100-CONVERT-FIELDS-EXIT
                   VARYING WS-SCR-SUB  FROM +1  BY  +1
                       UNTIL WS-SCR-SUB  >  WS-SCR-LINES-MAX.


      *****************************************************************
      *    PROCEED WITH THE TRANSACTION EDIT PROCESSES                *
      *****************************************************************

           PERFORM  P03000-EDIT-PROCESS
               THRU P03000-EDIT-PROCESS-EXIT.


      *****************************************************************
      *    DISPLAY THE OUTPUT SCREEN, ONLY IF:                        *
      *    ERROR FOUND, PF7 SCROLL BACK, PF8 SCROLL FORWARD           *
      *                                                               *
      *    OTHERWISE CONTROL IS BEING PASSED TO ANOTHER FUNCTION      *
      *****************************************************************

           IF (ERROR-FOUND) OR
              (PDA105-PFKEY = '07' OR '08')
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


           INSPECT PDA105-SELCODE (WS-SCR-SUB)
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

           IF PDA105-PFKEY  =  'EN'
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
      *    VALID KEYS ARE: ENTER, PF3, PF7, PF8, PF12                 *
      *****************************************************************

           IF PDA105-PFKEY = 'EN' OR '03' OR '07' OR '08' OR '12'
               NEXT SENTENCE
           ELSE
               MOVE LOW-VALUES         TO  PDA105-SELCODE-ATTR  (1)
               MOVE WS-CURSOR-ATTR     TO  PDA105-SELCODE-ATTR1 (1)
               MOVE PM001-INVALID-PFKEY
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P03500-EDIT-TRANS-INTENT-EXIT.


      *****************************************************************
      *    DETERMINE IF ANY SELECTION CODES HAVE BEEN ENTERED         *
      *    (SELECTION CODES AND PFKEYS ARE MUTUALLY EXCLUSIVE)        *
      *****************************************************************

           MOVE ZEROES             TO WS-COUNT.

           PERFORM  P03600-CHK-SELECTION-DATA
               THRU P03600-CHK-SELECTION-DATA-EXIT
                   VARYING WS-SCR-SUB  FROM  +1  BY  +1
                       UNTIL WS-SCR-SUB  >  WS-SCR-LINES-MAX.


           IF PDA105-PFKEY         NOT = 'EN'
               IF SELECTION-DATA-ENTERED
                   MOVE PM003-ACTION-VS-PFKEY-CONFLICT
                                       TO  WMF-MESSAGE-AREA
                   PERFORM  P70000-ERROR-ROUTINE
                       THRU P70000-ERROR-ROUTINE-EXIT
                   PERFORM  P03700-HILITE-SELECTIONS
                       THRU P03700-HILITE-SELECTIONS-EXIT
                           VARYING WS-SCR-SUB FROM +1 BY +1
                               UNTIL WS-SCR-SUB > WS-SCR-LINES-MAX
                   GO TO P03500-EDIT-TRANS-INTENT-EXIT
               ELSE
                   NEXT SENTENCE
           ELSE
                   NEXT SENTENCE.


      *****************************************************************
      *    ONE SELECTION CODE MUST BE ENTERED, MORE THAN 1 IS ERROR   *
      *****************************************************************

           IF PDA105-PFKEY             =  'EN'
               IF NO-SELECTION-DATA-ENTERED
                   MOVE LOW-VALUES     TO  PDA105-SELCODE-ATTR  (1)
                   MOVE WS-CURSOR-ATTR TO  PDA105-SELCODE-ATTR1 (1)
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
                           VARYING WS-SCR-SUB FROM +1 BY +1
                               UNTIL WS-SCR-SUB > WS-SCR-LINES-MAX
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


           IF PDA105-SELCODE (WS-SCR-SUB) >  SPACES
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


           IF PDA105-SELCODE (WS-SCR-SUB) >  SPACES
               MOVE LOW-VALUES         TO  PDA105-SELCODE-ATTR
                                                           (WS-SCR-SUB)
               MOVE WS-CURSOR-ATTR     TO  PDA105-SELCODE-ATTR1
                                                           (WS-SCR-SUB)
               MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA105-SELCODE-ATTR2
                                                           (WS-SCR-SUB)
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
      *    PF3 (PREVIOUS), TRANSFER TO CUSTOMER IDENT (PDA104) IF:    *
      *    1) ORIGINAL MENU SELECTION WAS ORDER ADD (SELECTION 1)     *
      *    2) ORIGINAL MENU SELECTION WAS PENDING ORDER (SELECTION 4) *
      *    3) PREVIOUS PROGRAM WAS EITHER CUSTOMER IDENT (PDA104) OR  *
      *       OR PENDING ORDER (PDA108)                               *
      *                                                               *
      *    OTHERWISE TRANSFER TO THE ORIGINATING PROGRAM              *
      *****************************************************************

           IF PDA105-PFKEY = '03'
               IF (PDA105-SAVAREA-ORDER-MENU-SEL = '1' OR '4')    OR
                  (PDA105-ORIGINATING-PGRMID = 'PDA104' OR 'PDA108')
                   MOVE 'PDA10401'     TO  WMF-NEXT-TRANID
                   PERFORM  P80300-XFER-CONTROL
                       THRU P80300-XFER-CONTROL-EXIT
                   GO TO P04000-PFKEY-PROCESS-EXIT
               ELSE
                   MOVE PDA105-ORIGINATING-PGRMID
                                       TO  WMF-NEXT-TRANID
                   MOVE '01'           TO  WMF-NEXT-TRANID-SEQ
                   PERFORM  P80300-XFER-CONTROL
                       THRU P80300-XFER-CONTROL-EXIT
                   GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF PF7, PERFORM SCROLL BACKWARD PROCESS                    *
      *****************************************************************

           IF PDA105-PFKEY  =  '07'
               PERFORM  P06000-SCROLL-BACKWARD
                   THRU P06000-SCROLL-BACKWARD-EXIT
               GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF PF8, PERFORM SCROLL FORWARD PROCESS                     *
      *****************************************************************

           IF PDA105-PFKEY  =  '08'
               PERFORM  P07000-SCROLL-FORWARD
                   THRU P07000-SCROLL-FORWARD-EXIT
               GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF PF12, RETURN TO THE MAIN MENU                           *
      *****************************************************************

           IF PDA105-PFKEY  =  '12'
               MOVE 'PDA10101'     TO  WMF-NEXT-TRANID
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
                   VARYING WS-SCR-SUB FROM +1 BY +1
                       UNTIL PDA105-SELCODE (WS-SCR-SUB) > SPACES.


           IF PDA105-SELCODE (WS-SCR-SUB)    = 'S'
               NEXT SENTENCE
           ELSE
               MOVE LOW-VALUES         TO  PDA105-SELCODE-ATTR
                                                           (WS-SCR-SUB)
               MOVE WS-CURSOR-ATTR     TO  PDA105-SELCODE-ATTR1
                                                           (WS-SCR-SUB)
               MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA105-SELCODE-ATTR2
                                                           (WS-SCR-SUB)
               MOVE PM012-INVALID-SEL-CODE
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P05000-PROCESS-SCREEN-EXIT.


      *****************************************************************
      *    CAPTURE CAT/SUB-CAT INDEXES, STORE IN SCREEN SAVAREA,      *
      *    TRANSFER CONTROL TO BROWSE ITEMS BY CATEGORY (PDA106)      *
      *****************************************************************

           MOVE ZEROES                 TO WS-CAT-SUB
                                          WS-SUBCAT-SUB.

           PERFORM  P05200-SEARCH-CAT
               THRU P05200-SEARCH-CAT-EXIT
                   VARYING WS-SUB1 FROM +1 BY +1
                       UNTIL WS-SUB1 > PDA-CATEGORY-MAX.

           IF WS-CAT-SUB               = ZEROES  OR
              WS-SUBCAT-SUB            = ZEROES
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA105'           TO WPIE-PROGRAM-ID
               MOVE 'P05000'           TO WPIE-PARAGRAPH
               MOVE 'PGM ERROR, CAT/SUBCAT NOT FOUND'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


           MOVE WS-CAT-SUB             TO PDA105-SELECTED-CAT.
           MOVE WS-SUBCAT-SUB          TO PDA105-SELECTED-SUBCAT.

           MOVE 'PDA10601'             TO WMF-NEXT-TRANID.

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
      *    PARAGRAPH:  P05200-SEARCH-CAT                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO SEARCH CATEGORY / SUB CATEGORY      *
      *                ARRAYS FOR THE ENTRIES MATCHING THE USER       *
      *                SCREEN SELECTION                               *
      *                                                               *
      *    CALLED BY:  P05000-PROCESS-SCREEN                          *
      *                                                               *
      *****************************************************************

       P05200-SEARCH-CAT.

      *****************************************************************
      *    WHEN SCREEN CATEGORY = ARRAY CATEGORY SAVE THE INDEX,      *
      *    LOCATE THE SUB-CATEGORY ENTRY                              *
      *****************************************************************

           IF PDA105-ITEM-CAT (WS-SCR-SUB) =
                                      PCAR-CATEGORY (WS-SUB1)
               MOVE WS-SUB1            TO WS-CAT-SUB

               PERFORM  P05230-SEARCH-SUBCAT
                   THRU P05230-SEARCH-SUBCAT-EXIT
                       VARYING WS-SUB2 FROM +1 BY +1
                           UNTIL WS-SUB2 > PDA-SUB-CATEGORY-MAX
           ELSE
               NEXT SENTENCE.


       P05200-SEARCH-CAT-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05230-SEARCH-SUBCAT                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO SEARCH THE SUB CATEGORY             *
      *                ARRAY FOR THE ENTRY MATCHING THE USER          *
      *                SCREEN SELECTION                               *
      *                                                               *
      *    CALLED BY:  P05200-SEARCH-CAT                              *
      *                                                               *
      *****************************************************************

       P05230-SEARCH-SUBCAT.

      *****************************************************************
      *    WHEN SCREEN SUB CATEGORY = ARRAY SUB CATEGORY SAVE THE INDX*
      *****************************************************************

           IF PDA105-ITEM-SUBCAT (WS-SCR-SUB) =
                              PCAR-SUB-CATEGORY (WS-CAT-SUB, WS-SUB2)
               MOVE WS-SUB2            TO WS-SUBCAT-SUB
           ELSE
               NEXT SENTENCE.

       P05230-SEARCH-SUBCAT-EXIT.
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
      *    VARIABLES AND SCREEN DISPLAY                               *
      *****************************************************************

           PERFORM  P06050-GET-BACKWARD-KEY
               THRU P06050-GET-BACKWARD-KEY-EXIT.

           MOVE ZEROES                 TO PDA105-FIRST-CAT-SUB
                                          PDA105-LAST-CAT-SUB.

           PERFORM  P79200-CLEAR-SCR-FIELDS
               THRU P79200-CLEAR-SCR-FIELDS-EXIT.


      *****************************************************************
      *    IF NOT AT TOP OF DATA (I.E. MORE DATA TO DISPLAY),         *
      *    BUILD THE SCREEN DISPLAY FROM THE WORK CATEGORY ARRAY      *
      *    (BUILD FROM END OF THE MAP UP AND FROM THE PREDETERMINED   *
      *     STARTING POINT IN THE CATEGORY ARRAY UP -- BOTTOM TO TOP) *
      *****************************************************************

           IF NOT-TOP-OF-DATA
               MOVE WS-SCR-LINES-MAX   TO WS-SCR-SUB

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

               MOVE +1                 TO PDA105-FIRST-CAT-SUB
               MOVE ZEROES             TO PDA105-LAST-CAT-SUB
               PERFORM  P07000-SCROLL-FORWARD
                   THRU P07000-SCROLL-FORWARD-EXIT

               MOVE PM014-TOP-MSG      TO WMF-MESSAGE-AREA
               PERFORM  P70100-INFO-MSG-ROUTINE
                   THRU P70100-INFO-MSG-ROUTINE-EXIT

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

           IF PDA105-FIRST-CAT-SUB     >  ZEROES
               MOVE PDA105-FIRST-CAT-SUB TO WS-CAT-SUB
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

           IF PDA105-FIRST-CAT-SUB     >  ZEROES
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

           IF WS-SCR-SUB               =   +1
               MOVE WS-CAT-SUB         TO  PDA105-FIRST-CAT-SUB
           ELSE
           IF WS-SCR-SUB               =   WS-SCR-LINES-MAX
               MOVE WS-CAT-SUB         TO  PDA105-LAST-CAT-SUB
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    FORMAT SCREEN CATEGORY / SUB-CATEGORY INFORMATION           *
      *****************************************************************

           MOVE WPCA-CATEGORY     (WPCA-CAT-IX)
                               TO PDA105-ITEM-CAT    (WS-SCR-SUB).
           MOVE WPCA-SUB-CATEGORY (WPCA-CAT-IX)
                               TO PDA105-ITEM-SUBCAT (WS-SCR-SUB).


      *****************************************************************
      *    IF SCREEN IS FULL, SET VARIABLES TO TERMINATE PROCESS       *
      *****************************************************************

           COMPUTE WS-SCR-SUB          =   WS-SCR-SUB -  +1.

           IF WS-SCR-SUB               <   +1
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

           MOVE ZEROES                 TO PDA105-FIRST-CAT-SUB
                                          PDA105-LAST-CAT-SUB.

           PERFORM  P79200-CLEAR-SCR-FIELDS
               THRU P79200-CLEAR-SCR-FIELDS-EXIT.


      *****************************************************************
      *    BUILD THE SCREEN DISPLAY FROM THE WORK CATEGORY ARRAY,     *
      *    IF END OF DATA ENCOUNTERED  -- FORMAT MESSAGE              *
      *****************************************************************

           MOVE ZEROES                 TO WS-SCR-SUB.

           PERFORM  P07100-BUILD-SCREEN
               THRU P07100-BUILD-SCREEN-EXIT.


           IF BOTTOM-OF-DATA
               MOVE PM013-BOTTOM-MSG   TO WMF-MESSAGE-AREA
               PERFORM  P70100-INFO-MSG-ROUTINE
                   THRU P70100-INFO-MSG-ROUTINE-EXIT
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

           IF PDA105-LAST-CAT-SUB      >  ZEROES
               MOVE PDA105-LAST-CAT-SUB TO WS-CAT-SUB
           ELSE

      *****************************************************************
      *    IF NO LAST LINE SCREEN ENTRY, USE SCREEN FIRST LINE ENTRY  *
      *    (IF PRESENT) AS THE STARTING POINT                         *
      *****************************************************************

           IF PDA105-FIRST-CAT-SUB     >  ZEROES
               MOVE PDA105-FIRST-CAT-SUB TO WS-CAT-SUB
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

           IF PDA105-ITEM-CAT (WS-SCR-LINES-MAX)  >  SPACES
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

           SET WPCA-CAT-IX             TO WS-CAT-SUB.

      *****************************************************************
      *    IF SCREEN IS FULL, SAVE KEY INFORMATION FOR FIRST AND      *
      *    LAST ENTRIES DISPLAYED ON THE SCREEN                       *
      *****************************************************************

           ADD +1                      TO  WS-SCR-SUB.

           IF WS-SCR-SUB               >   WS-SCR-LINES-MAX
               MOVE WPCA-CATEGORY-MAX  TO  WS-CAT-SUB
               GO TO P07200-BUILD-CATEGORY-EXIT
           ELSE
           IF WS-SCR-SUB               =   +1
               MOVE WS-CAT-SUB         TO  PDA105-FIRST-CAT-SUB
           ELSE
           IF WS-SCR-SUB               =   WS-SCR-LINES-MAX
               MOVE WS-CAT-SUB         TO  PDA105-LAST-CAT-SUB
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    FORMAT SCREEN CATEGORY / SUB-CATEGORY INFORMATION           *
      *****************************************************************

           MOVE WPCA-CATEGORY     (WPCA-CAT-IX)
                               TO PDA105-ITEM-CAT    (WS-SCR-SUB).
           MOVE WPCA-SUB-CATEGORY (WPCA-CAT-IX)
                               TO PDA105-ITEM-SUBCAT (WS-SCR-SUB).


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
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA105'           TO WPIE-PROGRAM-ID
               MOVE 'P08000'           TO WPIE-PARAGRAPH
               MOVE 'INTERNAL ERROR-ARRAY OVERFLOW'
                                       TO WPIE-COMMAND
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

      *****************************************************************
      *    MAKE FINAL ADJUSTMENTS TO ATTRIBUTES AND FIELDS            *
      *****************************************************************

           PERFORM  P79100-SET-SCR-FIELDS
               THRU P79100-SET-SCR-FIELDS-EXIT
                   VARYING WS-SCR-SUB FROM +1 BY +1
                       UNTIL WS-SCR-SUB > WS-SCR-LINES-MAX.


      *****************************************************************
      *    POSITION CURSOR AT APPROPRIATE LOCATION                    *
      *****************************************************************

           IF PDA105-PREV-PGRMID       =  'PDA105'
               IF NO-ERROR-FOUND
                   MOVE LOW-VALUES     TO PDA105-SELCODE-ATTR  (1)
                   MOVE WS-CURSOR-ATTR TO PDA105-SELCODE-ATTR1 (1)
               ELSE
                   NEXT SENTENCE
           ELSE
                   MOVE LOW-VALUES     TO PDA105-SELCODE-ATTR  (1)
                   MOVE WS-CURSOR-ATTR TO PDA105-SELCODE-ATTR1 (1).


      *****************************************************************
      *    INSERT MESSAGE TO TERMINAL                                 *
      *****************************************************************

           PERFORM  P80000-INSERT-MSG
               THRU P80000-INSERT-MSG-EXIT.

       P79000-DISPLAY-SCREEN-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79100-SET-SCR-FIELDS                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO SET SCREEN FIELDS AND               *
      *                ATTRIBUTES BASED ON DATA PRESENT ON THE SCREEN *
      *                                                               *
      *    CALLED BY:  P79000-DISPLAY-SCREEN                          *
      *                                                               *
      *****************************************************************

       P79100-SET-SCR-FIELDS.

      *****************************************************************
      *    IF CATEGORY PRESENT, MAKE SELECTION CODE ENTERABLE AND     *
      *    INITIALIZE TO UNDERSCORE IF SELECTION CODE NOT PRESENT,    *
      *    OTHERWISE PROTECT SELECTION CODE AND SET VALUE TO SPACES   *
      *****************************************************************

           IF PDA105-ITEM-CAT (WS-SCR-SUB)  >  SPACES
               INSPECT PDA105-SELCODE (WS-SCR-SUB)
                   CONVERTING  WMF-SPACES-LOWVALUE-R TO '__'
           ELSE
               MOVE LOW-VALUES         TO PDA105-SELCODE-ATTR1
                                                          (WS-SCR-SUB)
               MOVE WS-PROT-MOD-ATTR   TO PDA105-SELCODE-ATTR2
                                                          (WS-SCR-SUB)
               MOVE SPACES             TO PDA105-SELCODE  (WS-SCR-SUB).


      *****************************************************************
      *    MAKE CATEGORY VIEWABLE ONLY ON A CHANGE IN CATEGORY NAME,  *
      *    OTHERWISE MAKE CATEGORY NON-DISPLAY                        *
      *****************************************************************

           IF PDA105-ITEM-CAT (WS-SCR-SUB)  >  SPACES
               IF PDA105-ITEM-CAT (WS-SCR-SUB) = WMF-HOLD-CATEGORY
                   MOVE LOW-VALUES     TO PDA105-ITEM-CAT-ATTR1
                                                          (WS-SCR-SUB)
                   MOVE WS-PROT-NODISP-MOD-ATTR
                                       TO PDA105-ITEM-CAT-ATTR2
                                                          (WS-SCR-SUB)
               ELSE
                   MOVE PDA105-ITEM-CAT (WS-SCR-SUB)
                                       TO WMF-HOLD-CATEGORY
           ELSE
               NEXT SENTENCE.


       P79100-SET-SCR-FIELDS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79200-CLEAR-SCR-FIELDS                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INITIALIZE THE MAP TO DEFAULT       *
      *                VALUES                                         *
      *                                                               *
      *    CALLED BY:  P06000-SCROLL-BACKWARD                         *
      *                P07000-SCROLL-FORWARD                          *
      *                                                               *
      *****************************************************************

       P79200-CLEAR-SCR-FIELDS.

           MOVE SPACES                 TO PDA105-SCREEN-MESSAGE.

           PERFORM  P79300-CLEAR-SCR-DETAIL
               THRU P79300-CLEAR-SCR-DETAIL-EXIT
                   VARYING WS-SCR-SUB FROM +1 BY +1
                       UNTIL WS-SCR-SUB  >  WS-SCR-LINES-MAX.


       P79200-CLEAR-SCR-FIELDS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79300-CLEAR-SCR-DETAIL                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INITIALIZE THE SCREEN DETAIL        *
      *                OCCURRENCES TO DEFAULT VALUES                  *
      *                                                               *
      *    CALLED BY:  P79200-CLEAR-SCR-FIELDS                        *
      *                                                               *
      *****************************************************************

       P79300-CLEAR-SCR-DETAIL.


           MOVE SPACES            TO PDA105-SELCODE     (WS-SCR-SUB)
                                     PDA105-ITEM-CAT    (WS-SCR-SUB)
                                     PDA105-ITEM-SUBCAT (WS-SCR-SUB).

       P79300-CLEAR-SCR-DETAIL-EXIT.
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

           IF PDA105-SCREEN-MESSAGE    >  SPACES
               NEXT SENTENCE
           ELSE
               MOVE WMF-MESSAGE-AREA   TO PDA105-SCREEN-MESSAGE.

       P70000-ERROR-ROUTINE-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P70100-INFO-MSG-ROUTINE                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO HANDLE THE SCREEN INFORMATIONAL     *
      *                ONLY MESSAGE PROCESSING (NON-ERROR SITUATIONS) *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************

       P70100-INFO-MSG-ROUTINE.

           IF PDA105-SCREEN-MESSAGE    >  SPACES
               NEXT SENTENCE
           ELSE
               MOVE WMF-MESSAGE-AREA   TO PDA105-SCREEN-MESSAGE.

       P70100-INFO-MSG-ROUTINE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80000-INSERT-MSG                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INSERT THE OUTPUT MESSAGE TO THE    *
      *                TERMINAL USING THE IO-PCB                      *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************

       P80000-INSERT-MSG.

      *****************************************************************
      *    FORMAT OUTPUT MESSAGE FIELDS,                              *
      *    RESET ENTERABLE FIELDS BACK TO DEFAULT (IF NECESSARY)      *
      *****************************************************************

           MOVE 'PDA105'               TO PDA105-PREV-PGRMID.

           MOVE LENGTH OF PDA105-MESSAGE
                                       TO PDA105-MSG-LL.

      *****************************************************************
      *    WRITE THE IMS MESSAGE                                      *
      *****************************************************************

           CALL 'CBLTDLI'    USING     ISRT
                                       IO-PCB
                                       CIOM-MESSAGE
                                       WMF-MODNAME.


           IF IO-PCB-STATUS            = SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA105'           TO WPIE-PROGRAM-ID
               MOVE IO-PCB-STATUS      TO WPIE-STATUS-CODE
               MOVE 'ISRT'             TO WPIE-FUNCTION-CODE
               MOVE 'P80000'           TO WPIE-PARAGRAPH
               MOVE 'ISRT IO-PCB FOR IMS/DC MESSAGE'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P80000-INSERT-MSG-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80300-XFER-CONTROL                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO TRANSFER CONTROL TO THE             *
      *                APPROPRIATE IMS FUNCTION BASED ON THE DATA     *
      *                SELECTION ENTERED OR PFKEY UTILIZED            *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************

       P80300-XFER-CONTROL.

      *****************************************************************
      *    ISSUE CHANGE CALL TO SET THE TRANSACTION DESTINATION       *
      *****************************************************************

           CALL 'CBLTDLI'    USING     CHNG
                                       ALT-IO-PCB2
                                       WMF-NEXT-TRANID.


           IF ALT-IO-PCB2-STATUS       = SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA105'           TO WPIE-PROGRAM-ID
               MOVE ALT-IO-PCB2-STATUS TO WPIE-STATUS-CODE
               MOVE 'CHNG'             TO WPIE-FUNCTION-CODE
               MOVE 'P80300'           TO WPIE-PARAGRAPH
               MOVE 'CHNG ALT-IO-PCB2 TO TRANSACTION'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    FORMAT THE OUTPUT MESSAGE FOR THE NEXT TRANSACTION         *
      *****************************************************************

           COMPUTE CIOM-MSG-LL =
               LENGTH OF CIOM-MESSAGE - LENGTH OF CIOM-THE-REST.
           MOVE LOW-VALUES             TO CIOM-MSG-ZZ.

           MOVE WMF-NEXT-TRANID        TO CIOM-MSG-TRANCODE.
           MOVE 'PDA'                  TO CIOM-MSG-SOURCE.
           MOVE SPACES                 TO CIOM-MSG-PFKEY.
           MOVE 'PDA105'               TO CIOM-PREV-PGRMID.


      *****************************************************************
      *    INSERT THE NEXT TRANSACTON MESSAGE TO ALTERNATE PCB        *
      *****************************************************************

           CALL 'CBLTDLI'    USING     ISRT
                                       ALT-IO-PCB2
                                       CIOM-MESSAGE.


           IF ALT-IO-PCB2-STATUS       = SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA105'           TO WPIE-PROGRAM-ID
               MOVE ALT-IO-PCB2-STATUS TO WPIE-STATUS-CODE
               MOVE 'ISRT'             TO WPIE-FUNCTION-CODE
               MOVE 'P80300'           TO WPIE-PARAGRAPH
               MOVE 'ISRT ALT-IO-PCB2, IMS/DC MESSAGE'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P80300-XFER-CONTROL-EXIT.
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
      *                DB2, IMS-DB/DC, MQSERIES ERRORS                *
      *                                                               *
      *                AN ERROR SCREEN CONTAINING TEXT IS SENT        *
      *                TO THE USER INDICATING THE NATURE OF THE ERROR *
      *                                                               *
      *                AN ERROR SCREEN CONTAINING TEXT IS SENT        *
      *                TO THE IMS MASTER TERMINAL INDICATING THE      *
      *                NATURE OF THE ERROR                            *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************

       P99500-PDA-ERROR.

      *****************************************************************
      *    ISSUE ROLB TO BACKOUT ANY UPDATES AND PENDING MESSAGES     *
      *    (CONTROL RETURNED TO APPLICATION AFTER ROLL BACK)          *
      *****************************************************************

           CALL 'CBLTDLI'    USING     ROLB IO-PCB.


      *****************************************************************
      *    FORMAT THE IMS OUTPUT ERROR MESSAGES                       *
      *****************************************************************

           MOVE LOW-VALUES             TO PDAERR-MESSAGE.

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


           MOVE WPEA-ERROR-01          TO PDAERR-MSGLIN01.
           MOVE WPEA-ERROR-02          TO PDAERR-MSGLIN02.
           MOVE WPEA-ERROR-03          TO PDAERR-MSGLIN03.
           MOVE WPEA-ERROR-04          TO PDAERR-MSGLIN04.
           MOVE WPEA-ERROR-05          TO PDAERR-MSGLIN05.
           MOVE WPEA-ERROR-06          TO PDAERR-MSGLIN06.
           MOVE WPEA-ERROR-07          TO PDAERR-MSGLIN07.
           MOVE WPEA-ERROR-08          TO PDAERR-MSGLIN08.
           MOVE WPEA-ERROR-09          TO PDAERR-MSGLIN09.
           MOVE WPEA-ERROR-10          TO PDAERR-MSGLIN10.


      *****************************************************************
      *    CHNG, ISRT, PURG TO SEND MESSAGE TO ORIGINATING TERMINAL   *
      *    USING THE ALTERNATE EXPRESS PCB                            *
      *****************************************************************

           CALL 'CBLTDLI'    USING     CHNG
                                       ALT-IO-PCB1
                                       WMF-IO-PCB-LTERM-NAME.


           MOVE LENGTH OF PDAERR-MESSAGE
                                       TO PDAERR-MSG-LL.

           CALL 'CBLTDLI'    USING     ISRT
                                       ALT-IO-PCB1
                                       PDAERR-MESSAGE
                                       WMF-MODNAME-ERROR.

           CALL 'CBLTDLI'    USING     PURG
                                       ALT-IO-PCB1.


      *****************************************************************
      *    ISSUE CHNG, ISRT, PURG TO SEND MESSAGE TO MASTER TERMINAL  *
      *    USING THE ALTERNATE EXPRESS PCB                            *
      *****************************************************************

           CALL 'CBLTDLI'    USING     CHNG
                                       ALT-IO-PCB1
                                       WMF-MASTER-LTERM-NAME.


           MOVE LENGTH OF PDAERR-MESSAGE
                                       TO PDAERR-MSG-LL.

           CALL 'CBLTDLI'    USING     ISRT
                                       ALT-IO-PCB1
                                       PDAERR-MESSAGE
                                       WMF-MODNAME-ERROR.

           CALL 'CBLTDLI'    USING     PURG
                                       ALT-IO-PCB1.


      *****************************************************************
      *    ISSUE ROLL CALL TO TERMINATE IN-FLIGHT MESSAGES, UPDATES   *
      *                                                               *
      *    ROLL ABNORMALLY TERMINATES THE PROGRAM U778                *
      *****************************************************************
      *****
      *****CALL 'CBLTDLI'    USING     ROLL.
      *****

           GOBACK.

       P99500-PDA-ERROR-EXIT.
           EXIT.
           EJECT