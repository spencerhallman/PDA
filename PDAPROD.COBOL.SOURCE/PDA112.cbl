       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA112.

      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA112                                            *
      * TRANS   :   PDA11201                                          *
      *                                                               *
      * FUNCTION:   PROGRAM PDA112 IS THE IMS/DC PRODUCT DEMONSTRATION*
      *             APPLICATION BROWSE SUBMITTED ORDERS PROGRAM. THE  *
      *             ORDER SCREEN DISPLAYS A SCROLLABLE LIST OF ALL    *
      *             ORDERS FOR A PARTICULAR USER.                     *
      *                                                               *
      *                                                               *
      * FILES   :   ORDER DATABASE (ORDER2DB)   - IMS (READ ONLY)     *
      *                                                               *
      *                                                               *
      * TRANSACTIONS GENERATED:                                       *
      *             PDA10201   ORDER MENU                             *
      *             PDA10101   MAIN MENU                              *
      *                                                               *
      *                                                               *
      * PFKEYS  :   PF03  =    PREVIOUS- PDA102, ORDER MENU           *
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
       77  WS-SUB1                     PIC S9(04)   COMP    VALUE +0.
       77  WS-SCR-SUB                  PIC S9(04)   COMP    VALUE +0.
       77  WS-SCR-LINES-MAX            PIC S9(04)   COMP    VALUE +5.
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

           05  WS-PROCESS-COMPLETE-SW  PIC X(01)             VALUE 'N'.
               88  PROCESS-COMPLETE                          VALUE 'Y'.
               88  NOT-PROCESS-COMPLETE                      VALUE 'N'.

           05  WS-CUSTOMER-FOUND-SW    PIC X(01)             VALUE 'N'.
               88  CUSTOMER-FOUND                            VALUE 'Y'.
               88  NOT-CUSTOMER-FOUND                        VALUE 'N'.

           05  WS-MORE-SEGMENTS-SW     PIC X(01)             VALUE 'Y'.
               88  MORE-SEGMENTS                             VALUE 'Y'.
               88  NO-MORE-SEGMENTS                          VALUE 'N'.
           EJECT

      *****************************************************************
      *    MISCELLANEOUS WORK FIELDS                                  *
      *****************************************************************
       01  WS-MISCELLANEOUS-FIELDS.

           05  WMF-MODNAME             PIC X(08)   VALUE 'PDA112O'.
           05  WMF-MODNAME-ERROR       PIC X(08)   VALUE 'PDAERRO'.
           05  WMF-MASTER-LTERM-NAME   PIC X(08)   VALUE 'SMASTER'.
           05  WMF-IO-PCB-LTERM-NAME   PIC X(08)   VALUE SPACES.

           05  WMF-NEXT-TRANID         PIC X(08)   VALUE SPACES.
           05  WMF-NEXT-TRANID-R       REDEFINES   WMF-NEXT-TRANID.
               10  FILLER              PIC X(06).
               10  WMF-NEXT-TRANID-SEQ PIC X(02).

           05  WMF-DATE-MMDDYY         PIC X(08)   VALUE SPACES.

           05  WMF-DATE-YYMMDD.
               10  WMF-DATE-YY         PIC X(02).
               10  WMF-DATE-MM         PIC X(02).
               10  WMF-DATE-DD         PIC X(02).

           05  WMF-TIME-HHMMSS         PIC X(08)   VALUE SPACES.

           05  WMF-MESSAGE-AREA        PIC X(79)   VALUE SPACES.

           05  WMF-ORDER-NUMBER        PIC X(10)   VALUE ZEROES.
           05  WMF-ORDER-NUMBER-R      REDEFINES   WMF-ORDER-NUMBER
                                       PIC 9(10).

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
      *  PENDING ORDER IN/OUT MESSAGE STORAGE AREA                    *
      *  PREFIX: PDA112                                               *
      *****************************************************************

       01  PDA112-MESSAGE              REDEFINES CIOM-MESSAGE.
           05 PDA112-MSG-LL            PIC S9(04)      COMP.
           05 PDA112-MSG-ZZ            PIC X(02).
           05 PDA112-MSG-TRANCODE      PIC X(08).
           05 FILLER                   PIC X(01).
           05 PDA112-MSG-SOURCE        PIC X(03).
           05 FILLER                   PIC X(01).
           05 PDA112-PFKEY             PIC X(02).
           05 PDA112-MSG-USERID-INFO.
              10  PDA112-USERID-ID     PIC X(08).
              10  PDA112-USERID-NUMBER PIC 9(05).
           05 PDA112-PREV-PGRMID       PIC X(08).
           05 PDA112-SAVAREA           PIC X(79).
           05 PDA112-SAVAREA-R         REDEFINES PDA112-SAVAREA.
              10 PDA112-SAVAREA-ORDER-MENU-SEL
                                       PIC X(01).
              10 PDA112-SAVAREA-CUSID  PIC X(32).
              10 PDA112-ORIGINATING-PGRMID
                                       PIC X(08).
              10 PDA112-SAVAREA-ORDERNBR
                                       PIC X(10).
           05 PDA112-ORDER-LINES       OCCURS 5 TIMES.
              10 PDA112-SELCODE-ATTR.
                  15 PDA112-SELCODE-ATTR1
                                       PIC X(01).
                  15 PDA112-SELCODE-ATTR2
                                       PIC X(01).
              10 PDA112-SELCODE        PIC X(01).
              10 PDA112-ORDER-NUMBER   PIC X(10).
              10 PDA112-ORDER-NUMBER-R REDEFINES PDA112-ORDER-NUMBER
                                       PIC 9(10).
              10 PDA112-ORDER-DATE     PIC X(08).
              10 PDA112-ORDER-DATE-R   REDEFINES PDA112-ORDER-DATE.
                 15 PDA112-ORDER-DATE-MM
                                       PIC X(02).
                 15 PDA112-ORDER-DATE-SL1
                                       PIC X(01).
                 15 PDA112-ORDER-DATE-DD
                                       PIC X(02).
                 15 PDA112-ORDER-DATE-SL2
                                       PIC X(01).
                 15 PDA112-ORDER-DATE-YY
                                       PIC X(02).
              10 PDA112-ORDER-AMT      PIC X(12).
              10 PDA112-ORDER-AMT-R    REDEFINES PDA112-ORDER-AMT
                                       PIC Z,ZZZ,ZZ9.99.
              10 PDA112-ORDER-STATUS   PIC X(14).
              10 PDA112-CUSTOMER-NAME  PIC X(64).
           05 PDA112-SCREEN-MESSAGE    PIC X(79).
           05 PDA112-SMESSAGE          PIC X(79).
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
      *    IMS  DATABASE SEGMENT LAYOUTS                              *
      *****************************************************************
      *****************************************************************
      *    ORDER DATABASE ROOT SEGMENT                                *
      *****************************************************************
           COPY IORDER.
           EJECT

      *****************************************************************
      *    ORDER DATABASE ORDER ITEM SEGMENT                          *
      *****************************************************************
           COPY IORDITEM.
           EJECT

      *****************************************************************
      *    IMS  DATABASE SEGMENT SEARCH ARGUMENTS (SSA)               *
      *****************************************************************
      *****************************************************************
      *    ORDER DATABASE ROOT SEGMENT (SSA)                          *
      *****************************************************************

       01  ORDER-UNQUAL-SSA.
           05  ORDER-UNQUAL-SEGMENT    PIC X(09)    VALUE 'ORDER '.

       01  ORDER-QUAL-SSA.
           05  ORDER-QUAL-SEGMENT      PIC X(08)   VALUE 'ORDER '.
           05  ORDER-QUAL-LPAREN       PIC X(01)   VALUE '('.
           05  ORDER-QUAL-FIELD-NAME
                                       PIC X(08)   VALUE 'ORDKEY '.
           05  ORDER-QUAL-OPERATOR     PIC X(02)   VALUE 'EQ'.
           05  ORDER-QUAL-FIELD-VALUE.
               10  ORDER-QUAL-PREFIX   PIC X(05)   VALUE SPACES.
               10  ORDER-QUAL-NUMBER   PIC X(10)   VALUE SPACES.
               10  ORDER-QUAL-NUMBER-R REDEFINES
                                       ORDER-QUAL-NUMBER
                                       PIC 9(10).
           05  ORDER-QUAL-RPAREN       PIC X(01)   VALUE ')'.


      *****************************************************************
      *    ORDER DATABASE ORDER ITEM SEGMENT SSA                      *
      *****************************************************************

       01  ORDITEM-UNQUAL-SSA.
           05  ORDITEM-UNQUAL-SEGMENT  PIC X(09)    VALUE 'ORDITEM'.
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
      *    CUSTOMER ARRAY                                             *
      *****************************************************************

           COPY CUSARRAY.
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


      *****************************************************************
      *    IMS ORDER DATABASE (ORDER2DB) PCB MASK                     *
      *****************************************************************

           COPY PCBORDER.
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
      *                BROWSE SUBMITTED ORDERS SCREEN                 *
      *                                                               *
      *    CALLED BY:  NONE                                           *
      *                                                               *
      *****************************************************************

       P00000-MAINLINE.

           ENTRY 'DLITCBL'   USING  IO-PCB
                                    ALT-IO-PCB1
                                    ALT-IO-PCB2
                                    ORDER-PCB.


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

           MOVE 'Y'                    TO WS-MORE-SEGMENTS-SW.

           MOVE 'N'                    TO WS-ERROR-FOUND-SW
                                          WS-SELECTION-DATA-ENTERED-SW
                                          WS-TOP-OF-DATA-SW
                                          WS-BOTTOM-OF-DATA-SW
                                          WS-PROCESS-COMPLETE-SW
                                          WS-CUSTOMER-FOUND-SW.

           MOVE SPACES                 TO WMF-IO-PCB-LTERM-NAME
                                          WMF-NEXT-TRANID
                                          WMF-DATE-MMDDYY
                                          WMF-TIME-HHMMSS
                                          WMF-MESSAGE-AREA.

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
               MOVE 'PDA112'           TO WPIE-PROGRAM-ID
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

           IF CIOM-MSG-TRANCODE        = 'PDA11201'    AND
              CIOM-MSG-SOURCE          = 'PDA'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA112'           TO WPIE-PROGRAM-ID
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
                                       TO  PDA112-SCREEN-MESSAGE
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

           IF PDA112-PREV-PGRMID       NOT = 'PDA112'
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
      *    ORDER DATABASE (USE SCROLL FORWARD ROUTINE)                *
      *****************************************************************

           MOVE 1                      TO WMF-ORDER-NUMBER-R.

           PERFORM  P07000-SCROLL-FORWARD
               THRU P07000-SCROLL-FORWARD-EXIT.


      *****************************************************************
      *    DISPLAY THE INITIAL SCREEN, SAVE CONTROL VARIABLES         *
      *****************************************************************

           PERFORM  P79000-DISPLAY-SCREEN
               THRU P79000-DISPLAY-SCREEN-EXIT.

           MOVE 'PDA112'               TO  PDA112-PREV-PGRMID.


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

           MOVE 'PDA112'               TO  PDA112-PREV-PGRMID.

      *****************************************************************
      *    CONVERT UNDERSCORES AND LOW-VALUES TO SPACES               *
      *****************************************************************

           MOVE SPACES                 TO  PDA112-SCREEN-MESSAGE.

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
      *    DISPLAY THE OUTPUT SCREEN IF ENTER KEY, SCROLL BACKWARD,   *
      *    SCROLL FORWARD, OR ERROR FOUND                             *
      *****************************************************************

           IF (PDA112-PFKEY  =  'EN' OR '07' OR '08')  OR
              (ERROR-FOUND)
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


           INSPECT PDA112-SELCODE (WS-SCR-SUB)
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

           IF PDA112-PFKEY  =  'EN'
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
      *    VALID KEYS ARE: ENTER, PF3, PF7, PF8, PF11, PF12           *
      *****************************************************************

           IF PDA112-PFKEY = 'EN' OR '03' OR '07' OR '08' OR
                             '11' OR '12'
               NEXT SENTENCE
           ELSE
               MOVE PM001-INVALID-PFKEY
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P03500-EDIT-TRANS-INTENT-EXIT.


      *****************************************************************
      *    DETERMINE IF ANY SELECTION CODES HAVE BEEN ENTERED         *
      *    (ACTION CODES AND PFKEYS ARE MUTUALLY EXCLUSIVE)           *
      *****************************************************************

           MOVE ZEROES             TO WS-COUNT.

           PERFORM  P03600-CHK-SELECTION-DATA
               THRU P03600-CHK-SELECTION-DATA-EXIT
                   VARYING WS-SCR-SUB  FROM  +1  BY  +1
                       UNTIL WS-SCR-SUB  >  WS-SCR-LINES-MAX.


           IF PDA112-PFKEY         NOT = 'EN'
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
      *    DO SOMETHING -- EITHER ACTION CODE OR A PFKEY              *
      *****************************************************************

           IF PDA112-PFKEY             =  'EN'
               IF NO-SELECTION-DATA-ENTERED
                   MOVE PM025-MAKE-SELECTION
                                       TO  WMF-MESSAGE-AREA
                   PERFORM  P70000-ERROR-ROUTINE
                       THRU P70000-ERROR-ROUTINE-EXIT
                   GO TO P03500-EDIT-TRANS-INTENT-EXIT
               ELSE
                   NEXT SENTENCE
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    ONE SELECTION CODE MUST BE ENTERED, MORE THAN 1 IS ERROR   *
      *****************************************************************

           IF PDA112-PFKEY             =  'EN'
               IF NO-SELECTION-DATA-ENTERED
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


           IF PDA112-SELCODE (WS-SCR-SUB) >  SPACES
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


           IF PDA112-SELCODE (WS-SCR-SUB) >  SPACES
               MOVE LOW-VALUES         TO  PDA112-SELCODE-ATTR
                                                           (WS-SCR-SUB)
               MOVE WS-CURSOR-ATTR     TO  PDA112-SELCODE-ATTR1
                                                           (WS-SCR-SUB)
               MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA112-SELCODE-ATTR2
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
      *    PF3 (PREVIOUS), TRANSFER TO ORDER MENU (PDA102)            *
      *****************************************************************

           IF PDA112-PFKEY = '03'
               MOVE 'PDA10201'     TO  WMF-NEXT-TRANID
               PERFORM  P80300-XFER-CONTROL
                   THRU P80300-XFER-CONTROL-EXIT
               GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF PF7, PERFORM SCROLL BACKWARD PROCESS                    *
      *****************************************************************

           IF PDA112-PFKEY  =  '07'
               PERFORM  P06000-SCROLL-BACKWARD
                   THRU P06000-SCROLL-BACKWARD-EXIT
               GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF PF8, PERFORM SCROLL FORWARD PROCESS                     *
      *****************************************************************

           IF PDA112-PFKEY  =  '08'
               PERFORM  P07000-SCROLL-FORWARD
                   THRU P07000-SCROLL-FORWARD-EXIT
               GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF PF11, TRANSFER TO THE ORDER MENU                        *
      *****************************************************************

           IF PDA112-PFKEY  =  '11'
               MOVE 'PDA10201'     TO  WMF-NEXT-TRANID
               PERFORM  P80300-XFER-CONTROL
                   THRU P80300-XFER-CONTROL-EXIT
               GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF PF12, RETURN TO THE MAIN MENU                           *
      *****************************************************************

           IF PDA112-PFKEY  =  '12'
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
      *    FUNCTION :  ROUTINE TO CONTROL THE ORDER SELECTION PROCESS,*
      *                ENTER KEY WITH A SELECTION CODE TRANSFERS      *
      *                TO THE ORDER INQUIRY / UPDATE FUNCTION         *
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
                       UNTIL PDA112-SELCODE (WS-SCR-SUB) > SPACES.


           IF PDA112-SELCODE (WS-SCR-SUB)    = 'S'
               NEXT SENTENCE
           ELSE
               MOVE LOW-VALUES         TO  PDA112-SELCODE-ATTR
                                                           (WS-SCR-SUB)
               MOVE WS-CURSOR-ATTR     TO  PDA112-SELCODE-ATTR1
                                                           (WS-SCR-SUB)
               MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA112-SELCODE-ATTR2
                                                           (WS-SCR-SUB)
               MOVE PM012-INVALID-SEL-CODE
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P05000-PROCESS-SCREEN-EXIT.


      *****************************************************************
      *    CAPTURE THE SELECTED ORDER NUMBER FROM THE SCREEN AND      *
      *    STORE IN THE SCREEN SAVAREA FOR THE ORDER INQUIRY / UPDATE *
      *    FUNCTION TO UTILIZE (PROGRAM PDA110, TRAN PDA11001)        *
      *****************************************************************

           MOVE 'PDA112'               TO PDA112-ORIGINATING-PGRMID.
           MOVE PDA112-ORDER-NUMBER (WS-SCR-SUB)
                                       TO PDA112-SAVAREA-ORDERNBR.

           MOVE 'PDA11001'             TO WMF-NEXT-TRANID.

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
      *    DETERMINE THE SCROLL BACKWARD START KEY, INITIALIZE        *
      *    VARIABLES AND SCREEN DISPLAY                               *
      *****************************************************************

           PERFORM  P06050-GET-BACKWARD-KEY
               THRU P06050-GET-BACKWARD-KEY-EXIT.

           PERFORM  P79200-CLEAR-SCR-FIELDS
               THRU P79200-CLEAR-SCR-FIELDS-EXIT.


      *****************************************************************
      *    IF NOT AT TOP OF DATA (I.E. MORE DATA TO DISPLAY),         *
      *    BUILD FROM END OF THE MAP UP AND FROM THE PREDETERMINED    *
      *    STARTING POINT UP -- BOTTOM TO TOP)                        *
      *****************************************************************

           IF NOT-TOP-OF-DATA
               PERFORM  P06100-BUILD-SCREEN
                   THRU P06100-BUILD-SCREEN-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF TOP OF DATA ENCOUNTERED, I.E. NOT ENOUGH DATA TO FILL   *
      *    THE SCREEN, RE-INITIALIZE VARIABLES AND PERFORM THE        *
      *    STANDARD SCROLL FORWARD PROCESS FORM THE 1ST ORDER         *
      *    GOING FORWARD --- FROM TOP TO BOTTOM                       *
      *****************************************************************

           IF TOP-OF-DATA
               MOVE SPACES             TO PDA112-ORDER-NUMBER (1)
                                          PDA112-ORDER-NUMBER
                                                    (WS-SCR-LINES-MAX)

               PERFORM  P07000-SCROLL-FORWARD
                   THRU P07000-SCROLL-FORWARD-EXIT

               MOVE SPACES             TO PDA112-SCREEN-MESSAGE
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

           IF PDA112-ORDER-NUMBER (1)  >  ZEROES
               MOVE PDA112-ORDER-NUMBER (1)
                                       TO WMF-ORDER-NUMBER
           ELSE

      *****************************************************************
      *    OTHERWISE SET TOP OF DATA INDICATOR WHICH WILL FORCE A     *
      *    SCROLL FORWARD FROM THE BEGINNING OF THE ORDER DATABASE    *
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
      *                PROCESS TO POPULATE THE SCREEN WITH            *
      *                ORDER INFORMATION                              *
      *                                                               *
      *    CALLED BY:  P06000-SCROLL-BACKWARD                         *
      *                                                               *
      *****************************************************************

       P06100-BUILD-SCREEN.

      *****************************************************************
      *    PROCESS THE ORDERS FROM PRE-DETERMINED START POINT,        *
      *    RETRIEVE THE FIRST ORDER                                   *
      *****************************************************************

           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.
           MOVE ZEROES                 TO WS-COUNT.
           COMPUTE WS-SCR-SUB          =  WS-SCR-LINES-MAX.

           MOVE 'EQ'                   TO ORDER-QUAL-OPERATOR.
           MOVE PDA112-USERID-NUMBER   TO ORDER-QUAL-PREFIX.
           MOVE WMF-ORDER-NUMBER       TO ORDER-QUAL-NUMBER.

           PERFORM  P78000-GU-ORDER
               THRU P78000-GU-ORDER-EXIT.


           PERFORM  P06200-PROCESS-ORDERS
               THRU P06200-PROCESS-ORDERS-EXIT
                   UNTIL PROCESS-COMPLETE.

           IF ERROR-FOUND
               GO TO P06100-BUILD-SCREEN-EXIT.


      *****************************************************************
      *    IF SCREEN NOT FULL, SET TOP OF DATA INDICATOR TO FORCE     *
      *    A SCROLL FORWARD FUNCTION                                  *
      *****************************************************************

           IF PDA112-ORDER-NUMBER (1)  >  SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'Y'                TO WS-TOP-OF-DATA-SW.


       P06100-BUILD-SCREEN-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06200-PROCESS-ORDERS                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS / FORMAT THE                *
      *                ORDER INFORMATION TO THE SCREEN                *
      *                                                               *
      *    CALLED BY:  P06100-BUILD-SCREEN                            *
      *                                                               *
      *****************************************************************

       P06200-PROCESS-ORDERS.

      *****************************************************************
      *    PERFORM A SET NUMBER OF READS TO ATTEMPT TO FILL THE SCREEN*
      *    ON THE SCROLL BACKWARD FUNCTION. WHEN MAX READS REACHED,   *
      *    TERMINATE THE LOOP,AND A SCROLL FORWARD FUNCTION FROM THE  *
      *    START OF THE ORDER DATABASE WILL BE INVOKED.               *
      *                                                               *
      *    THE CORRECT REAL WORLD SOLUTION IS A SECONDARY INDEX ON    *
      *    THE ORDER KEY IN DESCENDING ORDER. SCROLL BACKWARD COULD   *
      *    THEN READ IT SEQUENTIALLY TO FORMAT THE SCREEN             *
      *****************************************************************

           ADD +1                      TO WS-COUNT.

           IF WS-COUNT                 >  +25
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
               GO TO P06200-PROCESS-ORDERS-EXIT.


      *****************************************************************
      *    DETERMINE IF ORDER FOR GIVEN USERID EXISTS                 *
      *****************************************************************

           IF OP-STATUS                =  SPACES
               NEXT SENTENCE
           ELSE
               SUBTRACT 1              FROM WMF-ORDER-NUMBER-R
               IF WMF-ORDER-NUMBER-R   < 1
                   MOVE 'Y'            TO WS-PROCESS-COMPLETE-SW
                   MOVE 'Y'            TO WS-TOP-OF-DATA-SW
                   GO TO P06200-PROCESS-ORDERS-EXIT
               ELSE
                   MOVE WMF-ORDER-NUMBER
                                       TO ORDER-QUAL-NUMBER
                   PERFORM  P78000-GU-ORDER
                       THRU P78000-GU-ORDER-EXIT
                   GO TO P06200-PROCESS-ORDERS-EXIT.


      *****************************************************************
      *    FORMAT ORDER INFORMATION TO SCREEN                         *
      *****************************************************************

           PERFORM  P69000-FORMAT-SCREEN
               THRU P69000-FORMAT-SCREEN-EXIT.


      *****************************************************************
      *    IF SCREEN IS FULL, TERMINATE PROCESS, EXIT                 *
      *****************************************************************

           COMPUTE WS-SCR-SUB          =   WS-SCR-SUB - 1.

           IF WS-SCR-SUB               <   +1
               MOVE 'Y'                TO  WS-PROCESS-COMPLETE-SW
               GO TO P06200-PROCESS-ORDERS-EXIT.


      *****************************************************************
      *    READ THE NEXT ORDER                                        *
      *****************************************************************

           SUBTRACT 1                  FROM WMF-ORDER-NUMBER-R.

           IF WMF-ORDER-NUMBER-R       < +1
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
               MOVE 'Y'                TO WS-TOP-OF-DATA-SW
               GO TO P06200-PROCESS-ORDERS-EXIT
           ELSE
               MOVE WMF-ORDER-NUMBER   TO ORDER-QUAL-NUMBER
               PERFORM  P78000-GU-ORDER
                   THRU P78000-GU-ORDER-EXIT
               GO TO P06200-PROCESS-ORDERS-EXIT.


       P06200-PROCESS-ORDERS-EXIT.
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
      *                P06000-SCROLL-BACKWARD                         *
      *                                                               *
      *****************************************************************

       P07000-SCROLL-FORWARD.

           MOVE 'N'                    TO WS-BOTTOM-OF-DATA-SW.

      *****************************************************************
      *    DETERMINE THE SCROLL FORWARD START KEY, CLEAR SCREEN       *
      *****************************************************************

           PERFORM  P07050-GET-FORWARD-KEY
               THRU P07050-GET-FORWARD-KEY-EXIT.

           PERFORM  P79200-CLEAR-SCR-FIELDS
               THRU P79200-CLEAR-SCR-FIELDS-EXIT.


      *****************************************************************
      *    BUILD THE SCREEN FROM THE PRE-DETERMINED START POINT,      *
      *    USING THE LAST ORDER # IF AVAILABLE                        *
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
      *    IF AN ENTRY WAS DISPLAYED ON LAST LINE                     *
      *    USE IT AS THE STARTING POINT                               *
      *****************************************************************

           IF PDA112-ORDER-NUMBER (WS-SCR-LINES-MAX) > ZEROES
               MOVE PDA112-ORDER-NUMBER (WS-SCR-LINES-MAX)
                                       TO WMF-ORDER-NUMBER
           ELSE

      *****************************************************************
      *    IF NO LAST LINE SCREEN ENTRY, USE SCREEN FIRST LINE ENTRY, *
      *    IF PRESENT, AS THE STARTING POINT                          *
      *****************************************************************

           IF PDA112-ORDER-NUMBER (1)  >  ZEROES
               MOVE PDA112-ORDER-NUMBER (1)
                                       TO WMF-ORDER-NUMBER
           ELSE

      *****************************************************************
      *    OTHERWISE START AT THE BEGINNING OF THE ORDER DATABASE     *
      *****************************************************************

               MOVE 1                  TO WMF-ORDER-NUMBER-R.

       P07050-GET-FORWARD-KEY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07100-BUILD-SCREEN                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE SCREEN FORMAT / BUILD   *
      *                PROCESSES TO DISPLAY ORDER                     *
      *                INFORMATION                                    *
      *                                                               *
      *    CALLED BY:  P07000-SCROLL-FORWARD                          *
      *                                                               *
      *****************************************************************

       P07100-BUILD-SCREEN.

      *****************************************************************
      *    PROCESS THE ORDERS FROM PRE-DETERMINED START POINT,        *
      *    RETRIEVE THE FIRST ORDER                                   *
      *****************************************************************

           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.
           MOVE ZEROES                 TO WS-SCR-SUB.

           MOVE 'GE'                   TO ORDER-QUAL-OPERATOR.
           MOVE PDA112-USERID-NUMBER   TO ORDER-QUAL-PREFIX.
           MOVE WMF-ORDER-NUMBER       TO ORDER-QUAL-NUMBER.

           PERFORM  P78000-GU-ORDER
               THRU P78000-GU-ORDER-EXIT.


           PERFORM  P07200-PROCESS-ORDERS
               THRU P07200-PROCESS-ORDERS-EXIT
                   UNTIL PROCESS-COMPLETE.

           IF ERROR-FOUND
               GO TO P07100-BUILD-SCREEN-EXIT.

      *****************************************************************
      *    CHECK FOR BOTTOM OF DATA (NO DATA ON LAST LINE)            *
      *****************************************************************

           IF PDA112-ORDER-NUMBER (WS-SCR-LINES-MAX)  >  SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'Y'                TO WS-BOTTOM-OF-DATA-SW.


       P07100-BUILD-SCREEN-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07200-PROCESS-ORDERS                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS / FORMAT THE                *
      *                ORDER INFORMATION TO THE SCREEN                *
      *                                                               *
      *    CALLED BY:  P07100-BUILD-SCREEN                            *
      *                                                               *
      *****************************************************************

       P07200-PROCESS-ORDERS.

      *****************************************************************
      *    DETERMINE IF ORDER FOR GIVEN USERID EXISTS                 *
      *****************************************************************

           IF (OP-STATUS               =  SPACES)    AND
              (ORDER-PREFIX            =  PDA112-USERID-NUMBER)
               NEXT SENTENCE
           ELSE
           IF WS-SCR-SUB               =  ZEROES
               MOVE PM013-BOTTOM-MSG   TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
               GO TO P07200-PROCESS-ORDERS-EXIT
           ELSE
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
               GO TO P07200-PROCESS-ORDERS-EXIT.

      *****************************************************************
      *    IF SCREEN IS FULL, TERMINATE PROCESS, EXIT                 *
      *****************************************************************

           ADD +1                      TO  WS-SCR-SUB.

           IF WS-SCR-SUB               >   WS-SCR-LINES-MAX
               MOVE 'Y'                TO  WS-PROCESS-COMPLETE-SW
               GO TO P07200-PROCESS-ORDERS-EXIT.


      *****************************************************************
      *    FORMAT ORDER INFORMATION TO SCREEN                         *
      *****************************************************************

           PERFORM  P69000-FORMAT-SCREEN
               THRU P69000-FORMAT-SCREEN-EXIT.


      *****************************************************************
      *    READ THE NEXT PENDING ORDER                                 *
      *****************************************************************

           PERFORM  P78100-GN-ORDER
               THRU P78100-GN-ORDER-EXIT.

       P07200-PROCESS-ORDERS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P69000-FORMAT-SCREEN                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO FORMAT ORDER AND RELATED            *
      *                INFORMATION TO THE SCREEN                      *
      *                                                               *
      *    CALLED BY:  P06200-PROCESS-ORDERS                          *
      *                P07200-PROCESS-ORDERS                          *
      *                                                               *
      *****************************************************************

       P69000-FORMAT-SCREEN.


           MOVE ORDER-NUMBER           TO PDA112-ORDER-NUMBER
                                                        (WS-SCR-SUB).

           MOVE ORDER-DATE-YYMMDD      TO WMF-DATE-YYMMDD.
           MOVE WMF-DATE-MM            TO PDA112-ORDER-DATE-MM
                                                        (WS-SCR-SUB).
           MOVE WMF-DATE-DD            TO PDA112-ORDER-DATE-DD
                                                        (WS-SCR-SUB).
           MOVE WMF-DATE-YY            TO PDA112-ORDER-DATE-YY
                                                        (WS-SCR-SUB).
           MOVE '/'                    TO PDA112-ORDER-DATE-SL1
                                                        (WS-SCR-SUB)
                                          PDA112-ORDER-DATE-SL2
                                                        (WS-SCR-SUB).

           MOVE ORDER-TOTAL-AMOUNT     TO PDA112-ORDER-AMT-R
                                                        (WS-SCR-SUB).
           MOVE ORDER-STATUS           TO PDA112-ORDER-STATUS
                                                        (WS-SCR-SUB).


      *****************************************************************
      *    LOCATE / FORMAT THE CUSTOMER RELATED INFORMATION           *
      *****************************************************************

           MOVE 'N'                    TO WS-CUSTOMER-FOUND-SW.

           PERFORM  P69100-FIND-CUSTOMER
               THRU P69100-FIND-CUSTOMER-EXIT
                   VARYING WS-SUB1 FROM +1 BY +1
                       UNTIL WS-SUB1 > +13.

           IF CUSTOMER-FOUND
               NEXT SENTENCE
           ELSE
               MOVE '***** CUSTOMER NOT FOUND *****'
                                       TO  PDA112-CUSTOMER-NAME
                                                        (WS-SCR-SUB).


      *****************************************************************
      *    PROCESS THE ORDER ITEM SEGMENTS (ORDITEM) FOR THE ORDER    *
      *****************************************************************

           MOVE 'Y'                    TO WS-MORE-SEGMENTS-SW.

           PERFORM  P78200-GNP-ORDITEM
               THRU P78200-GNP-ORDITEM-EXIT.

           PERFORM  P69200-ORDITEM-PROCESS
               THRU P69200-ORDITEM-PROCESS-EXIT
                   UNTIL NO-MORE-SEGMENTS.


       P69000-FORMAT-SCREEN-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P69100-FIND-CUSTOMER                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO SEARCH THE CUSTOMER ID ARRAY FOR    *
      *                THE SELECTED CUSTOMER ID.                      *
      *                                                               *
      *    CALLED BY:  P69000-FORMAT-SCREEN                           *
      *                                                               *
      *****************************************************************

       P69100-FIND-CUSTOMER.

      *****************************************************************
      *    IF CUSTOMER FOUND ---- FORMAT CUSTOMER INFO                *
      *****************************************************************

           IF WCAR-CUSTOMER-ID (WS-SUB1)  =  ORDER-CUSTOMER-ID
               MOVE 'Y'                TO WS-CUSTOMER-FOUND-SW
               MOVE WCAR-CUSTOMER-NAME    (WS-SUB1)
                                       TO PDA112-CUSTOMER-NAME
                                                        (WS-SCR-SUB)
               MOVE +14                TO WS-SUB1
           ELSE
               NEXT SENTENCE.


       P69100-FIND-CUSTOMER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P69200-ORDITEM-PROCESS                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE / PROCESS THE ORDER ITEM   *
      *                SEGMENTS FOR A GIVEN ORDER                     *
      *                                                               *
      *    CALLED BY:  P69000-FORMAT-SCREEN                           *
      *                                                               *
      *****************************************************************

       P69200-ORDITEM-PROCESS.

      *****************************************************************
      *    IF NO MORE ORDER ITEM SEGMENTS TERMINATE PROCESS, EXIT     *
      *****************************************************************

           IF OP-STATUS                = SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'N'                TO WS-MORE-SEGMENTS-SW
               GO TO P69200-ORDITEM-PROCESS-EXIT.


      *****************************************************************
      *    READ NEXT ORDER ITEM SEGMENT                               *
      *****************************************************************

           PERFORM  P78200-GNP-ORDITEM
               THRU P78200-GNP-ORDITEM-EXIT.

       P69200-ORDITEM-PROCESS-EXIT.
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

           IF PDA112-SCREEN-MESSAGE    >  SPACES
               NEXT SENTENCE
           ELSE
               MOVE WMF-MESSAGE-AREA   TO PDA112-SCREEN-MESSAGE.

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

           IF PDA112-SCREEN-MESSAGE    >  SPACES
               NEXT SENTENCE
           ELSE
               MOVE WMF-MESSAGE-AREA   TO PDA112-SCREEN-MESSAGE.

       P70100-INFO-MSG-ROUTINE-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P78000-GU-ORDER                                *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE ORDER ROOT             *
      *                SEGMENT                                        *
      *                                                               *
      *    CALLED BY:  P06100-BUILD-SCREEN                            *
      *                P06200-PROCESS-ORDERS                          *
      *                P07100-BUILD-SCREEN                            *
      *                                                               *
      *****************************************************************

       P78000-GU-ORDER.


           CALL 'CBLTDLI'    USING     GU
                                       ORDER-PCB
                                       ORDER-SEGMENT
                                       ORDER-QUAL-SSA.


           IF OP-STATUS                =  SPACES OR 'GE' OR 'GB'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA112'           TO WPIE-PROGRAM-ID
               MOVE OP-STATUS          TO WPIE-STATUS-CODE
               MOVE 'GU'               TO WPIE-FUNCTION-CODE
               MOVE 'P78000'           TO WPIE-PARAGRAPH
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME
               MOVE 'ORDER2DB'         TO WPIE-DATABASE-NAME
               MOVE 'GU ORDER ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P78000-GU-ORDER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P78100-GN-ORDER                                *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE ORDER ROOT             *
      *                SEGMENT                                        *
      *                                                               *
      *    CALLED BY:  P07200-PROCESS-ORDERS                          *
      *                                                               *
      *****************************************************************

       P78100-GN-ORDER.


           CALL 'CBLTDLI'    USING     GN
                                       ORDER-PCB
                                       ORDER-SEGMENT
                                       ORDER-QUAL-SSA.


           IF OP-STATUS                =  SPACES OR 'GE' OR 'GB'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA112'           TO WPIE-PROGRAM-ID
               MOVE OP-STATUS          TO WPIE-STATUS-CODE
               MOVE 'GN'               TO WPIE-FUNCTION-CODE
               MOVE 'P78100'           TO WPIE-PARAGRAPH
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME
               MOVE 'ORDER2DB'         TO WPIE-DATABASE-NAME
               MOVE 'GN ORDER ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P78100-GN-ORDER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P78200-GNP-ORDITEM                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE ORDER ITEM CHILD       *
      *                SEGMENT                                        *
      *                                                               *
      *    CALLED BY:  P69000-FORMAT-SCREEN                           *
      *                                                               *
      *****************************************************************

       P78200-GNP-ORDITEM.


           CALL 'CBLTDLI'    USING     GNP
                                       ORDER-PCB
                                       ORDER-ITEM-SEGMENT
                                       ORDITEM-UNQUAL-SSA.


           IF OP-STATUS                =  SPACES OR 'GE' OR 'GB'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA112'           TO WPIE-PROGRAM-ID
               MOVE OP-STATUS          TO WPIE-STATUS-CODE
               MOVE 'GNP'              TO WPIE-FUNCTION-CODE
               MOVE 'P78200'           TO WPIE-PARAGRAPH
               MOVE 'ORDITEM'          TO WPIE-SEGMENT-NAME
               MOVE 'ORDER2DB'         TO WPIE-DATABASE-NAME
               MOVE 'GNP ORDER ITEM SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P78200-GNP-ORDITEM-EXIT.
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
      *    MAKE FINAL ADJUSTMENTS TO ATTRIBUTES AND FIELDS            *
      *****************************************************************

           PERFORM  P79100-SET-SCR-FIELDS
               THRU P79100-SET-SCR-FIELDS-EXIT
                   VARYING WS-SCR-SUB FROM +1 BY +1
                       UNTIL WS-SCR-SUB > WS-SCR-LINES-MAX.


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
      *    IF ORDER NUMBER PRESENT MAKE SELECTION CODE ENTERABLE      *
      *    AND INITIALIZED TO UNDERSCORE                              *
      *****************************************************************

           IF PDA112-ORDER-NUMBER (WS-SCR-SUB)  >  SPACES
               INSPECT PDA112-SELCODE   (WS-SCR-SUB)
                   CONVERTING  WMF-SPACES-LOWVALUE-R TO '__'
               IF NO-ERROR-FOUND
                   MOVE WS-CURSOR-ATTR TO PDA112-SELCODE-ATTR1
                                                          (WS-SCR-SUB)
               ELSE
                   NEXT SENTENCE
           ELSE
               MOVE LOW-VALUES         TO PDA112-SELCODE-ATTR1
                                                          (WS-SCR-SUB)
               MOVE WS-PROT-MOD-ATTR   TO PDA112-SELCODE-ATTR2
                                                          (WS-SCR-SUB)
               MOVE SPACES             TO PDA112-SELCODE  (WS-SCR-SUB).

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

           MOVE LOW-VALUES             TO CIOM-THE-REST.

           PERFORM  P79300-CLEAR-SCR-DETAIL
               THRU P79300-CLEAR-SCR-DETAIL-EXIT
                   VARYING WS-SCR-SUB FROM +1 BY +1
                       UNTIL WS-SCR-SUB  >  WS-SCR-LINES-MAX.


           MOVE SPACES                 TO PDA112-SCREEN-MESSAGE.

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


           MOVE SPACES            TO PDA112-SELCODE      (WS-SCR-SUB)
                                     PDA112-ORDER-NUMBER (WS-SCR-SUB)
                                     PDA112-ORDER-DATE   (WS-SCR-SUB)
                                     PDA112-ORDER-AMT    (WS-SCR-SUB)
                                     PDA112-ORDER-STATUS (WS-SCR-SUB)
                                     PDA112-CUSTOMER-NAME
                                                         (WS-SCR-SUB).

       P79300-CLEAR-SCR-DETAIL-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80000-INSERT-MSG                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INSERT THE OUTPUT MESSAGE TO THE    *
      *                TERMINAL USING THE IO-PCB                      *
      *                                                               *
      *    CALLED BY:  P01000-PROCESS-MSG                             *
      *                P79000-DISPLAY-SCREEN                          *
      *                                                               *
      *****************************************************************

       P80000-INSERT-MSG.

      *****************************************************************
      *    FORMAT OUTPUT MESSAGE FIELDS,                              *
      *    RESET ENTERABLE FIELDS BACK TO DEFAULT (IF NECESSARY)      *
      *****************************************************************

           MOVE 'PDA112'               TO PDA112-PREV-PGRMID.

           MOVE LENGTH OF PDA112-MESSAGE
                                       TO PDA112-MSG-LL.

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
               MOVE 'PDA112'           TO WPIE-PROGRAM-ID
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
      *    CALLED BY:  P04000-PFKEY-PROCESS                           *
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
               MOVE 'PDA112'           TO WPIE-PROGRAM-ID
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
           MOVE 'PDA112'               TO CIOM-PREV-PGRMID.


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
               MOVE 'PDA112'           TO WPIE-PROGRAM-ID
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