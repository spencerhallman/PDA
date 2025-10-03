       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA108.

      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA108                                            *
      * TRANS   :   PDA10801                                          *
      *                                                               *
      * FUNCTION:   PROGRAM PDA108 IS THE IMS/DC PRODUCT DEMONSTRATION*
      *             APPLICATION PENDING ORDER PROGRAM. THE PENDING    *
      *             ORDER SCREEN DISPLAYS A SCROLLABLE LIST OF ALL    *
      *             ITEMS SELECTED BY THE USER DURING THE ORDER ADD   *
      *             PROCESS. PENDING ORDERS REPRESENT A HOLD/WORK AREA*
      *             FOR ITEMS BEFORE THE ACTUAL ORDER PLACEMENT       *
      *             PROCESS OCCURS.                                   *
      *                                                               *
      *             THE USER MAY CHANGE ITEM ORDER INFORMATION AND    *
      *             DELETE ITEMS.                                     *
      *                                                               *
      *                                                               *
      *                                                               *
      * FILES   :   ITEM                        - DB2 (READ ONLY)     *
      *             ITEM_SUPPLIER               - DB2 (READ ONLY)     *
      *             PENDING ORDER (PENDO1DB)    - IMS (READ/UPDATE)   *
      *                                                               *
      *                                                               *
      * TRANSACTIONS GENERATED:                                       *
      *             PDA10401   CUSTOMER IDENTIFICATION (VIA PF3=PREV) *
      *             PDA10701   ITEM DETAIL             (VIA PF3=PREV) *
      *             PDA10901   PROCESS ORDER                          *
      *             PDA10501   BROWSE CATEGORIES                      *
      *             PDA10201   ORDER MENU                             *
      *             PDA10101   MAIN MENU                              *
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
       77  WS-SCR-LINES-MAX            PIC S9(04)   COMP    VALUE +3.
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

           05  WS-ORDER-FOUND-SW       PIC X(01)             VALUE 'N'.
               88  ORDER-FOUND                               VALUE 'Y'.
               88  NO-ORDER-FOUND                            VALUE 'N'.
           EJECT

      *****************************************************************
      *    MISCELLANEOUS WORK FIELDS                                  *
      *****************************************************************
       01  WS-MISCELLANEOUS-FIELDS.

           05  WMF-MODNAME             PIC X(08)   VALUE 'PDA108O'.
           05  WMF-MODNAME-ERROR       PIC X(08)   VALUE 'PDAERRO'.
           05  WMF-MASTER-LTERM-NAME   PIC X(08)   VALUE 'SMASTER'.
           05  WMF-IO-PCB-LTERM-NAME   PIC X(08)   VALUE SPACES.

           05  WMF-NEXT-TRANID         PIC X(08)   VALUE SPACES.
           05  WMF-NEXT-TRANID-R       REDEFINES   WMF-NEXT-TRANID.
               10  FILLER              PIC X(06).
               10  WMF-NEXT-TRANID-SEQ PIC X(02).

           05  WMF-DATE-MMDDYY         PIC X(08)   VALUE SPACES.
           05  WMF-TIME-HHMMSS         PIC X(08)   VALUE SPACES.

           05  WMF-PEND-ORDER-SEQ      PIC X(05)   VALUE ZEROES.
           05  WMF-PEND-ORDER-SEQ-R    REDEFINES WMF-PEND-ORDER-SEQ
                                       PIC 9(05).

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

           05  WMF-QTY                 OCCURS 3 TIMES
                                       PIC 9(9).

           05  WMF-EXT-PRICE           PIC S9(11)V99  VALUE +0.
           05  WMF-TOTAL-COST          PIC S9(11)V99  VALUE +0.
           05  WMF-TOTAL-COST-R        REDEFINES WMF-TOTAL-COST
                                       PIC X(13).

           05  WMF-NUM-ERROR           PIC S9(04)  VALUE +0  COMP.
           05  WMF-NUM-LTH             PIC S9(04)  VALUE +0  COMP.
           05  WMF-NUM-INPUT           PIC X(18)   VALUE SPACES.
           05  WMF-NUM-INPUT-R         REDEFINES   WMF-NUM-INPUT
                                       OCCURS 18 TIMES
                                       PIC X(01).
           05  WMF-NUM-OUTPUT          PIC 9(18)   VALUE ZEROES.
           05  WMF-NUM-OUTPUT-R        REDEFINES   WMF-NUM-OUTPUT
                                       OCCURS 18 TIMES
                                       PIC X(01).


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
      *  PREFIX: PDA108                                               *
      *****************************************************************

       01  PDA108-MESSAGE              REDEFINES CIOM-MESSAGE.
           05 PDA108-MSG-LL            PIC S9(04)      COMP.
           05 PDA108-MSG-ZZ            PIC X(02).
           05 PDA108-MSG-TRANCODE      PIC X(08).
           05 FILLER                   PIC X(01).
           05 PDA108-MSG-SOURCE        PIC X(03).
           05 FILLER                   PIC X(01).
           05 PDA108-PFKEY             PIC X(02).
           05 PDA108-MSG-USERID-INFO.
              10  PDA108-USERID-ID     PIC X(08).
              10  PDA108-USERID-NUMBER PIC 9(05).
           05 PDA108-PREV-PGRMID       PIC X(08).
           05 PDA108-SAVAREA           PIC X(79).
           05 PDA108-SAVAREA-R         REDEFINES PDA108-SAVAREA.
              10 PDA108-SAVAREA-ORDER-MENU-SEL
                                       PIC X(01).
              10 PDA108-SAVAREA-CUSID  PIC X(32).
              10 PDA108-ORIGINATING-PGRMID
                                       PIC X(08).
           05 PDA108-PEND-ORDER-LINES  OCCURS 3 TIMES.
              10 PDA108-ACTCODE-ATTR.
                  15 PDA108-ACTCODE-ATTR1
                                       PIC X(01).
                  15 PDA108-ACTCODE-ATTR2
                                       PIC X(01).
              10 PDA108-ACTCODE        PIC X(01).
              10 PDA108-ORDER-QTY-ATTR.
                  15 PDA108-ORDER-QTY-ATTR1
                                       PIC X(01).
                  15 PDA108-ORDER-QTY-ATTR2
                                       PIC X(01).
              10 PDA108-ORDER-QTY      PIC X(09).
              10 PDA108-ORDER-QTY-R    REDEFINES PDA108-ORDER-QTY
                                       PIC ZZZZZZZZ9.
              10 PDA108-ITEM           PIC X(32).
              10 PDA108-NAME           PIC X(50).
              10 PDA108-SUPPLIER-ID    PIC X(32).
              10 PDA108-ORDER-SEQ      PIC X(05).
              10 PDA108-ORDER-SEQ-R    REDEFINES PDA108-ORDER-SEQ
                                       PIC 9(05).
              10 PDA108-HID-PRICE      PIC X(10).
              10 PDA108-HID-PRICE-R    REDEFINES PDA108-HID-PRICE
                                       PIC 9(8)V99.
              10 PDA108-UNIT-PRICE     PIC X(13).
              10 PDA108-UNIT-PRICE-R   REDEFINES PDA108-UNIT-PRICE
                                       PIC ZZ,ZZZ,ZZ9.99.
              10 PDA108-EXT-PRICE      PIC X(13).
              10 PDA108-EXT-PRICE-R    REDEFINES PDA108-EXT-PRICE
                                       PIC ZZ,ZZZ,ZZ9.99.
              10 PDA108-STOCK-STATUS   PIC X(12).
           05 PDA108-TOTAL-COST        PIC X(13).
           05 PDA108-TOTAL-COST-R      REDEFINES PDA108-TOTAL-COST
                                       PIC ZZ,ZZZ,ZZ9.99.
           05 PDA108-SCREEN-MESSAGE    PIC X(79).
           05 PDA108-SMESSAGE          PIC X(79).
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
      *    PENDING ORDER DATABASE ROOT SEGMENT                        *
      *****************************************************************

           COPY IPENDORD.
           EJECT

      *****************************************************************
      *    IMS  DATABASE SEGMENT SEARCH ARGUMENTS (SSA)               *
      *****************************************************************
      *****************************************************************
      *    PENDING ORDER DATABASE ROOT SEGMENT (SSA)                  *
      *****************************************************************

       01  PENDORD-UNQUAL-SSA.
           05  PENDORD-UNQUAL-SEGMENT  PIC X(09)    VALUE 'PENDORD '.

       01  PENDORD-QUAL-SSA.
           05  PENDORD-QUAL-SEGMENT    PIC X(08)   VALUE 'PENDORD'.
           05  PENDORD-QUAL-LPAREN     PIC X(01)   VALUE '('.
           05  PENDORD-QUAL-FIELD-NAME
                                       PIC X(08)   VALUE 'PENDKEY '.
           05  PENDORD-QUAL-OPERATOR   PIC X(02)   VALUE 'EQ'.
           05  PENDORD-QUAL-FIELD-VALUE.
               10  PENDORD-QUAL-PREFIX
                                       PIC X(05)   VALUE SPACES.
               10  PENDORD-QUAL-SEQUENCE
                                       PIC X(05)   VALUE SPACES.
               10  PENDORD-QUAL-SEQUENCE-R REDEFINES
                                       PENDORD-QUAL-SEQUENCE
                                       PIC 9(05).
           05  PENDORD-QUAL-RPAREN     PIC X(01)   VALUE ')'.


      *****************************************************************
      *    DB2  DEFINITIONS                                           *
      *****************************************************************
      *****************************************************************
      *    SQL COMMUNICATIONS AREA                                    *
      *****************************************************************
           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.
           EJECT

      *****************************************************************
      *    DB2 DCLGEN FOR THE ITEM TABLE                              *
      *****************************************************************
           EXEC SQL
              INCLUDE DITEM
           END-EXEC.
           EJECT

      *****************************************************************
      *    DB2 DCLGEN FOR THE ITEM SUPPLIER TABLE                     *
      *****************************************************************
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
      *    IMS PENDING ORDER DATABASE (PENDORD) PCB MASK              *
      *****************************************************************

           COPY PCBPNDOR.
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
      *                PENDING ORDER SCREEN                           *
      *                                                               *
      *    CALLED BY:  NONE                                           *
      *                                                               *
      *****************************************************************

       P00000-MAINLINE.

           ENTRY 'DLITCBL'   USING  IO-PCB
                                    ALT-IO-PCB1
                                    ALT-IO-PCB2
                                    PENDORD-PCB.


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

           MOVE 'N'                    TO WS-ERROR-FOUND-SW
                                          WS-SELECTION-DATA-ENTERED-SW
                                          WS-TOP-OF-DATA-SW
                                          WS-BOTTOM-OF-DATA-SW
                                          WS-ORDER-FOUND-SW
                                          WS-PROCESS-COMPLETE-SW.

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
               MOVE 'PDA108'           TO WPIE-PROGRAM-ID
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

           IF CIOM-MSG-TRANCODE        = 'PDA10801'    AND
              CIOM-MSG-SOURCE          = 'PDA'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA108'           TO WPIE-PROGRAM-ID
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
                                       TO  PDA108-SCREEN-MESSAGE
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

           IF PDA108-PREV-PGRMID       NOT = 'PDA108'
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
      *    PENDING ORDER DATABASE (USE SCROLL FORWARD ROUTINE)        *
      *****************************************************************

           MOVE PDA108-PREV-PGRMID     TO PDA108-ORIGINATING-PGRMID.
           MOVE 1                      TO WMF-PEND-ORDER-SEQ-R.

           PERFORM  P07000-SCROLL-FORWARD
               THRU P07000-SCROLL-FORWARD-EXIT.


      *****************************************************************
      *    CALCULATE THE TOTAL COST FOR ALL PENDING ORDERS            *
      *****************************************************************

           PERFORM  P69100-TOTAL-COST
               THRU P69100-TOTAL-COST-EXIT.


      *****************************************************************
      *    DISPLAY THE INITIAL SCREEN, SAVE CONTROL VARIABLES         *
      *****************************************************************

           PERFORM  P79000-DISPLAY-SCREEN
               THRU P79000-DISPLAY-SCREEN-EXIT.

           MOVE 'PDA108'               TO  PDA108-PREV-PGRMID.


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

           MOVE 'PDA108'               TO  PDA108-PREV-PGRMID.

      *****************************************************************
      *    CONVERT UNDERSCORES AND LOW-VALUES TO SPACES               *
      *****************************************************************

           MOVE SPACES                 TO  PDA108-SCREEN-MESSAGE.

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

           IF (PDA108-PFKEY  =  'EN' OR '07' OR '08')  OR
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


           INSPECT PDA108-ACTCODE (WS-SCR-SUB)
               CONVERTING  WMF-UNDERSCORE-LOWVALUE-R TO SPACES.


           INSPECT PDA108-ORDER-QTY (WS-SCR-SUB)
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

           IF PDA108-PFKEY  =  'EN'
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
      *    VALID KEYS ARE: ENTER, PF3, PF4, PF7, PF8, PF10, PF11, PF12*
      *****************************************************************

           IF PDA108-PFKEY = 'EN' OR '03' OR '04' OR '07' OR '08' OR
                             '10' OR '11' OR '12'
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


           IF PDA108-PFKEY         NOT = 'EN'
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

           IF PDA108-PFKEY             =  'EN'
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


           IF PDA108-ACTCODE (WS-SCR-SUB) >  SPACES
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


           IF PDA108-ACTCODE (WS-SCR-SUB) >  SPACES
               MOVE LOW-VALUES         TO  PDA108-ACTCODE-ATTR
                                                           (WS-SCR-SUB)
               MOVE WS-CURSOR-ATTR     TO  PDA108-ACTCODE-ATTR1
                                                           (WS-SCR-SUB)
               MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA108-ACTCODE-ATTR2
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
      *    PF3 (PREVIOUS), TRANSFER TO:                               *
      *    1) ORDER MENU (PDA102) IF PENDING ORDER SELECTED VIA MENU  *
      *    2) BROWSE CATEGORIES (PDA105) IF IN ORDER ADD PROCESS      *
      *                                                               *
      *****************************************************************

           IF PDA108-PFKEY = '03'
               IF PDA108-SAVAREA-ORDER-MENU-SEL = '1'
                   MOVE 'PDA10501'     TO  WMF-NEXT-TRANID
                   PERFORM  P80300-XFER-CONTROL
                       THRU P80300-XFER-CONTROL-EXIT
                   GO TO P04000-PFKEY-PROCESS-EXIT
               ELSE
                   MOVE 'PDA10201'     TO  WMF-NEXT-TRANID
                   PERFORM  P80300-XFER-CONTROL
                       THRU P80300-XFER-CONTROL-EXIT
                   GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.

      *****************************************************************
      *    PF4 (PROCESS ORDER)                                        *
      *                                                               *
      *    VERIFY AT LEAST 1 PENDING ORDER EXISTS BEFORE GOING        *
      *****************************************************************

           IF PDA108-PFKEY = '04'
               PERFORM  P04100-PFKEY04-PROCESS
                   THRU P04100-PFKEY04-PROCESS-EXIT
               GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF PF7, PERFORM SCROLL BACKWARD PROCESS                    *
      *****************************************************************

           IF PDA108-PFKEY  =  '07'
               PERFORM  P06000-SCROLL-BACKWARD
                   THRU P06000-SCROLL-BACKWARD-EXIT
               GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF PF8, PERFORM SCROLL FORWARD PROCESS                     *
      *****************************************************************

           IF PDA108-PFKEY  =  '08'
               PERFORM  P07000-SCROLL-FORWARD
                   THRU P07000-SCROLL-FORWARD-EXIT
               GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF PF10, TRANSFER TO THE BROWSE CATEGORIES FUNCTION        *
      *****************************************************************

           IF PDA108-PFKEY  =  '10'
               MOVE 'PDA10501'     TO  WMF-NEXT-TRANID
               PERFORM  P80300-XFER-CONTROL
                   THRU P80300-XFER-CONTROL-EXIT
               GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF PF11, TRANSFER TO THE ORDER MENU                        *
      *****************************************************************

           IF PDA108-PFKEY  =  '11'
               MOVE 'PDA10201'     TO  WMF-NEXT-TRANID
               PERFORM  P80300-XFER-CONTROL
                   THRU P80300-XFER-CONTROL-EXIT
               GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF PF12, RETURN TO THE MAIN MENU                           *
      *****************************************************************

           IF PDA108-PFKEY  =  '12'
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
      *    PARAGRAPH:  P04100-PFKEY04-PROCESS                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO HANDLE THE TRANSFER TO THE PROCESS  *
      *                ORDER FUNCTION, PFKEY 04.                      *
      *                                                               *
      *    CALLED BY:  P04000-PFKEY-PROCESS                           *
      *                                                               *
      *****************************************************************

       P04100-PFKEY04-PROCESS.

      *****************************************************************
      *    A PENDING ORDER MUST EXIST TO ALLOW TRANSFER TO ORD PROCESS*
      *****************************************************************

           MOVE 'GE'                   TO PENDORD-QUAL-OPERATOR.
           MOVE PDA108-USERID-NUMBER   TO PENDORD-QUAL-PREFIX.
           MOVE 1                      TO PENDORD-QUAL-SEQUENCE-R.

           MOVE 'N'                    TO WS-ORDER-FOUND-SW.
           PERFORM  P78000-GU-PENDORD
               THRU P78000-GU-PENDORD-EXIT.

           IF PENDORD-STATUS           =  SPACES
               IF PENDING-ORDER-PREFIX =  PDA108-USERID-NUMBER
                   MOVE 'Y'            TO WS-ORDER-FOUND-SW
               ELSE
                   MOVE PM032-NO-PENDING-ORDER
                                       TO  WMF-MESSAGE-AREA
                   PERFORM  P70000-ERROR-ROUTINE
                       THRU P70000-ERROR-ROUTINE-EXIT
                   GO TO P04100-PFKEY04-PROCESS-EXIT
           ELSE
                   MOVE PM032-NO-PENDING-ORDER
                                       TO  WMF-MESSAGE-AREA
                   PERFORM  P70000-ERROR-ROUTINE
                       THRU P70000-ERROR-ROUTINE-EXIT
                   GO TO P04100-PFKEY04-PROCESS-EXIT.


      *****************************************************************
      *    TRANSFER TO THE PROCESS ORDER SCREEN                       *
      *****************************************************************

           MOVE 'PDA10901'         TO  WMF-NEXT-TRANID.

           PERFORM  P80300-XFER-CONTROL
               THRU P80300-XFER-CONTROL-EXIT.

       P04100-PFKEY04-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05000-PROCESS-SCREEN                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE PROCESSING WHEN THE     *
      *                ENTER KEY IS USED TO UPDATE PENDING ORDERS     *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************

       P05000-PROCESS-SCREEN.

      *****************************************************************
      *    EDIT SCREEN ENTERABLE FIELDS                               *
      *****************************************************************

           PERFORM  P05100-EDIT-SELECTION
               THRU P05100-EDIT-SELECTION-EXIT
                   VARYING WS-SCR-SUB FROM +1 BY +1
                       UNTIL WS-SCR-SUB > WS-SCR-LINES-MAX.

           IF ERROR-FOUND
               GO TO P05000-PROCESS-SCREEN-EXIT.


      *****************************************************************
      *    IF NO EDIT ERRORS, PROCEED WITH PENDING ORDER UPDATES      *
      *****************************************************************

           PERFORM  P05500-PROCESS-SELECTION
               THRU P05500-PROCESS-SELECTION-EXIT
                   VARYING WS-SCR-SUB FROM +1 BY +1
                       UNTIL WS-SCR-SUB > WS-SCR-LINES-MAX.


      *****************************************************************
      *    CALCULATE THE TOTAL COST FOR ALL PENDING ORDERS            *
      *****************************************************************

           PERFORM  P69100-TOTAL-COST
               THRU P69100-TOTAL-COST-EXIT.


      *****************************************************************
      *    IF NO MORE ORDERS ON FILE, REMOVE THE ORDER CONTROL RECORD *
      *****************************************************************

           PERFORM  P05800-NO-ORDERS-CHECK
               THRU P05800-NO-ORDERS-CHECK-EXIT.


      *****************************************************************
      *    DISPLAY SUCCESSFUL UPDATE MESSAGE                          *
      *****************************************************************

           MOVE PM043-UPDATE-COMPLETE  TO  WMF-MESSAGE-AREA.

           PERFORM  P70100-INFO-MSG-ROUTINE
               THRU P70100-INFO-MSG-ROUTINE-EXIT.


       P05000-PROCESS-SCREEN-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05100-EDIT-SELECTION                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PERFORM SCREEN EDITS                *
      *                                                               *
      *    CALLED BY:  P05000-PROCESS-SCREEN                          *
      *                                                               *
      *****************************************************************

       P05100-EDIT-SELECTION.

      *****************************************************************
      *    IF ENTERED, ACTION CODE MUST BE C (CHANGE) OR D (DELETE)   *
      *****************************************************************

           IF PDA108-ACTCODE (WS-SCR-SUB) =  'C' OR 'D' OR SPACES
               NEXT SENTENCE
           ELSE
               MOVE LOW-VALUES         TO  PDA108-ACTCODE-ATTR
                                                           (WS-SCR-SUB)
               MOVE WS-CURSOR-ATTR     TO  PDA108-ACTCODE-ATTR1
                                                           (WS-SCR-SUB)
               MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA108-ACTCODE-ATTR2
                                                           (WS-SCR-SUB)
               MOVE PM020-INVALID-ACTION-CODE
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P05100-EDIT-SELECTION-EXIT.


      *****************************************************************
      *    IF ACTION IS CHANGE, PROCEED TO EDIT THE ORDER QUANTITY    *
      *****************************************************************

           IF PDA108-ACTCODE (WS-SCR-SUB) = 'C'
               PERFORM  P05130-EDIT-QUANTITY
                   THRU P05130-EDIT-QUANTITY-EXIT.


       P05100-EDIT-SELECTION-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05130-EDIT-QUANTITY                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO EDIT THE PENDING ORDER QUANTITY     *
      *                                                               *
      *    CALLED BY:  P05100-EDIT-SELECTION                          *
      *                                                               *
      *****************************************************************

       P05130-EDIT-QUANTITY.


      *****************************************************************
      *    ORDER QUANTITY MUST BE NUMERIC, AND GREATER THAN ZERO      *
      *****************************************************************

           MOVE +9                     TO WMF-NUM-LTH.
           MOVE PDA108-ORDER-QTY (WS-SCR-SUB)
                                       TO WMF-NUM-INPUT.

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF WMF-NUM-ERROR            >  ZEROES    OR
              WMF-NUM-OUTPUT           =  ZEROES
               MOVE LOW-VALUES         TO  PDA108-ORDER-QTY-ATTR
                                                           (WS-SCR-SUB)
               MOVE WS-CURSOR-ATTR     TO  PDA108-ORDER-QTY-ATTR1
                                                           (WS-SCR-SUB)
               MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA108-ORDER-QTY-ATTR2
                                                           (WS-SCR-SUB)
               MOVE PM016-QUANTITY-INVALID
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
           ELSE
               MOVE WMF-NUM-OUTPUT     TO PDA108-ORDER-QTY-R
                                                          (WS-SCR-SUB)
                                          WMF-QTY (WS-SCR-SUB).

       P05130-EDIT-QUANTITY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05500-PROCESS-SELECTION                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PERFORM THE PENDING ORDER UPDATE    *
      *                PROCESS                                        *
      *                                                               *
      *    CALLED BY:  P05000-PROCESS-SCREEN                          *
      *                                                               *
      *****************************************************************

       P05500-PROCESS-SELECTION.

           IF PDA108-ACTCODE (WS-SCR-SUB) = 'C' OR 'D'
               NEXT SENTENCE
           ELSE
               GO TO P05500-PROCESS-SELECTION-EXIT.

      *****************************************************************
      *    RETRIEVE THE PENDING ORDER WITH HOLD (FOR UPDATE),         *
      *    IF UNSUCCESSFUL RETRIEVAL TERMINATE PROGRAM                *
      *****************************************************************

           MOVE 'EQ'                   TO PENDORD-QUAL-OPERATOR.
           MOVE PDA108-USERID-NUMBER   TO PENDORD-QUAL-PREFIX.
           MOVE PDA108-ORDER-SEQ (WS-SCR-SUB)
                                       TO PENDORD-QUAL-SEQUENCE.

           PERFORM  P78030-GHU-PENDORD
               THRU P78030-GHU-PENDORD-EXIT.

           IF PENDORD-STATUS           =  SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA108'           TO WPIE-PROGRAM-ID
               MOVE PENDORD-STATUS     TO WPIE-STATUS-CODE
               MOVE 'GHU'              TO WPIE-FUNCTION-CODE
               MOVE 'P05500'           TO WPIE-PARAGRAPH
               MOVE 'PENDORD'          TO WPIE-SEGMENT-NAME
               MOVE 'PENDO1DB'         TO WPIE-DATABASE-NAME
               MOVE 'GHU PENDORD ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    PROCESS EITHER THE PENDING ORDER CHANGE OR DELETE          *
      *****************************************************************

           IF PDA108-ACTCODE (WS-SCR-SUB) =  'C'
               PERFORM  P05600-CHANGE-PROCESS
                   THRU P05600-CHANGE-PROCESS-EXIT
           ELSE
               PERFORM  P05700-DELETE-PROCESS
                   THRU P05700-DELETE-PROCESS-EXIT.


      *****************************************************************
      *    CLEAR THE ACTION CODE AFTER SUCCESSFUL UPDATE              *
      *****************************************************************

           MOVE SPACES                 TO PDA108-ACTCODE (WS-SCR-SUB).

       P05500-PROCESS-SELECTION-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05600-CHANGE-PROCESS                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PERFORM THE PENDING ORDER CHANGE    *
      *                PROCESS                                        *
      *                                                               *
      *    CALLED BY:  P05500-PROCESS-SELECTION                       *
      *                                                               *
      *****************************************************************

       P05600-CHANGE-PROCESS.

      *****************************************************************
      *    FORMAT PENDING ORDER DATA, UPDATE THE PENDING ORDER ROOT   *
      *****************************************************************

           MOVE WMF-QTY (WS-SCR-SUB)   TO PENDING-ORDER-QUANTITY.
           COMPUTE WMF-EXT-PRICE       = PENDING-ORDER-QUANTITY   *
                                         PDA108-HID-PRICE-R
                                                          (WS-SCR-SUB).
           MOVE WMF-EXT-PRICE      TO PDA108-EXT-PRICE-R  (WS-SCR-SUB).


           PERFORM  P78160-REPL-PENDORD
               THRU P78160-REPL-PENDORD-EXIT.


       P05600-CHANGE-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05700-DELETE-PROCESS                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PERFORM THE PENDING ORDER DELETE    *
      *                PROCESS                                        *
      *                                                               *
      *    CALLED BY:  P05500-PROCESS-SELECTION                       *
      *                                                               *
      *****************************************************************

       P05700-DELETE-PROCESS.

      *****************************************************************
      *    DELETE PENDING ORDER, RE-INITIALIZE SCREEN VARIABLES       *
      *****************************************************************

           PERFORM  P78130-DLET-PENDORD
               THRU P78130-DLET-PENDORD-EXIT.

           MOVE SPACES                 TO PDA108-ACTCODE  (WS-SCR-SUB)
                                          PDA108-ORDER-QTY
                                                          (WS-SCR-SUB)
                                          PDA108-ITEM     (WS-SCR-SUB)
                                          PDA108-NAME     (WS-SCR-SUB)
                                          PDA108-SUPPLIER-ID
                                                          (WS-SCR-SUB)
                                          PDA108-HID-PRICE
                                                          (WS-SCR-SUB)
                                          PDA108-UNIT-PRICE
                                                          (WS-SCR-SUB)
                                          PDA108-EXT-PRICE
                                                          (WS-SCR-SUB)
                                          PDA108-STOCK-STATUS
                                                          (WS-SCR-SUB).


           MOVE LOW-VALUES             TO PDA108-ACTCODE-ATTR1
                                                          (WS-SCR-SUB).
           MOVE WS-PROT-MOD-ATTR       TO PDA108-ACTCODE-ATTR2
                                                          (WS-SCR-SUB).

           MOVE LOW-VALUES             TO PDA108-ORDER-QTY-ATTR1
                                                          (WS-SCR-SUB).
           MOVE WS-PROT-MOD-ATTR       TO PDA108-ORDER-QTY-ATTR2
                                                          (WS-SCR-SUB).

           MOVE 'ITEM DELETED'         TO PDA108-NAME     (WS-SCR-SUB).

       P05700-DELETE-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05800-NO-ORDERS-CHECK                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO REMOVE THE USER ORDER CONTROL       *
      *                RECORD IF NO ORDERS REMAIN FOR THE SPECIFIC    *
      *                USER ID                                        *
      *                                                               *
      *    CALLED BY:  P05000-PROCESS-SCREEN                          *
      *                                                               *
      *****************************************************************

       P05800-NO-ORDERS-CHECK.

      *****************************************************************
      *    OBTAIN 1ST PENDING ORDER FOR THE SPECIFIC USER,            *
      *    IF ORDER EXISTS ---- EXIT                                  *
      *****************************************************************

           MOVE 'GE'                   TO PENDORD-QUAL-OPERATOR.
           MOVE PDA108-USERID-NUMBER   TO PENDORD-QUAL-PREFIX.
           MOVE 1                      TO PENDORD-QUAL-SEQUENCE-R.

           PERFORM  P78000-GU-PENDORD
               THRU P78000-GU-PENDORD-EXIT.

           IF (PENDORD-STATUS          =  SPACES)    AND
              (PENDING-ORDER-PREFIX    =  PDA108-USERID-NUMBER)
               GO TO P05800-NO-ORDERS-CHECK-EXIT.


      *****************************************************************
      *    IF NO ORDERS FOR USER, DELETE ORDER CONTROL RECORD         *
      *****************************************************************

           MOVE 'EQ'                   TO PENDORD-QUAL-OPERATOR.
           MOVE PDA108-USERID-NUMBER   TO PENDORD-QUAL-PREFIX.
           MOVE ZEROES                 TO PENDORD-QUAL-SEQUENCE-R.

           PERFORM  P78030-GHU-PENDORD
               THRU P78030-GHU-PENDORD-EXIT.

           IF PENDORD-STATUS           =  SPACES
               PERFORM  P78130-DLET-PENDORD
                   THRU P78130-DLET-PENDORD-EXIT.


       P05800-NO-ORDERS-CHECK-EXIT.
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
      *    STANDARD SCROLL FORWARD PROCESS FORM THE 1ST PENDING ORDER *
      *    GOING FORWARD --- FROM TOP TO BOTTOM                       *
      *****************************************************************

           IF TOP-OF-DATA
               MOVE SPACES             TO PDA108-ORDER-SEQ (1)
                                          PDA108-ORDER-SEQ
                                                    (WS-SCR-LINES-MAX)

               PERFORM  P07000-SCROLL-FORWARD
                   THRU P07000-SCROLL-FORWARD-EXIT

               MOVE SPACES             TO PDA108-SCREEN-MESSAGE
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

           IF PDA108-ORDER-SEQ (1)     >  ZEROES
               MOVE PDA108-ORDER-SEQ (1) TO WMF-PEND-ORDER-SEQ
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
      *                PROCESS TO POPULATE THE SCREEN WITH PENDING    *
      *                ORDER INFORMATION                              *
      *                                                               *
      *    CALLED BY:  P06000-SCROLL-BACKWARD                         *
      *                                                               *
      *****************************************************************

       P06100-BUILD-SCREEN.

      *****************************************************************
      *    PROCESS THE PENDING ORDERS FROM PRE-DETERMINED START POINT,*
      *    RETRIEVE THE FIRST PENDING ORDER                           *
      *****************************************************************

           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.
           MOVE ZEROES                 TO WS-COUNT.
           COMPUTE WS-SCR-SUB          =  WS-SCR-LINES-MAX.

           MOVE 'EQ'                   TO PENDORD-QUAL-OPERATOR.
           MOVE PDA108-USERID-NUMBER   TO PENDORD-QUAL-PREFIX.
           MOVE WMF-PEND-ORDER-SEQ-R   TO PENDORD-QUAL-SEQUENCE.

           PERFORM  P78000-GU-PENDORD
               THRU P78000-GU-PENDORD-EXIT.


           PERFORM  P06200-PROCESS-ORDERS
               THRU P06200-PROCESS-ORDERS-EXIT
                   UNTIL PROCESS-COMPLETE.

           IF ERROR-FOUND
               GO TO P06100-BUILD-SCREEN-EXIT.


      *****************************************************************
      *    IF SCREEN NOT FULL, SET TOP OF DATA INDICATOR TO FORCE     *
      *    A SCROLL FORWARD FUNCTION                                  *
      *****************************************************************

           IF PDA108-ORDER-SEQ (1)     >  SPACES
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
      *    FUNCTION :  ROUTINE TO PROCESS / FORMAT THE PENDING        *
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
      *    DETERMINE IF PENDING ORDER FOR GIVEN USERID EXISTS         *
      *****************************************************************

           IF PENDORD-STATUS           =  SPACES
               NEXT SENTENCE
           ELSE
               SUBTRACT 1              FROM WMF-PEND-ORDER-SEQ-R
               IF WMF-PEND-ORDER-SEQ-R < 1
                   MOVE 'Y'            TO WS-PROCESS-COMPLETE-SW
                   MOVE 'Y'            TO WS-TOP-OF-DATA-SW
                   GO TO P06200-PROCESS-ORDERS-EXIT
               ELSE
                   MOVE WMF-PEND-ORDER-SEQ-R
                                       TO PENDORD-QUAL-SEQUENCE
                   PERFORM  P78000-GU-PENDORD
                       THRU P78000-GU-PENDORD-EXIT
                   GO TO P06200-PROCESS-ORDERS-EXIT.


      *****************************************************************
      *    FORMAT PENDING ORDER INFORMATION TO SCREEN                  *
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
      *    READ THE NEXT PENDING ORDER                                 *
      *****************************************************************

           SUBTRACT 1                  FROM WMF-PEND-ORDER-SEQ-R.

           IF WMF-PEND-ORDER-SEQ-R     < +1
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
               MOVE 'Y'                TO WS-TOP-OF-DATA-SW
               GO TO P06200-PROCESS-ORDERS-EXIT
           ELSE
               MOVE WMF-PEND-ORDER-SEQ-R
                                       TO PENDORD-QUAL-SEQUENCE
               PERFORM  P78000-GU-PENDORD
                   THRU P78000-GU-PENDORD-EXIT
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
      *    USING THE LAST PENDING ORDER SEQ # IF AVAILABLE            *
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
      *    IF AN ENTRY WAS DISPLAYED ON LAST LINE AND IS NOT DELETED, *
      *    USE IT AS THE STARTING POINT                               *
      *****************************************************************

           IF (PDA108-ORDER-SEQ (WS-SCR-LINES-MAX) > ZEROES)    AND
              (PDA108-HID-PRICE (WS-SCR-LINES-MAX) > SPACES)
               MOVE PDA108-ORDER-SEQ (WS-SCR-LINES-MAX)
                                       TO WMF-PEND-ORDER-SEQ
           ELSE

      *****************************************************************
      *    IF NO LAST LINE SCREEN ENTRY, USE SCREEN FIRST LINE ENTRY  *
      *    (IF PRESENT, AND NOT DELETED) AS THE STARTING POINT *
      *****************************************************************

           IF (PDA108-ORDER-SEQ (1)     >  ZEROES)    AND
              (PDA108-HID-PRICE (1)     >  SPACES)
               MOVE PDA108-ORDER-SEQ (1)
                                       TO WMF-PEND-ORDER-SEQ
           ELSE

      *****************************************************************
      *    OTHERWISE START AT THE BEGINNING OF THE PENDING ORDER DB   *
      *****************************************************************

               MOVE 1                  TO WMF-PEND-ORDER-SEQ-R.

       P07050-GET-FORWARD-KEY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07100-BUILD-SCREEN                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE SCREEN FORMAT / BUILD   *
      *                PROCESSES TO DISPLAY PENDING ORDER             *
      *                INFORMATION                                    *
      *                                                               *
      *    CALLED BY:  P07000-SCROLL-FORWARD                          *
      *                                                               *
      *****************************************************************

       P07100-BUILD-SCREEN.

      *****************************************************************
      *    PROCESS THE PENDING ORDERS FROM PRE-DETERMINED START POINT,*
      *    RETRIEVE THE FIRST PENDING ORDER                           *
      *****************************************************************

           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.
           MOVE ZEROES                 TO WS-SCR-SUB.

           MOVE 'GE'                   TO PENDORD-QUAL-OPERATOR.
           MOVE PDA108-USERID-NUMBER   TO PENDORD-QUAL-PREFIX.
           MOVE WMF-PEND-ORDER-SEQ-R   TO PENDORD-QUAL-SEQUENCE.

           PERFORM  P78000-GU-PENDORD
               THRU P78000-GU-PENDORD-EXIT.


           PERFORM  P07200-PROCESS-ORDERS
               THRU P07200-PROCESS-ORDERS-EXIT
                   UNTIL PROCESS-COMPLETE.

           IF ERROR-FOUND
               GO TO P07100-BUILD-SCREEN-EXIT.

      *****************************************************************
      *    CHECK FOR BOTTOM OF DATA (NO DATA ON LAST LINE)            *
      *****************************************************************

           IF PDA108-ORDER-SEQ (WS-SCR-LINES-MAX)  >  SPACES
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
      *    FUNCTION :  ROUTINE TO PROCESS / FORMAT THE PENDING        *
      *                ORDER INFORMATION TO THE SCREEN                *
      *                                                               *
      *    CALLED BY:  P07100-BUILD-SCREEN                            *
      *                                                               *
      *****************************************************************

       P07200-PROCESS-ORDERS.

      *****************************************************************
      *    DETERMINE IF PENDING ORDER FOR GIVEN USERID EXISTS         *
      *****************************************************************

           IF (PENDORD-STATUS          =  SPACES)    AND
              (PENDING-ORDER-PREFIX    =  PDA108-USERID-NUMBER)
               NEXT SENTENCE
           ELSE
           IF WS-SCR-SUB               =  ZEROES
               MOVE PM032-NO-PENDING-ORDER
                                       TO  WMF-MESSAGE-AREA
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
      *    FORMAT PENDING ORDER INFORMATION TO SCREEN                  *
      *****************************************************************

           PERFORM  P69000-FORMAT-SCREEN
               THRU P69000-FORMAT-SCREEN-EXIT.


      *****************************************************************
      *    READ THE NEXT PENDING ORDER                                 *
      *****************************************************************

           PERFORM  P78100-GN-PENDORD
               THRU P78100-GN-PENDORD-EXIT.

       P07200-PROCESS-ORDERS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P69000-FORMAT-SCREEN                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO FORMAT PENDING ORDER AND RELATED    *
      *                INFORMATION TO THE SCREEN                      *
      *                                                               *
      *    CALLED BY:  P06200-PROCESS-ORDERS                          *
      *                P07200-PROCESS-ORDERS                          *
      *                                                               *
      *****************************************************************

       P69000-FORMAT-SCREEN.


           MOVE PENDING-ORDER-ITEM-PREFIX
                                       TO ITEM-PREFIX.
           MOVE PENDING-ORDER-SUPPLIER-PREFIX
                                       TO ITEM-SUPPLIER-ITEM-PREFIX.

           MOVE PENDING-ORDER-QUANTITY
                                   TO PDA108-ORDER-QTY-R (WS-SCR-SUB).

           MOVE PENDING-ORDER-ITEM-NUMBER TO PDA108-ITEM (WS-SCR-SUB)
                                             ITEM-NUMBER
                                             ITEM-SUPPLIER-ITEM-NUMBER.


      *****************************************************************
      *    RETRIEVE ITEM NAME FROM THE ITEM TABLE                     *
      *****************************************************************

           EXEC SQL
               SELECT  NAME
               INTO    :ITEM-NAME
               FROM    ITEM
               WHERE   PREFIX             = :ITEM-PREFIX AND
                       NUMBER             = :ITEM-NUMBER
           END-EXEC.


           IF SQLCODE              NOT = +0
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA108'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'SELECT ITEM'      TO WPDE-FUNCTION
               MOVE 'P69000'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           END-IF.

           MOVE ITEM-NAME              TO PDA108-NAME (WS-SCR-SUB).


      *****************************************************************
      *    RETRIEVE THE ITEM UNIT PRICE FROM THE ITEM_SUPPLIER TABLE  *
      *****************************************************************

           MOVE PENDING-ORDER-SUPPLIER-ID
                                    TO PDA108-SUPPLIER-ID (WS-SCR-SUB)
                                       ITEM-SUPPLIER-SUPPLIER-ID.
           MOVE PENDING-ORDER-SEQUENCE
                                    TO PDA108-ORDER-SEQ (WS-SCR-SUB).

           EXEC SQL
               SELECT  UNIT_PRICE
               INTO    :ITEM-SUPPLIER-UNIT-PRICE
               FROM    ITEM_SUPPLIER
               WHERE   ITEM_PREFIX    = :ITEM-SUPPLIER-ITEM-PREFIX AND
                       ITEM_NUMBER    = :ITEM-SUPPLIER-ITEM-NUMBER AND
                       SUPPLIER_ID    = :ITEM-SUPPLIER-SUPPLIER-ID
           END-EXEC.


           IF SQLCODE              NOT = +0
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA108'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'SELECT ITEM-SUPPLIER'
                                       TO WPDE-FUNCTION
               MOVE 'P69000'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           END-IF.


           MOVE ITEM-SUPPLIER-UNIT-PRICE
                                   TO PDA108-HID-PRICE-R  (WS-SCR-SUB)
                                      PDA108-UNIT-PRICE-R (WS-SCR-SUB).

           COMPUTE WMF-EXT-PRICE       = ITEM-SUPPLIER-UNIT-PRICE *
                                         PENDING-ORDER-QUANTITY.

           MOVE WMF-EXT-PRICE      TO PDA108-EXT-PRICE-R  (WS-SCR-SUB).
           MOVE 'IN STOCK'         TO PDA108-STOCK-STATUS (WS-SCR-SUB).


       P69000-FORMAT-SCREEN-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P69100-TOTAL-COST                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO OBTAIN THE TOTAL ORDER COST VIA     *
      *                READING ALL PENDING ORDERS FOR THE USERID      *
      *                                                               *
      *    CALLED BY:  P01800-1ST-TIME-PROCESS                        *
      *                                                               *
      *****************************************************************

       P69100-TOTAL-COST.

      *****************************************************************
      *    ESTABLISH DATABASE POSITION AT 1ST PENDING ORDER FOR USER  *
      *****************************************************************

           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.
           MOVE ZEROES                 TO WMF-TOTAL-COST.

           MOVE 'GE'                   TO PENDORD-QUAL-OPERATOR.
           MOVE PDA108-USERID-NUMBER   TO PENDORD-QUAL-PREFIX.
           MOVE 1                      TO PENDORD-QUAL-SEQUENCE-R.

           PERFORM  P78000-GU-PENDORD
               THRU P78000-GU-PENDORD-EXIT.


      *****************************************************************
      *    PROCESS ALL PENDING ORDERS FOR THE USER                    *
      *****************************************************************

           PERFORM  P69200-RETRIEVE-ORDERS
               THRU P69200-RETRIEVE-ORDERS-EXIT
                   UNTIL PROCESS-COMPLETE.

           MOVE WMF-TOTAL-COST         TO PDA108-TOTAL-COST-R.

       P69100-TOTAL-COST-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P69200-RETRIEVE-ORDERS                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS / FORMAT THE PENDING        *
      *                ORDER INFORMATION TO THE SCREEN                *
      *                                                               *
      *    CALLED BY:  P69100-TOTAL-COST                              *
      *                                                               *
      *****************************************************************

       P69200-RETRIEVE-ORDERS.

      *****************************************************************
      *    DETERMINE IF PENDING ORDER IS FOR THE SELECTED USER        *
      *****************************************************************

           IF (PENDORD-STATUS          =  SPACES)    AND
              (PENDING-ORDER-PREFIX    =  PDA108-USERID-NUMBER)
               NEXT SENTENCE
           ELSE
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
               GO TO P69200-RETRIEVE-ORDERS-EXIT.


      *****************************************************************
      *    RETRIEVE THE ITEM UNIT PRICE FROM THE ITEM_SUPPLIER TABLE, *
      *    CALCULATE / ACCUMULATE TOTAL COST, READ NEXT PENDING ORDER *
      *****************************************************************

           MOVE PENDING-ORDER-ITEM-PREFIX
                                       TO ITEM-SUPPLIER-ITEM-PREFIX.
           MOVE PENDING-ORDER-ITEM-NUMBER
                                       TO ITEM-SUPPLIER-ITEM-NUMBER.
           MOVE PENDING-ORDER-SUPPLIER-ID
                                       TO ITEM-SUPPLIER-SUPPLIER-ID.

           EXEC SQL
               SELECT  UNIT_PRICE
               INTO    :ITEM-SUPPLIER-UNIT-PRICE
               FROM    ITEM_SUPPLIER
               WHERE   ITEM_PREFIX    = :ITEM-SUPPLIER-ITEM-PREFIX AND
                       ITEM_NUMBER    = :ITEM-SUPPLIER-ITEM-NUMBER AND
                       SUPPLIER_ID    = :ITEM-SUPPLIER-SUPPLIER-ID
           END-EXEC.


           IF SQLCODE              NOT = +0
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA108'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'SELECT ITEM-SUPPLIER'
                                       TO WPDE-FUNCTION
               MOVE 'P69200'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           END-IF.


           COMPUTE WMF-EXT-PRICE       = ITEM-SUPPLIER-UNIT-PRICE *
                                         PENDING-ORDER-QUANTITY.

           ADD WMF-EXT-PRICE           TO WMF-TOTAL-COST.


      *****************************************************************
      *    READ THE NEXT PENDING ORDER                                 *
      *****************************************************************

           PERFORM  P78100-GN-PENDORD
               THRU P78100-GN-PENDORD-EXIT.

       P69200-RETRIEVE-ORDERS-EXIT.
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

           IF PDA108-SCREEN-MESSAGE    >  SPACES
               NEXT SENTENCE
           ELSE
               MOVE WMF-MESSAGE-AREA   TO PDA108-SCREEN-MESSAGE.

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

           IF PDA108-SCREEN-MESSAGE    >  SPACES
               NEXT SENTENCE
           ELSE
               MOVE WMF-MESSAGE-AREA   TO PDA108-SCREEN-MESSAGE.

       P70100-INFO-MSG-ROUTINE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P70500-EDIT-NUMERIC-FIELD                      *
      *                P70550-EDIT-NUMERIC                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RIGHT JUSTIFY AND VALIDATE NUMERICS *
      *                IN A FIELD                                     *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************

       P70500-EDIT-NUMERIC-FIELD.


           MOVE ZEROES                 TO WMF-NUM-ERROR.
           MOVE ZEROES                 TO WMF-NUM-OUTPUT.
           MOVE +18                    TO WS-SUB2.

           PERFORM  P70550-EDIT-NUMERIC
               THRU P70550-EDIT-NUMERIC-EXIT
                   VARYING WS-SUB1 FROM WMF-NUM-LTH BY -1
                       UNTIL WS-SUB1 < 1.


       P70500-EDIT-NUMERIC-FIELD-EXIT.
           EXIT.


       P70550-EDIT-NUMERIC.


           IF WMF-NUM-INPUT-R (WS-SUB1) > SPACES
               IF WMF-NUM-INPUT-R (WS-SUB1) NUMERIC
                   MOVE WMF-NUM-INPUT-R (WS-SUB1)
                                       TO WMF-NUM-OUTPUT-R (WS-SUB2)
                   COMPUTE WS-SUB2  =  WS-SUB2 - 1
               ELSE
                   ADD +1              TO WMF-NUM-ERROR
           ELSE
                   NEXT SENTENCE.


       P70550-EDIT-NUMERIC-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P78000-GU-PENDORD                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE PENDING ORDER ROOT     *
      *                SEGMENT                                        *
      *                                                               *
      *    CALLED BY:  P04100-PFKEY04-PROCESS                         *
      *                                                               *
      *****************************************************************

       P78000-GU-PENDORD.


           CALL 'CBLTDLI'    USING     GU
                                       PENDORD-PCB
                                       PENDING-ORDER-SEGMENT
                                       PENDORD-QUAL-SSA.


           IF PENDORD-STATUS           =  '  ' OR 'GE' OR 'GB'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA108'           TO WPIE-PROGRAM-ID
               MOVE PENDORD-STATUS     TO WPIE-STATUS-CODE
               MOVE 'GU'               TO WPIE-FUNCTION-CODE
               MOVE 'P78000'           TO WPIE-PARAGRAPH
               MOVE 'PENDORD'          TO WPIE-SEGMENT-NAME
               MOVE 'PENDO1DB'         TO WPIE-DATABASE-NAME
               MOVE 'GU PENDORD ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P78000-GU-PENDORD-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P78030-GHU-PENDORD                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE PENDING ORDER ROOT     *
      *                SEGMENT WITH HOLD (FOR UPDATE)                 *
      *                                                               *
      *    CALLED BY:  P05500-PROCESS-SELECTION                       *
      *                                                               *
      *****************************************************************

       P78030-GHU-PENDORD.


           CALL 'CBLTDLI'    USING     GHU
                                       PENDORD-PCB
                                       PENDING-ORDER-SEGMENT
                                       PENDORD-QUAL-SSA.


           IF PENDORD-STATUS           =  SPACES OR 'GE' OR 'GB'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA108'           TO WPIE-PROGRAM-ID
               MOVE PENDORD-STATUS     TO WPIE-STATUS-CODE
               MOVE 'GHU'              TO WPIE-FUNCTION-CODE
               MOVE 'P78030'           TO WPIE-PARAGRAPH
               MOVE 'PENDORD'          TO WPIE-SEGMENT-NAME
               MOVE 'PENDO1DB'         TO WPIE-DATABASE-NAME
               MOVE 'GHU PENDORD ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P78030-GHU-PENDORD-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P78100-GN-PENDORD                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE PENDING ORDER ROOT     *
      *                SEGMENT                                        *
      *                                                               *
      *    CALLED BY:  P07200-PROCESS-ORDERS                          *
      *                                                               *
      *****************************************************************

       P78100-GN-PENDORD.


           CALL 'CBLTDLI'    USING     GN
                                       PENDORD-PCB
                                       PENDING-ORDER-SEGMENT
                                       PENDORD-QUAL-SSA.


           IF PENDORD-STATUS           =  SPACES OR 'GE' OR 'GB'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA108'           TO WPIE-PROGRAM-ID
               MOVE PENDORD-STATUS     TO WPIE-STATUS-CODE
               MOVE 'GN'               TO WPIE-FUNCTION-CODE
               MOVE 'P78100'           TO WPIE-PARAGRAPH
               MOVE 'PENDORD'          TO WPIE-SEGMENT-NAME
               MOVE 'PENDO1DB'         TO WPIE-DATABASE-NAME
               MOVE 'GN PENDORD ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P78100-GN-PENDORD-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P78130-DLET-PENDORD                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DELETE THE PENDING ORDER ROOT       *
      *                SEGMENT                                        *
      *                                                               *
      *    CALLED BY:  XXXXXX-XXXXXXX                                 *
      *                                                               *
      *****************************************************************

       P78130-DLET-PENDORD.


           CALL 'CBLTDLI'    USING     DLET
                                       PENDORD-PCB
                                       PENDING-ORDER-SEGMENT.


           IF PENDORD-STATUS           =  SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA108'           TO WPIE-PROGRAM-ID
               MOVE PENDORD-STATUS     TO WPIE-STATUS-CODE
               MOVE 'DLET'             TO WPIE-FUNCTION-CODE
               MOVE 'P78130'           TO WPIE-PARAGRAPH
               MOVE 'PENDORD'          TO WPIE-SEGMENT-NAME
               MOVE 'PENDO1DB'         TO WPIE-DATABASE-NAME
               MOVE 'DLET PENDORD ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P78130-DLET-PENDORD-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P78160-REPL-PENDORD                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO UPDATE THE PENDING ORDER ROOT       *
      *                SEGMENT                                        *
      *                                                               *
      *    CALLED BY:  XXXXXX-XXXXXXX                                 *
      *                                                               *
      *****************************************************************

       P78160-REPL-PENDORD.


           CALL 'CBLTDLI'    USING     REPL
                                       PENDORD-PCB
                                       PENDING-ORDER-SEGMENT.


           IF PENDORD-STATUS           =  SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA108'           TO WPIE-PROGRAM-ID
               MOVE PENDORD-STATUS     TO WPIE-STATUS-CODE
               MOVE 'REPL'             TO WPIE-FUNCTION-CODE
               MOVE 'P78160'           TO WPIE-PARAGRAPH
               MOVE 'PENDORD'          TO WPIE-SEGMENT-NAME
               MOVE 'PENDO1DB'         TO WPIE-DATABASE-NAME
               MOVE 'REPL PENDORD ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P78160-REPL-PENDORD-EXIT.
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
      *    IF ORDER SEQUENCE AND HIDDEN UNIT PRICE ARE NOT PRESENT,   *
      *    MAKE ACTION CODE AND ORDER                                 *
      *    QUANTITY PROTECTED OTHERWISE FIELDS ARE ENTERABLE AND      *
      *    DEFAULTED TO UNDERSCORE IF NO VALUE PRESENT.               *
      *****************************************************************

           IF (PDA108-ORDER-SEQ (WS-SCR-SUB)  >  SPACES)    AND
              (PDA108-HID-PRICE (WS-SCR-SUB)  >  SPACES)
               INSPECT PDA108-ACTCODE   (WS-SCR-SUB)
                   CONVERTING  WMF-SPACES-LOWVALUE-R TO '__'
               IF NO-ERROR-FOUND
                   MOVE WS-CURSOR-ATTR TO PDA108-ACTCODE-ATTR1
                                                          (WS-SCR-SUB)
               ELSE
                   NEXT SENTENCE
           ELSE
               MOVE LOW-VALUES         TO PDA108-ACTCODE-ATTR1
                                                          (WS-SCR-SUB)
               MOVE WS-PROT-MOD-ATTR   TO PDA108-ACTCODE-ATTR2
                                                          (WS-SCR-SUB)
               MOVE SPACES             TO PDA108-ACTCODE  (WS-SCR-SUB)
               MOVE LOW-VALUES         TO PDA108-ORDER-QTY-ATTR1
                                                          (WS-SCR-SUB)
               MOVE WS-PROT-MOD-ATTR   TO PDA108-ORDER-QTY-ATTR2
                                                          (WS-SCR-SUB)
               MOVE SPACES             TO PDA108-ORDER-QTY
                                                          (WS-SCR-SUB).

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

           MOVE PDA108-TOTAL-COST      TO WMF-TOTAL-COST-R.
           MOVE LOW-VALUES             TO CIOM-THE-REST.

           PERFORM  P79300-CLEAR-SCR-DETAIL
               THRU P79300-CLEAR-SCR-DETAIL-EXIT
                   VARYING WS-SCR-SUB FROM +1 BY +1
                       UNTIL WS-SCR-SUB  >  WS-SCR-LINES-MAX.


           IF PDA108-PFKEY             = '07' OR '08'
               MOVE WMF-TOTAL-COST-R   TO PDA108-TOTAL-COST.

           MOVE SPACES                 TO PDA108-SCREEN-MESSAGE.

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


           MOVE SPACES            TO PDA108-ACTCODE      (WS-SCR-SUB)
                                     PDA108-ORDER-QTY    (WS-SCR-SUB)
                                     PDA108-ITEM         (WS-SCR-SUB)
                                     PDA108-NAME         (WS-SCR-SUB)
                                     PDA108-SUPPLIER-ID  (WS-SCR-SUB)
                                     PDA108-ORDER-SEQ    (WS-SCR-SUB)
                                     PDA108-HID-PRICE    (WS-SCR-SUB)
                                     PDA108-UNIT-PRICE   (WS-SCR-SUB)
                                     PDA108-STOCK-STATUS (WS-SCR-SUB).

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
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************

       P80000-INSERT-MSG.

      *****************************************************************
      *    FORMAT OUTPUT MESSAGE FIELDS,                              *
      *    RESET ENTERABLE FIELDS BACK TO DEFAULT (IF NECESSARY)      *
      *****************************************************************

           MOVE 'PDA108'               TO PDA108-PREV-PGRMID.

           MOVE LENGTH OF PDA108-MESSAGE
                                       TO PDA108-MSG-LL.

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
               MOVE 'PDA108'           TO WPIE-PROGRAM-ID
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
               MOVE 'PDA108'           TO WPIE-PROGRAM-ID
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
           MOVE 'PDA108'               TO CIOM-PREV-PGRMID.


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
               MOVE 'PDA108'           TO WPIE-PROGRAM-ID
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