       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA109.

      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA109                                            *
      * TRANS   :   PDA10901                                          *
      *                                                               *
      * FUNCTION:   PROGRAM PDA109 IS THE IMS/DC PRODUCT DEMONSTRATION*
      *             APPLICATION PROCESS ORDER PROGRAM. A SINGLE ORDER *
      *             CONTAINING ALL ITEMS CURRENTLY RESIDING IN THE    *
      *             PENDING ORDER DATABASE IS CREATED.                *
      *                                                               *
      *             THE USER MAY SUBMIT THE ORDER, CANCEL THE ORDER,  *
      *             OR TRANSFER TO THE PENDING ORDER FUNCTION FOR     *
      *             ADDITIONAL MODIFICATIONS BEFORE ORDER CREATION    *
      *                                                               *
      *                                                               *
      *                                                               *
      * FILES   :   ITEM_SUPPLIER               - DB2 (READ ONLY)     *
      *             PURCHASE_TYPE               - DB2 (READ ONLY)     *
      *             PENDING ORDER (PENDO1DB)    - IMS (READ/UPDATE)   *
      *             ORDER         (ORDER2DB)    - IMS (READ/UPDATE)   *
      *                                                               *
      *                                                               *
      * TRANSACTIONS GENERATED:                                       *
      *             PDA10801   PENDING ORDER                          *
      *             PDA10201   ORDER MENU                             *
      *             PDA10101   MAIN MENU                              *
      *                                                               *
      *                                                               *
      * PFKEYS  :   PF04  =    SUBMIT ORDER (CREATE ORDER)            *
      *             PF05  =    CANCEL ORDER (REMOVE ORDER)            *
      *             PF10  =    PENDING ORDER                          *
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

           05  WS-PROCESS-COMPLETE-SW  PIC X(01)             VALUE 'N'.
               88  PROCESS-COMPLETE                          VALUE 'Y'.
               88  NOT-PROCESS-COMPLETE                      VALUE 'N'.

           05  WS-CUSTOMER-FOUND-SW    PIC X(01)             VALUE 'N'.
               88  CUSTOMER-FOUND                            VALUE 'Y'.
               88  NOT-CUSTOMER-FOUND                        VALUE 'N'.

           05  WS-DISPLAY-CONFIRMATION-SW
                                       PIC X(01)             VALUE 'N'.
               88  DISPLAY-CONFIRMATION                      VALUE 'Y'.
               88  NOT-DISPLAY-CONFIRMATION                  VALUE 'N'.
           EJECT

      *****************************************************************
      *    MISCELLANEOUS WORK FIELDS                                  *
      *****************************************************************
       01  WS-MISCELLANEOUS-FIELDS.

           05  WMF-TOTAL-ORDERS        PIC S9(5)   VALUE +0  COMP-3.

           05  WMF-MODNAME             PIC X(08)   VALUE 'PDA109O'.
           05  WMF-MODNAME-ERROR       PIC X(08)   VALUE 'PDAERRO'.
           05  WMF-MASTER-LTERM-NAME   PIC X(08)   VALUE 'SMASTER'.
           05  WMF-IO-PCB-LTERM-NAME   PIC X(08)   VALUE SPACES.

           05  WMF-NEXT-TRANID         PIC X(08)   VALUE SPACES.
           05  WMF-NEXT-TRANID-R       REDEFINES   WMF-NEXT-TRANID.
               10  FILLER              PIC X(06).
               10  WMF-NEXT-TRANID-SEQ PIC X(02).

           05  WMF-DATE-MMDDYY         PIC X(08)   VALUE SPACES.

           05  WMF-DATE-YYMMDD.
               10 WMF-DATE-YY          PIC 9(02).
               10 WMF-DATE-MM          PIC X(02).
               10 WMF-DATE-DD          PIC X(02).

           05  WMF-TIME-HHMMSS         PIC X(08)   VALUE SPACES.

           05  WMF-ORDER-NUMBER        PIC 9(10)   VALUE ZEROES.
           05  WMF-ORDER-ITEM-SEQ      PIC 9(05)   VALUE ZEROES.

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
      *  PROCESS ORDER IN/OUT MESSAGE STORAGE AREA                    *
      *  PREFIX: PDA109                                               *
      *****************************************************************

       01  PDA109-MESSAGE              REDEFINES CIOM-MESSAGE.
           05 PDA109-MSG-LL            PIC S9(04)      COMP.
           05 PDA109-MSG-ZZ            PIC X(02).
           05 PDA109-MSG-TRANCODE      PIC X(08).
           05 FILLER                   PIC X(01).
           05 PDA109-MSG-SOURCE        PIC X(03).
           05 FILLER                   PIC X(01).
           05 PDA109-PFKEY             PIC X(02).
           05 PDA109-MSG-USERID-INFO.
              10  PDA109-USERID-ID     PIC X(08).
              10  PDA109-USERID-NUMBER PIC 9(05).
           05 PDA109-PREV-PGRMID       PIC X(08).
           05 PDA109-SAVAREA           PIC X(79).
           05 PDA109-SAVAREA-R         REDEFINES PDA109-SAVAREA.
              10 PDA109-SAVAREA-ORDER-MENU-SEL
                                       PIC X(01).
              10 PDA109-SAVAREA-CUSID  PIC X(32).
              10 PDA109-ORIGINATING-PGRMID
                                       PIC X(08).
              10 PDA109-SAVAREA-PFKEY  PIC X(02).
              10 PDA109-SAVAREA-MESSAGE
                                       PIC X(05).
              10 PDA109-SAVAREA-ORDER  PIC X(10).
           05 PDA109-CUS-INFO.
              10 PDA109-CUS-ID         PIC X(32).
              10 PDA109-CUS-NAME       PIC X(34).
              10 PDA109-CUS-ADDR       PIC X(64).
              10 PDA109-CUS-CITY       PIC X(32).
              10 PDA109-CUS-STATE      PIC X(32).
              10 PDA109-CUS-ZIP        PIC X(12).
              10 PDA109-CUS-EMAIL      PIC X(64).
           05 PDA109-SHP-INFO.
              10 PDA109-SHP-NAME       PIC X(64).
              10 PDA109-SHP-ADDR       PIC X(64).
              10 PDA109-SHP-CITY       PIC X(32).
              10 PDA109-SHP-STATE      PIC X(32).
              10 PDA109-SHP-ZIP        PIC X(12).
           05 PDA109-PUR-INFO.
              10 PDA109-PURTYPE-ATTR.
                  15 PDA109-PURTYPE-ATTR1
                                       PIC X(01).
                  15 PDA109-PURTYPE-ATTR2
                                       PIC X(01).
              10 PDA109-PURTYPE        PIC X(03).
              10 PDA109-PURTYPE-R      REDEFINES PDA109-PURTYPE
                                       PIC 9(03).
              10 PDA109-PURDESC        PIC X(13).
              10 PDA109-PURNBR-ATTR.
                  15 PDA109-PURNBR-ATTR1
                                       PIC X(01).
                  15 PDA109-PURNBR-ATTR2
                                       PIC X(01).
              10 PDA109-PURNBR         PIC X(13).
              10 PDA109-PURNBR-R       REDEFINES PDA109-PURNBR
                                       PIC 9(13).
           05 PDA109-TOTAL-COST        PIC X(13).
           05 PDA109-TOTAL-COST-R      REDEFINES PDA109-TOTAL-COST
                                       PIC ZZ,ZZZ,ZZ9.99.
           05 PDA109-SCREEN-MESSAGE    PIC X(79).
           05 PDA109-SMESSAGE          PIC X(79).
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
      *    ORDER DATABASE ROOT SEGMENT                                *
      *****************************************************************

       01  ORDER-SEGMENT-SAVE          PIC X(250).

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
      *    ORDER DATABASE ORDER ITEM SEGMENT (SSA)                    *
      *****************************************************************

       01  ORDITEM-UNQUAL-SSA.
           05  ORDITEM-UNQUAL-SEGMENT  PIC X(09)   VALUE 'ORDITEM '.

       01  ORDITEM-QUAL-SSA.
           05  ORDITEM-QUAL-SEGMENT    PIC X(08)   VALUE 'ORDITEM'.
           05  ORDITEM-QUAL-LPAREN     PIC X(01)   VALUE '('.
           05  ORDITEM-QUAL-FIELD-NAME
                                       PIC X(08)   VALUE 'ITEMKEY '.
           05  ORDITEM-QUAL-OPERATOR   PIC X(02)   VALUE 'EQ'.
           05  ORDITEM-QUAL-FIELD-VALUE.
               10  ORDITEM-QUAL-PREFIX
                                       PIC X(05)   VALUE SPACES.
               10  ORDITEM-QUAL-SEQUENCE
                                       PIC X(05)   VALUE SPACES.
               10  ORDITEM-QUAL-SEQUENCE-R REDEFINES
                                       ORDITEM-QUAL-SEQUENCE
                                       PIC 9(05).
           05  ORDITEM-QUAL-RPAREN     PIC X(01)   VALUE ')'.


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
      *    DB2 DCLGEN FOR THE ITEM SUPPLIER TABLE                     *
      *****************************************************************
           EXEC SQL
              INCLUDE DITMSUP
           END-EXEC.
           EJECT

      *****************************************************************
      *    DB2 DCLGEN FOR THE PURCHASE TYPE TABLE                     *
      *****************************************************************
           EXEC SQL
              INCLUDE DPURTYP
           END-EXEC.
           EJECT

      *****************************************************************
      *    CUSTOMER ARRAY                                             *
      *****************************************************************

           COPY CUSARRAY.
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

           COPY PCBORDER.
           EJECT

           COPY PCBORDE1.
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
      *                PROCESS ORDER SCREEN                           *
      *                                                               *
      *    CALLED BY:  NONE                                           *
      *                                                               *
      *****************************************************************

       P00000-MAINLINE.

           ENTRY 'DLITCBL'   USING  IO-PCB
                                    ALT-IO-PCB1
                                    ALT-IO-PCB2
                                    PENDORD-PCB
                                    ORDER-PCB
                                    ORDER-PCB1.


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
                                          WS-PROCESS-COMPLETE-SW
                                          WS-CUSTOMER-FOUND-SW
                                          WS-DISPLAY-CONFIRMATION-SW.

           MOVE SPACES                 TO WMF-IO-PCB-LTERM-NAME
                                          WMF-NEXT-TRANID
                                          WMF-DATE-MMDDYY
                                          WMF-TIME-HHMMSS
                                          WMF-MESSAGE-AREA.

      *****************************************************************
      *    OBTAIN CURRENT SYSTEM DATE / TIME                          *
      *****************************************************************

           MOVE FUNCTION CURRENT-DATE  TO WS-CURRENT-DATE-TIME.         00020001
           MOVE WS-CDT-D-YEAR          TO WMF-DATE-YY.
           MOVE WS-CDT-D-MONTH         TO WMF-DATE-MM.
           MOVE WS-CDT-D-DAY           TO WMF-DATE-DD.


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
               MOVE 'PDA109'           TO WPIE-PROGRAM-ID
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

           IF CIOM-MSG-TRANCODE        = 'PDA10901'    AND
              CIOM-MSG-SOURCE          = 'PDA'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA109'           TO WPIE-PROGRAM-ID
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
                                       TO  PDA109-SCREEN-MESSAGE
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

           IF PDA109-PREV-PGRMID       NOT = 'PDA109'
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
           MOVE PDA109-PREV-PGRMID     TO PDA109-ORIGINATING-PGRMID.


      *****************************************************************
      *    PERFORM THE INITIAL INQUIRY CONTAINING CUSTOMER INFO AND   *
      *    TOTAL ORDER AMOUNT INFO                                    *
      *****************************************************************

           PERFORM  P01830-INQUIRY-PROCESS
               THRU P01830-INQUIRY-PROCESS-EXIT.


      *****************************************************************
      *    DISPLAY THE INITIAL SCREEN, SAVE CONTROL VARIABLES         *
      *****************************************************************

           PERFORM  P80000-INSERT-MSG
               THRU P80000-INSERT-MSG-EXIT.

           MOVE 'PDA109'               TO  PDA109-PREV-PGRMID.

       P01800-1ST-TIME-PROCESS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01830-INQUIRY-PROCESS                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PERFORM THE INITIAL INQUIRY         *
      *                PROCESSES                                      *
      *                                                               *
      *    CALLED BY:  P01800-1ST-TIME-PROCESS                        *
      *                                                               *
      *****************************************************************

       P01830-INQUIRY-PROCESS.


      *****************************************************************
      *    LOCATE / FORMAT THE CUSTOMER RELATED INFORMATION           *
      *****************************************************************

           MOVE 'N'                    TO WS-CUSTOMER-FOUND-SW.

           PERFORM  P07100-FIND-CUSTOMER
               THRU P07100-FIND-CUSTOMER-EXIT
                   VARYING WS-SUB1 FROM +1 BY +1
                       UNTIL WS-SUB1 > +13.

           IF CUSTOMER-FOUND
               NEXT SENTENCE
           ELSE
               MOVE PM008-CUST-NOT-FOUND
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P01830-INQUIRY-PROCESS-EXIT.


      *****************************************************************
      *    CALCULATE THE TOTAL COST FOR ALL PENDING ORDERS            *
      *****************************************************************

           PERFORM  P69100-TOTAL-COST
               THRU P69100-TOTAL-COST-EXIT.

       P01830-INQUIRY-PROCESS-EXIT.
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

           MOVE 'PDA109'               TO  PDA109-PREV-PGRMID.

      *****************************************************************
      *    CONVERT UNDERSCORES AND LOW-VALUES TO SPACES               *
      *****************************************************************

           MOVE SPACES                 TO  PDA109-SCREEN-MESSAGE.

           PERFORM  P02100-CONVERT-FIELDS
               THRU P02100-CONVERT-FIELDS-EXIT.


      *****************************************************************
      *    PROCEED WITH THE TRANSACTION EDIT PROCESSES                *
      *****************************************************************

           PERFORM  P03000-EDIT-PROCESS
               THRU P03000-EDIT-PROCESS-EXIT.


      *****************************************************************
      *    DISPLAY THE OUTPUT SCREEN IF ENTER KEY, OR ERROR FOUND     *
      *    OR CONFIRMATION SCREEN IS REQUIRED (PF4 SUBMIT ORDER,      *
      *    PF5 CANCEL ORDER)                                          *
      *****************************************************************

           IF (PDA109-PFKEY  =  'EN')  OR
              (ERROR-FOUND)            OR
              (DISPLAY-CONFIRMATION)
               PERFORM  P80000-INSERT-MSG
                   THRU P80000-INSERT-MSG-EXIT.

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


           INSPECT PDA109-PURTYPE
               CONVERTING  WMF-UNDERSCORE-LOWVALUE-R TO SPACES.


           INSPECT PDA109-PURNBR
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
      *    LOCATE / FORMAT THE CUSTOMER RELATED INFORMATION           *
      *****************************************************************

           MOVE 'N'                    TO WS-CUSTOMER-FOUND-SW.

           PERFORM  P07100-FIND-CUSTOMER
               THRU P07100-FIND-CUSTOMER-EXIT
                   VARYING WS-SUB1 FROM +1 BY +1
                       UNTIL WS-SUB1 > +13.

           IF CUSTOMER-FOUND
               NEXT SENTENCE
           ELSE
               MOVE PM008-CUST-NOT-FOUND
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P03000-EDIT-PROCESS-EXIT.


      *****************************************************************
      *    VALID KEYS ARE: ENTER, PF4, PF5, PF10, PF11, PF12          *
      *****************************************************************

           IF PDA109-PFKEY = 'EN' OR '04' OR '05' OR '10' OR '11' OR
                             '12'
               NEXT SENTENCE
           ELSE
               MOVE PM001-INVALID-PFKEY
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P03000-EDIT-PROCESS-EXIT.


      *****************************************************************
      *    IF ENTER KEY, OR PF04 (SUBMIT ORDER)PROCESS SCREEN EDITS,  *
      *    OTHERWISE PERFORM PFKEY PROCESS                            *
      *****************************************************************

           IF PDA109-PFKEY  =  'EN'  OR  '04'
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
      *    PF5 -- CANCEL ORDER PROCESS                                *
      *    1. DELETE PENDING ORDERS                                   *
      *    2. TRANSFER CONTROL TO THE ORDER MENU WITH MESSAGE         *
      *****************************************************************

           IF  PDA109-PFKEY = '05'
               IF PDA109-SAVAREA-PFKEY = '05'
                   PERFORM  P06500-CANCEL-ORDER
                       THRU P06500-CANCEL-ORDER-EXIT
                   GO TO P04000-PFKEY-PROCESS-EXIT
               ELSE
                   MOVE 'Y'            TO  WS-DISPLAY-CONFIRMATION-SW
                   MOVE LOW-VALUES     TO  PDA109-PURTYPE-ATTR
                   MOVE WS-CURSOR-ATTR TO  PDA109-PURTYPE-ATTR1
                   MOVE PM035-CONFIRM-CANCELL
                                       TO  WMF-MESSAGE-AREA
                   PERFORM  P70100-INFO-MSG-ROUTINE
                       THRU P70100-INFO-MSG-ROUTINE-EXIT
                   GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF PF10, TRANSFER TO THE PENDING ORDER FUNCTION            *
      *****************************************************************

           IF PDA109-PFKEY  =  '10'
               MOVE 'PDA10801'     TO  WMF-NEXT-TRANID
               PERFORM  P80300-XFER-CONTROL
                   THRU P80300-XFER-CONTROL-EXIT
               GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF PF11, TRANSFER TO THE ORDER MENU                        *
      *****************************************************************

           IF PDA109-PFKEY  =  '11'
               MOVE 'PDA10201'     TO  WMF-NEXT-TRANID
               PERFORM  P80300-XFER-CONTROL
                   THRU P80300-XFER-CONTROL-EXIT
               GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF PF12, RETURN TO THE MAIN MENU                           *
      *****************************************************************

           IF PDA109-PFKEY  =  '12'
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
      *    FUNCTION :  ROUTINE TO CONTROL THE PROCESSING WHEN THE     *
      *                ENTER KEY OR PFKEY 04 (SUBMIT ORDER) ARE       *
      *                USED                                           *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************

       P05000-PROCESS-SCREEN.

      *****************************************************************
      *    EDIT SCREEN ENTERABLE FIELDS                               *
      *****************************************************************

           PERFORM  P05100-EDIT-SCREEN
               THRU P05100-EDIT-SCREEN-EXIT.

           IF ERROR-FOUND
               GO TO P05000-PROCESS-SCREEN-EXIT.


      *****************************************************************
      *    IF NO EDIT ERRORS AND THE ENTER KEY USED -- FORMAT ERROR   *
      *    TO DIRECT USER TO USE PFKEY 04 TO SUBMIT ORDER             *
      *****************************************************************

           IF PDA109-PFKEY             =  'EN'
               MOVE LOW-VALUES         TO  PDA109-PURTYPE-ATTR
               MOVE WS-CURSOR-ATTR     TO  PDA109-PURTYPE-ATTR1
               MOVE PM031-USE-PFKEY    TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P05000-PROCESS-SCREEN-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF PFKEY 04 USED AND THIS IS THE USER CONFIRMATION         *
      *    (2ND TIME THRU) PROCEED ON TO CREATE THE ORDER,            *
      *    OTHERWISE SEND OUT USER CONFIRMATION MESSAGE               *
      *****************************************************************

           IF (PDA109-PFKEY            =  '04')    AND
              (PDA109-SAVAREA-PFKEY    =  '04')
               NEXT SENTENCE
           ELSE
               MOVE 'Y'                TO  WS-DISPLAY-CONFIRMATION-SW
               MOVE LOW-VALUES         TO  PDA109-PURTYPE-ATTR
               MOVE WS-CURSOR-ATTR     TO  PDA109-PURTYPE-ATTR1
               MOVE PM034-CONFIRM-PROCESS
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70100-INFO-MSG-ROUTINE
                   THRU P70100-INFO-MSG-ROUTINE-EXIT
               GO TO P05000-PROCESS-SCREEN-EXIT.


      *****************************************************************
      ********** TEMPORARY **********                                 *
      *    LIMIT EACH USER TO 20 ORDERS MAX, DO NOT WANT              *
      *    DATABASE FILLING, USERS DO NOT ALWAYS MAINTAIN THEIR DATA  *
      *****************************************************************

           PERFORM  P68000-COUNT-ORDERS
               THRU P68000-COUNT-ORDERS-EXIT.

           ADD +1                      TO WMF-TOTAL-ORDERS.

           IF WMF-TOTAL-ORDERS         > +20
               MOVE PM052-ORDER-MAXIMUM-EXCEEDED
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P05000-PROCESS-SCREEN-EXIT.


      *****************************************************************
      *    PROCEED ON WITH THE ORDER ADD PROCESS                      *
      *****************************************************************

           PERFORM  P06000-ADD-ORDER
               THRU P06000-ADD-ORDER-EXIT.

           IF ERROR-FOUND
               GO TO P05000-PROCESS-SCREEN-EXIT.


      *****************************************************************
      *    TRANSFER TO THE ORDER MENU WITH -ORDER ADDED- MESSAGE      *
      *****************************************************************

           MOVE SPACES                 TO  PDA109-SAVAREA-PFKEY.
           MOVE 'PM029'                TO  PDA109-SAVAREA-MESSAGE.
           MOVE WMF-ORDER-NUMBER       TO  PDA109-SAVAREA-ORDER.

           MOVE 'PDA10201'             TO  WMF-NEXT-TRANID.

           PERFORM  P80300-XFER-CONTROL
               THRU P80300-XFER-CONTROL-EXIT.


       P05000-PROCESS-SCREEN-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05100-EDIT-SCREEN                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PERFORM SCREEN EDITS                *
      *                                                               *
      *    CALLED BY:  P05000-PROCESS-SCREEN                          *
      *                                                               *
      *****************************************************************

       P05100-EDIT-SCREEN.

      *****************************************************************
      *    EDIT THE PURCHASE TYPE                                     *
      *****************************************************************

           PERFORM  P05130-EDIT-PURCH-TYPE
               THRU P05130-EDIT-PURCH-TYPE-EXIT.


      *****************************************************************
      *    EDIT THE PURCHASE ORDER NUMBER                             *
      *****************************************************************

           PERFORM  P05160-EDIT-PURCH-NBR
               THRU P05160-EDIT-PURCH-NBR-EXIT.


       P05100-EDIT-SCREEN-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05130-EDIT-PURCH-TYPE                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PERFORM THE PURCHASE TYPE EDITS     *
      *                                                               *
      *    CALLED BY:  P05100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P05130-EDIT-PURCH-TYPE.

      *****************************************************************
      *    PURCHASE TYPE IS A REQUIRED ENTRY                          *
      *****************************************************************

           IF PDA109-PURTYPE           =  SPACES
               MOVE LOW-VALUES         TO  PDA109-PURTYPE-ATTR
               MOVE WS-CURSOR-ATTR     TO  PDA109-PURTYPE-ATTR1
               MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA109-PURTYPE-ATTR2
               MOVE PM027-ENTER-PURCHASE-TYPE
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P05130-EDIT-PURCH-TYPE-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    PURCHASE TYPE MUST BE A NUMERIC ENTRY, VALUE 1,2, OR 3     *
      *****************************************************************

           MOVE +3                     TO WMF-NUM-LTH.
           MOVE PDA109-PURTYPE         TO WMF-NUM-INPUT.

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF (WMF-NUM-ERROR           >  ZEROES)    OR
              (WMF-NUM-OUTPUT          <  1)         OR
              (WMF-NUM-OUTPUT          >  3)
               MOVE LOW-VALUES         TO  PDA109-PURTYPE-ATTR
               MOVE WS-CURSOR-ATTR     TO  PDA109-PURTYPE-ATTR1
               MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA109-PURTYPE-ATTR2
               MOVE PM028-INVALID-PURCHASE-TYPE
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P05130-EDIT-PURCH-TYPE-EXIT
           ELSE
               MOVE WMF-NUM-OUTPUT     TO PDA109-PURTYPE-R.


      *****************************************************************
      *    PURCHASE TYPE MUST EXIST IN THE DB2 PURCHASE TYPE TABLE    *
      *****************************************************************

           MOVE PDA109-USERID-NUMBER   TO PURCHASE-TYPE-PREFIX.
           MOVE PDA109-PURTYPE         TO PURCHASE-TYPE-TYPE.

           EXEC SQL
               SELECT  DESCRIPTION
               INTO    :PURCHASE-TYPE-DESCRIPTION
               FROM    PURCHASE_TYPE
               WHERE   PREFIX       = :PURCHASE-TYPE-PREFIX AND
                       TYPE         = :PURCHASE-TYPE-TYPE
           END-EXEC.


           EVALUATE TRUE

               WHEN SQLCODE = +0
                   MOVE PURCHASE-TYPE-DESCRIPTION
                                       TO  PDA109-PURDESC

               WHEN SQLCODE = +100
                   MOVE SPACES         TO  PDA109-PURDESC
                   MOVE LOW-VALUES     TO  PDA109-PURTYPE-ATTR
                   MOVE WS-CURSOR-ATTR TO  PDA109-PURTYPE-ATTR1
                   MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA109-PURTYPE-ATTR2
                   MOVE PM028-INVALID-PURCHASE-TYPE
                                       TO  WMF-MESSAGE-AREA
                   PERFORM  P70000-ERROR-ROUTINE
                       THRU P70000-ERROR-ROUTINE-EXIT

               WHEN OTHER
                   MOVE 'DB2'          TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA109'       TO WPDE-PROGRAM-ID
                   MOVE SQLCODE        TO WPDE-DB2-SQLCODE
                   MOVE 'SELECT PURCHASE_TYPE'
                                       TO WPDE-FUNCTION
                   MOVE 'P05130'       TO WPDE-PARAGRAPH
                   PERFORM  P99500-PDA-ERROR
                       THRU P99500-PDA-ERROR-EXIT

           END-EVALUATE.

       P05130-EDIT-PURCH-TYPE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05160-EDIT-PURCH-NBR                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PERFORM THE PURCHASE ORDER NUMBER   *
      *                EDITS                                          *
      *                                                               *
      *    CALLED BY:  P05100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P05160-EDIT-PURCH-NBR.

      *****************************************************************
      *    PURCHASE ORDER NUMBER IS A REQUIRED ENTRY                  *
      *****************************************************************

           IF PDA109-PURNBR            =  SPACES
               MOVE LOW-VALUES         TO  PDA109-PURNBR-ATTR
               MOVE WS-CURSOR-ATTR     TO  PDA109-PURNBR-ATTR1
               MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA109-PURNBR-ATTR2
               MOVE PM046-INVALID-P-O-NUMBER
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P05160-EDIT-PURCH-NBR-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    PURCHASE ORDER NUMBER MUST BE NUMERIC, NON-ZERO ENTRY      *
      *****************************************************************

           MOVE +13                    TO WMF-NUM-LTH.
           MOVE PDA109-PURNBR          TO WMF-NUM-INPUT.

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF (WMF-NUM-ERROR           >  ZEROES)    OR
              (WMF-NUM-OUTPUT          =  ZEROES)
               MOVE LOW-VALUES         TO  PDA109-PURNBR-ATTR
               MOVE WS-CURSOR-ATTR     TO  PDA109-PURNBR-ATTR1
               MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA109-PURNBR-ATTR2
               MOVE PM046-INVALID-P-O-NUMBER
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P05160-EDIT-PURCH-NBR-EXIT
           ELSE
               MOVE WMF-NUM-OUTPUT     TO PDA109-PURNBR-R.


       P05160-EDIT-PURCH-NBR-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06000-ADD-ORDER                               *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PERFORM THE ORDER ADD PROCESS.      *
      *                BUILD AN ORDER (IMS ORDER DATABASE) FROM ALL   *
      *                PENDING ORDER (IMS DATABASE) RECORDS CREATED   *
      *                BY THE SPECIFIC USER.                          *
      *                                                               *
      *    CALLED BY:  P05000-PROCESS-SCREEN                          *
      *                                                               *
      *****************************************************************

       P06000-ADD-ORDER.

      *****************************************************************
      *    RETRIEVE / POSITION ON THE 1ST PENDING ORDER RECORD,       *
      *    IF UNSUCCESSFUL RETRIEVAL TERMINATE PROGRAM (BECAUSE TO    *
      *    GET THIS FAR IN THE PROCESS THERE HAD TO BE AT LEAST 1     *
      *    PENDING ORDER ON FILE)                                     *
      *****************************************************************

           MOVE 'GE'                   TO PENDORD-QUAL-OPERATOR.
           MOVE PDA109-USERID-NUMBER   TO PENDORD-QUAL-PREFIX.
           MOVE 1                      TO PENDORD-QUAL-SEQUENCE-R.

           PERFORM  P78030-GHU-PENDORD
               THRU P78030-GHU-PENDORD-EXIT.

           IF (PENDORD-STATUS          =  SPACES)    AND
              (PENDING-ORDER-PREFIX    =  PDA109-USERID-NUMBER)
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA109'           TO WPIE-PROGRAM-ID
               MOVE PENDORD-STATUS     TO WPIE-STATUS-CODE
               MOVE 'GHU'              TO WPIE-FUNCTION-CODE
               MOVE 'P06000'           TO WPIE-PARAGRAPH
               MOVE 'PENDORD'          TO WPIE-SEGMENT-NAME
               MOVE 'PENDO1DB'         TO WPIE-DATABASE-NAME
               MOVE 'GHU PENDORD ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    READ / CREATE THE ORDER DATABASE CONTROL RECORD            *
      *****************************************************************

           PERFORM  P06030-ORDERDB-CONTROL
               THRU P06030-ORDERDB-CONTROL-EXIT.

           ADD 1                       TO ORDER-PURCHASE-NUMBER.
           MOVE ORDER-PURCHASE-NUMBER  TO WMF-ORDER-NUMBER.
           MOVE ORDER-SEGMENT          TO ORDER-SEGMENT-SAVE.


      *****************************************************************
      *    CREATE THE ORDER ROOT SEGMENT                              *
      *****************************************************************

           PERFORM  P06060-ADD-ORDER-ROOT
               THRU P06060-ADD-ORDER-ROOT-EXIT.


      *****************************************************************
      *    CREATE THE ORDER ITEM SEGMENTS FROM THE PENDING ORDER      *
      *    DATABASE RECORDS                                           *
      *****************************************************************

           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.
           MOVE ZEROES                 TO WMF-ORDER-ITEM-SEQ
                                          WMF-TOTAL-COST.

           PERFORM  P06100-ADD-ORDER-ITEMS
               THRU P06100-ADD-ORDER-ITEMS-EXIT
                   UNTIL PROCESS-COMPLETE.


      *****************************************************************
      *    UPDATE THE ORDER ROOT SEGMENT (TOTALS, COUNTERS, ETC).     *
      *****************************************************************

           PERFORM  P06130-UPDATE-ORDER
               THRU P06130-UPDATE-ORDER-EXIT.


      *****************************************************************
      *    UPDATE THE ORDER CONTROL ROOT -- LAST ORDER NUMBER         *
      *****************************************************************

           MOVE ORDER-SEGMENT-SAVE     TO ORDER-SEGMENT.

           PERFORM  P79090-REPL-ORDER
               THRU P79090-REPL-ORDER-EXIT.


      *****************************************************************
      *    ALL DONE --- CLEAN OUT THE PENDING ORDER RECORDS (DELETE)  *
      *****************************************************************

           PERFORM  P06500-CANCEL-ORDER
               THRU P06500-CANCEL-ORDER-EXIT.

       P06000-ADD-ORDER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06030-ORDERDB-CONTROL                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE ORDER DB CONTROL REC   *
      *                TO OBTAIN THE NEXT ORDER NUMBER FOR AN ADD. THE*
      *                CONTROL RECORD IS CREATED IF IT DOES NOT EXIST.*
      *                                                               *
      *    CALLED BY:  P06000-ADD-ORDER                               *
      *                                                               *
      *****************************************************************

       P06030-ORDERDB-CONTROL.

      *****************************************************************
      *    ATTEMPT CONTROL RECORD RETRIEVAL, IF FOUND --- EXIT        *
      *****************************************************************

           MOVE 'EQ'                   TO ORDER-QUAL-OPERATOR.
           MOVE PDA109-USERID-NUMBER   TO ORDER-QUAL-PREFIX.
           MOVE ZEROES                 TO ORDER-QUAL-NUMBER.

           PERFORM  P79000-GHU-ORDER
               THRU P79000-GHU-ORDER-EXIT.

           IF OP-STATUS                =  SPACES
               GO TO P06030-ORDERDB-CONTROL-EXIT.


      *****************************************************************
      *    OTHERWISE FORMAT AND CREATE AN ORDER CONTROL REC FOR USER  *
      *****************************************************************

           MOVE SPACES                 TO ORDER-SEGMENT.
           MOVE PDA109-USERID-NUMBER   TO ORDER-PREFIX.
           MOVE ZEROES                 TO ORDER-NUMBER.
           MOVE 5000                   TO ORDER-PURCHASE-NUMBER.
           MOVE ZEROES                 TO ORDER-TOTAL-AMOUNT
                                          ORDER-NEXT-ITEM-SEQUENCE
                                          ORDER-SHIPPER-NUMBER.

           PERFORM  P79060-ISRT-ORDER
               THRU P79060-ISRT-ORDER-EXIT.


      *****************************************************************
      *    RETRIEVE CONTROL RECORD, NOT FOUND --- TERMINATE           *
      *****************************************************************

           MOVE 'EQ'                   TO ORDER-QUAL-OPERATOR.
           MOVE PDA109-USERID-NUMBER   TO ORDER-QUAL-PREFIX.
           MOVE ZEROES                 TO ORDER-QUAL-NUMBER.

           PERFORM  P79000-GHU-ORDER
               THRU P79000-GHU-ORDER-EXIT.

           IF OP-STATUS                =  SPACES
               GO TO P06030-ORDERDB-CONTROL-EXIT.


           MOVE 'IMS'                  TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA109'               TO WPIE-PROGRAM-ID.
           MOVE OP-STATUS              TO WPIE-STATUS-CODE.
           MOVE 'GHU'                  TO WPIE-FUNCTION-CODE.
           MOVE 'P06030'               TO WPIE-PARAGRAPH.
           MOVE 'ORDER'                TO WPIE-SEGMENT-NAME.
           MOVE 'ORDER2DB'             TO WPIE-DATABASE-NAME.
           MOVE 'GHU ORDER ROOT SEGMENT'
                                       TO WPIE-COMMAND.
           PERFORM  P99500-PDA-ERROR
               THRU P99500-PDA-ERROR-EXIT.

       P06030-ORDERDB-CONTROL-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06060-ADD-ORDER-ROOT                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO ADD THE ORDER ROOT SEGMENT          *
      *                                                               *
      *    CALLED BY:  P06000-ADD-ORDER                               *
      *                                                               *
      *****************************************************************

       P06060-ADD-ORDER-ROOT.

      *****************************************************************
      *    FORMAT AND CREATE THE ORDER ROOT SEGMENT                   *
      *****************************************************************

           MOVE SPACES                 TO ORDER-SEGMENT.
           MOVE PDA109-USERID-NUMBER   TO ORDER-PREFIX.
           MOVE WMF-ORDER-NUMBER       TO ORDER-NUMBER.
           MOVE PDA109-PURNBR-R        TO ORDER-PURCHASE-NUMBER.
           MOVE WMF-DATE-YYMMDD        TO ORDER-DATE-YYMMDD.
           MOVE 'IN PROCESS'           TO ORDER-STATUS.
           MOVE ZEROES                 TO ORDER-TOTAL-AMOUNT
                                          ORDER-NEXT-ITEM-SEQUENCE.

           MOVE ZEROES                 TO ORDER-CUSTOMER-PREFIX.
           MOVE PDA109-SAVAREA-CUSID   TO ORDER-CUSTOMER-ID.

           MOVE ZEROES                 TO ORDER-PURCHASE-TYPE-PREFIX.
           MOVE PDA109-PURTYPE-R       TO ORDER-PURCHASE-TYPE.

           MOVE ZEROES                 TO ORDER-SHIPPER-NUMBER.

           PERFORM  P79060-ISRT-ORDER
               THRU P79060-ISRT-ORDER-EXIT.

       P06060-ADD-ORDER-ROOT-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06100-ADD-ORDER-ITEMS                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO ADD THE ORDER ITEMS USING THE       *
      *                RELATED PENDING ORDER FILE RECORDS             *
      *                                                               *
      *    CALLED BY:  P06000-ADD-ORDER                               *
      *                                                               *
      *****************************************************************

       P06100-ADD-ORDER-ITEMS.

      *****************************************************************
      *    FORMAT AND CREATE THE ORDER ITEM SEGMENT                   *
      *****************************************************************

           MOVE SPACES                 TO ORDER-ITEM-SEGMENT.
           MOVE PDA109-USERID-NUMBER   TO ORDER-ITEM-PREFIX.
           ADD  1                      TO WMF-ORDER-ITEM-SEQ.
           MOVE WMF-ORDER-ITEM-SEQ     TO ORDER-ITEM-SEQUENCE.
           MOVE PENDING-ORDER-QUANTITY TO ORDER-ITEM-QUANTITY.
           MOVE PENDING-ORDER-ITEM-KEY TO ORDER-ITEM-ITEM-KEY.
           MOVE PENDING-ORDER-SUPPLIER-KEY
                                       TO ORDER-ITEM-SUPPLIER-KEY.


      *****************************************************************
      *    RETRIEVE THE ITEM UNIT PRICE FROM THE ITEM_SUPPLIER TABLE, *
      *    CALCULATE / ACCUMULATE TOTAL COST, INSERT THE ORDER ITEM,  *
      *    COST, READ NEXT PENDING ORDER                              *
      *****************************************************************

           MOVE PENDING-ORDER-PREFIX   TO ITEM-SUPPLIER-ITEM-PREFIX.
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
               MOVE 'PDA109'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'SELECT ITEM-SUPPLIER'
                                       TO WPDE-FUNCTION
               MOVE 'P06100'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           END-IF.


           MOVE ITEM-SUPPLIER-UNIT-PRICE
                                       TO ORDER-ITEM-UNIT-PRICE.

           COMPUTE WMF-EXT-PRICE       =  ITEM-SUPPLIER-UNIT-PRICE *
                                          PENDING-ORDER-QUANTITY.

           ADD WMF-EXT-PRICE           TO WMF-TOTAL-COST.


           MOVE 'EQ'                   TO ORDER-QUAL-OPERATOR.
           MOVE ORDER-PREFIX           TO ORDER-QUAL-PREFIX.
           MOVE ORDER-NUMBER           TO ORDER-QUAL-NUMBER.
           PERFORM  P79130-ISRT-ORDITEM
               THRU P79130-ISRT-ORDITEM-EXIT.


      *****************************************************************
      *    READ THE NEXT PENDING ORDER                                 *
      *****************************************************************

           PERFORM  P78100-GN-PENDORD
               THRU P78100-GN-PENDORD-EXIT.


           IF (PENDORD-STATUS          =  SPACES)    AND
              (PENDING-ORDER-PREFIX    =  PDA109-USERID-NUMBER)
               NEXT SENTENCE
           ELSE
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW.

       P06100-ADD-ORDER-ITEMS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06130-UPDATE-ORDER                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO UPDATE THE ORDER ROOT WITH TOTALS,  *
      *                COUNTERS, ETC. AT THE END OF THE PROCESS.      *
      *                                                               *
      *    CALLED BY:  P06000-ADD-ORDER                               *
      *                                                               *
      *****************************************************************

       P06130-UPDATE-ORDER.

      *****************************************************************
      *    RETRIEVE ORDER ROOT WITH HOLD                              *
      *****************************************************************

           MOVE 'EQ'                   TO ORDER-QUAL-OPERATOR.
           MOVE PDA109-USERID-NUMBER   TO ORDER-QUAL-PREFIX.
           MOVE ORDER-NUMBER           TO ORDER-QUAL-NUMBER.

           PERFORM  P79010-GHU-ORDER1
               THRU P79010-GHU-ORDER1-EXIT.

           IF OP1-STATUS               =  SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA109'           TO WPIE-PROGRAM-ID
               MOVE OP1-STATUS         TO WPIE-STATUS-CODE
               MOVE 'GHU'              TO WPIE-FUNCTION-CODE
               MOVE 'P06130'           TO WPIE-PARAGRAPH
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME
               MOVE 'ORDER2DB'         TO WPIE-DATABASE-NAME
               MOVE 'GHU ORDER ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

      *****************************************************************
      *    FORMAT ACCUMULATED INFO INTO THE ORDER ROOT                *
      *****************************************************************

           MOVE WMF-TOTAL-COST         TO ORDER-TOTAL-AMOUNT.
           MOVE WMF-ORDER-ITEM-SEQ     TO ORDER-NEXT-ITEM-SEQUENCE.

           PERFORM  P79100-REPL-ORDER1
               THRU P79100-REPL-ORDER1-EXIT.

       P06130-UPDATE-ORDER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06500-CANCEL-ORDER                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO HANDLE THE ORDER CANCELLATION       *
      *                PROCESS, INVOKED VIA PFKEY 05                  *
      *                                                               *
      *    CALLED BY:  P04000-PFKEY-PROCESS                           *
      *                P06000-ADD-ORDER                               *
      *                                                               *
      *****************************************************************

       P06500-CANCEL-ORDER.

      *****************************************************************
      *    ESTABLISH DATABASE POSITION AT 1ST PENDING ORDER RECORD    *
      *    FOR THE USER (SHOULD BE THE CONTROL RECORD)                *
      *****************************************************************

           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.
           MOVE 'GE'                   TO PENDORD-QUAL-OPERATOR.
           MOVE PDA109-USERID-NUMBER   TO PENDORD-QUAL-PREFIX.
           MOVE ZEROES                 TO PENDORD-QUAL-SEQUENCE-R.

           PERFORM  P78030-GHU-PENDORD
               THRU P78030-GHU-PENDORD-EXIT.


      *****************************************************************
      *    READ / DELETE ALL PENDING ORDER RECORDS FOR THE USER       *
      *****************************************************************

           PERFORM  P06530-RETRIEVE-ORDERS
               THRU P06530-RETRIEVE-ORDERS-EXIT
                   UNTIL PROCESS-COMPLETE.


      *****************************************************************
      *    FOR PFKEY = 05, FORMAT THE -ORDER CANCELLED- MESSAGE,      *
      *    TRANSFER TO THE ORDER MENU                                 *
      *                                                               *
      *    FOR PFKEY = 04, ORDER ADD, THIS ROUTINE IS USED TO CLEAR   *
      *    ALL PENDING ORDER RECORDS FROM THE PENDING ORDER DATABASE, *
      *    TRANSFER WITH A DIFFERENT MESSAGE OCCURS IN THE ORDER ADD  *
      *****************************************************************

           IF PDA109-PFKEY             =  '05'
               MOVE SPACES             TO  PDA109-SAVAREA-PFKEY
               MOVE 'PM030'            TO  PDA109-SAVAREA-MESSAGE
               MOVE 'PDA10201'         TO  WMF-NEXT-TRANID
               PERFORM  P80300-XFER-CONTROL
                   THRU P80300-XFER-CONTROL-EXIT.

       P06500-CANCEL-ORDER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06530-RETRIEVE-ORDERS                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ / DELETE PENDING ORDER RECORDS *
      *                FOR AN ORDER CANCELLATION                      *
      *                                                               *
      *    CALLED BY:  P06500-CANCEL-ORDER                            *
      *                                                               *
      *****************************************************************

       P06530-RETRIEVE-ORDERS.

      *****************************************************************
      *    DELETE IF PENDING ORDER IS FOR THE SELECTED USER           *
      *****************************************************************

           IF (PENDORD-STATUS          =  SPACES)    AND
              (PENDING-ORDER-PREFIX    =  PDA109-USERID-NUMBER)
               PERFORM  P78130-DLET-PENDORD
                   THRU P78130-DLET-PENDORD-EXIT
           ELSE
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
               GO TO P06530-RETRIEVE-ORDERS-EXIT.


      *****************************************************************
      *    READ THE NEXT PENDING ORDER                                 *
      *****************************************************************

           PERFORM  P78110-GHN-PENDORD
               THRU P78110-GHN-PENDORD-EXIT.

       P06530-RETRIEVE-ORDERS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07100-FIND-CUSTOMER                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO SEARCH THE CUSTOMER ID ARRAY FOR    *
      *                THE FIRST CUSTOMER ID EQUAL TO THE HIDDEN      *
      *                CUSTOMER ID. IF ENTRY FOUND                    *
      *                FORMAT SCREEN WITH CUSTOMER INFO.              *
      *                                                               *
      *    CALLED BY:  P01830-INQUIRY-PROCESS                         *
      *                                                               *
      *****************************************************************

       P07100-FIND-CUSTOMER.

      *****************************************************************
      *    IF CUSTOMER FOUND ---- FORMAT CUSTOMER INFO                *
      *****************************************************************

           IF WCAR-CUSTOMER-ID (WS-SUB1)  =  PDA109-SAVAREA-CUSID
               MOVE 'Y'                TO WS-CUSTOMER-FOUND-SW
               MOVE WCAR-CUSTOMER-ID      (WS-SUB1)
                                       TO PDA109-CUS-ID
               MOVE WCAR-CUSTOMER-NAME    (WS-SUB1)
                                       TO PDA109-CUS-NAME
               MOVE WCAR-CUSTOMER-ADDRESS (WS-SUB1)
                                       TO PDA109-CUS-ADDR
               MOVE WCAR-CUSTOMER-CITY    (WS-SUB1)
                                       TO PDA109-CUS-CITY
               MOVE WCAR-CUSTOMER-STATE   (WS-SUB1)
                                       TO PDA109-CUS-STATE
               MOVE WCAR-CUSTOMER-ZIP     (WS-SUB1)
                                       TO PDA109-CUS-ZIP
               MOVE WCAR-CUSTOMER-EMAIL   (WS-SUB1)
                                       TO PDA109-CUS-EMAIL
               MOVE WCAR-SHIP-NAME        (WS-SUB1)
                                       TO PDA109-SHP-NAME
               MOVE WCAR-SHIP-ADDRESS     (WS-SUB1)
                                       TO PDA109-SHP-ADDR
               MOVE WCAR-SHIP-CITY        (WS-SUB1)
                                       TO PDA109-SHP-CITY
               MOVE WCAR-SHIP-STATE       (WS-SUB1)
                                       TO PDA109-SHP-STATE
               MOVE WCAR-SHIP-ZIP         (WS-SUB1)
                                       TO PDA109-SHP-ZIP
               MOVE +14                TO WS-SUB1
           ELSE
               NEXT SENTENCE.


       P07100-FIND-CUSTOMER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P68000-COUNT-ORDERS                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO COUNT THE NUMBER OF ORDERS          *
      *                FOR A PARTICULAR USER                          *
      *                                                               *
      *    CALLED BY:  P05000-PROCESS-SCREEN                          *
      *                                                               *
      *****************************************************************

       P68000-COUNT-ORDERS.

      *****************************************************************
      *    ESTABLISH DATABASE POSITION AT 1ST ORDER FOR USER          *
      *****************************************************************

           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.
           MOVE ZEROES                 TO WMF-TOTAL-ORDERS.

           MOVE 'GE'                   TO ORDER-QUAL-OPERATOR.
           MOVE PDA109-USERID-NUMBER   TO ORDER-QUAL-PREFIX.
           MOVE 1                      TO ORDER-QUAL-NUMBER-R.

           PERFORM  P79000-GHU-ORDER
               THRU P79000-GHU-ORDER-EXIT.


      *****************************************************************
      *    PROCESS ALL ORDERS FOR THE USER                            *
      *****************************************************************

           PERFORM  P68100-RETRIEVE-ORDERS
               THRU P68100-RETRIEVE-ORDERS-EXIT
                   UNTIL PROCESS-COMPLETE.


       P68000-COUNT-ORDERS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P68100-RETRIEVE-ORDERS                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ THE ORDERS FROM THE            *
      *                ORDER DATABASE                                 *
      *                                                               *
      *    CALLED BY:  P68000-COUNT-ORDERS                            *
      *                                                               *
      *****************************************************************

       P68100-RETRIEVE-ORDERS.

      *****************************************************************
      *    DETERMINE IF ORDER IS FOR THE SELECTED USER                *
      *****************************************************************

           IF (OP-STATUS               =  SPACES)    AND
              (ORDER-PREFIX            =  PDA109-USERID-NUMBER)
               NEXT SENTENCE
           ELSE
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
               GO TO P68100-RETRIEVE-ORDERS-EXIT.


      *****************************************************************
      *    INCREMENT ORDER COUNTER, READ THE NEXT ORDER               *
      *****************************************************************

           ADD +1                      TO WMF-TOTAL-ORDERS.

           PERFORM  P79030-GHN-ORDER
               THRU P79030-GHN-ORDER-EXIT.

       P68100-RETRIEVE-ORDERS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P69100-TOTAL-COST                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO OBTAIN THE TOTAL ORDER COST VIA     *
      *                READING ALL PENDING ORDERS FOR THE USERID      *
      *                                                               *
      *    CALLED BY:  P01830-INQUIRY-PROCESS                         *
      *                                                               *
      *****************************************************************

       P69100-TOTAL-COST.

      *****************************************************************
      *    ESTABLISH DATABASE POSITION AT 1ST PENDING ORDER FOR USER  *
      *****************************************************************

           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.
           MOVE ZEROES                 TO WMF-TOTAL-COST.

           MOVE 'GE'                   TO PENDORD-QUAL-OPERATOR.
           MOVE PDA109-USERID-NUMBER   TO PENDORD-QUAL-PREFIX.
           MOVE 1                      TO PENDORD-QUAL-SEQUENCE-R.

           PERFORM  P78000-GU-PENDORD
               THRU P78000-GU-PENDORD-EXIT.


      *****************************************************************
      *    PROCESS ALL PENDING ORDERS FOR THE USER                    *
      *****************************************************************

           PERFORM  P69200-RETRIEVE-ORDERS
               THRU P69200-RETRIEVE-ORDERS-EXIT
                   UNTIL PROCESS-COMPLETE.

           MOVE WMF-TOTAL-COST         TO PDA109-TOTAL-COST-R.

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
              (PENDING-ORDER-PREFIX    =  PDA109-USERID-NUMBER)
               NEXT SENTENCE
           ELSE
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
               GO TO P69200-RETRIEVE-ORDERS-EXIT.


      *****************************************************************
      *    RETRIEVE THE ITEM UNIT PRICE FROM THE ITEM_SUPPLIER TABLE, *
      *    CALCULATE / ACCUMULATE TOTAL COST, READ NEXT PENDING ORDER *
      *****************************************************************

           MOVE PENDING-ORDER-PREFIX   TO ITEM-SUPPLIER-ITEM-PREFIX.
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
               MOVE 'PDA109'           TO WPDE-PROGRAM-ID
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

           IF PDA109-SCREEN-MESSAGE    >  SPACES
               NEXT SENTENCE
           ELSE
               MOVE WMF-MESSAGE-AREA   TO PDA109-SCREEN-MESSAGE.

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

           IF PDA109-SCREEN-MESSAGE    >  SPACES
               NEXT SENTENCE
           ELSE
               MOVE WMF-MESSAGE-AREA   TO PDA109-SCREEN-MESSAGE.

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
      *    CALLED BY:  P69100-TOTAL-COST                              *
      *                                                               *
      *****************************************************************

       P78000-GU-PENDORD.


           CALL 'CBLTDLI'    USING     GU
                                       PENDORD-PCB
                                       PENDING-ORDER-SEGMENT
                                       PENDORD-QUAL-SSA.


           IF PENDORD-STATUS           =  SPACES OR 'GE' OR 'GB'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA109'           TO WPIE-PROGRAM-ID
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
      *    CALLED BY:  P06000-ADD-ORDER                               *
      *                P06500-CANCEL-ORDER                            *
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
               MOVE 'PDA109'           TO WPIE-PROGRAM-ID
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
      *    CALLED BY:  P06100-ADD-ORDER-ITEMS                         *
      *                P69200-RETRIEVE-ORDERS                         *
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
               MOVE 'PDA109'           TO WPIE-PROGRAM-ID
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
      *    PARAGRAPH:  P78110-GHN-PENDORD                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE PENDING ORDER ROOT     *
      *                SEGMENT (WITH HOLD)                            *
      *                                                               *
      *    CALLED BY:  P06530-RETRIEVE-ORDERS                         *
      *                                                               *
      *****************************************************************

       P78110-GHN-PENDORD.


           CALL 'CBLTDLI'    USING     GHN
                                       PENDORD-PCB
                                       PENDING-ORDER-SEGMENT
                                       PENDORD-QUAL-SSA.


           IF PENDORD-STATUS           =  SPACES OR 'GE' OR 'GB'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA109'           TO WPIE-PROGRAM-ID
               MOVE PENDORD-STATUS     TO WPIE-STATUS-CODE
               MOVE 'GHN'              TO WPIE-FUNCTION-CODE
               MOVE 'P78110'           TO WPIE-PARAGRAPH
               MOVE 'PENDORD'          TO WPIE-SEGMENT-NAME
               MOVE 'PENDO1DB'         TO WPIE-DATABASE-NAME
               MOVE 'GHN PENDORD ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P78110-GHN-PENDORD-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P78130-DLET-PENDORD                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DELETE THE PENDING ORDER ROOT       *
      *                SEGMENT                                        *
      *                                                               *
      *    CALLED BY:  P06530-RETRIEVE-ORDERS                         *
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
               MOVE 'PDA109'           TO WPIE-PROGRAM-ID
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
      *    PARAGRAPH:  P79000-GHU-ORDER                               *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE ORDER ROOT             *
      *                SEGMENT WITH HOLD                              *
      *                                                               *
      *    CALLED BY:  P06030-ORDERDB-CONTROL                         *
      *                                                               *
      *****************************************************************

       P79000-GHU-ORDER.


           CALL 'CBLTDLI'    USING     GHU
                                       ORDER-PCB
                                       ORDER-SEGMENT
                                       ORDER-QUAL-SSA.


           IF OP-STATUS                =  SPACES OR 'GE' OR 'GB'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA109'           TO WPIE-PROGRAM-ID
               MOVE OP-STATUS          TO WPIE-STATUS-CODE
               MOVE 'GHU'              TO WPIE-FUNCTION-CODE
               MOVE 'P79000'           TO WPIE-PARAGRAPH
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME
               MOVE 'ORDER2DB'         TO WPIE-DATABASE-NAME
               MOVE 'GHU ORDER ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P79000-GHU-ORDER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79010-GHU-ORDER1                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE ORDER ROOT             *
      *                SEGMENT WITH HOLD USING ORDER-PCB1             *
      *                                                               *
      *    CALLED BY:  P06130-UPDATE-ORDER                            *
      *                                                               *
      *****************************************************************

       P79010-GHU-ORDER1.


           CALL 'CBLTDLI'    USING     GHU
                                       ORDER-PCB1
                                       ORDER-SEGMENT
                                       ORDER-QUAL-SSA.


           IF OP1-STATUS               =  SPACES OR 'GE' OR 'GB'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA109'           TO WPIE-PROGRAM-ID
               MOVE OP1-STATUS         TO WPIE-STATUS-CODE
               MOVE 'GHU'              TO WPIE-FUNCTION-CODE
               MOVE 'P79010'           TO WPIE-PARAGRAPH
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME
               MOVE 'ORDER2DB'         TO WPIE-DATABASE-NAME
               MOVE 'GHU ORDER ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P79010-GHU-ORDER1-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79030-GHN-ORDER                               *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE ORDER ROOT             *
      *                SEGMENT WITH HOLD                              *
      *                                                               *
      *    CALLED BY:  P68000-COUNT-ORDERS                            *
      *                                                               *
      *****************************************************************

       P79030-GHN-ORDER.


           CALL 'CBLTDLI'    USING     GHN
                                       ORDER-PCB
                                       ORDER-SEGMENT
                                       ORDER-QUAL-SSA.


           IF OP-STATUS                =  SPACES OR 'GE' OR 'GB'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA109'           TO WPIE-PROGRAM-ID
               MOVE OP-STATUS          TO WPIE-STATUS-CODE
               MOVE 'GHN'              TO WPIE-FUNCTION-CODE
               MOVE 'P79030'           TO WPIE-PARAGRAPH
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME
               MOVE 'ORDER2DB'         TO WPIE-DATABASE-NAME
               MOVE 'GHN ORDER ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P79030-GHN-ORDER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79060-ISRT-ORDER                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INSERT THE ORDER ROOT               *
      *                SEGMENT                                        *
      *                                                               *
      *    CALLED BY:  P06030-ORDERDB-CONTROL                         *
      *                P06060-ADD-ORDER-ROOT                          *
      *                                                               *
      *****************************************************************

       P79060-ISRT-ORDER.


           CALL 'CBLTDLI'    USING     ISRT
                                       ORDER-PCB1
                                       ORDER-SEGMENT
                                       ORDER-UNQUAL-SSA.


           IF OP1-STATUS               =  SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA109'           TO WPIE-PROGRAM-ID
               MOVE OP1-STATUS         TO WPIE-STATUS-CODE
               MOVE 'ISRT'             TO WPIE-FUNCTION-CODE
               MOVE 'P79060'           TO WPIE-PARAGRAPH
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME
               MOVE 'ORDER2DB'         TO WPIE-DATABASE-NAME
               MOVE 'ISRT ORDER ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P79060-ISRT-ORDER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79090-REPL-ORDER                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO REPLACE THE ORDER ROOT SEGMENT      *
      *                                                               *
      *    CALLED BY:  P06000-ADD-ORDER                               *
      *                                                               *
      *****************************************************************

       P79090-REPL-ORDER.


           CALL 'CBLTDLI'    USING     REPL
                                       ORDER-PCB
                                       ORDER-SEGMENT.


           IF OP-STATUS                =  SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA109'           TO WPIE-PROGRAM-ID
               MOVE OP-STATUS          TO WPIE-STATUS-CODE
               MOVE 'REPL'             TO WPIE-FUNCTION-CODE
               MOVE 'P79090'           TO WPIE-PARAGRAPH
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME
               MOVE 'ORDER2DB'         TO WPIE-DATABASE-NAME
               MOVE 'REPL ORDER ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P79090-REPL-ORDER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79100-REPL-ORDER1                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO REPLACE THE ORDER ROOT SEGMENT      *
      *                USING ORDER-PCB1                               *
      *                                                               *
      *    CALLED BY:  P06130-UPDATE-ROOT                             *
      *                                                               *
      *****************************************************************

       P79100-REPL-ORDER1.


           CALL 'CBLTDLI'    USING     REPL
                                       ORDER-PCB1
                                       ORDER-SEGMENT.


           IF OP1-STATUS               =  SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA109'           TO WPIE-PROGRAM-ID
               MOVE OP1-STATUS         TO WPIE-STATUS-CODE
               MOVE 'REPL'             TO WPIE-FUNCTION-CODE
               MOVE 'P79100'           TO WPIE-PARAGRAPH
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME
               MOVE 'ORDER2DB'         TO WPIE-DATABASE-NAME
               MOVE 'REPL ORDER ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P79100-REPL-ORDER1-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79130-ISRT-ORDITEM                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INSERT THE ORDER ITEM CHILD         *
      *                SEGMENT                                        *
      *                                                               *
      *    CALLED BY:  P06100-ADD-ORDITEMS                            *
      *                                                               *
      *****************************************************************

       P79130-ISRT-ORDITEM.


           CALL 'CBLTDLI'    USING     ISRT
                                       ORDER-PCB1
                                       ORDER-ITEM-SEGMENT
                                       ORDER-QUAL-SSA
                                       ORDITEM-UNQUAL-SSA.


           IF OP1-STATUS               =  SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA109'           TO WPIE-PROGRAM-ID
               MOVE OP1-STATUS         TO WPIE-STATUS-CODE
               MOVE 'ISRT'             TO WPIE-FUNCTION-CODE
               MOVE 'P79130'           TO WPIE-PARAGRAPH
               MOVE 'ORDITEM'          TO WPIE-SEGMENT-NAME
               MOVE 'ORDER2DB'         TO WPIE-DATABASE-NAME
               MOVE 'ISRT ORDER ITEM SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P79130-ISRT-ORDITEM-EXIT.
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
      *                P01800-1ST-TIME-PROCESS                        *
      *                P02000-PROCESS-TRANS                           *
      *                                                               *
      *****************************************************************

       P80000-INSERT-MSG.

      *****************************************************************
      *    FORMAT OUTPUT MESSAGE FIELDS,                              *
      *    RESET ENTERABLE FIELDS BACK TO DEFAULT (IF NECESSARY)      *
      *****************************************************************

           MOVE 'PDA109'               TO PDA109-PREV-PGRMID.

           IF NO-ERROR-FOUND
               MOVE PDA109-PFKEY       TO PDA109-SAVAREA-PFKEY
           ELSE
               MOVE SPACES             TO PDA109-SAVAREA-PFKEY.

           MOVE LENGTH OF PDA109-MESSAGE
                                       TO PDA109-MSG-LL.

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
               MOVE 'PDA109'           TO WPIE-PROGRAM-ID
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
      *                P04000-PFKEY-PROCESS                           *
      *                P05000-PROCESS-SCREEN                          *
      *                P06500-CANCEL-ORDER                            *
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
               MOVE 'PDA109'           TO WPIE-PROGRAM-ID
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
           MOVE 'PDA109'               TO CIOM-PREV-PGRMID.


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
               MOVE 'PDA109'           TO WPIE-PROGRAM-ID
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