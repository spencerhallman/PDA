       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA110.

      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA110                                            *
      * TRANS   :   PDA11001                                          *
      *                                                               *
      * FUNCTION:   PROGRAM PDA110 IS THE IMS/DC PRODUCT DEMONSTRATION*
      *             APPLICATION ORDER INQUIRY / MAINTENANCE FUNCTION. *
      *             THE SCREEN CONTAINS ALL RELEVANT ORDER            *
      *             INFORMATION INCLUDING CUSTOMER AND SHIPPING       *
      *             INFORMATION.                                      *
      *                                                               *
      *             THE SCREEN ENTERABLE KEY FIELDS ARE ACTION AND    *
      *             ORDER NUMBER. NO ACTION CODE (BLANK) INDICATES    *
      *             AN INQUIRY, AN ACTION OF -C- INDICATES CHANGE,    *
      *             AN ACTION OF -D- INDICATES DELETE.                *
      *                                                               *
      *                                                               *
      *                                                               *
      * FILES   :   ORDER         (ORDER2DB)    - IMS (READ/UPDATE)   *
      *             PURCHASE_TYPE (TABLE)       - DB2 (READ)          *
      *                                                               *
      *                                                               *
      * TRANSACTIONS GENERATED:                                       *
      *             PDA10101   MAIN MENU                              *
      *             PDA10201   ORDER MENU                             *
      *             PDA11101   VIEW ORDER ITEMS                       *
      *             PDA11201   BROWSE SUBMITTED ORDERS                *
      *                                                               *
      *                                                               *
      * PFKEYS  :   PF4   =    VIEW ORDER ITEMS        (PDA111)       *
      *             PF5   =    BROWSE SUBMITTED ORDERS (PDA112)       *
      *             PF11  =    ORDER MENU              (PDA102)       *
      *             PF12  =    MAIN MENU               (PDA101)       *
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

           05  WS-ACTION-CODE-SW       PIC X(01)             VALUE ' '.
               88  ACTION-IS-INQUIRY                         VALUE ' '.
               88  ACTION-IS-CHANGE                          VALUE 'C'.
               88  ACTION-IS-DELETE                          VALUE 'D'.
               88  ACTION-IS-VALID                           VALUE ' '
                                                                   'C'
                                                                   'D'.

           05  WS-CUSTOMER-FOUND-SW    PIC X(01)             VALUE 'N'.
               88  CUSTOMER-FOUND                            VALUE 'Y'.
               88  NOT-CUSTOMER-FOUND                        VALUE 'N'.

           EJECT

      *****************************************************************
      *    MISCELLANEOUS WORK FIELDS                                  *
      *****************************************************************
       01  WS-MISCELLANEOUS-FIELDS.

           05  WMF-MODNAME             PIC X(08)   VALUE 'PDA110O'.
           05  WMF-MODNAME-ERROR       PIC X(08)   VALUE 'PDAERRO'.
           05  WMF-MASTER-LTERM-NAME   PIC X(08)   VALUE 'SMASTER'.
           05  WMF-IO-PCB-LTERM-NAME   PIC X(08)   VALUE SPACES.
           05  WMF-CUSTOMER-ID         PIC X(32)   VALUE SPACES.

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

           05  WMF-ORDER-DATE          PIC X(06)   VALUE ZEROES.
           05  WMF-ORDER-DATE-R        REDEFINES   WMF-ORDER-DATE.
               10 WMF-ORDER-DATE-YY    PIC 9(02).
               10 WMF-ORDER-DATE-MM    PIC 9(02).
               10 WMF-ORDER-DATE-DD    PIC 9(02).


           05  WMF-DAYS-IN-MONTH       PIC X(24)   VALUE
               '312831303130313130313031'.
           05  WMF-DAYS-IN-MONTH-R     REDEFINES   WMF-DAYS-IN-MONTH
                                       OCCURS  12 TIMES
                                       PIC 9(02).

           05  WMF-ORDER-KEY           PIC X(15)   VALUE SPACES.
           05  WMF-ORDER-KEY-R         REDEFINES   WMF-ORDER-KEY.
               10 WMF-ORDER-KEY-PREFIX PIC 9(05).
               10 WMF-ORDER-KEY-NUMBER PIC X(10).

           05  WMF-PURCHASE-TYPE-KEY.
               10 WMF-PURCHASE-PREFIX  PIC X(05)   VALUE ZEROES.
               10 WMF-PURCHASE-NUMBER  PIC X(03)   VALUE ZEROES.


       01  WS-PDAS02                   PIC X(8)    VALUE 'PDAS02'.

           COPY PDAS01CY.

       01  PDAS03-PARMS.
           03  PDAS03-AGE-DAYS         PIC 9(5)    VALUE ZEROES.
           03  PDAS03-MESSAGE          PIC X(15)   VALUE SPACES.

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
      *  ORDER INQUIRY-UPDATE IN/OUT MESSAGE STORAGE AREA
      *  PREFIX: PDA110                                               *
      *****************************************************************

       01  PDA110-MESSAGE              REDEFINES CIOM-MESSAGE.
           05 PDA110-MSG-LL            PIC S9(04)      COMP.
           05 PDA110-MSG-ZZ            PIC X(02).
           05 PDA110-MSG-TRANCODE      PIC X(08).
           05 FILLER                   PIC X(01).
           05 PDA110-MSG-SOURCE        PIC X(03).
           05 FILLER                   PIC X(01).
           05 PDA110-PFKEY             PIC X(02).
           05 PDA110-MSG-USERID-INFO.
              10  PDA110-USERID-ID     PIC X(08).
              10  PDA110-USERID-NUMBER PIC 9(05).
           05 PDA110-PREV-PGRMID       PIC X(08).
           05 PDA110-SAVAREA           PIC X(79).
           05 PDA110-SAVAREA-R         REDEFINES PDA110-SAVAREA.
              10 PDA110-SAVAREA-ORDER-MENU-SEL
                                       PIC X(01).
              10 PDA110-SAVAREA-CUSID  PIC X(32).
              10 PDA110-ORIGINATING-PGRMID
                                       PIC X(08).
              10 PDA110-SAVAREA-ORDERNBR
                                       PIC X(10).

           05 PDA110-ORDERNBR-ATTR.
              10 PDA110-ORDERNBR-ATTR1 PIC X(01).
              10 PDA110-ORDERNBR-ATTR2 PIC X(01).
           05 PDA110-ORDERNBR          PIC X(10).

           05 PDA110-ACTION-ATTR.
              10 PDA110-ACTION-ATTR1   PIC X(01).
              10 PDA110-ACTION-ATTR2   PIC X(01).
           05 PDA110-ACTION            PIC X(01).

           05 PDA110-CUS-INFO.
              10 PDA110-CUS-ID         PIC X(32).
              10 PDA110-CUS-NAME       PIC X(34).
              10 PDA110-CUS-ADDR       PIC X(64).
              10 PDA110-CUS-CITY       PIC X(32).
              10 PDA110-CUS-STATE      PIC X(32).
              10 PDA110-CUS-ZIP        PIC X(12).
              10 PDA110-CUS-EMAIL      PIC X(64).

           05 PDA110-SHP-INFO.
              10 PDA110-SHP-NAME       PIC X(64).
              10 PDA110-SHP-ADDR       PIC X(64).
              10 PDA110-SHP-CITY       PIC X(32).
              10 PDA110-SHP-STATE      PIC X(32).
              10 PDA110-SHP-ZIP        PIC X(12).

           05 PDA110-ORDERDATEMM-ATTR.
              10 PDA110-ORDERDATEMM-ATTR1
                                       PIC X(01).
              10 PDA110-ORDERDATEMM-ATTR2
                                       PIC X(01).
           05 PDA110-ORDERDATEMM       PIC X(02).
           05 PDA110-ORDERDATEMM-R     REDEFINES PDA110-ORDERDATEMM
                                       PIC 9(02).

           05 PDA110-ORDERDATEDD-ATTR.
              10 PDA110-ORDERDATEDD-ATTR1
                                       PIC X(01).
              10 PDA110-ORDERDATEDD-ATTR2
                                       PIC X(01).
           05 PDA110-ORDERDATEDD       PIC X(02).
           05 PDA110-ORDERDATEDD-R     REDEFINES PDA110-ORDERDATEDD
                                       PIC 9(02).

           05 PDA110-ORDERDATEYY-ATTR.
              10 PDA110-ORDERDATEYY-ATTR1
                                       PIC X(01).
              10 PDA110-ORDERDATEYY-ATTR2
                                       PIC X(01).
           05 PDA110-ORDERDATEYY       PIC X(02).
           05 PDA110-ORDERDATEYY-R     REDEFINES PDA110-ORDERDATEYY
                                       PIC 9(02).

           05 PDA110-ORDERSTATUS       PIC X(15).
           05 PDA110-TOTALCOST         PIC X(13).
           05 PDA110-TOTALCOST-R       REDEFINES PDA110-TOTALCOST
                                       PIC ZZ,ZZZ,ZZ9.99.
           05 PDA110-PURTYPE           PIC X(03).
           05 PDA110-PURTYPE-R         REDEFINES PDA110-PURTYPE
                                       PIC 9(03).
           05 PDA110-PURDESC           PIC X(13).
           05 PDA110-PURNBR            PIC X(13).
           05 PDA110-PURNBR-R          REDEFINES PDA110-PURNBR
                                       PIC 9(13).

           05 PDA110-SCREEN-MESSAGE    PIC X(79).
           05 PDA110-SMESSAGE          PIC X(79).
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
      *                ORDER INQUIRY / MAINTENANCE SCREEN             *
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

           MOVE 'N'                    TO WS-ERROR-FOUND-SW
                                          WS-CUSTOMER-FOUND-SW.

           MOVE SPACES                 TO WS-ACTION-CODE-SW.

           MOVE SPACES                 TO WMF-IO-PCB-LTERM-NAME
                                          WMF-NEXT-TRANID
                                          WMF-DATE-MMDDYY
                                          WMF-DATE-YYMMDD
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
               MOVE 'PDA110'           TO WPIE-PROGRAM-ID
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

           IF CIOM-MSG-TRANCODE        = 'PDA11001'    AND
              CIOM-MSG-SOURCE          = 'PDA'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA110'           TO WPIE-PROGRAM-ID
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
                                       TO  PDA110-SCREEN-MESSAGE
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

           IF PDA110-PREV-PGRMID       NOT = 'PDA110'
               PERFORM  P01700-1ST-TIME-PROCESS
                   THRU P01700-1ST-TIME-PROCESS-EXIT
           ELSE
               PERFORM  P02000-PROCESS-TRANS
                   THRU P02000-PROCESS-TRANS-EXIT.


       P01500-MAIN-PROCESS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01700-1ST-TIME-PROCESS                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL PROGRAM INITIAL INQUIRY     *
      *                PROCESSES                                      *
      *                                                               *
      *    CALLED BY:  P01500-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P01700-1ST-TIME-PROCESS.

           MOVE LOW-VALUES             TO CIOM-THE-REST.
           MOVE SPACES                 TO PDA110-ORDERNBR
                                          PDA110-ACTION.
           MOVE PDA110-PREV-PGRMID     TO PDA110-ORIGINATING-PGRMID.


      *****************************************************************
      *    IF THE PREVIOUS PROGRAM WAS VIEW ORDER ITEMS (PDA111), OR  *
      *    BROWSE SUBMITTED ORDERS (PDA112), CHECK FOR ORDER NUMBER   *
      *    IN THE SCREEN HIDDEN AREA                                  *
      *                                                               *
      *****************************************************************

           IF (PDA110-PREV-PGRMID      = 'PDA111' OR 'PDA112')  AND
              (PDA110-SAVAREA-ORDERNBR > SPACES)
               MOVE PDA110-SAVAREA-ORDERNBR
                                       TO PDA110-ORDERNBR
               MOVE SPACES             TO PDA110-SAVAREA-ORDERNBR
               PERFORM  P06000-INQUIRY-PROCESS
                   THRU P06000-INQUIRY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    DISPLAY THE INITIAL SCREEN, SAVE CONTROL VARIABLES         *
      *****************************************************************

           PERFORM  P80000-INSERT-MSG
               THRU P80000-INSERT-MSG-EXIT.

           MOVE 'PDA110'               TO  PDA110-PREV-PGRMID.

       P01700-1ST-TIME-PROCESS-EXIT.
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

           MOVE 'PDA110'               TO  PDA110-PREV-PGRMID.

      *****************************************************************
      *    CONVERT UNDERSCORES AND LOW-VALUES TO SPACES               *
      *****************************************************************

           MOVE SPACES                 TO  PDA110-SCREEN-MESSAGE.

           PERFORM  P02100-CONVERT-FIELDS
               THRU P02100-CONVERT-FIELDS-EXIT.


      *****************************************************************
      *    PROCEED WITH THE TRANSACTION EDIT PROCESSES                *
      *****************************************************************

           PERFORM  P03000-EDIT-PROCESS
               THRU P03000-EDIT-PROCESS-EXIT.


      *****************************************************************
      *    DISPLAY THE OUTPUT SCREEN IF ENTER KEY, OR ERROR FOUND     *
      *****************************************************************

           IF (PDA110-PFKEY  =  'EN')  OR
              (ERROR-FOUND)
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

           INSPECT PDA110-ORDERNBR
               CONVERTING  WMF-UNDERSCORE-LOWVALUE-R TO SPACES.

           INSPECT PDA110-ACTION
               CONVERTING  WMF-UNDERSCORE-LOWVALUE-R TO SPACES.

           INSPECT PDA110-ORDERDATEMM
               CONVERTING  WMF-UNDERSCORE-LOWVALUE-R TO SPACES.

           INSPECT PDA110-ORDERDATEDD
               CONVERTING  WMF-UNDERSCORE-LOWVALUE-R TO SPACES.

           INSPECT PDA110-ORDERDATEYY
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
      *    VALID KEYS ARE: ENTER, PF4, PF5, PF11, PF12                *
      *****************************************************************

           IF PDA110-PFKEY = 'EN' OR '04' OR '05' OR '11' OR '12'
               NEXT SENTENCE
           ELSE
               MOVE PM001-INVALID-PFKEY
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P03000-EDIT-PROCESS-EXIT.


      *****************************************************************
      *    DETERMINE IF ACTION CODE HAS BEEN ENTERED                  *
      *    (ACTION CODE AND PFKEYS ARE MUTUALLY EXCLUSIVE)            *
      *****************************************************************

           IF PDA110-PFKEY             =  'EN'
               NEXT SENTENCE
           ELSE
           IF PDA110-ACTION            >  SPACES
               MOVE LOW-VALUES         TO  PDA110-ACTION-ATTR
               MOVE WS-CURSOR-ATTR     TO  PDA110-ACTION-ATTR1
               MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA110-ACTION-ATTR2
               MOVE PM003-ACTION-VS-PFKEY-CONFLICT
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P03000-EDIT-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    EDIT THE SCREEN KEY FIELDS ACTION CODE, ORDER NUMBER       *
      *****************************************************************

           IF PDA110-PFKEY  =  'EN' OR '04'
               PERFORM  P03200-EDIT-KEY-FIELDS
                   THRU P03200-EDIT-KEY-FIELDS-EXIT
           ELSE
               NEXT SENTENCE.

           IF ERROR-FOUND
               GO TO P03000-EDIT-PROCESS-EXIT.


      *****************************************************************
      *    IF REQUEST IS PF4 (VIEW ORDER ITEMS) OR A CHANGE OR DELETE  *
      *    REQUEST, AN INQUIRY MUST HAVE BEEN PERFORMED FIRST         *
      *****************************************************************

           IF (PDA110-PFKEY  =  '04')      OR
              (PDA110-PFKEY  =  'EN'  AND  PDA110-ACTION > SPACES)
               IF PDA110-ORDERNBR   =  PDA110-SAVAREA-ORDERNBR
                   NEXT SENTENCE
               ELSE
                   MOVE PM022-INQUIRY-REQUIRED
                                       TO  WMF-MESSAGE-AREA
                   PERFORM  P70000-ERROR-ROUTINE
                       THRU P70000-ERROR-ROUTINE-EXIT
                   IF PDA110-ACTION    >   SPACES
                       MOVE LOW-VALUES TO  PDA110-ACTION-ATTR
                       MOVE WS-CURSOR-ATTR
                                       TO  PDA110-ACTION-ATTR1
                       MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA110-ACTION-ATTR2
                       GO TO P03000-EDIT-PROCESS-EXIT
                   ELSE
                       GO TO P03000-EDIT-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF ENTER KEY, PROCESS SCREEN EDITS, OTHERWISE PROCESS PFKEY*
      *****************************************************************

           IF PDA110-PFKEY  =  'EN'
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
      *    PARAGRAPH:  P03200-EDIT-KEY-FIELDS                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO EDIT THE SCREEN KEY FIELDS ACTION   *
      *                CODE, ORDER NUMBER                             *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************

       P03200-EDIT-KEY-FIELDS.

      *****************************************************************
      *    ORDER NUMBER MUST BE NUMERIC, AND GREATER THAN ZERO        *
      *****************************************************************

           MOVE +10                    TO WMF-NUM-LTH.
           MOVE PDA110-ORDERNBR        TO WMF-NUM-INPUT.

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.


           IF WMF-NUM-ERROR            =  ZEROES    AND
              PDA110-ORDERNBR          >  SPACES
               MOVE WMF-NUM-OUTPUT     TO WMF-ORDER-NUMBER
               MOVE WMF-ORDER-NUMBER   TO PDA110-ORDERNBR.


           IF WMF-NUM-ERROR            >  ZEROES    OR
              WMF-NUM-OUTPUT           =  ZEROES
               MOVE SPACES             TO PDA110-SAVAREA-ORDERNBR
               PERFORM  P79200-CLEAR-SCREEN
                   THRU P79200-CLEAR-SCREEN-EXIT
               MOVE LOW-VALUES         TO PDA110-ORDERNBR-ATTR
               MOVE WS-CURSOR-ATTR     TO PDA110-ORDERNBR-ATTR1
               MOVE WS-HI-INTENSITY-ATTR
                                       TO PDA110-ORDERNBR-ATTR2
               MOVE PM021-INVALID-ORDER-NUMBER
                                       TO WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    ACTION CODE MUST BE SPACE (INQUIRY), -C- (CHANGE) OR       *
      *    -D- (DELETE)                                               *
      *****************************************************************

           MOVE PDA110-ACTION          TO WS-ACTION-CODE-SW.

           IF ACTION-IS-VALID
               NEXT SENTENCE
           ELSE
               MOVE LOW-VALUES         TO  PDA110-ACTION-ATTR
               MOVE WS-CURSOR-ATTR     TO  PDA110-ACTION-ATTR1
               MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA110-ACTION-ATTR2
               MOVE PM020-INVALID-ACTION-CODE
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT.


       P03200-EDIT-KEY-FIELDS-EXIT.
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
      *    PF4 (VIEW ORDER ITEMS), TRANSFER TO PROGRAM PDA111 PASSING *
      *    THE USER ENTERED ORDER NUMBER IN THE SCREEN HIDDEN SAVEAREA*
      *****************************************************************

           IF  PDA110-PFKEY = '04'
               MOVE PDA110-ORDERNBR    TO PDA110-SAVAREA-ORDERNBR
               MOVE 'PDA11101'     TO  WMF-NEXT-TRANID
               PERFORM  P80300-XFER-CONTROL
                   THRU P80300-XFER-CONTROL-EXIT
               GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    PF5 (BROWSE SUBMITTED ORDERS) TRANSFER TO PDA112 PASSING   *
      *****************************************************************

           IF  PDA110-PFKEY = '05'
               MOVE 'PDA11201'     TO  WMF-NEXT-TRANID
               PERFORM  P80300-XFER-CONTROL
                   THRU P80300-XFER-CONTROL-EXIT
               GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF PF11, TRANSFER TO THE ORDER MENU                        *
      *****************************************************************

           IF PDA110-PFKEY  =  '11'
               MOVE 'PDA10201'     TO  WMF-NEXT-TRANID
               PERFORM  P80300-XFER-CONTROL
                   THRU P80300-XFER-CONTROL-EXIT
               GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF PF12, RETURN TO THE MAIN MENU                           *
      *****************************************************************

           IF PDA110-PFKEY  =  '12'
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
      *    FUNCTION :  ROUTINE TO CONTROL THE ORDER INQUIRY / UPDATE  *
      *                PROCESSING BASED ON THE ACTION CODE ENTERED    *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************

       P05000-PROCESS-SCREEN.

      *****************************************************************
      *    IF NO ACTION CODE, PROCESS INQUIRY                         *
      *****************************************************************

           IF ACTION-IS-INQUIRY
               PERFORM  P06000-INQUIRY-PROCESS
                   THRU P06000-INQUIRY-PROCESS-EXIT
           ELSE

      *****************************************************************
      *    OTHERWISE PERFORM THE UPDATE PROCESS (CHANGE, DELETE)      *
      *****************************************************************

               PERFORM  P07000-UPDATE-PROCESS
                   THRU P07000-UPDATE-PROCESS-EXIT.

       P05000-PROCESS-SCREEN-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06000-INQUIRY-PROCESS                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PERFORM THE INITIAL INQUIRY         *
      *                PROCESSES                                      *
      *                                                               *
      *    CALLED BY:  P01700-1ST-TIME-PROCESS                        *
      *                P05000-PROCESS-SCREEN                          *
      *                                                               *
      *****************************************************************

       P06000-INQUIRY-PROCESS.


      *****************************************************************
      *    INITIALIZE / CLEAR SCREEN AREA                             *
      *****************************************************************

           MOVE SPACES                 TO PDA110-SAVAREA-ORDERNBR.

           PERFORM  P79200-CLEAR-SCREEN
               THRU P79200-CLEAR-SCREEN-EXIT.


      *****************************************************************
      *    PERFORM THE ORDER QUERY PROCESS                            *
      *****************************************************************

           PERFORM  P06100-ORDER-PROCESS
               THRU P06100-ORDER-PROCESS-EXIT.


      *****************************************************************
      *    IF SUCCESSFUL INQUIRY, SAVE ORDER NBR IN SCREEN HIDDEN AREA*
      *****************************************************************

           IF NO-ERROR-FOUND
               MOVE PDA110-ORDERNBR    TO  PDA110-SAVAREA-ORDERNBR
               MOVE PM038-INQUIRY-COMPLETE
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70100-INFO-MSG-ROUTINE
                   THRU P70100-INFO-MSG-ROUTINE-EXIT.

       P06000-INQUIRY-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06100-ORDER-PROCESS                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO OBTAIN AND FORMAT ORDER INFORMATION *
      *                FOR THE SELECTED ORDER NUMBER                  *
      *                                                               *
      *    CALLED BY:  P06000-INQUIRY-PROCESS                         *
      *                                                               *
      *****************************************************************

       P06100-ORDER-PROCESS.

      *****************************************************************
      *    ATTEMPT ORDER ROOT SEG RETRIEVAL, IF NOT FOUND -- ERROR    *
      *****************************************************************

           MOVE 'EQ'                   TO ORDER-QUAL-OPERATOR.
           MOVE PDA110-USERID-NUMBER   TO ORDER-QUAL-PREFIX.
           MOVE PDA110-ORDERNBR        TO ORDER-QUAL-NUMBER.

           PERFORM  P79000-GHU-ORDER
               THRU P79000-GHU-ORDER-EXIT.

           IF OP-STATUS            NOT =  SPACES
               MOVE LOW-VALUES         TO  PDA110-ORDERNBR-ATTR
               MOVE WS-CURSOR-ATTR     TO  PDA110-ORDERNBR-ATTR1
               MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA110-ORDERNBR-ATTR2
               MOVE PM023-ORDER-NOT-FOUND
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P06100-ORDER-PROCESS-EXIT.


      *****************************************************************
      *    LOCATE / FORMAT THE CUSTOMER RELATED INFORMATION           *
      *****************************************************************

           MOVE 'N'                    TO WS-CUSTOMER-FOUND-SW.
           MOVE ORDER-CUSTOMER-ID      TO WMF-CUSTOMER-ID.

           PERFORM  P20100-FIND-CUSTOMER
               THRU P20100-FIND-CUSTOMER-EXIT
                   VARYING WS-SUB1 FROM +1 BY +1
                       UNTIL WS-SUB1 > +13.

           IF CUSTOMER-FOUND
               NEXT SENTENCE
           ELSE
               MOVE PM008-CUST-NOT-FOUND
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P06100-ORDER-PROCESS-EXIT.


      *****************************************************************
      *    OBTAIN THE PURCHASE TYPE DESCRIPTION                       *
      *****************************************************************

           MOVE ORDER-PURCHASE-TYPE-PREFIX
                                       TO PURCHASE-TYPE-PREFIX.
           MOVE ORDER-PURCHASE-TYPE    TO PURCHASE-TYPE-TYPE
                                          PDA110-PURTYPE.

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
                                       TO  PDA110-PURDESC

               WHEN SQLCODE = +100
                   MOVE SPACES         TO  PDA110-PURDESC
                   MOVE PM028-INVALID-PURCHASE-TYPE
                                       TO  WMF-MESSAGE-AREA
                   PERFORM  P70000-ERROR-ROUTINE
                       THRU P70000-ERROR-ROUTINE-EXIT
                   GO TO P06100-ORDER-PROCESS-EXIT

               WHEN OTHER
                   MOVE 'DB2'          TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA110'       TO WPDE-PROGRAM-ID
                   MOVE SQLCODE        TO WPDE-DB2-SQLCODE
                   MOVE 'SELECT PURCHASE_TYPE'
                                       TO WPDE-FUNCTION
                   MOVE 'P06100'       TO WPDE-PARAGRAPH
                   PERFORM  P99500-PDA-ERROR
                       THRU P99500-PDA-ERROR-EXIT

           END-EVALUATE.


      *****************************************************************
      *    IF NO ERRORS ------- FORMAT REMAINING DATA TO SCREEN       *
      *****************************************************************

           PERFORM  P06200-FORMAT-SCREEN
               THRU P06200-FORMAT-SCREEN-EXIT.


       P06100-ORDER-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06200-FORMAT-SCREEN                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO FORMAT ORDER INFORMATION TO THE     *
      *                SCREEN I/O AREA                                *
      *                                                               *
      *    CALLED BY:  P06100-ORDER-PROCESS                           *
      *                                                               *
      *****************************************************************

       P06200-FORMAT-SCREEN.

           MOVE ORDER-TOTAL-AMOUNT     TO PDA110-TOTALCOST-R.
           MOVE ORDER-PURCHASE-NUMBER  TO PDA110-PURNBR-R.

           MOVE ORDER-DATE-YYMMDD      TO WMF-DATE-YYMMDD.
           MOVE WMF-DATE-MM            TO PDA110-ORDERDATEMM.
           MOVE WMF-DATE-DD            TO PDA110-ORDERDATEDD.
           MOVE WMF-DATE-YY            TO PDA110-ORDERDATEYY.


      *****************************************************************
      *    DETERMINR ORDER AGE                                        *
      *****************************************************************

           PERFORM  P20000-CHECK-ORDER-AGE
               THRU P20000-CHECK-ORDER-AGE-EXIT.

       P06200-FORMAT-SCREEN-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07000-UPDATE-PROCESS                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE ORDER UPDATE            *
      *                PROCESSING BASED ON THE ACTION CODE ENTERED    *
      *                (CHANGE, DELETE PROCESSES)                     *
      *                                                               *
      *    CALLED BY:  P05000-PROCESS-SCREEN                          *
      *                                                               *
      *****************************************************************

       P07000-UPDATE-PROCESS.

      *****************************************************************
      *    PERFORM SCREEN FIELD EDITS FOR A CHANGE OPERATION          *
      *****************************************************************

           IF ACTION-IS-CHANGE
               PERFORM  P07100-EDIT-FIELDS
                   THRU P07100-EDIT-FIELDS-EXIT
           ELSE
               NEXT SENTENCE.

           IF ERROR-FOUND
               GO TO P07000-UPDATE-PROCESS-EXIT.


      *****************************************************************
      *    PERFORM EITHER CHANGE OR DELETE OPERATIONS                 *
      *****************************************************************

           IF ACTION-IS-CHANGE
               PERFORM  P08000-CHANGE-PROCESS
                   THRU P08000-CHANGE-PROCESS-EXIT
           ELSE
               PERFORM  P09000-DELETE-PROCESS
                   THRU P09000-DELETE-PROCESS-EXIT.

       P07000-UPDATE-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07100-EDIT-FIELDS                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE SCREEN FIELD            *
      *                EDIT PROCESS                                   *
      *                                                               *
      *    CALLED BY:  P07000-UPDATE-PROCESS                          *
      *                                                               *
      *****************************************************************

       P07100-EDIT-FIELDS.


           PERFORM  P07130-EDIT-ORDER-DATE-MM
               THRU P07130-EDIT-ORDER-DATE-MM-EXIT.


           PERFORM  P07160-EDIT-ORDER-DATE-DD
               THRU P07160-EDIT-ORDER-DATE-DD-EXIT.


           PERFORM  P07190-EDIT-ORDER-DATE-YY
               THRU P07190-EDIT-ORDER-DATE-YY-EXIT.


           IF ERROR-FOUND
               GO TO P07100-EDIT-FIELDS-EXIT.


      *****************************************************************
      *    IF NO ERRORS ENCOUNTERED, PERFORM FIELD CROSS EDITS        *
      *****************************************************************

           PERFORM  P07500-EDIT-ORDER-DATE-DD
               THRU P07500-EDIT-ORDER-DATE-DD-EXIT.


       P07100-EDIT-FIELDS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07130-EDIT-ORDER-DATE-MM                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO EDIT THE MONTH PORTION OF THE       *
      *                ORDER DATE                                     *
      *                                                               *
      *    CALLED BY:  P07100-EDIT-FIELDS                             *
      *                                                               *
      *****************************************************************

       P07130-EDIT-ORDER-DATE-MM.

      *****************************************************************
      *    EDIT THE ORDER DATE MONTH (VALUES NUMERIC, 01 - 12)        *
      *****************************************************************

           MOVE +2                     TO WMF-NUM-LTH.
           MOVE PDA110-ORDERDATEMM     TO WMF-NUM-INPUT.

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.


           IF WMF-NUM-ERROR            =  ZEROES    AND
              PDA110-ORDERDATEMM       >  SPACES
               MOVE WMF-NUM-OUTPUT     TO WMF-ORDER-DATE-MM
               MOVE WMF-ORDER-DATE-MM  TO PDA110-ORDERDATEMM.


           IF (WMF-NUM-ERROR            >  ZEROES)    OR
              (WMF-NUM-OUTPUT           <  01         OR
               WMF-NUM-OUTPUT           >  12)
               MOVE LOW-VALUES         TO  PDA110-ORDERDATEMM-ATTR
               MOVE WS-CURSOR-ATTR     TO  PDA110-ORDERDATEMM-ATTR1
               MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA110-ORDERDATEMM-ATTR2
               MOVE PM039-INVALID-DATE-MONTH
                                       TO WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
           ELSE
               NEXT SENTENCE.

       P07130-EDIT-ORDER-DATE-MM-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07160-EDIT-ORDER-DATE-DD                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO EDIT THE DAY PORTION OF THE         *
      *                ORDER DATE                                     *
      *                                                               *
      *    CALLED BY:  P07100-EDIT-FIELDS                             *
      *                                                               *
      *****************************************************************

       P07160-EDIT-ORDER-DATE-DD.

      *****************************************************************
      *    EDIT THE ORDER DATE DAY   (VALUES NUMERIC, 01 - 31)        *
      *****************************************************************

           MOVE +2                     TO WMF-NUM-LTH.
           MOVE PDA110-ORDERDATEDD     TO WMF-NUM-INPUT.

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.


           IF WMF-NUM-ERROR            =  ZEROES    AND
              PDA110-ORDERDATEDD       >  SPACES
               MOVE WMF-NUM-OUTPUT     TO WMF-ORDER-DATE-DD
               MOVE WMF-ORDER-DATE-DD  TO PDA110-ORDERDATEDD.


           IF (WMF-NUM-ERROR            >  ZEROES)    OR
              (WMF-NUM-OUTPUT           <  01         OR
               WMF-NUM-OUTPUT           >  31)
               MOVE LOW-VALUES         TO  PDA110-ORDERDATEDD-ATTR
               MOVE WS-CURSOR-ATTR     TO  PDA110-ORDERDATEDD-ATTR1
               MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA110-ORDERDATEDD-ATTR2
               MOVE PM040-INVALID-DATE-DAY
                                       TO WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
           ELSE
               NEXT SENTENCE.


       P07160-EDIT-ORDER-DATE-DD-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07190-EDIT-ORDER-DATE-YY                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO EDIT THE YEAR PORTION OF THE        *
      *                ORDER DATE                                     *
      *                                                               *
      *    CALLED BY:  P07100-EDIT-FIELDS                             *
      *                                                               *
      *****************************************************************

       P07190-EDIT-ORDER-DATE-YY.

      *****************************************************************
      *    EDIT THE ORDER DATE YEAR  (VALUES NUMERIC)                 *
      *****************************************************************

           MOVE +2                     TO WMF-NUM-LTH.
           MOVE PDA110-ORDERDATEYY     TO WMF-NUM-INPUT.

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.


           IF WMF-NUM-ERROR            =  ZEROES
               MOVE WMF-NUM-OUTPUT     TO WMF-ORDER-DATE-YY
               MOVE WMF-ORDER-DATE-YY  TO PDA110-ORDERDATEYY.


           IF  WMF-NUM-ERROR           >  ZEROES
               MOVE LOW-VALUES         TO  PDA110-ORDERDATEYY-ATTR
               MOVE WS-CURSOR-ATTR     TO  PDA110-ORDERDATEYY-ATTR1
               MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA110-ORDERDATEYY-ATTR2
               MOVE PM041-INVALID-DATE-YEAR
                                       TO WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
           ELSE
               NEXT SENTENCE.


       P07190-EDIT-ORDER-DATE-YY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07500-EDIT-ORDER-DATE-DD                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO EDIT THE ORDER DATE DAY VERSUS      *
      *                THE ORDER DATE MONTH                           *
      *                                                               *
      *    CALLED BY:  P07100-EDIT-FIELDS                             *
      *                                                               *
      *****************************************************************

       P07500-EDIT-ORDER-DATE-DD.

      *****************************************************************
      *    NUMBER OF DAYS MAY NOT EXCEED THE DAYS OF A GIVEN MONTH    *
      *****************************************************************

           IF WMF-ORDER-DATE-DD  > WMF-DAYS-IN-MONTH-R
                                               (WMF-ORDER-DATE-MM)
               MOVE LOW-VALUES         TO  PDA110-ORDERDATEDD-ATTR
               MOVE WS-CURSOR-ATTR     TO  PDA110-ORDERDATEDD-ATTR1
               MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA110-ORDERDATEDD-ATTR2
               MOVE PM042-DATE-DAYS-EXCEED-MAX
                                       TO WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT.


       P07500-EDIT-ORDER-DATE-DD-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P08000-CHANGE-PROCESS                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE ORDER CHANGE            *
      *                PROCESSING.                                    *
      *                                                               *
      *    CALLED BY:  P07000-UPDATE-PROCESS                          *
      *                                                               *
      *****************************************************************

       P08000-CHANGE-PROCESS.

      *****************************************************************
      *    RETRIEVE THE ORDER ROOT, NOT FOUND IS AN ERROR             *
      *****************************************************************

           MOVE 'EQ'                   TO ORDER-QUAL-OPERATOR.
           MOVE PDA110-USERID-NUMBER   TO ORDER-QUAL-PREFIX.
           MOVE PDA110-ORDERNBR        TO ORDER-QUAL-NUMBER.

           PERFORM  P79000-GHU-ORDER
               THRU P79000-GHU-ORDER-EXIT.

           IF OP-STATUS            NOT =  SPACES
               MOVE LOW-VALUES         TO  PDA110-ORDERNBR-ATTR
               MOVE WS-CURSOR-ATTR     TO  PDA110-ORDERNBR-ATTR1
               MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA110-ORDERNBR-ATTR2
               MOVE PM023-ORDER-NOT-FOUND
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P08000-CHANGE-PROCESS-EXIT.


      *****************************************************************
      *    FORMAT DATA INTO SEGMENT I/O AREA, UPDATE SEGMENT          *
      *****************************************************************

           MOVE WMF-ORDER-DATE         TO  ORDER-DATE-YYMMDD.

           PERFORM  P79090-REPL-ORDER
               THRU P79090-REPL-ORDER-EXIT.

           PERFORM P20000-CHECK-ORDER-AGE
              THRU P20000-CHECK-ORDER-AGE-EXIT.

      *****************************************************************
      *    FORMAT COMPLETION MESSAGE, CLEAR ACTION CODE               *
      *****************************************************************

           MOVE SPACES                 TO  PDA110-ACTION.
           MOVE PM043-UPDATE-COMPLETE  TO  WMF-MESSAGE-AREA.

           PERFORM  P70100-INFO-MSG-ROUTINE
               THRU P70100-INFO-MSG-ROUTINE-EXIT.

       P08000-CHANGE-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P09000-DELETE-PROCESS                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE ORDER DELETE            *
      *                PROCESSING.                                    *
      *                                                               *
      *    CALLED BY:  P07000-UPDATE-PROCESS                          *
      *                                                               *
      *****************************************************************

       P09000-DELETE-PROCESS.

      *****************************************************************
      *    RETRIEVE THE ORDER ROOT, NOT FOUND IS AN ERROR             *
      *****************************************************************

           MOVE 'EQ'                   TO ORDER-QUAL-OPERATOR.
           MOVE PDA110-USERID-NUMBER   TO ORDER-QUAL-PREFIX.
           MOVE PDA110-ORDERNBR        TO ORDER-QUAL-NUMBER.

           PERFORM  P79000-GHU-ORDER
               THRU P79000-GHU-ORDER-EXIT.

           IF OP-STATUS            NOT =  SPACES
               MOVE LOW-VALUES         TO  PDA110-ORDERNBR-ATTR
               MOVE WS-CURSOR-ATTR     TO  PDA110-ORDERNBR-ATTR1
               MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA110-ORDERNBR-ATTR2
               MOVE PM023-ORDER-NOT-FOUND
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P09000-DELETE-PROCESS-EXIT.


      *****************************************************************
      *    PHYSICALLY DELETE THE ORDER SEGMENT                        *
      *****************************************************************

           PERFORM  P79100-DLET-ORDER
               THRU P79100-DLET-ORDER-EXIT.


      *****************************************************************
      *    FORMAT COMPLETION MESSAGE, CLEAR SCREEN DETAIL, ACTION     *
      *****************************************************************

           MOVE SPACES                 TO  PDA110-ACTION
                                           PDA110-SAVAREA-ORDERNBR.

           PERFORM  P79200-CLEAR-SCREEN
               THRU P79200-CLEAR-SCREEN-EXIT.

           MOVE PM044-ORDER-DELETED    TO  WMF-MESSAGE-AREA.

           PERFORM  P70100-INFO-MSG-ROUTINE
               THRU P70100-INFO-MSG-ROUTINE-EXIT.

       P09000-DELETE-PROCESS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P20000-CHECK-ORDER-AGE                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DETERMINE IF AN ORDER IS OVERDUE    *
      *                                                               *
      *    CALLED BY:  P06200-FORMAT-SCREEN                           *
      *                                                               *
      *****************************************************************

       P20000-CHECK-ORDER-AGE.

           MOVE SPACES                 TO PDAS01-PARMS.
           MOVE PDA110-ORDERNBR        TO PDAS01-ORDER-NUMBER.
           MOVE PDA110-ORDERDATEYY     TO PDAS01-OD-YR.
           MOVE PDA110-ORDERDATEMM     TO PDAS01-OD-MONTH.
           MOVE PDA110-ORDERDATEDD     TO PDAS01-OD-DAY.

           IF PDAS01-OD-YR             > 50
               MOVE 19                 TO PDAS01-OD-CE
           ELSE
               MOVE 20                 TO PDAS01-OD-CE
           END-IF.

           MOVE ZEROES                 TO PDAS01-ORDER-COUNT
                                          PDAS01-ORDER-DOLLAR-AMT.

           CALL 'PDAS01'               USING PDAS01-PARMS

           MOVE PDAS01-AGE-DAYS        TO PDAS03-AGE-DAYS.

           CALL 'PDAS03'               USING PDAS03-PARMS.

           MOVE PDAS03-MESSAGE         TO PDA110-ORDERSTATUS.

       P20000-CHECK-ORDER-AGE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P20100-FIND-CUSTOMER                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO SEARCH THE CUSTOMER ID ARRAY FOR    *
      *                THE FIRST CUSTOMER ID EQUAL TO THE HIDDEN      *
      *                CUSTOMER ID. IF ENTRY FOUND                    *
      *                FORMAT SCREEN WITH CUSTOMER INFO.              *
      *                                                               *
      *    CALLED BY:  P01830-INQUIRY-PROCESS                         *
      *                                                               *
      *****************************************************************

       P20100-FIND-CUSTOMER.

      *****************************************************************
      *    IF CUSTOMER FOUND ---- FORMAT CUSTOMER INFO                *
      *****************************************************************

           IF WCAR-CUSTOMER-ID (WS-SUB1)  =  WMF-CUSTOMER-ID
               MOVE 'Y'                TO WS-CUSTOMER-FOUND-SW
               MOVE WCAR-CUSTOMER-ID      (WS-SUB1)
                                       TO PDA110-CUS-ID
               MOVE WCAR-CUSTOMER-NAME    (WS-SUB1)
                                       TO PDA110-CUS-NAME
               MOVE WCAR-CUSTOMER-ADDRESS (WS-SUB1)
                                       TO PDA110-CUS-ADDR
               MOVE WCAR-CUSTOMER-CITY    (WS-SUB1)
                                       TO PDA110-CUS-CITY
               MOVE WCAR-CUSTOMER-STATE   (WS-SUB1)
                                       TO PDA110-CUS-STATE
               MOVE WCAR-CUSTOMER-ZIP     (WS-SUB1)
                                       TO PDA110-CUS-ZIP
               MOVE WCAR-CUSTOMER-EMAIL   (WS-SUB1)
                                       TO PDA110-CUS-EMAIL
               MOVE WCAR-SHIP-NAME        (WS-SUB1)
                                       TO PDA110-SHP-NAME
               MOVE WCAR-SHIP-ADDRESS     (WS-SUB1)
                                       TO PDA110-SHP-ADDR
               MOVE WCAR-SHIP-CITY        (WS-SUB1)
                                       TO PDA110-SHP-CITY
               MOVE WCAR-SHIP-STATE       (WS-SUB1)
                                       TO PDA110-SHP-STATE
               MOVE WCAR-SHIP-ZIP         (WS-SUB1)
                                       TO PDA110-SHP-ZIP
               MOVE +14                TO WS-SUB1
           ELSE
               NEXT SENTENCE.


       P20100-FIND-CUSTOMER-EXIT.
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

           IF PDA110-SCREEN-MESSAGE    >  SPACES
               NEXT SENTENCE
           ELSE
               MOVE WMF-MESSAGE-AREA   TO PDA110-SCREEN-MESSAGE.

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

           IF PDA110-SCREEN-MESSAGE    >  SPACES
               NEXT SENTENCE
           ELSE
               MOVE WMF-MESSAGE-AREA   TO PDA110-SCREEN-MESSAGE.

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
      *    PARAGRAPH:  P79000-GHU-ORDER                               *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE ORDER ROOT             *
      *                SEGMENT WITH HOLD                              *
      *                                                               *
      *    CALLED BY:  P06100-ORDER-PROCESS                           *
      *                P08000-CHANGE-PROCESS                          *
      *                P09000-DELETE-PROCESS                          *
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
               MOVE 'PDA110'           TO WPIE-PROGRAM-ID
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
      *    PARAGRAPH:  P79090-REPL-ORDER                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO REPLACE THE ORDER ROOT SEGMENT      *
      *                                                               *
      *    CALLED BY:  P08000-CHANGE-PROCESS                          *
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
               MOVE 'PDA110'           TO WPIE-PROGRAM-ID
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
      *    PARAGRAPH:  P79100-DLET-ORDER                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DELETE THE ORDER ROOT SEGMENT       *
      *                                                               *
      *    CALLED BY:  P09000-DELETE-PROCESS                          *
      *                                                               *
      *****************************************************************

       P79100-DLET-ORDER.


           CALL 'CBLTDLI'    USING     DLET
                                       ORDER-PCB
                                       ORDER-SEGMENT.


           IF OP-STATUS                =  SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA110'           TO WPIE-PROGRAM-ID
               MOVE OP-STATUS          TO WPIE-STATUS-CODE
               MOVE 'DLET'             TO WPIE-FUNCTION-CODE
               MOVE 'P79100'           TO WPIE-PARAGRAPH
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME
               MOVE 'ORDER2DB'         TO WPIE-DATABASE-NAME
               MOVE 'DLET ORDER ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P79100-DLET-ORDER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79200-CLEAR-SCREEN                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INITIALIZE THE SCREEN I/O AREA TO   *
      *                DEFAULT VALUES                                 *
      *                                                               *
      *    CALLED BY:  P06000-INQUIRY-PROCESS                         *
      *                P08000-DELETE-PROCESS                          *
      *                                                               *
      *****************************************************************

       P79200-CLEAR-SCREEN.

           MOVE SPACES                 TO PDA110-CUS-ID
                                          PDA110-CUS-NAME
                                          PDA110-CUS-ADDR
                                          PDA110-CUS-CITY
                                          PDA110-CUS-STATE
                                          PDA110-CUS-ZIP
                                          PDA110-CUS-EMAIL
                                          PDA110-SHP-NAME
                                          PDA110-SHP-ADDR
                                          PDA110-SHP-CITY
                                          PDA110-SHP-STATE
                                          PDA110-SHP-ZIP
                                          PDA110-ORDERDATEMM
                                          PDA110-ORDERDATEDD
                                          PDA110-ORDERDATEYY
                                          PDA110-ORDERSTATUS
                                          PDA110-TOTALCOST
                                          PDA110-PURTYPE
                                          PDA110-PURDESC
                                          PDA110-PURNBR
                                          PDA110-SCREEN-MESSAGE.


       P79200-CLEAR-SCREEN-EXIT.
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
      *                P01700-1ST-TIME-PROCESS                        *
      *                P02000-PROCESS-TRANS                           *
      *                                                               *
      *****************************************************************

       P80000-INSERT-MSG.

      *****************************************************************
      *    FORMAT OUTPUT MESSAGE FIELDS,                              *
      *    RESET ENTERABLE FIELDS BACK TO DEFAULT (IF NECESSARY)      *
      *****************************************************************

           MOVE 'PDA110'               TO PDA110-PREV-PGRMID.
           MOVE LENGTH OF PDA110-MESSAGE
                                       TO PDA110-MSG-LL.

           INSPECT PDA110-ORDERNBR
                   CONVERTING WMF-SPACES-LOWVALUE-R  TO '__'.

           INSPECT PDA110-ACTION
                   CONVERTING WMF-SPACES-LOWVALUE-R  TO '__'.

           INSPECT PDA110-ORDERDATEMM
                   CONVERTING WMF-SPACES-LOWVALUE-R  TO '__'.

           INSPECT PDA110-ORDERDATEDD
                   CONVERTING WMF-SPACES-LOWVALUE-R  TO '__'.

           INSPECT PDA110-ORDERDATEYY
                   CONVERTING WMF-SPACES-LOWVALUE-R  TO '__'.


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
               MOVE 'PDA110'           TO WPIE-PROGRAM-ID
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
               MOVE 'PDA110'           TO WPIE-PROGRAM-ID
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
           MOVE 'PDA110'               TO CIOM-PREV-PGRMID.


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
               MOVE 'PDA110'           TO WPIE-PROGRAM-ID
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