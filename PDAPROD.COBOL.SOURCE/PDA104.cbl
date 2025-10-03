       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA104.

      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA104 (IMS MPP PROGRAM)                          *
      * TRANS   :   PDA10401                                          *
      * MFS     :   FORMAT=PDA104, MID=PDA104I, MOD=PDA104O           *
      *                                                               *
      * FUNCTION:   PROGRAM PDA104 IS THE IMS/DC PRODUCT DEMONSTRATION*
      *             APPLICATION CUSTOMER IDENTIFICATION PROGRAM. THE  *
      *             SCREEN IS THE FIRST IN BOTH THE ORDER ADD AND     *
      *             PENDING ORDER PROCESSES. THE USER MUST ENTER A    *
      *             VALID CUSTOMER ID TO CONTINUE, ONCE ENTERED THE   *
      *             SCREEN IS POPULATED WITH CUSTOMER INFORMATION.    *
      *                                                               *
      *                                                               *
      * FILES   :   PENDO1DB     (IMS PENDING ORDER DB) (INPUT)       *
      *                                                               *
      *                                                               *
      * TRANSACTIONS GENERATED:                                       *
      *             PDA10501   BROWSE CATEGORIES                      *
      *             PDA10801   PENDING ORDER                          *
      *             PDA10201   ORDER MENU                             *
      *             PDA10101   MAIN MENU                              *
      *                                                               *
      *                                                               *
      * PFKEYS  :   PFKEY 03 - ORDER MENU (PREVIOUS FUNCTION)         *
      *             PFKEY 12 - MAIN MENU                              *
      *                                                               *
      *****************************************************************
      *             PROGRAM CHANGE LOG                                *
      *             -------------------                               *
      *                                                               *
      *  DATE       UPDATED BY            CHANGE DESCRIPTION          *
      *  --------   --------------------  --------------------------  *
      *  MM/DD/YY   XXXXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXX  *
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

      *****************************************************************
      *    SWITCHES                                                   *
      *****************************************************************
       01  WS-SWITCHES.

           05  WS-MORE-MSGS-SW         PIC X(01)             VALUE 'Y'.
               88  MORE-MSGS                                 VALUE 'Y'.
               88  NO-MORE-MSGS                              VALUE 'N'.

           05  WS-MENU-SELECTION-SW    PIC X(01)             VALUE ' '.
               88  SELECTION-IS-ADD-ORDER                    VALUE '1'.
               88  SELECTION-IS-PENDING-ORDER                VALUE '4'.

           05  WS-TRANS-INTENT-SW      PIC X(01)             VALUE 'I'.
               88  INQUIRY-TRANS                             VALUE 'I'.
               88  UPDATE-TRANS                              VALUE 'U'.

           05  WS-ERROR-FOUND-SW       PIC X(01)             VALUE 'N'.
               88  ERROR-FOUND                               VALUE 'Y'.
               88  NO-ERROR-FOUND                            VALUE 'N'.

           05  WS-ORDER-FOUND-SW       PIC X(01)             VALUE 'N'.
               88  ORDER-FOUND                               VALUE 'Y'.
               88  NO-ORDER-FOUND                            VALUE 'N'.

           05  WS-CUSTOMER-FOUND-SW    PIC X(01)             VALUE 'N'.
               88  CUSTOMER-FOUND                            VALUE 'Y'.
               88  NO-CUSTOMER-FOUND                         VALUE 'N'.

           EJECT
      *****************************************************************
      *    MISCELLANEOUS WORK FIELDS                                  *
      *****************************************************************
       01  WS-MISCELLANEOUS-FIELDS.

           05  WMF-MODNAME             PIC X(08)   VALUE 'PDA104O'.
           05  WMF-MODNAME-ERROR       PIC X(08)   VALUE 'PDAERRO'.
           05  WMF-MASTER-LTERM-NAME   PIC X(08)   VALUE 'SMASTER'.
           05  WMF-IO-PCB-LTERM-NAME   PIC X(08)   VALUE SPACES.
           05  WMF-NEXT-TRANID         PIC X(08)   VALUE SPACES.
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
      *  PDA CUSTOMER IDENTIFICATION  IN/OUT MESSAGE STORAGE AREA     *
      *  PREFIX: PDA104                                               *
      *****************************************************************

       01  PDA104-MESSAGE              REDEFINES CIOM-MESSAGE.
           05 PDA104-MSG-LL            PIC S9(04)      COMP.
           05 PDA104-MSG-ZZ            PIC X(02).
           05 PDA104-MSG-TRANCODE      PIC X(08).
           05 FILLER                   PIC X(01).
           05 PDA104-MSG-SOURCE        PIC X(03).
           05 FILLER                   PIC X(01).
           05 PDA104-PFKEY             PIC X(02).
           05 PDA104-MSG-USERID-INFO.
              10  PDA104-USERID-ID     PIC X(08).
              10  PDA104-USERID-NUMBER PIC 9(05).
           05 PDA104-PREV-PGRMID       PIC X(08).
           05 PDA104-SAVAREA           PIC X(79).
           05 PDA104-SAVAREA-R         REDEFINES PDA104-SAVAREA.
              10 PDA104-SAVAREA-ORDER-MENU-SEL
                                       PIC X(01).
              10 PDA104-SAVAREA-CUSID  PIC X(32).
           05 PDA104-CUSID-ATTR.
              10  PDA104-CUSID-ATTR1   PIC X(01).
              10  PDA104-CUSID-ATTR2   PIC X(01).
           05 PDA104-CUSID             PIC X(32).
           05 PDA104-CUSNAME           PIC X(64).
           05 PDA104-CUSADDR           PIC X(64).
           05 PDA104-CUSCITY           PIC X(32).
           05 PDA104-CUSSTATE          PIC X(32).
           05 PDA104-CUSZIP            PIC X(12).
           05 PDA104-CUSEMAIL          PIC X(64).
           05 PDA104-SCREEN-MESSAGE    PIC X(79).
           05 PDA104-SMESSAGE          PIC X(79).
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
           05  PENDORD-QUAL-RPAREN     PIC X(01)   VALUE ')'.
           EJECT

      *****************************************************************
      *    DB2  DEFINITIONS  ----- NONE                               *
      *****************************************************************

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
      *                CUSTOMER IDENTIFICATION SCREEN                 *
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

           MOVE SPACES                 TO WS-MENU-SELECTION-SW.
           MOVE 'I'                    TO WS-TRANS-INTENT-SW.
           MOVE 'N'                    TO WS-ERROR-FOUND-SW.
           MOVE 'N'                    TO WS-ORDER-FOUND-SW.
           MOVE 'N'                    TO WS-CUSTOMER-FOUND-SW.

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
               MOVE 'PDA104'           TO WPIE-PROGRAM-ID
               MOVE IO-PCB-STATUS      TO WPIE-STATUS-CODE
               MOVE 'GU'               TO WPIE-FUNCTION-CODE
               MOVE 'P01000'           TO WPIE-PARAGRAPH
               MOVE 'GU IO-PCB FOR IMS/DC MESSAGE'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    EDIT MESSAGE RELATED INFORMATION                           *
      *****************************************************************

           PERFORM  P03000-EDIT-PROCESS
               THRU P03000-EDIT-PROCESS-EXIT.


      *****************************************************************
      *    SEND MESSAGE TO SCREEN -- IF INQUIRY, OR ERROR             *
      *****************************************************************

           IF (INQUIRY-TRANS)  OR  (ERROR-FOUND)
               PERFORM  P80000-INSERT-MSG
                   THRU P80000-INSERT-MSG-EXIT.


       P01000-PROCESS-MSG-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE PROGRAM EDIT PROCESS    *
      *                                                               *
      *    CALLED BY:  P01000-PROCESS-MSG                             *
      *                                                               *
      *****************************************************************

       P03000-EDIT-PROCESS.

      *****************************************************************
      *    VERIFY TRANSACTION ENTRY INTO THIS PROGRAM WAS VIA A       *
      *    LEGITIMATE SOURCE AND FORMAT                               *
      *    (TRANCODE AND SOURCE OF MESSAGE FROM A VALID APPL SCREEN)  *
      *****************************************************************

           IF CIOM-MSG-TRANCODE        = 'PDA10401'    AND
              CIOM-MSG-SOURCE          = 'PDA'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA104'           TO WPIE-PROGRAM-ID
               MOVE 'P03000'           TO WPIE-PARAGRAPH
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
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P03000-EDIT-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    DETERMINE THE TRANSACTION INTENT -- INQUIRY / UPDATE       *
      *****************************************************************

           PERFORM  P03100-CHK-TRANS-INTENT
               THRU P03100-CHK-TRANS-INTENT-EXIT.


      *****************************************************************
      *    PERFORM EITHER THE INQUIRY OR UPDATE PROCESS               *
      *****************************************************************

           INSPECT PDA104-CUSID
               CONVERTING  WMF-UNDERSCORE-LOWVALUE-R TO SPACES.


           IF INQUIRY-TRANS
               PERFORM  P04000-INQUIRY-PROCESS
                   THRU P04000-INQUIRY-PROCESS-EXIT
           ELSE
               PERFORM  P05000-UPDATE-PROCESS
                   THRU P05000-UPDATE-PROCESS-EXIT.


       P03000-EDIT-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03100-CHK-TRANS-INTENT                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DETERMINE IF TRANSACTION IS         *
      *                INQUIRY (1ST ITERATION THRU) OR                *
      *                UPDATE (PREVIOUS TRANSACTION WAS PDA104)       *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************

       P03100-CHK-TRANS-INTENT.

      *****************************************************************
      *    INQUIRY TRANSACTION IF: PREVIOUS PROGRAM NOT PDA104        *
      *    OTHERWISE TRANSACTION IS AN UPDATE                         *
      *****************************************************************

           IF CIOM-PREV-PGRMID    NOT = 'PDA104'                        00020001
               MOVE 'I'               TO WS-TRANS-INTENT-SW             00020001
               MOVE LOW-VALUES        TO CIOM-THE-REST                  00020001
           ELSE
               MOVE 'U'               TO WS-TRANS-INTENT-SW             00020001
               GO TO P03100-CHK-TRANS-INTENT-EXIT.                      00020001


      *****************************************************************
      *    IF PREVIOUS PROGRAM IS ORDER MENU OR BROWSE CATEGORIES     *
      *    (PART OF THE ORDER ADD PROCESS) USE THE SAVED CUSTOMER ID  *
      *****************************************************************

           IF CIOM-PREV-PGRMID        = 'PDA102'  OR 'PDA105'           00020001
               MOVE PDA104-SAVAREA-CUSID                                00020001
                                      TO PDA104-CUSID.                  00020001


       P03100-CHK-TRANS-INTENT-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P04000-INQUIRY-PROCESS                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE CUSTOMER IDENTIFICATION *
      *                INQUIRY PROCESS                                *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************

       P04000-INQUIRY-PROCESS.

      *****************************************************************
      *    IF CUSTOMER PRESENT, SEARCH ARRAY, NOT FOUND IS AN ERROR   *
      *    OTHERWISE DISPLAY THE INITIALIZED SCREEN                   *
      *****************************************************************

           IF PDA104-CUSID        NOT  >   SPACES
               GO TO P04000-INQUIRY-PROCESS-EXIT.


           MOVE 'N'                    TO WS-CUSTOMER-FOUND-SW.
           PERFORM  P07100-FIND-CUSTOMER
               THRU P07100-FIND-CUSTOMER-EXIT
                   VARYING WS-SUB1 FROM +1 BY +1
                       UNTIL WS-SUB1 > +13.

           IF CUSTOMER-FOUND
               MOVE LOW-VALUES         TO  PDA104-CUSID-ATTR
               MOVE WS-CURSOR-ATTR     TO  PDA104-CUSID-ATTR1
               MOVE PM015-PROCEED      TO  WMF-MESSAGE-AREA
               PERFORM  P70100-INFO-MSG-ROUTINE
                   THRU P70100-INFO-MSG-ROUTINE-EXIT
           ELSE
               MOVE LOW-VALUES         TO  PDA104-CUSID-ATTR
               MOVE WS-CURSOR-ATTR     TO  PDA104-CUSID-ATTR1
               MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA104-CUSID-ATTR2
               MOVE PM008-CUST-NOT-FOUND
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P04000-INQUIRY-PROCESS-EXIT.


       P04000-INQUIRY-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05000-UPDATE-PROCESS                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL PROGRAM UPDATE LOGIC FLOW   *
      *                PROCESSING                                     *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************

       P05000-UPDATE-PROCESS.

      *****************************************************************
      *    PERFORM THE SCREEN EDIT PROCESS (PFKEY AND DATA VALIDATION)*
      *****************************************************************

           PERFORM  P05100-EDIT-SCREEN
               THRU P05100-EDIT-SCREEN-EXIT.

           IF ERROR-FOUND
               GO TO P05000-UPDATE-PROCESS-EXIT.

      *****************************************************************
      *    IF PFKEY 03, PASS CONTROL TO THE ORDER MENU                *
      *****************************************************************

           IF PDA104-PFKEY  =  '03'
               MOVE 'PDA10201'         TO  WMF-NEXT-TRANID
               PERFORM  P80300-XFER-CONTROL
                   THRU P80300-XFER-CONTROL-EXIT
               GO TO P05000-UPDATE-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF PFKEY 12, PASS CONTROL TO THE MAIN MENU                 *
      *****************************************************************

           IF PDA104-PFKEY  =  '12'
               MOVE 'PDA10101'         TO  WMF-NEXT-TRANID
               PERFORM  P80300-XFER-CONTROL
                   THRU P80300-XFER-CONTROL-EXIT
               GO TO P05000-UPDATE-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    ***** ENTER KEY *****                                      *
      *    IF 1ST TIME THRU FOR THE ENTERED CUSTOMER ID, ASK FOR      *
      *    USER CONFIRMATION, TREAT TRANS AS AN -INQUIRY- TRANS, EXIT *
      *****************************************************************

           IF PDA104-CUSID             =   PDA104-SAVAREA-CUSID
               NEXT SENTENCE
           ELSE
               MOVE 'I'                TO  WS-TRANS-INTENT-SW
               MOVE LOW-VALUES         TO  PDA104-CUSID-ATTR
               MOVE WS-CURSOR-ATTR     TO  PDA104-CUSID-ATTR1
               MOVE PM015-PROCEED      TO  WMF-MESSAGE-AREA
               PERFORM  P70100-INFO-MSG-ROUTINE
                   THRU P70100-INFO-MSG-ROUTINE-EXIT
               GO TO P05000-UPDATE-PROCESS-EXIT.


      *****************************************************************
      *    ***** ENTER KEY USAGE *****                                *
      *    OTHERWISE PASS CONTROL TO EITHER THE ORDER ADD PROCESS OR  *
      *    THE PENDING ORDER PROCESS DEPENDING ON THE PASSED ORDER    *
      *    MENU SELECTION                                             *
      *****************************************************************

           MOVE PDA104-SAVAREA-ORDER-MENU-SEL
                                       TO  WS-MENU-SELECTION-SW.

           IF SELECTION-IS-ADD-ORDER
               MOVE 'PDA10501'         TO  WMF-NEXT-TRANID
               PERFORM  P80300-XFER-CONTROL
                   THRU P80300-XFER-CONTROL-EXIT
               GO TO P05000-UPDATE-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    CHECK THE PENDING ORDER DATABASE, AT LEAST 1 PENDING ORDER *
      *    MUST EXIST TO ALLOW TRANSFER TO THE PENDING ORDER SCREEN   *
      *****************************************************************

           PERFORM  P05500-CHECK-PENDORD
               THRU P05500-CHECK-PENDORD-EXIT.


           IF ORDER-FOUND
               MOVE 'PDA10801'         TO  WMF-NEXT-TRANID
               PERFORM  P80300-XFER-CONTROL
                   THRU P80300-XFER-CONTROL-EXIT
               GO TO P05000-UPDATE-PROCESS-EXIT
           ELSE
               MOVE 'I'                TO  WS-TRANS-INTENT-SW
               MOVE LOW-VALUES         TO  PDA104-CUSID-ATTR
               MOVE WS-CURSOR-ATTR     TO  PDA104-CUSID-ATTR1
               MOVE PM032-NO-PENDING-ORDER
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70100-INFO-MSG-ROUTINE
                   THRU P70100-INFO-MSG-ROUTINE-EXIT
               GO TO P05000-UPDATE-PROCESS-EXIT.


       P05000-UPDATE-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05100-EDIT-SCREEN                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE SCREEN EDIT PROCESS     *
      *                                                               *
      *    CALLED BY:  P05000-UPDATE-PROCESS                          *
      *                                                               *
      *****************************************************************

       P05100-EDIT-SCREEN.

      *****************************************************************
      *    EDIT THE OPERATOR PROGRAM FUNCTION KEY SELECTION (PFKEY)   *
      *****************************************************************

           PERFORM  P05200-EDIT-PFKEY
               THRU P05200-EDIT-PFKEY-EXIT.


      *****************************************************************
      *    EDIT THE CUSTOMER ID (IF ENTER KEY USED)                   *
      *****************************************************************

           IF PDA104-PFKEY             = 'EN'
               PERFORM  P05300-EDIT-CUSID
                   THRU P05300-EDIT-CUSID-EXIT.


       P05100-EDIT-SCREEN-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05200-EDIT-PFKEY                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO VALIDATE PROGRAM FUNCTION KEY USAGE *
      *                                                               *
      *    CALLED BY:  P05100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P05200-EDIT-PFKEY.

      *****************************************************************
      *    VALID KEYS ARE: ENTER, PF03 PREV SCREEN, PF12 (MAIN MENU)  *
      *****************************************************************

           IF PDA104-PFKEY  =  'EN' OR '03' OR '12'
               NEXT SENTENCE
           ELSE
               MOVE LOW-VALUES         TO  PDA104-CUSID-ATTR
               MOVE WS-CURSOR-ATTR     TO  PDA104-CUSID-ATTR1
               MOVE PM001-INVALID-PFKEY
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P05200-EDIT-PFKEY-EXIT.


       P05200-EDIT-PFKEY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05300-EDIT-CUSID                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO VALIDATE THE CUSTOMER IDENTIFICATION*
      *                                                               *
      *    CALLED BY:  P05100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************

       P05300-EDIT-CUSID.

      *****************************************************************
      *    CUSTOMER ID IS A REQUIRED ENTRY                            *
      *****************************************************************

           IF PDA104-CUSID             >   SPACES
               NEXT SENTENCE
           ELSE
               MOVE LOW-VALUES         TO  PDA104-CUSID-ATTR
               MOVE WS-CURSOR-ATTR     TO  PDA104-CUSID-ATTR1
               MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA104-CUSID-ATTR2
               MOVE PM009-ENTER-CUST-ID
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P05300-EDIT-CUSID-EXIT.


      *****************************************************************
      *    LOCATE CUSTOMER INFO IN ARRAY                              *
      *****************************************************************

           MOVE 'N'                    TO WS-CUSTOMER-FOUND-SW.

           PERFORM  P07100-FIND-CUSTOMER
               THRU P07100-FIND-CUSTOMER-EXIT
                   VARYING WS-SUB1 FROM +1 BY +1
                       UNTIL WS-SUB1 > +13.


      *****************************************************************
      *    CHECK FOR CUSTOMER ID NOT FOUND  --- ERROR                 *
      *****************************************************************

           IF CUSTOMER-FOUND
               NEXT SENTENCE
           ELSE
               MOVE LOW-VALUES         TO  PDA104-CUSID-ATTR
               MOVE WS-CURSOR-ATTR     TO  PDA104-CUSID-ATTR1
               MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA104-CUSID-ATTR2
               MOVE PM008-CUST-NOT-FOUND
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P05300-EDIT-CUSID-EXIT.


       P05300-EDIT-CUSID-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05500-CHECK-PENDORD                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DETERMINE IF ANY PENDING ORDERS     *
      *                FOR THE SPECCIFIED USER EXIST IN THE DATABASE  *
      *                                                               *
      *    CALLED BY:  P05000-UPDATE-PROCESS                          *
      *                                                               *
      *****************************************************************

       P05500-CHECK-PENDORD.

           MOVE 'N'                    TO WS-ORDER-FOUND-SW.
           MOVE 'GE'                   TO PENDORD-QUAL-OPERATOR.
           MOVE PDA104-USERID-NUMBER   TO PENDORD-QUAL-PREFIX.
           MOVE '00001'                TO PENDORD-QUAL-SEQUENCE.

           PERFORM  P79000-GU-PENDORD
               THRU P79000-GU-PENDORD-EXIT.

           IF PENDORD-STATUS      NOT =  SPACES
               GO TO P05500-CHECK-PENDORD-EXIT.

           IF PENDING-ORDER-PREFIX = PDA104-USERID-NUMBER
               MOVE 'Y'                TO WS-ORDER-FOUND-SW.


       P05500-CHECK-PENDORD-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07100-FIND-CUSTOMER                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO SEARCH THE CUSTOMER ID ARRAY FOR    *
      *                THE FIRST CUSTOMER ID EQUAL OR GREATER THAN    *
      *                THE USER ENTERED CUSTOMER ID. IF ENTRY FOUND   *
      *                FORMAT SCREEN WITH CUSTOMER INFO.              *
      *                                                               *
      *    CALLED BY:  P04000-INQUIRY-PROCESS                         *
      *                P05300-EDIT-CUSID                              *
      *                                                               *
      *****************************************************************

       P07100-FIND-CUSTOMER.

      *****************************************************************
      *    FORMAT CUSTOMER INFO IF TABLE => THAN SCREEN VALUE         *
      *****************************************************************

           IF WCAR-CUSTOMER-ID (WS-SUB1)  =  PDA104-CUSID    OR
              WCAR-CUSTOMER-ID (WS-SUB1)  >  PDA104-CUSID
               MOVE 'Y'                TO WS-CUSTOMER-FOUND-SW
               MOVE WCAR-CUSTOMER-ID      (WS-SUB1)
                                       TO PDA104-CUSID
               MOVE WCAR-CUSTOMER-NAME    (WS-SUB1)
                                       TO PDA104-CUSNAME
               MOVE WCAR-CUSTOMER-ADDRESS (WS-SUB1)
                                       TO PDA104-CUSADDR
               MOVE WCAR-CUSTOMER-CITY    (WS-SUB1)
                                       TO PDA104-CUSCITY
               MOVE WCAR-CUSTOMER-STATE   (WS-SUB1)
                                       TO PDA104-CUSSTATE
               MOVE WCAR-CUSTOMER-ZIP     (WS-SUB1)
                                       TO PDA104-CUSZIP
               MOVE WCAR-CUSTOMER-EMAIL   (WS-SUB1)
                                       TO PDA104-CUSEMAIL
               MOVE +14                TO WS-SUB1
           ELSE
               NEXT SENTENCE.


       P07100-FIND-CUSTOMER-EXIT.
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

           IF PDA104-SCREEN-MESSAGE    >  SPACES
               NEXT SENTENCE
           ELSE
               MOVE WMF-MESSAGE-AREA   TO PDA104-SCREEN-MESSAGE.

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

           IF PDA104-SCREEN-MESSAGE    >  SPACES
               NEXT SENTENCE
           ELSE
               MOVE WMF-MESSAGE-AREA   TO PDA104-SCREEN-MESSAGE.

       P70100-INFO-MSG-ROUTINE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79000-GU-PENDORD                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE PENDING ORDER ROOT     *
      *                SEGMENT                                        *
      *                                                               *
      *    CALLED BY:  P05100-CHECK-PENDORD                           *
      *                                                               *
      *****************************************************************

       P79000-GU-PENDORD.


           CALL 'CBLTDLI'    USING     GU
                                       PENDORD-PCB
                                       PENDING-ORDER-SEGMENT
                                       PENDORD-QUAL-SSA.


           IF PENDORD-STATUS           =  SPACES OR 'GE' OR 'GB'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA104'           TO WPIE-PROGRAM-ID
               MOVE PENDORD-STATUS     TO WPIE-STATUS-CODE
               MOVE 'GU'               TO WPIE-FUNCTION-CODE
               MOVE 'P79000'           TO WPIE-PARAGRAPH
               MOVE 'PENDORD'          TO WPIE-SEGMENT-NAME
               MOVE 'PENDO1DB'         TO WPIE-DATABASE-NAME
               MOVE 'GU PENDORD ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P79000-GU-PENDORD-EXIT.
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

           MOVE 'PDA104  '             TO PDA104-PREV-PGRMID.

           INSPECT PDA104-CUSID
               CONVERTING  WMF-SPACES-LOWVALUE-R TO SPACES.

           MOVE LENGTH OF PDA104-MESSAGE
                                       TO PDA104-MSG-LL.

           IF ERROR-FOUND
               MOVE SPACES             TO PDA104-SAVAREA-CUSID
           ELSE
               MOVE PDA104-CUSID       TO PDA104-SAVAREA-CUSID.


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
               MOVE 'PDA104'           TO WPIE-PROGRAM-ID
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
      *                APPROPRIATE IMS FUNCTION BASED ON THE MENU     *
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
               MOVE 'PDA104'           TO WPIE-PROGRAM-ID
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
           MOVE 'PDA104'               TO CIOM-PREV-PGRMID.


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
               MOVE 'PDA104'           TO WPIE-PROGRAM-ID
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