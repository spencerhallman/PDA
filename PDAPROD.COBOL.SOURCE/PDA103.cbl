       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA103.

      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA103 (IMS MPP PROGRAM)                          *
      * TRANS   :   PDA10301                                          *
      * MFS     :   FORMAT=PDA103, MID=PDA103I, MOD=PDA103O           *
      *                                                               *
      * FUNCTION:   PROGRAM PDA103 IS THE IMS/DC PRODUCT DEMONSTRATION*
      *             APPLICATION MAINTENANCE MENU PROGRAM. THE         *
      *             MAINTENANCE MENU PROVIDES THE FOLLOWING           *
      *             FUNCTIONALITY:                                    *
      *                                                               *
      *             BASE DATA REFRESH                                 *
      *             USER IDENTIFICATION UTILITY                       *
      *                                                               *
      *                                                               *
      * FILES   :   ORDER2DB     (IMS ORDER DB)         (INPUT/OUTPUT)*
      *             PENDO1DB     (IMS PENDING ORDER DB) (INPUT/OUTPUT)*
      *                                                               *
      *                                                               *
      * TRANSACTIONS GENERATED:                                       *
      *             PDA10101   MAIN MENU                              *
      *                                                               *
      *                                                               *
      * PFKEYS  :   PFKEY 12 - MAIN MENU                              *
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

           05  WS-PROCESS-COMPLETE-SW  PIC X(01)             VALUE 'N'.
               88  PROCESS-COMPLETE                          VALUE 'Y'.
               88  NOT-PROCESS-COMPLETE                      VALUE 'N'.

           05  WS-MENU-SELECTION-SW    PIC X(01)             VALUE ' '.
               88  SELECTION-IS-DATA-REFRESH                 VALUE '7'.
               88  SELECTION-IS-USER-ID-UTILITY              VALUE '8'.
               88  SELECTION-IS-VALID                        VALUE '7'
                                                                   '8'.

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

           05  WMF-MODNAME             PIC X(08)   VALUE 'PDA103O'.
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

      *    HIGH INTENS,MOD  DEC=201, HEX=C9, BITS=11001001
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
           05 CIOM-SAVAREA             PIC X(79).
           05 CIOM-THE-REST            PIC X(1880).
           EJECT

      *****************************************************************
      *  PDA MAINTENANCE MENU INPUT / OUTPUT MESSAGE STORAGE AREA     *
      *  PREFIX: PDA103                                               *
      *****************************************************************

       01  PDA103-MESSAGE              REDEFINES CIOM-MESSAGE.
           05 PDA103-MSG-LL            PIC S9(04)      COMP.
           05 PDA103-MSG-ZZ            PIC X(02).
           05 PDA103-MSG-TRANCODE      PIC X(08).
           05 FILLER                   PIC X(01).
           05 PDA103-MSG-SOURCE        PIC X(03).
           05 FILLER                   PIC X(01).
           05 PDA103-PFKEY             PIC X(02).
           05 PDA103-MSG-USERID-INFO.
              10  PDA103-USERID-ID     PIC X(08).
              10  PDA103-USERID-NUMBER PIC 9(05).
           05 PDA103-PREV-PGRMID       PIC X(08).
           05 PDA103-SAVAREA           PIC X(79).
           05 PDA103-DATA-REFRESH-IND  PIC X(01).
           05 PDA103-MENU-SELECTION-ATTR.
              10  PDA103-MENU-SELECTION-ATTR1
                                       PIC X(01).
              10  PDA103-MENU-SELECTION-ATTR2
                                       PIC X(01).
           05 PDA103-MENU-SELECTION    PIC X(01).
           05 PDA103-SCREEN-MESSAGE    PIC X(79).
           05 PDA103-SMESSAGE          PIC X(79).
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

           COPY IORDER.
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


      *****************************************************************
      *    ORDER DATABASE ROOT SEGMENT (SSA)                          *
      *****************************************************************

       01  ORDER-UNQUAL-SSA.
           05  ORDER-UNQUAL-SEGMENT    PIC X(09)   VALUE 'ORDER '.

       01  ORDER-QUAL-SSA.
           05  ORDER-QUAL-SEGMENT      PIC X(08)   VALUE 'ORDER '.
           05  ORDER-QUAL-LPAREN       PIC X(01)   VALUE '('.
           05  ORDER-QUAL-FIELD-NAME   PIC X(08)   VALUE 'ORDKEY '.
           05  ORDER-QUAL-OPERATOR     PIC X(02)   VALUE 'EQ'.
           05  ORDER-QUAL-FIELD-VALUE.
               10  ORDER-QUAL-PREFIX   PIC X(05)   VALUE SPACES.
               10  ORDER-QUAL-NUMBER
                                       PIC X(10)   VALUE SPACES.
           05  ORDER-QUAL-RPAREN       PIC X(01)   VALUE ')'.
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


      *****************************************************************
      *    IMS ORDER DATABASE (ORDER) PCB MASK                        *
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
      *                MAINTENANCE MENU.                              *
      *                                                               *
      *    CALLED BY:  NONE                                           *
      *                                                               *
      *****************************************************************

       P00000-MAINLINE.

           ENTRY 'DLITCBL'   USING  IO-PCB
                                    ALT-IO-PCB1
                                    ALT-IO-PCB2
                                    PENDORD-PCB
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

           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.
           MOVE SPACES                 TO WS-MENU-SELECTION-SW.
           MOVE 'I'                    TO WS-TRANS-INTENT-SW.
           MOVE 'N'                    TO WS-ERROR-FOUND-SW.

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
               MOVE 'PDA103'           TO WPIE-PROGRAM-ID
               MOVE IO-PCB-STATUS      TO WPIE-STATUS-CODE
               MOVE 'GU'               TO WPIE-FUNCTION-CODE
               MOVE 'P01000'           TO WPIE-PARAGRAPH
               MOVE 'GU IO-PCB FOR IMS/DC MESSAGE'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    INITIALIZE VARIABLES BASED ON INCOMING MESSAGE             *
      *    (INITIALIZE BODY OF MESSAGE IF 1ST TIME THRU)              *
      *****************************************************************

           IF CIOM-PREV-PGRMID    NOT = 'PDA103'                        00020001
               MOVE LOW-VALUES        TO CIOM-THE-REST.                 00020001


      *****************************************************************
      *    EDIT MESSAGE RELATED INFORMATION                           *
      *****************************************************************

           PERFORM  P03000-EDIT-PROCESS
               THRU P03000-EDIT-PROCESS-EXIT.


      *****************************************************************
      *    SEND MESSAGE TO SCREEN -- IF INQUIRY, ERROR, OR ENTER KEY  *
      *****************************************************************

           IF (INQUIRY-TRANS)  OR  (ERROR-FOUND)  OR
              (CIOM-MSG-PFKEY  =  'EN')
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

           IF CIOM-MSG-TRANCODE        = 'PDA10301'    AND
              CIOM-MSG-SOURCE          = 'PDA'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA103'           TO WPIE-PROGRAM-ID
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
      *    PERFORM THE SCREEN EDIT PROCESS (PFKEY AND DATA VALIDATION)*
      *****************************************************************

           PERFORM  P03100-EDIT-SCREEN
               THRU P03100-EDIT-SCREEN-EXIT.


      *****************************************************************
      *    IF ANY ERRORS ENCOUNTERED, OR 1ST TIME INQUIRY TRANS,      *
      *    EXIT THE PROCESS                                           *
      *****************************************************************

           IF INQUIRY-TRANS  OR    ERROR-FOUND
               GO TO P03000-EDIT-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    OTHERWISE PERFORM UPDATE PROCESSES                         *
      *****************************************************************

           PERFORM  P05000-UPDATE-PROCESS
               THRU P05000-UPDATE-PROCESS-EXIT.


      *****************************************************************
      *    CLEAR THE MENU SELECTION AFTER SUCCESSFUL EXECUTION        *
      *****************************************************************

           MOVE SPACES                 TO PDA103-MENU-SELECTION.


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

           INSPECT PDA103-MENU-SELECTION
               CONVERTING  WMF-UNDERSCORE-LOWVALUE-R TO SPACES.


      *****************************************************************
      *    IF FIRST TIME THRU, EXIT, AND DISPLAY INITIALIZED SCREEN   *
      *    (U=UPDATE TRANS, I=INQUIRY TRANS - 1ST TIME THRU)          *
      *****************************************************************

           IF PDA103-PREV-PGRMID       =  'PDA103'
               MOVE 'U'                TO WS-TRANS-INTENT-SW
           ELSE
               MOVE 'I'                TO WS-TRANS-INTENT-SW
               GO TO P03100-EDIT-SCREEN-EXIT.


      *****************************************************************
      *    EDIT THE OPERATOR PROGRAM FUNCTION KEY SELECTION (PFKEY)   *
      *****************************************************************

           PERFORM  P03200-EDIT-PFKEY
               THRU P03200-EDIT-PFKEY-EXIT.

           IF ERROR-FOUND
               GO TO P03100-EDIT-SCREEN-EXIT.


      *****************************************************************
      *    EDIT THE ENTERED MENU SELECTION (IF ENTER KEY USED)        *
      *****************************************************************

           IF PDA103-PFKEY             = 'EN'
               PERFORM  P03300-EDIT-SELECTION
                   THRU P03300-EDIT-SELECTION-EXIT.

           IF ERROR-FOUND
               GO TO P03100-EDIT-SCREEN-EXIT.


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
      *    VALID KEYS ARE: ENTER, PF12 (MAIN MENU)                    *
      *****************************************************************

           IF PDA103-PFKEY  =  'EN' OR '12'
               NEXT SENTENCE
           ELSE
               MOVE LOW-VALUES         TO  PDA103-MENU-SELECTION-ATTR
               MOVE WS-CURSOR-ATTR     TO  PDA103-MENU-SELECTION-ATTR1
               MOVE PM001-INVALID-PFKEY
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P03200-EDIT-PFKEY-EXIT.


      *****************************************************************
      *    PFKEY USAGE AND SELECTION CODE ENTRY NOT ALLOWED           *
      *    (MUTUALLY EXCLUSIVE OPERATIONS)                            *
      *****************************************************************

           IF (PDA103-PFKEY      NOT = 'EN')   AND
              (PDA103-MENU-SELECTION > SPACES)
               MOVE LOW-VALUES         TO  PDA103-MENU-SELECTION-ATTR
               MOVE WS-CURSOR-ATTR     TO  PDA103-MENU-SELECTION-ATTR1
               MOVE PM003-ACTION-VS-PFKEY-CONFLICT
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
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
      *    MENU SELECTION MUST BE NUMERIC, AND VALUE OF 7 OR 8        *
      *****************************************************************

           MOVE PDA103-MENU-SELECTION  TO WS-MENU-SELECTION-SW.

           IF SELECTION-IS-VALID
               NEXT SENTENCE
           ELSE
               MOVE LOW-VALUES         TO  PDA103-MENU-SELECTION-ATTR
               MOVE WS-CURSOR-ATTR     TO  PDA103-MENU-SELECTION-ATTR1
               MOVE WS-HI-INTENSITY-ATTR
                                       TO  PDA103-MENU-SELECTION-ATTR2
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
      *    IF PFKEY 12, PASS CONTROL TO THE MAIN MENU                 *
      *****************************************************************

           IF PDA103-PFKEY  =  '12'
               MOVE 'PDA10101'         TO  WMF-NEXT-TRANID
               PERFORM  P80300-XFER-CONTROL
                   THRU P80300-XFER-CONTROL-EXIT
               GO TO P05000-UPDATE-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    BASED ON MENU SELECTION PERFORM APPROPRIATE ROUTINE        *
      *****************************************************************

           IF SELECTION-IS-DATA-REFRESH
               PERFORM  P06000-DATA-REFRESH
                   THRU P06000-DATA-REFRESH-EXIT
               GO TO P05000-UPDATE-PROCESS-EXIT
           ELSE

           IF SELECTION-IS-USER-ID-UTILITY
               PERFORM  P08000-USER-ID-UTILITY
                   THRU P08000-USER-ID-UTILITY-EXIT
               GO TO P05000-UPDATE-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


       P05000-UPDATE-PROCESS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06000-DATA-REFRESH                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO HANDLE THE USER DATA REFRESH        *
      *                FUNCTIONALITY.                                 *
      *                                                               *
      *                THE SPECIFIC SET OF USER DATA IS RETURNED      *
      *                TO ITS BASE / DEFAULT STATE                    *
      *                                                               *
      *    CALLED BY:  P05000-UPDATE-PROCESS                          *
      *                                                               *
      *****************************************************************

       P06000-DATA-REFRESH.

      *****************************************************************
      *   VERIFY DATA REFRESH USER CONFIRMATION HAS BEEN PERFORMED    *
      *   (USER TWO PASS PROCESS TO BE SURE THIS IS REALLY WANT THEY  *
      *    WANT TO DO)                                                *
      *****************************************************************

           IF PDA103-DATA-REFRESH-IND NOT = '7'
               MOVE '7'                TO PDA103-DATA-REFRESH-IND
               MOVE SPACES             TO PDA103-MENU-SELECTION
               MOVE PM036-CONFIRM-REFRESH
                                       TO WMF-MESSAGE-AREA
               PERFORM  P70100-INFO-MSG-ROUTINE
                   THRU P70100-INFO-MSG-ROUTINE-EXIT
               GO TO P06000-DATA-REFRESH-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *   REMOVE PENDING ORDERS FROM IMS DATABASE -PENDORD-           *
      *****************************************************************

           PERFORM  P06100-REFRESH-PENDORD
               THRU P06100-REFRESH-PENDORD-EXIT.


      *****************************************************************
      *   REMOVE SUBMITTED ORDERS FROM THE IMS DATABASE -ORDER-       *
      *****************************************************************

           PERFORM  P06500-REFRESH-ORDER
               THRU P06500-REFRESH-ORDER-EXIT.


      *****************************************************************
      *   FORMAT DATA REFRESH COMPLETE MESSAGE TO SCREEN              *
      *****************************************************************

           MOVE PM037-REFRESH-COMPLETE TO WMF-MESSAGE-AREA.
           MOVE SPACES                 TO PDA103-DATA-REFRESH-IND.
           PERFORM  P70100-INFO-MSG-ROUTINE
               THRU P70100-INFO-MSG-ROUTINE-EXIT.


       P06000-DATA-REFRESH-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06100-REFRESH-PENDORD                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO HANDLE THE PENDING ORDER DATABASE   *
      *                DELETION OF USER ID SPECIFIC DATA              *
      *                                                               *
      *    CALLED BY:  P06000-DATA-REFRESH                            *
      *                                                               *
      *****************************************************************

       P06100-REFRESH-PENDORD.

      *****************************************************************
      *   REMOVE PENDING ORDERS FROM IMS DATABASE -PENDORD-           *
      *****************************************************************

           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.
           MOVE 'GE'                   TO PENDORD-QUAL-OPERATOR.
           MOVE PDA103-USERID-NUMBER   TO PENDORD-QUAL-PREFIX.
           MOVE LOW-VALUES             TO PENDORD-QUAL-SEQUENCE.

           PERFORM  P79000-GHU-PENDORD
               THRU P79000-GHU-PENDORD-EXIT.

           PERFORM  P06200-PENDING-ORDERS
               THRU P06200-PENDING-ORDERS-EXIT
                   UNTIL PROCESS-COMPLETE.


       P06100-REFRESH-PENDORD-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06200-PENDING-ORDERS                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO HANDLE PENDING ORDER DELETION AND   *
      *                RETRIEVAL. ROUTINE IS PERFORMED UNTIL ALL      *
      *                PENDING ORDERS FOR THE SPECIFIC USER ARE       *
      *                REMOVED.                                       *
      *                                                               *
      *    CALLED BY:  P06100-REFRESH-PENDORD                         *
      *                                                               *
      *****************************************************************

       P06200-PENDING-ORDERS.

      *****************************************************************
      *   IF NO SEGMENT FOUND, TERMINATE LOOP, EXIT                   *
      *****************************************************************

           IF PENDORD-STATUS           =  'GE' OR 'GB'
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
               GO TO P06200-PENDING-ORDERS-EXIT
           ELSE

      *****************************************************************
      *   IF ORDER NOT FOR DESIRED USER, TERMINATE LOOP, EXIT         *
      *****************************************************************

           IF PENDING-ORDER-PREFIX NOT =  PDA103-USERID-NUMBER
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
               GO TO P06200-PENDING-ORDERS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *   REMOVE THE PENDING ORDER, READ NEXT PENDING ORDER           *
      *****************************************************************

           PERFORM  P79200-DLET-PENDORD
               THRU P79200-DLET-PENDORD-EXIT.

           PERFORM  P79100-GHN-PENDORD
               THRU P79100-GHN-PENDORD-EXIT.


       P06200-PENDING-ORDERS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06500-REFRESH-ORDER                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO HANDLE THE ORDER DATABASE           *
      *                DELETION OF USER ID SPECIFIC DATA              *
      *                                                               *
      *    CALLED BY:  P06000-DATA-REFRESH                            *
      *                                                               *
      *****************************************************************

       P06500-REFRESH-ORDER.

      *****************************************************************
      *   REMOVE ORDERS FROM IMS DATABASE -ORDER-                     *
      *****************************************************************

           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.
           MOVE 'GE'                   TO ORDER-QUAL-OPERATOR.
           MOVE PDA103-USERID-NUMBER   TO ORDER-QUAL-PREFIX.
           MOVE LOW-VALUES             TO ORDER-QUAL-NUMBER.

           PERFORM  P79300-GHU-ORDER
               THRU P79300-GHU-ORDER-EXIT.

           PERFORM  P06600-ORDERS
               THRU P06600-ORDERS-EXIT
                   UNTIL PROCESS-COMPLETE.


       P06500-REFRESH-ORDER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06600-ORDERS                                  *
      *                                                               *
      *    FUNCTION :  ROUTINE TO HANDLE ORDER DELETION AND           *
      *                RETRIEVAL. ROUTINE IS PERFORMED UNTIL ALL      *
      *                ORDERS FOR THE SPECIFIC USER ARE REMOVED.      *
      *                                                               *
      *    CALLED BY:  P06500-REFRESH-ORDER                           *
      *                                                               *
      *****************************************************************

       P06600-ORDERS.

      *****************************************************************
      *   IF NO SEGMENT FOUND, TERMINATE LOOP, EXIT                   *
      *****************************************************************

           IF OP-STATUS                =  'GE' OR 'GB'
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
               GO TO P06600-ORDERS-EXIT
           ELSE

      *****************************************************************
      *   IF ORDER NOT FOR DESIRED USER, TERMINATE LOOP, EXIT         *
      *****************************************************************

           IF ORDER-PREFIX         NOT =  PDA103-USERID-NUMBER
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
               GO TO P06600-ORDERS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *   REMOVE THE ORDER, READ NEXT ORDER                           *
      *****************************************************************

           PERFORM  P79500-DLET-ORDER
               THRU P79500-DLET-ORDER-EXIT.

           PERFORM  P79400-GHN-ORDER
               THRU P79400-GHN-ORDER-EXIT.


       P06600-ORDERS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P08000-USER-ID-UTILITY                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DISPLAY THE UNIQUE NUMERIC          *
      *                IDENTIFIER ASSOCIATED WITH THE USER LOGON ID.  *
      *                                                               *
      *                THE UNIQUE IDENTIFIER IS DEFINED IN ALL FILE   *
      *                KEY STRUCTURES TO FACILITATE EACH USER OWNING  *
      *                A SET OF DATA                                  *
      *                                                               *
      *    CALLED BY:  P05000-UPDATE-PROCESS                          *
      *                                                               *
      *****************************************************************

       P08000-USER-ID-UTILITY.

      *****************************************************************
      *    USERID UNIQUE IDENTIFIER IS PASSED IN THE IMS MESSAGE.     *
      *                                                               *
      *    NO NEED TO RETRIEVE FROM THE USERID1 DB2 TABLE             *
      *                                                               *
      *    FORMAT THE OUTPUT MESSAGE                                  *
      *****************************************************************

           MOVE PDA103-USERID-NUMBER   TO PM006-MSG-IDNUM.
           MOVE PM006-NUMBER-FOR-USERID
                                       TO WMF-MESSAGE-AREA.

           PERFORM  P70100-INFO-MSG-ROUTINE
               THRU P70100-INFO-MSG-ROUTINE-EXIT.


       P08000-USER-ID-UTILITY-EXIT.
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

           IF PDA103-SCREEN-MESSAGE    >  SPACES
               NEXT SENTENCE
           ELSE
               MOVE WMF-MESSAGE-AREA   TO PDA103-SCREEN-MESSAGE.

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

           IF PDA103-SCREEN-MESSAGE    >  SPACES
               NEXT SENTENCE
           ELSE
               MOVE WMF-MESSAGE-AREA   TO PDA103-SCREEN-MESSAGE.

       P70100-INFO-MSG-ROUTINE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79000-GHU-PENDORD                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE PENDING ORDER ROOT     *
      *                SEGMENT WITH HOLD                              *
      *                                                               *
      *    CALLED BY:  P06100-REFRESH-PENDORD                         *
      *                                                               *
      *****************************************************************

       P79000-GHU-PENDORD.


           CALL 'CBLTDLI'    USING     GHU
                                       PENDORD-PCB
                                       PENDING-ORDER-SEGMENT
                                       PENDORD-QUAL-SSA.


           IF PENDORD-STATUS           =  SPACES OR 'GE' OR 'GB'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA103'           TO WPIE-PROGRAM-ID
               MOVE PENDORD-STATUS     TO WPIE-STATUS-CODE
               MOVE 'GHU'              TO WPIE-FUNCTION-CODE
               MOVE 'P79000'           TO WPIE-PARAGRAPH
               MOVE 'PENDORD'          TO WPIE-SEGMENT-NAME
               MOVE 'PENDO1DB'         TO WPIE-DATABASE-NAME
               MOVE 'GHU PENDORD ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P79000-GHU-PENDORD-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79100-GHN-PENDORD                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE PENDING ORDER ROOT     *
      *                SEGMENT WITH HOLD                              *
      *                                                               *
      *    CALLED BY:  P06200-PENDING-ORDERS                          *
      *                                                               *
      *****************************************************************

       P79100-GHN-PENDORD.


           CALL 'CBLTDLI'    USING     GHN
                                       PENDORD-PCB
                                       PENDING-ORDER-SEGMENT
                                       PENDORD-QUAL-SSA.


           IF PENDORD-STATUS           =  SPACES OR 'GE' OR 'GB'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA103'           TO WPIE-PROGRAM-ID
               MOVE PENDORD-STATUS     TO WPIE-STATUS-CODE
               MOVE 'GHN'              TO WPIE-FUNCTION-CODE
               MOVE 'P79100'           TO WPIE-PARAGRAPH
               MOVE 'PENDORD'          TO WPIE-SEGMENT-NAME
               MOVE 'PENDO1DB'         TO WPIE-DATABASE-NAME
               MOVE 'GHN PENDORD ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P79100-GHN-PENDORD-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79200-DLET-PENDORD                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DELETE THE PENDING ORDER ROOT       *
      *                SEGMENT                                        *
      *                                                               *
      *    CALLED BY:  P06200-PENDING-ORDERS                          *
      *                                                               *
      *****************************************************************

       P79200-DLET-PENDORD.


           CALL 'CBLTDLI'    USING     DLET
                                       PENDORD-PCB
                                       PENDING-ORDER-SEGMENT.


           IF PENDORD-STATUS           =  SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA103'           TO WPIE-PROGRAM-ID
               MOVE PENDORD-STATUS     TO WPIE-STATUS-CODE
               MOVE 'DLET'             TO WPIE-FUNCTION-CODE
               MOVE 'P79200'           TO WPIE-PARAGRAPH
               MOVE 'PENDORD'          TO WPIE-SEGMENT-NAME
               MOVE 'PENDO1DB'         TO WPIE-DATABASE-NAME
               MOVE 'DLET PENDORD ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P79200-DLET-PENDORD-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79300-GHU-ORDER                               *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE ORDER ROOT             *
      *                SEGMENT WITH HOLD                              *
      *                                                               *
      *    CALLED BY:  P06500-REFRESH-ORDER                           *
      *                                                               *
      *****************************************************************

       P79300-GHU-ORDER.


           CALL 'CBLTDLI'    USING     GHU
                                       ORDER-PCB
                                       ORDER-SEGMENT
                                       ORDER-QUAL-SSA.


           IF OP-STATUS                =  SPACES OR 'GE' OR 'GB'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA103'           TO WPIE-PROGRAM-ID
               MOVE OP-STATUS          TO WPIE-STATUS-CODE
               MOVE 'GHU'              TO WPIE-FUNCTION-CODE
               MOVE 'P79300'           TO WPIE-PARAGRAPH
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME
               MOVE 'ORDER2DB'         TO WPIE-DATABASE-NAME
               MOVE 'GHU ORDER ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P79300-GHU-ORDER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79400-GHN-ORDER                               *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE ORDER ROOT             *
      *                SEGMENT WITH HOLD                              *
      *                                                               *
      *    CALLED BY:  P06600-ORDERS                                  *
      *                                                               *
      *****************************************************************

       P79400-GHN-ORDER.


           CALL 'CBLTDLI'    USING     GHN
                                       ORDER-PCB
                                       ORDER-SEGMENT
                                       ORDER-QUAL-SSA.


           IF OP-STATUS                =  SPACES OR 'GE' OR 'GB'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA103'           TO WPIE-PROGRAM-ID
               MOVE OP-STATUS          TO WPIE-STATUS-CODE
               MOVE 'GHN'              TO WPIE-FUNCTION-CODE
               MOVE 'P79400'           TO WPIE-PARAGRAPH
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME
               MOVE 'ORDER2DB'         TO WPIE-DATABASE-NAME
               MOVE 'GHN ORDER ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P79400-GHN-ORDER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79500-DLET-ORDER                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DELETE THE ORDER ROOT               *
      *                SEGMENT                                        *
      *                                                               *
      *    CALLED BY:  P06600-ORDERS                                  *
      *                                                               *
      *****************************************************************

       P79500-DLET-ORDER.


           CALL 'CBLTDLI'    USING     DLET
                                       ORDER-PCB
                                       ORDER-SEGMENT.


           IF OP-STATUS                =  SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA103'           TO WPIE-PROGRAM-ID
               MOVE OP-STATUS          TO WPIE-STATUS-CODE
               MOVE 'DLET'             TO WPIE-FUNCTION-CODE
               MOVE 'P79500'           TO WPIE-PARAGRAPH
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME
               MOVE 'ORDER2DB'         TO WPIE-DATABASE-NAME
               MOVE 'DLET ORDER ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P79500-DLET-ORDER-EXIT.
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

           MOVE 'PDA103  '             TO PDA103-PREV-PGRMID.

           INSPECT PDA103-MENU-SELECTION
               CONVERTING  WMF-SPACES-LOWVALUE-R TO '__'.

           IF (ERROR-FOUND)  OR  (NOT SELECTION-IS-DATA-REFRESH)
               MOVE SPACES             TO PDA103-DATA-REFRESH-IND.

           MOVE LENGTH OF PDA103-MESSAGE
                                       TO PDA103-MSG-LL.


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
               MOVE 'PDA103'           TO WPIE-PROGRAM-ID
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
               MOVE 'PDA103'           TO WPIE-PROGRAM-ID
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
           MOVE 'PDA103'               TO CIOM-PREV-PGRMID.


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
               MOVE 'PDA103'           TO WPIE-PROGRAM-ID
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