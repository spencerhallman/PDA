       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA106.

      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA106 (IMS MPP PROGRAM)                          *
      * TRANS   :   PDA10601                                          *
      * MFS     :   NONE                                              *
      *                                                               *
      * FUNCTION:   PROGRAM PDA106 IS THE IMS/DC PRODUCT DEMONSTRATION*
      *             APPLICATION CATEGORY ITEM AND ITEM SUPPLIER       *
      *             SELECTION PROGRAM.                                *
      *                                                               *
      *                                                               *
      * FILES   :   PENDO1DB     (IMS PENDING ORDER DB) (INPUT/OUTPUT)*
      *                                                               *
      *                                                               *
      * TRANSACTIONS GENERATED:                                       *
      *             PDA10801   PENDING ORDER                          *
      *                                                               *
      *                                                               *
      * PFKEYS  :   NONE                                              *
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

           05  WMF-SUPPLIER-COUNT      PIC S9(5)   VALUE +0  COMP-3.
           05  WMF-ORDER-SEQUENCE      PIC S9(5)   VALUE +0  COMP-3.
           05  WMF-TOTAL-ORDERS        PIC S9(5)   VALUE +0  COMP-3.

           05  WMF-MODNAME             PIC X(08)   VALUE 'PDA106O'.
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
           05 CIOM-THE-REST            PIC X(1801).
           EJECT

      *****************************************************************
      *  CATEGORY ITEM / ITEM SUPPLIER MESSAGE INPUT - OUTPUT AREA    *
      *  PREFIX: PDA106                                               *
      *****************************************************************

       01  PDA106-MESSAGE              REDEFINES CIOM-MESSAGE.
           05 PDA106-MSG-LL            PIC S9(04)      COMP.
           05 PDA106-MSG-ZZ            PIC X(02).
           05 PDA106-MSG-TRANCODE      PIC X(08).
           05 FILLER                   PIC X(01).
           05 PDA106-MSG-SOURCE        PIC X(03).
           05 FILLER                   PIC X(01).
           05 PDA106-PFKEY             PIC X(02).
           05 PDA106-MSG-USERID-INFO.
              10  PDA106-USERID-ID     PIC X(08).
              10  PDA106-USERID-NUMBER PIC 9(05).
           05 PDA106-PREV-PGRMID       PIC X(08).
           05 PDA106-SAVAREA           PIC X(79).
           05 PDA106-SAVAREA-R         REDEFINES PDA106-SAVAREA.
              10 PDA106-SAVAREA-ORDER-MENU-SEL
                                       PIC X(01).
              10 PDA106-SAVAREA-CUSID  PIC X(32).
              10 PDA106-ORIGINATING-PGRMID
                                       PIC X(08).
              10 PDA106-FIRST-CAT-SUB  PIC 9(03).
              10 PDA106-LAST-CAT-SUB   PIC 9(03).
              10 PDA106-SELECTED-CAT   PIC 9(03).
              10 PDA106-SELECTED-SUBCAT
                                       PIC 9(03).
           05 PDA106-SCREEN-MESSAGE    PIC X(79).
           05 PDA106-SMESSAGE          PIC X(79).
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
      *    PENDING ORDER DATABASE ROOT SEGMENT (SAVE AREA)            *
      *****************************************************************

       01  PENDING-ORDER-SEGMENT-SAVE  PIC X(200).
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
               10  PENDORD-QUAL-SEQUENCE-R
                                       REDEFINES PENDORD-QUAL-SEQUENCE
                                       PIC 9(05).
           05  PENDORD-QUAL-RPAREN     PIC X(01)   VALUE ')'.

      *****************************************************************
      *    DB2  DEFINITIONS                                           *
      *****************************************************************
      *****************************************************************
      *         SQL COMMUNICATIONS AREA                               *
      *****************************************************************
           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.


      *****************************************************************
      *         DB2 DCLGEN FOR ITEM TABLE                             *
      *****************************************************************
           EXEC SQL
              INCLUDE DITEM
           END-EXEC.


      *****************************************************************
      *         DB2 DCLGEN FOR ITEM SUPPLIER TABLE                    *
      *****************************************************************
           EXEC SQL
              INCLUDE DITMSUP
           END-EXEC.


      *****************************************************************
      *         DB2 CURSOR FOR ITEM TABLE                             *
      *****************************************************************
           EXEC SQL
           DECLARE ITEMFORW CURSOR FOR
             SELECT  NUMBER,
                     NAME
             FROM ITEM
             WHERE PREFIX             = :ITEM-PREFIX            AND
                   CATEGORY_NAME      = :ITEM-CATEGORY-NAME     AND
                   SUB_CATEGORY_NAME  = :ITEM-SUB-CATEGORY-NAME AND
                   NUMBER            >= :ITEM-NUMBER
             ORDER BY 1
           END-EXEC.


      *****************************************************************
      *         DB2 CURSOR FOR ITEM SUPPLIER TABLE                    *
      *****************************************************************
           EXEC SQL
           DECLARE ITEMSUPP CURSOR FOR
             SELECT  ITEM_PREFIX,
                     ITEM_NUMBER,
                     SUPPLIER_PREFIX,
                     SUPPLIER_ID
             FROM    ITEM_SUPPLIER
             WHERE   ITEM_PREFIX      = :ITEM-SUPPLIER-ITEM-PREFIX AND
                     ITEM_NUMBER      = :ITEM-SUPPLIER-ITEM-NUMBER
                   ORDER BY SUPPLIER_ID
           END-EXEC.


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
      *    STATIC CATEGORY / SUB-CATEGORY DEFINITIONS                 *
      *****************************************************************

           COPY PDACATGY.
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
      *    IMS PENDING ORDER DATABASE (PENDORD) PCB MASK 1            *
      *****************************************************************

           COPY PCBPNDO1.
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
                                    PENDORD1-PCB.


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
      *                PERFORM THE EDIT PROCESS                       *
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
               MOVE 'PDA106'           TO WPIE-PROGRAM-ID
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

           IF CIOM-PREV-PGRMID    NOT = 'PDA106'                        00020001
               MOVE LOW-VALUES        TO CIOM-THE-REST.                 00020001


      *****************************************************************
      *    EDIT MESSAGE RELATED INFORMATION                           *
      *****************************************************************

           PERFORM  P03000-EDIT-PROCESS
               THRU P03000-EDIT-PROCESS-EXIT.

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

           IF CIOM-MSG-TRANCODE        = 'PDA10601'    AND
              CIOM-MSG-SOURCE          = 'PDA'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA106'           TO WPIE-PROGRAM-ID
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
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA106'           TO WPIE-PROGRAM-ID
               MOVE 'P03000'           TO WPIE-PARAGRAPH
               MOVE 'VALID IMS USER SIGNON REQUIRED'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    OTHERWISE PERFORM UPDATE PROCESSES                         *
      *****************************************************************

           PERFORM  P05000-UPDATE-PROCESS
               THRU P05000-UPDATE-PROCESS-EXIT.


       P03000-EDIT-PROCESS-EXIT.
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
      *    COUNT THE NUMBER OF PENDING ORDERS ON FILE, USED LATER     *
      *    TO LIMIT THE NUMBER OF PENDING ORDERS A USER MAY CREATE    *
      *****************************************************************

           PERFORM  P69100-COUNT-ORDERS
               THRU P69100-COUNT-ORDERS-EXIT.


      *****************************************************************
      *    LOCATE 1ST ITEM FOR CATEGORY / SUB-CATEGORY COMBO          *
      *****************************************************************

           PERFORM  P06000-ITEM-PROCESS
               THRU P06000-ITEM-PROCESS-EXIT.


      *****************************************************************
      *    LOCATE SUPPLIERS FOR THE ITEM, CREATE PENDING ORDERS       *
      *****************************************************************

           PERFORM  P07000-SUPPLIER-PROCESS
               THRU P07000-SUPPLIER-PROCESS-EXIT.


      *****************************************************************
      *    SCHEDULE THE NEXT IMS TRANSACTION, PDA10801, PENDING ORDER *
      *    INQUIRY / UPDATE FUNCTION                                  *
      *****************************************************************

           MOVE 'PDA10801'             TO WMF-NEXT-TRANID.

           PERFORM  P80300-XFER-CONTROL
               THRU P80300-XFER-CONTROL-EXIT.


       P05000-UPDATE-PROCESS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06000-ITEM-PROCESS                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE ITEM RETRIEVAL PROCESS  *
      *                FOR THE SPECIFIC CATEGORY / SUB-CATEGORY       *
      *                                                               *
      *    CALLED BY:  P05000-UPDATE-PROCESS                          *
      *                                                               *
      *****************************************************************

       P06000-ITEM-PROCESS.


      *****************************************************************
      *    SET COLUMN SEARCH VALUES, OPEN THE CURSOR                  *
      *****************************************************************

           MOVE ZEROES                 TO ITEM-PREFIX.
           MOVE PCAR-CATEGORY     (PDA106-SELECTED-CAT)
                                       TO ITEM-CATEGORY-NAME.
           MOVE PCAR-SUB-CATEGORY (PDA106-SELECTED-CAT,
                                          PDA106-SELECTED-SUBCAT)
                                       TO ITEM-SUB-CATEGORY-NAME.
           MOVE LOW-VALUES             TO ITEM-NUMBER.

           PERFORM  P06100-OPEN-ITEM-CURSOR
               THRU P06100-OPEN-ITEM-CURSOR-EXIT.

           IF ERROR-FOUND
             GO TO P06000-ITEM-PROCESS-EXIT.


      *****************************************************************
      *    FETCH ITEM ROW                                             *
      *****************************************************************

           PERFORM  P06200-FETCH-ITEM-ROW
               THRU P06200-FETCH-ITEM-ROW-EXIT.

           IF ERROR-FOUND
             GO TO P06000-ITEM-PROCESS-EXIT.


      *****************************************************************
      *    CLOSE THE ITEM CURSOR                                      *
      *****************************************************************

           PERFORM  P06300-CLOSE-ITEM-CURSOR
               THRU P06300-CLOSE-ITEM-CURSOR-EXIT.

       P06000-ITEM-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06100-OPEN-ITEM-CURSOR                        *
      *                                                               *
      *    FUNCTION :  OPENS CURSOR TO BUILD THE ITEM RESULT SET      *
      *                                                               *
      *    CALLED BY:  P06000-ITEM-PROCESS                            *
      *                                                               *
      *****************************************************************

       P06100-OPEN-ITEM-CURSOR.

           EXEC SQL
               OPEN ITEMFORW
           END-EXEC.


      *****************************************************************
      *    ZEROES (SUCCESS) IS THE ONLY ACCEPTABLE RETURN CODE        *
      *****************************************************************

           IF SQLCODE                  = ZEROS
               NEXT SENTENCE
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA106'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'OPEN ITEM CURSOR' TO WPDE-FUNCTION
               MOVE 'P06100'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P06100-OPEN-ITEM-CURSOR-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06200-FETCH-ITEM-ROW                          *
      *                                                               *
      *    FUNCTION :  FETCH ROW FROM ITEM FORWARD CURSOR             *
      *                                                               *
      *    CALLED BY:  P06000-ITEM-PROCESS                            *
      *                                                               *
      *****************************************************************

       P06200-FETCH-ITEM-ROW.


           EXEC SQL
               FETCH  ITEMFORW
                INTO  :ITEM-NUMBER,
                      :ITEM-NAME
           END-EXEC.


      *****************************************************************
      *    AT LEAST 1 ITEM IS NEEDED, OTHERWISE ORDER ADD CANNOT      *
      *    CONTINUE                                                   *
      *****************************************************************

           IF SQLCODE                  = ZEROS
               NEXT SENTENCE
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA106'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'FETCH ITEM CURS'  TO WPDE-FUNCTION
               MOVE 'P06200'           TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR
                  THRU P99500-PDA-ERROR-EXIT.

       P06200-FETCH-ITEM-ROW-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06300-CLOSE-ITEM-CURSOR                       *
      *                                                               *
      *    FUNCTION :  CLOSES THE ITEM CURSOR                         *
      *                                                               *
      *    CALLED BY:  P06000-ITEM-PROCESS                            *
      *                                                               *
      *****************************************************************

       P06300-CLOSE-ITEM-CURSOR.


           EXEC SQL
               CLOSE ITEMFORW
           END-EXEC.


      *****************************************************************
      *    ZEROES (SUCCESS) IS THE ONLY ACCEPTABLE RETURN CODE        *
      *****************************************************************

           IF SQLCODE                  = ZEROS
               NEXT SENTENCE
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA106'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'CLOSE ITEM CURSOR'
                                       TO WPDE-FUNCTION
               MOVE 'P06300'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P06300-CLOSE-ITEM-CURSOR-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07000-SUPPLIER-PROCESS                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE SUPPLIER RETRIEVAL      *
      *                PROCESS FOR A SPECIFIC ITEM                    *
      *                                                               *
      *    CALLED BY:  P05000-UPDATE-PROCESS                          *
      *                                                               *
      *****************************************************************

       P07000-SUPPLIER-PROCESS.


      *****************************************************************
      *    SET COLUMN SEARCH VALUES, OPEN THE CURSOR                  *
      *****************************************************************

           MOVE ZEROES                 TO ITEM-SUPPLIER-ITEM-PREFIX.
           MOVE ITEM-NUMBER            TO ITEM-SUPPLIER-ITEM-NUMBER.

           PERFORM  P07100-OPEN-SUPP-CURSOR
               THRU P07100-OPEN-SUPP-CURSOR-EXIT.

           IF ERROR-FOUND
             GO TO P07000-SUPPLIER-PROCESS-EXIT.


      *****************************************************************
      *    FETCH ITEM SUPPLIER ROW                                    *
      *****************************************************************

           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.
           MOVE ZEROES                 TO WMF-SUPPLIER-COUNT.

           PERFORM  P07200-FETCH-SUPP-ROW
               THRU P07200-FETCH-SUPP-ROW-EXIT
                   UNTIL PROCESS-COMPLETE.

           IF ERROR-FOUND
             GO TO P07000-SUPPLIER-PROCESS-EXIT.


      *****************************************************************
      *    CLOSE THE ITEM SUPPLIER CURSOR                             *
      *****************************************************************

           PERFORM  P07300-CLOSE-SUPP-CURSOR
               THRU P07300-CLOSE-SUPP-CURSOR-EXIT.

       P07000-SUPPLIER-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07100-OPEN-SUPP-CURSOR                        *
      *                                                               *
      *    FUNCTION :  OPENS CURSOR TO BUILD THE ITEM SUPPLIER RESULT *
      *                SET                                            *
      *                                                               *
      *    CALLED BY:  P07000-SUPPLIER-PROCESS                        *
      *                                                               *
      *****************************************************************

       P07100-OPEN-SUPP-CURSOR.

           EXEC SQL
               OPEN ITEMSUPP
           END-EXEC.


      *****************************************************************
      *    ZEROES (SUCCESS) IS THE ONLY ACCEPTABLE RETURN CODE        *
      *****************************************************************

           IF SQLCODE                  = ZEROS
               NEXT SENTENCE
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA106'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'OPEN SUPP CURSOR' TO WPDE-FUNCTION
               MOVE 'P07100'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P07100-OPEN-SUPP-CURSOR-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07200-FETCH-SUPP-ROW                          *
      *                                                               *
      *    FUNCTION :  FETCH ROW FROM ITEM SUPPLIER CURSOR            *
      *                                                               *
      *    CALLED BY:  P07000-SUPPLIER-PROCESS                        *
      *                                                               *
      *****************************************************************

       P07200-FETCH-SUPP-ROW.


           EXEC SQL
               FETCH  ITEMSUPP
                INTO  :ITEM-SUPPLIER-ITEM-PREFIX,
                      :ITEM-SUPPLIER-ITEM-NUMBER,
                      :ITEM-SUPPLIER-SUPPLIER-PREFIX,
                      :ITEM-SUPPLIER-SUPPLIER-ID
           END-EXEC.


      *****************************************************************
      *    AT LEAST 1 SUPPLIER IS NEEDED, OTHERWISE TERMINATE,        *
      *    CREATE A PENDING ORDER FOR EACH SUPPLIER FOUND             *
      *****************************************************************

           IF SQLCODE                  = ZEROS
               ADD +1                  TO WMF-SUPPLIER-COUNT
               PERFORM  P09000-PENDING-ORDER
                   THRU P09000-PENDING-ORDER-EXIT
               GO TO P07200-FETCH-SUPP-ROW-EXIT.


           IF SQLCODE                 = +100
                  MOVE 'Y'            TO WS-PROCESS-COMPLETE-SW
              IF WMF-SUPPLIER-COUNT      > ZEROES
                  GO TO P07200-FETCH-SUPP-ROW-EXIT
              ELSE
                  NEXT SENTENCE
           ELSE
                  NEXT SENTENCE.


      *****************************************************************
      *    IF NO SUPPLIERS FOR THE ITEM OR BAD SQL CODE -- TERMINATE  *
      *****************************************************************

           MOVE 'DB2'                  TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA106'               TO WPDE-PROGRAM-ID.
           MOVE SQLCODE                TO WPDE-DB2-SQLCODE.
           MOVE 'FETCH SUPP CURS'      TO WPDE-FUNCTION.
           MOVE 'P07200'               TO WPDE-PARAGRAPH.
           PERFORM  P99500-PDA-ERROR
               THRU P99500-PDA-ERROR-EXIT.

       P07200-FETCH-SUPP-ROW-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P07300-CLOSE-SUPP-CURSOR                       *
      *                                                               *
      *    FUNCTION :  CLOSES THE ITEM SUPPLIER CURSOR                *
      *                                                               *
      *    CALLED BY:  P07000-SUPPLIER-PROCESS                        *
      *                                                               *
      *****************************************************************

       P07300-CLOSE-SUPP-CURSOR.


           EXEC SQL
               CLOSE ITEMSUPP
           END-EXEC.


      *****************************************************************
      *    ZEROES (SUCCESS) IS THE ONLY ACCEPTABLE RETURN CODE        *
      *****************************************************************

           IF SQLCODE                  = ZEROS
               NEXT SENTENCE
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA106'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'CLOSE SUPP CURSOR'
                                       TO WPDE-FUNCTION
               MOVE 'P07300'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P07300-CLOSE-SUPP-CURSOR-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P09000-PENDING-ORDER                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL PENDING ORDER CREATION      *
      *                FOR A GIVEN SUPPLIER                           *
      *                                                               *
      *    CALLED BY:  P07200-FETCH-SUPP-ROW                          *
      *                                                               *
      *****************************************************************

       P09000-PENDING-ORDER.

      *****************************************************************
      *    RETRIEVE AND/OR CREATE THE PENDING ORDER CONTROL RECORD,   *
      *    INCREMENT ORDER SEQUENCE, SAVE CONTROL RECORD              *
      *****************************************************************

           PERFORM  P09100-CONTROL-RECORD
               THRU P09100-CONTROL-RECORD-EXIT.

           ADD +1                      TO PENDING-ORDER-QUANTITY.
           MOVE PENDING-ORDER-QUANTITY TO WMF-ORDER-SEQUENCE.
           MOVE PENDING-ORDER-SEGMENT  TO PENDING-ORDER-SEGMENT-SAVE.


      *****************************************************************
      ***************** TEMPORARY *************************************
      *    LIMIT EACH USER TO 20 PENDING ORDERS MAX, DO NOT WANT      *
      *    DATABASE FILLING, USERS DO NOT ALWAYS MAINTAIN THEIR DATA  *
      *****************************************************************

           ADD +1                      TO WMF-TOTAL-ORDERS.

           IF WMF-TOTAL-ORDERS         > +20
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
               GO TO P09000-PENDING-ORDER-EXIT.


      *****************************************************************
      *    FORMAT AND WRITE A PENDING ORDER                           *
      *****************************************************************

           MOVE SPACES                 TO PENDING-ORDER-SEGMENT.
           MOVE PDA106-USERID-NUMBER   TO PENDING-ORDER-PRE.
           MOVE WMF-ORDER-SEQUENCE     TO PENDING-ORDER-SEQUENCE.

           MOVE 1                      TO PENDING-ORDER-QUANTITY.
           MOVE ITEM-SUPPLIER-ITEM-PREFIX
                                       TO PENDING-ORDER-ITEM-PRE.
           MOVE ITEM-SUPPLIER-ITEM-NUMBER
                                       TO PENDING-ORDER-ITEM-NUMBER.
           MOVE ITEM-SUPPLIER-SUPPLIER-PREFIX
                                       TO PENDING-ORDER-SUPPLIER-PRE.
           MOVE ITEM-SUPPLIER-SUPPLIER-ID
                                       TO PENDING-ORDER-SUPPLIER-ID.


           PERFORM  P79200-ISRT-PENDORD
               THRU P79200-ISRT-PENDORD-EXIT.


      *****************************************************************
      *    UPDATE THE PENDING ORDER CONTROL RECORD, LAST ORDER        *
      *    SEQUENCE # UPDATE (RESIDES IN PENDING-ORDER-QUANTITY)      *
      *****************************************************************

           MOVE PENDING-ORDER-SEGMENT-SAVE
                                       TO PENDING-ORDER-SEGMENT.

           PERFORM  P79100-REPL-PENDORD
               THRU P79100-REPL-PENDORD-EXIT.


       P09000-PENDING-ORDER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P09100-CONTROL-RECORD                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE PENDING ORDER CONTROL  *
      *                RECORD TO OBTAIN THE NEXT SEQUENCE NUMBER. THE *
      *                CONTROL RECORD IS CREATED IF IT DOES NOT EXIST.*
      *                                                               *
      *    CALLED BY:  P09000-PENDING-ORDER                           *
      *                                                               *
      *****************************************************************

       P09100-CONTROL-RECORD.

      *****************************************************************
      *    ATTEMPT CONTROL RECORD RETRIEVAL, IF FOUND --- EXIT        *
      *****************************************************************

           MOVE 'EQ'                   TO PENDORD-QUAL-OPERATOR.
           MOVE PDA106-USERID-NUMBER   TO PENDORD-QUAL-PREFIX.
           MOVE ZEROES                 TO PENDORD-QUAL-SEQUENCE.

           PERFORM  P79000-GHU-PENDORD
               THRU P79000-GHU-PENDORD-EXIT.

           IF PENDORD-STATUS           =  SPACES
               GO TO P09100-CONTROL-RECORD-EXIT.


      *****************************************************************
      *    OTHERWISE FORMAT AND CREATE A CONTROL RECORD FOR USER      *
      *****************************************************************

           MOVE SPACES                 TO PENDING-ORDER-SEGMENT.
           MOVE PDA106-USERID-NUMBER   TO PENDING-ORDER-PRE.
           MOVE ZEROES                 TO PENDING-ORDER-SEQUENCE.
           MOVE ZEROES                 TO PENDING-ORDER-QUANTITY.

           PERFORM  P79200-ISRT-PENDORD
               THRU P79200-ISRT-PENDORD-EXIT.


      *****************************************************************
      *    RETRIEVE CONTROL RECORD, NOT FOUND --- TERMINATE           *
      *****************************************************************

           MOVE 'EQ'                   TO PENDORD-QUAL-OPERATOR.
           MOVE PDA106-USERID-NUMBER   TO PENDORD-QUAL-PREFIX.
           MOVE ZEROES                 TO PENDORD-QUAL-SEQUENCE.

           PERFORM  P79000-GHU-PENDORD
               THRU P79000-GHU-PENDORD-EXIT.

           IF PENDORD-STATUS           =  SPACES
               GO TO P09100-CONTROL-RECORD-EXIT.


           MOVE 'IMS'                  TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA106'               TO WPIE-PROGRAM-ID.
           MOVE PENDORD-STATUS         TO WPIE-STATUS-CODE.
           MOVE 'GHU'                  TO WPIE-FUNCTION-CODE.
           MOVE 'P09100'               TO WPIE-PARAGRAPH.
           MOVE 'PENDORD'              TO WPIE-SEGMENT-NAME.
           MOVE 'PENDO1DB'             TO WPIE-DATABASE-NAME.
           MOVE 'GHU PENDORD ROOT SEGMENT'
                                       TO WPIE-COMMAND.
           PERFORM  P99500-PDA-ERROR
               THRU P99500-PDA-ERROR-EXIT.

       P09100-CONTROL-RECORD-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P69100-COUNT-ORDERS                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO COUNT THE NUMBER OF PENDING ORDERS  *
      *                FOR A PARTICULAR USER                          *
      *                                                               *
      *    CALLED BY:  P05000-UPDATE-PROCESS                          *
      *                                                               *
      *****************************************************************

       P69100-COUNT-ORDERS.

      *****************************************************************
      *    ESTABLISH DATABASE POSITION AT 1ST PENDING ORDER FOR USER  *
      *****************************************************************

           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.
           MOVE ZEROES                 TO WMF-TOTAL-ORDERS.

           MOVE 'GE'                   TO PENDORD-QUAL-OPERATOR.
           MOVE PDA106-USERID-NUMBER   TO PENDORD-QUAL-PREFIX.
           MOVE 1                      TO PENDORD-QUAL-SEQUENCE-R.

           PERFORM  P79030-GU-PENDORD
               THRU P79030-GU-PENDORD-EXIT.


      *****************************************************************
      *    PROCESS ALL PENDING ORDERS FOR THE USER                    *
      *****************************************************************

           PERFORM  P69200-RETRIEVE-ORDERS
               THRU P69200-RETRIEVE-ORDERS-EXIT
                   UNTIL PROCESS-COMPLETE.


       P69100-COUNT-ORDERS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P69200-RETRIEVE-ORDERS                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ THE PENDING ORDERS FROM THE    *
      *                PENDING ORDER DATABASE                         *
      *                                                               *
      *    CALLED BY:  P69100-COUNT-ORDERS                            *
      *                                                               *
      *****************************************************************

       P69200-RETRIEVE-ORDERS.

      *****************************************************************
      *    DETERMINE IF PENDING ORDER IS FOR THE SELECTED USER        *
      *****************************************************************

           IF (PENDORD-STATUS          =  SPACES)    AND
              (PENDING-ORDER-PREFIX    =  PDA106-USERID-NUMBER)
               NEXT SENTENCE
           ELSE
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
               GO TO P69200-RETRIEVE-ORDERS-EXIT.


      *****************************************************************
      *    INCREMENT ORDER COUNTER, READ THE NEXT PENDING ORDER       *
      *****************************************************************

           ADD +1                      TO WMF-TOTAL-ORDERS.

           PERFORM  P79060-GN-PENDORD
               THRU P79060-GN-PENDORD-EXIT.

       P69200-RETRIEVE-ORDERS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79000-GHU-PENDORD                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE PENDING ORDER ROOT     *
      *                SEGMENT WITH HOLD                              *
      *                                                               *
      *    CALLED BY:  P09100-CONTROL-RECORD                          *
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
               MOVE 'PDA106'           TO WPIE-PROGRAM-ID
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
      *    PARAGRAPH:  P79030-GU-PENDORD                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE PENDING ORDER ROOT     *
      *                SEGMENT                                        *
      *                                                               *
      *    CALLED BY:  P69100-COUNT-ORDERS                            *
      *                                                               *
      *****************************************************************

       P79030-GU-PENDORD.


           CALL 'CBLTDLI'    USING     GU
                                       PENDORD-PCB
                                       PENDING-ORDER-SEGMENT
                                       PENDORD-QUAL-SSA.


           IF PENDORD-STATUS           =  SPACES OR 'GE' OR 'GB'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA106'           TO WPIE-PROGRAM-ID
               MOVE PENDORD-STATUS     TO WPIE-STATUS-CODE
               MOVE 'GU'               TO WPIE-FUNCTION-CODE
               MOVE 'P79030'           TO WPIE-PARAGRAPH
               MOVE 'PENDORD'          TO WPIE-SEGMENT-NAME
               MOVE 'PENDO1DB'         TO WPIE-DATABASE-NAME
               MOVE 'GU PENDORD ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P79030-GU-PENDORD-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79060-GN-PENDORD                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE PENDING ORDER ROOT     *
      *                SEGMENT                                        *
      *                                                               *
      *    CALLED BY:  P69200-RETRIEVE-ORDERS                         *
      *                                                               *
      *****************************************************************

       P79060-GN-PENDORD.


           CALL 'CBLTDLI'    USING     GN
                                       PENDORD-PCB
                                       PENDING-ORDER-SEGMENT
                                       PENDORD-QUAL-SSA.


           IF PENDORD-STATUS           =  SPACES OR 'GE' OR 'GB'
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA106'           TO WPIE-PROGRAM-ID
               MOVE PENDORD-STATUS     TO WPIE-STATUS-CODE
               MOVE 'GN'               TO WPIE-FUNCTION-CODE
               MOVE 'P79060'           TO WPIE-PARAGRAPH
               MOVE 'PENDORD'          TO WPIE-SEGMENT-NAME
               MOVE 'PENDO1DB'         TO WPIE-DATABASE-NAME
               MOVE 'GN PENDORD ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P79060-GN-PENDORD-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79100-REPL-PENDORD                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO UPDATE THE PENDING ORDER ROOT       *
      *                SEGMENT                                        *
      *                                                               *
      *    CALLED BY:  P09000-PENDING-ORDER                           *
      *                                                               *
      *****************************************************************

       P79100-REPL-PENDORD.


           CALL 'CBLTDLI'    USING     REPL
                                       PENDORD-PCB
                                       PENDING-ORDER-SEGMENT.


           IF PENDORD-STATUS           =  SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA106'           TO WPIE-PROGRAM-ID
               MOVE PENDORD-STATUS     TO WPIE-STATUS-CODE
               MOVE 'REPL'             TO WPIE-FUNCTION-CODE
               MOVE 'P79100'           TO WPIE-PARAGRAPH
               MOVE 'PENDORD'          TO WPIE-SEGMENT-NAME
               MOVE 'PENDO1DB'         TO WPIE-DATABASE-NAME
               MOVE 'REPL PENDORD ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P79100-REPL-PENDORD-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79200-ISRT-PENDORD                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CREATE THE PENDING ORDER ROOT       *
      *                SEGMENT                                        *
      *                                                               *
      *    CALLED BY:  P09000-PENDING-ORDER                           *
      *                P09100-CONTROL-RECORD                          *
      *                                                               *
      *****************************************************************

       P79200-ISRT-PENDORD.


           CALL 'CBLTDLI'    USING     ISRT
                                       PENDORD1-PCB
                                       PENDING-ORDER-SEGMENT
                                       PENDORD-UNQUAL-SSA.


           IF PENDORD1-STATUS          =  SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA106'           TO WPIE-PROGRAM-ID
               MOVE PENDORD1-STATUS    TO WPIE-STATUS-CODE
               MOVE 'ISRT'             TO WPIE-FUNCTION-CODE
               MOVE 'P79200'           TO WPIE-PARAGRAPH
               MOVE 'PENDORD'          TO WPIE-SEGMENT-NAME
               MOVE 'PENDO1DB'         TO WPIE-DATABASE-NAME
               MOVE 'ISRT PENDORD ROOT SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P79200-ISRT-PENDORD-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80300-XFER-CONTROL                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO TRANSFER CONTROL TO THE             *
      *                APPROPRIATE IMS FUNCTION.                      *
      *                                                               *
      *    CALLED BY:  XXXXXXXXXXXXXXXXXXXXX                          *
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
               MOVE 'PDA106'           TO WPIE-PROGRAM-ID
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
           MOVE 'PDA106'               TO CIOM-PREV-PGRMID.


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
               MOVE 'PDA106'           TO WPIE-PROGRAM-ID
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