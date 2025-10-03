       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA150.

      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA150 (IMS MPP PROGRAM)                          *
      * TRANS   :   PDA15001                                          *
      * MFS     :   NONE                                              *
      *                                                               *
      * FUNCTION:   PROGRAM PDA150 IS THE IMS/DC PRODUCT DEMONSTRATION*
      *             APPLICATION CUSTOMER ORDER INQUIRY PROGRAM.       *
      *                                                               *
      *             PROGRAM RECEIVES AN IMS MESSAGE REQUEST FROM A    *
      *             USER GUI FRONT END. THE INCOMING MESSAGE AND THE  *
      *             RESULTING RESPONSE ARE HANDLED VIA THE JAVA       *
      *             CONNECTOR ARCHITECTURE (JCA) ON THE CLIENT        *
      *             WORKSTATION AND IMS CONNECT SOFTWARE RESIDING ON  *
      *             THE MAINFRAME.                                    *
      *                                                               *
      *                                                               *
      * FILES   :   ORDER2DB     (IMS ORDER DB)      (INPUT)          *
      *                                                               *
      *                                                               *
      * TRANSACTIONS GENERATED: NONE                                  *
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
       77  WS-MAX-ORDERS               PIC S9(04)   COMP  VALUE +14.

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

           05  WS-ERROR-FOUND-SW       PIC 9(01)             VALUE 0.
               88  NO-ERROR-FOUND                            VALUE 0.
               88  ERROR-FOUND                               VALUE 1.
               88  FATAL-ERROR-FOUND                         VALUE 9.

           EJECT
      *****************************************************************
      *    MISCELLANEOUS WORK FIELDS                                  *
      *****************************************************************
       01  WS-MISCELLANEOUS-FIELDS.

           05  WMF-USERID-NUMBER       PIC S9(9)   VALUE +0  COMP.

           05  WMF-USERID              PIC X(08)   VALUE SPACES.
           05  WMF-MODNAME-ERROR       PIC X(08)   VALUE 'PDAERRO'.
           05  WMF-MASTER-LTERM-NAME   PIC X(08)   VALUE 'SMASTER'.
           05  WMF-IO-PCB-LTERM-NAME   PIC X(08)   VALUE SPACES.
           05  WMF-DATE-MMDDYY         PIC X(08)   VALUE SPACES.
           05  WMF-TIME-HHMMSS         PIC X(08)   VALUE SPACES.
           05  WMF-MESSAGE-AREA        PIC X(79)   VALUE SPACES.

           05  WMF-TOTAL-ORDERS        PIC S9(5)   VALUE +0  COMP-3.
           05  WMF-TOTAL-DOLLAR-AMOUNT PIC 9(09)V99 VALUE 0   COMP-3.
           05  WMF-AVG-DOLLAR-AMOUNT   PIC 9(09)V99 VALUE 0   COMP-3.
           05  WMF-LAST-ORDER-DATE     PIC X(06)    VALUE SPACES.
           05  WMF-LAST-ORDER-AMOUNT   PIC 9(07)V99 VALUE 0   COMP-3.
           05  WMF-LAST-ORDER-NUMBER   PIC X(10)    VALUE SPACES.

           05  WMF-HOLD-DATE-CCYYMMDD  PIC X(08)    VALUE SPACES.

           05  WMF-DATE-CCYYMMDD.
               10  WMF-DATE-CC         PIC 9(02)    VALUE ZEROES.
               10  WMF-DATE-YYMMDD.
                   15  WMF-DATE-YY     PIC 9(02)    VALUE ZEROES.
                   15  WMF-DATE-MM     PIC 9(02)    VALUE ZEROES.
                   15  WMF-DATE-DD     PIC 9(02)    VALUE ZEROES.


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
      *  CUSTOMER ORDER INQUIRY MESSAGE INPUT - OUTPUT AREA           *
      *  PREFIX: PDA150                                               *
      *  LENGTH: 1000                                                 *
      *****************************************************************

       01  PDA150-MESSAGE.
           05 PDA150-MSG-LL            PIC S9(04)      COMP.
           05 PDA150-MSG-ZZ            PIC X(02).
           05 PDA150-MSG-TRANCODE      PIC X(08).
           05 FILLER                   PIC X(01).
           05 PDA150-MSG-SOURCE        PIC X(03).
           05 FILLER                   PIC X(03).
           05 PDA150-MSG-USERID-INFO.
              10  PDA150-USERID-ID     PIC X(08).
              10  PDA150-USERID-NUMBER PIC 9(05).
           05  PDA150-RETURN-CODE      PIC 9(01).
               88  PDA150-NO-ERROR                        VALUE 0.
               88  PDA150-ERROR                           VALUE 1.
               88  PDA150-FATAL-ERROR                     VALUE 9.
           05  PDA150-CUSTOMER-ID      PIC X(32).
           05  PDA150-TOTAL-ORDERS     PIC 9(05).
           05  PDA150-TOTAL-DOLLAR-AMOUNT
                                       PIC 9(09)V99.
           05  PDA150-AVG-DOLLAR-AMOUNT
                                       PIC 9(09)V99.
           05  PDA150-LAST-ORDER-DATE  PIC X(06).
           05  PDA150-LAST-ORDER-AMOUNT
                                       PIC 9(07)V99.
           05  PDA150-LAST-ORDER-NUMBER
                                       PIC X(10).
           05  PDA150-ORDER-DETAIL     OCCURS 14 TIMES.
               10  PDA150-ORDER-NUMBER
                                       PIC X(10).
               10  PDA150-ORDER-AMOUNT
                                       PIC 9(07)V99.
           05  PDA150-SCREEN-MESSAGE   PIC X(79).
           05  PDA150-ERROR-INFORMATION.
               10  PDA150-PDA-ERROR-TYPE
                                       PIC X(04).
               10  PDA150-PDA-ERROR-LINE-01
                                       PIC X(78).
               10  PDA150-PDA-ERROR-LINE-02
                                       PIC X(78).
           05  FILLER                  PIC X(378).
           EJECT

      *****************************************************************
      *  PDA ERROR OUTPUT MESSAGE STORAGE AREA,                       *
      *  MESSAGE SENT TO IMS MASTER TERMINAL (SMASTER)                *
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
      *    DB2  DEFINITIONS                                           *
      *****************************************************************
      *****************************************************************
      *         SQL COMMUNICATIONS AREA                               *
      *****************************************************************
      ***  EXEC SQL
      ***     INCLUDE SQLCA
      ***  END-EXEC.


      *****************************************************************
      *         DB2 DCLGEN FOR USER IDENTIFICATION TABLE (USERID1)    *
      *****************************************************************
      ***  EXEC SQL
      ***     INCLUDE DUSERID1
      ***  END-EXEC.


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
      *                MAINTENANCE MENU.                              *
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

           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.
           MOVE ZEROES                 TO WS-ERROR-FOUND-SW.


      *****************************************************************
      *    OBTAIN CURRENT SYSTEM DATE / TIME                          *
      *****************************************************************

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME.          00020001


      *****************************************************************
      *    INITIALIZE MESSAGE AREAS, WORK AREAS ETC.                  *
      *****************************************************************

           MOVE ZEROES                 TO WMF-TOTAL-ORDERS
                                          WMF-TOTAL-DOLLAR-AMOUNT
                                          WMF-AVG-DOLLAR-AMOUNT
                                          WMF-LAST-ORDER-AMOUNT.

           MOVE SPACES                 TO WMF-IO-PCB-LTERM-NAME
                                          WMF-DATE-MMDDYY
                                          WMF-TIME-HHMMSS
                                          WMF-MESSAGE-AREA
                                          WMF-LAST-ORDER-DATE
                                          WMF-LAST-ORDER-NUMBER.

           MOVE LOW-VALUES             TO PDA150-MESSAGE.               00020001


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
                                       PDA150-MESSAGE.

           MOVE IO-PCB-LTERM-NAME      TO WMF-IO-PCB-LTERM-NAME.


           IF IO-PCB-STATUS            = SPACES
               NEXT SENTENCE
           ELSE
           IF IO-PCB-STATUS            = 'QC'
               MOVE 'N'                TO WS-MORE-MSGS-SW
               GO TO P01000-PROCESS-MSG-EXIT
           ELSE
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA150'           TO WPIE-PROGRAM-ID
               MOVE IO-PCB-STATUS      TO WPIE-STATUS-CODE
               MOVE 'GU'               TO WPIE-FUNCTION-CODE
               MOVE 'P01000'           TO WPIE-PARAGRAPH
               MOVE 'GU IO-PCB FOR IMS/DC MESSAGE'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    INITIALIZE RELEVANT MESSAGE INPUT / OUTPUT AREAS           *
      *****************************************************************

           PERFORM  P01100-INIT-MESSAGE
               THRU P01100-INIT-MESSAGE-EXIT.


      *****************************************************************
      *    EDIT MESSAGE RELATED INFORMATION                           *
      *****************************************************************

           PERFORM  P03000-EDIT-PROCESS
               THRU P03000-EDIT-PROCESS-EXIT.


      *****************************************************************
      *    SEND RESULTS TO USER                                       *
      *****************************************************************

           PERFORM  P80000-INSERT-MSG
               THRU P80000-INSERT-MSG-EXIT.


       P01000-PROCESS-MSG-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01100-INIT-MESSAGE                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INITIALIZE RELEVANT MESSAGE AREAS   *
      *                                                               *
      *    CALLED BY:  P01000-PROCESS-MSG                             *
      *                                                               *
      *****************************************************************

       P01100-INIT-MESSAGE.


           MOVE ZEROES                 TO  PDA150-RETURN-CODE
                                           PDA150-TOTAL-ORDERS
                                           PDA150-TOTAL-DOLLAR-AMOUNT
                                           PDA150-AVG-DOLLAR-AMOUNT.

           MOVE SPACES                 TO  PDA150-LAST-ORDER-DATE.

           MOVE ZEROES                 TO  PDA150-LAST-ORDER-AMOUNT.

           MOVE SPACES                 TO  PDA150-LAST-ORDER-NUMBER.


           PERFORM  P01130-INIT-ORDER-DETAIL
               THRU P01130-INIT-ORDER-DETAIL-EXIT
                   VARYING WS-SUB1 FROM +1 BY +1
                       UNTIL WS-SUB1 > WS-MAX-ORDERS.


           MOVE SPACES                 TO  PDA150-SCREEN-MESSAGE
                                           PDA150-ERROR-INFORMATION.

       P01100-INIT-MESSAGE-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01130-INIT-ORDER-DETAIL                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INITIALIZE THE ORDER INFO           *
      *                OCCURRENCES OF THE IMS MESSAGES.               *
      *                                                               *
      *    CALLED BY:  P01100-INIT-MESSAGE                            *
      *                                                               *
      *****************************************************************

       P01130-INIT-ORDER-DETAIL.


           MOVE SPACES                 TO  PDA150-ORDER-NUMBER
                                                           (WS-SUB1).

           MOVE ZEROES                 TO  PDA150-ORDER-AMOUNT
                                                           (WS-SUB1).


       P01130-INIT-ORDER-DETAIL-EXIT.
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
      *    (TRANCODE AND SOURCE OF MESSAGE FROM A VALID APPL SOURCE)  *
      *****************************************************************

           IF PDA150-MSG-TRANCODE      = 'PDA15001'    AND
              PDA150-MSG-SOURCE        = 'PDA'
               NEXT SENTENCE
           ELSE
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA150'           TO WPIE-PROGRAM-ID
               MOVE 'P03000'           TO WPIE-PARAGRAPH
               MOVE 'INITIAL ENTRY INTO PROG INVALID'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    VERIFY USERID EXISTS IN THE IMS DB/DC APPLICATION          *
      *****************************************************************

           PERFORM  P04000-VERIFY-USERID
               THRU P04000-VERIFY-USERID-EXIT.

           IF ERROR-FOUND
               GO TO P03000-EDIT-PROCESS-EXIT.


      *****************************************************************
      *    PERFORM THE ORDER QUERY PROCESS                            *
      *****************************************************************

           PERFORM  P05000-ORDER-PROCESS
               THRU P05000-ORDER-PROCESS-EXIT.


       P03000-EDIT-PROCESS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P04000-VERIFY-USERID                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PERFORM USERID VERIFICATION:        *
      *                                                               *
      *                USERID MUST EXIST IN THE APPLICATION IN THE    *
      *                USERID1 DB2 TABLE                              *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************

       P04000-VERIFY-USERID.

      *****************************************************************
      *    USERID RESIDES IS PASSED IN THE INCOMING IMS MESSAGE,      *
      *    CHECK IF USERID EXISTS IN THE USERID1 DB2 TABLE            *
      *                                                               *
      *    IF NOT EXISTS ------- ERROR                                *
      *****************************************************************

           MOVE PDA150-USERID-ID       TO WMF-USERID.

      *****************************************************************
      **** TEMPORARY UNTIL THIS FUNCTION IS HOOKED INTO THE REAL ******
      **** IMS DB/DC APPLICATION                                      *
      *****************************************************************
           MOVE 1                      TO WMF-USERID-NUMBER
                                          PDA150-USERID-NUMBER.

      ***  EXEC SQL SELECT    ID,
      ***                     NUMBER,
      ***                     ACTIVE_SCENARIOS
      ***
      ***           INTO      :USERID1-ID,
      ***                     :USERID1-NUMBER,
      ***                     :USERID1-ACTIVE-SCENARIOS
      ***
      ***           FROM      USERID1
      ***
      ***           WHERE     ID = :WMF-USERID
      ***  END-EXEC.


      ***  IF SQLCODE                  =  ZEROES
      ***      MOVE USERID1-NUMBER     TO WMF-USERID-NUMBER
      ***                                 PDA150-USERID-NUMBER
      ***  ELSE
      ***  IF SQLCODE                  =  +100
      ***      MOVE 'USER'             TO PDA150-PDA-ERROR-TYPE
      ***      MOVE PM033-USERID-NOT-FOUND
      ***                              TO WMF-MESSAGE-AREA
      ***      PERFORM  P70000-ERROR-ROUTINE
      ***          THRU P70000-ERROR-ROUTINE-EXIT
      ***  ELSE
      ***      MOVE 9                  TO WS-ERROR-FOUND-SW
      ***      MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
      ***      MOVE 'PDA150'           TO WPDE-PROGRAM-ID
      ***      MOVE SQLCODE            TO WPDE-DB2-SQLCODE
      ***      MOVE 'SELECT ID FROM USERID1'
      ***                              TO WPDE-FUNCTION
      ***      MOVE 'P04000'           TO WPDE-PARAGRAPH
      ***      PERFORM  P99500-PDA-ERROR
      ***          THRU P99500-PDA-ERROR-EXIT.

       P04000-VERIFY-USERID-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05000-ORDER-PROCESS                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE CUSTOMER ORDER INQUIRY  *
      *                PROCESS                                        *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************

       P05000-ORDER-PROCESS.


      *****************************************************************
      **** TEMPORARY UNTIL THIS FUNCTION IS HOOKED INTO THE REAL ******
      **** IMS DB/DC APPLICATION                                      *
      *****************************************************************

           MOVE 1                      TO PDA150-USERID-NUMBER.


      *****************************************************************
      *    PROCESS THE ORDERS FROM THE 1ST ORDER FOR THE SPECIFIED    *
      *    USER ID NUMBER                                             *
      *****************************************************************

           MOVE ZEROES                 TO WS-SUB1.
           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.

           MOVE 'GT'                   TO ORDER-QUAL-OPERATOR.
           MOVE PDA150-USERID-NUMBER   TO ORDER-QUAL-PREFIX.
           MOVE ZEROES                 TO ORDER-QUAL-NUMBER.

           PERFORM  P78000-GU-ORDER
               THRU P78000-GU-ORDER-EXIT.


           PERFORM  P05200-PROCESS-ORDERS
               THRU P05200-PROCESS-ORDERS-EXIT
                   UNTIL PROCESS-COMPLETE.


      *****************************************************************
      *    AT END OF ORDERS, PERFORM TOTAL CALCULATIONS, MOVE INFO    *
      *    TO RETURN MESSAGE AREA                                     *
      *****************************************************************

           MOVE WMF-TOTAL-ORDERS       TO PDA150-TOTAL-ORDERS.
           MOVE WMF-TOTAL-DOLLAR-AMOUNT
                                       TO PDA150-TOTAL-DOLLAR-AMOUNT.
           MOVE WMF-LAST-ORDER-DATE    TO PDA150-LAST-ORDER-DATE.
           MOVE WMF-LAST-ORDER-AMOUNT  TO PDA150-LAST-ORDER-AMOUNT.
           MOVE WMF-LAST-ORDER-NUMBER  TO PDA150-LAST-ORDER-NUMBER.

           IF WMF-TOTAL-ORDERS         > ZEROES
               COMPUTE WMF-AVG-DOLLAR-AMOUNT ROUNDED =
                   WMF-TOTAL-DOLLAR-AMOUNT / WMF-TOTAL-ORDERS.

           MOVE WMF-AVG-DOLLAR-AMOUNT  TO PDA150-AVG-DOLLAR-AMOUNT.


       P05000-ORDER-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05200-PROCESS-ORDERS                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS / FORMAT THE                *
      *                ORDER INFORMATION INTO THE MESSAGE RETURN AREA *
      *                                                               *
      *    CALLED BY:  P05000-ORDER-PROCESS                           *
      *                                                               *
      *****************************************************************

       P05200-PROCESS-ORDERS.

      *****************************************************************
      *    DETERMINE IF ORDER FOR GIVEN USERID EXISTS                 *
      *****************************************************************

           IF (OP-STATUS               =  SPACES)    AND
              (ORDER-PREFIX            =  PDA150-USERID-NUMBER)
               NEXT SENTENCE
           ELSE
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
               GO TO P05200-PROCESS-ORDERS-EXIT.


      *****************************************************************
      *    ONLY SELECT ORDERS FOR THE DESIRED CUSTOMER ID             *
      *****************************************************************

           IF ORDER-CUSTOMER-ID    NOT =  PDA150-CUSTOMER-ID
               PERFORM  P78100-GN-ORDER
                   THRU P78100-GN-ORDER-EXIT
               GO TO P05200-PROCESS-ORDERS-EXIT.


      *****************************************************************
      *    SELECT  14 ORDERS MAXIMUM                                  *
      *****************************************************************

           ADD +1                      TO  WS-SUB1.

           IF WS-SUB1                  >   WS-MAX-ORDERS
               MOVE 'Y'                TO  WS-PROCESS-COMPLETE-SW
               GO TO P05200-PROCESS-ORDERS-EXIT.


      *****************************************************************
      *    FORMAT ORDER INFORMATION INTO THE MESSAGE RETURN AREA      *
      *****************************************************************

           PERFORM  P05300-FORMAT-ORDINFO
               THRU P05300-FORMAT-ORDINFO-EXIT.


      *****************************************************************
      *    READ THE NEXT ORDER                                        *
      *****************************************************************

           PERFORM  P78100-GN-ORDER
               THRU P78100-GN-ORDER-EXIT.


       P05200-PROCESS-ORDERS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05300-FORMAT-ORDINFO                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO FORMAT ORDER AND RELATED            *
      *                INFORMATION TO THE MESSAGE RETURN AREA         *
      *                                                               *
      *    CALLED BY:  P05200-PROCESS-ORDERS                          *
      *                                                               *
      *****************************************************************

       P05300-FORMAT-ORDINFO.


      *****************************************************************
      *    ADD TO RUNNING TOTALS, STORE ORDER INFORMATION             *
      *****************************************************************

           ADD +1                      TO WMF-TOTAL-ORDERS.

           COMPUTE WMF-TOTAL-DOLLAR-AMOUNT  =
                   WMF-TOTAL-DOLLAR-AMOUNT  +  ORDER-TOTAL-AMOUNT.


           MOVE ORDER-DATE-YYMMDD      TO WMF-DATE-YYMMDD.
           IF WMF-DATE-YY              >  50
               MOVE 19                 TO WMF-DATE-CC
           ELSE
               MOVE 20                 TO WMF-DATE-CC.

           IF WMF-DATE-CCYYMMDD  NOT   <  WMF-HOLD-DATE-CCYYMMDD
               MOVE ORDER-DATE-YYMMDD  TO WMF-LAST-ORDER-DATE
               MOVE ORDER-TOTAL-AMOUNT TO WMF-LAST-ORDER-AMOUNT
               MOVE ORDER-NUMBER       TO WMF-LAST-ORDER-NUMBER
               MOVE WMF-DATE-CCYYMMDD  TO WMF-HOLD-DATE-CCYYMMDD.


           MOVE ORDER-NUMBER          TO PDA150-ORDER-NUMBER (WS-SUB1).
           MOVE ORDER-TOTAL-AMOUNT    TO PDA150-ORDER-AMOUNT (WS-SUB1).


       P05300-FORMAT-ORDINFO-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P70000-ERROR-ROUTINE                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO HANDLE NON-FATAL ERROR MESSAGE      *
      *                PROCESSING                                     *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************

       P70000-ERROR-ROUTINE.

           MOVE 1                      TO WS-ERROR-FOUND-SW
                                          PDA150-RETURN-CODE.

           IF PDA150-SCREEN-MESSAGE    >  SPACES
               NEXT SENTENCE
           ELSE
               MOVE WMF-MESSAGE-AREA   TO PDA150-SCREEN-MESSAGE.


       P70000-ERROR-ROUTINE-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P78000-GU-ORDER                                *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE ORDER ROOT             *
      *                SEGMENT                                        *
      *                                                               *
      *    CALLED BY:  P05000-ORDER-PROCESS                           *
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
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA150'           TO WPIE-PROGRAM-ID
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
      *    CALLED BY:  P05200-PROCESS-ORDERS                          *
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
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA150'           TO WPIE-PROGRAM-ID
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
      *    PARAGRAPH:  P80000-INSERT-MSG                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INSERT THE OUTPUT MESSAGE TO THE    *
      *                IO-PCB, MESSAGE IS SENT BACK TO THE GUI        *
      *                FRONT END PROCESS VIA IMS CONNECT AND THE JAVA *
      *                CONNECTOR ARCHITECTURE.                        *
      *                                                               *
      *    CALLED BY:  P01000-PROCESS-MSG                             *
      *                                                               *
      *****************************************************************

       P80000-INSERT-MSG.

           MOVE LENGTH OF PDA150-MESSAGE
                                       TO PDA150-MSG-LL.

      *****************************************************************
      *    WRITE THE IMS MESSAGE                                      *
      *****************************************************************

           CALL 'CBLTDLI'    USING     ISRT
                                       IO-PCB
                                       PDA150-MESSAGE.


           IF IO-PCB-STATUS            = SPACES
               NEXT SENTENCE
           ELSE
               MOVE 9                  TO WS-ERROR-FOUND-SW
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA150'           TO WPIE-PROGRAM-ID
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
      *                AN IMS MESSAGE IS RETURNED TO THE CALLER       *
      *                INDICATING THE NATURE OF THE ERROR             *
      *                                                               *
      *                AN IMS MESSAGE IS ROUTED TO THE                *
      *                IMS MASTER TERMINAL INDICATING THE             *
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
      * MAKE AN ATTEMPT TO GET A MESSAGE BACK TO THE USER             *
      *****************************************************************
      * THE RETURN IMS MESSAGE FOR THE GUI FRONT END, ISRT TO IO-PCB, *
      * FORMAT THE 2 KEY ERROR LINES (7 AND 8) INTO THE RETURN MSG    *
      *****************************************************************

           MOVE WS-ERROR-FOUND-SW      TO PDA150-RETURN-CODE.
           MOVE WS-PDA-ERROR-TYPE      TO PDA150-PDA-ERROR-TYPE.
           MOVE WPEA-ERROR-07-TEXT     TO PDA150-PDA-ERROR-LINE-01.
           MOVE WPEA-ERROR-08-TEXT     TO PDA150-PDA-ERROR-LINE-02.

           MOVE LENGTH OF PDA150-MESSAGE
                                       TO PDA150-MSG-LL.

           CALL 'CBLTDLI'    USING     ISRT
                                       IO-PCB
                                       PDA150-MESSAGE.


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