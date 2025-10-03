       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA010.

      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA010                                            *
      * TRANS   :   PD10                                              *
      * MAPSET  :   PDA010M                                           *
      *                                                               *
      * FUNCTION:   PROGRAM PDA010 IS THE PRODUCT DEMONSTRATION       *
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
      * FILES   :   ORDER DATABASE - IMS/DLI (READ / UPDATE)          *
      *                                                               *
      *                                                               *
      * TRANSACTIONS GENERATED:                                       *
      *             PD01       MAIN MENU                              *
      *             PD02       ORDER MENU                             *
      *             PD11       VIEW ORDER ITEMS                       *
      *             PD12       BROWSE SUBMITTED ORDERS                *
      *                                                               *
      *                                                               *
      * PFKEYS  :   PF4   =    VIEW ORDER ITEMS        (PDA011)       *
      *             PF5   =    BROWSE SUBMITTED ORDERS (PDA012)       *
      *             PF11  =    ORDER MENU              (PDA002)       *
      *             PF12  =    MAIN MENU               (PDA001)       *
      *                                                               *
      *****************************************************************
      *             PROGRAM CHANGE LOG                                *
      *             -------------------                               *
      *                                                               *
      *  DATE       UPDATED BY            CHANGE DESCRIPTION          *
      *  --------   --------------------  --------------------------  *
      *                                                               *
      *  12/13/05   PAUL BARON            ELIMINATE USE OF PDAS01     *
      *                                   PARAMETER FIELD             *
      *                                   PDAS01-ORDER-DOLLAR-AMT-R   *
      *                                                               *
      *  01/15/04   PAUL BARON            CHANGE TO OPTIONALLY INVOKE *
      *                                   PDA019, PL/I VERSION OF THE *
      *                                   ORDER INQUIRY IF SCENARIO 20*
      *                                   OR 21 HAS BEEN ACTIVATED.   *
      *                                   OTHERWISE COBOL PROGRAM     *
      *                                   PDA014 IS INVOKED TO PERFORM*
      *                                   THE ORDER INQUIRY           *
      *                                                               *
      *  04/20/01   PAUL BARON            CHANGED ORDER INQUIRY TO    *
      *                                   NOT CALL                    *
      *                                   P20000-CHECK-ORDER-AGE.     *
      *                                   ORDER AGE IS NOW DETERMINED *
      *                                   IN PDA014 (LINKED TO MODULE)*
      *                                   FOR THE INQUIRY             *
      *                                                               *
      *  XX/XX/XX   XXXXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXX  *
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
       77  WS-RESPONSE-CODE            PIC S9(08)   COMP    VALUE +0.
       77  WS-MESSAGE-LTH              PIC S9(04)   COMP    VALUE +79.


      *****************************************************************
      *    SWITCHES                                                   *
      *****************************************************************
       01  WS-SWITCHES.

           05  WS-ACTION-CODE-SW       PIC X(01)             VALUE ' '.
               88  ACTION-IS-INQUIRY                         VALUE ' '.
               88  ACTION-IS-CHANGE                          VALUE 'C'.
               88  ACTION-IS-DELETE                          VALUE 'D'.
               88  ACTION-IS-VALID                           VALUE ' '
                                                                   'C'
                                                                   'D'.

           05  WS-ERROR-FOUND-SW       PIC X(01)             VALUE 'N'.
               88  ERROR-FOUND                               VALUE 'Y'.
               88  NO-ERROR-FOUND                            VALUE 'N'.

           EJECT
      *****************************************************************
      *    MISCELLANEOUS WORK FIELDS                                  *
      *****************************************************************
       01  WS-MISCELLANEOUS-FIELDS.

           05  WMF-ABSTIME             PIC S9(15)  VALUE +0  COMP-3.
           05  WMF-DATE-MMDDYY         PIC X(08)   VALUE SPACES.
           05  WMF-TIME-HHMMSS         PIC X(08)   VALUE SPACES.
           05  WMF-PSB-NAME            PIC X(08)   VALUE 'PDA010'.
           05  WMF-PROGRAM-NAME        PIC X(08)   VALUE SPACES.
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

           05  WMF-ORDER-NUMBER        PIC 9(10)   VALUE ZEROES.

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
      *         MAP DSECTS -- ORDER INQUIRY / MAINTENANCE (PDA010M)   *
      *****************************************************************

           COPY PDA010M.
           EJECT

      *****************************************************************
      *    IMS / DLI DEFINITIONS                                      *
      *****************************************************************

           COPY IORDER.
           EJECT

      *****************************************************************
      *    DB2  DEFINITIONS                                           *
      *****************************************************************
      *****************************************************************
      *    NO DB2 USED IN THE MODULE                                  *
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
           05  WPCW-ORDER-NUMBER-IN    PIC X(10).
           05  FILLER                  PIC X(990).

       01  WS-PDA-COMMAREA-WORKAREA-R REDEFINES
                                       WS-PDA-COMMAREA-WORKAREA.

           05  WPCW-ORDER-NUMBER       PIC X(10).
           05  WPCW-ORDER-PURCHASE-NUMBER
                                       PIC 9(13).
           05  WPCW-ORDER-DATE-YYMMDD.
               10  WPCW-ORDER-DATE-YY  PIC X(02).
               10  WPCW-ORDER-DATE-MM  PIC X(02).
               10  WPCW-ORDER-DATE-DD  PIC X(02).
           05  WPCW-ORDER-STATUS       PIC X(32).
           05  WPCW-ORDER-TOTAL-AMOUNT PIC S9(07)V99.
           05  WPCW-ORDER-CUSTOMER-INFO.
               10  WPCW-ORDER-CUSTOMER-ID
                                       PIC X(32).
               10  WPCW-ORDER-CUSTOMER-NAME
                                       PIC X(64).
               10  WPCW-ORDER-CUSTOMER-ADDRESS
                                       PIC X(128).
               10  WPCW-ORDER-CUSTOMER-CITY
                                       PIC X(32).
               10  WPCW-ORDER-CUSTOMER-STATE
                                       PIC X(32).
               10  WPCW-ORDER-CUSTOMER-POSTAL
                                       PIC X(12).
               10  WPCW-ORDER-CUSTOMER-EMAIL
                                       PIC X(128).
           05  WPCW-ORDER-SHIP-TO-INFO.
               10  WPCW-ORDER-SHIP-TO-NAME
                                       PIC X(64).
               10  WPCW-ORDER-SHIP-TO-ADDRESS
                                       PIC X(128).
               10  WPCW-ORDER-SHIP-TO-CITY
                                       PIC X(32).
               10  WPCW-ORDER-SHIP-TO-STATE
                                       PIC X(32).
               10  WPCW-ORDER-SHIP-TO-POSTAL
                                       PIC X(12).
           05  WPCW-ORDER-PURCHASE-TYPE-INFO.
               10  WPCW-ORDER-PURCHASE-TYPE
                                       PIC 9(03).
               10  WPCW-ORDER-PURCHASE-TYPE-DESC
                                       PIC X(32).

           05  WPCW-ERROR-INFORMATION.
               10  WPCW-ERROR-SW       PIC X(01).
                   88 WPCW-NO-ERROR                VALUE SPACES.
                   88 WPCW-ORDER-NOT-FOUND         VALUE '1'.
                   88 WPCW-CUSTOMER-NOT-FOUND      VALUE '2'.
                   88 WPCW-USERID-NOT-FOUND        VALUE '3'.
                   88 WPCW-DB2-ERROR               VALUE '7'.
                   88 WPCW-CICS-ERROR              VALUE '8'.
                   88 WPCW-IMS-ERROR               VALUE '9'.

               10  WPCW-PROGRAM-ID     PIC X(08).
               10  WPCW-PARAGRAPH      PIC X(06).
               10  WPCW-COMMAND        PIC X(30).

             07    WPCW-DB2-INFO.
               10  WPCW-SQLCODE        PIC S9(08)  COMP.

             07    WPCW-CICS-INFO.
               10  WPCW-RESPONSE-CODE  PIC S9(08)  COMP.

             07    WPCW-IMS-INFO.
               10  WPCW-FUNCTION       PIC X(04).
               10  WPCW-SEGMENT        PIC X(08).
               10  WPCW-DATABASE       PIC X(08).
               10  WPCW-STATUS-CODE    PIC X(02).

           05  FILLER                  PIC X(124).

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
      *                ORDER INQUIRY / MAINTENANCE FUNCTION           *
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
                   MOVE 'PDA010'       TO WPCE-PROGRAM-ID
                   MOVE ZEROES         TO WPCE-RESPONSE-CODE
                   MOVE 'COMMAREA LENGTH NOT CORRECT'
                                       TO WPCE-COMMAND
                   MOVE 'P00050'       TO WPCE-PARAGRAPH
                   PERFORM  P99500-PDA-ERROR
                       THRU P99500-PDA-ERROR-EXIT
           ELSE
PWB423             MOVE PM019-ENTER-APPLICATION
PWB423                                 TO  WMF-MESSAGE-AREA
PWB423             PERFORM  P80400-SEND-MESSAGE
PWB423                 THRU P80400-SEND-MESSAGE-EXIT
PWB423             GO TO P00050-INITIALIZE-EXIT.


      *****************************************************************
      *    INITIALZE VARIABLES, WORK AREAS, ETC.                      *
      *****************************************************************

           MOVE SPACES                 TO WS-ACTION-CODE-SW.
           MOVE 'N'                    TO WS-ERROR-FOUND-SW.
                                                                        00010000
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME.          00020001


           IF PC-PREV-PGRMID           =  'PDA010'
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
               MOVE 'PDA010'           TO WPCE-PROGRAM-ID
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

           IF PC-PREV-PGRMID           NOT = 'PDA010'

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
                     TRANSID       ('PD10')
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
               MOVE 'PDA010'           TO WPCE-PROGRAM-ID
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

           MOVE LOW-VALUES             TO PDA010I.
           MOVE SPACES                 TO ACTIONI
                                          ORDNBRI
                                          SAVORDI.

      *****************************************************************
      *    IF THE PREVIOUS PROGRAM WAS VIEW ORDER ITEMS (PDA011), OR  *
      *    BROWSE SUBMITTED ORDERS (PDA012), CHECK FOR ORDER NUMBER   *
      *    PASSED IN THE COMMAREA                                     *
      *****************************************************************

           IF (PC-PREV-PGRMID          =  'PDA011'  OR 'PDA012')  AND
              (PC-ORDER-NUMBER          >  SPACES)
               MOVE PC-ORDER-NUMBER    TO ORDNBRI
               PERFORM  P06000-INQUIRY-PROCESS
                   THRU P06000-INQUIRY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    DISPLAY THE INITIAL SCREEN, UPDATE COMMAREA VARIABLES      *
      *****************************************************************


           PERFORM  P79000-DISPLAY-SCREEN
               THRU P79000-DISPLAY-SCREEN-EXIT.

           MOVE 'PDA010'               TO  PC-PREV-PGRMID.
           MOVE SPACES                 TO  PC-ORDER-NUMBER
                                           WS-PDA-COMMAREA-WORKAREA.


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

           MOVE 'PDA010'               TO  PC-PREV-PGRMID.

      *****************************************************************
      *    RECEIVE THE INPUT MAP, CONVERT UNDERSCORES AND LOW-VALUES  *
      *    TO SPACES                                                  *
      *****************************************************************

           PERFORM  P80200-RECEIVE-MAP
               THRU P80200-RECEIVE-MAP-EXIT.

           MOVE SPACES                 TO  PDAMSGO.

           PERFORM  P02100-CONVERT-FIELDS
               THRU P02100-CONVERT-FIELDS-EXIT.


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


           INSPECT ACTIONI
               CONVERTING  WMF-UNDERSCORE-LOWVALUE-R TO SPACES.

           INSPECT ORDNBRI
               CONVERTING  WMF-UNDERSCORE-LOWVALUE-R TO SPACES.

           INSPECT ORDDATMI
               CONVERTING  WMF-UNDERSCORE-LOWVALUE-R TO SPACES.

           INSPECT ORDDATDI
               CONVERTING  WMF-UNDERSCORE-LOWVALUE-R TO SPACES.

           INSPECT ORDDATYI
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
      *                SELECTION VERSUS ACTION CODE ENTRY             *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************

       P03500-EDIT-TRANS-INTENT.

      *****************************************************************
      *    VALID KEYS ARE: ENTER, CLEAR, PF4, PF5, PF11, PF12         *
      *****************************************************************

           IF EIBAID  = DFHENTER  OR  DFHCLEAR  OR  DFHPF4  OR          PWB423
                        DFHPF5    OR  DFHPF11   OR  DFHPF12             ADDED UP
               NEXT SENTENCE                                            FRONT
           ELSE                                                         EDIT FOR
               MOVE -1                 TO ORDNBRL                       VALID
               MOVE PM001-INVALID-PFKEY                                 KEYS
                                       TO  WMF-MESSAGE-AREA             PWB423
               PERFORM  P70000-ERROR-ROUTINE                            PWB423
                   THRU P70000-ERROR-ROUTINE-EXIT                       PWB423
               GO TO P03500-EDIT-TRANS-INTENT-EXIT.                     PWB423


      *****************************************************************
      *    DETERMINE IF ACTION CODE HAS BEEN ENTERED                  *
      *    (ACTION CODE AND PFKEYS ARE MUTUALLY EXCLUSIVE)            *
      *****************************************************************

           IF EIBAID                   =  DFHENTER OR DFHCLEAR
               NEXT SENTENCE
           ELSE
           IF ACTIONI                  >  SPACES
               MOVE -1                 TO ACTIONL
               MOVE DFHUNIMD           TO ACTIONA
               MOVE PM003-ACTION-VS-PFKEY-CONFLICT
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P03500-EDIT-TRANS-INTENT-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    EDIT THE SCREEN KEY FIELDS ACTION CODE, ORDER NUMBER       *
      *****************************************************************

           IF (EIBAID  =  DFHENTER  OR  DFHPF4)
               PERFORM  P03600-EDIT-KEY-FIELDS
                   THRU P03600-EDIT-KEY-FIELDS-EXIT
           ELSE
               NEXT SENTENCE.

           IF ERROR-FOUND
               GO TO P03500-EDIT-TRANS-INTENT-EXIT.


      *****************************************************************
      *    IF REQUEST IS PF4 (VIEW ORDER ITEMS) OR A CHANGE OR DELETE  *
      *    REQUEST, AN INQUIRY MUST HAVE BEEN PERFORMED FIRST         *
      *****************************************************************

           IF (EIBAID  =  DFHPF4)      OR
              (EIBAID  =  DFHENTER  AND  ACTIONI > SPACES)
               IF ORDNBRI              =  SAVORDI
                   NEXT SENTENCE
               ELSE
                   MOVE -1             TO ACTIONL
                   MOVE PM022-INQUIRY-REQUIRED
                                       TO  WMF-MESSAGE-AREA
                   PERFORM  P70000-ERROR-ROUTINE
                       THRU P70000-ERROR-ROUTINE-EXIT
                   GO TO P03500-EDIT-TRANS-INTENT-EXIT
           ELSE
               NEXT SENTENCE.


       P03500-EDIT-TRANS-INTENT-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03600-EDIT-KEY-FIELDS                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO EDIT THE SCREEN KEY FIELDS ACTION   *
      *                CODE, ORDER NUMBER                             *
      *                                                               *
      *    CALLED BY:  P03500-EDIT-TRANS-INTENT                       *
      *                                                               *
      *****************************************************************

       P03600-EDIT-KEY-FIELDS.

      *****************************************************************
      *    ORDER NUMBER MUST BE NUMERIC, AND GREATER THAN ZERO        *
      *****************************************************************

           MOVE +10                    TO WMF-NUM-LTH.
           MOVE ORDNBRI                TO WMF-NUM-INPUT.

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

PWB423     IF WMF-NUM-ERROR            >  ZEROES    OR
PWB423        WMF-NUM-OUTPUT           =  ZEROES
PWB423         MOVE -1                 TO ORDNBRL
PWB423         MOVE DFHUNINT           TO ORDNBRA
PWB423         MOVE PM021-INVALID-ORDER-NUMBER
PWB423                                 TO WMF-MESSAGE-AREA
PWB423         PERFORM  P70000-ERROR-ROUTINE
PWB423             THRU P70000-ERROR-ROUTINE-EXIT
PWB423     ELSE
PWB423         MOVE WMF-NUM-OUTPUT     TO WMF-ORDER-NUMBER
PWB423         MOVE WMF-ORDER-NUMBER   TO ORDNBRI.


      *****************************************************************
      *    ACTION CODE MUST BE SPACE (INQUIRY), -C- (CHANGE) OR       *
      *    -D- (DELETE)                                               *
      *****************************************************************

           MOVE ACTIONI                TO WS-ACTION-CODE-SW.

           IF ACTION-IS-VALID
               NEXT SENTENCE
           ELSE
               MOVE -1                 TO ACTIONL
               MOVE DFHUNIMD           TO ACTIONA
               MOVE PM020-INVALID-ACTION-CODE
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT.


       P03600-EDIT-KEY-FIELDS-EXIT.
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

PWB423     IF EIBAID  = DFHCLEAR
ALLOW          MOVE PM002-EXIT-APPLICATION
CLEAR                                  TO  WMF-MESSAGE-AREA
KEY            PERFORM  P80400-SEND-MESSAGE
USAGE              THRU P80400-SEND-MESSAGE-EXIT
EXIT           GO TO P04000-PFKEY-PROCESS-EXIT
APPL       ELSE
PWB423         NEXT SENTENCE.


      *****************************************************************
      *    PF4 (VIEW ORDER ITEMS), TRANSFER TO PROGRAM PDA011 PASSING *
      *    THE USER ENTERED ORDER NUMBER IN THE COMMAREA              *
      *****************************************************************

           IF EIBAID  = DFHPF4
               MOVE ORDNBRI            TO  PC-ORDER-NUMBER
               MOVE 'PDA011'           TO  PC-NEXT-PGRMID
               PERFORM  P80300-XFER-CONTROL
                   THRU P80300-XFER-CONTROL-EXIT
               GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    PF5 (BROWSE SUBMITTED ORDERS) TRANSFER TO PDA012           *
      *****************************************************************

           IF EIBAID  = DFHPF5
               MOVE 'PDA012'           TO  PC-NEXT-PGRMID
               PERFORM  P80300-XFER-CONTROL
                   THRU P80300-XFER-CONTROL-EXIT
               GO TO P04000-PFKEY-PROCESS-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    IF PF11, RETURN TO THE ORDER MENU                          *
      *****************************************************************

           IF EIBAID  = DFHPF11
               MOVE 'PDA002'       TO  PC-NEXT-PGRMID
               PERFORM  P80300-XFER-CONTROL
                   THRU P80300-XFER-CONTROL-EXIT
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
      *    FUNCTION :  ROUTINE TO CONTROL THE ORDER INQUIRY           *
      *                PROCESSING.                                    *
      *                                                               *
      *    CALLED BY:  P01000-1ST-TIME-PROCESS                        *
      *                P05000-PROCESS-SCREEN                          *
      *                                                               *
      *****************************************************************

       P06000-INQUIRY-PROCESS.

      *****************************************************************
      *    ORDER INFORMATION IS RETRIEVED BY EITHER PDA014 OR PDA019  *
      *                                                               *
      *    PDA014 - THE DEFAULT ORDER INQUIRY MODULE IS  COBOL        *
      *    (DEMONSTRATION REQUIREMENT FOR UNIFACE)                    *
      *                                                               *
      *    PDA019 - A PL/I VERSION OF PDA014 IS INVOKED IN PLACE OF   *
      *    PDA014 WHEN EITHER SCENARIOS 20 OR 21 ARE ACTIVE           *
      *                                                               *
      *    INITIALIZE COMMAREA WORKAREA, SET THE ORDER KEY AND LINK   *
      *    TO THE APPROPRIATE MODULE (PDA014 OR PDA019)               *
      *****************************************************************

           MOVE SPACES                 TO WS-PDA-COMMAREA-WORKAREA
                                          SAVORDI.
           MOVE ORDNBRI                TO WPCW-ORDER-NUMBER.
           MOVE WS-PDA-COMMAREA-WORKAREA
                                       TO PC-PROGRAM-WORKAREA.

           PERFORM  P79200-CLEAR-SCREEN
               THRU P79200-CLEAR-SCREEN-EXIT.


           IF PC-ACTIVE-SCENARIO(20) = 'Y' OR
              PC-ACTIVE-SCENARIO(21) = 'Y'
               MOVE 'PDA019'           TO WMF-PROGRAM-NAME
           ELSE
               MOVE 'PDA014'           TO WMF-PROGRAM-NAME.


           EXEC CICS LINK
                     PROGRAM       (WMF-PROGRAM-NAME)
                     COMMAREA      (DFHCOMMAREA)
                     LENGTH        (PC-COMMAREA-LTH)
                     NOHANDLE
                     RESP          (WS-RESPONSE-CODE)
           END-EXEC.


      *****************************************************************
      *    IF ERROR (ON LINK), FORMAT ERROR INFORMATION AND TERMINATE *
      *****************************************************************

           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               NEXT SENTENCE
           ELSE
               MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
               MOVE 'PDA010'           TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
               MOVE 'CICS LINK '       TO WPCE-COMMAND-1
               MOVE WMF-PROGRAM-NAME   TO WPCE-COMMAND-2
               MOVE 'P06000'           TO WPCE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    CHECK RETURN CODE FROM PDA014 (IN THE COMMAREA)            *
      *    IF NO ERROR, FORMAT DATA FOR SCREEN                        *
      *    IF ORDER, CUSTOMER,OR USERID NOT FOUND FORMAT MESSAGE      *
      *    IF CICS, DB2, OR IMS ERROR, TERMINATE                      *
      *****************************************************************

           MOVE PC-PROGRAM-WORKAREA    TO WS-PDA-COMMAREA-WORKAREA.

           IF WPCW-NO-ERROR
               PERFORM  P06100-FORMAT-SCREEN
                   THRU P06100-FORMAT-SCREEN-EXIT

               MOVE ORDNBRI            TO SAVORDI
               MOVE PM038-INQUIRY-COMPLETE
                                       TO WMF-MESSAGE-AREA
               PERFORM  P70100-MESSAGE-ROUTINE
                   THRU P70100-MESSAGE-ROUTINE-EXIT
           ELSE

           IF WPCW-ORDER-NOT-FOUND
               MOVE -1                 TO ORDNBRL
               MOVE DFHUNINT           TO ORDNBRA
               MOVE PM023-ORDER-NOT-FOUND
                                       TO WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
           ELSE

           IF WPCW-CUSTOMER-NOT-FOUND
               MOVE -1                 TO ORDNBRL
               MOVE DFHUNINT           TO ORDNBRA
               MOVE PM024-ORDER-CUSTOMER-NOT-FOUND
                                       TO WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
           ELSE

           IF WPCW-USERID-NOT-FOUND
               MOVE -1                 TO ORDNBRL
               MOVE DFHUNINT           TO ORDNBRA
               MOVE PM033-USERID-NOT-FOUND
                                       TO WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
           ELSE

           IF WPCW-CICS-ERROR
               MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
               MOVE WPCW-PROGRAM-ID    TO WPCE-PROGRAM-ID
               MOVE WPCW-RESPONSE-CODE TO WPCE-RESPONSE-CODE
               MOVE WPCW-COMMAND       TO WPCE-COMMAND
               MOVE WPCW-PARAGRAPH     TO WPCE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           ELSE

           IF WPCW-DB2-ERROR
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE WPCW-PROGRAM-ID    TO WPDE-PROGRAM-ID
               MOVE WPCW-SQLCODE       TO WPDE-DB2-SQLCODE
               MOVE WPCW-COMMAND       TO WPDE-FUNCTION
               MOVE WPCW-PARAGRAPH     TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           ELSE

           IF WPCW-IMS-ERROR
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE WPCW-PROGRAM-ID    TO WPIE-PROGRAM-ID
               MOVE WPCW-STATUS-CODE   TO WPIE-STATUS-CODE
               MOVE WPCW-FUNCTION      TO WPIE-FUNCTION-CODE
               MOVE WPCW-SEGMENT       TO WPIE-SEGMENT-NAME
               MOVE WPCW-DATABASE      TO WPIE-DATABASE-NAME
               MOVE WPCW-PARAGRAPH     TO WPIE-PARAGRAPH
               MOVE WPCW-COMMAND       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           ELSE
               NEXT SENTENCE.

           IF PC-ACTIVE-SCENARIO(6) = 'Y' OR
               PC-ACTIVE-SCENARIO(7) = 'Y'
                   EXEC CICS
                       LINK
                           PROGRAM       ('PDA999')
                           COMMAREA      (DFHCOMMAREA)
                           LENGTH        (PC-COMMAREA-LTH)
                           NOHANDLE
                           RESP          (WS-RESPONSE-CODE)
                   END-EXEC
                   IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
                       MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
                       MOVE 'PDA010'           TO WPCE-PROGRAM-ID
                       MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
                       MOVE 'CICS LINK PDA999' TO WPCE-COMMAND
                       MOVE 'P06000'           TO WPCE-PARAGRAPH
                       PERFORM  P99500-PDA-ERROR
                           THRU P99500-PDA-ERROR-EXIT.

       P06000-INQUIRY-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06100-FORMAT-SCREEN                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO FORMAT THE OUTPUT SCREEN WITH       *
      *                ORDER AND CUSTOMER INFORMATION                 *
      *                                                               *
      *    CALLED BY:  P06000-INQUIRY-PROCESS                         *
      *                                                               *
      *****************************************************************

       P06100-FORMAT-SCREEN.

           MOVE WPCW-ORDER-CUSTOMER-ID TO PDACUSTO.
           MOVE WPCW-ORDER-DATE-MM     TO ORDDATMO.
           MOVE WPCW-ORDER-DATE-DD     TO ORDDATDO.
           MOVE WPCW-ORDER-DATE-YY     TO ORDDATYO.
           MOVE WPCW-ORDER-CUSTOMER-NAME
                                       TO NAMEO.
           MOVE WPCW-ORDER-STATUS      TO ORDSTATO.


           MOVE WPCW-ORDER-CUSTOMER-ADDRESS
                                       TO ADDRO.
           MOVE WPCW-ORDER-CUSTOMER-CITY
                                       TO CITYO.
           MOVE WPCW-ORDER-TOTAL-AMOUNT
                                       TO TOTAMTO.
           MOVE WPCW-ORDER-CUSTOMER-STATE
                                       TO STATEO.
           MOVE WPCW-ORDER-PURCHASE-TYPE
                                       TO PURTYPEO.
           MOVE WPCW-ORDER-PURCHASE-TYPE-DESC
                                       TO PURTDESO.
           MOVE WPCW-ORDER-CUSTOMER-POSTAL
                                       TO ZIPO.
           MOVE WPCW-ORDER-PURCHASE-NUMBER
                                       TO PONBRO.
           MOVE WPCW-ORDER-CUSTOMER-EMAIL
                                       TO EMAILO.
           MOVE WPCW-ORDER-SHIP-TO-NAME
                                       TO SHNAMEO.
           MOVE WPCW-ORDER-SHIP-TO-ADDRESS
                                       TO SHADDRO.
           MOVE WPCW-ORDER-SHIP-TO-CITY
                                       TO SHCITYO.
           MOVE WPCW-ORDER-SHIP-TO-STATE
                                       TO SHSTATEO.
           MOVE WPCW-ORDER-SHIP-TO-POSTAL
                                       TO SHZIPO.

       P06100-FORMAT-SCREEN-EXIT.
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
      *    SCHEDULE THE PSB FOR IMS-DLI OPERATIONS                    *
      *****************************************************************

           PERFORM  P10000-SCHEDULE-PSB
               THRU P10000-SCHEDULE-PSB-EXIT.

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


      *****************************************************************
      *    TERMINATE THE PSB USAGE                                    *
      *****************************************************************

           PERFORM  P10100-TERMINATE-PSB
               THRU P10100-TERMINATE-PSB-EXIT.


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
           MOVE ORDDATMI               TO WMF-NUM-INPUT.

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF (WMF-NUM-ERROR            >  ZEROES)    OR
              (WMF-NUM-OUTPUT           <  01         OR
               WMF-NUM-OUTPUT           >  12)
               MOVE -1                 TO ORDDATML
               MOVE DFHUNINT           TO ORDDATMA
               MOVE PM039-INVALID-DATE-MONTH
                                       TO WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
           ELSE
               MOVE WMF-NUM-OUTPUT     TO WMF-ORDER-DATE-MM
               MOVE WMF-ORDER-DATE-MM  TO ORDDATMI.


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
           MOVE ORDDATDI               TO WMF-NUM-INPUT.

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF (WMF-NUM-ERROR            >  ZEROES)    OR
              (WMF-NUM-OUTPUT           <  01         OR
               WMF-NUM-OUTPUT           >  31)
               MOVE -1                 TO ORDDATDL
               MOVE DFHUNINT           TO ORDDATDA
               MOVE PM040-INVALID-DATE-DAY
                                       TO WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
           ELSE
               MOVE WMF-NUM-OUTPUT     TO WMF-ORDER-DATE-DD
               MOVE WMF-ORDER-DATE-DD  TO ORDDATDI.


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
           MOVE ORDDATYI               TO WMF-NUM-INPUT.

           PERFORM  P70500-EDIT-NUMERIC-FIELD
               THRU P70500-EDIT-NUMERIC-FIELD-EXIT.

           IF  WMF-NUM-ERROR           >  ZEROES
               MOVE -1                 TO ORDDATYL
               MOVE DFHUNINT           TO ORDDATYA
               MOVE PM041-INVALID-DATE-YEAR
                                       TO WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
           ELSE
               MOVE WMF-NUM-OUTPUT     TO WMF-ORDER-DATE-YY
               MOVE WMF-ORDER-DATE-YY  TO ORDDATYI.


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
               MOVE -1                 TO ORDDATDL
               MOVE DFHUNINT           TO ORDDATDA
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

           MOVE PC-USERID-NUMBER       TO  WMF-ORDER-KEY-PREFIX.
           MOVE ORDNBRI                TO  WMF-ORDER-KEY-NUMBER.

           PERFORM  P10200-GU-ORDER-SEGMENT
               THRU P10200-GU-ORDER-SEGMENT-EXIT.

           IF ERROR-FOUND
               GO TO P08000-CHANGE-PROCESS-EXIT.


      *****************************************************************
      *    FORMAT DATA INTO SEGMENT I/O AREA, UPDATE SEGMENT          *
      *****************************************************************

           MOVE WMF-ORDER-DATE         TO  ORDER-DATE-YYMMDD.

           PERFORM  P10300-REPL-ORDER-SEGMENT
               THRU P10300-REPL-ORDER-SEGMENT-EXIT.

           PERFORM P20000-CHECK-ORDER-AGE
              THRU P20000-CHECK-ORDER-AGE-EXIT.

      *****************************************************************
      *    FORMAT COMPLETION MESSAGE, CLEAR ACTION CODE               *
      *****************************************************************

           MOVE SPACES                 TO  ACTIONI.
           MOVE PM043-UPDATE-COMPLETE  TO  WMF-MESSAGE-AREA.

           PERFORM  P70100-MESSAGE-ROUTINE
               THRU P70100-MESSAGE-ROUTINE-EXIT.


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

           MOVE PC-USERID-NUMBER       TO  WMF-ORDER-KEY-PREFIX.
           MOVE ORDNBRI                TO  WMF-ORDER-KEY-NUMBER.

           PERFORM  P10200-GU-ORDER-SEGMENT
               THRU P10200-GU-ORDER-SEGMENT-EXIT.

           IF ERROR-FOUND
               GO TO P09000-DELETE-PROCESS-EXIT.


      *****************************************************************
      *    PHYSICALLY DELETE THE ORDER SEGMENT                        *
      *****************************************************************

           PERFORM  P10400-DLET-ORDER-SEGMENT
               THRU P10400-DLET-ORDER-SEGMENT-EXIT.


      *****************************************************************
      *    FORMAT COMPLETION MESSAGE, CLEAR SCREEN DETAIL, ACTION     *
      *****************************************************************

           MOVE SPACES                 TO  ACTIONI
                                           SAVORDI.
           PERFORM  P79200-CLEAR-SCREEN
               THRU P79200-CLEAR-SCREEN-EXIT.

           MOVE PM044-ORDER-DELETED    TO  WMF-MESSAGE-AREA.

           PERFORM  P70100-MESSAGE-ROUTINE
               THRU P70100-MESSAGE-ROUTINE-EXIT.


       P09000-DELETE-PROCESS-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P10000-SCHEDULE-PSB                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO SCHEDULE THE IMS-DLI PSB USED       *
      *                BY THE MODULE TO ACCESS THE ORDER DATABASE     *
      *                                                               *
      *    CALLED BY:  P07000-UPDATE-PROCESS                          *
      *                                                               *
      *****************************************************************

       P10000-SCHEDULE-PSB.


           EXEC DLI  SCHEDULE
                     PSB((WMF-PSB-NAME))
                     NODHABEND
           END-EXEC.


      *****************************************************************
      *    CHECK FOR PSB SCHEDULING ERROR                             *
      *****************************************************************

           IF DIBSTAT    =  SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'Y'                TO WS-ERROR-FOUND-SW
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA010'           TO WPIE-PROGRAM-ID
               MOVE 'P10000'           TO WPIE-PARAGRAPH
               MOVE DIBSTAT            TO WPIE-STATUS-CODE
               MOVE 'SCHD'             TO WPIE-FUNCTION-CODE
               MOVE SPACES             TO WPIE-SEGMENT-NAME
               MOVE SPACES             TO WPIE-DATABASE-NAME
               MOVE 'PSB SCHEDULING ERROR'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P10000-SCHEDULE-PSB-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P10100-TERMINATE-PSB                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO TERMINATE THE IMS-DLI PSB USED      *
      *                BY THE MODULE TO ACCESS THE ORDER DATABASE     *
      *                                                               *
      *    CALLED BY:  P07000-UPDATE-PROCESS                          *
      *                                                               *
      *****************************************************************

       P10100-TERMINATE-PSB.


           EXEC DLI  TERMINATE
           END-EXEC.


      *****************************************************************
      *    CHECK FOR PSB TERMINATION ERROR                            *
      *****************************************************************

           IF DIBSTAT    =  SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'Y'                TO WS-ERROR-FOUND-SW
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA010'           TO WPIE-PROGRAM-ID
               MOVE 'P10100'           TO WPIE-PARAGRAPH
               MOVE DIBSTAT            TO WPIE-STATUS-CODE
               MOVE 'TERM'             TO WPIE-FUNCTION-CODE
               MOVE SPACES             TO WPIE-SEGMENT-NAME
               MOVE SPACES             TO WPIE-DATABASE-NAME
               MOVE 'PSB TERMINATION ERROR'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P10100-TERMINATE-PSB-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P10200-GU-ORDER-SEGMENT                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE ORDER ROOT SEGMENT     *
      *                                                               *
      *    CALLED BY:  P08000-CHANGE-PROCESS                          *
      *                P09000-DELETE-PROCESS                          *
      *                                                               *
      *****************************************************************

       P10200-GU-ORDER-SEGMENT.


           EXEC DLI GU USING
                    PCB         (1)
                    SEGMENT     (ORDER)
                    INTO        (ORDER-SEGMENT)
                    SEGLENGTH   (123)
                    WHERE       (ORDKEY=WMF-ORDER-KEY)
                    FIELDLENGTH (15)
           END-EXEC.


      *****************************************************************
      *    CHECK STATUS CODE FOR SUCCESS, NOT FOUND, ALL OTHERS ARE   *
      *    AN ERROR                                                   *
      *****************************************************************

           IF DIBSTAT    =  SPACES
               NEXT SENTENCE
           ELSE

           IF DIBSTAT    =  'GE'
               MOVE -1                 TO ORDNBRL
               MOVE DFHUNINT           TO ORDNBRA
               MOVE PM023-ORDER-NOT-FOUND
                                       TO WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P10200-GU-ORDER-SEGMENT-EXIT
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA010'           TO WPIE-PROGRAM-ID
               MOVE DIBSTAT            TO WPIE-STATUS-CODE
               MOVE 'GU'               TO WPIE-FUNCTION-CODE
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME
               MOVE 'ORDER1DB'         TO WPIE-DATABASE-NAME
               MOVE 'P10200'           TO WPIE-PARAGRAPH
               MOVE 'GU ORDER SEGMENT' TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P10200-GU-ORDER-SEGMENT-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P10300-REPL-ORDER-SEGMENT                    *
      *                                                               *
      *    FUNCTION :  ROUTINE TO REPLACE THE ORDER ROOT SEGMENT     *
      *                                                               *
      *    CALLED BY:  P08000-CHANGE-PROCESS                          *
      *                                                               *
      *****************************************************************

       P10300-REPL-ORDER-SEGMENT.


           EXEC DLI REPL USING
                    PCB         (1)
                    SEGMENT     (ORDER)
                    FROM        (ORDER-SEGMENT)
                    SEGLENGTH   (123)
           END-EXEC.


      *****************************************************************
      *    CHECK STATUS CODE FOR SUCCESS, ALL OTHERS ARE AN ERROR     *
      *****************************************************************

           IF DIBSTAT    =  SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA010'           TO WPIE-PROGRAM-ID
               MOVE DIBSTAT            TO WPIE-STATUS-CODE
               MOVE 'REPL'             TO WPIE-FUNCTION-CODE
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME
               MOVE 'ORDER1DB'         TO WPIE-DATABASE-NAME
               MOVE 'P10300'           TO WPIE-PARAGRAPH
               MOVE 'REPL ORDER SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P10300-REPL-ORDER-SEGMENT-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P10400-DLET-ORDER-SEGMENT                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DELETE THE ORDER ROOT SEGMENT       *
      *                                                               *
      *    CALLED BY:  P09000-DELETE-PROCESS                          *
      *                                                               *
      *****************************************************************

       P10400-DLET-ORDER-SEGMENT.


           EXEC DLI DLET USING
                    PCB         (1)
                    SEGMENT     (ORDER)
                    FROM        (ORDER-SEGMENT)
                    SEGLENGTH   (123)
           END-EXEC.


      *****************************************************************
      *    CHECK STATUS CODE FOR SUCCESS, ALL OTHERS ARE AN ERROR     *
      *****************************************************************

           IF DIBSTAT    =  SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA010'           TO WPIE-PROGRAM-ID
               MOVE DIBSTAT            TO WPIE-STATUS-CODE
               MOVE 'DLET'             TO WPIE-FUNCTION-CODE
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME
               MOVE 'ORDER1DB'         TO WPIE-DATABASE-NAME
               MOVE 'P10400'           TO WPIE-PARAGRAPH
               MOVE 'DLET ORDER SEGMENT'
                                       TO WPIE-COMMAND
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P10400-DLET-ORDER-SEGMENT-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P20000-CHECK-ORDER-AGE                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DETERMINE IF AN ORDER IS OVERDUE    *
      *                                                               *
      *    CALLED BY:  P08000-CHANGE-PROCESS                          *
      *                                                               *
      *****************************************************************

       P20000-CHECK-ORDER-AGE.

           MOVE SPACES     TO PDAS01-PARMS.
           MOVE ORDNBRO    TO PDAS01-ORDER-NUMBER.
           MOVE ORDDATYO   TO PDAS01-OD-YR.
           MOVE ORDDATMO   TO PDAS01-OD-MONTH.
           MOVE ORDDATDO   TO PDAS01-OD-DAY.

           IF PDAS01-OD-YR > 50
               MOVE 19 TO PDAS01-OD-CE
           ELSE
               MOVE 20 TO PDAS01-OD-CE
           END-IF.

           MOVE ZEROES     TO PDAS01-ORDER-COUNT
                              PDAS01-ORDER-DOLLAR-AMT.

           IF PC-ACTIVE-SCENARIO(13) = 'Y'
               CALL WS-PDAS02 USING PDAS01-PARMS
           ELSE
               CALL 'PDAS01' USING PDAS01-PARMS
           END-IF.

           MOVE PDAS01-AGE-DAYS TO PDAS03-AGE-DAYS.

           CALL 'PDAS03' USING PDAS03-PARMS.

           MOVE PDAS03-MESSAGE TO ORDSTATO.

       P20000-CHECK-ORDER-AGE-EXIT.
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

           MOVE WMF-DATE-MMDDYY        TO PDADATEO.
           MOVE EIBTRMID               TO PDATERMO.
           MOVE WMF-TIME-HHMMSS        TO PDATIMEO.


      *****************************************************************
      *    MAKE FINAL ADJUSTMENTS TO ATTRIBUTES AND FIELDS            *
      *****************************************************************

           INSPECT ACTIONI
                   CONVERTING  WMF-SPACES-LOWVALUE-R TO '__'.

           INSPECT ORDNBRI
                   CONVERTING  WMF-SPACES-LOWVALUE-R TO '__'.

           INSPECT ORDDATMI
                   CONVERTING  WMF-SPACES-LOWVALUE-R TO '__'.

           INSPECT ORDDATDI
                   CONVERTING  WMF-SPACES-LOWVALUE-R TO '__'.

           INSPECT ORDDATYI
                   CONVERTING  WMF-SPACES-LOWVALUE-R TO '__'.


      *****************************************************************
      *    POSITION CURSOR AT APPROPRIATE LOCATION                    *
      *****************************************************************

           IF PC-PREV-PGRMID           =  'PDA010'
               IF NO-ERROR-FOUND
                   MOVE -1             TO ORDNBRL
               ELSE
                   NEXT SENTENCE
           ELSE
                   MOVE -1             TO ORDNBRL.


      *****************************************************************
      *    SEND FULL MAP IF 1ST TIME, OTHERWISE SEND DATAONLY         *
      *****************************************************************

           IF PC-PREV-PGRMID  =  'PDA010'
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
      *    PARAGRAPH:  P79200-CLEAR-SCREEN                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INITIALIZE THE MAP TO DEFAULT       *
      *                VALUES                                         *
      *                                                               *
      *    CALLED BY:  P06000-INQUIRY-PROCESS                         *
      *                P08000-DELETE-PROCESS                          *
      *                                                               *
      *****************************************************************

       P79200-CLEAR-SCREEN.

           MOVE SPACES                 TO PDACUSTO
                                          ORDDATMO
                                          ORDDATDO
                                          ORDDATYO
                                          NAMEO
                                          ORDSTATO
                                          ADDRO
                                          CITYO
                                          TOTAMTI
                                          STATEO
                                          PURTYPEO
                                          PURTDESO
                                          ZIPO
                                          PONBRO
                                          EMAILO
                                          SHNAMEO
                                          SHADDRO
                                          SHCITYO
                                          SHSTATEO
                                          SHZIPO
                                          PDAMSGO.


       P79200-CLEAR-SCREEN-EXIT.
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
                     MAP           ('PDA010')
                     MAPSET        ('PDA010M')
                     FROM          (PDA010O)
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
               MOVE 'PDA010'           TO WPCE-PROGRAM-ID
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
                     MAP           ('PDA010')
                     MAPSET        ('PDA010M')
                     FROM          (PDA010O)
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
               MOVE 'PDA010'           TO WPCE-PROGRAM-ID
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
                     MAP           ('PDA010')
                     MAPSET        ('PDA010M')
                     INTO          (PDA010I)
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
               MOVE 'PDA010'           TO WPCE-PROGRAM-ID
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
               MOVE 'PDA010'           TO WPCE-PROGRAM-ID
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
               MOVE 'PDA010'           TO WPCE-PROGRAM-ID
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
               MOVE 'PDA010'           TO WPCE-PROGRAM-ID
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
           MOVE 'PDA010'               TO WPCE-PROGRAM-ID.
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


           EVALUATE TRUE
               WHEN PC-ACTIVE-SCENARIO(6) = 'Y'
                   EXEC CICS DUMP
                             TRANSACTION
                             DUMPCODE('P10A')
                   END-EXEC
               WHEN PC-ACTIVE-SCENARIO(7) = 'Y'
                   EXEC CICS ABEND
                             ABCODE('P10B')
                   END-EXEC
               WHEN OTHER
                   EXEC CICS DUMP
                             TRANSACTION
                             DUMPCODE('PDER')
                   END-EXEC
           END-EVALUATE.



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
AS OF      MOVE 'PDA010'               TO WPCE-PROGRAM-ID.               AS OF
JAN        MOVE EIBRESP                TO WPCE-RESPONSE-CODE.            JAN
2001       MOVE 'ERROR'                TO WPCE-COMMAND.                  2001
           MOVE 'P99999'               TO WPCE-PARAGRAPH.
LLR                                                                      LLR
           PERFORM  P99500-PDA-ERROR
               THRU P99500-PDA-ERROR-EXIT.

       P99999-ERROR-EXIT.
           EXIT.
           EJECT