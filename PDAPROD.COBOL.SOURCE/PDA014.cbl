       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA014.

      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA014                                            *
      * TRANS   :   PD14                                              *
      * MAPSET  :   NONE                                              *
      *                                                               *
      * FUNCTION:   PROGRAM PDA014 IS THE PRODUCT DEMONSTRATION       *
      *             APPLICATION SINGLE ORDER INQUIRY PROGRAM.         *
      *             GIVEN AN ORDER NUMBER PASSED IN THE COMMAREA,     *
      *             PDA014 READS THE ORDER DATABASE AND THE CUSTOMER  *
      *             RECORD ASSOCIATED WITH THE ORDER TO FORMAT AND    *
      *             RETURN ORDER AND CUSTOMER INFORMATION TO THE      *
      *             CALLING PROGRAM.                                  *
      *                                                               *
      *             PDA014 IS INVOKED VIA A CICS LINK                 *
      *                                                               *
      *             PDA014 SATISFIES THE UNIFACE REQUIREMENT OF       *
      *             INVOKING A CICS MODULE TO PERFORM FILE READS AND  *
      *             RETURN FILE INFORMATION.                          *
      *                                                               *
      *             ANY ERRORS ENCOUNTERED ARE REPORTED IN THE        *
      *             COMMAREA AND RETURNED TO THE CALLING PROGRAM      *
      *                                                               *
      *                                                               *
      *                                                               *
      * FILES   :   ORDER DATABASE (IMS-DLI)  DBD = ORDER1DB          *
      *                   ORDER (ROOT SEGMENT)               - READ   *
      *                                                               *
      *             CUSTOMER FILE  (VSAM)     DD  = PDACUST  - READ   *
      *                                                               *
      *             USERID         (DB2 TABLE)               - READ   *
      *             PURCHASE_TYPE  (DB2 TABLE)               - READ   *
      *                                                               *
      *                                                               *
      * TRANSACTIONS GENERATED: NONE                                  *
      *                                                               *
      *                                                               *
      * PFKEYS  :   NONE                                              *
      *                                                               *
      *                                                               *
      *****************************************************************
      *             PROGRAM CHANGE LOG                                *
      *             -------------------                               *
      *                                                               *
      *  DATE       UPDATED BY            CHANGE DESCRIPTION          *
      *  --------   --------------------  --------------------------  *
      *  12/13/05   PAUL BARON            ELIMINATE THE USE OF        *
      *                                   REDEFINED FIELDS            *
      *                                   CUSTOMER-TOTAL-DOLLAR-AMT-R *
      *                                   PDAS01-ORDER-DOLLAR-AMT-R   *
      *                                                               *
      *  04/20/01   PAUL BARON            PROGRAM NOW DETERMINES ORDER*
      *                                   AGE (P20000-CHECK-ORDER-AGE)*
      *                                   USING SUBROUTINES PDAS01,   *
      *                                   PDAS02, PDAS03 TO           *
      *                                   ACCOMODATE SCENARIO 1,      *
      *                                   ABEND ASRA. THE ORDER       *
      *                                   STATUS IS RETURNED IN THE   *
      *                                   COMMAREA.                   *
      *                                                               *
      *                                   ADDED CATEGORY/SUB-CATEGORY *
      *                                   ARRAY PROCESSING USING      *
      *                                   SUBSCRIPTS / INDEXES TO BE  *
      *                                   VISIBLE FOR SCENARIO 1,     *
      *                                   ABEND ASRA                  *
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
       77  WS-SUB1                     PIC S9(04)   COMP  VALUE +0.
       77  WS-SUB2                     PIC S9(04)   COMP  VALUE +0.
       77  WS-RESPONSE-CODE            PIC S9(08)   COMP  VALUE +0.
       77  WS-MESSAGE-LTH              PIC S9(4)    COMP  VALUE +79.


      *****************************************************************
      *    SWITCHES                                                   *
      *****************************************************************
       01  WS-SWITCHES.

           05  WS-ERROR-FOUND-SW       PIC X(01)             VALUE 'N'.
               88  ERROR-FOUND                               VALUE 'Y'.
               88  NO-ERROR-FOUND                            VALUE 'N'.
           EJECT
      *****************************************************************
      *    MISCELLANEOUS WORK FIELDS                                  *
      *****************************************************************
       01  WS-MISCELLANEOUS-FIELDS.

           05  WMF-USERID              PIC X(08)   VALUE SPACES.
           05  WMF-USERID-NUMBER       PIC S9(09)  VALUE +0  COMP.
           05  WMF-PSB-NAME            PIC X(08)   VALUE 'PDA014'.
           05  WMF-MESSAGE-AREA        PIC X(79)   VALUE SPACES.

           05  WMF-ORDER-KEY           PIC X(15)   VALUE SPACES.
           05  WMF-ORDER-KEY-R         REDEFINES   WMF-ORDER-KEY.
               10  WMF-ORDER-PREFIX    PIC 9(05).
               10  WMF-ORDER-NUMBER    PIC X(10).

           05  WMF-CUSTOMER-KEY.
               10  WMF-CUSTOMER-PREFIX PIC 9(05)   VALUE ZEROES.
               10  WMF-CUSTOMER-ID     PIC X(32)   VALUE SPACES.

           05  WMF-PURCHASE-TYPE-KEY.
               10  WMF-PURCHASE-PREFIX PIC X(05)   VALUE ZEROES.
               10  WMF-PURCHASE-TYPE   PIC X(03)   VALUE ZEROES.

           05  WMF-DATE-YYMMDD.
               10  WMF-DATE-YY         PIC 9(02)   VALUE ZEROES.
               10  WMF-DATE-MM         PIC 9(02)   VALUE ZEROES.
               10  WMF-DATE-DD         PIC 9(02)   VALUE ZEROES.

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
      *  SUBROUTINE PARAMETER AREAS                                   *
      *****************************************************************

       01  WS-PDAS02                   PIC X(8)    VALUE 'PDAS02'.

           COPY PDAS01CY.

       01  PDAS03-PARMS.
           03  PDAS03-AGE-DAYS         PIC 9(5)    VALUE ZEROES.
           03  PDAS03-MESSAGE          PIC X(15)   VALUE SPACES.


      *****************************************************************
      *    CICS DEFINITIONS                                           *
      *****************************************************************
      *****************************************************************
      *    NO CICS COPYBOOKS USED IN THIS MODULE                      *
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
      *    IMS / DLI DEFINITIONS                                      *
      *****************************************************************

      *****************************************************************
      *    ORDER DATABASE ROOT SEGMENT                                *
      *****************************************************************
           COPY IORDER.
           EJECT

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
      *    USER IDENTIFICATION TABLE (USERID)         DCLGEN DUSERID  *
      *****************************************************************

           EXEC SQL
                INCLUDE DUSERID
           END-EXEC.

           EJECT

      *****************************************************************
      *    PURCHASE TYPE TABLE       (PURCHASE_TYPE)  DCLGEN DPURTYP  *
      *****************************************************************

           EXEC SQL
                INCLUDE DPURTYP
           END-EXEC.

           EJECT


      *****************************************************************
      *    MISCELLANEOUS FILE DEFINITIONS                             *
      *****************************************************************

      *****************************************************************
      *    CUSTOMER VSAM FILE                                         *
      *****************************************************************
           COPY VCUSTOMR.
           EJECT


      *****************************************************************
      *    PRODUCT DEMONSTRATION APPLICATION (PDA) COMMAREA LAYOUT    *
      *****************************************************************

       01  WS-PDA-COMMAREA-WORKAREA.
           05  WPCW-ORDER-NUMBER-IN    PIC X(10).
           05  FILLER                  PIC X(990).

       01  WS-PDA-COMMAREA-WORKAREA-R  REDEFINES
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


      *****************************************************************
      *    PDA CATEGORY / SUB-CATEGORY CONSTANT ARRAY                 *
      *****************************************************************

           COPY PDACATGY.


      *****************************************************************
      *    PDA CATEGORY STATISICS ARRAY                               *
      *****************************************************************

       01  WS-CATEGORY-STATISTICS-ARRAY.
           05  WCSA-CATEGORY-MAX       PIC S9(04)  COMP  VALUE +25.
           05  WCSA-SUBCATEGORY-MAX    PIC S9(04)  COMP  VALUE +50.
           05  WCSA-CATEGORY-GRP       OCCURS  25  TIMES
                                       INDEXED BY  WCSA-CAT-INDEX.
               10  WCSA-CATEGORY-NAME  PIC X(32).
               10  WCSA-CATEGORY-ORDER-COUNT
                                       PIC S9(7)   COMP-3.
               10  WCSA-SUBCATEGORY-GRP OCCURS 50 TIMES
                                       INDEXED BY  WCSA-SUBCAT-INDEX.
                   15  WCSA-SUBCATEGORY-NAME
                                       PIC X(32).
                   15  WCSA-SUBCATEGORY-ORDER-COUNT
                                       PIC S9(07)  COMP-3.
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
      *                ORDER INFORMATION RETRIEVAL PROGRAM            *
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


           PERFORM  P99500-CICS-RETURN
               THRU P99500-CICS-RETURN-EXIT.

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
      *    VERIFY COMMAREA IS PASSED AND CORRECT                      *
      *****************************************************************

           IF EIBCALEN                 NOT = 2000
               MOVE '8'                TO WPCW-ERROR-SW
               MOVE 'PDA014'           TO WPCW-PROGRAM-ID
               MOVE 'P00050'           TO WPCW-PARAGRAPH
               MOVE ZEROES             TO WPCW-RESPONSE-CODE
               MOVE 'COMMAREA LENGTH NOT CORRECT'
                                       TO WPCW-COMMAND
               PERFORM  P99600-COMMAREA-ERROR
                   THRU P99600-COMMAREA-ERROR-EXIT.


      *****************************************************************
      *    COMMAREA PROGRAM WORK AREA CONTAINS ALL INFO WE NEED       *
      *****************************************************************

           MOVE PC-PROGRAM-WORKAREA    TO WS-PDA-COMMAREA-WORKAREA.
           MOVE 'N'                    TO WS-ERROR-FOUND-SW.
                                                                        00010000
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME.          00020001


      *****************************************************************
      *    INITIALIZE AND LOAD THE CATEGORY STATISTICS ARRAY          *
      *****************************************************************

PWB423     PERFORM  P00060-INIT-CAT-ARRAY
PWB423         THRU P00060-INIT-CAT-ARRAY-EXIT
PWB423             VARYING WCSA-CAT-INDEX FROM +1 BY +1
PWB423                 UNTIL WCSA-CAT-INDEX >  WCSA-CATEGORY-MAX.


           SET WCSA-CAT-INDEX          TO 1.

           PERFORM  P00070-LOAD-CAT-ARRAY                               PWB423
               THRU P00070-LOAD-CAT-ARRAY-EXIT                          PWB423
                   VARYING WS-SUB1 FROM +1 BY +1                        PWB423
                       UNTIL WS-SUB1  >  PDA-CATEGORY-MAX.              PWB423

       P00050-INITIALIZE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00060-INIT-CAT-ARRAY                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INITIALIZE THE CATEGORY /           *
      *                SUB-CATEGORY ARRAY TO DEFAULT VALUES           *
      *                                                               *
      *    CALLED BY:  P00050-INITIALIZE                              *
      *                                                               *
      *****************************************************************

       P00060-INIT-CAT-ARRAY.


           MOVE SPACES                 TO WCSA-CATEGORY-NAME
                                                 (WCSA-CAT-INDEX).
           MOVE ZEROES                 TO WCSA-CATEGORY-ORDER-COUNT
                                                 (WCSA-CAT-INDEX).
                                                                        00010000

           PERFORM  P00065-INIT-SUBCAT-ARRAY
               THRU P00065-INIT-SUBCAT-ARRAY-EXIT
                   VARYING WCSA-SUBCAT-INDEX FROM +1 BY +1
                       UNTIL WCSA-SUBCAT-INDEX >  WCSA-SUBCATEGORY-MAX.

       P00060-INIT-CAT-ARRAY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00065-INIT-SUBCAT-ARRAY                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INITIALIZE THE SUB-CATEGORY PORTION *
      *                OF THE CATEGORY ARRAY (2ND DIMENSION)          *
      *                                                               *
      *    CALLED BY:  P00060-INIT-CAT-ARRAY                          *
      *                                                               *
      *****************************************************************

       P00065-INIT-SUBCAT-ARRAY.


           MOVE SPACES                 TO WCSA-SUBCATEGORY-NAME
                                (WCSA-CAT-INDEX, WCSA-SUBCAT-INDEX).
           MOVE ZEROES                 TO WCSA-SUBCATEGORY-ORDER-COUNT
                                (WCSA-CAT-INDEX, WCSA-SUBCAT-INDEX).

       P00065-INIT-SUBCAT-ARRAY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00070-LOAD-CAT-ARRAY                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO LOAD THE CATEGORY / SUB-CATEGORY    *
      *                ARRAY FROM THE PRODUCT DEMONSTRATION           *
      *                APPLICATION (PDA) SYSTEM FIXED CATEGORIES ARRAY*
      *                                                               *
      *    CALLED BY:  P00050-INITIALIZE                              *
      *                                                               *
      *****************************************************************

       P00070-LOAD-CAT-ARRAY.


           MOVE PCAR-CATEGORY (WS-SUB1)
                                       TO WCSA-CATEGORY-NAME
                                                 (WCSA-CAT-INDEX).

           MOVE ZEROES                 TO WCSA-CATEGORY-ORDER-COUNT
                                                 (WCSA-CAT-INDEX).

           SET WCSA-SUBCAT-INDEX       TO 1.

           PERFORM  P00075-LOAD-SUBCAT-ARRAY
               THRU P00075-LOAD-SUBCAT-ARRAY-EXIT
                   VARYING WS-SUB2 FROM +1 BY +1
                       UNTIL WS-SUB2  >  PCAR-SUB-CATEGORY-COUNT
                                                           (WS-SUB1).


           IF WS-SUB1                  <  PDA-CATEGORY-MAX
               SET WCSA-CAT-INDEX      UP BY 1.


       P00070-LOAD-CAT-ARRAY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00075-LOAD-SUBCAT-ARRAY                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO LOAD THE SUB-CATEGORY PORTION OF THE*
      *                CATEGORY ARRAY (2ND DIMENSION) FROM THE        *
      *                APPLICATION (PDA) SYSTEM FIXED CATEGORIES ARRAY*
      *                                                               *
      *    CALLED BY:  P00070-LOAD-CAT-ARRAY                          *
      *                                                               *
      *****************************************************************

       P00075-LOAD-SUBCAT-ARRAY.


           MOVE PCAR-SUB-CATEGORY (WS-SUB1, WS-SUB2)
                                       TO WCSA-SUBCATEGORY-NAME
                                  (WCSA-CAT-INDEX, WCSA-SUBCAT-INDEX).

           MOVE ZEROES                 TO WCSA-SUBCATEGORY-ORDER-COUNT
                                  (WCSA-CAT-INDEX, WCSA-SUBCAT-INDEX).


           IF WS-SUB2                  <  PDA-SUB-CATEGORY-MAX
               SET WCSA-SUBCAT-INDEX   UP BY 1.


       P00075-LOAD-SUBCAT-ARRAY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00100-MAIN-PROCESS                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE HIGH LEVEL PROCESSING   *
      *                FOR THE PROGRAM                                *
      *                                                               *
      *    CALLED BY:  P00000-MAINLINE                                *
      *                                                               *
      *****************************************************************

       P00100-MAIN-PROCESS.


      *****************************************************************
      *    VERIFY USERID, OBTAIN USERID UNIQUE IDENTIFIER FOR         *
      *    FILE READS  --- IF ERROR, EXIT THE PROCESS                 *
      *****************************************************************

           PERFORM  P00200-VERIFY-USERID
               THRU P00200-VERIFY-USERID-EXIT.

           IF ERROR-FOUND
               GO TO P00100-MAIN-PROCESS-EXIT.


      *****************************************************************
      *    SCHEDULE THE PSB TO BE USED (PDA014)                       *
      *****************************************************************

           PERFORM  P00500-SCHEDULE-PSB
               THRU P00500-SCHEDULE-PSB-EXIT.

           IF ERROR-FOUND
               GO TO P00100-MAIN-PROCESS-EXIT.


      *****************************************************************
      *    PROCESS THE ORDER DATABASE FOR THE ORDER NUMBER SUPPLIED   *
      *    IN THE COMMAREA                                            *
      *****************************************************************

           PERFORM  P02000-ORDER-PROCESS
               THRU P02000-ORDER-PROCESS-EXIT.


      *****************************************************************
      *    TERMINATE THE PSB                                          *
      *****************************************************************

           PERFORM  P00600-TERMINATE-PSB
               THRU P00600-TERMINATE-PSB-EXIT.


       P00100-MAIN-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00200-VERIFY-USERID                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PERFORM USERID VERIFICATION:        *
      *                                                               *
      *                1) OBTAIN CICS USER ID (SIGNON VALUE)          *
      *                                                               *
      *                2) RETRIEVE USER FROM USERID TABLE (DB2)       *
      *                                                               *
      *                                                               *
      *    CALLED BY:  P00100-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P00200-VERIFY-USERID.

      *****************************************************************
      *    OBTAIN THE USERID (CICS SIGN ON ID)                        *
      *****************************************************************

           EXEC CICS ASSIGN
                     USERID        (WMF-USERID)
                     NOHANDLE
                     RESP          (WS-RESPONSE-CODE)
           END-EXEC.


           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               NEXT SENTENCE
           ELSE
               MOVE 'Y'                TO WS-ERROR-FOUND-SW
               MOVE '8'                TO WPCW-ERROR-SW
               MOVE 'PDA014'           TO WPCW-PROGRAM-ID
               MOVE 'P00200'           TO WPCW-PARAGRAPH
               MOVE WS-RESPONSE-CODE   TO WPCW-RESPONSE-CODE
               MOVE 'ASSIGN USERID ERROR'
                                       TO WPCW-COMMAND
               GO TO P00200-VERIFY-USERID-EXIT.


      *****************************************************************
      *    RETRIEVE USERID UNIQUE IDENTIFIER FROM DB2 TABLE (USERID)  *
      *****************************************************************

           EXEC SQL SELECT    ID,
                              NUMBER

                    INTO      :USERID-ID,
                              :USERID-NUMBER

                    FROM      USERID

                    WHERE     ID = :WMF-USERID
           END-EXEC.


           IF SQLCODE                  =  ZEROES
               MOVE USERID-NUMBER      TO WMF-USERID-NUMBER
           ELSE
           IF SQLCODE                  =  +100
               MOVE 'Y'                TO WS-ERROR-FOUND-SW
               MOVE '3'                TO WPCW-ERROR-SW
               GO TO P00200-VERIFY-USERID-EXIT
           ELSE
               MOVE 'Y'                TO WS-ERROR-FOUND-SW
               MOVE '7'                TO WPCW-ERROR-SW
               MOVE 'PDA014'           TO WPCW-PROGRAM-ID
               MOVE 'P00200'           TO WPCW-PARAGRAPH
               MOVE SQLCODE            TO WPCW-SQLCODE
               MOVE 'SELECT USERID-ID FROM USERID'
                                       TO WPCW-COMMAND
               GO TO P00200-VERIFY-USERID-EXIT.


       P00200-VERIFY-USERID-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00500-SCHEDULE-PSB                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO SCHEDULE THE IMS-DLI PSB USED       *
      *                BY THE MODULE TO ACCESS THE ORDER DATABASE     *
      *                                                               *
      *    CALLED BY:  P00100-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P00500-SCHEDULE-PSB.


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
               MOVE '9'                TO WPCW-ERROR-SW
               MOVE 'PDA014'           TO WPCW-PROGRAM-ID
               MOVE 'P00500'           TO WPCW-PARAGRAPH
               MOVE 'SCHD'             TO WPCW-FUNCTION
               MOVE SPACES             TO WPCW-SEGMENT
               MOVE SPACES             TO WPCW-DATABASE
               MOVE DIBSTAT            TO WPCW-STATUS-CODE
               MOVE 'PSB SCHEDULING ERROR'
                                       TO WPCW-COMMAND
               GO TO P00500-SCHEDULE-PSB-EXIT.


       P00500-SCHEDULE-PSB-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00600-TERMINATE-PSB                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO TERMINATE THE IMS-DLI PSB USED      *
      *                BY THE MODULE TO ACCESS THE ORDER DATABASE     *
      *                                                               *
      *    CALLED BY:  P00100-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P00600-TERMINATE-PSB.


           EXEC DLI  TERMINATE
           END-EXEC.


      *****************************************************************
      *    CHECK FOR PSB TERMINATION ERROR                            *
      *****************************************************************

           IF DIBSTAT    =  SPACES
               NEXT SENTENCE
           ELSE
               MOVE 'Y'                TO WS-ERROR-FOUND-SW
               MOVE '9'                TO WPCW-ERROR-SW
               MOVE 'PDA014'           TO WPCW-PROGRAM-ID
               MOVE 'P00600'           TO WPCW-PARAGRAPH
               MOVE 'TERM'             TO WPCW-FUNCTION
               MOVE SPACES             TO WPCW-SEGMENT
               MOVE SPACES             TO WPCW-DATABASE
               MOVE DIBSTAT            TO WPCW-STATUS-CODE
               MOVE 'PSB TERMINATION ERROR'
                                       TO WPCW-COMMAND
               GO TO P00600-TERMINATE-PSB-EXIT.


       P00600-TERMINATE-PSB-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P02000-ORDER-PROCESS                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE PROCESSING FOR THE      *
      *                ORDER AND CUSTOMER INFORMATION RETRIEVAL,      *
      *                BASED ON THE ORDER NUMBER PASSED TO THE        *
      *                PROGRAM IN THE COMMAREA                        *
      *                                                               *
      *    CALLED BY:  P00100-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P02000-ORDER-PROCESS.

      *****************************************************************
      *    READ THE ORDER ROOT SEGMENT                                *
      *****************************************************************

           MOVE WMF-USERID-NUMBER      TO WMF-ORDER-PREFIX.
           MOVE WPCW-ORDER-NUMBER      TO WMF-ORDER-NUMBER.

           PERFORM  P02100-GU-ORDER-SEGMENT
               THRU P02100-GU-ORDER-SEGMENT-EXIT.

           IF ERROR-FOUND
               GO TO P02000-ORDER-PROCESS-EXIT.


      *****************************************************************
      *    READ THE CUSTOMER INFORMATION USING THE CUSTOMER ID        *
      *    FROM THE ORDER                                             *
      *****************************************************************

           MOVE ORDER-CUSTOMER-KEY     TO WMF-CUSTOMER-KEY.

           IF PC-ACTIVE-SCENARIO(1)    =  'Y'
               MOVE 99999              TO WMF-CUSTOMER-PREFIX.

           PERFORM  P02200-READ-CUSTOMER
               THRU P02200-READ-CUSTOMER-EXIT.

           IF ERROR-FOUND
               GO TO P02000-ORDER-PROCESS-EXIT.


      *****************************************************************
      *    FORMAT ORDER AND CUSTOMER INFORMATION INTO THE COMMAREA    *
      *    TO BE RETURNED TO THE CALLING PROGRAM                      *
      *****************************************************************

           PERFORM  P02500-FORMAT-ORDER-INFO
               THRU P02500-FORMAT-ORDER-INFO-EXIT.


       P02000-ORDER-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P02100-GU-ORDER-SEGMENT                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE ORDER ROOT SEGMENT     *
      *                USING THE SUPPLIED ORDER NUMBER (COMMAREA)     *
      *                                                               *
      *    CALLED BY:  P02000-ORDER-PROCESS                           *
      *                                                               *
      *****************************************************************

       P02100-GU-ORDER-SEGMENT.


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
               MOVE 'Y'                TO WS-ERROR-FOUND-SW
               MOVE '1'                TO WPCW-ERROR-SW
               GO TO P02100-GU-ORDER-SEGMENT-EXIT
           ELSE
               MOVE 'Y'                TO WS-ERROR-FOUND-SW
               MOVE '9'                TO WPCW-ERROR-SW
               MOVE 'PDA014'           TO WPCW-PROGRAM-ID
               MOVE 'P02100'           TO WPCW-PARAGRAPH
               MOVE 'GU'               TO WPCW-FUNCTION
               MOVE 'ORDER'            TO WPCW-SEGMENT
               MOVE 'ORDER1DB'         TO WPCW-DATABASE
               MOVE DIBSTAT            TO WPCW-STATUS-CODE
               MOVE 'GU ORDER ROOT SEGMENT'
                                       TO WPCW-COMMAND
               GO TO P02100-GU-ORDER-SEGMENT-EXIT.


       P02100-GU-ORDER-SEGMENT-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P02200-READ-CUSTOMER                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE CUSTOMER INFORMATION   *
      *                RELATED TO THE ORDER BEING PROCESSED           *
      *                                                               *
      *    CALLED BY:  P02000-ORDER-PROCESS                           *
      *                                                               *
      *****************************************************************

       P02200-READ-CUSTOMER.


           EXEC CICS READ
                     DATASET ('PDACUST')
                     INTO    (CUSTOMER-RECORD)
                     RIDFLD  (WMF-CUSTOMER-KEY)
                     UPDATE
                     NOHANDLE
                     RESP    (WS-RESPONSE-CODE)
           END-EXEC.


      *****************************************************************
      *    CHECK RETURN CODE FOR SUCCESS, NOT FOUND, ALL OTHERS ARE   *
      *    AN ERROR                                                   *
      *****************************************************************

           IF WS-RESPONSE-CODE         =  DFHRESP(NORMAL)
               NEXT SENTENCE
           ELSE
           IF WS-RESPONSE-CODE         =  DFHRESP(NOTFND)
               MOVE 'Y'                TO WS-ERROR-FOUND-SW
               MOVE '2'                TO WPCW-ERROR-SW
               GO TO P02200-READ-CUSTOMER-EXIT
           ELSE
               MOVE 'Y'                TO WS-ERROR-FOUND-SW
               MOVE '8'                TO WPCW-ERROR-SW
               MOVE 'PDA014'           TO WPCW-PROGRAM-ID
               MOVE 'P02200'           TO WPCW-PARAGRAPH
               MOVE WS-RESPONSE-CODE   TO WPCW-RESPONSE-CODE
               MOVE 'ERROR READING PDACUST FILE'
                                       TO WPCW-COMMAND
               GO TO P02200-READ-CUSTOMER-EXIT.



       P02200-READ-CUSTOMER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P02500-FORMAT-ORDER-INFO                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO FORMAT THE ORDER AND CUSTOMER       *
      *                INFORMATION INTO THE COMMAREA TO BE RETURNED   *
      *                TO THE CALLING PROGRAM                         *
      *                                                               *
      *    CALLED BY:  P02000-ORDER-PROCESS                           *
      *                                                               *
      *****************************************************************

       P02500-FORMAT-ORDER-INFO.

      *****************************************************************
      *    FORMAT PRIMARY ORDER INFORMATION                           *
      *****************************************************************

           MOVE ORDER-PURCHASE-NUMBER  TO  WPCW-ORDER-PURCHASE-NUMBER.
           MOVE ORDER-DATE-YYMMDD      TO  WPCW-ORDER-DATE-YYMMDD.
           MOVE ORDER-TOTAL-AMOUNT     TO  WPCW-ORDER-TOTAL-AMOUNT.

           MOVE ORDER-DATE-YYMMDD      TO  WMF-DATE-YYMMDD.
PWB423     PERFORM P20000-CHECK-ORDER-AGE
PWB423        THRU P20000-CHECK-ORDER-AGE-EXIT.


      *****************************************************************
      *    FORMAT CUSTOMER NAME / ADDRESS AND SHIPPING INFORMATION    *
      *****************************************************************

           MOVE CUSTOMER-ID            TO  WPCW-ORDER-CUSTOMER-ID.
           MOVE CUSTOMER-NAME          TO  WPCW-ORDER-CUSTOMER-NAME.
           MOVE CUSTOMER-ADDRESS       TO  WPCW-ORDER-CUSTOMER-ADDRESS.
           MOVE CUSTOMER-CITY          TO  WPCW-ORDER-CUSTOMER-CITY.
           MOVE CUSTOMER-STATE         TO  WPCW-ORDER-CUSTOMER-STATE.
           MOVE CUSTOMER-POSTAL-CODE   TO  WPCW-ORDER-CUSTOMER-POSTAL.
           MOVE CUSTOMER-EMAIL-ADDRESS TO  WPCW-ORDER-CUSTOMER-EMAIL.

           MOVE CUSTOMER-SHIP-TO-NAME  TO  WPCW-ORDER-SHIP-TO-NAME.
           MOVE CUSTOMER-SHIP-TO-ADDRESS
                                       TO  WPCW-ORDER-SHIP-TO-ADDRESS.
           MOVE CUSTOMER-SHIP-TO-CITY  TO  WPCW-ORDER-SHIP-TO-CITY.
           MOVE CUSTOMER-SHIP-TO-STATE TO  WPCW-ORDER-SHIP-TO-STATE.
           MOVE CUSTOMER-SHIP-TO-POSTAL-CODE
                                       TO  WPCW-ORDER-SHIP-TO-POSTAL.


      *****************************************************************
      *    FORMAT PURCHASE TYPE INFORMATION                           *
      *****************************************************************

           MOVE ORDER-PURCHASE-TYPE    TO  WPCW-ORDER-PURCHASE-TYPE.
           MOVE ORDER-PURCHASE-TYPE-KEY
                                       TO  WMF-PURCHASE-TYPE-KEY.

           PERFORM  P02800-GET-PURCHASE-DESC
               THRU P02800-GET-PURCHASE-DESC-EXIT.


           IF SQLCODE  =  ZEROES
               MOVE PURCHASE-TYPE-DESCRIPTION
                                    TO  WPCW-ORDER-PURCHASE-TYPE-DESC
           ELSE
           IF SQLCODE  =  +100
               MOVE 'NOT FOUND'      TO  WPCW-ORDER-PURCHASE-TYPE-DESC
           ELSE
               GO TO P02500-FORMAT-ORDER-INFO-EXIT.


       P02500-FORMAT-ORDER-INFO-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P02800-GET-PURCHASE-DESC                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO OBTAIN THE PURCHASE TYPE            *
      *                DESCRIPTION USING THE PURCHASE TYPE RESIDING   *
      *                IN THE ORDER ROOT SEGMENT                      *
      *                                                               *
      *                PURCHASE TYPE IS A DB2 TABLE                   *
      *                                                               *
      *                                                               *
      *    CALLED BY:  P02500-FORMAT-ORDER-INFO                       *
      *                                                               *
      *****************************************************************

       P02800-GET-PURCHASE-DESC.


           EXEC SQL SELECT    PREFIX,
                              TYPE,
                              DESCRIPTION

                    INTO      :PURCHASE-TYPE-PREFIX,
                              :PURCHASE-TYPE-TYPE,
                              :PURCHASE-TYPE-DESCRIPTION

                    FROM      PURCHASE_TYPE

                    WHERE     PREFIX = :WMF-PURCHASE-PREFIX  AND
                              TYPE   = :WMF-PURCHASE-TYPE
           END-EXEC.


           IF SQLCODE                  =  ZEROES   OR  +100
               NEXT SENTENCE
           ELSE
               MOVE 'Y'                TO WS-ERROR-FOUND-SW
               MOVE '7'                TO WPCW-ERROR-SW
               MOVE 'PDA014'           TO WPCW-PROGRAM-ID
               MOVE 'P02800'           TO WPCW-PARAGRAPH
               MOVE SQLCODE            TO WPCW-SQLCODE
               MOVE 'SELECT FROM PURCHASE_TYPE'
                                       TO WPCW-COMMAND
               GO TO P02800-GET-PURCHASE-DESC-EXIT.


       P02800-GET-PURCHASE-DESC-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P20000-CHECK-ORDER-AGE                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DETERMINE IF AN ORDER IS OVERDUE    *
      *                                                               *
      *    CALLED BY:  P02500-FORMAT-ORDER-INFO                       *
      *                                                               *
      *****************************************************************

PWB423 P20000-CHECK-ORDER-AGE.                                          PWB423
                                                                        ADDED
PWB423     MOVE SPACES                 TO PDAS01-PARMS.                 ROUTINE
PWB423     MOVE WPCW-ORDER-NUMBER      TO PDAS01-ORDER-NUMBER.          TO CHECK
PWB423     MOVE WMF-DATE-YY            TO PDAS01-OD-YR.                 ORDER
PWB423     MOVE WMF-DATE-MM            TO PDAS01-OD-MONTH.              AGE
PWB423     MOVE WMF-DATE-DD            TO PDAS01-OD-DAY.                PWB423

           IF PDAS01-OD-YR > 50
               MOVE 19 TO PDAS01-OD-CE
           ELSE
               MOVE 20 TO PDAS01-OD-CE
           END-IF.

           MOVE ZEROES                 TO PDAS01-ORDER-COUNT
                                          PDAS01-ORDER-DOLLAR-AMT.

           IF PC-ACTIVE-SCENARIO(1) = 'Y'
               MOVE CUSTOMER-TOTAL-ORDER-COUNT
                                       TO PDAS01-ORDER-COUNT
               MOVE CUSTOMER-TOTAL-DOLLAR-AMT-GRP
                                       TO PDAS01-ORDER-DOLLAR-AMT-GRP
           ELSE
               NEXT SENTENCE.


           IF PC-ACTIVE-SCENARIO(13) = 'Y'
               CALL WS-PDAS02 USING PDAS01-PARMS
           ELSE
               CALL 'PDAS01' USING PDAS01-PARMS
           END-IF.


           MOVE PDAS01-AGE-DAYS TO PDAS03-AGE-DAYS.

           CALL 'PDAS03' USING PDAS03-PARMS.

           MOVE PDAS03-MESSAGE TO WPCW-ORDER-STATUS.

       P20000-CHECK-ORDER-AGE-EXIT.
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

           MOVE 'Y'                TO WS-ERROR-FOUND-SW
           MOVE '8'                TO WPCW-ERROR-SW
           MOVE 'PDA014'           TO WPCW-PROGRAM-ID
           MOVE 'P99100'           TO WPCW-PARAGRAPH
           MOVE EIBRESP            TO WPCW-RESPONSE-CODE
           MOVE 'UNHANDLED CICS ERROR'
                                   TO WPCW-COMMAND.


           PERFORM  P99500-CICS-RETURN
               THRU P99500-CICS-RETURN-EXIT.


       P99100-GENERAL-ERROR-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P99500-CICS-RETURN                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETURN CONTROL TO THE CALLING       *
      *                PROGRAM.                                       *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************

       P99500-CICS-RETURN.

      *****************************************************************
      *    SUSPEND ANY HANDLE CONDITIONS IN EFFECT                    *
      *****************************************************************

           EXEC CICS PUSH HANDLE
           END-EXEC.


      *****************************************************************
      *    ATTEMPT RETURN, TRANSACTION WILL ABEND IF UNSUCCESSFUL     *
      *****************************************************************

           MOVE WS-PDA-COMMAREA-WORKAREA
                                       TO PC-PROGRAM-WORKAREA.


           EXEC CICS RETURN
           END-EXEC.


       P99500-CICS-RETURN-EXIT.
           EXIT.
           EJECT


      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P99600-COMMAREA-ERROR                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO ABEND THE TRANSACTION WHEN EITHER   *
      *                NO COMMAREA OR COMMAREA NOT OF CORRECT LENGTH  *
      *                CONDITIONS OCCUR IN THE PROGRAM                *
      *                                                               *
      *                                                               *
      *    CALLED BY:  P00050-INITIALIZE                              *
      *                                                               *
      *****************************************************************

       P99600-COMMAREA-ERROR.

      *****************************************************************
      *    SUSPEND ANY HANDLE CONDITIONS IN EFFECT                    *
      *****************************************************************

           EXEC CICS PUSH HANDLE
           END-EXEC.


           EXEC CICS DUMP
                     TRANSACTION
                     DUMPCODE('PDER')
           END-EXEC.


      *****************************************************************
      *    ATTEMPT RETURN, TRANSACTION WILL ABEND IF UNSUCCESSFUL     *
      *****************************************************************

           EXEC CICS ABEND
                     ABCODE('CAER')
           END-EXEC.


       P99600-COMMAREA-ERROR-EXIT.
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
AS OF      MOVE 'PDA014'               TO WPCE-PROGRAM-ID.               AS OF
JAN        MOVE EIBRESP                TO WPCE-RESPONSE-CODE.            JAN
2001       MOVE 'ERROR'                TO WPCE-COMMAND.                  2001
           MOVE 'P99999'               TO WPCE-PARAGRAPH.
LLR                                                                      LLR
       P99999-ERROR-EXIT.
           EXIT.
           EJECT