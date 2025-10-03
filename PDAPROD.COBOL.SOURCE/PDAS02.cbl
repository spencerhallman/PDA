       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. PDAS02.                                              00020000
      *                                                                 00030000
      ***************************************************************** 00040000
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       * 00050000
      *                       COMPUWARE CORPORATION                   * 00060000
      *                                                               * 00070000
      * PROGRAM :   PDAS02                                            * 00080000
      *                                                               * 00090000
      * FUNCTION:   PROGRAM PDAS02 IS A CALLED SUBROUTINE THAT WILL   * 00100000
      *             ACCEPT A PARM FROM THE CALLING PROGRAM THAT       * 00110000
      *             CONTAINS A DAYS AGING FIELD.  THE PROGRAM WILL    * 00120000
      *             OBTAIN THE CURRENT SYSTEM DATE AND SUBTRACT THE   * 00130000
      *             PASSED NUMBER OF DAYS TO IT TO OBTAIN THE AGED    * 00140000
      *             DATE.  THE CURRENT DATE AND THE AGED DATE WILL    * 00150000
      *             THEN BE RETURNED TO THE CALLING PROGRAM.          * 00160000
      *                                                               * 00170000
      *                                                               * 00171000
      * FILES   :   NONE                                              * 00172000
      *                                                               * 00173000
      * TRANSACTIONS GENERATED:                                       * 00174000
      *             NONE                                              * 00175000
      *                                                               * 00176000
      * PFKEYS  :   NONE                                              * 00177000
      *                                                               * 00178000
      *                                                               * 00179000
      ***************************************************************** 00180000
      *             PROGRAM CHANGE LOG                                * 00190000
      *             -------------------                               * 00200000
      *                                                               * 00210000
      *  DATE       UPDATED BY            CHANGE DESCRIPTION          * 00220000
      *  --------   --------------------  --------------------------  * 00230000
      *                                                               * 00231000
      *  12/13/05   PAUL BARON            ELIMINATE USE OF PDAS02     * 00232001
      *                                   PARAMETER FIELD             * 00233001
      *                                   PDAS01-ORDER-DOLLAR-AMT-R   * 00234001
      *                                                               * 00234101
      *  04/19/02   PAUL BARON            ADDED CALL TO LANGUAGE      * 00234201
      *                                   ENVIRONMENT (LE) ROUTINE    * 00234301
      *                                   CEEGMT (OBTAIN GREENWICH    * 00234401
      *                                   MEAN TIME) SO ABEND AID     * 00235000
      *                                   REPORT CAN SHOW LAST CALL   * 00236000
      *                                   EXECUTED FOR SCENARIO 1     * 00237000
      *                                   (ABEND ASRA)                * 00238000
      *                                                               * 00239000
      *  04/20/01   PAUL BARON            MADE CHANGES FOR SCENARIO 1 * 00240000
      *                                   TO CALCULATE A MONTH TO DATE* 00250000
      *                                   AVERAGE ORDER AMOUNT WHEN   * 00260000
      *                                   ORDER COUNT AND DOLLAR AMT  * 00270000
      *                                   VALUES ARE SUPPLIED.        * 00280000
      *                                   VALUES ARE ONLY SUPPLIED    * 00290000
      *                                   WHEN SCENARIO 1 IS ACTIVE,  * 00300000
      *                                   AND THE VALUES WILL PRODUCE * 00310000
      *                                   AN ABEND ASRA.              * 00320000
      *                                                               * 00330000
      *  XX/XX/XX   XXXXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXX  * 00331000
      *                                                               * 00332000
      ***************************************************************** 00333000
                                                                        00334000
       ENVIRONMENT DIVISION.                                            00335000
       DATA DIVISION.                                                   00336000
           EJECT                                                        00337000
       WORKING-STORAGE SECTION.                                         00338000
                                                                        00339000
      ***************************************************************** 00340000
      *    77 LEVEL DATA ITEMS HERE  (SUBSCRIPTS, INDEXES ETC.)       * 00350000
      ***************************************************************** 00360000
       77  WS-SUB1                     PIC S9(4) COMP VALUE +0.         00370000
                                                                        00380000
      ***************************************************************** 00390000
      *    SWITCHES                                                   * 00400000
      ***************************************************************** 00410000
                                                                        00420000
       01  WS-SWITCHES.                                                 00430000
           05  WS-END-OF-PROCESS-SW    PIC X     VALUE 'N'.             00440000
               88  END-OF-PROCESS                VALUE 'Y'.             00450000
               88  NOT-END-OF-PROCESS            VALUE 'N'.             00460000
           05  WS-ERROR-FOUND-SW       PIC X     VALUE 'N'.             00470000
               88  ERROR-FOUND                   VALUE 'Y'.             00480000
               88  NO-ERROR-FOUND                VALUE 'N'.             00490000
           EJECT                                                        00500000
      ***************************************************************** 00510000
      *    MISCELLANEOUS WORK FIELDS                                  * 00520000
      ***************************************************************** 00530000
                                                                        00540000
       01  WS-MISCELLANEOUS-FIELDS.                                     00550000
           05  WMF-USERID              PIC X(8)  VALUE SPACES.          00560000
           05  WMF-ABSTIME             PIC S9(15) VALUE +0      COMP-3. 00570000
           05  WMF-DATE-MMDDYY         PIC X(08) VALUE SPACES.          00580000
           05  WMF-TIME-HHMMSS         PIC X(08) VALUE SPACES.          00590000
           05  WMF-MESSAGE-AREA        PIC X(79) VALUE SPACES.          00600000
           05  WS-YEARS                PIC 9(4)  VALUE ZEROES.          00610000
           05  WS-REMAIN               PIC 9(4)  VALUE ZEROES.          00620000
           05  WS-JULIAN-DATE          PIC 9(7)  VALUE ZEROES.          00630000
           05  FILLER                  REDEFINES WS-JULIAN-DATE.        00640000
               07  WS-JD-YEAR          PIC 9(4).                        00650000
               07  FILLER              REDEFINES WS-JD-YEAR.            00660000
                   09  FILLER          PIC XX.                          00670000
                   09  WS-JD-YR        PIC XX.                          00680000
               07  WS-JD-DAY           PIC 9(3).                        00690000
           05  WS-ORDER-DATE           PIC 9(7)  VALUE ZEROES.          00700000
           05  FILLER                  REDEFINES WS-ORDER-DATE.         00710000
               07  WS-OD-YEAR          PIC 9(4).                        00720000
               07  FILLER              REDEFINES WS-OD-YEAR.            00730000
                   09  FILLER          PIC XX.                          00740000
                   09  WS-OD-YR        PIC XX.                          00750000
               07  WS-OD-DAY           PIC 9(3).                        00760000
           05  WS-YEAR-TABLE.                                           00770000
               07  FILLER              PIC 9(3)  VALUE 0.               00780000
               07  FILLER              PIC 9(3)  VALUE 31.              00790000
               07  FILLER              PIC 9(3)  VALUE 59.              00800000
               07  FILLER              PIC 9(3)  VALUE 90.              00810000
               07  FILLER              PIC 9(3)  VALUE 120.             00820000
               07  FILLER              PIC 9(3)  VALUE 151.             00830000
               07  FILLER              PIC 9(3)  VALUE 181.             00840000
               07  FILLER              PIC 9(3)  VALUE 212.             00850000
               07  FILLER              PIC 9(3)  VALUE 243.             00860000
               07  FILLER              PIC 9(3)  VALUE 273.             00870000
               07  FILLER              PIC 9(3)  VALUE 304.             00880000
               07  FILLER              PIC 9(3)  VALUE 334.             00890000
           05  FILLER                  REDEFINES WS-YEAR-TABLE.         00900000
               07  WS-DAYS             OCCURS 12 TIMES                  00910000
                                       PIC 9(3).                        00920000
           05  WS-LEAP-YEAR-TABLE.                                      00930000
               07  FILLER              PIC 9(3)  VALUE 0.               00940000
               07  FILLER              PIC 9(3)  VALUE 31.              00950000
               07  FILLER              PIC 9(3)  VALUE 60.              00960000
               07  FILLER              PIC 9(3)  VALUE 91.              00970000
               07  FILLER              PIC 9(3)  VALUE 121.             00980000
               07  FILLER              PIC 9(3)  VALUE 152.             00990000
               07  FILLER              PIC 9(3)  VALUE 182.             01000000
               07  FILLER              PIC 9(3)  VALUE 213.             01010000
               07  FILLER              PIC 9(3)  VALUE 244.             01020000
               07  FILLER              PIC 9(3)  VALUE 274.             01030000
               07  FILLER              PIC 9(3)  VALUE 305.             01040000
               07  FILLER              PIC 9(3)  VALUE 335.             01050000
           05  FILLER                  REDEFINES WS-LEAP-YEAR-TABLE.    01060000
               07  WS-LEAP-DAYS        OCCURS 12 TIMES                  01070000
                                       PIC 9(3).                        01080000
                                                                        01090000
      ***************************************************************** 01100000
      *  THIS AREA CONTAINS THE DATA FROM THE FUNCTION CURRENT-DATE   * 01110000
      ***************************************************************** 01120000
                                                                        01130000
       01  WS-CURRENT-DATE-TIME.                                        01140000
           03  WS-CDT-DATE.                                             01150000
               05  WS-CDT-D-YEAR       PIC 9(4)  VALUE ZEROES.          01160000
               05  WS-CDT-D-MONTH      PIC 99    VALUE ZEROES.          01170000
               05  WS-CDT-D-DAY        PIC 99    VALUE ZEROES.          01180000
           03  WS-CDT-TIME.                                             01190000
               05  WS-CDT-T-HOURS      PIC 99    VALUE ZEROES.          01200000
               05  WS-CDT-T-MINUTES    PIC 99    VALUE ZEROES.          01210000
               05  WS-CDT-T-SECONDS    PIC 99    VALUE ZEROES.          01220000
               05  WS-CDT-T-HUNDRETHS  PIC 99    VALUE ZEROES.          01230000
           03  WS-CDT-GMT-INDICATOR    PIC X     VALUE SPACES.          01240000
               88  AHEAD-OF-GMT                  VALUE '+'.             01250000
               88  BEHIND-GMT                    VALUE '-'.             01260000
               88  GMT-NOT-AVAILABLE             VALUE '0'.             01270000
           03  WS-CDT-GMT-TIME-DIFFERENTIAL.                            01280000
               05  WS-CDT-GMT-HOURS    PIC 99    VALUE ZEROES.          01290000
               05  WS-CDT-GMT-MINUTES  PIC 99    VALUE ZEROES.          01300000
                                                                        01311000
      ***************************************************************** 01312000
      *  LANGUAGE ENVIRONMENT (LE) ROUTINE WORK AREAS                 * 01313000
      ***************************************************************** 01314000
       01  WS-LE-LILIAN-DATE           PIC S9(9) BINARY.                01315000
       01  WS-LE-SECS                  COMP-2.                          01316000
       01  WS-LE-RETURN-CODE.                                           01317000
           02  FILLER                  PIC X(50).                       01318000
                                                                        01319000
      ***************************************************************** 01319200
      *  SUBROUTINE PARAMETER WORK AREAS                              * 01319300
      ***************************************************************** 01319400
           COPY PDAS01CY.                                               01320000
           EJECT                                                        01330000
                                                                        01331000
      ***************************************************************** 01340000
      *    MESSAGES   (ERROR AND INFORMATIONAL)                       * 01350000
      ***************************************************************** 01360000
                                                                        01370000
           COPY PDAMSGS.                                                01380000
           EJECT                                                        01390000
      ***************************************************************** 01400000
      *    GENERAL ERROR PROCESSING WORK AREAS (CICS, IMS-DLI, DB2)   * 01410000
      ***************************************************************** 01420000
                                                                        01430000
           COPY PDAERRWS.                                               01440000
           EJECT                                                        01450000
                                                                        01460000
      ***************************************************************** 01470000
      *    L I N K A G E     S E C T I O N                            * 01480000
      ***************************************************************** 01490000
                                                                        01500000
       LINKAGE SECTION.                                                 01510000
                                                                        01520000
       01  LS-PDAS01-PARMS             PIC X(42).                       01530000
                                                                        01540000
      ***************************************************************** 01550000
      *    P R O C E D U R E    D I V I S I O N                       * 01560000
      ***************************************************************** 01570000
                                                                        01580000
       PROCEDURE DIVISION USING LS-PDAS01-PARMS.                        01590000
                                                                        01600000
                                                                        01610000
       P00000-MAINLINE.                                                 01620000
                                                                        01630000
           MOVE LS-PDAS01-PARMS       TO PDAS01-PARMS.                  01640000
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME.          01650000
                                                                        01651000
           PERFORM P00050-GET-JULIAN-DATE THRU P00050-EXIT.             01651100
                                                                        01670000
           PERFORM P00100-CONVERT-CURRENT-DATE THRU P00100-EXIT.        01671000
                                                                        01672000
           PERFORM P00200-CONVERT-ORDER-DATE THRU P00200-EXIT.          01900100
                                                                        02120300
           PERFORM P01000-CALC-ORDER-AGE THROUGH P01000-EXIT.           02121000
                                                                        02130000
                                                                        02320000
      ***************************************************************** 02330000
      *    CALCULATE ORDER AVERAGE ORDER AMOUNT - IF VALUES SUPPLIED  * 02340000
      *    BY CALLING PROGRAM                                         * 02350000
      ***************************************************************** 02360000
                                                                        02370000
           IF PDAS01-ORDER-COUNT        NOT = ZEROES                    02380001
               DIVIDE PDAS01-ORDER-DOLLAR-AMT                           02400000
                   BY PDAS01-ORDER-COUNT                                02410001
               GIVING PDAS01-AVERAGE-DOLLAR-AMT.                        02420001
                                                                        02450000
                                                                        02460000
      ***************************************************************** 02470000
      *    RETURN RESULTS TO CALLING PROGRAM                          * 02471000
      ***************************************************************** 02472000
                                                                        02473000
           MOVE PDAS01-PARMS TO LS-PDAS01-PARMS.                        02474000
                                                                        02475000
           GOBACK.                                                      02476000
                                                                        02476100
       P00000-MAINLINE-EXIT.                                            02476200
           EXIT.                                                        02476300
           EJECT                                                        02476400
                                                                        02476500
       P00050-GET-JULIAN-DATE.                                          02476600
           CALL 'CEEGMT'               USING WS-LE-LILIAN-DATE,         02476700
                                             WS-LE-SECS,                02476800
                                             WS-LE-RETURN-CODE.         02476900
                                                                        02477000
       P00050-EXIT.                                                     02477100
           EXIT.                                                        02477200
                                                                        02477300
       P00100-CONVERT-CURRENT-DATE.                                     02477400
      ***************************************************************** 02477500
      *    CONVERT CURRENT DATE TO JULIAN                             * 02477600
      ***************************************************************** 02477700
                                                                        02477800
           MOVE WS-CDT-D-YEAR TO WS-JD-YEAR.                            02477900
                                                                        02478000
           IF WS-JD-YR = '00'                                           02478100
               DIVIDE WS-JD-YEAR BY 400 GIVING WS-YEARS                 02478200
                                        REMAINDER WS-REMAIN             02478300
           ELSE                                                         02478400
               DIVIDE WS-JD-YEAR BY 4 GIVING WS-YEARS                   02478500
                                      REMAINDER WS-REMAIN               02478600
           END-IF.                                                      02478700
                                                                        02478800
           IF WS-REMAIN = 0                                             02478900
               MOVE WS-LEAP-DAYS(WS-CDT-D-MONTH) TO WS-JD-DAY           02479000
           ELSE                                                         02479100
               MOVE WS-DAYS(WS-CDT-D-MONTH) TO WS-JD-DAY                02479200
           END-IF.                                                      02479300
                                                                        02479400
           ADD WS-CDT-D-DAY TO WS-JD-DAY.                               02479500
                                                                        02479600
       P00100-EXIT.                                                     02479700
           EXIT.                                                        02479800
                                                                        02479900
       P00200-CONVERT-ORDER-DATE.                                       02480000
      ***************************************************************** 02480100
      *    CONVERT ORDER DATE TO JULIAN                               * 02480200
      ***************************************************************** 02480300
                                                                        02480400
           MOVE PDAS01-OD-YEAR TO WS-OD-YEAR.                           02480500
                                                                        02480600
           IF WS-OD-YR = '00'                                           02480700
               DIVIDE WS-OD-YEAR BY 400 GIVING WS-YEARS                 02480800
                                        REMAINDER WS-REMAIN             02480900
           ELSE                                                         02481000
               DIVIDE WS-OD-YEAR BY 4 GIVING WS-YEARS                   02481100
                                      REMAINDER WS-REMAIN               02481200
           END-IF.                                                      02481300
                                                                        02481400
           IF WS-REMAIN = 0                                             02481500
               MOVE WS-LEAP-DAYS(PDAS01-OD-MONTH) TO WS-OD-DAY          02481600
           ELSE                                                         02481700
               MOVE WS-DAYS(PDAS01-OD-MONTH) TO WS-OD-DAY               02481800
           END-IF.                                                      02481900
                                                                        02482000
           ADD PDAS01-OD-DAY TO WS-OD-DAY.                              02482100
                                                                        02482200
       P00200-EXIT.                                                     02482300
           EXIT.                                                        02482400
                                                                        02482500
       P01000-CALC-ORDER-AGE.                                           02482600
      ***************************************************************** 02482700
      *    CALCULATE HOW OLD THE ORDER IS                             * 02482800
      ***************************************************************** 02482900
                                                                        02483000
           IF WS-JULIAN-DATE NOT > WS-ORDER-DATE                        02483100
               MOVE ZEROES TO PDAS01-AGE-DAYS                           02483200
           ELSE                                                         02483300
               IF WS-JD-YEAR = WS-OD-YEAR                               02483400
                   COMPUTE PDAS01-AGE-DAYS = WS-JULIAN-DATE -           02483500
                                             WS-ORDER-DATE              02483600
               ELSE                                                     02483700
                   SUBTRACT 1 FROM WS-JD-YEAR                           02483800
                   ADD 365 TO WS-JD-DAY                                 02483900
                   COMPUTE PDAS01-AGE-DAYS = WS-JULIAN-DATE -           02484000
                                             WS-ORDER-DATE              02484100
               END-IF                                                   02484200
           END-IF.                                                      02484300
                                                                        02484400
       P01000-EXIT.                                                     02484500
           EXIT.                                                        02485000
           EJECT                                                        02490000