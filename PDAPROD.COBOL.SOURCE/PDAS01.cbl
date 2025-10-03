       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. PDAS01.                                              00020000
      *                                                                 00030000
      ***************************************************************** 00040000
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       * 00050000
      *                       COMPUWARE CORPORATION                   * 00060000
      *                                                               * 00070000
      * PROGRAM :   PDAS01                                            * 00080000
      *                                                               * 00090000
      * FUNCTION:   PROGRAM PDAS01 IS A CALLED SUBROUTINE THAT WILL   * 00100000
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
      *                                                               * 00240000
      *  12/12/05   PAUL BARON            SHERYL KING, CATHY RAZUMICH * 00250000
      *                                   WANT MORE REALISTIC SCENARIO* 00260000
      *                                   #1. ELIMINATE DEFINITION AND* 00270002
      *                                   USE OF REDEFINED FIELD      * 00280002
      *                                   PDAS01-ORDER-DOLLAR-AMT-R.  * 00290000
      *                                   MANIPULATION USING ALPHA /  * 00300000
      *                                   NUMERIC DEFINITION LOOKED   * 00310000
      *                                   CONTRIVED.                  * 00310100
      *                                                               * 00310200
      *  04/19/02   PAUL BARON            ADDED CALL TO LANGUAGE      * 00310300
      *                                   ENVIRONMENT (LE) ROUTINE    * 00310400
      *                                   CEEGMT (OBTAIN GREENWICH    * 00310500
      *                                   MEAN TIME) SO ABEND AID     * 00310600
      *                                   REPORT CAN SHOW LAST CALL   * 00310700
      *                                   EXECUTED FOR SCENARIO 1     * 00310800
      *                                   (ABEND ASRA)                * 00310900
      *                                                               * 00311000
      *  04/20/01   PAUL BARON            MADE CHANGES FOR SCENARIO 1 * 00312000
      *                                   TO CALCULATE A MONTH TO DATE* 00313000
      *                                   AVERAGE ORDER AMOUNT WHEN   * 00314000
      *                                   ORDER COUNT AND DOLLAR AMT  * 00315000
      *                                   VALUES ARE SUPPLIED.        * 00316000
      *                                   VALUES ARE ONLY SUPPLIED    * 00317000
      *                                   WHEN SCENARIO 1 IS ACTIVE,  * 00318000
      *                                   AND THE VALUES WILL PRODUCE * 00319000
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
           05  WMF-ACCEPT-DATE-YYMMDD  PIC 9(06) VALUE ZEROES.          00600000
           05  WMF-MESSAGE-AREA        PIC X(79) VALUE SPACES.          00610000
           05  WS-YEARS                PIC 9(4)  VALUE ZEROES.          00620000
           05  WS-REMAIN               PIC 9(4)  VALUE ZEROES.          00630000
           05  WS-JULIAN-DATE          PIC 9(7)  VALUE ZEROES.          00640000
           05  FILLER                  REDEFINES WS-JULIAN-DATE.        00650000
               07  WS-JD-YEAR          PIC 9(4).                        00660000
               07  FILLER              REDEFINES WS-JD-YEAR.            00670000
                   09  FILLER          PIC XX.                          00680000
                   09  WS-JD-YR        PIC XX.                          00690000
               07  WS-JD-DAY           PIC 9(3).                        00700000
           05  WS-ORDER-DATE           PIC 9(7)  VALUE ZEROES.          00710000
           05  FILLER                  REDEFINES WS-ORDER-DATE.         00720000
               07  WS-OD-YEAR          PIC 9(4).                        00730000
               07  FILLER              REDEFINES WS-OD-YEAR.            00740000
                   09  FILLER          PIC XX.                          00750000
                   09  WS-OD-YR        PIC XX.                          00760000
               07  WS-OD-DAY           PIC 9(3).                        00770000
           05  WS-YEAR-TABLE.                                           00780000
               07  FILLER              PIC 9(3)  VALUE 0.               00790000
               07  FILLER              PIC 9(3)  VALUE 31.              00800000
               07  FILLER              PIC 9(3)  VALUE 59.              00810000
               07  FILLER              PIC 9(3)  VALUE 90.              00820000
               07  FILLER              PIC 9(3)  VALUE 120.             00830000
               07  FILLER              PIC 9(3)  VALUE 151.             00840000
               07  FILLER              PIC 9(3)  VALUE 181.             00850000
               07  FILLER              PIC 9(3)  VALUE 212.             00860000
               07  FILLER              PIC 9(3)  VALUE 243.             00870000
               07  FILLER              PIC 9(3)  VALUE 273.             00880000
               07  FILLER              PIC 9(3)  VALUE 304.             00890000
               07  FILLER              PIC 9(3)  VALUE 334.             00900000
           05  FILLER                  REDEFINES WS-YEAR-TABLE.         00910000
               07  WS-DAYS             OCCURS 12 TIMES                  00920000
                                       PIC 9(3).                        00930000
           05  WS-LEAP-YEAR-TABLE.                                      00940000
               07  FILLER              PIC 9(3)  VALUE 0.               00950000
               07  FILLER              PIC 9(3)  VALUE 31.              00960000
               07  FILLER              PIC 9(3)  VALUE 60.              00970000
               07  FILLER              PIC 9(3)  VALUE 91.              00980000
               07  FILLER              PIC 9(3)  VALUE 121.             00990000
               07  FILLER              PIC 9(3)  VALUE 152.             01000000
               07  FILLER              PIC 9(3)  VALUE 182.             01010000
               07  FILLER              PIC 9(3)  VALUE 213.             01020000
               07  FILLER              PIC 9(3)  VALUE 244.             01030000
               07  FILLER              PIC 9(3)  VALUE 274.             01040000
               07  FILLER              PIC 9(3)  VALUE 305.             01050000
               07  FILLER              PIC 9(3)  VALUE 335.             01060000
           05  FILLER                  REDEFINES WS-LEAP-YEAR-TABLE.    01070000
               07  WS-LEAP-DAYS        OCCURS 12 TIMES                  01080000
                                       PIC 9(3).                        01090000
                                                                        01100000
      ***************************************************************** 01110000
      *  THIS AREA CONTAINS THE DATA FROM THE FUNCTION CURRENT-DATE   * 01120000
      ***************************************************************** 01130000
                                                                        01140000
       01  WS-CURRENT-DATE-TIME.                                        01150000
           03  WS-CDT-DATE.                                             01160000
               05  WS-CDT-D-YEAR       PIC 9(4)  VALUE ZEROES.          01170000
               05  WS-CDT-D-MONTH      PIC 99    VALUE ZEROES.          01180000
               05  WS-CDT-D-DAY        PIC 99    VALUE ZEROES.          01190000
           03  WS-CDT-TIME.                                             01200000
               05  WS-CDT-T-HOURS      PIC 99    VALUE ZEROES.          01210000
               05  WS-CDT-T-MINUTES    PIC 99    VALUE ZEROES.          01220000
               05  WS-CDT-T-SECONDS    PIC 99    VALUE ZEROES.          01230000
               05  WS-CDT-T-HUNDRETHS  PIC 99    VALUE ZEROES.          01240000
           03  WS-CDT-GMT-INDICATOR    PIC X     VALUE SPACES.          01250000
               88  AHEAD-OF-GMT                  VALUE '+'.             01260000
               88  BEHIND-GMT                    VALUE '-'.             01270000
               88  GMT-NOT-AVAILABLE             VALUE '0'.             01280000
           03  WS-CDT-GMT-TIME-DIFFERENTIAL.                            01290000
               05  WS-CDT-GMT-HOURS    PIC 99    VALUE ZEROES.          01300000
               05  WS-CDT-GMT-MINUTES  PIC 99    VALUE ZEROES.          01310000
                                                                        01320000
      ***************************************************************** 01330000
      *  LANGUAGE ENVIRONMENT (LE) ROUTINE WORK AREAS                 * 01340000
      ***************************************************************** 01350000
       01  WS-LE-LILIAN-DATE           PIC S9(9) BINARY.                01360000
       01  WS-LE-SECS                  COMP-2.                          01370000
       01  WS-LE-RETURN-CODE.                                           01380000
           02  FILLER                  PIC X(50).                       01381000
                                                                        01382000
      ***************************************************************** 01383000
      *    SUBROUTINE PARAMETER / WORK AREAS                          * 01384000
      ***************************************************************** 01385000
                                                                        01386000
           COPY PDAS01CY.                                               01387002
           EJECT                                                        01388000
      ***************************************************************** 01389000
      *    MESSAGES   (ERROR AND INFORMATIONAL)                       * 01390000
      ***************************************************************** 01400000
                                                                        01410000
           COPY PDAMSGS.                                                01420000
           EJECT                                                        01430000
      ***************************************************************** 01440000
      *    GENERAL ERROR PROCESSING WORK AREAS (CICS, IMS-DLI, DB2)   * 01450000
      ***************************************************************** 01460000
                                                                        01470000
           COPY PDAERRWS.                                               01480000
           EJECT                                                        01490000
                                                                        01500000
      ***************************************************************** 01510000
      *    L I N K A G E     S E C T I O N                            * 01520000
      ***************************************************************** 01530000
                                                                        01540000
       LINKAGE SECTION.                                                 01550000
                                                                        01560000
       01  LS-PDAS01-PARMS             PIC X(42).                       01570000
                                                                        01580000
      ***************************************************************** 01590000
      *    P R O C E D U R E    D I V I S I O N                       * 01600000
      ***************************************************************** 01610000
                                                                        01620000
       PROCEDURE DIVISION USING LS-PDAS01-PARMS.                        01630000
                                                                        01640000
                                                                        01650000
       P00000-MAINLINE.                                                 01660000
                                                                        01670000
           MOVE LS-PDAS01-PARMS       TO PDAS01-PARMS.                  01680000
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME.          01690000
                                                                        01700000
           CALL 'CEEGMT'               USING WS-LE-LILIAN-DATE,         01710000
                                             WS-LE-SECS,                01720000
                                             WS-LE-RETURN-CODE.         01730000
                                                                        01740000
      ***************************************************************** 01750000
      *    CONVERT CURRENT DATE TO JULIAN                             * 01760000
      ***************************************************************** 01770000
                                                                        01780000
           MOVE WS-CDT-D-YEAR TO WS-JD-YEAR.                            01790000
                                                                        01800000
           IF WS-JD-YR = '00'                                           01810000
               DIVIDE WS-JD-YEAR BY 400 GIVING WS-YEARS                 01820000
                                        REMAINDER WS-REMAIN             01830000
           ELSE                                                         01840000
               DIVIDE WS-JD-YEAR BY 4 GIVING WS-YEARS                   01850000
                                      REMAINDER WS-REMAIN               01860000
           END-IF.                                                      01870000
                                                                        01880000
           IF WS-REMAIN = 0                                             01890000
               MOVE WS-LEAP-DAYS(WS-CDT-D-MONTH) TO WS-JD-DAY           01900000
           ELSE                                                         01910000
               MOVE WS-DAYS(WS-CDT-D-MONTH) TO WS-JD-DAY                01920000
           END-IF.                                                      01930000
                                                                        01940000
           ADD WS-CDT-D-DAY TO WS-JD-DAY.                               01950000
                                                                        01960000
                                                                        01970000
      ***************************************************************** 01980000
      *    CONVERT ORDER DATE TO JULIAN                               * 01990000
      ***************************************************************** 02000000
                                                                        02010000
           MOVE PDAS01-OD-YEAR TO WS-OD-YEAR.                           02020000
                                                                        02030000
           IF WS-OD-YR = '00'                                           02040000
               DIVIDE WS-OD-YEAR BY 400 GIVING WS-YEARS                 02050000
                                        REMAINDER WS-REMAIN             02060000
           ELSE                                                         02070000
               DIVIDE WS-OD-YEAR BY 4 GIVING WS-YEARS                   02080000
                                      REMAINDER WS-REMAIN               02090000
           END-IF.                                                      02100000
                                                                        02110000
           IF WS-REMAIN = 0                                             02120000
               MOVE WS-LEAP-DAYS(PDAS01-OD-MONTH) TO WS-OD-DAY          02130000
           ELSE                                                         02140000
               MOVE WS-DAYS(PDAS01-OD-MONTH) TO WS-OD-DAY               02150000
           END-IF.                                                      02160000
                                                                        02170000
           ADD PDAS01-OD-DAY TO WS-OD-DAY.                              02180000
                                                                        02190000
                                                                        02200000
      ***************************************************************** 02210000
      *    CALCULATE HOW OLD THE ORDER IS                             * 02220000
      ***************************************************************** 02230000
                                                                        02240000
           IF WS-JULIAN-DATE NOT > WS-ORDER-DATE                        02250000
               MOVE ZEROES TO PDAS01-AGE-DAYS                           02260000
           ELSE                                                         02270000
               IF WS-JD-YEAR = WS-OD-YEAR                               02280000
                   COMPUTE PDAS01-AGE-DAYS = WS-JULIAN-DATE -           02290000
                                             WS-ORDER-DATE              02300000
               ELSE                                                     02310000
                   SUBTRACT 1 FROM WS-JD-YEAR                           02320000
                   ADD 365 TO WS-JD-DAY                                 02330000
                   COMPUTE PDAS01-AGE-DAYS = WS-JULIAN-DATE -           02340000
                                             WS-ORDER-DATE              02350000
               END-IF                                                   02360000
           END-IF.                                                      02370000
                                                                        02380000
                                                                        02390000
      ***************************************************************** 02400000
      *    CALCULATE ORDER AVERAGE ORDER AMOUNT - IF VALUES SUPPLIED  * 02410000
      *    BY CALLING PROGRAM                                         * 02420000
      ***************************************************************** 02430000
                                                                        02440000
           IF PDAS01-ORDER-COUNT        NOT = ZEROES                    02450001
               DIVIDE PDAS01-ORDER-DOLLAR-AMT                           02470000
                   BY PDAS01-ORDER-COUNT                                02471001
               GIVING PDAS01-AVERAGE-DOLLAR-AMT.                        02472001
                                                                        02475000
                                                                        02475100
      ***************************************************************** 02475200
      *    RETURN RESULTS TO CALLING PROGRAM                          * 02475300
      ***************************************************************** 02475400
                                                                        02475500
           MOVE PDAS01-PARMS TO LS-PDAS01-PARMS.                        02475600
                                                                        02475700
           GOBACK.                                                      02475800
           EJECT                                                        02475900