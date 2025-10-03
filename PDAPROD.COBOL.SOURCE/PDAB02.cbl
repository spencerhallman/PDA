       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. PDAB02.                                              00020000
      *                                                                 00030000
      ***************************************************************** 00040000
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       * 00050000
      *                       COMPUWARE CORPORATION                   * 00060000
      *                                                               * 00070000
      * PROGRAM :   PDAB02                                            * 00080000
      *                                                               * 00090000
      * FUNCTION:   PROGRAM PDAB02 IS A BATCH PROGRAM THAT WILL LOAD  * 00100000
      *             THE USERID ZERO RECORDS TO THE FOLLOWING FILES:   * 00110000
      *                  1)  CUSTOMER FILE               (VSAM)       * 00120000
      *                  2)  ITEM TABLE                  (DB2)        * 00130000
      *                  3)  SUPPLIER TABLE              (DB2)        * 00140000
      *                  4)  ITEM SUPPLIER TABLE         (DB2)        * 00150000
      *                  5)  PURCHASE TYPE TABLE         (DB2)        * 00160000
      *                  6)  ORDER DATABASE              (IMS)        * 00170000
      *                                                               * 00180000
      * FILES   :   CUSTOMER FILE         -  VSAM KSDS     (UPDATE)   * 00190000
      *             ITEM TABLE            -  DB2           (UPDATE)   * 00200000
      *             SUPPLIER TABLE        -  DB2           (UPDATE)   * 00210000
      *             ITEM SUPPLIER TABLE   -  DB2           (UPDATE)   * 00220000
      *             PURCHASE TYPES TABLE  -  DB2           (UPDATE)   * 00230000
      *             ORDER DATABASE        -  IMS           (UPDATE)   * 00240000
      *                                                               * 00250000
      ***************************************************************** 00260000
      *             PROGRAM CHANGE LOG                                * 00270000
      *             -------------------                               * 00280000
      *                                                               * 00290000
      *  DATE       UPDATED BY            CHANGE DESCRIPTION          * 00300000
      *  --------   --------------------  --------------------------  * 00310000
      *  12/14/05   PAUL BARON            ELIMINATE USE OF FIELD      * 00320000
      *                                   CUSTOMER-TOTAL-DOLLAR-AMT-R * 00330000
      *                                                               * 00340000
      *  08/02/02   PAUL BARON            INITIALIZE NEW CUSTOMER     * 00340100
      *                                   FILE RECORD FIELD           * 00340200
      *                                   CUSTOMER-LAST-ORDER-AMT TO  * 00340300
      *                                   ZEROES WHEN CREATING RECORD * 00340400
      *                                                               * 00340500
      *  04/16/01   PAUL BARON            MOVE INVALID CHARACTERS TO  * 00341000
      *                                   CUSTOMER-TOTAL-DOLLAR-AMT   * 00342000
      *                                   FOR CUSTOMER RECORDS WITH A * 00343000
      *                                   KEY VALUE OF 99999. USED IN * 00344000
      *                                   SCENARIO 1 - GENERATE ASRA  * 00345000
      *                                                               * 00346000
      ***************************************************************** 00350000
           EJECT                                                        00360000
       ENVIRONMENT DIVISION.                                            00370000
                                                                        00380000
       INPUT-OUTPUT SECTION.                                            00390000
                                                                        00400000
       FILE-CONTROL.                                                    00410000
                                                                        00420000
           SELECT INPUT-CUSTOMER       ASSIGN TO ICUSTOMR.              00430000
                                                                        00440000
           SELECT INPUT-ITEM           ASSIGN TO IITEM.                 00450000
                                                                        00460000
           SELECT INPUT-SUPPLIER       ASSIGN TO ISUPPLR.               00470000
                                                                        00480000
           SELECT INPUT-ITEM-SUPPLIER  ASSIGN TO IITMSUP.               00490000
                                                                        00500000
           SELECT INPUT-PURCHASE-TYPE  ASSIGN TO IPURTYP.               00510000
                                                                        00520000
           SELECT INPUT-ORDER          ASSIGN TO IORDER.                00530000
                                                                        00540000
           SELECT VSAM-CUSTOMER        ASSIGN TO VCUSTOMR               00550000
                                       ORGANIZATION IS INDEXED          00560000
                                       ACCESS IS DYNAMIC                00570000
                                       RECORD KEY IS CUSTOMER-KEY       00580000
                                       FILE STATUS IS WS-CUSTOMR-STATUS.00590000
           EJECT                                                        00600000
       DATA DIVISION.                                                   00610000
                                                                        00620000
       FILE SECTION.                                                    00630000
                                                                        00640000
       FD INPUT-CUSTOMER                                                00650000
           LABEL RECORDS ARE STANDARD                                   00660000
           RECORDING MODE IS F                                          00670000
           RECORD CONTAINS 256 CHARACTERS                               00680000
           BLOCK CONTAINS 27904 CHARACTERS.                             00690000
                                                                        00700000
       01  INPUT-CUSTOMER-REC          PIC X(256).                      00710000
                                                                        00720000
                                                                        00730000
       FD INPUT-ITEM                                                    00740000
           LABEL RECORDS ARE STANDARD                                   00750000
           RECORDING MODE IS F                                          00760000
           RECORD CONTAINS 256 CHARACTERS                               00770000
           BLOCK CONTAINS 27904 CHARACTERS.                             00780000
                                                                        00790000
       01  INPUT-ITEM-REC              PIC X(256).                      00800000
                                                                        00810000
                                                                        00820000
       FD INPUT-SUPPLIER                                                00830000
           LABEL RECORDS ARE STANDARD                                   00840000
           RECORDING MODE IS F                                          00850000
           RECORD CONTAINS 256 CHARACTERS                               00860000
           BLOCK CONTAINS 27904 CHARACTERS.                             00870000
                                                                        00880000
       01  INPUT-SUPPLIER-REC          PIC X(256).                      00890000
                                                                        00900000
                                                                        00910000
       FD INPUT-ITEM-SUPPLIER                                           00920000
           LABEL RECORDS ARE STANDARD                                   00930000
           RECORDING MODE IS F                                          00940000
           RECORD CONTAINS 256 CHARACTERS                               00950000
           BLOCK CONTAINS 27904 CHARACTERS.                             00960000
                                                                        00970000
       01  INPUT-ITEM-SUPPLIER-REC     PIC X(256).                      00980000
                                                                        00990000
                                                                        01000000
       FD INPUT-PURCHASE-TYPE                                           01010000
           LABEL RECORDS ARE STANDARD                                   01020000
           RECORDING MODE IS F                                          01030000
           RECORD CONTAINS 256 CHARACTERS                               01040000
           BLOCK CONTAINS 27904 CHARACTERS.                             01050000
                                                                        01060000
       01  INPUT-PURCHASE-TYPE-REC     PIC X(256).                      01070000
                                                                        01080000
                                                                        01090000
       FD INPUT-ORDER                                                   01100000
           LABEL RECORDS ARE STANDARD                                   01110000
           RECORDING MODE IS F                                          01120000
           RECORD CONTAINS 256 CHARACTERS                               01130000
           BLOCK CONTAINS 27904 CHARACTERS.                             01140000
                                                                        01150000
       01  INPUT-ORDER-REC             PIC X(256).                      01160000
           EJECT                                                        01170000
       FD  VSAM-CUSTOMER                                                01180000
           RECORD CONTAINS 733 CHARACTERS.                              01190000
                                                                        01200000
           COPY VCUSTOMR.                                               01210000
           EJECT                                                        01220000
       WORKING-STORAGE SECTION.                                         01230000
                                                                        01240000
                                                                        01250000
      ***************************************************************** 01260000
      *    SWITCHES                                                   * 01270000
      ***************************************************************** 01280000
                                                                        01290000
       01  WS-SWITCHES.                                                 01300000
           05  WS-END-OF-CUSTOMER-SW   PIC X     VALUE 'N'.             01310000
               88  END-OF-CUSTOMER               VALUE 'Y'.             01320000
           05  WS-END-OF-ITEM-SW       PIC X     VALUE 'N'.             01330000
               88  END-OF-ITEM                   VALUE 'Y'.             01340000
           05  WS-END-OF-SUPPLIER-SW   PIC X     VALUE 'N'.             01350000
               88  END-OF-SUPPLIER               VALUE 'Y'.             01360000
           05  WS-END-OF-ITEM-SUPLR-SW PIC X     VALUE 'N'.             01370000
               88  END-OF-ITEM-SUPPLIER          VALUE 'Y'.             01380000
           05  WS-END-OF-PURCHASE-SW   PIC X     VALUE 'N'.             01390000
               88  END-OF-PURCHASE-TYPE          VALUE 'Y'.             01400000
           05  WS-END-OF-ORDER-SW      PIC X     VALUE 'N'.             01410000
               88  END-OF-ORDER                  VALUE 'Y'.             01420000
           05  WS-LOAD-ERROR-SW        PIC X     VALUE 'N'.             01430000
               88  LOAD-ERROR                    VALUE 'Y'.             01440000
                                                                        01450000
                                                                        01460000
      ***************************************************************** 01470000
      *    MISCELLANEOUS WORK FIELDS                                  * 01480000
      ***************************************************************** 01490000
                                                                        01500000
       01  WS-MISCELLANEOUS-FIELDS.                                     01510000
           03  WS-RETURN-CODE          PIC 9(4)  VALUE ZEROES   COMP.   01520000
           03  WS-CUSTOMR-STATUS       PIC XX    VALUE SPACES.          01530000
           03  WS-ISRT                 PIC X(4)  VALUE 'ISRT'.          01540000
           03  WS-OP-STATUS            PIC XX    VALUE SPACES.          01550000
               88  OP-GOOD-RETURN                VALUE '  '.            01560000
               88  OP-END-OF-DATABASE            VALUE 'GB'.            01570000
               88  OP-SEGMENT-NOT-FOUND          VALUE 'GE'.            01580000
               88  OP-END-OF-INPUT-MSG           VALUE 'QC'.            01590000
               88  OP-END-OF-INPUT-SEGMENT       VALUE 'QD'.            01600000
               88  OP-SEGMENT-ALREADY-EXISTS     VALUE 'II'.            01610000
               88  OP-CALL-IOPCB-FROM-BATCH      VALUE 'AL'.            01620000
               88  OP-SECURITY-VIOLATION         VALUE 'A4'.            01630000
           03  WS-CUSTOMER-IN          PIC S9(5) VALUE +0       COMP-3. 01640000
           03  WS-CUSTOMER-OUT         PIC S9(5) VALUE +0       COMP-3. 01650000
           03  WS-CUSTOMER-TOT         PIC S9(5) VALUE +0       COMP-3. 01660000
           03  WS-ITEM-IN              PIC S9(5) VALUE +0       COMP-3. 01670000
           03  WS-ITEM-OUT             PIC S9(5) VALUE +0       COMP-3. 01680000
           03  WS-ITEM-TOT             PIC S9(5) VALUE +0       COMP-3. 01690000
           03  WS-SUPPLIER-IN          PIC S9(5) VALUE +0       COMP-3. 01700000
           03  WS-SUPPLIER-OUT         PIC S9(5) VALUE +0       COMP-3. 01710000
           03  WS-SUPPLIER-TOT         PIC S9(5) VALUE +0       COMP-3. 01720000
           03  WS-ITEM-SUPPLIER-IN     PIC S9(5) VALUE +0       COMP-3. 01730000
           03  WS-ITEM-SUPPLIER-OUT    PIC S9(5) VALUE +0       COMP-3. 01740000
           03  WS-ITEM-SUPPLIER-TOT    PIC S9(5) VALUE +0       COMP-3. 01750000
           03  WS-PURCHASE-TYPE-IN     PIC S9(5) VALUE +0       COMP-3. 01760000
           03  WS-PURCHASE-TYPE-OUT    PIC S9(5) VALUE +0       COMP-3. 01770000
           03  WS-PURCHASE-TYPE-TOT    PIC S9(5) VALUE +0       COMP-3. 01780000
           03  WS-ORDER-IN             PIC S9(5) VALUE +0       COMP-3. 01790000
           03  WS-ORDER-OUT            PIC S9(5) VALUE +0       COMP-3. 01800000
           03  WS-ORDER-TOT            PIC S9(5) VALUE +0       COMP-3. 01810000
           03  WS-ORDER-ITEM-OUT       PIC S9(5) VALUE +0       COMP-3. 01820000
           03  WS-ORDER-ITEM-TOT       PIC S9(5) VALUE +0       COMP-3. 01830000
           03  WS-USERID               PIC X(5)  VALUE SPACES.          01840000
           03  WS-DATE.                                                 01850000
               05  WS-DATE-YEAR        PIC X(4)  VALUE SPACES.          01860000
               05  WS-DATE-MONTH       PIC XX    VALUE SPACES.          01870000
               05  WS-DATE-DAY         PIC XX    VALUE SPACES.          01880000
           03  WS-ORDER-NUMBER         PIC 9(10) VALUE 4900.            01890000
           03  WS-SUB-CHAR             PIC 99    VALUE ZEROES.          01900000
           03  WS-SUB-NUM              PIC 99    VALUE ZEROES.          01910000
           03  WS-SUB-DECIMAL          PIC 99    VALUE ZEROES.          01920000
           03  WS-CHARACTER            PIC X(15) VALUE SPACES.          01930000
           03  WS-CHAR                 REDEFINES WS-CHARACTER           01940000
                                       OCCURS 15 TIMES                  01950000
                                       PIC X.                           01960000
           03  WS-NUMERIC              PIC 9(13)V99 VALUE ZEROES.       01970000
           03  WS-NUM                  REDEFINES WS-NUMERIC             01980000
                                       OCCURS 15 TIMES                  01990000
                                       PIC 9.                           02000000
           03  WS-EMAIL-ADDRESS.                                        02010000
               05  WS-EA-PREFIX        PIC X(5)  VALUE SPACES.          02020000
               05  WS-EA-EMAIL-ADDRESS PIC X(123) VALUE SPACES.         02030000
           EJECT                                                        02040000
      ***************************************************************** 02050000
      *    DISPLAY AREA                                               * 02060000
      ***************************************************************** 02070000
                                                                        02080000
       01  WS-DISPLAY-LINES.                                            02090000
           03  WS-DL-ASTERISK.                                          02100000
               05  FILLER         PIC XX    VALUE SPACES.               02110000
               05  FILLER         PIC X(68) VALUE ALL '*'.              02120000
           03  WS-DL-SPACER.                                            02130000
               05  FILLER         PIC X(69) VALUE '  *'.                02140000
               05  FILLER         PIC X     VALUE '*'.                  02150000
           03  WS-DL-TITLE.                                             02160000
               05  FILLER    PIC X(24) VALUE '  *  PDAB02 - LOAD BASE'. 02170000
               05  FILLER    PIC X(22) VALUE 'DATA TO THE PDA VSAM,'.   02180000
               05  FILLER    PIC X(24) VALUE 'DB2, AND IMS FILES     *'.02190000
           03  WS-DL-DATE.                                              02200000
               05  FILLER         PIC X(18) VALUE '  *        DATE ='.  02210000
               05  WS-DL-D-MONTH  PIC XX    VALUE SPACES.               02220000
               05  FILLER         PIC X     VALUE '/'.                  02230000
               05  WS-DL-D-DAY    PIC XX    VALUE SPACES.               02240000
               05  FILLER         PIC X     VALUE '/'.                  02250000
               05  WS-DL-D-YEAR   PIC X(4)  VALUE SPACES.               02260000
               05  FILLER         PIC X(41) VALUE SPACES.               02270000
               05  FILLER         PIC X     VALUE '*'.                  02280000
           03  WS-DL-LOADED.                                            02290000
               05  FILLER         PIC X(5)  VALUE '  *'.                02300000
               05  WS-DL-L-LIT.                                         02310000
                   07  FILLER     PIC X(5)  VALUE 'ID -'.               02320000
                   07  WS-DL-L-ID PIC X(5)  VALUE SPACES.               02330000
                   07  FILLER     PIC XX    VALUE ','.                  02340000
               05  WS-DL-L-CNT    PIC ZZ,ZZ9.                           02350000
               05  FILLER         PIC X(3)  VALUE ' -'.                 02360000
               05  WS-DL-L-TITLE  PIC X(43) VALUE SPACES.               02370000
               05  FILLER         PIC X     VALUE '*'.                  02380000
           EJECT                                                        02390000
      ***************************************************************** 02400000
      *    INPUT RECORD LAYOUT USED TO BUILD OUTPUT FILES             * 02410000
      ***************************************************************** 02420000
                                                                        02430000
       01  PDA-INPUT-FORMAT.                                            02440000
           03  PDA-FLAG                PIC X     VALUE SPACES.          02450000
               88  PDA-SPACER-REC                VALUE '*'.             02460000
               88  PDA-DATA-REC                  VALUE ' '.             02470000
           03  PDA-FIELD-NAME          PIC X(31) VALUE SPACES.          02480000
           03  FILLER                  PIC X(8)  VALUE SPACES.          02490000
           03  PDA-FIELD-DATA          PIC X(200) VALUE SPACES.         02500000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02510000
               05  PDA-DATA-128        PIC X(128).                      02520000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02530000
               05  PDA-DATA-064        PIC X(64).                       02540000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02550000
               05  PDA-DATA-050        PIC X(50).                       02560000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02570000
               05  PDA-DATA-032        PIC X(32).                       02580000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02590000
               05  PDA-DATA-013-N      PIC 9(13).                       02600000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02610000
               05  PDA-DATA-012        PIC X(12).                       02620000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02630000
               05  PDA-DATA-010        PIC X(10).                       02640000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02650000
               05  PDA-DATA-009        PIC X(9).                        02660000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02670000
               05  PDA-DATA-006        PIC X(6).                        02680000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02690000
               05  PDA-DATA-005        PIC X(5).                        02700000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02710000
               05  PDA-DATA-005-N      PIC 9(5).                        02720000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02730000
               05  PDA-DATA-003        PIC X(3).                        02740000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02750000
               05  PDA-DATA-003-N      PIC 9(3).                        02760000
           EJECT                                                        02770000
      ***************************************************************** 02780000
      *    DB2  DEFINITIONS                                           * 02790000
      ***************************************************************** 02800000
                                                                        02810000
      ***************************************************************** 02820000
      *         SQL COMMUNICATIONS AREA                               * 02830000
      ***************************************************************** 02840000
                                                                        02850000
           EXEC SQL                                                     02860000
              INCLUDE SQLCA                                             02870000
           END-EXEC.                                                    02880000
           EJECT                                                        02890000
           EXEC SQL                                                     02900000
              INCLUDE DITEM                                             02910000
           END-EXEC.                                                    02920000
           EJECT                                                        02930000
           EXEC SQL                                                     02940000
              INCLUDE DSUPPLR                                           02950000
           END-EXEC.                                                    02960000
           EJECT                                                        02970000
           EXEC SQL                                                     02980000
              INCLUDE DITMSUP                                           02990000
           END-EXEC.                                                    03000000
           EJECT                                                        03010000
           EXEC SQL                                                     03020000
              INCLUDE DPURTYP                                           03030000
           END-EXEC.                                                    03040000
           EJECT                                                        03050000
      ***************************************************************** 03060000
      *    IMS SSA AREAS                                              * 03070000
      ***************************************************************** 03080000
                                                                        03090000
       01  ORDER-SSA.                                                   03100000
           03  FILLER                  PIC X(8)  VALUE 'ORDER'.         03110000
           03  FILLER                  PIC X     VALUE '('.             03120000
           03  FILLER                  PIC X(8)  VALUE 'ORDKEY'.        03130000
           03  FILLER                  PIC XX    VALUE ' ='.            03140000
           03  OS-ORDER-KEY.                                            03150000
               05  OS-ORDER-PREFIX     PIC 9(5)  VALUE ZEROES.          03160000
               05  OS-ORDER-NUMBER     PIC 9(10) VALUE ZEROES.          03170000
           03  FILLER                  PIC X     VALUE ')'.             03180000
                                                                        03190000
       01  ORDER-SSA-UNQUAL.                                            03200000
           03  FILLER                  PIC X(8)  VALUE 'ORDER'.         03210000
           03  FILLER                  PIC X     VALUE SPACES.          03220000
                                                                        03230000
       01  ORDER-ITEM-SSA-UNQUAL.                                       03240000
           03  FILLER                  PIC X(8)  VALUE 'ORDITEM'.       03250000
           03  FILLER                  PIC X     VALUE SPACES.          03260000
           EJECT                                                        03270000
      ***************************************************************** 03280000
      *    IMS RECORD AREAS                                           * 03290000
      ***************************************************************** 03300000
                                                                        03310000
           COPY IORDER.                                                 03320000
                                                                        03330000
                                                                        03340000
           COPY IORDITEM.                                               03350000
           EJECT                                                        03360000
      ***************************************************************** 03370000
      *    GENERAL ERROR PROCESSING WORK AREAS (CICS, IMS-DLI, DB2)   * 03380000
      ***************************************************************** 03390000
                                                                        03400000
           COPY PDAERRWS.                                               03410000
                                                                        03420000
       01  WS-PDA-BATCH-ERROR-01.                                       03430000
           05  FILLER             PIC X     VALUE SPACES.               03440000
           05  FILLER             PIC X(7)  VALUE 'ERROR: '.            03450000
           05  FILLER             PIC X(10) VALUE 'PROGRAM = '.         03460000
           05  WPBE-PROGRAM-ID    PIC X(8)  VALUE 'PDAB02'.             03470000
           05  FILLER             PIC X(14) VALUE ', PARAGRAPH = '.     03480000
           05  WPBE-PARAGRAPH     PIC X(6)  VALUE SPACES.               03490000
                                                                        03500000
       01  WS-PDA-BATCH-ERROR-02.                                       03510000
           05  FILLER             PIC X(8)  VALUE SPACES.               03520000
           05  WPBE-MESSAGE       PIC X(39) VALUE SPACES.               03530000
           05  FILLER             PIC X(16) VALUE 'RECORD NUMBER ='.    03540000
           05  WPBE-RECORD-NUMBER PIC X(7)  VALUE ZEROES.               03550000
           05  FILLER             PIC X(8)  VALUE SPACES.               03560000
                                                                        03570000
       01  WS-PDA-BATCH-ERROR-03.                                       03580000
           05  FILLER             PIC X(8)  VALUE SPACES.               03590000
           05  FILLER             PIC X(20) VALUE 'RECORD IS DISPLAYED'.03600000
           05  FILLER             PIC X(5)  VALUE 'BELOW'.              03610000
                                                                        03620000
       01  WS-PDA-BATCH-ERROR-04.                                       03630000
           05  FILLER             PIC X(10) VALUE '----+----1'.         03640000
           05  FILLER             PIC X(10) VALUE '----+----2'.         03650000
           05  FILLER             PIC X(10) VALUE '----+----3'.         03660000
           05  FILLER             PIC X(10) VALUE '----+----4'.         03670000
           05  FILLER             PIC X(10) VALUE '----+----5'.         03680000
           05  FILLER             PIC X(10) VALUE '----+----6'.         03690000
           05  FILLER             PIC X(10) VALUE '----+----7'.         03700000
           05  FILLER             PIC X(5)  VALUE '  ...'.              03710000
           EJECT                                                        03720000
      ***************************************************************** 03730000
      *    LINKAGE SECTION                                            * 03740000
      ***************************************************************** 03750000
                                                                        03760000
       LINKAGE SECTION.                                                 03770000
                                                                        03780000
      ****************************************************************  03790000
      *****  I-O PCB                                                    03800000
      ****************************************************************  03810000
                                                                        03820000
       01  IO-PCB.                                                      03830000
           05  FILLER                  PIC X(10) VALUE SPACES.          03840000
           05  IO-STATUS               PIC XX    VALUE SPACES.          03850000
           05  FILLER                  PIC X(20) VALUE SPACES.          03860000
                                                                        03870000
           COPY PCBORDER.                                               03880000
           EJECT                                                        03890000
      ***************************************************************** 03900000
      *    P R O C E D U R E    D I V I S I O N                       * 03910000
      ***************************************************************** 03920000
                                                                        03930000
       PROCEDURE DIVISION.                                              03940000
                                                                        03950000
                                                                        03960000
      ***************************************************************** 03970000
      *                                                               * 03980000
      *    PARAGRAPH:  P00000-MAINLINE                                * 03990000
      *                                                               * 04000000
      *    FUNCTION :  PROGRAM ENTRY, OPEN FILES, PROCESS.            * 04010000
      *                                                               * 04020000
      *    CALLED BY:  NONE                                           * 04030000
      *                                                               * 04040000
      ***************************************************************** 04050000
                                                                        04060000
       P00000-MAINLINE.                                                 04070000
                                                                        04080000
           ENTRY 'DLITCBL' USING                                        04090000
                           IO-PCB                                       04100000
                           ORDER-PCB.                                   04110000
                                                                        04120000
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATE.                  04130000
           MOVE WS-DATE-MONTH TO WS-DL-D-MONTH.                         04140000
           MOVE WS-DATE-DAY TO WS-DL-D-DAY.                             04150000
           MOVE WS-DATE-YEAR TO WS-DL-D-YEAR.                           04160000
                                                                        04170000
      *    DISPLAY ' '.                                                 04180000
      *    DISPLAY WS-DL-ASTERISK.                                      04190000
      *    DISPLAY WS-DL-SPACER.                                        04200000
      *    DISPLAY WS-DL-TITLE.                                         04210000
      *    DISPLAY WS-DL-SPACER.                                        04220000
      *    DISPLAY WS-DL-DATE.                                          04230000
      *    DISPLAY WS-DL-SPACER.                                        04240000
      *    DISPLAY WS-DL-ASTERISK.                                      04250000
                                                                        04260000
           OPEN INPUT INPUT-CUSTOMER                                    04270000
                      INPUT-ITEM                                        04280000
                      INPUT-SUPPLIER                                    04290000
                      INPUT-ITEM-SUPPLIER                               04300000
                      INPUT-PURCHASE-TYPE                               04310000
                      INPUT-ORDER                                       04320000
                OUTPUT VSAM-CUSTOMER.                                   04330000
                                                                        04340000
           MOVE SPACES TO WS-USERID                                     04350000
                          CUSTOMER-RECORD.                              04360000
                                                                        04370000
      *    DISPLAY ' '.                                                 04380000
      *    DISPLAY ' '.                                                 04390000
      *    DISPLAY WS-DL-ASTERISK.                                      04400000
      *    DISPLAY WS-DL-SPACER.                                        04410000
                                                                        04420000
           PERFORM P10000-LOAD-CUSTOMER THRU P10000-EXIT                04430000
               UNTIL END-OF-CUSTOMER.                                   04440000
                                                                        04450000
           MOVE SPACES TO WS-USERID.                                    04460000
                                                                        04470000
      *    DISPLAY WS-DL-SPACER.                                        04480000
                                                                        04490000
           PERFORM P20000-LOAD-ITEM THRU P20000-EXIT                    04500000
               UNTIL END-OF-ITEM.                                       04510000
                                                                        04520000
           MOVE SPACES TO WS-USERID.                                    04530000
                                                                        04540000
      *    DISPLAY WS-DL-SPACER.                                        04550000
                                                                        04560000
           PERFORM P30000-LOAD-SUPPLIER THRU P30000-EXIT                04570000
               UNTIL END-OF-SUPPLIER.                                   04580000
                                                                        04590000
           MOVE SPACES TO WS-USERID.                                    04600000
                                                                        04610000
      *    DISPLAY WS-DL-SPACER.                                        04620000
                                                                        04630000
           PERFORM P40000-LOAD-ITEM-SUPPLIER THRU P40000-EXIT           04640000
               UNTIL END-OF-ITEM-SUPPLIER.                              04650000
                                                                        04660000
           MOVE SPACES TO WS-USERID.                                    04670000
                                                                        04680000
      *    DISPLAY WS-DL-SPACER.                                        04690000
                                                                        04700000
           PERFORM P50000-LOAD-PURCHASE-TYPE THRU P50000-EXIT           04710000
               UNTIL END-OF-PURCHASE-TYPE.                              04720000
                                                                        04730000
           MOVE SPACES TO WS-USERID                                     04740000
                          ORDER-SEGMENT                                 04750000
                          ORDER-ITEM-SEGMENT.                           04760000
                                                                        04770000
      *    DISPLAY WS-DL-SPACER.                                        04780000
                                                                        04790000
           PERFORM P60000-LOAD-ORDER THRU P60000-EXIT                   04800000
               UNTIL END-OF-ORDER.                                      04810000
                                                                        04820000
      *    DISPLAY WS-DL-SPACER.                                        04830000
      *    DISPLAY WS-DL-ASTERISK.                                      04840000
                                                                        04850000
           CLOSE INPUT-CUSTOMER                                         04860000
                 INPUT-ITEM                                             04870000
                 INPUT-SUPPLIER                                         04880000
                 INPUT-ITEM-SUPPLIER                                    04890000
                 INPUT-PURCHASE-TYPE                                    04900000
                 INPUT-ORDER                                            04910000
                 VSAM-CUSTOMER.                                         04920000
                                                                        04930000
      *    DISPLAY ' '.                                                 04940000
      *    DISPLAY ' '.                                                 04950000
      *    DISPLAY WS-DL-ASTERISK.                                      04960000
      *    DISPLAY WS-DL-SPACER.                                        04970000
                                                                        04980000
           MOVE 'TOTALS' TO WS-DL-L-LIT.                                04990000
           MOVE 'VSAM CUSTOMER RECORDS LOADED' TO WS-DL-L-TITLE.        05000000
           MOVE WS-CUSTOMER-TOT TO WS-DL-L-CNT.                         05010000
                                                                        05020000
      *    DISPLAY WS-DL-LOADED.                                        05030000
      *    DISPLAY WS-DL-SPACER.                                        05040000
                                                                        05050000
           MOVE SPACES TO WS-DL-L-LIT.                                  05060000
           MOVE 'DB2 ITEM ROWS LOADED' TO WS-DL-L-TITLE.                05070000
           MOVE WS-ITEM-TOT TO WS-DL-L-CNT.                             05080000
                                                                        05090000
      *    DISPLAY WS-DL-LOADED.                                        05100000
           DISPLAY WS-DL-SPACER.                                        05110000
                                                                        05120000
           MOVE 'DB2 SUPPLIER ROWS LOADED' TO WS-DL-L-TITLE.            05130000
           MOVE WS-SUPPLIER-TOT TO WS-DL-L-CNT.                         05140000
                                                                        05150000
      *    DISPLAY WS-DL-LOADED.                                        05160000
      *    DISPLAY WS-DL-SPACER.                                        05170000
                                                                        05180000
           MOVE 'DB2 ITEM-SUPPLIER ROWS LOADED' TO WS-DL-L-TITLE.       05190000
           MOVE WS-ITEM-SUPPLIER-TOT TO WS-DL-L-CNT.                    05200000
                                                                        05210000
      *    DISPLAY WS-DL-LOADED.                                        05220000
      *    DISPLAY WS-DL-SPACER.                                        05230000
                                                                        05240000
           MOVE 'DB2 PURCHASE TYPE ROWS LOADED' TO WS-DL-L-TITLE.       05250000
           MOVE WS-PURCHASE-TYPE-TOT TO WS-DL-L-CNT.                    05260000
                                                                        05270000
      *    DISPLAY WS-DL-LOADED.                                        05280000
      *    DISPLAY WS-DL-SPACER.                                        05290000
                                                                        05300000
           MOVE 'IMS ORDER RECORDS LOADED' TO WS-DL-L-TITLE.            05310000
           MOVE WS-ORDER-TOT TO WS-DL-L-CNT.                            05320000
                                                                        05330000
      *    DISPLAY WS-DL-LOADED.                                        05340000
      *    DISPLAY WS-DL-SPACER.                                        05350000
                                                                        05360000
           MOVE 'IMS ORDER ITEM RECORDS LOADED' TO WS-DL-L-TITLE.       05370000
           MOVE WS-ORDER-ITEM-TOT TO WS-DL-L-CNT.                       05380000
                                                                        05390000
      *    DISPLAY WS-DL-LOADED.                                        05400000
      *    DISPLAY WS-DL-SPACER.                                        05410000
      *    DISPLAY WS-DL-ASTERISK.                                      05420000
      *    DISPLAY ' '.                                                 05430000
                                                                        05440000
           GOBACK.                                                      05450000
                                                                        05460000
       P00000-EXIT.                                                     05470000
           EXIT.                                                        05480000
           EJECT                                                        05490000
      ***************************************************************** 05500000
      *                                                               * 05510000
      *    PARAGRAPH:  P10000-LOAD-CUSTOMER                           * 05520000
      *                                                               * 05530000
      *    FUNCTION :  ROUTINE TO LOAD THE CUSTOMER VSAM FILE         * 05540000
      *                                                               * 05550000
      *    CALLED BY:  P00000-MAINLINE                                * 05560000
      *                                                               * 05570000
      ***************************************************************** 05580000
                                                                        05590000
       P10000-LOAD-CUSTOMER.                                            05600000
                                                                        05610000
           READ INPUT-CUSTOMER INTO PDA-INPUT-FORMAT                    05620000
               AT END                                                   05630000
                   MOVE 'Y' TO WS-END-OF-CUSTOMER-SW                    05640000
                   IF CUSTOMER-RECORD > SPACES                          05650000
                       ADD +1 TO WS-CUSTOMER-OUT                        05660000
                       WRITE CUSTOMER-RECORD                            05670000
                       MOVE '99999'  TO CUSTOMER-PREFIX                 05680000
PWB416                 MOVE 7        TO CUSTOMER-TOTAL-ORDER-COUNT      05690000
PWB416                 MOVE 'TRN-91' TO CUSTOMER-TOTAL-DOLLAR-AMT-GRP   05691000
                       ADD +1 TO WS-CUSTOMER-OUT                        05700000
                       WRITE CUSTOMER-RECORD                            05710000
                   END-IF                                               05720000
                   MOVE WS-USERID TO WS-DL-L-ID                         05730000
                   MOVE 'VSAM CUSTOMER RECORDS LOADED' TO WS-DL-L-TITLE 05740000
                   MOVE WS-CUSTOMER-OUT TO WS-DL-L-CNT                  05750000
                   ADD WS-CUSTOMER-OUT TO WS-CUSTOMER-TOT               05760000
      *            DISPLAY WS-DL-LOADED                                 05770000
                   GO TO P10000-EXIT.                                   05780000
                                                                        05790000
           ADD +1 TO WS-CUSTOMER-IN.                                    05800000
                                                                        05810000
           IF PDA-SPACER-REC                                            05820000
               IF CUSTOMER-RECORD > SPACES                              05830000
                   ADD +1 TO WS-CUSTOMER-OUT                            05840000
                   WRITE CUSTOMER-RECORD                                05850000
                   MOVE '99999'  TO CUSTOMER-PREFIX                     05860000
PWB416             MOVE 7        TO CUSTOMER-TOTAL-ORDER-COUNT          05860100
PWB416             MOVE 'TRN-91' TO CUSTOMER-TOTAL-DOLLAR-AMT-GRP       05860200
                   ADD +1 TO WS-CUSTOMER-OUT                            05880000
                   WRITE CUSTOMER-RECORD                                05890000
                   MOVE SPACES TO CUSTOMER-RECORD                       05900000
               END-IF                                                   05910000
               GO TO P10000-EXIT                                        05920000
           END-IF.                                                      05930000
                                                                        05940000
           IF NOT PDA-DATA-REC                                          05950000
               MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                         05960000
               MOVE 'P10000' TO WPBE-PARAGRAPH                          05970000
               MOVE 'UNIDENTIFIED INPUT RECORD,' TO WPBE-MESSAGE        05980000
               MOVE WS-CUSTOMER-IN TO WPBE-RECORD-NUMBER                05990000
               PERFORM P99999-ABEND THRU P99999-EXIT                    06000000
           END-IF.                                                      06010000
                                                                        06020000
           MOVE ZEROES                 TO CUSTOMER-LAST-ORDER-AMT.      06021000
                                                                        06022000
           EVALUATE TRUE                                                06030000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.PREFIX'                  06040000
                   MOVE PDA-DATA-005-N TO CUSTOMER-PREFIX               06050000
                   IF PDA-DATA-005 NOT = WS-USERID                      06060000
                       IF WS-USERID = SPACES                            06070000
                           MOVE PDA-DATA-005 TO WS-USERID               06080000
                       ELSE                                             06090000
                           MOVE WS-USERID TO WS-DL-L-ID                 06100000
                           MOVE 'VSAM CUSTOMER RECORDS LOADED' TO       06110000
                               WS-DL-L-TITLE                            06120000
                           MOVE WS-CUSTOMER-OUT TO WS-DL-L-CNT          06130000
                           ADD WS-CUSTOMER-OUT TO WS-CUSTOMER-TOT       06140000
                           MOVE +0 TO WS-CUSTOMER-OUT                   06150000
      *                    DISPLAY WS-DL-LOADED                         06160000
                           MOVE PDA-DATA-005 TO WS-USERID               06170000
                       END-IF                                           06180000
                   END-IF                                               06190000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.ID'                      06200000
                   MOVE PDA-DATA-032 TO CUSTOMER-ID                     06210000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.PASSWORD'                06220000
                   MOVE PDA-DATA-032 TO CUSTOMER-PASSWORD               06230000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.NAME'                    06240000
                   MOVE PDA-DATA-064 TO CUSTOMER-NAME                   06250000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.ADDRESS'                 06260000
                   MOVE PDA-DATA-128 TO CUSTOMER-ADDRESS                06270000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.CITY'                    06280000
                   MOVE PDA-DATA-032 TO CUSTOMER-CITY                   06290000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.STATE'                   06300000
                   MOVE PDA-DATA-032 TO CUSTOMER-STATE                  06310000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.POSTAL-CODE'             06320000
                   MOVE PDA-DATA-012 TO CUSTOMER-POSTAL-CODE            06330000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.SHIP-TO-NAME'            06340000
                   MOVE PDA-DATA-064 TO CUSTOMER-SHIP-TO-NAME           06350000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.SHIP-TO-ADDRESS'         06360000
                   MOVE PDA-DATA-128 TO CUSTOMER-SHIP-TO-ADDRESS        06370000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.SHIP-TO-CITY'            06380000
                   MOVE PDA-DATA-032 TO CUSTOMER-SHIP-TO-CITY           06390000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.SHIP-TO-STATE'           06400000
                   MOVE PDA-DATA-032 TO CUSTOMER-SHIP-TO-STATE          06410000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.SHIP-TO-POSTAL-CODE'     06420000
                   MOVE PDA-DATA-012 TO CUSTOMER-SHIP-TO-POSTAL-CODE    06430000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.EMAIL-ADDRESS'           06440000
                   MOVE PDA-DATA-128 TO CUSTOMER-EMAIL-ADDRESS          06450000
               WHEN OTHER                                               06460000
                   MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                     06470000
                   MOVE 'P10000' TO WPBE-PARAGRAPH                      06480000
                   MOVE 'UNIDENTIFIED INPUT RECORD FIELD NAME,' TO      06490000
                        WPBE-MESSAGE                                    06500000
                   MOVE WS-CUSTOMER-IN TO WPBE-RECORD-NUMBER            06510000
                   PERFORM P99999-ABEND THRU P99999-EXIT                06520000
           END-EVALUATE.                                                06530000
                                                                        06540000
       P10000-EXIT.                                                     06550000
           EXIT.                                                        06560000
           EJECT                                                        06570000
      ***************************************************************** 06580000
      *                                                               * 06590000
      *    PARAGRAPH:  P20000-LOAD-ITEM                               * 06600000
      *                                                               * 06610000
      *    FUNCTION :  ROUTINE TO LOAD THE ITEM TABLE                 * 06620000
      *                                                               * 06630000
      *    CALLED BY:  P00000-MAINLINE                                * 06640000
      *                                                               * 06650000
      ***************************************************************** 06660000
                                                                        06670000
       P20000-LOAD-ITEM.                                                06680000
                                                                        06690000
           READ INPUT-ITEM INTO PDA-INPUT-FORMAT                        06700000
               AT END                                                   06710000
                   MOVE 'Y' TO WS-END-OF-ITEM-SW                        06720000
                   IF ITEM > SPACES                                     06730000
                       ADD +1 TO WS-ITEM-OUT                            06740000
                       PERFORM P21000-INSERT-ITEM THRU P21000-EXIT      06750000
                   END-IF                                               06760000
                   MOVE WS-USERID TO WS-DL-L-ID                         06770000
                   MOVE 'DB2 ITEM ROWS LOADED' TO WS-DL-L-TITLE         06780000
                   MOVE WS-ITEM-OUT TO WS-DL-L-CNT                      06790000
                   ADD WS-ITEM-OUT TO WS-ITEM-TOT                       06800000
      *            DISPLAY WS-DL-LOADED                                 06810000
                   GO TO P20000-EXIT.                                   06820000
                                                                        06830000
           ADD +1 TO WS-ITEM-IN.                                        06840000
                                                                        06850000
           IF PDA-SPACER-REC                                            06860000
               IF ITEM > SPACES                                         06870000
                   ADD +1 TO WS-ITEM-OUT                                06880000
                   PERFORM P21000-INSERT-ITEM THRU P21000-EXIT          06890000
                   MOVE SPACES TO ITEM                                  06900000
               END-IF                                                   06910000
               GO TO P20000-EXIT                                        06920000
           END-IF.                                                      06930000
                                                                        06940000
           IF NOT PDA-DATA-REC                                          06950000
               MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                         06960000
               MOVE 'P20000' TO WPBE-PARAGRAPH                          06970000
               MOVE 'UNIDENTIFIED INPUT RECORD,' TO WPBE-MESSAGE        06980000
               MOVE WS-ITEM-IN TO WPBE-RECORD-NUMBER                    06990000
               PERFORM P99999-ABEND THRU P99999-EXIT                    07000000
           END-IF.                                                      07010000
                                                                        07020000
           EVALUATE TRUE                                                07030000
               WHEN PDA-FIELD-NAME = 'ITEM.PREFIX'                      07040000
                   MOVE PDA-DATA-005 TO ITEM-PREFIX                     07050000
                   IF PDA-DATA-005 NOT = WS-USERID                      07060000
                       IF WS-USERID = SPACES                            07070000
                           MOVE PDA-DATA-005 TO WS-USERID               07080000
                       ELSE                                             07090000
                           MOVE WS-USERID TO WS-DL-L-ID                 07100000
                           MOVE 'DB2 ITEM ROWS LOADED' TO WS-DL-L-TITLE 07110000
                           MOVE WS-ITEM-OUT TO WS-DL-L-CNT              07120000
                           ADD WS-ITEM-OUT TO WS-ITEM-TOT               07130000
                           MOVE +0 TO WS-ITEM-OUT                       07140000
      *                    DISPLAY WS-DL-LOADED                         07150000
                           MOVE PDA-DATA-005 TO WS-USERID               07160000
                       END-IF                                           07170000
                   END-IF                                               07180000
               WHEN PDA-FIELD-NAME = 'ITEM.NUMBER'                      07190000
                   MOVE PDA-DATA-032 TO ITEM-NUMBER                     07200000
               WHEN PDA-FIELD-NAME = 'ITEM.CATEGORY-NAME'               07210000
                   MOVE PDA-DATA-032 TO ITEM-CATEGORY-NAME              07220000
               WHEN PDA-FIELD-NAME = 'ITEM.SUB-CATEGORY-NAME'           07230000
                   MOVE PDA-DATA-032 TO ITEM-SUB-CATEGORY-NAME          07240000
               WHEN PDA-FIELD-NAME = 'ITEM.NAME'                        07250000
                   MOVE PDA-DATA-050 TO ITEM-NAME                       07260000
               WHEN PDA-FIELD-NAME = 'ITEM.LENGTH'                      07270000
                   MOVE PDA-DATA-010 TO WS-CHARACTER                    07280000
                   PERFORM P90000-CONVERT-NUMERIC THRU P90000-EXIT      07290000
                   MOVE WS-NUMERIC TO ITEM-LENGTH                       07300000
               WHEN PDA-FIELD-NAME = 'ITEM.DIAMETER'                    07310000
                   MOVE PDA-DATA-010 TO WS-CHARACTER                    07320000
                   PERFORM P90000-CONVERT-NUMERIC THRU P90000-EXIT      07330000
                   MOVE WS-NUMERIC TO ITEM-DIAMETER                     07340000
               WHEN OTHER                                               07350000
                   MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                     07360000
                   MOVE 'P20000' TO WPBE-PARAGRAPH                      07370000
                   MOVE 'UNIDENTIFIED INPUT RECORD FIELD NAME,' TO      07380000
                        WPBE-MESSAGE                                    07390000
                   MOVE WS-ITEM-IN TO WPBE-RECORD-NUMBER                07400000
                   PERFORM P99999-ABEND THRU P99999-EXIT                07410000
           END-EVALUATE.                                                07420000
                                                                        07430000
       P20000-EXIT.                                                     07440000
           EXIT.                                                        07450000
           EJECT                                                        07460000
      ***************************************************************** 07470000
      *                                                               * 07480000
      *    PARAGRAPH:  P21000-INSERT-ITEM                             * 07490000
      *                                                               * 07500000
      *    FUNCTION :  ROUTINE TO INSERT TO THE ITEM TABLE            * 07510000
      *                                                               * 07520000
      *    CALLED BY:  P20000-LOAD-ITEM                               * 07530000
      *                                                               * 07540000
      ***************************************************************** 07550000
                                                                        07560000
       P21000-INSERT-ITEM.                                              07570000
                                                                        07580000
           EXEC SQL                                                     07590000
               INSERT                                                   07600000
               INTO   ITEM                                              07610000
                     (PREFIX,                                           07620000
                      NUMBER,                                           07630000
                      CATEGORY_NAME,                                    07640000
                      SUB_CATEGORY_NAME,                                07650000
                      NAME,                                             07660000
                      LENGTH,                                           07670000
                      DIAMETER)                                         07680000
               VALUES                                                   07690000
                     (:ITEM-PREFIX,                                     07700000
                      :ITEM-NUMBER,                                     07710000
                      :ITEM-CATEGORY-NAME,                              07720000
                      :ITEM-SUB-CATEGORY-NAME,                          07730000
                      :ITEM-NAME,                                       07740000
                      :ITEM-LENGTH,                                     07750000
                      :ITEM-DIAMETER)                                   07760000
           END-EXEC.                                                    07770000
                                                                        07780000
           IF SQLCODE NOT = +0                                          07790000
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE                          07800000
               MOVE 'PDAB02' TO WPDE-PROGRAM-ID                         07810000
               MOVE SQLCODE TO WPDE-DB2-SQLCODE                         07820000
               MOVE 'INSERT TO ITEM TABLE' TO WPDE-FUNCTION             07830000
               MOVE 'P21000' TO WPDE-PARAGRAPH                          07840000
               MOVE ITEM TO PDA-INPUT-FORMAT                            07850000
               PERFORM P99999-ABEND THRU P99999-EXIT                    07860000
           END-IF.                                                      07870000
                                                                        07880000
       P21000-EXIT.                                                     07890000
           EXIT.                                                        07900000
           EJECT                                                        07910000
      ***************************************************************** 07920000
      *                                                               * 07930000
      *    PARAGRAPH:  P30000-LOAD-SUPPLIER                           * 07940000
      *                                                               * 07950000
      *    FUNCTION :  ROUTINE TO LOAD THE SUPPLIER TABLE             * 07960000
      *                                                               * 07970000
      *    CALLED BY:  P00000-MAINLINE                                * 07980000
      *                                                               * 07990000
      ***************************************************************** 08000000
                                                                        08010000
       P30000-LOAD-SUPPLIER.                                            08020000
                                                                        08030000
           READ INPUT-SUPPLIER INTO PDA-INPUT-FORMAT                    08040000
               AT END                                                   08050000
                   MOVE 'Y' TO WS-END-OF-SUPPLIER-SW                    08060000
                   IF SUPPLIER > SPACES                                 08070000
                       ADD +1 TO WS-SUPPLIER-OUT                        08080000
                       PERFORM P31000-INSERT-SUPPLIER THRU P31000-EXIT  08090000
                   END-IF                                               08100000
                   MOVE WS-USERID TO WS-DL-L-ID                         08110000
                   MOVE 'DB2 SUPPLIER ROWS LOADED' TO WS-DL-L-TITLE     08120000
                   MOVE WS-SUPPLIER-OUT TO WS-DL-L-CNT                  08130000
                   ADD WS-SUPPLIER-OUT TO WS-SUPPLIER-TOT               08140000
      *            DISPLAY WS-DL-LOADED                                 08150000
                   GO TO P30000-EXIT.                                   08160000
                                                                        08170000
           ADD +1 TO WS-SUPPLIER-IN.                                    08180000
                                                                        08190000
           IF PDA-SPACER-REC                                            08200000
               IF SUPPLIER > SPACES                                     08210000
                   ADD +1 TO WS-SUPPLIER-OUT                            08220000
                   PERFORM P31000-INSERT-SUPPLIER THRU P31000-EXIT      08230000
                   MOVE SPACES TO SUPPLIER                              08240000
               END-IF                                                   08250000
               GO TO P30000-EXIT                                        08260000
           END-IF.                                                      08270000
                                                                        08280000
           IF NOT PDA-DATA-REC                                          08290000
               MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                         08300000
               MOVE 'P30000' TO WPBE-PARAGRAPH                          08310000
               MOVE 'UNIDENTIFIED INPUT RECORD,' TO WPBE-MESSAGE        08320000
               MOVE WS-SUPPLIER-IN TO WPBE-RECORD-NUMBER                08330000
               PERFORM P99999-ABEND THRU P99999-EXIT                    08340000
           END-IF.                                                      08350000
                                                                        08360000
           EVALUATE TRUE                                                08370000
               WHEN PDA-FIELD-NAME = 'SUPPLIER.PREFIX'                  08380000
                   MOVE PDA-DATA-005 TO SUPPLIER-PREFIX                 08390000
                                        WS-EA-PREFIX                    08400000
                   IF PDA-DATA-005 NOT = WS-USERID                      08410000
                       IF WS-USERID = SPACES                            08420000
                           MOVE PDA-DATA-005 TO WS-USERID               08430000
                       ELSE                                             08440000
                           MOVE WS-USERID TO WS-DL-L-ID                 08450000
                           MOVE 'DB2 SUPPLIER ROWS LOADED' TO           08460000
                               WS-DL-L-TITLE                            08470000
                           MOVE WS-SUPPLIER-OUT TO WS-DL-L-CNT          08480000
                           ADD WS-SUPPLIER-OUT TO WS-SUPPLIER-TOT       08490000
                           MOVE +0 TO WS-SUPPLIER-OUT                   08500000
      *                    DISPLAY WS-DL-LOADED                         08510000
                           MOVE PDA-DATA-005 TO WS-USERID               08520000
                       END-IF                                           08530000
                   END-IF                                               08540000
               WHEN PDA-FIELD-NAME = 'SUPPLIER.SUPPLIER-ID'             08550000
                   MOVE PDA-DATA-032 TO SUPPLIER-SUPPLIER-ID            08560000
               WHEN PDA-FIELD-NAME = 'SUPPLIER.PASSWORD'                08570000
                   MOVE PDA-DATA-032 TO SUPPLIER-PASSWORD               08580000
               WHEN PDA-FIELD-NAME = 'SUPPLIER.NAME'                    08590000
                   MOVE PDA-DATA-064 TO SUPPLIER-NAME                   08600000
               WHEN PDA-FIELD-NAME = 'SUPPLIER.ADDRESS'                 08610000
                   MOVE PDA-DATA-128 TO SUPPLIER-ADDRESS                08620000
               WHEN PDA-FIELD-NAME = 'SUPPLIER.CITY'                    08630000
                   MOVE PDA-DATA-032 TO SUPPLIER-CITY                   08640000
               WHEN PDA-FIELD-NAME = 'SUPPLIER.STATE'                   08650000
                   MOVE PDA-DATA-032 TO SUPPLIER-STATE                  08660000
               WHEN PDA-FIELD-NAME = 'SUPPLIER.POSTAL-CODE'             08670000
                   MOVE PDA-DATA-012 TO SUPPLIER-POSTAL-CODE            08680000
               WHEN PDA-FIELD-NAME = 'SUPPLIER.EMAIL-ADDRESS'           08690000
                   MOVE PDA-DATA-128 TO WS-EA-EMAIL-ADDRESS             08700000
                   MOVE WS-EMAIL-ADDRESS TO SUPPLIER-EMAIL-ADDRESS      08710000
               WHEN OTHER                                               08720000
                   MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                     08730000
                   MOVE 'P30000' TO WPBE-PARAGRAPH                      08740000
                   MOVE 'UNIDENTIFIED INPUT RECORD FIELD NAME,' TO      08750000
                        WPBE-MESSAGE                                    08760000
                   MOVE WS-SUPPLIER-IN TO WPBE-RECORD-NUMBER            08770000
                   PERFORM P99999-ABEND THRU P99999-EXIT                08780000
           END-EVALUATE.                                                08790000
                                                                        08800000
       P30000-EXIT.                                                     08810000
           EXIT.                                                        08820000
           EJECT                                                        08830000
      ***************************************************************** 08840000
      *                                                               * 08850000
      *    PARAGRAPH:  P31000-INSERT-SUPPLIER                         * 08860000
      *                                                               * 08870000
      *    FUNCTION :  ROUTINE TO INSERT TO THE SUPPLIER TABLE        * 08880000
      *                                                               * 08890000
      *    CALLED BY:  P30000-LOAD-SUPPLIER                           * 08900000
      *                                                               * 08910000
      ***************************************************************** 08920000
                                                                        08930000
       P31000-INSERT-SUPPLIER.                                          08940000
                                                                        08950000
           EXEC SQL                                                     08960000
               INSERT                                                   08970000
               INTO   SUPPLIER                                          08980000
                     (PREFIX,                                           08990000
                      SUPPLIER_ID,                                      09000000
                      PASSWORD,                                         09010000
                      NAME,                                             09020000
                      ADDRESS,                                          09030000
                      CITY,                                             09040000
                      STATE,                                            09050000
                      POSTAL_CODE,                                      09060000
                      EMAIL_ADDRESS)                                    09070000
               VALUES                                                   09080000
                     (:SUPPLIER-PREFIX,                                 09090000
                      :SUPPLIER-SUPPLIER-ID,                            09100000
                      :SUPPLIER-PASSWORD,                               09110000
                      :SUPPLIER-NAME,                                   09120000
                      :SUPPLIER-ADDRESS,                                09130000
                      :SUPPLIER-CITY,                                   09140000
                      :SUPPLIER-STATE,                                  09150000
                      :SUPPLIER-POSTAL-CODE,                            09160000
                      :SUPPLIER-EMAIL-ADDRESS)                          09170000
           END-EXEC.                                                    09180000
                                                                        09190000
           IF SQLCODE NOT = +0                                          09200000
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE                          09210000
               MOVE 'PDAB02' TO WPDE-PROGRAM-ID                         09220000
               MOVE SQLCODE TO WPDE-DB2-SQLCODE                         09230000
               MOVE 'INSERT TO SUPPLIER TABLE' TO WPDE-FUNCTION         09240000
               MOVE 'P31000' TO WPDE-PARAGRAPH                          09250000
               MOVE SUPPLIER TO PDA-INPUT-FORMAT                        09260000
               PERFORM P99999-ABEND THRU P99999-EXIT                    09270000
           END-IF.                                                      09280000
                                                                        09290000
       P31000-EXIT.                                                     09300000
           EXIT.                                                        09310000
           EJECT                                                        09320000
      ***************************************************************** 09330000
      *                                                               * 09340000
      *    PARAGRAPH:  P40000-LOAD-ITEM-SUPPLIER                      * 09350000
      *                                                               * 09360000
      *    FUNCTION :  ROUTINE TO LOAD THE ITEM-SUPPLIER TABLE        * 09370000
      *                                                               * 09380000
      *    CALLED BY:  P00000-MAINLINE                                * 09390000
      *                                                               * 09400000
      ***************************************************************** 09410000
                                                                        09420000
       P40000-LOAD-ITEM-SUPPLIER.                                       09430000
                                                                        09440000
           READ INPUT-ITEM-SUPPLIER INTO PDA-INPUT-FORMAT               09450000
               AT END                                                   09460000
                   MOVE 'Y' TO WS-END-OF-ITEM-SUPLR-SW                  09470000
                   IF ITEM-SUPPLIER > SPACES                            09480000
                       ADD +1 TO WS-ITEM-SUPPLIER-OUT                   09490000
                       PERFORM P41000-INSERT-ITEM-SUPPLIER              09500000
                           THRU P41000-EXIT                             09510000
                   END-IF                                               09520000
                   MOVE WS-USERID TO WS-DL-L-ID                         09530000
                   MOVE 'DB2 ITEM SUPPLIER ROWS LOADED' TO WS-DL-L-TITLE09540000
                   MOVE WS-ITEM-SUPPLIER-OUT TO WS-DL-L-CNT             09550000
                   ADD WS-ITEM-SUPPLIER-OUT TO WS-ITEM-SUPPLIER-TOT     09560000
      *            DISPLAY WS-DL-LOADED                                 09570000
                   GO TO P40000-EXIT.                                   09580000
                                                                        09590000
           ADD +1 TO WS-ITEM-SUPPLIER-IN.                               09600000
                                                                        09610000
           IF PDA-SPACER-REC                                            09620000
               IF ITEM-SUPPLIER > SPACES                                09630000
                   ADD +1 TO WS-ITEM-SUPPLIER-OUT                       09640000
                   PERFORM P41000-INSERT-ITEM-SUPPLIER THRU P41000-EXIT 09650000
                   MOVE SPACES TO ITEM-SUPPLIER                         09660000
               END-IF                                                   09670000
               GO TO P40000-EXIT                                        09680000
           END-IF.                                                      09690000
                                                                        09700000
           IF NOT PDA-DATA-REC                                          09710000
               MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                         09720000
               MOVE 'P40000' TO WPBE-PARAGRAPH                          09730000
               MOVE 'UNIDENTIFIED INPUT RECORD,' TO WPBE-MESSAGE        09740000
               MOVE WS-ITEM-SUPPLIER-IN TO WPBE-RECORD-NUMBER           09750000
               PERFORM P99999-ABEND THRU P99999-EXIT                    09760000
           END-IF.                                                      09770000
                                                                        09780000
           EVALUATE TRUE                                                09790000
               WHEN PDA-FIELD-NAME = 'ITEM-SUPPLIER.ITEM-PREFIX'        09800000
                   MOVE PDA-DATA-005 TO ITEM-SUPPLIER-ITEM-PREFIX       09810000
                   IF PDA-DATA-005 NOT = WS-USERID                      09820000
                       IF WS-USERID = SPACES                            09830000
                           MOVE PDA-DATA-005 TO WS-USERID               09840000
                       ELSE                                             09850000
                           MOVE WS-USERID TO WS-DL-L-ID                 09860000
                           MOVE 'DB2 ITEM SUPPLIER ROWS LOADED' TO      09870000
                               WS-DL-L-TITLE                            09880000
                           MOVE WS-ITEM-SUPPLIER-OUT TO WS-DL-L-CNT     09890000
                           ADD WS-ITEM-SUPPLIER-OUT TO                  09900000
                               WS-ITEM-SUPPLIER-TOT                     09910000
                           MOVE +0 TO WS-ITEM-SUPPLIER-OUT              09920000
      *                    DISPLAY WS-DL-LOADED                         09930000
                           MOVE PDA-DATA-005 TO WS-USERID               09940000
                       END-IF                                           09950000
                   END-IF                                               09960000
               WHEN PDA-FIELD-NAME = 'ITEM-SUPPLIER.ITEM-NUMBER'        09970000
                   MOVE PDA-DATA-032 TO ITEM-SUPPLIER-ITEM-NUMBER       09980000
               WHEN PDA-FIELD-NAME = 'ITEM-SUPPLIER.SUPPLIER-PREFIX'    09990000
                   MOVE PDA-DATA-005 TO ITEM-SUPPLIER-SUPPLIER-PREFIX   10000000
               WHEN PDA-FIELD-NAME = 'ITEM-SUPPLIER.SUPPLIER-ID'        10010000
                   MOVE PDA-DATA-032 TO ITEM-SUPPLIER-SUPPLIER-ID       10020000
               WHEN PDA-FIELD-NAME = 'ITEM-SUPPLIER.QUANTITY-ON-HAND'   10030000
                   MOVE PDA-DATA-009 TO WS-CHARACTER                    10040000
                   PERFORM P90000-CONVERT-NUMERIC THRU P90000-EXIT      10050000
                   MOVE WS-NUMERIC TO ITEM-SUPPLIER-QUANTITY-ON-HAND    10060000
               WHEN PDA-FIELD-NAME = 'ITEM-SUPPLIER.UNIT-PRICE'         10070000
                   MOVE PDA-DATA-010 TO WS-CHARACTER                    10080000
                   PERFORM P90000-CONVERT-NUMERIC THRU P90000-EXIT      10090000
                   MOVE WS-NUMERIC TO ITEM-SUPPLIER-UNIT-PRICE          10100000
               WHEN OTHER                                               10110000
                   MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                     10120000
                   MOVE 'P40000' TO WPBE-PARAGRAPH                      10130000
                   MOVE 'UNIDENTIFIED INPUT RECORD FIELD NAME,' TO      10140000
                        WPBE-MESSAGE                                    10150000
                   MOVE WS-ITEM-SUPPLIER-IN TO WPBE-RECORD-NUMBER       10160000
                   PERFORM P99999-ABEND THRU P99999-EXIT                10170000
           END-EVALUATE.                                                10180000
                                                                        10190000
       P40000-EXIT.                                                     10200000
           EXIT.                                                        10210000
           EJECT                                                        10220000
      ***************************************************************** 10230000
      *                                                               * 10240000
      *    PARAGRAPH:  P41000-INSERT-ITEM-SUPPLIER                    * 10250000
      *                                                               * 10260000
      *    FUNCTION :  ROUTINE TO INSERT TO THE ITEM-SUPPLIER TABLE   * 10270000
      *                                                               * 10280000
      *    CALLED BY:  P40000-LOAD-ITEM-SUPPLIER                      * 10290000
      *                                                               * 10300000
      ***************************************************************** 10310000
                                                                        10320000
       P41000-INSERT-ITEM-SUPPLIER.                                     10330000
                                                                        10340000
           EXEC SQL                                                     10350000
               INSERT                                                   10360000
               INTO   ITEM_SUPPLIER                                     10370000
                     (ITEM_PREFIX,                                      10380000
                      ITEM_NUMBER,                                      10390000
                      SUPPLIER_PREFIX,                                  10400000
                      SUPPLIER_ID,                                      10410000
                      QUANTITY_ON_HAND,                                 10420000
                      UNIT_PRICE)                                       10430000
               VALUES                                                   10440000
                     (:ITEM-SUPPLIER-ITEM-PREFIX,                       10450000
                      :ITEM-SUPPLIER-ITEM-NUMBER,                       10460000
                      :ITEM-SUPPLIER-SUPPLIER-PREFIX,                   10470000
                      :ITEM-SUPPLIER-SUPPLIER-ID,                       10480000
                      :ITEM-SUPPLIER-QUANTITY-ON-HAND,                  10490000
                      :ITEM-SUPPLIER-UNIT-PRICE)                        10500000
           END-EXEC.                                                    10510000
                                                                        10520000
           EVALUATE TRUE                                                10530000
               WHEN SQLCODE = +0                                        10540000
                   EXIT                                                 10550000
               WHEN SQLCODE = -803                                      10560000
                   DISPLAY ' '                                          10570000
                   DISPLAY 'P41000-INSERT-ITEM-SUPPLIER'                10580000
                   DISPLAY ' '                                          10590000
                   DISPLAY '    DUPLICATE ITEM SUPPLIER IGNORED, '      10600000
                   DISPLAY '    ITEM NUMBER = '                         10610000
                           ITEM-SUPPLIER-ITEM-NUMBER                    10620000
                   DISPLAY '    SUPPLIER ID = '                         10630000
                           ITEM-SUPPLIER-SUPPLIER-ID                    10640000
                   DISPLAY ' '                                          10650000
               WHEN OTHER                                               10660000
                   MOVE 'DB2' TO WS-PDA-ERROR-TYPE                      10670000
                   MOVE 'PDAB02' TO WPDE-PROGRAM-ID                     10680000
                   MOVE SQLCODE TO WPDE-DB2-SQLCODE                     10690000
                   MOVE 'INSERT TO ITEM SUPPLIER TABLE' TO WPDE-FUNCTION10700000
                   MOVE 'P41000' TO WPDE-PARAGRAPH                      10710000
                   MOVE ITEM-SUPPLIER TO PDA-INPUT-FORMAT               10720000
                   PERFORM P99999-ABEND THRU P99999-EXIT                10730000
           END-EVALUATE.                                                10740000
                                                                        10750000
       P41000-EXIT.                                                     10760000
           EXIT.                                                        10770000
           EJECT                                                        10780000
      ***************************************************************** 10790000
      *                                                               * 10800000
      *    PARAGRAPH:  P50000-LOAD-PURCHASE-TYPE                      * 10810000
      *                                                               * 10820000
      *    FUNCTION :  ROUTINE TO LOAD THE PURCHASE-TYPE TABLE        * 10830000
      *                                                               * 10840000
      *    CALLED BY:  P00000-MAINLINE                                * 10850000
      *                                                               * 10860000
      ***************************************************************** 10870000
                                                                        10880000
       P50000-LOAD-PURCHASE-TYPE.                                       10890000
                                                                        10900000
           READ INPUT-PURCHASE-TYPE INTO PDA-INPUT-FORMAT               10910000
               AT END                                                   10920000
                   MOVE 'Y' TO WS-END-OF-PURCHASE-SW                    10930000
                   IF PURCHASE-TYPE > SPACES                            10940000
                       ADD +1 TO WS-PURCHASE-TYPE-OUT                   10950000
                       PERFORM P51000-INSERT-PURCHASE-TYPE              10960000
                           THRU P51000-EXIT                             10970000
                   END-IF                                               10980000
                   MOVE WS-USERID TO WS-DL-L-ID                         10990000
                   MOVE 'DB2 PURCHASE TYPE ROWS LOADED' TO WS-DL-L-TITLE11000000
                   MOVE WS-PURCHASE-TYPE-OUT TO WS-DL-L-CNT             11010000
                   ADD WS-PURCHASE-TYPE-OUT TO WS-PURCHASE-TYPE-TOT     11020000
      *            DISPLAY WS-DL-LOADED                                 11030000
                   GO TO P50000-EXIT.                                   11040000
                                                                        11050000
           ADD +1 TO WS-PURCHASE-TYPE-IN.                               11060000
                                                                        11070000
           IF PDA-SPACER-REC                                            11080000
               IF PURCHASE-TYPE > SPACES                                11090000
                   ADD +1 TO WS-PURCHASE-TYPE-OUT                       11100000
                   PERFORM P51000-INSERT-PURCHASE-TYPE THRU P51000-EXIT 11110000
                   MOVE SPACES TO PURCHASE-TYPE                         11120000
               END-IF                                                   11130000
               GO TO P50000-EXIT                                        11140000
           END-IF.                                                      11150000
                                                                        11160000
           IF NOT PDA-DATA-REC                                          11170000
               MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                         11180000
               MOVE 'P50000' TO WPBE-PARAGRAPH                          11190000
               MOVE 'UNIDENTIFIED INPUT RECORD,' TO WPBE-MESSAGE        11200000
               MOVE WS-PURCHASE-TYPE-IN TO WPBE-RECORD-NUMBER           11210000
               PERFORM P99999-ABEND THRU P99999-EXIT                    11220000
           END-IF.                                                      11230000
                                                                        11240000
           EVALUATE TRUE                                                11250000
               WHEN PDA-FIELD-NAME = 'PURCHASE-TYPE.PREFIX'             11260000
                   MOVE PDA-DATA-005 TO PURCHASE-TYPE-PREFIX            11270000
                   IF PDA-DATA-005 NOT = WS-USERID                      11280000
                       IF WS-USERID = SPACES                            11290000
                           MOVE PDA-DATA-005 TO WS-USERID               11300000
                       ELSE                                             11310000
                           MOVE WS-USERID TO WS-DL-L-ID                 11320000
                           MOVE 'DB2 PURCHASE TYPE ROWS LOADED' TO      11330000
                               WS-DL-L-TITLE                            11340000
                           MOVE WS-PURCHASE-TYPE-OUT TO WS-DL-L-CNT     11350000
                           ADD WS-PURCHASE-TYPE-OUT TO                  11360000
                               WS-PURCHASE-TYPE-TOT                     11370000
                           MOVE +0 TO WS-PURCHASE-TYPE-OUT              11380000
      *                    DISPLAY WS-DL-LOADED                         11390000
                           MOVE PDA-DATA-005 TO WS-USERID               11400000
                       END-IF                                           11410000
                   END-IF                                               11420000
               WHEN PDA-FIELD-NAME = 'PURCHASE-TYPE.TYPE'               11430000
                   MOVE PDA-DATA-003 TO PURCHASE-TYPE-TYPE              11440000
               WHEN PDA-FIELD-NAME = 'PURCHASE-TYPE.DESCRIPTION'        11450000
                   MOVE PDA-DATA-032 TO PURCHASE-TYPE-DESCRIPTION       11460000
               WHEN OTHER                                               11470000
                   MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                     11480000
                   MOVE 'P50000' TO WPBE-PARAGRAPH                      11490000
                   MOVE 'UNIDENTIFIED INPUT RECORD FIELD NAME,' TO      11500000
                        WPBE-MESSAGE                                    11510000
                   MOVE WS-PURCHASE-TYPE-IN TO WPBE-RECORD-NUMBER       11520000
                   PERFORM P99999-ABEND THRU P99999-EXIT                11530000
           END-EVALUATE.                                                11540000
                                                                        11550000
       P50000-EXIT.                                                     11560000
           EXIT.                                                        11570000
           EJECT                                                        11580000
      ***************************************************************** 11590000
      *                                                               * 11600000
      *    PARAGRAPH:  P51000-INSERT-PURCHASE-TYPE                    * 11610000
      *                                                               * 11620000
      *    FUNCTION :  ROUTINE TO INSERT TO THE PURCHASE-TYPE TABLE   * 11630000
      *                                                               * 11640000
      *    CALLED BY:  P50000-LOAD-PURCHASE-TYPE                      * 11650000
      *                                                               * 11660000
      ***************************************************************** 11670000
                                                                        11680000
       P51000-INSERT-PURCHASE-TYPE.                                     11690000
                                                                        11700000
           EXEC SQL                                                     11710000
               INSERT                                                   11720000
               INTO   PURCHASE_TYPE                                     11730000
                     (PREFIX,                                           11740000
                      TYPE,                                             11750000
                      DESCRIPTION)                                      11760000
               VALUES                                                   11770000
                     (:PURCHASE-TYPE-PREFIX,                            11780000
                      :PURCHASE-TYPE-TYPE,                              11790000
                      :PURCHASE-TYPE-DESCRIPTION)                       11800000
           END-EXEC.                                                    11810000
                                                                        11820000
           IF SQLCODE NOT = +0                                          11830000
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE                          11840000
               MOVE 'PDAB02' TO WPDE-PROGRAM-ID                         11850000
               MOVE SQLCODE TO WPDE-DB2-SQLCODE                         11860000
               MOVE 'INSERT TO PURCHASE TYPE TABLE' TO WPDE-FUNCTION    11870000
               MOVE 'P51000' TO WPDE-PARAGRAPH                          11880000
               MOVE PURCHASE-TYPE TO PDA-INPUT-FORMAT                   11890000
               PERFORM P99999-ABEND THRU P99999-EXIT                    11900000
           END-IF.                                                      11910000
                                                                        11920000
       P51000-EXIT.                                                     11930000
           EXIT.                                                        11940000
           EJECT                                                        11950000
      ***************************************************************** 11960000
      *                                                               * 11970000
      *    PARAGRAPH:  P60000-LOAD-ORDER                              * 11980000
      *                                                               * 11990000
      *    FUNCTION :  ROUTINE TO LOAD THE ORDER DATABASE             * 12000000
      *                                                               * 12010000
      *    CALLED BY:  P00000-MAINLINE                                * 12020000
      *                                                               * 12030000
      ***************************************************************** 12040000
                                                                        12050000
       P60000-LOAD-ORDER.                                               12060000
                                                                        12070000
           READ INPUT-ORDER INTO PDA-INPUT-FORMAT                       12080000
               AT END                                                   12090000
                   MOVE 'Y' TO WS-END-OF-ORDER-SW                       12100000
                   IF ORDER-SEGMENT > SPACES                            12110000
                       ADD +1 TO WS-ORDER-OUT                           12120000
                       PERFORM P61000-INSERT-ORDER THRU P61000-EXIT     12130000
                   END-IF                                               12140000
                   MOVE WS-USERID TO WS-DL-L-ID                         12150000
                   MOVE 'IMS ORDER RECORDS LOADED' TO WS-DL-L-TITLE     12160000
                   MOVE WS-ORDER-OUT TO WS-DL-L-CNT                     12170000
                   ADD WS-ORDER-OUT TO WS-ORDER-TOT                     12180000
      *            DISPLAY WS-DL-LOADED                                 12190000
                   IF ORDER-ITEM-SEGMENT > SPACES                       12200000
                       ADD +1 TO WS-ORDER-ITEM-OUT                      12210000
                       PERFORM P62000-INSERT-ORDER-ITEM THRU P62000-EXIT12220000
                   END-IF                                               12230000
                   MOVE WS-USERID TO WS-DL-L-ID                         12240000
                   MOVE 'IMS ORDER ITEM RECORDS LOADED' TO WS-DL-L-TITLE12250000
                   MOVE WS-ORDER-ITEM-OUT TO WS-DL-L-CNT                12260000
                   ADD WS-ORDER-ITEM-OUT TO WS-ORDER-ITEM-TOT           12270000
      *            DISPLAY WS-DL-LOADED                                 12280000
                   GO TO P60000-EXIT.                                   12290000
                                                                        12300000
           ADD +1 TO WS-ORDER-IN.                                       12310000
                                                                        12320000
           IF PDA-SPACER-REC                                            12330000
               IF ORDER-SEGMENT > SPACES                                12340000
                   ADD +1 TO WS-ORDER-OUT                               12350000
                   PERFORM P61000-INSERT-ORDER THRU P61000-EXIT         12360000
               END-IF                                                   12370000
               IF ORDER-ITEM-SEGMENT > SPACES                           12380000
                   ADD +1 TO WS-ORDER-ITEM-OUT                          12390000
                   PERFORM P62000-INSERT-ORDER-ITEM THRU P62000-EXIT    12400000
               END-IF                                                   12410000
               GO TO P60000-EXIT                                        12420000
           END-IF.                                                      12430000
                                                                        12440000
           IF NOT PDA-DATA-REC                                          12450000
               MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                         12460000
               MOVE 'P60000' TO WPBE-PARAGRAPH                          12470000
               MOVE 'UNIDENTIFIED INPUT RECORD,' TO WPBE-MESSAGE        12480000
               MOVE WS-ORDER-IN TO WPBE-RECORD-NUMBER                   12490000
               PERFORM P99999-ABEND THRU P99999-EXIT                    12500000
           END-IF.                                                      12510000
                                                                        12520000
           EVALUATE TRUE                                                12530000
               WHEN PDA-FIELD-NAME = 'ORDER.PREFIX'                     12540000
                   MOVE PDA-DATA-005-N TO ORDER-PREFIX                  12550000
                                          OS-ORDER-PREFIX               12560000
                   IF PDA-DATA-005 NOT = WS-USERID                      12570000
                       IF WS-USERID = SPACES                            12580000
                           MOVE PDA-DATA-005 TO WS-USERID               12590000
                       ELSE                                             12600000
                           MOVE WS-USERID TO WS-DL-L-ID                 12610000
                           MOVE 'IMS ORDER RECORDS LOADED' TO           12620000
                               WS-DL-L-TITLE                            12630000
                           MOVE WS-ORDER-OUT TO WS-DL-L-CNT             12640000
                           ADD WS-ORDER-OUT TO WS-ORDER-TOT             12650000
                           MOVE +0 TO WS-ORDER-OUT                      12660000
      *                    DISPLAY WS-DL-LOADED                         12670000
                           MOVE 'IMS ORDER ITEM RECORDS LOADED' TO      12680000
                               WS-DL-L-TITLE                            12690000
                           MOVE WS-ORDER-ITEM-OUT TO WS-DL-L-CNT        12700000
                           ADD WS-ORDER-ITEM-OUT TO WS-ORDER-ITEM-TOT   12710000
                           MOVE +0 TO WS-ORDER-ITEM-OUT                 12720000
      *                    DISPLAY WS-DL-LOADED                         12730000
                           MOVE PDA-DATA-005 TO WS-USERID               12740000
                       END-IF                                           12750000
                   END-IF                                               12760000
               WHEN PDA-FIELD-NAME = 'ORDER.NUMBER'                     12770000
                   MOVE WS-ORDER-NUMBER TO ORDER-NUMBER                 12780000
                                           OS-ORDER-NUMBER              12790000
                   ADD 1 TO WS-ORDER-NUMBER                             12800000
               WHEN PDA-FIELD-NAME = 'ORDER.PURCHASE-NUMBER'            12810000
                   MOVE PDA-DATA-013-N TO ORDER-PURCHASE-NUMBER         12820000
               WHEN PDA-FIELD-NAME = 'ORDER.DATE-YYMMDD'                12830000
                   MOVE PDA-DATA-006 TO ORDER-DATE-YYMMDD               12840000
               WHEN PDA-FIELD-NAME = 'ORDER.STATUS'                     12850000
                   MOVE PDA-DATA-032 TO ORDER-STATUS                    12860000
               WHEN PDA-FIELD-NAME = 'ORDER.TOTAL-AMOUNT'               12870000
                   MOVE PDA-DATA-009 TO WS-CHARACTER                    12880000
                   PERFORM P90000-CONVERT-NUMERIC THRU P90000-EXIT      12890000
                   MOVE WS-NUMERIC TO ORDER-TOTAL-AMOUNT                12900000
               WHEN PDA-FIELD-NAME = 'ORDER.NEXT-ITEM-SEQUENCE'         12910000
                   MOVE PDA-DATA-005-N TO ORDER-NEXT-ITEM-SEQUENCE      12920000
               WHEN PDA-FIELD-NAME = 'ORDER.CUSTOMER-PREFIX'            12930000
                   MOVE PDA-DATA-005-N TO ORDER-CUSTOMER-PREFIX         12940000
               WHEN PDA-FIELD-NAME = 'ORDER.CUSTOMER-ID'                12950000
                   MOVE PDA-DATA-032 TO ORDER-CUSTOMER-ID               12960000
               WHEN PDA-FIELD-NAME = 'ORDER.PURCHASE-TYPE-PREFIX'       12970000
                   MOVE PDA-DATA-005-N TO ORDER-PURCHASE-TYPE-PREFIX    12980000
               WHEN PDA-FIELD-NAME = 'ORDER.PURCHASE-TYPE'              12990000
                   MOVE PDA-DATA-003-N TO ORDER-PURCHASE-TYPE           13000000
               WHEN PDA-FIELD-NAME = 'ORDER.SHIPPER-NUMBER'             13010000
                   MOVE PDA-DATA-010 TO WS-CHARACTER                    13020000
                   PERFORM P90000-CONVERT-NUMERIC THRU P90000-EXIT      13030000
                   MOVE WS-NUMERIC TO ORDER-SHIPPER-NUMBER              13040000
               WHEN PDA-FIELD-NAME = 'ORDER-ITEM.PREFIX'                13050000
                   MOVE PDA-DATA-005-N TO ORDER-ITEM-PREFIX             13060000
               WHEN PDA-FIELD-NAME = 'ORDER-ITEM.SEQUENCE'              13070000
                   MOVE PDA-DATA-005-N TO ORDER-ITEM-SEQUENCE           13080000
               WHEN PDA-FIELD-NAME = 'ORDER-ITEM.QUANTITY'              13090000
                   MOVE PDA-DATA-009 TO WS-CHARACTER                    13100000
                   PERFORM P90000-CONVERT-NUMERIC THRU P90000-EXIT      13110000
                   MOVE WS-NUMERIC TO ORDER-ITEM-QUANTITY               13120000
               WHEN PDA-FIELD-NAME = 'ORDER-ITEM.UNIT-PRICE'            13130000
                   MOVE PDA-DATA-010 TO WS-CHARACTER                    13140000
                   PERFORM P90000-CONVERT-NUMERIC THRU P90000-EXIT      13150000
                   MOVE WS-NUMERIC TO ORDER-ITEM-UNIT-PRICE             13160000
               WHEN PDA-FIELD-NAME = 'ORDER-ITEM.ITEM-PREFIX'           13170000
                   MOVE PDA-DATA-005-N TO ORDER-ITEM-ITEM-PREFIX        13180000
               WHEN PDA-FIELD-NAME = 'ORDER-ITEM.ITEM-NUMBER'           13190000
                   MOVE PDA-DATA-032 TO ORDER-ITEM-ITEM-NUMBER          13200000
               WHEN PDA-FIELD-NAME = 'ORDER-ITEM.SUPPLIER-PREFIX'       13210000
                   MOVE PDA-DATA-005-N TO ORDER-ITEM-SUPPLIER-PREFIX    13220000
               WHEN PDA-FIELD-NAME = 'ORDER-ITEM.SUPPLIER-ID'           13230000
                   MOVE PDA-DATA-032 TO ORDER-ITEM-SUPPLIER-ID          13240000
               WHEN OTHER                                               13250000
                   MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                     13260000
                   MOVE 'P60000' TO WPBE-PARAGRAPH                      13270000
                   MOVE 'UNIDENTIFIED INPUT RECORD FIELD NAME,' TO      13280000
                        WPBE-MESSAGE                                    13290000
                   MOVE WS-ORDER-IN TO WPBE-RECORD-NUMBER               13300000
                   PERFORM P99999-ABEND THRU P99999-EXIT                13310000
           END-EVALUATE.                                                13320000
                                                                        13330000
       P60000-EXIT.                                                     13340000
           EXIT.                                                        13350000
           EJECT                                                        13360000
      ***************************************************************** 13370000
      *                                                               * 13380000
      *    PARAGRAPH:  P61000-INSERT-ORDER                            * 13390000
      *                                                               * 13400000
      *    FUNCTION :  ROUTINE TO INSERT THE ORDER TO THE ORDER       * 13410000
      *                DATABASE                                       * 13420000
      *                                                               * 13430000
      *    CALLED BY:  P60000-LOAD-ORDER                              * 13440000
      *                                                               * 13450000
      ***************************************************************** 13460000
                                                                        13470000
       P61000-INSERT-ORDER.                                             13480000
                                                                        13490000
           CALL 'CBLTDLI' USING                                         13500000
                          WS-ISRT                                       13510000
                          ORDER-PCB                                     13520000
                          ORDER-SEGMENT                                 13530000
                          ORDER-SSA-UNQUAL                              13540000
           END-CALL.                                                    13550000
                                                                        13560000
           MOVE OP-STATUS TO WS-OP-STATUS.                              13570000
                                                                        13580000
           IF OP-GOOD-RETURN                                            13590000
               MOVE SPACES TO ORDER-SEGMENT                             13600000
           ELSE                                                         13610000
               MOVE 'IMS' TO WS-PDA-ERROR-TYPE                          13620000
               MOVE 'PDAB02' TO WPIE-PROGRAM-ID                         13630000
               MOVE 'P61000' TO WPIE-PARAGRAPH                          13640000
               MOVE OP-STATUS TO WPIE-STATUS-CODE                       13650000
               MOVE 'ISRT' TO WPIE-FUNCTION-CODE                        13660000
               MOVE 'ORDER' TO WPIE-SEGMENT-NAME                        13670000
               MOVE 'ORDER' TO WPIE-DATABASE-NAME                       13680000
               MOVE 'INSERT ORDER TO DATABASE' TO WPIE-COMMAND          13690000
               PERFORM P99999-ABEND THRU P99999-EXIT                    13700000
           END-IF.                                                      13710000
                                                                        13720000
       P61000-EXIT.                                                     13730000
           EXIT.                                                        13740000
           EJECT                                                        13750000
      ***************************************************************** 13760000
      *                                                               * 13770000
      *    PARAGRAPH:  P62000-INSERT-ORDER-ITEM                       * 13780000
      *                                                               * 13790000
      *    FUNCTION :  ROUTINE TO INSERT THE ORDER-ITEM TO THE ORDER  * 13800000
      *                DATABASE                                       * 13810000
      *                                                               * 13820000
      *    CALLED BY:  P60000-LOAD-ORDER                              * 13830000
      *                                                               * 13840000
      ***************************************************************** 13850000
                                                                        13860000
       P62000-INSERT-ORDER-ITEM.                                        13870000
                                                                        13880000
           CALL 'CBLTDLI' USING                                         13890000
                          WS-ISRT                                       13900000
                          ORDER-PCB                                     13910000
                          ORDER-ITEM-SEGMENT                            13920000
                          ORDER-SSA                                     13930000
                          ORDER-ITEM-SSA-UNQUAL                         13940000
           END-CALL.                                                    13950000
                                                                        13960000
           MOVE OP-STATUS TO WS-OP-STATUS.                              13970000
                                                                        13980000
           IF OP-GOOD-RETURN                                            13990000
               MOVE SPACES TO ORDER-ITEM-SEGMENT                        14000000
           ELSE                                                         14010000
               MOVE 'IMS' TO WS-PDA-ERROR-TYPE                          14020000
               MOVE 'PDAB02' TO WPIE-PROGRAM-ID                         14030000
               MOVE 'P62000' TO WPIE-PARAGRAPH                          14040000
               MOVE OP-STATUS TO WPIE-STATUS-CODE                       14050000
               MOVE 'ISRT' TO WPIE-FUNCTION-CODE                        14060000
               MOVE 'ORDITEM' TO WPIE-SEGMENT-NAME                      14070000
               MOVE 'ORDER' TO WPIE-DATABASE-NAME                       14080000
               MOVE 'INSERT ORDER ITEM TO DATABASE' TO WPIE-COMMAND     14090000
               PERFORM P99999-ABEND THRU P99999-EXIT                    14100000
           END-IF.                                                      14110000
                                                                        14120000
       P62000-EXIT.                                                     14130000
           EXIT.                                                        14140000
           EJECT                                                        14150000
      ***************************************************************** 14160000
      *                                                               * 14170000
      *    PARAGRAPH:  P90000-CONVERT-NUMERIC                         * 14180000
      *                                                               * 14190000
      *    FUNCTION :  ROUTINE TO CONVERT THE EDITTED DATA TO A       * 14200000
      *                STRAIGHT NUMERIC VALUE                         * 14210000
      *                                                               * 14220000
      *    CALLED BY:  VARIOUS                                        * 14230000
      *                                                               * 14240000
      ***************************************************************** 14250000
                                                                        14260000
       P90000-CONVERT-NUMERIC.                                          14270000
                                                                        14280000
           MOVE ZEROES TO WS-NUMERIC.                                   14290000
                                                                        14300000
           PERFORM P91000-FIND-DECIMAL THRU P91000-EXIT                 14310000
               VARYING WS-SUB-CHAR FROM 1 BY 1                          14320000
                   UNTIL WS-SUB-CHAR > 15.                              14330000
                                                                        14340000
           MOVE WS-SUB-DECIMAL TO WS-SUB-CHAR.                          14350000
                                                                        14360000
           ADD 1 TO WS-SUB-CHAR.                                        14370000
                                                                        14380000
           IF WS-CHAR(WS-SUB-CHAR) NUMERIC                              14390000
               MOVE WS-CHAR(WS-SUB-CHAR) TO WS-NUM(14)                  14400000
           END-IF.                                                      14410000
                                                                        14420000
           ADD 1 TO WS-SUB-CHAR.                                        14430000
                                                                        14440000
           IF WS-CHAR(WS-SUB-CHAR) NUMERIC                              14450000
               MOVE WS-CHAR(WS-SUB-CHAR) TO WS-NUM(15)                  14460000
           END-IF.                                                      14470000
                                                                        14480000
           MOVE WS-SUB-DECIMAL TO WS-SUB-CHAR.                          14490000
                                                                        14500000
           PERFORM P92000-COPY-NUMBERS THRU P92000-EXIT                 14510000
               VARYING WS-SUB-NUM FROM 13 BY -1                         14520000
                   UNTIL WS-SUB-CHAR < 1.                               14530000
                                                                        14540000
       P90000-EXIT.                                                     14550000
           EXIT.                                                        14560000
           EJECT                                                        14570000
      ***************************************************************** 14580000
      *                                                               * 14590000
      *    PARAGRAPH:  P91000-FIND-DECIMAL                            * 14600000
      *                                                               * 14610000
      *    FUNCTION :  ROUTINE TO FIND THE DECIMAL POINT IN THE       * 14620000
      *                INCOMMING DATA                                 * 14630000
      *                                                               * 14640000
      *    CALLED BY:  P90000-CONVERT-NUMERIC                         * 14650000
      *                                                               * 14660000
      ***************************************************************** 14670000
                                                                        14680000
       P91000-FIND-DECIMAL.                                             14690000
                                                                        14700000
           IF WS-CHAR(WS-SUB-CHAR) = '.'                                14710000
               MOVE WS-SUB-CHAR TO WS-SUB-DECIMAL                       14720000
               MOVE 16 TO WS-SUB-CHAR                                   14730000
           END-IF.                                                      14740000
                                                                        14750000
       P91000-EXIT.                                                     14760000
           EXIT.                                                        14770000
           EJECT                                                        14780000
      ***************************************************************** 14790000
      *                                                               * 14800000
      *    PARAGRAPH:  P92000-COPY-NUMBERS                            * 14810000
      *                                                               * 14820000
      *    FUNCTION :  ROUTINE TO COPY THE NUMBERS TO THE LEFT OF THE * 14830000
      *                DECIMAL POINT                                  * 14840000
      *                                                               * 14850000
      *    CALLED BY:  P90000-CONVERT-NUMERIC                         * 14860000
      *                                                               * 14870000
      ***************************************************************** 14880000
                                                                        14890000
       P92000-COPY-NUMBERS.                                             14900000
                                                                        14910000
           SUBTRACT 1 FROM WS-SUB-CHAR.                                 14920000
                                                                        14930000
           IF WS-SUB-CHAR > 0                                           14940000
               MOVE WS-CHAR(WS-SUB-CHAR) TO WS-NUM(WS-SUB-NUM)          14950000
           END-IF.                                                      14960000
                                                                        14970000
       P92000-EXIT.                                                     14980000
           EXIT.                                                        14990000
           EJECT                                                        15000000
      ***************************************************************** 15010000
      *                                                               * 15020000
      *    PARAGRAPH:  P99999-ABEND                                   * 15030000
      *                                                               * 15040000
      *    FUNCTION :  ROUTINE TO ABEND THE PROGRAM WHEN A CRITICAL   * 15050000
      *                ERROR HAS BEEN ENCOUNTERED                     * 15060000
      *                                                               * 15070000
      *    CALLED BY:  VARIOUS                                        * 15080000
      *                                                               * 15090000
      ***************************************************************** 15100000
                                                                        15110000
       P99999-ABEND.                                                    15120000
                                                                        15130000
           DISPLAY ' '.                                                 15140000
           DISPLAY WPEA-ERROR-01.                                       15150000
           DISPLAY WPEA-ERROR-02.                                       15160000
           DISPLAY WPEA-ERROR-03.                                       15170000
           DISPLAY WPEA-ERROR-04.                                       15180000
           DISPLAY WPEA-ERROR-05.                                       15190000
           DISPLAY WPEA-ERROR-06.                                       15200000
                                                                        15210000
           EVALUATE TRUE                                                15220000
               WHEN PDA-DB2-ERROR                                       15230000
                   MOVE WS-PDA-DB2-ERROR-01 TO WPEA-ERROR-07-TEXT       15240000
                   DISPLAY WPEA-ERROR-07                                15250000
                   DISPLAY WPEA-ERROR-06                                15260000
                   MOVE WS-PDA-DB2-ERROR-02 TO WPEA-ERROR-08-TEXT       15270000
                   DISPLAY WPEA-ERROR-08                                15280000
                   DISPLAY WPEA-ERROR-06                                15290000
                   MOVE PDA-INPUT-FORMAT TO WPEA-ERROR-08-TEXT          15300000
                   DISPLAY WPEA-ERROR-08                                15310000
               WHEN PDA-IMS-ERROR                                       15320000
                   MOVE WS-PDA-IMS-ERROR-01 TO WPEA-ERROR-07-TEXT       15330000
                   DISPLAY WPEA-ERROR-07                                15340000
                   DISPLAY WPEA-ERROR-06                                15350000
                   MOVE WS-PDA-IMS-ERROR-02 TO WPEA-ERROR-08-TEXT       15360000
                   DISPLAY WPEA-ERROR-08                                15370000
               WHEN OTHER                                               15380000
                   MOVE WS-PDA-BATCH-ERROR-01 TO WPEA-ERROR-07-TEXT     15390000
                   DISPLAY WPEA-ERROR-07                                15400000
                   DISPLAY WPEA-ERROR-06                                15410000
                   MOVE WS-PDA-BATCH-ERROR-02 TO WPEA-ERROR-08-TEXT     15420000
                   DISPLAY WPEA-ERROR-08                                15430000
                   DISPLAY WPEA-ERROR-06                                15440000
                   MOVE WS-PDA-BATCH-ERROR-03 TO WPEA-ERROR-08-TEXT     15450000
                   DISPLAY WPEA-ERROR-08                                15460000
                   DISPLAY WPEA-ERROR-06                                15470000
                   MOVE WS-PDA-BATCH-ERROR-04 TO WPEA-ERROR-08-TEXT     15480000
                   DISPLAY WPEA-ERROR-08                                15490000
                   MOVE PDA-INPUT-FORMAT TO WPEA-ERROR-08-TEXT          15500000
                   DISPLAY WPEA-ERROR-08                                15510000
           END-EVALUATE.                                                15520000
                                                                        15530000
           DISPLAY WPEA-ERROR-09.                                       15540000
           DISPLAY WPEA-ERROR-10.                                       15550000
           DISPLAY ' '.                                                 15560000
                                                                        15570000
           MOVE 99 TO WS-RETURN-CODE.                                   15580000
                                                                        15590000
           CALL 'ILBOABN0' USING WS-RETURN-CODE.                        15600000
                                                                        15610000
           MOVE WS-RETURN-CODE TO RETURN-CODE.                          15620000
                                                                        15630000
           GOBACK.                                                      15640000
                                                                        15650000
       P99999-EXIT.                                                     15660000
           EXIT.                                                        15670000
           EJECT                                                        15680000