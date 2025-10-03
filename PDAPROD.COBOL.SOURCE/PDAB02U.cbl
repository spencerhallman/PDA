       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. PDAB02U.                                             00020000
      *                                                                 00030000
      ***************************************************************** 00040000
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       * 00050000
      *                       COMPUWARE CORPORATION                   * 00060000
      *                                                               * 00070000
      * PROGRAM :   PDAB02U                                           * 00080000
      *                                                               * 00090000
      * FUNCTION:   PROGRAM PDAB02U IS A BATCH PROGRAM THAT WILL LOAD * 00100000
      *             THE USERID ZERO RECORDS TO THE FOLLOWING FILES:   * 00110000
      *                  1)  CUSTOMER FILE               (VSAM)       * 00120000
      *                  2)  ITEM TABLE                  (DB2)        * 00130000
      *                  3)  SUPPLIER TABLE              (DB2)        * 00140000
      *                  4)  ITEM SUPPLIER TABLE         (DB2)        * 00150000
      *                  5)  PURCHASE TYPE TABLE         (DB2)        * 00160000
      *                  6)  ORDER DATABASE              (IMS)        * 00170000
      *        **** NON-CICS FILES/TABLES ADDED FOR FA 4/12/02 ****   * 00170100
      *                  7)  AFFILIATE SUPPLIER TABLE    (DB2)        * 00170200
      *                  8)  ORDER LOG TABLE             (DB2)        * 00170300
      *                  9)  REPORT ORDER FILE           (VSAM)       * 00170400
      *                 10)  AFFILIATE CUSTOMER FILE     (VSAM)       * 00170500
      *                                                               * 00170600
      * FILES   :   CUSTOMER FILE         -  VSAM KSDS     (UPDATE)   * 00170700
      *             ITEM TABLE            -  DB2           (UPDATE)   * 00170800
      *             SUPPLIER TABLE        -  DB2           (UPDATE)   * 00170900
      *             ITEM SUPPLIER TABLE   -  DB2           (UPDATE)   * 00171000
      *             PURCHASE TYPES TABLE  -  DB2           (UPDATE)   * 00172000
      *             ORDER DATABASE        -  IMS           (UPDATE)   * 00173000
      *        **** NON-CICS FILES/TABLES ADDED FOR FA *****          * 00174000
      *             AFFILIATE SUPPLIER    - DB2            (UPDATE)   * 00175000
      *             ORDER LOG TABLE       - DB2            (UPDATE)   * 00176000
      *             REPORT ORDER FILE     - VSAM KSDS      (UPDATE)   * 00177000
      *             AFFILIATE CUSTOMER    - VSAM KSDS      (UPDATE)   * 00178000
      *                                                               * 00179000
      ***************************************************************** 00180000
      *             PROGRAM CHANGE LOG                                * 00190000
      *             -------------------                               * 00200000
      *                                                               * 00210000
      *  DATE       UPDATED BY            CHANGE DESCRIPTION          * 00220000
      *  --------   --------------------  --------------------------  * 00230000
      *  12/14/05   PAUL BARON            ELIMINATE USE OF FIELD      * 00240000
      *                                   CUSTOMER-TOTAL-DOLLAR-AMT-R * 00250000
      *                                                               * 00283000
      *  08/02/02   PWB                   INITIALIZE NEW FIELDS:      * 00283100
      *                                   IN THE CUSTOMER FILE INIT   * 00283200
      *                                   CUSTOMER-LAST-ORDER-AMT     * 00283300
      *                                   TO ZEROES AND IN THE        * 00283400
      *                                   AFFILIATE CUSTOMER INIT     * 00283500
      *                                   AFF-CUSTOMER-LAST-ORDER-AMT * 00283600
      *                                   TO ZEROES.                  * 00283700
      *                                                               * 00283800
      *  04/12/02   JLS                   NEW COPY OF PDAB02 FOR USE  * 00284000
      *                                   BY PROC APDAUSER. CREATES   * 00285000
      *                                   COPIES OF ALL CICS FILES/TBL* 00286000
      *                                   PLUS NEW FILES/TBLS TO FUL- * 00287000
      *                                   FILL FA REQUIREMENTS        * 00288000
      *                                                               * 00289000
      ***************************************************************** 00290000
           EJECT                                                        00300000
       ENVIRONMENT DIVISION.                                            00310000
                                                                        00320000
       INPUT-OUTPUT SECTION.                                            00330000
                                                                        00340000
       FILE-CONTROL.                                                    00350000
                                                                        00360000
           SELECT INPUT-CUSTOMER       ASSIGN TO ICUSTOMR.              00370000
                                                                        00380000
           SELECT INPUT-ITEM           ASSIGN TO IITEM.                 00390000
                                                                        00400000
           SELECT INPUT-SUPPLIER       ASSIGN TO ISUPPLR.               00410000
                                                                        00420000
           SELECT INPUT-ITEM-SUPPLIER  ASSIGN TO IITMSUP.               00430000
                                                                        00440000
           SELECT INPUT-PURCHASE-TYPE  ASSIGN TO IPURTYP.               00450000
                                                                        00460000
           SELECT INPUT-ORDER          ASSIGN TO IORDER.                00470000
                                                                        00480000
           SELECT INPUT-AFF-CUSTOMER   ASSIGN TO IAFFCUST.              00490000
                                                                        00500000
           SELECT INPUT-ORDER-LOG      ASSIGN TO IORDERLG.              00510000
                                                                        00520000
           SELECT INPUT-AFF-SUPPLIER   ASSIGN TO IAFFSUPP.              00530000
                                                                        00540000
           SELECT INPUT-REPORT-ORDER   ASSIGN TO IRPTORDR.              00541000
                                                                        00542000
           SELECT VSAM-CUSTOMER        ASSIGN TO VCUSTOMR               00543000
                                       ORGANIZATION IS INDEXED          00544000
                                       ACCESS IS DYNAMIC                00545000
                                       RECORD KEY IS CUSTOMER-KEY       00546000
                                       FILE STATUS IS WS-CUSTOMR-STATUS.00547000
                                                                        00548000
           SELECT VSAM-AFF-CUSTOMER    ASSIGN TO VAFFCUST               00549000
                                       ORGANIZATION IS INDEXED          00550000
                                       ACCESS IS DYNAMIC                00560000
                                       RECORD KEY IS AFF-CUSTOMER-KEY   00570000
                                       FILE STATUS IS WS-AFFCUST-STATUS.00580000
                                                                        00590000
           SELECT VSAM-REPORT-ORDER    ASSIGN TO VRPTORDR               00591000
                                       ORGANIZATION IS INDEXED          00592000
                                       ACCESS IS DYNAMIC                00593000
                                       RECORD KEY IS REPORT-ORDER-KEY   00594000
                                       FILE STATUS IS WS-RPTORDR-STATUS.00595000
           EJECT                                                        00596000
       DATA DIVISION.                                                   00597000
                                                                        00598000
       FILE SECTION.                                                    00599000
                                                                        00600000
       FD INPUT-CUSTOMER                                                00610000
           LABEL RECORDS ARE STANDARD                                   00620000
           RECORDING MODE IS F                                          00630000
           RECORD CONTAINS 256 CHARACTERS                               00640000
           BLOCK CONTAINS 27904 CHARACTERS.                             00650000
                                                                        00660000
       01  INPUT-CUSTOMER-REC          PIC X(256).                      00670000
                                                                        00680000
                                                                        00690000
       FD INPUT-ITEM                                                    00700000
           LABEL RECORDS ARE STANDARD                                   00710000
           RECORDING MODE IS F                                          00720000
           RECORD CONTAINS 256 CHARACTERS                               00730000
           BLOCK CONTAINS 27904 CHARACTERS.                             00740000
                                                                        00750000
       01  INPUT-ITEM-REC              PIC X(256).                      00760000
                                                                        00770000
                                                                        00780000
       FD INPUT-SUPPLIER                                                00790000
           LABEL RECORDS ARE STANDARD                                   00800000
           RECORDING MODE IS F                                          00810000
           RECORD CONTAINS 256 CHARACTERS                               00820000
           BLOCK CONTAINS 27904 CHARACTERS.                             00830000
                                                                        00840000
       01  INPUT-SUPPLIER-REC          PIC X(256).                      00850000
                                                                        00860000
                                                                        00870000
       FD INPUT-ITEM-SUPPLIER                                           00880000
           LABEL RECORDS ARE STANDARD                                   00890000
           RECORDING MODE IS F                                          00900000
           RECORD CONTAINS 256 CHARACTERS                               00910000
           BLOCK CONTAINS 27904 CHARACTERS.                             00920000
                                                                        00930000
       01  INPUT-ITEM-SUPPLIER-REC     PIC X(256).                      00940000
                                                                        00950000
                                                                        00960000
       FD INPUT-PURCHASE-TYPE                                           00970000
           LABEL RECORDS ARE STANDARD                                   00980000
           RECORDING MODE IS F                                          00990000
           RECORD CONTAINS 256 CHARACTERS                               01000000
           BLOCK CONTAINS 27904 CHARACTERS.                             01010000
                                                                        01020000
       01  INPUT-PURCHASE-TYPE-REC     PIC X(256).                      01030000
                                                                        01040000
                                                                        01050000
       FD INPUT-ORDER                                                   01060000
           LABEL RECORDS ARE STANDARD                                   01070000
           RECORDING MODE IS F                                          01080000
           RECORD CONTAINS 256 CHARACTERS                               01090000
           BLOCK CONTAINS 27904 CHARACTERS.                             01100000
                                                                        01110000
       01  INPUT-ORDER-REC             PIC X(256).                      01120000
                                                                        01130000
       FD INPUT-AFF-CUSTOMER                                            01140000
           LABEL RECORDS ARE STANDARD                                   01150000
           RECORDING MODE IS F                                          01160000
           RECORD CONTAINS 256 CHARACTERS                               01161000
           BLOCK CONTAINS 27904 CHARACTERS.                             01162000
                                                                        01163000
       01  INPUT-AFF-CUSTOMER-REC      PIC X(256).                      01164000
                                                                        01165000
       FD INPUT-ORDER-LOG                                               01166000
           LABEL RECORDS ARE STANDARD                                   01167000
           RECORDING MODE IS F                                          01168000
           RECORD CONTAINS 256 CHARACTERS                               01169000
           BLOCK CONTAINS 27904 CHARACTERS.                             01169100
                                                                        01169200
       01  INPUT-ORDER-LOG-REC         PIC X(256).                      01169300
                                                                        01169400
       FD INPUT-AFF-SUPPLIER                                            01169500
           LABEL RECORDS ARE STANDARD                                   01169600
           RECORDING MODE IS F                                          01169700
           RECORD CONTAINS 256 CHARACTERS                               01169800
           BLOCK CONTAINS 27904 CHARACTERS.                             01169900
                                                                        01170000
       01  INPUT-AFF-SUPPLIER-REC      PIC X(256).                      01170100
                                                                        01170200
       FD INPUT-REPORT-ORDER                                            01170300
           LABEL RECORDS ARE STANDARD                                   01170400
           RECORDING MODE IS F                                          01170500
           RECORD CONTAINS 256 CHARACTERS                               01170600
           BLOCK CONTAINS 27904 CHARACTERS.                             01170700
                                                                        01170800
       01  INPUT-REPORT-ORDER-REC      PIC X(256).                      01170900
           EJECT                                                        01171000
       FD  VSAM-CUSTOMER                                                01172000
           RECORD CONTAINS 733 CHARACTERS.                              01173000
                                                                        01174000
           COPY VCUSTOMR.                                               01175000
       FD  VSAM-AFF-CUSTOMER                                            01176000
           RECORD CONTAINS 733 CHARACTERS.                              01177000
                                                                        01178000
           COPY VAFFCUST.                                               01179000
       FD  VSAM-REPORT-ORDER                                            01180000
           RECORD CONTAINS 130 CHARACTERS.                              01190000
                                                                        01200000
           COPY VRPTORDR.                                               01210000
           EJECT                                                        01211000
       WORKING-STORAGE SECTION.                                         01212000
                                                                        01213000
                                                                        01214000
      ***************************************************************** 01215000
      *    SWITCHES                                                   * 01216000
      ***************************************************************** 01217000
                                                                        01218000
       01  WS-SWITCHES.                                                 01219000
           05  WS-END-OF-CUSTOMER-SW   PIC X     VALUE 'N'.             01220000
               88  END-OF-CUSTOMER               VALUE 'Y'.             01230000
           05  WS-END-OF-ITEM-SW       PIC X     VALUE 'N'.             01240000
               88  END-OF-ITEM                   VALUE 'Y'.             01250000
           05  WS-END-OF-SUPPLIER-SW   PIC X     VALUE 'N'.             01260000
               88  END-OF-SUPPLIER               VALUE 'Y'.             01270000
           05  WS-END-OF-ITEM-SUPLR-SW PIC X     VALUE 'N'.             01280000
               88  END-OF-ITEM-SUPPLIER          VALUE 'Y'.             01290000
           05  WS-END-OF-PURCHASE-SW   PIC X     VALUE 'N'.             01300000
               88  END-OF-PURCHASE-TYPE          VALUE 'Y'.             01310000
           05  WS-END-OF-AFF-CUST-SW   PIC X     VALUE 'N'.             01320000
               88  END-OF-AFF-CUSTOMER           VALUE 'Y'.             01330000
           05  WS-END-OF-ORDER-LOG-SW  PIC X     VALUE 'N'.             01340000
               88  END-OF-ORDER-LOG              VALUE 'Y'.             01350000
           05  WS-END-OF-AFF-SUPLR-SW  PIC X     VALUE 'N'.             01360000
               88  END-OF-AFF-SUPPLIER           VALUE 'Y'.             01370000
           05  WS-END-OF-RPT-ORD-SW    PIC X     VALUE 'N'.             01380000
               88  END-OF-REPORT-ORDER           VALUE 'Y'.             01390000
           05  WS-END-OF-ORDER-SW      PIC X     VALUE 'N'.             01400000
               88  END-OF-ORDER                  VALUE 'Y'.             01410000
           05  WS-LOAD-ERROR-SW        PIC X     VALUE 'N'.             01420000
               88  LOAD-ERROR                    VALUE 'Y'.             01430000
                                                                        01440000
                                                                        01450000
      ***************************************************************** 01460000
      *    MISCELLANEOUS WORK FIELDS                                  * 01470000
      ***************************************************************** 01480000
                                                                        01490000
       01  WS-MISCELLANEOUS-FIELDS.                                     01500000
           03  WS-RETURN-CODE          PIC 9(4)  VALUE ZEROES   COMP.   01510000
           03  WS-CUSTOMR-STATUS       PIC XX    VALUE SPACES.          01520000
           03  WS-AFFCUST-STATUS       PIC XX    VALUE SPACES.          01530000
           03  WS-RPTORDR-STATUS       PIC XX    VALUE SPACES.          01531000
           03  WS-ISRT                 PIC X(4)  VALUE 'ISRT'.          01532000
           03  WS-OP-STATUS            PIC XX    VALUE SPACES.          01533000
               88  OP-GOOD-RETURN                VALUE '  '.            01534000
               88  OP-END-OF-DATABASE            VALUE 'GB'.            01535000
               88  OP-SEGMENT-NOT-FOUND          VALUE 'GE'.            01536000
               88  OP-END-OF-INPUT-MSG           VALUE 'QC'.            01537000
               88  OP-END-OF-INPUT-SEGMENT       VALUE 'QD'.            01538000
               88  OP-SEGMENT-ALREADY-EXISTS     VALUE 'II'.            01539000
               88  OP-CALL-IOPCB-FROM-BATCH      VALUE 'AL'.            01540000
               88  OP-SECURITY-VIOLATION         VALUE 'A4'.            01550000
           03  WS-CUSTOMER-IN          PIC S9(5) VALUE +0       COMP-3. 01560000
           03  WS-CUSTOMER-OUT         PIC S9(5) VALUE +0       COMP-3. 01570000
           03  WS-CUSTOMER-TOT         PIC S9(5) VALUE +0       COMP-3. 01580000
           03  WS-ITEM-IN              PIC S9(5) VALUE +0       COMP-3. 01590000
           03  WS-ITEM-OUT             PIC S9(5) VALUE +0       COMP-3. 01600000
           03  WS-ITEM-TOT             PIC S9(5) VALUE +0       COMP-3. 01610000
           03  WS-SUPPLIER-IN          PIC S9(5) VALUE +0       COMP-3. 01620000
           03  WS-SUPPLIER-OUT         PIC S9(5) VALUE +0       COMP-3. 01630000
           03  WS-SUPPLIER-TOT         PIC S9(5) VALUE +0       COMP-3. 01640000
           03  WS-ITEM-SUPPLIER-IN     PIC S9(5) VALUE +0       COMP-3. 01650000
           03  WS-ITEM-SUPPLIER-OUT    PIC S9(5) VALUE +0       COMP-3. 01660000
           03  WS-ITEM-SUPPLIER-TOT    PIC S9(5) VALUE +0       COMP-3. 01670000
           03  WS-PURCHASE-TYPE-IN     PIC S9(5) VALUE +0       COMP-3. 01680000
           03  WS-PURCHASE-TYPE-OUT    PIC S9(5) VALUE +0       COMP-3. 01690000
           03  WS-PURCHASE-TYPE-TOT    PIC S9(5) VALUE +0       COMP-3. 01700000
           03  WS-AFF-CUSTOMER-IN      PIC S9(5) VALUE +0       COMP-3. 01710000
           03  WS-AFF-CUSTOMER-OUT     PIC S9(5) VALUE +0       COMP-3. 01720000
           03  WS-AFF-CUSTOMER-TOT     PIC S9(5) VALUE +0       COMP-3. 01730000
           03  WS-ORDER-LOG-IN         PIC S9(5) VALUE +0       COMP-3. 01740000
           03  WS-ORDER-LOG-OUT        PIC S9(5) VALUE +0       COMP-3. 01750000
           03  WS-ORDER-LOG-TOT        PIC S9(5) VALUE +0       COMP-3. 01760000
           03  WS-AFF-SUPPLIER-IN      PIC S9(5) VALUE +0       COMP-3. 01770000
           03  WS-AFF-SUPPLIER-OUT     PIC S9(5) VALUE +0       COMP-3. 01780000
           03  WS-AFF-SUPPLIER-TOT     PIC S9(5) VALUE +0       COMP-3. 01781000
           03  WS-REPORT-ORDER-IN      PIC S9(5) VALUE +0       COMP-3. 01782000
           03  WS-REPORT-ORDER-OUT     PIC S9(5) VALUE +0       COMP-3. 01783000
           03  WS-REPORT-ORDER-TOT     PIC S9(5) VALUE +0       COMP-3. 01784000
           03  WS-ORDER-IN             PIC S9(5) VALUE +0       COMP-3. 01785000
           03  WS-ORDER-OUT            PIC S9(5) VALUE +0       COMP-3. 01786000
           03  WS-ORDER-TOT            PIC S9(5) VALUE +0       COMP-3. 01787000
           03  WS-ORDER-ITEM-OUT       PIC S9(5) VALUE +0       COMP-3. 01788000
           03  WS-ORDER-ITEM-TOT       PIC S9(5) VALUE +0       COMP-3. 01789000
           03  WS-USERID               PIC X(5)  VALUE SPACES.          01790000
           03  WS-DATE.                                                 01800000
               05  WS-DATE-YEAR        PIC X(4)  VALUE SPACES.          01810000
               05  WS-DATE-MONTH       PIC XX    VALUE SPACES.          01820000
               05  WS-DATE-DAY         PIC XX    VALUE SPACES.          01830000
           03  WS-ORDER-NUMBER         PIC 9(10) VALUE 4900.            01840000
           03  WS-REPORT-ORDER-NUMBER  PIC 9(10) VALUE 4900.            01850000
           03  WS-ORDER-LOG-NUMBER     PIC 9(10) VALUE 4900.            01860000
           03  WS-SUB-CHAR             PIC 99    VALUE ZEROES.          01870000
           03  WS-SUB-NUM              PIC 99    VALUE ZEROES.          01880000
           03  WS-SUB-DECIMAL          PIC 99    VALUE ZEROES.          01890000
           03  WS-CHARACTER            PIC X(15) VALUE SPACES.          01900000
           03  WS-CHAR                 REDEFINES WS-CHARACTER           01910000
                                       OCCURS 15 TIMES                  01920000
                                       PIC X.                           01930000
           03  WS-NUMERIC              PIC 9(13)V99 VALUE ZEROES.       01940000
           03  WS-NUM                  REDEFINES WS-NUMERIC             01950000
                                       OCCURS 15 TIMES                  01960000
                                       PIC 9.                           01970000
           03  WS-EMAIL-ADDRESS.                                        01980000
               05  WS-EA-PREFIX        PIC X(5)  VALUE SPACES.          01990000
               05  WS-EA-EMAIL-ADDRESS PIC X(123) VALUE SPACES.         02000000
           EJECT                                                        02010000
      ***************************************************************** 02020000
      *    DISPLAY AREA                                               * 02030000
      ***************************************************************** 02040000
                                                                        02050000
       01  WS-DISPLAY-LINES.                                            02060000
           03  WS-DL-ASTERISK.                                          02070000
               05  FILLER         PIC XX    VALUE SPACES.               02080000
               05  FILLER         PIC X(68) VALUE ALL '*'.              02090000
           03  WS-DL-SPACER.                                            02100000
               05  FILLER         PIC X(69) VALUE '  *'.                02110000
               05  FILLER         PIC X     VALUE '*'.                  02120000
           03  WS-DL-TITLE.                                             02130000
               05  FILLER    PIC X(24) VALUE '  * PDAB02U - LOAD BASE'. 02140000
               05  FILLER    PIC X(22) VALUE 'DATA TO THE PDA VSAM,'.   02150000
               05  FILLER    PIC X(24) VALUE 'DB2, AND IMS FILES     *'.02160000
           03  WS-DL-DATE.                                              02170000
               05  FILLER         PIC X(18) VALUE '  *        DATE ='.  02180000
               05  WS-DL-D-MONTH  PIC XX    VALUE SPACES.               02190000
               05  FILLER         PIC X     VALUE '/'.                  02200000
               05  WS-DL-D-DAY    PIC XX    VALUE SPACES.               02210000
               05  FILLER         PIC X     VALUE '/'.                  02220000
               05  WS-DL-D-YEAR   PIC X(4)  VALUE SPACES.               02230000
               05  FILLER         PIC X(41) VALUE SPACES.               02240000
               05  FILLER         PIC X     VALUE '*'.                  02250000
           03  WS-DL-LOADED.                                            02260000
               05  FILLER         PIC X(5)  VALUE '  *'.                02270000
               05  WS-DL-L-LIT.                                         02280000
                   07  FILLER     PIC X(5)  VALUE 'ID -'.               02290000
                   07  WS-DL-L-ID PIC X(5)  VALUE SPACES.               02300000
                   07  FILLER     PIC XX    VALUE ','.                  02310000
               05  WS-DL-L-CNT    PIC ZZ,ZZ9.                           02320000
               05  FILLER         PIC X(3)  VALUE ' -'.                 02330000
               05  WS-DL-L-TITLE  PIC X(43) VALUE SPACES.               02340000
               05  FILLER         PIC X     VALUE '*'.                  02350000
           EJECT                                                        02360000
      ***************************************************************** 02370000
      *    INPUT RECORD LAYOUT USED TO BUILD OUTPUT FILES             * 02380000
      ***************************************************************** 02390000
                                                                        02400000
       01  PDA-INPUT-FORMAT.                                            02410000
           03  PDA-FLAG                PIC X     VALUE SPACES.          02420000
               88  PDA-SPACER-REC                VALUE '*'.             02430000
               88  PDA-DATA-REC                  VALUE ' '.             02440000
           03  PDA-FIELD-NAME          PIC X(31) VALUE SPACES.          02450000
           03  FILLER                  PIC X(8)  VALUE SPACES.          02460000
           03  PDA-FIELD-DATA          PIC X(200) VALUE SPACES.         02470000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02480000
               05  PDA-DATA-128        PIC X(128).                      02490000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02500000
               05  PDA-DATA-064        PIC X(64).                       02510000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02520000
               05  PDA-DATA-050        PIC X(50).                       02530000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02540000
               05  PDA-DATA-032        PIC X(32).                       02550000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02560000
               05  PDA-DATA-013-N      PIC 9(13).                       02570000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02580000
               05  PDA-DATA-012        PIC X(12).                       02590000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02600000
               05  PDA-DATA-010        PIC X(10).                       02610000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02620000
               05  PDA-DATA-009        PIC X(9).                        02630000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02640000
               05  PDA-DATA-006        PIC X(6).                        02650000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02660000
               05  PDA-DATA-005        PIC X(5).                        02670000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02680000
               05  PDA-DATA-005-N      PIC 9(5).                        02690000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02700000
               05  PDA-DATA-003        PIC X(3).                        02710000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        02720000
               05  PDA-DATA-003-N      PIC 9(3).                        02730000
           EJECT                                                        02740000
      ***************************************************************** 02750000
      *    DB2  DEFINITIONS                                           * 02760000
      ***************************************************************** 02770000
                                                                        02780000
      ***************************************************************** 02790000
      *         SQL COMMUNICATIONS AREA                               * 02800000
      ***************************************************************** 02810000
                                                                        02820000
           EXEC SQL                                                     02830000
              INCLUDE SQLCA                                             02840000
           END-EXEC.                                                    02850000
           EJECT                                                        02860000
           EXEC SQL                                                     02870000
              INCLUDE DITEM                                             02880000
           END-EXEC.                                                    02890000
           EJECT                                                        02900000
           EXEC SQL                                                     02910000
              INCLUDE DSUPPLR                                           02920000
           END-EXEC.                                                    02930000
           EJECT                                                        02940000
           EXEC SQL                                                     02950000
              INCLUDE DITMSUP                                           02960000
           END-EXEC.                                                    02970000
           EJECT                                                        02980000
           EXEC SQL                                                     02990000
              INCLUDE DPURTYP                                           03000000
           END-EXEC.                                                    03010000
           EJECT                                                        03020000
           EXEC SQL                                                     03030000
              INCLUDE DAFFSUPP                                          03040000
           END-EXEC.                                                    03050000
           EJECT                                                        03051000
           EXEC SQL                                                     03052000
              INCLUDE DORDLOG                                           03053000
           END-EXEC.                                                    03054000
           EJECT                                                        03055000
      ***************************************************************** 03056000
      *    IMS SSA AREAS                                              * 03057000
      ***************************************************************** 03058000
                                                                        03059000
       01  ORDER-SSA.                                                   03060000
           03  FILLER                  PIC X(8)  VALUE 'ORDER'.         03070000
           03  FILLER                  PIC X     VALUE '('.             03080000
           03  FILLER                  PIC X(8)  VALUE 'ORDKEY'.        03090000
           03  FILLER                  PIC XX    VALUE ' ='.            03100000
           03  OS-ORDER-KEY.                                            03110000
               05  OS-ORDER-PREFIX     PIC 9(5)  VALUE ZEROES.          03120000
               05  OS-ORDER-NUMBER     PIC 9(10) VALUE ZEROES.          03130000
           03  FILLER                  PIC X     VALUE ')'.             03140000
                                                                        03150000
       01  ORDER-SSA-UNQUAL.                                            03160000
           03  FILLER                  PIC X(8)  VALUE 'ORDER'.         03170000
           03  FILLER                  PIC X     VALUE SPACES.          03180000
                                                                        03190000
       01  ORDER-ITEM-SSA-UNQUAL.                                       03200000
           03  FILLER                  PIC X(8)  VALUE 'ORDITEM'.       03210000
           03  FILLER                  PIC X     VALUE SPACES.          03220000
           EJECT                                                        03230000
      ***************************************************************** 03240000
      *    IMS RECORD AREAS                                           * 03250000
      ***************************************************************** 03260000
                                                                        03270000
           COPY IORDER.                                                 03280000
                                                                        03290000
                                                                        03300000
           COPY IORDITEM.                                               03310000
           EJECT                                                        03320000
      ***************************************************************** 03330000
      *    GENERAL ERROR PROCESSING WORK AREAS (CICS, IMS-DLI, DB2)   * 03340000
      ***************************************************************** 03350000
                                                                        03360000
           COPY PDAERRWS.                                               03370000
                                                                        03380000
       01  WS-PDA-BATCH-ERROR-01.                                       03390000
           05  FILLER             PIC X     VALUE SPACES.               03400000
           05  FILLER             PIC X(7)  VALUE 'ERROR: '.            03410000
           05  FILLER             PIC X(10) VALUE 'PROGRAM = '.         03420000
           05  WPBE-PROGRAM-ID    PIC X(8)  VALUE 'PDAB02U'.            03430000
           05  FILLER             PIC X(14) VALUE ', PARAGRAPH = '.     03440000
           05  WPBE-PARAGRAPH     PIC X(6)  VALUE SPACES.               03450000
                                                                        03460000
       01  WS-PDA-BATCH-ERROR-02.                                       03470000
           05  FILLER             PIC X(8)  VALUE SPACES.               03480000
           05  WPBE-MESSAGE       PIC X(39) VALUE SPACES.               03490000
           05  FILLER             PIC X(16) VALUE 'RECORD NUMBER ='.    03500000
           05  WPBE-RECORD-NUMBER PIC X(7)  VALUE ZEROES.               03510000
           05  FILLER             PIC X(8)  VALUE SPACES.               03520000
                                                                        03530000
       01  WS-PDA-BATCH-ERROR-03.                                       03540000
           05  FILLER             PIC X(8)  VALUE SPACES.               03550000
           05  FILLER             PIC X(20) VALUE 'RECORD IS DISPLAYED'.03560000
           05  FILLER             PIC X(5)  VALUE 'BELOW'.              03570000
                                                                        03580000
       01  WS-PDA-BATCH-ERROR-04.                                       03590000
           05  FILLER             PIC X(10) VALUE '----+----1'.         03600000
           05  FILLER             PIC X(10) VALUE '----+----2'.         03610000
           05  FILLER             PIC X(10) VALUE '----+----3'.         03620000
           05  FILLER             PIC X(10) VALUE '----+----4'.         03630000
           05  FILLER             PIC X(10) VALUE '----+----5'.         03640000
           05  FILLER             PIC X(10) VALUE '----+----6'.         03650000
           05  FILLER             PIC X(10) VALUE '----+----7'.         03660000
           05  FILLER             PIC X(5)  VALUE '  ...'.              03670000
           EJECT                                                        03680000
      ***************************************************************** 03690000
      *    LINKAGE SECTION                                            * 03700000
      ***************************************************************** 03710000
                                                                        03720000
       LINKAGE SECTION.                                                 03730000
                                                                        03740000
      ****************************************************************  03750000
      *****  I-O PCB                                                    03760000
      ****************************************************************  03770000
                                                                        03780000
       01  IO-PCB.                                                      03790000
           05  FILLER                  PIC X(10) VALUE SPACES.          03800000
           05  IO-STATUS               PIC XX    VALUE SPACES.          03810000
           05  FILLER                  PIC X(20) VALUE SPACES.          03820000
                                                                        03830000
           COPY PCBORDER.                                               03840000
           EJECT                                                        03850000
      ***************************************************************** 03860000
      *    P R O C E D U R E    D I V I S I O N                       * 03870000
      ***************************************************************** 03880000
                                                                        03890000
       PROCEDURE DIVISION.                                              03900000
                                                                        03910000
                                                                        03920000
      ***************************************************************** 03930000
      *                                                               * 03940000
      *    PARAGRAPH:  P00000-MAINLINE                                * 03950000
      *                                                               * 03960000
      *    FUNCTION :  PROGRAM ENTRY, OPEN FILES, PROCESS.            * 03970000
      *                                                               * 03980000
      *    CALLED BY:  NONE                                           * 03990000
      *                                                               * 04000000
      ***************************************************************** 04010000
                                                                        04020000
       P00000-MAINLINE.                                                 04030000
                                                                        04040000
           ENTRY 'DLITCBL' USING                                        04050000
                           IO-PCB                                       04060000
                           ORDER-PCB.                                   04070000
                                                                        04080000
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATE.                  04090000
           MOVE WS-DATE-MONTH TO WS-DL-D-MONTH.                         04100000
           MOVE WS-DATE-DAY TO WS-DL-D-DAY.                             04110000
           MOVE WS-DATE-YEAR TO WS-DL-D-YEAR.                           04120000
                                                                        04130000
      *    DISPLAY ' '.                                                 04140000
      *    DISPLAY WS-DL-ASTERISK.                                      04150000
      *    DISPLAY WS-DL-SPACER.                                        04160000
      *    DISPLAY WS-DL-TITLE.                                         04170000
      *    DISPLAY WS-DL-SPACER.                                        04180000
      *    DISPLAY WS-DL-DATE.                                          04190000
      *    DISPLAY WS-DL-SPACER.                                        04200000
      *    DISPLAY WS-DL-ASTERISK.                                      04210000
                                                                        04220000
           OPEN INPUT INPUT-CUSTOMER                                    04230000
                      INPUT-ITEM                                        04240000
                      INPUT-SUPPLIER                                    04250000
                      INPUT-ITEM-SUPPLIER                               04260000
                      INPUT-PURCHASE-TYPE                               04270000
                      INPUT-ORDER                                       04280000
                      INPUT-AFF-CUSTOMER                                04290000
                      INPUT-ORDER-LOG                                   04300000
                      INPUT-AFF-SUPPLIER                                04310000
                      INPUT-REPORT-ORDER                                04320000
                OUTPUT VSAM-AFF-CUSTOMER                                04321000
                       VSAM-REPORT-ORDER                                04322000
                       VSAM-CUSTOMER.                                   04323000
                                                                        04324000
           MOVE SPACES TO WS-USERID                                     04325000
                          REPORT-ORDER-RECORD                           04326000
                          AFF-CUSTOMER-RECORD                           04327000
                          CUSTOMER-RECORD.                              04328000
                                                                        04329000
      *    DISPLAY ' '.                                                 04330000
      *    DISPLAY ' '.                                                 04340000
      *    DISPLAY WS-DL-ASTERISK.                                      04350000
      *    DISPLAY WS-DL-SPACER.                                        04360000
                                                                        04370000
           PERFORM P10000-LOAD-CUSTOMER THRU P10000-EXIT                04380000
               UNTIL END-OF-CUSTOMER.                                   04390000
                                                                        04400000
           MOVE SPACES TO WS-USERID.                                    04410000
                                                                        04420000
      *    DISPLAY WS-DL-SPACER.                                        04430000
                                                                        04440000
           PERFORM P20000-LOAD-ITEM THRU P20000-EXIT                    04450000
               UNTIL END-OF-ITEM.                                       04460000
                                                                        04470000
           MOVE SPACES TO WS-USERID.                                    04480000
                                                                        04490000
      *    DISPLAY WS-DL-SPACER.                                        04500000
                                                                        04510000
           PERFORM P30000-LOAD-SUPPLIER THRU P30000-EXIT                04520000
               UNTIL END-OF-SUPPLIER.                                   04530000
                                                                        04540000
           MOVE SPACES TO WS-USERID.                                    04550000
                                                                        04560000
      *    DISPLAY WS-DL-SPACER.                                        04570000
                                                                        04580000
           PERFORM P40000-LOAD-ITEM-SUPPLIER THRU P40000-EXIT           04590000
               UNTIL END-OF-ITEM-SUPPLIER.                              04600000
                                                                        04610000
           MOVE SPACES TO WS-USERID.                                    04620000
                                                                        04630000
      *    DISPLAY WS-DL-SPACER.                                        04640000
                                                                        04650000
           PERFORM P50000-LOAD-PURCHASE-TYPE THRU P50000-EXIT           04660000
               UNTIL END-OF-PURCHASE-TYPE.                              04670000
                                                                        04680000
           MOVE SPACES TO WS-USERID                                     04690000
                          ORDER-SEGMENT                                 04700000
                          ORDER-ITEM-SEGMENT.                           04710000
                                                                        04720000
      *    DISPLAY WS-DL-SPACER.                                        04730000
                                                                        04740000
           PERFORM P60000-LOAD-ORDER THRU P60000-EXIT                   04750000
               UNTIL END-OF-ORDER.                                      04760000
                                                                        04770000
      *    DISPLAY WS-DL-SPACER.                                        04780000
                                                                        04790000
           PERFORM P70000-LOAD-AFF-CUSTOMER THRU P70000-EXIT            04800000
               UNTIL END-OF-AFF-CUSTOMER.                               04810000
                                                                        04820000
           MOVE SPACES TO WS-USERID.                                    04821000
                                                                        04822000
      *    DISPLAY WS-DL-SPACER.                                        04823000
                                                                        04824000
           PERFORM P75000-LOAD-ORDER-LOG THRU P75000-EXIT               04825000
               UNTIL END-OF-ORDER-LOG.                                  04826000
                                                                        04827000
           MOVE SPACES TO WS-USERID.                                    04828000
                                                                        04829000
      *    DISPLAY WS-DL-SPACER.                                        04829100
                                                                        04829200
           PERFORM P80000-LOAD-AFF-SUPPLIER THRU P80000-EXIT            04829300
               UNTIL END-OF-AFF-SUPPLIER.                               04829400
                                                                        04829500
           MOVE SPACES TO WS-USERID.                                    04829600
                                                                        04829700
      *    DISPLAY WS-DL-SPACER.                                        04829800
                                                                        04829900
           PERFORM P85000-LOAD-REPORT-ORDER THRU P85000-EXIT            04830000
               UNTIL END-OF-REPORT-ORDER.                               04830100
                                                                        04830200
           MOVE SPACES TO WS-USERID.                                    04830300
                                                                        04830400
      *    DISPLAY WS-DL-SPACER.                                        04830500
      *    DISPLAY WS-DL-ASTERISK.                                      04830600
                                                                        04830700
           CLOSE INPUT-CUSTOMER                                         04830800
                 INPUT-ITEM                                             04830900
                 INPUT-SUPPLIER                                         04831000
                 INPUT-ITEM-SUPPLIER                                    04832000
                 INPUT-PURCHASE-TYPE                                    04833000
                 INPUT-AFF-CUSTOMER                                     04834000
                 INPUT-ORDER-LOG                                        04835000
                 INPUT-AFF-SUPPLIER                                     04836000
                 INPUT-REPORT-ORDER                                     04837000
                 INPUT-ORDER                                            04838000
                 VSAM-AFF-CUSTOMER                                      04839000
                 VSAM-REPORT-ORDER                                      04840000
                 VSAM-CUSTOMER.                                         04850000
                                                                        04860000
      *    DISPLAY ' '.                                                 04870000
      *    DISPLAY ' '.                                                 04880000
      *    DISPLAY WS-DL-ASTERISK.                                      04890000
      *    DISPLAY WS-DL-SPACER.                                        04900000
                                                                        04910000
           MOVE 'TOTALS' TO WS-DL-L-LIT.                                04920000
           MOVE 'VSAM CUSTOMER RECORDS LOADED' TO WS-DL-L-TITLE.        04930000
           MOVE WS-CUSTOMER-TOT TO WS-DL-L-CNT.                         04940000
                                                                        04950000
           DISPLAY WS-DL-LOADED.                                        04960000
           DISPLAY WS-DL-SPACER.                                        04970000
                                                                        04980000
           MOVE SPACES TO WS-DL-L-LIT.                                  04990000
           MOVE 'DB2 ITEM ROWS LOADED' TO WS-DL-L-TITLE.                05000000
           MOVE WS-ITEM-TOT TO WS-DL-L-CNT.                             05010000
                                                                        05020000
           DISPLAY WS-DL-LOADED.                                        05030000
           DISPLAY WS-DL-SPACER.                                        05040000
                                                                        05050000
           MOVE 'DB2 SUPPLIER ROWS LOADED' TO WS-DL-L-TITLE.            05060000
           MOVE WS-SUPPLIER-TOT TO WS-DL-L-CNT.                         05070000
                                                                        05080000
           DISPLAY WS-DL-LOADED.                                        05090000
           DISPLAY WS-DL-SPACER.                                        05100000
                                                                        05110000
           MOVE 'DB2 ITEM-SUPPLIER ROWS LOADED' TO WS-DL-L-TITLE.       05120000
           MOVE WS-ITEM-SUPPLIER-TOT TO WS-DL-L-CNT.                    05130000
                                                                        05140000
           DISPLAY WS-DL-LOADED.                                        05150000
           DISPLAY WS-DL-SPACER.                                        05160000
                                                                        05170000
           MOVE 'DB2 PURCHASE TYPE ROWS LOADED' TO WS-DL-L-TITLE.       05180000
           MOVE WS-PURCHASE-TYPE-TOT TO WS-DL-L-CNT.                    05190000
                                                                        05200000
           DISPLAY WS-DL-LOADED.                                        05210000
           DISPLAY WS-DL-SPACER.                                        05220000
                                                                        05230000
           MOVE 'DB2 AFFILIATE SUPPLIER ROWS LOADED' TO WS-DL-L-TITLE.  05240000
           MOVE WS-AFF-SUPPLIER-TOT TO WS-DL-L-CNT.                     05250000
                                                                        05260000
           DISPLAY WS-DL-LOADED.                                        05270000
           DISPLAY WS-DL-SPACER.                                        05280000
                                                                        05290000
           MOVE 'DB2 ORDER LOG ROWS LOADED' TO WS-DL-L-TITLE.           05291000
           MOVE WS-ORDER-LOG-TOT TO WS-DL-L-CNT.                        05292000
                                                                        05293000
           DISPLAY WS-DL-LOADED.                                        05294000
           DISPLAY WS-DL-SPACER.                                        05295000
                                                                        05296000
           MOVE 'VSAM AFFILIATE CUSTOMER ROWS LOADED' TO WS-DL-L-TITLE. 05297000
           MOVE WS-AFF-CUSTOMER-TOT TO WS-DL-L-CNT.                     05298000
                                                                        05299000
           DISPLAY WS-DL-LOADED.                                        05299100
           DISPLAY WS-DL-SPACER.                                        05299200
                                                                        05299300
           MOVE 'VSAM REPORT ORDER ROWS LOADED' TO WS-DL-L-TITLE.       05299400
           MOVE WS-REPORT-ORDER-TOT TO WS-DL-L-CNT.                     05299500
                                                                        05299600
           DISPLAY WS-DL-LOADED.                                        05299700
           DISPLAY WS-DL-SPACER.                                        05299800
                                                                        05299900
           MOVE 'IMS ORDER RECORDS LOADED' TO WS-DL-L-TITLE.            05300000
           MOVE WS-ORDER-TOT TO WS-DL-L-CNT.                            05310000
                                                                        05320000
           DISPLAY WS-DL-LOADED.                                        05330000
           DISPLAY WS-DL-SPACER.                                        05340000
                                                                        05350000
           MOVE 'IMS ORDER ITEM RECORDS LOADED' TO WS-DL-L-TITLE.       05360000
           MOVE WS-ORDER-ITEM-TOT TO WS-DL-L-CNT.                       05370000
                                                                        05380000
           DISPLAY WS-DL-LOADED.                                        05390000
           DISPLAY WS-DL-SPACER.                                        05400000
           DISPLAY WS-DL-ASTERISK.                                      05410000
           DISPLAY ' '.                                                 05420000
                                                                        05430000
           GOBACK.                                                      05440000
                                                                        05450000
       P00000-EXIT.                                                     05460000
           EXIT.                                                        05470000
           EJECT                                                        05480000
      ***************************************************************** 05490000
      *                                                               * 05500000
      *    PARAGRAPH:  P10000-LOAD-CUSTOMER                           * 05510000
      *                                                               * 05520000
      *    FUNCTION :  ROUTINE TO LOAD THE CUSTOMER VSAM FILE         * 05530000
      *                                                               * 05540000
      *    CALLED BY:  P00000-MAINLINE                                * 05550000
      *                                                               * 05560000
      ***************************************************************** 05570000
                                                                        05580000
       P10000-LOAD-CUSTOMER.                                            05590000
                                                                        05600000
           READ INPUT-CUSTOMER INTO PDA-INPUT-FORMAT                    05610000
               AT END                                                   05620000
                   MOVE 'Y' TO WS-END-OF-CUSTOMER-SW                    05630000
                   IF CUSTOMER-RECORD > SPACES                          05640000
                       WRITE CUSTOMER-RECORD                            05650000
                       IF WS-CUSTOMR-STATUS NOT = ZEROS                 05660000
                         MOVE 'VSAM' TO WS-PDA-ERROR-TYPE               05670000
                         MOVE 'P10000' TO WPBE-PARAGRAPH                05671000
                         MOVE WS-CUSTOMR-STATUS TO WPBE-MESSAGE         05672000
                         MOVE WS-CUSTOMER-IN TO WPBE-RECORD-NUMBER      05673000
                         PERFORM P99999-ABEND THRU P99999-EXIT          05674000
                       END-IF                                           05675000
                       ADD +1 TO WS-CUSTOMER-OUT                        05676000
                       MOVE '99999'  TO CUSTOMER-PREFIX                 05677000
PWB416                 MOVE 7        TO CUSTOMER-TOTAL-ORDER-COUNT      05678000
PWB416                 MOVE 'TRN-91' TO CUSTOMER-TOTAL-DOLLAR-AMT-GRP   05679000
                       WRITE CUSTOMER-RECORD                            05680000
                       IF WS-CUSTOMR-STATUS NOT = ZEROS                 05690000
                         MOVE 'VSAM' TO WS-PDA-ERROR-TYPE               05700000
                         MOVE 'P10000' TO WPBE-PARAGRAPH                05710000
                         MOVE WS-CUSTOMR-STATUS TO WPBE-MESSAGE         05711000
                         MOVE WS-CUSTOMER-IN TO WPBE-RECORD-NUMBER      05712000
                         PERFORM P99999-ABEND THRU P99999-EXIT          05713000
                       END-IF                                           05714000
                       ADD +1 TO WS-CUSTOMER-OUT                        05715000
                   END-IF                                               05716000
                   MOVE WS-USERID TO WS-DL-L-ID                         05717000
                   MOVE 'VSAM CUSTOMER RECORDS LOADED' TO WS-DL-L-TITLE 05718000
                   MOVE WS-CUSTOMER-OUT TO WS-DL-L-CNT                  05719000
                   ADD WS-CUSTOMER-OUT TO WS-CUSTOMER-TOT               05720000
      *            DISPLAY WS-DL-LOADED                                 05730000
                   GO TO P10000-EXIT.                                   05740000
                                                                        05750000
           ADD +1 TO WS-CUSTOMER-IN.                                    05760000
                                                                        05770000
           IF PDA-SPACER-REC                                            05780000
               IF CUSTOMER-RECORD > SPACES                              05790000
                   ADD +1 TO WS-CUSTOMER-OUT                            05800000
                   WRITE CUSTOMER-RECORD                                05810000
                   MOVE '99999'  TO CUSTOMER-PREFIX                     05820000
PWB416             MOVE 7        TO CUSTOMER-TOTAL-ORDER-COUNT          05830000
PWB416             MOVE 'TRN-91' TO CUSTOMER-TOTAL-DOLLAR-AMT-GRP       05840000
                   ADD +1 TO WS-CUSTOMER-OUT                            05850000
                   WRITE CUSTOMER-RECORD                                05860000
                   MOVE SPACES TO CUSTOMER-RECORD                       05870000
               END-IF                                                   05880000
               GO TO P10000-EXIT                                        05890000
           END-IF.                                                      05900000
                                                                        05910000
           IF NOT PDA-DATA-REC                                          05920000
               MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                         05930000
               MOVE 'P10000' TO WPBE-PARAGRAPH                          05940000
               MOVE 'UNIDENTIFIED INPUT RECORD,' TO WPBE-MESSAGE        05950000
               MOVE WS-CUSTOMER-IN TO WPBE-RECORD-NUMBER                05960000
               PERFORM P99999-ABEND THRU P99999-EXIT                    05970000
           END-IF.                                                      05980000
                                                                        05990000
           MOVE ZEROES                 TO CUSTOMER-LAST-ORDER-AMT.      06000000
                                                                        06001000
           EVALUATE TRUE                                                06002000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.PREFIX'                  06003000
                   MOVE PDA-DATA-005-N TO CUSTOMER-PREFIX               06004000
                   IF PDA-DATA-005 NOT = WS-USERID                      06005000
                       IF WS-USERID = SPACES                            06006000
                           MOVE PDA-DATA-005 TO WS-USERID               06007000
                       ELSE                                             06008000
                           MOVE WS-USERID TO WS-DL-L-ID                 06009000
                           MOVE 'VSAM CUSTOMER RECORDS LOADED' TO       06010000
                               WS-DL-L-TITLE                            06020000
                           MOVE WS-CUSTOMER-OUT TO WS-DL-L-CNT          06030000
                           ADD WS-CUSTOMER-OUT TO WS-CUSTOMER-TOT       06040000
                           MOVE +0 TO WS-CUSTOMER-OUT                   06050000
      *                    DISPLAY WS-DL-LOADED                         06060000
                           MOVE PDA-DATA-005 TO WS-USERID               06070000
                       END-IF                                           06080000
                   END-IF                                               06090000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.ID'                      06100000
                   MOVE PDA-DATA-032 TO CUSTOMER-ID                     06110000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.PASSWORD'                06120000
                   MOVE PDA-DATA-032 TO CUSTOMER-PASSWORD               06130000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.NAME'                    06140000
                   MOVE PDA-DATA-064 TO CUSTOMER-NAME                   06150000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.ADDRESS'                 06160000
                   MOVE PDA-DATA-128 TO CUSTOMER-ADDRESS                06170000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.CITY'                    06180000
                   MOVE PDA-DATA-032 TO CUSTOMER-CITY                   06190000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.STATE'                   06200000
                   MOVE PDA-DATA-032 TO CUSTOMER-STATE                  06210000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.POSTAL-CODE'             06220000
                   MOVE PDA-DATA-012 TO CUSTOMER-POSTAL-CODE            06230000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.SHIP-TO-NAME'            06240000
                   MOVE PDA-DATA-064 TO CUSTOMER-SHIP-TO-NAME           06250000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.SHIP-TO-ADDRESS'         06260000
                   MOVE PDA-DATA-128 TO CUSTOMER-SHIP-TO-ADDRESS        06270000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.SHIP-TO-CITY'            06280000
                   MOVE PDA-DATA-032 TO CUSTOMER-SHIP-TO-CITY           06290000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.SHIP-TO-STATE'           06300000
                   MOVE PDA-DATA-032 TO CUSTOMER-SHIP-TO-STATE          06310000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.SHIP-TO-POSTAL-CODE'     06320000
                   MOVE PDA-DATA-012 TO CUSTOMER-SHIP-TO-POSTAL-CODE    06330000
               WHEN PDA-FIELD-NAME = 'CUSTOMER.EMAIL-ADDRESS'           06340000
                   MOVE PDA-DATA-128 TO CUSTOMER-EMAIL-ADDRESS          06350000
               WHEN OTHER                                               06360000
                   MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                     06370000
                   MOVE 'P10000' TO WPBE-PARAGRAPH                      06380000
                   MOVE 'UNIDENTIFIED INPUT RECORD FIELD NAME,' TO      06390000
                        WPBE-MESSAGE                                    06400000
                   MOVE WS-CUSTOMER-IN TO WPBE-RECORD-NUMBER            06410000
                   PERFORM P99999-ABEND THRU P99999-EXIT                06420000
           END-EVALUATE.                                                06430000
                                                                        06440000
       P10000-EXIT.                                                     06450000
           EXIT.                                                        06460000
           EJECT                                                        06470000
      ***************************************************************** 06480000
      *                                                               * 06490000
      *    PARAGRAPH:  P20000-LOAD-ITEM                               * 06500000
      *                                                               * 06510000
      *    FUNCTION :  ROUTINE TO LOAD THE ITEM TABLE                 * 06520000
      *                                                               * 06530000
      *    CALLED BY:  P00000-MAINLINE                                * 06540000
      *                                                               * 06550000
      ***************************************************************** 06560000
                                                                        06570000
       P20000-LOAD-ITEM.                                                06580000
                                                                        06590000
           READ INPUT-ITEM INTO PDA-INPUT-FORMAT                        06600000
               AT END                                                   06610000
                   MOVE 'Y' TO WS-END-OF-ITEM-SW                        06620000
                   IF ITEM > SPACES                                     06630000
                       ADD +1 TO WS-ITEM-OUT                            06640000
                       PERFORM P21000-INSERT-ITEM THRU P21000-EXIT      06650000
                   END-IF                                               06660000
                   MOVE WS-USERID TO WS-DL-L-ID                         06670000
                   MOVE 'DB2 ITEM ROWS LOADED' TO WS-DL-L-TITLE         06680000
                   MOVE WS-ITEM-OUT TO WS-DL-L-CNT                      06690000
                   ADD WS-ITEM-OUT TO WS-ITEM-TOT                       06700000
      *            DISPLAY WS-DL-LOADED                                 06710000
                   GO TO P20000-EXIT.                                   06720000
                                                                        06730000
           ADD +1 TO WS-ITEM-IN.                                        06740000
                                                                        06750000
           IF PDA-SPACER-REC                                            06760000
               IF ITEM > SPACES                                         06770000
                   ADD +1 TO WS-ITEM-OUT                                06780000
                   PERFORM P21000-INSERT-ITEM THRU P21000-EXIT          06790000
                   MOVE SPACES TO ITEM                                  06800000
               END-IF                                                   06810000
               GO TO P20000-EXIT                                        06820000
           END-IF.                                                      06830000
                                                                        06840000
           IF NOT PDA-DATA-REC                                          06850000
               MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                         06860000
               MOVE 'P20000' TO WPBE-PARAGRAPH                          06870000
               MOVE 'UNIDENTIFIED INPUT RECORD,' TO WPBE-MESSAGE        06880000
               MOVE WS-ITEM-IN TO WPBE-RECORD-NUMBER                    06890000
               PERFORM P99999-ABEND THRU P99999-EXIT                    06900000
           END-IF.                                                      06910000
                                                                        06920000
           EVALUATE TRUE                                                06930000
               WHEN PDA-FIELD-NAME = 'ITEM.PREFIX'                      06940000
                   MOVE PDA-DATA-005 TO ITEM-PREFIX                     06950000
                   IF PDA-DATA-005 NOT = WS-USERID                      06960000
                       IF WS-USERID = SPACES                            06970000
                           MOVE PDA-DATA-005 TO WS-USERID               06980000
                       ELSE                                             06990000
                           MOVE WS-USERID TO WS-DL-L-ID                 07000000
                           MOVE 'DB2 ITEM ROWS LOADED' TO WS-DL-L-TITLE 07010000
                           MOVE WS-ITEM-OUT TO WS-DL-L-CNT              07020000
                           ADD WS-ITEM-OUT TO WS-ITEM-TOT               07030000
                           MOVE +0 TO WS-ITEM-OUT                       07040000
      *                    DISPLAY WS-DL-LOADED                         07050000
                           MOVE PDA-DATA-005 TO WS-USERID               07060000
                       END-IF                                           07070000
                   END-IF                                               07080000
               WHEN PDA-FIELD-NAME = 'ITEM.NUMBER'                      07090000
                   MOVE PDA-DATA-032 TO ITEM-NUMBER                     07100000
               WHEN PDA-FIELD-NAME = 'ITEM.CATEGORY-NAME'               07110000
                   MOVE PDA-DATA-032 TO ITEM-CATEGORY-NAME              07120000
               WHEN PDA-FIELD-NAME = 'ITEM.SUB-CATEGORY-NAME'           07130000
                   MOVE PDA-DATA-032 TO ITEM-SUB-CATEGORY-NAME          07140000
               WHEN PDA-FIELD-NAME = 'ITEM.NAME'                        07150000
                   MOVE PDA-DATA-050 TO ITEM-NAME                       07160000
               WHEN PDA-FIELD-NAME = 'ITEM.LENGTH'                      07170000
                   MOVE PDA-DATA-010 TO WS-CHARACTER                    07180000
                   PERFORM P90000-CONVERT-NUMERIC THRU P90000-EXIT      07190000
                   MOVE WS-NUMERIC TO ITEM-LENGTH                       07200000
               WHEN PDA-FIELD-NAME = 'ITEM.DIAMETER'                    07210000
                   MOVE PDA-DATA-010 TO WS-CHARACTER                    07220000
                   PERFORM P90000-CONVERT-NUMERIC THRU P90000-EXIT      07230000
                   MOVE WS-NUMERIC TO ITEM-DIAMETER                     07240000
               WHEN OTHER                                               07250000
                   MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                     07260000
                   MOVE 'P20000' TO WPBE-PARAGRAPH                      07270000
                   MOVE 'UNIDENTIFIED INPUT RECORD FIELD NAME,' TO      07280000
                        WPBE-MESSAGE                                    07290000
                   MOVE WS-ITEM-IN TO WPBE-RECORD-NUMBER                07300000
                   PERFORM P99999-ABEND THRU P99999-EXIT                07310000
           END-EVALUATE.                                                07320000
                                                                        07330000
       P20000-EXIT.                                                     07340000
           EXIT.                                                        07350000
           EJECT                                                        07360000
      ***************************************************************** 07370000
      *                                                               * 07380000
      *    PARAGRAPH:  P21000-INSERT-ITEM                             * 07390000
      *                                                               * 07400000
      *    FUNCTION :  ROUTINE TO INSERT TO THE ITEM TABLE            * 07410000
      *                                                               * 07420000
      *    CALLED BY:  P20000-LOAD-ITEM                               * 07430000
      *                                                               * 07440000
      ***************************************************************** 07450000
                                                                        07460000
       P21000-INSERT-ITEM.                                              07470000
                                                                        07480000
           EXEC SQL                                                     07490000
               INSERT                                                   07500000
               INTO   ITEM                                              07510000
                     (PREFIX,                                           07520000
                      NUMBER,                                           07530000
                      CATEGORY_NAME,                                    07540000
                      SUB_CATEGORY_NAME,                                07550000
                      NAME,                                             07560000
                      LENGTH,                                           07570000
                      DIAMETER)                                         07580000
               VALUES                                                   07590000
                     (:ITEM-PREFIX,                                     07600000
                      :ITEM-NUMBER,                                     07610000
                      :ITEM-CATEGORY-NAME,                              07620000
                      :ITEM-SUB-CATEGORY-NAME,                          07630000
                      :ITEM-NAME,                                       07640000
                      :ITEM-LENGTH,                                     07650000
                      :ITEM-DIAMETER)                                   07660000
           END-EXEC.                                                    07670000
                                                                        07680000
           IF SQLCODE NOT = +0                                          07690000
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE                          07700000
               MOVE 'PDAB02' TO WPDE-PROGRAM-ID                         07710000
               MOVE SQLCODE TO WPDE-DB2-SQLCODE                         07720000
               MOVE 'INSERT TO ITEM TABLE' TO WPDE-FUNCTION             07730000
               MOVE 'P21000' TO WPDE-PARAGRAPH                          07740000
               MOVE ITEM TO PDA-INPUT-FORMAT                            07750000
               PERFORM P99999-ABEND THRU P99999-EXIT                    07760000
           END-IF.                                                      07770000
                                                                        07780000
       P21000-EXIT.                                                     07790000
           EXIT.                                                        07800000
           EJECT                                                        07810000
      ***************************************************************** 07820000
      *                                                               * 07830000
      *    PARAGRAPH:  P30000-LOAD-SUPPLIER                           * 07840000
      *                                                               * 07850000
      *    FUNCTION :  ROUTINE TO LOAD THE SUPPLIER TABLE             * 07860000
      *                                                               * 07870000
      *    CALLED BY:  P00000-MAINLINE                                * 07880000
      *                                                               * 07890000
      ***************************************************************** 07900000
                                                                        07910000
       P30000-LOAD-SUPPLIER.                                            07920000
                                                                        07930000
           READ INPUT-SUPPLIER INTO PDA-INPUT-FORMAT                    07940000
               AT END                                                   07950000
                   MOVE 'Y' TO WS-END-OF-SUPPLIER-SW                    07960000
                   IF SUPPLIER > SPACES                                 07970000
                       ADD +1 TO WS-SUPPLIER-OUT                        07980000
                       PERFORM P31000-INSERT-SUPPLIER THRU P31000-EXIT  07990000
                   END-IF                                               08000000
                   MOVE WS-USERID TO WS-DL-L-ID                         08010000
                   MOVE 'DB2 SUPPLIER ROWS LOADED' TO WS-DL-L-TITLE     08020000
                   MOVE WS-SUPPLIER-OUT TO WS-DL-L-CNT                  08030000
                   ADD WS-SUPPLIER-OUT TO WS-SUPPLIER-TOT               08040000
      *            DISPLAY WS-DL-LOADED                                 08050000
                   GO TO P30000-EXIT.                                   08060000
                                                                        08070000
           ADD +1 TO WS-SUPPLIER-IN.                                    08080000
                                                                        08090000
           IF PDA-SPACER-REC                                            08100000
               IF SUPPLIER > SPACES                                     08110000
                   ADD +1 TO WS-SUPPLIER-OUT                            08120000
                   PERFORM P31000-INSERT-SUPPLIER THRU P31000-EXIT      08130000
                   MOVE SPACES TO SUPPLIER                              08140000
               END-IF                                                   08150000
               GO TO P30000-EXIT                                        08160000
           END-IF.                                                      08170000
                                                                        08180000
           IF NOT PDA-DATA-REC                                          08190000
               MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                         08200000
               MOVE 'P30000' TO WPBE-PARAGRAPH                          08210000
               MOVE 'UNIDENTIFIED INPUT RECORD,' TO WPBE-MESSAGE        08220000
               MOVE WS-SUPPLIER-IN TO WPBE-RECORD-NUMBER                08230000
               PERFORM P99999-ABEND THRU P99999-EXIT                    08240000
           END-IF.                                                      08250000
                                                                        08260000
           EVALUATE TRUE                                                08270000
               WHEN PDA-FIELD-NAME = 'SUPPLIER.PREFIX'                  08280000
                   MOVE PDA-DATA-005 TO SUPPLIER-PREFIX                 08290000
                                        WS-EA-PREFIX                    08300000
                   IF PDA-DATA-005 NOT = WS-USERID                      08310000
                       IF WS-USERID = SPACES                            08320000
                           MOVE PDA-DATA-005 TO WS-USERID               08330000
                       ELSE                                             08340000
                           MOVE WS-USERID TO WS-DL-L-ID                 08350000
                           MOVE 'DB2 SUPPLIER ROWS LOADED' TO           08360000
                               WS-DL-L-TITLE                            08370000
                           MOVE WS-SUPPLIER-OUT TO WS-DL-L-CNT          08380000
                           ADD WS-SUPPLIER-OUT TO WS-SUPPLIER-TOT       08390000
                           MOVE +0 TO WS-SUPPLIER-OUT                   08400000
      *                    DISPLAY WS-DL-LOADED                         08410000
                           MOVE PDA-DATA-005 TO WS-USERID               08420000
                       END-IF                                           08430000
                   END-IF                                               08440000
               WHEN PDA-FIELD-NAME = 'SUPPLIER.SUPPLIER-ID'             08450000
                   MOVE PDA-DATA-032 TO SUPPLIER-SUPPLIER-ID            08460000
               WHEN PDA-FIELD-NAME = 'SUPPLIER.PASSWORD'                08470000
                   MOVE PDA-DATA-032 TO SUPPLIER-PASSWORD               08480000
               WHEN PDA-FIELD-NAME = 'SUPPLIER.NAME'                    08490000
                   MOVE PDA-DATA-064 TO SUPPLIER-NAME                   08500000
               WHEN PDA-FIELD-NAME = 'SUPPLIER.ADDRESS'                 08510000
                   MOVE PDA-DATA-128 TO SUPPLIER-ADDRESS                08520000
               WHEN PDA-FIELD-NAME = 'SUPPLIER.CITY'                    08530000
                   MOVE PDA-DATA-032 TO SUPPLIER-CITY                   08540000
               WHEN PDA-FIELD-NAME = 'SUPPLIER.STATE'                   08550000
                   MOVE PDA-DATA-032 TO SUPPLIER-STATE                  08560000
               WHEN PDA-FIELD-NAME = 'SUPPLIER.POSTAL-CODE'             08570000
                   MOVE PDA-DATA-012 TO SUPPLIER-POSTAL-CODE            08580000
               WHEN PDA-FIELD-NAME = 'SUPPLIER.EMAIL-ADDRESS'           08590000
                   MOVE PDA-DATA-128 TO WS-EA-EMAIL-ADDRESS             08600000
                   MOVE WS-EMAIL-ADDRESS TO SUPPLIER-EMAIL-ADDRESS      08610000
               WHEN OTHER                                               08620000
                   MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                     08630000
                   MOVE 'P30000' TO WPBE-PARAGRAPH                      08640000
                   MOVE 'UNIDENTIFIED INPUT RECORD FIELD NAME,' TO      08650000
                        WPBE-MESSAGE                                    08660000
                   MOVE WS-SUPPLIER-IN TO WPBE-RECORD-NUMBER            08670000
                   PERFORM P99999-ABEND THRU P99999-EXIT                08680000
           END-EVALUATE.                                                08690000
                                                                        08700000
       P30000-EXIT.                                                     08710000
           EXIT.                                                        08720000
           EJECT                                                        08730000
      ***************************************************************** 08740000
      *                                                               * 08750000
      *    PARAGRAPH:  P31000-INSERT-SUPPLIER                         * 08760000
      *                                                               * 08770000
      *    FUNCTION :  ROUTINE TO INSERT TO THE SUPPLIER TABLE        * 08780000
      *                                                               * 08790000
      *    CALLED BY:  P30000-LOAD-SUPPLIER                           * 08800000
      *                                                               * 08810000
      ***************************************************************** 08820000
                                                                        08830000
       P31000-INSERT-SUPPLIER.                                          08840000
                                                                        08850000
           EXEC SQL                                                     08860000
               INSERT                                                   08870000
               INTO   SUPPLIER                                          08880000
                     (PREFIX,                                           08890000
                      SUPPLIER_ID,                                      08900000
                      PASSWORD,                                         08910000
                      NAME,                                             08920000
                      ADDRESS,                                          08930000
                      CITY,                                             08940000
                      STATE,                                            08950000
                      POSTAL_CODE,                                      08960000
                      EMAIL_ADDRESS)                                    08970000
               VALUES                                                   08980000
                     (:SUPPLIER-PREFIX,                                 08990000
                      :SUPPLIER-SUPPLIER-ID,                            09000000
                      :SUPPLIER-PASSWORD,                               09010000
                      :SUPPLIER-NAME,                                   09020000
                      :SUPPLIER-ADDRESS,                                09030000
                      :SUPPLIER-CITY,                                   09040000
                      :SUPPLIER-STATE,                                  09050000
                      :SUPPLIER-POSTAL-CODE,                            09060000
                      :SUPPLIER-EMAIL-ADDRESS)                          09070000
           END-EXEC.                                                    09080000
                                                                        09090000
           IF SQLCODE NOT = +0                                          09100000
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE                          09110000
               MOVE 'PDAB02U' TO WPDE-PROGRAM-ID                        09120000
               MOVE SQLCODE TO WPDE-DB2-SQLCODE                         09130000
               MOVE 'INSERT TO SUPPLIER TABLE' TO WPDE-FUNCTION         09140000
               MOVE 'P31000' TO WPDE-PARAGRAPH                          09150000
               MOVE SUPPLIER TO PDA-INPUT-FORMAT                        09160000
               PERFORM P99999-ABEND THRU P99999-EXIT                    09170000
           END-IF.                                                      09180000
                                                                        09190000
       P31000-EXIT.                                                     09200000
           EXIT.                                                        09210000
           EJECT                                                        09220000
      ***************************************************************** 09230000
      *                                                               * 09240000
      *    PARAGRAPH:  P40000-LOAD-ITEM-SUPPLIER                      * 09250000
      *                                                               * 09260000
      *    FUNCTION :  ROUTINE TO LOAD THE ITEM-SUPPLIER TABLE        * 09270000
      *                                                               * 09280000
      *    CALLED BY:  P00000-MAINLINE                                * 09290000
      *                                                               * 09300000
      ***************************************************************** 09310000
                                                                        09320000
       P40000-LOAD-ITEM-SUPPLIER.                                       09330000
                                                                        09340000
           READ INPUT-ITEM-SUPPLIER INTO PDA-INPUT-FORMAT               09350000
               AT END                                                   09360000
                   MOVE 'Y' TO WS-END-OF-ITEM-SUPLR-SW                  09370000
                   IF ITEM-SUPPLIER > SPACES                            09380000
                       ADD +1 TO WS-ITEM-SUPPLIER-OUT                   09390000
                       PERFORM P41000-INSERT-ITEM-SUPPLIER              09400000
                           THRU P41000-EXIT                             09410000
                   END-IF                                               09420000
                   MOVE WS-USERID TO WS-DL-L-ID                         09430000
                   MOVE 'DB2 ITEM SUPPLIER ROWS LOADED' TO WS-DL-L-TITLE09440000
                   MOVE WS-ITEM-SUPPLIER-OUT TO WS-DL-L-CNT             09450000
                   ADD WS-ITEM-SUPPLIER-OUT TO WS-ITEM-SUPPLIER-TOT     09460000
      *            DISPLAY WS-DL-LOADED                                 09470000
                   GO TO P40000-EXIT.                                   09480000
                                                                        09490000
           ADD +1 TO WS-ITEM-SUPPLIER-IN.                               09500000
                                                                        09510000
           IF PDA-SPACER-REC                                            09520000
               IF ITEM-SUPPLIER > SPACES                                09530000
                   ADD +1 TO WS-ITEM-SUPPLIER-OUT                       09540000
                   PERFORM P41000-INSERT-ITEM-SUPPLIER THRU P41000-EXIT 09550000
                   MOVE SPACES TO ITEM-SUPPLIER                         09560000
               END-IF                                                   09570000
               GO TO P40000-EXIT                                        09580000
           END-IF.                                                      09590000
                                                                        09600000
           IF NOT PDA-DATA-REC                                          09610000
               MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                         09620000
               MOVE 'P40000' TO WPBE-PARAGRAPH                          09630000
               MOVE 'UNIDENTIFIED INPUT RECORD,' TO WPBE-MESSAGE        09640000
               MOVE WS-ITEM-SUPPLIER-IN TO WPBE-RECORD-NUMBER           09650000
               PERFORM P99999-ABEND THRU P99999-EXIT                    09660000
           END-IF.                                                      09670000
                                                                        09680000
           EVALUATE TRUE                                                09690000
               WHEN PDA-FIELD-NAME = 'ITEM-SUPPLIER.ITEM-PREFIX'        09700000
                   MOVE PDA-DATA-005 TO ITEM-SUPPLIER-ITEM-PREFIX       09710000
                   IF PDA-DATA-005 NOT = WS-USERID                      09720000
                       IF WS-USERID = SPACES                            09730000
                           MOVE PDA-DATA-005 TO WS-USERID               09740000
                       ELSE                                             09750000
                           MOVE WS-USERID TO WS-DL-L-ID                 09760000
                           MOVE 'DB2 ITEM SUPPLIER ROWS LOADED' TO      09770000
                               WS-DL-L-TITLE                            09780000
                           MOVE WS-ITEM-SUPPLIER-OUT TO WS-DL-L-CNT     09790000
                           ADD WS-ITEM-SUPPLIER-OUT TO                  09800000
                               WS-ITEM-SUPPLIER-TOT                     09810000
                           MOVE +0 TO WS-ITEM-SUPPLIER-OUT              09820000
      *                    DISPLAY WS-DL-LOADED                         09830000
                           MOVE PDA-DATA-005 TO WS-USERID               09840000
                       END-IF                                           09850000
                   END-IF                                               09860000
               WHEN PDA-FIELD-NAME = 'ITEM-SUPPLIER.ITEM-NUMBER'        09870000
                   MOVE PDA-DATA-032 TO ITEM-SUPPLIER-ITEM-NUMBER       09880000
               WHEN PDA-FIELD-NAME = 'ITEM-SUPPLIER.SUPPLIER-PREFIX'    09890000
                   MOVE PDA-DATA-005 TO ITEM-SUPPLIER-SUPPLIER-PREFIX   09900000
               WHEN PDA-FIELD-NAME = 'ITEM-SUPPLIER.SUPPLIER-ID'        09910000
                   MOVE PDA-DATA-032 TO ITEM-SUPPLIER-SUPPLIER-ID       09920000
               WHEN PDA-FIELD-NAME = 'ITEM-SUPPLIER.QUANTITY-ON-HAND'   09930000
                   MOVE PDA-DATA-009 TO WS-CHARACTER                    09940000
                   PERFORM P90000-CONVERT-NUMERIC THRU P90000-EXIT      09950000
                   MOVE WS-NUMERIC TO ITEM-SUPPLIER-QUANTITY-ON-HAND    09960000
               WHEN PDA-FIELD-NAME = 'ITEM-SUPPLIER.UNIT-PRICE'         09970000
                   MOVE PDA-DATA-010 TO WS-CHARACTER                    09980000
                   PERFORM P90000-CONVERT-NUMERIC THRU P90000-EXIT      09990000
                   MOVE WS-NUMERIC TO ITEM-SUPPLIER-UNIT-PRICE          10000000
               WHEN OTHER                                               10010000
                   MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                     10020000
                   MOVE 'P40000' TO WPBE-PARAGRAPH                      10030000
                   MOVE 'UNIDENTIFIED INPUT RECORD FIELD NAME,' TO      10040000
                        WPBE-MESSAGE                                    10050000
                   MOVE WS-ITEM-SUPPLIER-IN TO WPBE-RECORD-NUMBER       10060000
                   PERFORM P99999-ABEND THRU P99999-EXIT                10070000
           END-EVALUATE.                                                10080000
                                                                        10090000
       P40000-EXIT.                                                     10100000
           EXIT.                                                        10110000
           EJECT                                                        10120000
      ***************************************************************** 10130000
      *                                                               * 10140000
      *    PARAGRAPH:  P41000-INSERT-ITEM-SUPPLIER                    * 10150000
      *                                                               * 10160000
      *    FUNCTION :  ROUTINE TO INSERT TO THE ITEM-SUPPLIER TABLE   * 10170000
      *                                                               * 10180000
      *    CALLED BY:  P40000-LOAD-ITEM-SUPPLIER                      * 10190000
      *                                                               * 10200000
      ***************************************************************** 10210000
                                                                        10220000
       P41000-INSERT-ITEM-SUPPLIER.                                     10230000
                                                                        10240000
           EXEC SQL                                                     10250000
               INSERT                                                   10260000
               INTO   ITEM_SUPPLIER                                     10270000
                     (ITEM_PREFIX,                                      10280000
                      ITEM_NUMBER,                                      10290000
                      SUPPLIER_PREFIX,                                  10300000
                      SUPPLIER_ID,                                      10310000
                      QUANTITY_ON_HAND,                                 10320000
                      UNIT_PRICE)                                       10330000
               VALUES                                                   10340000
                     (:ITEM-SUPPLIER-ITEM-PREFIX,                       10350000
                      :ITEM-SUPPLIER-ITEM-NUMBER,                       10360000
                      :ITEM-SUPPLIER-SUPPLIER-PREFIX,                   10370000
                      :ITEM-SUPPLIER-SUPPLIER-ID,                       10380000
                      :ITEM-SUPPLIER-QUANTITY-ON-HAND,                  10390000
                      :ITEM-SUPPLIER-UNIT-PRICE)                        10400000
           END-EXEC.                                                    10410000
                                                                        10420000
           EVALUATE TRUE                                                10430000
               WHEN SQLCODE = +0                                        10440000
                   EXIT                                                 10450000
               WHEN SQLCODE = -803                                      10460000
                   DISPLAY ' '                                          10470000
                   DISPLAY 'P41000-INSERT-ITEM-SUPPLIER'                10480000
                   DISPLAY ' '                                          10490000
                   DISPLAY '    DUPLICATE ITEM SUPPLIER IGNORED, '      10500000
                   DISPLAY '    ITEM NUMBER = '                         10510000
                           ITEM-SUPPLIER-ITEM-NUMBER                    10520000
                   DISPLAY '    SUPPLIER ID = '                         10530000
                           ITEM-SUPPLIER-SUPPLIER-ID                    10540000
                   DISPLAY ' '                                          10550000
               WHEN OTHER                                               10560000
                   MOVE 'DB2' TO WS-PDA-ERROR-TYPE                      10570000
                   MOVE 'PDAB02U' TO WPDE-PROGRAM-ID                    10580000
                   MOVE SQLCODE TO WPDE-DB2-SQLCODE                     10590000
                   MOVE 'INSERT TO ITEM SUPPLIER TABLE' TO WPDE-FUNCTION10600000
                   MOVE 'P41000' TO WPDE-PARAGRAPH                      10610000
                   MOVE ITEM-SUPPLIER TO PDA-INPUT-FORMAT               10620000
                   PERFORM P99999-ABEND THRU P99999-EXIT                10630000
           END-EVALUATE.                                                10640000
                                                                        10650000
       P41000-EXIT.                                                     10660000
           EXIT.                                                        10670000
           EJECT                                                        10680000
      ***************************************************************** 10690000
      *                                                               * 10700000
      *    PARAGRAPH:  P50000-LOAD-PURCHASE-TYPE                      * 10710000
      *                                                               * 10720000
      *    FUNCTION :  ROUTINE TO LOAD THE PURCHASE-TYPE TABLE        * 10730000
      *                                                               * 10740000
      *    CALLED BY:  P00000-MAINLINE                                * 10750000
      *                                                               * 10760000
      ***************************************************************** 10770000
                                                                        10780000
       P50000-LOAD-PURCHASE-TYPE.                                       10790000
                                                                        10800000
           READ INPUT-PURCHASE-TYPE INTO PDA-INPUT-FORMAT               10810000
               AT END                                                   10820000
                   MOVE 'Y' TO WS-END-OF-PURCHASE-SW                    10830000
                   IF PURCHASE-TYPE > SPACES                            10840000
                       ADD +1 TO WS-PURCHASE-TYPE-OUT                   10850000
                       PERFORM P51000-INSERT-PURCHASE-TYPE              10860000
                           THRU P51000-EXIT                             10870000
                   END-IF                                               10880000
                   MOVE WS-USERID TO WS-DL-L-ID                         10890000
                   MOVE 'DB2 PURCHASE TYPE ROWS LOADED' TO WS-DL-L-TITLE10900000
                   MOVE WS-PURCHASE-TYPE-OUT TO WS-DL-L-CNT             10910000
                   ADD WS-PURCHASE-TYPE-OUT TO WS-PURCHASE-TYPE-TOT     10920000
      *            DISPLAY WS-DL-LOADED                                 10930000
                   GO TO P50000-EXIT.                                   10940000
                                                                        10950000
           ADD +1 TO WS-PURCHASE-TYPE-IN.                               10960000
                                                                        10970000
           IF PDA-SPACER-REC                                            10980000
               IF PURCHASE-TYPE > SPACES                                10990000
                   ADD +1 TO WS-PURCHASE-TYPE-OUT                       11000000
                   PERFORM P51000-INSERT-PURCHASE-TYPE THRU P51000-EXIT 11010000
                   MOVE SPACES TO PURCHASE-TYPE                         11020000
               END-IF                                                   11030000
               GO TO P50000-EXIT                                        11040000
           END-IF.                                                      11050000
                                                                        11060000
           IF NOT PDA-DATA-REC                                          11070000
               MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                         11080000
               MOVE 'P50000' TO WPBE-PARAGRAPH                          11090000
               MOVE 'UNIDENTIFIED INPUT RECORD,' TO WPBE-MESSAGE        11100000
               MOVE WS-PURCHASE-TYPE-IN TO WPBE-RECORD-NUMBER           11110000
               PERFORM P99999-ABEND THRU P99999-EXIT                    11120000
           END-IF.                                                      11130000
                                                                        11140000
           EVALUATE TRUE                                                11150000
               WHEN PDA-FIELD-NAME = 'PURCHASE-TYPE.PREFIX'             11160000
                   MOVE PDA-DATA-005 TO PURCHASE-TYPE-PREFIX            11170000
                   IF PDA-DATA-005 NOT = WS-USERID                      11180000
                       IF WS-USERID = SPACES                            11190000
                           MOVE PDA-DATA-005 TO WS-USERID               11200000
                       ELSE                                             11210000
                           MOVE WS-USERID TO WS-DL-L-ID                 11220000
                           MOVE 'DB2 PURCHASE TYPE ROWS LOADED' TO      11230000
                               WS-DL-L-TITLE                            11240000
                           MOVE WS-PURCHASE-TYPE-OUT TO WS-DL-L-CNT     11250000
                           ADD WS-PURCHASE-TYPE-OUT TO                  11260000
                               WS-PURCHASE-TYPE-TOT                     11270000
                           MOVE +0 TO WS-PURCHASE-TYPE-OUT              11280000
      *                    DISPLAY WS-DL-LOADED                         11290000
                           MOVE PDA-DATA-005 TO WS-USERID               11300000
                       END-IF                                           11310000
                   END-IF                                               11320000
               WHEN PDA-FIELD-NAME = 'PURCHASE-TYPE.TYPE'               11330000
                   MOVE PDA-DATA-003 TO PURCHASE-TYPE-TYPE              11340000
               WHEN PDA-FIELD-NAME = 'PURCHASE-TYPE.DESCRIPTION'        11350000
                   MOVE PDA-DATA-032 TO PURCHASE-TYPE-DESCRIPTION       11360000
               WHEN OTHER                                               11370000
                   MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                     11380000
                   MOVE 'P50000' TO WPBE-PARAGRAPH                      11390000
                   MOVE 'UNIDENTIFIED INPUT RECORD FIELD NAME,' TO      11400000
                        WPBE-MESSAGE                                    11410000
                   MOVE WS-PURCHASE-TYPE-IN TO WPBE-RECORD-NUMBER       11420000
                   PERFORM P99999-ABEND THRU P99999-EXIT                11430000
           END-EVALUATE.                                                11440000
                                                                        11450000
       P50000-EXIT.                                                     11460000
           EXIT.                                                        11470000
           EJECT                                                        11480000
      ***************************************************************** 11490000
      *                                                               * 11500000
      *    PARAGRAPH:  P51000-INSERT-PURCHASE-TYPE                    * 11510000
      *                                                               * 11520000
      *    FUNCTION :  ROUTINE TO INSERT TO THE PURCHASE-TYPE TABLE   * 11530000
      *                                                               * 11540000
      *    CALLED BY:  P50000-LOAD-PURCHASE-TYPE                      * 11550000
      *                                                               * 11560000
      ***************************************************************** 11570000
                                                                        11580000
       P51000-INSERT-PURCHASE-TYPE.                                     11590000
                                                                        11600000
           EXEC SQL                                                     11610000
               INSERT                                                   11620000
               INTO   PURCHASE_TYPE                                     11630000
                     (PREFIX,                                           11640000
                      TYPE,                                             11650000
                      DESCRIPTION)                                      11660000
               VALUES                                                   11670000
                     (:PURCHASE-TYPE-PREFIX,                            11680000
                      :PURCHASE-TYPE-TYPE,                              11690000
                      :PURCHASE-TYPE-DESCRIPTION)                       11700000
           END-EXEC.                                                    11710000
                                                                        11720000
           IF SQLCODE NOT = +0                                          11730000
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE                          11740000
               MOVE 'PDAB02U' TO WPDE-PROGRAM-ID                        11750000
               MOVE SQLCODE TO WPDE-DB2-SQLCODE                         11760000
               MOVE 'INSERT TO PURCHASE TYPE TABLE' TO WPDE-FUNCTION    11770000
               MOVE 'P51000' TO WPDE-PARAGRAPH                          11780000
               MOVE PURCHASE-TYPE TO PDA-INPUT-FORMAT                   11790000
               PERFORM P99999-ABEND THRU P99999-EXIT                    11800000
           END-IF.                                                      11810000
                                                                        11820000
       P51000-EXIT.                                                     11830000
           EXIT.                                                        11840000
           EJECT                                                        11850000
      ***************************************************************** 11860000
      *                                                               * 11870000
      *    PARAGRAPH:  P60000-LOAD-ORDER                              * 11880000
      *                                                               * 11890000
      *    FUNCTION :  ROUTINE TO LOAD THE ORDER DATABASE             * 11900000
      *                                                               * 11910000
      *    CALLED BY:  P00000-MAINLINE                                * 11920000
      *                                                               * 11930000
      ***************************************************************** 11940000
                                                                        11950000
       P60000-LOAD-ORDER.                                               11960000
                                                                        11970000
           READ INPUT-ORDER INTO PDA-INPUT-FORMAT                       11980000
               AT END                                                   11990000
                   MOVE 'Y' TO WS-END-OF-ORDER-SW                       12000000
                   IF ORDER-SEGMENT > SPACES                            12010000
                       ADD +1 TO WS-ORDER-OUT                           12020000
                       PERFORM P61000-INSERT-ORDER THRU P61000-EXIT     12030000
                   END-IF                                               12040000
                   MOVE WS-USERID TO WS-DL-L-ID                         12050000
                   MOVE 'IMS ORDER RECORDS LOADED' TO WS-DL-L-TITLE     12060000
                   MOVE WS-ORDER-OUT TO WS-DL-L-CNT                     12070000
                   ADD WS-ORDER-OUT TO WS-ORDER-TOT                     12080000
      *            DISPLAY WS-DL-LOADED                                 12090000
                   IF ORDER-ITEM-SEGMENT > SPACES                       12100000
                       ADD +1 TO WS-ORDER-ITEM-OUT                      12110000
                       PERFORM P62000-INSERT-ORDER-ITEM THRU P62000-EXIT12120000
                   END-IF                                               12130000
                   MOVE WS-USERID TO WS-DL-L-ID                         12140000
                   MOVE 'IMS ORDER ITEM RECORDS LOADED' TO WS-DL-L-TITLE12150000
                   MOVE WS-ORDER-ITEM-OUT TO WS-DL-L-CNT                12160000
                   ADD WS-ORDER-ITEM-OUT TO WS-ORDER-ITEM-TOT           12170000
      *            DISPLAY WS-DL-LOADED                                 12180000
                   GO TO P60000-EXIT.                                   12190000
                                                                        12200000
           ADD +1 TO WS-ORDER-IN.                                       12210000
                                                                        12220000
           IF PDA-SPACER-REC                                            12230000
               IF ORDER-SEGMENT > SPACES                                12240000
                   ADD +1 TO WS-ORDER-OUT                               12250000
                   PERFORM P61000-INSERT-ORDER THRU P61000-EXIT         12260000
               END-IF                                                   12270000
               IF ORDER-ITEM-SEGMENT > SPACES                           12280000
                   ADD +1 TO WS-ORDER-ITEM-OUT                          12290000
                   PERFORM P62000-INSERT-ORDER-ITEM THRU P62000-EXIT    12300000
               END-IF                                                   12310000
               GO TO P60000-EXIT                                        12320000
           END-IF.                                                      12330000
                                                                        12340000
           IF NOT PDA-DATA-REC                                          12350000
               MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                         12360000
               MOVE 'P60000' TO WPBE-PARAGRAPH                          12370000
               MOVE 'UNIDENTIFIED INPUT RECORD,' TO WPBE-MESSAGE        12380000
               MOVE WS-ORDER-IN TO WPBE-RECORD-NUMBER                   12390000
               PERFORM P99999-ABEND THRU P99999-EXIT                    12400000
           END-IF.                                                      12410000
                                                                        12420000
           EVALUATE TRUE                                                12430000
               WHEN PDA-FIELD-NAME = 'ORDER.PREFIX'                     12440000
                   MOVE PDA-DATA-005-N TO ORDER-PREFIX                  12450000
                                          OS-ORDER-PREFIX               12460000
                   IF PDA-DATA-005 NOT = WS-USERID                      12470000
                       IF WS-USERID = SPACES                            12480000
                           MOVE PDA-DATA-005 TO WS-USERID               12490000
                       ELSE                                             12500000
                           MOVE WS-USERID TO WS-DL-L-ID                 12510000
                           MOVE 'IMS ORDER RECORDS LOADED' TO           12520000
                               WS-DL-L-TITLE                            12530000
                           MOVE WS-ORDER-OUT TO WS-DL-L-CNT             12540000
                           ADD WS-ORDER-OUT TO WS-ORDER-TOT             12550000
                           MOVE +0 TO WS-ORDER-OUT                      12560000
      *                    DISPLAY WS-DL-LOADED                         12570000
                           MOVE 'IMS ORDER ITEM RECORDS LOADED' TO      12580000
                               WS-DL-L-TITLE                            12590000
                           MOVE WS-ORDER-ITEM-OUT TO WS-DL-L-CNT        12600000
                           ADD WS-ORDER-ITEM-OUT TO WS-ORDER-ITEM-TOT   12610000
                           MOVE +0 TO WS-ORDER-ITEM-OUT                 12620000
      *                    DISPLAY WS-DL-LOADED                         12630000
                           MOVE PDA-DATA-005 TO WS-USERID               12640000
                       END-IF                                           12650000
                   END-IF                                               12660000
               WHEN PDA-FIELD-NAME = 'ORDER.NUMBER'                     12670000
                   MOVE WS-ORDER-NUMBER TO ORDER-NUMBER                 12680000
                                           OS-ORDER-NUMBER              12690000
                   ADD 1 TO WS-ORDER-NUMBER                             12700000
               WHEN PDA-FIELD-NAME = 'ORDER.PURCHASE-NUMBER'            12710000
                   MOVE PDA-DATA-013-N TO ORDER-PURCHASE-NUMBER         12720000
               WHEN PDA-FIELD-NAME = 'ORDER.DATE-YYMMDD'                12730000
                   MOVE PDA-DATA-006 TO ORDER-DATE-YYMMDD               12740000
               WHEN PDA-FIELD-NAME = 'ORDER.STATUS'                     12750000
                   MOVE PDA-DATA-032 TO ORDER-STATUS                    12760000
               WHEN PDA-FIELD-NAME = 'ORDER.TOTAL-AMOUNT'               12761000
                   MOVE PDA-DATA-009 TO WS-CHARACTER                    12762000
                   PERFORM P90000-CONVERT-NUMERIC THRU P90000-EXIT      12763000
                   MOVE WS-NUMERIC TO ORDER-TOTAL-AMOUNT                12764000
               WHEN PDA-FIELD-NAME = 'ORDER.NEXT-ITEM-SEQUENCE'         12765000
                   MOVE PDA-DATA-005-N TO ORDER-NEXT-ITEM-SEQUENCE      12766000
               WHEN PDA-FIELD-NAME = 'ORDER.CUSTOMER-PREFIX'            12767000
                   MOVE PDA-DATA-005-N TO ORDER-CUSTOMER-PREFIX         12768000
               WHEN PDA-FIELD-NAME = 'ORDER.CUSTOMER-ID'                12769000
                   MOVE PDA-DATA-032 TO ORDER-CUSTOMER-ID               12769100
               WHEN PDA-FIELD-NAME = 'ORDER.PURCHASE-TYPE-PREFIX'       12769200
                   MOVE PDA-DATA-005-N TO ORDER-PURCHASE-TYPE-PREFIX    12769300
               WHEN PDA-FIELD-NAME = 'ORDER.PURCHASE-TYPE'              12769400
                   MOVE PDA-DATA-003-N TO ORDER-PURCHASE-TYPE           12769500
               WHEN PDA-FIELD-NAME = 'ORDER.SHIPPER-NUMBER'             12769600
                   MOVE PDA-DATA-010 TO WS-CHARACTER                    12769700
                   PERFORM P90000-CONVERT-NUMERIC THRU P90000-EXIT      12769800
                   MOVE WS-NUMERIC TO ORDER-SHIPPER-NUMBER              12769900
               WHEN PDA-FIELD-NAME = 'ORDER-ITEM.PREFIX'                12770000
                   MOVE PDA-DATA-005-N TO ORDER-ITEM-PREFIX             12770100
               WHEN PDA-FIELD-NAME = 'ORDER-ITEM.SEQUENCE'              12770200
                   MOVE PDA-DATA-005-N TO ORDER-ITEM-SEQUENCE           12770300
               WHEN PDA-FIELD-NAME = 'ORDER-ITEM.QUANTITY'              12770400
                   MOVE PDA-DATA-009 TO WS-CHARACTER                    12770500
                   PERFORM P90000-CONVERT-NUMERIC THRU P90000-EXIT      12770600
                   MOVE WS-NUMERIC TO ORDER-ITEM-QUANTITY               12770700
               WHEN PDA-FIELD-NAME = 'ORDER-ITEM.UNIT-PRICE'            12770800
                   MOVE PDA-DATA-010 TO WS-CHARACTER                    12770900
                   PERFORM P90000-CONVERT-NUMERIC THRU P90000-EXIT      12771000
                   MOVE WS-NUMERIC TO ORDER-ITEM-UNIT-PRICE             12771100
               WHEN PDA-FIELD-NAME = 'ORDER-ITEM.ITEM-PREFIX'           12771200
                   MOVE PDA-DATA-005-N TO ORDER-ITEM-ITEM-PREFIX        12771300
               WHEN PDA-FIELD-NAME = 'ORDER-ITEM.ITEM-NUMBER'           12771400
                   MOVE PDA-DATA-032 TO ORDER-ITEM-ITEM-NUMBER          12771500
               WHEN PDA-FIELD-NAME = 'ORDER-ITEM.SUPPLIER-PREFIX'       12771600
                   MOVE PDA-DATA-005-N TO ORDER-ITEM-SUPPLIER-PREFIX    12771700
               WHEN PDA-FIELD-NAME = 'ORDER-ITEM.SUPPLIER-ID'           12771800
                   MOVE PDA-DATA-032 TO ORDER-ITEM-SUPPLIER-ID          12771900
               WHEN OTHER                                               12772000
                   MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                     12772100
                   MOVE 'P60000' TO WPBE-PARAGRAPH                      12772200
                   MOVE 'UNIDENTIFIED INPUT RECORD FIELD NAME,' TO      12772300
                        WPBE-MESSAGE                                    12772400
                   MOVE WS-ORDER-IN TO WPBE-RECORD-NUMBER               12772500
                   PERFORM P99999-ABEND THRU P99999-EXIT                12772600
           END-EVALUATE.                                                12772700
                                                                        12772800
       P60000-EXIT.                                                     12772900
           EXIT.                                                        12773000
           EJECT                                                        12773100
      ***************************************************************** 12773200
      *                                                               * 12773300
      *    PARAGRAPH:  P61000-INSERT-ORDER                            * 12773400
      *                                                               * 12773500
      *    FUNCTION :  ROUTINE TO INSERT THE ORDER TO THE ORDER       * 12773600
      *                DATABASE                                       * 12773700
      *                                                               * 12773800
      *    CALLED BY:  P60000-LOAD-ORDER                              * 12773900
      *                                                               * 12774000
      ***************************************************************** 12775000
                                                                        12776000
       P61000-INSERT-ORDER.                                             12777000
                                                                        12778000
           CALL 'CBLTDLI' USING                                         12779000
                          WS-ISRT                                       12780000
                          ORDER-PCB                                     12790000
                          ORDER-SEGMENT                                 12800000
                          ORDER-SSA-UNQUAL                              12810000
           END-CALL.                                                    12820000
                                                                        12830000
           MOVE OP-STATUS TO WS-OP-STATUS.                              12840000
                                                                        12850000
           IF OP-GOOD-RETURN                                            12860000
               MOVE SPACES TO ORDER-SEGMENT                             12870000
           ELSE                                                         12880000
               MOVE 'IMS' TO WS-PDA-ERROR-TYPE                          12890000
               MOVE 'PDAB02U' TO WPIE-PROGRAM-ID                        12900000
               MOVE 'P61000' TO WPIE-PARAGRAPH                          12910000
               MOVE OP-STATUS TO WPIE-STATUS-CODE                       12920000
               MOVE 'ISRT' TO WPIE-FUNCTION-CODE                        12930000
               MOVE 'ORDER' TO WPIE-SEGMENT-NAME                        12940000
               MOVE 'ORDER' TO WPIE-DATABASE-NAME                       12950000
               MOVE 'INSERT ORDER TO DATABASE' TO WPIE-COMMAND          12960000
               PERFORM P99999-ABEND THRU P99999-EXIT                    12970000
           END-IF.                                                      12980000
                                                                        12990000
       P61000-EXIT.                                                     13000000
           EXIT.                                                        13010000
           EJECT                                                        13020000
      ***************************************************************** 13030000
      *                                                               * 13040000
      *    PARAGRAPH:  P62000-INSERT-ORDER-ITEM                       * 13050000
      *                                                               * 13060000
      *    FUNCTION :  ROUTINE TO INSERT THE ORDER-ITEM TO THE ORDER  * 13070000
      *                DATABASE                                       * 13080000
      *                                                               * 13090000
      *    CALLED BY:  P60000-LOAD-ORDER                              * 13100000
      *                                                               * 13110000
      ***************************************************************** 13120000
                                                                        13130000
       P62000-INSERT-ORDER-ITEM.                                        13140000
                                                                        13150000
           CALL 'CBLTDLI' USING                                         13160000
                          WS-ISRT                                       13170000
                          ORDER-PCB                                     13180000
                          ORDER-ITEM-SEGMENT                            13190000
                          ORDER-SSA                                     13200000
                          ORDER-ITEM-SSA-UNQUAL                         13210000
           END-CALL.                                                    13220000
                                                                        13230000
           MOVE OP-STATUS TO WS-OP-STATUS.                              13240000
                                                                        13250000
           IF OP-GOOD-RETURN                                            13260000
               MOVE SPACES TO ORDER-ITEM-SEGMENT                        13270000
           ELSE                                                         13280000
               MOVE 'IMS' TO WS-PDA-ERROR-TYPE                          13290000
               MOVE 'PDAB02U' TO WPIE-PROGRAM-ID                        13300000
               MOVE 'P62000' TO WPIE-PARAGRAPH                          13310000
               MOVE OP-STATUS TO WPIE-STATUS-CODE                       13320000
               MOVE 'ISRT' TO WPIE-FUNCTION-CODE                        13330000
               MOVE 'ORDITEM' TO WPIE-SEGMENT-NAME                      13340000
               MOVE 'ORDER' TO WPIE-DATABASE-NAME                       13350000
               MOVE 'INSERT ORDER ITEM TO DATABASE' TO WPIE-COMMAND     13360000
               PERFORM P99999-ABEND THRU P99999-EXIT                    13370000
           END-IF.                                                      13380000
                                                                        13390000
       P62000-EXIT.                                                     13400000
           EXIT.                                                        13410000
           EJECT                                                        13420000
      ***************************************************************** 13430000
      *                                                               * 13440000
      *    PARAGRAPH:  P70000-LOAD-AFF-CUSTOMER                       * 13450000
      *                                                               * 13460000
      *    FUNCTION :  ROUTINE TO LOAD THE AFFILIATE CUSTOMER DB2 TBL * 13470000
      *                                                               * 13480000
      *    CALLED BY:  P00000-MAINLINE                                * 13490000
      *                                                               * 13500000
      ***************************************************************** 13510000
                                                                        13520000
       P70000-LOAD-AFF-CUSTOMER.                                        13530000
                                                                        13540000
           READ INPUT-AFF-CUSTOMER INTO PDA-INPUT-FORMAT                13550000
             AT END                                                     13560000
               MOVE 'Y' TO WS-END-OF-AFF-CUST-SW                        13570000
                 IF AFF-CUSTOMER-RECORD > SPACES                        13580000
                   WRITE AFF-CUSTOMER-RECORD                            13590000
                   IF WS-AFFCUST-STATUS NOT = ZEROS                     13600000
                     MOVE 'VSAM' TO WS-PDA-ERROR-TYPE                   13610000
                     MOVE 'P70000' TO WPBE-PARAGRAPH                    13620000
                     MOVE WS-AFFCUST-STATUS TO WPBE-MESSAGE             13630000
                     MOVE WS-AFF-CUSTOMER-IN TO WPBE-RECORD-NUMBER      13640000
                     PERFORM P99999-ABEND THRU P99999-EXIT              13650000
                   END-IF                                               13660000
                   ADD +1 TO WS-AFF-CUSTOMER-OUT                        13670000
                 END-IF                                                 13680000
                 MOVE WS-USERID TO WS-DL-L-ID                           13690000
                 MOVE 'VSAM AFF CUSTOMER RECS LOADED' TO WS-DL-L-TITLE  13700000
                 MOVE WS-AFF-CUSTOMER-OUT TO WS-DL-L-CNT                13710000
                 ADD WS-AFF-CUSTOMER-OUT TO WS-AFF-CUSTOMER-TOT         13720000
      *          DISPLAY WS-DL-LOADED                                   13730000
                 GO TO P70000-EXIT.                                     13740000
                                                                        13750000
           ADD +1 TO WS-AFF-CUSTOMER-IN.                                13760000
                                                                        13770000
           IF PDA-SPACER-REC                                            13780000
             IF AFF-CUSTOMER-RECORD > SPACES                            13790000
               WRITE AFF-CUSTOMER-RECORD                                13800000
               IF WS-AFFCUST-STATUS NOT = ZEROS                         13810000
                 MOVE 'VSAM' TO WS-PDA-ERROR-TYPE                       13820000
                 MOVE 'P70000' TO WPBE-PARAGRAPH                        13830000
                 MOVE WS-AFFCUST-STATUS TO WPBE-MESSAGE                 13840000
                 MOVE WS-AFF-CUSTOMER-IN TO WPBE-RECORD-NUMBER          13850000
                 PERFORM P99999-ABEND THRU P99999-EXIT                  13860000
               END-IF                                                   13870000
               ADD +1 TO WS-AFF-CUSTOMER-OUT                            13880000
               MOVE SPACES TO AFF-CUSTOMER-RECORD                       13890000
             END-IF                                                     13900000
             GO TO P70000-EXIT                                          13910000
           END-IF.                                                      13920000
                                                                        13930000
           IF NOT PDA-DATA-REC                                          13940000
             MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                           13950000
             MOVE 'P70000' TO WPBE-PARAGRAPH                            13960000
             MOVE 'UNIDENTIFIED INPUT RECORD,' TO WPBE-MESSAGE          13970000
             MOVE WS-AFF-CUSTOMER-IN TO WPBE-RECORD-NUMBER              13980000
             PERFORM P99999-ABEND THRU P99999-EXIT                      13990000
           END-IF.                                                      14000000
                                                                        14010000
           MOVE ZEROES                 TO AFF-CUSTOMER-LAST-ORDER-AMT.  14020000
                                                                        14030000
           EVALUATE TRUE                                                14040000
             WHEN PDA-FIELD-NAME = 'CUSTOMER.PREFIX'                    14050000
               MOVE PDA-DATA-005-N TO AFF-CUSTOMER-PREFIX               14060000
               IF PDA-DATA-005 NOT = WS-USERID                          14070000
                 IF WS-USERID = SPACES                                  14080000
                   MOVE PDA-DATA-005 TO WS-USERID                       14090000
                 ELSE                                                   14100000
                   MOVE WS-USERID TO WS-DL-L-ID                         14110000
                   MOVE 'VSAM CUSTOMER RECORDS LOADED' TO               14120000
                        WS-DL-L-TITLE                                   14130000
                   MOVE WS-AFF-CUSTOMER-OUT TO WS-DL-L-CNT              14140000
                   ADD WS-AFF-CUSTOMER-OUT TO WS-AFF-CUSTOMER-TOT       14150000
                   MOVE +0 TO WS-AFF-CUSTOMER-OUT                       14160000
      *            DISPLAY WS-DL-LOADED                                 14161000
                   MOVE PDA-DATA-005 TO WS-USERID                       14162000
                 END-IF                                                 14163000
               END-IF                                                   14164000
             WHEN PDA-FIELD-NAME = 'CUSTOMER.ID'                        14165000
               MOVE PDA-DATA-032 TO AFF-CUSTOMER-ID                     14165100
             WHEN PDA-FIELD-NAME = 'CUSTOMER.PASSWORD'                  14165200
               MOVE PDA-DATA-032 TO AFF-CUSTOMER-PASSWORD               14165300
             WHEN PDA-FIELD-NAME = 'CUSTOMER.NAME'                      14165400
               MOVE PDA-DATA-064 TO AFF-CUSTOMER-NAME                   14165500
             WHEN PDA-FIELD-NAME = 'CUSTOMER.ADDRESS'                   14165600
               MOVE PDA-DATA-128 TO AFF-CUSTOMER-ADDRESS                14165700
             WHEN PDA-FIELD-NAME = 'CUSTOMER.CITY'                      14165800
               MOVE PDA-DATA-032 TO AFF-CUSTOMER-CITY                   14165900
             WHEN PDA-FIELD-NAME = 'CUSTOMER.STATE'                     14166000
               MOVE PDA-DATA-032 TO AFF-CUSTOMER-STATE                  14166100
             WHEN PDA-FIELD-NAME = 'CUSTOMER.POSTAL-CODE'               14166200
               MOVE PDA-DATA-012 TO AFF-CUSTOMER-POSTAL-CODE            14166300
             WHEN PDA-FIELD-NAME = 'CUSTOMER.SHIP-TO-NAME'              14166400
               MOVE PDA-DATA-064 TO AFF-CUSTOMER-SHIP-TO-NAME           14166500
             WHEN PDA-FIELD-NAME = 'CUSTOMER.SHIP-TO-ADDRESS'           14166600
               MOVE PDA-DATA-128 TO AFF-CUSTOMER-SHIP-TO-ADDRESS        14166700
             WHEN PDA-FIELD-NAME = 'CUSTOMER.SHIP-TO-CITY'              14166800
               MOVE PDA-DATA-032 TO AFF-CUSTOMER-SHIP-TO-CITY           14166900
             WHEN PDA-FIELD-NAME = 'CUSTOMER.SHIP-TO-STATE'             14167000
               MOVE PDA-DATA-032 TO AFF-CUSTOMER-SHIP-TO-STATE          14167100
             WHEN PDA-FIELD-NAME = 'CUSTOMER.SHIP-TO-POSTAL-CODE'       14167200
               MOVE PDA-DATA-012 TO AFF-CUSTOMER-SHIP-TO-POST-CODE      14167300
             WHEN PDA-FIELD-NAME = 'CUSTOMER.EMAIL-ADDRESS'             14167400
               MOVE PDA-DATA-128 TO AFF-CUSTOMER-EMAIL-ADDRESS          14167500
             WHEN OTHER                                                 14167600
               MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                         14167700
               MOVE 'P70000' TO WPBE-PARAGRAPH                          14167800
               MOVE 'UNIDENTIFIED INPUT RECORD FIELD NAME,' TO          14167900
                     WPBE-MESSAGE                                       14168000
               MOVE WS-AFF-CUSTOMER-IN TO WPBE-RECORD-NUMBER            14168100
               PERFORM P99999-ABEND THRU P99999-EXIT                    14168200
           END-EVALUATE.                                                14168300
                                                                        14168400
       P70000-EXIT.                                                     14168500
           EXIT.                                                        14168600
           EJECT                                                        14168700
      ***************************************************************** 14168800
      *                                                               * 14168900
      *    PARAGRAPH:  P75000-LOAD-ORDER-LOG                          * 14169000
      *                                                               * 14169100
      *    FUNCTION :  ROUTINE TO LOAD THE ORDER LOG TABLE            * 14169200
      *                                                               * 14169300
      *    CALLED BY:  P00000-MAINLINE                                * 14169400
      *                                                               * 14169500
      ***************************************************************** 14169600
                                                                        14169700
       P75000-LOAD-ORDER-LOG.                                           14169800
                                                                        14169900
           READ INPUT-ORDER-LOG INTO PDA-INPUT-FORMAT                   14170000
             AT END                                                     14170100
               MOVE 'Y' TO WS-END-OF-ORDER-LOG-SW                       14170200
               IF ORDER-LOG > SPACES                                    14170300
                 ADD +1 TO WS-ORDER-LOG-OUT                             14170400
                 PERFORM P75100-INSERT-ORDER-LOG THRU P75100-EXIT       14170500
               END-IF                                                   14170600
               MOVE WS-USERID TO WS-DL-L-ID                             14170700
               MOVE 'DB2 ORDER LOG RECORDS LOADED' TO WS-DL-L-TITLE     14170800
               MOVE WS-ORDER-LOG-OUT TO WS-DL-L-CNT                     14170900
               ADD WS-ORDER-LOG-OUT TO WS-ORDER-LOG-TOT                 14171000
      *        DISPLAY WS-DL-LOADED                                     14171100
               GO TO P75000-EXIT.                                       14171200
                                                                        14171300
           ADD +1 TO WS-ORDER-LOG-IN.                                   14171400
                                                                        14171500
           IF PDA-SPACER-REC                                            14171600
             IF ORDER-LOG > SPACES                                      14171700
               ADD +1 TO WS-ORDER-LOG-OUT                               14171800
               PERFORM P75100-INSERT-ORDER-LOG THRU P75100-EXIT         14171900
               MOVE SPACES TO ORDER-LOG                                 14172000
             END-IF                                                     14173000
             GO TO P75000-EXIT                                          14173100
           END-IF.                                                      14173200
                                                                        14173300
           IF NOT PDA-DATA-REC                                          14173400
             MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                           14173500
             MOVE 'P75000' TO WPBE-PARAGRAPH                            14173600
             MOVE 'UNIDENTIFIED INPUT RECORD,' TO WPBE-MESSAGE          14173700
             MOVE WS-ORDER-LOG-IN TO WPBE-RECORD-NUMBER                 14173800
             PERFORM P99999-ABEND THRU P99999-EXIT                      14173900
           END-IF.                                                      14174000
                                                                        14174100
           EVALUATE TRUE                                                14174200
             WHEN PDA-FIELD-NAME = 'ORDER.PREFIX'                       14174300
               MOVE PDA-DATA-005-N TO ORDER-LOG-PREFIX                  14174400
               IF PDA-DATA-005 NOT = WS-USERID                          14174500
                 IF WS-USERID = SPACES                                  14174600
                   MOVE PDA-DATA-005 TO WS-USERID                       14174700
                 ELSE                                                   14174800
                   MOVE WS-USERID TO WS-DL-L-ID                         14174900
                   MOVE 'DB2 ORDER LOG RECORDS LOADED' TO               14175000
                        WS-DL-L-TITLE                                   14175100
                   MOVE WS-ORDER-LOG-OUT TO WS-DL-L-CNT                 14175200
                   ADD WS-ORDER-LOG-OUT TO WS-ORDER-LOG-TOT             14175300
                   MOVE +0 TO WS-ORDER-LOG-OUT                          14175400
      *            DISPLAY WS-DL-LOADED                                 14175500
                   MOVE PDA-DATA-005 TO WS-USERID                       14175600
                 END-IF                                                 14175700
               END-IF                                                   14175800
               WHEN PDA-FIELD-NAME = 'ORDER.NUMBER'                     14175900
                 MOVE WS-ORDER-LOG-NUMBER TO ORDER-LOG-NUMBER           14176000
                 ADD 1 TO WS-ORDER-LOG-NUMBER                           14177000
               WHEN PDA-FIELD-NAME = 'ORDER.PURCHASE-NUMBER'            14177100
                 MOVE PDA-DATA-013-N TO ORDER-LOG-PURCHASE-NUMBER       14177200
               WHEN PDA-FIELD-NAME = 'ORDER.DATE-YYMMDD'                14177300
                 MOVE PDA-DATA-006 TO ORDER-LOG-DATE-YYMMDD             14177400
               WHEN PDA-FIELD-NAME = 'ORDER.STATUS'                     14177500
                 MOVE PDA-DATA-032 TO ORDER-LOG-STATUS                  14177600
               WHEN PDA-FIELD-NAME = 'ORDER.TOTAL-AMOUNT'               14177700
                 MOVE PDA-DATA-009 TO WS-CHARACTER                      14177800
                 PERFORM P90000-CONVERT-NUMERIC THRU P90000-EXIT        14177900
                 MOVE WS-NUMERIC TO ORDER-LOG-TOTAL-AMOUNT              14178000
               WHEN PDA-FIELD-NAME = 'ORDER.NEXT-ITEM-SEQUENCE'         14178100
                 CONTINUE                                               14178200
               WHEN PDA-FIELD-NAME = 'ORDER.CUSTOMER-PREFIX'            14178300
                 MOVE PDA-DATA-005-N TO ORDER-LOG-CUSTOMER-PREFIX       14178400
               WHEN PDA-FIELD-NAME = 'ORDER.CUSTOMER-ID'                14178500
                 MOVE PDA-DATA-032 TO ORDER-LOG-CUSTOMER-ID             14178600
               WHEN PDA-FIELD-NAME = 'ORDER.PURCHASE-TYPE-PREFIX'       14178700
                 MOVE PDA-DATA-005-N TO ORDER-LOG-PURCHASE-TYPE-PRE     14178800
               WHEN PDA-FIELD-NAME = 'ORDER.PURCHASE-TYPE'              14178900
                 MOVE PDA-DATA-003-N TO ORDER-LOG-PURCHASE-TYPE         14179000
               WHEN PDA-FIELD-NAME = 'ORDER.SHIPPER-NUMBER'             14179100
                 MOVE PDA-DATA-010 TO WS-CHARACTER                      14179200
                 PERFORM P90000-CONVERT-NUMERIC THRU P90000-EXIT        14179300
                 MOVE WS-NUMERIC TO ORDER-LOG-SHIPPER-NUMBER            14179400
               WHEN OTHER                                               14179500
                 MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                       14179600
                 MOVE 'P75000' TO WPBE-PARAGRAPH                        14179700
                 MOVE 'UNIDENTIFIED INPUT RECORD FIELD NAME,' TO        14179800
                      WPBE-MESSAGE                                      14179900
                 MOVE WS-ORDER-LOG-IN TO WPBE-RECORD-NUMBER             14180000
                 PERFORM P99999-ABEND THRU P99999-EXIT                  14180100
           END-EVALUATE.                                                14180200
                                                                        14180300
       P75000-EXIT.                                                     14180400
           EXIT.                                                        14180500
           EJECT                                                        14180600
      ***************************************************************** 14180700
      *                                                               * 14180800
      *    PARAGRAPH:  P75100-INSERT-ORDER-LOG                        * 14180900
      *                                                               * 14181000
      *    FUNCTION :  ROUTINE TO INSERT TO THE ORDER LOG DB2 TABLE   * 14182000
      *                                                               * 14183000
      *    CALLED BY:  P75000-LOAD-ORDER-LOG                          * 14184000
      *                                                               * 14185000
      ***************************************************************** 14186000
                                                                        14187000
       P75100-INSERT-ORDER-LOG.                                         14187100
                                                                        14187200
           EXEC SQL                                                     14187300
               INSERT                                                   14187400
               INTO   ORDER_LOG                                         14187500
                     (PREFIX,                                           14187600
                      NUMBER,                                           14187700
                      PURCHASE_NUMBER,                                  14187800
                      DATE_YYMMDD,                                      14187900
                      STATUS,                                           14188000
                      TOTAL_AMOUNT,                                     14188100
                      CUSTOMER_PREFIX,                                  14188200
                      CUSTOMER_ID,                                      14188300
                      PURCHASE_TYPE_PRE,                                14188400
                      PURCHASE_TYPE,                                    14188500
                      SHIPPER_NUMBER)                                   14188600
               VALUES                                                   14188700
                     (:ORDER-LOG-PREFIX,                                14188800
                      :ORDER-LOG-NUMBER,                                14188900
                      :ORDER-LOG-PURCHASE-NUMBER,                       14189000
                      :ORDER-LOG-DATE-YYMMDD,                           14189100
                      :ORDER-LOG-STATUS,                                14189200
                      :ORDER-LOG-TOTAL-AMOUNT,                          14189300
                      :ORDER-LOG-CUSTOMER-PREFIX,                       14189400
                      :ORDER-LOG-CUSTOMER-ID,                           14189500
                      :ORDER-LOG-PURCHASE-TYPE-PRE,                     14189600
                      :ORDER-LOG-PURCHASE-TYPE,                         14189700
                      :ORDER-LOG-SHIPPER-NUMBER)                        14189800
           END-EXEC.                                                    14189900
                                                                        14190000
           IF SQLCODE NOT = +0                                          14191000
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE                          14191100
               MOVE 'PDAB02U' TO WPDE-PROGRAM-ID                        14191200
               MOVE SQLCODE TO WPDE-DB2-SQLCODE                         14191300
               MOVE 'INSERT TO ORDER LOG TABLE' TO WPDE-FUNCTION        14191400
               MOVE 'P75100' TO WPDE-PARAGRAPH                          14191500
               MOVE ORDER-LOG TO PDA-INPUT-FORMAT                       14191600
               PERFORM P99999-ABEND THRU P99999-EXIT                    14191700
           END-IF.                                                      14191800
                                                                        14191900
       P75100-EXIT.                                                     14192000
           EXIT.                                                        14192100
           EJECT                                                        14192200
      ***************************************************************** 14192300
      *                                                               * 14192400
      *    PARAGRAPH:  P80000-LOAD-AFF-SUPPLIER                        *14192500
      *                                                               * 14192600
      *    FUNCTION :  ROUTINE TO LOAD THE AFFILIATE SUPPLIER TABLE   * 14192700
      *                                                               * 14192800
      *    CALLED BY:  P00000-MAINLINE                                * 14192900
      *                                                               * 14193000
      ***************************************************************** 14193100
                                                                        14193200
       P80000-LOAD-AFF-SUPPLIER.                                        14193300
                                                                        14193400
           READ INPUT-AFF-SUPPLIER INTO PDA-INPUT-FORMAT                14193500
             AT END                                                     14193600
               MOVE 'Y' TO WS-END-OF-AFF-SUPLR-SW                       14193700
               IF AFFILIATE-SUPPLIER > SPACES                           14193800
                 ADD +1 TO WS-AFF-SUPPLIER-OUT                          14193900
                 PERFORM P81000-INSERT-AFF-SUPPLIER THRU P81000-EXIT    14194000
               END-IF                                                   14194100
               MOVE WS-USERID TO WS-DL-L-ID                             14194200
               MOVE 'DB2 AFF SUPPLIER ROWS LOADED' TO WS-DL-L-TITLE     14194300
               MOVE WS-AFF-SUPPLIER-OUT TO WS-DL-L-CNT                  14194400
               ADD WS-AFF-SUPPLIER-OUT TO WS-AFF-SUPPLIER-TOT           14194500
      *        DISPLAY WS-DL-LOADED                                     14194600
               GO TO P80000-EXIT.                                       14194700
                                                                        14194800
           ADD +1 TO WS-AFF-SUPPLIER-IN.                                14194900
                                                                        14195000
           IF PDA-SPACER-REC                                            14195100
             IF AFFILIATE-SUPPLIER > SPACES                             14195200
               ADD +1 TO WS-AFF-SUPPLIER-OUT                            14195300
               PERFORM P81000-INSERT-AFF-SUPPLIER THRU P81000-EXIT      14195400
               MOVE SPACES TO AFFILIATE-SUPPLIER                        14195500
             END-IF                                                     14195600
             GO TO P80000-EXIT                                          14195700
           END-IF.                                                      14195800
                                                                        14195900
           IF NOT PDA-DATA-REC                                          14196000
             MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                           14196100
             MOVE 'P80000' TO WPBE-PARAGRAPH                            14196200
             MOVE 'UNIDENTIFIED INPUT RECORD,' TO WPBE-MESSAGE          14196300
             MOVE WS-AFF-SUPPLIER-IN TO WPBE-RECORD-NUMBER              14196400
             PERFORM P99999-ABEND THRU P99999-EXIT                      14196500
           END-IF.                                                      14196600
                                                                        14196700
           EVALUATE TRUE                                                14196800
             WHEN PDA-FIELD-NAME = 'SUPPLIER.PREFIX'                    14196900
               MOVE PDA-DATA-005 TO AFF-SUPP-PREFIX                     14197000
                                    WS-EA-PREFIX                        14197100
               IF PDA-DATA-005 NOT = WS-USERID                          14197200
                 IF WS-USERID = SPACES                                  14197300
                   MOVE PDA-DATA-005 TO WS-USERID                       14197400
                 ELSE                                                   14197500
                   MOVE WS-USERID TO WS-DL-L-ID                         14197600
                   MOVE 'DB2 AFF SUPPLIER ROWS LOADED' TO               14197700
                         WS-DL-L-TITLE                                  14197800
                   MOVE WS-AFF-SUPPLIER-OUT TO WS-DL-L-CNT              14197900
                   ADD WS-AFF-SUPPLIER-OUT TO WS-AFF-SUPPLIER-TOT       14198000
                   MOVE +0 TO WS-AFF-SUPPLIER-OUT                       14198100
      *            DISPLAY WS-DL-LOADED                                 14198200
                   MOVE PDA-DATA-005 TO WS-USERID                       14198300
                 END-IF                                                 14198400
               END-IF                                                   14198500
             WHEN PDA-FIELD-NAME = 'SUPPLIER.SUPPLIER-ID'               14198600
               MOVE PDA-DATA-032 TO AFF-SUPP-SUPPLIER-ID                14198700
             WHEN PDA-FIELD-NAME = 'SUPPLIER.PASSWORD'                  14198800
               MOVE PDA-DATA-032 TO AFF-SUPP-PASSWORD                   14198900
             WHEN PDA-FIELD-NAME = 'SUPPLIER.NAME'                      14199000
               MOVE PDA-DATA-064 TO AFF-SUPP-NAME                       14199100
             WHEN PDA-FIELD-NAME = 'SUPPLIER.ADDRESS'                   14199200
               MOVE PDA-DATA-128 TO AFF-SUPP-ADDRESS                    14199300
             WHEN PDA-FIELD-NAME = 'SUPPLIER.CITY'                      14199400
               MOVE PDA-DATA-032 TO AFF-SUPP-CITY                       14199500
             WHEN PDA-FIELD-NAME = 'SUPPLIER.STATE'                     14199600
               MOVE PDA-DATA-032 TO AFF-SUPP-STATE                      14199700
             WHEN PDA-FIELD-NAME = 'SUPPLIER.POSTAL-CODE'               14199800
               MOVE PDA-DATA-012 TO AFF-SUPP-POSTAL-CODE                14199900
             WHEN PDA-FIELD-NAME = 'SUPPLIER.EMAIL-ADDRESS'             14200000
               MOVE PDA-DATA-128 TO WS-EA-EMAIL-ADDRESS                 14200100
               MOVE WS-EMAIL-ADDRESS TO AFF-SUPP-EMAIL-ADDRESS          14200200
             WHEN OTHER                                                 14200300
               MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                         14200400
               MOVE 'P80000' TO WPBE-PARAGRAPH                          14200500
               MOVE 'UNIDENTIFIED INPUT RECORD FIELD NAME,' TO          14200600
                     WPBE-MESSAGE                                       14200700
               MOVE WS-AFF-SUPPLIER-IN TO WPBE-RECORD-NUMBER            14200800
               PERFORM P99999-ABEND THRU P99999-EXIT                    14200900
           END-EVALUATE.                                                14201000
                                                                        14201100
       P80000-EXIT.                                                     14201200
           EXIT.                                                        14201300
           EJECT                                                        14201400
      ***************************************************************** 14201500
      *                                                               * 14201600
      *    PARAGRAPH:  P81000-INSERT-AFF-SUPPLIER                     * 14201700
      *                                                               * 14201800
      *    FUNCTION :  ROUTINE TO INSERT TO THE AFFILIATE SUPPLIER TBL* 14201900
      *                                                               * 14202000
      *    CALLED BY:  P80000-LOAD-AFF-SUPPLIER                       * 14202100
      *                                                               * 14202200
      ***************************************************************** 14202300
                                                                        14202400
       P81000-INSERT-AFF-SUPPLIER.                                      14202500
                                                                        14202600
           EXEC SQL                                                     14202700
               INSERT                                                   14202800
               INTO   AFFILIATE_SUPPLIER                                14202900
                     (PREFIX,                                           14203000
                      SUPPLIER_ID,                                      14203100
                      PASSWORD,                                         14203200
                      NAME,                                             14203300
                      ADDRESS,                                          14203400
                      CITY,                                             14203500
                      STATE,                                            14203600
                      POSTAL_CODE,                                      14203700
                      EMAIL_ADDRESS)                                    14203800
               VALUES                                                   14203900
                     (:AFF-SUPP-PREFIX,                                 14204000
                      :AFF-SUPP-SUPPLIER-ID,                            14204100
                      :AFF-SUPP-PASSWORD,                               14204200
                      :AFF-SUPP-NAME,                                   14204300
                      :AFF-SUPP-ADDRESS,                                14204400
                      :AFF-SUPP-CITY,                                   14204500
                      :AFF-SUPP-STATE,                                  14204600
                      :AFF-SUPP-POSTAL-CODE,                            14204700
                      :AFF-SUPP-EMAIL-ADDRESS)                          14204800
           END-EXEC.                                                    14204900
                                                                        14205000
           IF SQLCODE NOT = +0                                          14205100
             MOVE 'DB2' TO WS-PDA-ERROR-TYPE                            14205200
             MOVE 'PDAB02U' TO WPDE-PROGRAM-ID                          14205300
             MOVE SQLCODE TO WPDE-DB2-SQLCODE                           14205400
             MOVE 'INSERT TO AFF SUPPLIER TABLE' TO WPDE-FUNCTION       14205500
             MOVE 'P81000' TO WPDE-PARAGRAPH                            14205600
             MOVE AFFILIATE-SUPPLIER TO PDA-INPUT-FORMAT                14205700
             PERFORM P99999-ABEND THRU P99999-EXIT                      14205800
           END-IF.                                                      14205900
                                                                        14206000
       P81000-EXIT.                                                     14206100
           EXIT.                                                        14206200
           EJECT                                                        14206300
      ***************************************************************** 14206400
      *                                                               * 14206500
      *    PARAGRAPH:  P85000-LOAD-REPORT-ORDER                       * 14206600
      *                                                               * 14206700
      *    FUNCTION :  ROUTINE TO LOAD THE VSAM ORDER FILE FOR REPORTS* 14206800
      *                                                               * 14206900
      *    CALLED BY:  P00000-MAINLINE                                * 14207000
      *                                                               * 14207100
      ***************************************************************** 14207200
                                                                        14207300
       P85000-LOAD-REPORT-ORDER.                                        14207400
                                                                        14207500
           READ INPUT-REPORT-ORDER INTO PDA-INPUT-FORMAT                14207600
             AT END                                                     14207700
               MOVE 'Y' TO WS-END-OF-RPT-ORD-SW                         14207800
               IF REPORT-ORDER-RECORD > SPACES                          14207900
                 WRITE REPORT-ORDER-RECORD                              14208000
                 IF WS-RPTORDR-STATUS NOT = ZEROS                       14208100
                   MOVE 'VSAM' TO WS-PDA-ERROR-TYPE                     14208200
                   MOVE 'P85000' TO WPBE-PARAGRAPH                      14208300
                   MOVE WS-RPTORDR-STATUS TO WPBE-MESSAGE               14208400
                   MOVE WS-REPORT-ORDER-IN TO WPBE-RECORD-NUMBER        14208500
                   PERFORM P99999-ABEND THRU P99999-EXIT                14208600
                 END-IF                                                 14208700
                 ADD +1 TO WS-REPORT-ORDER-OUT                          14208800
               END-IF                                                   14208900
               MOVE WS-USERID TO WS-DL-L-ID                             14209000
               MOVE 'VSAM REPORT ORDER RECORDS LOADED' TO WS-DL-L-TITLE 14209100
               MOVE WS-REPORT-ORDER-OUT TO WS-DL-L-CNT                  14209200
               ADD WS-REPORT-ORDER-OUT TO WS-REPORT-ORDER-TOT           14209300
      *        DISPLAY WS-DL-LOADED                                     14209400
               GO TO P85000-EXIT.                                       14209500
                                                                        14209600
           ADD +1 TO WS-REPORT-ORDER-IN.                                14209700
                                                                        14209800
           IF PDA-SPACER-REC                                            14209900
             IF REPORT-ORDER-RECORD > SPACES                            14210000
               WRITE REPORT-ORDER-RECORD                                14210100
               IF WS-RPTORDR-STATUS NOT = ZEROS                         14210200
                 MOVE 'VSAM' TO WS-PDA-ERROR-TYPE                       14210300
                 MOVE 'P85000' TO WPBE-PARAGRAPH                        14210400
                 MOVE WS-RPTORDR-STATUS TO WPBE-MESSAGE                 14210500
                 MOVE WS-REPORT-ORDER-IN TO WPBE-RECORD-NUMBER          14210600
                 PERFORM P99999-ABEND THRU P99999-EXIT                  14210700
               END-IF                                                   14210800
               ADD +1 TO WS-REPORT-ORDER-OUT                            14210900
               MOVE SPACES TO REPORT-ORDER-RECORD                       14211000
             END-IF                                                     14211100
             GO TO P85000-EXIT                                          14211200
           END-IF.                                                      14211300
                                                                        14211400
           IF NOT PDA-DATA-REC                                          14211500
             MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                           14211600
             MOVE 'P85000' TO WPBE-PARAGRAPH                            14211700
             MOVE 'UNIDENTIFIED INPUT RECORD,' TO WPBE-MESSAGE          14211800
             MOVE WS-REPORT-ORDER-IN TO WPBE-RECORD-NUMBER              14211900
             PERFORM P99999-ABEND THRU P99999-EXIT                      14212000
           END-IF.                                                      14212100
                                                                        14212200
           EVALUATE TRUE                                                14212300
             WHEN PDA-FIELD-NAME = 'ORDER.PREFIX'                       14212400
               MOVE PDA-DATA-005-N TO REPORT-ORDER-PREFIX               14212500
               IF PDA-DATA-005 NOT = WS-USERID                          14212600
                 IF WS-USERID = SPACES                                  14212700
                   MOVE PDA-DATA-005 TO WS-USERID                       14212800
                 ELSE                                                   14212900
                   MOVE WS-USERID TO WS-DL-L-ID                         14213000
                   MOVE 'VSAM REPORT ORDER RECORDS LOADED' TO           14213100
                        WS-DL-L-TITLE                                   14213200
                   MOVE WS-REPORT-ORDER-OUT TO WS-DL-L-CNT              14213300
                   ADD WS-REPORT-ORDER-OUT TO WS-REPORT-ORDER-TOT       14213400
                   MOVE +0 TO WS-REPORT-ORDER-OUT                       14213500
      *            DISPLAY WS-DL-LOADED                                 14213600
                   MOVE PDA-DATA-005 TO WS-USERID                       14213700
                 END-IF                                                 14213800
               END-IF                                                   14213900
               WHEN PDA-FIELD-NAME = 'ORDER.NUMBER'                     14214000
                 MOVE WS-REPORT-ORDER-NUMBER TO REPORT-ORDER-NUMBER     14214100
                 ADD 1 TO WS-REPORT-ORDER-NUMBER                        14214200
               WHEN PDA-FIELD-NAME = 'ORDER.PURCHASE-NUMBER'            14214300
                 MOVE PDA-DATA-013-N TO REPORT-ORDER-PURCHASE-NUMBER    14214400
               WHEN PDA-FIELD-NAME = 'ORDER.DATE-YYMMDD'                14214500
                 MOVE PDA-DATA-006 TO REPORT-ORDER-DATE-YYMMDD          14214600
               WHEN PDA-FIELD-NAME = 'ORDER.STATUS'                     14214700
                 MOVE PDA-DATA-032 TO REPORT-ORDER-STATUS               14214800
               WHEN PDA-FIELD-NAME = 'ORDER.TOTAL-AMOUNT'               14214900
                 MOVE PDA-DATA-009 TO WS-CHARACTER                      14215000
                 PERFORM P90000-CONVERT-NUMERIC THRU P90000-EXIT        14215100
                 MOVE WS-NUMERIC TO REPORT-ORDER-TOTAL-AMOUNT           14215200
               WHEN PDA-FIELD-NAME = 'ORDER.NEXT-ITEM-SEQUENCE'         14215300
                 MOVE ZEROS TO REPORT-ORDER-NEXT-ITEM-SEQ               14215400
               WHEN PDA-FIELD-NAME = 'ORDER.CUSTOMER-PREFIX'            14215500
                 MOVE PDA-DATA-005-N TO REPORT-ORDER-CUSTOMER-PREFIX    14215600
               WHEN PDA-FIELD-NAME = 'ORDER.CUSTOMER-ID'                14215700
                 MOVE PDA-DATA-032 TO REPORT-ORDER-CUSTOMER-ID          14215800
               WHEN PDA-FIELD-NAME = 'ORDER.PURCHASE-TYPE-PREFIX'       14215900
                 MOVE PDA-DATA-005-N TO REPORT-ORDER-PURCHASE-TYPE-PRE  14216000
               WHEN PDA-FIELD-NAME = 'ORDER.PURCHASE-TYPE'              14216100
                 MOVE PDA-DATA-003-N TO REPORT-ORDER-PURCHASE-TYPE      14216200
               WHEN PDA-FIELD-NAME = 'ORDER.SHIPPER-NUMBER'             14216300
                 MOVE PDA-DATA-010 TO WS-CHARACTER                      14216400
                 PERFORM P90000-CONVERT-NUMERIC THRU P90000-EXIT        14216500
                 MOVE WS-NUMERIC TO REPORT-ORDER-SHIPPER-NUMBER         14216600
               WHEN OTHER                                               14216700
                 MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                       14216800
                 MOVE 'P85000' TO WPBE-PARAGRAPH                        14216900
                 MOVE 'UNIDENTIFIED INPUT RECORD FIELD NAME,' TO        14217000
                      WPBE-MESSAGE                                      14217100
                 MOVE WS-REPORT-ORDER-IN TO WPBE-RECORD-NUMBER          14217200
                 PERFORM P99999-ABEND THRU P99999-EXIT                  14217300
           END-EVALUATE.                                                14217400
                                                                        14217500
       P85000-EXIT.                                                     14217600
           EXIT.                                                        14217700
           EJECT                                                        14217800
      ***************************************************************** 14217900
      *                                                               * 14218000
      *    PARAGRAPH:  P90000-CONVERT-NUMERIC                         * 14218100
      *                                                               * 14218200
      *    FUNCTION :  ROUTINE TO CONVERT THE EDITTED DATA TO A       * 14218300
      *                STRAIGHT NUMERIC VALUE                         * 14218400
      *                                                               * 14218500
      *    CALLED BY:  VARIOUS                                        * 14218600
      *                                                               * 14218700
      ***************************************************************** 14218800
                                                                        14218900
       P90000-CONVERT-NUMERIC.                                          14219000
                                                                        14220000
           MOVE ZEROES TO WS-NUMERIC.                                   14230000
                                                                        14240000
           PERFORM P91000-FIND-DECIMAL THRU P91000-EXIT                 14250000
               VARYING WS-SUB-CHAR FROM 1 BY 1                          14260000
                   UNTIL WS-SUB-CHAR > 15.                              14270000
                                                                        14280000
           MOVE WS-SUB-DECIMAL TO WS-SUB-CHAR.                          14290000
                                                                        14300000
           ADD 1 TO WS-SUB-CHAR.                                        14310000
                                                                        14320000
           IF WS-CHAR(WS-SUB-CHAR) NUMERIC                              14330000
               MOVE WS-CHAR(WS-SUB-CHAR) TO WS-NUM(14)                  14340000
           END-IF.                                                      14350000
                                                                        14360000
           ADD 1 TO WS-SUB-CHAR.                                        14370000
                                                                        14380000
           IF WS-CHAR(WS-SUB-CHAR) NUMERIC                              14390000
               MOVE WS-CHAR(WS-SUB-CHAR) TO WS-NUM(15)                  14400000
           END-IF.                                                      14410000
                                                                        14420000
           MOVE WS-SUB-DECIMAL TO WS-SUB-CHAR.                          14430000
                                                                        14440000
           PERFORM P92000-COPY-NUMBERS THRU P92000-EXIT                 14450000
               VARYING WS-SUB-NUM FROM 13 BY -1                         14460000
                   UNTIL WS-SUB-CHAR < 1.                               14470000
                                                                        14480000
       P90000-EXIT.                                                     14490000
           EXIT.                                                        14500000
           EJECT                                                        14510000
      ***************************************************************** 14520000
      *                                                               * 14530000
      *    PARAGRAPH:  P91000-FIND-DECIMAL                            * 14540000
      *                                                               * 14550000
      *    FUNCTION :  ROUTINE TO FIND THE DECIMAL POINT IN THE       * 14560000
      *                INCOMMING DATA                                 * 14570000
      *                                                               * 14580000
      *    CALLED BY:  P90000-CONVERT-NUMERIC                         * 14590000
      *                                                               * 14600000
      ***************************************************************** 14610000
                                                                        14620000
       P91000-FIND-DECIMAL.                                             14630000
                                                                        14640000
           IF WS-CHAR(WS-SUB-CHAR) = '.'                                14650000
               MOVE WS-SUB-CHAR TO WS-SUB-DECIMAL                       14660000
               MOVE 16 TO WS-SUB-CHAR                                   14670000
           END-IF.                                                      14680000
                                                                        14690000
       P91000-EXIT.                                                     14700000
           EXIT.                                                        14710000
           EJECT                                                        14720000
      ***************************************************************** 14730000
      *                                                               * 14740000
      *    PARAGRAPH:  P92000-COPY-NUMBERS                            * 14750000
      *                                                               * 14760000
      *    FUNCTION :  ROUTINE TO COPY THE NUMBERS TO THE LEFT OF THE * 14770000
      *                DECIMAL POINT                                  * 14780000
      *                                                               * 14790000
      *    CALLED BY:  P90000-CONVERT-NUMERIC                         * 14800000
      *                                                               * 14810000
      ***************************************************************** 14820000
                                                                        14830000
       P92000-COPY-NUMBERS.                                             14840000
                                                                        14850000
           SUBTRACT 1 FROM WS-SUB-CHAR.                                 14860000
                                                                        14870000
           IF WS-SUB-CHAR > 0                                           14880000
               MOVE WS-CHAR(WS-SUB-CHAR) TO WS-NUM(WS-SUB-NUM)          14890000
           END-IF.                                                      14900000
                                                                        14910000
       P92000-EXIT.                                                     14920000
           EXIT.                                                        14930000
           EJECT                                                        14940000
      ***************************************************************** 14950000
      *                                                               * 14960000
      *    PARAGRAPH:  P99999-ABEND                                   * 14970000
      *                                                               * 14980000
      *    FUNCTION :  ROUTINE TO ABEND THE PROGRAM WHEN A CRITICAL   * 14990000
      *                ERROR HAS BEEN ENCOUNTERED                     * 15000000
      *                                                               * 15010000
      *    CALLED BY:  VARIOUS                                        * 15020000
      *                                                               * 15030000
      ***************************************************************** 15040000
                                                                        15050000
       P99999-ABEND.                                                    15060000
                                                                        15070000
           DISPLAY ' '.                                                 15080000
           DISPLAY WPEA-ERROR-01.                                       15090000
           DISPLAY WPEA-ERROR-02.                                       15100000
           DISPLAY WPEA-ERROR-03.                                       15110000
           DISPLAY WPEA-ERROR-04.                                       15120000
           DISPLAY WPEA-ERROR-05.                                       15130000
           DISPLAY WPEA-ERROR-06.                                       15140000
                                                                        15150000
           EVALUATE TRUE                                                15160000
               WHEN PDA-DB2-ERROR                                       15170000
                   MOVE WS-PDA-DB2-ERROR-01 TO WPEA-ERROR-07-TEXT       15180000
                   DISPLAY WPEA-ERROR-07                                15190000
                   DISPLAY WPEA-ERROR-06                                15200000
                   MOVE WS-PDA-DB2-ERROR-02 TO WPEA-ERROR-08-TEXT       15210000
                   DISPLAY WPEA-ERROR-08                                15220000
                   DISPLAY WPEA-ERROR-06                                15230000
                   MOVE PDA-INPUT-FORMAT TO WPEA-ERROR-08-TEXT          15240000
                   DISPLAY WPEA-ERROR-08                                15250000
               WHEN PDA-IMS-ERROR                                       15260000
                   MOVE WS-PDA-IMS-ERROR-01 TO WPEA-ERROR-07-TEXT       15270000
                   DISPLAY WPEA-ERROR-07                                15280000
                   DISPLAY WPEA-ERROR-06                                15290000
                   MOVE WS-PDA-IMS-ERROR-02 TO WPEA-ERROR-08-TEXT       15300000
                   DISPLAY WPEA-ERROR-08                                15310000
               WHEN OTHER                                               15320000
                   MOVE WS-PDA-BATCH-ERROR-01 TO WPEA-ERROR-07-TEXT     15330000
                   DISPLAY WPEA-ERROR-07                                15340000
                   DISPLAY WPEA-ERROR-06                                15350000
                   MOVE WS-PDA-BATCH-ERROR-02 TO WPEA-ERROR-08-TEXT     15360000
                   DISPLAY WPEA-ERROR-08                                15370000
                   DISPLAY WPEA-ERROR-06                                15380000
                   MOVE WS-PDA-BATCH-ERROR-03 TO WPEA-ERROR-08-TEXT     15390000
                   DISPLAY WPEA-ERROR-08                                15400000
                   DISPLAY WPEA-ERROR-06                                15410000
                   MOVE WS-PDA-BATCH-ERROR-04 TO WPEA-ERROR-08-TEXT     15420000
                   DISPLAY WPEA-ERROR-08                                15430000
                   MOVE PDA-INPUT-FORMAT TO WPEA-ERROR-08-TEXT          15440000
                   DISPLAY WPEA-ERROR-08                                15450000
           END-EVALUATE.                                                15460000
                                                                        15470000
           DISPLAY WPEA-ERROR-09.                                       15480000
           DISPLAY WPEA-ERROR-10.                                       15490000
           DISPLAY ' '.                                                 15500000
                                                                        15510000
           MOVE 99 TO WS-RETURN-CODE.                                   15520000
                                                                        15530000
           CALL 'ILBOABN0' USING WS-RETURN-CODE.                        15540000
                                                                        15550000
           MOVE WS-RETURN-CODE TO RETURN-CODE.                          15560000
                                                                        15570000
           GOBACK.                                                      15580000
                                                                        15590000
       P99999-EXIT.                                                     15600000
           EXIT.                                                        15610000
           EJECT                                                        15620000