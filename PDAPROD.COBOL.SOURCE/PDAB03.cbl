       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. PDAB03.                                              00020000
      *                                                                 00030000
      ***************************************************************** 00040000
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       * 00050000
      *                       COMPUWARE CORPORATION                   * 00060000
      *                                                               * 00070000
      * PROGRAM :   PDAB03                                            * 00080000
      *                                                               * 00090000
      * FUNCTION:   PROGRAM PDAB03 IS A BATCH PROGRAM THAT WILL READ  * 00100000
      *             THE USERID TABLE LOOKING FOR USERIDS THAT HAVE    * 00110000
      *             NOT BEEN ACCESSED FOR A WHILE.  THIS TIME SPAN    * 00120000
      *             WILL BE PARAMETER CONTROLLED AT EXECUTION TIME.   * 00130000
      *             A PARM WILL BE SUPPLIED THAT SPECIFIES A NUMBER   * 00140000
      *             OF DAYS.  IF NO PARM IS SUPPLIED, THEN THE        * 00150000
      *             DEFAULT WILL BE 90 DAYS.                          * 00160000
      *                  1)  USER ID TABLE               (DB2)        * 00170039
      *                  2)  CUSTOMER FILE               (VSAM)       * 00180000
      *                  3)  PENDING ORDER FILE          (VSAM)       * 00190040
      *                  4)  ITEM TABLE                  (DB2)        * 00200040
      *                  5)  SUPPLIER TABLE              (DB2)        * 00210040
      *                  6)  ITEM SUPPLIER TABLE         (DB2)        * 00220040
      *                  7)  PURCHASE TYPE TABLE         (DB2)        * 00230040
      *                  8)  ORDER DATABASE              (IMS)        * 00240040
      *                                                               * 00250000
      * FILES   :   USER ID TABLE         -  DB2           (UPDATE)   * 00260000
      *             CUSTOMER FILE         -  VSAM KSDS     (UPDATE)   * 00270000
      *             PENDING ORDER FILE    -  VSAM KSDS     (UPDATE)   * 00280040
      *             ITEM TABLE            -  DB2           (UPDATE)   * 00290000
      *             SUPPLIER TABLE        -  DB2           (UPDATE)   * 00300000
      *             ITEM SUPPLIER TABLE   -  DB2           (UPDATE)   * 00310000
      *             PURCHASE TYPES TABLE  -  DB2           (UPDATE)   * 00320000
      *             ORDER DATABASE        -  IMS           (UPDATE)   * 00330000
      *                                                               * 00340000
      ***************************************************************** 00350000
      *             PROGRAM CHANGE LOG                                * 00360000
      *             -------------------                               * 00370000
      *                                                               * 00380000
      *  DATE       UPDATED BY            CHANGE DESCRIPTION          * 00390000
      *  --------   --------------------  --------------------------  * 00400000
      *  XX/XX/XX   XXXXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXX  * 00410000
      *                                                               * 00420000
      *                                                               * 00430000
      ***************************************************************** 00440000
           EJECT                                                        00450000
       ENVIRONMENT DIVISION.                                            00460000
                                                                        00470000
       INPUT-OUTPUT SECTION.                                            00480000
                                                                        00490000
       FILE-CONTROL.                                                    00500000
                                                                        00510000
           SELECT CONTROL-CARD         ASSIGN TO ICNTLCRD.              00520015
                                                                        00530015
           SELECT VSAM-CUSTOMER        ASSIGN TO VCUSTOMR               00540015
                                       ORGANIZATION IS INDEXED          00550000
                                       ACCESS IS DYNAMIC                00560000
                                       RECORD KEY IS CUSTOMER-KEY       00570000
                                       FILE STATUS IS WS-CUSTOMR-STATUS.00580000
                                                                        00590040
           SELECT VSAM-PENDING-ORDER   ASSIGN TO VPENDORD               00600040
                                       ORGANIZATION IS INDEXED          00610040
                                       ACCESS IS DYNAMIC                00620040
                                       RECORD KEY IS PENDING-ORDER-KEY  00630040
                                       FILE STATUS IS WS-PENDORD-STATUS.00640040
           EJECT                                                        00650000
       DATA DIVISION.                                                   00660000
                                                                        00670000
       FILE SECTION.                                                    00680000
                                                                        00690015
       FD CONTROL-CARD                                                  00700015
           LABEL RECORDS ARE STANDARD                                   00710015
           RECORDING MODE IS F                                          00720015
           RECORD CONTAINS 80 CHARACTERS                                00730038
           BLOCK CONTAINS 27920 CHARACTERS.                             00740038
                                                                        00750015
       01  CONTROL-CARD-REC            PIC X(80).                       00760038
                                                                        00770000
                                                                        00780000
       FD  VSAM-CUSTOMER                                                00790000
           RECORD CONTAINS 733 CHARACTERS.                              00800000
                                                                        00810000
           COPY VCUSTOMR.                                               00820000
           EJECT                                                        00830000
       FD  VSAM-PENDING-ORDER                                           00840040
           RECORD CONTAINS 89 CHARACTERS.                               00850040
                                                                        00860040
           COPY VPENDORD.                                               00870040
           EJECT                                                        00880040
       WORKING-STORAGE SECTION.                                         00890000
                                                                        00900000
      ***************************************************************** 00910000
      *    SWITCHES                                                   * 00920000
      ***************************************************************** 00930000
                                                                        00940000
       01  WS-SWITCHES.                                                 00950000
           05  WS-END-OF-PROCESS-SW    PIC X     VALUE 'N'.             00960000
               88  END-OF-PROCESS                VALUE 'Y'.             00970000
           05  WS-END-OF-CUSTOMER-SW   PIC X     VALUE 'N'.             00980007
               88  END-OF-CUSTOMER               VALUE 'Y'.             00990007
           05  WS-END-OF-PENDORDR-SW   PIC X     VALUE 'N'.             01000040
               88  END-OF-PENDORDR               VALUE 'Y'.             01010040
           05  WS-END-OF-ORDER-SW      PIC X     VALUE 'N'.             01020007
               88  END-OF-ORDER                  VALUE 'Y'.             01030007
           EJECT                                                        01040000
      ***************************************************************** 01050000
      *    MISCELLANEOUS WORK FIELDS                                  * 01060000
      ***************************************************************** 01070000
                                                                        01080000
       01  WS-MISCELLANEOUS-FIELDS.                                     01090000
           03  WS-RETURN-CODE          PIC 9(4)  VALUE ZEROES   COMP.   01100000
           03  WS-CUSTOMR-STATUS       PIC XX    VALUE SPACES.          01110000
               88  CUSTOMER-OK                   VALUE '  ' '00'.       01120010
               88  CUSTOMER-NOTFOUND             VALUE '23'.            01130042
               88  CUSTOMER-EMPTY                VALUE '47'.            01140042
           03  WS-PENDORD-STATUS       PIC XX    VALUE SPACES.          01150041
               88  PENDORDR-OK                   VALUE '  ' '00'.       01160040
               88  PENDORDR-NOTFOUND             VALUE '23'.            01170040
               88  PENDORDR-EMPTY                VALUE '47'.            01180042
           03  WS-GHU                  PIC X(4)  VALUE 'GHU '.          01190035
           03  WS-DLET                 PIC X(4)  VALUE 'DLET'.          01200018
           03  WS-OP-STATUS            PIC XX    VALUE SPACES.          01210017
               88  OP-GOOD-RETURN                VALUE '  '.            01220017
               88  OP-END-OF-DATABASE            VALUE 'GB'.            01230017
               88  OP-SEGMENT-NOT-FOUND          VALUE 'GE'.            01240017
               88  OP-END-OF-INPUT-MSG           VALUE 'QC'.            01250017
               88  OP-END-OF-INPUT-SEGMENT       VALUE 'QD'.            01260017
               88  OP-SEGMENT-ALREADY-EXISTS     VALUE 'II'.            01270017
               88  OP-CALL-IOPCB-FROM-BATCH      VALUE 'AL'.            01280017
               88  OP-SECURITY-VIOLATION         VALUE 'A4'.            01290017
           03  WS-DAYS-OLD             PIC S9(8) VALUE +0       COMP.   01300056
           03  WS-CUSTOMER-DEL         PIC S9(5) VALUE +0       COMP-3. 01310022
           03  WS-CUSTOMER-TOT         PIC S9(5) VALUE +0       COMP-3. 01320022
           03  WS-PENDORDR-DEL         PIC S9(5) VALUE +0       COMP-3. 01330040
           03  WS-PENDORDR-TOT         PIC S9(5) VALUE +0       COMP-3. 01340040
           03  WS-ITEM-SUPPLIER-DEL    PIC S9(5) VALUE +0       COMP-3. 01350027
           03  WS-ITEM-SUPPLIER-TOT    PIC S9(5) VALUE +0       COMP-3. 01360027
           03  WS-SUPPLIER-DEL         PIC S9(5) VALUE +0       COMP-3. 01370022
           03  WS-SUPPLIER-TOT         PIC S9(5) VALUE +0       COMP-3. 01380022
           03  WS-ITEM-DEL             PIC S9(5) VALUE +0       COMP-3. 01390027
           03  WS-ITEM-TOT             PIC S9(5) VALUE +0       COMP-3. 01400027
           03  WS-PURCHASE-TYPE-DEL    PIC S9(5) VALUE +0       COMP-3. 01410022
           03  WS-PURCHASE-TYPE-TOT    PIC S9(5) VALUE +0       COMP-3. 01420022
           03  WS-USERID-DEL           PIC S9(5) VALUE +0       COMP-3. 01430026
           03  WS-USERID-TOT           PIC S9(5) VALUE +0       COMP-3. 01440026
           03  WS-ORDER-DEL            PIC S9(5) VALUE +0       COMP-3. 01450022
           03  WS-ORDER-TOT            PIC S9(5) VALUE +0       COMP-3. 01460022
           03  WS-USERID-ID            PIC X(8)  VALUE SPACES.          01470004
           03  WS-USERID-NUMBER        PIC 9(5)  VALUE ZEROES.          01480004
           03  WS-USERID-NUM           REDEFINES WS-USERID-NUMBER       01490005
                                       PIC X(5).                        01500005
           03  WS-DATE.                                                 01510022
               05  WS-DATE-YEAR        PIC X(4)  VALUE SPACES.          01520022
               05  WS-DATE-MONTH       PIC XX    VALUE SPACES.          01530022
               05  WS-DATE-DAY         PIC XX    VALUE SPACES.          01540022
           03  WS-CURRENT-DATE         REDEFINES WS-DATE                01550003
                                       PIC 9(8).                        01560003
           EJECT                                                        01570000
      ***************************************************************** 01580000
      *    CONTROL CARD AREA                                          * 01590015
      ***************************************************************** 01600000
                                                                        01610000
       01  WS-CONTROL-CARD.                                             01620015
           03  FILLER                  PIC X(5)  VALUE 'DAYS='.         01630015
           03  WS-CC-DAYS              PIC 9(3)  VALUE ZEROES.          01640015
           EJECT                                                        01650000
      ***************************************************************** 01660023
      *    DISPLAY AREA                                               * 01670023
      ***************************************************************** 01680023
                                                                        01690023
       01  WS-DISPLAY-LINES.                                            01700023
           03  WS-DL-ASTERISK.                                          01710027
               05  FILLER         PIC XX    VALUE SPACES.               01720027
               05  FILLER         PIC X(68) VALUE ALL '*'.              01730027
           03  WS-DL-SPACER.                                            01740023
               05  FILLER         PIC X(69) VALUE '  *'.                01750027
               05  FILLER         PIC X     VALUE '*'.                  01760023
           03  WS-DL-TITLE.                                             01770027
               05  FILLER    PIC X(21) VALUE '  *  PDAB03 - DELETE'.    01780028
               05  FILLER    PIC X(21) VALUE 'EXPIRED IDS FROM PDA'.    01790028
               05  FILLER    PIC X(19) VALUE 'VSAM, DB2, AND IMS'.      01800028
               05  FILLER    PIC X(9)  VALUE 'FILES   *'.               01810028
           03  WS-DL-DATE.                                              01820023
               05  FILLER         PIC X(18) VALUE '  *        DATE ='.  01830027
               05  WS-DL-D-MONTH  PIC XX    VALUE SPACES.               01840023
               05  FILLER         PIC X     VALUE '/'.                  01850023
               05  WS-DL-D-DAY    PIC XX    VALUE SPACES.               01860023
               05  FILLER         PIC X     VALUE '/'.                  01870023
               05  WS-DL-D-YEAR   PIC X(4)  VALUE SPACES.               01880023
               05  FILLER         PIC X(41) VALUE SPACES.               01890027
               05  FILLER         PIC X     VALUE '*'.                  01900023
           03  WS-DL-DAYS.                                              01910023
               05  FILLER         PIC X(18) VALUE '  *    DELETE ALL'.  01920027
               05  FILLER         PIC X(18) VALUE 'IDS THAT HAVE NOT'.  01930027
               05  FILLER         PIC X(18) VALUE 'BEEN ACCESSED FOR'.  01940029
               05  WS-DL-CC-DAYS  PIC 9(3)  VALUE ZEROES.               01950023
               05  FILLER         PIC X(13) VALUE ' DAYS       *'.      01960029
           03  WS-DL-USERID.                                            01970023
               05  FILLER         PIC X(18) VALUE '  *     USER ID ='.  01980027
               05  WS-DL-U-ID     PIC X(8)  VALUE SPACES.               01990023
               05  FILLER         PIC X(17) VALUE ',  USER NUMBER ='.   02000023
               05  WS-DL-U-NBR    PIC 9(5)  VALUE ZEROES.               02010030
               05  FILLER         PIC X(21) VALUE ', DELETED'.          02020027
               05  FILLER         PIC X     VALUE '*'.                  02030023
           03  WS-DL-DELETED.                                           02040024
               05  FILLER         PIC X(10) VALUE '  *'.                02050027
               05  WS-DL-D-CNT    PIC ZZ,ZZ9.                           02060024
               05  FILLER         PIC X(3)  VALUE ' - '.                02070023
               05  WS-DL-D-TITLE  PIC X(50) VALUE SPACES.               02080027
               05  FILLER         PIC X     VALUE '*'.                  02090023
           EJECT                                                        02100023
      ***************************************************************** 02110023
      *    DB2  DEFINITIONS                                           * 02120000
      ***************************************************************** 02130000
                                                                        02140000
      ***************************************************************** 02150000
      *         SQL COMMUNICATIONS AREA                               * 02160000
      ***************************************************************** 02170000
                                                                        02180000
           EXEC SQL                                                     02190000
              INCLUDE SQLCA                                             02200000
           END-EXEC.                                                    02210000
           EJECT                                                        02220000
           EXEC SQL                                                     02230000
              INCLUDE DUSERID                                           02240002
           END-EXEC.                                                    02250000
           EJECT                                                        02260000
           EXEC SQL                                                     02270002
              INCLUDE DITEM                                             02280037
           END-EXEC.                                                    02290002
           EJECT                                                        02300002
           EXEC SQL                                                     02310000
              INCLUDE DSUPPLR                                           02320037
           END-EXEC.                                                    02330000
           EJECT                                                        02340000
           EXEC SQL                                                     02350000
              INCLUDE DITMSUP                                           02360037
           END-EXEC.                                                    02370000
           EJECT                                                        02380000
           EXEC SQL                                                     02390000
              INCLUDE DPURTYP                                           02400037
           END-EXEC.                                                    02410000
           EJECT                                                        02420000
           EXEC SQL                                                     02430002
               DECLARE USERID CURSOR FOR                                02440002
                   SELECT    ID,                                        02450002
                             NUMBER,                                    02460055
                             DAYS(CURRENT DATE) - DAYS(LAST_ACCESSED)   02470055
                   FROM      USERID                                     02480002
                   ORDER BY  ID                                         02490002
           END-EXEC.                                                    02500002
           EJECT                                                        02510002
      ***************************************************************** 02520015
      *    IMS SSA AREAS                                              * 02530015
      ***************************************************************** 02540015
                                                                        02550015
       01  ORDER-SSA.                                                   02560015
           03  FILLER                  PIC X(8)  VALUE 'ORDER'.         02570015
           03  FILLER                  PIC X     VALUE '('.             02580015
           03  FILLER                  PIC X(8)  VALUE 'ORDKEY'.        02590015
           03  FILLER                  PIC XX    VALUE '>='.            02600016
           03  OS-ORDER-KEY.                                            02610018
               05  OS-ORDER-PREFIX     PIC 9(5)  VALUE ZEROES.          02620018
               05  OS-ORDER-NUMBER     PIC 9(10) VALUE ZEROES.          02630032
           03  FILLER                  PIC X     VALUE ')'.             02640015
           EJECT                                                        02650015
      ***************************************************************** 02660000
      *    IMS RECORD AREAS                                           * 02670000
      ***************************************************************** 02680000
                                                                        02690000
           COPY IORDER.                                                 02700000
                                                                        02710000
                                                                        02720000
           COPY IORDITEM.                                               02730000
           EJECT                                                        02740000
      ***************************************************************** 02750000
      *    GENERAL ERROR PROCESSING WORK AREAS (CICS, IMS-DLI, DB2)   * 02760000
      ***************************************************************** 02770000
                                                                        02780000
           COPY PDAERRWS.                                               02790000
                                                                        02800002
       01  WS-PDA-BATCH-ERROR-01.                                       02810007
           05  FILLER             PIC X     VALUE SPACES.               02820007
           05  FILLER             PIC X(7)  VALUE 'ERROR:'.             02830007
           05  FILLER             PIC X(10) VALUE 'PROGRAM ='.          02840007
           05  WPBE-PROGRAM-ID    PIC X(8)  VALUE 'PDAB03'.             02850007
           05  FILLER             PIC X(14) VALUE ', PARAGRAPH ='.      02860007
           05  WPBE-PARAGRAPH     PIC X(6)  VALUE SPACES.               02870007
                                                                        02880002
       01  WS-PDA-BATCH-ERROR-02.                                       02890007
           05  FILLER             PIC X(8)  VALUE SPACES.               02900007
           05  WPBE-MESSAGE       PIC X(40) VALUE SPACES.               02910007
           05  FILLER             PIC X(17) VALUE '   FILE STATUS ='.   02920007
           05  WPBE-FILE-STATUS   PIC XX    VALUE SPACES.               02930007
           EJECT                                                        02940000
      ***************************************************************** 02950000
      *    LINKAGE SECTION                                            * 02960000
      ***************************************************************** 02970000
                                                                        02980000
       LINKAGE SECTION.                                                 02990000
                                                                        03000014
      ****************************************************************  03010014
      *****  I-O PCB                                                    03020014
      ****************************************************************  03030014
                                                                        03040014
       01  IO-PCB.                                                      03050014
           05  FILLER                  PIC X(10) VALUE SPACES.          03060014
           05  IO-STATUS               PIC XX    VALUE SPACES.          03070014
           05  FILLER                  PIC X(20) VALUE SPACES.          03080014
                                                                        03090014
           COPY PCBORDER.                                               03100014
           EJECT                                                        03110003
      ***************************************************************** 03120003
      *    P R O C E D U R E    D I V I S I O N                       * 03130003
      ***************************************************************** 03140003
                                                                        03150003
       PROCEDURE DIVISION.                                              03160015
                                                                        03170000
                                                                        03180000
      ***************************************************************** 03190000
      *                                                               * 03200000
      *    PARAGRAPH:  P00000-MAINLINE                                * 03210000
      *                                                               * 03220000
      *    FUNCTION :  PROGRAM ENTRY, OPEN FILES, PROCESS.            * 03230000
      *                                                               * 03240000
      *    CALLED BY:  NONE                                           * 03250000
      *                                                               * 03260000
      ***************************************************************** 03270000
                                                                        03280000
       P00000-MAINLINE.                                                 03290000
                                                                        03300014
           ENTRY 'DLITCBL' USING                                        03310014
                           IO-PCB                                       03320014
                           ORDER-PCB.                                   03330014
                                                                        03340000
           OPEN INPUT CONTROL-CARD.                                     03350015
                                                                        03360015
           READ CONTROL-CARD INTO WS-CONTROL-CARD                       03370015
               AT END                                                   03380015
                   MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                     03390015
                   MOVE 'P00000' TO WPBE-PARAGRAPH                      03400015
                   MOVE 'NO CONTROL FOUND' TO WPBE-MESSAGE              03410015
                   PERFORM P99999-ABEND THRU P99999-EXIT.               03420015
                                                                        03430015
           IF WS-CC-DAYS NOT NUMERIC                                    03440015
               MOVE 90 TO WS-CC-DAYS                                    03450015
           END-IF.                                                      03460003
                                                                        03470003
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATE.                  03480009
           MOVE WS-DATE-MONTH TO WS-DL-D-MONTH.                         03490023
           MOVE WS-DATE-DAY TO WS-DL-D-DAY.                             03500023
           MOVE WS-DATE-YEAR TO WS-DL-D-YEAR.                           03510023
           MOVE WS-CC-DAYS TO WS-DL-CC-DAYS.                            03520023
                                                                        03530003
           DISPLAY ' '.                                                 03540022
           DISPLAY WS-DL-ASTERISK.                                      03550023
           DISPLAY WS-DL-SPACER.                                        03560023
           DISPLAY WS-DL-TITLE.                                         03570027
           DISPLAY WS-DL-SPACER.                                        03580023
           DISPLAY WS-DL-DATE.                                          03590027
           DISPLAY WS-DL-SPACER.                                        03600027
           DISPLAY WS-DL-DAYS.                                          03610025
           DISPLAY WS-DL-SPACER.                                        03620023
           DISPLAY WS-DL-ASTERISK.                                      03630023
                                                                        03640003
           CLOSE CONTROL-CARD.                                          03650015
                                                                        03660015
           OPEN I-O VSAM-CUSTOMER                                       03670040
                    VSAM-PENDING-ORDER.                                 03680040
                                                                        03690003
           EXEC SQL                                                     03700003
               OPEN USERID                                              03710003
           END-EXEC.                                                    03720003
                                                                        03730003
           IF SQLCODE NOT = +0                                          03740003
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE                          03750003
               MOVE 'PDAB03' TO WPDE-PROGRAM-ID                         03760003
               MOVE SQLCODE TO WPDE-DB2-SQLCODE                         03770003
               MOVE 'OPEN USERIC CURSOR' TO WPDE-FUNCTION               03780003
               MOVE 'P00000' TO WPDE-PARAGRAPH                          03790003
               PERFORM P99999-ABEND THRU P99999-EXIT.                   03800003
                                                                        03810000
           PERFORM P10000-PROCESS THRU P10000-EXIT                      03820003
               UNTIL END-OF-PROCESS.                                    03830003
                                                                        03840003
           EXEC SQL                                                     03850003
               CLOSE USERID                                             03860003
           END-EXEC.                                                    03870003
                                                                        03880003
           IF SQLCODE NOT = +0                                          03890003
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE                          03900003
               MOVE 'PDAB03' TO WPDE-PROGRAM-ID                         03910003
               MOVE SQLCODE TO WPDE-DB2-SQLCODE                         03920003
               MOVE 'CLOSE USERIC CURSOR' TO WPDE-FUNCTION              03930003
               MOVE 'P00000' TO WPDE-PARAGRAPH                          03940003
               PERFORM P99999-ABEND THRU P99999-EXIT.                   03950003
                                                                        03960000
           CLOSE VSAM-CUSTOMER                                          03970040
                 VSAM-PENDING-ORDER.                                    03980040
                                                                        03990000
           DISPLAY ' '.                                                 04000023
           DISPLAY ' '.                                                 04010023
           DISPLAY WS-DL-ASTERISK.                                      04020023
           DISPLAY WS-DL-SPACER.                                        04030023
                                                                        04040023
           MOVE 'TOTAL VSAM CUSTOMER RECORDS DELETED' TO WS-DL-D-TITLE. 04050024
           MOVE WS-CUSTOMER-TOT TO WS-DL-D-CNT.                         04060024
                                                                        04070023
           DISPLAY WS-DL-DELETED.                                       04080024
           DISPLAY WS-DL-SPACER.                                        04090023
                                                                        04100040
           MOVE 'TOTAL VSAM PENDING ORDER RECORDS DELETED' TO           04110040
               WS-DL-D-TITLE.                                           04120040
           MOVE WS-PENDORDR-TOT TO WS-DL-D-CNT.                         04130040
                                                                        04140040
           DISPLAY WS-DL-DELETED.                                       04150040
           DISPLAY WS-DL-SPACER.                                        04160040
                                                                        04170027
           MOVE 'TOTAL DB2 ITEM-SUPPLIER ROWS DELETED' TO               04180027
               WS-DL-D-TITLE.                                           04190027
           MOVE WS-ITEM-SUPPLIER-TOT TO WS-DL-D-CNT.                    04200027
                                                                        04210027
           DISPLAY WS-DL-DELETED.                                       04220027
           DISPLAY WS-DL-SPACER.                                        04230027
                                                                        04240023
           MOVE 'TOTAL DB2 SUPPLIER ROWS DELETED' TO WS-DL-D-TITLE.     04250024
           MOVE WS-SUPPLIER-TOT TO WS-DL-D-CNT.                         04260024
                                                                        04270023
           DISPLAY WS-DL-DELETED.                                       04280024
           DISPLAY WS-DL-SPACER.                                        04290023
                                                                        04300027
           MOVE 'TOTAL DB2 ITEM ROWS DELETED' TO WS-DL-D-TITLE.         04310027
           MOVE WS-ITEM-TOT TO WS-DL-D-CNT.                             04320027
                                                                        04330027
           DISPLAY WS-DL-DELETED.                                       04340027
           DISPLAY WS-DL-SPACER.                                        04350027
                                                                        04360023
           MOVE 'TOTAL DB2 PURCHASE TYPE ROWS DELETED' TO               04370023
               WS-DL-D-TITLE.                                           04380024
           MOVE WS-PURCHASE-TYPE-TOT TO WS-DL-D-CNT.                    04390024
                                                                        04400023
           DISPLAY WS-DL-DELETED.                                       04410024
           DISPLAY WS-DL-SPACER.                                        04420023
                                                                        04430026
           MOVE 'TOTAL DB2 USERID ROWS DELETED' TO WS-DL-D-TITLE.       04440026
           MOVE WS-USERID-TOT TO WS-DL-D-CNT.                           04450026
                                                                        04460026
           DISPLAY WS-DL-DELETED.                                       04470026
           DISPLAY WS-DL-SPACER.                                        04480026
                                                                        04490023
           MOVE 'TOTAL IMS ORDER RECORDS DELETED' TO WS-DL-D-TITLE.     04500024
           MOVE WS-ORDER-TOT TO WS-DL-D-CNT.                            04510024
                                                                        04520023
           DISPLAY WS-DL-DELETED.                                       04530024
           DISPLAY WS-DL-SPACER.                                        04540023
           DISPLAY WS-DL-ASTERISK.                                      04550023
           DISPLAY ' '.                                                 04560023
                                                                        04570023
           GOBACK.                                                      04580022
                                                                        04590000
       P00000-EXIT.                                                     04600000
           EXIT.                                                        04610000
           EJECT                                                        04620000
      ***************************************************************** 04630002
      *                                                               * 04640002
      *    PARAGRAPH:  P10000-PROCESS                                 * 04650002
      *                                                               * 04660002
      *    FUNCTION :  PROCESS THE USERID CURSOR TO DETERMINE IF THE  * 04670003
      *                USERID IS TO BE DELETED                        * 04680002
      *                                                               * 04690002
      *    CALLED BY:  P00000-MAINLINE                                * 04700002
      *                                                               * 04710002
      ***************************************************************** 04720002
                                                                        04730002
       P10000-PROCESS.                                                  04740002
                                                                        04750003
           EXEC SQL                                                     04760003
               FETCH  USERID                                            04770003
               INTO   :USERID-ID,                                       04780003
                      :USERID-NUMBER,                                   04790055
                      :WS-DAYS-OLD                                      04800056
           END-EXEC.                                                    04810003
                                                                        04820003
           EVALUATE TRUE                                                04830003
               WHEN SQLCODE = +0                                        04840003
                   EXIT                                                 04850003
               WHEN SQLCODE = +100                                      04860003
                   MOVE 'Y' TO WS-END-OF-PROCESS-SW                     04870003
                   GO TO P10000-EXIT                                    04880003
               WHEN OTHER                                               04890003
                   MOVE 'DB2' TO WS-PDA-ERROR-TYPE                      04900003
                   MOVE 'PDAB03' TO WPDE-PROGRAM-ID                     04910003
                   MOVE SQLCODE TO WPDE-DB2-SQLCODE                     04920003
                   MOVE 'FETCH USERID CURSOR' TO WPDE-FUNCTION          04930003
                   MOVE 'P10000' TO WPDE-PARAGRAPH                      04940003
                   PERFORM P99999-ABEND THRU P99999-EXIT                04950003
           END-EVALUATE.                                                04960003
                                                                        04970004
           IF WS-DAYS-OLD < WS-CC-DAYS                                  04980056
               GO TO P10000-EXIT                                        04990047
           END-IF.                                                      05000047
                                                                        05010004
           MOVE USERID-ID TO WS-USERID-ID                               05020023
                             WS-DL-U-ID.                                05030023
           MOVE USERID-NUMBER TO WS-USERID-NUMBER                       05040023
                                 WS-DL-U-NBR.                           05050023
           MOVE 'N' TO WS-END-OF-CUSTOMER-SW                            05060040
                       WS-END-OF-PENDORDR-SW                            05070040
                       WS-END-OF-ORDER-SW.                              05080040
                                                                        05090003
           PERFORM P11000-DELETE-CUSTOMER THRU P11000-EXIT              05100007
               UNTIL END-OF-CUSTOMER.                                   05110007
                                                                        05120040
           PERFORM P12000-DELETE-PENDORDR THRU P12000-EXIT              05130040
               UNTIL END-OF-PENDORDR.                                   05140040
                                                                        05150027
           PERFORM P13000-DELETE-ITEM-SUPPLIER THRU P13000-EXIT.        05160040
                                                                        05170002
           PERFORM P14000-DELETE-SUPPLIER THRU P14000-EXIT.             05180040
                                                                        05190027
           PERFORM P15000-DELETE-ITEM THRU P15000-EXIT.                 05200040
                                                                        05210002
           PERFORM P16000-DELETE-PURCHASE-TYPE THRU P16000-EXIT.        05220040
                                                                        05230004
           PERFORM P17000-DELETE-USERID THRU P17000-EXIT.               05240040
                                                                        05250002
           PERFORM P18000-DELETE-ORDER THRU P18000-EXIT                 05260040
               UNTIL END-OF-ORDER.                                      05270002
                                                                        05280023
           DISPLAY ' '.                                                 05290023
           DISPLAY ' '.                                                 05300027
           DISPLAY WS-DL-ASTERISK.                                      05310023
           DISPLAY WS-DL-SPACER.                                        05320023
           DISPLAY WS-DL-USERID.                                        05330023
           DISPLAY WS-DL-SPACER.                                        05340023
                                                                        05350023
           MOVE 'VSAM CUSTOMER RECORDS DELETED' TO WS-DL-D-TITLE.       05360024
           MOVE WS-CUSTOMER-DEL TO WS-DL-D-CNT.                         05370024
                                                                        05380023
           DISPLAY WS-DL-DELETED.                                       05390024
                                                                        05400040
           MOVE 'VSAM PENDING ORDER RECORDS DELETED' TO WS-DL-D-TITLE.  05410040
           MOVE WS-PENDORDR-DEL TO WS-DL-D-CNT.                         05420040
                                                                        05430040
           DISPLAY WS-DL-DELETED.                                       05440040
                                                                        05450023
           MOVE 'DB2 ITEM-SUPPLIER ROWS DELETED' TO WS-DL-D-TITLE.      05460024
           MOVE WS-ITEM-SUPPLIER-DEL TO WS-DL-D-CNT.                    05470024
                                                                        05480023
           DISPLAY WS-DL-DELETED.                                       05490024
                                                                        05500027
           MOVE 'DB2 SUPPLIER ROWS DELETED' TO WS-DL-D-TITLE.           05510027
           MOVE WS-SUPPLIER-DEL TO WS-DL-D-CNT.                         05520027
                                                                        05530027
           DISPLAY WS-DL-DELETED.                                       05540027
                                                                        05550027
           MOVE 'DB2 ITEM ROWS DELETED' TO WS-DL-D-TITLE.               05560027
           MOVE WS-ITEM-DEL TO WS-DL-D-CNT.                             05570027
                                                                        05580027
           DISPLAY WS-DL-DELETED.                                       05590027
                                                                        05600023
           MOVE 'DB2 PURCHASE TYPE ROWS DELETED' TO WS-DL-D-TITLE.      05610024
           MOVE WS-PURCHASE-TYPE-DEL TO WS-DL-D-CNT.                    05620024
                                                                        05630023
           DISPLAY WS-DL-DELETED.                                       05640024
                                                                        05650026
           MOVE 'DB2 USERID ROWS DELETED' TO WS-DL-D-TITLE.             05660026
           MOVE WS-USERID-DEL TO WS-DL-D-CNT.                           05670026
                                                                        05680026
           DISPLAY WS-DL-DELETED.                                       05690026
                                                                        05700023
           MOVE 'IMS ORDER RECORDS DELETED' TO WS-DL-D-TITLE.           05710024
           MOVE WS-ORDER-DEL TO WS-DL-D-CNT.                            05720024
                                                                        05730023
           DISPLAY WS-DL-DELETED.                                       05740024
           DISPLAY WS-DL-SPACER.                                        05750023
           DISPLAY WS-DL-ASTERISK.                                      05760023
                                                                        05770002
           ADD WS-CUSTOMER-DEL TO WS-CUSTOMER-TOT.                      05780023
           ADD WS-PENDORDR-DEL TO WS-PENDORDR-TOT.                      05790057
           ADD WS-ITEM-DEL TO WS-ITEM-TOT.                              05800023
           ADD WS-SUPPLIER-DEL TO WS-SUPPLIER-TOT.                      05810023
           ADD WS-ITEM-SUPPLIER-DEL TO WS-ITEM-SUPPLIER-TOT.            05820023
           ADD WS-PURCHASE-TYPE-DEL TO WS-PURCHASE-TYPE-TOT.            05830023
           ADD WS-USERID-DEL TO WS-USERID-TOT.                          05840026
           ADD WS-ORDER-DEL TO WS-ORDER-TOT.                            05850023
                                                                        05860023
           MOVE +0 TO WS-CUSTOMER-DEL                                   05870023
                      WS-PENDORDR-DEL                                   05880043
                      WS-ITEM-DEL                                       05890023
                      WS-SUPPLIER-DEL                                   05900023
                      WS-ITEM-SUPPLIER-DEL                              05910023
                      WS-PURCHASE-TYPE-DEL                              05920023
                      WS-USERID-DEL                                     05930026
                      WS-ORDER-DEL.                                     05940023
                                                                        05950023
       P10000-EXIT.                                                     05960003
           EXIT.                                                        05970002
           EJECT                                                        05980002
      ***************************************************************** 05990001
      *                                                               * 06000000
      *    PARAGRAPH:  P11000-DELETE-CUSTOMER                         * 06010003
      *                                                               * 06020000
      *    FUNCTION :  ROUTINE TO DELETE THE USERID FROM THE CUSTOMER * 06030004
      *                VSAM FILE                                      * 06040004
      *                                                               * 06050000
      *    CALLED BY:  P10000-PROCESS                                 * 06060003
      *                                                               * 06070000
      ***************************************************************** 06080000
                                                                        06090000
       P11000-DELETE-CUSTOMER.                                          06100003
                                                                        06110000
           MOVE WS-USERID-NUMBER TO CUSTOMER-KEY.                       06120007
                                                                        06130007
           START VSAM-CUSTOMER                                          06140007
               KEY NOT < CUSTOMER-KEY.                                  06150007
                                                                        06160007
           EVALUATE TRUE                                                06170011
               WHEN CUSTOMER-OK                                         06180011
                   EXIT                                                 06190011
               WHEN CUSTOMER-NOTFOUND                                   06200011
               WHEN CUSTOMER-EMPTY                                      06210042
                   MOVE 'Y' TO WS-END-OF-CUSTOMER-SW                    06220011
                   GO TO P11000-EXIT                                    06230011
               WHEN OTHER                                               06240021
                   MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                     06250011
                   MOVE 'P11000' TO WPBE-PARAGRAPH                      06260011
                   MOVE 'START ERROR ON VSAM CUSTOMER' TO WPBE-MESSAGE  06270011
                   MOVE WS-CUSTOMR-STATUS TO WPBE-FILE-STATUS           06280011
                   PERFORM P99999-ABEND THRU P99999-EXIT                06290011
           END-EVALUATE.                                                06300011
                                                                        06310007
           READ VSAM-CUSTOMER NEXT.                                     06320007
                                                                        06330007
           IF NOT CUSTOMER-OK                                           06340007
               MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                         06350007
               MOVE 'P11000' TO WPBE-PARAGRAPH                          06360007
               MOVE 'READ NEXT ERROR ON VSAM CUSTOMER' TO WPBE-MESSAGE  06370007
               MOVE WS-CUSTOMR-STATUS TO WPBE-FILE-STATUS               06380007
               PERFORM P99999-ABEND THRU P99999-EXIT                    06390007
           END-IF.                                                      06400007
                                                                        06410007
           IF CUSTOMER-PREFIX NOT = WS-USERID-NUM                       06420007
               MOVE 'Y' TO WS-END-OF-CUSTOMER-SW                        06430007
               GO TO P11000-EXIT                                        06440007
           END-IF.                                                      06450007
                                                                        06460007
           DELETE VSAM-CUSTOMER.                                        06470007
                                                                        06480007
           IF CUSTOMER-OK                                               06490022
               ADD +1 TO WS-CUSTOMER-DEL                                06500022
           ELSE                                                         06510022
               MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                         06520022
               MOVE 'P11000' TO WPBE-PARAGRAPH                          06530007
               MOVE 'DELETE ERROR ON VSAM CUSTOMER' TO WPBE-MESSAGE     06540007
               MOVE WS-CUSTOMR-STATUS TO WPBE-FILE-STATUS               06550007
               PERFORM P99999-ABEND THRU P99999-EXIT                    06560007
           END-IF.                                                      06570007
                                                                        06580000
       P11000-EXIT.                                                     06590003
           EXIT.                                                        06600000
           EJECT                                                        06610000
      ***************************************************************** 06620040
      *                                                               * 06630040
      *    PARAGRAPH:  P12000-DELETE-PENDORDR                         * 06640040
      *                                                               * 06650040
      *    FUNCTION :  ROUTINE TO DELETE THE USERID FROM THE PENDING  * 06660040
      *                ORDER VSAM FILE                                * 06670040
      *                                                               * 06680040
      *    CALLED BY:  P10000-PROCESS                                 * 06690040
      *                                                               * 06700040
      ***************************************************************** 06710040
                                                                        06720040
       P12000-DELETE-PENDORDR.                                          06730040
                                                                        06740040
           MOVE WS-USERID-NUMBER TO PENDING-ORDER-KEY.                  06750040
                                                                        06760040
           START VSAM-PENDING-ORDER                                     06770040
               KEY NOT < PENDING-ORDER-KEY.                             06780040
                                                                        06790040
           EVALUATE TRUE                                                06800040
               WHEN PENDORDR-OK                                         06810040
                   EXIT                                                 06820040
               WHEN PENDORDR-NOTFOUND                                   06830040
               WHEN PENDORDR-EMPTY                                      06840042
                   MOVE 'Y' TO WS-END-OF-PENDORDR-SW                    06850040
                   GO TO P12000-EXIT                                    06860040
               WHEN OTHER                                               06870040
                   MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                     06880040
                   MOVE 'P12000' TO WPBE-PARAGRAPH                      06890040
                   MOVE 'START ERROR ON VSAM PENDORDR' TO WPBE-MESSAGE  06900040
                   MOVE WS-PENDORD-STATUS TO WPBE-FILE-STATUS           06910041
                   PERFORM P99999-ABEND THRU P99999-EXIT                06920040
           END-EVALUATE.                                                06930040
                                                                        06940040
           READ VSAM-PENDING-ORDER NEXT.                                06950040
                                                                        06960040
           IF NOT PENDORDR-OK                                           06970040
               MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                         06980040
               MOVE 'P13000' TO WPBE-PARAGRAPH                          06990040
               MOVE 'READ NEXT ERROR ON VSAM PENDORDR' TO WPBE-MESSAGE  07000040
               MOVE WS-PENDORD-STATUS TO WPBE-FILE-STATUS               07010041
               PERFORM P99999-ABEND THRU P99999-EXIT                    07020040
           END-IF.                                                      07030040
                                                                        07040040
           IF PENDING-ORDER-PREFIX NOT = WS-USERID-NUM                  07050040
               MOVE 'Y' TO WS-END-OF-PENDORDR-SW                        07060040
               GO TO P12000-EXIT                                        07070040
           END-IF.                                                      07080040
                                                                        07090040
           DELETE VSAM-PENDING-ORDER.                                   07100040
                                                                        07110040
           IF PENDORDR-OK                                               07120040
               ADD +1 TO WS-PENDORDR-DEL                                07130040
           ELSE                                                         07140040
               MOVE 'BTCH' TO WS-PDA-ERROR-TYPE                         07150040
               MOVE 'P12000' TO WPBE-PARAGRAPH                          07160040
               MOVE 'DELETE ERROR ON VSAM PENDORDR' TO WPBE-MESSAGE     07170040
               MOVE WS-PENDORD-STATUS TO WPBE-FILE-STATUS               07180041
               PERFORM P99999-ABEND THRU P99999-EXIT                    07190040
           END-IF.                                                      07200040
                                                                        07210040
       P12000-EXIT.                                                     07220040
           EXIT.                                                        07230040
           EJECT                                                        07240040
      ***************************************************************** 07250040
      *                                                               * 07260030
      *    PARAGRAPH:  P13000-DELETE-ITEM-SUPPLIER                    * 07270040
      *                                                               * 07280030
      *    FUNCTION :  ROUTINE TO DELETE THE USERID FROM THE          * 07290030
      *                ITEM-SUPPLIER TABLE                            * 07300030
      *                                                               * 07310030
      *    CALLED BY:  P10000-PROCESS                                 * 07320030
      *                                                               * 07330030
      ***************************************************************** 07340030
                                                                        07350030
       P13000-DELETE-ITEM-SUPPLIER.                                     07360040
                                                                        07370030
           EXEC SQL                                                     07380030
               DELETE                                                   07390030
               FROM   ITEM_SUPPLIER                                     07400037
               WHERE  ITEM_PREFIX = :WS-USERID-NUM                      07410030
           END-EXEC.                                                    07420030
                                                                        07430030
           EVALUATE TRUE                                                07440030
               WHEN SQLCODE = +0                                        07450030
                   MOVE SQLERRD(3) TO WS-ITEM-SUPPLIER-DEL              07460030
               WHEN SQLCODE = +100                                      07470030
                   EXIT                                                 07480030
               WHEN OTHER                                               07490030
                   MOVE 'DB2' TO WS-PDA-ERROR-TYPE                      07500030
                   MOVE 'PDAB03' TO WPDE-PROGRAM-ID                     07510030
                   MOVE 'P13000' TO WPDE-PARAGRAPH                      07520040
                   MOVE 'DELETE FROM ITEM SUPPLIER TABLE' TO            07530030
                       WPDE-FUNCTION                                    07540030
                   MOVE SQLCODE TO WPDE-DB2-SQLCODE                     07550030
                   PERFORM P99999-ABEND THRU P99999-EXIT                07560030
           END-EVALUATE.                                                07570030
                                                                        07580030
       P13000-EXIT.                                                     07590040
           EXIT.                                                        07600030
           EJECT                                                        07610030
      ***************************************************************** 07620000
      *                                                               * 07630000
      *    PARAGRAPH:  P14000-DELETE-SUPPLIER                         * 07640040
      *                                                               * 07650000
      *    FUNCTION :  ROUTINE TO DELETE THE USERID FROM THE SUPPLIER * 07660004
      *                TABLE                                          * 07670004
      *                                                               * 07680004
      *    CALLED BY:  P10000-PROCESS                                 * 07690003
      *                                                               * 07700000
      ***************************************************************** 07710000
                                                                        07720000
       P14000-DELETE-SUPPLIER.                                          07730040
                                                                        07740000
           EXEC SQL                                                     07750000
               DELETE                                                   07760004
               FROM   SUPPLIER                                          07770037
               WHERE  PREFIX = :WS-USERID-NUM                           07780005
           END-EXEC.                                                    07790000
                                                                        07800013
           EVALUATE TRUE                                                07810013
               WHEN SQLCODE = +0                                        07820013
                   MOVE SQLERRD(3) TO WS-SUPPLIER-DEL                   07830025
               WHEN SQLCODE = +100                                      07840013
                   EXIT                                                 07850013
               WHEN OTHER                                               07860013
                   MOVE 'DB2' TO WS-PDA-ERROR-TYPE                      07870013
                   MOVE 'PDAB03' TO WPDE-PROGRAM-ID                     07880013
                   MOVE 'P14000' TO WPBE-PARAGRAPH                      07890040
                   MOVE 'DELETE FROM SUPPLIER TABLE' TO WPDE-FUNCTION   07900013
                   MOVE SQLCODE TO WPDE-DB2-SQLCODE                     07910013
                   PERFORM P99999-ABEND THRU P99999-EXIT                07920013
           END-EVALUATE.                                                07930013
                                                                        07940000
       P14000-EXIT.                                                     07950040
           EXIT.                                                        07960000
           EJECT                                                        07970000
      ***************************************************************** 07980030
      *                                                               * 07990030
      *    PARAGRAPH:  P15000-DELETE-ITEM                             * 08000040
      *                                                               * 08010030
      *    FUNCTION :  ROUTINE TO DELETE THE USERID FROM THE ITEM     * 08020030
      *                TABLE                                          * 08030030
      *                                                               * 08040030
      *    CALLED BY:  P10000-PROCESS                                 * 08050030
      *                                                               * 08060030
      ***************************************************************** 08070030
                                                                        08080030
       P15000-DELETE-ITEM.                                              08090040
                                                                        08100030
           EXEC SQL                                                     08110030
               DELETE                                                   08120030
               FROM   ITEM                                              08130037
               WHERE  PREFIX = :WS-USERID-NUM                           08140030
           END-EXEC.                                                    08150030
                                                                        08160030
           EVALUATE TRUE                                                08170030
               WHEN SQLCODE = +0                                        08180030
                   MOVE SQLERRD(3) TO WS-ITEM-DEL                       08190030
               WHEN SQLCODE = +100                                      08200030
                   EXIT                                                 08210030
               WHEN OTHER                                               08220030
                   MOVE 'DB2' TO WS-PDA-ERROR-TYPE                      08230030
                   MOVE 'PDAB03' TO WPDE-PROGRAM-ID                     08240030
                   MOVE 'P15000' TO WPDE-PARAGRAPH                      08250040
                   MOVE 'DELETE FROM ITEM TABLE' TO WPDE-FUNCTION       08260030
                   MOVE SQLCODE TO WPDE-DB2-SQLCODE                     08270030
                   PERFORM P99999-ABEND THRU P99999-EXIT                08280030
           END-EVALUATE.                                                08290030
                                                                        08300030
       P15000-EXIT.                                                     08310040
           EXIT.                                                        08320030
           EJECT                                                        08330030
      ***************************************************************** 08340000
      *                                                               * 08350000
      *    PARAGRAPH:  P16000-DELETE-PURCHASE-TYPE                    * 08360040
      *                                                               * 08370000
      *    FUNCTION :  ROUTINE TO DELETE THE USERID FROM THE          * 08380004
      *                PURCHASE-TYPE TABLE                            * 08390004
      *                                                               * 08400004
      *    CALLED BY:  P10000-PROCESS                                 * 08410003
      *                                                               * 08420000
      ***************************************************************** 08430000
                                                                        08440000
       P16000-DELETE-PURCHASE-TYPE.                                     08450040
                                                                        08460000
           EXEC SQL                                                     08470000
               DELETE                                                   08480004
               FROM   PURCHASE_TYPE                                     08490037
               WHERE  PREFIX = :WS-USERID-NUM                           08500005
           END-EXEC.                                                    08510000
                                                                        08520000
           EVALUATE TRUE                                                08530013
               WHEN SQLCODE = +0                                        08540013
                   MOVE SQLERRD(3) TO WS-PURCHASE-TYPE-DEL              08550025
               WHEN SQLCODE = +100                                      08560013
                   EXIT                                                 08570013
               WHEN OTHER                                               08580013
                   MOVE 'DB2' TO WS-PDA-ERROR-TYPE                      08590013
                   MOVE 'PDAB03' TO WPDE-PROGRAM-ID                     08600013
                   MOVE 'P16000' TO WPBE-PARAGRAPH                      08610040
                   MOVE 'DELETE FROM PURCHASE TYPE TABLE' TO            08620013
                       WPDE-FUNCTION                                    08630013
                   MOVE SQLCODE TO WPDE-DB2-SQLCODE                     08640013
                   PERFORM P99999-ABEND THRU P99999-EXIT                08650013
           END-EVALUATE.                                                08660013
                                                                        08670000
       P16000-EXIT.                                                     08680040
           EXIT.                                                        08690000
           EJECT                                                        08700000
      ***************************************************************** 08710004
      *                                                               * 08720004
      *    PARAGRAPH:  P17000-DELETE-USERID                           * 08730040
      *                                                               * 08740004
      *    FUNCTION :  ROUTINE TO DELETE THE USERID FROM THE USERID   * 08750004
      *                TABLE                                          * 08760004
      *                                                               * 08770004
      *    CALLED BY:  P10000-PROCESS                                 * 08780004
      *                                                               * 08790004
      ***************************************************************** 08800004
                                                                        08810004
       P17000-DELETE-USERID.                                            08820040
                                                                        08830004
           EXEC SQL                                                     08840004
               DELETE                                                   08850004
               FROM   USERID                                            08860004
               WHERE  ID = :WS-USERID-ID                                08870004
           END-EXEC.                                                    08880004
                                                                        08890004
           IF SQLCODE = +0                                              08900023
               MOVE SQLERRD(3) TO WS-USERID-DEL                         08910025
           ELSE                                                         08920023
               MOVE 'DB2' TO WS-PDA-ERROR-TYPE                          08930023
               MOVE 'PDAB03' TO WPDE-PROGRAM-ID                         08940004
               MOVE 'P17000' TO WPBE-PARAGRAPH                          08950040
               MOVE 'DELETE FROM USERID TABLE' TO WPDE-FUNCTION         08960004
               MOVE SQLCODE TO WPDE-DB2-SQLCODE                         08970004
               PERFORM P99999-ABEND THRU P99999-EXIT                    08980004
           END-IF.                                                      08990004
                                                                        09000004
       P17000-EXIT.                                                     09010040
           EXIT.                                                        09020004
           EJECT                                                        09030004
      ***************************************************************** 09040004
      *                                                               * 09050000
      *    PARAGRAPH:  P18000-DELETE-ORDER                            * 09060040
      *                                                               * 09070000
      *    FUNCTION :  ROUTINE TO DELETE THE ORDER DATABASE           * 09080003
      *                                                               * 09090000
      *    CALLED BY:  P10000-PROCESS                                 * 09100003
      *                                                               * 09110000
      ***************************************************************** 09120000
                                                                        09130000
       P18000-DELETE-ORDER.                                             09140040
                                                                        09150007
           MOVE WS-USERID-NUMBER TO OS-ORDER-PREFIX.                    09160018
           MOVE ZEROES TO OS-ORDER-NUMBER.                              09170032
                                                                        09180000
           CALL 'CBLTDLI' USING                                         09190016
                          WS-GHU                                        09200035
                          ORDER-PCB                                     09210016
                          ORDER-SEGMENT                                 09220016
                          ORDER-SSA                                     09230016
           END-CALL.                                                    09240016
                                                                        09250016
           MOVE OP-STATUS TO WS-OP-STATUS.                              09260016
                                                                        09270018
           EVALUATE TRUE                                                09280020
               WHEN OP-GOOD-RETURN                                      09290020
                   IF ORDER-PREFIX NOT = WS-USERID-NUMBER               09300020
                       MOVE 'Y' TO WS-END-OF-ORDER-SW                   09310020
                       GO TO P18000-EXIT                                09320040
                   END-IF                                               09330020
               WHEN OP-END-OF-DATABASE                                  09340020
               WHEN OP-SEGMENT-NOT-FOUND                                09350020
                   MOVE 'Y' TO WS-END-OF-ORDER-SW                       09360020
                   GO TO P18000-EXIT                                    09370040
               WHEN OTHER                                               09380020
                   MOVE 'IMS' TO WS-PDA-ERROR-TYPE                      09390020
                   MOVE 'PDAB03' TO WPIE-PROGRAM-ID                     09400020
                   MOVE 'P18000' TO WPIE-PARAGRAPH                      09410040
                   MOVE OP-STATUS TO WPIE-STATUS-CODE                   09420020
                   MOVE 'GHN ' TO WPIE-FUNCTION-CODE                    09430020
                   MOVE 'ORDER' TO WPIE-SEGMENT-NAME                    09440020
                   MOVE 'ORDER' TO WPIE-DATABASE-NAME                   09450020
                   MOVE 'GET HOLD NEXT ON ORDER DATABASE' TO            09460020
                       WPIE-COMMAND                                     09470020
                   PERFORM P99999-ABEND THRU P99999-EXIT                09480020
           END-EVALUATE.                                                09490020
                                                                        09500018
           CALL 'CBLTDLI' USING                                         09510018
                          WS-DLET                                       09520018
                          ORDER-PCB                                     09530018
                          ORDER-SEGMENT                                 09540018
           END-CALL.                                                    09550018
                                                                        09560018
           MOVE OP-STATUS TO WS-OP-STATUS.                              09570018
                                                                        09580016
           IF OP-GOOD-RETURN                                            09590023
               ADD +1 TO WS-ORDER-DEL                                   09600023
           ELSE                                                         09610023
               MOVE 'IMS' TO WS-PDA-ERROR-TYPE                          09620023
               MOVE 'PDAB03' TO WPIE-PROGRAM-ID                         09630016
               MOVE 'P18000' TO WPIE-PARAGRAPH                          09640040
               MOVE OP-STATUS TO WPIE-STATUS-CODE                       09650016
               MOVE 'DLET' TO WPIE-FUNCTION-CODE                        09660016
               MOVE 'ORDER' TO WPIE-SEGMENT-NAME                        09670016
               MOVE 'ORDER' TO WPIE-DATABASE-NAME                       09680016
               MOVE 'DELETE ORDERS FROM DATABASE' TO WPIE-COMMAND       09690016
               PERFORM P99999-ABEND THRU P99999-EXIT                    09700016
           END-IF.                                                      09710016
                                                                        09720016
       P18000-EXIT.                                                     09730040
           EXIT.                                                        09740000
           EJECT                                                        09750000
      ***************************************************************** 09760000
      *                                                               * 09770000
      *    PARAGRAPH:  P99999-ABEND                                   * 09780000
      *                                                               * 09790000
      *    FUNCTION :  ROUTINE TO ABEND THE PROGRAM WHEN A CRITICAL   * 09800000
      *                ERROR HAS BEEN ENCOUNTERED                     * 09810000
      *                                                               * 09820000
      *    CALLED BY:  VARIOUS                                        * 09830000
      *                                                               * 09840000
      ***************************************************************** 09850000
                                                                        09860000
       P99999-ABEND.                                                    09870000
                                                                        09880000
           DISPLAY ' '.                                                 09890000
           DISPLAY WPEA-ERROR-01.                                       09900000
           DISPLAY WPEA-ERROR-02.                                       09910000
           DISPLAY WPEA-ERROR-03.                                       09920000
           DISPLAY WPEA-ERROR-04.                                       09930000
           DISPLAY WPEA-ERROR-05.                                       09940000
           DISPLAY WPEA-ERROR-06.                                       09950000
                                                                        09960000
           EVALUATE TRUE                                                09970000
               WHEN PDA-DB2-ERROR                                       09980000
                   MOVE WS-PDA-DB2-ERROR-01 TO WPEA-ERROR-07-TEXT       09990000
                   DISPLAY WPEA-ERROR-07                                10000000
                   MOVE WS-PDA-DB2-ERROR-02 TO WPEA-ERROR-08-TEXT       10010000
                   DISPLAY WPEA-ERROR-08                                10020000
               WHEN PDA-IMS-ERROR                                       10030000
                   MOVE WS-PDA-IMS-ERROR-01 TO WPEA-ERROR-07-TEXT       10040000
                   DISPLAY WPEA-ERROR-07                                10050000
                   MOVE WS-PDA-IMS-ERROR-02 TO WPEA-ERROR-08-TEXT       10060000
                   DISPLAY WPEA-ERROR-08                                10070000
               WHEN OTHER                                               10080007
                   MOVE WS-PDA-BATCH-ERROR-01 TO WPEA-ERROR-07-TEXT     10090007
                   DISPLAY WPEA-ERROR-07                                10100007
                   DISPLAY WPEA-ERROR-06                                10110007
                   MOVE WS-PDA-BATCH-ERROR-02 TO WPEA-ERROR-08-TEXT     10120007
                   DISPLAY WPEA-ERROR-08                                10130007
           END-EVALUATE.                                                10140000
                                                                        10150000
           DISPLAY WPEA-ERROR-09.                                       10160000
           DISPLAY WPEA-ERROR-10.                                       10170000
           DISPLAY ' '.                                                 10180000
                                                                        10190000
           MOVE 99 TO WS-RETURN-CODE.                                   10200000
                                                                        10210000
           CALL 'ILBOABN0' USING WS-RETURN-CODE.                        10220000
                                                                        10230000
           MOVE WS-RETURN-CODE TO RETURN-CODE.                          10240000
                                                                        10250000
           GOBACK.                                                      10260000
                                                                        10270000
       P99999-EXIT.                                                     10280000
           EXIT.                                                        10290000
           EJECT                                                        10300000