       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. PDAB04.                                              00020000
      *                                                                 00030000
      ***************************************************************** 00040000
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       * 00050000
      *                       COMPUWARE CORPORATION                   * 00060000
      *                                                               * 00070000
      * PROGRAM :   PDAB04                                            * 00080000
      *                                                               * 00090000
      * FUNCTION:   PROGRAM PDAB04 IS A BATCH PROGRAM THAT WILL READ  * 00100000
      *             THE PENDING ORDER FILE AND REPORT ON AVAILABLE    * 00110000
      *             INVENTORY.                                        * 00120000
      *                                                               * 00130000
      * FILES   :   PENDING ORDER FILE    -  VSAM KSDS     (READ)     * 00140000
      *             ITEM TABLE            -  DB2           (READ)     * 00150000
      *             SUPPLIER TABLE        -  DB2           (READ)     * 00160000
      *             ITEM SUPPLIER TABLE   -  DB2           (READ)     * 00170000
      *             REPORT                -  PRINT         (OUTPUT)   * 00180000
      *                                                               * 00190000
      ***************************************************************** 00200000
      *             PROGRAM CHANGE LOG                                * 00210000
      *             -------------------                               * 00220000
      *                                                               * 00230000
      *  DATE       UPDATED BY            CHANGE DESCRIPTION          * 00240000
      *  --------   --------------------  --------------------------  * 00250000
      *  XX/XX/XX   XXX                   XXXXXXXXXXXXXXXXXXXXXXXXXX  * 00260000
      ***************************************************************** 00270000
           EJECT                                                        00280000
       ENVIRONMENT DIVISION.                                            00290000
                                                                        00300000
       INPUT-OUTPUT SECTION.                                            00310000
                                                                        00320000
       FILE-CONTROL.                                                    00330000
                                                                        00340000
           SELECT INVRPT-OUT           ASSIGN TO INVRPTO.               00350000
                                                                        00360000
           SELECT VSAM-PENDORDER       ASSIGN TO VPENDORD               00370000
                                       ORGANIZATION IS INDEXED          00380000
                                       ACCESS IS SEQUENTIAL             00390000
                                       RECORD KEY IS PEND-ORDER-KEY     00400000
                                       FILE STATUS IS WS-PENDORD-STATUS.00410000
           EJECT                                                        00420000
       DATA DIVISION.                                                   00430000
                                                                        00440000
       FILE SECTION.                                                    00450000
                                                                        00460000
       FD INVRPT-OUT                                                    00470000
           LABEL RECORDS ARE STANDARD                                   00480000
           RECORDING MODE IS F                                          00490000
           RECORD CONTAINS 133 CHARACTERS.                              00500000
                                                                        00510000
       01  INVRPT-OUT-REC              PIC X(133).                      00520000
                                                                        00530000
           EJECT                                                        00540000
       FD  VSAM-PENDORDER                                               00550000
           RECORD CONTAINS 89  CHARACTERS.                              00560000
       01  PENDING-ORDER-REC.                                           00570000
           05  PEND-ORDER-KEY.                                          00580000
               10 PEND-ORDER-PREFIX    PIC 9(05).                       00590000
               10 PEND-ORDER-SEQUENCE                                   00600000
                                       PIC 9(05).                       00610000
           05  FILLER                  PIC X(79).                       00620000
                                                                        00630000
           EJECT                                                        00640000
       WORKING-STORAGE SECTION.                                         00650000
                                                                        00660000
                                                                        00670000
      ***************************************************************** 00680000
      *    SWITCHES                                                   * 00690000
      ***************************************************************** 00700000
                                                                        00710000
       01  WS-SWITCHES.                                                 00720000
           05 WS-END-OF-PROCESS-SW     PIC X VALUE SPACES.              00730000
              88 END-OF-PROCESS              VALUE 'Y'.                 00740000
                                                                        00750000
      ***************************************************************** 00760000
      *    MISCELLANEOUS WORK FIELDS                                  * 00770000
      ***************************************************************** 00780000
                                                                        00790000
       01  WS-MISCELLANEOUS-FIELDS.                                     00800000
           05  WS-RETURN-CODE          PIC 9(4)  VALUE ZEROES   COMP.   00810000
           05  WS-PENDORD-STATUS       PIC XX    VALUE SPACES.          00820000
               88  PENDORDR-OK                   VALUE '  ' '00'.       00830000
               88  PENDORDR-NOTFOUND             VALUE '23'.            00840000
               88  PENDORDR-EMPTY                VALUE '47'.            00850000
               88  PENDORDR-END                  VALUE '10'.            00860000
               88  PENDORDR-ERR                  VALUE '23' '47'.       00870000
           05  WS-USERID               PIC X(5)  VALUE SPACES.          00880000
           05  WS-DATE.                                                 00890000
               10  WS-DATE-YEAR        PIC X(4)  VALUE SPACES.          00900000
               10  WS-DATE-MONTH       PIC XX    VALUE SPACES.          00910000
               10  WS-DATE-DAY         PIC XX    VALUE SPACES.          00920000
           05  WS-QUANTITY-ORDERED     PIC S9(9) COMP-3 VALUE ZEROS.    00930000
           05  WS-QUANTITY-AVAIL       PIC S9(9) COMP-3 VALUE ZEROS.    00940000
           05  WS-QUANTITY-REMAINING   PIC S9(9) COMP-3 VALUE ZEROS.    00950000
           05  WS-LINE-CNT             PIC S99   VALUE ZEROES.          00960000
           EJECT                                                        00970000
           COPY VPENDORD.                                               00980000
           EJECT                                                        00990000
      ***************************************************************** 01000000
      *    INVENTORY REPORT                                           * 01010000
      ***************************************************************** 01020000
                                                                        01030000
       01  WS-RPT-TITLE.                                                01040000
           05  FILLER             PIC X     VALUE '1'.                  01050000
           05  FILLER             PIC X(50) VALUE SPACES.               01060000
           05  FILLER             PIC X(21) VALUE                       01070000
               'PENDING ORDERS AS OF '.                                 01080000
           05  WS-RT-MONTH        PIC XX.                               01090000
           05  FILLER             PIC X VALUE '/'.                      01100000
           05  WS-RT-DAY          PIC XX.                               01110000
           05  FILLER             PIC X VALUE '/'.                      01120000
           05  WS-RT-YEAR         PIC X(4).                             01130000
           05  FILLER             PIC X(51).                            01140000
                                                                        01150000
       01  WS-RPT-HEADING.                                              01160000
           05  FILLER             PIC X     VALUE '-'.                  01170000
           05  FILLER             PIC X(20) VALUE SPACES.               01180000
           05  FILLER             PIC X(16) VALUE                       01190000
               'ITEM DESCRIPTION'.                                      01200000
           05  FILLER             PIC X(19) VALUE SPACES.               01210000
           05  FILLER             PIC X(11) VALUE                       01220000
               'QTY ORDERED'.                                           01230000
           05  FILLER             PIC X(11) VALUE SPACES.               01240000
           05  FILLER             PIC X(13) VALUE                       01250000
               'SUPPLIER NAME'.                                         01260000
           05  FILLER             PIC X(12) VALUE SPACES.               01270000
           05  FILLER             PIC X(11) VALUE                       01280000
               'QTY ON HAND'.                                           01290000
           05  FILLER             PIC X(2)  VALUE SPACES.               01300000
           05  FILLER             PIC X(14) VALUE                       01310000
               'AVAILABLE FLAG'.                                        01320000
           05  FILLER             PIC X(3)  VALUE SPACES.               01330000
           EJECT                                                        01340000
                                                                        01350000
       01  WS-RPT-DETAIL.                                               01360000
           05  WS-RD-CC           PIC X     VALUE ' '.                  01370000
           05  FILLER             PIC X(3).                             01380000
           05  WS-RD-ITEM-NAME    PIC X(50).                            01390000
           05  FILLER             PIC XX.                               01400000
           05  WS-RD-QTY-ORDERED  PIC ZZZ,ZZZ,ZZ9.                      01410000
           05  FILLER             PIC XX.                               01420000
           05  WS-RD-SUPPLIER     PIC X(32).                            01430000
           05  FILLER             PIC XX.                               01440000
           05  WS-RD-QTY-AVAIL    PIC ZZZ,ZZZ,ZZ9.                      01450000
           05  FILLER             PIC X(8).                             01460000
           05  WS-RD-AVAIL-FLAG   PIC X.                                01470000
           05  FILLER             PIC X(10).                            01480000
           EJECT                                                        01490000
      ***************************************************************** 01500000
      *    DB2  DEFINITIONS                                           * 01510000
      ***************************************************************** 01520000
                                                                        01530000
      ***************************************************************** 01540000
      *         SQL COMMUNICATIONS AREA                               * 01550000
      ***************************************************************** 01560000
                                                                        01570000
           EXEC SQL                                                     01580000
              INCLUDE SQLCA                                             01590000
           END-EXEC.                                                    01600000
           EJECT                                                        01610000
           EXEC SQL                                                     01620000
              INCLUDE DITEM                                             01630000
           END-EXEC.                                                    01640000
           EJECT                                                        01650000
           EXEC SQL                                                     01660000
              INCLUDE DSUPPLR                                           01670000
           END-EXEC.                                                    01680000
           EJECT                                                        01690000
           EXEC SQL                                                     01700000
              INCLUDE DITMSUP                                           01710000
           END-EXEC.                                                    01720000
           EJECT                                                        01730000
      ***************************************************************** 01740000
      *    GENERAL ERROR PROCESSING WORK AREAS (CICS, IMS-DLI, DB2)   * 01750000
      ***************************************************************** 01760000
                                                                        01770000
           COPY PDAERRWS.                                               01780000
                                                                        01790000
       01  WS-PDA-BATCH-ERROR-01.                                       01800000
           05  FILLER             PIC X     VALUE SPACES.               01810000
           05  FILLER             PIC X(7)  VALUE 'ERROR: '.            01820000
           05  FILLER             PIC X(10) VALUE 'PROGRAM = '.         01830000
           05  WPBE-PROGRAM-ID    PIC X(8)  VALUE 'PDAB04'.             01840000
           05  FILLER             PIC X(14) VALUE ', PARAGRAPH = '.     01850000
           05  WPBE-PARAGRAPH     PIC X(6)  VALUE SPACES.               01860000
                                                                        01870000
       01  WS-PDA-BATCH-ERROR-02.                                       01880000
           05  FILLER             PIC X(8)  VALUE SPACES.               01890000
           05  WPBE-MESSAGE       PIC X(39) VALUE SPACES.               01900000
           05  FILLER             PIC X(16) VALUE 'RECORD NUMBER ='.    01910000
           05  WPBE-RECORD-NUMBER PIC X(7)  VALUE ZEROES.               01920000
           05  FILLER             PIC X(8)  VALUE SPACES.               01930000
                                                                        01940000
       01  WS-PDA-BATCH-ERROR-03.                                       01950000
           05  FILLER             PIC X(8)  VALUE SPACES.               01960000
           05  FILLER             PIC X(20) VALUE 'RECORD IS DISPLAYED'.01970000
           05  FILLER             PIC X(5)  VALUE 'BELOW'.              01980000
                                                                        01990000
       01  WS-PDA-BATCH-ERROR-04.                                       02000000
           05  FILLER             PIC X(10) VALUE '----+----1'.         02010000
           05  FILLER             PIC X(10) VALUE '----+----2'.         02020000
           05  FILLER             PIC X(10) VALUE '----+----3'.         02030000
           05  FILLER             PIC X(10) VALUE '----+----4'.         02040000
           05  FILLER             PIC X(10) VALUE '----+----5'.         02050000
           05  FILLER             PIC X(10) VALUE '----+----6'.         02060000
           05  FILLER             PIC X(10) VALUE '----+----7'.         02070000
           05  FILLER             PIC X(5)  VALUE '  ...'.              02080000
           EJECT                                                        02090000
      ***************************************************************** 02100000
      *    P R O C E D U R E    D I V I S I O N                       * 02110000
      ***************************************************************** 02120000
                                                                        02130000
       PROCEDURE DIVISION.                                              02140000
                                                                        02150000
                                                                        02160000
      ***************************************************************** 02170000
      *                                                               * 02180000
      *    PARAGRAPH:  P00000-MAINLINE                                * 02190000
      *                                                               * 02200000
      *    FUNCTION :  PROGRAM ENTRY, OPEN FILES, PROCESS.            * 02210000
      *                                                               * 02220000
      *    CALLED BY:  NONE                                           * 02230000
      *                                                               * 02240000
      ***************************************************************** 02250000
                                                                        02260000
       P00000-MAINLINE.                                                 02270000
                                                                        02280000
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATE.                  02290000
           MOVE WS-DATE-MONTH TO WS-RT-MONTH.                           02300000
           MOVE WS-DATE-DAY TO WS-RT-DAY.                               02310000
           MOVE WS-DATE-YEAR TO WS-RT-YEAR.                             02320000
                                                                        02330000
           OPEN INPUT VSAM-PENDORDER                                    02340000
               OUTPUT INVRPT-OUT.                                       02350000
                                                                        02360000
           MOVE SPACES TO WS-USERID.                                    02370000
                                                                        02380000
           MOVE ZEROS             TO WS-LINE-CNT.                       02390000
           MOVE '-'               TO WS-RD-CC.                          02400000
                                                                        02410000
           WRITE INVRPT-OUT-REC   FROM WS-RPT-TITLE.                    02420000
           WRITE INVRPT-OUT-REC   FROM WS-RPT-HEADING.                  02430000
                                                                        02440000
           PERFORM P10000-RPT-LOOP      THRU P10000-EXIT                02450000
               UNTIL PENDORDR-END OR PENDORDR-ERR                       02460000
                     OR END-OF-PROCESS                                  02470000
                                                                        02480000
           CLOSE VSAM-PENDORDER                                         02490000
                 INVRPT-OUT.                                            02500000
                                                                        02510000
           GOBACK.                                                      02520000
                                                                        02530000
       P00000-EXIT.                                                     02540000
           EXIT.                                                        02550000
           EJECT                                                        02560000
      ***************************************************************** 02570000
      *                                                               * 02580000
      *    PARAGRAPH:  P10000-RPT-LOOP                                * 02590000
      *                                                               * 02600000
      *    FUNCTION :  READ PEND ORDER FILE AND WRITE REPORT RECORD   * 02610000
      *      WILL READ ALL PENDING ORDER RECORDS, DISPITE USER        * 02620000
      *      IF SEQUENCE NUMBER IS ZERO, SKIP RECORD                  * 02630000
      *                                                               * 02640000
      *    CALLED BY:  P00000-MAINLINE                                * 02650000
      *                                                               * 02660000
      ***************************************************************** 02670000
                                                                        02680000
       P10000-RPT-LOOP.                                                 02690000
                                                                        02700000
           READ VSAM-PENDORDER INTO PENDING-ORDER-RECORD.               02710000
                                                                        02720000
           IF PENDORDR-END OR PENDORDR-ERR                              02730000
              GO TO P10000-EXIT.                                        02740000
           IF PENDING-ORDER-SEQUENCE  = ZEROS                           02750000
              GO TO P10000-EXIT.                                        02760000
                                                                        02770000
PWB416     MOVE PENDING-ORDER-QUANTITY TO WS-RD-QTY-ORDERED             02780000
                                          WS-QUANTITY-ORDERED.          02790000
PWB416     MOVE PENDING-ORDER-ITEM-KEY TO ITEM-KEY                      02800000
                                          ITEM-SUPPLIER-ITEM-KEY.       02810000
                                                                        02820000
           EXEC SQL SELECT    NAME                                      02830000
                                                                        02840000
                    INTO      :ITEM-NAME                                02850000
                                                                        02860000
                    FROM      ITEM                                      02870000
                                                                        02880000
             WHERE PREFIX             = :ITEM-PREFIX AND                02890000
                   NUMBER             = :ITEM-NUMBER                    02900000
           END-EXEC.                                                    02910000
                                                                        02920000
           IF SQLCODE                  = ZEROS                          02930000
             NEXT SENTENCE                                              02940000
           ELSE                                                         02950000
      *      IF SQLCODE                = +100                           02960000
               MOVE 'Y'                TO WS-END-OF-PROCESS-SW          02970000
               GO TO P10000-EXIT.                                       02980000
                                                                        02990000
           MOVE ITEM-NAME              TO WS-RD-ITEM-NAME               03000000
           MOVE PENDING-ORDER-SUPPLIER-KEY                              03010000
                                       TO SUPPLIER-KEY                  03020000
                                          ITEM-SUPPLIER-SUPPLIER-KEY.   03030000
                                                                        03040000
           EXEC SQL SELECT    NAME                                      03050000
                                                                        03060000
                    INTO      :SUPPLIER-NAME                            03070000
                                                                        03080000
                    FROM      SUPPLIER                                  03090000
                                                                        03100000
             WHERE PREFIX             = :SUPPLIER-PREFIX AND            03110000
                   SUPPLIER_ID        = :SUPPLIER-SUPPLIER-ID           03120000
           END-EXEC.                                                    03130000
                                                                        03140000
           IF SQLCODE                  = ZEROS                          03150000
             NEXT SENTENCE                                              03160000
           ELSE                                                         03170000
      *      IF SQLCODE                = +100                           03180000
               MOVE 'Y'                TO WS-END-OF-PROCESS-SW          03190000
               GO TO P10000-EXIT.                                       03200000
                                                                        03210000
           MOVE SUPPLIER-NAME          TO WS-RD-SUPPLIER.               03220000
                                                                        03230000
           EXEC SQL SELECT    QUANTITY_ON_HAND                          03240000
                                                                        03250000
                    INTO      :ITEM-SUPPLIER-QUANTITY-ON-HAND           03260000
                                                                        03270000
                    FROM      ITEM_SUPPLIER                             03280000
                                                                        03290000
             WHERE ITEM_PREFIX        = :ITEM-SUPPLIER-ITEM-PREFIX AND  03300000
                   ITEM_NUMBER        = :ITEM-SUPPLIER-ITEM-NUMBER AND  03310000
                   SUPPLIER_PREFIX    = :ITEM-SUPPLIER-SUPPLIER-PREFIX  03320000
               AND SUPPLIER_ID        = :ITEM-SUPPLIER-SUPPLIER-ID      03330000
           END-EXEC.                                                    03340000
                                                                        03350000
           IF SQLCODE                  = ZEROS                          03360000
             NEXT SENTENCE                                              03370000
           ELSE                                                         03380000
      *      IF SQLCODE                = +100                           03390000
               MOVE 'Y'                TO WS-END-OF-PROCESS-SW          03400000
               GO TO P10000-EXIT.                                       03410000
                                                                        03420000
           MOVE ITEM-SUPPLIER-QUANTITY-ON-HAND                          03430000
                                       TO WS-RD-QTY-AVAIL               03440000
                                          WS-QUANTITY-AVAIL.            03450000
                                                                        03460000
           SUBTRACT WS-QUANTITY-ORDERED                                 03470000
                                       FROM WS-QUANTITY-AVAIL           03480000
             GIVING WS-QUANTITY-REMAINING.                              03490000
                                                                        03500000
           IF WS-QUANTITY-REMAINING    > ZERO                           03510000
             MOVE 'Y'                  TO WS-RD-AVAIL-FLAG              03520000
           ELSE                                                         03530000
             MOVE 'N'                  TO WS-RD-AVAIL-FLAG.             03540000
                                                                        03550000
           WRITE INVRPT-OUT-REC        FROM WS-RPT-DETAIL.              03560000
                                                                        03570000
           ADD +1                      TO WS-LINE-CNT.                  03580000
           MOVE ' '                    TO WS-RD-CC.                     03590000
           IF WS-LINE-CNT              > +60                            03600000
             MOVE ZEROS                TO WS-LINE-CNT                   03610000
PWB416       WRITE INVRPT-OUT-REC      FROM WS-RPT-TITLE                03620000
PWB416       WRITE INVRPT-OUT-REC      FROM WS-RPT-HEADING              03630000
             MOVE '-'                  TO WS-RD-CC.                     03640000
                                                                        03650000
       P10000-EXIT.                                                     03660000
           EXIT.                                                        03670000
           EJECT                                                        03680000