       IDENTIFICATION DIVISION.                                         00010002
       PROGRAM-ID. PDAB04A.                                             00020002
      *                                                                 00030002
      ***************************************************************** 00040002
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       * 00050002
      *                       COMPUWARE CORPORATION                   * 00060002
      *                                                               * 00070002
      * PROGRAM :   PDAB04A                                           * 00080002
      *                                                               * 00090002
      * FUNCTION:   PROGRAM PDAB04A IS A BATCH PROGRAM THAT WILL READ * 00100002
      *             THE PENDING ORDER FILE AND REPORT ON AVAILABLE    * 00110002
      *             INVENTORY BUT IT FAILS WITH A -303 DB2 SQL CODE.  * 00120002
      *                                                               * 00130002
      * FILES   :   PENDING ORDER FILE    -  VSAM KSDS     (READ)     * 00140002
      *             ITEM TABLE            -  DB2           (READ)     * 00150002
      *             SUPPLIER TABLE        -  DB2           (READ)     * 00160002
      *             ITEM SUPPLIER TABLE   -  DB2           (READ)     * 00170002
      *             REPORT                -  PRINT         (OUTPUT)   * 00180002
      *                                                               * 00190002
      ***************************************************************** 00200002
      *             PROGRAM CHANGE LOG                                * 00210002
      *             -------------------                               * 00220002
      *                                                               * 00230002
      *  DATE       UPDATED BY            CHANGE DESCRIPTION          * 00240002
      *  --------   --------------------  --------------------------  * 00250002
      *  XX/XX/XX   XXX                   XXXXXXXXXXXXXXXXXXXXXXXXXX  * 00260002
      ***************************************************************** 00270002
           EJECT                                                        00280002
       ENVIRONMENT DIVISION.                                            00290002
                                                                        00300002
       INPUT-OUTPUT SECTION.                                            00310002
                                                                        00320002
       FILE-CONTROL.                                                    00330002
                                                                        00340002
           SELECT INVRPT-OUT           ASSIGN TO INVRPTO.               00350002
                                                                        00360002
           SELECT VSAM-PENDORDER       ASSIGN TO VPENDORD               00370002
                                       ORGANIZATION IS INDEXED          00380002
                                       ACCESS IS SEQUENTIAL             00390002
                                       RECORD KEY IS PEND-ORDER-KEY     00400002
                                       FILE STATUS IS WS-PENDORD-STATUS.00410002
           EJECT                                                        00420002
       DATA DIVISION.                                                   00430002
                                                                        00440002
       FILE SECTION.                                                    00450002
                                                                        00460002
       FD INVRPT-OUT                                                    00470002
           LABEL RECORDS ARE STANDARD                                   00480002
           RECORDING MODE IS F                                          00490002
           RECORD CONTAINS 133 CHARACTERS.                              00500002
                                                                        00510002
       01  INVRPT-OUT-REC              PIC X(133).                      00520002
                                                                        00530002
           EJECT                                                        00540002
       FD  VSAM-PENDORDER                                               00550002
           RECORD CONTAINS 89  CHARACTERS.                              00560002
       01  PENDING-ORDER-REC.                                           00570002
           05  PEND-ORDER-KEY.                                          00580002
               10 PEND-ORDER-PREFIX    PIC 9(05).                       00590002
               10 PEND-ORDER-SEQUENCE                                   00600002
                                       PIC 9(05).                       00610002
           05  FILLER                  PIC X(79).                       00620002
                                                                        00630002
           EJECT                                                        00640002
       WORKING-STORAGE SECTION.                                         00650002
                                                                        00660002
                                                                        00670002
      ***************************************************************** 00680002
      *    SWITCHES                                                   * 00690002
      ***************************************************************** 00700002
                                                                        00710002
       01  WS-SWITCHES.                                                 00720002
           05 WS-END-OF-PROCESS-SW     PIC X VALUE SPACES.              00730002
              88 END-OF-PROCESS              VALUE 'Y'.                 00740002
                                                                        00750002
      ***************************************************************** 00760002
      *    MISCELLANEOUS WORK FIELDS                                  * 00770002
      ***************************************************************** 00780002
                                                                        00790002
       01  WS-MISC-FIELDS.                                              00800002
           05  WS-RETURN-CODE          PIC 9(4)  VALUE ZEROES   COMP.   00810002
           05  WS-PENDORD-STATUS       PIC XX    VALUE SPACES.          00820002
               88  PENDORDR-OK                   VALUE '  ' '00'.       00830002
               88  PENDORDR-NOTFOUND             VALUE '23'.            00840002
               88  PENDORDR-EMPTY                VALUE '47'.            00850002
               88  PENDORDR-END                  VALUE '10'.            00860002
               88  PENDORDR-ERR                  VALUE '23' '47'.       00870002
           05  WS-USERID               PIC X(5)  VALUE SPACES.          00880002
           05  WS-DATE.                                                 00890002
               10  WS-DATE-YEAR        PIC X(4)  VALUE SPACES.          00900002
               10  WS-DATE-MONTH       PIC XX    VALUE SPACES.          00910002
               10  WS-DATE-DAY         PIC XX    VALUE SPACES.          00920002
           05  WS-QUANTITY-ORDERED     PIC S9(9) COMP-3 VALUE ZEROS.    00930002
           05  WS-QUANTITY-AVAIL       PIC S9(9) COMP-3 VALUE ZEROS.    00940002
           05  WS-QUANTITY-REMAINING   PIC S9(9) COMP-3 VALUE ZEROS.    00950002
           05  WS-LINE-CNT             PIC S99   VALUE ZEROES.          00960002
           05  WS-ITEM-NUMBER          PIC S9(8) COMP-3 VALUE ZEROES.   00961002
           EJECT                                                        00962002
           COPY VPENDORD.                                               00963002
           EJECT                                                        00964002
      ***************************************************************** 00965002
      *    INVENTORY REPORT                                           * 00966002
      ***************************************************************** 00967002
                                                                        00968002
       01  WS-RPT-TITLE.                                                00969002
           05  FILLER             PIC X     VALUE '1'.                  00970002
           05  FILLER             PIC X(50) VALUE SPACES.               00980002
           05  FILLER             PIC X(21) VALUE                       00990002
               'PENDING ORDERS AS OF '.                                 01000002
           05  WS-RT-MONTH        PIC XX.                               01010002
           05  FILLER             PIC X VALUE '/'.                      01020002
           05  WS-RT-DAY          PIC XX.                               01030002
           05  FILLER             PIC X VALUE '/'.                      01040002
           05  WS-RT-YEAR         PIC X(4).                             01050002
           05  FILLER             PIC X(51).                            01060002
                                                                        01070002
       01  WS-RPT-HEADING.                                              01080002
           05  FILLER             PIC X     VALUE '-'.                  01090002
           05  FILLER             PIC X(20) VALUE SPACES.               01100002
           05  FILLER             PIC X(16) VALUE                       01110002
               'ITEM DESCRIPTION'.                                      01120002
           05  FILLER             PIC X(19) VALUE SPACES.               01130002
           05  FILLER             PIC X(11) VALUE                       01140002
               'QTY ORDERED'.                                           01150002
           05  FILLER             PIC X(11) VALUE SPACES.               01160002
           05  FILLER             PIC X(13) VALUE                       01170002
               'SUPPLIER NAME'.                                         01180002
           05  FILLER             PIC X(12) VALUE SPACES.               01190002
           05  FILLER             PIC X(11) VALUE                       01200002
               'QTY ON HAND'.                                           01210002
           05  FILLER             PIC X(2)  VALUE SPACES.               01220002
           05  FILLER             PIC X(14) VALUE                       01230002
               'AVAILABLE FLAG'.                                        01240002
           05  FILLER             PIC X(3)  VALUE SPACES.               01250002
           EJECT                                                        01260002
                                                                        01270002
       01  WS-RPT-DETAIL.                                               01280002
           05  WS-RD-CC           PIC X     VALUE ' '.                  01290002
           05  FILLER             PIC X(3).                             01300002
           05  WS-RD-ITEM-NAME    PIC X(50).                            01310002
           05  FILLER             PIC XX.                               01320002
           05  WS-RD-QTY-ORDERED  PIC ZZZ,ZZZ,ZZ9.                      01330002
           05  FILLER             PIC XX.                               01340002
           05  WS-RD-SUPPLIER     PIC X(32).                            01350002
           05  FILLER             PIC XX.                               01360002
           05  WS-RD-QTY-AVAIL    PIC ZZZ,ZZZ,ZZ9.                      01370002
           05  FILLER             PIC X(8).                             01380002
           05  WS-RD-AVAIL-FLAG   PIC X.                                01390002
           05  FILLER             PIC X(10).                            01400002
           EJECT                                                        01410002
      ***************************************************************** 01420002
      *    DB2  DEFINITIONS                                           * 01430002
      ***************************************************************** 01440002
                                                                        01450002
      ***************************************************************** 01460002
      *         SQL COMMUNICATIONS AREA                               * 01470002
      ***************************************************************** 01480002
                                                                        01490002
           EXEC SQL                                                     01500002
              INCLUDE SQLCA                                             01510002
           END-EXEC.                                                    01520002
           EJECT                                                        01530002
           EXEC SQL                                                     01540002
              INCLUDE DITEM                                             01550002
           END-EXEC.                                                    01560002
           EJECT                                                        01570002
           EXEC SQL                                                     01580002
              INCLUDE DSUPPLR                                           01590002
           END-EXEC.                                                    01600002
           EJECT                                                        01610002
           EXEC SQL                                                     01620002
              INCLUDE DITMSUP                                           01630002
           END-EXEC.                                                    01640002
           EJECT                                                        01650002
      ***************************************************************** 01660002
      *    GENERAL ERROR PROCESSING WORK AREAS (CICS, IMS-DLI, DB2)   * 01670002
      ***************************************************************** 01680002
                                                                        01690002
           COPY PDAERRWS.                                               01700002
                                                                        01710002
       01  WS-PDA-BATCH-ERROR-01.                                       01720002
           05  FILLER             PIC X     VALUE SPACES.               01730002
           05  FILLER             PIC X(7)  VALUE 'ERROR: '.            01740002
           05  FILLER             PIC X(10) VALUE 'PROGRAM = '.         01750002
           05  WPBE-PROGRAM-ID    PIC X(8)  VALUE 'PDAB04A'.            01760002
           05  FILLER             PIC X(14) VALUE ', PARAGRAPH = '.     01770002
           05  WPBE-PARAGRAPH     PIC X(6)  VALUE SPACES.               01780002
                                                                        01790002
       01  WS-PDA-BATCH-ERROR-02.                                       01800002
           05  FILLER             PIC X(8)  VALUE SPACES.               01810002
           05  WPBE-MESSAGE       PIC X(39) VALUE SPACES.               01820002
           05  FILLER             PIC X(16) VALUE 'RECORD NUMBER ='.    01830002
           05  WPBE-RECORD-NUMBER PIC X(7)  VALUE ZEROES.               01840002
           05  FILLER             PIC X(8)  VALUE SPACES.               01850002
                                                                        01860002
       01  WS-PDA-BATCH-ERROR-03.                                       01870002
           05  FILLER             PIC X(8)  VALUE SPACES.               01880002
           05  FILLER             PIC X(20) VALUE 'RECORD IS DISPLAYED'.01890002
           05  FILLER             PIC X(5)  VALUE 'BELOW'.              01900002
                                                                        01910002
       01  WS-PDA-BATCH-ERROR-04.                                       01920002
           05  FILLER             PIC X(10) VALUE '----+----1'.         01930002
           05  FILLER             PIC X(10) VALUE '----+----2'.         01940002
           05  FILLER             PIC X(10) VALUE '----+----3'.         01950002
           05  FILLER             PIC X(10) VALUE '----+----4'.         01960002
           05  FILLER             PIC X(10) VALUE '----+----5'.         01970002
           05  FILLER             PIC X(10) VALUE '----+----6'.         01980002
           05  FILLER             PIC X(10) VALUE '----+----7'.         01990002
           05  FILLER             PIC X(5)  VALUE '  ...'.              02000002
           EJECT                                                        02010002
      ***************************************************************** 02020002
      *    P R O C E D U R E    D I V I S I O N                       * 02030002
      ***************************************************************** 02040002
                                                                        02050002
       PROCEDURE DIVISION.                                              02060002
                                                                        02070002
                                                                        02080002
      ***************************************************************** 02090002
      *                                                               * 02100002
      *    PARAGRAPH:  P00000-MAINLINE                                * 02110002
      *                                                               * 02120002
      *    FUNCTION :  PROGRAM ENTRY, OPEN FILES, PROCESS.            * 02130002
      *                                                               * 02140002
      *    CALLED BY:  NONE                                           * 02150002
      *                                                               * 02160002
      ***************************************************************** 02170002
                                                                        02180002
       P00000-MAINLINE.                                                 02190002
                                                                        02200002
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATE.                  02210002
           MOVE WS-DATE-MONTH TO WS-RT-MONTH.                           02220002
           MOVE WS-DATE-DAY TO WS-RT-DAY.                               02230002
           MOVE WS-DATE-YEAR TO WS-RT-YEAR.                             02240002
                                                                        02250002
           OPEN INPUT VSAM-PENDORDER                                    02260002
               OUTPUT INVRPT-OUT.                                       02270002
                                                                        02280002
           MOVE SPACES TO WS-USERID.                                    02290002
                                                                        02300002
           MOVE ZEROS             TO WS-LINE-CNT.                       02310002
           MOVE '-'               TO WS-RD-CC.                          02320002
                                                                        02330002
           WRITE INVRPT-OUT-REC   FROM WS-RPT-TITLE.                    02340002
           WRITE INVRPT-OUT-REC   FROM WS-RPT-HEADING.                  02350002
                                                                        02360002
           PERFORM P10000-RPT-LOOP      THRU P10000-EXIT                02370002
               UNTIL PENDORDR-END OR PENDORDR-ERR                       02380002
                     OR END-OF-PROCESS                                  02390002
                                                                        02400002
           CLOSE VSAM-PENDORDER                                         02410002
                 INVRPT-OUT.                                            02420002
                                                                        02430002
           GOBACK.                                                      02440002
                                                                        02450002
       P00000-EXIT.                                                     02460002
           EXIT.                                                        02470002
           EJECT                                                        02480002
      ***************************************************************** 02490002
      *                                                               * 02500002
      *    PARAGRAPH:  P10000-RPT-LOOP                                * 02510002
      *                                                               * 02520002
      *    FUNCTION :  READ PEND ORDER FILE AND WRITE REPORT RECORD   * 02530002
      *      WILL READ ALL PENDING ORDER RECORDS, DISPITE USER        * 02540002
      *      IF SEQUENCE NUMBER IS ZERO, SKIP RECORD                  * 02550002
      *                                                               * 02560002
      *    CALLED BY:  P00000-MAINLINE                                * 02570002
      *                                                               * 02580002
      ***************************************************************** 02590002
                                                                        02600002
       P10000-RPT-LOOP.                                                 02610002
                                                                        02620002
           READ VSAM-PENDORDER INTO PENDING-ORDER-RECORD.               02630002
                                                                        02640002
           IF PENDORDR-END OR PENDORDR-ERR                              02650002
              GO TO P10000-EXIT.                                        02660002
           IF PENDING-ORDER-SEQUENCE  = ZEROS                           02670002
              GO TO P10000-EXIT.                                        02680002
                                                                        02690002
PWB416     MOVE PENDING-ORDER-QUANTITY TO WS-RD-QTY-ORDERED             02700002
                                          WS-QUANTITY-ORDERED.          02710002
PWB416     MOVE PENDING-ORDER-ITEM-KEY TO ITEM-KEY                      02720002
                                          ITEM-SUPPLIER-ITEM-KEY.       02730002
                                                                        02740002
           EXEC SQL SELECT    NAME, NUMBER                              02750002
                                                                        02760002
                    INTO      :ITEM-NAME,                               02770002
                              :WS-ITEM-NUMBER                           02780002
                                                                        02790002
                    FROM      ITEM                                      02800002
                                                                        02810002
             WHERE PREFIX             = :ITEM-PREFIX AND                02820002
                   NUMBER             = :ITEM-NUMBER                    02830002
           END-EXEC.                                                    02840002
                                                                        02850002
           IF SQLCODE                  = ZEROS                          02860002
             NEXT SENTENCE                                              02870002
           ELSE                                                         02880002
             MOVE 'Y'                TO WS-END-OF-PROCESS-SW            02890002
             IF SQLCODE                = +100                           02900002
               GO TO P10000-EXIT                                        02910002
             ELSE                                                       02920002
               CALL 'SNAPAID'                                           02930002
               GO TO P10000-EXIT.                                       02940002
                                                                        02950002
           MOVE ITEM-NAME              TO WS-RD-ITEM-NAME               02960002
           MOVE PENDING-ORDER-SUPPLIER-KEY                              02970002
                                       TO SUPPLIER-KEY                  02980002
                                          ITEM-SUPPLIER-SUPPLIER-KEY.   02990002
                                                                        03000002
           EXEC SQL SELECT    NAME                                      03010002
                                                                        03020002
                    INTO      :SUPPLIER-NAME                            03030002
                                                                        03040002
                    FROM      SUPPLIER                                  03050002
                                                                        03060002
             WHERE PREFIX             = :SUPPLIER-PREFIX AND            03070002
                   SUPPLIER_ID        = :SUPPLIER-SUPPLIER-ID           03080002
           END-EXEC.                                                    03090002
                                                                        03100002
           IF SQLCODE                  = ZEROS                          03110002
             NEXT SENTENCE                                              03120002
           ELSE                                                         03130002
      *      IF SQLCODE                = +100                           03140002
               MOVE 'Y'                TO WS-END-OF-PROCESS-SW          03150002
               GO TO P10000-EXIT.                                       03160002
                                                                        03170002
           MOVE SUPPLIER-NAME          TO WS-RD-SUPPLIER.               03180002
                                                                        03190002
           EXEC SQL SELECT    QUANTITY_ON_HAND                          03200002
                                                                        03210002
                    INTO      :ITEM-SUPPLIER-QUANTITY-ON-HAND           03220002
                                                                        03230002
                    FROM      ITEM_SUPPLIER                             03240002
                                                                        03250002
             WHERE ITEM_PREFIX        = :ITEM-SUPPLIER-ITEM-PREFIX AND  03260002
                   ITEM_NUMBER        = :ITEM-SUPPLIER-ITEM-NUMBER AND  03270002
                   SUPPLIER_PREFIX    = :ITEM-SUPPLIER-SUPPLIER-PREFIX  03280002
               AND SUPPLIER_ID        = :ITEM-SUPPLIER-SUPPLIER-ID      03290002
           END-EXEC.                                                    03300002
                                                                        03310002
           IF SQLCODE                  = ZEROS                          03320002
             NEXT SENTENCE                                              03330002
           ELSE                                                         03340002
      *      IF SQLCODE                = +100                           03350002
               MOVE 'Y'                TO WS-END-OF-PROCESS-SW          03360002
               GO TO P10000-EXIT.                                       03370002
                                                                        03380002
           MOVE ITEM-SUPPLIER-QUANTITY-ON-HAND                          03390002
                                       TO WS-RD-QTY-AVAIL               03400002
                                          WS-QUANTITY-AVAIL.            03410002
                                                                        03420002
           SUBTRACT WS-QUANTITY-ORDERED                                 03430002
                                       FROM WS-QUANTITY-AVAIL           03440002
             GIVING WS-QUANTITY-REMAINING.                              03450002
                                                                        03460002
           IF WS-QUANTITY-REMAINING    > ZERO                           03470002
             MOVE 'Y'                  TO WS-RD-AVAIL-FLAG              03480002
           ELSE                                                         03490002
             MOVE 'N'                  TO WS-RD-AVAIL-FLAG.             03500002
                                                                        03510002
           WRITE INVRPT-OUT-REC        FROM WS-RPT-DETAIL.              03520002
                                                                        03530002
           ADD +1                      TO WS-LINE-CNT.                  03540002
           MOVE ' '                    TO WS-RD-CC.                     03550002
           IF WS-LINE-CNT              > +60                            03560002
             MOVE ZEROS                TO WS-LINE-CNT                   03570002
PWB416       WRITE INVRPT-OUT-REC      FROM WS-RPT-TITLE                03580002
PWB416       WRITE INVRPT-OUT-REC      FROM WS-RPT-HEADING              03590002
             MOVE '-'                  TO WS-RD-CC.                     03600002
                                                                        03610002
       P10000-EXIT.                                                     03620002
           EXIT.                                                        03630002
           EJECT                                                        03640002