       ID DIVISION.                                                     00010019
       PROGRAM-ID.  PDASP1.                                             00020019
                                                                        00021019
      ***************************************************************** 00022019
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       * 00023019
      *                       COMPUWARE CORPORATION                   * 00024019
      *                                                               * 00025019
      * PROGRAM :   PDASP1                                            * 00026019
      * TRANS   :   N/A                                               * 00027019
      * MAPSET  :   N/A                                               * 00028019
      *                                                               * 00029019
      * FUNCTION:   PROGRAM PDASP1 IS PART OF THE PRODUCT             * 00029119
      *             DEMONSTRATION APPLICATION. IT WILL CALCULATE THE  * 00029219
      *             PENDING ORDER TOTAL.                              * 00029319
      *                                                               * 00029419
      * FILES   :   PENDING ORDER FILE (VSAM INPUT)                   * 00029519
      *                                                               * 00029619
      *                                                               * 00029719
      * TRANSACTIONS GENERATED: N/A                                   * 00029819
      *                                                               * 00029919
      * PFKEYS  :   N/A                                               * 00030019
      *                                                               * 00030119
      *                                                               * 00030219
      ***************************************************************** 00030319
      *             PROGRAM CHANGE LOG                                * 00030419
      *             -------------------                               * 00030519
      *                                                               * 00030619
      *  DATE       UPDATED BY            CHANGE DESCRIPTION          * 00030719
      *  --------   --------------------  --------------------------  * 00030819
      *  06/12/01    JS                   ON CALL TO SNAPAID, ADDED   * 00030919
      *                                   PARMS TO DYN ALLOC ABENDAID * 00031019
      *                                   DD STMT                     * 00031119
      *                                                               * 00031219
      ***************************************************************** 00031319
       ENVIRONMENT DIVISION.                                            00031419
       INPUT-OUTPUT SECTION.                                            00031519
       FILE-CONTROL.                                                    00031619
           SELECT PEND-ORDER ASSIGN TO VPENDORD                         00031719
             ORGANIZATION IS INDEXED                                    00031819
             ACCESS IS DYNAMIC                                          00031919
             RECORD KEY IS PEND-ORDER-KEY                               00032019
             FILE STATUS IS PEND-STATUS.                                00033019
       DATA DIVISION.                                                   00034019
       FILE SECTION.                                                    00035019
       FD  PEND-ORDER                                                   00036019
           RECORD CONTAINS 89 CHARACTERS.                               00037019
       01  PEND-ORDER-RECORD.                                           00038019
           05  PEND-ORDER-KEY          PIC X(10).                       00039019
           05  FILLER                  PIC X(79).                       00040019
       WORKING-STORAGE SECTION.                                         00050019
       77  FILLER               PIC X(12)  VALUE 'PDASP1  WS:'.         00060019
       77  PEND-STATUS          PIC XX.                                 00070019
       01  SWITCHES.                                                    00080019
           05 WS-END-OF-PROCESS-SW     PIC X VALUE 'N'.                 00090019
              88 END-OF-PROCESS              VALUE 'Y'.                 00100019
              88 NOT-END-OF-PROCESS          VALUE 'N'.                 00110019
           05 WS-ERROR-SW              PIC X VALUE 'N'.                 00120019
              88 ERROR-FOUND                 VALUE 'Y'.                 00130019
              88 NO-ERROR-FOUND              VALUE 'N'.                 00140019
           05 WS-ACTIVE-SCENARIO       PIC X VALUE '0'.                 00150019
              88 ACTIVE-SCENARIO             VALUE '8'.                 00160019
                                                                        00170019
       01  WS-EXTENDED-PRICE           PIC S9(13)V99 COMP-3 VALUE 0.    00180019
       01  WS-TOTAL-COST               PIC S9(15)V99 COMP-3 VALUE 0.    00190019
       01  WS-STATUS                   PIC 9(04).                       00200019
       01  WS-UNIT-PRICE               PIC X(9).                        00210019
           EJECT                                                        00220019
       01  PARM-INTERFACE.                                              00230019
           03  PARM-IDENT              PIC X(6)  VALUE 'SNPRML'.        00240019
           03  PARM-ACTION             PIC 9     VALUE ZERO.            00250019
               88  DISPLAY-ALL-SECTIONS          VALUE 0.               00260019
               88  DISPLAY-SECTION               VALUE 1.               00270019
               88  SUPPRESS-SECTION              VALUE 2.               00280019
      *            0 FULL SNAPAID REPORT (NO CUSTOMIZATION)             00290019
      *            1 DISPLAY SELECTED SECTIONS                          00300019
      *            2 SUPPRESS SELECTED SECTIONS                         00310019
           03  PARM-SECTION-SELECTION.                                  00320019
               05  NSI-SECTION         PIC 9     VALUE ZERO.            00330019
                   88 DO-NOT-APPLY-PARM-ACTION   VALUE 0.               00340019
                   88 APPLY-PARM-ACTION          VALUE 1.               00350019
      *               0 DO NOT APPLY PARM-ACTION TO THIS SECTION        00360019
      *               1 APPLY PARM-ACTION TO THIS SECTION               00370019
               05  REGISTERS-PSW       PIC 9     VALUE ZERO.            00380019
               05  TRACE-SUMMARY       PIC 9     VALUE ZERO.            00390019
               05  PROGRAM-STORAGE     PIC 9     VALUE ZERO.            00400019
               05  FILES-SECTION       PIC 9     VALUE ZERO.            00410019
               05  IMS-SECTION         PIC 9     VALUE ZERO.            00420019
               05  IDMS-SECTION        PIC 9     VALUE ZERO.            00430019
               05  DB2-SECTION         PIC 9     VALUE ZERO.            00440019
               05  SORT-SECTION        PIC 9     VALUE ZERO.            00450019
               05  FILLER              PIC 9     VALUE ZERO.            00460019
               05  PARML-VERSION       PIC 9     VALUE 1.               00470019
      *            DYNAMIC ALLOCATION INFORMATION NOT PRESENT IF BLANK  00480019
      *          1 DYNAMIC ALLOCATION INFORMATION PRESENT               00490019
           03  PARM-COMMENT            PIC X(10) VALUE SPACES.          00500019
           03  FILLER                  REDEFINES PARM-COMMENT.          00510019
               05  PARM-SQL-COMM       PIC X(5).                        00520019
               05  PARM-SQL-CODE       PIC -9(4).                       00530019
           03  DYNALC-REQ              PIC X     VALUE 'D'.             00540019
               88  DONT-ALLOCATE-ABENDAID        VALUE SPACE.           00550019
               88  ALLOCATE-ABENDAID             VALUE 'D'.             00560019
               88  ALLOC-AND-FREE-ON-CLOSE       VALUE 'F'.             00570019
      *   THE ABENDAID DD WILL NOT BE DYNAMICALLY ALLOCATED IF BLANK    00580019
      *    D DYNAMICALLY ALLOCATE THE ABENDAID DD                       00590019
      *    F DYNAMICALLY ALLOCATE THE ABENDAID DD AND FREE=CLOSE        00600019
               03  OUTPUT-CLASS        PIC X     VALUE '*'.             00610019
               03  HOLD-OUTPUT         PIC X     VALUE 'Y'.             00620019
                   88  DONT-HOLD-DEF             VALUE SPACE.           00630019
                   88  HOLD                      VALUE 'Y'.             00640019
                   88  DONT-HOLD                 VALUE 'N'.             00650019
               03  FILLER              PIC X     VALUE SPACE.           00660019
               03  DESTINATION-PARM    PIC X(8)  VALUE 'LOCAL'.         00670019
               03  USERID              PIC X(8)  VALUE SPACE.           00680019
               03  OUTPUT-WRITER       PIC X(8)  VALUE SPACE.           00690019
               03  USER-FORM           PIC X(4)  VALUE SPACE.           00700019
           EJECT                                                        00710019
           EXEC SQL INCLUDE SQLCA END-EXEC.                             00720019
                                                                        00730019
      *    EXEC SQL INCLUDE DITMSUP END-EXEC.                           00740019
      ******************************************************************00750019
      * DCLGEN TABLE(ITEM_SUPPLIER)                                    *00760019
      *        LIBRARY(PDADEMO.COBOL.COPYLIB(DITMSUP))                 *00770019
      *        ACTION(REPLACE)                                         *00780019
      *        LANGUAGE(COBOL)                                         *00790019
      *        STRUCTURE(ITEM_SUPPLIER)                                *00800019
      *        QUOTE                                                   *00810019
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *00820019
      ******************************************************************00830019
           EXEC SQL DECLARE ITEM_SUPPLIER TABLE                         00840019
           ( ITEM_PREFIX                    CHAR(5) NOT NULL,           00850019
             ITEM_NUMBER                    CHAR(32) NOT NULL,          00860019
             SUPPLIER_PREFIX                CHAR(5) NOT NULL,           00870019
             SUPPLIER_ID                    CHAR(32) NOT NULL,          00880019
             QUANTITY_ON_HAND               INTEGER NOT NULL,           00890019
             UNIT_PRICE                     DECIMAL(10, 2) NOT NULL     00900019
           ) END-EXEC.                                                  00910019
      ******************************************************************00920019
      * COBOL DECLARATION FOR TABLE ITEM_SUPPLIER                      *00930019
      ******************************************************************00940019
       01  ITEM-SUPPLIER.                                               00950019
           05  ITEM-SUPPLIER-ITEM-KEY.                                  00960019
               10 ITEM-SUPPLIER-ITEM-PREFIX                             00970019
                                       PIC X(5).                        00980019
               10 ITEM-SUPPLIER-ITEM-NUMBER                             00990019
                                       PIC X(32).                       01000019
           05  ITEM-SUPPLIER-SUPPLIER-KEY.                              01010019
               10 ITEM-SUPPLIER-SUPPLIER-PREFIX                         01020019
                                       PIC X(5).                        01030019
               10 ITEM-SUPPLIER-SUPPLIER-ID                             01040019
                                       PIC X(32).                       01050019
           05  ITEM-SUPPLIER-QUANTITY-ON-HAND                           01060019
                                       PIC S9(9)      USAGE COMP.       01070019
           05  ITEM-SUPPLIER-UNIT-PRICE                                 01080019
                                       PIC S9(8)V9(2) USAGE COMP-3.     01090019
      ******************************************************************01100019
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *01110019
      ******************************************************************01120019
                                                                        01130019
                                                                        01140019
      *COPY VPENDORD.                                                   01150019
      ******************************************************************01160019
      * PENDING ORDER RECORD  -- VSAM KSDS                             *01170019
      ******************************************************************01180019
       01  PENDING-ORDER-RECORD.                                        01190019
           05  PENDING-ORDER-KEY.                                       01200019
               10 PENDING-ORDER-PREFIX PIC 9(05).                       01210019
               10 PENDING-ORDER-SEQUENCE                                01220019
                                       PIC 9(05).                       01230019
           05 PENDING-ORDER-QUANTITY   PIC 9(09)       COMP-3.          01240019
           05 PENDING-ORDER-ITEM-KEY.                                   01250019
              10 PENDING-ORDER-ITEM-PREFIX                              01260019
                                       PIC 9(05).                       01270019
              10 PENDING-ORDER-ITEM-NUMBER                              01280019
                                       PIC X(32).                       01290019
           05 PENDING-ORDER-SUPPLIER-KEY.                               01300019
              10 PENDING-ORDER-SUPPLIER-PREFIX                          01310019
                                       PIC 9(05).                       01320019
              10 PENDING-ORDER-SUPPLIER-ID                              01330019
                                       PIC X(32).                       01340019
                                                                        01350019
                                                                        01360019
       LINKAGE SECTION.                                                 01370019
       01  LS-USERID-PREFIX        PIC 9(05).                           01380019
       01  LS-TOTAL-COST           PIC S9(15)V99 COMP-3.                01390019
       01  LS-STATUS               PIC X(04).                           01400019
                                                                        01410019
       PROCEDURE DIVISION USING LS-USERID-PREFIX LS-TOTAL-COST          01420019
                                 LS-STATUS.                             01430019
                                                                        01440019
      *    EXEC SQL WHENEVER SQLERROR GO TO 9999-SQLERROR END-EXEC.     01450019
                                                                        01460019
           IF LS-STATUS            =  '0008'                            01470019
               MOVE '8'            TO WS-ACTIVE-SCENARIO.               01480019
                                                                        01490019
           MOVE LS-USERID-PREFIX   TO PENDING-ORDER-PREFIX.             01500019
           MOVE ZEROS              TO LS-TOTAL-COST.                    01510019
           MOVE +1                 TO PENDING-ORDER-SEQUENCE.           01520019
                                                                        01530019
           PERFORM 0100-START-BROWSE THRU 0100-START-BROWSE-EXIT.       01540019
                                                                        01550019
           IF NO-ERROR-FOUND                                            01560019
             PERFORM 0200-ACCUM-TOTAL-COST                              01570019
                THRU 0200-ACCUM-TOTAL-COST-EXIT                         01580019
                 UNTIL END-OF-PROCESS.                                  01590019
                                                                        01600019
           IF NO-ERROR-FOUND                                            01610019
             MOVE WS-TOTAL-COST    TO LS-TOTAL-COST                     01620019
             MOVE ZEROS            TO LS-STATUS                         01630019
           ELSE                                                         01640019
             MOVE WS-STATUS        TO LS-STATUS.                        01650019
                                                                        01660019
           GOBACK.                                                      01670019
                                                                        01680019
       0100-START-BROWSE.                                               01690019
                                                                        01700019
           OPEN INPUT PEND-ORDER.                                       01710019
           MOVE PENDING-ORDER-KEY      TO PEND-ORDER-KEY.               01720019
           START PEND-ORDER KEY        >= PEND-ORDER-KEY.               01730019
           IF PEND-STATUS              = ZEROS                          01740019
             NEXT SENTENCE                                              01750019
           ELSE                                                         01760019
             MOVE 'Y'                  TO WS-ERROR-SW                   01770019
             MOVE PEND-STATUS          TO WS-STATUS.                    01780019
                                                                        01790019
       0100-START-BROWSE-EXIT.                                          01800019
           EXIT.                                                        01810019
                                                                        01820019
       0200-ACCUM-TOTAL-COST.                                           01830019
                                                                        01840019
           READ PEND-ORDER NEXT INTO PENDING-ORDER-RECORD.              01850019
                                                                        01860019
           IF PEND-STATUS              = ZEROS                          01870019
             NEXT SENTENCE                                              01880019
           ELSE                                                         01890019
             IF PEND-STATUS            = '10'                           01900019
               MOVE 'Y'                TO WS-END-OF-PROCESS-SW          01910019
               GO TO 0200-ACCUM-TOTAL-COST-EXIT                         01920019
             ELSE                                                       01930019
               MOVE 'Y'                TO WS-END-OF-PROCESS-SW          01940019
                                          WS-ERROR-SW                   01950019
               MOVE PEND-STATUS        TO WS-STATUS                     01960019
               GO TO 0200-ACCUM-TOTAL-COST-EXIT.                        01970019
                                                                        01980019
           IF PENDING-ORDER-PREFIX NOT = LS-USERID-PREFIX               01990019
             MOVE 'Y'                  TO WS-END-OF-PROCESS-SW          02000019
             GO TO 0200-ACCUM-TOTAL-COST-EXIT.                          02010019
                                                                        02020019
           MOVE PENDING-ORDER-ITEM-KEY TO ITEM-SUPPLIER-ITEM-KEY.       02030019
           MOVE PENDING-ORDER-SUPPLIER-KEY                              02040019
                                       TO ITEM-SUPPLIER-SUPPLIER-KEY.   02050019
                                                                        02060019
           IF ACTIVE-SCENARIO                                           02070019
               EXEC SQL                                                 02080019
                   SELECT UNIT_PRICE INTO :WS-UNIT-PRICE                02090019
                     FROM ITEM_SUPPLIER                                 02100019
                    WHERE ITEM_PREFIX = :ITEM-SUPPLIER-ITEM-PREFIX      02110019
                      AND ITEM_NUMBER = :ITEM-SUPPLIER-ITEM-NUMBER      02120019
                    AND SUPPLIER_PREFIX = :ITEM-SUPPLIER-SUPPLIER-PREFIX02130019
                      AND SUPPLIER_ID = :ITEM-SUPPLIER-SUPPLIER-ID      02140019
               END-EXEC                                                 02150019
               IF SQLCODE              NOT = +0                         02160019
                   MOVE 1              TO DB2-SECTION                   02170019
                   MOVE 'SQL='         TO PARM-SQL-COMM                 02180019
                   MOVE SQLCODE        TO PARM-SQL-CODE                 02190019
      *            MOVE LS-USERID-PREFIX TO USERID                      02200019
                   CALL 'SNAPAID' USING PARM-INTERFACE                  02210019
               END-IF                                                   02220019
           ELSE                                                         02230019
               EXEC SQL                                                 02240019
                   SELECT UNIT_PRICE INTO :ITEM-SUPPLIER-UNIT-PRICE     02250019
                     FROM ITEM_SUPPLIER                                 02260019
                    WHERE ITEM_PREFIX = :ITEM-SUPPLIER-ITEM-PREFIX      02270019
                      AND ITEM_NUMBER = :ITEM-SUPPLIER-ITEM-NUMBER      02280019
                    AND SUPPLIER_PREFIX = :ITEM-SUPPLIER-SUPPLIER-PREFIX02290019
                      AND SUPPLIER_ID = :ITEM-SUPPLIER-SUPPLIER-ID      02300019
               END-EXEC.                                                02310019
                                                                        02320019
           IF SQLCODE                  = ZEROS                          02330019
             NEXT SENTENCE                                              02340019
           ELSE                                                         02350019
             IF SQLCODE                = 100                            02360019
               MOVE 'Y'                TO WS-END-OF-PROCESS-SW          02370019
               GO TO 0200-ACCUM-TOTAL-COST-EXIT                         02380019
             ELSE                                                       02390019
               MOVE 'Y'                TO WS-END-OF-PROCESS-SW          02400019
                                          WS-ERROR-SW                   02410019
               MOVE SQLCODE            TO WS-STATUS                     02420019
               GO TO 0200-ACCUM-TOTAL-COST-EXIT.                        02430019
                                                                        02440019
           COMPUTE WS-EXTENDED-PRICE   = ITEM-SUPPLIER-UNIT-PRICE *     02450019
                                         PENDING-ORDER-QUANTITY.        02460019
                                                                        02470019
           ADD WS-EXTENDED-PRICE       TO WS-TOTAL-COST.                02480019
                                                                        02490019
       0200-ACCUM-TOTAL-COST-EXIT.                                      02500019
           EXIT.                                                        02510019
                                                                        02520019
       9999-SQLERROR.                                                   02530019
                                                                        02540019
           MOVE SQLCODE TO WS-STATUS.                                   02550019
           CALL 'ILBOABN0' USING WS-STATUS.                             02560019
                                                                        02570019
           STOP RUN.                                                    02580019