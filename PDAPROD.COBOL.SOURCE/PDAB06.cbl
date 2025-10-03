       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDAB06.

      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDAB06                                            *
      *                                                               *
      * FUNCTION:   PROGRAM PDAB06 IS A PRODUCT DEMONSTRATION         *
      *             APPLICATION BATCH PROGRAM WITH SIMILAR            *
      *             FUNCTIONALITY TO THE ONLINE CICS PRODUCT          *
      *             DEMONSTRATION APPLICATION WITH REGARD TO ORDER    *
      *             ADD, CHANGE, AND DELETE PROCESSING.               *
      *                                                               *
      *             ALL SPECIAL PROCESSING SCENARIOS ESTABLISHED IN   *
      *             THE ONLINE CICS PRODUCT DEMONSTRATION APPLICATION *
      *             HAVING BATCH RELEVANCE WILL BE REPRODUCED IN THIS *
      *             BATCH PROCESS.                                    *
      *                                                               *
      *                                                               *
      * FILES   :   PARAMETER FILE       (SEQUENTIAL) (INPUT)         *
      *             PENDING ORDER FILE   (VSAM)       (INPUT / OUTPUT)*
      *             CUSTOMER FILE        (VSAM)       (INPUT / OUTPUT)*
      *             ORDER DATABASE       (IMS)        (INPUT / OUTPUT)*
      *             USERID TABLE         (DB2)        (INPUT / OUTPUT)*
      *             ITEM TABLE           (DB2)        (INPUT)         *
      *             SUPPLIER TABLE       (DB2)        (INPUT)         *
      *             ITEM SUPPLIER TABLE  (DB2)        (INPUT)         *
      *             PURCHASE TYPE TABLE  (DB2)        (INPUT)         *
      *                                                               *
      *                                                               *
      *****************************************************************
      *             PROGRAM CHANGE LOG                                *
      *             -------------------                               *
      *                                                               *
      * DATE        UPDATED BY            CHANGE DESCRIPTION          *
      * ----------  --------------------  --------------------------  *
      * 12/14/2005  PAUL BARON            ELIMINATE USE OF THE FIELDS *
      *                                   CUSTOMER-TOTAL-DOLLAR-AMT-R *
      *                                   PDAS01-ORDER-DOLLAR-AMT-R   *
      *                                                               *
      *                                   CHANGE SCENARIOS #2 AND #22 *
      *                                   TO REFERENCE AS "STORAGE    *
      *                                   OVERLAY" NOT "STORAGE       *
      *                                   VIOLATION", CHANGE WS-16 AND*
      *                                   LS-16 ARRAYS TO BE MORE     *
      *                                   DESCRIPTIVE, CHANGE THE 1   *
      *                                   BYTE ARRAY ENTRIES TO BE    *
      *                                   MORE REALISTIC I.E. ITEM    *
      *                                   STATUS INDICATORS           *
      *                                                               *
      * MM/DD/YYYY  XXXXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                                               *
      *****************************************************************

       ENVIRONMENT DIVISION.
                                                                        00380000
       INPUT-OUTPUT SECTION.                                            00390000
       FILE-CONTROL.                                                    00410000
                                                                        00420000
           SELECT INPUT-PARAMETERS   ASSIGN TO IPARAMS.                 00430000
                                                                        00540000
           SELECT VSAM-CUSTOMER      ASSIGN TO VCUSTOMR                 00550000
                                     ORGANIZATION IS INDEXED            00560000
                                     ACCESS IS DYNAMIC                  00570000
                                     RECORD KEY IS CUSTOMER-KEY         00580000
                                     FILE STATUS IS WMF-CUSTOMR-STATUS. 00590000
                                                                        00540000
           SELECT VSAM-PENDING-ORDER ASSIGN TO VPENDORD                 00550000
                                     ORGANIZATION IS INDEXED            00560000
                                     ACCESS IS DYNAMIC                  00570000
                                     RECORD KEY IS PENDING-ORDER-KEY    00580000
                                     FILE STATUS IS WMF-PENDORD-STATUS. 00590000
           EJECT                                                        00600000
       DATA DIVISION.                                                   00610000
       FILE SECTION.                                                    00630000
                                                                        00640000
       FD INPUT-PARAMETERS                                              00650000
           LABEL RECORDS ARE STANDARD                                   00660000
           RECORDING MODE IS F                                          00670000
           RECORD CONTAINS 80 CHARACTERS                                00680000
           BLOCK CONTAINS 0 CHARACTERS.                                 00690000
                                                                        00700000
       01  INPUT-PARAMETER-RECORD      PIC X(80).                       00710000
                                                                        00730000
                                                                        00730000
       FD  VSAM-CUSTOMER                                                01180000
           RECORD CONTAINS 733 CHARACTERS.                              01190000
                                                                        01200000
           COPY VCUSTOMR.                                               01210000
           EJECT                                                        01220000
                                                                        00730000
                                                                        00730000
       FD  VSAM-PENDING-ORDER                                           01180000
           RECORD CONTAINS 89 CHARACTERS.                               01190000
                                                                        01200000
           COPY VPENDORD.                                               01210000
           EJECT                                                        01220000
       WORKING-STORAGE SECTION.

      *****************************************************************
      *    77 LEVEL DATA ITEMS HERE  (SUBSCRIPTS, INDEXES ETC.)       *
      *****************************************************************
       77  WS-SUB                      PIC S9(04)  COMP   VALUE +0.
       77  WS-SUB1                     PIC S9(04)  COMP   VALUE +0.
       77  WS-SUB2                     PIC S9(04)  COMP   VALUE +0.
       77  LS-SUB                      PIC S9(04)  COMP   VALUE +0.
       77  WS-MAX-PARAMETERS           PIC S9(04)  COMP   VALUE +500.
       77  WS-USERID-PARM-COUNT        PIC S9(04)  COMP   VALUE +0.
       77  WS-RETURN-CODE              PIC  9(04)  COMP   VALUE  0.
       77  WS-PARAMETER-RECORDS-IN     PIC S9(05)  COMP-3 VALUE +0.
       77  WS-COUNT                    PIC S9(04)  COMP   VALUE +0.
       77  WS-SUPPLIER-COUNT           PIC S9(07)  COMP-3 VALUE +0.
       77  WS-CAT-SUB                  PIC S9(04)  COMP   VALUE +0.
       77  WS-SUBCAT-SUB               PIC S9(04)  COMP   VALUE +0.
       77  WS-STATUS-ARRAY-MAX         PIC S9(04)  COMP   VALUE +17.

      *****************************************************************
      *    SWITCHES                                                   *
      *****************************************************************
       01  WS-SWITCHES.

           05  WS-ERROR-FOUND-SW       PIC X(01)             VALUE 'N'.
               88  ERROR-FOUND                               VALUE 'Y'.
               88  NO-ERROR-FOUND                            VALUE 'N'.

           05  WS-END-OF-PARM-FILE-SW  PIC X(01)             VALUE 'N'.
               88  END-OF-PARM-FILE                          VALUE 'Y'.
               88  NOT-END-OF-PARM-FILE                      VALUE 'N'.

           05  WS-PROCESS-COMPLETE-SW  PIC X(01)             VALUE 'N'.
               88  PROCESS-COMPLETE                          VALUE 'Y'.
               88  NOT-PROCESS-COMPLETE                      VALUE 'N'.

           05  WS-PARM-ERROR-FOUND-SW  PIC X(01)             VALUE 'N'.
               88  PARM-ERROR-FOUND                          VALUE 'Y'.
               88  NOT-PARM-ERROR-FOUND                      VALUE 'N'.

           EJECT
      *****************************************************************
      *    MISCELLANEOUS WORK FIELDS                                  *
      *****************************************************************
       01  WS-MISCELLANEOUS-FIELDS.

           05  WMF-CATEGORY            PIC X(32)   VALUE 'BOLTS'.
           05  WMF-SUB-CATEGORY        PIC X(32)   VALUE 'ANCHOR'.
           05  WMF-CUSTOMER-ID         PIC X(32)   VALUE 'ARROW'.
           05  WMF-ITEM                PIC X(32)   VALUE '1000'.
           05  WMF-USERID              PIC X(08)   VALUE 'USERIDXX'.
           05  WMF-PO-NUMBER           PIC  9(13)  VALUE 998123 COMP-3.

           05  WMF-CUSTOMR-STATUS      PIC X(02)   VALUE SPACES.
           05  WMF-PENDORD-STATUS      PIC X(02)   VALUE SPACES.
           05  WMF-DATE-MMDDYY         PIC X(08)   VALUE SPACES.
PWB305     05  WMF-MAX-DAYS            PIC S9(03)  VALUE +366.          PWB32005
PWB305     05  WMF-MAX-DAYS-PER-MTH    PIC  9(03)  VALUE 31.            PWB32005

           05  WMF-DATE-YYMMDD.
               10 WMF-DATE-YY          PIC 9(02)   VALUE ZEROES.
               10 WMF-DATE-MM          PIC 9(02)   VALUE ZEROES.
               10 WMF-DATE-DD          PIC 9(02)   VALUE ZEROES.

           05  WMF-TIME-HHMMSS         PIC X(08)   VALUE SPACES.
           05  WMF-MESSAGE-AREA        PIC X(80)   VALUE SPACES.

           05  WMF-ITEM-NUMBER         PIC S9(08)  VALUE +0  COMP-3.
           05  WMF-USERID-NUMBER       PIC S9(09)  VALUE +0  COMP.
           05  WMF-NULL-IND            PIC S9(04)  VALUE +0  COMP.
           05  WMF-ITEM-SEQ            PIC S9(04)  VALUE +0  COMP.
           05  WMF-ORDER-TOTAL-AMOUNT  PIC S9(11)V99
                                                   VALUE +0  COMP-3.
           05  WMF-EXTENDED-PRICE      PIC S9(11)V99
                                                   VALUE +0  COMP-3.

           05  WMF-ACTIVE-SCENARIOS    PIC X(250)  VALUE SPACES.
           05  WMF-ACTIVE-SCENARIOS-R  REDEFINES WMF-ACTIVE-SCENARIOS
                                       OCCURS 250 TIMES
                                       PIC X(01).

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

PWB305 01  WS-CURRENT-DATE-TIME-R      REDEFINES WS-CURRENT-DATE-TIME.  PWB32005
PWB305     05  WS-CDT-DATE-R           PIC X(08).                       PWB32005
PWB305     05  WS-CDT-TIME-R           PIC X(08).                       PWB32005
PWB305     05  FILLER                  PIC X(01).                       PWB32005
PWB305     05  FILLER                  PIC X(04).                       PWB32005
           EJECT

      *****************************************************************
      *  SUBROUTINE PARAMETER AREAS                                   *
      *****************************************************************

       01  WS-PDAS02                   PIC X(8)    VALUE 'PDAS02'.

           COPY PDAS01CY.

       01  PDAS03-PARMS.
           03  PDAS03-AGE-DAYS         PIC 9(5)    VALUE ZEROES.
           03  PDAS03-MESSAGE          PIC X(15)   VALUE SPACES.
           EJECT

      *****************************************************************
      *    PARAMETER RECORD LAYOUTS                                   *
      *****************************************************************

       01  WS-PARAMETER-RECORD.
           05  WPR-RECORD-TYPE         PIC X(01).
               88  WPR-ADD-ORDER       VALUE 'A'.
               88  WPR-CHANGE-ORDER    VALUE 'C'.
               88  WPR-DELETE-ORDER    VALUE 'D'.
               88  WPR-SCENARIO        VALUE 'S'.
               88  WPR-USERID          VALUE 'U'.
           05  FILLER                  PIC X(01).
           05  WPR-RECORD-DATA         PIC X(78).
           05  WPR-RECORD-DATA-ORDER   REDEFINES WPR-RECORD-DATA.
               10  WPR-ORDER-NUMBER    PIC X(10).
               10  WPR-ORDER-NUMBER-R  REDEFINES WPR-ORDER-NUMBER
                                       PIC 9(10).
               10  FILLER              PIC X(68).
           05  WPR-RECORD-DATA-SCENARIO
                                       REDEFINES WPR-RECORD-DATA.
               10  WPR-SCENARIO-NUMBER PIC X(03).
               10  WPR-SCENARIO-NUMBER-R
                                       REDEFINES WPR-SCENARIO-NUMBER
                                       PIC 9(03).
               10  FILLER              PIC X(75).
           05  WPR-RECORD-DATA-USERID  REDEFINES WPR-RECORD-DATA.
               10  WPR-USERID-VALUE    PIC X(08).
               10  FILLER              PIC X(70).


      *****************************************************************
      *    PARAMETER RECORD ARRAY                                     *
      *****************************************************************
       01  WS-PARAMETER-RECORD-ARRAY.
           05  WPRA-RECORD             OCCURS 500 TIMES
                                       PIC X(80).


      *****************************************************************
      *    VSAM FILE DEFINITIONS                                      *
      *****************************************************************

KCS305**** COPY VPENDORD.                                               KCS32005
           EJECT

KCS305**** COPY VCUSTOMR.                                               KCS32005
           EJECT


      *****************************************************************
      *    IMS / DLI DEFINITIONS                                      *
      *****************************************************************
                                                                        03330000
      ***************************************************************** 03060000
      *    IMS FUNCTION DEFINITIONS                                   * 03070000
      ***************************************************************** 03080000
                                                                        03330000
       01  IMS-CALL-FUNCTIONS.                                          03330000
           05 ICF-GHU                  PIC X(04)   VALUE 'GHU'.         03330000
           05 ICF-GN                   PIC X(04)   VALUE 'GN'.          03330000
           05 ICF-ISRT                 PIC X(04)   VALUE 'ISRT'.        03330000
           05 ICF-REPL                 PIC X(04)   VALUE 'REPL'.        03330000
           05 ICF-DLET                 PIC X(04)   VALUE 'DLET'.        03330000
                                                                        03330000
      ***************************************************************** 03060000
      *    IMS SEGMENT SEARCH ARGUMENTS (SSA)                         * 03070000
      ***************************************************************** 03080000
                                                                        03090000
       01  ORDER-SSA-QUAL.                                              03100000
           03  FILLER                  PIC X(8)  VALUE 'ORDER'.         03110000
           03  FILLER                  PIC X     VALUE '('.             03120000
           03  FILLER                  PIC X(8)  VALUE 'ORDKEY'.        03130000
           03  FILLER                  PIC XX    VALUE ' ='.            03140000
           03  OSQ-ORDER-KEY.                                           03150000
               05  OSQ-ORDER-PREFIX    PIC 9(5)  VALUE ZEROES.          03160000
               05  OSQ-ORDER-NUMBER    PIC 9(10) VALUE ZEROES.          03170000
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
                                                                        03330000
      ***************************************************************** 03280000
      *    IMS SEGMENT I/O AREAS                                      * 03290000
      ***************************************************************** 03300000
                                                                        03310000
           COPY IORDER.                                                 03320000
                                                                        03330000
                                                                        03340000
           COPY IORDITEM.                                               03350000
           EJECT                                                        03360000

      *****************************************************************
      *    DB2  DEFINITIONS                                           *
      *****************************************************************

      *****************************************************************
      *         SQL COMMUNICATIONS AREA                               *
      *****************************************************************

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

           EJECT
      *****************************************************************
      *         USER IDENTIFICATION TABLE        -- DCLGEN DUSERID    *
      *****************************************************************

           EXEC SQL
               INCLUDE DUSERID
           END-EXEC.
           EJECT

      *****************************************************************
      *         ITEM TABLE                       -- DCLGEN DITEM      *
      *****************************************************************

           EXEC SQL
               INCLUDE DITEM
           END-EXEC.
           EJECT

      *****************************************************************
      *         SUPPLIER TABLE                   -- DCLGEN DSUPPLR    *
      *****************************************************************

           EXEC SQL
               INCLUDE DSUPPLR
           END-EXEC.
           EJECT

      *****************************************************************
      *         PURCHASE TYPE TABLE              -- DCLGEN DPURTYP    *
      *****************************************************************

           EXEC SQL
               INCLUDE DPURTYP
           END-EXEC.
           EJECT

      *****************************************************************
      *         ITEM SUPPLIER TABLE              -- DCLGEN DITMSUP    *
      *****************************************************************

           EXEC SQL
               INCLUDE DITMSUP
           END-EXEC.
           EJECT

      *****************************************************************
      *         DB2 CURSORS                                           *
      *****************************************************************

      *****************************************************************
      *         ITEM TABLE CURSOR                                     *
      *****************************************************************
           EXEC SQL
           DECLARE ITEMFORW CURSOR FOR
             SELECT  NUMBER,
                     NAME
             FROM ITEM
             WHERE PREFIX             = :ITEM-PREFIX AND
                   CATEGORY_NAME      = :ITEM-CATEGORY-NAME AND
                   SUB_CATEGORY_NAME  = :ITEM-SUB-CATEGORY-NAME AND
                   NUMBER            >= :ITEM-NUMBER
             ORDER BY 1
           END-EXEC.

      *****************************************************************
      *         ITEM TABLE CURSOR (USED IN SCENARIO PROCESSING)       *
      *****************************************************************

           EXEC SQL
           DECLARE CATEGORY CURSOR FOR
             SELECT  NUMBER,
                     NAME
             FROM ITEM
             WHERE PREFIX             = :ITEM-PREFIX AND
                   CATEGORY_NAME      = :ITEM-CATEGORY-NAME
           END-EXEC.

      *****************************************************************
      *         ITEM TABLE CURSOR (USED IN SCENARIO PROCESSING)       *
      *****************************************************************

           EXEC SQL
           DECLARE SUBCAT CURSOR FOR
             SELECT  NUMBER,
                     NAME
             FROM ITEM
             WHERE PREFIX             = :ITEM-PREFIX
             ORDER BY SUB_CATEGORY_NAME
           END-EXEC.
           EJECT

      *****************************************************************
      *    ITEM TABLE CURSOR (USES ITEM, ITEM SUPPLIER, SUPPLIER)     *
      *****************************************************************

           EXEC SQL
               DECLARE ITEMSUPL CURSOR FOR
                   SELECT  NUMBER,
                           CATEGORY_NAME,
                           SUB_CATEGORY_NAME,
                           ITEM.NAME,
                           LENGTH,
                           DIAMETER,
                           ITEM_SUPPLIER.SUPPLIER_ID,
                           UNIT_PRICE,
                           SUPPLIER.NAME
                   FROM    ITEM,
                           ITEM_SUPPLIER,
                           SUPPLIER
                   WHERE   (ITEM.PREFIX       = :ITEM-PREFIX AND
                           ITEM_PREFIX        = :ITEM-PREFIX AND
                           SUPPLIER.PREFIX    = :ITEM-PREFIX)
                           AND
                           (ITEM.NUMBER       = :ITEM-NUMBER AND
                           ITEM_NUMBER        = :ITEM-NUMBER)
                           AND
                           ITEM_SUPPLIER.SUPPLIER_ID
                                              = SUPPLIER.SUPPLIER_ID
                   ORDER BY SUPPLIER_ID
           END-EXEC.
           EJECT

      *****************************************************************
      *         DB2 STORED PROCEDURE PARAMETER / WORK AREAS           *
      *****************************************************************

       01  PDASP1-PARAMETERS.
           05  PDASP1-PREFIX           PIC X(05)          VALUE ZEROES.
           05  PDASP1-PREFIX-R         REDEFINES PDASP1-PREFIX
                                       PIC 9(05).
           05  PDASP1-TOTAL-COST       PIC S9(15)V99 COMP-3 VALUE +0.
           05  PDASP1-STATUS           PIC X(04)          VALUE SPACES.


       01  PDASP2-PARAMETERS.
           05  PDASP2-USERID           PIC X(08)          VALUE SPACES.
           05  PDASP2-USERID-NUMBER    PIC S9(09)  COMP   VALUE +0.
           05  PDASP2-ACTIVE-SCENARIOS PIC X(250)         VALUE SPACES.
           05  PDASP2-STATUS           PIC X(04)          VALUE SPACES.

      *****************************************************************
      *    GENERAL ERROR PROCESSING WORK AREAS                        *
      *****************************************************************
      ******************************************************************
      * PRODUCT DEMONSTRATION APPLICATION (PDA)                        *
      *                                                                *
      * ERROR WORK AREA DEFINITIONS FOR: CICS, IMS-DLI, DB2, MQSERIES  *
      *                                                                *
      ******************************************************************

       01  WS-PDA-ERROR-GENERAL.

           05  WS-PDA-ERROR-TYPE       PIC X(04)       VALUE SPACES.
               88  PDA-GENERAL-ERROR                   VALUE 'GEN'.
               88  PDA-DB2-ERROR                       VALUE 'DB2'.
               88  PDA-IMS-ERROR                       VALUE 'IMS'.
               88  PDA-MQSERIES-ERROR                  VALUE 'MQS'.


      ******************************************************************
      *    PDA FORMATTED ERROR LINES                                   *
      ******************************************************************

       01  WS-PDA-ERROR-AREA.
           05  WPEA-ERROR-01           PIC X(80)       VALUE ALL '*'.
           05  WPEA-ERROR-02.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-03.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE
               '   PRODUCT DEMONSTRATION APPLICATION (PDA) ERROR '.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-04.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-05           PIC X(80)       VALUE ALL '*'.
           05  WPEA-ERROR-06.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-07.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 WPEA-ERROR-07-TEXT   PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-08.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 WPEA-ERROR-08-TEXT   PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-09.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-10           PIC X(80)       VALUE ALL '*'.


      ******************************************************************
      *    PDA GENERAL ERROR LINES                                     *
      ******************************************************************

       01  WS-PDA-GEN-ERROR-01.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(07)       VALUE
               'ERROR: '.
           05  FILLER                  PIC X(10)       VALUE
               'PROGRAM = '.
           05  WPGE-PROGRAM-ID         PIC X(08)       VALUE SPACES.
           05  FILLER                  PIC X(14)       VALUE
               ', PARAGRAPH = '.
           05  WPGE-PARAGRAPH          PIC X(06).
           05  FILLER                  PIC X(32)       VALUE SPACES.

       01  WS-PDA-GEN-ERROR-02.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  WPGE-DESCRIPTION        PIC X(78)       VALUE SPACES.


      ******************************************************************
      *    PDA IMS-DLI ERROR LINES                                     *
      ******************************************************************

       01  WS-PDA-IMS-ERROR-01.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(15)       VALUE
               'IMS-DLI ERROR: '.
           05  FILLER                  PIC X(08)       VALUE
               'PROGRAM='.
           05  WPIE-PROGRAM-ID         PIC X(08)       VALUE SPACES.
           05  FILLER                  PIC X(12)       VALUE
               ', PARAGRAPH='.
           05  WPIE-PARAGRAPH          PIC X(06)       VALUE SPACES.
           05  FILLER                  PIC X(09)       VALUE
               ', STATUS='.
           05  WPIE-STATUS-CODE        PIC X(2)        VALUE SPACES.
           05  FILLER                  PIC X(12)       VALUE
               ', FUNCTION='.
           05  WPIE-FUNCTION-CODE      PIC X(4)        VALUE SPACES.

       01  WS-PDA-IMS-ERROR-02.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(08)       VALUE
               'SEGMENT='.
           05  WPIE-SEGMENT-NAME       PIC X(8)        VALUE SPACES.
           05  FILLER                  PIC X(11)       VALUE
               ', DATABASE='.
           05  WPIE-DATABASE-NAME      PIC X(8)        VALUE SPACES.
           05  FILLER                  PIC X(10)       VALUE
               ', COMMAND='.
           05  WPIE-COMMAND            PIC X(32)       VALUE SPACES.


      ******************************************************************
      *    PDA DB2 ERROR LINES                                         *
      ******************************************************************

       01  WS-PDA-DB2-ERROR-01.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(11)       VALUE
               'DB2 ERROR: '.
           05  FILLER                  PIC X(10)       VALUE
               'PROGRAM = '.
           05  WPDE-PROGRAM-ID         PIC X(08)       VALUE SPACES.
           05  FILLER                  PIC X(12)       VALUE
               ', SQLCODE = '.
           05  WPDE-DB2-SQLCODE        PIC ZZZZZZ9-.
           05  FILLER                  PIC X(28)       VALUE SPACES.

       01  WS-PDA-DB2-ERROR-02.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(11)       VALUE
               'FUNCTION = '.
           05  WPDE-FUNCTION           PIC X(30)       VALUE SPACES.
           05  WPDE-FUNCTION-R         REDEFINES WPDE-FUNCTION.
               10  WPDE-FUNCTION-1     PIC X(15).
               10  WPDE-FUNCTION-2     PIC X(15).
           05  FILLER                  PIC X(14)       VALUE
               ', PARAGRAPH = '.
           05  WPDE-PARAGRAPH          PIC X(06)       VALUE SPACES.
           05  FILLER                  PIC X(16)       VALUE SPACES.


      ******************************************************************
      *    PDA MQSERIES ERROR LINES                                    *
      ******************************************************************

       01  WS-PDA-MQSERIES-ERROR-01.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(16)       VALUE
               'MQSERIES ERROR: '.
           05  FILLER                  PIC X(10)       VALUE
               'PROGRAM = '.
           05  WPME-PROGRAM-ID         PIC X(08)       VALUE SPACES.
           05  FILLER                  PIC X(16)       VALUE
               ', REASON CODE = '.
           05  WPME-REASON-CODE        PIC ZZZZZZZZ9.
           05  FILLER                  PIC X(18)       VALUE SPACES.

       01  WS-PDA-MQSERIES-ERROR-02.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(11)       VALUE
               'FUNCTION = '.
           05  WPME-FUNCTION           PIC X(30)       VALUE SPACES.
           05  WPME-FUNCTION-R         REDEFINES WPME-FUNCTION.
               10  WPME-FUNCTION-1     PIC X(15).
               10  WPME-FUNCTION-2     PIC X(15).
           05  FILLER                  PIC X(14)       VALUE
               ', PARAGRAPH = '.
           05  WPME-PARAGRAPH          PIC X(06)       VALUE SPACES.
           05  FILLER                  PIC X(17)       VALUE SPACES.

      *****************************************************************
      *    MESSAGES   (ERROR AND INFORMATIONAL)                       *
      *****************************************************************

       01  WS-PDAB06-MESSAGES.

           05  WPM-BLANK               PIC X(01)       VALUE     ' '.
           05  WPM-ALL-ASTERISK        PIC X(80)       VALUE ALL '*'.

           05  WPM-BEGIN-PROGRAM.
               10 FILLER               PIC X(78)   VALUE
                  '***** BEGIN PROGRAM PDAB06 *****'.

           05  WPM-END-PROGRAM.
               10 FILLER               PIC X(78)   VALUE
                  '***** END PROGRAM PDAB06 *****'.

           05  WPM-VSAM-ERROR.
               10 FILLER               PIC X(21)   VALUE
                  'VSAM ERROR ON FILE - '.
               10 WPM-VSAM-ERROR-FILE  PIC X(09)   VALUE SPACES.
               10 FILLER               PIC X(15)   VALUE
                  ',FILE STATUS = '.
               10 WPM-VSAM-ERROR-STATUS
                                       PIC X(02)   VALUE SPACES.
               10 FILLER               PIC X(12)   VALUE
                  ', COMMAND = '.
               10 WPM-VSAM-ERROR-COMMAND
                                       PIC X(19)   VALUE SPACES.

           05  WPM-PARAMETER-FILE-EMPTY.
               10 FILLER               PIC X(78)   VALUE
                  'INPUT PARAMETER FILE (IPARAMS) IS EMPTY - PARAMETERS
      -           'ARE REQUIRED'.

           05  WPM-MAX-PARAMETERS-EXCEEDED.
               10 FILLER               PIC X(48)   VALUE
                  'MAX NUMBER OF INPUT PARAMETER RECORDS EXCEEDED, '.
               10 FILLER               PIC X(14)   VALUE
                  'MAX ALLOWED = '.
               10 WPM-MAX-PARAMETERS   PIC ZZZZ9.
               10 FILLER               PIC X(11)   VALUE SPACES.

           05  WPM-PARM-INVALID-RECORD-TYPE.
               10 FILLER               PIC X(78)   VALUE
                  'POSITION 1 - RECORD TYPE MUST BE A,C,D,S OR U '.

           05  WPM-RECORD-NUMBER-MSG.
               10 FILLER               PIC X(16)   VALUE
                  'RECORD NUMBER = '.
               10 WPM-RECORD-NUMBER    PIC 9(05)   VALUE ZEROES.
               10 FILLER               PIC X(59)   VALUE SPACES.

           05  WPM-INVALID-ORDER-NUMBER.
               10 FILLER               PIC X(78)   VALUE
                  'POSITION 3 - 12, ORDER NUMBER MUST BE NUMERIC '.

           05  WPM-INVALID-SCENARIO-NUMBER.
               10 FILLER               PIC X(78)   VALUE
                  'POSITION 3 - 5, SCENARIO NUMBER MUST BE NUMERIC, VALU
      -           'E 1 THRU 250'.

           05  WPM-ORDER-NOT-FOUND-CHANGE.
               10 FILLER               PIC X(06)   VALUE
                  'ORDER '.
               10 WPM-ORDER-NUMBER-CHG PIC X(10)   VALUE ZEROES.
               10 FILLER               PIC X(64)   VALUE
                  ' NOT ON FILE, CHANGE UNSUCCESSFUL'.

           05  WPM-ORDER-NOT-FOUND-DELETE.
               10 FILLER               PIC X(06)   VALUE
                  'ORDER '.
               10 WPM-ORDER-NUMBER-DEL PIC X(10)   VALUE ZEROES.
               10 FILLER               PIC X(64)   VALUE
                  ' NOT ON FILE, DELETE UNSUCCESSFUL'.

           05  WPM-TABLE-OVERFLOW.
               10 FILLER               PIC X(25)   VALUE
                  'TABLE/ARRAY OVERFLOW ON: '.
               10 WPM-TABLE-NAME       PIC X(30)   VALUE SPACES.
               10 FILLER               PIC X(23)   VALUE SPACES.

           05  WPM-USERID-PARM-REQUIRED.
               10 FILLER               PIC X(78)   VALUE
                  'USER ID INPUT PARAMETER RECORD IS REQUIRED '.

           05  WPM-USERID-PARM-TOO-MANY.
               10 FILLER               PIC X(78)   VALUE
                  'ONLY 1 USER ID INPUT PARAMETER RECORD IS ALLOWED '.

           05  WPM-INVALID-USERID.
               10 FILLER               PIC X(78)   VALUE
                  'POSITION 3 - 10, USER ID IS REQUIRED '.

           05  WPM-USERID-NOT-FOUND.
               10 FILLER               PIC X(08)   VALUE
                  'USER ID '.
               10 WPM-USERID-VALUE     PIC X(08)   VALUE SPACES.
               10 FILLER               PIC X(62)   VALUE
                  ' NOT FOUND IN THE PDA APP., ADD THE ID USING THE PDA
      -           'CICS APP.'.

           05  WPM-PROGRAM-ERROR.
               10 FILLER               PIC X(29)   VALUE
                  'ERROR RETURNED FROM PROGRAM: '.
               10 WPM-PROGRAM-NAME     PIC X(09)   VALUE SPACES.
               10 FILLER               PIC X(15)   VALUE
                  ',RETURN CODE = '.
               10 WPM-RETURN-CODE      PIC X(10)   VALUE SPACES.
               10 FILLER               PIC X(15)   VALUE SPACES.

           EJECT

      *****************************************************************
      *    PDA STANDARD CATEGORY / SUB-CATEGORY FOR THE APPLICATION   *
      *****************************************************************

           COPY PDACATGY.
           EJECT

      *****************************************************************
      *    PROGRAM INTERNAL USE ARRAYS CATEGORY, SUB-CATEGORY         *
      *****************************************************************

       01  WS-PDA-CATEGORY-ARRAY.
           05  WPCA-CATEGORY-MAX       PIC S9(05)   COMP-3  VALUE +100.
           05  WPCA-CATEGORY-COUNT     PIC S9(05)   COMP-3.
           05  WPCA-CATEGORY-GRP       OCCURS 1 TO 100 TIMES
                                       DEPENDING ON
                                         WPCA-CATEGORY-COUNT
                                           INDEXED BY WPCA-CAT-IX.
               10  WPCA-CATEGORY       PIC X(32).
               10  WPCA-SUB-CATEGORY   PIC X(32).

           EJECT

      *****************************************************************
      *    D E M O N S T R A T I O N    P U R P O S E S   O N L Y     *
      *    ARRAY NOT USED IN APPLICATION                              *
      *    PROGRAM INTERNAL USE ARRAY  SUB-CATEGORY                   *
      *****************************************************************

       01  WS-SUB-CATEGORY-ARRAY.
           05  WSCA-MAX-ENTRIES        PIC S9(05)   COMP-3  VALUE +100.
           05  WSCA-SUB-CATEGORY-COUNT PIC S9(05)   COMP-3.
           05  WSCA-SUB-CATEGORY-GRP   OCCURS 1 TO 100 TIMES
                                       DEPENDING ON
                                         WSCA-SUB-CATEGORY-COUNT
                                           INDEXED BY WSCA-SUBCAT-IX.
               10  WSCA-SUB-CATEGORY   PIC X(32).
           EJECT

      *****************************************************************
      *         DB2 TABLE ITEM STATUS CODES FROM VENDOR               *
      *         BLANK  = ACTIVE                                       *
      *         S      = SUSPENDED                                    *
      *         C      = CANCELLED                                    *
      *         N      = NO STOCK                                     *
      *                                                               *
      *         (DEMO PURPOSES ONLY NOT REALLY IN DB2 TABLE)          *
      *****************************************************************

       01  WS-VENDOR-ITEM-STATUS.
           05  VENDOR-ITEM-STATUS-CODE-GRP
                                       PIC X(12).
           05  VENDOR-ITEM-STATUS-CODE REDEFINES
                                       VENDOR-ITEM-STATUS-CODE-GRP
                                       OCCURS 12
                                       PIC X.


      *****************************************************************
      *    WORKING STORAGE ITEM STATUS ARRAY                          *
      *                                                               *
      *    VALID ITEM STATUS CODES (CORPORATE MASTER)                 *
      *    A = ACTIVE                                                 *
      *    B = BACK ORDER                                             *
      *    D = DISCONTINUED                                           *
      *    I = INACTIVE                                               *
      *****************************************************************

       01  WS-ITEM-STATUS-ARRAY.
           03  WISA-ITEM-STATUS-GRP    PIC X(12).
           03  WISA-ITEM-STATUS        REDEFINES WISA-ITEM-STATUS-GRP
                                       PIC X(01)
                                       OCCURS 12 TIMES
                                       INDEXED BY STATUS-INDEX.

       77  WS-SAVE-NUMBER-OF-ENTRIES   PIC S9(3) COMP-3 VALUE +0.
       77  WS-NUMBER-OF-ENTRIES        PIC S9(3) COMP-3 VALUE +0.


      *****************************************************************
      *    LINKAGE ITEM STATUS ARRAY                                  *
      *                                                               *
      *    VALID ITEM STATUS CODES (CORPORATE MASTER)                 *
      *    A = ACTIVE                                                 *
      *    B = BACK ORDER                                             *
      *    D = DISCONTINUED                                           *
      *    I = INACTIVE                                               *
      *****************************************************************

       01  LS-ITEM-STATUS-ARRAY.
           03  LISA-ITEM-STATUS-GRP    PIC X(12).
           03  LISA-ITEM-STATUS        REDEFINES LISA-ITEM-STATUS-GRP
                                       PIC X(01)
                                       OCCURS 12 TIMES.


       01  WS-END-OF-WS.
           05  FILLER                  PIC X(05)   VALUE '#####'.

      *****************************************************************
      *    L I N K A G E     S E C T I O N                            *
      *****************************************************************

       LINKAGE SECTION.
                                                                        03780000
      ****************************************************************  03790000
      *****  I-O PCB                                                    03800000
      ****************************************************************  03810000
                                                                        03820000
       01  IO-PCB.                                                      03830000
           05  FILLER                  PIC X(10) VALUE SPACES.          03840000
           05  IO-STATUS               PIC XX    VALUE SPACES.          03850000
           05  FILLER                  PIC X(20) VALUE SPACES.          03860000


           COPY PCBORDER.
           EJECT

      *****************************************************************
      *    P R O C E D U R E    D I V I S I O N                       *
      *****************************************************************

       PROCEDURE DIVISION.

           ENTRY 'DLITCBL' USING IO-PCB
                                 ORDER-PCB.

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00000-MAINLINE                                *
      *                                                               *
      *    FUNCTION :  PROGRAM ENTRY, CONTROL HIGH LEVEL PROCESSING   *
      *                FOR THE PRODUCT DEMONSTRATION APPLICATION      *
      *                BATCH PROCESS                                  *
      *                                                               *
      *    CALLED BY:  NONE                                           *
      *                                                               *
      *****************************************************************

       P00000-MAINLINE.

           DISPLAY WPM-BLANK.
           DISPLAY WPM-ALL-ASTERISK.
           DISPLAY WPM-BEGIN-PROGRAM.
           DISPLAY WPM-ALL-ASTERISK.


           PERFORM  P00050-INITIALIZE                                   TAGGED
               THRU P00050-INITIALIZE-EXIT.                             CODE
                                                                        TESTING
                                                                        03/13/01
           IF NO-ERROR-FOUND
               PERFORM  P00500-MAIN-PROCESS
                   THRU P00500-MAIN-PROCESS-EXIT.
                                                                        TESTING
                                                                        03/13/01
           PERFORM  P00100-END-OF-JOB
               THRU P00100-END-OF-JOB-EXIT.


           DISPLAY WPM-BLANK.
           DISPLAY WPM-ALL-ASTERISK.
           DISPLAY WPM-END-PROGRAM.
           DISPLAY WPM-ALL-ASTERISK.

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

           MOVE 'N'                    TO WS-ERROR-FOUND-SW
                                          WS-END-OF-PARM-FILE-SW
                                          WS-PROCESS-COMPLETE-SW.


      *****************************************************************
      *    OBTAIN CURRENT DATE AND TIME                               *
      *****************************************************************

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME.          00020001


      *****************************************************************
      *    OPEN FILES, VERIFY SUCCESSFUL VSAM FILE OPENS              *
      *****************************************************************

           OPEN INPUT    INPUT-PARAMETERS                               00020001
                I-O      VSAM-CUSTOMER
                I-O      VSAM-PENDING-ORDER.


           IF WMF-CUSTOMR-STATUS = '00'                                 00020001
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P00050'           TO WPGE-PARAGRAPH
               MOVE 'VCUSTOMR'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-CUSTOMR-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'OPEN'             TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


           IF WMF-PENDORD-STATUS = '00'                                 00020001
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P00050'           TO WPGE-PARAGRAPH
               MOVE 'VPENDORD'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-PENDORD-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'OPEN'             TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    PERFORM 1ST READ ON PARAMETER FILE -- EOF IS AN ERROR      *
      *****************************************************************

           PERFORM  P80000-READ-PARAMETERS
               THRU P80000-READ-PARAMETERS-EXIT.

           IF END-OF-PARM-FILE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P00050'           TO WPGE-PARAGRAPH
               MOVE WPM-PARAMETER-FILE-EMPTY
                                       TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P00050-INITIALIZE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00100-END-OF-JOB                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PERFORM NORMAL END OF PROGRAM       *
      *                OPERATIONS, I.E. CLOSE FILES, ETC.             *
      *                                                               *
      *    CALLED BY:  P00000-MAINLINE                                *
      *                                                               *
      *****************************************************************

       P00100-END-OF-JOB.

      *****************************************************************
      *    CLOSE FILES, VERIFY SUCCESSFUL VSAM FILE CLOSURES          *
      *****************************************************************

           CLOSE  INPUT-PARAMETERS                                      00020001
                  VSAM-CUSTOMER
                  VSAM-PENDING-ORDER.


           IF WMF-CUSTOMR-STATUS = '00'                                 00020001
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P00100'           TO WPGE-PARAGRAPH
               MOVE 'VCUSTOMR'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-CUSTOMR-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'CLOSE'            TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


           IF WMF-PENDORD-STATUS = '00'                                 00020001
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P00100'           TO WPGE-PARAGRAPH
               MOVE 'VPENDORD'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-PENDORD-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'CLOSE'            TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P00100-END-OF-JOB-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00500-MAIN-PROCESS                            *
      *                                                               *
      *    FUNCTION :  CONTROL HIGH LEVEL PROCESSING FOR BOTH         *
      *                PARAMETER AND ORDER PROCESSES                  *
      *                                                               *
      *    CALLED BY:  P00000-MAINLINE                                *
      *                                                               *
      *****************************************************************

       P00500-MAIN-PROCESS.

      *****************************************************************
      *    PERFORM INPUT PARAMETER PROCESS -- IF ERROR FOUND, EXIT    *
      *****************************************************************

           PERFORM  P00600-PARAMETER-PROCESS                            TAGGED
               THRU P00600-PARAMETER-PROCESS-EXIT.                      CODE
                                                                        TESTING
           IF ERROR-FOUND                                               TESTING
               GO TO P00500-MAIN-PROCESS-EXIT.


      *****************************************************************
      *    PERFORM USERID VERIFICATION PROCESS, IF ERROR FOUND, EXIT  *
      *****************************************************************

           PERFORM  P01000-VERIFY-USERID                                TAGGED
               THRU P01000-VERIFY-USERID-EXIT.                          CODE
                                                                        TESTING
           IF ERROR-FOUND                                               TESTING
               GO TO P00500-MAIN-PROCESS-EXIT.


      *****************************************************************
      *    PERFORM ORDER PROCESSES BASED ON PARAMETER INPUT           *
      *****************************************************************
                                                                        03/13/01
           PERFORM  P02000-ORDER-PROCESS
               THRU P02000-ORDER-PROCESS-EXIT.

       P00500-MAIN-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00600-PARAMETER-PROCESS                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ THE PARAMETER INPUT RECORDS,   *
      *                STORE PARAMETERS IN AN ARRAY, EDIT THE         *
      *                PARAMETER CONTENT                               *
      *                                                               *
      *    CALLED BY:  P00500-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P00600-PARAMETER-PROCESS.

      *****************************************************************
      *    PROCESS PARAMETERS UNTIL END OF FILE                       *
      *****************************************************************

           MOVE ZEROES                 TO WS-SUB1
                                          WS-USERID-PARM-COUNT.

           PERFORM  P00630-LOAD-PARM-ARRAY                              TAGGED
               THRU P00630-LOAD-PARM-ARRAY-EXIT                         CODE
                   UNTIL END-OF-PARM-FILE.                              TESTING
                                                                        TESTING
           IF ERROR-FOUND                                               TESTING
               GO TO P00600-PARAMETER-PROCESS-EXIT.

      *****************************************************************
      *    PERFORM PARAMETER RECORD EDITS                             *
      *****************************************************************

           MOVE SPACES                 TO WMF-ACTIVE-SCENARIOS.

           PERFORM  P00660-EDIT-PARMS
               THRU P00660-EDIT-PARMS-EXIT
                   VARYING WS-SUB1 FROM +1 BY +1
                       UNTIL WS-SUB1 > WS-PARAMETER-RECORDS-IN.
                                                                        TESTING
           IF ERROR-FOUND                                               TESTING
               GO TO P00600-PARAMETER-PROCESS-EXIT.


      *****************************************************************
      *    IF NO USER ID SPECIFICATION RECORD, ERROR - TERMINATE      *
      *****************************************************************

           IF WS-USERID-PARM-COUNT     > ZEROES                         00020001
               NEXT SENTENCE
           ELSE
               MOVE WPM-USERID-PARM-REQUIRED
                                       TO WMF-MESSAGE-AREA
               PERFORM  P99400-ERROR-ROUTINE
                   THRU P99400-ERROR-ROUTINE-EXIT.

       P00600-PARAMETER-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00630-LOAD-PARM-ARRAY                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ PARAMETER RECORDS AND STORE THE*
      *                PARAMETERS IN AN ARRAY FOR LATER PROCESSING    *
      *                                                               *
      *    CALLED BY:  P00600-PARAMETER-PROCESS                       *
      *                                                               *
      *****************************************************************

       P00630-LOAD-PARM-ARRAY.

      *****************************************************************
      *    CHECK FOR MAXIMUM PARAMETER RECORDS ALLOWED                *
      *****************************************************************

           ADD +1                      TO WS-SUB1.

           IF WS-SUB1                  >  WS-MAX-PARAMETERS             00020001
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P00630'           TO WPGE-PARAGRAPH
               MOVE WS-MAX-PARAMETERS  TO WPM-MAX-PARAMETERS
               MOVE WPM-MAX-PARAMETERS-EXCEEDED
                                       TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

           MOVE WS-PARAMETER-RECORD    TO WPRA-RECORD (WS-SUB1).


      *****************************************************************
      *    READ NEXT PARAMETER RECORD                                 *
      *****************************************************************

           PERFORM  P80000-READ-PARAMETERS
               THRU P80000-READ-PARAMETERS-EXIT.

       P00630-LOAD-PARM-ARRAY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00660-EDIT-PARMS                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO EDIT THE PARAMETER RECORD SYNTAX     *
      *                                                               *
      *    CALLED BY:  P00600-PARAMETER-PROCESS                       *
      *                                                               *
      *****************************************************************

       P00660-EDIT-PARMS.

           MOVE 'N'                    TO WS-PARM-ERROR-FOUND-SW.
           MOVE WPRA-RECORD (WS-SUB1)  TO WS-PARAMETER-RECORD.

      *****************************************************************
      *    EDIT THE RECORD TYPE -  A = ADD ORDER, C = CHANGE ORDER,   *
      *    D = DELETE ORDER, S = SCENARIO NUMBER SPECIFICATION,       *
      *    U = USERID SPECIFICATION                                   *
      *****************************************************************

           IF WPR-ADD-ORDER            OR
              WPR-CHANGE-ORDER         OR
              WPR-DELETE-ORDER         OR
              WPR-SCENARIO             OR
              WPR-USERID
               NEXT SENTENCE
           ELSE                                                         00020001
               MOVE WPM-PARM-INVALID-RECORD-TYPE
                                       TO WMF-MESSAGE-AREA
               PERFORM  P00700-PARM-ERROR
                   THRU P00700-PARM-ERROR-EXIT.

      *****************************************************************
      *    FOR ACTION A= ADD ORDER, C= CHANGE ORDER, D= DELETE ORDER  *
      *    A 10 POSITION NUMERIC ORDER NUMBER IS REQUIRED             *
      *****************************************************************

           IF WPR-ADD-ORDER            OR
              WPR-CHANGE-ORDER         OR
              WPR-DELETE-ORDER
               IF  WPR-ORDER-NUMBER NUMERIC
                   NEXT SENTENCE
               ELSE
                   MOVE WPM-INVALID-ORDER-NUMBER
                                       TO WMF-MESSAGE-AREA
                   PERFORM  P00700-PARM-ERROR
                       THRU P00700-PARM-ERROR-EXIT
           ELSE                                                         00020001
                   NEXT SENTENCE.

      *****************************************************************
      *    FOR ACTION S= SCENARIO,                                    *
      *    A 3 POSITION NUMERIC SCENARIO NUMBER IS REQUIRED           *
      *****************************************************************

           IF WPR-SCENARIO
               IF (WPR-SCENARIO-NUMBER NUMERIC)    AND
                  (WPR-SCENARIO-NUMBER-R > 0)      AND
                  (WPR-SCENARIO-NUMBER-R < 251)
                   MOVE 'Y'            TO WMF-ACTIVE-SCENARIOS-R
                                             (WPR-SCENARIO-NUMBER-R)
               ELSE
                   MOVE WPM-INVALID-SCENARIO-NUMBER
                                       TO WMF-MESSAGE-AREA
                   PERFORM  P00700-PARM-ERROR
                       THRU P00700-PARM-ERROR-EXIT
           ELSE                                                         00020001
                   NEXT SENTENCE.


      *****************************************************************
      *    FOR ACTION U= USER ID SPECIFICATION, ONLY 1 USER ID PARM   *
      *    RECORD IS ALLOWED, USERID MUST BE NON-BLANK                *
      *****************************************************************

           IF WPR-USERID
               ADD +1                  TO WS-USERID-PARM-COUNT
               IF  WS-USERID-PARM-COUNT > +1
                   MOVE WPM-USERID-PARM-TOO-MANY
                                       TO WMF-MESSAGE-AREA
                   PERFORM  P00700-PARM-ERROR
                       THRU P00700-PARM-ERROR-EXIT
               ELSE
               IF  WPR-USERID-VALUE     > SPACES
                   MOVE WPR-USERID-VALUE
                                       TO WMF-USERID
               ELSE
                   MOVE WPM-INVALID-USERID
                                       TO WMF-MESSAGE-AREA
                   PERFORM  P00700-PARM-ERROR
                       THRU P00700-PARM-ERROR-EXIT
           ELSE                                                         00020001
                   NEXT SENTENCE.


      *****************************************************************
      *    IF ERROR IN THIS PARM RECORD -- FINISH DISPLAY OF ERROR    *
      *****************************************************************

           IF PARM-ERROR-FOUND
               DISPLAY WPEA-ERROR-01
               DISPLAY ' '.

       P00660-EDIT-PARMS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00700-PARM-ERROR                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS / DISPLAY PARM RECORD ERRORS*
      *                                                               *
      *    CALLED BY:  P00660-EDIT-PARMS                              *
      *                                                               *
      *****************************************************************

       P00700-PARM-ERROR.

           MOVE 'Y'                    TO WS-ERROR-FOUND-SW.

      *****************************************************************
      *    IF ERROR ALREADY ENCOUNTERED FOR THIS RECORD, JUST ADD THE *
      *    SINGLE LINE MESSAGE TO THE DISPLAY -- EXIT                 *
      *****************************************************************

           IF PARM-ERROR-FOUND
               DISPLAY WMF-MESSAGE-AREA
               GO TO P00700-PARM-ERROR-EXIT.

      *****************************************************************
      *    IF 1ST ERROR FOR THIS RECORD, DISPLAY THE ALL ASTERISK     *
      *    SINGLE LINE MESSAGE TO THE DISPLAY -- EXIT                 *
      *****************************************************************

           MOVE 'Y'                    TO WS-PARM-ERROR-FOUND-SW.
           DISPLAY ' '.
           DISPLAY WPEA-ERROR-01.
           MOVE WS-SUB1                TO WPM-RECORD-NUMBER.
           DISPLAY WPM-RECORD-NUMBER-MSG.
           DISPLAY 'PARAMETER RECORD IN ERROR FOLLOWS: '.
           DISPLAY WS-PARAMETER-RECORD.
           DISPLAY WMF-MESSAGE-AREA.

       P00700-PARM-ERROR-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01000-VERIFY-USERID                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PERFORM USERID VERIFICATION:        *
      *                                                               *
      ********** MASTER USERID TABLE PROCESS:                         *
      *                1) VERIFY USERID ( INPUT PARAMETER VALUE)      *
      *                   EXISTS IN THE USERID MASTER TABLE USING     *
      *                   STORED PROCEDURE PDASP2                     *
      *                                                               *
      ********** LOCAL USERID TABLE PROCESS:                          *
      *                1) CHECK IF USERID (INPUT PARAMETER VALUE)     *
      *                   ALREADY EXISTS                              *
      *                                                               *
      *                2) IF USERID EXISTS, UPDATE THE DATE ACCESSED  *
      *                                                               *
      *                3) IF USERID DOES NOT EXIST, OBTAIN THE NEXT   *
      *                   USERID UNIQUE IDENTIFIER, INSERT THE USERID *
      *                   ROW.                                        *
      *                                                               *
      *                4) ON EITHER ADD OR CHANGE SET THE SCENARIO    *
      *                   INDICATORS BASED ON INPUT PARAMETER VALUES  *
      *                                                               *
      *    CALLED BY:  P00500-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P01000-VERIFY-USERID.

      *****************************************************************
      *    VERIFY USER IN THE USER ID MASTER TABLE VIA DB2            *
      *    STORED PROCEDURE PDASP2                                    *
      *****************************************************************

           MOVE WMF-USERID              TO PDASP2-USERID.
           MOVE ZEROS                   TO PDASP2-USERID-NUMBER.
           MOVE SPACES                  TO PDASP2-ACTIVE-SCENARIOS
                                           PDASP2-STATUS.

           EXEC SQL
               CALL PDAPROD.PDASP2 (:PDASP2-USERID,
                                    :PDASP2-USERID-NUMBER,
                                    :PDASP2-ACTIVE-SCENARIOS,
                                    :PDASP2-STATUS)
           END-EXEC.

      *****************************************************************
      *    RETURN ZERO (SUCCESS), 100 (NOT FOUND), ANY OTHER IS ERROR *
      *****************************************************************

           IF PDASP2-STATUS            =  ZEROES
               NEXT SENTENCE
           ELSE
           IF PDASP2-STATUS            =  '0100'
               MOVE WMF-USERID         TO WPM-USERID-VALUE
               MOVE WPM-USERID-NOT-FOUND
                                       TO WMF-MESSAGE-AREA
               PERFORM  P99400-ERROR-ROUTINE
                   THRU P99400-ERROR-ROUTINE-EXIT
               GO TO P01000-VERIFY-USERID-EXIT
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P01000'           TO WPGE-PARAGRAPH
               MOVE 'PDASP2'           TO WPM-PROGRAM-NAME
               MOVE PDASP2-STATUS      TO WPM-RETURN-CODE
               MOVE WPM-PROGRAM-ERROR  TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    CHECK IF USERID EXISTS IN THE USERID DB2 TABLE (LOCALLY)   *
      *                                                               *
      *    1) IF EXISTS, UPDATE USERID TABLE LAST ACCESSED DATE       *
      *    2) IF NOT EXISTS, PERFORM USERID ADD PROCESS               *
      *    3) IN EITHER CASE SET SCENARIO INDICATORS FROM PARAMETER   *
      *       INPUT RECORDS SPECIFICATIONS                            *
      *****************************************************************

           EXEC SQL SELECT    ID,
                              NUMBER,
                              ACTIVE_SCENARIOS

                    INTO      :USERID-ID,
                              :USERID-NUMBER,
                              :USERID-ACTIVE-SCENARIOS

                    FROM      USERID

                    WHERE     ID = :WMF-USERID
           END-EXEC.


      *****************************************************************
      *    IF USER FOUND     ... PERFORM USER UPDATE                  *
      *    IF USER NOT FOUND ... PERFORM USER ADD                     *
      *    OTHERWISE         ... ERROR                                *
      *****************************************************************

           IF SQLCODE                  =  ZEROES
               PERFORM  P01100-UPDATE-USERID
                   THRU P01100-UPDATE-USERID-EXIT
           ELSE
           IF SQLCODE                  =  +100
               PERFORM  P01200-ADD-USERID
                   THRU P01200-ADD-USERID-EXIT
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'SELECT ID FROM USERID'
                                       TO WPDE-FUNCTION
               MOVE 'P01000'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P01000-VERIFY-USERID-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01100-UPDATE-USERID                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO UPDATE THE USERID DB2 TABLE         *
      *                LAST ACCESSED DATE AND ACTIVE SCENARIOS        *
      *                COLUMNS FOR THE USER                           *
      *                                                               *
      *    CALLED BY:  P01000-VERIFY-USERID                           *
      *                                                               *
      *****************************************************************

       P01100-UPDATE-USERID.

      *****************************************************************
      *    UPDATE THE USERID LAST ACCESSED DATE COLUMN WITH THE       *
      *    CURRENT DATE, AND THE ACTIVE SCENARIOS COLUMN WITH THE     *
      *    PARAMETER INPUT SCENARIO SPECIFICATIONS                    *
      *****************************************************************

           EXEC SQL UPDATE  USERID
                    SET  LAST_ACCESSED     =  CURRENT DATE,
                         ACTIVE_SCENARIOS  = :WMF-ACTIVE-SCENARIOS

                    WHERE   ID             = :WMF-USERID
           END-EXEC.


      *****************************************************************
      *    RETURN CODE OTHER THAN ZEROES IS AN ERROR                  *
      *****************************************************************

           IF SQLCODE                  = ZEROES
               NEXT SENTENCE
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'UPDATE USERID - LAST_ACCESSED'
                                       TO WPDE-FUNCTION
               MOVE 'P01100'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P01100-UPDATE-USERID-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01200-ADD-USERID                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO ADD A USERID TO THE USERID DB2 TABLE*
      *                                                               *
      *                1) DETERMINE NEXT AVAILABLE UNIQUE IDENTIFIER  *
      *                   FROM USERID TABLE (MAX COLUMN FUNCTION)     *
      *                                                               *
      *                2) INSERT USERID ROW                           *
      *                                                               *
      *                                                               *
      *    CALLED BY:  P01000-VERIFY-USERID                           *
      *                                                               *
      *****************************************************************

       P01200-ADD-USERID.

      *****************************************************************
      *    LOCK THE USERID TABLE IN SHARE MODE FOR NEW USER ADD       *
      *****************************************************************

           EXEC SQL LOCK TABLE USERID IN SHARE MODE
           END-EXEC.


           IF SQLCODE                  =  ZEROES
               NEXT SENTENCE
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'LOCK TABLE USERID'
                                       TO WPDE-FUNCTION
               MOVE 'P01200'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    OBTAIN THE MAXIMUM USERID NUMBER CURRENTLY ON FILE         *
      *****************************************************************

           EXEC SQL SELECT  MAX(NUMBER)
                    INTO    :WMF-USERID-NUMBER    :WMF-NULL-IND
                    FROM    USERID
           END-EXEC.


      *****************************************************************
      *    FOR THIS BATCH PROCESS SET USERID NUMBER TO 0 IN ALL CASES *
      *****************************************************************

           IF SQLCODE                  =  ZEROES
               IF WMF-NULL-IND         <  ZEROES
                   MOVE ZEROES         TO WMF-USERID-NUMBER
               ELSE
                   MOVE ZEROES         TO WMF-USERID-NUMBER
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'SELECT MAX(NUMBER) FROM USERID'
                                       TO WPDE-FUNCTION
               MOVE 'P01200'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    ADD (INSERT) THE USERID ROW INTO THE TABLE                 *
      *****************************************************************

           EXEC SQL INSERT  INTO  USERID
                            (ID,
                             NUMBER,
                             LAST_ACCESSED,
                             ACTIVE_SCENARIOS)
                    VALUES  (:WMF-USERID,
                             :WMF-USERID-NUMBER,
                             CURRENT DATE,
                             :WMF-ACTIVE-SCENARIOS)
           END-EXEC.


      *****************************************************************
      *    RETURN CODE OTHER THAN ZERO IS AN ERROR                    *
      *****************************************************************

           IF SQLCODE                  =  ZEROES
               NEXT SENTENCE
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'INSERT INTO USERID'
                                       TO WPDE-FUNCTION
               MOVE 'P01200'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P01200-ADD-USERID-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P02000-ORDER-PROCESS                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE HIGH LEVEL ORDER        *
      *                PROCESSING                                     *
      *                                                               *
      *    CALLED BY:  P00500-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P02000-ORDER-PROCESS.

      *****************************************************************
      *    PERFORM THE ORDER PROCESSING REQUESTS (PARM RECORD INPUT)  *
      *****************************************************************

           PERFORM  P02030-PROCESS-ORDER-PARM
               THRU P02030-PROCESS-ORDER-PARM-EXIT
                   VARYING WS-SUB1 FROM +1 BY +1
                       UNTIL WS-SUB1 > WS-PARAMETER-RECORDS-IN.


       P02000-ORDER-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P02030-PROCESS-ORDER-PARM                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS ALL ORDER RELATED INPUT     *
      *                PARAMETER PROCESSING. ORDER ADD, CHANGE, AND   *
      *                DELETE PROCESSING ARE PERFORMED.               *
      *                                                               *
      *    CALLED BY:  P02000-ORDER-PROCESS                           *
      *                                                               *
      *****************************************************************

       P02030-PROCESS-ORDER-PARM.

      *****************************************************************
      *    BASED ON ACTION CODE PERFORM ADD, CHANGE, DELETE FUNCTIONS *
      *****************************************************************

           MOVE WPRA-RECORD (WS-SUB1)  TO WS-PARAMETER-RECORD.

           IF WPR-CHANGE-ORDER
               PERFORM  P02200-ORDER-CHANGE
                   THRU P02200-ORDER-CHANGE-EXIT
           ELSE

           IF WPR-DELETE-ORDER
               PERFORM  P03000-ORDER-DELETE
                   THRU P03000-ORDER-DELETE-EXIT
           ELSE

           IF WPR-ADD-ORDER
               PERFORM  P03600-ORDER-ADD
                   THRU P03600-ORDER-ADD-EXIT
           ELSE

               NEXT SENTENCE.


       P02030-PROCESS-ORDER-PARM-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P02200-ORDER-CHANGE                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PERFORM THE ORDER CHANGE PROCESS.   *
      *                THE FUNCTIONALITY IS SIMILAR TO THE CICS       *
      *                APPLICATION ORDER CHANGE PROCESS, THE ORDER    *
      *                DATE IS SET TO THE CURRENT DATE.               *
      *                                                               *
      *    CALLED BY:  P02030-PROCESS-ORDER-PARM                      *
      *                                                               *
      *****************************************************************

       P02200-ORDER-CHANGE.

      *****************************************************************
      *    RETRIEVE THE ORDER FOR UPDATE -- NOT FOUND IS AN ERROR     *
      *****************************************************************

           MOVE ZEROES                 TO OSQ-ORDER-PREFIX.
           MOVE WPR-ORDER-NUMBER       TO OSQ-ORDER-NUMBER.

           PERFORM  P80100-GHU-ORDER
               THRU P80100-GHU-ORDER-EXIT.
                                                                        13580000
           IF OP-STATUS                = SPACES                         13590000
               NEXT SENTENCE                                            13600000
           ELSE                                                         13610000
               DISPLAY WPM-BLANK
               DISPLAY WPM-ALL-ASTERISK
               MOVE WPR-ORDER-NUMBER   TO WPM-ORDER-NUMBER-CHG
               DISPLAY WPM-ORDER-NOT-FOUND-CHANGE
               DISPLAY WPM-ALL-ASTERISK
               DISPLAY WPM-BLANK
               GO TO P02200-ORDER-CHANGE-EXIT
           END-IF.                                                      13710000


      *****************************************************************
      *    UPDATE ORDER DATE WITH CURRENT SYSTEM DATE -- YYMMDD       *
      *****************************************************************

           MOVE WS-CDT-D-YEAR          TO WMF-DATE-YY.
           MOVE WS-CDT-D-MONTH         TO WMF-DATE-MM.
           MOVE WS-CDT-D-DAY           TO WMF-DATE-DD.
           MOVE WMF-DATE-YYMMDD        TO ORDER-DATE-YYMMDD.


      *****************************************************************
      *    REPLACE THE ORDER ROOT SEGMENT                             *
      *****************************************************************

           PERFORM  P80300-REPL-ORDER
               THRU P80300-REPL-ORDER-EXIT.


       P02200-ORDER-CHANGE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03000-ORDER-DELETE                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PERFORM THE ORDER DELETE PROCESS.   *
      *                THE FUNCTIONALITY IS SIMILAR TO THE CICS       *
      *                APPLICATION ORDER DELETE PROCESS.              *
      *                                                               *
      *    CALLED BY:  P02030-PROCESS-ORDER-PARM                      *
      *                                                               *
      *****************************************************************

       P03000-ORDER-DELETE.

      *****************************************************************
      *    RETRIEVE THE ORDER FOR UPDATE -- NOT FOUND IS AN ERROR     *
      *****************************************************************

           MOVE ZEROES                 TO OSQ-ORDER-PREFIX.
           MOVE WPR-ORDER-NUMBER       TO OSQ-ORDER-NUMBER.

           PERFORM  P80100-GHU-ORDER
               THRU P80100-GHU-ORDER-EXIT.
                                                                        13580000
           IF OP-STATUS                = SPACES                         13590000
               NEXT SENTENCE                                            13600000
           ELSE                                                         13610000
               DISPLAY WPM-BLANK
               DISPLAY WPM-ALL-ASTERISK
               MOVE WPR-ORDER-NUMBER   TO WPM-ORDER-NUMBER-DEL
               DISPLAY WPM-ORDER-NOT-FOUND-DELETE
               DISPLAY WPM-ALL-ASTERISK
               DISPLAY WPM-BLANK
               GO TO P03000-ORDER-DELETE-EXIT
           END-IF.                                                      13710000


      *****************************************************************
      *    DELETE THE ORDER ROOT SEGMENT                              *
      *****************************************************************

           PERFORM  P80400-DLET-ORDER
               THRU P80400-DLET-ORDER-EXIT.


       P03000-ORDER-DELETE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03600-ORDER-ADD                               *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PERFORM THE ORDER ADD PROCESS.      *
      *                THE FUNCTIONALITY IS SIMILAR TO THE CICS       *
      *                APPLICATION ORDER ADD PROCESS                  *
      *                                                               *
      *    CALLED BY:  P02030-PROCESS-ORDER-PARM                      *
      *                                                               *
      *****************************************************************

       P03600-ORDER-ADD.

      *****************************************************************
      *    IF ORDER EXISTS, DELETE IT, THEN BUILD NEW ORDER (INSERT)  *
      *****************************************************************

           MOVE ZEROES                 TO OSQ-ORDER-PREFIX.
           MOVE WPR-ORDER-NUMBER       TO OSQ-ORDER-NUMBER.

           PERFORM  P80100-GHU-ORDER
               THRU P80100-GHU-ORDER-EXIT.
                                                                        13580000
           IF OP-STATUS                = SPACES                         13590000
               PERFORM  P80400-DLET-ORDER
                   THRU P80400-DLET-ORDER-EXIT
           END-IF.                                                      13710000


      *****************************************************************
      *    ESTABLISH THE CUSTOMER FOR THE ORDER ADD                   *
      *****************************************************************

           PERFORM  P03700-CUSTOMER-PROCESS
               THRU P03700-CUSTOMER-PROCESS-EXIT.
                                                                        TESTING
           IF ERROR-FOUND                                               TESTING
               GO TO P03600-ORDER-ADD-EXIT.


      *****************************************************************
      *    ESTABLISH THE PARTS CATEGORIES / SUB-CATEGORIES FOR THE ADD*
      *****************************************************************

           PERFORM  P05000-LOAD-WORK-ARRAY
               THRU P05000-LOAD-WORK-ARRAY-EXIT.
                                                                        TESTING
           IF ERROR-FOUND                                               TESTING
               GO TO P03600-ORDER-ADD-EXIT.


      *****************************************************************
      *    PROCESS THE ITEMS FOR THE SELECTED CATEGORY                *
      *****************************************************************

           PERFORM  P06000-ITEMS-BY-CATEGORY
               THRU P06000-ITEMS-BY-CATEGORY-EXIT.
                                                                        TESTING
           IF ERROR-FOUND                                               TESTING
               GO TO P03600-ORDER-ADD-EXIT.


      *****************************************************************
      *    LOCATE SUPPLIERS FOR THE SELECTED ITEM                     *
      *****************************************************************

           PERFORM  P08000-ITEM-SUPPLIERS
               THRU P08000-ITEM-SUPPLIERS-EXIT.
                                                                        TESTING
           IF ERROR-FOUND                                               TESTING
               GO TO P03600-ORDER-ADD-EXIT.


      *****************************************************************
      *    CREATE THE ORDER IN THE IMS ORDER DATA BASE FROM THE       *
      *    VSAM PENDING ORDER FILE ITEMS                              *
      *****************************************************************

           PERFORM  P09000-CREATE-ORDER
               THRU P09000-CREATE-ORDER-EXIT.
                                                                        TESTING
           IF ERROR-FOUND                                               TESTING
               GO TO P03600-ORDER-ADD-EXIT.


      *****************************************************************
      *    IF SCENARIO 12, INEFFICIENT IMS/DLI PROCESSING, PERFORM    *
      *    THE SCENARIO SPECIAL PROCESSING                            *
      *****************************************************************

           IF WMF-ACTIVE-SCENARIOS-R (12) = 'Y'
               PERFORM  P09900-SCAN-ORDERS
                   THRU P09900-SCAN-ORDERS-EXIT
                       VARYING WS-SUB2 FROM +1 BY +1                        TEST
                           UNTIL WS-SUB2 > +1000.                           TEST
                                                                        TESTING

       P03600-ORDER-ADD-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03700-CUSTOMER-PROCESS                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO ESTABLISH A CUSTOMER FOR THE NEW    *
      *                ORDER. -ARROW- HAS BEEN ARBITRARILY SELECTED   *
      *                FOR NEW ORDERS.                                *
      *                                                               *
      *                SCENARIO 3 - A CUSTOMER NOT FOUND IS ALSO      *
      *                EXECUTED IF THE SCENARIO HAS BEEN ACTIVATED    *
      *                VIA A PROGRAM PARAMETER.                       *
      *                                                               *
      *    CALLED BY:  P03600-ORDER-ADD                               *
      *                                                               *
      *****************************************************************

       P03700-CUSTOMER-PROCESS.

      *****************************************************************
      *    CHECK FOR SCENARIO 3, CUSTOMER NOT FOUND (VSAM READ)       *
      *****************************************************************

           IF WMF-ACTIVE-SCENARIOS-R (3) = 'Y'
               MOVE 88888              TO CUSTOMER-PREFIX
               MOVE WMF-CUSTOMER-ID    TO CUSTOMER-ID
                                                                        13580000
               PERFORM  P80600-READ-CUSTOMER
                   THRU P80600-READ-CUSTOMER-EXIT
                                                                        13580000
           END-IF.                                                      13710000

      *****************************************************************
      *    READ CUSTOMER USING THE APPROPRIATE KEY (VSAM READ)        *
      *****************************************************************

           MOVE ZEROES                 TO CUSTOMER-PREFIX.
           MOVE WMF-CUSTOMER-ID        TO CUSTOMER-ID.
                                                                        13580000
           PERFORM  P80600-READ-CUSTOMER
               THRU P80600-READ-CUSTOMER-EXIT.

      *****************************************************************
      *    READ SUCCESSFUL (00) IS THE ONLY ACCEPTABLE STATUS,        *
      *    OTHERWISE FORMAT ERROR AND TERMINATE PROGRAM               *
      *****************************************************************

           IF WMF-CUSTOMR-STATUS = '00'                                 00020001
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P03700'           TO WPGE-PARAGRAPH
               MOVE 'VCUSTOMR'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-CUSTOMR-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'READ'             TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P03700-CUSTOMER-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05000-LOAD-WORK-ARRAY                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO LOAD A WORK VERSION OF THE          *
      *                CATEGORY / SUB-CATEGORY ARRAY FROM THE         *
      *                STANDARD COPYBOOK CATEGORY ARRAY               *
      *                (BEING DONE FOR DEMONSTRATION PURPOSES ONLY)   *
      *                                                               *
      *    CALLED BY:  P03600-ORDER-ADD                               *
      *                                                               *
      *****************************************************************

       P05000-LOAD-WORK-ARRAY.

      *****************************************************************
      *    COUNT THE CATEGORY / SUB-CATEGORY COMBINATIONS             *
      *****************************************************************

           MOVE ZEROES                 TO WS-COUNT.

           PERFORM  P05020-COUNT-CATEGORY
               THRU P05020-COUNT-CATEGORY-EXIT
                   VARYING WS-CAT-SUB FROM +1 BY +1
                       UNTIL WS-CAT-SUB > PDA-CATEGORY-MAX.


           IF WS-COUNT                 >  WPCA-CATEGORY-MAX
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P05000'           TO WPGE-PARAGRAPH
               MOVE 'WS-PDA-CATEGORY-ARRAY'
                                       TO WPM-TABLE-NAME
               MOVE WPM-TABLE-OVERFLOW TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT
           ELSE
               NEXT SENTENCE.


      *****************************************************************
      *    INITIALIZE WORK ARRAY, PROCESS ALL THE CATEGORIES          *
      *****************************************************************

           MOVE WS-COUNT               TO WPCA-CATEGORY-COUNT.
           SET  WPCA-CAT-IX            TO 1.

           PERFORM  P05050-LOAD-CATEGORY
               THRU P05050-LOAD-CATEGORY-EXIT
                   VARYING WS-CAT-SUB FROM +1 BY +1
                       UNTIL WS-CAT-SUB > PDA-CATEGORY-MAX.

       P05000-LOAD-WORK-ARRAY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05020-COUNT-CATEGORY                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO COUNT THE TOTAL NUMBER OF           *
      *                CATEGORY / SUB-CATEGORY COMBINATIONS           *
      *                TO BE LOADED INTO THE WORK ARRAY               *
      *                (BEING DONE FOR DEMONSTRATION PURPOSES ONLY)   *
      *                                                               *
      *    CALLED BY:  P05000-LOAD-WORK-ARRAY                         *
      *                                                               *
      *****************************************************************

       P05020-COUNT-CATEGORY.


           COMPUTE WS-COUNT            =
                   WS-COUNT + PCAR-SUB-CATEGORY-COUNT (WS-CAT-SUB).


       P05020-COUNT-CATEGORY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05050-LOAD-CATEGORY                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO LOAD CATEGORY INFORMATION INTO THE  *
      *                WORK ARRAY, AND CONTROL THE SUB-CATEGORY       *
      *                PROCESSING                                     *
      *                                                               *
      *    CALLED BY:  P05000-LOAD-WORK-ARRAY                         *
      *                                                               *
      *****************************************************************

       P05050-LOAD-CATEGORY.


      *****************************************************************
      *    D E M O N S T R A T I O N    P U R P O S E S   O N L Y     *
      *    THIS ARRAY NOT USED IN THE APPLICATION                     *
      *****************************************************************

           MOVE PCAR-SUB-CATEGORY-COUNT (WS-CAT-SUB)
                                       TO WSCA-SUB-CATEGORY-COUNT.
           SET  WSCA-SUBCAT-IX         TO 1.


      *****************************************************************
      *    LOAD THE SUB-CATEGORY INFORMATION OF THE ARRAY             *
      *****************************************************************

           PERFORM  P05100-LOAD-SUB-CATEGORY
               THRU P05100-LOAD-SUB-CATEGORY-EXIT
                   VARYING WS-SUBCAT-SUB FROM +1 BY +1
                       UNTIL WS-SUBCAT-SUB > PCAR-SUB-CATEGORY-COUNT
                                                     (WS-CAT-SUB).

       P05050-LOAD-CATEGORY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05100-LOAD-SUB-CATEGORY                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO LOAD THE SUB-CATEGORY PORTION OF    *
      *                THE WORK ARRAY                                 *
      *                                                               *
      *    CALLED BY:  P05050-LOAD-CATEGORY                           *
      *                                                               *
      *****************************************************************

       P05100-LOAD-SUB-CATEGORY.

      *****************************************************************
      *    IF SCENARIO 9 ACTIVE, MAKE UNNECESSARY REQUESTS FOR        *
      *    SYSTEM DATE / TIME                                         *
      *****************************************************************

           IF WMF-ACTIVE-SCENARIOS-R (9) = 'Y'
               MOVE FUNCTION CURRENT-DATE
                                       TO WS-CURRENT-DATE-TIME
           END-IF.                                                      13710000

      *****************************************************************
      *    FORMAT THE CATEGORY AND SUB-CATEGORY INFORMATION           *
      *****************************************************************

           MOVE PCAR-CATEGORY (WS-CAT-SUB)
                                       TO WPCA-CATEGORY (WPCA-CAT-IX).
           MOVE PCAR-SUB-CATEGORY      (WS-CAT-SUB,  WS-SUBCAT-SUB)
               TO WPCA-SUB-CATEGORY    (WPCA-CAT-IX).


           IF WPCA-CAT-IX  <  WPCA-CATEGORY-COUNT
               SET WPCA-CAT-IX             UP BY 1.


      *****************************************************************
      *    D E M O N S T R A T I O N    P U R P O S E S   O N L Y     *
      *    THIS ARRAY NOT USED IN THE APPLICATION                     *
      *****************************************************************

           MOVE PCAR-SUB-CATEGORY      (WS-CAT-SUB,  WS-SUBCAT-SUB)
               TO WSCA-SUB-CATEGORY    (WSCA-SUBCAT-IX).

           IF WSCA-SUBCAT-IX  <  PCAR-SUB-CATEGORY-COUNT (WS-CAT-SUB)
               SET  WSCA-SUBCAT-IX     UP BY 1.


       P05100-LOAD-SUB-CATEGORY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06000-ITEMS-BY-CATEGORY                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS THE ITEMS FOR THE SELECTED  *
      *                CATEGORY / SUB-CATEGORY COMBINATION.           *
      *                                                               *
      *    CALLED BY:  P03600-ORDER-ADD                               *
      *                                                               *
      *****************************************************************

       P06000-ITEMS-BY-CATEGORY.

      *****************************************************************
      *    CHECK FOR ACTIVATED SCENARIO PROCESSING                    *
      *    SCENARIO 10 - INEFFICIENT SQL QUERY (WHERE CLAUSE)         *
      *    SCENARIO 11 - INEFFICIENT SQL QUERY (ORDER BY CLAUSE)      *
      *****************************************************************

           IF WMF-ACTIVE-SCENARIOS-R (10) = 'Y'
               MOVE ZEROES             TO ITEM-PREFIX
               MOVE WMF-CATEGORY       TO ITEM-CATEGORY-NAME
               PERFORM P06200-VERIFY-CATEGORY
                  THRU P06200-VERIFY-CATEGORY-EXIT
           END-IF.                                                      13710000

           IF WMF-ACTIVE-SCENARIOS-R (11) = 'Y'
               MOVE ZEROES             TO ITEM-PREFIX
               PERFORM P06400-VERIFY-SUBCATEGORY
                  THRU P06400-VERIFY-SUBCATEGORY-EXIT
           END-IF.                                                      13710000


      *****************************************************************
      *    PROCESS ITEMS BY CATEGORY VIA A CURSOR AGAINST THE ITEM    *
      *    TABLE. ALL ITEMS SATISFYING SELECTION CRITERIA WILL BE     *
      *    PROCESSED.                                                 *
      *****************************************************************

           MOVE WMF-CATEGORY       TO ITEM-CATEGORY-NAME.
           MOVE WMF-SUB-CATEGORY   TO ITEM-SUB-CATEGORY-NAME.
           MOVE ZEROES             TO ITEM-PREFIX.
           MOVE SPACES             TO ITEM-NUMBER.

           PERFORM P06600-SCROLL-FORWARD
              THRU P06600-SCROLL-FORWARD-EXIT.


       P06000-ITEMS-BY-CATEGORY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06200-VERIFY-CATEGORY                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO VALIDATE THE CATEGORY               *
      *                                                               *
      *    CALLED BY:  P06000-ITEMS-BY-CATEGORY                       *
      *                                                               *
      *****************************************************************

       P06200-VERIFY-CATEGORY.

           PERFORM P06210-OPEN-CATEGORY-CSR
              THRU P06210-OPEN-CATEGORY-CSR-EXIT.


      *****************************************************************
      *    PROCESS ITEMS FOR THE SELECTED CATEGORY                    *
      *****************************************************************

           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.

           PERFORM P06220-FETCH-CATEGORY
              THRU P06220-FETCH-CATEGORY-EXIT
                  UNTIL PROCESS-COMPLETE.


           PERFORM P06230-CLOSE-CATEGORY-CSR
              THRU P06230-CLOSE-CATEGORY-CSR-EXIT.

       P06200-VERIFY-CATEGORY-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06210-OPEN-CATEGORY-CSR                       *
      *                                                               *
      *    FUNCTION :  OPENS CURSOR USED TO VERIFY CATEGORY           *
      *                                                               *
      *    CALLED BY:  P06200-VERIFY-CATEGORY                         *
      *                                                               *
      *****************************************************************

       P06210-OPEN-CATEGORY-CSR.

           EXEC SQL
               OPEN CATEGORY
           END-EXEC.


           IF SQLCODE                  =  ZEROES
               NEXT SENTENCE
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'OPEN CATEGORY CURSOR'
                                       TO WPDE-FUNCTION
               MOVE 'P06210'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P06210-OPEN-CATEGORY-CSR-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06220-FETCH-CATEGORY                          *
      *                                                               *
      *    FUNCTION :  FETCHS THE ITEM ROW FOR THE SELECTED CATEGORY  *
      *                                                               *
      *    CALLED BY:  P06200-VERIFY-CATEGORY                         *
      *                                                               *
      *****************************************************************

       P06220-FETCH-CATEGORY.

           EXEC SQL
               FETCH  CATEGORY
                INTO  :ITEM-NUMBER,
                      :ITEM-NAME
           END-EXEC.


           IF SQLCODE                  = ZEROS
               NEXT SENTENCE
           ELSE
           IF SQLCODE                  = +100
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'FETCH CATEGORY CURSOR'
                                       TO WPDE-FUNCTION
               MOVE 'P06220'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P06220-FETCH-CATEGORY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06230-CLOSE-CATEGORY-CSR                      *
      *                                                               *
      *    FUNCTION :  CLOSES CURSOR USED TO VERIFY CATEGORY          *
      *                                                               *
      *    CALLED BY:  P06200-VERIFY-CATEGORY                         *
      *                                                               *
      *****************************************************************

       P06230-CLOSE-CATEGORY-CSR.

           EXEC SQL
               CLOSE CATEGORY
           END-EXEC.


           IF SQLCODE                  = ZEROS
               NEXT SENTENCE
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'CLOSE CATEGORY CURSOR'
                                       TO WPDE-FUNCTION
               MOVE 'P06230'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P06230-CLOSE-CATEGORY-CSR-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06400-VERIFY-SUBCATEGORY                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO VALIDATE THE SUB-CATEGORY           *
      *                                                               *
      *    CALLED BY:  P06000-ITEMS-BY-CATEGORY                       *
      *                                                               *
      *****************************************************************

       P06400-VERIFY-SUBCATEGORY.

           PERFORM P06410-OPEN-SUBCAT-CSR
              THRU P06410-OPEN-SUBCAT-CSR-EXIT.


      *****************************************************************
      *    PROCESS ITEMS, ORDER BY SUB-CATEGORY                       *
      *****************************************************************

           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.

           PERFORM P06420-FETCH-SUBCAT
              THRU P06420-FETCH-SUBCAT-EXIT
                  UNTIL PROCESS-COMPLETE.


           PERFORM P06430-CLOSE-SUBCAT-CSR
              THRU P06430-CLOSE-SUBCAT-CSR-EXIT.

       P06400-VERIFY-SUBCATEGORY-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06410-OPEN-SUBCAT-CSR                         *
      *                                                               *
      *    FUNCTION :  OPENS CURSOR USED TO PROCESS SUB-CATEGORY      *
      *                                                               *
      *    CALLED BY:  P06400-VERIFY-SUBCATEGORY                      *
      *                                                               *
      *****************************************************************

       P06410-OPEN-SUBCAT-CSR.

           EXEC SQL
               OPEN SUBCAT
           END-EXEC.


           IF SQLCODE                  = ZEROS
               NEXT SENTENCE
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'OPEN SUBCAT CURSOR'
                                       TO WPDE-FUNCTION
               MOVE 'P06410'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P06410-OPEN-SUBCAT-CSR-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06420-FETCH-SUBCAT                            *
      *                                                               *
      *    FUNCTION :  FETCHS THE ITEMS, ORDER BY SUB-CATEGORY        *
      *                                                               *
      *    CALLED BY:  P06400-VERIFY-SUBCATEGORY                      *
      *                                                               *
      *****************************************************************

       P06420-FETCH-SUBCAT.

           EXEC SQL
               FETCH  SUBCAT
                INTO  :ITEM-NUMBER,
                      :ITEM-NAME
           END-EXEC.


           IF SQLCODE                  = ZEROS
               NEXT SENTENCE
           ELSE
           IF SQLCODE                  = +100
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'FETCH SUBCAT CURSOR'
                                       TO WPDE-FUNCTION
               MOVE 'P06420'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P06420-FETCH-SUBCAT-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06430-CLOSE-SUBCAT-CSR                        *
      *                                                               *
      *    FUNCTION :  CLOSES ITEM CURSOR, SUB-CATEGORY SEQUENCE      *
      *                                                               *
      *    CALLED BY:  P06400-VERIFY-SUBCATEGORY                      *
      *                                                               *
      *****************************************************************

       P06430-CLOSE-SUBCAT-CSR.

           EXEC SQL
               CLOSE SUBCAT
           END-EXEC.


           IF SQLCODE                  = ZEROS
               NEXT SENTENCE
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'CLOSE SUBCAT CURSOR'
                                       TO WPDE-FUNCTION
               MOVE 'P06430'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P06430-CLOSE-SUBCAT-CSR-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06600-SCROLL-FORWARD                          *
      *                                                               *
      *    FUNCTION :  PROCESS ALL ITEMS FOR THE CATEGORY /           *
      *                SUB-CATEGORY FROM LOW ITEM SEQUENCE -- FORWARD *
      *                                                               *
      *    CALLED BY:  P06000-ITEMS-BY-CATEGORY                       *
      *                                                               *
      *****************************************************************

       P06600-SCROLL-FORWARD.

           PERFORM  P06630-OPEN-FORW-CURSOR
               THRU P06630-OPEN-FORW-CURSOR-EXIT.

      *****************************************************************
      *    PROCESS ITEMS FOR CATEGORY / SUB-CATEGORY COMBINATION      *
      *****************************************************************

           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.

           PERFORM P06660-FETCH-FORW-ITEMS
              THRU P06660-FETCH-FORW-ITEMS-EXIT
                  UNTIL PROCESS-COMPLETE.


           PERFORM  P06700-CLOSE-FORW-CURSOR
               THRU P06700-CLOSE-FORW-CURSOR-EXIT.


           MOVE ' SC N S  N C'         TO VENDOR-ITEM-STATUS-CODE-GRP.
           PERFORM  P06800-LOAD-STATUS-ARRAYS
               THRU P06800-LOAD-STATUS-ARRAYS-EXIT.

       P06600-SCROLL-FORWARD-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06630-OPEN-FORW-CURSOR                        *
      *                                                               *
      *    FUNCTION :  OPENS ITEM CURSOR FOR CATEGORY / SUB-CATEGORY  *
      *                                                               *
      *    CALLED BY:  P06600-SCROLL-FORWARD                          *
      *                                                               *
      *****************************************************************

       P06630-OPEN-FORW-CURSOR.

           EXEC SQL
               OPEN ITEMFORW
           END-EXEC.


           IF SQLCODE                  = ZEROS
               NEXT SENTENCE
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'OPEN FORWARD CURSOR'
                                       TO WPDE-FUNCTION
               MOVE 'P06630'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P06630-OPEN-FORW-CURSOR-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06660-FETCH-FORW-ITEMS                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE ITEM FETCHES            *
      *                                                               *
      *    CALLED BY:  P06600-SCROLL-FORWARD                          *
      *                                                               *
      *****************************************************************

       P06660-FETCH-FORW-ITEMS.

      *****************************************************************
      *    IF SCENARIO 5 ACTIVE, EXECUTE A DIFFERENT FETCH            *
      *    (GENERATES A -303 SQLCODE, FETCH INTO INCOMPATABLE COLUMN) *
      *****************************************************************

           IF WMF-ACTIVE-SCENARIOS-R (05) = 'Y'
               PERFORM  P06680-FETCH-FORWARD-ROW
                   THRU P06680-FETCH-FORWARD-ROW-EXIT.

           PERFORM P06690-FETCH-FORWARD-ROW
              THRU P06690-FETCH-FORWARD-ROW-EXIT.


       P06660-FETCH-FORW-ITEMS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06680-FETCH-FORWARD-ROW                       *
      *                                                               *
      *    FUNCTION :  FETCH ROW FROM FORWARD CURSOR USING AN         *
      *                INCOMPATABLE COLUMN TO FORCE SQLCODE -303      *
      *                                                               *
      *    CALLED BY:  P06660-FETCH-FORW-ITEMS                        *
      *                                                               *
      *****************************************************************

       P06680-FETCH-FORWARD-ROW.

           EXEC SQL
               FETCH  ITEMFORW
                INTO  :WMF-ITEM-NUMBER,
                      :ITEM-NAME
           END-EXEC.


           IF SQLCODE                  = ZEROS
               NEXT SENTENCE
           ELSE
           IF SQLCODE                  = +100
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'FETCH ITEMFORW CURSOR'
                                       TO WPDE-FUNCTION
               MOVE 'P06680'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P06680-FETCH-FORWARD-ROW-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06690-FETCH-FORWARD-ROW                       *
      *                                                               *
      *    FUNCTION :  FETCH ROW FROM FORWARD CURSOR                  *
      *                                                               *
      *    CALLED BY:  P06660-FETCH-FORW-ITEMS                        *
      *                                                               *
      *****************************************************************

       P06690-FETCH-FORWARD-ROW.

           EXEC SQL
               FETCH  ITEMFORW
                INTO  :ITEM-NUMBER,
                      :ITEM-NAME
           END-EXEC.


           IF SQLCODE                  = ZEROS
               NEXT SENTENCE
           ELSE
           IF SQLCODE                  = +100
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'FETCH ITEMFORW CURSOR'
                                       TO WPDE-FUNCTION
               MOVE 'P06690'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P06690-FETCH-FORWARD-ROW-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06700-CLOSE-FORW-CURSOR                       *
      *                                                               *
      *    FUNCTION :  CLOSES ITEM CURSOR                             *
      *                                                               *
      *    CALLED BY:  P06600-SCROLL-FORWARD                          *
      *                                                               *
      *****************************************************************

       P06700-CLOSE-FORW-CURSOR.

           EXEC SQL
               CLOSE ITEMFORW
           END-EXEC.


           IF SQLCODE                  = ZEROS
               NEXT SENTENCE
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'CLOSE FORW CURSOR'
                                       TO WPDE-FUNCTION
               MOVE 'P06700'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P06700-CLOSE-FORW-CURSOR-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06800-LOAD-STATUS-ARRAYS                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO LOAD THE ITEM STATUS ARRAYS IN BOTH *
      *                THE LINKAGE AND WORKING STORAGE SECTIONS. THE  *
      *                VENDOR ITEM STATUS FROM THE DB2 TABLE MUST BE  *
      *                CONVERTED TO THE CORPORATE MASTER EQUIVALENT   *
      *                BEFORE PROCESSING.                             *
      *                                                               *
      *                THE ITEM STATUS CONVERSIONS ARE:               *
      *                                                               *
      *                VENDOR ITEM STATUS       CORPORATE ITEM STATUS *
      *                ------------------       --------------------- *
      *                BLANK = ACTIVE    =====> A = ACTIVE            *
      *                C     = CANCELLED =====> D = DISCONTINUED      *
      *                N     = NO STOCK  =====> B = BACKORDER         *
      *                S     = SUSPENDED =====> I = INACTIVE          *
      *                                                               *
      *                                                               *
      *    CALLED BY:  P06600-SCROLL-FORWARD                          *
      *                                                               *
      *****************************************************************

       P06800-LOAD-STATUS-ARRAYS.

      *****************************************************************
      * LOAD STATUS BYTE ARRAY (WORKING STORAGE SECTION)              *
      *****************************************************************

           IF WMF-ACTIVE-SCENARIOS-R (2) = 'Y'

               PERFORM  P06860-LOAD-STATUS
                   THRU P06860-LOAD-STATUS-EXIT
                       VARYING LS-SUB FROM 1 BY 1
                           UNTIL LS-SUB > WS-STATUS-ARRAY-MAX
           END-IF.

      *****************************************************************
      * LOAD STATUS BYTE ARRAY (WORKING STORAGE)                      *
      *****************************************************************

           IF WMF-ACTIVE-SCENARIOS-R (22) = 'Y'

               SET STATUS-INDEX TO 1
               MOVE +1          TO WS-SUB

               PERFORM  P06830-LOAD-STATUS
                   THRU P06830-LOAD-STATUS-EXIT
                       UNTIL STATUS-INDEX > WS-STATUS-ARRAY-MAX

               ADD +1           TO WS-SAVE-NUMBER-OF-ENTRIES

           END-IF.

       P06800-LOAD-STATUS-ARRAYS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06830-LOAD-STATUS                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO LOAD THE STATUS BYTE ARRAY RESIDING *
      *                IN WORKING STORAGE                             *
      *                                                               *
      *    CALLED BY:  P06800-LOAD-STATUS-ARRAYS                      *
      *                                                               *
      *****************************************************************

       P06830-LOAD-STATUS.

      *    **** ACTIVE ITEM ****
           IF VENDOR-ITEM-STATUS-CODE (WS-SUB) = SPACES
               MOVE 'A'    TO WISA-ITEM-STATUS (STATUS-INDEX)
           ELSE

      *    **** CANCELLED / DISCONTINUED ITEM ****
           IF VENDOR-ITEM-STATUS-CODE (WS-SUB) = 'C'
               MOVE 'D'    TO WISA-ITEM-STATUS (STATUS-INDEX)
           ELSE

      *    **** NO STOCK  / BACK ORDER  ITEM  ****
           IF VENDOR-ITEM-STATUS-CODE (WS-SUB) = 'N'
               MOVE 'B'    TO WISA-ITEM-STATUS (STATUS-INDEX)
           ELSE

      *    **** SUSPENDED / INACTIVE ITEM     ****
               MOVE 'I'    TO WISA-ITEM-STATUS (STATUS-INDEX).


           SET STATUS-INDEX            UP BY 1.
           ADD +1                      TO WS-SUB.
           ADD +1                      TO WS-NUMBER-OF-ENTRIES.


       P06830-LOAD-STATUS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06860-LOAD-STATUS                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO LOAD THE STATUS BYTE ARRAY RESIDING *
      *                IN LINKAGE SECTION                             *
      *                                                               *
      *    CALLED BY:  P06800-LOAD-STATUS-ARRAYS                      *
      *                                                               *
      *****************************************************************

       P06860-LOAD-STATUS.

      *    **** ACTIVE ITEM ****
           IF VENDOR-ITEM-STATUS-CODE (LS-SUB) = SPACES
               MOVE 'A'    TO LISA-ITEM-STATUS (LS-SUB)
           ELSE

      *    **** CANCELLED / DISCONTINUED ITEM ****
           IF VENDOR-ITEM-STATUS-CODE (LS-SUB) = 'C'
               MOVE 'D'    TO LISA-ITEM-STATUS (LS-SUB)
           ELSE

      *    **** NO STOCK  / BACK ORDER  ITEM  ****
           IF VENDOR-ITEM-STATUS-CODE (LS-SUB) = 'N'
               MOVE 'B'    TO LISA-ITEM-STATUS (LS-SUB)
           ELSE

      *    **** SUSPENDED / INACTIVE ITEM     ****
               MOVE 'I'    TO LISA-ITEM-STATUS (LS-SUB).

       P06860-LOAD-STATUS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P08000-ITEM-SUPPLIERS                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DETERMINE THE SUPPLIERS FOR THE     *
      *                SELECTED ITEM. AN ORDER ITEM IS WRITTEN TO     *
      *                THE PENDING ORDER FILE FOR EACH SUPPLIER       *
      *                HANDLING THE SELECTED ITEM, AN ORDER QUANTITY  *
      *                OF 1 FROM EACH SUPPLIER WILL BE USED.          *
      *                                                               *
      *    CALLED BY:  P03600-ORDER-ADD                               *
      *                                                               *
      *****************************************************************

       P08000-ITEM-SUPPLIERS.

           MOVE ZEROES                 TO ITEM-PREFIX.
           MOVE WMF-ITEM               TO ITEM-NUMBER.

      *****************************************************************
      * DETERMINE THE NUMBER OF SUPPLIERS HANDLING THE SELECTED ITEM  *
      *****************************************************************

           PERFORM  P08030-COUNT-SUPPLIERS
               THRU P08030-COUNT-SUPPLIERS-EXIT.

           IF ERROR-FOUND
               GO TO P08000-ITEM-SUPPLIERS-EXIT.


      *****************************************************************
      * CLEAR THE PENDING ORDER FILE OF ANY PREVIOUS ORDER ITEMS      *
      *****************************************************************

           PERFORM  P08060-CLEAR-PEND-ORDER
               THRU P08060-CLEAR-PEND-ORDER-EXIT.

           IF ERROR-FOUND
               GO TO P08000-ITEM-SUPPLIERS-EXIT.


      *****************************************************************
      * CREATE PENDING ORDER FILE CONTROL RECORD (ZERO KEY RECORD)    *
      *****************************************************************

           PERFORM  P08090-CREATE-PEND-CTRL
               THRU P08090-CREATE-PEND-CTRL-EXIT.

           IF ERROR-FOUND
               GO TO P08000-ITEM-SUPPLIERS-EXIT.

      *****************************************************************
      * PROCESS SUPPLIERS FOR ITEM USING -ITEMSUPL- DB2 CURSOR        *
      *****************************************************************

           PERFORM  P08200-PROCESS-SUPPLIERS
               THRU P08200-PROCESS-SUPPLIERS-EXIT.

       P08000-ITEM-SUPPLIERS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P08030-COUNT-SUPPLIERS                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DETERMINE THE NUMBER OF SUPPLIERS   *
      *                HANDLING THE SELECTED ITEM.                    *
      *                                                               *
      *    CALLED BY:  P08000-ITEM-SUPPLIERS                          *
      *                                                               *
      *****************************************************************

       P08030-COUNT-SUPPLIERS.

           MOVE ZEROES                 TO ITEM-PREFIX.
           MOVE WMF-ITEM               TO ITEM-NUMBER.

           EXEC SQL
               SELECT  COUNT(*)
               INTO    :WS-SUPPLIER-COUNT
               FROM    ITEM,
                       ITEM_SUPPLIER,
                       SUPPLIER
               WHERE   (ITEM.PREFIX       = :ITEM-PREFIX AND
                       ITEM_PREFIX        = :ITEM-PREFIX AND
                       SUPPLIER.PREFIX    = :ITEM-PREFIX)
                       AND
                       (ITEM.NUMBER       = :ITEM-NUMBER AND
                       ITEM_NUMBER        = :ITEM-NUMBER)
                       AND
                       ITEM_SUPPLIER.SUPPLIER_ID
                                          = SUPPLIER.SUPPLIER_ID
           END-EXEC.


      *****************************************************************
      * ONLY RETURN CODE ZERO IS ACCEPTABLE -- OTHERWISE ERROR        *
      *****************************************************************

           IF SQLCODE                  = ZEROS
               NEXT SENTENCE
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'COUNT(*) SUPPLIERS'
                                       TO WPDE-FUNCTION
               MOVE 'P08030'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P08030-COUNT-SUPPLIERS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P08060-CLEAR-PEND-ORDER                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO REMOVE ALL RECORDS FROM THE         *
      *                PENDING ORDER FILE                             *
      *                                                               *
      *    CALLED BY:  P08000-ITEM-SUPPLIERS                          *
      *                                                               *
      *****************************************************************

       P08060-CLEAR-PEND-ORDER.

      *****************************************************************
      * CLOSE / OPEN PENDING ORDER FILE TO POSITION AT 1ST RECORD     *
      *****************************************************************

           CLOSE  VSAM-PENDING-ORDER.                                   00020001

           IF WMF-PENDORD-STATUS = '00'                                 00020001
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P08060'           TO WPGE-PARAGRAPH
               MOVE 'VPENDORD'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-PENDORD-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'CLOSE'            TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


           OPEN I-O      VSAM-PENDING-ORDER.                            00020001

           IF WMF-PENDORD-STATUS = '00'                                 00020001
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P08060'           TO WPGE-PARAGRAPH
               MOVE 'VPENDORD'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-PENDORD-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'OPEN'             TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

      *****************************************************************
      * PERFORM READ / DELETE PROCESS TO REMOVE ALL RECORDS           *
      *****************************************************************

           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.

           PERFORM  P08070-CLEAR-RECORDS
               THRU P08070-CLEAR-RECORDS-EXIT
                   UNTIL PROCESS-COMPLETE.


       P08060-CLEAR-PEND-ORDER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P08070-CLEAR-RECORDS                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO REMOVE ALL RECORDS FROM THE         *
      *                PENDING ORDER FILE                             *
      *                                                               *
      *    CALLED BY:  P08060-CLEAR-PEND-ORDER                        *
      *                                                               *
      *****************************************************************

       P08070-CLEAR-RECORDS.

      *****************************************************************
      * READ A PENDING ORDER RECORD,                                  *
      * IF EOF (10) OR NOT FOUND (23), END PROCESS ------ EXIT        *
      *****************************************************************

           PERFORM  P80710-READ-PENDORD-SEQ
               THRU P80710-READ-PENDORD-SEQ-EXIT.

           IF WMF-PENDORD-STATUS = '10' OR '23'                         00020001
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
               GO TO P08070-CLEAR-RECORDS-EXIT.

      *****************************************************************
      * DELETE THE PENDING ORDER RECORD                               *
      *****************************************************************

           PERFORM  P80730-DELETE-PEND-ORDER
               THRU P80730-DELETE-PEND-ORDER-EXIT.

       P08070-CLEAR-RECORDS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P08090-CREATE-PEND-CTRL                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CREATE THE PENDING ORDER FILE       *
      *                CONTROL RECORD (ZERO KEY RECORD)               *
      *                                                               *
      *    CALLED BY:  P08000-ITEM-SUPPLIERS                          *
      *                                                               *
      *****************************************************************

       P08090-CREATE-PEND-CTRL.

      *****************************************************************
      * FORMAT PENDING ORDER RECORD FIELD VALUES                      *
      *****************************************************************

           MOVE SPACES                 TO PENDING-ORDER-RECORD.
           MOVE ZEROES                 TO PENDING-ORDER-PREFIX.
           MOVE ZEROES                 TO PENDING-ORDER-SEQUENCE.
           MOVE ZEROES                 TO PENDING-ORDER-QUANTITY.


      *****************************************************************
      * WRITE THE PENDING ORDER RECORD                                *
      *****************************************************************

           PERFORM  P80760-WRITE-PEND-ORDER
               THRU P80760-WRITE-PEND-ORDER-EXIT.


       P08090-CREATE-PEND-CTRL-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P08200-PROCESS-SUPPLIERS                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PROCESS ALL THE SUPPLIERS           *
      *                STOCKING THE SELECTED ORDER ITEM.              *
      *                                                               *
      *    CALLED BY:  P08000-ITEM-SUPPLIERS                          *
      *                                                               *
      *****************************************************************

       P08200-PROCESS-SUPPLIERS.

      *****************************************************************
      * READ THE PENDING ORDER CONTROL RECORD (ZERO RECORD),          *
      * SUCCESSFUL READ (00) IS ONLY ACCEPTABLE RETURN CODE           *
      *****************************************************************

           MOVE ZEROES                 TO PENDING-ORDER-PREFIX.
           MOVE ZEROES                 TO PENDING-ORDER-SEQUENCE.

           PERFORM  P80700-READ-PENDORD-RANDM
               THRU P80700-READ-PENDORD-RANDM-EXIT.

           IF WMF-PENDORD-STATUS = '00'                                 00020001
               MOVE PENDING-ORDER-QUANTITY
                                       TO WMF-ITEM-SEQ
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P08200'           TO WPGE-PARAGRAPH
               MOVE 'VPENDORD'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-PENDORD-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'READ'             TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      *    PROCESS THE ITEM SUPPLIER CURSOR ROWS                      *
      *****************************************************************

           MOVE ZEROES                 TO ITEM-PREFIX.
           MOVE WMF-ITEM               TO ITEM-NUMBER.

           PERFORM P08230-OPEN-ITEMSUPL-CSR
              THRU P08230-OPEN-ITEMSUPL-CSR-EXIT.


           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.

           PERFORM P08260-FETCH-ITEMSUPL
              THRU P08260-FETCH-ITEMSUPL-EXIT
                  UNTIL PROCESS-COMPLETE.


           PERFORM P08290-CLOSE-ITEMSUPL-CSR
              THRU P08290-CLOSE-ITEMSUPL-CSR-EXIT.


      *****************************************************************
      * UPDATE THE PENDING ORDER CONTROL RECORD (ZERO KEY VALUE)      *
      *****************************************************************

           MOVE ZEROES                 TO PENDING-ORDER-PREFIX.
           MOVE ZEROES                 TO PENDING-ORDER-SEQUENCE.

           PERFORM  P80700-READ-PENDORD-RANDM
               THRU P80700-READ-PENDORD-RANDM-EXIT.

           IF WMF-PENDORD-STATUS = '00'                                 00020001
               MOVE WMF-ITEM-SEQ       TO PENDING-ORDER-QUANTITY
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P08200'           TO WPGE-PARAGRAPH
               MOVE 'VPENDORD'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-PENDORD-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'READ'             TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


           PERFORM  P80790-REWRITE-PEND-ORDER
               THRU P80790-REWRITE-PEND-ORDER-EXIT.


       P08200-PROCESS-SUPPLIERS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P08230-OPEN-ITEMSUPL-CSR                       *
      *                                                               *
      *    FUNCTION :  OPENS CURSOR TO RETRIEVE SUPPLIERS FOR THE ITEM*
      *                                                               *
      *    CALLED BY:  P08200-PROCESS-SUPPLIERS                       *
      *                                                               *
      *****************************************************************

       P08230-OPEN-ITEMSUPL-CSR.

           EXEC SQL
               OPEN ITEMSUPL
           END-EXEC.


           IF SQLCODE                  =  ZEROES
               NEXT SENTENCE
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'OPEN ITEMSUPL CURSOR'
                                       TO WPDE-FUNCTION
               MOVE 'P08230'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P08230-OPEN-ITEMSUPL-CSR-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P08260-FETCH-ITEMSUPL                          *
      *                                                               *
      *    FUNCTION :  FETCHS THE SUPPLIER ROW FOR SELECTED ITEM      *
      *                                                               *
      *    CALLED BY:  P08200-PROCESS-SUPPLIERS                       *
      *                                                               *
      *****************************************************************

       P08260-FETCH-ITEMSUPL.


           EXEC SQL
               FETCH  ITEMSUPL
               INTO   :ITEM-NUMBER,
                      :ITEM-CATEGORY-NAME,
                      :ITEM-SUB-CATEGORY-NAME,
                      :ITEM-NAME,
                      :ITEM-LENGTH,
                      :ITEM-DIAMETER,
                      :ITEM-SUPPLIER-SUPPLIER-ID,
                      :ITEM-SUPPLIER-UNIT-PRICE,
                      :SUPPLIER-NAME
           END-EXEC.


           IF SQLCODE                  = ZEROS
               NEXT SENTENCE
           ELSE
           IF SQLCODE                  = +100
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
               GO TO P08260-FETCH-ITEMSUPL-EXIT
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'FETCH CATEGORY CURSOR'
                                       TO WPDE-FUNCTION
               MOVE 'P08260'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


      *****************************************************************
      * WRITE A PENDING ORDER RECORD FOR EACH SUPPLIER ROW RETURNED   *
      *****************************************************************

           MOVE SPACES                 TO PENDING-ORDER-RECORD.
           MOVE ZEROES                 TO PENDING-ORDER-PREFIX.

           ADD +1                      TO WMF-ITEM-SEQ.
           MOVE WMF-ITEM-SEQ           TO PENDING-ORDER-SEQUENCE.

           MOVE +1                     TO PENDING-ORDER-QUANTITY.
           MOVE ZEROES                 TO PENDING-ORDER-ITEM-PREFIX.
           MOVE ITEM-NUMBER            TO PENDING-ORDER-ITEM-NUMBER.
           MOVE ZEROES                TO PENDING-ORDER-SUPPLIER-PREFIX.
           MOVE ITEM-SUPPLIER-SUPPLIER-ID
                                       TO PENDING-ORDER-SUPPLIER-ID.

           PERFORM  P80760-WRITE-PEND-ORDER
               THRU P80760-WRITE-PEND-ORDER-EXIT.

       P08260-FETCH-ITEMSUPL-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P08290-CLOSE-ITEMSUPL-CSR                      *
      *                                                               *
      *    FUNCTION :  CLOSES ITEM SUPPLIER CURSOR                    *
      *                                                               *
      *    CALLED BY:  P08200-PROCESS-SUPPLIERS                       *
      *                                                               *
      *****************************************************************

       P08290-CLOSE-ITEMSUPL-CSR.


           EXEC SQL
               CLOSE ITEMSUPL
           END-EXEC.


           IF SQLCODE                  = ZEROS
               NEXT SENTENCE
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'CLOSE ITEMSUPL CURSOR'
                                       TO WPDE-FUNCTION
               MOVE 'P08290'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P08290-CLOSE-ITEMSUPL-CSR-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P09000-CREATE-ORDER                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CREATE ORDERS IN THE IMS ORDER      *
      *                DATA BASE FROM THE ITEMS IN THE PENDING ORDER  *
      *                VSAM FILE.                                      *
      *                                                               *
      *    CALLED BY:  P03600-ORDER-ADD                               *
      *                                                               *
      *****************************************************************

       P09000-CREATE-ORDER.

      *****************************************************************
      * READ THE PENDING ORDER CONTROL RECORD (ZERO RECORD),          *
      * CONTAINS TOTAL # OF ITEMS ON FILE THAT FOLLOW,                *
      * SUCCESSFUL READ (00) IS ONLY ACCEPTABLE RETURN CODE           *
      *****************************************************************

           MOVE ZEROES                 TO PENDING-ORDER-PREFIX.
           MOVE ZEROES                 TO PENDING-ORDER-SEQUENCE.

           PERFORM  P80700-READ-PENDORD-RANDM
               THRU P80700-READ-PENDORD-RANDM-EXIT.

           IF WMF-PENDORD-STATUS = '00'                                 00020001
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P09000'           TO WPGE-PARAGRAPH
               MOVE 'VPENDORD'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-PENDORD-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'READ'             TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

      *****************************************************************
      * READ 1ST PENDING ORDER ITEM RECORD,                           *
      * IF RECORD FOUND, BUILD / CREATE THE ORDER ROOT SEGMENT        *
      *****************************************************************

           PERFORM  P80710-READ-PENDORD-SEQ
               THRU P80710-READ-PENDORD-SEQ-EXIT.

           IF WMF-PENDORD-STATUS = '00'                                 00020001
               PERFORM  P09030-CREATE-ORDER-ROOT
                   THRU P09030-CREATE-ORDER-ROOT-EXIT.


      *****************************************************************
      * PROCESS PENDING ORDER ITEMS UNTIL NO MORE                     *
      *****************************************************************

           MOVE 'N'                    TO WS-PROCESS-COMPLETE-SW.
           MOVE ZEROES                 TO WMF-ORDER-TOTAL-AMOUNT.
           MOVE ZEROES                 TO WMF-ITEM-SEQ.

           PERFORM  P09060-ORDER-ITEMS
               THRU P09060-ORDER-ITEMS-EXIT
                   UNTIL PROCESS-COMPLETE.

      *****************************************************************
      * UPDATE ORDER ROOT FIELDS                                      *
      *****************************************************************

           MOVE ORDER-KEY              TO OSQ-ORDER-KEY.

           PERFORM  P80100-GHU-ORDER
               THRU P80100-GHU-ORDER-EXIT.
                                                                        13580000
           IF OP-STATUS                = SPACES                         13590000
               NEXT SENTENCE                                            13600000
           ELSE                                                         13610000
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE             13620000
               MOVE 'PDAB06'           TO WPIE-PROGRAM-ID               13630000
               MOVE 'P09000'           TO WPIE-PARAGRAPH                13640000
               MOVE OP-STATUS          TO WPIE-STATUS-CODE              13650000
               MOVE 'GHU'              TO WPIE-FUNCTION-CODE            13660000
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME             13670000
               MOVE 'ORDER1DB'         TO WPIE-DATABASE-NAME            13680000
               MOVE 'GHU ORDER SEGMENT'                                 13690000
                                       TO WPIE-COMMAND                  13690000
               PERFORM  P99500-PDA-ERROR                                13700000
                   THRU P99500-PDA-ERROR-EXIT                           13700000
           END-IF.                                                      13710000


           MOVE WMF-ITEM-SEQ           TO ORDER-NEXT-ITEM-SEQUENCE.
           MOVE WMF-ORDER-TOTAL-AMOUNT TO ORDER-TOTAL-AMOUNT.

           PERFORM  P80300-REPL-ORDER
               THRU P80300-REPL-ORDER-EXIT.


      *****************************************************************
      * UPDATE CUSTOMER VSAM RECORD (LAST ORDER AMOUNT)               *
      *****************************************************************

           PERFORM  P09090-UPDATE-CUSTOMER
               THRU P09090-UPDATE-CUSTOMER-EXIT.


      *****************************************************************
      * UPDATE PURCHASE TYPE DB2 TABLE (LAST ORDER AMOUNT)            *
      *****************************************************************

           PERFORM  P09130-UPDATE-PURCH-TYPE
               THRU P09130-UPDATE-PURCH-TYPE-EXIT.


      *****************************************************************
      * CHECK FOR PENDING ORDERS AT THE BRANCH OFFICE FOR THE USER    *
      *****************************************************************

           PERFORM  P09500-CHK-BRANCH-ORDERS
               THRU P09500-CHK-BRANCH-ORDERS-EXIT.


       P09000-CREATE-ORDER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P09030-CREATE-ORDER-ROOT                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO FORMAT / CREATE THE IMS ORDER ROOT  *
      *                                                               *
      *    CALLED BY:  P09000-CREATE-ORDER                            *
      *                                                               *
      *****************************************************************

       P09030-CREATE-ORDER-ROOT.

           MOVE SPACES                 TO ORDER-SEGMENT.
           MOVE ZEROES                 TO ORDER-PREFIX.
           MOVE WPR-ORDER-NUMBER-R     TO ORDER-NUMBER.

           ADD  1                      TO WMF-PO-NUMBER
           MOVE WMF-PO-NUMBER          TO ORDER-PURCHASE-NUMBER.

           MOVE WS-CDT-D-YEAR          TO WMF-DATE-YY.
           MOVE WS-CDT-D-MONTH         TO WMF-DATE-MM.
           MOVE WS-CDT-D-DAY           TO WMF-DATE-DD.
           MOVE WMF-DATE-YYMMDD        TO ORDER-DATE-YYMMDD.

           MOVE 'IN PROGRESS'          TO ORDER-STATUS.
           MOVE +0                     TO ORDER-TOTAL-AMOUNT.

           MOVE +0                     TO ORDER-NEXT-ITEM-SEQUENCE.
           MOVE ZEROES                 TO ORDER-CUSTOMER-PREFIX.
           MOVE WMF-CUSTOMER-ID        TO ORDER-CUSTOMER-ID.
           MOVE ZEROES                 TO ORDER-PURCHASE-TYPE-PREFIX.
           MOVE 1                      TO ORDER-PURCHASE-TYPE.
           MOVE ZEROES                 TO ORDER-SHIPPER-NUMBER.

           PERFORM  P80200-ISRT-ORDER
               THRU P80200-ISRT-ORDER-EXIT.

      *****************************************************************
      * CHECK ORDER AGE (DAYS) TO OBTAIN ORDER STATUS DESCRIPTION     *
      *****************************************************************

           PERFORM  P20000-CHECK-ORDER-AGE
               THRU P20000-CHECK-ORDER-AGE-EXIT.

       P09030-CREATE-ORDER-ROOT-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P09060-ORDER-ITEMS                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO FORMAT AND INSERT THE ORDER ITEM    *
      *                CHILD SEGMENT                                  *
      *                                                               *
      *    CALLED BY:  P09000-CREATE-ORDER                            *
      *                                                               *
      *****************************************************************

       P09060-ORDER-ITEMS.

      *****************************************************************
      * IF NO MORE ITEMS --- TERMINATE LOOP AND EXIT                  *
      *****************************************************************

           IF WMF-PENDORD-STATUS = '10' OR '23'                         00020001
               MOVE 'Y'                TO WS-PROCESS-COMPLETE-SW
               GO TO P09060-ORDER-ITEMS-EXIT.

           MOVE ORDER-KEY              TO OSQ-ORDER-KEY.


      *****************************************************************
      * FORMAT ORDER ITEM CHILD SEGMENT -- INSERT                     *
      *****************************************************************

           MOVE SPACES                 TO ORDER-ITEM-SEGMENT.
           MOVE ZEROES                 TO ORDER-ITEM-PREFIX.

           ADD +1                      TO WMF-ITEM-SEQ.
           MOVE WMF-ITEM-SEQ           TO ORDER-ITEM-SEQUENCE.

           MOVE PENDING-ORDER-QUANTITY TO ORDER-ITEM-QUANTITY.
           MOVE PENDING-ORDER-PREFIX   TO ITEM-SUPPLIER-ITEM-PREFIX.
           MOVE PENDING-ORDER-ITEM-NUMBER
                                       TO ITEM-SUPPLIER-ITEM-NUMBER.
           MOVE PENDING-ORDER-SUPPLIER-ID
                                       TO ITEM-SUPPLIER-SUPPLIER-ID.

      *****************************************************************
      * OBTAIN THE UNIT PRICE FOR THE ITEM                            *
      * ZEROES (SUCCESSFUL) IS THE ONLY ACCEPTABLE RETURN CODE        *
      *****************************************************************

           EXEC SQL
               SELECT  UNIT_PRICE
               INTO    :ITEM-SUPPLIER-UNIT-PRICE
               FROM    ITEM_SUPPLIER
               WHERE   ITEM_PREFIX    = :ITEM-SUPPLIER-ITEM-PREFIX AND
                       ITEM_NUMBER    = :ITEM-SUPPLIER-ITEM-NUMBER AND
                       SUPPLIER_ID    = :ITEM-SUPPLIER-SUPPLIER-ID
           END-EXEC.

           IF SQLCODE                  = ZEROS
               NEXT SENTENCE
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'SELECT ITEM_SUPPLIER'
                                       TO WPDE-FUNCTION
               MOVE 'P09060'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


           MOVE ITEM-SUPPLIER-UNIT-PRICE
                                       TO ORDER-ITEM-UNIT-PRICE.
           MOVE PENDING-ORDER-ITEM-KEY TO ORDER-ITEM-ITEM-KEY.
           MOVE PENDING-ORDER-SUPPLIER-KEY
                                       TO ORDER-ITEM-SUPPLIER-KEY.

           PERFORM  P80500-ISRT-ORDER-ITEM
               THRU P80500-ISRT-ORDER-ITEM-EXIT.


           COMPUTE WMF-EXTENDED-PRICE  =  ORDER-ITEM-UNIT-PRICE *
                                          PENDING-ORDER-QUANTITY.
           ADD WMF-EXTENDED-PRICE      TO WMF-ORDER-TOTAL-AMOUNT.


      *****************************************************************
      * READ NEXT PENDING ORDER ITEM RECORD --- EXIT                  *
      *****************************************************************

           PERFORM  P80710-READ-PENDORD-SEQ
               THRU P80710-READ-PENDORD-SEQ-EXIT.

       P09060-ORDER-ITEMS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P09090-UPDATE-CUSTOMER                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO UPDATE THE CUSTOMER RECORD (VSAM)   *
      *                LAST ORDER AMOUNT FIELD ON AN ORDER ADD        *
      *                                                               *
      *    CALLED BY:  P09000-CREATE-ORDER                            *
      *                                                               *
      *****************************************************************

       P09090-UPDATE-CUSTOMER.

      *****************************************************************
      * UPDATE CUSTOMER VSAM RECORD (LAST ORDER AMOUNT)               *
      *****************************************************************

           MOVE ZEROES                 TO CUSTOMER-PREFIX.
           MOVE WMF-CUSTOMER-ID        TO CUSTOMER-ID.
                                                                        13580000
           PERFORM  P80600-READ-CUSTOMER
               THRU P80600-READ-CUSTOMER-EXIT.

      *****************************************************************
      *    READ SUCCESSFUL (00) IS THE ONLY ACCEPTABLE STATUS,        *
      *    OTHERWISE FORMAT ERROR AND TERMINATE PROGRAM               *
      *****************************************************************

           IF WMF-CUSTOMR-STATUS = '00'                                 00020001
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P09090'           TO WPGE-PARAGRAPH
               MOVE 'VCUSTOMR'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-CUSTOMR-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'READ'             TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

      *****************************************************************
      * UPDATE CUSTOMER VSAM RECORD (LAST ORDER AMOUNT)               *
      *****************************************************************

           MOVE ORDER-TOTAL-AMOUNT     TO CUSTOMER-LAST-ORDER-AMT.
                                                                        13580000
           PERFORM  P80630-REWRITE-CUSTOMER
               THRU P80630-REWRITE-CUSTOMER-EXIT.

       P09090-UPDATE-CUSTOMER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P09130-UPDATE-PURCH-TYPE                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO UPDATE THE PURCHASE TYPE   (DB2)    *
      *                LAST ORDER AMOUNT COLUMN ON AN ORDER ADD       *
      *                                                               *
      *    CALLED BY:  P09000-CREATE-ORDER                            *
      *                                                               *
      *****************************************************************

       P09130-UPDATE-PURCH-TYPE.

           MOVE ORDER-TOTAL-AMOUNT     TO PURCHASE-TYPE-LAST-ORDER-AMT.
           MOVE ORDER-PURCHASE-TYPE-PREFIX
                                       TO PURCHASE-TYPE-PREFIX.
           MOVE ORDER-PURCHASE-TYPE    TO PURCHASE-TYPE-TYPE.


           EXEC SQL UPDATE  PURCHASE_TYPE
               SET   LAST_ORDER_AMT = :PURCHASE-TYPE-LAST-ORDER-AMT

               WHERE PREFIX         = :PURCHASE-TYPE-PREFIX  AND
                     TYPE           = :PURCHASE-TYPE-TYPE
           END-EXEC.


      *****************************************************************
      *    UPDATE SUCCESSFUL (00) IS THE ONLY ACCEPTABLE STATUS,      *
      *    OTHERWISE FORMAT ERROR AND TERMINATE PROGRAM               *
      *****************************************************************

           IF SQLCODE                  = ZEROS
               NEXT SENTENCE
           ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'UPDATE PURCHASE_TYPE'
                                       TO WPDE-FUNCTION
               MOVE 'P09130'           TO WPDE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P09130-UPDATE-PURCH-TYPE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P09500-CHK-BRANCH-ORDERS                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CHECK FOR PENDING ORDERS RESIDING   *
      *                AT THE BRANCH OFFICE FOR THE SPECIFIC SALES    *
      *                PERSON (USER).                                 *
      *                                                               *
      *    CALLED BY:  P09000-CREATE-ORDER                            *
      *                                                               *
      *****************************************************************

       P09500-CHK-BRANCH-ORDERS.

      *****************************************************************
      *    OBTAIN THE ORDER AMOUNT VIA STORED PROCEDURE PDASP1        *
      *    (CHECK FOR SCENARIO 8 -- FORCES DB2 -303 IN PDASP1)        *
      *****************************************************************

           IF WMF-ACTIVE-SCENARIOS-R (08) = 'Y'
               MOVE '0008'             TO PDASP1-STATUS
           END-IF.

           MOVE PDASP2-USERID-NUMBER   TO PDASP1-PREFIX-R.

           EXEC SQL
               CALL PDAPROD.PDASP1 (:PDASP1-PREFIX,
                                    :PDASP1-TOTAL-COST,
                                    :PDASP1-STATUS)
           END-EXEC.


           IF PDASP1-STATUS            =  ZEROES
               NEXT SENTENCE
           ELSE
           IF PDASP1-STATUS            =  '0010' OR '0023'
               MOVE ZEROES             TO PDASP1-TOTAL-COST
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P09500'           TO WPGE-PARAGRAPH
               MOVE 'PDASP1'           TO WPM-PROGRAM-NAME
               MOVE PDASP1-STATUS      TO WPM-RETURN-CODE
               MOVE WPM-PROGRAM-ERROR  TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P09500-CHK-BRANCH-ORDERS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P09900-SCAN-ORDERS                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO SCAN SUBMITTED ORDERS TO CALCULATE  *
      *                REVENUES                                       *
      *                                                               *
      *    CALLED BY:  P03600-ORDER-ADD                               *
      *                                                               *
      *****************************************************************

       P09900-SCAN-ORDERS.

      *****************************************************************
      * READ ORDER ROOT SEGMENTS SEQUENTIALLY                         *
      *****************************************************************

           PERFORM  P80130-GN-ORDER
               THRU P80130-GN-ORDER-EXIT.


       P09900-SCAN-ORDERS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P20000-CHECK-ORDER-AGE                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DETERMINE ORDER AGE IN DAYS AND     *
      *                OBTAIN THE ORDER STATUS DESCRIPTION            *
      *                                                               *
      *    CALLED BY:  P09030-CREATE-ORDER-ROOT                       *
      *                                                               *
      *****************************************************************

PWB305 P20000-CHECK-ORDER-AGE.                                          PWB32005
PWB305                                                                  PWB32005
PWB305***************************************************************** PWB32005
PWB305*    READ THE CUSTOMER INFORMATION USING THE CUSTOMER ID        * PWB32005
PWB305*    FROM THE ORDER                                             * PWB32005
PWB305***************************************************************** PWB32005
PWB305                                                                  PWB32005
PWB305     MOVE ORDER-CUSTOMER-KEY     TO CUSTOMER-KEY.                 PWB32005
PWB305                                                                  PWB32005
PWB305     IF WMF-ACTIVE-SCENARIOS-R (01) = 'Y'                         PWB32005
PWB305         MOVE 99999              TO CUSTOMER-PREFIX.              PWB32005
PWB305                                                                  PWB32005
PWB305                                                                  PWB32005
PWB305     PERFORM  P80600-READ-CUSTOMER                                PWB32005
PWB305         THRU P80600-READ-CUSTOMER-EXIT.                          PWB32005
PWB305                                                                  PWB32005
PWB305     IF WMF-CUSTOMR-STATUS = '00'                                 00020001
PWB305         NEXT SENTENCE                                            PWB32005
PWB305     ELSE                                                         PWB32005
PWB305         MOVE 'GEN'              TO WS-PDA-ERROR-TYPE             PWB32005
PWB305         MOVE 'PDAB06'           TO WPGE-PROGRAM-ID               PWB32005
PWB305         MOVE 'P20000'           TO WPGE-PARAGRAPH                PWB32005
PWB305         MOVE 'VCUSTOMR'         TO WPM-VSAM-ERROR-FILE           PWB32005
PWB305         MOVE WMF-CUSTOMR-STATUS TO WPM-VSAM-ERROR-STATUS         PWB32005
PWB305         MOVE 'READ'             TO WPM-VSAM-ERROR-COMMAND        PWB32005
PWB305         MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION              PWB32005
PWB305         PERFORM  P99500-PDA-ERROR                                PWB32005
PWB305             THRU P99500-PDA-ERROR-EXIT.                          PWB32005
PWB305                                                                  PWB32005
PWB305***************************************************************** PWB32005
PWB305*    FORMAT PARAMETERS AND CALL APPROPRIATE SUBROUTINES,        * PWB32005
PWB305*    PDAS01 OR PDAS02 TO CALC DAYS, PDAS03 FOR STATUS DESC      * PWB32005
PWB305***************************************************************** PWB32005
PWB305                                                                  PWB32005
PWB305     MOVE ORDER-DATE-YYMMDD      TO  WMF-DATE-YYMMDD.             PWB32005
PWB305                                                                  PWB32005
PWB305     MOVE SPACES                 TO PDAS01-PARMS.                 PWB32005
PWB305     MOVE ORDER-NUMBER           TO PDAS01-ORDER-NUMBER.          PWB32005
PWB305     MOVE WMF-DATE-YY            TO PDAS01-OD-YR.                 PWB32005
PWB305     MOVE WMF-DATE-MM            TO PDAS01-OD-MONTH.              PWB32005
PWB305     MOVE WMF-DATE-DD            TO PDAS01-OD-DAY.                PWB32005
PWB305                                                                  PWB32005
PWB305     IF PDAS01-OD-YR > 50                                         PWB32005
PWB305         MOVE 19 TO PDAS01-OD-CE                                  PWB32005
PWB305     ELSE                                                         PWB32005
PWB305         MOVE 20 TO PDAS01-OD-CE                                  PWB32005
PWB305     END-IF.                                                      PWB32005
PWB305                                                                  PWB32005
PWB305     MOVE ZEROES                 TO PDAS01-ORDER-COUNT            PWB32005
PWB305                                    PDAS01-ORDER-DOLLAR-AMT.      PWB32005
PWB305                                                                  PWB32005
PWB305     IF WMF-ACTIVE-SCENARIOS-R (01) = 'Y'                         PWB32005
PWB305         MOVE CUSTOMER-TOTAL-ORDER-COUNT                          PWB32005
PWB305                                 TO PDAS01-ORDER-COUNT            PWB32005
PWB305         MOVE CUSTOMER-TOTAL-DOLLAR-AMT-GRP                       PWB32005
PWB305                                 TO PDAS01-ORDER-DOLLAR-AMT-GRP   PWB32005
PWB305     ELSE                                                         PWB32005
PWB305         NEXT SENTENCE.                                           PWB32005
PWB305                                                                  PWB32005
PWB305                                                                  PWB32005
PWB305     IF WMF-ACTIVE-SCENARIOS-R (13) = 'Y'                         PWB32005
PWB305         CALL WS-PDAS02 USING PDAS01-PARMS                        PWB32005
PWB305     ELSE                                                         PWB32005
PWB305         CALL 'PDAS01' USING PDAS01-PARMS                         PWB32005
PWB305     END-IF.                                                      PWB32005
PWB305                                                                  PWB32005
PWB305                                                                  PWB32005
PWB305     MOVE PDAS01-AGE-DAYS TO PDAS03-AGE-DAYS.                     PWB32005
PWB305     CALL 'PDAS03' USING PDAS03-PARMS.                            PWB32005
PWB305                                                                  PWB32005
PWB305 P20000-CHECK-ORDER-AGE-EXIT.                                     PWB32005
PWB305     EXIT.                                                        PWB32005
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80000-READ-PARAMETERS                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ THE INPUT PARAMETER FILE       *
      *                                                               *
      *    CALLED BY:  P00050-INITIALIZE                              *
      *                P00630-LOAD-PARM-ARRAY                         *
      *                                                               *
      *****************************************************************

       P80000-READ-PARAMETERS.

           READ INPUT-PARAMETERS INTO WS-PARAMETER-RECORD
               AT END
                   MOVE 'Y' TO WS-END-OF-PARM-FILE-SW
                   GO TO P80000-READ-PARAMETERS-EXIT.

           ADD +1                      TO WS-PARAMETER-RECORDS-IN.

           DISPLAY ' '.
           DISPLAY WPEA-ERROR-01.
           MOVE WS-PARAMETER-RECORDS-IN TO WPM-RECORD-NUMBER.
           DISPLAY WPM-RECORD-NUMBER-MSG.
           DISPLAY WS-PARAMETER-RECORD.
           DISPLAY WPEA-ERROR-01.

       P80000-READ-PARAMETERS-EXIT.
           EXIT.
           EJECT

      ***************************************************************** 13370000
      *                                                               * 13380000
      *    PARAGRAPH:  P80100-GHU-ORDER                               * 13390000
      *                                                               * 13400000
      *    FUNCTION :  ROUTINE TO READ (WITH HOLD) AN ORDER ROOT      * 13410000
      *                SEGMENT                                        * 13420000
      *                                                               * 13430000
      *    CALLED BY:  P02200-ORDER-CHANGE                            * 13440000
      *                P03000-ORDER-DELETE                            * 13450000
      *                P03600-ORDER-ADD                               * 13450000
      *                P09000-CREATE-ORDER                            * 13450000
      *                                                               * 13450000
      ***************************************************************** 13460000
                                                                        13470000
       P80100-GHU-ORDER.                                                13480000
                                                                        13490000
           CALL 'CBLTDLI' USING                                         13500000
                          ICF-GHU                                       13510000
                          ORDER-PCB                                     13520000
                          ORDER-SEGMENT                                 13530000
                          ORDER-SSA-QUAL                                13540000
           END-CALL.                                                    13550000
                                                                        13580000
           IF OP-STATUS                = SPACES OR 'GE' OR 'GB'         13590000
               NEXT SENTENCE                                            13600000
           ELSE                                                         13610000
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE             13620000
               MOVE 'PDAB06'           TO WPIE-PROGRAM-ID               13630000
               MOVE 'P80100'           TO WPIE-PARAGRAPH                13640000
               MOVE OP-STATUS          TO WPIE-STATUS-CODE              13650000
               MOVE 'GHU'              TO WPIE-FUNCTION-CODE            13660000
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME             13670000
               MOVE 'ORDER1DB'         TO WPIE-DATABASE-NAME            13680000
               MOVE 'GHU ORDER SEGMENT'                                 13690000
                                       TO WPIE-COMMAND                  13690000
               PERFORM  P99500-PDA-ERROR                                13700000
                   THRU P99500-PDA-ERROR-EXIT                           13700000
           END-IF.                                                      13710000
                                                                        13720000
       P80100-GHU-ORDER-EXIT.                                           13730000
           EXIT.                                                        13740000
           EJECT                                                        13750000

      ***************************************************************** 13370000
      *                                                               * 13380000
      *    PARAGRAPH:  P80130-GN-ORDER                                * 13390000
      *                                                               * 13400000
      *    FUNCTION :  ROUTINE TO READ ORDER ROOT SEGMENTS            * 13410000
      *                SEQUENTIALLY                                   * 13420000
      *                                                               * 13430000
      *    CALLED BY:  P09900-SCAN-ORDERS                             * 13440000
      *                                                               * 13450000
      ***************************************************************** 13460000
                                                                        13470000
       P80130-GN-ORDER.                                                 13480000
                                                                        13490000
           CALL 'CBLTDLI' USING                                         13500000
                          ICF-GN                                        13510000
                          ORDER-PCB                                     13520000
                          ORDER-SEGMENT                                 13530000
                          ORDER-SSA-UNQUAL                              13540000
           END-CALL.                                                    13550000
                                                                        13580000
                                                                        13580000
           IF OP-STATUS                = SPACES OR 'GE' OR 'GB'         13590000
               NEXT SENTENCE                                            13600000
           ELSE                                                         13610000
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE             13620000
               MOVE 'PDAB06'           TO WPIE-PROGRAM-ID               13630000
               MOVE 'P80130'           TO WPIE-PARAGRAPH                13640000
               MOVE OP-STATUS          TO WPIE-STATUS-CODE              13650000
               MOVE 'GN'               TO WPIE-FUNCTION-CODE            13660000
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME             13670000
               MOVE 'ORDER1DB'         TO WPIE-DATABASE-NAME            13680000
               MOVE 'GN ORDER SEGMENT' TO WPIE-COMMAND                  13690000
               PERFORM  P99500-PDA-ERROR                                13700000
                   THRU P99500-PDA-ERROR-EXIT                           13700000
           END-IF.                                                      13710000
                                                                        13720000
       P80130-GN-ORDER-EXIT.                                            13730000
           EXIT.                                                        13740000
           EJECT                                                        13750000

      ***************************************************************** 13370000
      *                                                               * 13380000
      *    PARAGRAPH:  P80200-ISRT-ORDER                              * 13390000
      *                                                               * 13400000
      *    FUNCTION :  ROUTINE TO INSERT AN ORDER ROOT SEGMENT        * 13410000
      *                                                               * 13430000
      *    CALLED BY:  P03600-ORDER-ADD                               * 13440000
      *                                                               * 13450000
      ***************************************************************** 13460000
                                                                        13470000
       P80200-ISRT-ORDER.                                               13480000
                                                                        13490000
           CALL 'CBLTDLI' USING                                         13500000
                          ICF-ISRT                                      13510000
                          ORDER-PCB                                     13520000
                          ORDER-SEGMENT                                 13530000
                          ORDER-SSA-UNQUAL                              13540000
           END-CALL.                                                    13550000
                                                                        13580000
                                                                        13580000
           IF OP-STATUS                = SPACES                         13590000
               NEXT SENTENCE                                            13600000
           ELSE                                                         13610000
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE             13620000
               MOVE 'PDAB06'           TO WPIE-PROGRAM-ID               13630000
               MOVE 'P80200'           TO WPIE-PARAGRAPH                13640000
               MOVE OP-STATUS          TO WPIE-STATUS-CODE              13650000
               MOVE 'ISRT'             TO WPIE-FUNCTION-CODE            13660000
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME             13670000
               MOVE 'ORDER1DB'         TO WPIE-DATABASE-NAME            13680000
               MOVE 'ISRT ORDER SEGMENT'                                13690000
                                       TO WPIE-COMMAND                  13690000
               PERFORM  P99500-PDA-ERROR                                13700000
                   THRU P99500-PDA-ERROR-EXIT                           13700000
           END-IF.                                                      13710000
                                                                        13720000
       P80200-ISRT-ORDER-EXIT.                                          13730000
           EXIT.                                                        13740000
           EJECT                                                        13750000

      ***************************************************************** 13370000
      *                                                               * 13380000
      *    PARAGRAPH:  P80300-REPL-ORDER                              * 13390000
      *                                                               * 13400000
      *    FUNCTION :  ROUTINE TO UPDATE AN ORDER ROOT SEGMENT        * 13410000
      *                                                               * 13430000
      *    CALLED BY:  P02200-ORDER-CHANGE                            * 13440000
      *                                                               * 13450000
      ***************************************************************** 13460000
                                                                        13470000
       P80300-REPL-ORDER.                                               13480000
                                                                        13490000
           CALL 'CBLTDLI' USING                                         13500000
                          ICF-REPL                                      13510000
                          ORDER-PCB                                     13520000
                          ORDER-SEGMENT                                 13530000
           END-CALL.                                                    13550000
                                                                        13580000
                                                                        13580000
           IF OP-STATUS                = SPACES                         13590000
               NEXT SENTENCE                                            13600000
           ELSE                                                         13610000
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE             13620000
               MOVE 'PDAB06'           TO WPIE-PROGRAM-ID               13630000
               MOVE 'P80300'           TO WPIE-PARAGRAPH                13640000
               MOVE OP-STATUS          TO WPIE-STATUS-CODE              13650000
               MOVE 'REPL'             TO WPIE-FUNCTION-CODE            13660000
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME             13670000
               MOVE 'ORDER1DB'         TO WPIE-DATABASE-NAME            13680000
               MOVE 'REPL ORDER SEGMENT'                                13690000
                                       TO WPIE-COMMAND                  13690000
               PERFORM  P99500-PDA-ERROR                                13700000
                   THRU P99500-PDA-ERROR-EXIT                           13700000
           END-IF.                                                      13710000
                                                                        13720000
       P80300-REPL-ORDER-EXIT.                                          13730000
           EXIT.                                                        13740000
           EJECT                                                        13750000

      ***************************************************************** 13370000
      *                                                               * 13380000
      *    PARAGRAPH:  P80400-DLET-ORDER                              * 13390000
      *                                                               * 13400000
      *    FUNCTION :  ROUTINE TO REMOVE AN ORDER ROOT SEGMENT        * 13410000
      *                                                               * 13430000
      *    CALLED BY:  P03000-ORDER-DELETE                            * 13440000
      *                P03600-ORDER-ADD                               * 13440000
      *                                                               * 13450000
      ***************************************************************** 13460000
                                                                        13470000
       P80400-DLET-ORDER.                                               13480000
                                                                        13490000
           CALL 'CBLTDLI' USING                                         13500000
                          ICF-DLET                                      13510000
                          ORDER-PCB                                     13520000
                          ORDER-SEGMENT                                 13530000
           END-CALL.                                                    13550000
                                                                        13580000
                                                                        13580000
           IF OP-STATUS                = SPACES                         13590000
               NEXT SENTENCE                                            13600000
           ELSE                                                         13610000
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE             13620000
               MOVE 'PDAB06'           TO WPIE-PROGRAM-ID               13630000
               MOVE 'P80400'           TO WPIE-PARAGRAPH                13640000
               MOVE OP-STATUS          TO WPIE-STATUS-CODE              13650000
               MOVE 'DLET'             TO WPIE-FUNCTION-CODE            13660000
               MOVE 'ORDER'            TO WPIE-SEGMENT-NAME             13670000
               MOVE 'ORDER1DB'         TO WPIE-DATABASE-NAME            13680000
               MOVE 'DLET ORDER SEGMENT'                                13690000
                                       TO WPIE-COMMAND                  13690000
               PERFORM  P99500-PDA-ERROR                                13700000
                   THRU P99500-PDA-ERROR-EXIT                           13700000
           END-IF.                                                      13710000
                                                                        13720000
       P80400-DLET-ORDER-EXIT.                                          13730000
           EXIT.                                                        13740000
           EJECT                                                        13750000

      ***************************************************************** 13370000
      *                                                               * 13380000
      *    PARAGRAPH:  P80500-ISRT-ORDER-ITEM                         * 13390000
      *                                                               * 13400000
      *    FUNCTION :  ROUTINE TO INSERT AN ORDER ITEM CHILD SEGMENT  * 13410000
      *                                                               * 13430000
      *    CALLED BY:  P09060-ORDER-ITEMS                             * 13440000
      *                                                               * 13450000
      ***************************************************************** 13460000
                                                                        13470000
       P80500-ISRT-ORDER-ITEM.                                          13480000
                                                                        13490000
           CALL 'CBLTDLI' USING                                         13500000
                          ICF-ISRT                                      13510000
                          ORDER-PCB                                     13520000
                          ORDER-ITEM-SEGMENT                            13530000
                          ORDER-SSA-QUAL                                13540000
                          ORDER-ITEM-SSA-UNQUAL                         13940000
           END-CALL.                                                    13550000
                                                                        13580000
                                                                        13580000
           IF OP-STATUS                = SPACES                         13590000
               NEXT SENTENCE                                            13600000
           ELSE                                                         13610000
               MOVE 'IMS'              TO WS-PDA-ERROR-TYPE             13620000
               MOVE 'PDAB06'           TO WPIE-PROGRAM-ID               13630000
               MOVE 'P80500'           TO WPIE-PARAGRAPH                13640000
               MOVE OP-STATUS          TO WPIE-STATUS-CODE              13650000
               MOVE 'ISRT'             TO WPIE-FUNCTION-CODE            13660000
               MOVE 'ORDITEM'          TO WPIE-SEGMENT-NAME             13670000
               MOVE 'ORDER1DB'         TO WPIE-DATABASE-NAME            13680000
               MOVE 'ISRT ORDER ITEM SEGMENT'                           13690000
                                       TO WPIE-COMMAND                  13690000
               PERFORM  P99500-PDA-ERROR                                13700000
                   THRU P99500-PDA-ERROR-EXIT                           13700000
           END-IF.                                                      13710000
                                                                        13720000
       P80500-ISRT-ORDER-ITEM-EXIT.                                     13730000
           EXIT.                                                        13740000
           EJECT                                                        13750000

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80600-READ-CUSTOMER                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ THE CUSTOMER VSAM FILE         *
      *                                                               *
      *    CALLED BY:  P03700-CUSTOMER-PROCESS                        *
      *                                                               *
      *****************************************************************

       P80600-READ-CUSTOMER.

           READ VSAM-CUSTOMER.

      *****************************************************************
      *    READ SUCCESSFUL (00) OR NOT FOUND (23) ARE ACCETABLE,      *
      *    OTHERWISE FORMAT ERROR AND TERMINATE PROGRAM               *
      *****************************************************************

           IF WMF-CUSTOMR-STATUS = '00' OR '23'                         00020001
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P80600'           TO WPGE-PARAGRAPH
               MOVE 'VCUSTOMR'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-CUSTOMR-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'READ'             TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P80600-READ-CUSTOMER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80630-REWRITE-CUSTOMER                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO UPDATE THE CUSSTOMER VSAM FILE      *
      *                                                               *
      *    CALLED BY:  P09000-CREATE-ORDER                            *
      *                                                               *
      *****************************************************************

       P80630-REWRITE-CUSTOMER.

           REWRITE CUSTOMER-RECORD.

      *****************************************************************
      *    UPDATE SUCCESSFUL (00) IS ONLY ACCEPTABLE RETURN CODE      *
      *    OTHERWISE FORMAT ERROR AND TERMINATE PROGRAM               *
      *****************************************************************

           IF WMF-CUSTOMR-STATUS = '00'                                 00020001
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P80630'           TO WPGE-PARAGRAPH
               MOVE 'VCUSTOMR'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-CUSTOMR-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'REWRITE'          TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.


       P80630-REWRITE-CUSTOMER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80700-READ-PENDORD-RANDM                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ THE PENDING ORDER VSAM FILE    *
      *                (RANDOM MODE USING KEY)                        *
      *                                                               *
      *    CALLED BY:  P08200-PROCESS-SUPPLIERS                       *
      *                                                               *
      *****************************************************************

       P80700-READ-PENDORD-RANDM.

           READ VSAM-PENDING-ORDER.

      *****************************************************************
      *    READ SUCCESSFUL (00) OR NOT FOUND (23) ARE ACCETABLE,      *
      *    OTHERWISE FORMAT ERROR AND TERMINATE PROGRAM               *
      *****************************************************************

           IF WMF-PENDORD-STATUS = '00' OR '23'                         00020001
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P80700'           TO WPGE-PARAGRAPH
               MOVE 'VPENDORD'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-PENDORD-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'READ'             TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P80700-READ-PENDORD-RANDM-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80710-READ-PENDORD-SEQ                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO READ THE PENDING ORDER VSAM FILE    *
      *                (SEQUENTIAL MODE)                              *
      *                                                               *
      *    CALLED BY:  P08060-CLEAR-PEND-ORDER                        *
      *                                                               *
      *****************************************************************

       P80710-READ-PENDORD-SEQ.

           READ VSAM-PENDING-ORDER NEXT RECORD.

      *****************************************************************
      *    SUCCESSFUL (00), EOF (10) OR NOT FOUND (23) ARE ACCETABLE, *
      *    OTHERWISE FORMAT ERROR AND TERMINATE PROGRAM               *
      *****************************************************************

           IF WMF-PENDORD-STATUS = '00' OR '10' OR '23'                 00020001
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P80710'           TO WPGE-PARAGRAPH
               MOVE 'VPENDORD'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-PENDORD-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'READ'             TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P80710-READ-PENDORD-SEQ-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80730-DELETE-PEND-ORDER                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DELETE A PENDING ORDER VSAM FILE    *
      *                RECORD                                         *
      *                                                               *
      *    CALLED BY:  P08060-CLEAR-PEND-ORDER                        *
      *                                                               *
      *****************************************************************

       P80730-DELETE-PEND-ORDER.

           DELETE VSAM-PENDING-ORDER.

      *****************************************************************
      *    DELETE SUCCESSFUL (00) IS ACCETABLE,                       *
      *    OTHERWISE FORMAT ERROR AND TERMINATE PROGRAM               *
      *****************************************************************

           IF WMF-PENDORD-STATUS = '00'                                 00020001
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P80730'           TO WPGE-PARAGRAPH
               MOVE 'VPENDORD'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-PENDORD-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'DELETE'           TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P80730-DELETE-PEND-ORDER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80760-WRITE-PEND-ORDER                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO ADD A PENDING ORDER VSAM FILE       *
      *                RECORD                                         *
      *                                                               *
      *    CALLED BY:  P08090-CREATE-PEND-CTRL                        *
      *                P08260-FETCH-ITEMSUPL                          *
      *                                                               *
      *****************************************************************

       P80760-WRITE-PEND-ORDER.

           WRITE  PENDING-ORDER-RECORD.

      *****************************************************************
      *    WRITE  SUCCESSFUL (00) IS ACCETABLE,                       *
      *    OTHERWISE FORMAT ERROR AND TERMINATE PROGRAM               *
      *****************************************************************

           IF WMF-PENDORD-STATUS = '00'                                 00020001
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P80760'           TO WPGE-PARAGRAPH
               MOVE 'VPENDORD'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-PENDORD-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'WRITE'            TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P80760-WRITE-PEND-ORDER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80790-REWRITE-PEND-ORDER                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO UPDATE A PENDING ORDER VSAM FILE    *
      *                RECORD                                         *
      *                                                               *
      *    CALLED BY:  P08200-PROCESS-SUPPLIERS                       *
      *                                                               *
      *****************************************************************

       P80790-REWRITE-PEND-ORDER.

           REWRITE  PENDING-ORDER-RECORD.

      *****************************************************************
      *    WRITE  SUCCESSFUL (00) IS ACCETABLE,                       *
      *    OTHERWISE FORMAT ERROR AND TERMINATE PROGRAM               *
      *****************************************************************

           IF WMF-PENDORD-STATUS = '00'                                 00020001
               NEXT SENTENCE
           ELSE
               MOVE 'GEN'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDAB06'           TO WPGE-PROGRAM-ID
               MOVE 'P80790'           TO WPGE-PARAGRAPH
               MOVE 'VPENDORD'         TO WPM-VSAM-ERROR-FILE
               MOVE WMF-PENDORD-STATUS TO WPM-VSAM-ERROR-STATUS
               MOVE 'REWRITE'          TO WPM-VSAM-ERROR-COMMAND
               MOVE WPM-VSAM-ERROR     TO WPGE-DESCRIPTION
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.

       P80790-REWRITE-PEND-ORDER-EXIT.
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

KCS305                                                                  KCS32005
KCS305***************************************************************** KCS32005
KCS305*                                                               * KCS32005
KCS305*    PARAGRAPH:  P99400-ERROR-ROUTINE                           * KCS32005
KCS305*                                                               * KCS32005
KCS305*    FUNCTION :  ROUTINE TO FORMAT AND DISPLAY NON-FATAL ERRORS * KCS32005
KCS305*                                                               * KCS32005
KCS305*                ERROR TEXT IS DISPLAYED                        * KCS32005
KCS305*                TO THE USER INDICATING THE NATURE OF THE ERROR * KCS32005
KCS305*                                                               * KCS32005
KCS305*                CONTROL IS RETURNED TO CALLING ROUTINE         * KCS32005
KCS305*                                                               * KCS32005
KCS305*    CALLED BY:  GLOBAL                                         * KCS32005
KCS305*                                                               * KCS32005
KCS305***************************************************************** KCS32005
KCS305                                                                  KCS32005
KCS305 P99400-ERROR-ROUTINE.                                            KCS32005
KCS305                                                                  KCS32005
KCS305     MOVE 'Y'                    TO WS-ERROR-FOUND-SW.            KCS32005
KCS305                                                                  KCS32005
KCS305     DISPLAY ' '.                                                 KCS32005
KCS305     DISPLAY WPEA-ERROR-01.                                       KCS32005
KCS305     DISPLAY WPEA-ERROR-02.                                       KCS32005
KCS305     DISPLAY WPEA-ERROR-03.                                       KCS32005
KCS305     DISPLAY WPEA-ERROR-04.                                       KCS32005
KCS305     DISPLAY WPEA-ERROR-05.                                       KCS32005
KCS305     DISPLAY WPEA-ERROR-06.                                       KCS32005
KCS305                                                                  00636300
KCS305     MOVE WMF-MESSAGE-AREA       TO WPEA-ERROR-07-TEXT.           00638300
KCS305     DISPLAY WPEA-ERROR-07.                                       KCS32005
KCS305                                                                  00638600
KCS305     DISPLAY WPEA-ERROR-08.                                       KCS32005
KCS305     DISPLAY WPEA-ERROR-09.                                       KCS32005
KCS305     DISPLAY WPEA-ERROR-10.                                       KCS32005
KCS305     DISPLAY ' '.                                                 KCS32005
KCS305                                                                  KCS32005
KCS305 P99400-ERROR-ROUTINE-EXIT.                                       KCS32005
KCS305     EXIT.                                                        KCS32005
KCS305     EJECT                                                        KCS32005

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P99500-PDA-ERROR                               *
      *                                                               *
      *    FUNCTION :  ROUTINE TO HANDLE FATAL / TERMINATING GENERAL, *
      *                DB2, IMS-DLI, MQSERIES ERRORS                  *
      *                                                               *
      *                ERROR TEXT IS DISPLAYED                        *
      *                TO THE USER INDICATING THE NATURE OF THE ERROR *
      *                                                               *
      *                PROGRAM IS ABNORMALLY TERMINATED               *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************

       P99500-PDA-ERROR.

           MOVE 'Y'                    TO WS-ERROR-FOUND-SW.

           DISPLAY ' '.
           DISPLAY WPEA-ERROR-01.
           DISPLAY WPEA-ERROR-02.
           DISPLAY WPEA-ERROR-03.
           DISPLAY WPEA-ERROR-04.
           DISPLAY WPEA-ERROR-05.
           DISPLAY WPEA-ERROR-06.

      *****************************************************************
      *      FORMAT AND SEND ERROR TEXT                               *
      *****************************************************************
                                                                        00636300
           IF PDA-DB2-ERROR                                             00636400
               MOVE WS-PDA-DB2-ERROR-01                                 00636500
                                       TO WPEA-ERROR-07-TEXT            00636600
               MOVE WS-PDA-DB2-ERROR-02                                 00636700
                                       TO WPEA-ERROR-08-TEXT            00636800
           ELSE                                                         00636900
           IF PDA-IMS-ERROR                                             00637000
               MOVE WS-PDA-IMS-ERROR-01                                 00637100
                                       TO WPEA-ERROR-07-TEXT            00637200
               MOVE WS-PDA-IMS-ERROR-02                                 00637300
                                       TO WPEA-ERROR-08-TEXT            00637400
           ELSE                                                         00637500
           IF PDA-MQSERIES-ERROR                                        00637602
               MOVE WS-PDA-MQSERIES-ERROR-01                            00637702
                                       TO WPEA-ERROR-07-TEXT            00637802
               MOVE WS-PDA-MQSERIES-ERROR-02                            00637902
                                       TO WPEA-ERROR-08-TEXT            00638002
           ELSE                                                         00638102
               MOVE WS-PDA-GEN-ERROR-01                                 00638200
                                       TO WPEA-ERROR-07-TEXT            00638300
               MOVE WS-PDA-GEN-ERROR-02                                 00638400
                                       TO WPEA-ERROR-08-TEXT.           00638500
                                                                        00638600
           DISPLAY WPEA-ERROR-07.
           DISPLAY WPEA-ERROR-08.
                                                                        00638600
           DISPLAY WPEA-ERROR-09.
           DISPLAY WPEA-ERROR-10.
           DISPLAY ' '.


           MOVE 99                     TO WS-RETURN-CODE.
           CALL 'ILBOABN0'          USING WS-RETURN-CODE.
           MOVE WS-RETURN-CODE         TO RETURN-CODE.

           GOBACK.

       P99500-PDA-ERROR-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P99999-ERROR                                   *
      *                                                               *
      *    FUNCTION :  GENERIC ERROR ROUTINE TO HANDLE GENERAL ERRORS *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************

RTN    P99999-ERROR.                                                     RTN
NOT                                                                      NOT
USED       MOVE 'GEN'                  TO WS-PDA-ERROR-TYPE.             USED
AS OF      MOVE 'PDAB06'               TO WPGE-PROGRAM-ID.               AS OF
JAN        MOVE 99                     TO WS-RETURN-CODE.                JAN
2001       MOVE 'ERROR'                TO WPGE-DESCRIPTION.              2001
           MOVE 'P99999'               TO WPGE-PARAGRAPH.
LLR                                                                      LLR
           PERFORM  P99500-PDA-ERROR
               THRU P99500-PDA-ERROR-EXIT.

       P99999-ERROR-EXIT.
           EXIT.
           EJECT