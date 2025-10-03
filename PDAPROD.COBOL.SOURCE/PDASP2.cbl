       ID DIVISION.                                                     00010001
       PROGRAM-ID.  PDASP2.                                             00020001
                                                                        00021001
      ***************************************************************** 00022001
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       * 00023001
      *                       COMPUWARE CORPORATION                   * 00024001
      *                                                               * 00025001
      * PROGRAM :   PDASP2                                            * 00026001
      * TRANS   :   N/A                                               * 00027001
      * MAPSET  :   N/A                                               * 00028001
      *                                                               * 00029001
      * FUNCTION:   PROGRAM PDASP2 IS PART OF THE PRODUCT             * 00029101
      *             DEMONSTRATION APPLICATION. IT WILL QUERY THE USER * 00029201
      *             ID TABLE AND RETURN THE INFO FOR THE USER TO THE  * 00029301
      *             CALLING PROGRAM                                   * 00029401
      *                                                               * 00029501
      * FILES   :   USER ID TABLE (DB2 INPUT)                         * 00029601
      *                                                               * 00029701
      *                                                               * 00029801
      * TRANSACTIONS GENERATED: N/A                                   * 00029901
      *                                                               * 00030001
      * PFKEYS  :   N/A                                               * 00030101
      *                                                               * 00030201
      *                                                               * 00030301
      ***************************************************************** 00030401
      *             PROGRAM CHANGE LOG                                * 00030501
      *             -------------------                               * 00030601
      *                                                               * 00030701
      *  DATE       UPDATED BY            CHANGE DESCRIPTION          * 00030801
      *  --------   --------------------  --------------------------  * 00030901
      *  11/06/02    JLS                  FIXED INCORRECT PASSING     * 00031001
      *                                   OF SQLCODE IN STATUS FIELD  * 00031101
      *                                   TO CALLING PROGRAM          * 00031201
      *                                                               * 00031301
      ***************************************************************** 00031401
       ENVIRONMENT DIVISION.                                            00031501
       INPUT-OUTPUT SECTION.                                            00031601
       DATA DIVISION.                                                   00031701
       WORKING-STORAGE SECTION.                                         00031801
       77  FILLER               PIC X(12)  VALUE 'PDASP2  WS:'.         00031901
       01  WS-STATUS                   PIC 9(04).                       00032001
           EJECT                                                        00033001
           EXEC SQL INCLUDE SQLCA END-EXEC.                             00034001
                                                                        00035001
      ***************************************************************** 00036001
      *         USER IDENTIFICATION TABLE   -- DCLGEN DUSERID         * 00037001
      ***************************************************************** 00038001
                                                                        00039001
           EXEC SQL                                                     00040001
               INCLUDE DUSERID                                          00050001
           END-EXEC.                                                    00060001
           EJECT                                                        00070001
       LINKAGE SECTION.                                                 00080001
       01  LS-USERID               PIC X(08).                           00090001
       01  LS-NUMBER               PIC S9(9) COMP.                      00100001
       01  LS-ACTIVE-SCENARIOS     PIC X(250).                          00110001
       01  LS-STATUS               PIC X(04).                           00120001
       01  LS-STATUS-NUM REDEFINES                                      00130001
           LS-STATUS               PIC 9(04).                           00140001
                                                                        00150001
       PROCEDURE DIVISION USING LS-USERID                               00160001
                                 LS-NUMBER                              00170001
                                 LS-ACTIVE-SCENARIOS                    00180001
                                 LS-STATUS.                             00190001
                                                                        00200001
                                                                        00210001
      ***************************************************************** 00220001
      *    USERID MUST EXIST IN THE DB2 USERID TABLE                  * 00230001
      ***************************************************************** 00240001
                                                                        00250001
           EXEC SQL SELECT    ID,                                       00260001
                              NUMBER,                                   00270001
                              ACTIVE_SCENARIOS                          00280001
                                                                        00290001
                    INTO      :USERID-ID,                               00300001
                              :USERID-NUMBER,                           00310001
                              :USERID-ACTIVE-SCENARIOS                  00320001
                                                                        00330001
                    FROM      USERID                                    00340001
                                                                        00350001
                    WHERE     ID = :LS-USERID                           00360001
           END-EXEC.                                                    00370001
                                                                        00380001
                                                                        00390001
      ***************************************************************** 00400001
      *    ZERO RETURN CODE (SUCCESS) IS ONLY ACCEPTABLE CODE         * 00410001
      ***************************************************************** 00420001
                                                                        00430001
           IF SQLCODE                  =  ZEROES                        00440001
               MOVE USERID-NUMBER      TO LS-NUMBER                     00450001
               MOVE USERID-ACTIVE-SCENARIOS                             00460001
                                       TO LS-ACTIVE-SCENARIOS           00470001
               MOVE ZEROS              TO LS-STATUS                     00480001
           ELSE                                                         00490001
             MOVE SQLCODE                TO LS-STATUS-NUM.              00500001
                                                                        00510001
           GOBACK.                                                      00520001
                                                                        00530001