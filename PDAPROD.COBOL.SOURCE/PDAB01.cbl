       IDENTIFICATION DIVISION.                                         00010002
       PROGRAM-ID. PDAB01.                                              00020002
      *                                                                 00030002
      ***************************************************************** 00040002
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       * 00050002
      *                       COMPUWARE CORPORATION                   * 00060002
      *                                                               * 00070002
      * PROGRAM :   PDAB01                                            * 00080002
      *                                                               * 00090002
      * FUNCTION:   PROGRAM PDAB01 IS A BATCH PROGRAM THAT WILL COPY  * 00100002
      *             THE CONTROL MEMBERS TO TEMPORARY FILES THAT WILL  * 00110002
      *             THEN BE USED TO ALLOCATE THE DB2 TABLES, THE VSAM * 00120002
      *             FILES, AND THE IMS DATABASES.  IT WILL ALSO COPY  * 00130002
      *             A CONTROL MEMBER THAT WILL PERFORM A BIND OF PGM  * 00140002
      *             PDAB02 SO THAT VARIOUS USERS WILL BE ABLE TO      * 00150002
      *             CREATE DB2 TABLES FOR THEIR USE.                  * 00160002
      *                                                               * 00170002
      ***************************************************************** 00180002
      *             PROGRAM CHANGE LOG                                * 00190002
      *             -------------------                               * 00200002
      *                                                               * 00210002
      *  DATE       UPDATED BY            CHANGE DESCRIPTION          * 00220002
      *  --------   --------------------  --------------------------  * 00230002
      *  04/11/02   J SCHNELZ             ADDED IN/OUT & LOGIC FOR    * 00240002
      *                                   PRINT CONTROL MEMBERS FOR   * 00240102
      *                                   NEW USER DB2 TABLES:        * 00240202
      *                                   AFFILIATE SUPPLIER, ORDER   * 00240302
      *                                   LOG                         * 00240402
      *                                                               * 00240502
      ***************************************************************** 00240602
           EJECT                                                        00240702
       ENVIRONMENT DIVISION.                                            00240802
                                                                        00240902
       INPUT-OUTPUT SECTION.                                            00241002
                                                                        00242002
       FILE-CONTROL.                                                    00243002
                                                                        00244002
           SELECT INPUT-CRDB2          ASSIGN TO ICRDB2.                00245002
                                                                        00246002
           SELECT INPUT-CRIMS          ASSIGN TO ICRIMS.                00247002
                                                                        00248002
           SELECT INPUT-CRVSAM         ASSIGN TO ICRVSAM.               00249002
                                                                        00250002
           SELECT INPUT-DLDB2          ASSIGN TO IDLDB2.                00260002
                                                                        00270002
           SELECT INPUT-DLIMS          ASSIGN TO IDLIMS.                00280002
                                                                        00290002
           SELECT INPUT-DLVSAM         ASSIGN TO IDLVSAM.               00300002
                                                                        00310002
           SELECT INPUT-BPDAB02        ASSIGN TO IBPDAB02.              00320002
                                                                        00330002
           SELECT INPUT-PRITEM         ASSIGN TO IPRITEM.               00340002
                                                                        00350002
           SELECT INPUT-PRSUPPLR       ASSIGN TO IPRSUPPL.              00360002
                                                                        00370002
           SELECT INPUT-PRITMSUP       ASSIGN TO IPRITMSU.              00380002
                                                                        00390002
           SELECT INPUT-PRPURTYP       ASSIGN TO IPRPURTY.              00400002
                                                                        00410002
           SELECT INPUT-PRAFFSUP       ASSIGN TO IPRAFSUP.              00420002
                                                                        00430002
           SELECT INPUT-PRORDLOG       ASSIGN TO IPRORDLG.              00440002
                                                                        00450002
           SELECT OUTPUT-CRDB2         ASSIGN TO OCRDB2.                00460002
                                                                        00470002
           SELECT OUTPUT-CRIMS         ASSIGN TO OCRIMS.                00480002
                                                                        00490002
           SELECT OUTPUT-CRVSAM        ASSIGN TO OCRVSAM.               00500002
                                                                        00510002
           SELECT OUTPUT-DLDB2         ASSIGN TO ODLDB2.                00520002
                                                                        00530002
           SELECT OUTPUT-DLIMS         ASSIGN TO ODLIMS.                00540002
                                                                        00550002
           SELECT OUTPUT-DLVSAM        ASSIGN TO ODLVSAM.               00560002
                                                                        00570002
           SELECT OUTPUT-BPDAB02       ASSIGN TO OBPDAB02.              00580002
                                                                        00590002
           SELECT OUTPUT-PRITEM        ASSIGN TO OPRITEM.               00600002
                                                                        00610002
           SELECT OUTPUT-PRSUPPLR      ASSIGN TO OPRSUPPL.              00620002
                                                                        00630002
           SELECT OUTPUT-PRITMSUP      ASSIGN TO OPRITMSU.              00640002
                                                                        00650002
           SELECT OUTPUT-PRPURTYP      ASSIGN TO OPRPURTY.              00660002
                                                                        00670002
           SELECT OUTPUT-PRAFFSUP      ASSIGN TO OPRAFSUP.              00680002
                                                                        00690002
           SELECT OUTPUT-PRORDLOG      ASSIGN TO OPRORDLG.              00700002
           EJECT                                                        00710002
       DATA DIVISION.                                                   00720002
                                                                        00730002
       FILE SECTION.                                                    00740002
                                                                        00750002
       FD INPUT-CRDB2                                                   00760002
           LABEL RECORDS ARE STANDARD                                   00770002
           RECORDING MODE IS F                                          00780002
           RECORD CONTAINS 80 CHARACTERS                                00790002
           BLOCK CONTAINS 27920 CHARACTERS.                             00800002
                                                                        00810002
       01  INPUT-CRDB2-REC             PIC X(80).                       00820002
                                                                        00830002
                                                                        00840002
       FD INPUT-CRIMS                                                   00850002
           LABEL RECORDS ARE STANDARD                                   00860002
           RECORDING MODE IS F                                          00870002
           RECORD CONTAINS 80 CHARACTERS                                00880002
           BLOCK CONTAINS 27920 CHARACTERS.                             00890002
                                                                        00900002
       01  INPUT-CRIMS-REC             PIC X(80).                       00910002
                                                                        00920002
                                                                        00930002
       FD INPUT-CRVSAM                                                  00940002
           LABEL RECORDS ARE STANDARD                                   00950002
           RECORDING MODE IS F                                          00960002
           RECORD CONTAINS 80 CHARACTERS                                00970002
           BLOCK CONTAINS 27920 CHARACTERS.                             00980002
                                                                        00990002
       01  INPUT-CRVSAM-REC            PIC X(80).                       01000002
                                                                        01010002
                                                                        01020002
       FD INPUT-DLDB2                                                   01030002
           LABEL RECORDS ARE STANDARD                                   01040002
           RECORDING MODE IS F                                          01050002
           RECORD CONTAINS 80 CHARACTERS                                01060002
           BLOCK CONTAINS 27920 CHARACTERS.                             01070002
                                                                        01080002
       01  INPUT-DLDB2-REC             PIC X(80).                       01090002
                                                                        01100002
                                                                        01110002
       FD INPUT-DLIMS                                                   01120002
           LABEL RECORDS ARE STANDARD                                   01130002
           RECORDING MODE IS F                                          01140002
           RECORD CONTAINS 80 CHARACTERS                                01150002
           BLOCK CONTAINS 27920 CHARACTERS.                             01160002
                                                                        01170002
       01  INPUT-DLIMS-REC             PIC X(80).                       01180002
                                                                        01190002
                                                                        01200002
       FD INPUT-DLVSAM                                                  01210002
           LABEL RECORDS ARE STANDARD                                   01220002
           RECORDING MODE IS F                                          01230002
           RECORD CONTAINS 80 CHARACTERS                                01240002
           BLOCK CONTAINS 27920 CHARACTERS.                             01250002
                                                                        01260002
       01  INPUT-DLVSAM-REC            PIC X(80).                       01270002
                                                                        01280002
                                                                        01290002
       FD INPUT-BPDAB02                                                 01300002
           LABEL RECORDS ARE STANDARD                                   01310002
           RECORDING MODE IS F                                          01320002
           RECORD CONTAINS 80 CHARACTERS                                01330002
           BLOCK CONTAINS 27920 CHARACTERS.                             01340002
                                                                        01350002
       01  INPUT-BPDAB02-REC           PIC X(80).                       01360002
                                                                        01370002
                                                                        01380002
       FD INPUT-PRITEM                                                  01390002
           LABEL RECORDS ARE STANDARD                                   01400002
           RECORDING MODE IS F                                          01410002
           RECORD CONTAINS 80 CHARACTERS                                01420002
           BLOCK CONTAINS 27920 CHARACTERS.                             01430002
                                                                        01440002
       01  INPUT-PRITEM-REC            PIC X(80).                       01450002
                                                                        01460002
                                                                        01470002
       FD INPUT-PRSUPPLR                                                01480002
           LABEL RECORDS ARE STANDARD                                   01490002
           RECORDING MODE IS F                                          01500002
           RECORD CONTAINS 80 CHARACTERS                                01510002
           BLOCK CONTAINS 27920 CHARACTERS.                             01520002
                                                                        01530002
       01  INPUT-PRSUPPLR-REC          PIC X(80).                       01540002
                                                                        01550002
                                                                        01560002
       FD INPUT-PRITMSUP                                                01570002
           LABEL RECORDS ARE STANDARD                                   01580002
           RECORDING MODE IS F                                          01590002
           RECORD CONTAINS 80 CHARACTERS                                01600002
           BLOCK CONTAINS 27920 CHARACTERS.                             01610002
                                                                        01620002
       01  INPUT-PRITMSUP-REC          PIC X(80).                       01630002
                                                                        01640002
                                                                        01650002
       FD INPUT-PRPURTYP                                                01660002
           LABEL RECORDS ARE STANDARD                                   01670002
           RECORDING MODE IS F                                          01680002
           RECORD CONTAINS 80 CHARACTERS                                01690002
           BLOCK CONTAINS 27920 CHARACTERS.                             01700002
                                                                        01710002
       01  INPUT-PRPURTYP-REC          PIC X(80).                       01720002
                                                                        01730002
       FD INPUT-PRAFFSUP                                                01740002
           LABEL RECORDS ARE STANDARD                                   01750002
           RECORDING MODE IS F                                          01760002
           RECORD CONTAINS 80 CHARACTERS                                01770002
           BLOCK CONTAINS 27920 CHARACTERS.                             01780002
                                                                        01790002
       01  INPUT-PRAFFSUP-REC          PIC X(80).                       01800002
                                                                        01801002
       FD INPUT-PRORDLOG                                                01802002
           LABEL RECORDS ARE STANDARD                                   01803002
           RECORDING MODE IS F                                          01804002
           RECORD CONTAINS 80 CHARACTERS                                01805002
           BLOCK CONTAINS 27920 CHARACTERS.                             01806002
                                                                        01807002
       01  INPUT-PRORDLOG-REC          PIC X(80).                       01808002
                                                                        01809002
       FD OUTPUT-CRDB2                                                  01810002
           LABEL RECORDS ARE STANDARD                                   01820002
           RECORDING MODE IS F                                          01830002
           RECORD CONTAINS 80 CHARACTERS                                01840002
           BLOCK CONTAINS 80 CHARACTERS.                                01850002
                                                                        01860002
       01  OUTPUT-CRDB2-REC            PIC X(80).                       01870002
                                                                        01880002
                                                                        01890002
       FD OUTPUT-CRIMS                                                  01900002
           LABEL RECORDS ARE STANDARD                                   01910002
           RECORDING MODE IS F                                          01920002
           RECORD CONTAINS 80 CHARACTERS                                01930002
           BLOCK CONTAINS 80 CHARACTERS.                                01940002
                                                                        01950002
       01  OUTPUT-CRIMS-REC            PIC X(80).                       01960002
                                                                        01970002
                                                                        01980002
       FD OUTPUT-CRVSAM                                                 01990002
           LABEL RECORDS ARE STANDARD                                   02000002
           RECORDING MODE IS F                                          02010002
           RECORD CONTAINS 80 CHARACTERS                                02020002
           BLOCK CONTAINS 80 CHARACTERS.                                02030002
                                                                        02040002
       01  OUTPUT-CRVSAM-REC           PIC X(80).                       02050002
                                                                        02060002
                                                                        02070002
       FD OUTPUT-DLDB2                                                  02080002
           LABEL RECORDS ARE STANDARD                                   02090002
           RECORDING MODE IS F                                          02100002
           RECORD CONTAINS 80 CHARACTERS                                02110002
           BLOCK CONTAINS 80 CHARACTERS.                                02120002
                                                                        02130002
       01  OUTPUT-DLDB2-REC            PIC X(80).                       02140002
                                                                        02150002
                                                                        02160002
       FD OUTPUT-DLIMS                                                  02170002
           LABEL RECORDS ARE STANDARD                                   02180002
           RECORDING MODE IS F                                          02190002
           RECORD CONTAINS 80 CHARACTERS                                02200002
           BLOCK CONTAINS 80 CHARACTERS.                                02210002
                                                                        02220002
       01  OUTPUT-DLIMS-REC            PIC X(80).                       02230002
                                                                        02240002
                                                                        02250002
       FD OUTPUT-DLVSAM                                                 02260002
           LABEL RECORDS ARE STANDARD                                   02270002
           RECORDING MODE IS F                                          02280002
           RECORD CONTAINS 80 CHARACTERS                                02290002
           BLOCK CONTAINS 80 CHARACTERS.                                02300002
                                                                        02310002
       01  OUTPUT-DLVSAM-REC           PIC X(80).                       02320002
                                                                        02330002
                                                                        02340002
       FD OUTPUT-BPDAB02                                                02350002
           LABEL RECORDS ARE STANDARD                                   02360002
           RECORDING MODE IS F                                          02370002
           RECORD CONTAINS 80 CHARACTERS                                02380002
           BLOCK CONTAINS 80 CHARACTERS.                                02390002
                                                                        02400002
       01  OUTPUT-BPDAB02-REC          PIC X(80).                       02410002
                                                                        02420002
                                                                        02430002
       FD OUTPUT-PRITEM                                                 02440002
           LABEL RECORDS ARE STANDARD                                   02450002
           RECORDING MODE IS F                                          02460002
           RECORD CONTAINS 80 CHARACTERS                                02470002
           BLOCK CONTAINS 80 CHARACTERS.                                02480002
                                                                        02490002
       01  OUTPUT-PRITEM-REC           PIC X(80).                       02500002
                                                                        02510002
                                                                        02520002
       FD OUTPUT-PRSUPPLR                                               02530002
           LABEL RECORDS ARE STANDARD                                   02540002
           RECORDING MODE IS F                                          02550002
           RECORD CONTAINS 80 CHARACTERS                                02560002
           BLOCK CONTAINS 80 CHARACTERS.                                02570002
                                                                        02580002
       01  OUTPUT-PRSUPPLR-REC         PIC X(80).                       02590002
                                                                        02600002
                                                                        02610002
       FD OUTPUT-PRITMSUP                                               02620002
           LABEL RECORDS ARE STANDARD                                   02630002
           RECORDING MODE IS F                                          02640002
           RECORD CONTAINS 80 CHARACTERS                                02650002
           BLOCK CONTAINS 80 CHARACTERS.                                02660002
                                                                        02670002
       01  OUTPUT-PRITMSUP-REC         PIC X(80).                       02680002
                                                                        02690002
                                                                        02700002
       FD OUTPUT-PRPURTYP                                               02710002
           LABEL RECORDS ARE STANDARD                                   02720002
           RECORDING MODE IS F                                          02730002
           RECORD CONTAINS 80 CHARACTERS                                02740002
           BLOCK CONTAINS 80 CHARACTERS.                                02750002
                                                                        02760002
       01  OUTPUT-PRPURTYP-REC         PIC X(80).                       02770002
                                                                        02780002
       FD OUTPUT-PRAFFSUP                                               02781002
           LABEL RECORDS ARE STANDARD                                   02782002
           RECORDING MODE IS F                                          02783002
           RECORD CONTAINS 80 CHARACTERS                                02784002
           BLOCK CONTAINS 80 CHARACTERS.                                02785002
                                                                        02786002
       01  OUTPUT-PRAFFSUP-REC         PIC X(80).                       02787002
                                                                        02788002
       FD OUTPUT-PRORDLOG                                               02789002
           LABEL RECORDS ARE STANDARD                                   02789102
           RECORDING MODE IS F                                          02789202
           RECORD CONTAINS 80 CHARACTERS                                02789302
           BLOCK CONTAINS 80 CHARACTERS.                                02789402
                                                                        02789502
       01  OUTPUT-PRORDLOG-REC         PIC X(80).                       02789602
           EJECT                                                        02789702
       WORKING-STORAGE SECTION.                                         02789802
                                                                        02789902
                                                                        02790002
      ***************************************************************** 02800002
      *    SWITCHES   AND   SUBSCRIPTS                                * 02810002
      ***************************************************************** 02820002
       77  WS-SUB1                     PIC S9(04)                       02830002
                                                 VALUE +0   COMP.       02840002
       77  WS-SUB2                     PIC S9(04)                       02850002
                                                 VALUE +0   COMP.       02860002
       77  WS-FIELD-LTH-MAX            PIC S9(04)                       02861002
                                                 VALUE +80  COMP.       02862002
                                                                        02863002
       01  WS-SWITCHES.                                                 02864002
           05  WS-PARM-ERROR           PIC X     VALUE 'N'.             02865002
               88  PARM-ERROR                    VALUE 'Y'.             02866002
           05  WS-END-OF-CRDB2         PIC X     VALUE 'N'.             02867002
               88  END-OF-CRDB2                  VALUE 'Y'.             02868002
           05  WS-END-OF-CRIMS         PIC X     VALUE 'N'.             02869002
               88  END-OF-CRIMS                  VALUE 'Y'.             02870002
           05  WS-END-OF-CRVSAM        PIC X     VALUE 'N'.             02880002
               88  END-OF-CRVSAM                 VALUE 'Y'.             02890002
           05  WS-END-OF-DLDB2         PIC X     VALUE 'N'.             02900002
               88  END-OF-DLDB2                  VALUE 'Y'.             02910002
           05  WS-END-OF-DLIMS         PIC X     VALUE 'N'.             02920002
               88  END-OF-DLIMS                  VALUE 'Y'.             02930002
           05  WS-END-OF-DLVSAM        PIC X     VALUE 'N'.             02940002
               88  END-OF-DLVSAM                 VALUE 'Y'.             02950002
           05  WS-END-OF-BPDAB02       PIC X     VALUE 'N'.             02960002
               88  END-OF-BPDAB02                VALUE 'Y'.             02970002
           05  WS-END-OF-PRITEM        PIC X     VALUE 'N'.             02980002
               88  END-OF-PRITEM                 VALUE 'Y'.             02990002
           05  WS-END-OF-PRSUPPLR      PIC X     VALUE 'N'.             03000002
               88  END-OF-PRSUPPLR               VALUE 'Y'.             03010002
           05  WS-END-OF-PRITMSUP      PIC X     VALUE 'N'.             03020002
               88  END-OF-PRITMSUP               VALUE 'Y'.             03030002
           05  WS-END-OF-PRPURTYP      PIC X     VALUE 'N'.             03040002
               88  END-OF-PRPURTYP               VALUE 'Y'.             03050002
           05  WS-END-OF-PRAFFSUP      PIC X     VALUE 'N'.             03060002
               88  END-OF-PRAFFSUP               VALUE 'Y'.             03070002
           05  WS-END-OF-PRORDLOG      PIC X     VALUE 'N'.             03080002
               88  END-OF-PRORDLOG               VALUE 'Y'.             03090002
           EJECT                                                        03100002
      ***************************************************************** 03110002
      *    MISCELLANEOUS WORK FIELDS                                  * 03120002
      ***************************************************************** 03130002
                                                                        03140002
       01  WS-MISCELLANEOUS-FIELDS.                                     03150002
           03  WS-USERID               PIC X(7)  VALUE 'PDA9999'.       03160002
           03  WS-CICSREGN             PIC X(8)  VALUE 'H0199999'.      03170002
           03  WS-STORCLAS             PIC X(6)  VALUE 'STG999'.        03180002
           03  WS-DATABASE             PIC X(8)  VALUE 'PDA999DB'.      03190002
           03  WS-RETURN-CODE          PIC S9(4) VALUE +0       COMP.   03200002
                                                                        03210002
                                                                        03220002
           03  WS-RIGHT-JUSTIFY-IN     PIC X(80) VALUE SPACES.          03221002
           03  WS-RIGHT-JUSTIFY-IN-R   REDEFINES WS-RIGHT-JUSTIFY-IN    03221102
                                       PIC X(01)                        03221202
                                       OCCURS 80 TIMES.                 03221302
                                                                        03221402
           03  WS-RIGHT-JUSTIFY-OUT    PIC X(80) VALUE SPACES.          03221502
           03  WS-RIGHT-JUSTIFY-OUT-R  REDEFINES WS-RIGHT-JUSTIFY-OUT   03221602
                                       PIC X(01)                        03221702
                                       OCCURS 80 TIMES.                 03221802
           03  WS-RIGHT-JUSTIFY-OUT-LTH08                               03221902
                                       REDEFINES WS-RIGHT-JUSTIFY-OUT.  03222002
               05  FILLER              PIC X(72).                       03223002
               05  WS-RIGHT-JUSTIFY-OUT-08                              03224002
                                       PIC X(08).                       03225002
           EJECT                                                        03226002
      ***************************************************************** 03227002
      *    INPUT RECORD LAYOUT USED TO BUILD OUTPUT FILES             * 03228002
      ***************************************************************** 03229002
                                                                        03230002
       01  PDA-WORK-FORMAT.                                             03240002
           03  FILLER                  PIC XX    VALUE SPACES.          03250002
               88  PDA-COMMENT                   VALUE '--'.            03260002
           03  PDA-LINE                PIC X(78) VALUE SPACES.          03270002
           03  FILLER                  REDEFINES PDA-LINE.              03280002
               05  PDA-6-01            PIC X(6).                        03290002
           03  FILLER                  REDEFINES PDA-LINE.              03300002
               05  FILLER              PIC X.                           03310002
               05  PDA-6-02            PIC X(6).                        03320002
           03  FILLER                  REDEFINES PDA-LINE.              03330002
               05  FILLER              PIC XX.                          03340002
               05  PDA-6-03            PIC X(6).                        03350002
           03  FILLER                  REDEFINES PDA-LINE.              03360002
               05  FILLER              PIC X(3).                        03370002
               05  PDA-6-04            PIC X(6).                        03380002
           03  FILLER                  REDEFINES PDA-LINE.              03390002
               05  FILLER              PIC X(4).                        03400002
               05  PDA-6-05            PIC X(6).                        03410002
           03  FILLER                  REDEFINES PDA-LINE.              03420002
               05  FILLER              PIC X(5).                        03430002
               05  PDA-6-06            PIC X(6).                        03440002
           03  FILLER                  REDEFINES PDA-LINE.              03450002
               05  FILLER              PIC X(6).                        03460002
               05  PDA-6-07            PIC X(6).                        03470002
           03  FILLER                  REDEFINES PDA-LINE.              03480002
               05  FILLER              PIC X(7).                        03490002
               05  PDA-6-08            PIC X(6).                        03500002
           03  FILLER                  REDEFINES PDA-LINE.              03510002
               05  FILLER              PIC X(8).                        03520002
               05  PDA-6-09            PIC X(6).                        03530002
           03  FILLER                  REDEFINES PDA-LINE.              03540002
               05  FILLER              PIC X(9).                        03550002
               05  PDA-6-10            PIC X(6).                        03560002
           03  FILLER                  REDEFINES PDA-LINE.              03570002
               05  FILLER              PIC X(10).                       03580002
               05  PDA-6-11            PIC X(6).                        03590002
           03  FILLER                  REDEFINES PDA-LINE.              03600002
               05  FILLER              PIC X(11).                       03610002
               05  PDA-6-12            PIC X(6).                        03620002
           03  FILLER                  REDEFINES PDA-LINE.              03630002
               05  FILLER              PIC X(12).                       03640002
               05  PDA-6-13            PIC X(6).                        03650002
           03  FILLER                  REDEFINES PDA-LINE.              03660002
               05  FILLER              PIC X(13).                       03670002
               05  PDA-6-14            PIC X(6).                        03680002
           03  FILLER                  REDEFINES PDA-LINE.              03690002
               05  FILLER              PIC X(14).                       03700002
               05  PDA-6-15            PIC X(6).                        03710002
           03  FILLER                  REDEFINES PDA-LINE.              03720002
               05  FILLER              PIC X(15).                       03730002
               05  PDA-6-16            PIC X(6).                        03740002
           03  FILLER                  REDEFINES PDA-LINE.              03750002
               05  FILLER              PIC X(16).                       03760002
               05  PDA-6-17            PIC X(6).                        03770002
           03  FILLER                  REDEFINES PDA-LINE.              03780002
               05  FILLER              PIC X(17).                       03790002
               05  PDA-6-18            PIC X(6).                        03800002
           03  FILLER                  REDEFINES PDA-LINE.              03810002
               05  FILLER              PIC X(18).                       03820002
               05  PDA-6-19            PIC X(6).                        03830002
           03  FILLER                  REDEFINES PDA-LINE.              03840002
               05  FILLER              PIC X(19).                       03850002
               05  PDA-6-20            PIC X(6).                        03860002
           03  FILLER                  REDEFINES PDA-LINE.              03870002
               05  FILLER              PIC X(20).                       03880002
               05  PDA-6-21            PIC X(6).                        03890002
           03  FILLER                  REDEFINES PDA-LINE.              03900002
               05  FILLER              PIC X(21).                       03910002
               05  PDA-6-22            PIC X(6).                        03920002
           03  FILLER                  REDEFINES PDA-LINE.              03930002
               05  FILLER              PIC X(22).                       03940002
               05  PDA-6-23            PIC X(6).                        03950002
           03  FILLER                  REDEFINES PDA-LINE.              03960002
               05  FILLER              PIC X(23).                       03970002
               05  PDA-6-24            PIC X(6).                        03980002
           03  FILLER                  REDEFINES PDA-LINE.              03990002
               05  FILLER              PIC X(24).                       04000002
               05  PDA-6-25            PIC X(6).                        04010002
           03  FILLER                  REDEFINES PDA-LINE.              04020002
               05  FILLER              PIC X(25).                       04030002
               05  PDA-6-26            PIC X(6).                        04040002
           03  FILLER                  REDEFINES PDA-LINE.              04050002
               05  FILLER              PIC X(26).                       04060002
               05  PDA-6-27            PIC X(6).                        04070002
           03  FILLER                  REDEFINES PDA-LINE.              04080002
               05  FILLER              PIC X(27).                       04090002
               05  PDA-6-28            PIC X(6).                        04100002
           03  FILLER                  REDEFINES PDA-LINE.              04110002
               05  FILLER              PIC X(28).                       04120002
               05  PDA-6-29            PIC X(6).                        04130002
           03  FILLER                  REDEFINES PDA-LINE.              04140002
               05  FILLER              PIC X(29).                       04150002
               05  PDA-6-30            PIC X(6).                        04160002
           03  FILLER                  REDEFINES PDA-LINE.              04170002
               05  FILLER              PIC X(30).                       04180002
               05  PDA-6-31            PIC X(6).                        04190002
           03  FILLER                  REDEFINES PDA-LINE.              04200002
               05  FILLER              PIC X(31).                       04210002
               05  PDA-6-32            PIC X(6).                        04220002
           03  FILLER                  REDEFINES PDA-LINE.              04230002
               05  FILLER              PIC X(32).                       04240002
               05  PDA-6-33            PIC X(6).                        04250002
           03  FILLER                  REDEFINES PDA-LINE.              04260002
               05  FILLER              PIC X(33).                       04270002
               05  PDA-6-34            PIC X(6).                        04280002
           03  FILLER                  REDEFINES PDA-LINE.              04290002
               05  FILLER              PIC X(34).                       04300002
               05  PDA-6-35            PIC X(6).                        04310002
           03  FILLER                  REDEFINES PDA-LINE.              04320002
               05  FILLER              PIC X(35).                       04330002
               05  PDA-6-36            PIC X(6).                        04340002
           03  FILLER                  REDEFINES PDA-LINE.              04350002
               05  FILLER              PIC X(36).                       04360002
               05  PDA-6-37            PIC X(6).                        04370002
           03  FILLER                  REDEFINES PDA-LINE.              04380002
               05  FILLER              PIC X(37).                       04390002
               05  PDA-6-38            PIC X(6).                        04400002
           03  FILLER                  REDEFINES PDA-LINE.              04410002
               05  FILLER              PIC X(38).                       04420002
               05  PDA-6-39            PIC X(6).                        04430002
           03  FILLER                  REDEFINES PDA-LINE.              04440002
               05  FILLER              PIC X(39).                       04450002
               05  PDA-6-40            PIC X(6).                        04460002
           03  FILLER                  REDEFINES PDA-LINE.              04470002
               05  FILLER              PIC X(40).                       04480002
               05  PDA-6-41            PIC X(6).                        04490002
           03  FILLER                  REDEFINES PDA-LINE.              04500002
               05  FILLER              PIC X(41).                       04510002
               05  PDA-6-42            PIC X(6).                        04520002
           03  FILLER                  REDEFINES PDA-LINE.              04530002
               05  FILLER              PIC X(42).                       04540002
               05  PDA-6-43            PIC X(6).                        04550002
           03  FILLER                  REDEFINES PDA-LINE.              04560002
               05  FILLER              PIC X(43).                       04570002
               05  PDA-6-44            PIC X(6).                        04580002
           03  FILLER                  REDEFINES PDA-LINE.              04590002
               05  FILLER              PIC X(44).                       04600002
               05  PDA-6-45            PIC X(6).                        04610002
           03  FILLER                  REDEFINES PDA-LINE.              04620002
               05  FILLER              PIC X(45).                       04630002
               05  PDA-6-46            PIC X(6).                        04640002
           03  FILLER                  REDEFINES PDA-LINE.              04650002
               05  FILLER              PIC X(46).                       04660002
               05  PDA-6-47            PIC X(6).                        04670002
           03  FILLER                  REDEFINES PDA-LINE.              04680002
               05  FILLER              PIC X(47).                       04690002
               05  PDA-6-48            PIC X(6).                        04700002
           03  FILLER                  REDEFINES PDA-LINE.              04710002
               05  FILLER              PIC X(48).                       04720002
               05  PDA-6-49            PIC X(6).                        04730002
           03  FILLER                  REDEFINES PDA-LINE.              04740002
               05  FILLER              PIC X(49).                       04750002
               05  PDA-6-50            PIC X(6).                        04760002
           03  FILLER                  REDEFINES PDA-LINE.              04770002
               05  FILLER              PIC X(50).                       04780002
               05  PDA-6-51            PIC X(6).                        04790002
           03  FILLER                  REDEFINES PDA-LINE.              04800002
               05  FILLER              PIC X(51).                       04810002
               05  PDA-6-52            PIC X(6).                        04820002
           03  FILLER                  REDEFINES PDA-LINE.              04830002
               05  FILLER              PIC X(52).                       04840002
               05  PDA-6-53            PIC X(6).                        04850002
           03  FILLER                  REDEFINES PDA-LINE.              04860002
               05  FILLER              PIC X(53).                       04870002
               05  PDA-6-54            PIC X(6).                        04880002
           03  FILLER                  REDEFINES PDA-LINE.              04890002
               05  FILLER              PIC X(54).                       04900002
               05  PDA-6-55            PIC X(6).                        04910002
           03  FILLER                  REDEFINES PDA-LINE.              04920002
               05  FILLER              PIC X(55).                       04930002
               05  PDA-6-56            PIC X(6).                        04940002
           03  FILLER                  REDEFINES PDA-LINE.              04950002
               05  FILLER              PIC X(56).                       04960002
               05  PDA-6-57            PIC X(6).                        04970002
           03  FILLER                  REDEFINES PDA-LINE.              04980002
               05  FILLER              PIC X(57).                       04990002
               05  PDA-6-58            PIC X(6).                        05000002
           03  FILLER                  REDEFINES PDA-LINE.              05010002
               05  FILLER              PIC X(58).                       05020002
               05  PDA-6-59            PIC X(6).                        05030002
           03  FILLER                  REDEFINES PDA-LINE.              05040002
               05  FILLER              PIC X(59).                       05050002
               05  PDA-6-60            PIC X(6).                        05060002
           03  FILLER                  REDEFINES PDA-LINE.              05070002
               05  FILLER              PIC X(60).                       05080002
               05  PDA-6-61            PIC X(6).                        05090002
           03  FILLER                  REDEFINES PDA-LINE.              05100002
               05  FILLER              PIC X(61).                       05110002
               05  PDA-6-62            PIC X(6).                        05120002
           03  FILLER                  REDEFINES PDA-LINE.              05130002
               05  FILLER              PIC X(62).                       05140002
               05  PDA-6-63            PIC X(6).                        05150002
           03  FILLER                  REDEFINES PDA-LINE.              05160002
               05  FILLER              PIC X(63).                       05170002
               05  PDA-6-64            PIC X(6).                        05180002
           03  FILLER                  REDEFINES PDA-LINE.              05190002
               05  FILLER              PIC X(64).                       05200002
               05  PDA-6-65            PIC X(6).                        05210002
           03  FILLER                  REDEFINES PDA-LINE.              05220002
               05  FILLER              PIC X(65).                       05230002
               05  PDA-6-66            PIC X(6).                        05240002
           03  FILLER                  REDEFINES PDA-LINE.              05250002
               05  FILLER              PIC X(66).                       05260002
               05  PDA-6-67            PIC X(6).                        05270002
           03  FILLER                  REDEFINES PDA-LINE.              05280002
               05  FILLER              PIC X(67).                       05290002
               05  PDA-6-68            PIC X(6).                        05300002
           03  FILLER                  REDEFINES PDA-LINE.              05310002
               05  FILLER              PIC X(68).                       05320002
               05  PDA-6-69            PIC X(6).                        05330002
           03  FILLER                  REDEFINES PDA-LINE.              05340002
               05  FILLER              PIC X(69).                       05350002
               05  PDA-6-70            PIC X(6).                        05360002
           03  FILLER                  REDEFINES PDA-LINE.              05370002
               05  PDA-7-01            PIC X(7).                        05380002
           03  FILLER                  REDEFINES PDA-LINE.              05390002
               05  FILLER              PIC X.                           05400002
               05  PDA-7-02            PIC X(7).                        05410002
           03  FILLER                  REDEFINES PDA-LINE.              05420002
               05  FILLER              PIC XX.                          05430002
               05  PDA-7-03            PIC X(7).                        05440002
           03  FILLER                  REDEFINES PDA-LINE.              05450002
               05  FILLER              PIC X(3).                        05460002
               05  PDA-7-04            PIC X(7).                        05470002
           03  FILLER                  REDEFINES PDA-LINE.              05480002
               05  FILLER              PIC X(4).                        05490002
               05  PDA-7-05            PIC X(7).                        05500002
           03  FILLER                  REDEFINES PDA-LINE.              05510002
               05  FILLER              PIC X(5).                        05520002
               05  PDA-7-06            PIC X(7).                        05530002
           03  FILLER                  REDEFINES PDA-LINE.              05540002
               05  FILLER              PIC X(6).                        05550002
               05  PDA-7-07            PIC X(7).                        05560002
           03  FILLER                  REDEFINES PDA-LINE.              05570002
               05  FILLER              PIC X(7).                        05580002
               05  PDA-7-08            PIC X(7).                        05590002
           03  FILLER                  REDEFINES PDA-LINE.              05600002
               05  FILLER              PIC X(8).                        05610002
               05  PDA-7-09            PIC X(7).                        05620002
           03  FILLER                  REDEFINES PDA-LINE.              05630002
               05  FILLER              PIC X(9).                        05640002
               05  PDA-7-10            PIC X(7).                        05650002
           03  FILLER                  REDEFINES PDA-LINE.              05660002
               05  FILLER              PIC X(10).                       05670002
               05  PDA-7-11            PIC X(7).                        05680002
           03  FILLER                  REDEFINES PDA-LINE.              05690002
               05  FILLER              PIC X(11).                       05700002
               05  PDA-7-12            PIC X(7).                        05710002
           03  FILLER                  REDEFINES PDA-LINE.              05720002
               05  FILLER              PIC X(12).                       05730002
               05  PDA-7-13            PIC X(7).                        05740002
           03  FILLER                  REDEFINES PDA-LINE.              05750002
               05  FILLER              PIC X(13).                       05760002
               05  PDA-7-14            PIC X(7).                        05770002
           03  FILLER                  REDEFINES PDA-LINE.              05780002
               05  FILLER              PIC X(14).                       05790002
               05  PDA-7-15            PIC X(7).                        05800002
           03  FILLER                  REDEFINES PDA-LINE.              05810002
               05  FILLER              PIC X(15).                       05820002
               05  PDA-7-16            PIC X(7).                        05830002
           03  FILLER                  REDEFINES PDA-LINE.              05840002
               05  FILLER              PIC X(16).                       05850002
               05  PDA-7-17            PIC X(7).                        05860002
           03  FILLER                  REDEFINES PDA-LINE.              05870002
               05  FILLER              PIC X(17).                       05880002
               05  PDA-7-18            PIC X(7).                        05890002
           03  FILLER                  REDEFINES PDA-LINE.              05900002
               05  FILLER              PIC X(18).                       05910002
               05  PDA-7-19            PIC X(7).                        05920002
           03  FILLER                  REDEFINES PDA-LINE.              05930002
               05  FILLER              PIC X(19).                       05940002
               05  PDA-7-20            PIC X(7).                        05950002
           03  FILLER                  REDEFINES PDA-LINE.              05960002
               05  FILLER              PIC X(20).                       05970002
               05  PDA-7-21            PIC X(7).                        05980002
           03  FILLER                  REDEFINES PDA-LINE.              05990002
               05  FILLER              PIC X(21).                       06000002
               05  PDA-7-22            PIC X(7).                        06010002
           03  FILLER                  REDEFINES PDA-LINE.              06020002
               05  FILLER              PIC X(22).                       06030002
               05  PDA-7-23            PIC X(7).                        06040002
           03  FILLER                  REDEFINES PDA-LINE.              06050002
               05  FILLER              PIC X(23).                       06060002
               05  PDA-7-24            PIC X(7).                        06070002
           03  FILLER                  REDEFINES PDA-LINE.              06080002
               05  FILLER              PIC X(24).                       06090002
               05  PDA-7-25            PIC X(7).                        06100002
           03  FILLER                  REDEFINES PDA-LINE.              06110002
               05  FILLER              PIC X(25).                       06120002
               05  PDA-7-26            PIC X(7).                        06130002
           03  FILLER                  REDEFINES PDA-LINE.              06140002
               05  FILLER              PIC X(26).                       06150002
               05  PDA-7-27            PIC X(7).                        06160002
           03  FILLER                  REDEFINES PDA-LINE.              06170002
               05  FILLER              PIC X(27).                       06180002
               05  PDA-7-28            PIC X(7).                        06190002
           03  FILLER                  REDEFINES PDA-LINE.              06200002
               05  FILLER              PIC X(28).                       06210002
               05  PDA-7-29            PIC X(7).                        06220002
           03  FILLER                  REDEFINES PDA-LINE.              06230002
               05  FILLER              PIC X(29).                       06240002
               05  PDA-7-30            PIC X(7).                        06250002
           03  FILLER                  REDEFINES PDA-LINE.              06260002
               05  FILLER              PIC X(30).                       06270002
               05  PDA-7-31            PIC X(7).                        06280002
           03  FILLER                  REDEFINES PDA-LINE.              06290002
               05  FILLER              PIC X(31).                       06300002
               05  PDA-7-32            PIC X(7).                        06310002
           03  FILLER                  REDEFINES PDA-LINE.              06320002
               05  FILLER              PIC X(32).                       06330002
               05  PDA-7-33            PIC X(7).                        06340002
           03  FILLER                  REDEFINES PDA-LINE.              06350002
               05  FILLER              PIC X(33).                       06360002
               05  PDA-7-34            PIC X(7).                        06370002
           03  FILLER                  REDEFINES PDA-LINE.              06380002
               05  FILLER              PIC X(34).                       06390002
               05  PDA-7-35            PIC X(7).                        06400002
           03  FILLER                  REDEFINES PDA-LINE.              06410002
               05  FILLER              PIC X(35).                       06420002
               05  PDA-7-36            PIC X(7).                        06430002
           03  FILLER                  REDEFINES PDA-LINE.              06440002
               05  FILLER              PIC X(36).                       06450002
               05  PDA-7-37            PIC X(7).                        06460002
           03  FILLER                  REDEFINES PDA-LINE.              06470002
               05  FILLER              PIC X(37).                       06480002
               05  PDA-7-38            PIC X(7).                        06490002
           03  FILLER                  REDEFINES PDA-LINE.              06500002
               05  FILLER              PIC X(38).                       06510002
               05  PDA-7-39            PIC X(7).                        06520002
           03  FILLER                  REDEFINES PDA-LINE.              06530002
               05  FILLER              PIC X(39).                       06540002
               05  PDA-7-40            PIC X(7).                        06550002
           03  FILLER                  REDEFINES PDA-LINE.              06560002
               05  FILLER              PIC X(40).                       06570002
               05  PDA-7-41            PIC X(7).                        06580002
           03  FILLER                  REDEFINES PDA-LINE.              06590002
               05  FILLER              PIC X(41).                       06600002
               05  PDA-7-42            PIC X(7).                        06610002
           03  FILLER                  REDEFINES PDA-LINE.              06620002
               05  FILLER              PIC X(42).                       06630002
               05  PDA-7-43            PIC X(7).                        06640002
           03  FILLER                  REDEFINES PDA-LINE.              06650002
               05  FILLER              PIC X(43).                       06660002
               05  PDA-7-44            PIC X(7).                        06670002
           03  FILLER                  REDEFINES PDA-LINE.              06680002
               05  FILLER              PIC X(44).                       06690002
               05  PDA-7-45            PIC X(7).                        06700002
           03  FILLER                  REDEFINES PDA-LINE.              06710002
               05  FILLER              PIC X(45).                       06720002
               05  PDA-7-46            PIC X(7).                        06730002
           03  FILLER                  REDEFINES PDA-LINE.              06740002
               05  FILLER              PIC X(46).                       06750002
               05  PDA-7-47            PIC X(7).                        06760002
           03  FILLER                  REDEFINES PDA-LINE.              06770002
               05  FILLER              PIC X(47).                       06780002
               05  PDA-7-48            PIC X(7).                        06790002
           03  FILLER                  REDEFINES PDA-LINE.              06800002
               05  FILLER              PIC X(48).                       06810002
               05  PDA-7-49            PIC X(7).                        06820002
           03  FILLER                  REDEFINES PDA-LINE.              06830002
               05  FILLER              PIC X(49).                       06840002
               05  PDA-7-50            PIC X(7).                        06850002
           03  FILLER                  REDEFINES PDA-LINE.              06860002
               05  FILLER              PIC X(50).                       06870002
               05  PDA-7-51            PIC X(7).                        06880002
           03  FILLER                  REDEFINES PDA-LINE.              06890002
               05  FILLER              PIC X(51).                       06900002
               05  PDA-7-52            PIC X(7).                        06910002
           03  FILLER                  REDEFINES PDA-LINE.              06920002
               05  FILLER              PIC X(52).                       06930002
               05  PDA-7-53            PIC X(7).                        06940002
           03  FILLER                  REDEFINES PDA-LINE.              06950002
               05  FILLER              PIC X(53).                       06960002
               05  PDA-7-54            PIC X(7).                        06970002
           03  FILLER                  REDEFINES PDA-LINE.              06980002
               05  FILLER              PIC X(54).                       06990002
               05  PDA-7-55            PIC X(7).                        07000002
           03  FILLER                  REDEFINES PDA-LINE.              07010002
               05  FILLER              PIC X(55).                       07020002
               05  PDA-7-56            PIC X(7).                        07030002
           03  FILLER                  REDEFINES PDA-LINE.              07040002
               05  FILLER              PIC X(56).                       07050002
               05  PDA-7-57            PIC X(7).                        07060002
           03  FILLER                  REDEFINES PDA-LINE.              07070002
               05  FILLER              PIC X(57).                       07080002
               05  PDA-7-58            PIC X(7).                        07090002
           03  FILLER                  REDEFINES PDA-LINE.              07100002
               05  FILLER              PIC X(58).                       07110002
               05  PDA-7-59            PIC X(7).                        07120002
           03  FILLER                  REDEFINES PDA-LINE.              07130002
               05  FILLER              PIC X(59).                       07140002
               05  PDA-7-60            PIC X(7).                        07150002
           03  FILLER                  REDEFINES PDA-LINE.              07160002
               05  FILLER              PIC X(60).                       07170002
               05  PDA-7-61            PIC X(7).                        07180002
           03  FILLER                  REDEFINES PDA-LINE.              07190002
               05  FILLER              PIC X(61).                       07200002
               05  PDA-7-62            PIC X(7).                        07210002
           03  FILLER                  REDEFINES PDA-LINE.              07220002
               05  FILLER              PIC X(62).                       07230002
               05  PDA-7-63            PIC X(7).                        07240002
           03  FILLER                  REDEFINES PDA-LINE.              07250002
               05  FILLER              PIC X(63).                       07260002
               05  PDA-7-64            PIC X(7).                        07270002
           03  FILLER                  REDEFINES PDA-LINE.              07280002
               05  FILLER              PIC X(64).                       07290002
               05  PDA-7-65            PIC X(7).                        07300002
           03  FILLER                  REDEFINES PDA-LINE.              07310002
               05  FILLER              PIC X(65).                       07320002
               05  PDA-7-66            PIC X(7).                        07330002
           03  FILLER                  REDEFINES PDA-LINE.              07340002
               05  FILLER              PIC X(66).                       07350002
               05  PDA-7-67            PIC X(7).                        07360002
           03  FILLER                  REDEFINES PDA-LINE.              07370002
               05  FILLER              PIC X(67).                       07380002
               05  PDA-7-68            PIC X(7).                        07390002
           03  FILLER                  REDEFINES PDA-LINE.              07400002
               05  FILLER              PIC X(68).                       07410002
               05  PDA-7-69            PIC X(7).                        07420002
           03  FILLER                  REDEFINES PDA-LINE.              07430002
               05  FILLER              PIC X(69).                       07440002
               05  PDA-7-70            PIC X(7).                        07450002
           03  FILLER                  REDEFINES PDA-LINE.              07460002
               05  PDA-8-01            PIC X(8).                        07470002
           03  FILLER                  REDEFINES PDA-LINE.              07480002
               05  FILLER              PIC X.                           07490002
               05  PDA-8-02            PIC X(8).                        07500002
           03  FILLER                  REDEFINES PDA-LINE.              07510002
               05  FILLER              PIC XX.                          07520002
               05  PDA-8-03            PIC X(8).                        07530002
           03  FILLER                  REDEFINES PDA-LINE.              07540002
               05  FILLER              PIC X(3).                        07550002
               05  PDA-8-04            PIC X(8).                        07560002
           03  FILLER                  REDEFINES PDA-LINE.              07570002
               05  FILLER              PIC X(4).                        07580002
               05  PDA-8-05            PIC X(8).                        07590002
           03  FILLER                  REDEFINES PDA-LINE.              07600002
               05  FILLER              PIC X(5).                        07610002
               05  PDA-8-06            PIC X(8).                        07620002
           03  FILLER                  REDEFINES PDA-LINE.              07630002
               05  FILLER              PIC X(6).                        07640002
               05  PDA-8-07            PIC X(8).                        07650002
           03  FILLER                  REDEFINES PDA-LINE.              07660002
               05  FILLER              PIC X(7).                        07670002
               05  PDA-8-08            PIC X(8).                        07680002
           03  FILLER                  REDEFINES PDA-LINE.              07690002
               05  FILLER              PIC X(8).                        07700002
               05  PDA-8-09            PIC X(8).                        07710002
           03  FILLER                  REDEFINES PDA-LINE.              07720002
               05  FILLER              PIC X(9).                        07730002
               05  PDA-8-10            PIC X(8).                        07740002
           03  FILLER                  REDEFINES PDA-LINE.              07750002
               05  FILLER              PIC X(10).                       07760002
               05  PDA-8-11            PIC X(8).                        07770002
           03  FILLER                  REDEFINES PDA-LINE.              07780002
               05  FILLER              PIC X(11).                       07790002
               05  PDA-8-12            PIC X(8).                        07800002
           03  FILLER                  REDEFINES PDA-LINE.              07810002
               05  FILLER              PIC X(12).                       07820002
               05  PDA-8-13            PIC X(8).                        07830002
           03  FILLER                  REDEFINES PDA-LINE.              07840002
               05  FILLER              PIC X(13).                       07850002
               05  PDA-8-14            PIC X(8).                        07860002
           03  FILLER                  REDEFINES PDA-LINE.              07870002
               05  FILLER              PIC X(14).                       07880002
               05  PDA-8-15            PIC X(8).                        07890002
           03  FILLER                  REDEFINES PDA-LINE.              07900002
               05  FILLER              PIC X(15).                       07910002
               05  PDA-8-16            PIC X(8).                        07920002
           03  FILLER                  REDEFINES PDA-LINE.              07930002
               05  FILLER              PIC X(16).                       07940002
               05  PDA-8-17            PIC X(8).                        07950002
           03  FILLER                  REDEFINES PDA-LINE.              07960002
               05  FILLER              PIC X(17).                       07970002
               05  PDA-8-18            PIC X(8).                        07980002
           03  FILLER                  REDEFINES PDA-LINE.              07990002
               05  FILLER              PIC X(18).                       08000002
               05  PDA-8-19            PIC X(8).                        08010002
           03  FILLER                  REDEFINES PDA-LINE.              08020002
               05  FILLER              PIC X(19).                       08030002
               05  PDA-8-20            PIC X(8).                        08040002
           03  FILLER                  REDEFINES PDA-LINE.              08050002
               05  FILLER              PIC X(20).                       08060002
               05  PDA-8-21            PIC X(8).                        08070002
           03  FILLER                  REDEFINES PDA-LINE.              08080002
               05  FILLER              PIC X(21).                       08090002
               05  PDA-8-22            PIC X(8).                        08100002
           03  FILLER                  REDEFINES PDA-LINE.              08110002
               05  FILLER              PIC X(22).                       08120002
               05  PDA-8-23            PIC X(8).                        08130002
           03  FILLER                  REDEFINES PDA-LINE.              08140002
               05  FILLER              PIC X(23).                       08150002
               05  PDA-8-24            PIC X(8).                        08160002
           03  FILLER                  REDEFINES PDA-LINE.              08170002
               05  FILLER              PIC X(24).                       08180002
               05  PDA-8-25            PIC X(8).                        08190002
           03  FILLER                  REDEFINES PDA-LINE.              08200002
               05  FILLER              PIC X(25).                       08210002
               05  PDA-8-26            PIC X(8).                        08220002
           03  FILLER                  REDEFINES PDA-LINE.              08230002
               05  FILLER              PIC X(26).                       08240002
               05  PDA-8-27            PIC X(8).                        08250002
           03  FILLER                  REDEFINES PDA-LINE.              08260002
               05  FILLER              PIC X(27).                       08270002
               05  PDA-8-28            PIC X(8).                        08280002
           03  FILLER                  REDEFINES PDA-LINE.              08290002
               05  FILLER              PIC X(28).                       08300002
               05  PDA-8-29            PIC X(8).                        08310002
           03  FILLER                  REDEFINES PDA-LINE.              08320002
               05  FILLER              PIC X(29).                       08330002
               05  PDA-8-30            PIC X(8).                        08340002
           03  FILLER                  REDEFINES PDA-LINE.              08350002
               05  FILLER              PIC X(30).                       08360002
               05  PDA-8-31            PIC X(8).                        08370002
           03  FILLER                  REDEFINES PDA-LINE.              08380002
               05  FILLER              PIC X(31).                       08390002
               05  PDA-8-32            PIC X(8).                        08400002
           03  FILLER                  REDEFINES PDA-LINE.              08410002
               05  FILLER              PIC X(32).                       08420002
               05  PDA-8-33            PIC X(8).                        08430002
           03  FILLER                  REDEFINES PDA-LINE.              08440002
               05  FILLER              PIC X(33).                       08450002
               05  PDA-8-34            PIC X(8).                        08460002
           03  FILLER                  REDEFINES PDA-LINE.              08470002
               05  FILLER              PIC X(34).                       08480002
               05  PDA-8-35            PIC X(8).                        08490002
           03  FILLER                  REDEFINES PDA-LINE.              08500002
               05  FILLER              PIC X(35).                       08510002
               05  PDA-8-36            PIC X(8).                        08520002
           03  FILLER                  REDEFINES PDA-LINE.              08530002
               05  FILLER              PIC X(36).                       08540002
               05  PDA-8-37            PIC X(8).                        08550002
           03  FILLER                  REDEFINES PDA-LINE.              08560002
               05  FILLER              PIC X(37).                       08570002
               05  PDA-8-38            PIC X(8).                        08580002
           03  FILLER                  REDEFINES PDA-LINE.              08590002
               05  FILLER              PIC X(38).                       08600002
               05  PDA-8-39            PIC X(8).                        08610002
           03  FILLER                  REDEFINES PDA-LINE.              08620002
               05  FILLER              PIC X(39).                       08630002
               05  PDA-8-40            PIC X(8).                        08640002
           03  FILLER                  REDEFINES PDA-LINE.              08650002
               05  FILLER              PIC X(40).                       08660002
               05  PDA-8-41            PIC X(8).                        08670002
           03  FILLER                  REDEFINES PDA-LINE.              08680002
               05  FILLER              PIC X(41).                       08690002
               05  PDA-8-42            PIC X(8).                        08700002
           03  FILLER                  REDEFINES PDA-LINE.              08710002
               05  FILLER              PIC X(42).                       08720002
               05  PDA-8-43            PIC X(8).                        08730002
           03  FILLER                  REDEFINES PDA-LINE.              08740002
               05  FILLER              PIC X(43).                       08750002
               05  PDA-8-44            PIC X(8).                        08760002
           03  FILLER                  REDEFINES PDA-LINE.              08770002
               05  FILLER              PIC X(44).                       08780002
               05  PDA-8-45            PIC X(8).                        08790002
           03  FILLER                  REDEFINES PDA-LINE.              08800002
               05  FILLER              PIC X(45).                       08810002
               05  PDA-8-46            PIC X(8).                        08820002
           03  FILLER                  REDEFINES PDA-LINE.              08830002
               05  FILLER              PIC X(46).                       08840002
               05  PDA-8-47            PIC X(8).                        08850002
           03  FILLER                  REDEFINES PDA-LINE.              08860002
               05  FILLER              PIC X(47).                       08870002
               05  PDA-8-48            PIC X(8).                        08880002
           03  FILLER                  REDEFINES PDA-LINE.              08890002
               05  FILLER              PIC X(48).                       08900002
               05  PDA-8-49            PIC X(8).                        08910002
           03  FILLER                  REDEFINES PDA-LINE.              08920002
               05  FILLER              PIC X(49).                       08930002
               05  PDA-8-50            PIC X(8).                        08940002
           03  FILLER                  REDEFINES PDA-LINE.              08950002
               05  FILLER              PIC X(50).                       08960002
               05  PDA-8-51            PIC X(8).                        08970002
           03  FILLER                  REDEFINES PDA-LINE.              08980002
               05  FILLER              PIC X(51).                       08990002
               05  PDA-8-52            PIC X(8).                        09000002
           03  FILLER                  REDEFINES PDA-LINE.              09010002
               05  FILLER              PIC X(52).                       09020002
               05  PDA-8-53            PIC X(8).                        09030002
           03  FILLER                  REDEFINES PDA-LINE.              09040002
               05  FILLER              PIC X(53).                       09050002
               05  PDA-8-54            PIC X(8).                        09060002
           03  FILLER                  REDEFINES PDA-LINE.              09070002
               05  FILLER              PIC X(54).                       09080002
               05  PDA-8-55            PIC X(8).                        09090002
           03  FILLER                  REDEFINES PDA-LINE.              09100002
               05  FILLER              PIC X(55).                       09110002
               05  PDA-8-56            PIC X(8).                        09120002
           03  FILLER                  REDEFINES PDA-LINE.              09130002
               05  FILLER              PIC X(56).                       09140002
               05  PDA-8-57            PIC X(8).                        09150002
           03  FILLER                  REDEFINES PDA-LINE.              09160002
               05  FILLER              PIC X(57).                       09170002
               05  PDA-8-58            PIC X(8).                        09180002
           03  FILLER                  REDEFINES PDA-LINE.              09190002
               05  FILLER              PIC X(58).                       09200002
               05  PDA-8-59            PIC X(8).                        09210002
           03  FILLER                  REDEFINES PDA-LINE.              09220002
               05  FILLER              PIC X(59).                       09230002
               05  PDA-8-60            PIC X(8).                        09240002
           03  FILLER                  REDEFINES PDA-LINE.              09250002
               05  FILLER              PIC X(60).                       09260002
               05  PDA-8-61            PIC X(8).                        09270002
           03  FILLER                  REDEFINES PDA-LINE.              09280002
               05  FILLER              PIC X(61).                       09290002
               05  PDA-8-62            PIC X(8).                        09300002
           03  FILLER                  REDEFINES PDA-LINE.              09310002
               05  FILLER              PIC X(62).                       09320002
               05  PDA-8-63            PIC X(8).                        09330002
           03  FILLER                  REDEFINES PDA-LINE.              09340002
               05  FILLER              PIC X(63).                       09350002
               05  PDA-8-64            PIC X(8).                        09360002
           03  FILLER                  REDEFINES PDA-LINE.              09370002
               05  FILLER              PIC X(64).                       09380002
               05  PDA-8-65            PIC X(8).                        09390002
           03  FILLER                  REDEFINES PDA-LINE.              09400002
               05  FILLER              PIC X(65).                       09410002
               05  PDA-8-66            PIC X(8).                        09420002
           03  FILLER                  REDEFINES PDA-LINE.              09430002
               05  FILLER              PIC X(66).                       09440002
               05  PDA-8-67            PIC X(8).                        09450002
           03  FILLER                  REDEFINES PDA-LINE.              09460002
               05  FILLER              PIC X(67).                       09470002
               05  PDA-8-68            PIC X(8).                        09480002
           03  FILLER                  REDEFINES PDA-LINE.              09490002
               05  FILLER              PIC X(68).                       09500002
               05  PDA-8-69            PIC X(8).                        09510002
           03  FILLER                  REDEFINES PDA-LINE.              09520002
               05  FILLER              PIC X(69).                       09530002
               05  PDA-8-70            PIC X(8).                        09540002
           EJECT                                                        09550002
      ***************************************************************** 09560002
      *    L I N K A G E   S E C T I O N                              * 09570002
      ***************************************************************** 09580002
                                                                        09590002
       LINKAGE SECTION.                                                 09600002
                                                                        09610002
       01  LINKPARMS.                                                   09620002
           03  FILLER                  PIC XX.                          09630002
           03  LS-PARMS.                                                09640002
               05  LS-SYSID            PIC X(7).                        09650002
               05  LS-COMMA1           PIC X.                           09660002
               05  LS-USERID           PIC X(7).                        09670002
               05  FILLER              REDEFINES LS-USERID.             09680002
                   07  LS-USERID-1     PIC X.                           09690002
                   07  LS-USERID-2     PIC X.                           09700002
                   07  LS-USERID-3     PIC X.                           09710002
                   07  LS-USERID-4     PIC X.                           09720002
                   07  LS-USERID-5     PIC X.                           09730002
                   07  LS-USERID-6     PIC X.                           09740002
                   07  LS-USERID-7     PIC X.                           09750002
               05  LS-COMMA2           PIC X.                           09760002
               05  LS-CICSREGN         PIC X(8).                        09770002
               05  FILLER              REDEFINES LS-CICSREGN.           09780002
                   07  LS-CICSREGN-1   PIC X.                           09790002
                   07  LS-CICSREGN-2   PIC X.                           09800002
                   07  LS-CICSREGN-3   PIC X.                           09810002
                   07  LS-CICSREGN-4   PIC X.                           09820002
                   07  LS-CICSREGN-5   PIC X.                           09830002
                   07  LS-CICSREGN-6   PIC X.                           09840002
                   07  LS-CICSREGN-7   PIC X.                           09850002
                   07  LS-CICSREGN-8   PIC X.                           09860002
               05  LS-COMMA3           PIC X.                           09870002
               05  LS-STORCLAS         PIC X(6).                        09880002
               05  FILLER              REDEFINES LS-STORCLAS.           09890002
                   07  LS-STORCLAS-1   PIC X.                           09900002
                   07  LS-STORCLAS-2   PIC X.                           09910002
                   07  LS-STORCLAS-3   PIC X.                           09920002
                   07  LS-STORCLAS-4   PIC X.                           09930002
                   07  LS-STORCLAS-5   PIC X.                           09940002
                   07  LS-STORCLAS-6   PIC X.                           09950002
               05  LS-COMMA4           PIC X.                           09960002
               05  LS-DATABASE         PIC X(8).                        09970002
               05  FILLER              REDEFINES LS-DATABASE.           09980002
                   07  LS-DATABASE-1   PIC X.                           09990002
                   07  LS-DATABASE-2   PIC X.                           10000002
                   07  LS-DATABASE-3   PIC X.                           10010002
                   07  LS-DATABASE-4   PIC X.                           10020002
                   07  LS-DATABASE-5   PIC X.                           10030002
                   07  LS-DATABASE-6   PIC X.                           10040002
                   07  LS-DATABASE-7   PIC X.                           10050002
                   07  LS-DATABASE-8   PIC X.                           10060002
           EJECT                                                        10070002
      ***************************************************************** 10080002
      *    P R O C E D U R E    D I V I S I O N                       * 10090002
      ***************************************************************** 10100002
                                                                        10110002
       PROCEDURE DIVISION USING LINKPARMS.                              10120002
                                                                        10130002
                                                                        10140002
      ***************************************************************** 10150002
      *                                                               * 10160002
      *    PARAGRAPH:  P00000-MAINLINE                                * 10170002
      *                                                               * 10180002
      *    FUNCTION :  PROGRAM ENTRY, OPEN FILES, PROCESS.            * 10190002
      *                                                               * 10200002
      *    CALLED BY:  NONE                                           * 10210002
      *                                                               * 10220002
      ***************************************************************** 10230002
                                                                        10240002
       P00000-MAINLINE.                                                 10250002
                                                                        10260002
           PERFORM P01000-EDIT-PARMS THRU P01000-EXIT.                  10270002
                                                                        10280002
           OPEN INPUT INPUT-CRDB2                                       10290002
                      INPUT-CRIMS                                       10300002
                      INPUT-CRVSAM                                      10310002
                      INPUT-DLDB2                                       10320002
                      INPUT-DLIMS                                       10330002
                      INPUT-DLVSAM                                      10340002
                      INPUT-BPDAB02                                     10350002
                      INPUT-PRITEM                                      10360002
                      INPUT-PRSUPPLR                                    10370002
                      INPUT-PRITMSUP                                    10380002
                      INPUT-PRPURTYP                                    10390002
                      INPUT-PRAFFSUP                                    10400002
                      INPUT-PRORDLOG                                    10410002
                OUTPUT OUTPUT-CRDB2                                     10420002
                       OUTPUT-CRIMS                                     10430002
                       OUTPUT-CRVSAM                                    10440002
                       OUTPUT-DLDB2                                     10450002
                       OUTPUT-DLIMS                                     10460002
                       OUTPUT-DLVSAM                                    10470002
                       OUTPUT-BPDAB02                                   10480002
                       OUTPUT-PRITEM                                    10490002
                       OUTPUT-PRSUPPLR                                  10500002
                       OUTPUT-PRITMSUP                                  10510002
                       OUTPUT-PRPURTYP                                  10520002
                       OUTPUT-PRAFFSUP                                  10530002
                       OUTPUT-PRORDLOG.                                 10540002
                                                                        10550002
           PERFORM P02000-CRDB2 THRU P02000-EXIT                        10560002
               UNTIL END-OF-CRDB2.                                      10570002
                                                                        10580002
           PERFORM P03000-CRIMS THRU P03000-EXIT                        10590002
               UNTIL END-OF-CRIMS.                                      10600002
                                                                        10610002
           PERFORM P04000-CRVSAM THRU P04000-EXIT                       10620002
               UNTIL END-OF-CRVSAM.                                     10630002
                                                                        10640002
           PERFORM P05000-DLDB2 THRU P05000-EXIT                        10650002
               UNTIL END-OF-DLDB2.                                      10660002
                                                                        10670002
           PERFORM P06000-DLIMS THRU P06000-EXIT                        10680002
               UNTIL END-OF-DLIMS.                                      10690002
                                                                        10700002
           PERFORM P07000-DLVSAM THRU P07000-EXIT                       10710002
               UNTIL END-OF-DLVSAM.                                     10720002
                                                                        10730002
           PERFORM P08000-BPDAB02 THRU P08000-EXIT                      10740002
               UNTIL END-OF-BPDAB02.                                    10750002
                                                                        10760002
           PERFORM P09000-PRITEM THRU P09000-EXIT                       10770002
               UNTIL END-OF-PRITEM.                                     10780002
                                                                        10790002
           PERFORM P10000-PRSUPPLR THRU P10000-EXIT                     10800002
               UNTIL END-OF-PRSUPPLR.                                   10810002
                                                                        10820002
           PERFORM P11000-PRITMSUP THRU P11000-EXIT                     10830002
               UNTIL END-OF-PRITMSUP.                                   10840002
                                                                        10850002
           PERFORM P12000-PRPURTYP THRU P12000-EXIT                     10860002
               UNTIL END-OF-PRPURTYP.                                   10870002
                                                                        10880002
           PERFORM P13000-PRAFFSUP THRU P13000-EXIT                     10881002
               UNTIL END-OF-PRAFFSUP.                                   10882002
                                                                        10883002
           PERFORM P14000-PRORDLOG THRU P14000-EXIT                     10884002
               UNTIL END-OF-PRORDLOG.                                   10885002
                                                                        10886002
           CLOSE INPUT-CRDB2                                            10887002
                 INPUT-CRIMS                                            10888002
                 INPUT-CRVSAM                                           10889002
                 INPUT-DLDB2                                            10890002
                 INPUT-DLIMS                                            10900002
                 INPUT-DLVSAM                                           10910002
                 INPUT-BPDAB02                                          10920002
                 INPUT-PRITEM                                           10930002
                 INPUT-PRSUPPLR                                         10940002
                 INPUT-PRITMSUP                                         10950002
                 INPUT-PRPURTYP                                         10960002
                 INPUT-PRAFFSUP                                         10970002
                 INPUT-PRORDLOG                                         10980002
                 OUTPUT-CRDB2                                           10990002
                 OUTPUT-CRIMS                                           11000002
                 OUTPUT-CRVSAM                                          11010002
                 OUTPUT-DLDB2                                           11020002
                 OUTPUT-DLIMS                                           11030002
                 OUTPUT-DLVSAM                                          11040002
                 OUTPUT-BPDAB02                                         11050002
                 OUTPUT-PRITEM                                          11060002
                 OUTPUT-PRSUPPLR                                        11070002
                 OUTPUT-PRITMSUP                                        11080002
                 OUTPUT-PRPURTYP                                        11090002
                 OUTPUT-PRAFFSUP                                        11100002
                 OUTPUT-PRORDLOG.                                       11101002
                                                                        11102002
           STOP RUN.                                                    11103002
                                                                        11104002
       P00000-EXIT.                                                     11105002
           EXIT.                                                        11106002
           EJECT                                                        11107002
      ***************************************************************** 11108002
      *                                                               * 11109002
      *    PARAGRAPH:  P01000-EDIT-PARMS                              * 11110002
      *                                                               * 11120002
      *    FUNCTION :  ROUTINE TO EDIT THE PARMS RECEIVED.            * 11130002
      *                                                               * 11140002
      *    CALLED BY:  P00000-MAINLINE                                * 11150002
      *                                                               * 11160002
      ***************************************************************** 11170002
                                                                        11180002
       P01000-EDIT-PARMS.                                               11190002
                                                                        11200002
           DISPLAY ' '.                                                 11210002
           DISPLAY ' '.                                                 11220002
           DISPLAY 'PARM RECEIVED = ' LS-PARMS.                         11230002
           DISPLAY ' '.                                                 11240002
           DISPLAY 'SYSID    = ' LS-SYSID.                              11250002
           DISPLAY 'USERID   = ' LS-USERID.                             11260002
           DISPLAY 'CICSREGN = ' LS-CICSREGN.                           11270002
           DISPLAY 'STORCLAS = ' LS-STORCLAS.                           11280002
           DISPLAY 'DATABASE = ' LS-DATABASE.                           11290002
           DISPLAY ' '.                                                 11300002
                                                                        11310002
           EVALUATE TRUE                                                11320002
               WHEN LS-COMMA1 NOT = ','                                 11330002
               WHEN LS-COMMA2 NOT = ','                                 11340002
               WHEN LS-COMMA3 NOT = ','                                 11350002
               WHEN LS-COMMA4 NOT = ','                                 11360002
                   MOVE 'Y' TO WS-PARM-ERROR                            11370002
               WHEN LS-USERID-1 NOT > SPACES                            11380002
                   MOVE 'Y' TO WS-PARM-ERROR                            11390002
               WHEN LS-USERID-2 NOT > SPACES                            11400002
                   MOVE 'Y' TO WS-PARM-ERROR                            11410002
               WHEN LS-USERID-3 NOT > SPACES                            11420002
                   MOVE 'Y' TO WS-PARM-ERROR                            11430002
               WHEN LS-USERID-4 NOT > SPACES                            11440002
                   MOVE 'Y' TO WS-PARM-ERROR                            11450002
               WHEN LS-USERID-5 NOT > SPACES                            11460002
                   MOVE 'Y' TO WS-PARM-ERROR                            11470002
               WHEN LS-USERID-6 NOT > SPACES                            11480002
                   MOVE 'Y' TO WS-PARM-ERROR                            11490002
               WHEN LS-USERID-7 NOT > SPACES                            11500002
                   MOVE 'Y' TO WS-PARM-ERROR                            11510002
               WHEN LS-CICSREGN-1 NOT > SPACES                          11520002
                   MOVE 'Y' TO WS-PARM-ERROR                            11530002
               WHEN LS-CICSREGN-2 NOT > SPACES                          11540002
                   MOVE 'Y' TO WS-PARM-ERROR                            11550002
               WHEN LS-CICSREGN-3 NOT > SPACES                          11560002
                   MOVE 'Y' TO WS-PARM-ERROR                            11570002
               WHEN LS-CICSREGN-4 NOT > SPACES                          11580002
                   MOVE 'Y' TO WS-PARM-ERROR                            11590002
               WHEN LS-CICSREGN-5 NOT > SPACES                          11600002
                   MOVE 'Y' TO WS-PARM-ERROR                            11610002
               WHEN LS-CICSREGN-6 NOT > SPACES                          11620002
                   MOVE 'Y' TO WS-PARM-ERROR                            11630002
               WHEN LS-CICSREGN-7 NOT > SPACES                          11640002
                   MOVE 'Y' TO WS-PARM-ERROR                            11650002
               WHEN LS-CICSREGN-8 NOT > SPACES                          11660002
                   MOVE 'Y' TO WS-PARM-ERROR                            11670002
               WHEN LS-STORCLAS-1 NOT > SPACES                          11680002
                   MOVE 'Y' TO WS-PARM-ERROR                            11690002
               WHEN LS-STORCLAS-2 NOT > SPACES                          11700002
                   MOVE 'Y' TO WS-PARM-ERROR                            11710002
               WHEN LS-STORCLAS-3 NOT > SPACES                          11720002
                   MOVE 'Y' TO WS-PARM-ERROR                            11730002
               WHEN LS-STORCLAS-4 NOT > SPACES                          11740002
                   MOVE 'Y' TO WS-PARM-ERROR                            11750002
               WHEN LS-STORCLAS-5 NOT > SPACES                          11760002
                   MOVE 'Y' TO WS-PARM-ERROR                            11770002
               WHEN LS-STORCLAS-6 NOT > SPACES                          11780002
                   MOVE 'Y' TO WS-PARM-ERROR                            11790002
               WHEN LS-DATABASE   NOT > SPACES                          11800002
                   MOVE 'Y' TO WS-PARM-ERROR                            11810002
           END-EVALUATE.                                                11820002
                                                                        11830002
           IF PARM-ERROR                                                11840002
               DISPLAY ' '                                              11850002
               DISPLAY 'PARMS PASSED TO PGM FAILED EDITS'               11860002
               DISPLAY ' '                                              11870002
               DISPLAY ' '                                              11880002
               MOVE +88 TO WS-RETURN-CODE                               11890002
               CALL 'ILBOABN0' USING WS-RETURN-CODE                     11900002
               MOVE WS-RETURN-CODE TO RETURN-CODE                       11910002
               STOP RUN                                                 11920002
           END-IF.                                                      11930002
                                                                        11940002
           IF LS-SYSID NOT = 'BFHJLS0' AND                              11950002
               LS-SYSID NOT = 'PFHPWB0' AND                             11960002
               LS-SYSID NOT = 'BFHLXR0'                                 11970002
                   IF LS-USERID = 'PDAPROD' OR                          11980002
                       LS-USERID = 'PDADEMO'                            11990002
                           DISPLAY ' '                                  12000002
                           DISPLAY 'PARMS PASSED TO PGM FAILED EDITS'   12010002
                           DISPLAY ' '                                  12020002
                           DISPLAY '   SYSID OF "' LS-SYSID '" IS NOT ' 12030002
                                   'ALLOWED TO USE USERID OF "'         12040002
                                   LS-USERID '"'                        12050002
                           DISPLAY ' '                                  12060002
                           DISPLAY ' '                                  12070002
                           MOVE +99 TO WS-RETURN-CODE                   12080002
                           CALL 'ILBOABN0' USING WS-RETURN-CODE         12090002
                           MOVE WS-RETURN-CODE TO RETURN-CODE           12100002
                           STOP RUN                                     12110002
                   END-IF                                               12120002
           END-IF.                                                      12130002
                                                                        12140002
      ***************************************************************** 12150002
      *    REFORMAT PARAMETERS AS NECESSARY FOR USAGE LATER,          * 12160002
      *    RIGHT JUSTIFY DB2 DATABASE NAME                            * 12170002
      ***************************************************************** 12180002
                                                                        12190002
           MOVE WS-FIELD-LTH-MAX       TO WS-SUB2.                      12200002
           MOVE LS-DATABASE            TO WS-RIGHT-JUSTIFY-IN.          12210002
           MOVE SPACES                 TO WS-RIGHT-JUSTIFY-OUT.         12220002
                                                                        12230002
           PERFORM  P80000-RIGHT-JUSTIFY  THRU P80000-EXIT              12240002
               VARYING WS-SUB1 FROM WS-FIELD-LTH-MAX BY -1              12250002
                   UNTIL WS-SUB1 < 1.                                   12260002
                                                                        12270002
           MOVE WS-RIGHT-JUSTIFY-OUT-08                                 12280002
                                       TO LS-DATABASE.                  12290002
                                                                        12300002
       P01000-EXIT.                                                     12310002
           EXIT.                                                        12320002
           EJECT                                                        12330002
      ***************************************************************** 12340002
      *                                                               * 12350002
      *    PARAGRAPH:  P02000-CRDB2                                   * 12360002
      *                                                               * 12370002
      *    FUNCTION :  ROUTINE TO COPY CRDB2                          * 12380002
      *                                                               * 12390002
      *    CALLED BY:  P00000-MAINLINE                                * 12400002
      *                                                               * 12410002
      ***************************************************************** 12420002
                                                                        12430002
       P02000-CRDB2.                                                    12440002
                                                                        12450002
           READ INPUT-CRDB2 INTO PDA-WORK-FORMAT                        12460002
               AT END                                                   12470002
                   MOVE 'Y' TO WS-END-OF-CRDB2                          12480002
                   GO TO P02000-EXIT.                                   12490002
                                                                        12500002
           IF PDA-COMMENT                                               12510002
               GO TO P02000-EXIT                                        12520002
           END-IF.                                                      12530002
                                                                        12540002
           PERFORM P90000-CONVERT THRU P90000-EXIT.                     12550002
                                                                        12560002
           WRITE OUTPUT-CRDB2-REC FROM PDA-WORK-FORMAT.                 12570002
                                                                        12580002
       P02000-EXIT.                                                     12590002
           EXIT.                                                        12600002
           EJECT                                                        12610002
      ***************************************************************** 12620002
      *                                                               * 12630002
      *    PARAGRAPH:  P03000-CRIMS                                   * 12640002
      *                                                               * 12650002
      *    FUNCTION :  ROUTINE TO COPY CRIMS                          * 12660002
      *                                                               * 12670002
      *    CALLED BY:  P00000-MAINLINE                                * 12680002
      *                                                               * 12690002
      ***************************************************************** 12700002
                                                                        12710002
       P03000-CRIMS.                                                    12720002
                                                                        12730002
           READ INPUT-CRIMS INTO PDA-WORK-FORMAT                        12740002
               AT END                                                   12750002
                   MOVE 'Y' TO WS-END-OF-CRIMS                          12760002
                   GO TO P03000-EXIT.                                   12770002
                                                                        12780002
           IF PDA-COMMENT                                               12790002
               GO TO P03000-EXIT                                        12800002
           END-IF.                                                      12810002
                                                                        12820002
           PERFORM P90000-CONVERT THRU P90000-EXIT.                     12830002
                                                                        12840002
           WRITE OUTPUT-CRIMS-REC FROM PDA-WORK-FORMAT.                 12850002
                                                                        12860002
       P03000-EXIT.                                                     12870002
           EXIT.                                                        12880002
           EJECT                                                        12890002
      ***************************************************************** 12900002
      *                                                               * 12910002
      *    PARAGRAPH:  P04000-CRVSAM                                  * 12920002
      *                                                               * 12930002
      *    FUNCTION :  ROUTINE TO COPY CRVSAM                         * 12940002
      *                                                               * 12950002
      *    CALLED BY:  P00000-MAINLINE                                * 12960002
      *                                                               * 12970002
      ***************************************************************** 12980002
                                                                        12990002
       P04000-CRVSAM.                                                   13000002
                                                                        13010002
           READ INPUT-CRVSAM INTO PDA-WORK-FORMAT                       13020002
               AT END                                                   13030002
                   MOVE 'Y' TO WS-END-OF-CRVSAM                         13040002
                   GO TO P04000-EXIT.                                   13050002
                                                                        13060002
           IF PDA-COMMENT                                               13070002
               GO TO P04000-EXIT                                        13080002
           END-IF.                                                      13090002
                                                                        13100002
           PERFORM P90000-CONVERT THRU P90000-EXIT.                     13110002
                                                                        13120002
           WRITE OUTPUT-CRVSAM-REC FROM PDA-WORK-FORMAT.                13130002
                                                                        13140002
       P04000-EXIT.                                                     13150002
           EXIT.                                                        13160002
           EJECT                                                        13170002
      ***************************************************************** 13180002
      *                                                               * 13190002
      *    PARAGRAPH:  P05000-DLDB2                                   * 13200002
      *                                                               * 13210002
      *    FUNCTION :  ROUTINE TO COPY DLDB2                          * 13220002
      *                                                               * 13230002
      *    CALLED BY:  P00000-MAINLINE                                * 13240002
      *                                                               * 13250002
      ***************************************************************** 13260002
                                                                        13270002
       P05000-DLDB2.                                                    13280002
                                                                        13290002
           READ INPUT-DLDB2 INTO PDA-WORK-FORMAT                        13300002
               AT END                                                   13310002
                   MOVE 'Y' TO WS-END-OF-DLDB2                          13320002
                   GO TO P05000-EXIT.                                   13330002
                                                                        13340002
           IF PDA-COMMENT                                               13350002
               GO TO P05000-EXIT                                        13360002
           END-IF.                                                      13370002
                                                                        13380002
           PERFORM P90000-CONVERT THRU P90000-EXIT.                     13390002
                                                                        13400002
           WRITE OUTPUT-DLDB2-REC FROM PDA-WORK-FORMAT.                 13410002
                                                                        13420002
       P05000-EXIT.                                                     13430002
           EXIT.                                                        13440002
           EJECT                                                        13450002
      ***************************************************************** 13460002
      *                                                               * 13470002
      *    PARAGRAPH:  P06000-DLIMS                                   * 13480002
      *                                                               * 13490002
      *    FUNCTION :  ROUTINE TO COPY DLIMS                          * 13500002
      *                                                               * 13510002
      *    CALLED BY:  P00000-MAINLINE                                * 13520002
      *                                                               * 13530002
      ***************************************************************** 13540002
                                                                        13550002
       P06000-DLIMS.                                                    13560002
                                                                        13570002
           READ INPUT-DLIMS INTO PDA-WORK-FORMAT                        13580002
               AT END                                                   13590002
                   MOVE 'Y' TO WS-END-OF-DLIMS                          13600002
                   GO TO P06000-EXIT.                                   13610002
                                                                        13620002
           IF PDA-COMMENT                                               13630002
               GO TO P06000-EXIT                                        13640002
           END-IF.                                                      13650002
                                                                        13660002
           PERFORM P90000-CONVERT THRU P90000-EXIT.                     13670002
                                                                        13680002
           WRITE OUTPUT-DLIMS-REC FROM PDA-WORK-FORMAT.                 13690002
                                                                        13700002
       P06000-EXIT.                                                     13710002
           EXIT.                                                        13720002
           EJECT                                                        13730002
      ***************************************************************** 13740002
      *                                                               * 13750002
      *    PARAGRAPH:  P07000-DLVSAM                                  * 13760002
      *                                                               * 13770002
      *    FUNCTION :  ROUTINE TO COPY DLVSAM                         * 13780002
      *                                                               * 13790002
      *    CALLED BY:  P00000-MAINLINE                                * 13800002
      *                                                               * 13810002
      ***************************************************************** 13820002
                                                                        13830002
       P07000-DLVSAM.                                                   13840002
                                                                        13850002
           READ INPUT-DLVSAM INTO PDA-WORK-FORMAT                       13860002
               AT END                                                   13870002
                   MOVE 'Y' TO WS-END-OF-DLVSAM                         13880002
                   GO TO P07000-EXIT.                                   13890002
                                                                        13900002
           IF PDA-COMMENT                                               13910002
               GO TO P07000-EXIT                                        13920002
           END-IF.                                                      13930002
                                                                        13940002
           PERFORM P90000-CONVERT THRU P90000-EXIT.                     13950002
                                                                        13960002
           WRITE OUTPUT-DLVSAM-REC FROM PDA-WORK-FORMAT.                13970002
                                                                        13980002
       P07000-EXIT.                                                     13990002
           EXIT.                                                        14000002
           EJECT                                                        14010002
      ***************************************************************** 14020002
      *                                                               * 14030002
      *    PARAGRAPH:  P08000-BPDAB02                                 * 14040002
      *                                                               * 14050002
      *    FUNCTION :  ROUTINE TO COPY BPDAB02                        * 14060002
      *                                                               * 14070002
      *    CALLED BY:  P00000-MAINLINE                                * 14080002
      *                                                               * 14090002
      ***************************************************************** 14100002
                                                                        14110002
       P08000-BPDAB02.                                                  14120002
                                                                        14130002
           READ INPUT-BPDAB02 INTO PDA-WORK-FORMAT                      14140002
               AT END                                                   14150002
                   MOVE 'Y' TO WS-END-OF-BPDAB02                        14160002
                   GO TO P08000-EXIT.                                   14170002
                                                                        14180002
           IF PDA-COMMENT                                               14190002
               GO TO P08000-EXIT                                        14200002
           END-IF.                                                      14210002
                                                                        14220002
           PERFORM P90000-CONVERT THRU P90000-EXIT.                     14230002
                                                                        14240002
           WRITE OUTPUT-BPDAB02-REC FROM PDA-WORK-FORMAT.               14250002
                                                                        14260002
       P08000-EXIT.                                                     14270002
           EXIT.                                                        14280002
           EJECT                                                        14290002
      ***************************************************************** 14300002
      *                                                               * 14310002
      *    PARAGRAPH:  P09000-PRITEM                                  * 14320002
      *                                                               * 14330002
      *    FUNCTION :  ROUTINE TO COPY PRITEM                         * 14340002
      *                                                               * 14350002
      *    CALLED BY:  P00000-MAINLINE                                * 14360002
      *                                                               * 14370002
      ***************************************************************** 14380002
                                                                        14390002
       P09000-PRITEM.                                                   14400002
                                                                        14410002
           READ INPUT-PRITEM INTO PDA-WORK-FORMAT                       14420002
               AT END                                                   14430002
                   MOVE 'Y' TO WS-END-OF-PRITEM                         14440002
                   GO TO P09000-EXIT.                                   14450002
                                                                        14460002
           IF PDA-COMMENT                                               14470002
               GO TO P09000-EXIT                                        14480002
           END-IF.                                                      14490002
                                                                        14500002
           PERFORM P90000-CONVERT THRU P90000-EXIT.                     14510002
                                                                        14520002
           WRITE OUTPUT-PRITEM-REC FROM PDA-WORK-FORMAT.                14530002
                                                                        14540002
       P09000-EXIT.                                                     14550002
           EXIT.                                                        14560002
           EJECT                                                        14570002
      ***************************************************************** 14580002
      *                                                               * 14590002
      *    PARAGRAPH:  P10000-PRSUPPLR                                * 14600002
      *                                                               * 14610002
      *    FUNCTION :  ROUTINE TO COPY PRSUPPLR                       * 14620002
      *                                                               * 14630002
      *    CALLED BY:  P00000-MAINLINE                                * 14640002
      *                                                               * 14650002
      ***************************************************************** 14660002
                                                                        14670002
       P10000-PRSUPPLR.                                                 14680002
                                                                        14690002
           READ INPUT-PRSUPPLR INTO PDA-WORK-FORMAT                     14700002
               AT END                                                   14710002
                   MOVE 'Y' TO WS-END-OF-PRSUPPLR                       14720002
                   GO TO P10000-EXIT.                                   14730002
                                                                        14740002
           IF PDA-COMMENT                                               14750002
               GO TO P10000-EXIT                                        14760002
           END-IF.                                                      14770002
                                                                        14780002
           PERFORM P90000-CONVERT THRU P90000-EXIT.                     14790002
                                                                        14800002
           WRITE OUTPUT-PRSUPPLR-REC FROM PDA-WORK-FORMAT.              14810002
                                                                        14820002
       P10000-EXIT.                                                     14830002
           EXIT.                                                        14840002
           EJECT                                                        14850002
      ***************************************************************** 14860002
      *                                                               * 14870002
      *    PARAGRAPH:  P11000-PRITMSUP                                * 14880002
      *                                                               * 14890002
      *    FUNCTION :  ROUTINE TO COPY PRITMSUP                       * 14900002
      *                                                               * 14910002
      *    CALLED BY:  P00000-MAINLINE                                * 14920002
      *                                                               * 14930002
      ***************************************************************** 14940002
                                                                        14950002
       P11000-PRITMSUP.                                                 14960002
                                                                        14970002
           READ INPUT-PRITMSUP INTO PDA-WORK-FORMAT                     14980002
               AT END                                                   14990002
                   MOVE 'Y' TO WS-END-OF-PRITMSUP                       15000002
                   GO TO P11000-EXIT.                                   15010002
                                                                        15020002
           IF PDA-COMMENT                                               15030002
               GO TO P11000-EXIT                                        15040002
           END-IF.                                                      15050002
                                                                        15060002
           PERFORM P90000-CONVERT THRU P90000-EXIT.                     15070002
                                                                        15080002
           WRITE OUTPUT-PRITMSUP-REC FROM PDA-WORK-FORMAT.              15090002
                                                                        15100002
       P11000-EXIT.                                                     15110002
           EXIT.                                                        15120002
           EJECT                                                        15130002
      ***************************************************************** 15140002
      *                                                               * 15150002
      *    PARAGRAPH:  P12000-PRPURTYP                                * 15160002
      *                                                               * 15170002
      *    FUNCTION :  ROUTINE TO COPY PRPURTYP                       * 15180002
      *                                                               * 15190002
      *    CALLED BY:  P00000-MAINLINE                                * 15200002
      *                                                               * 15210002
      ***************************************************************** 15220002
                                                                        15230002
       P12000-PRPURTYP.                                                 15240002
                                                                        15250002
           READ INPUT-PRPURTYP INTO PDA-WORK-FORMAT                     15260002
               AT END                                                   15270002
                   MOVE 'Y' TO WS-END-OF-PRPURTYP                       15280002
                   GO TO P12000-EXIT.                                   15290002
                                                                        15300002
           IF PDA-COMMENT                                               15310002
               GO TO P12000-EXIT                                        15320002
           END-IF.                                                      15330002
                                                                        15340002
           PERFORM P90000-CONVERT THRU P90000-EXIT.                     15350002
                                                                        15360002
           WRITE OUTPUT-PRPURTYP-REC FROM PDA-WORK-FORMAT.              15370002
                                                                        15380002
       P12000-EXIT.                                                     15390002
           EXIT.                                                        15400002
           EJECT                                                        15410002
      ***************************************************************** 15420002
      *                                                               * 15430002
      *    PARAGRAPH:  P13000-PRAFFSUP                                * 15440002
      *                                                               * 15450002
      *    FUNCTION :  ROUTINE TO COPY PRAFFSUP                       * 15460002
      *                                                               * 15470002
      *    CALLED BY:  P00000-MAINLINE                                * 15470102
      *                                                               * 15470202
      ***************************************************************** 15470302
                                                                        15470402
       P13000-PRAFFSUP.                                                 15470502
                                                                        15470602
           READ INPUT-PRAFFSUP INTO PDA-WORK-FORMAT                     15470702
               AT END                                                   15470802
                   MOVE 'Y' TO WS-END-OF-PRAFFSUP                       15470902
                   GO TO P13000-EXIT.                                   15471002
                                                                        15471102
           IF PDA-COMMENT                                               15471202
               GO TO P13000-EXIT                                        15471302
           END-IF.                                                      15471402
                                                                        15471502
           PERFORM P90000-CONVERT THRU P90000-EXIT.                     15471602
                                                                        15471702
           WRITE OUTPUT-PRAFFSUP-REC FROM PDA-WORK-FORMAT.              15471802
                                                                        15471902
       P13000-EXIT.                                                     15472002
           EXIT.                                                        15472102
           EJECT                                                        15472202
      ***************************************************************** 15472302
      *                                                               * 15472402
      *    PARAGRAPH:  P14000-PRORDLOG                                * 15472502
      *                                                               * 15472602
      *    FUNCTION :  ROUTINE TO COPY PRORDLOG                       * 15472702
      *                                                               * 15472802
      *    CALLED BY:  P00000-MAINLINE                                * 15472902
      *                                                               * 15473002
      ***************************************************************** 15473102
                                                                        15473202
       P14000-PRORDLOG.                                                 15473302
                                                                        15473402
           READ INPUT-PRORDLOG INTO PDA-WORK-FORMAT                     15473502
               AT END                                                   15473602
                   MOVE 'Y' TO WS-END-OF-PRORDLOG                       15473702
                   GO TO P14000-EXIT.                                   15473802
                                                                        15473902
           IF PDA-COMMENT                                               15474002
               GO TO P14000-EXIT                                        15474102
           END-IF.                                                      15474202
                                                                        15474302
           PERFORM P90000-CONVERT THRU P90000-EXIT.                     15474402
                                                                        15474502
           WRITE OUTPUT-PRORDLOG-REC FROM PDA-WORK-FORMAT.              15474602
                                                                        15474702
       P14000-EXIT.                                                     15474802
           EXIT.                                                        15474902
           EJECT                                                        15475002
      ***************************************************************** 15475102
      *                                                               * 15475202
      *    PARAGRAPH:  P80000-RIGHT-JUSTIFY                           * 15475302
      *                                                               * 15475402
      *    FUNCTION :  ROUTINE TO RIGHT JUSTIFY A FIELD               * 15475502
      *                                                               * 15475602
      *    CALLED BY:  GLOBAL                                         * 15475702
      *                                                               * 15475802
      ***************************************************************** 15475902
                                                                        15476002
       P80000-RIGHT-JUSTIFY.                                            15477002
                                                                        15478002
           IF WS-RIGHT-JUSTIFY-IN-R (WS-SUB1) NOT > SPACES              15479002
               GO TO P80000-EXIT.                                       15479102
                                                                        15479202
           MOVE WS-RIGHT-JUSTIFY-IN-R (WS-SUB1)                         15479302
                                   TO WS-RIGHT-JUSTIFY-OUT-R (WS-SUB2). 15479402
                                                                        15479502
           COMPUTE WS-SUB2 = WS-SUB2 - 1.                               15479602
                                                                        15479702
       P80000-EXIT.                                                     15479802
           EXIT.                                                        15479902
           EJECT                                                        15480002
      ***************************************************************** 15481002
      *                                                               * 15482002
      *    PARAGRAPH:  P90000-CONVERT                                 * 15483002
      *                                                               * 15484002
      *    FUNCTION :  ROUTINE TO PERFORM CONTROL CARD CONVERSION     * 15485002
      *                                                               * 15486002
      *    CALLED BY:  ALL                                            * 15487002
      *                                                               * 15488002
      ***************************************************************** 15489002
                                                                        15490002
       P90000-CONVERT.                                                  15500002
                                                                        15510002
           EVALUATE TRUE                                                15520002
               WHEN PDA-7-01 = WS-USERID                                15530002
                   MOVE LS-USERID TO PDA-7-01                           15540002
               WHEN PDA-7-02 = WS-USERID                                15550002
                   MOVE LS-USERID TO PDA-7-02                           15560002
               WHEN PDA-7-03 = WS-USERID                                15570002
                   MOVE LS-USERID TO PDA-7-03                           15580002
               WHEN PDA-7-04 = WS-USERID                                15590002
                   MOVE LS-USERID TO PDA-7-04                           15600002
               WHEN PDA-7-05 = WS-USERID                                15610002
                   MOVE LS-USERID TO PDA-7-05                           15620002
               WHEN PDA-7-06 = WS-USERID                                15630002
                   MOVE LS-USERID TO PDA-7-06                           15640002
               WHEN PDA-7-07 = WS-USERID                                15650002
                   MOVE LS-USERID TO PDA-7-07                           15660002
               WHEN PDA-7-08 = WS-USERID                                15670002
                   MOVE LS-USERID TO PDA-7-08                           15680002
               WHEN PDA-7-09 = WS-USERID                                15690002
                   MOVE LS-USERID TO PDA-7-09                           15700002
               WHEN PDA-7-10 = WS-USERID                                15710002
                   MOVE LS-USERID TO PDA-7-10                           15720002
               WHEN PDA-7-11 = WS-USERID                                15730002
                   MOVE LS-USERID TO PDA-7-11                           15740002
               WHEN PDA-7-12 = WS-USERID                                15750002
                   MOVE LS-USERID TO PDA-7-12                           15760002
               WHEN PDA-7-13 = WS-USERID                                15770002
                   MOVE LS-USERID TO PDA-7-13                           15780002
               WHEN PDA-7-14 = WS-USERID                                15790002
                   MOVE LS-USERID TO PDA-7-14                           15800002
               WHEN PDA-7-15 = WS-USERID                                15810002
                   MOVE LS-USERID TO PDA-7-15                           15820002
               WHEN PDA-7-16 = WS-USERID                                15830002
                   MOVE LS-USERID TO PDA-7-16                           15840002
               WHEN PDA-7-17 = WS-USERID                                15850002
                   MOVE LS-USERID TO PDA-7-17                           15860002
               WHEN PDA-7-18 = WS-USERID                                15870002
                   MOVE LS-USERID TO PDA-7-18                           15880002
               WHEN PDA-7-19 = WS-USERID                                15890002
                   MOVE LS-USERID TO PDA-7-19                           15900002
               WHEN PDA-7-20 = WS-USERID                                15910002
                   MOVE LS-USERID TO PDA-7-20                           15920002
               WHEN PDA-7-21 = WS-USERID                                15930002
                   MOVE LS-USERID TO PDA-7-21                           15940002
               WHEN PDA-7-22 = WS-USERID                                15950002
                   MOVE LS-USERID TO PDA-7-22                           15960002
               WHEN PDA-7-23 = WS-USERID                                15970002
                   MOVE LS-USERID TO PDA-7-23                           15980002
               WHEN PDA-7-24 = WS-USERID                                15990002
                   MOVE LS-USERID TO PDA-7-24                           16000002
               WHEN PDA-7-25 = WS-USERID                                16010002
                   MOVE LS-USERID TO PDA-7-25                           16020002
               WHEN PDA-7-26 = WS-USERID                                16030002
                   MOVE LS-USERID TO PDA-7-26                           16040002
               WHEN PDA-7-27 = WS-USERID                                16050002
                   MOVE LS-USERID TO PDA-7-27                           16060002
               WHEN PDA-7-28 = WS-USERID                                16070002
                   MOVE LS-USERID TO PDA-7-28                           16080002
               WHEN PDA-7-29 = WS-USERID                                16090002
                   MOVE LS-USERID TO PDA-7-29                           16100002
               WHEN PDA-7-30 = WS-USERID                                16110002
                   MOVE LS-USERID TO PDA-7-30                           16120002
               WHEN PDA-7-31 = WS-USERID                                16130002
                   MOVE LS-USERID TO PDA-7-31                           16140002
               WHEN PDA-7-32 = WS-USERID                                16150002
                   MOVE LS-USERID TO PDA-7-32                           16160002
               WHEN PDA-7-33 = WS-USERID                                16170002
                   MOVE LS-USERID TO PDA-7-33                           16180002
               WHEN PDA-7-34 = WS-USERID                                16190002
                   MOVE LS-USERID TO PDA-7-34                           16200002
               WHEN PDA-7-35 = WS-USERID                                16210002
                   MOVE LS-USERID TO PDA-7-35                           16220002
               WHEN PDA-7-36 = WS-USERID                                16230002
                   MOVE LS-USERID TO PDA-7-36                           16240002
               WHEN PDA-7-37 = WS-USERID                                16250002
                   MOVE LS-USERID TO PDA-7-37                           16260002
               WHEN PDA-7-38 = WS-USERID                                16270002
                   MOVE LS-USERID TO PDA-7-38                           16280002
               WHEN PDA-7-39 = WS-USERID                                16290002
                   MOVE LS-USERID TO PDA-7-39                           16300002
               WHEN PDA-7-40 = WS-USERID                                16310002
                   MOVE LS-USERID TO PDA-7-40                           16320002
               WHEN PDA-7-41 = WS-USERID                                16330002
                   MOVE LS-USERID TO PDA-7-41                           16340002
               WHEN PDA-7-42 = WS-USERID                                16350002
                   MOVE LS-USERID TO PDA-7-42                           16360002
               WHEN PDA-7-43 = WS-USERID                                16370002
                   MOVE LS-USERID TO PDA-7-43                           16380002
               WHEN PDA-7-44 = WS-USERID                                16390002
                   MOVE LS-USERID TO PDA-7-44                           16400002
               WHEN PDA-7-45 = WS-USERID                                16410002
                   MOVE LS-USERID TO PDA-7-45                           16420002
               WHEN PDA-7-46 = WS-USERID                                16430002
                   MOVE LS-USERID TO PDA-7-46                           16440002
               WHEN PDA-7-47 = WS-USERID                                16450002
                   MOVE LS-USERID TO PDA-7-47                           16460002
               WHEN PDA-7-48 = WS-USERID                                16470002
                   MOVE LS-USERID TO PDA-7-48                           16480002
               WHEN PDA-7-49 = WS-USERID                                16490002
                   MOVE LS-USERID TO PDA-7-49                           16500002
               WHEN PDA-7-50 = WS-USERID                                16510002
                   MOVE LS-USERID TO PDA-7-50                           16520002
               WHEN PDA-7-51 = WS-USERID                                16530002
                   MOVE LS-USERID TO PDA-7-51                           16540002
               WHEN PDA-7-52 = WS-USERID                                16550002
                   MOVE LS-USERID TO PDA-7-52                           16560002
               WHEN PDA-7-53 = WS-USERID                                16570002
                   MOVE LS-USERID TO PDA-7-53                           16580002
               WHEN PDA-7-54 = WS-USERID                                16590002
                   MOVE LS-USERID TO PDA-7-54                           16600002
               WHEN PDA-7-55 = WS-USERID                                16610002
                   MOVE LS-USERID TO PDA-7-55                           16620002
               WHEN PDA-7-56 = WS-USERID                                16630002
                   MOVE LS-USERID TO PDA-7-56                           16640002
               WHEN PDA-7-57 = WS-USERID                                16650002
                   MOVE LS-USERID TO PDA-7-57                           16660002
               WHEN PDA-7-58 = WS-USERID                                16670002
                   MOVE LS-USERID TO PDA-7-58                           16680002
               WHEN PDA-7-59 = WS-USERID                                16690002
                   MOVE LS-USERID TO PDA-7-59                           16700002
               WHEN PDA-7-60 = WS-USERID                                16710002
                   MOVE LS-USERID TO PDA-7-60                           16720002
               WHEN PDA-7-61 = WS-USERID                                16730002
                   MOVE LS-USERID TO PDA-7-61                           16740002
               WHEN PDA-7-62 = WS-USERID                                16750002
                   MOVE LS-USERID TO PDA-7-62                           16760002
               WHEN PDA-7-63 = WS-USERID                                16770002
                   MOVE LS-USERID TO PDA-7-63                           16780002
               WHEN PDA-7-64 = WS-USERID                                16790002
                   MOVE LS-USERID TO PDA-7-64                           16800002
               WHEN PDA-7-65 = WS-USERID                                16810002
                   MOVE LS-USERID TO PDA-7-65                           16820002
               WHEN PDA-7-66 = WS-USERID                                16830002
                   MOVE LS-USERID TO PDA-7-66                           16840002
               WHEN PDA-7-67 = WS-USERID                                16850002
                   MOVE LS-USERID TO PDA-7-67                           16860002
               WHEN PDA-7-68 = WS-USERID                                16870002
                   MOVE LS-USERID TO PDA-7-68                           16880002
               WHEN PDA-7-69 = WS-USERID                                16890002
                   MOVE LS-USERID TO PDA-7-69                           16900002
               WHEN PDA-7-70 = WS-USERID                                16910002
                   MOVE LS-USERID TO PDA-7-70                           16920002
           END-EVALUATE.                                                16930002
                                                                        16940002
           EVALUATE TRUE                                                16950002
               WHEN PDA-8-01 = WS-CICSREGN                              16960002
                   MOVE LS-CICSREGN TO PDA-8-01                         16970002
               WHEN PDA-8-02 = WS-CICSREGN                              16980002
                   MOVE LS-CICSREGN TO PDA-8-02                         16990002
               WHEN PDA-8-03 = WS-CICSREGN                              17000002
                   MOVE LS-CICSREGN TO PDA-8-03                         17010002
               WHEN PDA-8-04 = WS-CICSREGN                              17020002
                   MOVE LS-CICSREGN TO PDA-8-04                         17030002
               WHEN PDA-8-05 = WS-CICSREGN                              17040002
                   MOVE LS-CICSREGN TO PDA-8-05                         17050002
               WHEN PDA-8-06 = WS-CICSREGN                              17060002
                   MOVE LS-CICSREGN TO PDA-8-06                         17070002
               WHEN PDA-8-07 = WS-CICSREGN                              17080002
                   MOVE LS-CICSREGN TO PDA-8-07                         17090002
               WHEN PDA-8-08 = WS-CICSREGN                              17100002
                   MOVE LS-CICSREGN TO PDA-8-08                         17110002
               WHEN PDA-8-09 = WS-CICSREGN                              17120002
                   MOVE LS-CICSREGN TO PDA-8-09                         17130002
               WHEN PDA-8-10 = WS-CICSREGN                              17140002
                   MOVE LS-CICSREGN TO PDA-8-10                         17150002
               WHEN PDA-8-11 = WS-CICSREGN                              17160002
                   MOVE LS-CICSREGN TO PDA-8-11                         17170002
               WHEN PDA-8-12 = WS-CICSREGN                              17180002
                   MOVE LS-CICSREGN TO PDA-8-12                         17190002
               WHEN PDA-8-13 = WS-CICSREGN                              17200002
                   MOVE LS-CICSREGN TO PDA-8-13                         17210002
               WHEN PDA-8-14 = WS-CICSREGN                              17220002
                   MOVE LS-CICSREGN TO PDA-8-14                         17230002
               WHEN PDA-8-15 = WS-CICSREGN                              17240002
                   MOVE LS-CICSREGN TO PDA-8-15                         17250002
               WHEN PDA-8-16 = WS-CICSREGN                              17260002
                   MOVE LS-CICSREGN TO PDA-8-16                         17270002
               WHEN PDA-8-17 = WS-CICSREGN                              17280002
                   MOVE LS-CICSREGN TO PDA-8-17                         17290002
               WHEN PDA-8-18 = WS-CICSREGN                              17300002
                   MOVE LS-CICSREGN TO PDA-8-18                         17310002
               WHEN PDA-8-19 = WS-CICSREGN                              17320002
                   MOVE LS-CICSREGN TO PDA-8-19                         17330002
               WHEN PDA-8-20 = WS-CICSREGN                              17340002
                   MOVE LS-CICSREGN TO PDA-8-20                         17350002
               WHEN PDA-8-21 = WS-CICSREGN                              17360002
                   MOVE LS-CICSREGN TO PDA-8-21                         17370002
               WHEN PDA-8-22 = WS-CICSREGN                              17380002
                   MOVE LS-CICSREGN TO PDA-8-22                         17390002
               WHEN PDA-8-23 = WS-CICSREGN                              17400002
                   MOVE LS-CICSREGN TO PDA-8-23                         17410002
               WHEN PDA-8-24 = WS-CICSREGN                              17420002
                   MOVE LS-CICSREGN TO PDA-8-24                         17430002
               WHEN PDA-8-25 = WS-CICSREGN                              17440002
                   MOVE LS-CICSREGN TO PDA-8-25                         17450002
               WHEN PDA-8-26 = WS-CICSREGN                              17460002
                   MOVE LS-CICSREGN TO PDA-8-26                         17470002
               WHEN PDA-8-27 = WS-CICSREGN                              17480002
                   MOVE LS-CICSREGN TO PDA-8-27                         17490002
               WHEN PDA-8-28 = WS-CICSREGN                              17500002
                   MOVE LS-CICSREGN TO PDA-8-28                         17510002
               WHEN PDA-8-29 = WS-CICSREGN                              17520002
                   MOVE LS-CICSREGN TO PDA-8-29                         17530002
               WHEN PDA-8-30 = WS-CICSREGN                              17540002
                   MOVE LS-CICSREGN TO PDA-8-30                         17550002
               WHEN PDA-8-31 = WS-CICSREGN                              17560002
                   MOVE LS-CICSREGN TO PDA-8-31                         17570002
               WHEN PDA-8-32 = WS-CICSREGN                              17580002
                   MOVE LS-CICSREGN TO PDA-8-32                         17590002
               WHEN PDA-8-33 = WS-CICSREGN                              17600002
                   MOVE LS-CICSREGN TO PDA-8-33                         17610002
               WHEN PDA-8-34 = WS-CICSREGN                              17620002
                   MOVE LS-CICSREGN TO PDA-8-34                         17630002
               WHEN PDA-8-35 = WS-CICSREGN                              17640002
                   MOVE LS-CICSREGN TO PDA-8-35                         17650002
               WHEN PDA-8-36 = WS-CICSREGN                              17660002
                   MOVE LS-CICSREGN TO PDA-8-36                         17670002
               WHEN PDA-8-37 = WS-CICSREGN                              17680002
                   MOVE LS-CICSREGN TO PDA-8-37                         17690002
               WHEN PDA-8-38 = WS-CICSREGN                              17700002
                   MOVE LS-CICSREGN TO PDA-8-38                         17710002
               WHEN PDA-8-39 = WS-CICSREGN                              17720002
                   MOVE LS-CICSREGN TO PDA-8-39                         17730002
               WHEN PDA-8-40 = WS-CICSREGN                              17740002
                   MOVE LS-CICSREGN TO PDA-8-40                         17750002
               WHEN PDA-8-41 = WS-CICSREGN                              17760002
                   MOVE LS-CICSREGN TO PDA-8-41                         17770002
               WHEN PDA-8-42 = WS-CICSREGN                              17780002
                   MOVE LS-CICSREGN TO PDA-8-42                         17790002
               WHEN PDA-8-43 = WS-CICSREGN                              17800002
                   MOVE LS-CICSREGN TO PDA-8-43                         17810002
               WHEN PDA-8-44 = WS-CICSREGN                              17820002
                   MOVE LS-CICSREGN TO PDA-8-44                         17830002
               WHEN PDA-8-45 = WS-CICSREGN                              17840002
                   MOVE LS-CICSREGN TO PDA-8-45                         17850002
               WHEN PDA-8-46 = WS-CICSREGN                              17860002
                   MOVE LS-CICSREGN TO PDA-8-46                         17870002
               WHEN PDA-8-47 = WS-CICSREGN                              17880002
                   MOVE LS-CICSREGN TO PDA-8-47                         17890002
               WHEN PDA-8-48 = WS-CICSREGN                              17900002
                   MOVE LS-CICSREGN TO PDA-8-48                         17910002
               WHEN PDA-8-49 = WS-CICSREGN                              17920002
                   MOVE LS-CICSREGN TO PDA-8-49                         17930002
               WHEN PDA-8-50 = WS-CICSREGN                              17940002
                   MOVE LS-CICSREGN TO PDA-8-50                         17950002
               WHEN PDA-8-51 = WS-CICSREGN                              17960002
                   MOVE LS-CICSREGN TO PDA-8-51                         17970002
               WHEN PDA-8-52 = WS-CICSREGN                              17980002
                   MOVE LS-CICSREGN TO PDA-8-52                         17990002
               WHEN PDA-8-53 = WS-CICSREGN                              18000002
                   MOVE LS-CICSREGN TO PDA-8-53                         18010002
               WHEN PDA-8-54 = WS-CICSREGN                              18020002
                   MOVE LS-CICSREGN TO PDA-8-54                         18030002
               WHEN PDA-8-55 = WS-CICSREGN                              18040002
                   MOVE LS-CICSREGN TO PDA-8-55                         18050002
               WHEN PDA-8-56 = WS-CICSREGN                              18060002
                   MOVE LS-CICSREGN TO PDA-8-56                         18070002
               WHEN PDA-8-57 = WS-CICSREGN                              18080002
                   MOVE LS-CICSREGN TO PDA-8-57                         18090002
               WHEN PDA-8-58 = WS-CICSREGN                              18100002
                   MOVE LS-CICSREGN TO PDA-8-58                         18110002
               WHEN PDA-8-59 = WS-CICSREGN                              18120002
                   MOVE LS-CICSREGN TO PDA-8-59                         18130002
               WHEN PDA-8-60 = WS-CICSREGN                              18140002
                   MOVE LS-CICSREGN TO PDA-8-60                         18150002
               WHEN PDA-8-61 = WS-CICSREGN                              18160002
                   MOVE LS-CICSREGN TO PDA-8-61                         18170002
               WHEN PDA-8-62 = WS-CICSREGN                              18180002
                   MOVE LS-CICSREGN TO PDA-8-62                         18190002
               WHEN PDA-8-63 = WS-CICSREGN                              18200002
                   MOVE LS-CICSREGN TO PDA-8-63                         18210002
               WHEN PDA-8-64 = WS-CICSREGN                              18220002
                   MOVE LS-CICSREGN TO PDA-8-64                         18230002
               WHEN PDA-8-65 = WS-CICSREGN                              18240002
                   MOVE LS-CICSREGN TO PDA-8-65                         18250002
               WHEN PDA-8-66 = WS-CICSREGN                              18260002
                   MOVE LS-CICSREGN TO PDA-8-66                         18270002
               WHEN PDA-8-67 = WS-CICSREGN                              18280002
                   MOVE LS-CICSREGN TO PDA-8-67                         18290002
               WHEN PDA-8-68 = WS-CICSREGN                              18300002
                   MOVE LS-CICSREGN TO PDA-8-68                         18310002
               WHEN PDA-8-69 = WS-CICSREGN                              18320002
                   MOVE LS-CICSREGN TO PDA-8-69                         18330002
               WHEN PDA-8-70 = WS-CICSREGN                              18340002
                   MOVE LS-CICSREGN TO PDA-8-70                         18350002
           END-EVALUATE.                                                18360002
                                                                        18370002
           EVALUATE TRUE                                                18380002
               WHEN PDA-6-01 = WS-STORCLAS                              18390002
                   MOVE LS-STORCLAS TO PDA-6-01                         18400002
               WHEN PDA-6-02 = WS-STORCLAS                              18410002
                   MOVE LS-STORCLAS TO PDA-6-02                         18420002
               WHEN PDA-6-03 = WS-STORCLAS                              18430002
                   MOVE LS-STORCLAS TO PDA-6-03                         18440002
               WHEN PDA-6-04 = WS-STORCLAS                              18450002
                   MOVE LS-STORCLAS TO PDA-6-04                         18460002
               WHEN PDA-6-05 = WS-STORCLAS                              18470002
                   MOVE LS-STORCLAS TO PDA-6-05                         18480002
               WHEN PDA-6-06 = WS-STORCLAS                              18490002
                   MOVE LS-STORCLAS TO PDA-6-06                         18500002
               WHEN PDA-6-07 = WS-STORCLAS                              18510002
                   MOVE LS-STORCLAS TO PDA-6-07                         18520002
               WHEN PDA-6-08 = WS-STORCLAS                              18530002
                   MOVE LS-STORCLAS TO PDA-6-08                         18540002
               WHEN PDA-6-09 = WS-STORCLAS                              18550002
                   MOVE LS-STORCLAS TO PDA-6-09                         18560002
               WHEN PDA-6-10 = WS-STORCLAS                              18570002
                   MOVE LS-STORCLAS TO PDA-6-10                         18580002
               WHEN PDA-6-11 = WS-STORCLAS                              18590002
                   MOVE LS-STORCLAS TO PDA-6-11                         18600002
               WHEN PDA-6-12 = WS-STORCLAS                              18610002
                   MOVE LS-STORCLAS TO PDA-6-12                         18620002
               WHEN PDA-6-13 = WS-STORCLAS                              18630002
                   MOVE LS-STORCLAS TO PDA-6-13                         18640002
               WHEN PDA-6-14 = WS-STORCLAS                              18650002
                   MOVE LS-STORCLAS TO PDA-6-14                         18660002
               WHEN PDA-6-15 = WS-STORCLAS                              18670002
                   MOVE LS-STORCLAS TO PDA-6-15                         18680002
               WHEN PDA-6-16 = WS-STORCLAS                              18690002
                   MOVE LS-STORCLAS TO PDA-6-16                         18700002
               WHEN PDA-6-17 = WS-STORCLAS                              18710002
                   MOVE LS-STORCLAS TO PDA-6-17                         18720002
               WHEN PDA-6-18 = WS-STORCLAS                              18730002
                   MOVE LS-STORCLAS TO PDA-6-18                         18740002
               WHEN PDA-6-19 = WS-STORCLAS                              18750002
                   MOVE LS-STORCLAS TO PDA-6-19                         18760002
               WHEN PDA-6-20 = WS-STORCLAS                              18770002
                   MOVE LS-STORCLAS TO PDA-6-20                         18780002
               WHEN PDA-6-21 = WS-STORCLAS                              18790002
                   MOVE LS-STORCLAS TO PDA-6-21                         18800002
               WHEN PDA-6-22 = WS-STORCLAS                              18810002
                   MOVE LS-STORCLAS TO PDA-6-22                         18820002
               WHEN PDA-6-23 = WS-STORCLAS                              18830002
                   MOVE LS-STORCLAS TO PDA-6-23                         18840002
               WHEN PDA-6-24 = WS-STORCLAS                              18850002
                   MOVE LS-STORCLAS TO PDA-6-24                         18860002
               WHEN PDA-6-25 = WS-STORCLAS                              18870002
                   MOVE LS-STORCLAS TO PDA-6-25                         18880002
               WHEN PDA-6-26 = WS-STORCLAS                              18890002
                   MOVE LS-STORCLAS TO PDA-6-26                         18900002
               WHEN PDA-6-27 = WS-STORCLAS                              18910002
                   MOVE LS-STORCLAS TO PDA-6-27                         18920002
               WHEN PDA-6-28 = WS-STORCLAS                              18930002
                   MOVE LS-STORCLAS TO PDA-6-28                         18940002
               WHEN PDA-6-29 = WS-STORCLAS                              18950002
                   MOVE LS-STORCLAS TO PDA-6-29                         18960002
               WHEN PDA-6-30 = WS-STORCLAS                              18970002
                   MOVE LS-STORCLAS TO PDA-6-30                         18980002
               WHEN PDA-6-31 = WS-STORCLAS                              18990002
                   MOVE LS-STORCLAS TO PDA-6-31                         19000002
               WHEN PDA-6-32 = WS-STORCLAS                              19010002
                   MOVE LS-STORCLAS TO PDA-6-32                         19020002
               WHEN PDA-6-33 = WS-STORCLAS                              19030002
                   MOVE LS-STORCLAS TO PDA-6-33                         19040002
               WHEN PDA-6-34 = WS-STORCLAS                              19050002
                   MOVE LS-STORCLAS TO PDA-6-34                         19060002
               WHEN PDA-6-35 = WS-STORCLAS                              19070002
                   MOVE LS-STORCLAS TO PDA-6-35                         19080002
               WHEN PDA-6-36 = WS-STORCLAS                              19090002
                   MOVE LS-STORCLAS TO PDA-6-36                         19100002
               WHEN PDA-6-37 = WS-STORCLAS                              19110002
                   MOVE LS-STORCLAS TO PDA-6-37                         19120002
               WHEN PDA-6-38 = WS-STORCLAS                              19130002
                   MOVE LS-STORCLAS TO PDA-6-38                         19140002
               WHEN PDA-6-39 = WS-STORCLAS                              19150002
                   MOVE LS-STORCLAS TO PDA-6-39                         19160002
               WHEN PDA-6-40 = WS-STORCLAS                              19170002
                   MOVE LS-STORCLAS TO PDA-6-40                         19180002
               WHEN PDA-6-41 = WS-STORCLAS                              19190002
                   MOVE LS-STORCLAS TO PDA-6-41                         19200002
               WHEN PDA-6-42 = WS-STORCLAS                              19210002
                   MOVE LS-STORCLAS TO PDA-6-42                         19220002
               WHEN PDA-6-43 = WS-STORCLAS                              19230002
                   MOVE LS-STORCLAS TO PDA-6-43                         19240002
               WHEN PDA-6-44 = WS-STORCLAS                              19250002
                   MOVE LS-STORCLAS TO PDA-6-44                         19260002
               WHEN PDA-6-45 = WS-STORCLAS                              19270002
                   MOVE LS-STORCLAS TO PDA-6-45                         19280002
               WHEN PDA-6-46 = WS-STORCLAS                              19290002
                   MOVE LS-STORCLAS TO PDA-6-46                         19300002
               WHEN PDA-6-47 = WS-STORCLAS                              19310002
                   MOVE LS-STORCLAS TO PDA-6-47                         19320002
               WHEN PDA-6-48 = WS-STORCLAS                              19330002
                   MOVE LS-STORCLAS TO PDA-6-48                         19340002
               WHEN PDA-6-49 = WS-STORCLAS                              19350002
                   MOVE LS-STORCLAS TO PDA-6-49                         19360002
               WHEN PDA-6-50 = WS-STORCLAS                              19370002
                   MOVE LS-STORCLAS TO PDA-6-50                         19380002
               WHEN PDA-6-51 = WS-STORCLAS                              19390002
                   MOVE LS-STORCLAS TO PDA-6-51                         19400002
               WHEN PDA-6-52 = WS-STORCLAS                              19410002
                   MOVE LS-STORCLAS TO PDA-6-52                         19420002
               WHEN PDA-6-53 = WS-STORCLAS                              19430002
                   MOVE LS-STORCLAS TO PDA-6-53                         19440002
               WHEN PDA-6-54 = WS-STORCLAS                              19450002
                   MOVE LS-STORCLAS TO PDA-6-54                         19460002
               WHEN PDA-6-55 = WS-STORCLAS                              19470002
                   MOVE LS-STORCLAS TO PDA-6-55                         19480002
               WHEN PDA-6-56 = WS-STORCLAS                              19490002
                   MOVE LS-STORCLAS TO PDA-6-56                         19500002
               WHEN PDA-6-57 = WS-STORCLAS                              19510002
                   MOVE LS-STORCLAS TO PDA-6-57                         19520002
               WHEN PDA-6-58 = WS-STORCLAS                              19530002
                   MOVE LS-STORCLAS TO PDA-6-58                         19540002
               WHEN PDA-6-59 = WS-STORCLAS                              19550002
                   MOVE LS-STORCLAS TO PDA-6-59                         19560002
               WHEN PDA-6-60 = WS-STORCLAS                              19570002
                   MOVE LS-STORCLAS TO PDA-6-60                         19580002
               WHEN PDA-6-61 = WS-STORCLAS                              19590002
                   MOVE LS-STORCLAS TO PDA-6-61                         19600002
               WHEN PDA-6-62 = WS-STORCLAS                              19610002
                   MOVE LS-STORCLAS TO PDA-6-62                         19620002
               WHEN PDA-6-63 = WS-STORCLAS                              19630002
                   MOVE LS-STORCLAS TO PDA-6-63                         19640002
               WHEN PDA-6-64 = WS-STORCLAS                              19650002
                   MOVE LS-STORCLAS TO PDA-6-64                         19660002
               WHEN PDA-6-65 = WS-STORCLAS                              19670002
                   MOVE LS-STORCLAS TO PDA-6-65                         19680002
               WHEN PDA-6-66 = WS-STORCLAS                              19690002
                   MOVE LS-STORCLAS TO PDA-6-66                         19700002
               WHEN PDA-6-67 = WS-STORCLAS                              19710002
                   MOVE LS-STORCLAS TO PDA-6-67                         19720002
               WHEN PDA-6-68 = WS-STORCLAS                              19730002
                   MOVE LS-STORCLAS TO PDA-6-68                         19740002
               WHEN PDA-6-69 = WS-STORCLAS                              19750002
                   MOVE LS-STORCLAS TO PDA-6-69                         19760002
               WHEN PDA-6-70 = WS-STORCLAS                              19770002
                   MOVE LS-STORCLAS TO PDA-6-70                         19780002
           END-EVALUATE.                                                19790002
                                                                        19800002
           EVALUATE TRUE                                                19810002
               WHEN PDA-8-01 = WS-DATABASE                              19820002
                   MOVE LS-DATABASE TO PDA-8-01                         19830002
               WHEN PDA-8-02 = WS-DATABASE                              19840002
                   MOVE LS-DATABASE TO PDA-8-02                         19850002
               WHEN PDA-8-03 = WS-DATABASE                              19860002
                   MOVE LS-DATABASE TO PDA-8-03                         19870002
               WHEN PDA-8-04 = WS-DATABASE                              19880002
                   MOVE LS-DATABASE TO PDA-8-04                         19890002
               WHEN PDA-8-05 = WS-DATABASE                              19900002
                   MOVE LS-DATABASE TO PDA-8-05                         19910002
               WHEN PDA-8-06 = WS-DATABASE                              19920002
                   MOVE LS-DATABASE TO PDA-8-06                         19930002
               WHEN PDA-8-07 = WS-DATABASE                              19940002
                   MOVE LS-DATABASE TO PDA-8-07                         19950002
               WHEN PDA-8-08 = WS-DATABASE                              19960002
                   MOVE LS-DATABASE TO PDA-8-08                         19970002
               WHEN PDA-8-09 = WS-DATABASE                              19980002
                   MOVE LS-DATABASE TO PDA-8-09                         19990002
               WHEN PDA-8-10 = WS-DATABASE                              20000002
                   MOVE LS-DATABASE TO PDA-8-10                         20010002
               WHEN PDA-8-11 = WS-DATABASE                              20020002
                   MOVE LS-DATABASE TO PDA-8-11                         20030002
               WHEN PDA-8-12 = WS-DATABASE                              20040002
                   MOVE LS-DATABASE TO PDA-8-12                         20050002
               WHEN PDA-8-13 = WS-DATABASE                              20060002
                   MOVE LS-DATABASE TO PDA-8-13                         20070002
               WHEN PDA-8-14 = WS-DATABASE                              20080002
                   MOVE LS-DATABASE TO PDA-8-14                         20090002
               WHEN PDA-8-15 = WS-DATABASE                              20100002
                   MOVE LS-DATABASE TO PDA-8-15                         20110002
               WHEN PDA-8-16 = WS-DATABASE                              20120002
                   MOVE LS-DATABASE TO PDA-8-16                         20130002
               WHEN PDA-8-17 = WS-DATABASE                              20140002
                   MOVE LS-DATABASE TO PDA-8-17                         20150002
               WHEN PDA-8-18 = WS-DATABASE                              20160002
                   MOVE LS-DATABASE TO PDA-8-18                         20170002
               WHEN PDA-8-19 = WS-DATABASE                              20180002
                   MOVE LS-DATABASE TO PDA-8-19                         20190002
               WHEN PDA-8-20 = WS-DATABASE                              20200002
                   MOVE LS-DATABASE TO PDA-8-20                         20210002
               WHEN PDA-8-21 = WS-DATABASE                              20220002
                   MOVE LS-DATABASE TO PDA-8-21                         20230002
               WHEN PDA-8-22 = WS-DATABASE                              20240002
                   MOVE LS-DATABASE TO PDA-8-22                         20250002
               WHEN PDA-8-23 = WS-DATABASE                              20260002
                   MOVE LS-DATABASE TO PDA-8-23                         20270002
               WHEN PDA-8-24 = WS-DATABASE                              20280002
                   MOVE LS-DATABASE TO PDA-8-24                         20290002
               WHEN PDA-8-25 = WS-DATABASE                              20300002
                   MOVE LS-DATABASE TO PDA-8-25                         20310002
               WHEN PDA-8-26 = WS-DATABASE                              20320002
                   MOVE LS-DATABASE TO PDA-8-26                         20330002
               WHEN PDA-8-27 = WS-DATABASE                              20340002
                   MOVE LS-DATABASE TO PDA-8-27                         20350002
               WHEN PDA-8-28 = WS-DATABASE                              20360002
                   MOVE LS-DATABASE TO PDA-8-28                         20370002
               WHEN PDA-8-29 = WS-DATABASE                              20380002
                   MOVE LS-DATABASE TO PDA-8-29                         20390002
               WHEN PDA-8-30 = WS-DATABASE                              20400002
                   MOVE LS-DATABASE TO PDA-8-30                         20410002
               WHEN PDA-8-31 = WS-DATABASE                              20420002
                   MOVE LS-DATABASE TO PDA-8-31                         20430002
               WHEN PDA-8-32 = WS-DATABASE                              20440002
                   MOVE LS-DATABASE TO PDA-8-32                         20450002
               WHEN PDA-8-33 = WS-DATABASE                              20460002
                   MOVE LS-DATABASE TO PDA-8-33                         20470002
               WHEN PDA-8-34 = WS-DATABASE                              20480002
                   MOVE LS-DATABASE TO PDA-8-34                         20490002
               WHEN PDA-8-35 = WS-DATABASE                              20500002
                   MOVE LS-DATABASE TO PDA-8-35                         20510002
               WHEN PDA-8-36 = WS-DATABASE                              20520002
                   MOVE LS-DATABASE TO PDA-8-36                         20530002
               WHEN PDA-8-37 = WS-DATABASE                              20540002
                   MOVE LS-DATABASE TO PDA-8-37                         20550002
               WHEN PDA-8-38 = WS-DATABASE                              20560002
                   MOVE LS-DATABASE TO PDA-8-38                         20570002
               WHEN PDA-8-39 = WS-DATABASE                              20580002
                   MOVE LS-DATABASE TO PDA-8-39                         20590002
               WHEN PDA-8-40 = WS-DATABASE                              20600002
                   MOVE LS-DATABASE TO PDA-8-40                         20610002
               WHEN PDA-8-41 = WS-DATABASE                              20620002
                   MOVE LS-DATABASE TO PDA-8-41                         20630002
               WHEN PDA-8-42 = WS-DATABASE                              20640002
                   MOVE LS-DATABASE TO PDA-8-42                         20650002
               WHEN PDA-8-43 = WS-DATABASE                              20660002
                   MOVE LS-DATABASE TO PDA-8-43                         20670002
               WHEN PDA-8-44 = WS-DATABASE                              20680002
                   MOVE LS-DATABASE TO PDA-8-44                         20690002
               WHEN PDA-8-45 = WS-DATABASE                              20700002
                   MOVE LS-DATABASE TO PDA-8-45                         20710002
               WHEN PDA-8-46 = WS-DATABASE                              20720002
                   MOVE LS-DATABASE TO PDA-8-46                         20730002
               WHEN PDA-8-47 = WS-DATABASE                              20740002
                   MOVE LS-DATABASE TO PDA-8-47                         20750002
               WHEN PDA-8-48 = WS-DATABASE                              20760002
                   MOVE LS-DATABASE TO PDA-8-48                         20770002
               WHEN PDA-8-49 = WS-DATABASE                              20780002
                   MOVE LS-DATABASE TO PDA-8-49                         20790002
               WHEN PDA-8-50 = WS-DATABASE                              20800002
                   MOVE LS-DATABASE TO PDA-8-50                         20810002
               WHEN PDA-8-51 = WS-DATABASE                              20820002
                   MOVE LS-DATABASE TO PDA-8-51                         20830002
               WHEN PDA-8-52 = WS-DATABASE                              20840002
                   MOVE LS-DATABASE TO PDA-8-52                         20850002
               WHEN PDA-8-53 = WS-DATABASE                              20860002
                   MOVE LS-DATABASE TO PDA-8-53                         20870002
               WHEN PDA-8-54 = WS-DATABASE                              20880002
                   MOVE LS-DATABASE TO PDA-8-54                         20890002
               WHEN PDA-8-55 = WS-DATABASE                              20900002
                   MOVE LS-DATABASE TO PDA-8-55                         20910002
               WHEN PDA-8-56 = WS-DATABASE                              20920002
                   MOVE LS-DATABASE TO PDA-8-56                         20930002
               WHEN PDA-8-57 = WS-DATABASE                              20940002
                   MOVE LS-DATABASE TO PDA-8-57                         20950002
               WHEN PDA-8-58 = WS-DATABASE                              20960002
                   MOVE LS-DATABASE TO PDA-8-58                         20970002
               WHEN PDA-8-59 = WS-DATABASE                              20980002
                   MOVE LS-DATABASE TO PDA-8-59                         20990002
               WHEN PDA-8-60 = WS-DATABASE                              21000002
                   MOVE LS-DATABASE TO PDA-8-60                         21010002
               WHEN PDA-8-61 = WS-DATABASE                              21020002
                   MOVE LS-DATABASE TO PDA-8-61                         21030002
               WHEN PDA-8-62 = WS-DATABASE                              21040002
                   MOVE LS-DATABASE TO PDA-8-62                         21050002
               WHEN PDA-8-63 = WS-DATABASE                              21060002
                   MOVE LS-DATABASE TO PDA-8-63                         21070002
               WHEN PDA-8-64 = WS-DATABASE                              21080002
                   MOVE LS-DATABASE TO PDA-8-64                         21090002
               WHEN PDA-8-65 = WS-DATABASE                              21100002
                   MOVE LS-DATABASE TO PDA-8-65                         21110002
               WHEN PDA-8-66 = WS-DATABASE                              21120002
                   MOVE LS-DATABASE TO PDA-8-66                         21130002
               WHEN PDA-8-67 = WS-DATABASE                              21140002
                   MOVE LS-DATABASE TO PDA-8-67                         21150002
               WHEN PDA-8-68 = WS-DATABASE                              21160002
                   MOVE LS-DATABASE TO PDA-8-68                         21170002
               WHEN PDA-8-69 = WS-DATABASE                              21180002
                   MOVE LS-DATABASE TO PDA-8-69                         21190002
               WHEN PDA-8-70 = WS-DATABASE                              21200002
                   MOVE LS-DATABASE TO PDA-8-70                         21210002
           END-EVALUATE.                                                21220002
                                                                        21230002
       P90000-EXIT.                                                     21240002
           EXIT.                                                        21250002
           EJECT                                                        21260002