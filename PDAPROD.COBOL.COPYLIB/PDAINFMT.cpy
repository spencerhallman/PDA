       01  PDA-INPUT-FORMAT.                                            01530000
           03  PDA-FLAG                PIC X     VALUE SPACES.          01540000
               88  PDA-SPACER-REC                VALUE '*'.             01550000
               88  PDA-DATA-REC                  VALUE ' '.             01560000
           03  PDA-FIELD-NAME          PIC X(31) VALUE SPACES.          01570000
           03  FILLER                  PIC X     VALUE '('.             01580001
           03  PDA-FIELD-LENGTH        PIC X(3)  VALUE SPACES.          01581001
           03  FILLER                  PIC X(4)  VALUE ') -'.           01582001
           03  PDA-FIELD-DATA          PIC X(200) VALUE SPACES.         01590000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        01600000
               05  PDA-DATA-128        PIC X(128).                      01610000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        01620000
               05  PDA-DATA-064        PIC X(64).                       01630000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        01640000
               05  PDA-DATA-050        PIC X(50).                       01650000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        01660000
               05  PDA-DATA-032        PIC X(32).                       01670000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        01680000
               05  PDA-DATA-012        PIC X(12).                       01690000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        01700000
               05  PDA-DATA-010        PIC 9(8)V99.                     01710000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        01720000
               05  PDA-DATA-009        PIC 9(9).                        01730000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        01740000
               05  PDA-DATA-005        PIC X(5).                        01750000
           03  FILLER                  REDEFINES PDA-FIELD-DATA.        01760000
               05  PDA-DATA-003        PIC X(3).                        01770000
           EJECT                                                        01780000