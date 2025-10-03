      ****************************************************************  00000100
      *****  PCB FOR THE ORDER DATABASE                                 00001000
      ****************************************************************  00002000
       01  ORDER-PCB.                                                   00010000
           05  OP-DBDNAME              PIC X(8)  VALUE SPACES.          00020000
           05  OP-SEG-LEVEL            PIC XX    VALUE SPACES.          00030000
           05  OP-STATUS               PIC XX    VALUE SPACES.          00040000
           05  FILLER                  PIC X(8)  VALUE SPACES.          00130000
           05  OP-SEG-NAME             PIC X(8)  VALUE SPACES.          00140000
           05  FILLER                  PIC X(8)  VALUE SPACES.          00150000
           05  OP-FEEDBACK-AREA        PIC X(80) VALUE SPACES.          00160000