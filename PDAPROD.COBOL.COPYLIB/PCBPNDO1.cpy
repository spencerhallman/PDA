      ****************************************************************  00000100
      *****  PCB1 FOR THE PENDING ORDER DATABASE                        00000200
      ****************************************************************  00000300
       01  PENDORD1-PCB.                                                00000400
           05  PENDORD1-DBDNAME        PIC X(08).                       00000500
           05  PENDORD1-SEG-LEVEL      PIC X(02).                       00000600
           05  PENDORD1-STATUS         PIC X(02).                       00000700
           05  PENDORD1-PROCOPT        PIC X(04).                       00000800
           05  FILLER                  PIC X(04).                       00000900
           05  PENDORD1-SEG-NAME-FB    PIC X(08).                       00001000
           05  PENDORD1-KEY-FB-LTH     PIC S9(5) COMP.                  00002000
           05  FILLER                  PIC X(4).                        00003000
           05  PENDORD1-KEY-FB-AREA    PIC X(10).                       00004000
           05  PENDORD1-KEY-FB-AREA-R  REDEFINES PENDORD1-KEY-FB-AREA.  00005000
               10 PENDORD1-KEY-PREFIX  PIC 9(05).                       00006000
               10 PENDORD1-KEY-SEQUENCE                                 00007000
                                       PIC 9(05).                       00008000