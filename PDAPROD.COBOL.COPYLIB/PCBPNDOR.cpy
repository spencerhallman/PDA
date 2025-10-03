      ****************************************************************  00000100
      *****  PCB FOR THE PENDING ORDER DATABASE                         00001000
      ****************************************************************  00002000
       01  PENDORD-PCB.                                                 00010000
           05  PENDORD-DBDNAME         PIC X(08).                       00020000
           05  PENDORD-SEG-LEVEL       PIC X(02).                       00030000
           05  PENDORD-STATUS          PIC X(02).                       00040000
           05  PENDORD-PROCOPT         PIC X(04).                       00130000
           05  FILLER                  PIC X(04).                       00131000
           05  PENDORD-SEG-NAME-FB     PIC X(08).                       00140000
           05  PENDORD-KEY-FB-LTH      PIC S9(5) COMP.                  00150000
           05  FILLER                  PIC X(4).                        00151000
           05  PENDORD-KEY-FB-AREA     PIC X(10).                       00160000
           05  PENDORD-KEY-FB-AREA-R   REDEFINES PENDORD-KEY-FB-AREA.   00161000
               10 PENDORD-KEY-PREFIX   PIC 9(05).                       00180000
               10 PENDORD-KEY-SEQUENCE PIC 9(05).                       00190000