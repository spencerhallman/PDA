000001****************************************************************  00000100
000010*****  COMMON PCB MASK AREA                                       00001000
000020****************************************************************  00002000
000100 01  COMMON-PCB-MASK.                                             00010000
000200     05 CPM-DBDNAME                   PIC X(08) VALUE SPACES.     00020000
000300     05 CPM-SEG-LEVEL                 PIC X(02) VALUE SPACES.     00030000
000400     05 CPM-STATUS                    PIC X(02) VALUE SPACES.     00040000
000500        88 GOOD-RETURN                          VALUE '  '.       00050000
000600        88 END-OF-DATABASE                      VALUE 'GB'.       00060000
000700        88 SEGMENT-NOT-FOUND                    VALUE 'GE'.       00070000
000800        88 END-OF-INPUT-MSG                     VALUE 'QC'.       00080000
000900        88 END-OF-INPUT-SEGMENT                 VALUE 'QD'.       00090000
001000        88 SEGMENT-ALREADY-EXISTS               VALUE 'II'.       00100000
001100        88 CALL-IOPCB-FROM-BATCH                VALUE 'AL'.       00110000
001200        88 SECURITY-VIOLATION                   VALUE 'A4'.       00120000
001300     05 FILLER                        PIC X(08) VALUE SPACES.     00130000
001400     05 CPM-SEG-NAME                  PIC X(08) VALUE SPACES.     00140000
001500     05 FILLER                        PIC X(08) VALUE SPACES.     00150000
001600     05 CPM-FEEDBACK-AREA             PIC X(80) VALUE SPACES.     00160000