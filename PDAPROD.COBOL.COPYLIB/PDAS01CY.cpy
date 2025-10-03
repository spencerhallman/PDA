                                                                        00010000
      ***************************************************************** 00020000
      *    PDAS01 PARAMETER PASS AREA                                 * 00030000
      ***************************************************************** 00040000
                                                                        00050000
       01  PDAS01-PARMS.                                                00060000
           03  PDAS01-AGE-DAYS         PIC 9(5)    VALUE ZEROES.        00070000
           03  PDAS01-ORDER-NUMBER     PIC X(10)   VALUE SPACES.        00080000
           03  PDAS01-ORDER-DATE.                                       00090000
               05  PDAS01-OD-YEAR      PIC 9(4)    VALUE ZEROES.        00100000
               05  FILLER              REDEFINES PDAS01-OD-YEAR.        00110000
                   07  PDAS01-OD-CE    PIC 99.                          00120000
                   07  PDAS01-OD-YR    PIC 99.                          00130000
               05  PDAS01-OD-MONTH     PIC 99      VALUE ZEROES.        00140000
               05  PDAS01-OD-DAY       PIC 99      VALUE ZEROES.        00150000
           03  PDAS01-ORDER-COUNT      PIC 9(5)    VALUE ZEROES.        00160000
           03  PDAS01-ORDER-DOLLAR-AMT-GRP.                             00170000
               05  PDAS01-ORDER-DOLLAR-AMT                              00180000
                                       PIC 9(9)V99 VALUE ZEROES COMP-3. 00190000
           03  PDAS01-AVERAGE-DOLLAR-AMT                                00210000
                                      PIC 9(13)V99 VALUE ZEROES COMP-3. 00220000