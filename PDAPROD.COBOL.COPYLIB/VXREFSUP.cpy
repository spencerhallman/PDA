      ******************************************************************
      * XREF SUPPLIER RECORD -- VSAM KSDS                              *
      ******************************************************************
       01  SUPPLIER-RECORD.
           03  SR-KEY.
               05  SR-ID                       PIC X(15).
               05  SR-REC-TYPE                 PIC XX.
                 88  SUPPLIER-NAME                        VALUE '01'.
                 88  SUPPLIER-ADDRESS                     VALUE '02'.
                 88  SUPPLIER-CITY                        VALUE '03'.
                 88  SUPPLIER-ITEM                        VALUE '04'.
               05  FILLER                      PIC X(5).
           03  SR-DATA                         PIC X(58).


       01  SUPPLIER-NAME-RECORD      REDEFINES SUPPLIER-RECORD.
           03  SNR-KEY.
               05  SNR-ID                      PIC X(15).
               05  SNR-REC-TYPE                PIC XX.
               05  FILLER                      PIC X(5).
           03  SNR-NAME                        PIC X(30).
           03  FILLER                          PIC X(28).


       01  SUPPLIER-ADDRESS-RECORD   REDEFINES SUPPLIER-RECORD.
           03  SAR-KEY.
               05  SAR-ID                      PIC X(15).
               05  SAR-REC-TYPE                PIC XX.
               05  SAR-ADDRESS-NBR             PIC XX.
               05  FILLER                      PIC X(3).
           03  SAR-ADDRESS                     PIC X(30).
           03  FILLER                          PIC X(28).


       01  SUPPLIER-CITY-RECORD      REDEFINES SUPPLIER-RECORD.
           03  SCR-KEY.
               05  SCR-ID                      PIC X(15).
               05  SCR-REC-TYPE                PIC XX.
               05  FILLER                      PIC X(5).
           03  SCR-CITY                        PIC X(20).
           03  SCR-STATE                       PIC XX.
           03  SCR-POSTAL-CODE                 PIC X(10).
           03  FILLER                          PIC X(26).


       01  SUPPLIER-ITEM-RECORD      REDEFINES SUPPLIER-RECORD.
           03  SIR-KEY.
               05  SIR-ID                      PIC X(15).
               05  SIR-REC-TYPE                PIC XX.
               05  SIR-ITEM-NBR                PIC X(5).
           03  SIR-CATEGORY-NAME               PIC X(8).
           03  SIR-SUB-CATEGORY-NAME           PIC X(8).
           03  SIR-NAME                        PIC X(33).
           03  SIR-PRICE                       PIC S9(3)V99.
           03  SIR-LENGTH                      PIC S9(3)V99.
           03  SIR-DIAMETER                    PIC S9(3)V99.