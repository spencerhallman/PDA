      ******************************************************************
      * PRODUCT DEMONSTRATION APPLICATION (PDA)                        *
      *                                                                *
      * STANDARD ITEM CATEGORY / SUB-CATEGORY COMBINATIONS FOR THE     *
      * ENTIRE APPLICATION                                             *
      *                                                                *
      ******************************************************************

       77  PDA-CATEGORY-MAX            PIC S9(05)  VALUE +5   COMP-3.
       77  PDA-SUB-CATEGORY-MAX        PIC S9(05)  VALUE +10  COMP-3.

       01  PDA-CATEGORY-ARRAY.
      ***
      ***  STRUCTURE = CATEGORY, SUB-CATEGORY COUNT, SUB-CATEGORIES
      ***
           05  FILLER                  PIC X(32)   VALUE
               'BOLTS'.
           05  FILLER                  PIC S9(5)   VALUE +8  COMP-3.
           05  FILLER                  PIC X(32)   VALUE
               'ANCHOR'.
           05  FILLER                  PIC X(32)   VALUE
               'CARRIAGE'.
           05  FILLER                  PIC X(32)   VALUE
               'EYE'.
           05  FILLER                  PIC X(32)   VALUE
               'HANGER'.
           05  FILLER                  PIC X(32)   VALUE
               'HEX'.
           05  FILLER                  PIC X(32)   VALUE
               'LAG'.
           05  FILLER                  PIC X(32)   VALUE
               'SLOTTED'.
           05  FILLER                  PIC X(32)   VALUE
               'U BOLT'.
           05  FILLER                  PIC X(32)   VALUE  SPACES.
           05  FILLER                  PIC X(32)   VALUE  SPACES.

      ***
      ***  STRUCTURE = CATEGORY, SUB-CATEGORY COUNT, SUB-CATEGORIES
      ***
           05  FILLER                  PIC X(32)   VALUE
               'NAILS'.
           05  FILLER                  PIC S9(5)   VALUE +6  COMP-3.
           05  FILLER                  PIC X(32)   VALUE
               'DECK'.
           05  FILLER                  PIC X(32)   VALUE
               'DUPLEX'.
           05  FILLER                  PIC X(32)   VALUE
               'FINISHING'.
           05  FILLER                  PIC X(32)   VALUE
               'FLOORING'.
           05  FILLER                  PIC X(32)   VALUE
               'MASONRY'.
           05  FILLER                  PIC X(32)   VALUE
               'ROOFING'.
           05  FILLER                  PIC X(32)   VALUE  SPACES.
           05  FILLER                  PIC X(32)   VALUE  SPACES.
           05  FILLER                  PIC X(32)   VALUE  SPACES.
           05  FILLER                  PIC X(32)   VALUE  SPACES.

      ***
      ***  STRUCTURE = CATEGORY, SUB-CATEGORY COUNT, SUB-CATEGORIES
      ***
           05  FILLER                  PIC X(32)   VALUE
               'NUTS'.
           05  FILLER                  PIC S9(5)   VALUE +7  COMP-3.
           05  FILLER                  PIC X(32)   VALUE
               'CAP'.
           05  FILLER                  PIC X(32)   VALUE
               'HAT'.
           05  FILLER                  PIC X(32)   VALUE
               'HEX'.
           05  FILLER                  PIC X(32)   VALUE
               'JAM'.
           05  FILLER                  PIC X(32)   VALUE
               'LOCK'.
           05  FILLER                  PIC X(32)   VALUE
               'TEE'.
           05  FILLER                  PIC X(32)   VALUE
               'WING'.
           05  FILLER                  PIC X(32)   VALUE  SPACES.
           05  FILLER                  PIC X(32)   VALUE  SPACES.
           05  FILLER                  PIC X(32)   VALUE  SPACES.

      ***
      ***  STRUCTURE = CATEGORY, SUB-CATEGORY COUNT, SUB-CATEGORIES
      ***
           05  FILLER                  PIC X(32)   VALUE
               'SCREWS'.
           05  FILLER                  PIC S9(5)   VALUE +9  COMP-3.
           05  FILLER                  PIC X(32)   VALUE
               'DOWEL'.
           05  FILLER                  PIC X(32)   VALUE
               'DRYWALL'.
           05  FILLER                  PIC X(32)   VALUE
               'HEX'.
           05  FILLER                  PIC X(32)   VALUE
               'LAG'.
           05  FILLER                  PIC X(32)   VALUE
               'MACHINE'.
           05  FILLER                  PIC X(32)   VALUE
               'SHEET METAL'.
           05  FILLER                  PIC X(32)   VALUE
               'SOCKET CAP'.
           05  FILLER                  PIC X(32)   VALUE
               'THUMB'.
           05  FILLER                  PIC X(32)   VALUE
               'WOOD'.
           05  FILLER                  PIC X(32)   VALUE  SPACES.

      ***
      ***  STRUCTURE = CATEGORY, SUB-CATEGORY COUNT, SUB-CATEGORIES
      ***
           05  FILLER                  PIC X(32)   VALUE
               'WASHERS'.
           05  FILLER                  PIC S9(5)   VALUE +6  COMP-3.
           05  FILLER                  PIC X(32)   VALUE
               'FENDER'.
           05  FILLER                  PIC X(32)   VALUE
               'FINISHING'.
           05  FILLER                  PIC X(32)   VALUE
               'FLAT'.
           05  FILLER                  PIC X(32)   VALUE
               'LOCKING'.
           05  FILLER                  PIC X(32)   VALUE
               'SAE'.
           05  FILLER                  PIC X(32)   VALUE
               'SPLIT'.
           05  FILLER                  PIC X(32)   VALUE  SPACES.
           05  FILLER                  PIC X(32)   VALUE  SPACES.
           05  FILLER                  PIC X(32)   VALUE  SPACES.
           05  FILLER                  PIC X(32)   VALUE  SPACES.

      ***
      ***  REDEFINED CATEGORY ARRAY
      ***

       01  PDA-CATEGORY-ARRAY-R        REDEFINES PDA-CATEGORY-ARRAY.

           05  PCAR-CATEGORY-GRP       OCCURS 5  TIMES.
               10  PCAR-CATEGORY       PIC X(32).
               10  PCAR-SUB-CATEGORY-COUNT
                                       PIC S9(05)   COMP-3.
               10  PCAR-SUB-CATEGORY   OCCURS 10 TIMES
                                       PIC X(32).
           EJECT