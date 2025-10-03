       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA006.
      *
      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA006                                            *
      * TRANS   :   PD06                                              *
      * MAPSET  :   PDA006M                                           *
      *                                                               *
      * FUNCTION:   PROGRAM PDA006 IS THE BROWSE ITEMS BY CATEGORY    *
      *             SCREEN WHICH CONTAINS A SCROLLABLE LIST OF ALL    *
      *             THE VALID ITEMS FOR A SELECTED CATEGORY / SUB-    *
      *             CATEGORY COMBINATION. IT IS INVOKED DURING THE    *
      *             ORDER ADD PROCESS.                                *
      *                                                               *
      * FILES   :   ITEM       -  DB2       (READ-ONLY)               *
      *                                                               *
      *                                                               *
      * TRANSACTIONS GENERATED:                                       *
      *             PD05       BROWSE CATEGORIES                      *
      *             PD07       ITEM DETAIL                            *
      *             PD01       MAIN MENU                              *
      *                                                               *
      *                                                               *
      * PFKEYS  :   PF03  =    EXIT, RETURN TO PDA005, BROWSE CAT     *
      *             PF12  =    EXIT, RETURN TO MAIN MENU              *
      *             PF07  =    SCROLL BACKWARD                        *
      *             PF08  =    SCROLL FORWARD                         *
      *                                                               *
      *                                                               *
      *****************************************************************
      *             PROGRAM CHANGE LOG                                *
      *             -------------------                               *
      *                                                               *
      *  DATE       UPDATED BY            CHANGE DESCRIPTION          *
      *  --------   --------------------  --------------------------  *
      *                                                               *
      *  12/20/05   PAUL BARON            CHANGE SENERIOS #2 AND #22  *
      *                                   TO REFERENCE AS "STORAGE    *
      *                                   OVERLAY" NOT "STORAGE       *
      *                                   VIOLATION", CHANGE WS-16 AND*
      *                                   LS-16 ARRAYS TO BE MORE     *
      *                                   DESCRIPTIVE, CHANGE THE 1   *
      *                                   BYTE ARRAY ENTRIES TO BE    *
      *                                   MORE REALISTIC I.E. ITEM    *
      *                                   STATUS INDICATORS           *
      *                                                               *
      *  02/20/04   PAUL BARON            ADD SCENARIO 22 - STORAGE   *
      *                                   OVERLAY RESULTING IN A      *
      *                                   ABNORMAL TERMINATION- ASRA  *
      *                                                               *
      *  04/18/01   PAUL BARON            ABEND TASK FOR SCENARIO 5   *
      *                                   ON A -303 SQLCODE           *
      *                                                               *
      *  XX/XX/XX   XXXXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXX  *
      *                                                               *
      *****************************************************************
 
       ENVIRONMENT DIVISION.
       DATA DIVISION.
           EJECT
       WORKING-STORAGE SECTION.
 
      *****************************************************************
      *    77 LEVEL DATA ITEMS HERE  (SUBSCRIPTS, INDEXES ETC.)       *
      *****************************************************************
       77  WS-SUB                      PIC S9(04)   COMP  VALUE +0.
       77  WS-SUB1                     PIC S9(04)   COMP  VALUE +0.
       77  WS-SUB-MAX                  PIC S9(04)   COMP  VALUE +12.
       77  WS-SUB-MAX-PLUS-ONE         PIC S9(04)   COMP  VALUE +13.
       77  WS-MESSAGE-LTH              PIC S9(04)   COMP  VALUE +79.
       77  WS-RESPONSE-CODE            PIC S9(08)   COMP  VALUE +0.
       77  LS-INITIAL-IMAGE            PIC X              VALUE '$'.
       77  LS-SUB                      PIC 99             VALUE ZEROES.
       77  WS-STATUS-ARRAY-MAX         PIC S9(04)   COMP  VALUE +17.
 
      *****************************************************************
      *    SWITCHES                                                   *
      *****************************************************************
       01  WS-SWITCHES.
 
           05  WS-MENU-SELECTION-SW    PIC X(01)             VALUE ' '.
 
           05  WS-TRANS-INTENT-SW      PIC X(01)             VALUE 'I'.
               88  INQUIRY-TRANS                             VALUE 'I'.
               88  UPDATE-TRANS                              VALUE 'U'.
 
           05  WS-END-OF-PROCESS-SW PIC X(01)             VALUE 'N'.
               88  END-OF-PROCESS                            VALUE 'Y'.
               88  NOT-END-OF-PROCESS                        VALUE 'N'.
 
           05  WS-TOP-OF-DATA-SW       PIC X(01)             VALUE 'N'.
               88  TOP-OF-DATA                               VALUE 'Y'.
               88  NOT-TOP-OF-DATA                           VALUE 'N'.
 
           05  WS-BOTTOM-OF-DATA-SW    PIC X(01)             VALUE 'N'.
               88  BOTTOM-OF-DATA                            VALUE 'Y'.
               88  NOT-BOTTOM-OF-DATA                        VALUE 'N'.
 
           05  WS-ERROR-FOUND-SW       PIC X(01)             VALUE 'N'.
               88  ERROR-FOUND                               VALUE 'Y'.
               88  NO-ERROR-FOUND                            VALUE 'N'.
 
           05  EIBAID-SW               PIC X(01)           VALUE ' '.
               88  CLEAR-KEY                               VALUE '_'.
               88  ENTER-KEY                               VALUE ''''.
               88  END-KEY                                 VALUE '3'.
               88  RETURN-KEY                              VALUE '@'.
               88  BACK-KEY                                VALUE '7'.
               88  FORWARD-KEY                             VALUE '8'.
JDL322         88  VALID-KEY-ENTERED                       VALUE '_'
ADDED                                                            '@'
88 LVL                                                           '3'
                                                                 '7'
                                                                 '8'
                                                                 ''''.
           EJECT
      *****************************************************************
      *    MISCELLANEOUS WORK FIELDS                                  *
      *****************************************************************
       01  WS-MISCELLANEOUS-FIELDS.
 
           05  WMF-USERID              PIC X(8)    VALUE SPACES.
           05  WMF-ABSTIME             PIC S9(15)  VALUE +0  COMP-3.
           05  WMF-DATE-MMDDYY         PIC X(08)   VALUE SPACES.
           05  WMF-TIME-HHMMSS         PIC X(08)   VALUE SPACES.
           05  WMF-MESSAGE-AREA        PIC X(79)   VALUE SPACES.
           05  WMF-SELECTION-COUNT     PIC S9(4)   COMP VALUE +0.
           05  WMF-SEL-SUB             PIC S9(4)   COMP VALUE +0.
 
           05  WMF-UNDERSCORE-LOWVALUE.
               10  FILLER              PIC X(01)   VALUE '_'.
               10  FILLER              PIC X(01)   VALUE LOW-VALUES.
           05  WMF-UNDERSCORE-LOWVALUE-R
                                       REDEFINES
                                       WMF-UNDERSCORE-LOWVALUE
                                       PIC X(02).
 
           05  WMF-SPACES-LOWVALUE.
               10  FILLER              PIC X(01)   VALUE SPACES.
               10  FILLER              PIC X(01)   VALUE LOW-VALUES.
           05  WMF-SPACES-LOWVALUE-R   REDEFINES
                                       WMF-SPACES-LOWVALUE
                                       PIC X(02).
 
           05  WS-ITEM-NUMBER          PIC S9(8)   VALUE +0     COMP-3. 00010000
 
      *****************************************************************
      *  THIS AREA CONTAINS THE DATA FROM THE FUNCTION CURRENT-DATE   *
      *****************************************************************
 
       01  WS-CURRENT-DATE-TIME.
           03  WS-CDT-DATE.
               05  WS-CDT-D-YEAR       PIC 9(4)  VALUE ZEROES.
               05  WS-CDT-D-MONTH      PIC 99    VALUE ZEROES.
               05  WS-CDT-D-DAY        PIC 99    VALUE ZEROES.
           03  WS-CDT-TIME.
               05  WS-CDT-T-HOURS      PIC 99    VALUE ZEROES.
               05  WS-CDT-T-MINUTES    PIC 99    VALUE ZEROES.
               05  WS-CDT-T-SECONDS    PIC 99    VALUE ZEROES.
               05  WS-CDT-T-HUNDRETHS  PIC 99    VALUE ZEROES.
           03  WS-CDT-GMT-INDICATOR    PIC X     VALUE SPACES.
               88  AHEAD-OF-GMT                  VALUE '+'.
               88  BEHIND-GMT                    VALUE '-'.
               88  GMT-NOT-AVAILABLE             VALUE '0'.
           03  WS-CDT-GMT-TIME-DIFFERENTIAL.
               05  WS-CDT-GMT-HOURS    PIC 99    VALUE ZEROES.
               05  WS-CDT-GMT-MINUTES  PIC 99    VALUE ZEROES.
           EJECT
      *****************************************************************
      *    CICS DEFINITIONS                                           *
      *****************************************************************
 
      *****************************************************************
      *         CICS ATTRIBUTE VALUES                                 *
      *****************************************************************
 
      *COPY DFHBMSCA.
      *END DFHBMSCA.
           EJECT
      *****************************************************************
      *         CICS ATTENTION IDENDIFIER VALUES                      *
      *****************************************************************
 
      *COPY DFHAID.
      *END DFHAID.
           EJECT
      *****************************************************************
      *         MAP DSECTS -- BROWSE ITEMS BY CATEGORY - PDA006M      *
      *****************************************************************
 
      *COPY PDA006M.
       01  PDA006I.
           02  FILLER PIC X(12).
           02  PDADATEL    COMP  PIC  S9(4).
           02  PDADATEF    PICTURE X.
           02  FILLER REDEFINES PDADATEF.
             03 PDADATEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  PDADATEI  PIC X(8).
           02  PDATERML    COMP  PIC  S9(4).
           02  PDATERMF    PICTURE X.
           02  FILLER REDEFINES PDATERMF.
             03 PDATERMA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  PDATERMI  PIC X(8).
           02  PDATIMEL    COMP  PIC  S9(4).
           02  PDATIMEF    PICTURE X.
           02  FILLER REDEFINES PDATIMEF.
             03 PDATIMEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  PDATIMEI  PIC X(8).
           02  PDACATL    COMP  PIC  S9(4).
           02  PDACATF    PICTURE X.
           02  FILLER REDEFINES PDACATF.
             03 PDACATA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  PDACATI  PIC X(32).
           02  PDASCATL    COMP  PIC  S9(4).
           02  PDASCATF    PICTURE X.
           02  FILLER REDEFINES PDASCATF.
             03 PDASCATA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  PDASCATI  PIC X(32).
           02  SEL1L    COMP  PIC  S9(4).
           02  SEL1F    PICTURE X.
           02  FILLER REDEFINES SEL1F.
             03 SEL1A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  SEL1I  PIC X(1).
           02  ITEM1L    COMP  PIC  S9(4).
           02  ITEM1F    PICTURE X.
           02  FILLER REDEFINES ITEM1F.
             03 ITEM1A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  ITEM1I  PIC X(32).
           02  NAME1L    COMP  PIC  S9(4).
           02  NAME1F    PICTURE X.
           02  FILLER REDEFINES NAME1F.
             03 NAME1A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NAME1I  PIC X(39).
           02  SEL2L    COMP  PIC  S9(4).
           02  SEL2F    PICTURE X.
           02  FILLER REDEFINES SEL2F.
             03 SEL2A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  SEL2I  PIC X(1).
           02  ITEM2L    COMP  PIC  S9(4).
           02  ITEM2F    PICTURE X.
           02  FILLER REDEFINES ITEM2F.
             03 ITEM2A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  ITEM2I  PIC X(32).
           02  NAME2L    COMP  PIC  S9(4).
           02  NAME2F    PICTURE X.
           02  FILLER REDEFINES NAME2F.
             03 NAME2A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NAME2I  PIC X(39).
           02  SEL3L    COMP  PIC  S9(4).
           02  SEL3F    PICTURE X.
           02  FILLER REDEFINES SEL3F.
             03 SEL3A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  SEL3I  PIC X(1).
           02  ITEM3L    COMP  PIC  S9(4).
           02  ITEM3F    PICTURE X.
           02  FILLER REDEFINES ITEM3F.
             03 ITEM3A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  ITEM3I  PIC X(32).
           02  NAME3L    COMP  PIC  S9(4).
           02  NAME3F    PICTURE X.
           02  FILLER REDEFINES NAME3F.
             03 NAME3A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NAME3I  PIC X(39).
           02  SEL4L    COMP  PIC  S9(4).
           02  SEL4F    PICTURE X.
           02  FILLER REDEFINES SEL4F.
             03 SEL4A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  SEL4I  PIC X(1).
           02  ITEM4L    COMP  PIC  S9(4).
           02  ITEM4F    PICTURE X.
           02  FILLER REDEFINES ITEM4F.
             03 ITEM4A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  ITEM4I  PIC X(32).
           02  NAME4L    COMP  PIC  S9(4).
           02  NAME4F    PICTURE X.
           02  FILLER REDEFINES NAME4F.
             03 NAME4A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NAME4I  PIC X(39).
           02  SEL5L    COMP  PIC  S9(4).
           02  SEL5F    PICTURE X.
           02  FILLER REDEFINES SEL5F.
             03 SEL5A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  SEL5I  PIC X(1).
           02  ITEM5L    COMP  PIC  S9(4).
           02  ITEM5F    PICTURE X.
           02  FILLER REDEFINES ITEM5F.
             03 ITEM5A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  ITEM5I  PIC X(32).
           02  NAME5L    COMP  PIC  S9(4).
           02  NAME5F    PICTURE X.
           02  FILLER REDEFINES NAME5F.
             03 NAME5A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NAME5I  PIC X(39).
           02  SEL6L    COMP  PIC  S9(4).
           02  SEL6F    PICTURE X.
           02  FILLER REDEFINES SEL6F.
             03 SEL6A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  SEL6I  PIC X(1).
           02  ITEM6L    COMP  PIC  S9(4).
           02  ITEM6F    PICTURE X.
           02  FILLER REDEFINES ITEM6F.
             03 ITEM6A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  ITEM6I  PIC X(32).
           02  NAME6L    COMP  PIC  S9(4).
           02  NAME6F    PICTURE X.
           02  FILLER REDEFINES NAME6F.
             03 NAME6A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NAME6I  PIC X(39).
           02  SEL7L    COMP  PIC  S9(4).
           02  SEL7F    PICTURE X.
           02  FILLER REDEFINES SEL7F.
             03 SEL7A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  SEL7I  PIC X(1).
           02  ITEM7L    COMP  PIC  S9(4).
           02  ITEM7F    PICTURE X.
           02  FILLER REDEFINES ITEM7F.
             03 ITEM7A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  ITEM7I  PIC X(32).
           02  NAME7L    COMP  PIC  S9(4).
           02  NAME7F    PICTURE X.
           02  FILLER REDEFINES NAME7F.
             03 NAME7A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NAME7I  PIC X(39).
           02  SEL8L    COMP  PIC  S9(4).
           02  SEL8F    PICTURE X.
           02  FILLER REDEFINES SEL8F.
             03 SEL8A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  SEL8I  PIC X(1).
           02  ITEM8L    COMP  PIC  S9(4).
           02  ITEM8F    PICTURE X.
           02  FILLER REDEFINES ITEM8F.
             03 ITEM8A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  ITEM8I  PIC X(32).
           02  NAME8L    COMP  PIC  S9(4).
           02  NAME8F    PICTURE X.
           02  FILLER REDEFINES NAME8F.
             03 NAME8A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NAME8I  PIC X(39).
           02  SEL9L    COMP  PIC  S9(4).
           02  SEL9F    PICTURE X.
           02  FILLER REDEFINES SEL9F.
             03 SEL9A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  SEL9I  PIC X(1).
           02  ITEM9L    COMP  PIC  S9(4).
           02  ITEM9F    PICTURE X.
           02  FILLER REDEFINES ITEM9F.
             03 ITEM9A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  ITEM9I  PIC X(32).
           02  NAME9L    COMP  PIC  S9(4).
           02  NAME9F    PICTURE X.
           02  FILLER REDEFINES NAME9F.
             03 NAME9A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NAME9I  PIC X(39).
           02  SEL10L    COMP  PIC  S9(4).
           02  SEL10F    PICTURE X.
           02  FILLER REDEFINES SEL10F.
             03 SEL10A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  SEL10I  PIC X(1).
           02  ITEM10L    COMP  PIC  S9(4).
           02  ITEM10F    PICTURE X.
           02  FILLER REDEFINES ITEM10F.
             03 ITEM10A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  ITEM10I  PIC X(32).
           02  NAME10L    COMP  PIC  S9(4).
           02  NAME10F    PICTURE X.
           02  FILLER REDEFINES NAME10F.
             03 NAME10A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NAME10I  PIC X(39).
           02  SEL11L    COMP  PIC  S9(4).
           02  SEL11F    PICTURE X.
           02  FILLER REDEFINES SEL11F.
             03 SEL11A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  SEL11I  PIC X(1).
           02  ITEM11L    COMP  PIC  S9(4).
           02  ITEM11F    PICTURE X.
           02  FILLER REDEFINES ITEM11F.
             03 ITEM11A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  ITEM11I  PIC X(32).
           02  NAME11L    COMP  PIC  S9(4).
           02  NAME11F    PICTURE X.
           02  FILLER REDEFINES NAME11F.
             03 NAME11A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NAME11I  PIC X(39).
           02  SEL12L    COMP  PIC  S9(4).
           02  SEL12F    PICTURE X.
           02  FILLER REDEFINES SEL12F.
             03 SEL12A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  SEL12I  PIC X(1).
           02  ITEM12L    COMP  PIC  S9(4).
           02  ITEM12F    PICTURE X.
           02  FILLER REDEFINES ITEM12F.
             03 ITEM12A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  ITEM12I  PIC X(32).
           02  NAME12L    COMP  PIC  S9(4).
           02  NAME12F    PICTURE X.
           02  FILLER REDEFINES NAME12F.
             03 NAME12A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NAME12I  PIC X(39).
           02  PDAMSGL    COMP  PIC  S9(4).
           02  PDAMSGF    PICTURE X.
           02  FILLER REDEFINES PDAMSGF.
             03 PDAMSGA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  PDAMSGI  PIC X(79).
       01  PDA006O REDEFINES PDA006I.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  PDADATEC    PICTURE X.
           02  PDADATEH    PICTURE X.
           02  PDADATEO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  PDATERMC    PICTURE X.
           02  PDATERMH    PICTURE X.
           02  PDATERMO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  PDATIMEC    PICTURE X.
           02  PDATIMEH    PICTURE X.
           02  PDATIMEO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  PDACATC    PICTURE X.
           02  PDACATH    PICTURE X.
           02  PDACATO  PIC X(32).
           02  FILLER PICTURE X(3).
           02  PDASCATC    PICTURE X.
           02  PDASCATH    PICTURE X.
           02  PDASCATO  PIC X(32).
           02  FILLER PICTURE X(3).
           02  SEL1C    PICTURE X.
           02  SEL1H    PICTURE X.
           02  SEL1O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ITEM1C    PICTURE X.
           02  ITEM1H    PICTURE X.
           02  ITEM1O  PIC X(32).
           02  FILLER PICTURE X(3).
           02  NAME1C    PICTURE X.
           02  NAME1H    PICTURE X.
           02  NAME1O  PIC X(39).
           02  FILLER PICTURE X(3).
           02  SEL2C    PICTURE X.
           02  SEL2H    PICTURE X.
           02  SEL2O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ITEM2C    PICTURE X.
           02  ITEM2H    PICTURE X.
           02  ITEM2O  PIC X(32).
           02  FILLER PICTURE X(3).
           02  NAME2C    PICTURE X.
           02  NAME2H    PICTURE X.
           02  NAME2O  PIC X(39).
           02  FILLER PICTURE X(3).
           02  SEL3C    PICTURE X.
           02  SEL3H    PICTURE X.
           02  SEL3O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ITEM3C    PICTURE X.
           02  ITEM3H    PICTURE X.
           02  ITEM3O  PIC X(32).
           02  FILLER PICTURE X(3).
           02  NAME3C    PICTURE X.
           02  NAME3H    PICTURE X.
           02  NAME3O  PIC X(39).
           02  FILLER PICTURE X(3).
           02  SEL4C    PICTURE X.
           02  SEL4H    PICTURE X.
           02  SEL4O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ITEM4C    PICTURE X.
           02  ITEM4H    PICTURE X.
           02  ITEM4O  PIC X(32).
           02  FILLER PICTURE X(3).
           02  NAME4C    PICTURE X.
           02  NAME4H    PICTURE X.
           02  NAME4O  PIC X(39).
           02  FILLER PICTURE X(3).
           02  SEL5C    PICTURE X.
           02  SEL5H    PICTURE X.
           02  SEL5O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ITEM5C    PICTURE X.
           02  ITEM5H    PICTURE X.
           02  ITEM5O  PIC X(32).
           02  FILLER PICTURE X(3).
           02  NAME5C    PICTURE X.
           02  NAME5H    PICTURE X.
           02  NAME5O  PIC X(39).
           02  FILLER PICTURE X(3).
           02  SEL6C    PICTURE X.
           02  SEL6H    PICTURE X.
           02  SEL6O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ITEM6C    PICTURE X.
           02  ITEM6H    PICTURE X.
           02  ITEM6O  PIC X(32).
           02  FILLER PICTURE X(3).
           02  NAME6C    PICTURE X.
           02  NAME6H    PICTURE X.
           02  NAME6O  PIC X(39).
           02  FILLER PICTURE X(3).
           02  SEL7C    PICTURE X.
           02  SEL7H    PICTURE X.
           02  SEL7O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ITEM7C    PICTURE X.
           02  ITEM7H    PICTURE X.
           02  ITEM7O  PIC X(32).
           02  FILLER PICTURE X(3).
           02  NAME7C    PICTURE X.
           02  NAME7H    PICTURE X.
           02  NAME7O  PIC X(39).
           02  FILLER PICTURE X(3).
           02  SEL8C    PICTURE X.
           02  SEL8H    PICTURE X.
           02  SEL8O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ITEM8C    PICTURE X.
           02  ITEM8H    PICTURE X.
           02  ITEM8O  PIC X(32).
           02  FILLER PICTURE X(3).
           02  NAME8C    PICTURE X.
           02  NAME8H    PICTURE X.
           02  NAME8O  PIC X(39).
           02  FILLER PICTURE X(3).
           02  SEL9C    PICTURE X.
           02  SEL9H    PICTURE X.
           02  SEL9O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ITEM9C    PICTURE X.
           02  ITEM9H    PICTURE X.
           02  ITEM9O  PIC X(32).
           02  FILLER PICTURE X(3).
           02  NAME9C    PICTURE X.
           02  NAME9H    PICTURE X.
           02  NAME9O  PIC X(39).
           02  FILLER PICTURE X(3).
           02  SEL10C    PICTURE X.
           02  SEL10H    PICTURE X.
           02  SEL10O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ITEM10C    PICTURE X.
           02  ITEM10H    PICTURE X.
           02  ITEM10O  PIC X(32).
           02  FILLER PICTURE X(3).
           02  NAME10C    PICTURE X.
           02  NAME10H    PICTURE X.
           02  NAME10O  PIC X(39).
           02  FILLER PICTURE X(3).
           02  SEL11C    PICTURE X.
           02  SEL11H    PICTURE X.
           02  SEL11O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ITEM11C    PICTURE X.
           02  ITEM11H    PICTURE X.
           02  ITEM11O  PIC X(32).
           02  FILLER PICTURE X(3).
           02  NAME11C    PICTURE X.
           02  NAME11H    PICTURE X.
           02  NAME11O  PIC X(39).
           02  FILLER PICTURE X(3).
           02  SEL12C    PICTURE X.
           02  SEL12H    PICTURE X.
           02  SEL12O  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ITEM12C    PICTURE X.
           02  ITEM12H    PICTURE X.
           02  ITEM12O  PIC X(32).
           02  FILLER PICTURE X(3).
           02  NAME12C    PICTURE X.
           02  NAME12H    PICTURE X.
           02  NAME12O  PIC X(39).
           02  FILLER PICTURE X(3).
           02  PDAMSGC    PICTURE X.
           02  PDAMSGH    PICTURE X.
           02  PDAMSGO  PIC X(79).
      *END PDA006M.
       01  FILLER  REDEFINES PDA006O.
           05  FILLER                  PIC X(125).
           05  P6-SELECTION-AREA       OCCURS 12.
               10  SEL-LEN             PIC S9(4) COMP.
               10  SEL-ATTR            PIC X.
               10  FILLER              PIC XX.
               10  SEL-OUT             PIC X.
 
               10  ITEM-LEN            PIC S9(4) COMP.
               10  ITEM-ATTR           PIC X.
               10  FILLER              PIC XX.
               10  ITEM-OUT            PIC X(32).
 
               10  NAME-LEN            PIC S9(4) COMP.
               10  NAME-ATTR           PIC X.
               10  FILLER              PIC XX.
               10  NAME-OUT            PIC X(39).
 
           05  FILLER                  PIC X(84).
 
 
      *****************************************************************
      *         SCREEN ITEM STATUS CODES FROM VENDOR                  *
      *         BLANK  = ACTIVE                                       *
      *         S      = SUSPENDED                                    *
      *         C      = CANCELLED                                    *
      *         N      = NO STOCK                                     *
      *                                                               *
      *         (DEMO PURPOSES ONLY NOT REALLY ON SCREEN)             *
      *****************************************************************
 
       01  P6-VENDOR-ITEM-STATUS.
           05  VENDOR-ITEM-STATUS-CODE-GRP
                                       PIC X(12).
           05  VENDOR-ITEM-STATUS-CODE REDEFINES
                                       VENDOR-ITEM-STATUS-CODE-GRP
                                       OCCURS 12
                                       PIC X.
           05  VENDOR-ITEM-STATUS-EXPANSION
                                       PIC X(04).
           EJECT
      *****************************************************************
      *    IMS / DLI DEFINITIONS                                      *
      *****************************************************************
 
      *****************************************************************
      *    XXXXXXXXXXXX                                               *
      *****************************************************************
 
 
      *****************************************************************
      *    DB2  DEFINITIONS                                           *
      *****************************************************************
 
      *****************************************************************
      *         SQL COMMUNICATIONS AREA                               *
      *****************************************************************
 
           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.
 
           EXEC SQL
              INCLUDE DITEM
           END-EXEC.
 
           EXEC SQL
           DECLARE ITEMFORW CURSOR FOR
             SELECT  NUMBER,
                     NAME
             FROM ITEM
             WHERE PREFIX             = :ITEM-PREFIX AND
                   CATEGORY_NAME      = :ITEM-CATEGORY-NAME AND
                   SUB_CATEGORY_NAME  = :ITEM-SUB-CATEGORY-NAME AND
                   NUMBER            >= :ITEM-NUMBER
             ORDER BY 1
           END-EXEC.
 
           EXEC SQL
           DECLARE ITEMBACK CURSOR FOR
             SELECT  NUMBER,
                     NAME
             FROM ITEM
             WHERE PREFIX             = :ITEM-PREFIX AND
                   CATEGORY_NAME      = :ITEM-CATEGORY-NAME AND
                   SUB_CATEGORY_NAME  = :ITEM-SUB-CATEGORY-NAME AND
                   NUMBER            <= :ITEM-NUMBER
             ORDER BY 1 DESC
           END-EXEC.
 
           EXEC SQL
           DECLARE CATEGORY CURSOR FOR
             SELECT  NUMBER,
                     NAME
             FROM ITEM
             WHERE PREFIX             = :ITEM-PREFIX AND
                   CATEGORY_NAME      = 'BOLTS'
           END-EXEC.
 
           EXEC SQL
           DECLARE SUBCAT CURSOR FOR
             SELECT  NUMBER,
                     NAME
             FROM ITEM
             WHERE PREFIX             = :ITEM-PREFIX
             ORDER BY SUB_CATEGORY_NAME
           END-EXEC.
 
           EJECT
      *****************************************************************
      *    MESSAGES   (ERROR AND INFORMATIONAL)                       *
      *****************************************************************
 
      *COPY PDAMSGS.
      ******************************************************************
      * PRODUCT DEMONSTRATION APPLICATION (PDA)                        *
      * ERROR / INFORMATIONAL MESSAGES                                 *
      ******************************************************************
 
       01  PDA-MESSAGES.
 
           05  PM001-INVALID-PFKEY     PIC X(79)   VALUE
               'INVALID FUNCTION KEY SELECTION - PLEASE RE-SUBMIT'.
 
           05  PM002-EXIT-APPLICATION  PIC X(79)   VALUE
               'PRODUCT DEMONSTRATION APPLICATION SESSION COMPLETE'.
 
           05  PM003-ACTION-VS-PFKEY-CONFLICT
                                       PIC X(79)   VALUE
               'ACTION/SELECTION CODE ENTRY WITH PFKEY(S) NOT ALLOWED'.
 
           05  PM004-INVALID-MENU-SELECTION
                                       PIC X(79)   VALUE
               'THE MENU SELECTION ENTERED IS INVALID'.
 
           05  PM005-SYSTEM-AT-MAXIMUM-USERS
                                       PIC X(79)   VALUE
               'UNABLE TO ADD NEW USER, PDA SYSTEM AT MAXIMUM, CONTACT S
      -        'UPPORT'.
 
           05  PM006-NUMBER-FOR-USERID.
               10  FILLER              PIC X(53)   VALUE
                'THE UNIQUE IDENTIFIER ASSOCIATED WITH THIS USERID IS'.
               10  PM006-MSG-IDNUM     PIC ZZZZZZZZ9.
 
           05  PM007-LOADING-USER-DATA PIC X(79)   VALUE
               'PLEASE WAIT ....... LOADING USER DATA'.
 
           05  PM008-CUST-NOT-FOUND    PIC X(79)   VALUE
               'CUSTOMER ID ENTERED NOT FOUND'.
 
           05  PM009-ENTER-CUST-ID     PIC X(79)   VALUE
               'PLEASE ENTER A VALID CUSTOMER ID'.
 
           05  PM010-ENTER-SELECTION   PIC X(79)   VALUE
               'PLEASE ENTER A SELECTION CODE - ''S'''.
 
           05  PM011-ONE-SELECTION     PIC X(79)   VALUE
               'ONLY ONE SELECTION CODE - ''S'' - IS ALLOWED'.
 
           05  PM012-INVALID-SEL-CODE  PIC X(79)   VALUE
               'INVALID SELECTION CODE USED, ENTER ''S'''.
 
           05  PM013-BOTTOM-MSG        PIC X(79)   VALUE
               'BOTTOM OF DATA REACHED'.
 
           05  PM014-TOP-MSG           PIC X(79)   VALUE
               'TOP OF DATA REACHED'.
 
           05  PM015-PROCEED           PIC X(79)   VALUE
               'ENTER NEW CUSTOMER ID, OR ENTER TO PROCEED WITH ORDER'.
 
           05  PM016-QUANTITY-INVALID  PIC X(79)   VALUE
               'QUANTITY ENTERED IS NOT A VALID VALUE'.
 
           05  PM017-ENTER-QUANTITY    PIC X(79)   VALUE
               'PLEASE ENTER A QUANTITY FOR AN ITEM OR USE A PFKEY'.
 
           05  PM018-ADDED-TO-ORDER    PIC X(79)   VALUE
               'SELECTED ITEMS HAVE BEEN ADDED TO ORDER'.
 
           05  PM019-ENTER-APPLICATION PIC X(79)   VALUE
               'PD01 IS USED TO ACCESS THE PRODUCT DEMONSTRATION APPLICA
      -        'TION, PRESS ENTER'.
 
           05  PM020-INVALID-ACTION-CODE
                                       PIC X(79)   VALUE
               'INVALID ACTION CODE ENTERED '.
 
           05  PM021-INVALID-ORDER-NUMBER
                                       PIC X(79)   VALUE
               'ORDER NUMBER MUST BE NUMERIC AND A NON-ZERO VALUE'.
 
           05  PM022-INQUIRY-REQUIRED  PIC X(79)   VALUE
               'AN INQUIRY IS REQUIRED BEFORE THE REQUEST CAN BE PROCESS
      -        'ED '.
 
           05  PM023-ORDER-NOT-FOUND   PIC X(79)   VALUE
               'ORDER NOT FOUND '.
 
           05  PM024-ORDER-CUSTOMER-NOT-FOUND
                                       PIC X(79)   VALUE
               'THE CUSTOMER FOR THE SELECTED ORDER NOT FOUND '.
 
           05  PM025-MAKE-SELECTION    PIC X(79)   VALUE
               'PLEASE ENTER AN ACTION OR USE A PFKEY'.
 
           05  PM026-ITEMS-PROCESSED   PIC X(79)   VALUE
               'SELECTED ITEMS HAVE BEEN PROCESSED'.
 
           05  PM027-ENTER-PURCHASE-TYPE
                                       PIC X(79)   VALUE
               'PLEASE ENTER PURCHASE TYPE'.
 
           05  PM028-INVALID-PURCHASE-TYPE
                                       PIC X(79)   VALUE
               'INVALID PURCHASE TYPE'.
 
           05  PM029-ORDER-PROCESSED   PIC X(79)   VALUE
               'ORDER HAS BEEN PROCESSED'.
 
           05  PM030-ORDER-CANCELLED   PIC X(79)   VALUE
               'ORDER HAS BEEN CANCELLED'.
 
           05  PM031-USE-PFKEY         PIC X(79)   VALUE
               'PLEASE USE A PFKEY TO MAKE A SELECTION'.
 
           05  PM032-NO-PENDING-ORDER  PIC X(79)   VALUE
               'THERE ARE NO PENDING ORDERS TO PROCESS'.
 
           05  PM033-USERID-NOT-FOUND  PIC X(79)   VALUE
               'USER IDENTIFICATION NOT FOUND IN THE PRODUCT DEMONSTRATI
      -        'ON APPLICATION '.
 
           05  PM034-CONFIRM-PROCESS   PIC X(79)   VALUE
               'PRESS PF4 AGAIN TO CONFIRM ORDER PROCESS'.
 
           05  PM035-CONFIRM-CANCELL   PIC X(79)   VALUE
               'PRESS PF5 AGAIN TO CONFIRM ORDER CANCELLATION'.
 
           05  PM036-CONFIRM-REFRESH   PIC X(79)   VALUE
               'RE-ENTER OPTION 7 TO CONFIRM REFRESH REQUEST'.
 
           05  PM037-REFRESH-COMPLETE  PIC X(79)   VALUE
               'ALL DATA HAS BEEN REFRESHED'.
 
           05  PM038-INQUIRY-COMPLETE  PIC X(79)   VALUE
               'INQUIRY COMPLETE'.
 
           05  PM039-INVALID-DATE-MONTH
                                       PIC X(79)   VALUE
               'INVALID DATE - MONTH VALUE, MUST BE 01 - 12'.
 
           05  PM040-INVALID-DATE-DAY  PIC X(79)   VALUE
               'INVALID DATE - DAY VALUE '.
 
           05  PM041-INVALID-DATE-YEAR PIC X(79)   VALUE
               'INVALID DATE - YEAR VALUE '.
 
           05  PM042-DATE-DAYS-EXCEED-MAX
                                       PIC X(79)   VALUE
              'DATE DAY VALUE EXCEEDS THE NUMBER OF DAYS IN THE MONTH'.
 
           05  PM043-UPDATE-COMPLETE   PIC X(79)   VALUE
               'UPDATE COMPLETE '.
 
           05  PM044-ORDER-DELETED     PIC X(79)   VALUE
               'ORDER HAS BEEN DELETED '.
 
           05  PM045-QTY-VS-PFKEY-CONFLICT
                                       PIC X(79)   VALUE
               'QUANTITY ENTRY WITH PFKEY(S) NOT ALLOWED'.
 
           05  PM046-INVALID-P-O-NUMBER
                                       PIC X(79)   VALUE
               'PURCHASE ORDER NUMBER MUST BE NUMERIC AND A NON-ZERO VAL
      -        'UE'.
 
           05  PM047-SCENARIOS-PROCESSED
                                       PIC X(79)   VALUE
               'SCENARIOS PROCESSED'.
 
           05  PM048-PROCEED           PIC X(79)   VALUE
              'ENTER NEW CUSTOMER ID, OR ENTER TO PROCEED WITH INQUIRY'.
 
           05  PM049-NO-MSG-AVAILABLE  PIC X(79)   VALUE
              'INQUIRY FAILED, NO RESPONSE FROM QUERY, PLEASE RE-SUBMIT
      -       'OR CONTACT SYSTEMS'.
 
           05  PM050-INVALID-TRANS-REQUEST
                                       PIC X(79)   VALUE
              'INVALID TRANSACTION REQUEST FOR CUSTOMER ORDER QUERY'.
 
           05  PM051-USER-SIGNON-REQUIRED
                                       PIC X(79)   VALUE
              'A VALID IMS USER SIGNON IS REQUIRED'.
 
           05  PM052-ORDER-MAXIMUM-EXCEEDED
                                       PIC X(79)   VALUE
              'MAXIMUM NUMBER OF ORDERS ALREADY ON FILE, ORDER NOT ADDED
      -       ''.
      *END PDAMSGS.
           EJECT
 
      *****************************************************************
      *    GENERAL ERROR PROCESSING WORK AREAS (CICS, IMS-DLI, DB2)   *
      *****************************************************************
 
      *COPY PDAERRWS.
      ******************************************************************
      * PRODUCT DEMONSTRATION APPLICATION (PDA)                        *
      *                                                                *
      * ERROR WORK AREA DEFINITIONS FOR: CICS, IMS-DLI, DB2, MQSERIES  *
      *                                                                *
      ******************************************************************
 
       77  WS-PDA-ERROR-LENGTH         PIC S9(04)      COMP  VALUE +800.
 
 
       01  WS-PDA-ERROR-GENERAL.
 
           05  WS-PDA-ERROR-TYPE       PIC X(04)       VALUE SPACES.
               88  PDA-CICS-ERROR                      VALUE 'CICS'.
               88  PDA-DB2-ERROR                       VALUE 'DB2'.
               88  PDA-IMS-ERROR                       VALUE 'IMS'.
               88  PDA-MQSERIES-ERROR                  VALUE 'MQS'.
 
 
      ******************************************************************
      *    PDA FORMATTED ERROR LINES                                   *
      ******************************************************************
 
       01  WS-PDA-ERROR-AREA.
           05  WPEA-ERROR-01           PIC X(80)       VALUE ALL '*'.
           05  WPEA-ERROR-02.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-03.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE
               '   PRODUCT DEMONSTRATION APPLICATION (PDA) ERROR '.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-04.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-05           PIC X(80)       VALUE ALL '*'.
           05  WPEA-ERROR-06.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-07.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 WPEA-ERROR-07-TEXT   PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-08.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 WPEA-ERROR-08-TEXT   PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-09.
               10 FILLER               PIC X(01)       VALUE '*'.
               10 FILLER               PIC X(78)       VALUE SPACES.
               10 FILLER               PIC X(01)       VALUE '*'.
           05  WPEA-ERROR-10           PIC X(80)       VALUE ALL '*'.
 
 
      ******************************************************************
      *    PDA CICS ERROR LINES                                        *
      ******************************************************************
 
       01  WS-PDA-CICS-ERROR-01.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(12)       VALUE
               'CICS ERROR: '.
           05  FILLER                  PIC X(10)       VALUE
               'PROGRAM = '.
           05  WPCE-PROGRAM-ID         PIC X(08)       VALUE SPACES.
           05  FILLER                  PIC X(18)       VALUE
               ', RESPONSE CODE = '.
           05  WPCE-RESPONSE-CODE      PIC ZZZZZZZ9.
           05  FILLER                  PIC X(21)       VALUE SPACES.
      *
       01  WS-PDA-CICS-ERROR-02.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(10)       VALUE
               'COMMAND = '.
           05  WPCE-COMMAND            PIC X(30)       VALUE SPACES.
           05  WPCE-COMMAND-R          REDEFINES WPCE-COMMAND.
               10  WPCE-COMMAND-1      PIC X(15).
               10  WPCE-COMMAND-2      PIC X(15).
           05  FILLER                  PIC X(14)       VALUE
               ', PARAGRAPH = '.
           05  WPCE-PARAGRAPH          PIC X(06)       VALUE SPACES.
           05  FILLER                  PIC X(17)       VALUE SPACES.
 
 
      ******************************************************************
      *    PDA IMS-DLI ERROR LINES                                     *
      ******************************************************************
 
       01  WS-PDA-IMS-ERROR-01.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(15)       VALUE
               'IMS-DLI ERROR: '.
           05  FILLER                  PIC X(08)       VALUE
               'PROGRAM='.
           05  WPIE-PROGRAM-ID         PIC X(08)       VALUE SPACES.
           05  FILLER                  PIC X(12)       VALUE
               ', PARAGRAPH='.
           05  WPIE-PARAGRAPH          PIC X(06)       VALUE SPACES.
           05  FILLER                  PIC X(09)       VALUE
               ', STATUS='.
           05  WPIE-STATUS-CODE        PIC X(2)        VALUE SPACES.
           05  FILLER                  PIC X(12)       VALUE
               ', FUNCTION='.
           05  WPIE-FUNCTION-CODE      PIC X(4)        VALUE SPACES.
      *
       01  WS-PDA-IMS-ERROR-02.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(08)       VALUE
               'SEGMENT='.
           05  WPIE-SEGMENT-NAME       PIC X(8)        VALUE SPACES.
           05  FILLER                  PIC X(11)       VALUE
               ', DATABASE='.
           05  WPIE-DATABASE-NAME      PIC X(8)        VALUE SPACES.
           05  FILLER                  PIC X(10)       VALUE
               ', COMMAND='.
           05  WPIE-COMMAND            PIC X(32)       VALUE SPACES.
 
 
      ******************************************************************
      *    PDA DB2 ERROR LINES                                         *
      ******************************************************************
 
       01  WS-PDA-DB2-ERROR-01.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(11)       VALUE
               'DB2 ERROR: '.
           05  FILLER                  PIC X(10)       VALUE
               'PROGRAM = '.
           05  WPDE-PROGRAM-ID         PIC X(08)       VALUE SPACES.
           05  FILLER                  PIC X(12)       VALUE
               ', SQLCODE = '.
           05  WPDE-DB2-SQLCODE        PIC ZZZZZZ9-.
           05  FILLER                  PIC X(28)       VALUE SPACES.
      *
       01  WS-PDA-DB2-ERROR-02.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(11)       VALUE
               'FUNCTION = '.
           05  WPDE-FUNCTION           PIC X(30)       VALUE SPACES.
           05  WPDE-FUNCTION-R         REDEFINES WPDE-FUNCTION.
               10  WPDE-FUNCTION-1     PIC X(15).
               10  WPDE-FUNCTION-2     PIC X(15).
           05  FILLER                  PIC X(14)       VALUE
               ', PARAGRAPH = '.
           05  WPDE-PARAGRAPH          PIC X(06)       VALUE SPACES.
           05  FILLER                  PIC X(16)       VALUE SPACES.
 
 
      ******************************************************************
      *    PDA MQSERIES ERROR LINES                                    *
      ******************************************************************
 
       01  WS-PDA-MQSERIES-ERROR-01.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(16)       VALUE
               'MQSERIES ERROR: '.
           05  FILLER                  PIC X(10)       VALUE
               'PROGRAM = '.
           05  WPME-PROGRAM-ID         PIC X(08)       VALUE SPACES.
           05  FILLER                  PIC X(16)       VALUE
               ', REASON CODE = '.
           05  WPME-REASON-CODE        PIC ZZZZZZZZ9.
           05  FILLER                  PIC X(18)       VALUE SPACES.
      *
       01  WS-PDA-MQSERIES-ERROR-02.
           05  FILLER                  PIC X(01)       VALUE SPACES.
           05  FILLER                  PIC X(11)       VALUE
               'FUNCTION = '.
           05  WPME-FUNCTION           PIC X(30)       VALUE SPACES.
           05  WPME-FUNCTION-R         REDEFINES WPME-FUNCTION.
               10  WPME-FUNCTION-1     PIC X(15).
               10  WPME-FUNCTION-2     PIC X(15).
           05  FILLER                  PIC X(14)       VALUE
               ', PARAGRAPH = '.
           05  WPME-PARAGRAPH          PIC X(06)       VALUE SPACES.
           05  FILLER                  PIC X(17)       VALUE SPACES.
      *END PDAERRWS.
           EJECT
 
      *****************************************************************
      *    PRODUCT DEMONSTRATION APPLICATION (PDA) COMMAREA LAYOUT    *
      *                                                               *
      *    MAIN MENU USES COMMAREA FROM WORKING STORAGE IN ORDER TO   *
      *    ESTABLISH THE INITIAL COMMAREA FOR THE APPLICATION         *
      *                                                               *
      *    ALL OTHER PROGRAMS SHOULD DEFINE / USE THE COMMAREA FROM   *
      *    THE LINKAGE SECTION, AS IT WILL BE ESTABLISHED             *
      *****************************************************************
 
      *****************************************************************
      *    P R O G R A M     W O R K A R E A                          *
      *****************************************************************
 
       01  WS-PDA006-WORKAREA.
           05  WPW-PREV-LAST-ITEM      PIC X(32) VALUE SPACES.
           05  WPW-PREV-FIRST-ITEM     PIC X(32) VALUE SPACES.
 
 
      *****************************************************************
      *    WORKING STORAGE ITEM STATUS ARRAY                          *
      *                                                               *
      *    VALID ITEM STATUS CODES (CORPORATE MASTER)                 *
      *    A = ACTIVE                                                 *
      *    B = BACK ORDER                                             *
      *    D = DISCONTINUED                                           *
      *    I = INACTIVE                                               *
      *****************************************************************
 
       01  WS-ITEM-STATUS-ARRAY.
           03  WISA-ITEM-STATUS-GRP    PIC X(12).
           03  WISA-ITEM-STATUS        REDEFINES WISA-ITEM-STATUS-GRP
                                       PIC X(01)
                                       OCCURS 12 TIMES
                                       INDEXED BY STATUS-INDEX.
           03  WISA-ITEM-STATUS-EXPANSION
                                       PIC X(04).
 
       77  WS-SAVE-NUMBER-OF-ENTRIES   PIC S9(3) COMP-3 VALUE +0.
       77  WS-NUMBER-OF-ENTRIES        PIC S9(3) COMP-3 VALUE +0.
 
      *****************************************************************
      *    L I N K A G E     S E C T I O N                            *
      *****************************************************************
 
       LINKAGE SECTION.
 
       01  DFHCOMMAREA.
      *COPY PDACOMM.
      ******************************************************************
      * PRODUCT DEMONSTRATION APPLICATION (PDA)                        *
      * CICS COMMAREA DEFINITION                                       *
      * LENGTH = 2000                                                  *
      ******************************************************************
         03  PDA-COMMAREA.
             05  PC-COMMAREA-LTH       PIC S9(04)      COMP.
             05  PC-PREV-PGRMID        PIC X(08).
             05  PC-NEXT-PGRMID        PIC X(08).
             05  PC-USERID-ID          PIC X(08).
             05  PC-USERID-NUMBER      PIC 9(05).
             05  PC-PREV-MENU-SEL      PIC X.
             05  PC-CUSTOMER-ID        PIC X(32).
             05  PC-ITEM-CATEGORY      PIC X(32).
             05  PC-ITEM-SUB-CATEGORY  PIC X(32).
             05  PC-SELECTED-ITEM      PIC X(32).
             05  PC-ORDER-NUMBER       PIC X(10).
             05  PC-ORIGINATING-PGRMID PIC X(08).
      *
             05  PC-ACTIVE-SCENARIOS-GRP
                                       PIC X(250).
             05  PC-ACTIVE-SCENARIOS-ARRAY
                                       REDEFINES
                                       PC-ACTIVE-SCENARIOS-GRP.
                 10 PC-ACTIVE-SCENARIO OCCURS 250 TIMES
                                       PIC X.
      *
             05  PC-PDA008-ORIGINATING-PGRMID
                                       PIC X(08).
             05  FILLER                PIC X(564).
             05  PC-PROGRAM-WORKAREA   PIC X(1000).
      *END PDACOMM.
 
 
      *****************************************************************
      *    LINKAGE SECTION ITEM STATUS ARRAY                          *
      *                                                               *
      *    VALID ITEM STATUS CODES (CORPORATE MASTER)                 *
      *    A = ACTIVE                                                 *
      *    B = BACK ORDER                                             *
      *    D = DISCONTINUED                                           *
      *    I = INACTIVE                                               *
      *****************************************************************
 
       01  LS-ITEM-STATUS-ARRAY.
           03  LISA-ITEM-STATUS-GRP    PIC X(12).
           03  LISA-ITEM-STATUS        REDEFINES LISA-ITEM-STATUS-GRP
                                       PIC X(01)
                                       OCCURS 12 TIMES.
           03  LISA-ITEM-STATUS-EXPANSION
                                       PIC X(04).
 
 
      *****************************************************************
      *    P R O C E D U R E    D I V I S I O N                       *
      *****************************************************************
 
       PROCEDURE DIVISION.
 
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00000-MAINLINE                                *
      *                                                               *
      *    FUNCTION :  PROGRAM ENTRY, CONTROL HIGH LEVEL PROCESSING   *
      *                FOR THE PRODUCT DEMONSTRATION APPLICATION MAIN *
      *                MENU.                                          *
      *                                                               *
      *    CALLED BY:  NONE                                           *
      *                                                               *
      *****************************************************************
 
       P00000-MAINLINE.
 
 
           EXEC CICS HANDLE CONDITION
                ERROR(P99100-GENERAL-ERROR)
           END-EXEC.
 
 
      *****************************************************************
      *    ALLOW USER TO EXIT APPLICATION WITH CLEAR KEY              *
      *    (SEND MESSAGE, ERASE SCREEN)                               *
      *****************************************************************
 
LXR214     IF EIBAID = DFHCLEAR
LXR214         MOVE PM002-EXIT-APPLICATION TO WMF-MESSAGE-AREA
LXR214         PERFORM  P80400-SEND-MESSAGE
LXR214             THRU P80400-SEND-MESSAGE-EXIT.
 
 
           PERFORM  P00050-INITIALIZE                                   TAGGED
               THRU P00050-INITIALIZE-EXIT.                             CODE
                                                                        TESTING
                                                                        03/13/01
           PERFORM  P00100-MAIN-PROCESS
               THRU P00100-MAIN-PROCESS-EXIT.
 
 
           PERFORM  P00200-CICS-RETURN
               THRU P00200-CICS-RETURN-EXIT.
 
           GOBACK.
 
       P00000-MAINLINE-EXIT.
           EXIT.
           EJECT
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00050-INITIALIZE                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INITIALIZE RELEVANT WORK FIELDS     *
      *                AND VARIABLES, PERFORM ONE TIME TASKS          *
      *                                                               *
      *    CALLED BY:  P00000-MAINLINE                                *
      *                                                               *
      *****************************************************************
 
       P00050-INITIALIZE.
 
      *****************************************************************
      *    VERIFY THE COMMAREA IS PRESENT AND CORRECT LENGTH          *
      *****************************************************************
 
           IF EIBCALEN                 > ZEROES
               IF EIBCALEN             = PC-COMMAREA-LTH
                   NEXT SENTENCE
               ELSE
                   MOVE 'CICS'         TO WS-PDA-ERROR-TYPE
                   MOVE 'PDA006'       TO WPCE-PROGRAM-ID
                   MOVE ZEROES         TO WPCE-RESPONSE-CODE
                   MOVE 'COMMAREA LENGTH NOT CORRECT'
                                       TO WPCE-COMMAND
                   MOVE 'P00050'       TO WPCE-PARAGRAPH
                   PERFORM  P99500-PDA-ERROR
                       THRU P99500-PDA-ERROR-EXIT
           ELSE
                   MOVE PM019-ENTER-APPLICATION
                                       TO  WMF-MESSAGE-AREA
                   PERFORM  P80400-SEND-MESSAGE
                       THRU P80400-SEND-MESSAGE-EXIT
                   GO TO P00050-INITIALIZE-EXIT.
 
           MOVE SPACES                 TO WS-MENU-SELECTION-SW.
           MOVE 'I'                    TO WS-TRANS-INTENT-SW.
           MOVE 'N'                    TO WS-ERROR-FOUND-SW.
           MOVE 'N'                    TO WS-TOP-OF-DATA-SW.
           MOVE 'N'                    TO WS-BOTTOM-OF-DATA-SW.
                                                                        00010000
LXR220     MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME.          00020001
 
      *****************************************************************
      *    OBTAIN CURRENT DATE AND TIME FOR DISPLAY                   *
      *****************************************************************
 
           EXEC CICS ASKTIME
                     ABSTIME (WMF-ABSTIME)
           END-EXEC.
 
 
           EXEC CICS FORMATTIME
                     ABSTIME (WMF-ABSTIME)
                     MMDDYY  (WMF-DATE-MMDDYY)
                     DATESEP ('/')
                     TIME    (WMF-TIME-HHMMSS)
                     TIMESEP
                     NOHANDLE
                     RESP    (WS-RESPONSE-CODE)
           END-EXEC.
 
 
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)                        JLC320
               NEXT SENTENCE                                            ADD
           ELSE                                                         ERROR
               MOVE 'CICS'             TO WS-PDA-ERROR-TYPE             HANDLE
               MOVE 'PDA006'           TO WPCE-PROGRAM-ID               FOR
               MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE            DATE
               MOVE 'CICS FORMATTIME ABSTIME'                           TIME
                                       TO WPCE-COMMAND                  JLC320
               MOVE 'P00050'           TO WPCE-PARAGRAPH                JLC320
               PERFORM  P99500-PDA-ERROR                                JLC320
                   THRU P99500-PDA-ERROR-EXIT.                          JLC320
 
       P00050-INITIALIZE-EXIT.
           EXIT.
           EJECT
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00100-MAIN-PROCESS                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL PROGRAM INQUIRY OR          *
      *                EDIT / UPDATE PROCESSES                        *
      *                                                               *
      *    CALLED BY:  P00000-MAINLINE                                *
      *                                                               *
      *****************************************************************
 
       P00100-MAIN-PROCESS.
 
      *****************************************************************
      *    DETERMINE TRANSACTION INTENT, INQUIRY OR EDIT / UPDATE     *
      *****************************************************************
 
           PERFORM  P00500-CHK-TRANS-INTENT
               THRU P00500-CHK-TRANS-INTENT-EXIT.
 
 
      *****************************************************************
      *    EITHER SEND INITIAL MENU SCREEN OR PERFORM SCREEN EDIT     *
      *    PROCESS                                                    *
      *****************************************************************
 
           IF INQUIRY-TRANS
               PERFORM  P01000-FIRST-TIME
                   THRU P01000-FIRST-TIME-EXIT
           ELSE
               PERFORM  P03000-EDIT-PROCESS
                   THRU P03000-EDIT-PROCESS-EXIT.
 
           MOVE WS-PDA006-WORKAREA     TO PC-PROGRAM-WORKAREA.
 
       P00100-MAIN-PROCESS-EXIT.
           EXIT.
           EJECT
 
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00200-CICS-RETURN                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETURN CONTROL TO CICS WITH THE     *
      *                NEXT TRANSACTION ID OPTION                     *
      *                                                               *
      *    CALLED BY:  P00000-MAINLINE                                *
      *                                                               *
      *****************************************************************
 
       P00200-CICS-RETURN.
 
 
           EXEC CICS RETURN
                     TRANSID       ('PD06')
                     COMMAREA      (PDA-COMMAREA)
                     LENGTH        (PC-COMMAREA-LTH)
                     NOHANDLE
                     RESP          (WS-RESPONSE-CODE)
           END-EXEC.
 
 
 
      *****************************************************************
      *    IF ERROR, FORMAT ERROR INFORMATION AND TERMINATE           *
      *****************************************************************
 
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               NEXT SENTENCE
           ELSE
               MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
               MOVE 'PDA006'           TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
               MOVE 'CICS RETURN TRANSID'
                                       TO WPCE-COMMAND
               MOVE 'P00200'           TO WPCE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.
 
 
       P00200-CICS-RETURN-EXIT.
           EXIT.
           EJECT
 
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00500-CHK-TRANS-INTENT                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DETERMINE INQUIRY MODE (1ST TIME    *
      *                THRU) OR EDIT / UPDATE MODE                    *
      *                                                               *
      *    CALLED BY:  P00100-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************
 
       P00500-CHK-TRANS-INTENT.
 
      *****************************************************************
      *    IF PREVIOUS PROGRAM IS NOT PDA006, SET INQUIRY MODE        *
      *    OTHERWISE SET EDIT / UPDATE MODE                           *
      *****************************************************************
 
           IF PC-PREV-PGRMID           =  'PDA006'
               MOVE 'U'                TO WS-TRANS-INTENT-SW
           ELSE
               MOVE 'I'                TO WS-TRANS-INTENT-SW.
 
 
       P00500-CHK-TRANS-INTENT-EXIT.
           EXIT.
           EJECT
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P01000-FIRST-TIME                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL PROCESSING TO SEND THE      *
      *                INITIAL SCREEN                                 *
      *                                                               *
      *    CALLED BY:  P00100-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************
 
       P01000-FIRST-TIME.
 
      *****************************************************************
      *    INITIALIZE COMMAREA AND MAP                                *
      *****************************************************************
 
           MOVE LOW-VALUES             TO PDA006I.
 
           MOVE WMF-DATE-MMDDYY        TO PDADATEO.
           MOVE EIBTRMID               TO PDATERMO.
           MOVE WMF-TIME-HHMMSS        TO PDATIMEO.
           MOVE SPACES                 TO PC-SELECTED-ITEM.
 
      *****************************************************************
      *    FORMAT AND SEND THE FULL MAP -- LITERALS AND DATA          *
      *****************************************************************
 
           MOVE PC-ITEM-CATEGORY       TO ITEM-CATEGORY-NAME
                                          PDACATI.
           MOVE PC-ITEM-SUB-CATEGORY   TO ITEM-SUB-CATEGORY-NAME
                                          PDASCATI.
           MOVE PC-USERID-NUMBER       TO ITEM-PREFIX.
 
           MOVE SPACES                 TO ITEM-NUMBER.
           MOVE 'N'                    TO WS-END-OF-PROCESS-SW.
 
           PERFORM P05200-SCROLL-FORWARD
              THRU P05200-SCROLL-FORWARD-EXIT.
 
 
           PERFORM  P79000-DISPLAY-SCREEN
              THRU P79000-DISPLAY-SCREEN-EXIT.
 
           MOVE 'PDA006'               TO PC-PREV-PGRMID.
 
       P01000-FIRST-TIME-EXIT.
           EXIT.
           EJECT
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE PROGRAM EDIT PROCESS    *
      *                                                               *
      *    CALLED BY:  P00100-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************
 
       P03000-EDIT-PROCESS.
 
DFH401     MOVE 'PDA006'               TO PC-PREV-PGRMID.
DFH401     MOVE PC-PROGRAM-WORKAREA    TO WS-PDA006-WORKAREA.
 
      *****************************************************************
      *    RECEIVE THE INPUT MAP                                      *
      *****************************************************************
 
           PERFORM  P80200-RECEIVE-MAP
               THRU P80200-RECEIVE-MAP-EXIT.
 
           MOVE WMF-DATE-MMDDYY        TO PDADATEO.
           MOVE EIBTRMID               TO PDATERMO.
           MOVE WMF-TIME-HHMMSS        TO PDATIMEO.
           MOVE SPACES                 TO PDAMSGO.
 
           MOVE PC-ITEM-CATEGORY       TO PDACATI.
           MOVE PC-ITEM-SUB-CATEGORY   TO PDASCATI.
           MOVE ' SC N S  N C'         TO VENDOR-ITEM-STATUS-CODE-GRP.
 
      *****************************************************************
      *    PERFORM THE SCREEN EDIT PROCESS (PFKEY AND DATA VALIDATION)*
      *****************************************************************
 
           PERFORM  P03100-EDIT-SCREEN
               THRU P03100-EDIT-SCREEN-EXIT.
 
           PERFORM  P79000-DISPLAY-SCREEN
              THRU P79000-DISPLAY-SCREEN-EXIT.
 
       P03000-EDIT-PROCESS-EXIT.
           EXIT.
           EJECT
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE SCREEN EDIT PROCESS     *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************
 
       P03100-EDIT-SCREEN.
 
           IF PC-ACTIVE-SCENARIO(10)   = 'Y'
               MOVE PC-USERID-NUMBER   TO ITEM-PREFIX
               PERFORM P03400-VERIFY-CATEGORY
                  THRU P03400-VERIFY-CATEGORY-EXIT.
 
           IF PC-ACTIVE-SCENARIO(11)   = 'Y'
               MOVE PC-USERID-NUMBER   TO ITEM-PREFIX
               PERFORM P03500-VERIFY-SUBCATEGORY
                  THRU P03500-VERIFY-SUBCATEGORY-EXIT.
 
           PERFORM P03110-INSPECT-SELECTIONS
              THRU P03110-INSPECT-SELECTIONS-EXIT
             VARYING WS-SUB1 FROM 1 BY 1 UNTIL
               WS-SUB1 > WS-SUB-MAX.
 
      *****************************************************************
      *    EDIT THE OPERATOR PROGRAM FUNCTION KEY SELECTION (PFKEY)   *
      *****************************************************************
 
           PERFORM  P03200-EDIT-PFKEY
               THRU P03200-EDIT-PFKEY-EXIT.
 
           IF ERROR-FOUND
               GO TO P03100-EDIT-SCREEN-EXIT.
 
 
      *****************************************************************
      *    ONLY ONE ITEM MAY BE SELECTED                              *
      *****************************************************************
 
DRZ403     IF WMF-SELECTION-COUNT      > +1
ADDED        MOVE -1                   TO SEL-LEN  (WMF-SEL-SUB)
EDIT         MOVE DFHUNINT             TO SEL-ATTR (WMF-SEL-SUB)
DRZ403                                    SEL-ATTR (WS-SUB1)
DRZ403       MOVE PM011-ONE-SELECTION  TO  WMF-MESSAGE-AREA
DRZ403       PERFORM  P70000-ERROR-ROUTINE
DRZ403           THRU P70000-ERROR-ROUTINE-EXIT
DRZ403       GO TO P03100-EDIT-SCREEN-EXIT.
 
 
      *****************************************************************
      *    EDIT THE OPERATOR ENTERED SELECTION                        *
      *****************************************************************
 
           PERFORM  P03300-EDIT-SELECTION
               THRU P03300-EDIT-SELECTION-EXIT
             VARYING WS-SUB1 FROM 1 BY 1 UNTIL
               WS-SUB1 > WS-SUB-MAX.
 
           IF ERROR-FOUND
               GO TO P03100-EDIT-SCREEN-EXIT.
 
           IF ENTER-KEY                AND
            WMF-SELECTION-COUNT        = +1
             MOVE 'PDA007'             TO PC-NEXT-PGRMID
             PERFORM P80300-XFER-CONTROL
                THRU P80300-XFER-CONTROL-EXIT.
 
       P03100-EDIT-SCREEN-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03110-INSPECT-SELECTIONS                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO INSPECT ALL 12 SELECTION CODE FLDS  *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************
 
       P03110-INSPECT-SELECTIONS.
 
           INSPECT SEL-OUT (WS-SUB1)
               CONVERTING  WMF-UNDERSCORE-LOWVALUE-R TO SPACES.
 
       P03110-INSPECT-SELECTIONS-EXIT.
           EXIT.
           EJECT
 
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03200-EDIT-PFKEY                              *
      *                                                               *
      *    FUNCTION :  ROUTINE TO VALIDATE PROGRAM FUNCTION KEY USAGE *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************
 
       P03200-EDIT-PFKEY.
 
      *****************************************************************
      *    VALID KEYS ARE: ENTER, PF12, PF3, PF7, PF8, CLEAR          *
      *****************************************************************
 
           MOVE EIBAID                 TO EIBAID-SW.
 
           IF VALID-KEY-ENTERED
               NEXT SENTENCE
           ELSE
               MOVE -1                 TO SEL1L
               MOVE PM001-INVALID-PFKEY
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P03200-EDIT-PFKEY-EXIT.
 
 
           PERFORM P03220-COUNT-SELECTIONS
             THRU P03220-COUNT-SELECTIONS-EXIT
             VARYING WS-SUB1 FROM 1 BY 1 UNTIL
               WS-SUB1 > WS-SUB-MAX.
 
      *****************************************************************
      *    IF SELECTION ENTERED AND PFKEY HIT, DISPLAY ERROR MESSAGE  *
      *****************************************************************
 
           IF WMF-SELECTION-COUNT      > ZEROS AND
              VALID-KEY-ENTERED
             IF ENTER-KEY
               NEXT SENTENCE
             ELSE
               MOVE -1                 TO SEL-LEN (WMF-SEL-SUB)
               MOVE PM003-ACTION-VS-PFKEY-CONFLICT
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P03200-EDIT-PFKEY-EXIT.
 
           IF ENTER-KEY
             IF WMF-SELECTION-COUNT  < +1
               MOVE -1             TO  SEL-LEN (1)
               MOVE PM010-ENTER-SELECTION
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
               GO TO P03200-EDIT-PFKEY-EXIT.
 
      *****************************************************************
      *    PF03 FROM THIS SCREEN RETURNS USER TO BROWSE CAT SCREEN    *
      *****************************************************************
 
           IF END-KEY                                                   JLC405
               MOVE 'PDA005'           TO PC-NEXT-PGRMID                ADDED
               MOVE SPACES             TO PC-SELECTED-ITEM              PF3
               PERFORM  P80300-XFER-CONTROL                             FUNCTION
                   THRU P80300-XFER-CONTROL-EXIT.                       JLC405
 
 
      *****************************************************************
      *    PF12 FROM THIS SCREEN RETURNS USER TO THE MAIN MENU        *
      *****************************************************************
 
           IF RETURN-KEY
               MOVE 'PDA001'           TO PC-NEXT-PGRMID
               PERFORM  P80300-XFER-CONTROL
                   THRU P80300-XFER-CONTROL-EXIT.
 
      *****************************************************************
      *    ALLOW USER TO EXIT APPLICATION WITH CLEAR KEY              *
      *    (SEND MESSAGE, ERASE SCREEN)                               *
      *****************************************************************
 
           IF CLEAR-KEY
               MOVE PM002-EXIT-APPLICATION
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P80400-SEND-MESSAGE
                   THRU P80400-SEND-MESSAGE-EXIT
               GO TO P03200-EDIT-PFKEY-EXIT.
 
           IF BACK-KEY
             MOVE 'N'                    TO WS-END-OF-PROCESS-SW
             MOVE PC-ITEM-CATEGORY       TO ITEM-CATEGORY-NAME
             MOVE PC-ITEM-SUB-CATEGORY   TO ITEM-SUB-CATEGORY-NAME
             MOVE PC-USERID-NUMBER       TO ITEM-PREFIX
             MOVE WPW-PREV-FIRST-ITEM    TO ITEM-NUMBER
             IF WPW-PREV-FIRST-ITEM      > SPACES
               PERFORM P06200-SCROLL-BACK
                  THRU P06200-SCROLL-BACK-EXIT
             ELSE
               PERFORM P05200-SCROLL-FORWARD
                  THRU P05200-SCROLL-FORWARD-EXIT.
 
           IF FORWARD-KEY
             MOVE 'N'                    TO WS-END-OF-PROCESS-SW
             MOVE PC-ITEM-CATEGORY       TO ITEM-CATEGORY-NAME
             MOVE PC-ITEM-SUB-CATEGORY   TO ITEM-SUB-CATEGORY-NAME
             MOVE PC-USERID-NUMBER       TO ITEM-PREFIX
             IF WPW-PREV-LAST-ITEM       > SPACES
               MOVE WPW-PREV-LAST-ITEM   TO ITEM-NUMBER
             ELSE
               MOVE WPW-PREV-FIRST-ITEM  TO ITEM-NUMBER
             END-IF
             PERFORM P05200-SCROLL-FORWARD
                THRU P05200-SCROLL-FORWARD-EXIT.
 
       P03200-EDIT-PFKEY-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03220-COUNT-SELECTIONS                        *
      *                                                               *
      *    FUNCTION :  ROUTINE TO COUNT SELECTIONS ENTERED            *
      *                                                               *
      *    CALLED BY:  P03200-EDIT-PFKEY                              *
      *                                                               *
      *****************************************************************
 
       P03220-COUNT-SELECTIONS.
 
           IF SEL-OUT (WS-SUB1) > SPACES
             ADD +1             TO WMF-SELECTION-COUNT
             IF WMF-SEL-SUB     = +0
                 MOVE WS-SUB1   TO WMF-SEL-SUB.
 
       P03220-COUNT-SELECTIONS-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03300-EDIT-SELECTION                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO VALIDATE THE ITEM SELECTION         *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************
 
       P03300-EDIT-SELECTION.
 
           IF SEL-OUT (WS-SUB1)        > SPACES
      *****************************************************************
      *    SELECTION CODE MUST BE AN 'S'                              *
      *****************************************************************
             IF SEL-OUT (WS-SUB1)  NOT = 'S'
               MOVE -1                 TO SEL-LEN  (WS-SUB1)
               MOVE DFHUNINT           TO SEL-ATTR (WS-SUB1)
               MOVE PM012-INVALID-SEL-CODE
                                       TO  WMF-MESSAGE-AREA
               PERFORM  P70000-ERROR-ROUTINE
                   THRU P70000-ERROR-ROUTINE-EXIT
             ELSE
               MOVE ITEM-OUT (WS-SUB1)
                                       TO PC-SELECTED-ITEM.
 
       P03300-EDIT-SELECTION-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03400-VERIFY-CATEGORY                         *
      *                                                               *
      *    FUNCTION :  ROUTINE TO VALIDATE THE CATEGORY               *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************
 
       P03400-VERIFY-CATEGORY.
 
           PERFORM P03410-OPEN-CATEGORY-CSR
              THRU P03410-OPEN-CATEGORY-CSR-EXIT.
 
           IF ERROR-FOUND
             GO TO P03400-VERIFY-CATEGORY-EXIT.
 
           MOVE 'N' TO WS-END-OF-PROCESS-SW.
 
           PERFORM P03420-FETCH-CATEGORY
              THRU P03420-FETCH-CATEGORY-EXIT
                  UNTIL END-OF-PROCESS.
 
           PERFORM P03430-CLOSE-CATEGORY-CSR
              THRU P03430-CLOSE-CATEGORY-CSR-EXIT.
 
       P03400-VERIFY-CATEGORY-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03410-OPEN-CATEGORY-CSR                       *
      *                                                               *
      *    FUNCTION :  OPENS CURSOR USED TO VERIFY CATEGORY           *
      *                                                               *
      *    CALLED BY:  P03400-VERIFY-CATEGORY                         *
      *                                                               *
      *****************************************************************
 
       P03410-OPEN-CATEGORY-CSR.
 
           EXEC SQL
               OPEN CATEGORY
           END-EXEC.
 
           IF SQLCODE                  NOT = ZEROS
             MOVE 'DB2'                TO WS-PDA-ERROR-TYPE
             MOVE 'PDA006'             TO WPDE-PROGRAM-ID
             MOVE SQLCODE              TO WPDE-DB2-SQLCODE
             MOVE 'OPEN CATEGORY CURSOR' TO WPDE-FUNCTION
             MOVE 'P03410'             TO WPDE-PARAGRAPH
             PERFORM P99500-PDA-ERROR
                THRU P99500-PDA-ERROR-EXIT.
 
       P03410-OPEN-CATEGORY-CSR-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03420-FETCH-CATEGORY                          *
      *                                                               *
      *    FUNCTION :  FETCHS THE CATEGORY CURSOR                     *
      *                                                               *
      *    CALLED BY:  P03400-VERIFY-CATEGORY                         *
      *                                                               *
      *****************************************************************
 
       P03420-FETCH-CATEGORY.
 
           EXEC SQL
               FETCH  CATEGORY
                INTO  :ITEM-NUMBER,
                      :ITEM-NAME
           END-EXEC.
 
           IF SQLCODE                  = ZEROS
             NEXT SENTENCE
           ELSE
             IF SQLCODE                = +100
               MOVE 'Y'                TO WS-END-OF-PROCESS-SW
             ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA006'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'FETCH CATEGORY CURSOR' TO WPDE-FUNCTION
               MOVE 'P03420'           TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR
                  THRU P99500-PDA-ERROR-EXIT.
 
       P03420-FETCH-CATEGORY-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03430-CLOSE-CATEGORY-CSR                      *
      *                                                               *
      *    FUNCTION :  CLOSES CURSOR USED TO VERIFY CATEGORY          *
      *                                                               *
      *    CALLED BY:  P03400-VERIFY-CATEGORY                         *
      *                                                               *
      *****************************************************************
 
       P03430-CLOSE-CATEGORY-CSR.
 
           EXEC SQL
               CLOSE CATEGORY
           END-EXEC.
 
           IF SQLCODE                  NOT = ZEROS
             MOVE 'DB2'                TO WS-PDA-ERROR-TYPE
             MOVE 'PDA006'             TO WPDE-PROGRAM-ID
             MOVE SQLCODE              TO WPDE-DB2-SQLCODE
             MOVE 'CLOSE CATEGORY CURSOR' TO WPDE-FUNCTION
             MOVE 'P03430'             TO WPDE-PARAGRAPH
             PERFORM P99500-PDA-ERROR
                THRU P99500-PDA-ERROR-EXIT.
 
       P03430-CLOSE-CATEGORY-CSR-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03500-VERIFY-SUBCATEGORY                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO VALIDATE THE SUB-CATEGORY           *
      *                                                               *
      *    CALLED BY:  P03100-EDIT-SCREEN                             *
      *                                                               *
      *****************************************************************
 
       P03500-VERIFY-SUBCATEGORY.
 
           PERFORM P03510-OPEN-SUBCAT-CSR
              THRU P03510-OPEN-SUBCAT-CSR-EXIT.
 
           IF ERROR-FOUND
             GO TO P03500-VERIFY-SUBCATEGORY-EXIT.
 
           MOVE 'N' TO WS-END-OF-PROCESS-SW.
 
           PERFORM P03520-FETCH-SUBCAT
              THRU P03520-FETCH-SUBCAT-EXIT
                  UNTIL END-OF-PROCESS.
 
           PERFORM P03530-CLOSE-SUBCAT-CSR
              THRU P03530-CLOSE-SUBCAT-CSR-EXIT.
 
       P03500-VERIFY-SUBCATEGORY-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03510-OPEN-SUBCAT-CSR                         *
      *                                                               *
      *    FUNCTION :  OPENS CURSOR USED TO VERIFY SUB-CATEGORY       *
      *                                                               *
      *    CALLED BY:  P03500-VERIFY-SUBCATEGORY                      *
      *                                                               *
      *****************************************************************
 
       P03510-OPEN-SUBCAT-CSR.
 
           EXEC SQL
               OPEN SUBCAT
           END-EXEC.
 
           IF SQLCODE                  NOT = ZEROS
             MOVE 'DB2'                TO WS-PDA-ERROR-TYPE
             MOVE 'PDA006'             TO WPDE-PROGRAM-ID
             MOVE SQLCODE              TO WPDE-DB2-SQLCODE
             MOVE 'OPEN SUBCAT CURSOR' TO WPDE-FUNCTION
             MOVE 'P03510'             TO WPDE-PARAGRAPH
             PERFORM P99500-PDA-ERROR
                THRU P99500-PDA-ERROR-EXIT.
 
       P03510-OPEN-SUBCAT-CSR-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03520-FETCH-SUBCAT                            *
      *                                                               *
      *    FUNCTION :  FETCHS THE SUB-CATEGORY CURSOR                 *
      *                                                               *
      *    CALLED BY:  P03500-VERIFY-SUBCATEGORY                      *
      *                                                               *
      *****************************************************************
 
       P03520-FETCH-SUBCAT.
 
           EXEC SQL
               FETCH  SUBCAT
                INTO  :ITEM-NUMBER,
                      :ITEM-NAME
           END-EXEC.
 
           IF SQLCODE                  = ZEROS
             NEXT SENTENCE
           ELSE
             IF SQLCODE                = +100
               MOVE 'Y'                TO WS-END-OF-PROCESS-SW
             ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA006'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'FETCH SUBCAT CURSOR' TO WPDE-FUNCTION
               MOVE 'P03520'           TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR
                  THRU P99500-PDA-ERROR-EXIT.
 
       P03520-FETCH-SUBCAT-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P03530-CLOSE-SUBCAT-CSR                        *
      *                                                               *
      *    FUNCTION :  CLOSES CURSOR USED TO VERIFY SUB-CATEGORY      *
      *                                                               *
      *    CALLED BY:  P03500-VERIFY-SUBCATEGORY                      *
      *                                                               *
      *****************************************************************
 
       P03530-CLOSE-SUBCAT-CSR.
 
           EXEC SQL
               CLOSE SUBCAT
           END-EXEC.
 
           IF SQLCODE                  NOT = ZEROS
             MOVE 'DB2'                TO WS-PDA-ERROR-TYPE
             MOVE 'PDA006'             TO WPDE-PROGRAM-ID
             MOVE SQLCODE              TO WPDE-DB2-SQLCODE
             MOVE 'CLOSE SUBCAT CURSOR' TO WPDE-FUNCTION
             MOVE 'P03530'             TO WPDE-PARAGRAPH
             PERFORM P99500-PDA-ERROR
                THRU P99500-PDA-ERROR-EXIT.
 
       P03530-CLOSE-SUBCAT-CSR-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05200-SCROLL-FORWARD                          *
      *                                                               *
      *    FUNCTION :  PERFORMS FETCH AND FORMATS ITEM LINES          *
      *                                                               *
      *    CALLED BY:  P01000-FIRST-TIME                              *
      *                P03200-EDIT-PFKEY                              *
      *                                                               *
      *****************************************************************
 
       P05200-SCROLL-FORWARD.
 
      *****************************************************************
      * IF FIRST-TIME-PROCESSING, KEYS NEED NOT BE CHECKED            *
      *****************************************************************
 
           IF INQUIRY-TRANS
               NEXT SENTENCE
           ELSE
               PERFORM P78000-CLEAR-SCREEN
                  THRU P78000-CLEAR-SCREEN-EXIT
                       VARYING WS-SUB1 FROM 1 BY 1
                           UNTIL WS-SUB1 > WS-SUB-MAX.
 
 
           PERFORM P05220-OPEN-FORW-CURSOR
              THRU P05220-OPEN-FORW-CURSOR-EXIT.
 
           IF ERROR-FOUND
             GO TO P05200-SCROLL-FORWARD-EXIT.
 
 
           PERFORM P05230-FORMAT-FORW-LINE
              THRU P05230-FORMAT-FORW-LINE-EXIT
             VARYING WS-SUB1 FROM 1 BY 1
             UNTIL END-OF-PROCESS.
 
           IF END-OF-PROCESS
             PERFORM P05290-CLOSE-FORW-CURSOR
                THRU P05290-CLOSE-FORW-CURSOR-EXIT.
 
 
           IF UPDATE-TRANS
               PERFORM P05300-LOAD-STATUS-ARRAYS
                  THRU P05300-LOAD-STATUS-ARRAYS-EXIT.
 
       P05200-SCROLL-FORWARD-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05220-OPEN-FORW-CURSOR                        *
      *                                                               *
      *    FUNCTION :  OPENS CURSOR USED TO CREATE ITEM LIST          *
      *                                                               *
      *    CALLED BY:  P05200-SCROLL-FORWARD                          *
      *                                                               *
      *****************************************************************
 
       P05220-OPEN-FORW-CURSOR.
 
           EXEC SQL
               OPEN ITEMFORW
           END-EXEC.
 
           IF SQLCODE                  = ZEROS
             NEXT SENTENCE
           ELSE
             MOVE 'DB2'                TO WS-PDA-ERROR-TYPE
             MOVE 'PDA006'             TO WPDE-PROGRAM-ID
             MOVE SQLCODE              TO WPDE-DB2-SQLCODE
             MOVE 'OPEN FORWARD CURSR' TO WPDE-FUNCTION
             MOVE 'P05220'             TO WPDE-PARAGRAPH
             PERFORM P99500-PDA-ERROR
                THRU P99500-PDA-ERROR-EXIT.
 
       P05220-OPEN-FORW-CURSOR-EXIT.
           EXIT.
           EJECT
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05230-FORMAT-FORW-LINE                        *
      *                                                               *
      *    FUNCTION :  FORMATS LINE OF LIST                           *
      *                                                               *
      *    CALLED BY:  P05200-SCROLL-FORWARD                          *
      *                                                               *
      *****************************************************************
 
       P05230-FORMAT-FORW-LINE.
 
           IF WS-SUB1                  > WS-SUB-MAX
             MOVE 'Y'                  TO WS-END-OF-PROCESS-SW
             GO TO P05230-FORMAT-FORW-LINE-EXIT.
 
           IF FORWARD-KEY AND
               PC-ACTIVE-SCENARIO(5) = 'Y'
                   PERFORM P05240-FETCH-FORWARD-ROW
                      THRU P05240-FETCH-FORWARD-ROW-EXIT.
 
           PERFORM P05250-FETCH-FORWARD-ROW
              THRU P05250-FETCH-FORWARD-ROW-EXIT.
 
           IF ERROR-FOUND OR END-OF-PROCESS
             GO TO P05230-FORMAT-FORW-LINE-EXIT.
 
 
           MOVE ITEM-NUMBER            TO ITEM-OUT (WS-SUB1).
           MOVE ITEM-NAME              TO NAME-OUT (WS-SUB1).
 
 
 
       P05230-FORMAT-FORW-LINE-EXIT.
           EXIT.
           EJECT
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05240-FETCH-FORWARD-ROW                       *
      *                                                               *
      *    FUNCTION :  FETCH ROW FROM FORWARD CURSOR USING AN INVALID *
      *                FETCH TO CAUSE A SQLCODE OF -303               *
      *                                                               *
      *    CALLED BY:  P05200-SCROLL-FORWARD                          *
      *                                                               *
      *****************************************************************
 
       P05240-FETCH-FORWARD-ROW.
 
           EXEC SQL
               FETCH  ITEMFORW
                INTO  :WS-ITEM-NUMBER,
                      :ITEM-NAME
           END-EXEC.
 
           IF SQLCODE                  = ZEROS
             NEXT SENTENCE
           ELSE
             IF SQLCODE                = +100
               MOVE 'Y'                TO WS-END-OF-PROCESS-SW
               IF WS-SUB1              <  WS-SUB-MAX-PLUS-ONE
                 MOVE 'Y'              TO WS-BOTTOM-OF-DATA-SW
                 MOVE PM013-BOTTOM-MSG TO PDAMSGO
               ELSE
                 NEXT SENTENCE
             ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA006'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'FETCH FORW CURS'  TO WPDE-FUNCTION
               MOVE 'P05240'           TO WPDE-PARAGRAPH
 
               EXEC CICS DUMP
                         TRANSACTION
                         DUMPCODE('DB2E')
                         COMPLETE
               END-EXEC
 
               EXEC CICS ABEND
                         ABCODE('DB2E')
                         NODUMP
               END-EXEC.
 
       P05240-FETCH-FORWARD-ROW-EXIT.
           EXIT.
           EJECT
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05250-FETCH-FORWARD-ROW                       *
      *                                                               *
      *    FUNCTION :  FETCH ROW FROM FORWARD CURSOR                  *
      *                                                               *
      *    CALLED BY:  P05200-SCROLL-FORWARD                          *
      *                                                               *
      *****************************************************************
 
       P05250-FETCH-FORWARD-ROW.
 
           EXEC SQL
               FETCH  ITEMFORW
                INTO  :ITEM-NUMBER,
                      :ITEM-NAME
           END-EXEC.
 
           IF SQLCODE                  = ZEROS
             NEXT SENTENCE
           ELSE
             IF SQLCODE                = +100
               MOVE 'Y'                TO WS-END-OF-PROCESS-SW
               IF WS-SUB1              <  WS-SUB-MAX-PLUS-ONE
                 MOVE 'Y'              TO WS-BOTTOM-OF-DATA-SW
                 MOVE PM013-BOTTOM-MSG TO PDAMSGO
               ELSE
                 NEXT SENTENCE
             ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA006'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'FETCH FORW CURS'  TO WPDE-FUNCTION
               MOVE 'P05250'           TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR
                  THRU P99500-PDA-ERROR-EXIT.
 
       P05250-FETCH-FORWARD-ROW-EXIT.
           EXIT.
           EJECT
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05290-CLOSE-FORWARD-CURSOR                    *
      *                                                               *
      *    FUNCTION :  CLOSES CURSOR USED TO CREATE ITEM LIST         *
      *                                                               *
      *    CALLED BY:  P05200-SCROLL-FORWARD                          *
      *                                                               *
      *****************************************************************
 
       P05290-CLOSE-FORW-CURSOR.
 
           EXEC SQL
               CLOSE ITEMFORW
           END-EXEC.
 
           IF SQLCODE                  = ZEROS
             NEXT SENTENCE
           ELSE
             MOVE 'DB2'                TO WS-PDA-ERROR-TYPE
             MOVE 'PDA006'             TO WPDE-PROGRAM-ID
             MOVE SQLCODE              TO WPDE-DB2-SQLCODE
             MOVE 'CLOSE FORW CURSOR'  TO WPDE-FUNCTION
             MOVE 'P05290'             TO WPDE-PARAGRAPH
             PERFORM P99500-PDA-ERROR
                THRU P99500-PDA-ERROR-EXIT.
 
       P05290-CLOSE-FORW-CURSOR-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05300-LOAD-STATUS-ARRAYS                      *
      *                                                               *
      *    FUNCTION :  ROUTINE TO LOAD THE ITEM STATUS ARRAYS IN BOTH *
      *                THE LINKAGE AND WORKING STORAGE SECTIONS. THE  *
      *                VENDOR ITEM STATUS FROM THE SCREEN MUST BE     *
      *                CONVERTED TO THE CORPORATE MASTER EQUIVALENT   *
      *                BEFORE PROCESSING.                             *
      *                                                               *
      *                THE ITEM STATUS CONVERSIONS ARE:               *
      *                                                               *
      *                VENDOR ITEM STATUS       CORPORATE ITEM STATUS *
      *                ------------------       --------------------- *
      *                BLANK = ACTIVE    =====> A = ACTIVE            *
      *                C     = CANCELLED =====> D = DISCONTINUED      *
      *                N     = NO STOCK  =====> B = BACKORDER         *
      *                S     = SUSPENDED =====> I = INACTIVE          *
      *                                                               *
      *                                                               *
      *    CALLED BY:  P05200-SCROLL-FORWARD                          *
      *                                                               *
      *****************************************************************
 
       P05300-LOAD-STATUS-ARRAYS.
 
      *****************************************************************
      * LOAD STATUS BYTE ARRAY (LINKAGE SECTION)                      *
      *****************************************************************
 
           IF PC-ACTIVE-SCENARIO(2) = 'Y'
               EXEC CICS
                    GETMAIN
                    SET(ADDRESS OF LS-ITEM-STATUS-ARRAY)
                    LENGTH(16)
                    INITIMG(LS-INITIAL-IMAGE)
               END-EXEC
 
               PERFORM  P05360-LOAD-STATUS
                   THRU P05360-LOAD-STATUS-EXIT
                       VARYING LS-SUB FROM 1 BY 1
                           UNTIL LS-SUB > WS-STATUS-ARRAY-MAX
           END-IF.
 
      *****************************************************************
      * LOAD STATUS BYTE ARRAY (WORKING STORAGE)                      *
      *****************************************************************
 
           IF PC-ACTIVE-SCENARIO(22) = 'Y'
               SET STATUS-INDEX TO 1
               MOVE +1          TO WS-SUB
 
               PERFORM  P05330-LOAD-STATUS
                   THRU P05330-LOAD-STATUS-EXIT
                       UNTIL STATUS-INDEX > WS-STATUS-ARRAY-MAX
 
               ADD +1           TO WS-SAVE-NUMBER-OF-ENTRIES
 
           END-IF.
 
       P05300-LOAD-STATUS-ARRAYS-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05330-LOAD-STATUS                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO LOAD THE STATUS BYTE ARRAY RESIDING *
      *                IN WORKING STORAGE                             *
      *                                                               *
      *    CALLED BY:  P05300-LOAD-STATUS-ARRAYS                      *
      *                                                               *
      *****************************************************************
 
       P05330-LOAD-STATUS.
 
      *    **** ACTIVE ITEM ****
           IF VENDOR-ITEM-STATUS-CODE (WS-SUB) = SPACES
               MOVE 'A'    TO WISA-ITEM-STATUS (STATUS-INDEX)
           ELSE
 
      *    **** CANCELLED / DISCONTINUED ITEM ****
           IF VENDOR-ITEM-STATUS-CODE (WS-SUB) = 'C'
               MOVE 'D'    TO WISA-ITEM-STATUS (STATUS-INDEX)
           ELSE
 
      *    **** NO STOCK  / BACK ORDER  ITEM  ****
           IF VENDOR-ITEM-STATUS-CODE (WS-SUB) = 'N'
               MOVE 'B'    TO WISA-ITEM-STATUS (STATUS-INDEX)
           ELSE
 
      *    **** SUSPENDED / INACTIVE ITEM     ****
               MOVE 'I'    TO WISA-ITEM-STATUS (STATUS-INDEX).
 
 
           SET STATUS-INDEX            UP BY 1.
           ADD +1                      TO WS-SUB.
           ADD +1                      TO WS-NUMBER-OF-ENTRIES.
 
 
       P05330-LOAD-STATUS-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05360-LOAD-STATUS                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO LOAD THE STATUS BYTE ARRAY RESIDING *
      *                IN LINKAGE SECTION                             *
      *                                                               *
      *    CALLED BY:  P05300-LOAD-STATUS-ARRAYS                      *
      *                                                               *
      *****************************************************************
 
       P05360-LOAD-STATUS.
 
      *    **** ACTIVE ITEM ****
           IF VENDOR-ITEM-STATUS-CODE (LS-SUB) = SPACES
               MOVE 'A'    TO LISA-ITEM-STATUS (LS-SUB)
           ELSE
 
      *    **** CANCELLED / DISCONTINUED ITEM ****
           IF VENDOR-ITEM-STATUS-CODE (LS-SUB) = 'C'
               MOVE 'D'    TO LISA-ITEM-STATUS (LS-SUB)
           ELSE
 
      *    **** NO STOCK  / BACK ORDER  ITEM  ****
           IF VENDOR-ITEM-STATUS-CODE (LS-SUB) = 'N'
               MOVE 'B'    TO LISA-ITEM-STATUS (LS-SUB)
           ELSE
 
      *    **** SUSPENDED / INACTIVE ITEM     ****
               MOVE 'I'    TO LISA-ITEM-STATUS (LS-SUB).
 
 
       P05360-LOAD-STATUS-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06200-SCROLL-BACK                             *
      *                                                               *
      *    FUNCTION :  PERFORMS FETCH AND FORMATS ITEM LINES          *
      *                                                               *
      *    CALLED BY:  P03200-EDIT-PFKEY                              *
      *                                                               *
      *****************************************************************
 
       P06200-SCROLL-BACK.
 
           PERFORM P78000-CLEAR-SCREEN
              THRU P78000-CLEAR-SCREEN-EXIT
             VARYING WS-SUB1 FROM 1 BY 1
              UNTIL WS-SUB1 > WS-SUB-MAX.
 
           PERFORM P06220-OPEN-BACK-CURSOR
              THRU P06220-OPEN-BACK-CURSOR-EXIT.
 
           IF ERROR-FOUND
             GO TO P06200-SCROLL-BACK-EXIT.
 
           PERFORM P06230-FORMAT-BACK-LINE
              THRU P06230-FORMAT-BACK-LINE-EXIT
             VARYING WS-SUB1 FROM 12 BY -1
             UNTIL END-OF-PROCESS.
 
           IF END-OF-PROCESS
             PERFORM P06290-CLOSE-BACK-CURSOR
                THRU P06290-CLOSE-BACK-CURSOR-EXIT.
 
           IF TOP-OF-DATA
             MOVE 'N'                TO WS-END-OF-PROCESS-SW
             PERFORM P05200-SCROLL-FORWARD
                THRU P05200-SCROLL-FORWARD-EXIT.
 
       P06200-SCROLL-BACK-EXIT.
           EXIT.
           EJECT
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06220-OPEN-BACK-CURSOR                        *
      *                                                               *
      *    FUNCTION :  OPENS CURSOR USED TO CREATE ITEM LIST          *
      *                                                               *
      *    CALLED BY:  P06200-SCROLL-FORWARD                          *
      *                                                               *
      *****************************************************************
 
       P06220-OPEN-BACK-CURSOR.
 
           EXEC SQL
               OPEN ITEMBACK
           END-EXEC.
 
           IF SQLCODE                  = ZEROS
             NEXT SENTENCE
           ELSE
             MOVE 'DB2'                TO WS-PDA-ERROR-TYPE
             MOVE 'PDA006'             TO WPDE-PROGRAM-ID
             MOVE SQLCODE              TO WPDE-DB2-SQLCODE
             MOVE 'OPEN BACKWARD CURS' TO WPDE-FUNCTION
             MOVE 'P06220'             TO WPDE-PARAGRAPH
             PERFORM P99500-PDA-ERROR
                THRU P99500-PDA-ERROR-EXIT.
 
       P06220-OPEN-BACK-CURSOR-EXIT.
           EXIT.
           EJECT
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06230-FORMAT-BACK-LINE                        *
      *                                                               *
      *    FUNCTION :  FORMATS LINE OF LIST                           *
      *                                                               *
      *    CALLED BY:  P06200-SCROLL-FORWARD                          *
      *                                                               *
      *****************************************************************
 
       P06230-FORMAT-BACK-LINE.
 
           IF WS-SUB1                  < +1
             MOVE 'Y'                  TO WS-END-OF-PROCESS-SW
             GO TO P06230-FORMAT-BACK-LINE-EXIT.
 
           PERFORM P06250-FETCH-BACKWARD-ROW
              THRU P06250-FETCH-BACKWARD-ROW-EXIT.
 
           IF ERROR-FOUND OR END-OF-PROCESS
             GO TO P06230-FORMAT-BACK-LINE-EXIT.
 
 
           MOVE ITEM-NUMBER            TO ITEM-OUT (WS-SUB1).
           MOVE ITEM-NAME              TO NAME-OUT (WS-SUB1).
 
 
 
       P06230-FORMAT-BACK-LINE-EXIT.
           EXIT.
           EJECT
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06250-FETCH-BACKWARD-ROW                      *
      *                                                               *
      *    FUNCTION :  FETCH ROW FROM BACKWARD CURSOR                 *
      *                                                               *
      *    CALLED BY:  P06200-SCROLL-BACKWARD                         *
      *                                                               *
      *****************************************************************
 
       P06250-FETCH-BACKWARD-ROW.
 
           EXEC SQL
               FETCH  ITEMBACK
                INTO  :ITEM-NUMBER,
                      :ITEM-NAME
           END-EXEC.
 
           IF SQLCODE                  = ZEROS
             NEXT SENTENCE
           ELSE
             IF SQLCODE                = +100
               MOVE 'Y'                TO WS-END-OF-PROCESS-SW
               MOVE 'Y'                TO WS-TOP-OF-DATA-SW
               MOVE PM014-TOP-MSG      TO PDAMSGO
               IF WS-SUB1              <  +2
                 MOVE SPACES           TO WPW-PREV-LAST-ITEM
               ELSE
                 NEXT SENTENCE
             ELSE
               MOVE 'DB2'              TO WS-PDA-ERROR-TYPE
               MOVE 'PDA006'           TO WPDE-PROGRAM-ID
               MOVE SQLCODE            TO WPDE-DB2-SQLCODE
               MOVE 'FETCH BACK CURS'  TO WPDE-FUNCTION
               MOVE 'P06250'           TO WPDE-PARAGRAPH
               PERFORM P99500-PDA-ERROR
                  THRU P99500-PDA-ERROR-EXIT.
 
       P06250-FETCH-BACKWARD-ROW-EXIT.
           EXIT.
           EJECT
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P06290-CLOSE-FORWARD-CURSOR                    *
      *                                                               *
      *    FUNCTION :  CLOSES CURSOR USED TO CREATE ITEM LIST         *
      *                                                               *
      *    CALLED BY:  P06200-SCROLL-FORWARD                          *
      *                                                               *
      *****************************************************************
 
       P06290-CLOSE-BACK-CURSOR.
 
           EXEC SQL
               CLOSE ITEMBACK
           END-EXEC.
 
           IF SQLCODE                  = ZEROS
             NEXT SENTENCE
           ELSE
             MOVE 'DB2'                TO WS-PDA-ERROR-TYPE
             MOVE 'PDA006'             TO WPDE-PROGRAM-ID
             MOVE SQLCODE              TO WPDE-DB2-SQLCODE
             MOVE 'CLOSE BACK CURSOR'  TO WPDE-FUNCTION
             MOVE 'P06290'             TO WPDE-PARAGRAPH
             PERFORM P99500-PDA-ERROR
                THRU P99500-PDA-ERROR-EXIT.
 
       P06290-CLOSE-BACK-CURSOR-EXIT.
           EXIT.
           EJECT
 
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P70000-ERROR-ROUTINE                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO HANDLE THE SCREEN ERROR MESSAGE     *
      *                PROCESSING                                     *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************
 
       P70000-ERROR-ROUTINE.
 
           MOVE 'Y'                    TO WS-ERROR-FOUND-SW.
 
           IF PDAMSGO                  >  SPACES
               NEXT SENTENCE
           ELSE
               MOVE WMF-MESSAGE-AREA   TO PDAMSGO.
 
       P70000-ERROR-ROUTINE-EXIT.
           EXIT.
           EJECT
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P78000-CLEAR-SCREEN                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CLEAR DETAIL LINES BEFORE POPULATING*
      *                AGAIN                                          *
      *                                                               *
      *    CALLED BY:  P05200-SCROLL-FORWARD                          *
      *                                                               *
      *                                                               *
      *****************************************************************
 
       P78000-CLEAR-SCREEN.
 
      *****************************************************************
      *    MAKE FINAL ADJUSTMENTS TO ATTRIBUTES AND FIELDS            *
      *****************************************************************
 
           MOVE SPACES                 TO SEL-OUT  (WS-SUB1)
                                          ITEM-OUT (WS-SUB1)
                                          NAME-OUT (WS-SUB1).
 
       P78000-CLEAR-SCREEN-EXIT.
           EXIT.
           EJECT
 
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79000-DISPLAY-SCREEN                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CONTROL THE SCREEN DISPLAY          *
      *                PROCESSING                                     *
      *                                                               *
      *    CALLED BY:  P01000-1ST-TIME-PROCESS                        *
      *                P02000-PROCESS-TRANS                           *
      *                                                               *
      *****************************************************************
 
       P79000-DISPLAY-SCREEN.
 
      *****************************************************************
      *    SET CURSOR AND SAVE FIRST ITEM NUMBER IN COMM AREA         *
      *****************************************************************
 
LXR415     IF NO-ERROR-FOUND
LXR415       MOVE -1                   TO SEL1L.
LXR415     MOVE ITEM1I                 TO WPW-PREV-FIRST-ITEM.
LXR415     MOVE ITEM12I                TO WPW-PREV-LAST-ITEM.
 
      *****************************************************************
      *    MAKE FINAL ADJUSTMENTS TO ATTRIBUTES AND FIELDS            *
      *****************************************************************
 
           PERFORM  P79100-SET-MAP-FIELDS
               THRU P79100-SET-MAP-FIELDS-EXIT
                   VARYING WS-SUB1 FROM 1 BY 1
                       UNTIL WS-SUB1 > WS-SUB-MAX.
 
 
      *****************************************************************
      *    SEND FULL MAP IF 1ST TIME, OTHERWISE SEND DATAONLY         *
      *****************************************************************
 
           IF PC-PREV-PGRMID  =  'PDA006'
               PERFORM  P80100-SEND-MAP-DATAONLY
                   THRU P80100-SEND-MAP-DATAONLY-EXIT
           ELSE
               PERFORM  P80000-SEND-FULL-MAP
                   THRU P80000-SEND-FULL-MAP-EXIT.
 
 
       P79000-DISPLAY-SCREEN-EXIT.
           EXIT.
           EJECT
 
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P79100-SET-MAP-FIELDS                          *
      *                                                               *
      *    FUNCTION :  ROUTINE TO SET CERTAIN MAP FIELDS AND          *
      *                ATTRIBUTES BASED ON DATA PRESENT IN THE MAP    *
      *                                                               *
      *    CALLED BY:  P79000-DISPLAY-SCREEN                          *
      *                                                               *
      *****************************************************************
 
       P79100-SET-MAP-FIELDS.
 
      *****************************************************************
      *    IF ITEM PRESENT, MAKE SELECTION CODE ENTERABLE AND         *
      *    INITIALIZE TO UNDERSCORE IF SELECTION CODE NOT PRESENT,    *
      *    OTHERWISE PROTECT SELECTION CODE AND SET VALUE TO SPACES   *
      *                                                               *
      *****************************************************************
 
           IF ITEM-OUT (WS-SUB1)       >  SPACES
             MOVE DFHBMFSE             TO SEL-ATTR (WS-SUB1)
             INSPECT SEL-OUT (WS-SUB1)
                   CONVERTING  WMF-SPACES-LOWVALUE-R TO '__'
           ELSE
             MOVE DFHBMASF             TO SEL-ATTR (WS-SUB1)
             MOVE SPACES               TO SEL-OUT  (WS-SUB1).
 
       P79100-SET-MAP-FIELDS-EXIT.
           EXIT.
           EJECT
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80000-SEND-FULL-MAP                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DISPLAY THE INITIAL MAIN MENU       *
      *                                                               *
      *    CALLED BY:  P01000-FIRST-TIME                              *
      *                                                               *
      *****************************************************************
 
       P80000-SEND-FULL-MAP.
 
           EXEC CICS SEND
                     MAP           ('PDA006')
                     MAPSET        ('PDA006M')
                     FROM          (PDA006O)
                     ERASE
                     FREEKB
                     CURSOR
                     NOHANDLE
                     RESP          (WS-RESPONSE-CODE)
           END-EXEC.
 
 
      *****************************************************************
      *    IF ERROR, FORMAT ERROR INFORMATION AND TERMINATE           *
      *****************************************************************
 
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               NEXT SENTENCE
           ELSE
               MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
               MOVE 'PDA006'           TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
               MOVE 'CICS SEND MAP'    TO WPCE-COMMAND
               MOVE 'P80000'           TO WPCE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.
 
 
       P80000-SEND-FULL-MAP-EXIT.
           EXIT.
           EJECT
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80100-SEND-MAP-DATAONLY                       *
      *                                                               *
      *    FUNCTION :  ROUTINE TO DISPLAY THE MAIN MENU SENDING DATA  *
      *                ONLY (NO LITERALS)                             *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************
 
       P80100-SEND-MAP-DATAONLY.
 
      *****************************************************************
      *    RESET ENTERABLE FIELDS BACK TO DEFAULT (IF NECESSARY)      *
      *****************************************************************
 
      *    INSPECT PDACUSTI
      *        CONVERTING  WMF-SPACES-LOWVALUE-R TO '__'.
 
 
      *****************************************************************
      *    SEND THE MAP DATA ONLY, DO NOT ERASE SCREEN                *
      *****************************************************************
 
           EXEC CICS SEND
                     MAP           ('PDA006')
                     MAPSET        ('PDA006M')
                     FROM          (PDA006O)
                     DATAONLY
                     FREEKB
                     CURSOR
                     NOHANDLE
                     RESP          (WS-RESPONSE-CODE)
           END-EXEC.
 
 
      *****************************************************************
      *    IF ERROR, FORMAT ERROR INFORMATION AND TERMINATE           *
      *****************************************************************
 
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               NEXT SENTENCE
           ELSE
               MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
               MOVE 'PDA006'           TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
               MOVE 'CICS SEND MAP'    TO WPCE-COMMAND
               MOVE 'P80100'           TO WPCE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.
 
 
       P80100-SEND-MAP-DATAONLY-EXIT.
           EXIT.
           EJECT
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80200-RECEIVE-MAP                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RECEIVE / FORMAT THE INPUT MAP DATA *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************
 
       P80200-RECEIVE-MAP.
 
           EXEC CICS RECEIVE
                     MAP           ('PDA006')
                     MAPSET        ('PDA006M')
                     INTO          (PDA006I)
                     NOHANDLE
                     RESP          (WS-RESPONSE-CODE)
           END-EXEC.
 
 
      *****************************************************************
      *    IF ERROR, FORMAT ERROR INFORMATION AND TERMINATE           *
      *****************************************************************
 
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)        OR
              WS-RESPONSE-CODE = DFHRESP(MAPFAIL)
               NEXT SENTENCE
           ELSE
               MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
               MOVE 'PDA006'           TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
               MOVE 'CICS RECEIVE MAP' TO WPCE-COMMAND
               MOVE 'P80200'           TO WPCE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.
 
 
       P80200-RECEIVE-MAP-EXIT.
           EXIT.
           EJECT
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80300-XFER-CONTROL                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO TRANSFER CONTROL TO THE             *
      *                APPROPRIATE CICS FUNCTION BASED ON THE MENU    *
      *                SELECTION ENTERED                              *
      *                                                               *
      *    CALLED BY:  P03000-EDIT-PROCESS                            *
      *                                                               *
      *****************************************************************
 
       P80300-XFER-CONTROL.
 
           EXEC CICS XCTL
                     PROGRAM       (PC-NEXT-PGRMID)
                     COMMAREA      (PDA-COMMAREA)
                     LENGTH        (PC-COMMAREA-LTH)
                     NOHANDLE
                     RESP          (WS-RESPONSE-CODE)
           END-EXEC.
 
 
      *****************************************************************
      *    IF ERROR, FORMAT ERROR INFORMATION AND TERMINATE           *
      *****************************************************************
 
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               NEXT SENTENCE
           ELSE
               MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
               MOVE 'PDA006'           TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
               MOVE 'CICS XCTL --- '   TO WPCE-COMMAND-1
               MOVE PC-NEXT-PGRMID     TO WPCE-COMMAND-2
               MOVE 'P80300'           TO WPCE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.
 
 
       P80300-XFER-CONTROL-EXIT.
           EXIT.
           EJECT
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P80400-SEND-MESSAGE                            *
      *                                                               *
      *    FUNCTION :  ROUTINE TO SEND A ONE LINE MESSAGE TO THE      *
      *                TERMINAL                                       *
      *                                                               *
      *    CALLED BY:  P03200-EDIT-PFKEY                              *
      *                                                               *
      *****************************************************************
 
       P80400-SEND-MESSAGE.
 
      *****************************************************************
      *    SEND THE MESSAGE LINE, IF ERROR FORMAT ERROR AND TERMINATE *
      *****************************************************************
 
           EXEC CICS SEND
                     FROM          (WMF-MESSAGE-AREA)
                     LENGTH        (WS-MESSAGE-LTH)
                     ERASE
                     NOHANDLE
                     RESP          (WS-RESPONSE-CODE)
           END-EXEC.
 
 
 
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               NEXT SENTENCE
           ELSE
               MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
               MOVE 'PDA006'           TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
               MOVE 'CICS SEND'        TO WPCE-COMMAND
               MOVE 'P80400'           TO WPCE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.
 
 
      *****************************************************************
      *    CURSOR AT FIRST POSITION ON SCREEN, IF ERROR TERMINATE     *
      *****************************************************************
 
           EXEC CICS SEND
                     CONTROL
                     CURSOR        (0)
                     NOHANDLE
                     RESP          (WS-RESPONSE-CODE)
           END-EXEC.
 
 
 
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               NEXT SENTENCE
           ELSE
               MOVE 'CICS'             TO WS-PDA-ERROR-TYPE
               MOVE 'PDA006'           TO WPCE-PROGRAM-ID
               MOVE WS-RESPONSE-CODE   TO WPCE-RESPONSE-CODE
               MOVE 'CICS SEND CONTROL'
                                       TO WPCE-COMMAND
               MOVE 'P80400'           TO WPCE-PARAGRAPH
               PERFORM  P99500-PDA-ERROR
                   THRU P99500-PDA-ERROR-EXIT.
 
 
      *****************************************************************
      *    RETURN TO CICS (NO TRANSID OPTION)                         *
      *****************************************************************
 
           EXEC CICS RETURN
           END-EXEC.
 
           GOBACK.
 
       P80400-SEND-MESSAGE-EXIT.
           EXIT.
           EJECT
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P99100-GENERAL-ERROR                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CATCH ANY CICS ERROR(S) NOT         *
      *                SPECIFICALLY PROCESSED BY A CICS HANDLE        *
      *                CONDITION                                      *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************
 
       P99100-GENERAL-ERROR.
 
 
           MOVE 'CICS'                 TO WS-PDA-ERROR-TYPE.
           MOVE 'PDA006'               TO WPCE-PROGRAM-ID.
           MOVE EIBRESP                TO WPCE-RESPONSE-CODE.
           MOVE 'UNHANDLED CICS ERROR' TO WPCE-COMMAND.
           MOVE 'P99100'               TO WPCE-PARAGRAPH.
           PERFORM  P99500-PDA-ERROR
               THRU P99500-PDA-ERROR-EXIT.
 
 
       P99100-GENERAL-ERROR-EXIT.
           EXIT.
           EJECT
 
 
      *****************************************************************
      *                                                               *
      *    P R O D U C T    D E M O N S T R A T I O N     A P P L     *
      *                                                               *
      *             E R R O R    R O U T I N E S                      *
      *                                                               *
      *                                                               *
      *****************************************************************
 
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P99500-PDA-ERROR                               *
      *                                                               *
      *    FUNCTION :  ROUTINE TO HANDLE FATAL / TERMINATING CICS,    *
      *                DB2, IMS-DLI ERRORS                            *
      *                                                               *
      *                AN ERROR SCREEN CONTAINING TEXT IS SENT        *
      *                TO THE USER INDICATING THE NATURE OF THE ERROR *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************
 
       P99500-PDA-ERROR.
 
      *****************************************************************
      *      SUSPEND ANY HANDLE CONDITIONS IN EFFECT                  *
      *****************************************************************
 
           EXEC CICS PUSH HANDLE
           END-EXEC.
 
 
      *****************************************************************
      *      ROLLBACK ANY TRANSACTION UPDATES                         *
      *****************************************************************
 
           EXEC CICS SYNCPOINT ROLLBACK
           END-EXEC.
 
 
      *****************************************************************
      *      FORMAT AND SEND ERROR TEXT                               *
      *****************************************************************
 
           IF PDA-DB2-ERROR
               MOVE WS-PDA-DB2-ERROR-01
                                       TO WPEA-ERROR-07-TEXT
               MOVE WS-PDA-DB2-ERROR-02
                                       TO WPEA-ERROR-08-TEXT
           ELSE
           IF PDA-IMS-ERROR
               MOVE WS-PDA-IMS-ERROR-01
                                       TO WPEA-ERROR-07-TEXT
               MOVE WS-PDA-IMS-ERROR-02
                                       TO WPEA-ERROR-08-TEXT
           ELSE
               MOVE WS-PDA-CICS-ERROR-01
                                       TO WPEA-ERROR-07-TEXT
               MOVE WS-PDA-CICS-ERROR-02
                                       TO WPEA-ERROR-08-TEXT.
 
 
           EXEC CICS DUMP
                     TRANSACTION
                     DUMPCODE('PDER')
           END-EXEC.
 
 
 
           EXEC CICS SEND
                     FROM    (WS-PDA-ERROR-AREA)
                     LENGTH  (WS-PDA-ERROR-LENGTH)
                     ERASE
           END-EXEC.
 
 
 
           EXEC CICS SEND
                     CONTROL
                     CURSOR  (0)
           END-EXEC.
 
 
      *****************************************************************
      * RETURN CONTROL TO CICS                                        *
      *****************************************************************
 
           EXEC CICS RETURN
           END-EXEC.
 
 
           GOBACK.
 
       P99500-PDA-ERROR-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P99999-ERROR                                   *
      *                                                               *
      *    FUNCTION :  ROUTINE TO CATCH ANY ERROR(S) NOT              *
      *                SPECIFICALLY PROCESSED BY A CICS HANDLE        *
      *                CONDITION                                      *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************
 
RTN    P99999-ERROR.                                                     RTN
NOT                                                                      NOT
USED       MOVE 'CICS'                 TO WS-PDA-ERROR-TYPE.             USED
AS OF      MOVE 'PDA006'               TO WPCE-PROGRAM-ID.               AS OF
JAN        MOVE EIBRESP                TO WPCE-RESPONSE-CODE.            JAN
2001       MOVE 'ERROR'                TO WPCE-COMMAND.                  2001
           MOVE 'P99999'               TO WPCE-PARAGRAPH.
LLR                                                                      LLR
           PERFORM  P99500-PDA-ERROR
               THRU P99500-PDA-ERROR-EXIT.
 
       P99999-ERROR-EXIT.
           EXIT.
           EJECT
