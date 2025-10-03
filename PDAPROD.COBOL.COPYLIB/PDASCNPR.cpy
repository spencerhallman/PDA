      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P91010-SEARCH-SCENARIO-LIST                    *
      *                                                               *
      *    FUNCTION :  ROUTINE TO SEARCH THRU THE SCENARIO LIST       *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************

       P91010-SEARCH-SCENARIO-LIST.

           MOVE 'N' TO PDA-SWA-FOUND.
           MOVE SPACES TO PDA-SWA-PROGRAM
                          PDA-SWA-NAME.

           PERFORM VARYING PDA-SWA-SUB FROM 1 BY 1
               UNTIL PDA-SWA-SUB > PDA-SWA-MAX-ENTRIES
                   IF PDA-SWA-SCENARIO =
                                       PDA-SWA-SL-SCENARIO(PDA-SWA-SUB)
                       MOVE 'Y' TO PDA-SWA-FOUND
                       MOVE PDA-SWA-SL-NAME(PDA-SWA-SUB) TO PDA-SWA-NAME
                       MOVE PDA-SWA-SL-PROGRAM(PDA-SWA-SUB) TO
                           PDA-SWA-PROGRAM
                       MOVE PDA-SWA-MAX-ENTRIES TO PDA-SWA-SUB
                   END-IF
                   IF PDA-SWA-SL-SCENARIO(PDA-SWA-SUB) = SPACES
                       MOVE PDA-SWA-MAX-ENTRIES TO PDA-SWA-SUB
                   END-IF
           END-PERFORM.

       P91010-EXIT.
           EXIT.
           EJECT