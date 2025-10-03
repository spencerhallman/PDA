                                                                        00631700
                                                                        00631800
      ***************************************************************** 00631900
      *                                                               * 00632000
      *    P R O D U C T    D E M O N S T R A T I O N     A P P L     * 00632100
      *                                                               * 00632200
      *             E R R O R    R O U T I N E S                      * 00632300
      *                                                               * 00632400
      *                                                               * 00632500
      ***************************************************************** 00632600
                                                                        00632700
      ***************************************************************** 00632800
      *                                                               * 00632900
      *    PARAGRAPH:  P99500-PDA-ERROR                               * 00633000
      *                                                               * 00633100
      *    FUNCTION :  ROUTINE TO HANDLE FATAL / TERMINATING CICS,    * 00633200
      *                DB2, IMS-DLI, MQSERIES ERRORS                  * 00633300
      *                                                               * 00633400
      *                AN ERROR SCREEN CONTAINING TEXT IS SENT        * 00633500
      *                TO THE USER INDICATING THE NATURE OF THE ERROR * 00633600
      *                                                               * 00633700
      *    CALLED BY:  GLOBAL                                         * 00633800
      *                                                               * 00633900
      ***************************************************************** 00634000
                                                                        00634100
       P99500-PDA-ERROR.                                                00634200
                                                                        00634300
      ***************************************************************** 00634400
      *      SUSPEND ANY HANDLE CONDITIONS IN EFFECT                  * 00634500
      ***************************************************************** 00634600
                                                                        00634700
           EXEC CICS PUSH HANDLE                                        00634800
           END-EXEC.                                                    00634900
                                                                        00635000
                                                                        00635100
      ***************************************************************** 00635200
      *      ROLLBACK ANY TRANSACTION UPDATES                         * 00635300
      ***************************************************************** 00635400
                                                                        00635500
           EXEC CICS SYNCPOINT ROLLBACK                                 00635600
           END-EXEC.                                                    00635700
                                                                        00635800
                                                                        00635900
      ***************************************************************** 00636000
      *      FORMAT AND SEND ERROR TEXT                               * 00636100
      ***************************************************************** 00636200
                                                                        00636300
           IF PDA-DB2-ERROR                                             00636400
               MOVE WS-PDA-DB2-ERROR-01                                 00636500
                                       TO WPEA-ERROR-07-TEXT            00636600
               MOVE WS-PDA-DB2-ERROR-02                                 00636700
                                       TO WPEA-ERROR-08-TEXT            00636800
           ELSE                                                         00636900
           IF PDA-IMS-ERROR                                             00637000
               MOVE WS-PDA-IMS-ERROR-01                                 00637100
                                       TO WPEA-ERROR-07-TEXT            00637200
               MOVE WS-PDA-IMS-ERROR-02                                 00637300
                                       TO WPEA-ERROR-08-TEXT            00637400
           ELSE                                                         00637500
           IF PDA-MQSERIES-ERROR                                        00637600
               MOVE WS-PDA-MQSERIES-ERROR-01                            00637700
                                       TO WPEA-ERROR-07-TEXT            00637800
               MOVE WS-PDA-MQSERIES-ERROR-02                            00637900
                                       TO WPEA-ERROR-08-TEXT            00638000
           ELSE                                                         00638100
               MOVE WS-PDA-CICS-ERROR-01                                00638200
                                       TO WPEA-ERROR-07-TEXT            00638300
               MOVE WS-PDA-CICS-ERROR-02                                00638400
                                       TO WPEA-ERROR-08-TEXT.           00638500
                                                                        00638600
                                                                        00638700
                                                                        00638800
           EXEC CICS DUMP                                               00638900
                     TRANSACTION                                        00639000
                     DUMPCODE('PDER')                                   00639100
           END-EXEC.                                                    00639200
                                                                        00639300
                                                                        00639400
           EXEC CICS SEND                                               00639500
                     FROM    (WS-PDA-ERROR-AREA)                        00639600
                     LENGTH  (WS-PDA-ERROR-LENGTH)                      00639700
                     ERASE                                              00639800
           END-EXEC.                                                    00639900
                                                                        00640000
                                                                        00640100
                                                                        00640200
           EXEC CICS SEND                                               00640300
                     CONTROL                                            00640400
                     CURSOR  (0)                                        00640500
           END-EXEC.                                                    00640600
                                                                        00640700
                                                                        00640800
      ***************************************************************** 00640900
      * RETURN CONTROL TO CICS                                        * 00641000
      ***************************************************************** 00641100
                                                                        00641200
           EXEC CICS RETURN                                             00641300
           END-EXEC.                                                    00641400
                                                                        00641500
                                                                        00641600
           GOBACK.                                                      00641700
                                                                        00641800
       P99500-PDA-ERROR-EXIT.                                           00641900
           EXIT.                                                        00642000
           EJECT                                                        00650000