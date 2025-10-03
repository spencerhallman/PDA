       IDENTIFICATION DIVISION.
       PROGRAM-ID. PDA015.
      *
      *****************************************************************
      *                 PRODUCT DEMONSTRATION APPLICATION (PDA)       *
      *                       COMPUWARE CORPORATION                   *
      *                                                               *
      * PROGRAM :   PDA015                                            *
      * TRANS   :   PD15                                              *
      * MAPSET  :   N/A                                               *
      *                                                               *
      * FUNCTION:   PROGRAM PDA015 IS THE PROGRAM THAT WILL PROVIDE   *
      *             A LIST OF SUBMITTED ORDERS FOR THE USER. THE USER *
      *             IS THE CICS MIRROR PROGRAM. THE CALLER  WILL      *
      *             PROVIDE THE STARTING ORDER NUMBER. THERE          *
      *             IS NO SCREEN I/O SINCE IT IS BEING USED BY        *
      *             UNIFACE & XPEDITER/DEVENTERPRISE TO DEMONSTRATE   *
      *             LEGACY RENEWAL.                                   *
      *                                                               *
      * FILES   :   ORDER_DATABASE     -  IMS-DLI    (READ-ONLY)      *
      *             CUSTOMER_FILE      -  VSAM KSDS  (UPDATE)         *
      *             USERID             -  DB2        (READ-ONLY)      *
      *                                                               *
      * TRANSACTIONS GENERATED: N/A                                   *
      *                                                               *
      * PFKEYS  :   N/A                                               *
      *                                                               *
      *****************************************************************
      *             PROGRAM CHANGE LOG                                *
      *             -------------------                               *
      *                                                               *
      *  DATE       UPDATED BY            CHANGE DESCRIPTION          *
      *  --------   --------------------  --------------------------  *
      *  XX/XX/XX   XXXXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXX  *
      *                                                               *
      *                                                               *
      *****************************************************************

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EJECT

       01  WS-MISCELLANEOUS-FIELDS.

           05  WS-SUB1                 PIC S9(4) COMP.
           05  WS-RESPONSE-CODE        PIC S9(8) COMP VALUE +0.
           05  WS-SUB-MAX              PIC S9(3) COMP-3 VALUE +15.
           05  WS-END-OF-PROCESS-SW    PIC X       VALUE 'N'.
                   88  END-OF-PROCESS  VALUE 'Y'.
           05  WS-ERROR-FOUND-SW       PIC X.
                   88  ERROR-FOUND     VALUE 'Y'.
           05  WMF-USERID              PIC X(08)   VALUE SPACES.
           05  WMF-USERID-NUMBER       PIC S9(09)  VALUE +0  COMP.
           05  WMF-PSB-NAME            PIC X(08)   VALUE 'PDA015'.
           05  WMF-MESSAGE-AREA        PIC X(79)   VALUE SPACES.

           05  WMF-ORDER-KEY           PIC X(15)   VALUE SPACES.
           05  WMF-ORDER-KEY-R         REDEFINES   WMF-ORDER-KEY.
               10  WMF-ORDER-PREFIX    PIC 9(05).
               10  WMF-ORDER-NUMBER    PIC X(10).

           05  WMF-CUSTOMER-KEY.
               10  WMF-CUSTOMER-PREFIX PIC 9(05)   VALUE ZEROES.
               10  WMF-CUSTOMER-ID     PIC X(32)   VALUE SPACES.

      *****************************************************************
      *    SQL COMMUNICATIONS AREA                                    *
      *****************************************************************

           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.

           EJECT

      *****************************************************************
      *    USER IDENTIFICATION TABLE (USERID)         DCLGEN DUSERID  *
      *****************************************************************

           EXEC SQL
                INCLUDE DUSERID
           END-EXEC.

           EJECT

      *****************************************************************
      *    P R O G R A M     W O R K A R E A                          *
      *****************************************************************

       01  WS-PDA-COMMAREA-WORKAREA.

           05  WPCW-IN-INFO.
             10  WPCW-ORDER-IN         PIC X(10).
             10  FILLER                PIC X(1990).

           05  WPCW-OUT-INFO   REDEFINES WPCW-IN-INFO.
            07  FILLER                         OCCURS 15 TIMES.
             10  WPCW-ORDER-NUMBER     PIC X(10).
             10  WPCW-ORDER-DATE-YYMMDD.
               15  WPCW-ORDER-DATE-YY  PIC X(02).
               15  WPCW-ORDER-DATE-MM  PIC X(02).
               15  WPCW-ORDER-DATE-DD  PIC X(02).
             10  WPCW-ORDER-STATUS     PIC X(32).
             10  WPCW-ORDER-TOTAL-AMOUNT
                                       PIC S9(07)V99.
             10  WPCW-ORDER-CUSTOMER-NAME
                                       PIC X(64).

            07  WPCW-ERROR-INFORMATION.
               10  WPCW-ERROR-SW       PIC X(01).
                   88 WPCW-NO-ERROR                VALUE SPACES.
                   88 WPCW-ORDER-NOT-FOUND         VALUE '1'.
                   88 WPCW-CUSTOMER-NOT-FOUND      VALUE '2'.
                   88 WPCW-USERID-NOT-FOUND        VALUE '3'.
                   88 WPCW-DB2-ERROR               VALUE '7'.
                   88 WPCW-CICS-ERROR              VALUE '8'.
                   88 WPCW-IMS-ERROR               VALUE '9'.

               10  WPCW-PROGRAM-ID     PIC X(08).
               10  WPCW-PARAGRAPH      PIC X(06).
               10  WPCW-COMMAND        PIC X(30).

            07    WPCW-DB2-INFO.
               10  WPCW-SQLCODE        PIC S9(08)  COMP.

            07    WPCW-CICS-INFO.
               10  WPCW-RESPONSE-CODE  PIC S9(08)  COMP.

            07    WPCW-IMS-INFO.
               10  WPCW-FUNCTION       PIC X(04).
               10  WPCW-SEGMENT        PIC X(08).
               10  WPCW-DATABASE       PIC X(08).
               10  WPCW-STATUS-CODE    PIC X(02).

            07  FILLER                PIC X(110).

      *****************************************************************
      *    ORDER DATABASE ROOT SEGMENT                                *
      *****************************************************************
           COPY IORDER.
           EJECT

      *****************************************************************
      *    CUSTOMER VSAM FILE                                         *
      *****************************************************************
           COPY VCUSTOMR.
           EJECT

      *****************************************************************
      *    L I N K A G E     S E C T I O N                            *
      *****************************************************************

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           05 PC-PROGRAM-WORKAREA           PIC X(2000).
      *    COPY PDACOMM.
           EJECT

      *****************************************************************
      *    P R O C E D U R E    D I V I S I O N                       *
      *****************************************************************

       PROCEDURE DIVISION USING DFHCOMMAREA.

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00000-MAINLINE                                *
      *                                                               *
      *    FUNCTION :  PROGRAM ENTRY, CONTROL HIGH LEVEL PROCESSING   *
      *                FOR THE PRODUCT DEMONSTRATION APPLICATION      *
      *                ORDER INFORMATION RETRIEVAL PROGRAM            *
      *                                                               *
      *    CALLED BY:  NONE                                           *
      *                                                               *
      *****************************************************************

       P00000-MAINLINE.


           EXEC CICS HANDLE CONDITION
                ERROR(P99100-GENERAL-ERROR)
           END-EXEC.


           PERFORM  P00050-INITIALIZE                                   TAGGED
               THRU P00050-INITIALIZE-EXIT.                             CODE
                                                                        TESTING
                                                                        03/13/01
           PERFORM  P00100-MAIN-PROCESS
               THRU P00100-MAIN-PROCESS-EXIT.


           PERFORM  P99500-CICS-RETURN
               THRU P99500-CICS-RETURN-EXIT.

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
      *    VERIFY COMMAREA IS PASSED AND CORRECT                      *
      *****************************************************************

           IF EIBCALEN                 NOT = 2000
               MOVE '8'                TO WPCW-ERROR-SW
               MOVE 'PDA015'           TO WPCW-PROGRAM-ID
               MOVE 'P00050'           TO WPCW-PARAGRAPH
               MOVE ZEROES             TO WPCW-RESPONSE-CODE
               MOVE 'COMMAREA LENGTH NOT CORRECT'
                                       TO WPCW-COMMAND
               PERFORM  P99600-COMMAREA-ERROR
                   THRU P99600-COMMAREA-ERROR-EXIT.


      *****************************************************************
      *    COMMAREA PROGRAM WORK AREA CONTAINS ALL INFO WE NEED       *
      *****************************************************************

           MOVE PC-PROGRAM-WORKAREA    TO WS-PDA-COMMAREA-WORKAREA.
           MOVE 'N'                    TO WS-ERROR-FOUND-SW.
                                                                        00010000
           IF WPCW-ORDER-IN    NOT NUMERIC
             MOVE ZEROS                TO WPCW-ORDER-IN.

           MOVE WPCW-ORDER-IN          TO WMF-ORDER-NUMBER.

           PERFORM VARYING WS-SUB1 FROM 1 BY 1
             UNTIL WS-SUB1 > WS-SUB-MAX
            MOVE ZEROS                 TO WPCW-ORDER-NUMBER (WS-SUB1)
           END-PERFORM.

       P00050-INITIALIZE-EXIT.
           EXIT.
           EJECT

       P00100-MAIN-PROCESS.

      *****************************************************************
      *    VERIFY USERID, OBTAIN USERID UNIQUE IDENTIFIER FOR         *
      *    FILE READS  --- IF ERROR, EXIT THE PROCESS                 *
      *****************************************************************

           PERFORM  P00200-VERIFY-USERID
               THRU P00200-VERIFY-USERID-EXIT.

           IF ERROR-FOUND
               GO TO P00100-MAIN-PROCESS-EXIT.

      *****************************************************************
      *    SCHEDULE THE PSB TO BE USED (PDA015)                       *
      *****************************************************************

           PERFORM  P05100-SCHEDULE-PSB
               THRU P05100-SCHEDULE-PSB-EXIT.

           IF ERROR-FOUND
               GO TO P00100-MAIN-PROCESS-EXIT.

      *****************************************************************
      *    PROCESS THE ORDER DATABASE FOR THE USER  NUMBER SUPPLIED   *
      *    IN THE COMMAREA                                            *
      *****************************************************************

           PERFORM  P05200-FORMAT-LINE
               THRU P05200-FORMAT-LINE-EXIT
             VARYING WS-SUB1       FROM 1 BY 1
             UNTIL END-OF-PROCESS.

      *****************************************************************
      *    TERMINATE THE PSB                                          *
      *****************************************************************

           PERFORM  P05300-TERMINATE-PSB
               THRU P05300-TERMINATE-PSB-EXIT.


       P00100-MAIN-PROCESS-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P00200-VERIFY-USERID                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO PERFORM USERID VERIFICATION:        *
      *                                                               *
      *                1) OBTAIN CICS USER ID (SIGNON VALUE)          *
      *                                                               *
      *                2) RETRIEVE USER FROM USERID TABLE (DB2)       *
      *                                                               *
      *                                                               *
      *    CALLED BY:  P00100-MAIN-PROCESS                            *
      *                                                               *
      *****************************************************************

       P00200-VERIFY-USERID.

      *****************************************************************
      *    OBTAIN THE USERID (CICS SIGN ON ID)                        *
      *****************************************************************

           EXEC CICS ASSIGN
                     USERID        (WMF-USERID)
                     NOHANDLE
                     RESP          (WS-RESPONSE-CODE)
           END-EXEC.


           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               NEXT SENTENCE
           ELSE
               MOVE 'Y'                TO WS-ERROR-FOUND-SW
               MOVE '8'                TO WPCW-ERROR-SW
               MOVE 'PDA015'           TO WPCW-PROGRAM-ID
               MOVE 'P00200'           TO WPCW-PARAGRAPH
               MOVE WS-RESPONSE-CODE   TO WPCW-RESPONSE-CODE
               MOVE 'ASSIGN USERID ERROR'
                                       TO WPCW-COMMAND
               GO TO P00200-VERIFY-USERID-EXIT.


      *****************************************************************
      *    RETRIEVE USERID UNIQUE IDENTIFIER FROM DB2 TABLE (USERID)  *
      *****************************************************************

           EXEC SQL SELECT    ID,
                              NUMBER

                    INTO      :USERID-ID,
                              :USERID-NUMBER

                    FROM      USERID

                    WHERE     ID = :WMF-USERID
           END-EXEC.


           IF SQLCODE                  =  ZEROES
               MOVE USERID-NUMBER      TO WMF-ORDER-PREFIX
           ELSE
           IF SQLCODE                  =  +100
               MOVE 'Y'                TO WS-ERROR-FOUND-SW
               MOVE '3'                TO WPCW-ERROR-SW
               GO TO P00200-VERIFY-USERID-EXIT
           ELSE
               MOVE 'Y'                TO WS-ERROR-FOUND-SW
               MOVE '7'                TO WPCW-ERROR-SW
               MOVE 'PDA015'           TO WPCW-PROGRAM-ID
               MOVE 'P00200'           TO WPCW-PARAGRAPH
               MOVE SQLCODE            TO WPCW-SQLCODE
               MOVE 'SELECT USERID-ID FROM USERID'
                                       TO WPCW-COMMAND
               GO TO P00200-VERIFY-USERID-EXIT.

       P00200-VERIFY-USERID-EXIT.
           EXIT.
           EJECT

       P05100-SCHEDULE-PSB.

           EXEC DLI
               SCHEDULE
                   PSB((WMF-PSB-NAME))
                   NODHABEND
           END-EXEC.

      *****************************************************************
      *    CHECK FOR PSB SCHEDULING ERROR                             *
      *****************************************************************

           IF DIBSTAT NOT = SPACES
               MOVE 'Y'                TO WS-ERROR-FOUND-SW
               MOVE '9'                TO WPCW-ERROR-SW
               MOVE 'PDA015'           TO WPCW-PROGRAM-ID
               MOVE 'P05100'           TO WPCW-PARAGRAPH
               MOVE 'SCHD'             TO WPCW-FUNCTION
               MOVE SPACES             TO WPCW-SEGMENT
               MOVE SPACES             TO WPCW-DATABASE
               MOVE DIBSTAT            TO WPCW-STATUS-CODE
               MOVE 'PSB SCHEDULING ERROR'
                                       TO WPCW-COMMAND
               GO TO P05100-SCHEDULE-PSB-EXIT
           END-IF.

       P05100-SCHEDULE-PSB-EXIT.
           EXIT.

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05200-FORMAT-LINE                             *
      *                                                               *
      *    FUNCTION :  FORMATS LINE OF LIST                           *
      *                                                               *
      *    CALLED BY:  P05000-BUILD-SCREEN                            *
      *                                                               *
      *****************************************************************
       P05200-FORMAT-LINE.

           IF WS-SUB1              > WS-SUB-MAX
             MOVE 'Y'              TO WS-END-OF-PROCESS-SW
             GO TO P05200-FORMAT-LINE-EXIT
           END-IF.

           PERFORM P05210-GU-ORDER-DATABASE
              THRU P05210-GU-ORDER-DATABASE-EXIT.

            IF ORDER-PREFIX    NOT = WMF-ORDER-PREFIX OR
               DIBSTAT             = 'GE'
              MOVE 'Y'             TO WS-END-OF-PROCESS-SW
            END-IF.

           IF ERROR-FOUND OR END-OF-PROCESS
             GO TO P05200-FORMAT-LINE-EXIT
           END-IF.

           MOVE ORDER-NUMBER       TO WPCW-ORDER-NUMBER (WS-SUB1)
                                      WMF-ORDER-NUMBER.
           MOVE ORDER-DATE-YYMMDD  TO WPCW-ORDER-DATE-YYMMDD (WS-SUB1).
           MOVE ORDER-STATUS       TO WPCW-ORDER-STATUS (WS-SUB1).
           MOVE ORDER-TOTAL-AMOUNT TO
                WPCW-ORDER-TOTAL-AMOUNT (WS-SUB1).

           MOVE ORDER-CUSTOMER-KEY TO WMF-CUSTOMER-KEY.

           PERFORM P05220-READ-CUSTOMER
              THRU P05220-READ-CUSTOMER-EXIT.

           IF ERROR-FOUND
             GO TO P05200-FORMAT-LINE-EXIT.

           MOVE CUSTOMER-NAME      TO
                WPCW-ORDER-CUSTOMER-NAME (WS-SUB1).

       P05200-FORMAT-LINE-EXIT.
           EXIT.
       EJECT

       P05210-GU-ORDER-DATABASE.

           EXEC DLI GU USING
                    PCB         (1)
                    SEGMENT     (ORDER)
                    INTO        (ORDER-SEGMENT)
                    SEGLENGTH   (123)
                    WHERE       (ORDKEY>WMF-ORDER-KEY)
                    FIELDLENGTH (15)
           END-EXEC.


      *****************************************************************
      *    CHECK STATUS CODE FOR SUCCESS, NOT FOUND, ALL OTHERS ARE   *
      *    AN ERROR                                                   *
      *****************************************************************

           IF DIBSTAT    =  SPACES
               NEXT SENTENCE
           ELSE

           IF DIBSTAT    =  'GE'
               MOVE 'Y'                TO WS-ERROR-FOUND-SW
               MOVE '1'                TO WPCW-ERROR-SW
               GO TO P05210-GU-ORDER-DATABASE-EXIT
           ELSE
               MOVE 'Y'                TO WS-ERROR-FOUND-SW
               MOVE '9'                TO WPCW-ERROR-SW
               MOVE 'PDA015'           TO WPCW-PROGRAM-ID
               MOVE 'P05210'           TO WPCW-PARAGRAPH
               MOVE 'GU'               TO WPCW-FUNCTION
               MOVE 'ORDER'            TO WPCW-SEGMENT
               MOVE 'ORDER1DB'         TO WPCW-DATABASE
               MOVE DIBSTAT            TO WPCW-STATUS-CODE
               MOVE 'GU ORDER ROOT SEGMENT'
                                       TO WPCW-COMMAND
               GO TO P05210-GU-ORDER-DATABASE-EXIT.

       P05210-GU-ORDER-DATABASE-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05220-READ-CUSTOMER                           *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETRIEVE THE CUSTOMER INFORMATION   *
      *                RELATED TO THE ORDER BEING PROCESSED           *
      *                                                               *
      *    CALLED BY:  P02000-ORDER-PROCESS                           *
      *                                                               *
      *****************************************************************

       P05220-READ-CUSTOMER.


           EXEC CICS READ
                     DATASET ('PDACUST')
                     INTO    (CUSTOMER-RECORD)
                     RIDFLD  (WMF-CUSTOMER-KEY)
                     NOHANDLE
                     RESP    (WS-RESPONSE-CODE)
           END-EXEC.


      *****************************************************************
      *    CHECK RETURN CODE FOR SUCCESS, NOT FOUND, ALL OTHERS ARE   *
      *    AN ERROR                                                   *
      *****************************************************************

           IF WS-RESPONSE-CODE         =  DFHRESP(NORMAL)
               NEXT SENTENCE
           ELSE
           IF WS-RESPONSE-CODE         =  DFHRESP(NOTFND)
               MOVE 'Y'                TO WS-ERROR-FOUND-SW
               MOVE '2'                TO WPCW-ERROR-SW
               GO TO P05220-READ-CUSTOMER-EXIT
           ELSE
               MOVE 'Y'                TO WS-ERROR-FOUND-SW
               MOVE '8'                TO WPCW-ERROR-SW
               MOVE 'PDA015'           TO WPCW-PROGRAM-ID
               MOVE 'P05220'           TO WPCW-PARAGRAPH
               MOVE WS-RESPONSE-CODE   TO WPCW-RESPONSE-CODE
               MOVE 'ERROR READING PDACUST FILE'
                                       TO WPCW-COMMAND
               GO TO P05220-READ-CUSTOMER-EXIT.

       P05220-READ-CUSTOMER-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P05300-TERMINATE-PSB                           *
      *                                                               *
      *    FUNCTION :  PERFORMS TERMINATE PSB ON THE ORDER DATABASE.  *
      *                                                               *
      *    CALLED BY:  P05000-BUILD-SCREEN                            *
      *                                                               *
      *****************************************************************

       P05300-TERMINATE-PSB.

           EXEC DLI
               TERMINATE
           END-EXEC.

      *****************************************************************
      *    CHECK FOR PSB TERMINATION ERROR                            *
      *****************************************************************

           IF DIBSTAT              NOT = SPACES
               MOVE 'Y'                TO WS-ERROR-FOUND-SW
               MOVE '9'                TO WPCW-ERROR-SW
               MOVE 'PDA015'           TO WPCW-PROGRAM-ID
               MOVE 'P05300'           TO WPCW-PARAGRAPH
               MOVE 'TERM'             TO WPCW-FUNCTION
               MOVE SPACES             TO WPCW-SEGMENT
               MOVE SPACES             TO WPCW-DATABASE
               MOVE DIBSTAT            TO WPCW-STATUS-CODE
               MOVE 'PSB TERMINATION ERROR'
                                       TO WPCW-COMMAND
               GO TO P05300-TERMINATE-PSB-EXIT
           END-IF.

       P05300-TERMINATE-PSB-EXIT.
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

           MOVE 'Y'                TO WS-ERROR-FOUND-SW
           MOVE '8'                TO WPCW-ERROR-SW
           MOVE 'PDA014'           TO WPCW-PROGRAM-ID
           MOVE 'P99100'           TO WPCW-PARAGRAPH
           MOVE EIBRESP            TO WPCW-RESPONSE-CODE
           MOVE 'UNHANDLED CICS ERROR'
                                   TO WPCW-COMMAND.


           PERFORM  P99500-CICS-RETURN
               THRU P99500-CICS-RETURN-EXIT.


       P99100-GENERAL-ERROR-EXIT.
           EXIT.
           EJECT

      *****************************************************************
      *                                                               *
      *    PARAGRAPH:  P99500-CICS-RETURN                             *
      *                                                               *
      *    FUNCTION :  ROUTINE TO RETURN CONTROL TO THE CALLING       *
      *                PROGRAM.                                       *
      *                                                               *
      *    CALLED BY:  GLOBAL                                         *
      *                                                               *
      *****************************************************************

       P99500-CICS-RETURN.

      *****************************************************************
      *    SUSPEND ANY HANDLE CONDITIONS IN EFFECT                    *
      *****************************************************************

           EXEC CICS PUSH HANDLE
           END-EXEC.


      *****************************************************************
      *    ATTEMPT RETURN, TRANSACTION WILL ABEND IF UNSUCCESSFUL     *
      *****************************************************************

           MOVE WS-PDA-COMMAREA-WORKAREA
                                       TO PC-PROGRAM-WORKAREA.

           EXEC CICS RETURN
           END-EXEC.

       P99500-CICS-RETURN-EXIT.
           EXIT.
           EJECT
       P99600-COMMAREA-ERROR.

      *****************************************************************
      *    SUSPEND ANY HANDLE CONDITIONS IN EFFECT                    *
      *****************************************************************

           EXEC CICS PUSH HANDLE
           END-EXEC.


           EXEC CICS DUMP
                     TRANSACTION
                     DUMPCODE('PDER')
           END-EXEC.


      *****************************************************************
      *    ATTEMPT RETURN, TRANSACTION WILL ABEND IF UNSUCCESSFUL     *
      *****************************************************************

           EXEC CICS ABEND
                     ABCODE('CAER')
           END-EXEC.


       P99600-COMMAREA-ERROR-EXIT.
           EXIT.
           EJECT