      *****************************************************************
      *    CUSTOMER ORDER QUERY INFORMATION                           *
      *    (MQSERIES MESSAGE RECEIVED ON RESULTS QUEUE)               *
      *****************************************************************

       01  MQS-RESULTS-MESSAGE.
           05  MQS-RETURN-CODE         PIC X(01).
               88  MQS-NO-ERROR                           VALUE '0'.
               88  MQS-ERROR                              VALUE '1'.
               88  MQS-FATAL-ERROR                        VALUE '9'.
           05  MQS-TOTAL-ORDERS        PIC 9(05).
           05  MQS-TOTAL-DOLLAR-AMOUNT
                                       PIC 9(09)V99.
           05  MQS-AVG-DOLLAR-AMOUNT
                                       PIC 9(09)V99.
           05  MQS-LAST-ORDER-DATE     PIC X(06).
           05  MQS-LAST-ORDER-AMOUNT
                                       PIC 9(07)V99.
           05  MQS-LAST-ORDER-NUMBER
                                       PIC X(10).
           05  MQS-ORDER-DETAIL        OCCURS 14 TIMES.
               10  MQS-ORDER-NUMBER
                                       PIC X(10).
               10  MQS-ORDER-AMOUNT
                                       PIC 9(07)V99.
           05  MQS-SCREEN-MESSAGE      PIC X(79).
           05  MQS-ERROR-INFORMATION.
               10  MQS-PDA-ERROR-TYPE  PIC X(04).
               10  MQS-PDA-ERROR-LINE-01
                                       PIC X(78).
               10  MQS-PDA-ERROR-LINE-02
                                       PIC X(78).
           05  FILLER                  PIC X(442).