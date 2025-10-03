      *****************************************************************
      *    CUSTOMER ORDER PAYMENT MESSAGE                             *
      *****************************************************************

       01  MQS-CUSTOMER-PAYMENT-MESSAGE.
           05  MQS-CUSTOMER-PAYMENT-ID PIC X(32)       VALUE SPACES.
           05  MQS-CUSTOMER-PAYMENT-AMT
                                       PIC 9(3)V99     VALUE ZEROES.
           05  MQS-CUSTOMER-PAYMENT-DESC
                                       PIC X(50)       VALUE SPACES.