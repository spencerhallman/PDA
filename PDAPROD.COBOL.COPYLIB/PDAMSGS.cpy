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