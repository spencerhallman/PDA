      * $SEG(EZACIC6C)
      *--------------------------------------------------------------*
      *                                                              *
      *   Module Name : EZACIC6C                                     *
      *                                                              *
      *   Description :                                              *
      *                                                              *
      *      This is a sample CICS/TCP application program. It issues*
      *      TAKESOCKET to obtain the socket passed from MASTER      *
      *      SERVER and perform dialog function with CLIENT program. *
      *                                                              *
      *  COPYRIGHT = LICENSED MATERIALS - PROPERTY OF IBM            *
      *              5694-A01 (C) COPYRIGHT IBM CORP. 2003, 2005     *
      *              This module is restricted materials of IBM      *
      *              REFER TO IBM COPYRIGHT INSTRUCTIONS.            *
      *                                                              *
      *   Status :  CSV1R7                                           *
      *                                                              *
      *                                                              *
      *--------------------------------------------------------------*
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EZACIC6C.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       77  TASK-START                     PIC X(40)
            VALUE IS 'TASK STARTING THRU CICS/TCPIP INTERFACE '.
       77  GNI-ERR                       PIC X(24)
            VALUE IS ' GETNAMEINFO FAIL      '.
       77  GNI-SUCCESS                   PIC X(24)
            VALUE IS ' GETNAMEINFO SUCCESSFUL'.
       77  GPN-ERR                       PIC X(24)
            VALUE IS ' GETPEERNAME FAIL      '.
       77  GPN-SUCCESS                   PIC X(24)
            VALUE IS ' GETPEERNAME SUCCESSFUL'.
       77  TAKE-ERR                       PIC X(24)
            VALUE IS ' TAKESOCKET FAIL       '.
       77  TAKE-SUCCESS                    PIC X(24)
            VALUE IS ' TAKESOCKET SUCCESSFUL '.
       77  READ-ERR                        PIC X(24)
            VALUE IS ' READ SOCKET FAIL       '.
       77  READ-SUCCESS                    PIC X(24)
            VALUE IS ' READ SOCKET SUCCESSFUL '.
       77  WRITE-ERR                       PIC X(24)
            VALUE IS ' WRITE SOCKET FAIL      '.
       77  WRITE-END-ERR                       PIC X(32)
            VALUE IS ' WRITE SOCKET FAIL - PGM END MSG'.
       77  WRITE-SUCCESS                   PIC X(25)
            VALUE IS ' WRITE SOCKET SUCCESSFUL '.
       77  CLOS-ERR                        PIC X(24)
            VALUE IS ' CLOSE SOCKET FAIL      '.
       77  CLOS-SUCCESS                   PIC X(24)
            VALUE IS 'CLOSE SOCKET SUCCESSFUL '.
       77  INVREQ-ERR                     PIC X(24)
            VALUE IS 'INTERFACE IS NOT ACTIVE '.
       77  IOERR-ERR                     PIC X(24)
            VALUE IS 'IOERR OCCURRS           '.
       77  LENGERR-ERR                   PIC X(24)
            VALUE IS 'LENGERR ERROR           '.
       77  ITEMERR-ERR                   PIC X(24)
            VALUE IS 'ITEMERR ERROR           '.
       77  NOSPACE-ERR                   PIC X(24)
            VALUE IS 'NOSPACE CONDITION       '.
       77  QIDERR-ERR                   PIC X(24)
            VALUE IS 'QIDERR  CONDITION       '.
       77  ENDDATA-ERR                  PIC X(30)
            VALUE IS 'RETRIEVE DATA CAN NOT BE FOUND'.
       77  WRKEND                       PIC X(20)
            VALUE 'CONNECTION END      '.
       77  WRITE-SW                     PIC X(1)
            VALUE 'N'.
       77  FORCE-ERROR-MSG              PIC X(1)
            VALUE 'N'.
       01  SOKET-FUNCTIONS.
           02 SOKET-ACCEPT          PIC X(16) VALUE 'ACCEPT          '.
           02 SOKET-BIND            PIC X(16) VALUE 'BIND            '.
           02 SOKET-CLOSE           PIC X(16) VALUE 'CLOSE           '.
           02 SOKET-CONNECT         PIC X(16) VALUE 'CONNECT         '.
           02 SOKET-FCNTL           PIC X(16) VALUE 'FCNTL           '.
           02 SOKET-GETCLIENTID     PIC X(16) VALUE 'GETCLIENTID     '.
           02 SOKET-GETHOSTBYADDR   PIC X(16) VALUE 'GETHOSTBYADDR   '.
           02 SOKET-GETHOSTBYNAME   PIC X(16) VALUE 'GETHOSTBYNAME   '.
           02 SOKET-GETHOSTID       PIC X(16) VALUE 'GETHOSTID       '.
           02 SOKET-GETHOSTNAME     PIC X(16) VALUE 'GETHOSTNAME     '.
           02 SOKET-GETPEERNAME     PIC X(16) VALUE 'GETPEERNAME     '.
           02 SOKET-GETNAMEINFO     PIC X(16) VALUE 'GETNAMEINFO     '.
           02 SOKET-GETSOCKNAME     PIC X(16) VALUE 'GETSOCKNAME     '.
           02 SOKET-GETSOCKOPT      PIC X(16) VALUE 'GETSOCKOPT      '.
           02 SOKET-GIVESOCKET      PIC X(16) VALUE 'GIVESOCKET      '.
           02 SOKET-INITAPI         PIC X(16) VALUE 'INITAPI         '.
           02 SOKET-IOCTL           PIC X(16) VALUE 'IOCTL           '.
           02 SOKET-LISTEN          PIC X(16) VALUE 'LISTEN          '.
           02 SOKET-NTOP            PIC X(16) VALUE 'NTOP            '.
           02 SOKET-READ            PIC X(16) VALUE 'READ            '.
           02 SOKET-RECV            PIC X(16) VALUE 'RECV            '.
           02 SOKET-RECVFROM        PIC X(16) VALUE 'RECVFROM        '.
           02 SOKET-SELECT          PIC X(16) VALUE 'SELECT          '.
           02 SOKET-SEND            PIC X(16) VALUE 'SEND            '.
           02 SOKET-SENDTO          PIC X(16) VALUE 'SENDTO          '.
           02 SOKET-SETSOCKOPT      PIC X(16) VALUE 'SETSOCKOPT      '.
           02 SOKET-SHUTDOWN        PIC X(16) VALUE 'SHUTDOWN        '.
           02 SOKET-SOCKET          PIC X(16) VALUE 'SOCKET          '.
           02 SOKET-TAKESOCKET      PIC X(16) VALUE 'TAKESOCKET      '.
           02 SOKET-TERMAPI         PIC X(16) VALUE 'TERMAPI         '.
           02 SOKET-WRITE           PIC X(16) VALUE 'WRITE           '.

       01  WRKMSG.
           02 WRKM                         PIC X(14)
              VALUE IS 'DATA RECEIVED '.
      *---------------------------------------------------------------*
      *    program's variables                                        *
      *---------------------------------------------------------------*

       77  SUBTRACE                PIC X(8)  VALUE 'CONTRACE'.
       77  BITMASK-TOKEN           PIC X(16) VALUE 'TCPIPBITMASKCOBL'.
       77  TOEBCDIC-TOKEN          PIC X(16) VALUE 'TCPIPTOEBCDICXLT'.
       77  TOASCII-TOKEN           PIC X(16) VALUE 'TCPIPTOASCIIXLAT'.
       77  RESPONSE                       PIC 9(9) COMP.
       77  TASK-FLAG                      PIC X(1) VALUE '0'.
       77  TAKE-SOCKET                    PIC 9(8) COMP.
       77  DATA2-LENGTH                   PIC 9(04).
       77  NTOP-FAMILY                    PIC 9(8) COMP.
       77  NTOP-LENGTH                    PIC 9(4) COMP.
       77  SOCKID                         PIC 9(4) COMP.
       77  SOCKID-FWD                     PIC 9(8) COMP.
       77  ERRNO                          PIC 9(8) COMP.
       77  RETCODE                        PIC S9(8) COMP.
       01  TCP-BUF.
           05 TCP-BUF-H                   PIC X(3) VALUE IS SPACES.
           05 TCP-BUF-DATA                PIC X(197) VALUE IS SPACES.
       77  TCPLENG                        PIC 9(8) COMP.
       77  RECV-FLAG                      PIC 9(8) COMP.
       77  CLENG                          PIC 9(4) COMP.
       77  CPTRREF                        PIC 9(8) COMP.
       77  CNT                            PIC 9(4) COMP.
       77  MSGLENG                        PIC 9(4) COMP.

       01  ZERO-PARM                  PIC X(16) VALUE LOW-VALUES.
       01  DUMMY-MASK REDEFINES ZERO-PARM.
           05 DUMYMASK                PIC X(8).
           05 ZERO-FLD-8              PIC X(8).
       01  ZERO-FLD REDEFINES ZERO-PARM.
           05 ZERO-FWRD               PIC 9(8)  COMP.
           05 ZERO-HWRD               PIC 9(4)  COMP.
           05 ZERO-DUM                PIC X(10).

       01  TD-MSG.
           03 TASK-LABEL              PIC X(07) VALUE 'TASK # '.
           03 TASK-NUMBER             PIC 9(07).
           03 TASK-SEP                PIC X     VALUE ' '.
           03  CICS-MSG-AREA          PIC X(70).
       01  CICS-DETAIL-AREA.
           03  DETAIL-FIELD       PIC X(20).
           03  DETAIL-EQUALS      PIC X(02) VALUE '= '.
           03  DETAIL-DATA        PIC X(48) VALUE SPACES.
       01  CICS-ERR-AREA.
           03  ERR-MSG            PIC X(24).
           03  SOCK-HEADER        PIC X(08) VALUE ' SOCKET='.
           03  ERR-SOCKET         PIC 9(05).
           03  RETC-HEADER        PIC X(09) VALUE ' RETCDE=-'.
           03  ERR-RETCODE        PIC 9(05).
           03  ERRN-HEADER        PIC X(07) VALUE ' ERRNO='.
           03  ERR-ERRNO          PIC 9(05).
       01  CICS-DATA2-AREA.
           05 DATA-2-FOR-MSG      PIC X(48) VALUE SPACES.
           05 FILLER              PIC X(951).
      *
       01  CLIENTID-LSTN.
           05 CID-DOMAIN-LSTN               PIC 9(8) COMP.
           05 CID-NAME-LSTN                 PIC X(8).
           05 CID-SUBTASKNAME-LSTN          PIC X(8).
           05 CID-RES-LSTN                  PIC X(20).

       01  CLIENTID-APPL.
           05 CID-DOMAIN-APPL               PIC 9(8) COMP.
           05 CID-NAME-APPL                 PIC X(8).
           05 CID-SUBTASKNAME-APPL          PIC X(8).
           05 CID-RES-APPL                  PIC X(20).

      *
      * GETNAMEINFO Call variables.
      *
       01  NAME-LEN                       PIC 9(8) BINARY.
       01  HOST-NAME                      PIC X(255).
       01  HOST-NAME-LEN                  PIC 9(8) BINARY.
       01  SERVICE-NAME                   PIC X(32).
       01  SERVICE-NAME-LEN               PIC 9(8) BINARY.
       01  NAME-INFO-FLAGS                PIC 9(8) BINARY VALUE 0.

      *
      * GETNAMEINFO FLAG VALUES
      *
       01  NI-NOFQDN                      PIC 9(8) BINARY VALUE 1.
       01  NI-NUMERICHOST                 PIC 9(8) BINARY VALUE 2.
       01  NI-NAMEREQD                    PIC 9(8) BINARY VALUE 4.
       01  NI-NUMERICSERV                 PIC 9(8) BINARY VALUE 8.
       01  NI-DGRAM                       PIC 9(8) BINARY VALUE 16.

      *
      * GETPEERNAME SOCKET ADDRESS STRUCTURE
      *
       01 PEER-NAME.
          05 PEER-FAMILY                PIC 9(4) BINARY.
             88 PEER-FAMILY-IS-AFINET   VALUE 2.
             88 PEER-FAMILY-IS-AFINET6  VALUE 19.
          05 PEER-DATA                  PIC X(26).
          05 PEER-SIN REDEFINES PEER-DATA.
             10 PEER-SIN-PORT           PIC 9(4) BINARY.
             10 PEER-SIN-ADDR           PIC 9(8) BINARY.
             10 FILLER                  PIC X(8).
             10 FILLER                  PIC X(12).
          05 PEER-SIN6 REDEFINES PEER-DATA.
             10 PEER-SIN6-PORT          PIC 9(4) BINARY.
             10 PEER-SIN6-FLOWINFO      PIC 9(8) BINARY.
             10 PEER-SIN6-ADDR.
                15 FILLER               PIC 9(16) BINARY.
                15 FILLER               PIC 9(16) BINARY.
             10 PEER-SIN6-SCOPEID       PIC 9(8) BINARY.

      *
      * TRANSACTION INPUT MESSAGE FROMT THE LISTENER
      *
       01  TCPSOCKET-PARM.
           05 GIVE-TAKE-SOCKET              PIC 9(8) COMP.
           05 LSTN-NAME                     PIC X(8).
           05 LSTN-SUBTASKNAME              PIC X(8).
           05 CLIENT-IN-DATA                PIC X(35).
           05 THREADSAFE-INDICATOR          PIC X(1).
              88 INTERFACE-IS-THREADSAFE             VALUE '1'.
           05 SOCKADDR-IN.
              10 SOCK-FAMILY                PIC 9(4) BINARY.
                 88 SOCK-FAMILY-IS-AFINET   VALUE 2.
                 88 SOCK-FAMILY-IS-AFINET6  VALUE 19.
              10 SOCK-DATA                  PIC X(26).
              10 SOCK-SIN REDEFINES SOCK-DATA.
                 15 SOCK-SIN-PORT           PIC 9(4) BINARY.
                 15 SOCK-SIN-ADDR           PIC 9(8) BINARY.
                 15 FILLER                  PIC X(8).
                 15 FILLER                  PIC X(12).
              10 SOCK-SIN6 REDEFINES SOCK-DATA.
                 15 SOCK-SIN6-PORT          PIC 9(4) BINARY.
                 15 SOCK-SIN6-FLOWINFO      PIC 9(8) BINARY.
                 15 SOCK-SIN6-ADDR.
                    20 FILLER               PIC 9(16) BINARY.
                    20 FILLER               PIC 9(16) BINARY.
                 15 SOCK-SIN6-SCOPEID       PIC 9(8) BINARY.
           05 FILLER                        PIC X(68).
           05 CLIENT-IN-DATA-LENGTH         PIC 9(4) COMP.
           05 CLIENT-IN-DATA-2              PIC X(999).

       PROCEDURE DIVISION.

           MOVE 'Y' TO WRITE-SW.

           EXEC CICS HANDLE CONDITION INVREQ  (INVREQ-ERR-SEC)
                                      IOERR   (IOERR-SEC)
                                      ENDDATA (ENDDATA-SEC)
                                      NOSPACE (NOSPACE-ERR-SEC)
                                      QIDERR  (QIDERR-SEC)
                                      ITEMERR (ITEMERR-SEC)
                END-EXEC.

           EXEC CICS IGNORE CONDITION LENGERR
                END-EXEC.


           PERFORM INITIAL-SEC     THRU   INITIAL-SEC-EXIT.
           PERFORM TAKESOCKET-SEC  THRU   TAKESOCKET-SEC-EXIT.
           PERFORM GET-PEER-NAME   THRU   GET-PEER-NAME-EXIT.
           PERFORM GET-NAME-INFO   THRU   GET-NAME-INFO-EXIT.

           MOVE '0' TO TASK-FLAG.
           PERFORM CLIENT-TASK     THRU   CLIENT-TASK-EXIT
               VARYING CNT FROM 1 BY 1  UNTIL TASK-FLAG = '1'.

       CLOSE-SOCK.
      *---------------------------------------------------------------*
      *                                                               *
      *   CLOSE 'accept descriptor'                                   *
      *                                                               *
      *---------------------------------------------------------------*

           CALL 'EZASOKET' USING SOKET-CLOSE SOCKID
                 ERRNO RETCODE.

           IF RETCODE <  0 THEN
              MOVE 'Y' TO WRITE-SW FORCE-ERROR-MSG
              MOVE CLOS-ERR TO ERR-MSG
              MOVE SOCKID TO ERR-SOCKET
              MOVE RETCODE TO ERR-RETCODE
              MOVE ERRNO TO ERR-ERRNO
              MOVE CICS-ERR-AREA TO CICS-MSG-AREA
           ELSE
              MOVE CLOS-SUCCESS TO CICS-MSG-AREA.
           PERFORM WRITE-CICS  THRU WRITE-CICS-EXIT.

       PGM-EXIT.

           IF RETCODE < 0 THEN
              EXEC CICS ABEND ABCODE('SRV6') END-EXEC.

           MOVE SPACES TO CICS-MSG-AREA.
           MOVE 'END OF EZACIC6C PROGRAM' TO CICS-MSG-AREA.
           PERFORM WRITE-CICS THRU WRITE-CICS-EXIT.
           EXEC CICS RETURN  END-EXEC.
           GOBACK.

      *---------------------------------------------------------------*
      *
      *  RECEIVE PASSED PARAMETER WHICH ARE CID                       *
      *
      *---------------------------------------------------------------*
       INITIAL-SEC.

           MOVE SPACES TO CICS-MSG-AREA.
           MOVE 50 TO MSGLENG.
           MOVE 'SRV6 TRANSACTION START UP     ' TO CICS-MSG-AREA.
           PERFORM WRITE-CICS THRU WRITE-CICS-EXIT.

      *
      *  PREPARE TO RECEIVE AND ENHANCED TIM
      *
           MOVE 1153 TO CLENG.

           INITIALIZE TCPSOCKET-PARM.

           EXEC CICS RETRIEVE INTO(TCPSOCKET-PARM)
                              LENGTH(CLENG)
                              END-EXEC.

           MOVE 'LISTENER ADDR SPACE ' TO DETAIL-FIELD.
           MOVE SPACES TO DETAIL-DATA.
           MOVE LSTN-NAME TO DETAIL-DATA.
           MOVE CICS-DETAIL-AREA TO CICS-MSG-AREA.
           PERFORM WRITE-CICS THRU WRITE-CICS-EXIT.

           MOVE 'LISTENER TASK ID    ' TO DETAIL-FIELD.
           MOVE SPACES TO DETAIL-DATA.
           MOVE LSTN-SUBTASKNAME TO DETAIL-DATA.
           MOVE CICS-DETAIL-AREA TO CICS-MSG-AREA.
           PERFORM WRITE-CICS THRU WRITE-CICS-EXIT.

           IF CLIENT-IN-DATA-LENGTH <= 0
               MOVE 'TIM IS STANDARD' TO CICS-MSG-AREA
               PERFORM WRITE-CICS THRU WRITE-CICS-EXIT

               MOVE 'CLIENT IN DATA      ' TO DETAIL-FIELD
               MOVE SPACES TO DETAIL-DATA
               MOVE CLIENT-IN-DATA TO DETAIL-DATA
               MOVE CICS-DETAIL-AREA TO CICS-MSG-AREA
               PERFORM WRITE-CICS THRU WRITE-CICS-EXIT

           ELSE
               MOVE 'TIM IS ENHANCED' TO CICS-MSG-AREA
               PERFORM WRITE-CICS THRU WRITE-CICS-EXIT

               MOVE 'CLIENT IN DATA      ' TO DETAIL-FIELD
               MOVE SPACES TO DETAIL-DATA
               MOVE CLIENT-IN-DATA TO DETAIL-DATA
               MOVE CICS-DETAIL-AREA TO CICS-MSG-AREA
               PERFORM WRITE-CICS THRU WRITE-CICS-EXIT

               MOVE 'CLIENT IN DATA 2 LEN' TO DETAIL-FIELD
               MOVE SPACES TO DETAIL-DATA
               MOVE CLIENT-IN-DATA-LENGTH TO DATA2-LENGTH
               MOVE DATA2-LENGTH TO DETAIL-DATA
               MOVE CICS-DETAIL-AREA TO CICS-MSG-AREA
               PERFORM WRITE-CICS THRU WRITE-CICS-EXIT

               MOVE 'CLIENT IN DATA 2    ' TO DETAIL-FIELD
               MOVE SPACES TO DETAIL-DATA
               MOVE CLIENT-IN-DATA-2 TO CICS-DATA2-AREA
               MOVE DATA-2-FOR-MSG TO DETAIL-DATA
               MOVE CICS-DETAIL-AREA TO CICS-MSG-AREA
               PERFORM WRITE-CICS THRU WRITE-CICS-EXIT.

       INITIAL-SEC-EXIT.
           EXIT.

      *---------------------------------------------------------------*
      *                                                               *
      *  Perform TCP SOCKET functions by passing socket command to    *
      *  EZASOKET routine.  SOCKET command are translated to pre-     *
      *  define integer.                                              *
      *                                                               *
      *---------------------------------------------------------------*

       TAKESOCKET-SEC.

      *---------------------------------------------------------------*
      *                                                               *
      *   Issue 'TAKESOCKET' call to acquire a socket which was       *
      *   given by the LISTENER program.                              *
      *                                                               *
      *---------------------------------------------------------------*

      *    MOVE AF-INET TO CID-DOMAIN-LSTN CID-DOMAIN-APPL.
           MOVE SOCK-FAMILY TO CID-DOMAIN-LSTN CID-DOMAIN-APPL.

           MOVE LSTN-NAME TO CID-NAME-LSTN.
           MOVE LSTN-SUBTASKNAME TO CID-SUBTASKNAME-LSTN.
           MOVE GIVE-TAKE-SOCKET TO TAKE-SOCKET SOCKID SOCKID-FWD.
           CALL 'EZASOKET' USING SOKET-TAKESOCKET SOCKID
                CLIENTID-LSTN ERRNO RETCODE.


           IF RETCODE <  0 THEN
              MOVE 'Y' TO WRITE-SW FORCE-ERROR-MSG
              MOVE TAKE-ERR TO ERR-MSG
              MOVE SOCKID TO ERR-SOCKET
              MOVE RETCODE TO ERR-RETCODE
              MOVE ERRNO TO ERR-ERRNO
              MOVE CICS-ERR-AREA TO CICS-MSG-AREA
              PERFORM WRITE-CICS THRU WRITE-CICS-EXIT
              GO TO PGM-EXIT
           ELSE
               MOVE SPACES TO CICS-MSG-AREA
               MOVE TAKE-SUCCESS TO CICS-MSG-AREA
               PERFORM WRITE-CICS THRU WRITE-CICS-EXIT.

           MOVE SPACES TO CICS-MSG-AREA.
           IF SOCK-FAMILY-IS-AFINET
               MOVE 'TOOK AN AF_INET SOCKET' TO CICS-MSG-AREA
               PERFORM WRITE-CICS THRU WRITE-CICS-EXIT
               MOVE SPACES TO DETAIL-DATA
               MOVE 'AF_INET ADDRESS IS ' TO DETAIL-FIELD
               MOVE SOCK-FAMILY TO NTOP-FAMILY
               MOVE 16 TO NTOP-LENGTH
               CALL 'EZASOKET' USING SOKET-NTOP
                                    NTOP-FAMILY
                                    SOCK-SIN-ADDR
                                    DETAIL-DATA
                                    NTOP-LENGTH
                                    ERRNO
                                    RETCODE
           ELSE
               MOVE 'TOOK AN AF_INET6 SOCKET' TO CICS-MSG-AREA
               PERFORM WRITE-CICS THRU WRITE-CICS-EXIT
               MOVE 'AF_INET6 ADDRESS IS ' TO DETAIL-FIELD
               MOVE SPACES TO DETAIL-DATA
               MOVE SOCK-FAMILY TO NTOP-FAMILY
               MOVE 45 TO NTOP-LENGTH
               CALL 'EZASOKET' USING SOKET-NTOP
                                    NTOP-FAMILY
                                    SOCK-SIN6-ADDR
                                    DETAIL-DATA
                                    NTOP-LENGTH
                                    ERRNO
                                    RETCODE.
           MOVE CICS-DETAIL-AREA TO CICS-MSG-AREA.
           PERFORM WRITE-CICS THRU WRITE-CICS-EXIT.

           MOVE RETCODE TO SOCKID.
           MOVE SPACES TO TCP-BUF.
           MOVE TASK-START TO TCP-BUF.
           MOVE 50  TO TCPLENG.
      *
      *    REMOVE FOLLOWING STATEMENT IF USING EBCDIC CLIENT
      *
           CALL 'EZACIC04' USING TOASCII-TOKEN TCP-BUF TCPLENG.

           CALL 'EZASOKET' USING SOKET-WRITE SOCKID TCPLENG
                 TCP-BUF ERRNO RETCODE.

           IF RETCODE <  0 THEN
              MOVE 'Y' TO WRITE-SW FORCE-ERROR-MSG
              MOVE WRITE-ERR TO ERR-MSG
              MOVE SOCKID TO ERR-SOCKET
              MOVE RETCODE TO ERR-RETCODE
              MOVE ERRNO TO ERR-ERRNO
              MOVE CICS-ERR-AREA TO CICS-MSG-AREA
              PERFORM WRITE-CICS THRU WRITE-CICS-EXIT
              GO TO PGM-EXIT
           ELSE
              MOVE WRITE-SUCCESS TO CICS-MSG-AREA
              PERFORM WRITE-CICS THRU WRITE-CICS-EXIT.
       TAKESOCKET-SEC-EXIT.
           EXIT.

       GET-PEER-NAME.
           CALL 'EZASOKET' USING SOKET-GETPEERNAME
              SOCKID PEER-NAME ERRNO RETCODE.
           IF RETCODE <  0 THEN
              MOVE 'Y' TO WRITE-SW FORCE-ERROR-MSG
              MOVE GPN-ERR TO ERR-MSG
              MOVE SOCKID TO ERR-SOCKET
              MOVE RETCODE TO ERR-RETCODE
              MOVE ERRNO TO ERR-ERRNO
              MOVE CICS-ERR-AREA TO CICS-MSG-AREA
              PERFORM WRITE-CICS THRU WRITE-CICS-EXIT
              GO TO PGM-EXIT
           ELSE
              MOVE GPN-SUCCESS TO CICS-MSG-AREA
              PERFORM WRITE-CICS THRU WRITE-CICS-EXIT.
       GET-PEER-NAME-EXIT.
           EXIT.

       GET-NAME-INFO.
           IF PEER-FAMILY-IS-AFINET
              MOVE 16 TO NAME-LEN
           ELSE
              MOVE 28 TO NAME-LEN.
           MOVE SPACES TO HOST-NAME.
           MOVE 256 TO HOST-NAME-LEN.
           MOVE SPACES TO SERVICE-NAME.
           MOVE 32 TO SERVICE-NAME-LEN.
           CALL 'EZASOKET' USING SOKET-GETNAMEINFO
              PEER-NAME NAME-LEN
              HOST-NAME HOST-NAME-LEN
              SERVICE-NAME SERVICE-NAME-LEN
              NAME-INFO-FLAGS
              ERRNO RETCODE.
           IF RETCODE <  0 THEN
              MOVE 'Y' TO WRITE-SW FORCE-ERROR-MSG
              MOVE GNI-ERR TO ERR-MSG
              MOVE SOCKID TO ERR-SOCKET
              MOVE RETCODE TO ERR-RETCODE
              MOVE ERRNO TO ERR-ERRNO
              MOVE CICS-ERR-AREA TO CICS-MSG-AREA
              PERFORM WRITE-CICS THRU WRITE-CICS-EXIT
              GO TO PGM-EXIT
           ELSE
              MOVE GNI-SUCCESS TO CICS-MSG-AREA
              PERFORM WRITE-CICS THRU WRITE-CICS-EXIT.
       GET-NAME-INFO-EXIT.
           EXIT.

       CLIENT-TASK.
      *---------------------------------------------------------------*
      *                                                               *
      *  Issue 'RECV' socket to receive input data from client        *
      *                                                               *
      *---------------------------------------------------------------*


           MOVE LOW-VALUES TO TCP-BUF.
           MOVE 200 TO TCPLENG.
           MOVE ZEROS TO RECV-FLAG.

           CALL 'EZASOKET' USING SOKET-RECV SOCKID
                RECV-FLAG TCPLENG TCP-BUF ERRNO RETCODE.

           IF RETCODE <  0 THEN
              MOVE 'Y' TO WRITE-SW FORCE-ERROR-MSG
              MOVE READ-ERR TO ERR-MSG
              MOVE SOCKID TO ERR-SOCKET
              MOVE RETCODE TO ERR-RETCODE
              MOVE ERRNO TO ERR-ERRNO
              MOVE CICS-ERR-AREA TO CICS-MSG-AREA
              PERFORM WRITE-CICS THRU WRITE-CICS-EXIT
              GO TO PGM-EXIT
           ELSE
              MOVE READ-SUCCESS TO CICS-MSG-AREA
              PERFORM WRITE-CICS THRU WRITE-CICS-EXIT.

      *
      *    REMOVE FOLLOWING STATEMENT IF USING EBCDIC CLIENT
      *
           CALL 'EZACIC05' USING TOEBCDIC-TOKEN TCP-BUF TCPLENG.

      *
      *    DETERMINE WHETHER THE CLIENT IS FINISHED SENDING DATA
      *
           IF TCP-BUF-H = 'END' OR TCP-BUF-H = 'end' THEN
              MOVE '1' TO TASK-FLAG
              PERFORM CLIENT-TALK-END THRU CLIENT-TALK-END-EXIT
              GO TO CLIENT-TASK-EXIT.

           IF RETCODE = 0  THEN
              MOVE '1' TO TASK-FLAG
              GO TO CLIENT-TASK-EXIT.
      *---------------------------------------------------------------*
      **  ECHO RECEIVING DATA
      *---------------------------------------------------------------*
           MOVE TCP-BUF TO CICS-MSG-AREA.
           PERFORM WRITE-CICS THRU WRITE-CICS-EXIT.

           MOVE RETCODE TO TCPLENG.
      *
      *    REMOVE FOLLOWING STATEMENT IF USING EBCDIC CLIENT
      *
           CALL 'EZACIC04' USING TOASCII-TOKEN TCP-BUF TCPLENG.
           CALL 'EZASOKET' USING SOKET-WRITE SOCKID TCPLENG
                TCP-BUF ERRNO RETCODE.


           IF RETCODE <  0 THEN
              MOVE 'Y' TO WRITE-SW FORCE-ERROR-MSG
              MOVE WRITE-ERR TO ERR-MSG
              MOVE SOCKID TO ERR-SOCKET
              MOVE RETCODE TO ERR-RETCODE
              MOVE ERRNO TO ERR-ERRNO
              MOVE CICS-ERR-AREA TO CICS-MSG-AREA
              PERFORM WRITE-CICS THRU WRITE-CICS-EXIT
              GO TO PGM-EXIT
           ELSE
              MOVE WRITE-SUCCESS TO CICS-MSG-AREA
              PERFORM WRITE-CICS THRU WRITE-CICS-EXIT.

       CLIENT-TASK-EXIT.
           EXIT.


       WRITE-CICS.
           MOVE 78 TO CLENG.
           MOVE EIBTASKN TO TASK-NUMBER.
           IF WRITE-SW = 'Y' THEN
               IF INTERFACE-IS-THREADSAFE THEN
                   IF FORCE-ERROR-MSG = 'Y' THEN
                       EXEC CICS WRITEQ TD QUEUE('CSMT') FROM(TD-MSG)
                            LENGTH(CLENG) NOHANDLE
                       END-EXEC
                   ELSE
                       NEXT SENTENCE
               ELSE
                   EXEC CICS WRITEQ TD QUEUE('CSMT') FROM(TD-MSG)
                        LENGTH(CLENG) NOHANDLE
                   END-EXEC
           ELSE
               NEXT SENTENCE.
           MOVE SPACES TO CICS-MSG-AREA.

       WRITE-CICS-EXIT.
           EXIT.

       CLIENT-TALK-END.
              MOVE LOW-VALUES TO TCP-BUF.
              MOVE WRKEND TO TCP-BUF CICS-MSG-AREA.

              MOVE 50 TO TCPLENG.
      *
      *    REMOVE FOLLOWING STATEMENT IF USING EBCDIC CLIENT
      *
              CALL 'EZACIC04' USING TOASCII-TOKEN TCP-BUF TCPLENG.
              CALL 'EZASOKET' USING SOKET-WRITE SOCKID TCPLENG
                   TCP-BUF ERRNO RETCODE.

              IF RETCODE <  0 THEN
                 MOVE 'Y' TO WRITE-SW FORCE-ERROR-MSG
                 MOVE WRITE-END-ERR TO ERR-MSG
                 MOVE SOCKID TO ERR-SOCKET
                 MOVE RETCODE TO ERR-RETCODE
                 MOVE ERRNO TO ERR-ERRNO
                 MOVE CICS-ERR-AREA TO CICS-MSG-AREA
                 PERFORM WRITE-CICS THRU WRITE-CICS-EXIT
                 GO TO PGM-EXIT.


       CLIENT-TALK-END-EXIT.
           EXIT.

       INVREQ-ERR-SEC.
           MOVE 'Y' TO WRITE-SW FORCE-ERROR-MSG
           MOVE INVREQ-ERR TO CICS-MSG-AREA.
           PERFORM WRITE-CICS THRU WRITE-CICS-EXIT.
           GO TO PGM-EXIT.
       IOERR-SEC.
           MOVE 'Y' TO WRITE-SW FORCE-ERROR-MSG
           MOVE IOERR-ERR TO CICS-MSG-AREA.
           PERFORM WRITE-CICS THRU WRITE-CICS-EXIT.
           GO TO PGM-EXIT.
       LENGERR-SEC.
           MOVE 'Y' TO WRITE-SW FORCE-ERROR-MSG
           MOVE LENGERR-ERR TO CICS-MSG-AREA.
           PERFORM WRITE-CICS THRU WRITE-CICS-EXIT.
           GO TO PGM-EXIT.
       NOSPACE-ERR-SEC.
           MOVE 'Y' TO WRITE-SW FORCE-ERROR-MSG
           MOVE NOSPACE-ERR TO CICS-MSG-AREA.
           PERFORM WRITE-CICS THRU WRITE-CICS-EXIT.
           GO TO PGM-EXIT.
       QIDERR-SEC.
           MOVE 'Y' TO WRITE-SW FORCE-ERROR-MSG
           MOVE QIDERR-ERR TO CICS-MSG-AREA.
           PERFORM WRITE-CICS THRU WRITE-CICS-EXIT.
           GO TO PGM-EXIT.
       ITEMERR-SEC.
           MOVE 'Y' TO WRITE-SW FORCE-ERROR-MSG
           MOVE ITEMERR-ERR TO CICS-MSG-AREA.
           PERFORM WRITE-CICS THRU WRITE-CICS-EXIT.
           GO TO PGM-EXIT.
       ENDDATA-SEC.
           MOVE 'Y' TO WRITE-SW FORCE-ERROR-MSG
           MOVE ENDDATA-ERR TO CICS-MSG-AREA.
           PERFORM WRITE-CICS THRU WRITE-CICS-EXIT.
           GO TO PGM-EXIT.

