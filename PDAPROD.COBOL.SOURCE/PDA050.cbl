      ***************************************************************
      *                                                             *
      * Communications Server for z/OS     Version 1, Release 7     *
      *                                                             *
      *                                                             *
      * Copyright:    Licensed Materials - Property of IBM          *
      *                                                             *
      *               "Restricted Materials of IBM"                 *
      *                                                             *
      *               5694-A01                                      *
      *                                                             *
      *               (C) Copyright IBM Corp. 2003, 2005            *
      *                                                             *
      *               US Government Users Restricted Rights -       *
      *               Use, duplication or disclosure restricted by  *
      *               GSA ADP Schedule Contract with IBM Corp.      *
      *                                                             *
      * Status:       CSV1R7                                        *
      *                                                             *
      * $MOD(EZACIC6S),COMP(CICS),PROD(TCPIP):                      *
      *                                                             *
      ***************************************************************
      * $SEG(EZACIC6S)
      *--------------------------------------------------------------*
      *                                                              *
      *   Module Name :  EZACIC6S                                    *
      *                                                              *
      *   Description :  This is a sample server program.  It        *
      *                  establishes a connection between            *
      *                  CICS & TCPIP to process client requests.    *
      *                  The server expects the data received        *
      *                  from a host / workstation in ASCII.         *
      *                  All responses sent by the server to the     *
      *                  CLIENT are in ASCII.  This server is        *
      *                  started using CECI or via the LISTENER.     *
      *                                                              *
      *                    CECI START TRANS(xxxx) from(yyyy)         *
      *                       where xxxx is this servers CICS        *
      *                       transaction id and yyyy is the         *
      *                       port this server will listen on.       *
      *                                                              *
      *                  It processes request received from          *
      *                  clients for updates to a hypothetical       *
      *                  DB2 database.  Any and all references to    *
      *                  DB2 or SQL are commented out as this        *
      *                  sample is to illustrate CICS Sockets.       *
      *                                                              *
      *                  A client connection is broken when the      *
      *                  client transmits and 'END' token to the     *
      *                  server.  All processing is terminated       *
      *                  when an 'TRM' token is received from a      *
      *                  client.                                     *
      *                                                              *
      *                                                              *
      *--------------------------------------------------------------*
      *                                                              *
      *   LOGIC       :  1.  Establish server setup                  *
      *                      a).  TRUE Active                        *
      *                      b).  CAF Active                         *
      *                  2.  Assign user specified port at           *
      *                      start up or use the program             *
      *                      declared default.                       *
      *                  3.  Initialize the AF_INET6 socket.         *
      *                  4.  Bind the port and in6addr_any.          *
      *                  5.  Set Bit Mask to accept incoming         *
      *                      read request.                           *
      *                  6.  Process request from clients.           *
      *                      a).  Wait for connection                *
      *                      b).  Process request until 'END'        *
      *                           token is receive from client.      *
      *                      c).  Close connection.                  *
      *                      note:  The current client request       *
      *                             ends when the client closes      *
      *                             the connection or sends an       *
      *                             'END' token to the server.       *
      *                      d).  If the last request received by    *
      *                           the current client is not a        *
      *                           request to the server to           *
      *                           terminate processing ('TRM'),      *
      *                           continue at step 6A.               *
      *                  7.  Close the server's connection.          *
      *                                                              *
      *--------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EZACIC6S.
       ENVIRONMENT DIVISION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

      *---------------------------------------------------------------*
      *   MESSAGES                                                    *
      *---------------------------------------------------------------*

       77  BITMASK-ERR                     PIC X(30)
            VALUE IS 'BITMASK CONVERSION - FAILED   '.
       77  ENDDATA-ERR                     PIC X(30)
            VALUE IS 'RETRIEVE DATA CAN NOT BE FOUND'.
       77  INIT-MSG                        PIC X(30)
            VALUE IS 'INITAPI COMPLETE              '.
       77  IOERR-ERR                       PIC X(30)
            VALUE IS 'IOERR OCCURRS                 '.
       77  ITEMERR-ERR                     PIC X(30)
            VALUE IS 'ITEMERR ERROR                 '.
       77  KEYWORD-ERR                     PIC X(30)
            VALUE IS 'INPUT KEYWORD ERROR           '.
       77  LENGERR-ERR                     PIC X(30)
            VALUE IS 'LENGERR ERROR                 '.
       77  NOSPACE-ERR                     PIC X(30)
            VALUE IS 'NOSPACE CONDITION             '.
       77  NULL-DATA                       PIC X(30)
            VALUE IS 'READ NULL DATA                '.
       77  QIDERR-ERR                      PIC X(30)
            VALUE IS 'TRANSIENT DATA QUEUE NOT FOUND'.
       77  START-MSG                       PIC X(30)
            VALUE IS 'SERVER PROGRAM IS STARTING    '.
       77  TCP-EXIT-ERR                    PIC X(30)
            VALUE IS 'SERVER STOPPED:TRUE NOT ACTIVE'.
       77  TCP-SERVER-OFF                  PIC X(30)
            VALUE IS 'SERVER IS ENDING              '.
       77  TS-INVREQ-ERR                   PIC X(30)
            VALUE IS 'WRITE TS FAILED  - INVREQ     '.
       77  TS-NOTAUTH-ERR                  PIC X(30)
            VALUE IS 'WRITE TS FAILED  - NOTAUTH    '.
       77  TS-IOERR-ERR                    PIC X(30)
            VALUE IS 'WRITE TS FAILED  - IOERR      '.
       77  WRITETS-ERR                     PIC X(30)
            VALUE IS 'WRITE TS FAILED               '.

       01  ACCEPT-ERR.
           05  ACCEPT-ERR-M                PIC X(25)
                VALUE IS 'SOCKET CALL FAIL - ACCEPT'.
           05  FILLER                      PIC X(9)
                VALUE IS ' ERRNO = '.
           05  ACCEPT-ERRNO                PIC 9(8) DISPLAY.
           05  FILLER                      PIC X(13)
                VALUE IS SPACES.

       01  NTOP-ERR.
           05  NTOP-ERR-M                  PIC X(23)
                VALUE IS 'SOCKET CALL FAIL - NTOP'.
           05  FILLER                      PIC X(9)
                VALUE IS ' ERRNO = '.
           05  NTOP-ERRNO                  PIC 9(8) DISPLAY.
           05  FILLER                      PIC X(13)
                VALUE IS SPACES.

       01  NTOP-OK.
           05  NTOP-OK-M                   PIC X(21)
                VALUE IS 'ACCEPTED IP ADDRESS: '.
           05  NTOP-PRESENTABLE-ADDR       PIC X(45) DISPLAY
                VALUE IS SPACES.

       01  GNI-ERR.
           05  GNI-ERR-M                   PIC X(30)
                VALUE IS 'SOCKET CALL FAIL - GETNAMEINFO'.
           05  FILLER                      PIC X(9)
                VALUE IS ' ERRNO = '.
           05  GNI-ERRNO                   PIC 9(8) DISPLAY.
           05  FILLER                      PIC X(13)
                VALUE IS SPACES.

       01  GNI-HOST-NAME-OK.
           05  FILLER                      PIC X(19)
                VALUE IS 'CLIENTS HOST NAME: '.
           05  GNI-HOST-NAME               PIC X(255) DISPLAY
                VALUE IS SPACES.

       01  GNI-SERVICE-NAME-OK.
           05  FILLER                      PIC X(22)
                VALUE IS 'CLIENTS SERVICE NAME: '.
           05  GNI-SERVICE-NAME            PIC X(32) DISPLAY
                VALUE IS SPACES.

       01  GPN-ERR.
           05  GPN-ERR-M                   PIC X(30)
                VALUE IS 'SOCKET CALL FAIL - GETPEERNAME'.
           05  FILLER                      PIC X(9)
                VALUE IS ' ERRNO = '.
           05  GPN-ERRNO                   PIC 9(8) DISPLAY.
           05  FILLER                      PIC X(13)
                VALUE IS SPACES.

       01  BIND-ERR.
           05  BIND-ERR-M                  PIC X(25)
                VALUE IS 'SOCKET CALL FAIL  -  BIND'.
           05  FILLER                      PIC X(9)
                VALUE IS ' ERRNO = '.
           05  BIND-ERRNO                  PIC 9(8) DISPLAY.
           05  FILLER                      PIC X(13)
                VALUE IS SPACES.

       01  CLOSE-ERR.
           05  CLOSE-ERR-M                 PIC X(30)
                VALUE IS 'CLOSE SOCKET DESCRIPTOR FAILED'.
           05  FILLER                      PIC X(9)
                VALUE IS ' ERRNO = '.
           05  CLOSE-ERRNO                 PIC 9(8)  DISPLAY.
           05  FILLER                      PIC X(8)
                VALUE IS SPACES.

       01  DB2END.
           05  FILLER                      PIC X(16)
                VALUE IS 'DB2 PROCESS ENDS'.
           05  FILLER                      PIC X(39)
                VALUE IS SPACES.

       01  DB2-CAF-ERR.
           05  FILLER                      PIC X(24)
                VALUE IS 'CONNECT NOT ESTABLISHED '.
           05  FILLER                      PIC X(30)
                VALUE IS 'ATTACHMENT FACILITY NOT ACTIVE'.
           05  FILLER                      PIC X(1)
                VALUE IS SPACES.

       01  DB2MSG.
           05  DB2-ACT                     PIC X(6)  VALUE SPACES.
               88 DAINSERT                           VALUE 'INSERT'.
               88 DADELETE                           VALUE 'DELETE'.
               88 DAUPDATE                           VALUE 'UPDATE'.
           05  DB2M                        PIC X(18)
                VALUE IS ' COMPLETE - #ROWS '.
           05  DB2M-VAR                    PIC X(10).
           05  FILLER                      PIC X(2)  VALUE SPACES.
           05  DB2CODE                     PIC -(9)9.
           05  FILLER                      PIC X(11)
                VALUE IS SPACES.

       01  INITAPI-ERR.
           05  INITAPI-ERR-M               PIC X(35)
                VALUE IS 'INITAPI FAILED - SERVER NOT STARTED'.
           05  FILLER                      PIC X(9)
                VALUE IS ' ERRNO = '.
           05  INIT-ERRNO                  PIC 9(8) DISPLAY.
           05  FILLER                      PIC X(3)
                VALUE IS SPACES.

       01  LISTEN-ERR.
           05  LISTEN-ERR-M                PIC X(25)
                VALUE IS 'SOCKET CALL FAIL - LISTEN'.
           05  FILLER                      PIC X(9)
                VALUE IS ' ERRNO = '.
           05  LISTEN-ERRNO                PIC 9(8) DISPLAY.
           05  FILLER                      PIC X(13)
                VALUE IS SPACES.

       01  LISTEN-SUCC.
           05  FILLER                      PIC X(34)
                VALUE IS 'READY TO ACCEPT REQUEST ON PORT:  '.
           05  BIND-PORT                   PIC X(4).
           05  FILLER                      PIC X(10)  VALUE SPACES.
           05  FILLER                      PIC X(7)
                VALUE IS SPACES.

       01  PORTNUM-ERR.
           05  INVALID-PORT                PIC X(33)
                VALUE IS 'SERVER NOT STARTED - INVALID PORT'.
           05  FILLER                      PIC X(10)
                VALUE IS ' NUMBER = '.
           05  PORT-ERRNUM                 PIC X(4).
           05  FILLER                      PIC X(8)
                VALUE IS SPACES.

       01  RECVFROM-ERR.
           05  RECVFROM-ERR-M              PIC X(24)
                VALUE IS 'RECEIVE SOCKET CALL FAIL'.
           05  FILLER                      PIC X(9)
                VALUE IS ' ERRNO = '.
           05  RECVFROM-ERRNO              PIC 9(8) DISPLAY.
           05  FILLER                      PIC X(14)
                VALUE IS SPACES.

       01  SELECT-ERR.
           05  SELECT-ERR-M                PIC X(24)
                VALUE IS 'SELECT CALL FAIL        '.
           05  FILLER                      PIC X(9)
                VALUE IS ' ERRNO = '.
           05  SELECT-ERRNO                PIC 9(8) DISPLAY.
           05  FILLER                      PIC X(14)
                VALUE IS SPACES.

       01  SQL-ERROR.
           05  FILLER                      PIC X(35)
                VALUE IS 'SQLERR -PROG TERMINATION,SQLCODE = '.
           05  SQL-ERR-CODE                PIC -(9)9.
           05  FILLER                      PIC X(11)
                VALUE IS SPACES.

       01  SOCKET-ERR.
           05  SOCKET-ERR-M                PIC X(25)
                VALUE IS 'SOCKET CALL FAIL - SOCKET'.
           05  FILLER                      PIC X(9)
                VALUE IS ' ERRNO = '.
           05  SOCKET-ERRNO                PIC 9(8) DISPLAY.
           05  FILLER                      PIC X(13)
                VALUE IS SPACES.

       01  TAKE-ERR.
           05  TAKE-ERR-M                  PIC X(17)
                VALUE IS 'TAKESOCKET FAILED'.
           05  FILLER                      PIC X(9)
                VALUE IS ' ERRNO = '.
           05  TAKE-ERRNO                  PIC 9(8) DISPLAY.
           05  FILLER                      PIC X(21)
                VALUE IS SPACES.

       01  WRITE-ERR.
           05  WRITE-ERR-M                 PIC X(33)
                VALUE IS 'WRITE SOCKET FAIL'.
           05  FILLER                      PIC X(9)
                VALUE IS ' ERRNO = '.
           05  WRITE-ERRNO                 PIC 9(8) DISPLAY.
           05  FILLER                      PIC X(21)
                VALUE IS SPACES.


      *---------------------------------------------------------------*
      *    PROGRAM'S CONSTANTS                                        *
      *---------------------------------------------------------------*

       77  BITMASK-TOKEN            PIC X(16) VALUE 'TCPIPBITMASKCOBL'.
       77  TOEBCDIC-TOKEN           PIC X(16) VALUE 'TCPIPTOEBCDICXLT'.
       77  TOASCII-TOKEN            PIC X(16) VALUE 'TCPIPTOASCIIXLAT'.
       77  CTOB                     PIC X(4)  VALUE 'CTOB'.
       77  DEL-ID                   PIC X(1)  VALUE ','.
       77  BACKLOG                  PIC 9(8)  COMP VALUE 5.
       77  NONZERO-FWRD             PIC 9(8)  VALUE 256.
       77  TCP-FLAG                 PIC 9(8)  COMP VALUE 0.
       77  SOCK-TYPE                PIC 9(8)  COMP VALUE 1.
       77  AF-INET6                 PIC 9(8)  COMP VALUE 19.
       77  NUM-FDS                  PIC 9(8)  COMP VALUE 5.
       77  LOM                      PIC 9(4)  COMP VALUE 4.
       77  CECI-LENG                PIC 9(8)  COMP VALUE 5.
       77  BUFFER-LENG              PIC 9(8)  COMP VALUE 55.
       77  GWLENG                   PIC 9(4)  COMP VALUE 256.
       77  DEFAULT-PORT             PIC X(4)  VALUE '????'.
           88  DEFAULT-SPECIFIED              VALUE '1950'.
       01  IN6ADDR-ANY.
           05 FILLER                PIC 9(16) BINARY VALUE 0.
           05 FILLER                PIC 9(16) BINARY VALUE 0.

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


      *---------------------------------------------------------------*
      *    PROGRAM'S VARIABLES                                        *
      *---------------------------------------------------------------*

       77  PROTOCOL                 PIC 9(8)  COMP VALUE 0.
       77  SRV-SOCKID               PIC 9(4)  COMP VALUE 0.
       77  SRV-SOCKID-FWD           PIC 9(8)  COMP VALUE 0.
       77  CLI-SOCKID               PIC 9(4)  COMP VALUE 0.
       77  CLI-SOCKID-FWD           PIC S9(8) COMP VALUE 0.
       77  L-DESC                   PIC 9(8)  COMP VALUE 0.
       77  LENG                     PIC 9(4)  COMP.
       77  WSLENG                   PIC 9(4)  COMP.
       77  RESPONSE                 PIC 9(9)  COMP.
       77  TSTAMP                   PIC 9(8).
       77  TASK-FLAG                PIC X(1)  VALUE '0'.
           88  TASK-END             VALUE '1'.
           88  TASK-TERM            VALUE '2'.
       77  GWPTR                    PIC S9(8) COMP.
       77  WSPTR                    PIC S9(8) COMP.
       77  TCP-INDICATOR            PIC X(1)  VALUE IS SPACE.
       77  TAKESOCKET-SWITCH        PIC X(1)  VALUE IS SPACE.
           88  DOTAKESOCKET         VALUE '1'.
       77  TCPLENG                  PIC 9(8)  COMP VALUE 0.
       77  ERRNO                    PIC 9(8)  COMP.
       77  RETCODE                  PIC S9(8) COMP.
       77  TRANS                    PIC X(4).

       01  CLIENTID-LSTN.
           05  CID-DOMAIN-LSTN      PIC 9(8)  COMP VALUE 19.
           05  CID-LSTN-INFO.
               10  CID-NAME-LSTN    PIC X(8).
               10  CID-SUBTNAM-LSTN PIC X(8).
           05  CID-RES-LSTN         PIC X(20) VALUE LOW-VALUES.

       01  INIT-SUBTASKID.
           05  SUBTASKNO            PIC X(7)  VALUE LOW-VALUES.
           05  SUBT-CHAR            PIC A(1)  VALUE 'L'.

       01  IDENT.
           05  TCPNAME              PIC X(8) VALUE 'TCPCS   '.
           05  ADSNAME              PIC X(8) VALUE 'EZACIC6S'.

       01  MAXSOC                   PIC 9(4) BINARY VALUE 0.
       01  MAXSNO                   PIC 9(8) BINARY VALUE 0.

       01  NFDS                     PIC 9(8) BINARY.

       01  PORT-RECORD.
           05  PORT                 PIC X(4).
           05  FILLER               PIC X(36).

       01  SELECT-CSOCKET.
           05  READMASK             PIC X(4)  VALUE LOW-VALUES.
           05  DUMYMASK             PIC X(4)  VALUE LOW-VALUES.
           05  REPLY-RDMASK         PIC X(4)  VALUE LOW-VALUES.
           05  REPLY-RDMASK-FF      PIC X(4).

       01 SOCKADDR-IN.
          05 SAIN-FAMILY                PIC 9(4) BINARY.
             88 SAIN-FAMILY-IS-AFINET   VALUE 2.
             88 SAIN-FAMILY-IS-AFINET6  VALUE 19.
          05 SAIN-DATA                  PIC X(26).
          05 SAIN-SIN REDEFINES SAIN-DATA.
             10 SAIN-SIN-PORT           PIC 9(4) BINARY.
             10 SAIN-SIN-ADDR           PIC 9(8) BINARY.
             10 FILLER                  PIC X(8).
             10 FILLER                  PIC X(12).
          05 SAIN-SIN6 REDEFINES SAIN-DATA.
             10 SAIN-SIN6-PORT          PIC 9(4) BINARY.
             10 SAIN-SIN6-FLOWINFO      PIC 9(8) BINARY.
             10 SAIN-SIN6-ADDR.
                15 FILLER               PIC 9(16) BINARY.
                15 FILLER               PIC 9(16) BINARY.
             10 SAIN-SIN6-SCOPEID       PIC 9(8) BINARY.

       01 SOCKADDR-PEER.
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

       01  NTOP-FAMILY                    PIC 9(8) BINARY.
       01  PTON-FAMILY                    PIC 9(8) BINARY.
       01  PRESENTABLE-ADDR               PIC X(45) VALUE SPACES.
       01  PRESENTABLE-ADDR-LEN           PIC 9(4) BINARY VALUE 45.
       01  NUMERIC-ADDR.
           05 FILLER                      PIC 9(16) BINARY VALUE 0.
           05 FILLER                      PIC 9(16) BINARY VALUE 0.

       01  NAME-LEN                       PIC 9(8) BINARY.
       01  HOST-NAME                      PIC X(255).
       01  HOST-NAME-LEN                  PIC 9(8) BINARY.
       01  SERVICE-NAME                   PIC X(32).
       01  SERVICE-NAME-LEN               PIC 9(8) BINARY.
       01  NAME-INFO-FLAGS                PIC 9(8) BINARY VALUE 0.
       01  NI-NOFQDN                      PIC 9(8) BINARY VALUE 1.
       01  NI-NUMERICHOST                 PIC 9(8) BINARY VALUE 2.
       01  NI-NAMEREQD                    PIC 9(8) BINARY VALUE 4.
       01  NI-NUMERICSERV                 PIC 9(8) BINARY VALUE 8.
       01  NI-DGRAM                       PIC 9(8) BINARY VALUE 16.

       01  HOST-NAME-CHAR-COUNT       PIC 9(4) COMP.
       01  HOST-NAME-UNSTRUNG         PIC X(255) VALUE SPACES.
       01  SERVICE-NAME-CHAR-COUNT    PIC 9(4) COMP.
       01  SERVICE-NAME-UNSTRUNG      PIC X(32) VALUE SPACES.

       01  SOCKET-CONV.
           05  SOCKET-TBL  OCCURS 6 TIMES.
               10  SOCK-CHAR        PIC X(1)  VALUE '0'.

       01  TCP-BUF.
           05  TCP-BUF-H            PIC X(3).
           05  TCP-BUF-DATA         PIC X(52).

       01  TCPCICS-MSG-AREA.
           02  TCPCICS-MSG-1.
               05  MSGDATE          PIC 9(8).
               05  FILLER           PIC X(2)  VALUE SPACES.
               05  MSGTIME          PIC 9(8).
               05  FILLER           PIC X(2)  VALUE SPACES.
               05  MODULE           PIC X(10) VALUE 'EZACIC6S: '.
           02  TCPCICS-MSG-2.
               05  MSG-AREA         PIC X(55) VALUE SPACES.

       01  TCP-INPUT-DATA              PIC X(85) VALUE LOW-VALUES.
       01  TCPSOCKET-PARM REDEFINES TCP-INPUT-DATA.
           05 GIVE-TAKE-SOCKET              PIC 9(8) COMP.
           05 CLIENTID-PARM.
              10 LSTN-NAME                  PIC X(8).
              10 LSTN-SUBTASKNAME           PIC X(8).
           05 CLIENT-DATA-FLD.
              10 CLIENT-IN-DATA             PIC X(35).
              10 FILLER                     PIC X(1).
           05 TCPSOCKADDR-IN.
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

       01  TIMEVAL.
           02 TVSEC                 PIC 9(8)  COMP VALUE 180.
           02 TVUSEC                PIC 9(8)  COMP VALUE 0.

       01  ZERO-PARM                PIC X(16) VALUE LOW-VALUES.
       01  ZERO-FLD REDEFINES ZERO-PARM.
           02  ZERO-8               PIC X(8).
           02  ZERO-DUM             PIC X(2).
           02  ZERO-HWRD            PIC 9(4)  COMP.
           02  ZERO-FWRD            PIC 9(8)  COMP.


      * *********************************************** *
      *  INPUT FORMAT FOR UPDATING THE SAMPLE DB2 TABLE *
      * *********************************************** *

       01  INPUT-DEPT.
           05  IN-ACT               PIC X(3).
           05  IN-DEPTNO            PIC X(3).
           05  IN-DEPTN             PIC X(36).
           05  IN-MGRNO             PIC X(6).
           05  IN-ADMRDEPT          PIC X(3).


      *---------------------------------------------------------------*
      *    SQL STATEMENTS:  SQL COMMUNICATION AREA                    *
      *---------------------------------------------------------------*

      ***  EXEC SQL INCLUDE SQLCA    END-EXEC.

      *---------------------------------------------------------------*
      *    SQL STATEMENTS:  DEPARTMENT TABLE CREATE STATEMENT FOR DB2 *
      *                                                               *
      *              CREATE TABLE TCPCICS.DEPT                        *
      *                     (DEPTNO       CHAR(03),                   *
      *                      DEPTNAME     CHAR(36),                   *
      *                      MGRNO        CHAR(06),                   *
      *                      ADMRDEPT     CHAR(03));                  *
      *                                                               *
      *---------------------------------------------------------------*
      *    DCLGEN GENERATED FROM DB2 FOR THE DEPARTMENT TABLE.        *
      *---------------------------------------------------------------*

      * ***EXEC SQL INCLUDE DCLDEPT  END-EXEC.

      ******************************************************************
      * DCLGEN TABLE(TCPCICS.DEPT)                                     *
      *        LIBRARY(SYSADM.CICS.SPUFI(DCLDEPT))                     *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
      ***  EXEC SQL DECLARE TCPCICS.DEPT TABLE
      ***  ( DEPTNO                         CHAR(3),
      ***    DEPTNAME                       CHAR(36),
      ***    MGRNO                          CHAR(6),
      ***    ADMRDEPT                       CHAR(3)
      ***  ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TCPCICS.DEPT                       *
      ******************************************************************
       01  DCLDEPT.
           10 DEPTNO               PIC X(3).
           10 DEPTNAME             PIC X(36).
           10 MGRNO                PIC X(6).
           10 ADMRDEPT             PIC X(3).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************


       PROCEDURE DIVISION.

      ***  EXEC SQL WHENEVER SQLERROR    GO TO SQL-ERROR-ROU END-EXEC.

      ***  EXEC SQL WHENEVER SQLWARNING  GO TO SQL-ERROR-ROU END-EXEC.

           EXEC CICS IGNORE CONDITION TERMERR
                                      EOC
                                      SIGNAL
           END-EXEC.

           EXEC CICS HANDLE CONDITION ENDDATA   (ENDDATA-SEC)
                                      IOERR     (IOERR-SEC)
                                      LENGERR   (LENGERR-SEC)
                                      NOSPACE   (NOSPACE-ERR-SEC)
                                      QIDERR    (QIDERR-SEC)
           END-EXEC.

           MOVE START-MSG                   TO MSG-AREA.
           PERFORM HANDLE-TCPCICS           THRU HANDLE-TCPCICS-EXIT.


      *---------------------------------------------------------------*
      *                                                               *
      *  BEFORE SERVER  STARTS, TRUE MUST BE ACTIVE.  ISSUE 'EXTRACT  *
      *  EXIT' COMMAND TO CHECK IF TRUE IS ACTIVE OR NOT              *
      *                                                               *
      *---------------------------------------------------------------*

           EXEC CICS PUSH HANDLE END-EXEC.

           EXEC CICS HANDLE CONDITION
                INVEXITREQ(TCP-TRUE-REQ)
           END-EXEC.

           EXEC CICS EXTRACT EXIT
                PROGRAM ('EZACIC01')
                GASET   (GWPTR)
                GALENGTH(GWLENG)
           END-EXEC.

           EXEC CICS POP HANDLE END-EXEC.


      *---------------------------------------------------------------*
      *                                                               *
      *  CICS ATTACH FACILITY MUST BE STARTED FOR THE APPROPRIATE DB2 *
      *  SUBSYSTEM BEFORE YOU EXECUTE CICS TRANSACTIONS REQUIRING     *
      *  ACCESS TO DB2 DATABASES.                                     *
      *                                                               *
      *---------------------------------------------------------------*

      *    EXEC CICS PUSH HANDLE END-EXEC.
      *
      *    EXEC CICS HANDLE CONDITION
      *         INVEXITREQ(DB2-TRUE-REQ)
      *    END-EXEC.
      *
      *    EXEC CICS EXTRACT EXIT
      *         PROGRAM   ('DSNCEXT1')
      *         ENTRYNAME ('DSNCSQL')
      *         GASET     (WSPTR)
      *         GALENGTH  (WSLENG)
      *    END-EXEC.
      *
      *    EXEC CICS POP HANDLE END-EXEC.
      *
      *
      *---------------------------------------------------------------*
      *                                                               *
      *  AT START UP THE SERVER REQUIRES THE PORT NUMBER FOR TCP/IP   *
      *  IT WILL USE.  THE PORT NUMBER SUPPORTED BY THIS SAMPLE IS    *
      *  4 DIGITS IN LENGTH.                                          *
      *                                                               *
      *  INVOCATION:  <server>,<port number>                          *
      *   LISTENER => SRV2,4000  - OR -  SRV2,4    -                  *
      *   CECI     => CECI START TR(SRV2) FROM(4000)                  *
      *                                                               *
      *  THE LEADING SPACES ARE SIGNIFICANT.                          *
      *                                                               *
      *---------------------------------------------------------------*

           MOVE EIBTRNID                    TO TRANS.

           EXEC CICS RETRIEVE
                INTO   (TCP-INPUT-DATA)
                LENGTH (LENG)
           END-EXEC.

      * ************************************************************* *
      * THE PORT CAN SPECIFIED IN THE FROM(????) OPTION OF THE CECI   *
      * COMMAND OR THE DEFAULT PORT IS USED.                          *
      * THE PORT FOR THE LISTENER STARTED SERVER IS THE PORT          *
      * SPECIFIED IN THE CLIENT-DATA-FLD OR THE DEFAULT PORT          *
      * IS USED.                                                      *
      * ************************************************************* *
      *        THE DEFAULT PORT MUST BE SET, BY THE PROGRAMMER.       *
      * ************************************************************* *

           IF LENG < CECI-LENG
              THEN MOVE TCP-INPUT-DATA      TO PORT
              ELSE
                MOVE CLIENT-DATA-FLD        TO PORT-RECORD
                MOVE '1'                    TO TAKESOCKET-SWITCH
           END-IF.

           INSPECT PORT REPLACING LEADING SPACES BY '0'.
           IF PORT IS NUMERIC
              THEN MOVE PORT                TO BIND-PORT
              ELSE
                IF DEFAULT-SPECIFIED
                   THEN  MOVE DEFAULT-PORT  TO PORT
                                               BIND-PORT
                   ELSE
                     MOVE PORT              TO PORT-ERRNUM
                     MOVE PORTNUM-ERR       TO MSG-AREA
                     PERFORM HANDLE-TCPCICS THRU HANDLE-TCPCICS-EXIT
                     GO TO PGM-EXIT
                END-IF
           END-IF.

           IF DOTAKESOCKET
              THEN PERFORM LISTENER-STARTED-TASK THRU
                      LISTENER-STARTED-TASK-EXIT
              ELSE PERFORM INIT-SOCKET           THRU
                      INIT-SOCKET-EXIT
           END-IF.

           PERFORM SCKET-BIND-LSTN          THRU SCKET-BIND-LSTN-EXIT.

           MOVE 2                           TO CLI-SOCKID
                                               CLI-SOCKID-FWD.

           MOVE LISTEN-SUCC                 TO MSG-AREA.

           PERFORM HANDLE-TCPCICS           THRU HANDLE-TCPCICS-EXIT.

           COMPUTE NFDS = NUM-FDS + 1.

           MOVE LOW-VALUES                  TO READMASK.
           MOVE 6                           TO TCPLENG.

           CALL 'EZACIC06' USING BITMASK-TOKEN
                                 CTOB
                                 READMASK
                                 SOCKET-CONV
                                 TCPLENG
                                 RETCODE.

           IF RETCODE = -1
              THEN
                MOVE BITMASK-ERR            TO MSG-AREA
                PERFORM HANDLE-TCPCICS      THRU HANDLE-TCPCICS-EXIT
              ELSE
                PERFORM ACCEPT-CLIENT-REQ   THRU
                        ACCEPT-CLIENT-REQ-EXIT
                        UNTIL TASK-TERM
           END-IF.

           PERFORM CLOSE-SOCKET             THRU CLOSE-SOCKET-EXIT.

           MOVE TCP-SERVER-OFF              TO MSG-AREA.

           PERFORM HANDLE-TCPCICS           THRU HANDLE-TCPCICS-EXIT.

      *---------------------------------------------------------------*
      *                                                               *
      *    END OF PROGRAM                                             *
      *                                                               *
      *---------------------------------------------------------------*


       PGM-EXIT.

           EXEC CICS
                RETURN
           END-EXEC.

           GOBACK.


      *---------------------------------------------------------------*
      *                                                               *
      *          TRUE IS NOT ENABLED                                  *
      *                                                               *
      *---------------------------------------------------------------*

       TCP-TRUE-REQ.
           MOVE TCP-EXIT-ERR      TO MSG-AREA.
           PERFORM HANDLE-TCPCICS THRU HANDLE-TCPCICS-EXIT.
           GO TO PGM-EXIT.


      *---------------------------------------------------------------*
      *                                                               *
      *          DB2 CALL ATTACH FACILITY IS NOT ENABLED              *
      *                                                               *
      *---------------------------------------------------------------*

       DB2-TRUE-REQ.
           MOVE DB2-CAF-ERR       TO MSG-AREA.
           PERFORM HANDLE-TCPCICS THRU HANDLE-TCPCICS-EXIT.
           GO TO PGM-EXIT.


      *---------------------------------------------------------------*
      *                                                               *
      *  LISTENER STARTED TASK                                        *
      *                                                               *
      *---------------------------------------------------------------*

       LISTENER-STARTED-TASK.

           MOVE CLIENTID-PARM               TO CID-LSTN-INFO.
           MOVE -1 TO L-DESC.

           CALL 'EZASOKET' USING SOKET-TAKESOCKET
                                 GIVE-TAKE-SOCKET
                                 CLIENTID-LSTN
                                 ERRNO
                                 RETCODE.

           IF RETCODE < 0
              THEN
                MOVE ERRNO                  TO TAKE-ERRNO
                MOVE TAKE-ERR               TO MSG-AREA
                PERFORM HANDLE-TCPCICS      THRU HANDLE-TCPCICS-EXIT
                GO TO PGM-EXIT
              ELSE
                MOVE BUFFER-LENG            TO TCPLENG
                MOVE START-MSG              TO TCP-BUF
                MOVE RETCODE                TO SRV-SOCKID

                CALL 'EZACIC04' USING TOASCII-TOKEN TCP-BUF TCPLENG

                CALL 'EZASOKET' USING SOKET-WRITE
                                      SRV-SOCKID
                                      TCPLENG
                                      TCP-BUF
                                      ERRNO
                                      RETCODE

                IF RETCODE < 0
                   THEN
                     MOVE ERRNO             TO WRITE-ERRNO
                     MOVE WRITE-ERR         TO MSG-AREA
                     PERFORM HANDLE-TCPCICS THRU
                             HANDLE-TCPCICS-EXIT
                     GO TO PGM-EXIT
                   ELSE

                     CALL 'EZASOKET' USING SOKET-CLOSE
                                           SRV-SOCKID
                                           ERRNO
                                           RETCODE

                     IF RETCODE < 0
                        THEN
                          MOVE ERRNO        TO CLOSE-ERRNO
                          MOVE CLOSE-ERR    TO MSG-AREA
                          PERFORM HANDLE-TCPCICS  THRU
                                  HANDLE-TCPCICS-EXIT
                          GO TO PGM-EXIT
                        ELSE NEXT SENTENCE
                     END-IF
                 END-IF
           END-IF.

           MOVE LOW-VALUES                  TO TCP-BUF.


       LISTENER-STARTED-TASK-EXIT.
           EXIT.


      *---------------------------------------------------------------*
      *                                                               *
      *  START SERVER  PROGRAM                                        *
      *                                                               *
      *---------------------------------------------------------------*

       INIT-SOCKET.

           MOVE EIBTASKN                TO SUBTASKNO.

           CALL 'EZASOKET' USING SOKET-INITAPI
                                 MAXSOC
                                 IDENT
                                 INIT-SUBTASKID
                                 MAXSNO
                                 ERRNO
                                 RETCODE.

           IF RETCODE < 0
              THEN
                MOVE ERRNO              TO INIT-ERRNO
                MOVE INITAPI-ERR        TO MSG-AREA
                PERFORM HANDLE-TCPCICS  THRU HANDLE-TCPCICS-EXIT
                GO TO PGM-EXIT
              ELSE
                MOVE INIT-MSG           TO MSG-AREA
                PERFORM HANDLE-TCPCICS  THRU HANDLE-TCPCICS-EXIT
              END-IF.


       INIT-SOCKET-EXIT.
           EXIT.


       SCKET-BIND-LSTN.

           MOVE  -1                    TO SRV-SOCKID-FWD.

      *--------------------------------------------------------------*
      *                                                              *
      *   CREATING A SOCKET TO ALLOCATE                              *
      *   AN OPEN SOCKET FOR INCOMING CONNECTIONS                    *
      *                                                              *
      *--------------------------------------------------------------*

           CALL 'EZASOKET' USING SOKET-SOCKET
                                 AF-INET6
                                 SOCK-TYPE
                                 PROTOCOL
                                 ERRNO
                                 RETCODE.


           IF RETCODE < 0
              THEN
                MOVE ERRNO             TO SOCKET-ERRNO
                MOVE SOCKET-ERR        TO MSG-AREA
                PERFORM HANDLE-TCPCICS THRU HANDLE-TCPCICS-EXIT
                GO TO PGM-EXIT
              ELSE MOVE RETCODE        TO SRV-SOCKID
                   MOVE  '1' TO SOCK-CHAR(RETCODE + 1)
           END-IF.

      *--------------------------------------------------------------*
      *                                                              *
      *  BIND THE SOCKET TO THE SERVICE PORT                         *
      *  TO ESTABLISH A LOCAL ADDRESS FOR PROCESSING INCOMING        *
      *  CONNECTIONS.                                                *
      *                                                              *
      *--------------------------------------------------------------*

           MOVE AF-INET6               TO SAIN-FAMILY.
           MOVE ZEROS                  TO SAIN-SIN6-FLOWINFO.
           MOVE IN6ADDR-ANY            TO SAIN-SIN6-ADDR.
           MOVE ZEROS                  TO SAIN-SIN6-SCOPEID.
           MOVE PORT                   TO SAIN-SIN6-PORT.

           CALL 'EZASOKET' USING SOKET-BIND
                                 SRV-SOCKID
                                 SOCKADDR-IN
                                 ERRNO
                                 RETCODE.

           IF RETCODE < 0 THEN
              MOVE ERRNO               TO BIND-ERRNO
              MOVE BIND-ERR            TO MSG-AREA
              PERFORM HANDLE-TCPCICS   THRU HANDLE-TCPCICS-EXIT
              GO TO PGM-EXIT.

      *--------------------------------------------------------------*
      *                                                              *
      *  CALL THE LISTEN COMMAND TO ALLOWS SERVERS TO                *
      *  PREPARE A SOCKET FOR INCOMING CONNECTIONS AND SET MAXIMUM   *
      *  CONNECTIONS.                                                *
      *                                                              *
      *--------------------------------------------------------------*

           CALL 'EZASOKET' USING SOKET-LISTEN
                                 SRV-SOCKID
                                 BACKLOG
                                 ERRNO
                                 RETCODE.

           IF RETCODE < 0 THEN
              MOVE ERRNO               TO LISTEN-ERRNO
              MOVE LISTEN-ERR          TO MSG-AREA
              PERFORM HANDLE-TCPCICS   THRU HANDLE-TCPCICS-EXIT
              GO TO PGM-EXIT.


       SCKET-BIND-LSTN-EXIT.
           EXIT.


      *--------------------------------------------------------------*
      *                                                              *
      *  SOCKET HAS BEEN SET UP, THEN CALL 'ACCEPT' TO               *
      *  ACCEPT A REQUEST WHEN A CONNECTION ARRIVES.                 *
      *                                                              *
      *  THIS SAMPLE PROGRAM WILL ONLY USE 5 SOCKETS.                *
      *                                                              *
      *--------------------------------------------------------------*

       ACCEPT-CLIENT-REQ.

           CALL 'EZASOKET' USING SOKET-SELECT
                                 NFDS
                                 TIMEVAL
                                 READMASK
                                 DUMYMASK
                                 DUMYMASK
                                 REPLY-RDMASK
                                 DUMYMASK
                                 DUMYMASK
                                 ERRNO
                                 RETCODE.

           IF RETCODE < 0
              THEN
                MOVE ERRNO             TO SELECT-ERRNO
                MOVE SELECT-ERR        TO MSG-AREA
                PERFORM HANDLE-TCPCICS THRU HANDLE-TCPCICS-EXIT
                GO TO PGM-EXIT.

           IF RETCODE = 0
              THEN GO TO ACCEPT-CLIENT-REQ-EXIT.

      *--------------------------------------------------------------*
      *                                                              *
      *  ACCEPT REQUEST                                              *
      *                                                              *
      *--------------------------------------------------------------*

           CALL 'EZASOKET' USING SOKET-ACCEPT
                                 SRV-SOCKID
                                 SOCKADDR-IN
                                 ERRNO
                                 RETCODE.

           IF RETCODE < 0 THEN
              MOVE ERRNO               TO ACCEPT-ERRNO
              MOVE ACCEPT-ERR          TO MSG-AREA
              PERFORM HANDLE-TCPCICS   THRU HANDLE-TCPCICS-EXIT
              GO TO PGM-EXIT.

           MOVE RETCODE TO CLI-SOCKID.

           PERFORM GET-NAME-INFO       THRU GET-NAME-INFO-EXIT.

           PERFORM ACCEPT-RECV         THRU ACCEPT-RECV-EXIT
                   UNTIL TASK-END OR TASK-TERM.

           MOVE DB2END                 TO MSG-AREA.

           PERFORM HANDLE-TCPCICS      THRU HANDLE-TCPCICS-EXIT.

           CALL 'EZASOKET' USING SOKET-CLOSE
                                 CLI-SOCKID
                                 ERRNO
                                 RETCODE.

           IF RETCODE < 0 THEN
              MOVE ERRNO               TO CLOSE-ERRNO
              MOVE CLOSE-ERR           TO MSG-AREA
              PERFORM HANDLE-TCPCICS   THRU HANDLE-TCPCICS-EXIT.

           IF NOT TASK-TERM
              MOVE '0'                 TO TASK-FLAG.


       ACCEPT-CLIENT-REQ-EXIT.
           EXIT.

      *--------------------------------------------------------------*
      *                                                              *
      *  DETERMINE THE CONNECTED HOST NAME BY ISSUING THE            *
      *  GETNAMEINFO COMMAND.                                        *
      *                                                              *
      *--------------------------------------------------------------*

       GET-NAME-INFO.

           MOVE SAIN-SIN6-ADDR TO NUMERIC-ADDR.

           MOVE 45 TO PRESENTABLE-ADDR-LEN.
           MOVE SPACES TO PRESENTABLE-ADDR.

           CALL 'EZASOKET' USING SOKET-NTOP AF-INET6
              NUMERIC-ADDR
              PRESENTABLE-ADDR PRESENTABLE-ADDR-LEN
              ERRNO RETCODE.

           IF RETCODE < 0 THEN
              MOVE ERRNO               TO NTOP-ERRNO
              MOVE NTOP-ERR            TO MSG-AREA
              PERFORM HANDLE-TCPCICS   THRU HANDLE-TCPCICS-EXIT.

           MOVE PRESENTABLE-ADDR       TO NTOP-PRESENTABLE-ADDR.
           MOVE NTOP-OK                TO MSG-AREA.
           PERFORM HANDLE-TCPCICS   THRU HANDLE-TCPCICS-EXIT.

           CALL 'EZASOKET' USING SOKET-GETPEERNAME
                                 CLI-SOCKID
                                 SOCKADDR-PEER
                                 ERRNO
                                 RETCODE.

           IF RETCODE < 0 THEN
              MOVE ERRNO               TO GPN-ERRNO
              MOVE GPN-ERR             TO MSG-AREA
              PERFORM HANDLE-TCPCICS   THRU HANDLE-TCPCICS-EXIT
              GO TO PGM-EXIT.

           MOVE 28 TO NAME-LEN.
           MOVE 255 TO HOST-NAME-LEN.
           MOVE 32 TO SERVICE-NAME-LEN.
           MOVE ZEROS TO NAME-INFO-FLAGS.

           CALL 'EZASOKET' USING SOKET-GETNAMEINFO
                                 SOCKADDR-PEER
                                 NAME-LEN
                                 HOST-NAME
                                 HOST-NAME-LEN
                                 SERVICE-NAME
                                 SERVICE-NAME-LEN
                                 NAME-INFO-FLAGS
                                 ERRNO
                                 RETCODE.

           IF RETCODE < 0 THEN
              MOVE ERRNO               TO GNI-ERRNO
              MOVE GNI-ERR             TO MSG-AREA
              PERFORM HANDLE-TCPCICS   THRU HANDLE-TCPCICS-EXIT.

           MOVE 0 TO HOST-NAME-CHAR-COUNT.
           INSPECT HOST-NAME TALLYING HOST-NAME-CHAR-COUNT
              FOR CHARACTERS BEFORE X'00'.
           UNSTRING HOST-NAME DELIMITED BY X'00'
              INTO HOST-NAME-UNSTRUNG
              COUNT IN HOST-NAME-CHAR-COUNT.
           STRING HOST-NAME-UNSTRUNG DELIMITED BY ' '
              INTO GNI-HOST-NAME.
           MOVE GNI-HOST-NAME-OK       TO MSG-AREA.
           PERFORM HANDLE-TCPCICS      THRU HANDLE-TCPCICS-EXIT.

           MOVE 0 TO SERVICE-NAME-CHAR-COUNT.
           INSPECT SERVICE-NAME TALLYING SERVICE-NAME-CHAR-COUNT
              FOR CHARACTERS BEFORE X'00'.
           UNSTRING SERVICE-NAME DELIMITED BY X'00'
              INTO SERVICE-NAME-UNSTRUNG
              COUNT IN SERVICE-NAME-CHAR-COUNT.
           STRING SERVICE-NAME-UNSTRUNG DELIMITED BY ' '
              INTO GNI-SERVICE-NAME.
           MOVE GNI-SERVICE-NAME-OK    TO MSG-AREA.
           PERFORM HANDLE-TCPCICS      THRU HANDLE-TCPCICS-EXIT.

           DISPLAY 'HOST NAME = ' HOST-NAME.
           DISPLAY 'SERVICE = ' SERVICE-NAME.

       GET-NAME-INFO-EXIT.
           EXIT.

      *--------------------------------------------------------------*
      *                                                              *
      *  RECEIVING DATA THROUGH A SOCKET BY ISSUING 'RECVFROM'       *
      *  COMMAND.                                                    *
      *                                                              *
      *--------------------------------------------------------------*

       ACCEPT-RECV.

           MOVE 'T'                                TO TCP-INDICATOR.
           MOVE BUFFER-LENG                        TO TCPLENG.
           MOVE LOW-VALUES                         TO TCP-BUF.

           CALL 'EZASOKET' USING SOKET-RECVFROM
                                 CLI-SOCKID
                                 TCP-FLAG
                                 TCPLENG
                                 TCP-BUF
                                 SOCKADDR-IN
                                 ERRNO
                                 RETCODE.

           IF RETCODE EQUAL 0 AND TCPLENG EQUAL 0
              THEN NEXT SENTENCE
              ELSE
                IF RETCODE < 0
                   THEN
                     MOVE ERRNO                    TO RECVFROM-ERRNO
                     MOVE RECVFROM-ERR             TO MSG-AREA
                     PERFORM HANDLE-TCPCICS        THRU
                             HANDLE-TCPCICS-EXIT
                     MOVE '1'                      TO TASK-FLAG
                   ELSE
                     CALL 'EZACIC05' USING TOEBCDIC-TOKEN
                                           TCP-BUF
                                           TCPLENG
                     IF TCP-BUF-H = LOW-VALUES OR SPACES
                        THEN
                          MOVE NULL-DATA           TO MSG-AREA
                          PERFORM HANDLE-TCPCICS   THRU
                                  HANDLE-TCPCICS-EXIT
                        ELSE
                          IF TCP-BUF-H =  'END'
                             THEN MOVE '1'         TO TASK-FLAG
                             ELSE IF TCP-BUF-H = 'TRM'
                                     THEN MOVE '2' TO TASK-FLAG
                                     ELSE PERFORM TALK-CLIENT THRU
                                                  TALK-CLIENT-EXIT
                                  END-IF
                          END-IF
                     END-IF
                END-IF
           END-IF.


       ACCEPT-RECV-EXIT.
           EXIT.


      **********************************************************
      **    PROCESSES TALKING TO CLIENT THAT WILL UPDATE DB2  **
      **    TABLES.                                           **
      **********************************************************
      **    DATA PROCESS:                                     **
      **                                                      **
      **    INSERT REC -  INS,X81,TEST DEPT,A0213B,Y94        **
      **    UPDATE REC -  UPD,X81,,A1234C,                    **
      **    DELETE REC -  DEL,X81,,,                          **
      **    END CLIENT -  END,{end client connection     }    **
      **    END SERVER -  TRM,{terminate server          }    **
      **                                                      **
      **********************************************************

       TALK-CLIENT.


           UNSTRING TCP-BUF DELIMITED BY DEL-ID OR ALL '*'
               INTO IN-ACT
                    IN-DEPTNO
                    IN-DEPTN
                    IN-MGRNO
                    IN-ADMRDEPT.

           IF IN-ACT EQUAL 'END'
              THEN
                MOVE '1'                              TO TASK-FLAG
              ELSE
                IF IN-ACT EQUAL 'U' OR EQUAL 'UPD'
                   THEN
      ***            EXEC SQL UPDATE TCPCICS.DEPT
      ***              SET    MGRNO  = :IN-MGRNO
      ***              WHERE  DEPTNO = :IN-DEPTNO
      ***            END-EXEC
                     MOVE 'UPDATE'                    TO DB2-ACT
                     MOVE 'UPDATED:  '                TO DB2M-VAR
                   ELSE
                     IF IN-ACT EQUAL 'I' OR EQUAL 'INS'
                        THEN
      ***                 EXEC SQL INSERT
      ***                   INTO TCPCICS.DEPT (DEPTNO,     DEPTNAME,
      ***                                      MGRNO,      ADMRDEPT)
      ***                   VALUES           (:IN-DEPTNO, :IN-DEPTN,
      ***                                     :IN-MGRNO,  :IN-ADMRDEPT)
      ***                 END-EXEC
                          MOVE 'INSERT'               TO DB2-ACT
                          MOVE 'INSERTED: '           TO DB2M-VAR
                        ELSE
                          IF IN-ACT EQUAL 'D' OR EQUAL 'DEL'
                             THEN
      ***                      EXEC SQL DELETE
      ***                        FROM  TCPCICS.DEPT
      ***                        WHERE DEPTNO = :IN-DEPTNO
      ***                      END-EXEC
                               MOVE 'DELETE'          TO DB2-ACT
                               MOVE 'DELETED: '       TO DB2M-VAR
                             ELSE
                               MOVE KEYWORD-ERR       TO MSG-AREA
                               PERFORM HANDLE-TCPCICS THRU
                                       HANDLE-TCPCICS-EXIT
                          END-IF
                     END-IF
                END-IF
           END-IF.

           IF DADELETE OR DAINSERT OR DAUPDATE
              THEN
      *         MOVE SQLERRD(3)                       TO DB2CODE
                MOVE DB2MSG                           TO MSG-AREA
                MOVE LENGTH OF TCPCICS-MSG-AREA       TO LENG

                EXEC CICS SYNCPOINT END-EXEC

                EXEC CICS WRITEQ TD
                     QUEUE    ('CSMT')
                     FROM     (TCPCICS-MSG-AREA)
                     LENGTH   (LENG)
                     NOHANDLE
                END-EXEC

      **********************************************************
      **           WRITE THE DB2 MESSAGE TO CLIENT.           **
      **********************************************************

                MOVE TCPCICS-MSG-2                    TO TCP-BUF

                CALL 'EZACIC04' USING TOASCII-TOKEN TCP-BUF TCPLENG

                CALL 'EZASOKET' USING SOKET-WRITE
                                      CLI-SOCKID
                                      TCPLENG
                                      TCP-BUF
                                      ERRNO
                                      RETCODE

                MOVE LOW-VALUES                       TO TCP-BUF
                                                         TCP-INDICATOR
                                                         DB2-ACT

                IF RETCODE < 0
                   THEN
                     MOVE ERRNO                       TO WRITE-ERRNO
                     MOVE WRITE-ERR                   TO MSG-AREA
                     PERFORM HANDLE-TCPCICS           THRU
                             HANDLE-TCPCICS-EXIT
                     MOVE '1'                         TO TASK-FLAG
                END-IF
           END-IF.


       TALK-CLIENT-EXIT.
           EXIT.


      *---------------------------------------------------------------*
      *                                                               *
      *   CLOSE ORIGINAL SOCKET DESCRIPTOR                            *
      *                                                               *
      *---------------------------------------------------------------*

       CLOSE-SOCKET.

           CALL 'EZASOKET' USING SOKET-CLOSE
                                 SRV-SOCKID
                                 ERRNO
                                 RETCODE.

           IF RETCODE < 0 THEN
              MOVE ERRNO             TO CLOSE-ERRNO
              MOVE CLOSE-ERR         TO MSG-AREA
              PERFORM HANDLE-TCPCICS THRU HANDLE-TCPCICS-EXIT.


       CLOSE-SOCKET-EXIT.
           EXIT.


      *---------------------------------------------------------------*
      *                                                               *
      *  SEND TCP/IP ERROR MESSAGE                                    *
      *                                                               *
      *---------------------------------------------------------------*

       HANDLE-TCPCICS.

           MOVE LENGTH OF TCPCICS-MSG-AREA TO LENG.

           EXEC CICS ASKTIME
                ABSTIME (TSTAMP)
                NOHANDLE
           END-EXEC.

           EXEC CICS FORMATTIME
                ABSTIME (TSTAMP)
                MMDDYY  (MSGDATE)
                TIME    (MSGTIME)
                DATESEP ('/')
                TIMESEP (':')
                NOHANDLE
           END-EXEC.

           EXEC CICS WRITEQ TD
                QUEUE  ('CSMT')
                FROM   (TCPCICS-MSG-AREA)
                RESP   (RESPONSE)
                LENGTH (LENG)
           END-EXEC.

           IF RESPONSE = DFHRESP(NORMAL)
              THEN NEXT SENTENCE
              ELSE
                IF RESPONSE = DFHRESP(INVREQ)
                   THEN MOVE TS-INVREQ-ERR          TO MSG-AREA
                   ELSE
                     IF RESPONSE = DFHRESP(NOTAUTH)
                        THEN MOVE TS-NOTAUTH-ERR    TO MSG-AREA
                        ELSE
                          IF RESPONSE = DFHRESP(IOERR)
                             THEN MOVE TS-IOERR-ERR TO MSG-AREA
                             ELSE MOVE WRITETS-ERR  TO MSG-AREA
                          END-IF
                     END-IF
                END-IF
           END-IF.

           IF TCP-INDICATOR = 'T' THEN
              MOVE BUFFER-LENG            TO TCPLENG
              MOVE LOW-VALUES             TO TCP-BUF
              MOVE TCPCICS-MSG-2          TO TCP-BUF

              CALL 'EZACIC04' USING TOASCII-TOKEN TCP-BUF TCPLENG

              MOVE ' '                    TO TCP-INDICATOR

              CALL 'EZASOKET' USING SOKET-WRITE
                                    CLI-SOCKID
                                    TCPLENG
                                    TCP-BUF
                                    ERRNO
                                    RETCODE

              IF RETCODE < 0
                 THEN
                   MOVE ERRNO             TO WRITE-ERRNO
                   MOVE WRITE-ERR         TO MSG-AREA

                   EXEC CICS WRITEQ TD
                        QUEUE  ('CSMT')
                        FROM   (TCPCICS-MSG-AREA)
                        LENGTH (LENG)
                        NOHANDLE
                   END-EXEC

                   IF TASK-TERM OR TASK-END
                      THEN NEXT SENTENCE
                      ELSE MOVE '1'       TO  TASK-FLAG
                   END-IF
              END-IF.

           MOVE SPACES                    TO MSG-AREA.


       HANDLE-TCPCICS-EXIT.
           EXIT.


      *---------------------------------------------------------------*
      *                                                               *
      *  SEND DB2    ERROR MESSAGE                                    *
      *                                                               *
      *---------------------------------------------------------------*

       SQL-ERROR-ROU.

      *    MOVE SQLCODE         TO SQL-ERR-CODE.
           MOVE SPACES          TO MSG-AREA.
      *    MOVE SQL-ERROR       TO MSG-AREA.

           EXEC CICS WRITEQ TD
                QUEUE  ('CSMT')
                FROM   (TCPCICS-MSG-AREA)
                RESP   (RESPONSE)
                LENGTH (LENG)
           END-EXEC.

           MOVE LOW-VALUES      TO TCP-BUF.
           MOVE TCPCICS-MSG-2   TO TCP-BUF.

           CALL 'EZACIC04' USING TOASCII-TOKEN TCP-BUF TCPLENG.

           CALL 'EZASOKET' USING SOKET-WRITE
                                 CLI-SOCKID
                                 TCPLENG
                                 TCP-BUF
                                 ERRNO
                                 RETCODE.

           IF RETCODE < 0 THEN
              MOVE ERRNO        TO WRITE-ERRNO
              MOVE WRITE-ERR    TO MSG-AREA
              PERFORM HANDLE-TCPCICS THRU HANDLE-TCPCICS-EXIT.

           GO TO PGM-EXIT.


       SQL-ERROR-ROU-EXIT.
           EXIT.


      *---------------------------------------------------------------*
      *                                                               *
      *  OTHER ERRORS (HANDLE CONDITION)                              *
      *                                                               *
      *---------------------------------------------------------------*

       INVREQ-ERR-SEC.
           MOVE TCP-EXIT-ERR      TO MSG-AREA.
           PERFORM HANDLE-TCPCICS THRU HANDLE-TCPCICS-EXIT.
           GO TO PGM-EXIT.
       IOERR-SEC.
           MOVE IOERR-ERR         TO MSG-AREA.
           PERFORM HANDLE-TCPCICS THRU HANDLE-TCPCICS-EXIT.
           GO TO PGM-EXIT.
       LENGERR-SEC.
           MOVE LENGERR-ERR       TO MSG-AREA.
           PERFORM HANDLE-TCPCICS THRU HANDLE-TCPCICS-EXIT.
           GO TO PGM-EXIT.
       NOSPACE-ERR-SEC.
           MOVE NOSPACE-ERR       TO MSG-AREA.
           PERFORM HANDLE-TCPCICS THRU HANDLE-TCPCICS-EXIT.
           GO TO PGM-EXIT.
       QIDERR-SEC.
           MOVE QIDERR-ERR        TO MSG-AREA.
           PERFORM HANDLE-TCPCICS THRU HANDLE-TCPCICS-EXIT.
           GO TO PGM-EXIT.
       ITEMERR-SEC.
           MOVE ITEMERR-ERR       TO MSG-AREA.
           PERFORM HANDLE-TCPCICS THRU HANDLE-TCPCICS-EXIT.
           GO TO PGM-EXIT.
       ENDDATA-SEC.
           MOVE ENDDATA-ERR       TO MSG-AREA.
           PERFORM HANDLE-TCPCICS THRU HANDLE-TCPCICS-EXIT.
           GO TO PGM-EXIT.