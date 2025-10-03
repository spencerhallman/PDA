      ******************************************************************
      * PRODUCT DEMONSTRATION APPLICATION (PDA)                        *
      *                                                                *
      * SPECIAL SCENARIO WORKING STORAGE AREAS                         *
      *                                                                *
      ******************************************************************

      ******************************************************************
      * THIS IS AN AREA THAT DEFINES THE SPECIAL SCENARIOS THAT CAN BE *
      * USED IN THE PDA SYSTEM.                                        *
      ******************************************************************

       01  PDA-SCENARIO-WORK-AREA.
           03  PDA-SWA-MAX-ENTRIES     PIC 9(3)  VALUE 250.
           03  PDA-SWA-SUB             PIC 9(3)  VALUE 0.
           03  PDA-SWA-SCENARIO        PIC X(5)  VALUE SPACES.
           03  PDA-SWA-PROGRAM         PIC X(6)  VALUE SPACES.
           03  PDA-SWA-NAME            PIC X(50) VALUE SPACES.
           03  PDA-SWA-FOUND           PIC X     VALUE 'N'.
           03  PDA-SWA-SCENARIO-LIST.
               05  FILLER              PIC X(61) VALUE
        '00001PDA014COBOL,ABEND ASRA,BAD DATA ON VSAM FILE-S0C7       '.
               05  FILLER              PIC X(61) VALUE
        '00002PDA006COBOL,STORAGE OVERLAY,TABLE OVERFLOW              '.
               05  FILLER              PIC X(61) VALUE
        '00003PDA004COBOL,ABEND AEIM,UNHANDLED NOTFOUND ON VSAM READ  '.
               05  FILLER              PIC X(61) VALUE
        '00004PDA007COBOL,ABEND AEIP,VSAM UPDATE,NO UPDATE CAPABILITY '.
               05  FILLER              PIC X(61) VALUE
        '00005PDA006COBOL,DB2 SQLCODE -303                            '.
               05  FILLER              PIC X(61) VALUE
        '00006PDA010COBOL,CICS TRANSACTION DUMP WITH DUMP CODE        '.
               05  FILLER              PIC X(61) VALUE
        '00007PDA010COBOL,ISSUE CICS ABEND WITH ABEND CODE            '.
               05  FILLER              PIC X(61) VALUE
        '00008PDA008COBOL,DB2 SQLCODE -303 IN A DB2 STORED PROCEDURE  '.
               05  FILLER              PIC X(61) VALUE
        '00009PDA005COBOL,UNNECESSARY REQUESTS FOR SYSTEM DATE/TIME   '.
               05  FILLER              PIC X(61) VALUE
        '00010PDA006COBOL,INEFFICIENT SQL QUERY, WHERE CLAUSE         '.
               05  FILLER              PIC X(61) VALUE
        '00011PDA006COBOL,INEFFICIENT SQL QUERY, ORDER BY CLAUSE      '.
               05  FILLER              PIC X(61) VALUE
        '00012PDA012COBOL,INEFFICIENT IMS PROCESSING, GN CALLS        '.
               05  FILLER              PIC X(61) VALUE
        '00013PDA010COBOL,INVOKE COBOL DYNAMIC SUBROUTINE             '.
               05  FILLER              PIC X(61) VALUE
        '00014PDA017COBOL,MQSERIES - QUEUE OPEN FAILURE (CODE=2085)   '.
               05  FILLER              PIC X(61) VALUE
        '00015PDA017COBOL,MQSERIES - PUT INHIBITED QUEUE (CODE=2051)  '.
               05  FILLER              PIC X(61) VALUE
        '00016PDA017COBOL,MQSERIES - NO MESSAGE ON QUEUE (CODE=2033)  '.
               05  FILLER              PIC X(61) VALUE
        '00017PDA017COBOL,MQSERIES - ABEND ASRA, BAD DATA IN MESSAGE  '.
               05  FILLER              PIC X(61) VALUE
        '00018PDA016COBOL,CURRENCY - EURO                             '.
               05  FILLER              PIC X(61) VALUE
        '00019PDA016COBOL,CURRENCY - BRITISH POUND                    '.
               05  FILLER              PIC X(61) VALUE
        '00020PDA010PL/I,USES PL/I PROGRAM PDA019 FOR ORDER INQUIRY   '.
               05  FILLER              PIC X(61) VALUE
        '00021PDA019PL/I,ABEND ASRA,BAD DATA ON VSAM FILE-S0C7        '.
               05  FILLER              PIC X(61) VALUE
        '00022PDA006COBOL,STORAGE OVERLAY,TABLE OVERFLOW,ABEND ASRA   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00023  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00024  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00025  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00026  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00027  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00028  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00029  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00030  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00031  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00032  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00033  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00034  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00035  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00036  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00037  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00038  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00039  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00040  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00041  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00042  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00043  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00044  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00045  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00046  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00047  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00048  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00049  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00050  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00051  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00052  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00053  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00054  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00055  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00056  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00057  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00058  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00059  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00060  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00061  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00062  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00063  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00064  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00065  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00066  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00067  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00068  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00069  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00070  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00071  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00072  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00073  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00074  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00075  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00076  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00077  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00078  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00079  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00080  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00081  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00082  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00083  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00084  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00085  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00086  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00087  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00088  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00089  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00090  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00091  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00092  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00093  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00094  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00095  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00096  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00097  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00098  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00099  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00100  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00101  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00102  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00103  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00104  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00105  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00106  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00107  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00108  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00109  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00110  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00111  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00112  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00113  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00114  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00115  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00116  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00117  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00118  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00119  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00120  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00121  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00122  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00123  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00124  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00125  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00126  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00127  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00128  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00129  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00130  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00131  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00132  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00133  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00134  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00135  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00136  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00137  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00138  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00139  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00140  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00141  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00142  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00143  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00144  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00145  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00146  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00147  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00148  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00149  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00150  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00151  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00152  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00153  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00154  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00155  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00156  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00157  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00158  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00159  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00160  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00161  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00162  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00163  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00164  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00165  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00166  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00167  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00168  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00169  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00170  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00171  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00172  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00173  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00174  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00175  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00176  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00177  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00178  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00179  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00180  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00181  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00182  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00183  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00184  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00185  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00186  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00187  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00188  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00189  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00190  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00191  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00192  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00193  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00194  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00195  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00196  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00197  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00198  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00199  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00200  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00201  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00202  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00203  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00204  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00205  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00206  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00207  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00208  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00209  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00210  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00211  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00212  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00213  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00214  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00215  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00216  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00217  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00218  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00219  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00220  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00221  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00222  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00223  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00224  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00225  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00226  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00227  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00228  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00229  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00230  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00231  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00232  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00233  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00234  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00235  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00236  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00237  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00238  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00239  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00240  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00241  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00242  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00243  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00244  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00245  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00246  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00247  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00248  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00249  (UNUSED)                   '.
               05  FILLER              PIC X(61) VALUE
        '           SCENARIO NUMBER 00250  (UNUSED)                   '.
           03  FILLER                  REDEFINES PDA-SWA-SCENARIO-LIST
                                       OCCURS 250 TIMES.
               05  PDA-SWA-SL-SCENARIO PIC X(5).
               05  PDA-SWA-SL-PROGRAM  PIC X(6).
               05  PDA-SWA-SL-NAME     PIC X(50).