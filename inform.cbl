000100 IDENTIFICATION DIVISION.                                         00010000
000200 PROGRAM-ID.    INFORM.                                           00020056
000300 AUTHOR.        CSU0020 ADAM DAVIES.                              00030005
000400**************************************************************    00040000
000500*                                                            *    00050000
000600*    EXAMPLE PAYROLL PROGRAM SERIES FOR CPSC3111 (COBOL).    *    00060001
000700*    FORMAT OF THE SELECT STATEMENT FOR THE DATA SETS USED.  *    00070001
000800*                                                            *    00080000
000900**************************************************************    00090000
001000                                                                  00100000
001100 ENVIRONMENT DIVISION.                                            00110000
001200 CONFIGURATION SECTION.                                           00120002
001300 SOURCE-COMPUTER.                                                 00130002
001400     Z13                                                          00140002
001500     WITH DEBUGGING MODE                                          00150002
001600     .                                                            00160002
001700                                                                  00170002
001800 INPUT-OUTPUT SECTION.                                            00180000
001900                                                                  00190001
002000 FILE-CONTROL.                                                    00200000
002100     SELECT PAY-FILE               ASSIGN TO MYINFILE.            00210022
002200     SELECT PRINT-FILE             ASSIGN TO MYREPORT.            00220022
002300                                                                  00230000
002400 DATA DIVISION.                                                   00240000
002500 FILE SECTION.                                                    00250000
002600                                                                  00260000
002700 FD  PAY-FILE                                                     00270000
002800     RECORDING MODE IS F.                                         00280000
002900 01  PAY-REC.                                                     00290001
003000     10  FILLER                    PIC X(080).                    00300022
003100                                                                  00310000
003200 FD  PRINT-FILE                                                   00320000
003300     RECORDING MODE IS F.                                         00330000
003400 01  PRINT-REC.                                                   00340000
003500     10  FILLER                    PIC X(132).                    00350022
003600                                                                  00360000
003700 WORKING-STORAGE SECTION.                                         00370000
003800                                                                  00380000
003900 01  WS-COUNTERS-FLAGS.                                           00390000
004000     10  EOF-FLAG                  PIC X(03) VALUE "NO".          00400022
004100     10  WS-SPACING                PIC 9(01) VALUE 2.             00410022
004200     10  REC-COUNT                 PIC 9(05) VALUE ZERO.          00420022
004300     10  WS-LINE-COUNT             PIC 9(03) VALUE 0.             00430022
004400     10  WS-PAGE                   PIC 9(03) VALUE 0.             00440022
004500                                                                  00450015
004600 01  PAY-CAL-FIELDS.                                              00460043
004700     10  PC-EMP-PAY                PIC S9(07)V99 VALUE 0.         00470059
004800                                                                  00480059
004900 01  WS-FORMAT-DEPT.                                              00490059
005000     10  WS-DEPT                   PIC X(03).                     00500060
005100     10  FILLER                    PIC X(03) VALUE ' - '.         00510059
005200     10  WS-NAME                   PIC X(20).                     00520059
005300                                                                  00530027
005400     COPY PAYROLL.                                                00540000
005500                                                                  00550005
005600 01  WS-PRINT-REC.                                                00560057
005700     10  PR-TITLES                 PIC X(25) JUSTIFIED RIGHT.     00570057
005800     10  FILLER                    PIC X(03).                     00580057
005900     10  PR-ALPHA.                                                00590057
006000         15  PR-DOLLAR             PIC ZZZ,ZZZ.99.                00600057
006100         15  FILLER                PIC X(40).                     00610063
006200                                                                  00620005
006300 01  WS-DATE.                                                     00630012
006400     10  YY                        PIC 9(02).                     00640012
006500     10  MM                        PIC 9(02).                     00650012
006600     10  DD                        PIC 9(02).                     00660012
006700                                                                  00670012
006800 01  WS-TIME.                                                     00680012
006900     10  HH                        PIC 9(02).                     00690012
007000     10  MN                        PIC 9(02).                     00700012
007100     10  SS                        PIC 9(02).                     00710012
007200     10  MS                        PIC 9(02).                     00720012
007300                                                                  00730012
007400 01  HEADING-1.                                                   00740012
007500     10  FILLER                    PIC X(02) VALUE SPACES.        00750012
007600     10  H-DATE.                                                  00760012
007700         15  H-MM                  PIC 9(02).                     00770012
007800         15  H-SLASH1              PIC X(01) VALUE '/'.           00780012
007900         15  H-DD                  PIC 9(02).                     00790012
008000         15  H-SLASH2              PIC X(01) VALUE '/'.           00800012
008100         15  H-YY                  PIC 9(02).                     00810012
008200     10  FILLER                    PIC X(03) VALUE SPACES.        00820012
008300     10  H-TIME.                                                  00830012
008400         15  H-HH                  PIC Z9.                        00840012
008500         15  H-COLON               PIC X(01) VALUE ':'.           00850012
008600         15  H-MN                  PIC 9(02).                     00860012
008700     10  FILLER                    PIC X(04) VALUE SPACES.        00870012
008800     10  FILLER                    PIC X(40) VALUE                00880013
008900         'PROGRAM 4, (20) ADAM DAVIES            '.               00890056
009000     10  FILLER                    PIC X(05) VALUE SPACES.        00900012
009100     10  FILLER                    PIC X(06) VALUE 'PAGE: '.      00910012
009200     10  H-PAGE                    PIC Z(03).                     00920012
009300                                                                  00930012
009400 01  HEADING-2.                                                   00940012
009500     10  FILLER                    PIC X(40) VALUE                00950012
009600         '                EMPLOYEE INTERVIEW INFOR'.              00960061
009700     10  FILLER                    PIC X(40) VALUE                00970012
009800         'MATION SHEET                           '.               00980061
009900                                                                  00990012
010000 PROCEDURE DIVISION.                                              01000010
010100                                                                  01010010
010200 010-START-HERE.                                                  01020010
010300     OPEN INPUT PAY-FILE                                          01030010
010400     OPEN OUTPUT PRINT-FILE                                       01040010
010500     PERFORM 100-READ-INPUT                                       01050010
010600     PERFORM 850-GET-DATE                                         01060014
010700                                                                  01070012
010800     PERFORM 300-PROCESS-DATA                                     01080010
010900       UNTIL EOF-FLAG = "YES"                                     01090010
011000                                                                  01100005
011100     CLOSE PAY-FILE                                               01110010
011200     CLOSE PRINT-FILE                                             01120010
011300     GOBACK.                                                      01130010
011400                                                                  01140010
011500 100-READ-INPUT.                                                  01150010
011600     READ PAY-FILE INTO WS-PAY-REC                                01160010
011700       AT END                                                     01170010
011800     MOVE "YES" TO EOF-FLAG                                       01180010
011900     END-READ                                                     01190010
012000     ADD 1 TO REC-COUNT.                                          01200010
012100                                                                  01210010
012200 300-PROCESS-DATA.                                                01220010
012300     PERFORM 800-PRINT-HEADINGS                                   01230057
012400     MOVE 'DIVISION:' TO PR-TITLES                                01240057
012500     MOVE PAY-DIV TO PR-ALPHA                                     01250057
012600     PERFORM 700-PRINT-LINE                                       01260057
012700     MOVE 'DEPARTMENT:' TO PR-TITLES                              01270057
012800     MOVE PAY-DEPT TO WS-DEPT                                     01280059
012900     MOVE PAY-DEPT-NAME TO WS-NAME                                01290059
013000     MOVE WS-FORMAT-DEPT TO PR-ALPHA                              01300059
013100     PERFORM 700-PRINT-LINE                                       01310059
013200     MOVE 'EMP NUMBER:' TO PR-TITLES                              01320059
013300     MOVE PAY-ID-NUMBER TO PR-ALPHA                               01330059
013400     PERFORM 700-PRINT-LINE                                       01340057
013500     MOVE 'LAST NAME:' TO PR-TITLES                               01350059
013600     MOVE PAY-LAST-NAME TO PR-ALPHA                               01360059
013700     PERFORM 700-PRINT-LINE                                       01370059
013800     PERFORM 310-PROCESS-DATA-PART2                               01380059
013900     .                                                            01390059
014000                                                                  01400059
014100 310-PROCESS-DATA-PART2.                                          01410059
014200     MOVE 'FIRST NAME:' TO PR-TITLES                              01420059
014300     MOVE PAY-FIRST-NAME TO PR-ALPHA                              01430059
014400     PERFORM 700-PRINT-LINE                                       01440059
014500     MOVE 'MIDDLE INIT:' TO PR-TITLES                             01450059
014600     PERFORM 700-PRINT-LINE                                       01460059
014700     MOVE 'PAY TYPE:' TO PR-TITLES                                01470059
014800     MOVE PAY-PAY-TYPE TO PR-ALPHA                                01480059
014900     PERFORM 700-PRINT-LINE                                       01490059
015000     MOVE 'PAY RATE:' TO PR-TITLES                                01500059
015100     MOVE PAY-EMP-RATE TO PR-DOLLAR                               01510059
015200     PERFORM 700-PRINT-LINE                                       01520020
015300     MOVE 'DEDUCTIONS:' TO PR-TITLES                              01530059
015400     MOVE PAY-EMP-DEDS TO PR-DOLLAR                               01540061
015500     PERFORM 700-PRINT-LINE                                       01550059
015600     PERFORM 330-PROCESS-DATA-PART3                               01560059
015700     .                                                            01570059
015800                                                                  01580059
015900 330-PROCESS-DATA-PART3.                                          01590059
016000     MOVE 'DEPENDENTS:' TO PR-TITLES                              01600059
016100     PERFORM 700-PRINT-LINE                                       01610059
016200     MOVE 'DATE LAST RAISE:' TO PR-TITLES                         01620059
016300     PERFORM 700-PRINT-LINE                                       01630059
016400     MOVE 'ANNUAL SALARY:' TO PR-TITLES                           01640059
016500     PERFORM 400-CAL-PAY                                          01650059
016600     PERFORM 700-PRINT-LINE                                       01660059
016700     MOVE 'NOTES:' TO PR-TITLES                                   01670059
016800     PERFORM 700-PRINT-LINE                                       01680059
016900     PERFORM 100-READ-INPUT                                       01690059
017000     .                                                            01700034
017100                                                                  01710043
017200 400-CAL-PAY.                                                     01720025
017300     IF PAY-PAY-TYPE EQUAL "S"                                    01730028
017400         MOVE PAY-EMP-RATE TO PC-EMP-PAY                          01740065
017500         PERFORM 420-ANNUAL                                       01750059
017600     ELSE                                                         01760028
017700         IF PAY-PAY-TYPE EQUAL "H"                                01770028
017800             PERFORM 440-HOURLY                                   01780028
017900         ELSE                                                     01790028
018000             MOVE "*** INVALID PAY TYPE ***"                      01800062
018100             TO PR-ALPHA                                          01810059
018200         END-IF                                                   01820029
018300     END-IF                                                       01830029
018400     .                                                            01840027
018500                                                                  01850042
018600 420-ANNUAL.                                                      01860059
018700     MULTIPLY PC-EMP-PAY BY 52                                    01870065
018800       GIVING PR-DOLLAR                                           01880064
018900     .                                                            01890059
019000                                                                  01900059
019100 440-HOURLY.                                                      01910059
019200     MULTIPLY PAY-EMP-RATE BY 40                                  01920059
019300       GIVING PC-EMP-PAY                                          01930064
019400     PERFORM 420-ANNUAL                                           01940059
019500     .                                                            01950027
019600                                                                  01960042
019700 700-PRINT-LINE.                                                  01970012
019800     PERFORM 750-WRITE                                            01980012
019900     IF WS-LINE-COUNT > 50                                        01990012
020000         PERFORM 800-PRINT-HEADINGS                               02000012
020100     END-IF                                                       02010012
020200     .                                                            02020012
020300                                                                  02030012
020400 750-WRITE.                                                       02040012
020500     WRITE PRINT-REC FROM WS-PRINT-REC                            02050012
020600       AFTER ADVANCING WS-SPACING LINES                           02060012
020700     END-WRITE                                                    02070012
020800     ADD WS-SPACING TO WS-LINE-COUNT                              02080012
020900     MOVE 2 TO WS-SPACING                                         02090012
021000     MOVE SPACES TO WS-PRINT-REC                                  02100012
021100     .                                                            02110012
021200                                                                  02120012
021300 800-PRINT-HEADINGS.                                              02130012
021400     ADD 1 TO WS-PAGE                                             02140012
021500     MOVE WS-PAGE TO H-PAGE                                       02150012
021600     WRITE PRINT-REC FROM HEADING-1                               02160012
021700       AFTER ADVANCING PAGE                                       02170012
021800     END-WRITE                                                    02180012
021900     MOVE HEADING-2 TO WS-PRINT-REC                               02190012
022000     MOVE 2 TO WS-SPACING                                         02200012
022100     PERFORM 750-WRITE                                            02210012
022200     MOVE 2 TO WS-SPACING                                         02220012
022300     MOVE 0 TO WS-LINE-COUNT                                      02230012
022400     .                                                            02240012
022500                                                                  02250012
022600 850-GET-DATE.                                                    02260012
022700     ACCEPT WS-DATE FROM DATE                                     02270012
022800     MOVE MM TO H-MM                                              02280012
022900     MOVE DD TO H-DD                                              02290012
023000     MOVE YY TO H-YY                                              02300012
023100     ACCEPT WS-TIME FROM TIME                                     02310012
023200     MOVE HH TO H-HH                                              02320012
023300     MOVE MN TO H-MN                                              02330012
023400     .                                                            02340012
023500                                                                  02350042
