000100 IDENTIFICATION DIVISION.                                         00010000
000200 PROGRAM-ID.    TOTALS.                                           00020024
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
004700     10  PC-HR-USED                PIC S9(04)V9  VALUE 0.         00470045
004800     10  PC-REG-HR                 PIC S9(04)V9  VALUE 0.         00480045
004900     10  PC-OT-HR                  PIC S9(04)V9  VALUE 0.         00490045
005000     10  PC-REG-PAY                PIC S9(05)V99 VALUE 0.         00500043
005100     10  PC-REG-RATE               PIC S9(05)V99 VALUE 0.         00510043
005200     10  PC-OT-PAY                 PIC S9(05)V99 VALUE 0.         00520043
005300     10  PC-OT-RATE                PIC S9(05)V99 VALUE 0.         00530043
005400     10  PC-NET-PAY                PIC S9(05)V99 VALUE 0.         00540043
005500     10  PC-GROSS-PAY              PIC S9(05)V99 VALUE 0.         00550043
005600     10  PC-DEDS                   PIC S9(05)V99 VALUE 0.         00560053
005700                                                                  00570027
005800 01  FINAL-TOTALS.                                                00580028
005900     10  FT-HR-USED                PIC S9(04)V9  VALUE 0.         00590043
006000     10  FT-NET-PAY                PIC S9(05)V99 VALUE 0.         00600043
006100     10  FT-REG-PAY                PIC S9(05)V99 VALUE 0.         00610043
006200     10  FT-OT-PAY                 PIC S9(05)V99 VALUE 0.         00620043
006300     10  FT-EMP-DEDS               PIC S9(05)V99 VALUE 0.         00630043
006400                                                                  00640027
006500     COPY PAYROLL.                                                00650000
006600                                                                  00660005
006700 01  WS-PRINT-REC                  VALUE SPACES.                  00670022
006800     05  PR-TOTALS-NAME.                                          00680028
006900         10  FILLER                PIC X(03).                     00690025
007000         10  PR-LAST-NAME          PIC X(11).                     00700025
007100         10  FILLER                PIC X(01).                     00710025
007200         10  PR-ID-NUMBER          PIC X(05).                     00720029
007300         10  FILLER                PIC X(04).                     00730025
007400     05  PR-HR-WORKED.                                            00740028
007500         10  PR-HOURS-WORKED       PIC Z,ZZZ.9.                   00750043
007600         10  FILLER                PIC X(03).                     00760025
007700     05  PR-REST.                                                 00770028
007800         10  PR-REG-PAY            PIC ZZ,ZZZ.99.                 00780043
007900         10  FILLER                PIC X(02).                     00790025
008000         10  PR-OT-PAY             PIC ZZ,ZZ9.99.                 00800043
008100         10  FILLER                PIC X(02).                     00810025
008200         10  PR-EMP-DEDS           PIC ZZ,ZZ9.99.                 00820043
008300         10  FILLER                PIC X(02).                     00830025
008400         10  PR-NET-PAY            PIC ZZ,ZZ9.99CR.               00840052
008500         10  FILLER                PIC X(59).                     00850029
008600                                                                  00860005
008700 01  WS-DATE.                                                     00870012
008800     10  YY                        PIC 9(02).                     00880012
008900     10  MM                        PIC 9(02).                     00890012
009000     10  DD                        PIC 9(02).                     00900012
009100                                                                  00910012
009200 01  WS-TIME.                                                     00920012
009300     10  HH                        PIC 9(02).                     00930012
009400     10  MN                        PIC 9(02).                     00940012
009500     10  SS                        PIC 9(02).                     00950012
009600     10  MS                        PIC 9(02).                     00960012
009700                                                                  00970012
009800 01  HEADING-1.                                                   00980012
009900     10  FILLER                    PIC X(02) VALUE SPACES.        00990012
010000     10  H-DATE.                                                  01000012
010100         15  H-MM                  PIC 9(02).                     01010012
010200         15  H-SLASH1              PIC X(01) VALUE '/'.           01020012
010300         15  H-DD                  PIC 9(02).                     01030012
010400         15  H-SLASH2              PIC X(01) VALUE '/'.           01040012
010500         15  H-YY                  PIC 9(02).                     01050012
010600     10  FILLER                    PIC X(03) VALUE SPACES.        01060012
010700     10  H-TIME.                                                  01070012
010800         15  H-HH                  PIC Z9.                        01080012
010900         15  H-COLON               PIC X(01) VALUE ':'.           01090012
011000         15  H-MN                  PIC 9(02).                     01100012
011100     10  FILLER                    PIC X(04) VALUE SPACES.        01110012
011200     10  FILLER                    PIC X(40) VALUE                01120013
011300         'PROGRAM 3, (20) ADAM DAVIES            '.               01130025
011400     10  FILLER                    PIC X(05) VALUE SPACES.        01140012
011500     10  FILLER                    PIC X(06) VALUE 'PAGE: '.      01150012
011600     10  H-PAGE                    PIC Z(03).                     01160012
011700                                                                  01170012
011800 01  HEADING-2.                                                   01180012
011900     10  FILLER                    PIC X(40) VALUE                01190012
012000         '   LAST         EMP#      HOURS     REG '.              01200048
012100     10  FILLER                    PIC X(40) VALUE                01210057
012200         'PAY     OT PAY       DEDS    NET PAY   '.               01220057
012300                                                                  01230012
012400 PROCEDURE DIVISION.                                              01240010
012500                                                                  01250010
012600 010-START-HERE.                                                  01260010
012700     OPEN INPUT PAY-FILE                                          01270010
012800     OPEN OUTPUT PRINT-FILE                                       01280010
012900     PERFORM 100-READ-INPUT                                       01290010
013000     PERFORM 850-GET-DATE                                         01300014
013100     PERFORM 800-PRINT-HEADINGS                                   01310013
013200                                                                  01320012
013300     PERFORM 300-PROCESS-DATA                                     01330010
013400       UNTIL EOF-FLAG = "YES"                                     01340010
013500                                                                  01350005
013600     PERFORM 900-PRINT-FINAL-TOTALS                               01360028
013700     CLOSE PAY-FILE                                               01370010
013800     CLOSE PRINT-FILE                                             01380010
013900     GOBACK.                                                      01390010
014000                                                                  01400010
014100 100-READ-INPUT.                                                  01410010
014200     READ PAY-FILE INTO WS-PAY-REC                                01420010
014300       AT END                                                     01430010
014400     MOVE "YES" TO EOF-FLAG                                       01440010
014500     END-READ                                                     01450010
014600     ADD 1 TO REC-COUNT.                                          01460010
014700                                                                  01470010
014800 300-PROCESS-DATA.                                                01480010
014900     MOVE PAY-LAST-NAME TO PR-LAST-NAME                           01490028
015000     MOVE PAY-ID-NUMBER TO PR-ID-NUMBER                           01500028
015100     MOVE PAY-HOURS-WORKED TO PR-HOURS-WORKED                     01510010
015200     PERFORM 350-RESET                                            01520034
015300     PERFORM 400-CAL-PAY                                          01530028
015400     PERFORM 700-PRINT-LINE                                       01540020
015500     PERFORM 500-CAL-TOTALS                                       01550029
015600     PERFORM 100-READ-INPUT                                       01560029
015700     .                                                            01570034
015800                                                                  01580043
015900 350-RESET.                                                       01590034
016000     MOVE 0 TO PC-REG-PAY                                         01600034
016100     MOVE 0 TO PC-OT-PAY                                          01610034
016200     MOVE 0 TO PC-NET-PAY                                         01620034
016300     MOVE 0 TO PC-OT-HR                                           01630034
016400     MOVE 0 TO PC-REG-HR                                          01640034
016500     MOVE 0 TO PC-HR-USED                                         01650034
016600     MOVE 0 TO PC-DEDS                                            01660054
016700     .                                                            01670029
016800                                                                  01680042
016900 400-CAL-PAY.                                                     01690025
017000     MOVE PAY-HOURS-WORKED TO PC-HR-USED                          01700055
017100     IF PAY-PAY-TYPE EQUAL "S"                                    01710028
017200         PERFORM 420-SALARY                                       01720028
017300     ELSE                                                         01730028
017400         IF PAY-PAY-TYPE EQUAL "H"                                01740028
017500             PERFORM 440-HOURLY                                   01750028
017600         ELSE                                                     01760028
017700             MOVE 0 TO PC-HR-USED                                 01770055
017800             MOVE "*** INVALID PAY TYPE - RECORD IGNORED ***"     01780029
017900               TO PR-REST                                         01790058
018000         END-IF                                                   01800029
018100     END-IF                                                       01810029
018200     .                                                            01820027
018300                                                                  01830042
018400 420-SALARY.                                                      01840028
018500     MOVE PAY-EMP-RATE TO PC-GROSS-PAY                            01850028
018600     MOVE 0 TO PC-OT-PAY                                          01860028
018700     MOVE PAY-EMP-RATE TO PC-REG-PAY                              01870031
018800     PERFORM 480-NET-PAY                                          01880028
018900     .                                                            01890027
019000                                                                  01900042
019100 440-HOURLY.                                                      01910028
019200     IF PC-HR-USED > 40                                           01920028
019300         PERFORM 450-OT-PAY                                       01930028
019400     ELSE                                                         01940028
019500         MOVE 0 TO PC-OT-PAY                                      01950028
019600         MOVE PC-HR-USED TO PC-REG-HR                             01960028
019700         PERFORM 460-REG-PAY                                      01970028
019800     END-IF                                                       01980029
019900     .                                                            01990028
020000                                                                  02000042
020100 450-OT-PAY.                                                      02010028
020200     SUBTRACT 40 FROM PC-HR-USED                                  02020028
020300       GIVING PC-OT-HR                                            02030028
020400     MOVE 40 TO PC-REG-HR                                         02040034
020500     MULTIPLY PAY-EMP-RATE BY 1.5                                 02050028
020600       GIVING PC-OT-RATE ROUNDED                                  02060044
020700     MULTIPLY PC-OT-RATE BY PC-OT-HR                              02070028
020800       GIVING PC-OT-PAY ROUNDED                                   02080044
020900     PERFORM 460-REG-PAY                                          02090050
021000     .                                                            02100028
021100                                                                  02110050
021200 460-REG-PAY.                                                     02120028
021300     MULTIPLY PAY-EMP-RATE BY PC-REG-HR                           02130028
021400       GIVING PC-REG-PAY ROUNDED                                  02140044
021500     MOVE 0 TO PC-GROSS-PAY                                       02150035
021600     ADD PC-OT-PAY TO PC-GROSS-PAY                                02160035
021700     ADD PC-REG-PAY TO PC-GROSS-PAY                               02170035
021800     PERFORM 480-NET-PAY                                          02180028
021900     .                                                            02190028
022000                                                                  02200042
022100 480-NET-PAY.                                                     02210028
022200     MOVE PAY-EMP-DEDS TO PC-DEDS                                 02220053
022300     SUBTRACT PC-DEDS FROM PC-GROSS-PAY                           02230053
022400       GIVING PC-NET-PAY                                          02240028
022500     MOVE PC-REG-PAY TO PR-REG-PAY                                02250041
022600     MOVE PC-OT-PAY TO PR-OT-PAY                                  02260041
022700     MOVE PAY-EMP-DEDS TO PR-EMP-DEDS                             02270041
022800     MOVE PC-NET-PAY TO PR-NET-PAY                                02280041
022900     .                                                            02290041
023000                                                                  02300042
023100 500-CAL-TOTALS.                                                  02310028
023200     ADD PC-HR-USED TO FT-HR-USED                                 02320028
023300     ADD PC-REG-PAY TO FT-REG-PAY                                 02330028
023400     ADD PC-OT-PAY  TO FT-OT-PAY                                  02340028
023500     ADD PC-DEDS TO FT-EMP-DEDS                                   02350053
023600     ADD PC-NET-PAY TO FT-NET-PAY                                 02360028
023700     .                                                            02370028
023800                                                                  02380042
023900 700-PRINT-LINE.                                                  02390012
024000     PERFORM 750-WRITE                                            02400012
024100     IF WS-LINE-COUNT > 50                                        02410012
024200         PERFORM 800-PRINT-HEADINGS                               02420012
024300     END-IF                                                       02430012
024400     .                                                            02440012
024500                                                                  02450012
024600 750-WRITE.                                                       02460012
024700     WRITE PRINT-REC FROM WS-PRINT-REC                            02470012
024800       AFTER ADVANCING WS-SPACING LINES                           02480012
024900     END-WRITE                                                    02490012
025000     ADD WS-SPACING TO WS-LINE-COUNT                              02500012
025100     MOVE 2 TO WS-SPACING                                         02510012
025200     MOVE SPACES TO WS-PRINT-REC                                  02520012
025300     .                                                            02530012
025400                                                                  02540012
025500 800-PRINT-HEADINGS.                                              02550012
025600     ADD 1 TO WS-PAGE                                             02560012
025700     MOVE WS-PAGE TO H-PAGE                                       02570012
025800     WRITE PRINT-REC FROM HEADING-1                               02580012
025900       AFTER ADVANCING PAGE                                       02590012
026000     END-WRITE                                                    02600012
026100     MOVE HEADING-2 TO WS-PRINT-REC                               02610012
026200     MOVE 2 TO WS-SPACING                                         02620012
026300     PERFORM 750-WRITE                                            02630012
026400     MOVE 2 TO WS-SPACING                                         02640012
026500     MOVE 0 TO WS-LINE-COUNT                                      02650012
026600     .                                                            02660012
026700                                                                  02670012
026800 850-GET-DATE.                                                    02680012
026900     ACCEPT WS-DATE FROM DATE                                     02690012
027000     MOVE MM TO H-MM                                              02700012
027100     MOVE DD TO H-DD                                              02710012
027200     MOVE YY TO H-YY                                              02720012
027300     ACCEPT WS-TIME FROM TIME                                     02730012
027400     MOVE HH TO H-HH                                              02740012
027500     MOVE MN TO H-MN                                              02750012
027600     .                                                            02760012
027700                                                                  02770042
027800 900-PRINT-FINAL-TOTALS.                                          02780028
027900     MOVE FT-HR-USED TO PR-HOURS-WORKED                           02790028
028000     MOVE FT-REG-PAY TO PR-REG-PAY                                02800028
028100     MOVE FT-OT-PAY TO PR-OT-PAY                                  02810028
028200     MOVE FT-EMP-DEDS TO PR-EMP-DEDS                              02820036
028300     MOVE FT-NET-PAY TO PR-NET-PAY                                02830028
028400     MOVE '    FINAL TOTALS' TO PR-TOTALS-NAME                    02840029
028500     PERFORM 700-PRINT-LINE                                       02850028
028600     .                                                            02860028
