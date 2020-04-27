000100 IDENTIFICATION DIVISION.                                         00010000
000200 PROGRAM-ID.    BREAKS.                                           00020056
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
005700                                                                  00570060
005800 01  DIV-DEPT-FIELDS.                                             00580060
005900     10  DD-DIV                    PIC X(02) VALUE "  ".          00590061
006000     10  DD-DEPT                   PIC X(03) VALUE "   ".         00600061
006100                                                                  00610027
006200 01  FINAL-TOTALS.                                                00620028
006300     10  FT-HR-USED                PIC S9(04)V9  VALUE 0.         00630043
006400     10  FT-NET-PAY                PIC S9(05)V99 VALUE 0.         00640043
006500     10  FT-REG-PAY                PIC S9(05)V99 VALUE 0.         00650043
006600     10  FT-OT-PAY                 PIC S9(05)V99 VALUE 0.         00660043
006700     10  FT-EMP-DEDS               PIC S9(05)V99 VALUE 0.         00670043
006800                                                                  00680027
006900 01  DIVISION-TOTALS.                                             00690057
007000     10  DT-HR-USED                PIC S9(04)V9  VALUE 0.         00700057
007100     10  DT-NET-PAY                PIC S9(05)V99 VALUE 0.         00710057
007200     10  DT-REG-PAY                PIC S9(05)V99 VALUE 0.         00720057
007300     10  DT-OT-PAY                 PIC S9(05)V99 VALUE 0.         00730057
007400     10  DT-EMP-DEDS               PIC S9(05)V99 VALUE 0.         00740057
007500                                                                  00750057
007600 01  DEPT-TOTALS.                                                 00760075
007700     10  PT-HR-USED                PIC S9(04)V9  VALUE 0.         00770057
007800     10  PT-NET-PAY                PIC S9(05)V99 VALUE 0.         00780057
007900     10  PT-REG-PAY                PIC S9(05)V99 VALUE 0.         00790057
008000     10  PT-OT-PAY                 PIC S9(05)V99 VALUE 0.         00800057
008100     10  PT-EMP-DEDS               PIC S9(05)V99 VALUE 0.         00810057
008200                                                                  00820005
008300     COPY PAYROLL.                                                00830057
008400                                                                  00840057
008500 01  WS-PRINT-REC                  VALUE SPACES.                  00850022
008600     05  PR-TOTALS-NAME.                                          00860028
008700         10  FILLER                PIC X(01).                     00870064
008800         10  PR-DIV                PIC X(02).                     00880059
008900         10  FILLER                PIC X(01).                     00890059
009000         10  PR-DEPT               PIC X(03).                     00900059
009100         10  FILLER                PIC X(01).                     00910065
009200         10  PR-LAST-NAME          PIC X(11).                     00920025
009300         10  FILLER                PIC X(01).                     00930025
009400         10  PR-ID-NUMBER          PIC X(05).                     00940029
009500         10  FILLER                PIC X(01).                     00950065
009600     05  PR-HR-WORKED.                                            00960028
009700         10  PR-HOURS-WORKED       PIC ZZZ.9.                     00970065
009800         10  FILLER                PIC X(01).                     00980065
009900     05  PR-REST.                                                 00990028
010000         10  PR-REG-PAY            PIC Z,ZZZ.99.                  01000065
010100         10  FILLER                PIC X(02).                     01010025
010200         10  PR-OT-PAY             PIC Z,ZZ9.99.                  01020065
010300         10  FILLER                PIC X(02).                     01030025
010400         10  PR-EMP-DEDS           PIC Z,ZZ9.99.                  01040065
010500         10  FILLER                PIC X(02).                     01050025
010600         10  PR-NET-PAY            PIC Z,ZZ9.99CR.                01060065
010700         10  PR-AST                PIC X(03).                     01070060
010800         10  FILLER                PIC X(50).                     01080064
010900                                                                  01090005
011000 01  WS-DATE.                                                     01100012
011100     10  YY                        PIC 9(02).                     01110012
011200     10  MM                        PIC 9(02).                     01120012
011300     10  DD                        PIC 9(02).                     01130012
011400                                                                  01140012
011500 01  WS-TIME.                                                     01150012
011600     10  HH                        PIC 9(02).                     01160012
011700     10  MN                        PIC 9(02).                     01170012
011800     10  SS                        PIC 9(02).                     01180012
011900     10  MS                        PIC 9(02).                     01190012
012000                                                                  01200012
012100 01  HEADING-1.                                                   01210012
012200     10  FILLER                    PIC X(02) VALUE SPACES.        01220012
012300     10  H-DATE.                                                  01230012
012400         15  H-MM                  PIC 9(02).                     01240012
012500         15  H-SLASH1              PIC X(01) VALUE '/'.           01250012
012600         15  H-DD                  PIC 9(02).                     01260012
012700         15  H-SLASH2              PIC X(01) VALUE '/'.           01270012
012800         15  H-YY                  PIC 9(02).                     01280012
012900     10  FILLER                    PIC X(03) VALUE SPACES.        01290012
013000     10  H-TIME.                                                  01300012
013100         15  H-HH                  PIC Z9.                        01310012
013200         15  H-COLON               PIC X(01) VALUE ':'.           01320012
013300         15  H-MN                  PIC 9(02).                     01330012
013400     10  FILLER                    PIC X(04) VALUE SPACES.        01340012
013500     10  FILLER                    PIC X(40) VALUE                01350013
013600         'PROGRAM 5, (20) ADAM DAVIES            '.               01360056
013700     10  FILLER                    PIC X(05) VALUE SPACES.        01370012
013800     10  FILLER                    PIC X(06) VALUE 'PAGE: '.      01380012
013900     10  H-PAGE                    PIC Z(03).                     01390012
014000                                                                  01400012
014100 01  HEADING-2.                                                   01410012
014200     10  FILLER                    PIC X(40) VALUE                01420012
014300         ' CCTR   LAST         EMP# HOURS  REG PAY'.              01430082
014400     10  FILLER                    PIC X(40) VALUE                01440012
014500         '    OT PAY      DEDS   NET PAY         '.               01450082
014600                                                                  01460012
014700 PROCEDURE DIVISION.                                              01470010
014800                                                                  01480010
014900 010-START-HERE.                                                  01490010
015000     OPEN INPUT PAY-FILE                                          01500010
015100     OPEN OUTPUT PRINT-FILE                                       01510010
015200     PERFORM 100-READ-INPUT                                       01520010
015300     PERFORM 850-GET-DATE                                         01530014
015400     PERFORM 800-PRINT-HEADINGS                                   01540013
015500     MOVE PAY-DIV TO DD-DIV                                       01550060
015600     MOVE PAY-DEPT TO DD-DEPT                                     01560060
015700                                                                  01570060
015800     PERFORM 300-PROCESS-DATA                                     01580010
015900       UNTIL EOF-FLAG = "YES"                                     01590010
016000                                                                  01600005
016100     PERFORM 900-PRINT-FINAL-TOTALS                               01610028
016200     CLOSE PAY-FILE                                               01620010
016300     CLOSE PRINT-FILE                                             01630010
016400     GOBACK.                                                      01640010
016500                                                                  01650010
016600 100-READ-INPUT.                                                  01660010
016700     READ PAY-FILE INTO WS-PAY-REC                                01670010
016800       AT END                                                     01680010
016900         MOVE "YES" TO EOF-FLAG                                   01690083
017000     END-READ                                                     01700010
017100     ADD 1 TO REC-COUNT.                                          01710010
017200                                                                  01720010
017300 300-PROCESS-DATA.                                                01730010
017400     PERFORM 500-CAL-TYPE                                         01740078
017500     PERFORM 100-READ-INPUT                                       01750029
017600     .                                                            01760034
017700                                                                  01770078
017800 310-NAME-SETUP.                                                  01780078
017900     MOVE PAY-LAST-NAME TO PR-LAST-NAME                           01790078
018000     MOVE PAY-ID-NUMBER TO PR-ID-NUMBER                           01800078
018100     MOVE PAY-HOURS-WORKED TO PR-HOURS-WORKED                     01810078
018200     MOVE PAY-DIV TO PR-DIV                                       01820078
018300     MOVE PAY-DEPT TO PR-DEPT                                     01830078
018400     INITIALIZE PAY-CAL-FIELDS                                    01840078
018500     .                                                            01850078
018600                                                                  01860078
018700 400-CAL-PAY.                                                     01870025
018800     MOVE PAY-HOURS-WORKED TO PC-HR-USED                          01880055
018900     IF PAY-PAY-TYPE EQUAL "S"                                    01890028
019000         PERFORM 420-SALARY                                       01900028
019100     ELSE                                                         01910028
019200         IF PAY-PAY-TYPE EQUAL "H"                                01920028
019300             PERFORM 440-HOURLY                                   01930028
019400         ELSE                                                     01940028
019500             MOVE 0 TO PC-HR-USED                                 01950055
019600             MOVE "*** INVALID PAY TYPE - RECORD IGNORED ***"     01960029
019700               TO PR-REST                                         01970083
019800         END-IF                                                   01980029
019900     END-IF                                                       01990029
020000     .                                                            02000027
020100                                                                  02010042
020200 420-SALARY.                                                      02020028
020300     MOVE PAY-EMP-RATE TO PC-GROSS-PAY                            02030028
020400     MOVE 0 TO PC-OT-PAY                                          02040028
020500     MOVE PAY-EMP-RATE TO PC-REG-PAY                              02050031
020600     PERFORM 480-NET-PAY                                          02060028
020700     .                                                            02070027
020800                                                                  02080042
020900 440-HOURLY.                                                      02090028
021000     IF PC-HR-USED > 40                                           02100028
021100         PERFORM 450-OT-PAY                                       02110028
021200     ELSE                                                         02120028
021300         MOVE 0 TO PC-OT-PAY                                      02130028
021400         MOVE PC-HR-USED TO PC-REG-HR                             02140028
021500         PERFORM 460-REG-PAY                                      02150028
021600     END-IF                                                       02160029
021700     .                                                            02170028
021800                                                                  02180042
021900 450-OT-PAY.                                                      02190028
022000     SUBTRACT 40 FROM PC-HR-USED                                  02200028
022100       GIVING PC-OT-HR                                            02210028
022200     MOVE 40 TO PC-REG-HR                                         02220034
022300     MULTIPLY PAY-EMP-RATE BY 1.5                                 02230028
022400       GIVING PC-OT-RATE ROUNDED                                  02240044
022500     MULTIPLY PC-OT-RATE BY PC-OT-HR                              02250028
022600       GIVING PC-OT-PAY ROUNDED                                   02260044
022700     PERFORM 460-REG-PAY                                          02270050
022800     .                                                            02280028
022900                                                                  02290050
023000 460-REG-PAY.                                                     02300028
023100     MULTIPLY PAY-EMP-RATE BY PC-REG-HR                           02310028
023200       GIVING PC-REG-PAY ROUNDED                                  02320044
023300     MOVE 0 TO PC-GROSS-PAY                                       02330035
023400     ADD PC-OT-PAY TO PC-GROSS-PAY                                02340035
023500     ADD PC-REG-PAY TO PC-GROSS-PAY                               02350035
023600     PERFORM 480-NET-PAY                                          02360028
023700     .                                                            02370028
023800                                                                  02380042
023900 480-NET-PAY.                                                     02390028
024000     MOVE PAY-EMP-DEDS TO PC-DEDS                                 02400053
024100     SUBTRACT PC-DEDS FROM PC-GROSS-PAY                           02410053
024200       GIVING PC-NET-PAY                                          02420028
024300     MOVE PC-REG-PAY TO PR-REG-PAY                                02430041
024400     MOVE PC-OT-PAY TO PR-OT-PAY                                  02440041
024500     MOVE PAY-EMP-DEDS TO PR-EMP-DEDS                             02450041
024600     MOVE PC-NET-PAY TO PR-NET-PAY                                02460041
024700     .                                                            02470041
024800                                                                  02480042
024900 500-CAL-TYPE.                                                    02490060
025000     IF PAY-DIV NOT EQUAL DD-DIV                                  02500072
025100        PERFORM 510-DIV-CHANGE                                    02510072
025200     ELSE                                                         02520072
025300        IF PAY-DEPT NOT EQUAL DD-DEPT                             02530072
025400           PERFORM 520-DEPT-CHANGE                                02540072
025500        END-IF                                                    02550072
025600     END-IF                                                       02560072
025700     PERFORM 530-CAL-SETUP                                        02570090
025800     .                                                            02580072
025900                                                                  02590072
026300 510-DIV-CHANGE.                                                  02630072
026400     PERFORM 520-DEPT-CHANGE                                      02640091
026500     ADD DT-HR-USED TO FT-HR-USED                                 02650092
026600     ADD DT-REG-PAY TO FT-REG-PAY                                 02660092
026700     ADD DT-OT-PAY  TO FT-OT-PAY                                  02670092
026800     ADD DT-EMP-DEDS TO FT-EMP-DEDS                               02680092
026900     ADD DT-NET-PAY TO FT-NET-PAY                                 02690092
027000     PERFORM 610-PRINT-DIV-TOTALS                                 02700083
027100     INITIALIZE DIVISION-TOTALS                                   02710073
027200     .                                                            02720072
027300                                                                  02730072
027400 520-DEPT-CHANGE.                                                 02740077
027500     ADD PT-HR-USED TO DT-HR-USED                                 02750092
027600     ADD PT-REG-PAY TO DT-REG-PAY                                 02760092
027700     ADD PT-OT-PAY  TO DT-OT-PAY                                  02770092
027800     ADD PT-EMP-DEDS TO DT-EMP-DEDS                               02780092
027900     ADD PT-NET-PAY TO DT-NET-PAY                                 02790092
028000     PERFORM 620-PRINT-DEPT-TOTALS                                02800083
028100     INITIALIZE DEPT-TOTALS                                       02810075
028200     .                                                            02820072
028300                                                                  02830072
028400 530-CAL-SETUP.                                                   02840072
028500     PERFORM 310-NAME-SETUP                                       02850078
028600     PERFORM 400-CAL-PAY                                          02860078
028700     ADD PC-HR-USED TO PT-HR-USED                                 02870088
028800     ADD PC-REG-PAY TO PT-REG-PAY                                 02880088
028900     ADD PC-OT-PAY  TO PT-OT-PAY                                  02890088
029000     ADD PC-DEDS TO PT-EMP-DEDS                                   02900088
029100     ADD PC-NET-PAY TO PT-NET-PAY                                 02910088
029200     MOVE PAY-DIV TO DD-DIV                                       02920072
029300     MOVE PAY-DEPT TO DD-DEPT                                     02930076
029400     PERFORM 700-PRINT-LINE                                       02940072
029500     .                                                            02950072
029600                                                                  02960072
029700 610-PRINT-DIV-TOTALS.                                            02970083
029800     MOVE DT-HR-USED TO PR-HOURS-WORKED                           02980083
029900     MOVE '** ' TO PR-AST                                         02990083
030000     MOVE DT-REG-PAY TO PR-REG-PAY                                03000083
030100     MOVE DT-OT-PAY TO PR-OT-PAY                                  03010083
030200     MOVE DT-EMP-DEDS TO PR-EMP-DEDS                              03020083
030300     MOVE DT-NET-PAY TO PR-NET-PAY                                03030083
030400     MOVE '   DIVISION TOTALS' TO PR-TOTALS-NAME                  03040083
030500     PERFORM 700-PRINT-LINE                                       03050083
030600     MOVE 2 TO WS-SPACING                                         03060086
030700     .                                                            03070083
030800                                                                  03080083
030900 620-PRINT-DEPT-TOTALS.                                           03090083
031000     MOVE PT-HR-USED TO PR-HOURS-WORKED                           03100083
031100     MOVE '*  ' TO PR-AST                                         03110083
031200     MOVE PT-REG-PAY TO PR-REG-PAY                                03120083
031300     MOVE PT-OT-PAY TO PR-OT-PAY                                  03130083
031400     MOVE PT-EMP-DEDS TO PR-EMP-DEDS                              03140083
031500     MOVE PT-NET-PAY TO PR-NET-PAY                                03150083
031600     MOVE '   DEPARTMENT TOTALS' TO PR-TOTALS-NAME                03160083
031700     PERFORM 700-PRINT-LINE                                       03170083
031800     MOVE 2 TO WS-SPACING                                         03180086
031900     .                                                            03190083
032000                                                                  03200095
032100 700-PRINT-LINE.                                                  03210012
032200     PERFORM 750-WRITE                                            03220012
032300     IF WS-LINE-COUNT > 50                                        03230012
032400         PERFORM 800-PRINT-HEADINGS                               03240012
032500     END-IF                                                       03250012
032600     .                                                            03260012
032700                                                                  03270012
032800 750-WRITE.                                                       03280012
032900     WRITE PRINT-REC FROM WS-PRINT-REC                            03290012
033000       AFTER ADVANCING WS-SPACING LINES                           03300012
033100     END-WRITE                                                    03310012
033200     ADD WS-SPACING TO WS-LINE-COUNT                              03320012
033300     MOVE 1 TO WS-SPACING                                         03330085
033400     MOVE SPACES TO WS-PRINT-REC                                  03340012
033500     .                                                            03350012
033600                                                                  03360012
033700 800-PRINT-HEADINGS.                                              03370012
033800     ADD 1 TO WS-PAGE                                             03380012
033900     MOVE WS-PAGE TO H-PAGE                                       03390012
034000     WRITE PRINT-REC FROM HEADING-1                               03400012
034100       AFTER ADVANCING PAGE                                       03410012
034200     END-WRITE                                                    03420012
034300     MOVE HEADING-2 TO WS-PRINT-REC                               03430012
034400     MOVE 2 TO WS-SPACING                                         03440012
034500     PERFORM 750-WRITE                                            03450012
034600     MOVE 2 TO WS-SPACING                                         03460012
034700     MOVE 0 TO WS-LINE-COUNT                                      03470012
034800     .                                                            03480012
034900                                                                  03490012
035000 850-GET-DATE.                                                    03500012
035100     ACCEPT WS-DATE FROM DATE                                     03510012
035200     MOVE MM TO H-MM                                              03520012
035300     MOVE DD TO H-DD                                              03530012
035400     MOVE YY TO H-YY                                              03540012
035500     ACCEPT WS-TIME FROM TIME                                     03550012
035600     MOVE HH TO H-HH                                              03560012
035700     MOVE MN TO H-MN                                              03570012
035800     .                                                            03580012
035900                                                                  03590042
036000 900-PRINT-FINAL-TOTALS.                                          03600060
036200     PERFORM 510-DIV-CHANGE                                       03620093
036300     MOVE '***' TO PR-AST                                         03630060
036400     MOVE FT-HR-USED TO PR-HOURS-WORKED                           03640060
036500     MOVE FT-REG-PAY TO PR-REG-PAY                                03650060
036600     MOVE FT-OT-PAY TO PR-OT-PAY                                  03660060
036700     MOVE FT-EMP-DEDS TO PR-EMP-DEDS                              03670060
036800     MOVE FT-NET-PAY TO PR-NET-PAY                                03680060
036900     MOVE '   FINAL TOTALS' TO PR-TOTALS-NAME                     03690083
037000     PERFORM 700-PRINT-LINE                                       03700060
037100     .                                                            03710060
037200                                                                  03720060
