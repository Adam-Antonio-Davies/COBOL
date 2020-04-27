000100 IDENTIFICATION DIVISION.                                         00010000
000200 PROGRAM-ID.    LOOKUP.                                           00020099
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
004500     10  FOUND-FLAG                PIC X(03) VALUE "NO".          00450098
004600                                                                  00460015
004700 01  PAY-CAL-FIELDS.                                              00470043
004800     10  PC-HR-USED                PIC S9(04)V9  VALUE 0.         00480045
004900     10  PC-REG-HR                 PIC S9(04)V9  VALUE 0.         00490045
005000     10  PC-OT-HR                  PIC S9(04)V9  VALUE 0.         00500045
005100     10  PC-REG-PAY                PIC S9(05)V99 VALUE 0.         00510043
005200     10  PC-REG-RATE               PIC S9(05)V99 VALUE 0.         00520043
005300     10  PC-OT-PAY                 PIC S9(05)V99 VALUE 0.         00530043
005400     10  PC-OT-RATE                PIC S9(05)V99 VALUE 0.         00540043
005500     10  PC-NET-PAY                PIC S9(05)V99 VALUE 0.         00550043
005600     10  PC-GROSS-PAY              PIC S9(05)V99 VALUE 0.         00560043
005700     10  PC-DEDS                   PIC S9(05)V99 VALUE 0.         00570053
005800                                                                  00580060
005900 01  DIV-DEPT-FIELDS.                                             00590060
006000     10  DD-DIV                    PIC X(02) VALUE "  ".          00600061
006100     10  DD-DEPT                   PIC X(03) VALUE "   ".         00610061
006200     10  D-SUB                     PIC 9(02) VALUE 01.            00620098
006300     10  M-SUB                     PIC 9(02) VALUE 10.            00630098
006400                                                                  00640027
006500 01  FINAL-TOTALS.                                                00650028
006600     10  FT-HR-USED                PIC S9(04)V9  VALUE 0.         00660043
006700     10  FT-NET-PAY                PIC S9(05)V99 VALUE 0.         00670043
006800     10  FT-REG-PAY                PIC S9(05)V99 VALUE 0.         00680043
006900     10  FT-OT-PAY                 PIC S9(05)V99 VALUE 0.         00690043
007000     10  FT-EMP-DEDS               PIC S9(05)V99 VALUE 0.         00700043
007100                                                                  00710027
007200 01  DIVISION-TOTALS.                                             00720057
007300     10  DT-HR-USED                PIC S9(04)V9  VALUE 0.         00730057
007400     10  DT-NET-PAY                PIC S9(05)V99 VALUE 0.         00740057
007500     10  DT-REG-PAY                PIC S9(05)V99 VALUE 0.         00750057
007600     10  DT-OT-PAY                 PIC S9(05)V99 VALUE 0.         00760057
007700     10  DT-EMP-DEDS               PIC S9(05)V99 VALUE 0.         00770057
007800                                                                  00780057
007900 01  DEPT-TOTALS.                                                 00790075
008000     10  PT-HR-USED                PIC S9(04)V9  VALUE 0.         00800057
008100     10  PT-NET-PAY                PIC S9(05)V99 VALUE 0.         00810057
008200     10  PT-REG-PAY                PIC S9(05)V99 VALUE 0.         00820057
008300     10  PT-OT-PAY                 PIC S9(05)V99 VALUE 0.         00830057
008400     10  PT-EMP-DEDS               PIC S9(05)V99 VALUE 0.         00840057
008500                                                                  00850005
008600 01  DIV-NAMES.                                                   00860096
008700     10  FILLER                    PIC X(30) VALUE                00870099
008800         '  ***FINAL TOTALS***'.                                  00880099
008900     10  FILLER                    PIC X(30) VALUE                00890099
009000         '10ELECTRONIC DEVICES'.                                  00900099
009100     10  FILLER                    PIC X(30) VALUE                00910099
009200         '20KITCHEN APPLIANCES'.                                  00920099
009300     10  FILLER                    PIC X(30) VALUE                00930099
009400         '30COMPUTER EQUIPMENT'.                                  00940099
009500     10  FILLER                    PIC X(30) VALUE                00950099
009600         '40HOME IMPROVEMENT'.                                    00960099
009700     10  FILLER                    PIC X(30) VALUE                00970099
009800         '50PHOTOGRAPHIC EQUIPMENT'.                              00980099
009900     10  FILLER                    PIC X(30) VALUE                00990099
010000         '60CHILDREN''S TOYS'.                                    01000099
010100     10  FILLER                    PIC X(30) VALUE                01010099
010200         '70TRAVEL GOODS'.                                        01020099
010300     10  FILLER                    PIC X(30) VALUE                01030099
010400         '80APPAREL'.                                             01040099
010500     10  FILLER                    PIC X(30) VALUE                01050099
010600         '90SPORTING GOODS'.                                      01060099
010700     10  FILLER                    PIC X(30) VALUE                01070099
010800         '99***NEW DIVISION CREATED***'.                          01080099
010900                                                                  01090096
011000 01  DIV-TABLES                    REDEFINES DIV-NAMES.           01100096
011100     10  DIV-ITEM                  OCCURS 11 TIMES.               01110099
011200         15  DIV-NO                PIC X(02).                     01120099
011300         15  DIV-NAME              PIC X(28).                     01130099
011400                                                                  01140096
011500     COPY PAYROLL.                                                01150057
011600                                                                  01160057
011700 01  WS-PRINT-REC                  VALUE SPACES.                  01170022
011800     05  PR-TOTALS-NAME.                                          01180028
011900         10  FILLER                PIC X(01).                     01190064
012000         10  PR-DIV                PIC X(02).                     01200059
012100         10  FILLER                PIC X(01).                     01210059
012200         10  PR-DEPT               PIC X(03).                     01220059
012300         10  FILLER                PIC X(01).                     01230065
012400         10  PR-LAST-NAME          PIC X(11).                     01240025
012500         10  FILLER                PIC X(01).                     01250025
012600         10  PR-ID-NUMBER          PIC X(05).                     01260029
012700         10  FILLER                PIC X(01).                     01270065
012800     05  PR-HR-WORKED.                                            01280028
012900         10  PR-HOURS-WORKED       PIC ZZZ.9.                     01290065
013000         10  FILLER                PIC X(01).                     01300065
013100     05  PR-REST.                                                 01310028
013200         10  PR-REG-PAY            PIC Z,ZZZ.99.                  01320065
013300         10  FILLER                PIC X(02).                     01330025
013400         10  PR-OT-PAY             PIC Z,ZZ9.99.                  01340065
013500         10  FILLER                PIC X(02).                     01350025
013600         10  PR-EMP-DEDS           PIC Z,ZZ9.99.                  01360065
013700         10  FILLER                PIC X(02).                     01370025
013800         10  PR-NET-PAY            PIC Z,ZZ9.99CR.                01380065
013900         10  PR-AST                PIC X(03).                     01390060
014000         10  FILLER                PIC X(50).                     01400064
014100                                                                  01410005
014200 01  WS-DATE.                                                     01420012
014300     10  YY                        PIC 9(02).                     01430012
014400     10  MM                        PIC 9(02).                     01440012
014500     10  DD                        PIC 9(02).                     01450012
014600                                                                  01460012
014700 01  WS-TIME.                                                     01470012
014800     10  HH                        PIC 9(02).                     01480012
014900     10  MN                        PIC 9(02).                     01490012
015000     10  SS                        PIC 9(02).                     01500012
015100     10  MS                        PIC 9(02).                     01510012
015200                                                                  01520012
015300 01  HEADING-1.                                                   01530012
015400     10  FILLER                    PIC X(02) VALUE SPACES.        01540012
015500     10  H-DATE.                                                  01550012
015600         15  H-MM                  PIC 9(02).                     01560012
015700         15  H-SLASH1              PIC X(01) VALUE '/'.           01570012
015800         15  H-DD                  PIC 9(02).                     01580012
015900         15  H-SLASH2              PIC X(01) VALUE '/'.           01590012
016000         15  H-YY                  PIC 9(02).                     01600012
016100     10  FILLER                    PIC X(03) VALUE SPACES.        01610012
016200     10  H-TIME.                                                  01620012
016300         15  H-HH                  PIC Z9.                        01630012
016400         15  H-COLON               PIC X(01) VALUE ':'.           01640012
016500         15  H-MN                  PIC 9(02).                     01650012
016600     10  FILLER                    PIC X(04) VALUE SPACES.        01660012
016700     10  FILLER                    PIC X(40) VALUE                01670013
016800         'PROGRAM 5, (20) ADAM DAVIES            '.               01680056
016900     10  FILLER                    PIC X(05) VALUE SPACES.        01690012
017000     10  FILLER                    PIC X(06) VALUE 'PAGE: '.      01700012
017100     10  H-PAGE                    PIC Z(03).                     01710012
017200                                                                  01720012
017300 01  HEADING-2.                                                   01730097
017400     10  FILLER                    PIC X(15) VALUE SPACES.        01740097
017500     10  FILLER                    PIC X(15) VALUE                01750097
017600         ' FOR DIVISION:'.                                        01760097
017700     10  H2-DIV-NO                 PIC X(02).                     01770097
017800     10  H2-DASH                   PIC X(03) VALUE ' - '.         01780097
017900     10  H2-DIV-NAME               PIC X(28).                     01790097
018000                                                                  01800097
018100 01  HEADING-3.                                                   01810097
018200     10  FILLER                    PIC X(40) VALUE                01820012
018300         ' CCTR   LAST         EMP# HOURS  REG PAY'.              01830082
018400     10  FILLER                    PIC X(40) VALUE                01840012
018500         '    OT PAY      DEDS   NET PAY         '.               01850082
018600                                                                  01860012
018700 PROCEDURE DIVISION.                                              01870010
018800                                                                  01880010
018900 010-START-HERE.                                                  01890010
019000     OPEN INPUT PAY-FILE                                          01900010
019100     OPEN OUTPUT PRINT-FILE                                       01910010
019200     PERFORM 100-READ-INPUT                                       01920010
019300     MOVE PAY-DIV TO DD-DIV                                       01930099
019400     MOVE PAY-DEPT TO DD-DEPT                                     01940099
019500     PERFORM 850-GET-DATE                                         01950014
019600     PERFORM 800-PRINT-HEADINGS                                   01960013
019700                                                                  01970060
019800     PERFORM 300-PROCESS-DATA                                     01980010
019900       UNTIL EOF-FLAG = "YES"                                     01990010
020000                                                                  02000005
020100     PERFORM 900-PRINT-FINAL-TOTALS                               02010028
020200     CLOSE PAY-FILE                                               02020010
020300     CLOSE PRINT-FILE                                             02030010
020400     GOBACK.                                                      02040010
020500                                                                  02050010
020600 100-READ-INPUT.                                                  02060010
020700     READ PAY-FILE INTO WS-PAY-REC                                02070010
020800       AT END                                                     02080010
020900         MOVE "YES" TO EOF-FLAG                                   02090083
021000     END-READ                                                     02100010
021100     ADD 1 TO REC-COUNT.                                          02110010
021200                                                                  02120010
021300 300-PROCESS-DATA.                                                02130010
021400     IF PAY-DIV NOT EQUAL DD-DIV                                  02140099
021500        PERFORM 510-DIV-CHANGE                                    02150099
021600     ELSE                                                         02160099
021700        IF PAY-DEPT NOT EQUAL DD-DEPT                             02170099
021800           PERFORM 520-DEPT-CHANGE                                02180099
021900        END-IF                                                    02190099
022000     END-IF                                                       02200099
022100     MOVE PAY-DIV TO PR-DIV                                       02210099
022200     MOVE PAY-DEPT TO PR-DEPT                                     02220099
022300     MOVE PAY-LAST-NAME TO PR-LAST-NAME                           02230099
022400     MOVE PAY-ID-NUMBER TO PR-ID-NUMBER                           02240099
022500     MOVE PAY-HOURS-WORKED TO PR-HOURS-WORKED                     02250099
022600     PERFORM 400-CAL-PAY                                          02260099
022700     PERFORM 700-PRINT-LINE                                       02270099
022800     PERFORM 100-READ-INPUT                                       02280029
022900     .                                                            02290034
023000                                                                  02300078
023100 400-CAL-PAY.                                                     02310025
023200     INITIALIZE PAY-CAL-FIELDS                                    02320099
023300     MOVE PAY-HOURS-WORKED TO PC-HR-USED                          02330055
023400     IF PAY-PAY-TYPE EQUAL "S"                                    02340028
023500         MOVE PAY-EMP-RATE TO PC-GROSS-PAY                        02350099
023600         MOVE PAY-EMP-RATE TO PC-REG-PAY                          02360099
023700         PERFORM 480-NET-PAY                                      02370099
023800     ELSE                                                         02380028
023900         IF PAY-PAY-TYPE EQUAL "H"                                02390028
024000             PERFORM 440-HOURLY                                   02400028
024100         ELSE                                                     02410028
024200             MOVE "*** INVALID PAY TYPE - RECORD IGNORED ***"     02420029
024300               TO PR-REST                                         02430083
024400         END-IF                                                   02440029
024500     END-IF                                                       02450029
024600     .                                                            02460027
024700                                                                  02470042
024800 440-HOURLY.                                                      02480099
024900     IF PC-HR-USED > 40                                           02490028
025000         SUBTRACT 40 FROM PC-HR-USED                              02500099
025100           GIVING PC-OT-HR                                        02510099
025200         MOVE 40 TO PC-REG-HR                                     02520099
025300         MULTIPLY PAY-EMP-RATE BY 1.5                             02530099
025400           GIVING PC-OT-RATE ROUNDED                              02540099
025500         MULTIPLY PC-OT-RATE BY PC-OT-HR                          02550099
025600           GIVING PC-OT-PAY ROUNDED                               02560099
025700     ELSE                                                         02570028
025800         MOVE PC-HR-USED TO PC-REG-HR                             02580028
025900     END-IF                                                       02590029
026000     MULTIPLY PAY-EMP-RATE BY PC-REG-HR                           02600028
026100       GIVING PC-REG-PAY ROUNDED                                  02610044
026200     ADD PC-REG-PAY TO PC-OT-PAY                                  02620099
026300       GIVING PC-GROSS-PAY                                        02630099
026400     PERFORM 480-NET-PAY                                          02640028
026500     .                                                            02650028
026600                                                                  02660042
026700 480-NET-PAY.                                                     02670028
026800     MOVE PAY-EMP-DEDS TO PC-DEDS                                 02680053
026900     SUBTRACT PC-DEDS FROM PC-GROSS-PAY                           02690053
027000       GIVING PC-NET-PAY                                          02700028
027100     MOVE PC-REG-PAY TO PR-REG-PAY                                02710041
027200     MOVE PC-OT-PAY TO PR-OT-PAY                                  02720041
027300     MOVE PAY-EMP-DEDS TO PR-EMP-DEDS                             02730041
027400     MOVE PC-NET-PAY TO PR-NET-PAY                                02740041
027500     ADD PC-HR-USED TO PT-HR-USED                                 02750099
027600     ADD PC-REG-PAY TO PT-REG-PAY                                 02760099
027700     ADD PC-OT-PAY  TO PT-OT-PAY                                  02770099
027800     ADD PC-DEDS TO PT-EMP-DEDS                                   02780099
027900     ADD PC-NET-PAY TO PT-NET-PAY                                 02790099
028000     .                                                            02800041
028100                                                                  02810042
028200 510-DIV-CHANGE.                                                  02820072
028300     PERFORM 520-DEPT-CHANGE                                      02830091
028400     MOVE '   DIVISION TOTALS' TO PR-TOTALS-NAME                  02840099
028500     MOVE DT-HR-USED TO PR-HOURS-WORKED                           02850098
028600     MOVE DT-REG-PAY TO PR-REG-PAY                                02860098
028700     MOVE DT-OT-PAY TO PR-OT-PAY                                  02870098
028800     MOVE DT-EMP-DEDS TO PR-EMP-DEDS                              02880098
028900     MOVE DT-NET-PAY TO PR-NET-PAY                                02890098
029000     MOVE '** ' TO PR-AST                                         02900099
029100     PERFORM 700-PRINT-LINE                                       02910098
029200     ADD DT-HR-USED TO FT-HR-USED                                 02920099
029300     ADD DT-REG-PAY TO FT-REG-PAY                                 02930099
029400     ADD DT-OT-PAY  TO FT-OT-PAY                                  02940099
029500     ADD DT-EMP-DEDS TO FT-EMP-DEDS                               02950099
029600     ADD DT-NET-PAY TO FT-NET-PAY                                 02960099
029700     MOVE 2 TO WS-SPACING                                         02970098
029800     INITIALIZE DIVISION-TOTALS                                   02980073
029900     MOVE PAY-DIV TO DD-DIV                                       02990099
030000     PERFORM 800-PRINT-HEADINGS                                   03000099
030100     .                                                            03010072
030200                                                                  03020072
030300 520-DEPT-CHANGE.                                                 03030077
030400     MOVE '   DEPARTMENT TOTALS' TO PR-TOTALS-NAME                03040099
030500     MOVE PT-HR-USED TO PR-HOURS-WORKED                           03050098
030600     MOVE PT-REG-PAY TO PR-REG-PAY                                03060098
030700     MOVE PT-OT-PAY TO PR-OT-PAY                                  03070098
030800     MOVE PT-EMP-DEDS TO PR-EMP-DEDS                              03080098
030900     MOVE PT-NET-PAY TO PR-NET-PAY                                03090098
031000     MOVE '*  ' TO PR-AST                                         03100099
031100     PERFORM 700-PRINT-LINE                                       03110098
031200     MOVE 2 TO WS-SPACING                                         03120098
031300     ADD PT-HR-USED TO DT-HR-USED                                 03130099
031400     ADD PT-REG-PAY TO DT-REG-PAY                                 03140099
031500     ADD PT-OT-PAY  TO DT-OT-PAY                                  03150099
031600     ADD PT-EMP-DEDS TO DT-EMP-DEDS                               03160099
031700     ADD PT-NET-PAY TO DT-NET-PAY                                 03170099
031800     MOVE PAY-DEPT TO DD-DEPT                                     03180099
031900     INITIALIZE DEPT-TOTALS                                       03190075
032000     .                                                            03200072
032100                                                                  03210072
032200 700-PRINT-LINE.                                                  03220012
032300     PERFORM 750-WRITE                                            03230012
032400     IF WS-LINE-COUNT > 50                                        03240012
032500         PERFORM 800-PRINT-HEADINGS                               03250012
032600     END-IF                                                       03260012
032700     .                                                            03270012
032800                                                                  03280012
032900 750-WRITE.                                                       03290012
033000     WRITE PRINT-REC FROM WS-PRINT-REC                            03300012
033100       AFTER ADVANCING WS-SPACING LINES                           03310012
033200     END-WRITE                                                    03320012
033300     ADD WS-SPACING TO WS-LINE-COUNT                              03330012
033400     MOVE 1 TO WS-SPACING                                         03340085
033500     MOVE SPACES TO WS-PRINT-REC                                  03350012
033600     .                                                            03360012
033700                                                                  03370012
033800 800-PRINT-HEADINGS.                                              03380012
033900     ADD 1 TO WS-PAGE                                             03390012
034000     MOVE WS-PAGE TO H-PAGE                                       03400012
034100     WRITE PRINT-REC FROM HEADING-1                               03410012
034200       AFTER ADVANCING PAGE                                       03420012
034300     END-WRITE                                                    03430012
034400     PERFORM 825-DIV-HEADER                                       03440098
034500     MOVE HEADING-3 TO WS-PRINT-REC                               03450098
034600     MOVE 1 TO WS-SPACING                                         03460099
034700     PERFORM 700-PRINT-LINE                                       03470098
034800     MOVE 2 TO WS-SPACING                                         03480012
034900     MOVE 0 TO WS-LINE-COUNT                                      03490012
035000     .                                                            03500012
035100                                                                  03510099
035200 825-DIV-HEADER.                                                  03520098
035300     MOVE PAY-DIV TO H2-DIV-NO                                    03530099
035400     MOVE 'NO' TO FOUND-FLAG                                      03540098
035500     MOVE 1 TO D-SUB                                              03550098
035600     PERFORM 830-DIV-CHECK                                        03560098
035700       UNTIL FOUND-FLAG = 'YES'                                   03570098
035800       OR D-SUB > M-SUB                                           03580098
035900*  CRITICAL LINE ADDED!                                           03590099
036000     MOVE DIV-NAME(D-SUB) TO H2-DIV-NAME                          03600099
036100     MOVE HEADING-2 TO WS-PRINT-REC                               03610098
036200     MOVE 1 TO WS-SPACING                                         03620098
036300     PERFORM 750-WRITE                                            03630098
036400     .                                                            03640098
036500                                                                  03650098
036600 830-DIV-CHECK.                                                   03660098
036700     IF PAY-DIV = DIV-NO (D-SUB)                                  03670098
036800         MOVE 'YES' TO FOUND-FLAG                                 03680098
036900     ELSE                                                         03690098
037000         ADD 1 TO D-SUB                                           03700098
037100     END-IF                                                       03710098
037200     .                                                            03720098
037300                                                                  03730098
037400 850-GET-DATE.                                                    03740012
037500     ACCEPT WS-DATE FROM DATE                                     03750012
037600     MOVE MM TO H-MM                                              03760012
037700     MOVE DD TO H-DD                                              03770012
037800     MOVE YY TO H-YY                                              03780012
037900     ACCEPT WS-TIME FROM TIME                                     03790012
038000     MOVE HH TO H-HH                                              03800012
038100     MOVE MN TO H-MN                                              03810012
038200     .                                                            03820012
038300                                                                  03830042
038400 900-PRINT-FINAL-TOTALS.                                          03840060
038500     PERFORM 510-DIV-CHANGE                                       03850093
038600     MOVE '   FINAL TOTALS' TO PR-TOTALS-NAME                     03860099
038700     MOVE FT-HR-USED TO PR-HOURS-WORKED                           03870060
038800     MOVE FT-REG-PAY TO PR-REG-PAY                                03880060
038900     MOVE FT-OT-PAY TO PR-OT-PAY                                  03890060
039000     MOVE FT-EMP-DEDS TO PR-EMP-DEDS                              03900060
039100     MOVE FT-NET-PAY TO PR-NET-PAY                                03910060
039200     MOVE '***' TO PR-AST                                         03920099
039300     PERFORM 700-PRINT-LINE                                       03930060
039400     .                                                            03940060
039500                                                                  03950060
