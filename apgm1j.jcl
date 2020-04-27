//CSU0020C JOB 'C3111',REGION=0M,CLASS=A,MSGCLASS=A,TIME=(0,5),         00010038
//     LINES=5,MSGLEVEL=(0,0),NOTIFY=&SYSUID                            00020000
//JOBLIB   DD   DSN=CSU.PUBLIC.LOADLIB,DISP=SHR                         00030003
//         SET  ID=20                                                   00040038
//*   SET 20 ABOVE ONE TIME - SET NEXT TWO FOR EACH PROGRAM             00050038
//*        SET  PROGNUM=6                                               00060054
//*        SET  SOURCE=LOOKUP                                           00070054
//         SET  PROGNUM=3                                               00080054
//         SET  SOURCE=LABQZ3                                           00090054
//*        SET  PROGNUM=1                                               00100045
//*        SET  SOURCE=DOCTEST                                          00110045
//         SET  PRGNAME=PGM&PROGNUM&ID                                  00120000
//COMPILE  EXEC IGYWCL,GOPGM=&PRGNAME,                                  00130000
//   PGMLIB='CSU.PUBLIC.LOADLIB'                                        00140000
//SYSLIB   DD   DSN=CSU.PUBLIC.COPYLIB,DISP=SHR                         00150000
//COBOL.SYSPRINT DD SYSOUT=*                                            00160000
//SYSIN    DD   DSN=CSU.PUBLIC.DATA(OPTIONX),DISP=SHR                   00170003
//         DD   DSN=CSU00&ID..C3111.COBOL(&SOURCE),DISP=SHR             00180003
//LKED.SYSIN   DD DUMMY                                                 00190003
//LKED.SYSLMOD DD DSN=CSU.PUBLIC.LOADLIB(&PRGNAME),DISP=SHR             00200003
//*                                                                     00210000
// IF (RC = 00) THEN                                                    00220003
//STEP020  EXEC PGM=&PRGNAME                                            00230000
//*YINFILE   DD DSN=CSU00&ID..C3111.COBOL(PAYSAMP),DISP=SHR             00240041
//MYINFILE   DD DSN=CSU.PUBLIC.DATA(PAYROLL),DISP=SHR                   00250041
//MYREPORT   DD SYSOUT=*,OUTLIM=2500                                    00260003
//SYSUDUMP   DD DUMMY                                                   00270003
//SYSDUMP    DD DUMMY                                                   00280003
// ENDIF                                                                00290000
//                                                                      00300000
