*--------------------------------------------------------------------*
*  SUBROUTINE INITP6                                                 *
*  Author : Joost Wolf, based on routine of Rob Groot                *
*  Date   : originally 1996; revised in november 2011 for rotations  *
*  Purpose: This subroutine reads data required to run program       *
*           interactively from  screen  or in batch-mode             *
*                                                                    *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)   *
*  name    meaning                                     units  class  *
*  ----    -------                                     -----  -----  *
*  IIYEAR   year in which simulation starts              -      T,O  *
*  INYEAR  number of simulation runs (i.e. years)        -      T,O  *
*  IDPLT    day number of planting                        -      T,O *
*  IDEMT    day number of plant emergence                 -      T,O *
*  IFINITT  maximum number of days for single run         d      T,O *
*  IGAP    The maximum difference in days that is allowed            *
*          between subsequent data file records.         d      T,O  *
*  REMARK  remark, printed on top of output files        -      O    *
*  RUNNAME first 5 char's of output file-name (CHAR*5)   -      O    *
*  STATR   name of weather station (CHAR*5)              -      O    *
*  CONTIN  more weather stations                                O    *
*  SENSP   sensitivity parameter for precipitation       -      O    *
*  SENSR    idem for radiation                           -      O    *
*  SENSW    idem for windspeed                           -      O    *
*  SENSV    idem for vapour pressure                     -      O    *
*  SENST    idem for temperature                         -      O    *
*  CO       atmospheric CO2 concentration                ppmv   O    *
*  ISOIL   number of soil type                                  O    *
*  ICROPT  number of crop variety                               O    *
*  IMANAT  number of management type                            O    *     
*  IOPTT    indicates optimal (=1), water limited (=2)               *
*           or water and N limited run (=3)               -     O    *
*  IRRIT    automatic irrigation (=1), actual irrigation             * 
*           from table (=2) or non irrigated(=0)          -     O    *
*  IROT     no. of rotation (i.e. rotat.inp file)         -     O    *
*  IOUT    time interval for output                       d     O    *
*  WTRDIR  directory where weather data are stored                   *
*  SUBROUTINES and FUNCTIONS called : none                           *
*                                                                    *
*  FILE usage : none                                                 *
*--------------------------------------------------------------------*

      SUBROUTINE INITP6 (IIYR,IDSTART,INYEAR,INROT,IDPLT,IDEMT,IFINITT,
     $  IGAP,REMARK,
     $  RUNNAME,STATR,CONTIN,SENSP,SENSV,SENSW,SENSR,SENST,CO,
     $  ISOIL,ICROPT,IOPTT,IRRIT,IYROTA,IOUT,WTRDIR,IMANAT,IROT)


      IMPLICIT REAL (A-Z)
      INTEGER IIYR,INYEAR,IGAP,ISOIL,INROT,IR,IROT,IDSTART, I
      INTEGER IOUT,IIYEAR,IRRIT(10), IYROTA(10),IMANAT(10)
	INTEGER ICROPT(10),IDPLT(10),IDEMT(10),IFINITT(10),IOPTT(10)

      SAVE  
      CHARACTER REMARK*80,RUNNAME*5,STATR*5,WTRDIR*80
      CHARACTER ANS*1,CONTIN*1
      LOGICAL BATCH,START    
      DATA START /.TRUE./    
      DATA BATCH /.FALSE./

*---- maximum difference in days that is allowed between subsequent
*     data in weather-data-files (igap)

      IGAP  = 10
      IOUT= 10
      ISOIL= 1    
      WTRDIR= ' '
      SENSP= 1.
      SENSV= 1.
      SENSW= 1.
      SENSR= 1.
      SENST= 0.
      CO= 370.
	IROT= 1
      CALL CLS

      DO 5  I= 1, 10
	ICROPT(I)= 1
	IMANAT(I)= 1
	IOPTT(I)= 1
	IRRIT(I)= 0
	IFINITT(I)= 330
	IDPLT(I)= 100
	IDEMT(I)= 1
5	CONTINUE


      IF (.NOT. START) GOTO 10 

*    ask user if batch processing is required
     
      WRITE (*, '(/A$)') '  Do you want to run from a batch file',
     $  '  yes (Y) or no (N) ?            ' 
      READ (*,'(A)') ANS
      IF (ANS .EQ. 'Y' .OR. ANS .EQ. 'y') THEN
      BATCH = .TRUE.
      ENDIF

      IF (BATCH) THEN 
*     batch input
      OPEN (99,FILE= 'BATCHR.INP',STATUS= 'OLD')
      CALL MOFILP (99)
      START= .FALSE.
      ENDIF      

10    IF (BATCH) READ (99, *) RUNNAME, IIYEAR, IDSTART, INYEAR, INROT, 
     $   STATR, SENSP, SENSV, SENSW, SENSR, SENST, CO, 
     $   ISOIL, IROT,IOUT,WTRDIR, REMARK, CONTIN

      IF (BATCH)  GOTO 20
      CONTIN= 'N'     
      WRITE (*,'(////////A)') ' Crop and soil water dynamics'
      WRITE (*,'(/////A$)') ' start year for simulation of rotation : '
      READ  (*,'(I4)') IIYEAR

      WRITE (*,'(//A$)') ' simulation during how many years :  '
      READ  (*,'(I2)') INYEAR
      
      WRITE (*,'(//A$)')   ' start day for simulation of rotation:  '
      READ  (*,'(I3)') IDSTART

      WRITE (*,'(//a/a$)')      ' which rotation input file is used? ',
     $     '    (e.g. 1 --> ROTAT1.inp)  '
      READ  (*,'(I1)') IROT

      WRITE (*,'(//2A$)') ' no. of years of rotation',
     $     ' (see INROT in e.g. ROTAT1.inp; e.g. 2 or 6)  : '
      READ  (*,'(I1)') INROT

      WRITE (*,'(//A$)') ' soil type',
     $  ' (e.g.  1--> SOILP1.dat)  : '
      READ (*,'(I1)') ISOIL

      WRITE (*,'(//A$)') ' weather station (max.5 characters): '
      READ  (*,'(a)') STATR

      WRITE (*,'(//a$)') ' output filename (max.5 characters): '
      READ  (*,'(a)') RUNNAME

      WRITE (*,'(//A$)') ' output remarks                    : '
      READ  (*,'(a)') REMARK

20    CONTINUE


*      choice of rotation file
      IF (IROT .EQ. 1) OPEN (109, FILE= 'ROTAT1.inp', STATUS= 'OLD')
	IF (IROT .EQ. 2) OPEN (109, FILE= 'ROTAT2.inp', STATUS= 'OLD')
      IF (IROT .EQ. 3) OPEN (109, FILE= 'ROTAT3.inp', STATUS= 'OLD')
      IF (IROT .EQ. 4) OPEN (109, FILE= 'ROTAT4.inp', STATUS= 'OLD')
      IF (IROT .EQ. 5) OPEN (109, FILE= 'ROTAT5.inp', STATUS= 'OLD')
      IF (IROT .EQ. 6) OPEN (109, FILE= 'ROTAT6.inp', STATUS= 'OLD')
      IF (IROT .EQ. 7) OPEN (109, FILE= 'ROTAT7.inp', STATUS= 'OLD')
      IF (IROT .EQ. 8) OPEN (109, FILE= 'ROTAT8.inp', STATUS= 'OLD')
	CALL MOFILP (109)

*      reading data from rotation file (rotation year, crop file no., planting and emergence dates,
*      maximal growth duration, type of growth limitations, and irrigation or not)
       DO 50 IR= 1, INROT
	 READ (109, *) IYROTA(IR), ICROPT(IR),IDPLT(IR),
     $           IDEMT(IR),IFINITT(IR),IOPTT(IR),IRRIT(IR),IMANAT(IR)
50     CONTINUE      



      IIYR  = IIYEAR - 1900
     

      RETURN
      END