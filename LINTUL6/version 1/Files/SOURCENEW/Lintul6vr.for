*-------------------------------------------------------------------------*
* Copyright 2013. Wageningen University, Plant Production Systems group,  *
* P.O. Box 430, 6700 AK Wageningen, The Netherlands.                      *
* You may not use this work except in compliance with the Licence.        *
* You may obtain a copy of the Licence at:                                *
*                                                                         *
* http://models.pps.wur.nl/content/licence-agreement                      *
*                                                                         *
* Unless required by applicable law or agreed to in writing, software     *
* distributed under the Licence is distributed on an "AS IS" basis,       *
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.*
*-------------------------------------------------------------------------*

*---------------------------------------------------------------------*
*                                                                     *
*     PROGRAM LINTUL6VR.FOR                                           *
*     Author : joost wolf                                             *
*     Date of last revision:November 2011; made suitable for rotations*
*                                                                     *
*     Purpose: This model is the LINTUL-3 fst model but in FORTRAN    *
*              It simulates the growth of a crop as                   *
*              function of intercepted radiation, temperature and     *
*              light use efficiency. Soil water (free drainage) and   *
*              simple nitrogen balances                               *
*              are simulated and also the effects of water and        *
*              nitrogen supply on crop growth. This version includes  *
*              vernalisation effect on phenological development and   *
*              is changed to run crop rotations.                      *
*---------------------------------------------------------------------*

      PROGRAM LINTUL6VR

      IMPLICIT REAL (A-Z)
      INTEGER ISYR,ISYEAR,IUWE,IIYR,INYEAR,IDPL,IDEM,IFINIT,IGAP,ISOIL
      INTEGER ICROP,IOPT,IOUT,IDAY,IYEAR,IWAR,IRRI,IDEMERG,IDFLOW
      INTEGER IDHALT,IAIRDU,IRRIT(10),IYROTA(10),IMANAT(10)
	INTEGER IYRO,IDSTCRP,IYROT,INROT,IMANAG,IROT,IDSTART,IYR
	INTEGER ICROPT(10),IDPLT(10),IDEMT(10),IFINITT(10),IOPTT(10)

      CHARACTER RUNNAME*5,REMARK*80,STATR*5,CONTIN*1,WTRDIR*80
      COMMON /ITIME/ ISYR, ISYEAR,IYR

      LOGICAL TERMIN,INITI,INITC,TERMY,PL,EMERG,YCH,CRP      
      
*-----iunit number for weather
      DATA IUWE/65/, DVSEND/2.0/

*---- initialize program
*     LOOP for different weather stations

      
1      CALL INITP6 (IIYR,IDSTART,INYEAR,INROT,IDPLT,IDEMT,IFINITT,
     $  IGAP,REMARK,RUNNAME,
     $  STATR,CONTIN,SENSP,SENSV,SENSW,SENSR,SENST,CO,
     $  ISOIL,ICROPT,IOPTT,IRRIT,IYROTA,IOUT,WTRDIR,IMANAT,IROT)

       TERMY= .FALSE.
	 INITI = .TRUE.
	 INITC= .FALSE.
	 IDEND= IDSTART

      IYEAR = IIYR
*---- initial year of the rotation
      ISYR = IYEAR 
      ISYEAR= IYEAR + 1900
	IDAY= IDSTART
	IYRO= 1


*---- LOOP over years for a number of crops (IYRO) in the rotation
*---- No. of years can be longer than the rotation
5     CONTINUE
            
	CRP= .FALSE.
      TERMIN = .FALSE.
	YCH= .FALSE.

      ICROP= ICROPT(IYRO)
	IDPL= IDPLT(IYRO)
	IDEM= IDEMT(IYRO)
	IFINIT= IFINITT(IYRO)
	IOPT= IOPTT(IYRO)
	IRRI= IRRIT(IYRO)
	IYROT= IYROTA(IYRO)
	IMANAG= IMANAT(IYRO)

*---- Start date of growing season (planting or emergence)
      IF (IDPL .LE. 0) THEN
        IDSTCRP= IDEM
        PL= .FALSE.
      ELSE 
        IDSTCRP= IDPL
        PL= .TRUE.
      ENDIF
      

*---- LOOP for one growth period (year)     
     
10       CONTINUE

*---- start of growing season (planting or emergence)
        IF (IDAY .EQ. IDSTCRP) CRP= .TRUE.
	  IF (IDAY .EQ. IDSTCRP) INITC= .TRUE.
	  IF (TERMIN) CRP= .FALSE.

*------- read weather data
         CALL WEATHR (IWAR,WTRDIR,STATR,IYEAR,IDAY,IGAP,LONGIE,LATIN,
     $                ALTI,TMIN,TMAX,DTR,RAIN,VAP,WIND)


* ----- to prevent temperature effect on relative humidity
          
          TMPA = (TMIN + TMAX)/2.
          SVAP1  = 6.10588 * EXP (17.32491*TMPA/(TMPA+238.102))
          VAP= AMIN1(VAP,SVAP1)
          RH= VAP/SVAP1

* ------  sensitivity analyses
        
           TMIN= TMIN + SENST
           TMAX= TMAX + SENST  
         
           TMPA = (TMIN + TMAX)/2.
           SVAP2  = 6.10588 * EXP (17.32491*TMPA/(TMPA+238.102))
           VAP= RH*SVAP2
         
               
           VAP= AMIN1(SVAP2, VAP * SENSV)
           WIND= WIND * SENSW
           DTR= DTR * SENSR
           RAIN= RAIN * SENSP
          
*------- calculate daylength

         CALL ASTRO (IDAY,LATIN,DAYL,DAYLP,SINLD,COSLD)                    


*------- calculate potential soil evaporation and crop transpiration

         CALL PENMAN (IDAY,DAYL,SINLD,COSLD,ALTI,TMIN,TMAX,
     $                DTR,WIND,VAP,CO,E0,ES0,ETC,AVRAD)


*------- calculate soil water balance  

         CALL WATBALS6(ICROP,ISOIL,INITI,IOPT, IRRI,TERMIN,EMERG,IDAY,
     $              ES0,ETC,RAIN, FINT,DEPNR,RD,RDMCR,RR,RDM,IMANAG,
     $              CFET,IAIRDU,SMACT,SMACTL,TTRANS,TDRAIN,TRAIN,TESOIL,
     $           TRUNOF,TIRR,TRANRF,RUNFR,WTOT, WTOTL,WAVT,WAVTL,INITC,
     $           CRP,WTOTLX,TERMY,IYRO,IYEAR,IIYR)


*------- when finish conditions for crop or rotation are reached (TERMIN .TRUE.)

*-------- calculate crop growth
         
         IF (TERMIN) THEN
         CALL CROPV(ICROP,INITC,IOPT, IDAY,IDEM,IDEMERG,IDPL,IDFLOW,
     $     IDHALT,PL,TERMIN,EMERG,TMIN,TMAX,AVRAD,CO,TRANRF,RDMSO,DAYLP,
     $	    TAGB,WLVG, WLVD, WST,WRT,WSO,RD,RDMCR,RR,RDM,LAI,IMANAG,
     $        CFET,DEPNR,IAIRDU,TSUM,DVS,DVSEND,TSULP,FINT,TPARINT,TPAR,
     $        TSUML,NNI,NMINT,NMIN,NUPTT,NFIXTT,NLIVT,NLOSST,YCH)

*------- daily output
           CALL DAILOUT6 (IOPT,IDAY,IYEAR,IOUT,IDEM,IDPL,IDEMERG,
     $                   WLVG, WLVD, WST,WSO,TAGB,TPARINT,TPAR,
     $                   REMARK,DVS,RUNNAME,LAI,RD,
     $             SMACT,SMACTL,TTRANS,TDRAIN,TRAIN,TESOIL,TRUNOF,TIRR,
     $             TRANRF,TERMIN,STATR,ISOIL,ICROP, IRRI,RUNFR,CO,
     $          WTOT,WAVT,WTOTL,WAVTL,WTOTLX,TSUML,NNI,NMINT,NMIN,NUPTT,
     $                   NFIXTT,NLIVT,NLOSST,INITI,TERMY,
     $                   IROT,IYROT,INYEAR,INROT,IDSTART,IMANAG)

*------- yearly output
           CALL STOUT6 (RUNNAME,REMARK,STATR,ISOIL,ICROP,TERMY,IYEAR,
     $       IDEMERG,IDPL,IDFLOW,IDHALT,CO,IOPT,IROT,IDSTART,INYEAR,
     $       IRRI,IMANAG)

         GOTO 20

         ENDIF

*-------- calculate crop growth

         IF (CRP) CALL CROPV(ICROP,INITC,IOPT, IDAY,IDEM,IDEMERG,IDPL,
     $   IDFLOW, IDHALT,PL,TERMIN,EMERG,TMIN,TMAX,AVRAD,CO,TRANRF,RDMSO,
     $	DAYLP,TAGB,WLVG, WLVD, WST,WRT,WSO,RD,RDMCR,RR,RDM,LAI,IMANAG,
     $        CFET,DEPNR,IAIRDU,TSUM,DVS,DVSEND,TSULP,FINT,TPARINT,TPAR,
     $        TSUML,NNI,NMINT,NMIN,NUPTT,NFIXTT,NLIVT,NLOSST,YCH)

*------- Finish conditions for total number of rotation years (TERMY= .TRUE.)
        IF ( IYEAR .EQ. IIYR + INYEAR .AND. IDAY .EQ. IDEND) THEN
         TERMY= .TRUE.
        ENDIF
   
*------- daily output
         CALL DAILOUT6 (IOPT,IDAY,IYEAR,IOUT,IDEM,IDPL,IDEMERG,
     $                   WLVG, WLVD, WST,WSO,TAGB,TPARINT,TPAR,
     $                   REMARK,DVS,RUNNAME,LAI,RD,
     $              SMACT,SMACTL,TTRANS,TDRAIN,TRAIN,TESOIL,TRUNOF,TIRR,
     $              TRANRF,TERMIN,STATR,ISOIL,ICROP, IRRI,RUNFR,CO,
     $          WTOT,WAVT,WTOTL,WAVTL,WTOTLX,TSUML,NNI,NMINT,NMIN,NUPTT,
     $                   NFIXTT,NLIVT,NLOSST,INITI,TERMY,
     $                   IROT,IYROT,INYEAR,INROT,IDSTART,IMANAG)


        IF (TERMIN) GOTO 15 
	  IF (TERMY) GOTO 20  


*------- update daynumber and year

         CALL TIMER (IDAY,IYEAR,IFINIT,CRP,TERMIN,INITC,DVS,DVSEND,YCH)

         INITI = .FALSE.  
	   INITC=  .FALSE.              
 
 
15      GOTO 10

20      CONTINUE
            
        IYRO= IYRO+1
	  IF (IYRO .GT. INROT) IYRO= IYRO - INROT

        IF (IYEAR .NE. IIYR + INYEAR)  GO TO 5

*------- calculate final soil water balance  

         CALL WATBALS6(ICROP,ISOIL,INITI,IOPT, IRRI,TERMIN,EMERG,IDAY,
     $              ES0,ETC,RAIN, FINT,DEPNR,RD,RDMCR,RR,RDM,IMANAG,
     $              CFET,IAIRDU,SMACT,SMACTL,TTRANS,TDRAIN,TRAIN,TESOIL,
     $           TRUNOF,TIRR,TRANRF,RUNFR,WTOT, WTOTL,WAVT,WAVTL,INITC,
     $           CRP,WTOTLX,TERMY,IYRO,IYEAR,IIYR)

*------- statistical analysis
         CALL STOUT6 (RUNNAME,REMARK,STATR,ISOIL,ICROP,TERMY,IYEAR,
     $       IDEMERG,IDPL,IDFLOW,IDHALT,CO,IOPT,IROT,IDSTART,INYEAR,
     $       IRRI,IMANAG)


*------- runs for new site or years ?

         IF (CONTIN .EQ. 'Y' .OR. CONTIN .EQ. 'y') GOTO 1
      END
