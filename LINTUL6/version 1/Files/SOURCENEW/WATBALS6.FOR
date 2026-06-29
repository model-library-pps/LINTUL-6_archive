*------------------------------------------------------------------------ *
*  SUBROUTINE WATBALS6                                                     *
*  Author: Joost Wolf                                                     *
*  Date  : May 2011                                                       *
*  Purpose: This subroutine, as adapted to the LINTUL model,  simulates   *
*  the soil water status in the                                           *
*  maximum effective rooted zone, of which the depth can be limited by    *
*  both crop and soil characteristics. The following processes are taken  *
*  into account:                                                          *
*    - soil surface evaporation                                           *
*    - crop transpiration                                                 *
*    - precipitation                                                      *
*    - surface runoff                                                     *
*    - irrigation                                                         *
*    - drainage to the sub-soil                                           *
*                                                                         *
*  FORMAL PARAMETERS:(I= input, O= output, C= control, IN= init., T-time) *
*  name     meaning                                  units       class    *
*  ----     -------                                  -----       -----    *
*   ISOIL   number of soil data set                   -            I      *  
*   INITI   indicates initialization of run           -            I,C    *
*   INITC   indicates initialization of growing season -           I,C    *
*   CRP     indicates growing season                  -            I,C    *
*   IOPT    indicates optimal (=1), water limited or                      *
*            or water and N limited conditions        -            I,C    *
*   IRRI    automatic irrigation (=1), actual irrigation                  * 
*           from table (=2) or non irrigated(=0)      -            I,C    *
*   TERMIN  indicates terminal section for growing season          I,C    *
*   TERMY   indicates end of rotation                              I,C    *              
*   EMERG   indicates crop emergence                  -            I,C    *
*   IDAY    Julian day number                         -            I      *
*   ES0     potential evaporation from soil surface   cm d-1       I      *
*   ETC     potential transpiration of crop           cm d-1       I      *   
*   RAIN    precipitation                             cm d-1       I      *
*   FINT    fractional light interception              -           I      *
*   DEPNR   crop group number for soil water depletion -           I      *
*   RD      actual rooting depth                      cm           I      *
*   RDMCR   crop specific maximum rooting depth       cm           I      *
*   RR      root growth rate                          cm d-1       I      *
*   RDM     crop/soil related maximal rooting depth   cm           O      *
*   CFET    crop specific correction for transpiration -           I      * 
*   IAIRDU air ducts in roots present (=1) or not(=0)  -           I,C    *
*   SMACT   actual soil mosture content               cm3 cm-3     O      *
*   TTRANS  cumulative crop transpiration             cm           O      *
*   TDRAIN  cumulative drainage to the sub-soil       cm           O      *
*   TRAIN   cumulative precipitation                  cm           O      *
*   TESOIL  cumulative soil evaporation               cm           O      *
*   TRUNOF  cumulative surface runoff                 cm           O      *
*   TIRR    cumulative irrigation                     cm           O      *
*   TRANRF  reduction as a result of drought/wetness  -            O      *
*   RUNFR   fraction of precipitation lost by runoff  -            O      *
*   WTOT    total water in rooted zone                cm           O      * 
*   WTOTL   total water in lower zone                 cm           O      * 
*   WAVT    total available water in rooted zone      cm           O      *
*   WAVTL   total available water in lower rooted zone cm          O      *
*   WTOTLX  total water in lowest zone                cm           O      *
*  File usage: none                                                       *
*------------------------------------------------------------------------
      SUBROUTINE WATBALS6(ICROP,ISOIL,INITI,IOPT, IRRI,TERMIN,EMERG,
     $            IDAY,ES0,ETC,RAIN, FINT,DEPNR,RD,RDMCR,RR,RDM,IMANAG,
     $              CFET,IAIRDU,SMACT,SMACTL,TTRANS,TDRAIN,TRAIN,TESOIL,
     $            TRUNOF,TIRR,TRANRF,RUNFR,WTOT, WTOTL,WAVT,WAVTL,INITC,
     $             CRP,WTOTLX,TERMY,IYRO,IYEAR,IIYR)


       IMPLICIT REAL (A-Z)
       INTEGER ISOIL,IRRI,IRRI1, IDAY, IAIRDU, ILIRRT,ICROP,IOPT
	 INTEGER IMANAG,IYRO, IYEAR, IIYR
       LOGICAL TERMIN,INITI,EMERG,INITC,CRP,TERMY
	 REAL IRRTAB (30)
       COMMON /WATBOUT/ TTRANS1, TDRAIN1, TRAIN1, TESOIL1, WAVT1,WAVTL1
       COMMON /WATBOUT/ TRUNOF1, TIRR1, RUNFR1, IRRI1,WAVTLX1, RD1

*----------------------------------------------------
*      INITIALIZATION
*----------------------------------------------------

       IF (.NOT. INITI) GO TO 200
 
*-----First and complete initialisation soil water balance
      
*---- Reading physical soil properties from data file SOILP.DAT
       
       IF (ISOIL .EQ. 1) CALL RDINIT (21,0, 'SOILP1.DAT')
       IF (ISOIL .EQ. 2) CALL RDINIT (21,0, 'SOILP2.DAT')
       IF (ISOIL .EQ. 3) CALL RDINIT (21,0, 'SOILP3.DAT')
       IF (ISOIL .EQ. 4) CALL RDINIT (21,0, 'SOILP4.DAT')

       CALL RDSREA ('SMDRY',SMDRY)
       CALL RDSREA ('SMW',SMW)
       CALL RDSREA ('SMFC',SMFC)
       CALL RDSREA ('SM0',SM0)
       CALL RDSREA ('SMI',SMI)
       CALL RDSREA ('SMLOWI',SMLOWI)
       CALL RDSREA ('RDMSO',RDMSO)
       CALL RDSREA ('RUNFR',RUNFR)
       CALL RDSREA ('CFEV',CFEV)
	 CALL RDSREA ('KSUB',KSUB)
       CALL RDSREA ('CRAIRC',CRAIRC)

       CLOSE (21, STATUS= 'DELETE')

*     Reading actual irrigation data
      IF (IMANAG .EQ. 1) Call RDINIT (22,0, 'MANAG1.DAT')
      IF (IMANAG .EQ. 2) Call RDINIT (22,0, 'MANAG2.DAT')
      IF (IMANAG .EQ. 3) Call RDINIT (22,0, 'MANAG3.DAT')
      IF (IMANAG .EQ. 4) Call RDINIT (22,0, 'MANAG4.DAT')
      IF (IMANAG .EQ. 5) Call RDINIT (22,0, 'MANAG5.DAT')

       call RDAREA ('IRRTAB', IRRTAB, 30, ILIRRT)
	 CLOSE (22, STATUS= 'DELETE')

*     Reading crop data
       IF (ICROP .EQ. 1) CALL RDINIT (14,0, 'CROPP1.DAT')
       IF (ICROP .EQ. 2) CALL RDINIT (14,0, 'CROPP2.DAT')
       IF (ICROP .EQ. 3) CALL RDINIT (14,0, 'CROPP3.DAT')
       IF (ICROP .EQ. 4) CALL RDINIT (14,0, 'CROPP4.DAT')
       IF (ICROP .EQ. 5) CALL RDINIT (14,0, 'CROPP5.DAT')
       IF (ICROP .EQ. 6) CALL RDINIT (14,0, 'CROPP6.DAT')
       call RDSREA ('RDMCR', RDMCR)
       call RDSREA ('RDI', RDI)
	 CLOSE (14, STATUS= 'DELETE')


*----- initialization of water balance

       TDRAIN= 0.
	 PERC3= 0.
	 PERC4= 0.
       TRAIN= 0.
       RAIN0= 0.

       TTRANS= 0.
       TESOIL= 0.
       TRA= 0.
       EVA= 0.

       TIRR= 0.
       DIRR= 0.
	 DIRRO= 0.  
	 RIRR= 0.   
       TRUNOF= 0.

       RIRR= 0.
       RUNOF= 0. 
       FINT = 0.

*----- Initialization of soil moisture content (SMACT, cm3/cm3), available amounts of water
*      (above wilting point) in maximum effective rooted zone, both actual (WAVT) 
*      and total amount of water in maximum rooted zone (WTOT),
*      all in cm.

       SMACT= AMAX1(SMW, AMIN1(SMI,SMFC))
	 SMACTL= AMAX1( SMW, AMIN1(SMLOWI,SMFC))
	 SMACTX= AMAX1( SMW, AMIN1(SMLOWI,SMFC))
	 RDM= AMIN1(RDMSO, RDMCR)
	 RD= RDI
       WAVTII= RDI * (SMACT - SMW)
       WAVTLII= (RDM-RDI) * (SMACTL - SMW)
	 WAVTLXII= (RDMSO-RDM) * ( SMACTX - SMW)

       WAVT= WAVTII
	 WAVTL= WAVTLII
       WTOT= RDI * SMACT
	 WTOTL= (RDM-RDI) * SMACTL 
       WTOTN= WTOT
	 WTOTLN= WTOTL
	 WTOTLX= (RDMSO - RDM) * SMACTX
	 DWOTLX= 0.
       DWAT= 0.
       DWATL= 0.
	 DWOT= 0.
	 DWOTL= 0.
	 WDR= 0.
	 TWDR= 0.    

       DSLR= 3.
200     CONTINUE

       IF (.NOT. INITC .OR. INITI) GOTO 300
*----  initialization for new growing season

*     Reading actual irrigation data
      IF (IMANAG .EQ. 1) Call RDINIT (22,0, 'MANAG1.DAT')
      IF (IMANAG .EQ. 2) Call RDINIT (22,0, 'MANAG2.DAT')
      IF (IMANAG .EQ. 3) Call RDINIT (22,0, 'MANAG3.DAT')
      IF (IMANAG .EQ. 4) Call RDINIT (22,0, 'MANAG4.DAT')
      IF (IMANAG .EQ. 5) Call RDINIT (22,0, 'MANAG5.DAT')

       call RDAREA ('IRRTAB', IRRTAB, 30, ILIRRT)
	 CLOSE (22, STATUS= 'DELETE')

*     Reading crop data
       IF (ICROP .EQ. 1) CALL RDINIT (14,0, 'CROPP1.DAT')
       IF (ICROP .EQ. 2) CALL RDINIT (14,0, 'CROPP2.DAT')
       IF (ICROP .EQ. 3) CALL RDINIT (14,0, 'CROPP3.DAT')
       IF (ICROP .EQ. 4) CALL RDINIT (14,0, 'CROPP4.DAT')
       IF (ICROP .EQ. 5) CALL RDINIT (14,0, 'CROPP5.DAT')
       IF (ICROP .EQ. 6) CALL RDINIT (14,0, 'CROPP6.DAT')
       call RDSREA ('RDMCR', RDMCR)
       call RDSREA ('RDI', RDI)
	 CLOSE (14, STATUS= 'DELETE')

*----- Resetting amounts of water in soil layers for new crop with new rooting depth 
       SMACT= AMAX1(SMW, AMIN1(SMACT,SMFC))
	 RD= RDI
       RDM= AMIN1(RDMSO, RDMCR) 
       WAVTI= RDI * (SMACT - SMW) 
       WAVT= WAVTI
       WTOT= RDI * SMACT

	 WTOTTI= TWTOT - WTOT
	 WTOTL= WTOTTI * (RDM - RDI) / (RDMSO -RDI)
	 WTOTLX= WTOTTI * (RDMSO - RDM) / (RDMSO - RDI)
       WAVTLI= WTOTL - (RDM - RDI) * SMW
	 WAVTL= WAVTLI
	 WAVTLXI= WTOTLX - (RDMSO - RDM) * SMW 
	 SMACTL= WTOTL / (RDM - RDI)
	 SMACTX= WTOTLX / AMAX1 (0.001,(RDMSO-RDM))
       WTOTN= WTOT +DWOT
	 WTOTLN= WTOTL + DWOTL
	 WDR= 0.
	 TWDR= 0.    

300    CONTINUE

*      IF (TERMIN) GOTO 999

        
*---------------------------------------------------------
*      INTEGRATION
*---------------------------------------------------------


*------- Integrals for drainage, rainfall, runoff, irrigation, transpiration, 
*        evaporation, total and available water in root and lower (between maximal and actual root) zones (cm),
*        and water added to root zone by root growth

         TDRAIN= INTGRL(TDRAIN, PERC4, 1.)
         TRAIN= INTGRL(TRAIN, RAIN0, 1.)
         TRUNOF= INTGRL(TRUNOF, RUNOF, 1.)
         TIRR= INTGRL(TIRR, RIRR, 1.)
         TESOIL= INTGRL(TESOIL, EVA, 1.)
         TTRANS= INTGRL(TTRANS, TRA, 1.)
         WTOT= INTGRL( WTOT, DWOT, 1.)
         WTOTL= INTGRL( WTOTL, DWOTL, 1.)
	   WTOTLX= INTGRL( WTOTLX,DWOTLX, 1.)
         WAVT= INTGRL( WAVT, DWAT, 1.)
	   WAVTL= INTGRL( WAVTL, DWATL, 1.)
	   TWDR= INTGRL(TWDR, WDR, 1.)

	   TWTOT= WTOT + WTOTL + WTOTLX

*------- Actual soil moisture content in root and lower zones(cm3/cm3)
         SMACT= WTOT/MAX(0.0001,RD)
	   SMACTL= WTOTL/MAX(0.0001,RDM-RD) 
	   SMACTX= WTOTLX/MAX(0.0001,RDMSO-RDM)
      
*-----------------------------------------------------------------------------
*      RATE CALCULATIONS
*-----------------------------------------------------------------------------

         DAY= REAL(IDAY)
*------ Infiltration of precipitation (RAIN) and irrigation (RIRR) (cm)

         RIRR= DIRR
         PERC= (1.-RUNFR) * RAIN + RIRR
         
*------ Water loss by surface runoff, preliminary

         RUNOFP= RUNFR * RAIN

*-------water added to root zone by root growth (cm/d), resp. total and available water
         IF (EMERG) WDR= RR * SMACTL 
         IF (EMERG) WDRA= RR * (SMACTL-SMW) 

*------ Maximum transpiration rate (cm/d) as function of light interception (FINT) and 
*       crop-specific correction factor of potential transpiration (CFET)
     
         TRMAX= MAX(0.0001,CFET * ETC * FINT)

*------ Actual transpiration (cm/d) as function soil moisture content (SMACT)
 
         SWDEP= SWEAF(ETC,DEPNR)
         SMCR= (1. - SWDEP) * (SMFC - SMW) + SMW
         RDRY= LIMIT(0., 1., (SMACT - SMW)/(SMCR - SMW))

        IF (IAIRDU .NE. 1) THEN
*        reduction in transpiration in case of oxygen shortage
*        for non-rice crops (IAIRDU not 1)

*        critical soil moisture content for aeration
         SMAIR = SM0 - CRAIRC
*        count days since start oxygen shortage (up to 4 days)
         IF (SMACT.GE.SMAIR) DSOS = MIN ((DSOS+1.),4.)
         IF (SMACT.LT.SMAIR) DSOS = 0.
*        maximum reduction reached after 4 days
         RWETMX = LIMIT (0.,1.,(SM0-SMACT)/(SM0-SMAIR))
         RWET   = RWETMX + (1.-DSOS/4.)*(1.-RWETMX)
        ELSE
*       no reduction for rice crops
	   RWET   = 1.
        END IF
         
*        actual crop transpiration rate reduced for drought and for oxygen shortage; not done for optimal conditions
         IF (IOPT .EQ. 1) THEN
	     TRA= TRMAX
	   ELSE
           TRA= MAX(0.0, MIN(WAVT, RDRY * RWET * TRMAX))
*        growth reduction function for drought or oxygen shortage
         END IF

         TRANRF= TRA/TRMAX

	   IF (.NOT. CRP)  THEN
	   TRA= 0.0
	   TRANRF= 1.
	   ENDIF



*------- Maximum soil evaporation rate (cm/d) as function of light interception (FINT)

         EVMAX= ES0 * (1. - FINT)

*------- Actual soil evaporation rate (cm/d) in dependence of days since last rain, last rainfall and topsoil moisture

         IF (PERC .GE. 0.5) THEN
           EVA= EVMAX
           DSLR= 1.
         ELSE 
           DSLR= DSLR + 1.
           EVMAXT= EVMAX * LIMIT(0.,1.,(SQRT (DSLR) - SQRT(DSLR-1.))
     $               * CFEV)
           EVA= AMAX1(0., AMIN1(EVMAX, EVMAXT + PERC,10.*(SMACT-SMDRY)))
         END IF

*------ Water capacity of rooted and lower zone, at field capacity and at soil saturation (cm)

         CAP= (SMFC - SMACT) * RD
	   CAPL= (SMFC - SMACTL) * (RDM - RD)
	   CAPLX= (SMFC - SMACTX) * (RDMSO - RDM)
	   CAP0= (SM0 - SMACT) * RD
	   CAPL0= (SM0 - SMACTL) * (RDM - RD)
	    

*------ Effective percolation incl. ET losses, potential and next dependent on soil water holding capacity (cm/d)
         PERC1P= PERC - EVA - TRA
	   PERC1=  MIN(KSUB + CAP0, PERC1P)


*------ water loss by surface runoff, final
         RUNOF= RUNOFP + MAX(0.,PERC1P -PERC1)
         
*------ Rooted zone is filled up to field capacity and 
*        the excess amount is lost by percolation to lower zone

         IF (CAP .LE. PERC1) THEN
            PERC2= MIN(KSUB + CAPL0, PERC1 - CAP)
         ELSE
            PERC2= 0.
         END IF

*------ Lower root zone is filled up to field capacity and 
*        the excess amount is lost by drainage to lowest (third) zone

         IF (CAPL .LE. PERC2) THEN
            PERC3= MIN(KSUB, PERC2 - CAPL)
         ELSE
            PERC3= 0.
         END IF

*------ Lowest (third) zone (below maximal rooted depth) is filled up to field capacity and 
*        the excess amount is lost by drainage to lower  zone

         IF (CAPLX .LE. PERC3) THEN
            PERC4= MIN(KSUB, PERC3 - CAPLX)
         ELSE
            PERC4= 0.
         END IF

*---- irrigation water application on previous day
       DIRROLD= DIRR 

*------ Demand for irrigation water

*       Automatic irrigation (cm)
        WAVFC= RD * (SMFC - SMW)
        IF (IRRI .EQ. 1 .AND. (SMACT .LE. (SMCR + 0.02)) 
     $      .AND. RAIN .LT. 1. .AND. CRP) THEN 
            DIRR= LIMIT(0., 1., 0.7 * (WAVFC - WAVT))
	      DIRRO= 0.
        ELSE IF (IRRI .EQ. 2 .AND. CRP) THEN
*       Actual effective irrigation from table	 
	      DIRR1= LINT (IRRTAB,ILIRRT, DAY) + DIRRO
            DIRR= MIN(1., DIRR1)
	      DIRRN= DIRR1 - DIRR
	      DIRRO= DIRRN
        ELSE
            DIRR= 0.
	      DIRRO= 0.
        END IF


*----- precipitation during last day
        RAIN0= RAIN

*------ Change in total water and available water (DWAT) in rooted, lower and lowest zones
        DWOT= PERC1 - PERC2 + WDR
        DWOTL= PERC2 - PERC3 -WDR
	  DWOTLX= PERC3 -PERC4
        DWAT= PERC1 - PERC2 + WDRA
	  DWATL= PERC2 - PERC3 -WDRA

*------ New amount of water in rooted and lower zones (after rate calculations)

        WTOTN= INTGRL(WTOTN,DWOT, 1.)
	  WTOTLN= INTGRL(WTOTLN,DWOTL,1.)

*------ Check on soil water balance: WBAL and WBALL should be zero.

         WBAL= ABS(WTOT -TRA - EVA + RAIN + WDR + DIRROLD  
     $             - RUNOF - PERC2 - WTOTN)


        IF ((.NOT.INITC) .AND.(WBAL .GE. 1.E-4)) STOP
     $    ' soil moisture balance WBAL not 0, program aborted'

         WBALL= ABS(WTOTL + PERC2 - PERC3 -WDR - WTOTLN)
      
        IF ((.NOT. INITC) .AND. (WBALL .GE. 1.E-4)) STOP
     $    ' soil moisture balance WBALL not 0, program aborted'

999      CONTINUE

* ----- Data for summary output

        IF (TERMIN .OR. TERMY) THEN

        TTRANS1= TTRANS
        TDRAIN1= TDRAIN
        TRAIN1= TRAIN
        TESOIL1= TESOIL
        TRUNOF1= TRUNOF
        TIRR1= TIRR
        IRRI1= IRRI
        RUNFR1= RUNFR
	  WAVT1= WAVT
	  WAVTL1= WAVTL
        WAVTLX= (RDMSO -RDM) * (SMACTX - SMW)
	  WAVTLX1= WAVTLX
	  RD1= RD

        END IF

         RETURN
         END