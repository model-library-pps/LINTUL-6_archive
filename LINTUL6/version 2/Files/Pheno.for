*---------------------------------------------------------------------------*
*  SUBROUTINE PHENO                                                         *
*  Author : Rob Groot                                                       *
*  Date   : november 1985                                                   *
*  Purpose: This subroutine calculates the development rate of the          *
*	    crop based on average daily temperatures and daylength.         *
*           References to literature :                                      *
*           Weir, A.H., P.L. Bragg, J.R. Porter & J.H. Rayner, 1984.        *
*              A winter wheat crop simulation model without water or        *
*              nutrient limitations. J.Agric. Sci.Camb. 102: 371-382.       *
*           Porter, J.R., 1984. A model of canopy development in            *
*              winter wheat. J.agric.Sci.,Camb., 102,383-392.               *
*           Reinink, K., I. Jorritsma & A. Darwinkel, 1986.                 *
*              Adaptation of the AFRC wheat phenology model for Dutch       *
*              conditions. Neth. J. Agric. Sc., 34:1-13.                    *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                    units  class         *
*  ----     -------                                    -----  -----         *
*  TMPA     average daily air temperature               C       I           *
*  TSUMEM    required degree days for emergence         C.day   I           *
*  TSUMFI    required degree days for floral initiat.   C.day   I           *
*  TSUMDR    required degree days for double ridge      C.day   I           *
*  TSUMAN    required degree days for anthesis          C.day   I           *
*  TSUMMA    required degree days from anth.to maturity C.day   I           *
*  DAYLP    photosynthetically active daylength         h       I           *
*  TMPSUM   temperature sum                             C.day   O           *
*  DVR1     pre- anthesis development rate              day-1   O           *
*  DVR2     post-anthesis development rate              day-1   O           *
*  VBASE    base number of vernalization days                   I           *
*  VERSAT   saturation number of vernalization days             I           *
*  SDAYL    saturating day length for photoperiod               I           *
*                   effects on phenology                 h      I           *
*  IVERN    0 = no vernalization effect (spring wheat),         I           *
*           1 = vernalization effect (winter wheat)                         *                                                *
*  SUBROUTINES and FUNCTIONS called : none                                  *
*  FILE usage : none                                                        *
*---------------------------------------------------------------------------*

      SUBROUTINE PHENO (TMPA,TSUMEM,TSUMFI,TSUMDR,TSUMAN,TSUMMA,
     $  DAYLP,TMPSUM,DVR1,DVR2,VBASE,VERSAT,SDAYL,IVERN,INIT)

      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)
      REAL INTGRL,INSW
      LOGICAL INIT
      DIMENSION VERNRT(16)


*---- Vernalization rate as a function of temperature

      DATA VERNRT/-20.,0.,  -4.,0., 0., 1., 3.,1., 7., 0.8, 
     $             9.,0.4,18.,0.0, 40.0,0.0/

      DATA IVERRN/16/	
    

      IF (INIT) THEN
      VERN = 0.0
      TMPSUM = 0.0
      ENDIF

*---- Vernalizationfactor

      VERNR = AFGEN(IVERRN,VERNRT,TMPA)
      VERN  = INTGRL(INSW(TMPSUM-TSUMFI,VERNR,0.),VERN)
      VERNF = LIMIT(0.,1.,(VERN-VBASE)/(VERSAT-VBASE))
      IF (IVERN .EQ. 0) VERNF = 1.

*---- Photoperiodfactor

      BDAYL = INSW(TMPSUM-TSUMDR,0.,7.)
      PULSE = INSW(TMPSUM-TSUMEM,0.,1.)
      FPERF = LIMIT(0.,1.,(DAYLP-BDAYL)/(SDAYL-BDAYL)) * PULSE


*---- accumulated temperature

      TMPB1 = AMAX1(0.,TMPA-1.)
      TMPB9 = AMAX1(0.,TMPA-9.)
      TMPCOR= INSW(TMPSUM-TSUMAN,TMPB1*VERNF*FPERF,TMPB9)
      TMP   = INSW(TMPSUM-TSUMEM,TMPB1,TMPCOR)
      TMPSUM= INTGRL(TMP,TMPSUM)


*---- developmentrate

      DVR1   = INSW(TMPSUM-TSUMAN,INSW(TMPSUM-TSUMEM,0.,TMP/
     $     (TSUMAN-TSUMEM)),0.)
      DVR2   = INSW(TMPSUM-TSUMAN,0.,TMP/TSUMMA)
 

      RETURN
      END