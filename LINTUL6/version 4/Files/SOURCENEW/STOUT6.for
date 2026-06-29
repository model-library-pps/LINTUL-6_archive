*---------------------------------------------------------------------------------------*
*    STOUT6:  summary output of crop production limited by radiation, temperature and   *
*               availability of water and nitrogen                                      *
*    Subroutine for LINTUL 6 rotation model for summary output of yield variables       *
*    simulated for a series of crops per rotation; subroutine was originally developed  *   
*    for the NWHEAT model.                                                              *
*    The yield data refer to different crops (see ROTAT.inp for information)  growing   *
*    under weather from one  station and on one soil type                                                               *
*---------------------------------------------------------------------------------------*
      SUBROUTINE STOUT6 (RUNNAME,REMARK,STATR,ISOIL,ICROP,TERMY,IYEAR,
     $    IDEMERG,IDPL,IDFLOW,IDHALT,CO,IOPT,IROT,IDSTART,INYEAR,
     $       IRRI,IMANAG)

      IMPLICIT REAL (A-Z)
      INTEGER ISOIL,ICROP,IDEMERG,IDPL,IDFLOW,IDHALT,ISYR,ISYEAR,IWRUN
      INTEGER IRRI1,IDPL1,IOPT
	INTEGER IROT,IDSTART,INYEAR,IYEAR
	INTEGER IRRI, IMANAG,IYR

      SAVE  
      LOGICAL TERMY,OPEN,INIT
      CHARACTER RUNNAME*5,NWSTAT*30,STATR*5,REMARK*80

      COMMON /ITIME/ ISYR, ISYEAR,IYR
      COMMON /CROPOUT/  TAGB1,WSO1,TPARINT1, TPAR1, HI1, RUEC
	COMMON /CROPOUT/ NUPTT1,NFIXTT1,NLIV1,NLOSS1,NROOT1,TRANRF1,NNI1
      COMMON /WATBOUT/ TTRANS1,TDRAIN1,TRAIN1,TESOIL1,WAVT1,WAVTL1
      COMMON /WATBOUT/ TRUNOF1,TIRR1,RUNFR1,IRRI1,WAVTLX1, RD1

      DATA OPEN /.FALSE./
      DATA INIT /.TRUE./

      IF (.NOT. OPEN)  THEN
      OPEN= .TRUE.

*------ open output file
       NWSTAT = RUNNAME//'.STA'
       OPEN (80,FILE=NWSTAT,STATUS='NEW', ACCESS='SEQUENTIAL')
       ENDIF

        IF (.NOT. INIT) GOTO 100

*------ initialization number of runs and means
      
       IWRUN= 0
	 TTRANSO= 0.
	 TESOILO= 0.
       MPL= 0.
       MEMER= 0.
       MFLOW= 0.
       MHALT= 0.
       MWSO= 0.
       MAGB= 0.
       MHI= 0.
       MPARA= 0.
       MRUE= 0.
       MWAT= 0.
	 MWATL= 0.
	 MWATLX= 0.
	 MRD= 0. 
       MRAIN= 0.
       MEV= 0.
       MTRA= 0.
       MDRAI= 0.
       MIRR= 0.
       MRUNO= 0.
       MWUE= 0.
	 MNUP= 0.
	 MNFIX= 0.
	 MNLIV= 0.
	 MNLOS=0.
	 MNUE= 0.
	 MTRAN= 0.
	 MNNI= 0.

       MYPL=  0.
       MYEMER= 0.
       MYFLOW=  0.
       MYHALT= 0.      
       MYWSO=  0.
       MYAGB=  0.
       MYHI=   0.
       MYPARA= 0.
       MYRUE=  0.
       MYWAT=  0.
	 MYWATL= 0.
	 MYWATLX= 0.
	 MYRD=   0.
       MYRAIN= 0.
       MYEV=   0.
       MYTRA=  0.
       MYDRAI= 0.
       MYIRR=  0.
       MYRUNO= 0.
       MYWUE=  0.
       MYNUP=  0.
	 MYNFIX= 0.
	 MYNLIV= 0.
	 MYNLOS= 0.
	 MYNUE=  0.
	 MYTRAN= 0.
	 MYNNI=  0.


*------ initialization sums
      
       SUMPL= 0.
       SUMEMER= 0.
       SUMFLOW= 0.
       SUMHALT= 0.
       SUMWSO= 0.
       SUMAGB= 0.
       SUMHI= 0.
       SUMPARA= 0.
       SUMRUE= 0.
       SUMWAT= 0.
	 SUMWATL= 0.
	 SUMWATLX= 0.
	 SUMRD= 0.
       SUMRAIN= 0.
       SUMEV= 0.
       SUMTRA= 0.
       SUMDRAI= 0.
       SUMIRR= 0.
       SUMRUNO= 0.
       SUMWUE= 0.
       SUMNUP= 0.
	 SUMNFIX= 0.
	 SUMNLIV= 0.
	 SUMNLOS=0.
	 SUMNUE= 0.
	 SUMTRAN= 0.
	 SUMNNI= 0.

*------ print headings
       
        WRITE (80,'(///,5A)') 
     $   '*** RESULTS OF CROP GROWTH MODEL LINTUL6 for rotations ***',
     $   ' SUMMARY OF OUTPUT FOR CROP GROWTH, YIELD, WATER BALANCE',
     $   ' AND NITROGEN UPTAKE ***',
     $   ' (components of water balance are cumulative values from ',
     $   ' start of rotation to harvest date per crop)'

      WRITE (80,'(/,A,A,A,A,A,I3,A,I3,A,I4,A,I3,A,I3,A,F4.2,A,F4.0)') 
     $   'Name of run: ',
     $   RUNNAME,', Station= ', STATR,', Soil= ', ISOIL,', Rotat= ',
     $   IROT,', StYear= ',ISYEAR,', StDay= ',IDSTART,', NYEAR= ',
     $   INYEAR,', Runof= ',RUNFR1,', CO2= ',CO

        WRITE (80,'(1X,A)') REMARK

        WRITE (80,'(//,4A)')
     $  'YEAR  CROP  IOPT  IRRI  MANA  PL   EMERG FLOW  HALT    WSO   ',   
     $  '    TAGB   HI  PARAB  RUE   RD  WAV  WAVL  WAVLX  RAIN  ESOIL',
     $  ' TRANS  DRAIN RUNOF  IRR     WUE  NUPT  NFIX    NLIV   NLOSS ',
     $  '   NUE  TRANRF   NNI  ' 


100     CONTINUE

        INIT= .FALSE.
   

        IF (TERMY) GOTO 200 
     
*----harvest year
       

*----water use efficiency (g DM above-gr./kg water transpired)
*----N use efficiency (g DM above-gr. / g N in live and dead above-gr.mat.) 
*----components of water balance in mm
*----rooting depth at end of growth period in cm
*----components of N balance in kg N/ha
*----crop production in kg DM/ha
*----PAR intercepted in MJ/m2
*----radiation use efficiency in g D.M. above-gr./MJ PAR intercepted

        WUE= TAGB1/((TTRANS1 + TESOIL1 -TTRANSO -TESOILO) * 100.)
	  NUE= TAGB1/(NLIV1+NLOSS1-NROOT1)
        WAVT2= WAVT1*10.
	  WAVTL2= WAVTL1*10.
	  WAVTLX2= WAVTLX1*10.
        TRAIN2= TRAIN1*10.
        TESOIL2= TESOIL1*10.
        TTRANS2= TTRANS1*10.
        TDRAIN2= TDRAIN1*10.
        TRUNOF2= TRUNOF1*10.
        TIRR2=   TIRR1*10.

	  TTRANSO= TTRANS1
	  TESOILO= TESOIL1

*----- no planting date used
        IF (IDPL .LE. 0) THEN
           IDPL1= 0
        ELSE
           IDPL1= IDPL
        ENDIF
 
*------ one line of output for each year

        WRITE (80,'(I4,8I6,2X,2F8.0,F5.2,F6.0,F6.2,6F6.0,1X,
     $   4F6.0,F6.2, 5F7.1,2F7.2)')  
     $   IYR, ICROP,IOPT,IRRI,IMANAG, IDPL1,IDEMERG,IDFLOW,IDHALT,WSO1,
     $   TAGB1,HI1,TPARINT1,RUEC,RD1,WAVT2,WAVTL2,WAVTLX2,TRAIN2,
     $   TESOIL2,TTRANS2,TDRAIN2,TRUNOF2,TIRR2,WUE,NUPTT1,NFIXTT1,NLIV1,
     $   NLOSS1,NUE,TRANRF1,NNI1
     

*---- counting the number of crops and summing the variables;
*---- note that the components of the water balance are cumulative 
*----  from start of the crop rotation to the harvest date per crop  

         IWRUN=   IWRUN+1
         SUMPL=  SUMPL + IDPL1
         SUMEMER= SUMEMER + IDEMERG
         SUMFLOW= SUMFLOW + IDFLOW
         SUMHALT= SUMHALT + IDHALT
         SUMWSO=  SUMWSO + WSO1
         SUMAGB=  SUMAGB + TAGB1
         SUMHI=   SUMHI + HI1
         SUMPARA= SUMPARA + TPARINT1
         SUMRUE=  SUMRUE + RUEC
	   SUMRD= SUMRD + RD1
	   SUMWAT= SUMWAT + WAVT2
	   SUMWATL= SUMWATL + WAVTL2
	   SUMWATLX= SUMWATLX + WAVTLX2 
         SUMWUE=  SUMWUE + WUE
         SUMNUP=  SUMNUP + NUPTT1
	   SUMNFIX= SUMNFIX + NFIXTT1
	   SUMNLIV= SUMNLIV + NLIV1
	   SUMNLOS= SUMNLOS + NLOSS1 
	   SUMNUE= SUMNUE + NUE
	   SUMTRAN= SUMTRAN + TRANRF1
	   SUMNNI= SUMNNI + NNI1

         IF (.NOT. TERMY) RETURN

*------elaboration for statistical analysis

200    CONTINUE


        TRAIN2= TRAIN1*10.
        TESOIL2= TESOIL1*10.
        TTRANS2= TTRANS1*10.
        TDRAIN2= TDRAIN1*10.
        TRUNOF2= TRUNOF1*10.
        TIRR2=   TIRR1*10.
        WAVT2= WAVT1*10.
	  WAVTL2= WAVTL1*10.
	  WAVTLX2= WAVTLX1*10.

*------ one line of output for final year

        WRITE (80,'(I4,50X,2F8.0,F5.2,12X,6F6.0,1X,
     $   4F6.0)')  
     $   IYR,WSO1,
     $   TAGB1,HI1,RD1,WAVT2,WAVTL2,WAVTLX2,TRAIN2,TESOIL2,TTRANS2,
     $   TDRAIN2,TRUNOF2,TIRR2

*------Sums for components of water balance 
       SUMRAIN= TRAIN2
       SUMEV=   TESOIL2
       SUMTRA=  TTRANS2
       SUMDRAI= TDRAIN2
       SUMIRR=  TIRR2
       SUMRUNO= TRUNOF2

*----Means for all components, first per crop and second, per year 
       MPL=  SUMPL/IWRUN
       MEMER= SUMEMER/IWRUN
       MFLOW=  SUMFLOW/IWRUN
       MHALT= SUMHALT/IWRUN       
       MWSO=  SUMWSO/IWRUN
       MAGB=  SUMAGB/IWRUN
       MHI=   SUMHI/IWRUN
       MPARA= SUMPARA/IWRUN
       MRUE=  SUMRUE/IWRUN
	 MRD=   SUMRD/IWRUN    
       MWAT=  SUMWAT/IWRUN
	 MWATL= SUMWATL/IWRUN
	 MWATLX= SUMWATLX/IWRUN
       MRAIN= SUMRAIN/IWRUN
       MEV=   SUMEV/IWRUN
       MTRA=  SUMTRA/IWRUN
       MDRAI= SUMDRAI/IWRUN
       MIRR=  SUMIRR/IWRUN
       MRUNO= SUMRUNO/IWRUN
       MWUE=  SUMWUE/IWRUN
       MNUP=  SUMNUP/IWRUN
	 MNFIX= SUMNFIX/IWRUN
	 MNLIV= SUMNLIV/IWRUN
	 MNLOS= SUMNLOS/IWRUN
	 MNUE=  SUMNUE/IWRUN
	 MTRAN= SUMTRAN/IWRUN
	 MNNI=  SUMNNI/IWRUN

       MYPL=  SUMPL/INYEAR
       MYEMER= SUMEMER/INYEAR
       MYFLOW=  SUMFLOW/INYEAR
       MYHALT= SUMHALT/INYEAR      
       MYWSO=  SUMWSO/INYEAR
       MYAGB=  SUMAGB/INYEAR
       MYHI=   SUMHI/INYEAR
       MYPARA= SUMPARA/INYEAR
       MYRUE=  SUMRUE/INYEAR
	 MYRD= SUMRD/INYEAR
	 MYWAT= SUMWAT/INYEAR
	 MYWATL= SUMWATL/INYEAR
	 MYWATLX= SUMWATLX/INYEAR 
       MYRAIN= SUMRAIN/INYEAR
       MYEV=   SUMEV/INYEAR
       MYTRA=  SUMTRA/INYEAR
       MYDRAI= SUMDRAI/INYEAR
       MYIRR=  SUMIRR/INYEAR
       MYRUNO= SUMRUNO/INYEAR
       MYWUE=  SUMWUE/INYEAR
       MYNUP=  SUMNUP/INYEAR
	 MYNFIX= SUMNFIX/INYEAR
	 MYNLIV= SUMNLIV/INYEAR
	 MYNLOS= SUMNLOS/INYEAR
	 MYNUE=  SUMNUE/INYEAR
	 MYTRAN= SUMTRAN/INYEAR
	 MYNNI=  SUMNNI/INYEAR

*------mean results per respectively crop and year

        WRITE (80, '(/A,I3,A)') ' MEANS for ',IWRUN,' crops  :'
        WRITE (80,'(29X,4F6.0,1X,2F8.0,F5.2,F6.0,F6.2,6F6.0,1X,
     $   4F6.0,F6.2,5F7.1,2F7.2)')  
     $   MPL,MEMER,MFLOW,MHALT,MWSO,
     $   MAGB,MHI,MPARA,MRUE,MRD,MWAT,MWATL,MWATLX,
     $   MRAIN,MEV,MTRA,MDRAI,MRUNO,MIRR,MWUE,
     $   MNUP,MNFIX,MNLIV,MNLOS,MNUE,MTRAN,MNNI

        WRITE (80, '(/A,I3,A)') ' MEANS over ',INYEAR,' years  :'
        WRITE (80,'(29X,4F6.0,1X,2F8.0,F5.2,F6.0,F6.2,6F6.0,1X,
     $   4F6.0,F6.2,5F7.1,2F7.2)')  
     $   MYPL,MYEMER,MYFLOW,MYHALT,MYWSO,
     $   MYAGB,MYHI,MYPARA,MYRUE,MYRD,MYWAT,MYWATL,MYWATLX,
     $   MYRAIN,MYEV,MYTRA,MYDRAI,MYRUNO,MYIRR,MYWUE,
     $   MYNUP,MYNFIX,MYNLIV,MYNLOS,MYNUE,MYTRAN,MYNNI

         INIT= .TRUE.
    

         END