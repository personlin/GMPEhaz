c ------------------------------------------------------------------            
C *** BCHydro Subduction (06/2010 - Model) Horizontal ***********
c ------------------------------------------------------------------            
      subroutine S04_AGA16_TW_C01 ( mag, fType, rRup, vs30, lnSa, sigma1, 
     2           specT, period1, iflag, Ztor, depth, disthypo )

      implicit none
     
      real mag, fType, rRup, vs30, pgaRock, faba, vs30_rock, period0,
     1     lnSa, sigma, tau, period1, sigma1, disthypo, deltac1,
     2     depth, specT, Ztor
      integer iflag, forearc

c     Ftype defines an interface event or intraslab events      
C     fType    Event Type
C     -------------------
C      0       Interface  - use rupture distance
C      1       Intraslab  - use hypocentral distance

c     compute pga on rock
      period0 = 0.0
      pgaRock = 0.0
      vs30_rock = 1000.

C     Compute Rock PGA
      call S04_AGA16_TW_C01_model ( mag, rRup, vs30_rock, pgaRock, lnSa, sigma, tau,
     2                     period0, Ftype, iflag, Ztor, depth, disthypo )
      pgaRock = exp(lnSa)
 
C     Compute regular ground motions. 
      call S04_AGA16_TW_C01_model ( mag, rRup, vs30, pgaRock, lnSa, sigma, tau, 
     2                     specT, Ftype, iflag, Ztor, depth, disthypo )

c     compute Sa (given the PGA rock value)
      sigma1 = sqrt( sigma**2 + tau**2 )
      period1 = specT

c     Convert units spectral acceleration in gal                                
      lnSa = lnSa + 6.89                                                
      return
      end
c ----------------------------------------------------------------------
      subroutine S04_AGA16_TW_C01_model ( mag, rRup, vs30, pgaRock, lnSa, sigma, tau, 
     2                     specT, Ftype, iflag, Ztor, depth, disthypo )

      implicit none
      
      integer MAXPER, nPer, i1, i      
      parameter (MAXPER=25)
      real a1(MAXPER), a2(MAXPER),dC1_itf(MAXPER) ,dC1_itb(MAXPER),
     1     a6(MAXPER), a10(MAXPER), a11(MAXPER), a11a(MAXPER),
     1     a12(MAXPER), a13(MAXPER), a14(MAXPER)
      real period(MAXPER), b_soil(MAXPER), vLin(MAXPER), sigs(MAXPER), sigt(MAXPER)
      real sigma, lnSa, pgaRock, vs30, rRup, disthypo,
     1     mag, a3, a4, a5, a9 ,a7 , a8 , a15, a16, a6a
      real a1T, a2T, a6T, a11aT, Ztor
      real a10T, a11T, a12T, a13T, a14T, sigsT, sigtT, dC1_itfT, dC1_itbT
      real vLinT, b_soilT, sumgm, Ftype, tau, period1
      integer count1, count2, iflag
      real n, c, c4, c1, deltac1, faba, R, testmag, VsStar, depth, specT
      real base, fmag, fdepth, fsite

      Data Period(1:25) / 0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.075, 0.1, 0.12, 0.15, 0.17, 0.2, 0.25, 
     1            0.3, 0.4, 0.5, 0.75, 1, 1.5, 2, 3, 4, 5, 7.5, 10/
       data a1 / 4.442324272, 4.454995106, 4.476989613, 4.570375455, 4.68085322, 4.795283362, 5.187708654, 
     1           5.366885037, 5.493314388, 5.606487923, 5.588152124, 5.56106773, 5.458627336, 5.256968986,  
     1           4.863221356, 4.406005361, 3.632386593, 3.163230307, 2.454565259, 1.873974482, 1.161774809,  
     1           0.622785806, 0.063732135, -1.21868348, -2.213522559 /
      data a2  / -1.35, -1.35, -1.35, -1.372125352, -1.38782354, -1.4, -1.45, -1.45, -1.45, -1.45, -1.428246273,  
     1           -1.4, -1.35, -1.28, -1.18, -1.08, -0.91, -0.85, -0.77, -0.71, -0.64, -0.58, -0.54, -0.46, -0.4 /
      data a6  / -0.000279112, -0.000236461, -0.000254929, -0.000277444, -0.000309597, -0.000328968, -0.000147104,  
     1           -0.000143273, -0.000101224, -0.000228865, -0.000398518, -0.000509299, -0.00056124, -0.00099964,  
     1           -0.001455436, -0.001931196, -0.003474407, -0.003950625, -0.004406306, -0.004095739, -0.003534331,  
     1           -0.003889444, -0.003579808, -0.002904953, -0.004265509 /
      data a10 /  2.614780773, 2.594860589, 2.601252405, 2.624840044, 2.66846963, 2.719062888, 2.82423238,  
     1           2.870663541, 2.873690705, 2.854306237, 2.690740299, 2.465778348, 2.135003365, 1.878180458,  
     1           1.517225292, 1.226658613, 0.719712006, 0.37882339, -0.02664084, -0.106219895, -0.17035158,  
     1           -0.079613443, 0.070314474, -0.07113619, 0.117820808 /
      data a11 /  0.019824826, 0.018076965, 0.01785939, 0.018011975, 0.017752615, 0.017565997, 0.017961591,  
     1           0.018186982, 0.018242659, 0.018510535, 0.018229181, 0.017858183, 0.017389833, 0.017450025,  
     1           0.018642358, 0.018912508, 0.019302661, 0.018150184, 0.015597208, 0.014257969, 0.011651582,  
     1           0.009288783, 0.008337491, 0.005121553, 0.002275984 /
      data a11a /  0.04081182, 0.036578254, 0.03708527, 0.038337152, 0.040004028, 0.041961023, 0.045225749,  
     1           0.048061124, 0.048918252, 0.047045382, 0.046243618, 0.044247831, 0.04247908, 0.038177175,  
     1           0.029427572, 0.022530308, 0.011216329, 0.004626401, -0.000948396, -0.004863106, -0.005063613, 
     1            -0.007774634, -0.004944396, -0.007533656, -0.017715245 /
      data a12  /  0.917324645, 0.918587417, 0.922959838, 1.009910498, 1.087815359, 1.150363857, 1.300591025,  
     1           1.450179555, 1.601462189, 1.776459199, 1.87491032, 2.031540841, 2.229875976, 2.379374019,  
     1           2.492260723, 2.537023323, 2.124665878, 1.57816376, 0.450025289, -0.391751696, -0.716939762,  
     1           -0.66351539, -0.670875563, -0.587002599, -0.56025643 /
      data a13  /  -0.0135, -0.0135, -0.0135, -0.013632752, -0.013726941, -0.0138, -0.0142, -0.0145, -0.014859728,  
     1           -0.0153, -0.015691567, -0.0162, -0.0172, -0.0183, -0.0206, -0.0231, -0.0296, -0.0363, -0.0493,  
     1           -0.061, -0.0798, -0.0935, -0.098, -0.098, -0.098 /
      data a14  /  -0.4, -0.4, -0.4, -0.4, -0.4, -0.4, -0.4, -0.4, -0.4, -0.4, -0.378246273, -0.35, -0.31, -0.28,  
     1           -0.23, -0.19, -0.12, -0.07, 0, 0, 0, 0, 0, 0, 0 /
      data Vlin /  865.1, 865.1, 865.1, 948.4683281, 1007.619098, 1053.5, 1085.7, 1032.5, 962.8476216, 877.6,  
     1           821.3013556, 748.2, 654.3, 587.1, 503, 456.6, 410.5, 400, 400, 400, 400, 400, 400, 400, 400 /
      data b_soil / -1.186, -1.186, -1.186, -1.256801128, -1.307035328, -1.346, -1.471, -1.624, -1.762045708,  
     1           -1.931, -2.042814155, -2.188, -2.381, -2.518, -2.657, -2.669, -2.401, -1.955, -1.025, -0.299,  
     1           0, 0, 0, 0, 0 /
      data dC1_itf / 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.14, 0.1,  
     1           0.04, 0, -0.06, -0.1, -0.2, -0.2, -0.2, -0.2, -0.2 /
      data dC1_itb / -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3,  
     1           -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3 /
      data sigs /  0.60,0.60, 0.60, 0.60, 0.60, 0.60, 0.60, 0.60, 0.60, 0.60, 0.60, 0.60, 0.60, 
     1            0.60, 0.60, 0.60, 0.60, 0.60, 0.60, 0.60, 0.60, 0.60, 0.60, 0.60, 0.60 /
      data sigt /  0.43,0.43, 0.43, 0.43, 0.43, 0.43, 0.43, 0.43, 0.43, 0.43, 0.43, 
     1            0.43, 0.43, 0.43, 0.43, 0.43, 0.43, 0.43, 0.43, 0.43, 0.43, 0.43, 0.43, 0.43, 0.43 /


C Constant parameters            
      n = 1.18
      c = 1.88
      a3 = 0.1
      a4 = 0.9
      a5 = 0.0
      a9 = 0.4
      c4 = 10.0
      c1 = 7.8
      a7 = 0
      a8 = 0
      a15 = 0
      a16 = 0
      a6a = 0
      
C Find the requested spectral period and corresponding coefficients
      nPer = 25

C First check for the PGA case 
      if (specT .eq. 0.0) then
         i1=1
         period1 = period(i1)
         a1T = a1(i1)
         a2T = a2(i1)
         a6T = a6(i1)
         a10T = a10(i1)
         a11T = a11(i1)
         a11aT = a11a(i1)
         a12T = a12(i1)
         a13T = a13(i1)
         a14T = a14(i1)
         b_soilT = b_soil(i1)
         vLinT   = vLin(i1)
         dC1_itfT = dC1_itf(i1)
         dC1_itbT = dC1_itb(i1)
         sigtT = sigt(i1)
         sigsT = sigs(i1)
         goto 1011
      endif

C   For other periods, loop over the spectral period range of the attenuation relationship.
      do i=2,nper-1
         if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
            count1 = i
            count2 = i+1
            goto 1020 
         endif
      enddo
 
C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'AGA16_TW_C01 Subduction Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: ' 
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*) 
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020       call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +                   specT,a1T,iflag)
            call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +                   specT,a2T,iflag)
            call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +                   specT,a6T,iflag)
            call S24_interp (period(count1),period(count2),a10(count1),a10(count2),
     +                   specT,a10T,iflag)
            call S24_interp (period(count1),period(count2),a11(count1),a11(count2),
     +                   specT,a11T,iflag)
            call S24_interp (period(count1),period(count2),a11a(count1),a11a(count2),
     +                   specT,a11aT,iflag)
            call S24_interp (period(count1),period(count2),a12(count1),a12(count2),
     +                   specT,a12T,iflag)
            call S24_interp (period(count1),period(count2),a13(count1),a13(count2),
     +                   specT,a13T,iflag)
            call S24_interp (period(count1),period(count2),a14(count1),a14(count2),
     +                   specT,a14T,iflag)
            call S24_interp (period(count1),period(count2),b_soil(count1),b_soil(count2),
     +                   specT,b_soilT,iflag)
            call S24_interp (period(count1),period(count2),vLin(count1),vLin(count2),
     +                   specT,vLinT,iflag)
            call S24_interp (period(count1),period(count2),dC1_itf(count1),dC1_itf(count2),
     +                   specT,dC1_itfT,iflag)
            call S24_interp (period(count1),period(count2),dC1_itb(count1),dC1_itb(count2),
     +                   specT,dC1_itbT,iflag)
            call S24_interp (period(count1),period(count2),sigs(count1),sigs(count2),
     +                   specT,sigsT,iflag)
            call S24_interp (period(count1),period(count2),sigt(count1),sigt(count2),
     +                   specT,sigtT,iflag)

 1011 period1 = specT                                                                                                              

C     Compute the R term and base model based on either Rupture Distance 
c         (Interface events) of Hypocentral distance (Intraslab events). 
      if (ftype .eq. 0.0) then
         deltaC1 = dC1_itfT
         R = rRup + c4*exp( (mag-6.0)*a9 ) 
         base = a1T + a4*deltaC1 + (a2T + a14T*ftype + a3*(mag - 7.8))*alog(R) + a6T*rRup + a10T*ftype
      elseif (ftype .eq. 1.0) then
         deltaC1 = dC1_itbT
         R = disthypo + c4*exp( (mag-6.0)*a9 ) 
         base = a1T + a4*deltaC1 + (a2T + a14T*ftype + a3*(mag - 7.8))*alog(R) + a6T*disthypo + a10T*ftype
      else
         write (*,*) 'AGA16_TW_C01 Model not defined for Ftype'
         write (*,*) 'other than 0 (interface) or 1 (intraslab)'
         stop 99
      endif
      
C     Base model for Magnitude scaling.      
      testmag = (7.8 + deltaC1)
      if (mag .le. testmag ) then
         fmag = a4*(mag-testmag) + a13T*(10.0-mag)**2.0
      else
         fmag = a5*(mag-testmag) + a13T*(10.0-mag)**2.0
      endif      
      
C     Depth Scaling
      if (ftype .eq. 0.0) then
        fdepth = a11aT*(Ztor - 20.0)
      elseif (ftype .eq. 1.0) then
C        fdepth = a11T*(depth - 60.0 )
        fdepth =  a11T*(min(depth, 80.0) -60.0 )
      else
         write (*,*) 'AGA16_TW_C01 Model not defined for Ftype'
         write (*,*) 'other than 0 (interface) or 1 (intraslab)'
         stop 99
      endif

C     Site Response 
      if (vs30 .ge. 1000.0) then
          VsStar = 1000.0
      else
          VsStar = vs30
      endif
       
      if (vs30 .ge. VlinT) then
         fsite = a12T*alog(VsStar/vLinT) + b_soilT*n*alog(VsStar/vLinT)
      else
         fsite = a12T*alog(VsStar/vLinT) - b_soilT*alog(pgarock + c) +
     1          b_soilT*alog(pgarock + c*(VsStar/vlinT)**n)     
      endif

      sumgm = base + fmag + fdepth + fsite
c      write(*,*) "deltaC1 = ", deltaC1
c      write(*,*) "testmag = ", testmag
c      write(*,*) "base = ", base
c      write(*,*) "fmag = ", fmag
c      write(*,*) "fdepth = ", fdepth
c      write(*,*) "fsite = ", fsite
c      write(*,*) "lnYSa = ", sumgm
c      write(*,*) "Sa = ", exp(sumgm)
	  
C     Set sigma values to return
      sigma = sigsT
      tau = sigtT

c     Set SA to return
      lnSa = sumgm

      return
      end