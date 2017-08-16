
c ------------------------------------------------------------------            
C *** Montalva2017 Subduction (2017 - Model) Horizontal ***********
c ------------------------------------------------------------------            
      subroutine Montalva2017 ( mag, fType, rRup, vs30, lnSa, sigma1, 
     2           specT, period1, iflag, forearc, depth, disthypo )

      implicit none
     
      real mag, fType, rRup, vs30, pgaRock, faba, vs30_rock, period0,
     1     lnSa, sigma, tau, period1, sigma1, disthypo, deltac1,
     2     depth, specT, depth1
      integer iflag, forearc

c     Ftype defines an interface event or intraslab events      
C     fType    Event Type
C     -------------------
C      0       Interface  - use rupture distance
C      1       Intraslab  - use hypocentral distance
C
C     faba     Note
C     -------------------------
C      0       Forearc site  
C      1       Backarc site  
C

      
c     compute pga on rock
      period0 = 0.0
      pgaRock = 0.0
      vs30_rock = 1000.
      faba = real(forearc)

C     Compute Rock PGA
      call Montalva2017_model ( mag, rRup, vs30_rock, pgaRock, lnSa, sigma, tau,
     2                     period0, Ftype, iflag, faba, depth, disthypo )
      pgaRock = exp(lnSa)
 
C     Compute regular ground motions. 
      call Montalva2017_model ( mag, rRup, vs30, pgaRock, lnSa, sigma, tau, 
     2                     specT, Ftype, iflag, faba, depth, disthypo )

c     compute Sa (given the PGA rock value)
      sigma1 = sqrt( sigma**2 + tau**2 )
      period1 = specT

c     Convert units spectral acceleration in gal                                
      lnSa = lnSa + 6.89                                                
      return
      end
c ----------------------------------------------------------------------
      subroutine Montalva2017_model ( mag, rRup, vs30, pgaRock, lnSa, sigma, tau, 
     2                     specT, Ftype, iflag, faba, depth, disthypo )

      implicit none
      
      integer MAXPER, nPer, i1, i      
      parameter (MAXPER=23)
      real a1(MAXPER), a2(MAXPER),
     1     a6(MAXPER), a7(MAXPER), a8(MAXPER), a10(MAXPER), a11(MAXPER),
     1     a12(MAXPER), a13(MAXPER), a14(MAXPER), a15(MAXPER), a16(MAXPER),
     1     a3(MAXPER), a4(MAXPER), a5(MAXPER), a9(MAXPER), dC1int(MAXPER)
      real period(MAXPER), b_soil(MAXPER), vLin(MAXPER), sigs(MAXPER), sigt(MAXPER)
      real sigma, lnSa, pgaRock, vs30, rRup, disthypo,
     1     mag
      real a1T, a2T, a6T, a7T, a8T, a3T, a4T, a5T, a9T, dC1intT
      real a10T, a11T, a12T, a13T, a14T, a15T, a16T, sigsT, sigtT
      real vLinT, b_soilT, sumgm, Ftype, tau, period1
      integer count1, count2, iflag
      real n, c, c4, c1, deltac1, faba, R, testmag, VsStar, depth, specT
	  real base, fmag, fdepth, fsite, farc, dC1slab

      data period /  0.00, 0.02, 0.05, 0.075, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5,
     1               0.6,  0.75, 1.00,  1.5, 2.00, 2.5, 3.00, 4.0, 5.0, 
     2               6.0, 7.5, 10.00  /
  
      data vLin   / 865.1, 865.1, 1053.5, 1085.7, 1032.5, 877.6, 748.2, 654.3, 587.1, 503,
     1             456.6, 430.3, 410.5, 400, 400, 400, 400, 400, 400, 400, 400, 400, 400 / 
      data b_soil / -1.186, -1.186, -1.346, -1.471, -1.624, -1.931, -2.188, -2.381, -2.518,
     1             -2.657, -2.669, -2.599, -2.401, -1.955, -1.025, -0.299, 0, 0, 0, 0, 0, 
     1            0, 0 / 
	  data dC1int / 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.143682921, 0.1,  
     1            0.073696559, 0.04150375, 0, -0.05849625, -0.1, -0.155033971,  
     1            -0.2, -0.2, -0.2, -0.2, -0.2, -0.2 /
      data a1 / 5.87504394, 5.97631438, 7.45297044, 8.04759521, 7.76085108, 6.171919, 
     1            4.83403302, 4.42687615, 4.57008643, 3.98311294, 4.8603434, 4.67510367, 
     1            4.30862113, 3.57339281, 2.92216459, 2.39779653, 1.64147667, 1.66482796, 
     1            0.90564754, 0.6123444, 0.32672294, -0.24139803, -0.96313983 / 
      data a2 / -1.75359772, -1.77010766, -2.03336398, -2.10610081, -1.99370934, -1.58654201, 
     1            -1.2971103, -1.18774055, -1.24895678, -1.13377346, -1.38019755, -1.35362409, 
     1            -1.30799859, -1.23082022, -1.18750273, -1.16319283, -1.06543862, -1.12677535, 
     1            -1.07619985, -1.13079589, -1.1573438, -1.1407007, -1.09295336 / 
      data a3 / 0.13125248, 0.12246057, 0.08332151, 0.08012671, 0.0730312, 0.05481839, 0.05249728, 
     1            0.02995137, 0.03865827, 0.04682762, 0.03822425, 0.02523729, 0.00995253, 0.03605351, 
     1            0.02768934, 0.040113, 0.08310064, 0.09403648, 0.13838017, 0.15259121, 0.1242091, 
     1            0.10950824, 0.11343926 / 
      data a4 / 0.80276784, 0.84131709, 1.03131243, 1.03436999, 1.07565004, 1.17061492, 1.20531288, 
     1            1.37607187, 1.34990775, 1.3795388, 1.51949871, 1.66662746, 1.85625091, 1.81217177, 
     1            2.03469107, 2.04340485, 1.88987024, 1.9050392, 1.71178342, 1.59358719, 1.69183532, 
     1            1.71125604, 1.67160339 / 
      data a5 / -0.33486952, -0.28054559, -0.03954116, -0.01295063, 0.00758131, 0.10490549, 0.17968066, 
     1            0.22912175, 0.15592549, 0.11670946, 0.18347677, 0.21967977, 0.29782648, 0.24372341, 
     1            0.22521403, 0.27382886, 0.18739875, 0.13268085, 0.01379686, 0.06464958, 0.32368231, 
     1            0.60252124, 0.7762083 / 
      data a6 / -0.00039095, -0.00038903, 0, -0.00009638, -0.00078515, -0.00267532, -0.0033759, -0.00355237, 
     1            -0.00244847, -0.00207613, -0.00001896, 0, 0, 0, -0.00009996, -0.00033356, -0.00121364, 
     1            -0.00087595, -0.00061861, 0, 0, 0, 0 / 
      data a7 / 1.0988, 1.0988, 1.2536, 1.4175, 1.3997, 1.3582, 1.1648, 0.994, 0.8821, 0.7046, 0.5799, 
     1            0.5021, 0.3687, 0.1746, -0.082, -0.2821, -0.4108, -0.4466, -0.4344, -0.4368, -0.4586, 
     1            -0.4433, -0.4828 / 
      data a8 / -1.42, -1.42, -1.65, -1.8, -1.8, -1.69, -1.49, -1.3, -1.18, -0.98, -0.82, -0.7, -0.54, 
     1            -0.34, -0.05, 0.12, 0.25, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3 / 
      data a9 / 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 
     1            0.4, 0.4, 0.4, 0.4, 0.4 / 
      data a10 / 4.53143081, 4.57416129, 4.56070915, 4.36639286, 3.90922953, 3.06236311, 3.50112817, 
     1            3.62815675, 3.87633808, 4.03388062, 4.31418239, 4.75196667, 4.70451938, 4.56020155, 
     1            4.83342978, 4.59028522, 4.13415056, 4.18978319, 4.50906779, 4.56385964, 4.55836575, 
     1            5.08281865, 5.49692364 / 
      data a11 / 0.0056735, 0.00565448, 0.00848068, 0.00921589, 0.00629627, 0.00558843, 0.00319554, 
     1            0.001817, 0.00212947, 0.00068979, 0.0006478, 0.0008707, -0.00031282, -0.00101097, 
     1            0.00009741, 0.00108512, 0.00035459, 0.0007295, 0.00084112, 0.00068188, 0.00137322, 
     1            0.00167053, -0.00070392 / 
      data a12 / 1.01494528, 1.03738201, 1.31034079, 1.48158019, 1.65618649, 1.93944484, 2.08901131, 
     1            2.25003086, 2.283387, 2.3140873, 2.33333479, 2.23421777, 2.05217228, 1.63506217, 
     1            0.69338467, -0.09761879, -0.34931995, -0.33269783, -0.41320697, -0.42395126, -0.38759507, 
     1            -0.32638288, -0.25811162 / 
      data a13 / 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 / 
      data a14 / -0.73080261, -0.73868917, -0.69848828, -0.65335577, -0.5505116, -0.42997222, -0.53087673, 
     1            -0.58085678, -0.66280655, -0.72244113, -0.79644275, -0.90120145, -0.89829099, -0.87330858, 
     1            -0.94685865, -0.90845421, -0.80518214, -0.81689247, -0.87331394, -0.87800447, -0.88436295, 
     1            -0.98803311, -1.05008478 / 
      data a15 / 0.9969, 0.9969, 1.103, 1.2732, 1.3042, 1.26, 1.223, 1.16, 1.05, 0.8, 0.662, 0.58, 0.48, 
     1            0.33, 0.31, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3 / 
      data a16 / -1, -1, -1.18, -1.36, -1.36, -1.3, -1.25, -1.17, -1.06, -0.78, -0.62, -0.5, -0.34, -0.14, 
     1            0, 0, 0, 0, 0, 0, 0, 0, 0 / 
      data sigs   / 0.6911808, 0.69938258, 0.70173433, 0.71412373, 0.741128, 0.74606525, 0.7451527, 
     1            0.72855743, 0.72093248, 0.71005053, 0.66934213, 0.66733247, 0.66329494, 0.63504015, 
     1            0.60012607, 0.56961713, 0.55384735, 0.53658882, 0.51345287, 0.51417184, 0.49080507, 
     1            0.4706381, 0.46023151 / 
      data sigt   / 0.47462209, 0.47631913, 0.53776165, 0.56188074, 0.52707475, 0.50642417, 0.44618739, 
     1            0.45040229, 0.42549471, 0.42945015, 0.43333698, 0.44599448, 0.46723155, 0.50143305, 
     1            0.51633193, 0.50688464, 0.51465398, 0.50365207, 0.45311429, 0.43900131, 0.4208419, 
     1            0.41701232, 0.38872242 / 

C Constant parameters            
      n = 1.18
      c = 1.88
      dC1slab = -0.3
      c4 = 10.0
      c1 = 7.2
 
C Find the requested spectral period and corresponding coefficients
      nPer = 23

C First check for the PGA case 
      if (specT .eq. 0.0) then
         i1=1
         period1 = period(i1)
         a1T = a1(i1)
         a2T = a2(i1)
         a3T = a3(i1)
         a4T = a4(i1)
         a5T = a5(i1)
         a6T = a6(i1)
         a7T = a7(i1)
         a8T = a8(i1)
         a9T = a9(i1)
         a10T = a10(i1)
         a11T = a11(i1)
         a12T = a12(i1)
         a13T = a13(i1)
         a14T = a14(i1)
         a15T = a15(i1)
         a16T = a16(i1)
         dC1intT = dC1int(i1)
         b_soilT = b_soil(i1)
         vLinT   = vLin(i1)
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
      write (*,*) 'Montalva Sub (2017 Model) Horizontal'
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
 1020       call interp (period(count1),period(count2),a1(count1),a1(count2),
     +                   specT,a1T,iflag)
            call interp (period(count1),period(count2),a2(count1),a2(count2),
     +                   specT,a2T,iflag)
            call interp (period(count1),period(count2),a3(count1),a3(count2),
     +                   specT,a3T,iflag)
            call interp (period(count1),period(count2),a4(count1),a4(count2),
     +                   specT,a4T,iflag)
            call interp (period(count1),period(count2),a5(count1),a5(count2),
     +                   specT,a5T,iflag)
            call interp (period(count1),period(count2),a6(count1),a6(count2),
     +                   specT,a6T,iflag)
            call interp (period(count1),period(count2),a7(count1),a7(count2),
     +                   specT,a7T,iflag)
            call interp (period(count1),period(count2),a8(count1),a8(count2),
     +                   specT,a8T,iflag)
            call interp (period(count1),period(count2),a9(count1),a9(count2),
     +                   specT,a9T,iflag)
            call interp (period(count1),period(count2),a10(count1),a10(count2),
     +                   specT,a10T,iflag)
            call interp (period(count1),period(count2),a11(count1),a11(count2),
     +                   specT,a11T,iflag)
            call interp (period(count1),period(count2),a12(count1),a12(count2),
     +                   specT,a12T,iflag)
            call interp (period(count1),period(count2),a13(count1),a13(count2),
     +                   specT,a13T,iflag)
            call interp (period(count1),period(count2),a14(count1),a14(count2),
     +                   specT,a14T,iflag)
            call interp (period(count1),period(count2),a15(count1),a15(count2),
     +                   specT,a15T,iflag)
            call interp (period(count1),period(count2),a16(count1),a16(count2),
     +                   specT,a16T,iflag)
            call interp (period(count1),period(count2),dC1int(count1),dC1int(count2),
     +                   specT,dC1intT,iflag)
            call interp (period(count1),period(count2),b_soil(count1),b_soil(count2),
     +                   specT,b_soilT,iflag)
            call interp (period(count1),period(count2),vLin(count1),vLin(count2),
     +                   specT,vLinT,iflag)
            call interp (period(count1),period(count2),sigs(count1),sigs(count2),
     +                   specT,sigsT,iflag)
            call interp (period(count1),period(count2),sigt(count1),sigt(count2),
     +                   specT,sigtT,iflag)

 1011 period1 = specT                                                                                                              

C     Compute the R term and base model based on either Rupture Distance 
c         (Interface events) of Hypocentral distance (Intraslab events). 
      if (ftype .eq. 0) then
	     deltaC1 = dC1intT
         R = rRup + c4*exp( (mag-6.0)*a9T ) 
         base = a1T + a4T*deltaC1 + (a2T + a14T*ftype + a3T*(mag - 7.2))*alog(R) + a6T*rRup + a10T*ftype
      elseif (ftype .eq. 1) then
	     deltac1 = dC1slab
         R = disthypo + c4*exp( (mag-6.0)*a9T ) 
         base = a1T + a4T*deltaC1 + (a2T + a14T*ftype + a3T*(mag - 7.2))*alog(R) + a6T*disthypo + a10T*ftype
      else
         write (*,*) 'BC Hydro V3 Model not defined for Ftype'
         write (*,*) 'other than 0 (interface) or 1 (intraslab)'
         stop 99
      endif
      
C     Base model for Magnitude scaling.      
      testmag = (c1 + deltaC1)
      if (mag .le. testmag ) then
         fmag = a4T*(mag-testmag) + a13T*(10.0-mag)**2.0
      else
         fmag = a5T*(mag-testmag) + a13T*(10.0-mag)**2.0
      endif      
      
C     Depth Scaling
        fdepth =  a11T*(min(depth, 120.0) -60.0 )*ftype

C     Forearc/Backarc scaling      
      if (ftype .eq. 1) then
         farc =  (a7T +a8T*alog(max(disthypo,85.0)/40.0))*faba
      elseif (ftype .eq. 0) then   
         farc =  (a15T +a16T*alog(max(rRup,100.0)/40.0))*faba
      endif 

C     Site Response 
      if (vs30 .gt. 1000.0) then
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

      sumgm = base + fmag + fdepth + farc + fsite

c	  write(*,*) "deltaC1 = ", deltaC1
c 	  write(*,*) "testmag = ", testmag
c	  write(*,*) "R = ", R
c	  write(*,*) "base = ", base
c	  write(*,*) "fmag = ", fmag
c	  write(*,*) "fdepth = ", fdepth
c	  write(*,*) "f_faba = ", farc
c	  write(*,*) "fsite = ", fsite
c	  write(*,*) "lnYSa = ", sumgm
c	  write(*,*) "Sa = ", exp(sumgm)
	  

C     Set sigma values to return
      sigma = sigsT
      tau = sigtT

c     Set SA to return
      lnSa = sumgm

      return
      end


