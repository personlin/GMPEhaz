c----------------------------------------------------------------------                
      Subroutine S04_Crustal_Common001 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag, 
     1                                   specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER), a10(MAXPER),  a11(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mc1(MAXPER), Mc2(MAXPER), Rrup, Ztor, HWfactor
      REAL M, specT, sigma, Rjb, dip, Width, Rx, a10h
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T, Mc1T, Mc2T, a10T, a11T, Fn, Frv, fType
      integer iflag, count1, count2, nPer, i, HWFlag

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 1.0043, 1.0043, 1.8140, -0.2905 / 
      data a2 / 0.1015, 0.1015, -0.0611, 0.8525 / 
      data a3 / 0.1140, 0.1140, 0.4292, 1.7018 / 
      data a4 / -0.3600, -0.3600, -0.1417, 0.0834 / 
      data a5 / -1.3911, -1.3911, -1.3582, -1.2562 / 
      data a6 / 0.2157, 0.2157, 0.1972, 0.1804 / 
      data a7 / 6.6695, 6.6695, 7.0197, 7.7410 / 
      data a8 / 0.0561, 0.0561, 0.0734, 0.0271 / 
      data a9 / 0.0110, 0.0110, 0.0144, 0.0135 / 
      data Mc1 / 5.6076, 5.6076, 5.8040, 5.8948 / 
      data Mc2 / 6.8246, 6.8246, 6.7669, 6.7549 / 
      data a10 / 0.0791, 0.0791, 0.0554, 0.1732 / 
      data a11 / -0.0591, -0.0591, -0.0988, -0.1836 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         Mc1T      = Mc1(1)
         Mc2T      = Mc2(1) 
         a10T      = a10(1) 
         a11T      = a11(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Crustal Common Model001 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)
      call S24_interp (period(count1),period(count2),Mc1(count1),Mc1(count2),
     +             specT,Mc1T,iflag)
      call S24_interp (period(count1),period(count2),Mc2(count1),Mc2(count2),
     +             specT,Mc2T,iflag)
      call S24_interp (period(count1),period(count2),a10(count1),a10(count2),
     +             specT,a10T,iflag)
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),
     +             specT,a11T,iflag)
   
 1011 period1 = specT                                                                                                              

      if ( fType .gt. 0.0 ) then
        Frv = 1.0
        Fn = 0.0
      elseif ( fType .lt. 0.0 ) then
        Frv = 0.0
        Fn = 1.0
      else
        Frv = 0.0
        Fn = 0.0
      endif
	  
C     Compute the Hanging Wall term if needed. 
      a10h = 3.0 
      if (HWFlag .eq. 1) then
         call S02_SWUS_HWModel (M, Rrup, Rjb, Rx, Dip, Width, a10h, ztor, specT, HWfactor)
      else 
         HWFactor = 0.0      
      endif
	  
C     Compute the ground motion for the given spectral period. 
      if (M .lt. Mc1T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(Mc1T-Mc2T) + a3T*(M-Mc1T)
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor 
      elseif (M .ge. Mc2T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a4T*(M-Mc2T)  
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor 
      else
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(M-Mc2T) 
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor 
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_Crustal_Common002 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag, 
     1                                   specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER), a10(MAXPER),  a11(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mc1(MAXPER), Mc2(MAXPER), Rrup, Ztor, HWfactor
      REAL M, specT, sigma, Rjb, dip, Width, Rx, a10h
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T, Mc1T, Mc2T, a10T, a11T, Fn, Frv, fType
      integer iflag, count1, count2, nPer, i, HWFlag

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 1.0850, 1.0850, 1.5530, -0.5026 / 
      data a2 / -0.5184, -0.5184, -0.1007, 0.9194 / 
      data a3 / 0.0243, 0.0243, 0.4286, 1.5592 / 
      data a4 / -0.5893, -0.5893, -0.2174, 0.0224 / 
      data a5 / -1.7337, -1.7337, -1.3160, -1.3152 / 
      data a6 / 0.3380, 0.3380, 0.1879, 0.2247 / 
      data a7 / 7.3956, 7.3956, 7.3393, 6.0233 / 
      data a8 / 0.0521, 0.0521, 0.0764, 0.0197 / 
      data a9 / 0.0207, 0.0207, 0.0153, 0.0151 / 
      data Mc1 / 5.4559, 5.4559, 5.8780, 5.6955 / 
      data Mc2 / 6.7333, 6.7333, 7.0662, 6.6882 / 
      data a10 / 0.0531, 0.0531, 0.0274, 0.1286 / 
      data a11 / -0.1013, -0.1013, -0.0641, -0.3028 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         Mc1T      = Mc1(1)
         Mc2T      = Mc2(1) 
         a10T      = a10(1) 
         a11T      = a11(1)  

         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Crustal Common Model002 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)
      call S24_interp (period(count1),period(count2),Mc1(count1),Mc1(count2),
     +             specT,Mc1T,iflag)
      call S24_interp (period(count1),period(count2),Mc2(count1),Mc2(count2),
     +             specT,Mc2T,iflag)
      call S24_interp (period(count1),period(count2),a10(count1),a10(count2),
     +             specT,a10T,iflag)
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),
     +             specT,a11T,iflag)
   
 1011 period1 = specT                                                                                                              

      if ( fType .gt. 0.0 ) then
        Frv = 1.0
        Fn = 0.0
      elseif ( fType .lt. 0.0 ) then
        Frv = 0.0
        Fn = 1.0
      else
        Frv = 0.0
        Fn = 0.0
      endif
      
C     Set Constant Terms

C     Compute the Hanging Wall term if needed.
      a10h = 3.0
      if (HWFlag .eq. 1) then
         call S02_SWUS_HWModel (M, Rrup, Rjb, Rx, Dip, Width, a10h, ztor, specT, HWfactor)
      else 
         HWFactor = 0.0      
      endif
	  
C     Compute the ground motion for the given spectral period. 
      if (M .lt. Mc1T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(Mc1T-Mc2T) + a3T*(M-Mc1T)
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      elseif (M .ge. Mc2T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a4T*(M-Mc2T)  
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      else
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(M-Mc2T) 
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_Crustal_Common003 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag, 
     1                                   specT, lnY, sigma, iflag )
	 
      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER), a10(MAXPER),  a11(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mc1(MAXPER), Mc2(MAXPER), Rrup, Ztor, HWfactor
      REAL M, specT, sigma, Rjb, dip, Width, Rx, a10h
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T, Mc1T, Mc2T, a10T, a11T, Fn, Frv, fType
      integer iflag, count1, count2, nPer, i, HWFlag

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 0.5220, 0.5220, 1.2274, -0.1760 / 
      data a2 / 0.0916, 0.0916, 0.0835, 0.7309 / 
      data a3 / 0.2653, 0.2653, 0.4925, 1.8120 / 
      data a4 / -0.0213, -0.0213, -0.2716, 0.0462 / 
      data a5 / -1.2528, -1.2528, -1.2224, -1.4113 / 
      data a6 / 0.1777, 0.1777, 0.1982, 0.2068 / 
      data a7 / 5.3278, 5.3278, 6.0578, 7.7143 / 
      data a8 / 0.0750, 0.0750, 0.0936, 0.0009 / 
      data a9 / 0.0102, 0.0102, 0.0165, 0.0135 / 
      data Mc1 / 6.0442, 6.0442, 5.3893, 5.8354 / 
      data Mc2 / 6.5704, 6.5704, 6.7338, 6.7609 / 
      data a10 / 0.1182, 0.1182, 0.1144, 0.1313 / 
      data a11 / -0.0245, -0.0245, -0.0004, -0.2167 / 

C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         Mc1T      = Mc1(1)
         Mc2T      = Mc2(1) 
         a10T      = a10(1) 
          a11T      = a11(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Crustal Common Model003 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)
      call S24_interp (period(count1),period(count2),Mc1(count1),Mc1(count2),
     +             specT,Mc1T,iflag)
      call S24_interp (period(count1),period(count2),Mc2(count1),Mc2(count2),
     +             specT,Mc2T,iflag)
      call S24_interp (period(count1),period(count2),a10(count1),a10(count2),
     +             specT,a10T,iflag)
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),
     +             specT,a11T,iflag)
   
 1011 period1 = specT                                                                                                              

      if ( fType .gt. 0.0 ) then
        Frv = 1.0
        Fn = 0.0
      elseif ( fType .lt. 0.0 ) then
        Frv = 0.0
        Fn = 1.0
      else
        Frv = 0.0
        Fn = 0.0
      endif
      
C     Set Constant Terms

C     Compute the Hanging Wall term if needed.
      a10h = 3.0
      if (HWFlag .eq. 1) then
         call S02_SWUS_HWModel (M, Rrup, Rjb, Rx, Dip, Width, a10h, ztor, specT, HWfactor)
      else 
         HWFactor = 0.0      
      endif
	  
C     Compute the ground motion for the given spectral period. 
      if (M .lt. Mc1T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(Mc1T-Mc2T) + a3T*(M-Mc1T)
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      elseif (M .ge. Mc2T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a4T*(M-Mc2T)  
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      else
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(M-Mc2T) 
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_Crustal_Common004 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag, 
     1                                   specT, lnY, sigma, iflag )

      implicit none
      
      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER), a10(MAXPER),  a11(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mc1(MAXPER), Mc2(MAXPER), Rrup, Ztor, HWfactor
      REAL M, specT, sigma, Rjb, dip, Width, Rx, a10h
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T, Mc1T, Mc2T, a10T, a11T, Fn, Frv, fType
      integer iflag, count1, count2, nPer, i, HWFlag

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 1.0360, 1.0360, 1.2964, 0.0333 / 
      data a2 / -0.0217, -0.0217, 0.1598, 0.8713 / 
      data a3 / 0.0668, 0.0668, 0.4748, 1.7507 / 
      data a4 / -0.5053, -0.5053, -0.1133, 0.1072 / 
      data a5 / -1.4155, -1.4155, -1.2252, -1.4069 / 
      data a6 / 0.2722, 0.2722, 0.1991, 0.1869 / 
      data a7 / 6.6044, 6.6044, 4.9111, 6.0709 / 
      data a8 / 0.0899, 0.0899, 0.0940, 0.0292 / 
      data a9 / 0.0130, 0.0130, 0.0182, 0.0114 / 
      data Mc1 / 5.4862, 5.4862, 5.4969, 5.8091 / 
      data Mc2 / 6.4075, 6.4075, 6.5337, 6.8292 / 
      data a10 / 0.0809, 0.0809, 0.0942, 0.1256 / 
      data a11 / -0.0459, -0.0459, -0.0496, -0.2980 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         Mc1T      = Mc1(1)
         Mc2T      = Mc2(1) 
         a10T      = a10(1) 
          a11T      = a11(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Crustal Common Model004 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)
      call S24_interp (period(count1),period(count2),Mc1(count1),Mc1(count2),
     +             specT,Mc1T,iflag)
      call S24_interp (period(count1),period(count2),Mc2(count1),Mc2(count2),
     +             specT,Mc2T,iflag)
      call S24_interp (period(count1),period(count2),a10(count1),a10(count2),
     +             specT,a10T,iflag)
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),
     +             specT,a11T,iflag)
   
 1011 period1 = specT                                                                                                              

      if ( fType .gt. 0.0 ) then
        Frv = 1.0
        Fn = 0.0
      elseif ( fType .lt. 0.0 ) then
        Frv = 0.0
        Fn = 1.0
      else
        Frv = 0.0
        Fn = 0.0
      endif

C     Set Constant Terms

C     Compute the Hanging Wall term if needed.
      a10h = 3.0
      if (HWFlag .eq. 1) then
         call S02_SWUS_HWModel (M, Rrup, Rjb, Rx, Dip, Width, a10h, ztor, specT, HWfactor)
      else 
         HWFactor = 0.0      
      endif
	  
C     Compute the ground motion for the given spectral period. 
      if (M .lt. Mc1T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(Mc1T-Mc2T) + a3T*(M-Mc1T)
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      elseif (M .ge. Mc2T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a4T*(M-Mc2T)  
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      else
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(M-Mc2T) 
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_Crustal_Common005 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag, 
     1                                   specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER), a10(MAXPER),  a11(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mc1(MAXPER), Mc2(MAXPER), Rrup, Ztor, HWfactor
      REAL M, specT, sigma, Rjb, dip, Width, Rx, a10h
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T, Mc1T, Mc2T, a10T, a11T, Fn, Frv, fType
      integer iflag, count1, count2, nPer, i, HWFlag

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 0.6332, 0.6332, 1.4214, -0.1771 / 
      data a2 / 0.1064, 0.1064, 0.0816, 0.9349 / 
      data a3 / 0.2512, 0.2512, 0.4475, 1.8174 / 
      data a4 / -0.2806, -0.2806, -0.1503, 0.2839 / 
      data a5 / -1.3105, -1.3105, -1.2012, -1.2908 / 
      data a6 / 0.2919, 0.2919, 0.2078, 0.1537 / 
      data a7 / 3.3986, 3.3986, 4.3409, 4.9907 / 
      data a8 / 0.0956, 0.0956, 0.0954, 0.0467 / 
      data a9 / 0.0224, 0.0224, 0.0171, 0.0100 / 
      data Mc1 / 5.7509, 5.7509, 5.9528, 5.8748 / 
      data Mc2 / 6.2291, 6.2291, 7.2681, 6.6116 / 
      data a10 / 0.0642, 0.0642, 0.1164, 0.1576 / 
      data a11 / -0.1668, -0.1668, -0.0481, -0.2417 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         Mc1T      = Mc1(1)
         Mc2T      = Mc2(1) 
         a10T      = a10(1) 
          a11T      = a11(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Crustal Common Model005 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)
      call S24_interp (period(count1),period(count2),Mc1(count1),Mc1(count2),
     +             specT,Mc1T,iflag)
      call S24_interp (period(count1),period(count2),Mc2(count1),Mc2(count2),
     +             specT,Mc2T,iflag)
      call S24_interp (period(count1),period(count2),a10(count1),a10(count2),
     +             specT,a10T,iflag)
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),
     +             specT,a11T,iflag)
   
 1011 period1 = specT                                                                                                              

      if ( fType .gt. 0.0 ) then
        Frv = 1.0
        Fn = 0.0
      elseif ( fType .lt. 0.0 ) then
        Frv = 0.0
        Fn = 1.0
      else
        Frv = 0.0
        Fn = 0.0
      endif

C     Set Constant Terms

C     Compute the Hanging Wall term if needed.
      a10h = 3.0
      if (HWFlag .eq. 1) then
         call S02_SWUS_HWModel (M, Rrup, Rjb, Rx, Dip, Width, a10h, ztor, specT, HWfactor)
      else 
         HWFactor = 0.0      
      endif
	  
C     Compute the ground motion for the given spectral period. 
      if (M .lt. Mc1T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(Mc1T-Mc2T) + a3T*(M-Mc1T)
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      elseif (M .ge. Mc2T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a4T*(M-Mc2T)  
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      else
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(M-Mc2T) 
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_Crustal_Common006 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag, 
     1                                   specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER), a10(MAXPER),  a11(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mc1(MAXPER), Mc2(MAXPER), Rrup, Ztor, HWfactor
      REAL M, specT, sigma, Rjb, dip, Width, Rx, a10h
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T, Mc1T, Mc2T, a10T, a11T, Fn, Frv, fType
      integer iflag, count1, count2, nPer, i, HWFlag

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 0.9925, 0.9925, 2.6241, 0.0554 / 
      data a2 / 0.0271, 0.0271, 0.1481, 1.1189 / 
      data a3 / 0.1214, 0.1214, 0.2703, 1.8314 / 
      data a4 / -0.4475, -0.4475, -0.3080, 0.2223 / 
      data a5 / -1.3849, -1.3849, -1.5990, -1.2241 / 
      data a6 / 0.2962, 0.2962, 0.2548, 0.1338 / 
      data a7 / 5.4617, 5.4617, 7.2811, 5.9266 / 
      data a8 / 0.0904, 0.0904, 0.0729, 0.0558 / 
      data a9 / 0.0167, 0.0167, 0.0161, 0.0049 / 
      data Mc1 / 6.0514, 6.0514, 5.9770, 5.8561 / 
      data Mc2 / 6.7832, 6.7832, 6.7235, 6.6455 / 
      data a10 / 0.0888, 0.0888, 0.1402, 0.1742 / 
      data a11 / -0.0834, -0.0834, -0.0339, -0.2335 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         Mc1T      = Mc1(1)
         Mc2T      = Mc2(1) 
         a10T      = a10(1) 
          a11T      = a11(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Crustal Common Model006 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)
      call S24_interp (period(count1),period(count2),Mc1(count1),Mc1(count2),
     +             specT,Mc1T,iflag)
      call S24_interp (period(count1),period(count2),Mc2(count1),Mc2(count2),
     +             specT,Mc2T,iflag)
      call S24_interp (period(count1),period(count2),a10(count1),a10(count2),
     +             specT,a10T,iflag)
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),
     +             specT,a11T,iflag)
   
 1011 period1 = specT                                                                                                              

      if ( fType .gt. 0.0 ) then
        Frv = 1.0
        Fn = 0.0
      elseif ( fType .lt. 0.0 ) then
        Frv = 0.0
        Fn = 1.0
      else
        Frv = 0.0
        Fn = 0.0
      endif
                                                                                                          

C     Set Constant Terms

C     Compute the Hanging Wall term if needed.
      a10h = 3.0
      if (HWFlag .eq. 1) then
         call S02_SWUS_HWModel (M, Rrup, Rjb, Rx, Dip, Width, a10h, ztor, specT, HWfactor)
      else 
         HWFactor = 0.0      
      endif
	  
C     Compute the ground motion for the given spectral period. 
      if (M .lt. Mc1T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(Mc1T-Mc2T) + a3T*(M-Mc1T)
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      elseif (M .ge. Mc2T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a4T*(M-Mc2T)  
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      else
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(M-Mc2T) 
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_Crustal_Common007 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag, 
     1                                   specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER), a10(MAXPER),  a11(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mc1(MAXPER), Mc2(MAXPER), Rrup, Ztor, HWfactor
      REAL M, specT, sigma, Rjb, dip, Width, Rx, a10h
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T, Mc1T, Mc2T, a10T, a11T, Fn, Frv, fType
      integer iflag, count1, count2, nPer, i, HWFlag

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 1.9442, 1.9442, 2.2962, -0.3152 / 
      data a2 / -0.0049, -0.0049, 0.1867, 1.0909 / 
      data a3 / 0.2335, 0.2335, 0.1583, 1.6158 / 
      data a4 / -0.5200, -0.5200, -0.1595, 0.1268 / 
      data a5 / -1.7318, -1.7318, -1.5141, -1.2078 / 
      data a6 / 0.2979, 0.2979, 0.2456, 0.1756 / 
      data a7 / 8.2171, 8.2171, 6.1367, 4.2719 / 
      data a8 / 0.0426, 0.0426, 0.0671, 0.0428 / 
      data a9 / 0.0131, 0.0131, 0.0195, 0.0101 / 
      data Mc1 / 5.8362, 5.8362, 6.2540, 5.6828 / 
      data Mc2 / 6.8367, 6.8367, 6.5627, 6.6900 / 
      data a10 / 0.1584, 0.1584, 0.0880, 0.1488 / 
      data a11 / -0.0364, -0.0364, -0.0585, -0.1941 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         Mc1T      = Mc1(1)
         Mc2T      = Mc2(1) 
         a10T      = a10(1) 
          a11T      = a11(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Crustal Common Model007 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)
      call S24_interp (period(count1),period(count2),Mc1(count1),Mc1(count2),
     +             specT,Mc1T,iflag)
      call S24_interp (period(count1),period(count2),Mc2(count1),Mc2(count2),
     +             specT,Mc2T,iflag)
      call S24_interp (period(count1),period(count2),a10(count1),a10(count2),
     +             specT,a10T,iflag)
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),
     +             specT,a11T,iflag)
   
 1011 period1 = specT                                                                                                              

      if ( fType .gt. 0.0 ) then
        Frv = 1.0
        Fn = 0.0
      elseif ( fType .lt. 0.0 ) then
        Frv = 0.0
        Fn = 1.0
      else
        Frv = 0.0
        Fn = 0.0
      endif
                                                                                                          

C     Set Constant Terms

C     Compute the Hanging Wall term if needed.
      a10h = 3.0
      if (HWFlag .eq. 1) then
         call S02_SWUS_HWModel (M, Rrup, Rjb, Rx, Dip, Width, a10h, ztor, specT, HWfactor)
      else 
         HWFactor = 0.0      
      endif
	  
C     Compute the ground motion for the given spectral period. 
      if (M .lt. Mc1T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(Mc1T-Mc2T) + a3T*(M-Mc1T)
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      elseif (M .ge. Mc2T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a4T*(M-Mc2T)  
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      else
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(M-Mc2T) 
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_Crustal_Common008 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag, 
     1                                   specT, lnY, sigma, iflag )

      implicit none
 
      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER), a10(MAXPER),  a11(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mc1(MAXPER), Mc2(MAXPER), Rrup, Ztor, HWfactor
      REAL M, specT, sigma, Rjb, dip, Width, Rx, a10h
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T, Mc1T, Mc2T, a10T, a11T, Fn, Frv, fType
      integer iflag, count1, count2, nPer, i, HWFlag

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 1.3164, 1.3164, 1.6522, -0.4184 / 
      data a2 / -0.2256, -0.2256, 0.0716, 0.8613 / 
      data a3 / 0.1301, 0.1301, 0.3883, 1.4474 / 
      data a4 / -0.5283, -0.5283, -0.2872, 0.0851 / 
      data a5 / -1.6997, -1.6997, -1.2959, -1.2261 / 
      data a6 / 0.3246, 0.3246, 0.2135, 0.1896 / 
      data a7 / 6.8828, 6.8828, 6.5737, 5.3075 / 
      data a8 / 0.0460, 0.0460, 0.0757, 0.0381 / 
      data a9 / 0.0286, 0.0286, 0.0185, 0.0109 / 
      data Mc1 / 5.6215, 5.6215, 5.5741, 5.8549 / 
      data Mc2 / 6.9587, 6.9587, 7.0680, 6.6056 / 
      data a10 / 0.0912, 0.0912, 0.1289, 0.0974 / 
      data a11 / -0.0990, -0.0990, -0.0108, -0.2330 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         Mc1T      = Mc1(1)
         Mc2T      = Mc2(1) 
         a10T      = a10(1) 
          a11T      = a11(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Crustal Common Model008 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)
      call S24_interp (period(count1),period(count2),Mc1(count1),Mc1(count2),
     +             specT,Mc1T,iflag)
      call S24_interp (period(count1),period(count2),Mc2(count1),Mc2(count2),
     +             specT,Mc2T,iflag)
      call S24_interp (period(count1),period(count2),a10(count1),a10(count2),
     +             specT,a10T,iflag)
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),
     +             specT,a11T,iflag)
   
 1011 period1 = specT                                                                                                              

      if ( fType .gt. 0.0 ) then
        Frv = 1.0
        Fn = 0.0
      elseif ( fType .lt. 0.0 ) then
        Frv = 0.0
        Fn = 1.0
      else
        Frv = 0.0
        Fn = 0.0
      endif
                                                                                                          

C     Set Constant Terms

C     Compute the Hanging Wall term if needed.
      a10h = 3.0
      if (HWFlag .eq. 1) then
         call S02_SWUS_HWModel (M, Rrup, Rjb, Rx, Dip, Width, a10h, ztor, specT, HWfactor)
      else 
         HWFactor = 0.0      
      endif
	  
C     Compute the ground motion for the given spectral period. 
      if (M .lt. Mc1T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(Mc1T-Mc2T) + a3T*(M-Mc1T)
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      elseif (M .ge. Mc2T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a4T*(M-Mc2T)  
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      else
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(M-Mc2T) 
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_Crustal_Common009 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag, 
     1                                   specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER), a10(MAXPER),  a11(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mc1(MAXPER), Mc2(MAXPER), Rrup, Ztor, HWfactor
      REAL M, specT, sigma, Rjb, dip, Width, Rx, a10h
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T, Mc1T, Mc2T, a10T, a11T, Fn, Frv, fType
      integer iflag, count1, count2, nPer, i, HWFlag

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 0.4324, 0.4324, 0.8402, -0.6070 / 
      data a2 / -0.0042, -0.0042, -0.2214, 1.0041 / 
      data a3 / 0.2215, 0.2215, 0.3858, 1.2248 / 
      data a4 / -0.2515, -0.2515, -0.0970, -0.2123 / 
      data a5 / -1.2957, -1.2957, -1.0381, -1.2334 / 
      data a6 / 0.2324, 0.2324, 0.1749, 0.2525 / 
      data a7 / 4.6095, 4.6095, 4.7500, 7.2821 / 
      data a8 / 0.0488, 0.0488, 0.0850, -0.0234 / 
      data a9 / 0.0167, 0.0167, 0.0174, 0.0119 / 
      data Mc1 / 5.7416, 5.7416, 5.7874, 5.5178 / 
      data Mc2 / 6.4674, 6.4674, 6.0670, 6.7239 / 
      data a10 / 0.0502, 0.0502, 0.0244, 0.1214 / 
      data a11 / -0.1237, -0.1237, -0.1129, -0.1821 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         Mc1T      = Mc1(1)
         Mc2T      = Mc2(1) 
         a10T      = a10(1) 
          a11T      = a11(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Crustal Common Model009 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)
      call S24_interp (period(count1),period(count2),Mc1(count1),Mc1(count2),
     +             specT,Mc1T,iflag)
      call S24_interp (period(count1),period(count2),Mc2(count1),Mc2(count2),
     +             specT,Mc2T,iflag)
      call S24_interp (period(count1),period(count2),a10(count1),a10(count2),
     +             specT,a10T,iflag)
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),
     +             specT,a11T,iflag)
   
 1011 period1 = specT                                                                                                              

      if ( fType .gt. 0.0 ) then
        Frv = 1.0
        Fn = 0.0
      elseif ( fType .lt. 0.0 ) then
        Frv = 0.0
        Fn = 1.0
      else
        Frv = 0.0
        Fn = 0.0
      endif
                                                                                                          

C     Set Constant Terms

C     Compute the Hanging Wall term if needed.
      a10h = 3.0
      if (HWFlag .eq. 1) then
         call S02_SWUS_HWModel (M, Rrup, Rjb, Rx, Dip, Width, a10h, ztor, specT, HWfactor)
      else 
         HWFactor = 0.0      
      endif
	  
C     Compute the ground motion for the given spectral period. 
      if (M .lt. Mc1T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(Mc1T-Mc2T) + a3T*(M-Mc1T)
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      elseif (M .ge. Mc2T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a4T*(M-Mc2T)  
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      else
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(M-Mc2T) 
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 

c----------------------------------------------------------------------                
      Subroutine S04_Crustal_Common010 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag, 
     1                                   specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER), a10(MAXPER),  a11(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mc1(MAXPER), Mc2(MAXPER), Rrup, Ztor, HWfactor
      REAL M, specT, sigma, Rjb, dip, Width, Rx, a10h
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T, Mc1T, Mc2T, a10T, a11T, Fn, Frv, fType
      integer iflag, count1, count2, nPer, i, HWFlag

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 0.6098, 0.6098, 0.9681, -0.5480 / 
      data a2 / 0.0709, 0.0709, -0.0965, 1.0237 / 
      data a3 / 0.0921, 0.0921, 0.2911, 1.5912 / 
      data a4 / -0.3596, -0.3596, -0.2686, -0.1723 / 
      data a5 / -1.3247, -1.3247, -1.0725, -1.2562 / 
      data a6 / 0.2004, 0.2004, 0.1826, 0.2209 / 
      data a7 / 6.4401, 6.4401, 6.5303, 5.1367 / 
      data a8 / 0.0601, 0.0601, 0.0876, 0.0401 / 
      data a9 / 0.0093, 0.0093, 0.0117, 0.0238 / 
      data Mc1 / 5.5622, 5.5622, 6.1299, 5.7315 / 
      data Mc2 / 6.6099, 6.6099, 5.8714, 6.6930 / 
      data a10 / 0.0420, 0.0420, 0.0185, 0.1210 / 
      data a11 / -0.1011, -0.1011, -0.0435, -0.1803 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         Mc1T      = Mc1(1)
         Mc2T      = Mc2(1) 
         a10T      = a10(1) 
          a11T      = a11(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Crustal Common Model010 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)
      call S24_interp (period(count1),period(count2),Mc1(count1),Mc1(count2),
     +             specT,Mc1T,iflag)
      call S24_interp (period(count1),period(count2),Mc2(count1),Mc2(count2),
     +             specT,Mc2T,iflag)
      call S24_interp (period(count1),period(count2),a10(count1),a10(count2),
     +             specT,a10T,iflag)
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),
     +             specT,a11T,iflag)
   
 1011 period1 = specT                                                                                                              

      if ( fType .gt. 0.0 ) then
        Frv = 1.0
        Fn = 0.0
      elseif ( fType .lt. 0.0 ) then
        Frv = 0.0
        Fn = 1.0
      else
        Frv = 0.0
        Fn = 0.0
      endif
                                                                                                          

C     Set Constant Terms

C     Compute the Hanging Wall term if needed.
      a10h = 3.0
      if (HWFlag .eq. 1) then
         call S02_SWUS_HWModel (M, Rrup, Rjb, Rx, Dip, Width, a10h, ztor, specT, HWfactor)
      else 
         HWFactor = 0.0      
      endif
	  
C     Compute the ground motion for the given spectral period. 
      if (M .lt. Mc1T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(Mc1T-Mc2T) + a3T*(M-Mc1T)
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      elseif (M .ge. Mc2T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a4T*(M-Mc2T)  
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      else
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(M-Mc2T) 
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 	  
c----------------------------------------------------------------------                
      Subroutine S04_Crustal_Common011 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag, 
     1                                   specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER), a10(MAXPER),  a11(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mc1(MAXPER), Mc2(MAXPER), Rrup, Ztor, HWfactor
      REAL M, specT, sigma, Rjb, dip, Width, Rx, a10h
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T, Mc1T, Mc2T, a10T, a11T, Fn, Frv, fType
      integer iflag, count1, count2, nPer, i, HWFlag

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 0.3330, 0.3330, 1.2434, -0.1898 / 
      data a2 / 0.0986, 0.0986, 0.2813, 0.8685 / 
      data a3 / 0.2412, 0.2412, 0.4735, 1.6715 / 
      data a4 / -0.3071, -0.3071, -0.2619, 0.0523 / 
      data a5 / -1.1884, -1.1884, -1.1417, -1.4041 / 
      data a6 / 0.2000, 0.2000, 0.1500, 0.2048 / 
      data a7 / 5.1289, 5.1289, 6.4769, 8.9643 / 
      data a8 / 0.0831, 0.0831, 0.0880, -0.0190 / 
      data a9 / 0.0000, 0.0000, 0.0001, 0.0063 / 
      data Mc1 / 5.4980, 5.4980, 4.8218, 5.8939 / 
      data Mc2 / 6.4987, 6.4987, 6.3167, 6.7445 / 
      data a10 / 0.0176, 0.0176, 0.0189, 0.1619 / 
      data a11 / -0.1283, -0.1283, -0.0783, -0.1163 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         Mc1T      = Mc1(1)
         Mc2T      = Mc2(1) 
         a10T      = a10(1) 
          a11T      = a11(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Crustal Common Model011 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)
      call S24_interp (period(count1),period(count2),Mc1(count1),Mc1(count2),
     +             specT,Mc1T,iflag)
      call S24_interp (period(count1),period(count2),Mc2(count1),Mc2(count2),
     +             specT,Mc2T,iflag)
      call S24_interp (period(count1),period(count2),a10(count1),a10(count2),
     +             specT,a10T,iflag)
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),
     +             specT,a11T,iflag)
   
 1011 period1 = specT                                                                                                              

      if ( fType .gt. 0.0 ) then
        Frv = 1.0
        Fn = 0.0
      elseif ( fType .lt. 0.0 ) then
        Frv = 0.0
        Fn = 1.0
      else
        Frv = 0.0
        Fn = 0.0
      endif
                                                                                                          

C     Set Constant Terms

C     Compute the Hanging Wall term if needed.
      a10h = 3.0
      if (HWFlag .eq. 1) then
         call S02_SWUS_HWModel (M, Rrup, Rjb, Rx, Dip, Width, a10h, ztor, specT, HWfactor)
      else 
         HWFactor = 0.0      
      endif
	  
C     Compute the ground motion for the given spectral period. 
      if (M .lt. Mc1T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(Mc1T-Mc2T) + a3T*(M-Mc1T)
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      elseif (M .ge. Mc2T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a4T*(M-Mc2T)  
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      else
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(M-Mc2T) 
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_Crustal_Common012 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag, 
     1                                   specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER), a10(MAXPER),  a11(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mc1(MAXPER), Mc2(MAXPER), Rrup, Ztor, HWfactor
      REAL M, specT, sigma, Rjb, dip, Width, Rx, a10h
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T, Mc1T, Mc2T, a10T, a11T, Fn, Frv, fType
      integer iflag, count1, count2, nPer, i, HWFlag

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 0.9464, 0.9464, 1.3300, -0.0764 / 
      data a2 / 0.1398, 0.1398, 0.1539, 0.7924 / 
      data a3 / 0.2695, 0.2695, 0.3945, 1.9367 / 
      data a4 / -0.1067, -0.1067, -0.1804, 0.2494 / 
      data a5 / -1.4173, -1.4173, -1.1472, -1.4313 / 
      data a6 / 0.2239, 0.2239, 0.1880, 0.1624 / 
      data a7 / 5.8980, 5.8980, 5.5583, 6.5661 / 
      data a8 / 0.0785, 0.0785, 0.1092, -0.0025 / 
      data a9 / 0.0068, 0.0068, 0.0107, 0.0100 / 
      data Mc1 / 5.9135, 5.9135, 5.4279, 5.9711 / 
      data Mc2 / 7.2901, 7.2901, 7.3778, 6.6104 / 
      data a10 / 0.1546, 0.1546, 0.0646, 0.1470 / 
      data a11 / -0.0145, -0.0145, -0.0850, -0.2268 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         Mc1T      = Mc1(1)
         Mc2T      = Mc2(1) 
         a10T      = a10(1) 
          a11T      = a11(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Crustal Common Model012 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)
      call S24_interp (period(count1),period(count2),Mc1(count1),Mc1(count2),
     +             specT,Mc1T,iflag)
      call S24_interp (period(count1),period(count2),Mc2(count1),Mc2(count2),
     +             specT,Mc2T,iflag)
      call S24_interp (period(count1),period(count2),a10(count1),a10(count2),
     +             specT,a10T,iflag)
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),
     +             specT,a11T,iflag)
   
 1011 period1 = specT                                                                                                              

      if ( fType .gt. 0.0 ) then
        Frv = 1.0
        Fn = 0.0
      elseif ( fType .lt. 0.0 ) then
        Frv = 0.0
        Fn = 1.0
      else
        Frv = 0.0
        Fn = 0.0
      endif
                                                                                                          

C     Set Constant Terms

C     Compute the Hanging Wall term if needed.
      a10h = 3.0
      if (HWFlag .eq. 1) then
         call S02_SWUS_HWModel (M, Rrup, Rjb, Rx, Dip, Width, a10h, ztor, specT, HWfactor)
      else 
         HWFactor = 0.0      
      endif
	  
C     Compute the ground motion for the given spectral period. 
      if (M .lt. Mc1T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(Mc1T-Mc2T) + a3T*(M-Mc1T)
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      elseif (M .ge. Mc2T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a4T*(M-Mc2T)  
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      else
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(M-Mc2T) 
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_Crustal_Common013 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag, 
     1                                   specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER), a10(MAXPER),  a11(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mc1(MAXPER), Mc2(MAXPER), Rrup, Ztor, HWfactor
      REAL M, specT, sigma, Rjb, dip, Width, Rx, a10h
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T, Mc1T, Mc2T, a10T, a11T, Fn, Frv, fType
      integer iflag, count1, count2, nPer, i, HWFlag

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 1.7885, 1.7885, 2.4649, 0.0185 / 
      data a2 / 0.1105, 0.1105, 0.6813, 1.1694 / 
      data a3 / 0.0906, 0.0906, 0.5829, 1.8759 / 
      data a4 / -0.3207, -0.3207, 0.1228, 0.0928 / 
      data a5 / -1.7603, -1.7603, -1.2298, -1.3419 / 
      data a6 / 0.3195, 0.3195, 0.1092, 0.1683 / 
      data a7 / 6.0135, 6.0135, 7.1178, 4.3155 / 
      data a8 / 0.0487, 0.0487, 0.0777, 0.0522 / 
      data a9 / 0.0059, 0.0059, -0.0131, 0.0101 / 
      data Mc1 / 5.4170, 5.4170, 5.5333, 5.6923 / 
      data Mc2 / 6.6173, 6.6173, 6.6143, 6.7375 / 
      data a10 / 0.0688, 0.0688, 0.1497, 0.1201 / 
      data a11 / -0.1582, -0.1582, -0.0401, -0.1766 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         Mc1T      = Mc1(1)
         Mc2T      = Mc2(1) 
         a10T      = a10(1) 
          a11T      = a11(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Crustal Common Model013 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)
      call S24_interp (period(count1),period(count2),Mc1(count1),Mc1(count2),
     +             specT,Mc1T,iflag)
      call S24_interp (period(count1),period(count2),Mc2(count1),Mc2(count2),
     +             specT,Mc2T,iflag)
      call S24_interp (period(count1),period(count2),a10(count1),a10(count2),
     +             specT,a10T,iflag)
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),
     +             specT,a11T,iflag)
   
 1011 period1 = specT                                                                                                              

      if ( fType .gt. 0.0 ) then
        Frv = 1.0
        Fn = 0.0
      elseif ( fType .lt. 0.0 ) then
        Frv = 0.0
        Fn = 1.0
      else
        Frv = 0.0
        Fn = 0.0
      endif
                                                                                                          

C     Set Constant Terms

C     Compute the Hanging Wall term if needed.
      a10h = 3.0
      if (HWFlag .eq. 1) then
         call S02_SWUS_HWModel (M, Rrup, Rjb, Rx, Dip, Width, a10h, ztor, specT, HWfactor)
      else 
         HWFactor = 0.0      
      endif
	  
C     Compute the ground motion for the given spectral period. 
      if (M .lt. Mc1T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(Mc1T-Mc2T) + a3T*(M-Mc1T)
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      elseif (M .ge. Mc2T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a4T*(M-Mc2T)  
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      else
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(M-Mc2T) 
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_Crustal_Common014 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag, 
     1                                   specT, lnY, sigma, iflag )

      implicit none
      
      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER), a10(MAXPER),  a11(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mc1(MAXPER), Mc2(MAXPER), Rrup, Ztor, HWfactor
      REAL M, specT, sigma, Rjb, dip, Width, Rx, a10h
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T, Mc1T, Mc2T, a10T, a11T, Fn, Frv, fType
      integer iflag, count1, count2, nPer, i, HWFlag

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 1.4965, 1.4965, 3.8209, -0.1636 / 
      data a2 / 0.4828, 0.4828, 0.3696, 1.6408 / 
      data a3 / 0.3691, 0.3691, 0.1254, 1.6917 / 
      data a4 / 0.0369, 0.0369, -0.4856, -0.0713 / 
      data a5 / -1.3326, -1.3326, -2.0334, -1.0426 / 
      data a6 / 0.1775, 0.1775, 0.2961, 0.1627 / 
      data a7 / 5.9573, 5.9573, 8.9957, 6.1390 / 
      data a8 / 0.0766, 0.0766, 0.0016, 0.0834 / 
      data a9 / 0.0008, 0.0008, 0.0205, 0.0003 / 
      data Mc1 / 5.7305, 5.7305, 6.0414, 5.7334 / 
      data Mc2 / 6.7493, 6.7493, 6.8448, 6.7503 / 
      data a10 / 0.1981, 0.1981, 0.0833, 0.2554 / 
      data a11 / 0.0000, 0.0000, -0.0821, -0.0004 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         Mc1T      = Mc1(1)
         Mc2T      = Mc2(1) 
         a10T      = a10(1) 
          a11T      = a11(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Crustal Common Model014 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)
      call S24_interp (period(count1),period(count2),Mc1(count1),Mc1(count2),
     +             specT,Mc1T,iflag)
      call S24_interp (period(count1),period(count2),Mc2(count1),Mc2(count2),
     +             specT,Mc2T,iflag)
      call S24_interp (period(count1),period(count2),a10(count1),a10(count2),
     +             specT,a10T,iflag)
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),
     +             specT,a11T,iflag)
   
 1011 period1 = specT                                                                                                              

      if ( fType .gt. 0.0 ) then
        Frv = 1.0
        Fn = 0.0
      elseif ( fType .lt. 0.0 ) then
        Frv = 0.0
        Fn = 1.0
      else
        Frv = 0.0
        Fn = 0.0
      endif
                                                                                                          

C     Set Constant Terms

C     Compute the Hanging Wall term if needed.
      a10h = 3.0
      if (HWFlag .eq. 1) then
         call S02_SWUS_HWModel (M, Rrup, Rjb, Rx, Dip, Width, a10h, ztor, specT, HWfactor)
      else 
         HWFactor = 0.0      
      endif
	  
C     Compute the ground motion for the given spectral period. 
      if (M .lt. Mc1T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(Mc1T-Mc2T) + a3T*(M-Mc1T)
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      elseif (M .ge. Mc2T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a4T*(M-Mc2T)  
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      else
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(M-Mc2T) 
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_Crustal_Common015 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag, 
     1                                   specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER), a10(MAXPER),  a11(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mc1(MAXPER), Mc2(MAXPER), Rrup, Ztor, HWfactor
      REAL M, specT, sigma, Rjb, dip, Width, Rx, a10h
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T, Mc1T, Mc2T, a10T, a11T, Fn, Frv, fType
      integer iflag, count1, count2, nPer, i, HWFlag

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 2.1057, 2.1057, 2.6634, 0.1774 / 
      data a2 / 0.3134, 0.3134, 0.2553, 1.0425 / 
      data a3 / 0.0559, 0.0559, 0.4027, 1.7805 / 
      data a4 / -0.4130, -0.4130, -0.1383, 0.1721 / 
      data a5 / -1.6991, -1.6991, -1.5862, -1.2788 / 
      data a6 / 0.2782, 0.2782, 0.2330, 0.1514 / 
      data a7 / 7.2339, 7.2339, 7.8014, 5.2673 / 
      data a8 / 0.0423, 0.0423, 0.0522, 0.0466 / 
      data a9 / 0.0200, 0.0200, 0.0246, 0.0051 / 
      data Mc1 / 5.7505, 5.7505, 5.6181, 5.6769 / 
      data Mc2 / 6.6401, 6.6401, 6.9552, 6.6241 / 
      data a10 / 0.1266, 0.1266, 0.1603, 0.1481 / 
      data a11 / -0.0753, -0.0753, -0.0191, -0.1733 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         Mc1T      = Mc1(1)
         Mc2T      = Mc2(1) 
         a10T      = a10(1) 
          a11T      = a11(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Crustal Common Model015 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)
      call S24_interp (period(count1),period(count2),Mc1(count1),Mc1(count2),
     +             specT,Mc1T,iflag)
      call S24_interp (period(count1),period(count2),Mc2(count1),Mc2(count2),
     +             specT,Mc2T,iflag)
      call S24_interp (period(count1),period(count2),a10(count1),a10(count2),
     +             specT,a10T,iflag)
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),
     +             specT,a11T,iflag)
   
 1011 period1 = specT                                                                                                              

      if ( fType .gt. 0.0 ) then
        Frv = 1.0
        Fn = 0.0
      elseif ( fType .lt. 0.0 ) then
        Frv = 0.0
        Fn = 1.0
      else
        Frv = 0.0
        Fn = 0.0
      endif
                                                                                                          

C     Set Constant Terms

C     Compute the Hanging Wall term if needed.
      a10h = 3.0
      if (HWFlag .eq. 1) then
         call S02_SWUS_HWModel (M, Rrup, Rjb, Rx, Dip, Width, a10h, ztor, specT, HWfactor)
      else 
         HWFactor = 0.0      
      endif
	  
C     Compute the ground motion for the given spectral period. 
      if (M .lt. Mc1T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(Mc1T-Mc2T) + a3T*(M-Mc1T)
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      elseif (M .ge. Mc2T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a4T*(M-Mc2T)  
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      else
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(M-Mc2T) 
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_Crustal_Common016 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag, 
     1                                   specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER), a10(MAXPER),  a11(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mc1(MAXPER), Mc2(MAXPER), Rrup, Ztor, HWfactor
      REAL M, specT, sigma, Rjb, dip, Width, Rx, a10h
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T, Mc1T, Mc2T, a10T, a11T, Fn, Frv, fType
      integer iflag, count1, count2, nPer, i, HWFlag

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 1.2525, 1.2525, 2.0954, -0.2879 / 
      data a2 / -0.1355, -0.1355, 0.2503, 1.0910 / 
      data a3 / 0.2269, 0.2269, 0.5221, 1.4687 / 
      data a4 / -0.0202, -0.0202, 0.0806, -0.0508 / 
      data a5 / -1.5938, -1.5938, -1.4191, -1.2195 / 
      data a6 / 0.2573, 0.2573, 0.1747, 0.2036 / 
      data a7 / 5.9415, 5.9415, 7.4970, 6.8855 / 
      data a8 / 0.0226, 0.0226, 0.0588, 0.0354 / 
      data a9 / 0.0267, 0.0267, 0.0239, 0.0092 / 
      data Mc1 / 6.0420, 6.0420, 5.4724, 5.6942 / 
      data Mc2 / 7.0986, 7.0986, 6.1179, 6.6614 / 
      data a10 / 0.1311, 0.1311, 0.1408, 0.1705 / 
      data a11 / -0.0827, -0.0827, -0.0127, -0.1802 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         Mc1T      = Mc1(1)
         Mc2T      = Mc2(1) 
         a10T      = a10(1) 
          a11T      = a11(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Crustal Common Model016 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)
      call S24_interp (period(count1),period(count2),Mc1(count1),Mc1(count2),
     +             specT,Mc1T,iflag)
      call S24_interp (period(count1),period(count2),Mc2(count1),Mc2(count2),
     +             specT,Mc2T,iflag)
      call S24_interp (period(count1),period(count2),a10(count1),a10(count2),
     +             specT,a10T,iflag)
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),
     +             specT,a11T,iflag)
   
 1011 period1 = specT                                                                                                              

      if ( fType .gt. 0.0 ) then
        Frv = 1.0
        Fn = 0.0
      elseif ( fType .lt. 0.0 ) then
        Frv = 0.0
        Fn = 1.0
      else
        Frv = 0.0
        Fn = 0.0
      endif
                                                                                                          

C     Set Constant Terms

C     Compute the Hanging Wall term if needed.
      a10h = 3.0
      if (HWFlag .eq. 1) then
         call S02_SWUS_HWModel (M, Rrup, Rjb, Rx, Dip, Width, a10h, ztor, specT, HWfactor)
      else 
         HWFactor = 0.0      
      endif
	  
C     Compute the ground motion for the given spectral period. 
      if (M .lt. Mc1T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(Mc1T-Mc2T) + a3T*(M-Mc1T)
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      elseif (M .ge. Mc2T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a4T*(M-Mc2T)  
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      else
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(M-Mc2T) 
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_Crustal_Common017 ( m, Rrup, Rjb, ztor, ftype, dip, Width, Rx, HWFlag, 
     1                                   specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER), a10(MAXPER),  a11(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER)
      real Mc1(MAXPER), Mc2(MAXPER), Rrup, Ztor, HWfactor
      REAL M, specT, sigma, Rjb, dip, Width, Rx, a10h
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL a7T, a8T, a9T, Mc1T, Mc2T, a10T, a11T, Fn, Frv, fType
      integer iflag, count1, count2, nPer, i, HWFlag

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 1.6620, 1.6620, 1.9720, -0.1720 / 
      data a2 / -0.3088, -0.3088, 0.1246, 0.7828 / 
      data a3 / 0.0752, 0.0752, 0.3515, 1.5635 / 
      data a4 / -0.5570, -0.5570, -0.2436, 0.1138 / 
      data a5 / -1.7972, -1.7972, -1.4091, -1.3159 / 
      data a6 / 0.3329, 0.3329, 0.2237, 0.1837 / 
      data a7 / 7.9745, 7.9745, 6.1290, 7.0238 / 
      data a8 / 0.0470, 0.0470, 0.0824, -0.0007 / 
      data a9 / 0.0131, 0.0131, 0.0213, 0.0069 / 
      data Mc1 / 5.4983, 5.4983, 5.9753, 5.8493 / 
      data Mc2 / 6.7994, 6.7994, 6.4343, 6.7616 / 
      data a10 / 0.0726, 0.0726, 0.1134, 0.1287 / 
      data a11 / -0.0983, -0.0983, -0.0507, -0.3125 / 



C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T      = a1(1)
         a2T      = a2(1)
         a3T      = a3(1)
         a4T      = a4(1)
         a5T      = a5(1)
         a6T      = a6(1)
         a7T      = a7(1)
         a8T      = a8(1)
         a9T      = a9(1)
         Mc1T      = Mc1(1)
         Mc2T      = Mc2(1) 
         a10T      = a10(1) 
          a11T      = a11(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Crustal Common Model017 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +             specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +             specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +             specT,a3T,iflag)
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +             specT,a4T,iflag)
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),
     +             specT,a5T,iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +             specT,a6T,iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +             specT,a7T,iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +             specT,a8T,iflag)
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +             specT,a9T,iflag)
      call S24_interp (period(count1),period(count2),Mc1(count1),Mc1(count2),
     +             specT,Mc1T,iflag)
      call S24_interp (period(count1),period(count2),Mc2(count1),Mc2(count2),
     +             specT,Mc2T,iflag)
      call S24_interp (period(count1),period(count2),a10(count1),a10(count2),
     +             specT,a10T,iflag)
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),
     +             specT,a11T,iflag)
   
 1011 period1 = specT                                                                                                              

      if ( fType .gt. 0.0 ) then
        Frv = 1.0
        Fn = 0.0
      elseif ( fType .lt. 0.0 ) then
        Frv = 0.0
        Fn = 1.0
      else
        Frv = 0.0
        Fn = 0.0
      endif

	  
C     Set Constant Terms

C     Compute the Hanging Wall term if needed.
      a10h = 3.0
      if (HWFlag .eq. 1) then
         call S02_SWUS_HWModel (M, Rrup, Rjb, Rx, Dip, Width, a10h, ztor, specT, HWfactor)
      else 
         HWFactor = 0.0      
      endif
	  
C     Compute the ground motion for the given spectral period. 
      if (M .lt. Mc1T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(Mc1T-Mc2T) + a3T*(M-Mc1T)
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      elseif (M .ge. Mc2T) then
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a4T*(M-Mc2T)  
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      else
         lnY = a1T - a8T**2*Rrup + a9T*Ztor + a2T*(M-Mc2T) 
     1         + (a5T+a6T*(M-5))*alog(sqrt(a7T*a7T+Rrup*Rrup)) + a10T*Frv + a11T*Fn + HWFactor
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 

c----------------------------------------------------------------------                
      Subroutine S04_SubIntra_Common001 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 7.6010, 7.6010, 6.5524, 1.0265 / 
      data a2 / -1.9563, -1.9563, -1.5544, -0.8219 / 
      data a9 / 0.6396, 0.6396, 0.4062, 0.3273 / 
      data a6 / -0.0129, -0.0129, -0.0384, -0.0474 / 
      data a4 / 1.1360, 1.1360, 0.4059, 0.9647 / 
      data a5 / 0.4968, 0.4968, -0.0164, 0.1406 / 
      data a13 / -0.0167, -0.0167, -0.0474, -0.0551 / 
      data Mref / 7.7734, 7.7734, 7.3962, 7.3556 / 
      data a11 / 0.0198, 0.0198, 0.0151, 0.0141 / 
      data a3 / 0.0625, 0.0625, 0.0972, 0.0868 / 
      data c4 / 17.3667, 17.3667, 12.5760, 9.4682 / 
      data dsi / 61.4544, 61.4544, 98.5737, 68.9010 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-intra Common Model001 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubIntra_Common002 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 6.6315, 6.6315, 4.9742, 0.3675 / 
      data a2 / -1.7536, -1.7536, -1.3114, -0.9139 / 
      data a9 / 0.5059, 0.5059, 0.5196, 0.5048 / 
      data a6 / -0.0270, -0.0270, -0.0414, -0.0348 / 
      data a4 / 1.0383, 1.0383, 0.3076, 1.7792 / 
      data a5 / 0.3197, 0.3197, 0.1108, 0.4393 / 
      data a13 / -0.0007, -0.0007, -0.0541, 0.0315 / 
      data Mref / 7.9432, 7.9432, 7.7129, 7.0885 / 
      data a11 / 0.0145, 0.0145, 0.0195, 0.0081 / 
      data a3 / 0.0748, 0.0748, 0.0810, 0.1024 / 
      data c4 / 14.1282, 14.1282, 7.3092, 6.5189 / 
      data dsi / 93.7570, 93.7570, 60.8882, 86.2021 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-intra Common Model002 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubIntra_Common003 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 3.7774, 3.7774, 7.0059, 1.4244 / 
      data a2 / -1.2463, -1.2463, -1.6520, -1.1243 / 
      data a9 / 0.6534, 0.6534, 0.3836, 0.5270 / 
      data a6 / -0.0499, -0.0499, -0.0487, -0.0255 / 
      data a4 / 0.3452, 0.3452, 0.2617, 1.8165 / 
      data a5 / 0.0039, 0.0039, 0.1626, 0.7126 / 
      data a13 / -0.0505, -0.0505, -0.0774, 0.0108 / 
      data Mref / 7.3320, 7.3320, 7.7770, 7.2068 / 
      data a11 / 0.0165, 0.0165, 0.0154, 0.0121 / 
      data a3 / 0.1120, 0.1120, 0.0755, 0.0516 / 
      data c4 / 1.3088, 1.3088, 12.9969, 9.3220 / 
      data dsi / 69.0272, 69.0272, 108.4136, 73.1003 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-intra Common Model003 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end  
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubIntra_Common004 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none
      
      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 3.8283, 3.8283, 6.3262, 0.5708 / 
      data a2 / -1.2365, -1.2365, -1.5653, -0.6131 / 
      data a9 / 0.2588, 0.2588, 0.3817, 1.1698 / 
      data a6 / -0.0653, -0.0653, -0.0312, -0.0694 / 
      data a4 / 0.3462, 0.3462, 0.3981, 0.7018 / 
      data a5 / -0.1812, -0.1812, 0.0448, 0.1398 / 
      data a13 / -0.0160, -0.0160, -0.0587, -0.0928 / 
      data Mref / 7.1206, 7.1206, 7.9352, 7.6605 / 
      data a11 / 0.0068, 0.0068, 0.0152, 0.0108 / 
      data a3 / 0.1351, 0.1351, 0.0769, 0.1083 / 
      data c4 / 2.3068, 2.3068, 6.8757, 1.6528 / 
      data dsi / 140.0033, 140.0033, 94.0696, 80.3279 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-intra Common Model004 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubIntra_Common005 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 5.2566, 5.2566, 6.3325, 2.2480 / 
      data a2 / -1.4931, -1.4931, -1.6338, -1.0795 / 
      data a9 / 0.4911, 0.4911, 0.3316, 0.2266 / 
      data a6 / -0.0402, -0.0402, -0.0254, -0.0411 / 
      data a4 / 0.6733, 0.6733, 0.6955, 1.3152 / 
      data a5 / 0.0601, 0.0601, 0.0060, 0.5264 / 
      data a13 / -0.0253, -0.0253, -0.0205, -0.0367 / 
      data Mref / 7.4836, 7.4836, 7.6130, 7.4294 / 
      data a11 / 0.0160, 0.0160, 0.0170, 0.0088 / 
      data a3 / 0.0928, 0.0928, 0.0926, 0.0648 / 
      data c4 / 5.2613, 5.2613, 4.9944, 16.0688 / 
      data dsi / 87.1517, 87.1517, 81.8567, 83.9385 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-intra Common Model005 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubIntra_Common006 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 4.4922, 4.4922, 6.7169, 0.8300 / 
      data a2 / -1.3198, -1.3198, -1.6517, -0.7128 / 
      data a9 / 0.3089, 0.3089, 0.5400, 0.5829 / 
      data a6 / -0.0381, -0.0381, -0.0418, -0.0556 / 
      data a4 / 0.2321, 0.2321, 0.9988, 0.8864 / 
      data a5 / -0.4776, -0.4776, 0.3316, 0.2955 / 
      data a13 / -0.0319, -0.0319, -0.0205, -0.0656 / 
      data Mref / 7.2758, 7.2758, 7.3512, 7.3714 / 
      data a11 / 0.0128, 0.0128, 0.0163, 0.0046 / 
      data a3 / 0.1566, 0.1566, 0.0588, 0.1072 / 
      data c4 / 5.5454, 5.5454, 4.6080, 3.7810 / 
      data dsi / 102.6698, 102.6698, 93.1004, 85.4824 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-intra Common Model006 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubIntra_Common007 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 4.0993, 4.0993, 6.4873, 1.4771 / 
      data a2 / -1.2073, -1.2073, -1.5296, -0.8852 / 
      data a9 / 0.2077, 0.2077, 0.4319, 0.4509 / 
      data a6 / -0.0597, -0.0597, -0.0529, -0.0495 / 
      data a4 / -0.3052, -0.3052, 0.7997, 0.9847 / 
      data a5 / -0.3777, -0.3777, 0.1360, 0.3975 / 
      data a13 / -0.0811, -0.0811, -0.0307, -0.0606 / 
      data Mref / 6.5863, 6.5863, 7.3565, 7.1541 / 
      data a11 / 0.0074, 0.0074, 0.0139, 0.0041 / 
      data a3 / 0.1199, 0.1199, 0.0719, 0.0938 / 
      data c4 / 5.7668, 5.7668, 6.1596, 10.4666 / 
      data dsi / 126.3711, 126.3711, 100.5852, 88.7429 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-intra Common Model007 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubIntra_Common008 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none
 
      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 5.1307, 5.1307, 5.9711, 1.5940 / 
      data a2 / -1.5487, -1.5487, -1.4710, -0.8534 / 
      data a9 / 0.3690, 0.3690, 0.2758, 0.2700 / 
      data a6 / -0.0331, -0.0331, -0.0319, -0.0504 / 
      data a4 / 0.4767, 0.4767, 0.5815, 0.6185 / 
      data a5 / 0.2524, 0.2524, 0.0830, 0.0163 / 
      data a13 / -0.0290, -0.0290, -0.0266, -0.0842 / 
      data Mref / 7.5607, 7.5607, 7.4862, 7.2653 / 
      data a11 / 0.0134, 0.0134, 0.0169, 0.0086 / 
      data a3 / 0.0787, 0.0787, 0.0915, 0.1143 / 
      data c4 / 15.5491, 15.5491, 8.1545, 13.8522 / 
      data dsi / 87.1045, 87.1045, 95.2334, 77.9879 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-intra Common Model008 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubIntra_Common009 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 5.3266, 5.3266, 6.6594, 0.4704 / 
      data a2 / -1.6325, -1.6325, -1.5621, -0.6435 / 
      data a9 / 0.6781, 0.6781, 0.5405, 0.8438 / 
      data a6 / -0.0177, -0.0177, -0.0412, -0.0600 / 
      data a4 / 1.1049, 1.1049, 0.4688, 0.7495 / 
      data a5 / 0.6079, 0.6079, 0.2201, 0.0232 / 
      data a13 / -0.0086, -0.0086, -0.0536, -0.0665 / 
      data Mref / 7.7873, 7.7873, 7.6401, 7.4228 / 
      data a11 / 0.0202, 0.0202, 0.0196, 0.0118 / 
      data a3 / 0.0393, 0.0393, 0.0768, 0.1085 / 
      data c4 / 11.6308, 11.6308, 13.3721, 4.8046 / 
      data dsi / 40.8043, 40.8043, 67.8297, 81.3104 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-intra Common Model009 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 

c----------------------------------------------------------------------                
      Subroutine S04_SubIntra_Common010 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 4.1489, 4.1489, 4.7173, 0.1842 / 
      data a2 / -1.4127, -1.4127, -1.1855, -0.8482 / 
      data a9 / 0.4411, 0.4411, 0.6867, 0.7215 / 
      data a6 / -0.0332, -0.0332, -0.0703, -0.0333 / 
      data a4 / 0.4451, 0.4451, 0.1021, 1.5905 / 
      data a5 / -0.2411, -0.2411, 0.0132, 0.3161 / 
      data a13 / -0.0269, -0.0269, -0.0806, 0.0161 / 
      data Mref / 7.2384, 7.2384, 7.3130, 7.2780 / 
      data a11 / 0.0186, 0.0186, 0.0180, 0.0125 / 
      data a3 / 0.1433, 0.1433, 0.0927, 0.0915 / 
      data c4 / 3.7703, 3.7703, 9.9141, 4.6502 / 
      data dsi / 64.8118, 64.8118, 59.1310, 80.0262 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-intra Common Model010 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 	  
c----------------------------------------------------------------------                
      Subroutine S04_SubIntra_Common011 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 6.3214, 6.3214, 5.7330, 0.4810 / 
      data a2 / -1.7507, -1.7507, -1.4570, -0.7149 / 
      data a9 / 0.2199, 0.2199, 0.4098, 0.7081 / 
      data a6 / -0.0213, -0.0213, -0.0583, -0.0551 / 
      data a4 / 0.8063, 0.8063, 0.2276, 0.8726 / 
      data a5 / -0.0681, -0.0681, 0.2076, -0.0774 / 
      data a13 / -0.0052, -0.0052, -0.0747, -0.0566 / 
      data Mref / 7.7219, 7.7219, 7.2074, 7.4848 / 
      data a11 / 0.0182, 0.0182, 0.0151, 0.0135 / 
      data a3 / 0.1246, 0.1246, 0.0670, 0.1249 / 
      data c4 / 10.4318, 10.4318, 11.7409, 6.1431 / 
      data dsi / 84.5264, 84.5264, 104.5682, 77.1699 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-intra Common Model011 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubIntra_Common012 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 6.0139, 6.0139, 5.3662, 1.1045 / 
      data a2 / -1.6224, -1.6224, -1.4087, -0.8415 / 
      data a9 / 0.4369, 0.4369, 0.5209, 0.4863 / 
      data a6 / -0.0334, -0.0334, -0.0576, -0.0585 / 
      data a4 / 0.7709, 0.7709, 0.5013, 0.9889 / 
      data a5 / -0.1388, -0.1388, 0.3414, 0.2105 / 
      data a13 / -0.0255, -0.0255, -0.0544, -0.0642 / 
      data Mref / 7.6515, 7.6515, 7.4414, 7.2496 / 
      data a11 / 0.0151, 0.0151, 0.0148, 0.0089 / 
      data a3 / 0.1117, 0.1117, 0.0515, 0.1188 / 
      data c4 / 4.5970, 4.5970, 4.7594, 8.5847 / 
      data dsi / 88.4311, 88.4311, 101.8119, 80.4040 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-intra Common Model012 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubIntra_Common013 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 6.1544, 6.1544, 5.0165, 1.7735 / 
      data a2 / -1.5918, -1.5918, -1.3094, -0.9073 / 
      data a9 / 0.5370, 0.5370, 0.9003, 0.3817 / 
      data a6 / -0.0348, -0.0348, -0.0567, -0.0603 / 
      data a4 / 0.8008, 0.8008, 0.6633, 1.1443 / 
      data a5 / -0.0462, -0.0462, 0.2486, 0.5699 / 
      data a13 / -0.0247, -0.0247, -0.0476, -0.0672 / 
      data Mref / 7.6204, 7.6204, 7.4097, 7.4593 / 
      data a11 / 0.0148, 0.0148, 0.0177, 0.0086 / 
      data a3 / 0.0988, 0.0988, 0.0655, 0.0611 / 
      data c4 / 6.4457, 6.4457, 0.5961, 10.2027 / 
      data dsi / 94.1379, 94.1379, 65.7016, 81.5994 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-intra Common Model013 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubIntra_Common014 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none
      
      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 4.6394, 4.6394, 6.4821, 1.8417 / 
      data a2 / -1.3217, -1.3217, -1.5732, -0.9433 / 
      data a9 / 0.1903, 0.1903, 0.4001, 0.1561 / 
      data a6 / -0.0397, -0.0397, -0.0407, -0.0518 / 
      data a4 / 0.0804, 0.0804, 0.7176, 1.1255 / 
      data a5 / -0.4410, -0.4410, -0.0111, 0.5776 / 
      data a13 / -0.0473, -0.0473, -0.0281, -0.0569 / 
      data Mref / 7.2315, 7.2315, 7.3453, 7.1780 / 
      data a11 / 0.0120, 0.0120, 0.0135, 0.0060 / 
      data a3 / 0.1369, 0.1369, 0.0893, 0.0663 / 
      data c4 / 5.9427, 5.9427, 2.4906, 13.8270 / 
      data dsi / 111.4712, 111.4712, 108.6521, 79.0768 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-intra Common Model014 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubIntra_Common015 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 5.3702, 5.3702, 6.5593, 3.0678 / 
      data a2 / -1.5065, -1.5065, -1.5348, -1.2869 / 
      data a9 / 0.6260, 0.6260, 0.4691, 0.5445 / 
      data a6 / -0.0368, -0.0368, -0.0300, -0.0363 / 
      data a4 / 0.8633, 0.8633, 0.6776, 1.4379 / 
      data a5 / 0.4420, 0.4420, -0.1784, 0.9867 / 
      data a13 / -0.0063, -0.0063, -0.0250, -0.0456 / 
      data Mref / 7.6397, 7.6397, 7.5294, 6.9958 / 
      data a11 / 0.0153, 0.0153, 0.0166, 0.0014 / 
      data a3 / 0.0650, 0.0650, 0.1268, 0.0573 / 
      data c4 / 14.1368, 14.1368, 3.6553, 18.4740 / 
      data dsi / 84.0762, 84.0762, 90.3494, 85.5721 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-intra Common Model015 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubIntra_Common016 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 4.8493, 4.8493, 6.1452, 1.4615 / 
      data a2 / -1.4707, -1.4707, -1.4313, -0.8889 / 
      data a9 / 0.6318, 0.6318, 0.2397, 0.3222 / 
      data a6 / -0.0518, -0.0518, -0.0592, -0.0435 / 
      data a4 / 0.3829, 0.3829, 0.3012, 0.9246 / 
      data a5 / 0.4741, 0.4741, -0.0550, 0.2915 / 
      data a13 / -0.0477, -0.0477, -0.0542, -0.0603 / 
      data Mref / 7.0911, 7.0911, 7.4798, 7.2608 / 
      data a11 / 0.0138, 0.0138, 0.0077, 0.0085 / 
      data a3 / 0.0710, 0.0710, 0.0771, 0.0840 / 
      data c4 / 16.0957, 16.0957, 10.1588, 11.4187 / 
      data dsi / 86.8582, 86.8582, 140.6040, 75.9709 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-intra Common Model016 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubIntra_Common017 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 5.9209, 5.9209, 6.2393, 2.4027 / 
      data a2 / -1.6806, -1.6806, -1.5484, -1.1370 / 
      data a9 / 0.3338, 0.3338, 0.3739, 0.4368 / 
      data a6 / -0.0453, -0.0453, -0.0449, -0.0410 / 
      data a4 / 0.3982, 0.3982, 0.6425, 1.0586 / 
      data a5 / 0.1463, 0.1463, 0.0951, 0.4804 / 
      data a13 / -0.0371, -0.0371, -0.0390, -0.0672 / 
      data Mref / 6.6640, 6.6640, 7.3289, 7.0792 / 
      data a11 / 0.0139, 0.0139, 0.0132, 0.0074 / 
      data a3 / 0.1106, 0.1106, 0.0695, 0.0793 / 
      data c4 / 12.6560, 12.6560, 8.6193, 16.0488 / 
      data dsi / 113.5005, 113.5005, 94.3497, 80.5859 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-intra Common Model017 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
      
c----------------------------------------------------------------------                
      Subroutine S04_SubInter_Common001 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 3.7424, 3.7424, 3.8775, 1.0247 / 
      data a2 / -1.3644, -1.3644, -1.0503, -0.8087 / 
      data a9 / 0.2930, 0.2930, 0.4142, 0.3487 / 
      data a6 / 0.0443, 0.0443, 0.0626, 0.0616 / 
      data a4 / 0.3180, 0.3180, 0.0171, 0.9498 / 
      data a5 / 0.0771, 0.0771, -0.1192, 0.1362 / 
      data a13 / -0.0370, -0.0370, -0.0859, -0.0553 / 
      data Mref / 7.3602, 7.3602, 6.4274, 7.2721 / 
      data a11 / 0.0288, 0.0288, 0.0300, -0.0027 / 
      data a3 / 0.0946, 0.0946, 0.1029, 0.0975 / 
      data c4 / 10.4162, 10.4162, 7.3671, 11.5527 / 
      data dsi / 30.2891, 30.2891, 44.8829, 45.8685 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-inter Common Model001 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubInter_Common002 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 5.3043, 5.3043, 6.2736, 1.4047 / 
      data a2 / -1.6592, -1.6592, -1.6213, -0.8688 / 
      data a9 / 0.4005, 0.4005, 0.3852, 0.3276 / 
      data a6 / 0.0429, 0.0429, 0.0422, 0.0546 / 
      data a4 / 0.3581, 0.3581, 0.2763, 0.7581 / 
      data a5 / 0.3253, 0.3253, 0.2038, 0.1679 / 
      data a13 / -0.0566, -0.0566, -0.0810, -0.0818 / 
      data Mref / 7.6478, 7.6478, 8.7756, 7.1762 / 
      data a11 / 0.0362, 0.0362, 0.0402, -0.0034 / 
      data a3 / 0.0839, 0.0839, 0.0649, 0.1098 / 
      data c4 / 13.7763, 13.7763, 15.7867, 11.6594 / 
      data dsi / 37.1124, 37.1124, 31.4039, 33.3315 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-inter Common Model002 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubInter_Common003 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 3.8164, 3.8164, 5.7130, 2.0769 / 
      data a2 / -1.3597, -1.3597, -1.5499, -1.0501 / 
      data a9 / 0.3580, 0.3580, 0.2663, 0.3705 / 
      data a6 / 0.0441, 0.0441, 0.0464, 0.0528 / 
      data a4 / 0.1181, 0.1181, 0.1041, 1.0282 / 
      data a5 / -0.3053, -0.3053, 0.1188, 0.7027 / 
      data a13 / -0.0558, -0.0558, -0.0985, -0.0737 / 
      data Mref / 7.3755, 7.3755, 7.6501, 7.0961 / 
      data a11 / 0.0214, 0.0214, 0.0241, -0.0016 / 
      data a3 / 0.1440, 0.1440, 0.0591, 0.0704 / 
      data c4 / 8.7174, 8.7174, 12.4920, 14.5969 / 
      data dsi / 39.0604, 39.0604, 52.0196, 30.1867 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-inter Common Model003 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end  
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubInter_Common004 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none
      
      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 3.6331, 3.6331, 5.4627, 1.8227 / 
      data a2 / -1.2616, -1.2616, -1.5036, -0.8205 / 
      data a9 / 0.2588, 0.2588, 0.3936, 0.4540 / 
      data a6 / 0.0523, 0.0523, 0.0481, 0.0639 / 
      data a4 / -0.1672, -0.1672, 0.3739, 0.7088 / 
      data a5 / -0.5452, -0.5452, 0.0766, 0.2037 / 
      data a13 / -0.0764, -0.0764, -0.0613, -0.1046 / 
      data Mref / 7.5591, 7.5591, 7.9215, 7.7277 / 
      data a11 / 0.0227, 0.0227, 0.0259, -0.0046 / 
      data a3 / 0.1775, 0.1775, 0.0823, 0.0835 / 
      data c4 / 7.6829, 7.6829, 9.8460, 10.5834 / 
      data dsi / 40.7574, 40.7574, 44.8569, 94.5097 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-inter Common Model004 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubInter_Common005 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 4.0315, 4.0315, 6.4900, 1.9982 / 
      data a2 / -1.4700, -1.4700, -1.6380, -0.9515 / 
      data a9 / 0.2128, 0.2128, 0.2840, 0.3020 / 
      data a6 / 0.0441, 0.0441, 0.0419, 0.0543 / 
      data a4 / 0.4509, 0.4509, 0.5884, 0.8981 / 
      data a5 / 0.2250, 0.2250, 0.3369, 0.4681 / 
      data a13 / -0.0333, -0.0333, -0.0617, -0.0733 / 
      data Mref / 7.7262, 7.7262, 8.1901, 7.2721 / 
      data a11 / 0.0224, 0.0224, 0.0177, -0.0013 / 
      data a3 / 0.0695, 0.0695, 0.0372, 0.0783 / 
      data c4 / 8.2992, 8.2992, 12.1733, 13.8116 / 
      data dsi / 28.7042, 28.7042, 56.4178, 16.7598 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-inter Common Model005 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubInter_Common006 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 4.7161, 4.7161, 4.9221, 1.8387 / 
      data a2 / -1.5401, -1.5401, -1.3365, -0.9124 / 
      data a9 / 0.2282, 0.2282, 0.2841, 0.4761 / 
      data a6 / 0.0351, 0.0351, 0.0610, 0.0533 / 
      data a4 / 0.5886, 0.5886, 0.2559, 0.9936 / 
      data a5 / -0.0864, -0.0864, 0.2783, 0.3269 / 
      data a13 / -0.0160, -0.0160, -0.0985, -0.0533 / 
      data Mref / 7.4555, 7.4555, 6.5394, 7.5154 / 
      data a11 / 0.0223, 0.0223, 0.0125, -0.0027 / 
      data a3 / 0.1237, 0.1237, 0.0588, 0.0746 / 
      data c4 / 10.1710, 10.1710, 8.7643, 12.7814 / 
      data dsi / 44.8458, 44.8458, 44.2255, 26.3434 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-inter Common Model006 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubInter_Common007 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 4.3371, 4.3371, 5.6258, 1.4876 / 
      data a2 / -1.4579, -1.4579, -1.3290, -0.9861 / 
      data a9 / 0.2150, 0.2150, 0.3967, 0.4154 / 
      data a6 / 0.0440, 0.0440, 0.0543, 0.0429 / 
      data a4 / 0.4709, 0.4709, 0.1021, 1.3009 / 
      data a5 / 0.1458, 0.1458, -0.1466, 0.5452 / 
      data a13 / -0.0216, -0.0216, -0.1026, -0.0232 / 
      data Mref / 7.3513, 7.3513, 7.5145, 7.1491 / 
      data a11 / 0.0168, 0.0168, 0.0204, -0.0022 / 
      data a3 / 0.1063, 0.1063, 0.0979, 0.0684 / 
      data c4 / 11.2785, 11.2785, 7.5206, 13.3581 / 
      data dsi / 45.9636, 45.9636, 70.1679, 30.6900 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-inter Common Model007 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubInter_Common008 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none
 
      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 4.2338, 4.2338, 4.5736, 2.2716 / 
      data a2 / -1.4621, -1.4621, -1.2444, -1.0188 / 
      data a9 / 0.3610, 0.3610, 0.2913, 0.6655 / 
      data a6 / 0.0459, 0.0459, 0.0496, 0.0600 / 
      data a4 / 0.6605, 0.6605, 0.3519, 0.9468 / 
      data a5 / 0.2848, 0.2848, 0.1728, 0.5838 / 
      data a13 / -0.0066, -0.0066, -0.0704, -0.0823 / 
      data Mref / 7.1221, 7.1221, 6.3425, 7.3565 / 
      data a11 / 0.0246, 0.0246, 0.0351, -0.0070 / 
      data a3 / 0.1004, 0.1004, 0.0715, 0.0556 / 
      data c4 / 11.1780, 11.1780, 8.3425, 16.8740 / 
      data dsi / 45.1281, 45.1281, 51.5860, 47.4536 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-inter Common Model008 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubInter_Common009 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 4.6888, 4.6888, 3.6625, 1.5377 / 
      data a2 / -1.4726, -1.4726, -0.9014, -0.9677 / 
      data a9 / 0.3634, 0.3634, 1.3694, 0.3981 / 
      data a6 / 0.0257, 0.0257, 0.0533, 0.0557 / 
      data a4 / 0.4345, 0.4345, 0.3430, 1.0165 / 
      data a5 / 0.0385, 0.0385, -0.1962, 0.3546 / 
      data a13 / -0.0370, -0.0370, -0.0453, -0.0545 / 
      data Mref / 7.2518, 7.2518, 9.1572, 7.2854 / 
      data a11 / 0.0389, 0.0389, 0.0364, -0.0007 / 
      data a3 / 0.0976, 0.0976, 0.1430, 0.0859 / 
      data c4 / 12.3550, 12.3550, 3.2002, 13.0072 / 
      data dsi / 50.1469, 50.1469, 40.0120, 41.4477 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-inter Common Model009 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 

c----------------------------------------------------------------------                
      Subroutine S04_SubInter_Common010 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 3.9684, 3.9684, 6.5625, 1.9887 / 
      data a2 / -1.3964, -1.3964, -1.4112, -1.0311 / 
      data a9 / 0.5189, 0.5189, 0.9364, 0.3983 / 
      data a6 / 0.0415, 0.0415, 0.0381, 0.0568 / 
      data a4 / 0.3623, 0.3623, 0.7416, 1.1997 / 
      data a5 / 0.1068, 0.1068, 0.1502, 0.5326 / 
      data a13 / -0.0410, -0.0410, -0.0427, -0.0497 / 
      data Mref / 7.4692, 7.4692, 9.2243, 7.3419 / 
      data a11 / 0.0350, 0.0350, 0.0375, -0.0021 / 
      data a3 / 0.0947, 0.0947, 0.0820, 0.0802 / 
      data c4 / 11.4784, 11.4784, 10.1992, 14.5644 / 
      data dsi / 38.4534, 38.4534, 57.7496, 18.4030 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-inter Common Model010 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 	  
c----------------------------------------------------------------------                
      Subroutine S04_SubInter_Common011 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 4.4088, 4.4088, 5.2053, 1.9462 / 
      data a2 / -1.6350, -1.6350, -1.5099, -0.9627 / 
      data a9 / 0.3923, 0.3923, 0.3651, 0.3418 / 
      data a6 / 0.0412, 0.0412, 0.0434, 0.0631 / 
      data a4 / 0.6072, 0.6072, -0.1079, 0.8899 / 
      data a5 / 0.4414, 0.4414, 0.0137, 0.4852 / 
      data a13 / -0.0193, -0.0193, -0.1055, -0.0844 / 
      data Mref / 7.2688, 7.2688, 7.2309, 7.2066 / 
      data a11 / 0.0187, 0.0187, 0.0352, -0.0046 / 
      data a3 / 0.0566, 0.0566, 0.0808, 0.1003 / 
      data c4 / 12.1529, 12.1529, 11.0067, 14.5843 / 
      data dsi / 41.6949, 41.6949, 36.6896, 78.2170 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-inter Common Model011 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubInter_Common012 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 4.0061, 4.0061, 4.3774, 2.2394 / 
      data a2 / -1.4827, -1.4827, -1.4047, -0.8579 / 
      data a9 / 0.3026, 0.3026, 0.3299, 0.4018 / 
      data a6 / 0.0489, 0.0489, 0.0500, 0.0631 / 
      data a4 / 0.4282, 0.4282, 0.2378, 0.7610 / 
      data a5 / 0.0520, 0.0520, -0.0657, 0.0929 / 
      data a13 / -0.0428, -0.0428, -0.0519, -0.0832 / 
      data Mref / 7.5922, 7.5922, 7.8303, 7.8494 / 
      data a11 / 0.0223, 0.0223, 0.0177, -0.0048 / 
      data a3 / 0.0953, 0.0953, 0.0943, 0.1132 / 
      data c4 / 8.2679, 8.2679, 6.1358, 12.4935 / 
      data dsi / 33.0608, 33.0608, 32.6260, 26.0587 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-inter Common Model012 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubInter_Common013 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 5.7752, 5.7752, 6.1411, 2.0259 / 
      data a2 / -1.8356, -1.8356, -1.5855, -0.8226 / 
      data a9 / 0.1917, 0.1917, 0.0399, 0.3818 / 
      data a6 / 0.0444, 0.0444, 0.0618, 0.0571 / 
      data a4 / 0.5433, 0.5433, 0.6053, 0.6445 / 
      data a5 / 0.3016, 0.3016, 0.2385, 0.0997 / 
      data a13 / -0.0339, -0.0339, -0.0455, -0.0976 / 
      data Mref / 7.2671, 7.2671, 8.3490, 7.7738 / 
      data a11 / 0.0245, 0.0245, 0.0135, -0.0040 / 
      data a3 / 0.1025, 0.1025, 0.0496, 0.0936 / 
      data c4 / 15.2827, 15.2827, 14.0772, 11.3501 / 
      data dsi / 29.5546, 29.5546, 46.9990, 58.7743 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-inter Common Model013 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubInter_Common014 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none
      
      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 3.7669, 3.7669, 5.1466, 1.4016 / 
      data a2 / -1.1842, -1.1842, -1.2774, -0.9265 / 
      data a9 / 0.2310, 0.2310, 0.0930, 0.4161 / 
      data a6 / 0.0315, 0.0315, 0.0695, 0.0453 / 
      data a4 / 0.5934, 0.5934, 0.6047, 1.4587 / 
      data a5 / -0.2999, -0.2999, -0.0152, 0.6386 / 
      data a13 / -0.0203, -0.0203, -0.0378, -0.0147 / 
      data Mref / 7.8361, 7.8361, 7.7247, 7.2088 / 
      data a11 / 0.0158, 0.0158, 0.0130, -0.0040 / 
      data a3 / 0.1285, 0.1285, 0.0963, 0.0511 / 
      data c4 / 5.6574, 5.6574, 10.8209, 12.5254 / 
      data dsi / 82.5864, 82.5864, 54.4898, 37.5935 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-inter Common Model014 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubInter_Common015 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 4.4670, 4.4670, 6.2871, 1.4544 / 
      data a2 / -1.3772, -1.3772, -1.5227, -0.9064 / 
      data a9 / 0.3084, 0.3084, 0.0780, 0.4053 / 
      data a6 / 0.0365, 0.0365, 0.0413, 0.0413 / 
      data a4 / 0.5852, 0.5852, 0.3567, 0.8821 / 
      data a5 / 0.1011, 0.1011, 0.1898, 0.1146 / 
      data a13 / -0.0301, -0.0301, -0.0944, -0.0546 / 
      data Mref / 7.6234, 7.6234, 6.4150, 7.0310 / 
      data a11 / 0.0136, 0.0136, 0.0238, -0.0040 / 
      data a3 / 0.0972, 0.0972, 0.0563, 0.1054 / 
      data c4 / 9.5482, 9.5482, 12.8497, 11.9022 / 
      data dsi / 60.1158, 60.1158, 64.9590, 35.2634 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-inter Common Model015 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubInter_Common016 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 4.6382, 4.6382, 5.5387, 2.1083 / 
      data a2 / -1.4006, -1.4006, -1.4005, -1.2265 / 
      data a9 / 0.3332, 0.3332, 0.2745, 0.6133 / 
      data a6 / 0.0346, 0.0346, 0.0377, 0.0355 / 
      data a4 / 0.3453, 0.3453, 0.7070, 1.3955 / 
      data a5 / 0.0096, 0.0096, 0.1410, 0.9071 / 
      data a13 / -0.0428, -0.0428, -0.0154, -0.0396 / 
      data Mref / 7.5658, 7.5658, 8.0771, 6.9316 / 
      data a11 / 0.0236, 0.0236, 0.0425, -0.0029 / 
      data a3 / 0.1092, 0.1092, 0.0892, 0.0322 / 
      data c4 / 13.7206, 13.7206, 12.1381, 16.0940 / 
      data dsi / 48.1439, 48.1439, 33.7854, 11.8031 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-inter Common Model016 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 
                        
c----------------------------------------------------------------------                
      Subroutine S04_SubInter_Common017 ( m, Rrup, ztor, specT, lnY, sigma, iflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=4)
      REAL Period(MAXPER), a1(MAXPER), a2(MAXPER), a3(MAXPER),  a11(MAXPER), dsi(MAXPER)
      REAL a4(MAXPER), a5(MAXPER), a6(MAXPER), c4(MAXPER), a9(MAXPER), a13(MAXPER)
      real Mref(MAXPER), Rrup, Ztor
      REAL M, specT, sigma
      REAL period2, lnY, period1, a1T, a2T, a3T, a4T, a5T, a6T
      REAL c4T, a9T, MrefT, dsiT, a11T, a13T
      integer iflag, count1, count2, nPer, i

      data period / 0.0,  0.01,  0.2,   2    / 
      data a1 / 4.7516, 4.7516, 6.4542, 1.8656 / 
      data a2 / -1.4691, -1.4691, -1.6687, -0.9552 / 
      data a9 / 0.3255, 0.3255, 0.3431, 0.3579 / 
      data a6 / 0.0375, 0.0375, 0.0240, 0.0536 / 
      data a4 / 0.2997, 0.2997, 0.7894, 0.9446 / 
      data a5 / -0.1630, -0.1630, 0.2290, 0.3337 / 
      data a13 / -0.0492, -0.0492, -0.0275, -0.0674 / 
      data Mref / 7.4556, 7.4556, 7.5839, 7.3076 / 
      data a11 / 0.0247, 0.0247, 0.0297, -0.0026 / 
      data a3 / 0.1396, 0.1396, 0.0499, 0.0817 / 
      data c4 / 11.0406, 11.0406, 11.3051, 13.2976 / 
      data dsi / 56.5213, 56.5213, 71.3392, 41.4432 / 


C Find the requested spectral period and corresponding coefficients
      nPer = 4
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         a1T   = a1(1)   
         a2T   = a2(1)   
         a3T   = a3(1)   
         a4T   = a4(1)   
         a5T   = a5(1)   
         a6T   = a6(1)   
         a9T   = a9(1)   
         a11T  = a11(1)  
         a13T  = a13(1)  
         c4T   = c4(1)  
         dsiT  = dsi(1)  
         MrefT = Mref(1) 
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020 
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Sub-inter Common Model017 Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),a1(count1),a1(count2),  
     +                 specT, a1T  ,iflag) 
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),    
     +                 specT, a2T  ,iflag) 
      call S24_interp (period(count1),period(count2),a3(count1),a3(count2),    
     +                 specT, a3T  ,iflag) 
      call S24_interp (period(count1),period(count2),a4(count1),a4(count2),    
     +                 specT, a4T  ,iflag) 
      call S24_interp (period(count1),period(count2),a5(count1),a5(count2),    
     +                 specT, a5T  ,iflag) 
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),    
     +                 specT, a6T  ,iflag) 
      call S24_interp (period(count1),period(count2),a9(count1),a9(count2),    
     +                 specT, a9T  ,iflag) 
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),    
     +                 specT, a11T ,iflag) 
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),    
     +                 specT, a13T ,iflag) 
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),    
     +                 specT, c4T  ,iflag) 
      call S24_interp (period(count1),period(count2),dsi(count1),dsi(count2),    
     +                 specT, dsiT ,iflag) 
      call S24_interp (period(count1),period(count2),Mref(count1),Mref(count2),    
     +                 specT, MrefT,iflag) 
   
 1011 period1 = specT                                                                                                              

C     Set Constant Terms

C     Compute the ground motion for the given spectral period. 
      if (M .le. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a4T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      elseif (M .gt. MrefT) then
         lnY = a1T - a6T**2*Rrup + ( a2T + a3T *(M-7.8))*alog(Rrup + c4T*exp(a9T*(M-6)))
     1         + a5T*(M-MrefT) + a13T*(10-M)**2 + a11T*(min(Ztor,dsiT)-0.5*dsiT)
      endif

      sigma = 0.65

c     Convert from g to gal                                                     
      lnY = lnY + 6.89                                                          

      period2 = period1

      return
      end 