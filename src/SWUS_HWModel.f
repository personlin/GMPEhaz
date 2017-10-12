c----------------------------------------------------------------------
      Subroutine S02_SWUS_HWModel ( m, Rrup, Rjb, Rx, Dip, Width, a10, ztor, specT, HWFactor )

      parameter (MAXPER=22)

      REAL Period(MAXPER), c11(MAXPER), c12(MAXPER), c13(MAXPER), c14(MAXPER), c15(MAXPER)
      REAL c2(MAXPER), c3(MAXPER), c4(MAXPER)
      real M, Rrup, Rjb, Rx, Dip, Width, specT, HWFactor, period1, ztor
      real c11T, c12T, c13T, c14T, c15T, c2T, c3T, c4T, a10, magtaper, disttaper, ztorTaper
      integer count1, count2

      data Period  /  0, 0.01, 0.02, 0.03, 0.05, 0.075, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4,
     1         0.5, 0.75, 1, 1.5, 2, 3, 4, 5, 7.5, 10  /
      data c11  /  0.868, 0.868, 0.867, 0.856, 0.84, 0.857, 0.848, 0.868, 0.85, 0.868, 0.839,
     1         0.78, 0.741, 0.613, 0.621, 0.506, 0.391, 0.128, 0, 0, 0, 0  /
      data c12  /  0.982, 0.982, 0.987, 0.997, 1.027, 1.041, 1.04, 1.009, 1.005, 0.985, 0.974,
     1         0.934, 0.902, 0.869, 0.788, 0.662, 0.537, 0.245, 0.034, 0, 0, 0  /
      data c13  /  1.038, 1.038, 1.046, 1.067, 1.121, 1.133, 1.135, 1.08, 1.082, 1.044, 1.041,
     1         1.011, 0.982, 0.997, 0.872, 0.74, 0.609, 0.304, 0.088, 0, 0, 0  /
      data c14  /  1.095, 1.095, 1.106, 1.138, 1.215, 1.226, 1.231, 1.15, 1.16, 1.102, 1.108,
     1         1.089, 1.063, 1.125, 0.955, 0.818, 0.682, 0.362, 0.138, 0, 0, 0  /
      data c15  /  1.209, 1.209, 1.226, 1.278, 1.402, 1.41, 1.422, 1.292, 1.315, 1.219, 1.242,
     1         1.243, 1.223, 1.38, 1.123, 0.974, 0.828, 0.48, 0.231, 0.04, 0, 0  /
      data c2  /  0.216, 0.216, 0.2172, 0.2178, 0.2199, 0.2218, 0.2213, 0.2169, 0.2131, 0.1988,
     1         0.2019, 0.209, 0.2053, 0.1713, 0.1571, 0.1559, 0.1559, 0.1616, 0.1616, 0.1616,
     1         0.1616, 0.1616  /
      data c3  /  2.0289, 2.0289, 2.026, 2.0163, 1.987, 1.9906, 1.9974, 2.0162, 1.9746, 1.9931,
     1         2.0179, 2.0249, 2.0041, 1.8697, 1.8526, 1.8336, 1.7996, 1.674, 1.674, 1.674,
     1         1.674, 1.674  /
      data c4  /  0.1675, 0.1675, 0.1666, 0.167, 0.1699, 0.1817, 0.1717, 0.1814, 0.1834,
     1         0.1767, 0.1658, 0.1624, 0.1719, 0.1866, 0.3143, 0.3195, 0.3246, 0.3314, 0.3314,
     1         0.3314, 0.3314, 0.3314  /

C Find the requested spectral period and corresponding coefficients
      nPer = 22
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         c11T      = c11(1)
         c12T      = c12(1)
         c13T      = c13(1)
         c14T      = c14(1)
         c15T      = c15(1)
         c2T       = c2(1)
         c3T       = c3(1)
         c4T       = c4(1)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the SWUS Hanging Wall Model.
         do i=2,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
C      write (*,*)
C      write (*,*) 'SWUS Hanging Wall Model'
C      write (*,*) 'is not defined for a '
C      write (*,*) ' spectral period of: '
C      write (*,*)') ' Period = ',specT
C      write (*,*) 'This spectral period is outside the defined'
C      write (*,*) 'period range in the code or beyond the range'
C      write (*,*) 'of spectral periods for interpolation.'
C      write (*,*) 'Please check the input file.'
C      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call interp (period(count1),period(count2),c11(count1),c11(count2),
     +             specT,c11T,iflag)
      call interp (period(count1),period(count2),c12(count1),c12(count2),
     +             specT,c12T,iflag)
      call interp (period(count1),period(count2),c13(count1),c13(count2),
     +             specT,c13T,iflag)
      call interp (period(count1),period(count2),c14(count1),c14(count2),
     +             specT,c14T,iflag)
      call interp (period(count1),period(count2),c14(count1),c15(count2),
     +             specT,c15T,iflag)
      call interp (period(count1),period(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call interp (period(count1),period(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call interp (period(count1),period(count2),c4(count1),c4(count2),
     +             specT,c4T,iflag)

 1011 period1 = specT

C     Compute the Hanging Wall Factor.
C     First set the C1 term based on the a10 coefficient
      if (a10 .eq. 1.0) then
         c1 = c11T
      elseif (a10 .eq. 2.0) then
         c1 = c12T
      elseif (a10 .eq. 3.0) then
         c1 = c13T
      elseif (a10 .eq. 4.0) then
         c1 = c14T
      elseif (a10 .eq. 5.0) then
         c1 = c15T
      else
         HWFactor = 0.0
         return
      endif

C     Compute HW Factor.
      magtaper = (1.0 + c4T*(M-7.0))
      disttaper = (1.0 - Rjb/(Rrup+0.1))
      ztortaper = max(0.0,1.0-ztor/12.0)

      HWFactor = c1*cos(dip*3.14159/180.0) * (c2T + (1.0 - c2T)*tanh(c3T*Rx/(width*cos(dip*3.14159/180.0)))) *
     1        magtaper * disttaper * ztorTaper

      return
      end