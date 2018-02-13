c ---------------------------------------------------------------------
C     *** Akkar, Sandikkaya, and Bommer (2013) ***
c ---------------------------------------------------------------------

      subroutine ASB_2013 ( mag, Rbjf, specT,
     1                     period2, lnY, sigma, iflag, ftype, Vs, phiT, tauT )

      parameter (MAXPER=20)
      REAL Period(MAXPER), a1(MAXPER), a3(MAXPER), a4(MAXPER), a8(MAXPER)
      Real a9(MAXPER), b1(MAXPER), b2(MAXPER), phi(MAXPER), tau(MAXPER)
      real specT, a1T, a3T, a4T, a8T, a9T, b1T, b2T, phiT, tauT
      real mag, Rbjf, Ftype, Fn, Fr, Vs, lnY
      real a2, a5, a6, a7, c1, c, n
      INTEGER iFlag, count1, count2


      Data period / 0.00, -1.00, 0.01, 0.02, 0.03, 0.04, 0.05, 0.075, 0.1, 0.15,
     1              0.2, 0.3, 0.4, 0.5, 0.75, 1.00, 1.5, 2.00, 3.00, 4.00 /
      Data a1     / 1.85329, 5.61201, 1.87032, 1.95279, 2.07006, 2.20452, 2.35413,
     1              2.63078, 2.85412, 2.96622, 2.73872, 2.3015, 1.89372, 1.67127,
     2              0.95211, 0.52349, -0.01867, -0.42891, -1.05642, -1.37536 /
      Data a3     / -0.02807, -0.0998, -0.0274, -0.02715, -0.02403, -0.01797, -0.01248,
     1              -0.00532, -0.00925, -0.02193, -0.03462, -0.05672, -0.07684, -0.0949,
     2              -0.12347, -0.14345, -0.17187, -0.19029, -0.21392, -0.23848 /
      Data a4     / -1.23452, -0.98388, -1.23698, -1.25363, -1.27525, -1.30123, -1.32632,
     1              -1.35722, -1.38182, -1.3646, -1.28877, -1.17072, -1.0653, -1.01909,
     2              -0.88393, -0.81838, -0.75751, -0.72033, -0.69085, -0.66482 /
      Data a8     / -0.1091, -0.0616, -0.1115, -0.104, -0.0973, -0.0884, -0.0853,
     1              -0.0779, -0.0749, -0.0265, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
     2               0.00, 0.00, 0.00, 0.00 /
      Data a9     / 0.0937, 0.063, 0.0953, 0.1029, 0.1148, 0.1073, 0.1052, 0.0837,
     1              0.0761, 0.0545, 0.0493, 0.0469, 0.04, 0.0271, 0.0141, 0.00,
     2              0.00, -0.009, -0.0683, -0.2231 /
      Data b1     / -0.41997, -0.72057, -0.41729, -0.39998, -0.34799, -0.27572,
     1              -0.21231, -0.14427, -0.27064, -0.48313, -0.65315, -0.82609,
     2              -0.89517, -0.94614, -1.00786, -1.01331, -0.98071, -0.91007,
     3              -0.85793, -0.75645 /
      Data b2     / -0.28846, -0.19688, -0.28685, -0.28241, -0.26842, -0.24759,
     1              -0.22385, -0.17525, -0.29293, -0.39551, -0.44644, -0.4573,
     2              -0.43008, -0.37408, -0.28957, -0.28702, -0.24695, -0.17336,
     3              -0.13336, -0.07749 /
      Data phi    / 0.6201, 0.6014, 0.6215, 0.6266, 0.641, 0.6534, 0.6622, 0.6626,
     1              0.667, 0.6796, 0.6645, 0.6599, 0.6697, 0.6512, 0.6744, 0.6787,
     2              0.7164, 0.7254, 0.6997, 0.6196 /
      Data tau    / 0.3501, 0.3311, 0.3526, 0.3555, 0.3565, 0.3484, 0.3551, 0.3759,
     1              0.4067, 0.3893, 0.3842, 0.3816, 0.3962, 0.4021, 0.4043, 0.3943,
     2              0.3799, 0.3717, 0.4046, 0.3566 /
c      Data sigma  / 0.7121, 0.6865, 0.7146, 0.7204, 0.7335, 0.7405, 0.7514, 0.7618,
c     1              0.7812, 0.7832, 0.7676, 0.7623, 0.7781, 0.7653, 0.7863, 0.7849,
c     2              0.8109, 0.8151, 0.8083, 0.7149 /



C First check for the PGA case (i.e., specT=0.0)
      nPer = 20
      if (specT .eq. 0.0) then
         period1 = period(1)
         a1T = a1(1)
         a3T = a3(1)
         a4T = a4(1)
         a8T = a8(1)
         a9T = a9(1)
         b1T = b1(1)
         b2T = b2(1)
         phiT = phi(1)
         tauT = tau(1)
         goto 1011
C Check for the PGV case (i.e., specT=-1.0)
      elseif (specT .eq. -1.0) then
         period1 = period(2)
         a1T = a1(2)
         a3T = a3(2)
         a4T = a4(2)
         a8T = a8(2)
         a9T = a9(2)
         b1T = b1(2)
         b2T = b2(2)
         phiT = phi(2)
         tauT = tau(2)
         goto 1011
      elseif (specT .gt. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=3,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C Selected spectral period is outside range defined by attenuaton model.
C      write (*,*)
C      write (*,*) 'Akkar,Sandikkaya&Bommer (2013) Horizontal'
C      write (*,*) 'attenuation model is not defined for a '
C      write (*,*) ' spectral period of: '
C      write (*,*)') ' Period = ',specT
C      write (*,*) 'This spectral period is outside the defined'
C      write (*,*) 'period range in the code or beyond the range'
C      write (*,*) 'of spectral periods for interpolation.'
C      write (*,*) 'Please check the input file.'
C      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020       call interp (period(count1),period(count2),a1(count1),a1(count2),
     +                   specT,a1T,iflag)
            call interp (period(count1),period(count2),a3(count1),a3(count2),
     +                   specT,a3T,iflag)
            call interp (period(count1),period(count2),a4(count1),a4(count2),
     +                   specT,a4T,iflag)
            call interp (period(count1),period(count2),a8(count1),a8(count2),
     +                   specT,a8T,iflag)
            call interp (period(count1),period(count2),a9(count1),a9(count2),
     +                   specT,a9T,iflag)
            call interp (period(count1),period(count2),b1(count1),b1(count2),
     +                   specT,b1T,iflag)
            call interp (period(count1),period(count2),b2(count1),b2(count2),
     +                   specT,b2T,iflag)
            call interp (period(count1),period(count2),phi(count1),phi(count2),
     +                   specT,phiT,iflag)
            call interp (period(count1),period(count2),tau(count1),tau(count2),
     +                   specT,tauT,iflag)
 1011 period1 = specT

C.....Set the mechanism terms based on ftype............
C     Set mechanism term and corresponding Frv and Fnm values.
C     fType     Mechanism                      Rake
C     ------------------------------------------------------
C      -1       Normal                    -120 < Rake <  -60
C     -0.5      Normal/Oblique            -150 < Rake < -120
C                                          -60 < Rake <  -30
C       0       Strike-Slip               -180 < Rake < -150
C                                          -30 < Rake <   30
C                                          150 < Rake <  180
C      0.5      Reverse/Oblique             30 < Rake <   60
C                                          120 < Rake <  150
C       1       Reverse                     60 < Rake <  120
      if (ftype .eq. -1.0) then
         Fr = 0.0
         Fn = 1.0
      elseif (ftype .eq. -0.5) then
         Fr = 0.0
         Fn = 1.0
      elseif (ftype .eq. 0.0) then
         Fr = 0.0
         Fn = 0.0
      elseif (ftype .eq. 0.5) then
         Fr = 1.0
         Fn = 0.0
      elseif (ftype .eq. 1.0) then
         Fr = 1.0
         Fn = 0.0
      endif

C     Set frequency independent terms
      a2 = 0.0029
      a5 = 0.2529
      a6 = 7.5
      a7 = -0.5096
      c1 = 6.75
      c = 2.5
      n = 3.2


C     Compute the PGA for reference Vs=750m/s.
      if (mag .le. c1 ) then
         pgaref = a1(1) + a2*(mag-c1) + a3(1)*(8.5-mag)**2.0 +
     1                 (a4(1)+a5*(mag-c1))*alog(sqrt(Rbjf*Rbjf+a6*a6)) +
     2                  a8(1)*Fn + a9(1)*Fr
      else
         pgaref = a1(1) + a7*(mag-c1) + a3(1)*(8.5-mag)**2.0 +
     1                 (a4(1)+a5*(mag-c1))*alog(sqrt(Rbjf*Rbjf+a6*a6)) +
     2                  a8(1)*Fn + a9(1)*Fr
      endif

      pgaref = exp(pgaref)

C.....Now compute the ground motion value........
      if (mag .le. c1 ) then
         lnY = a1T + a2*(mag-c1) + a3T*(8.5-mag)**2.0 +
     1                 (a4T+a5*(mag-c1))*alog(sqrt(Rbjf*Rbjf+a6*a6)) +
     2                  a8T*Fn + a9T*Fr
      else
         lnY = a1T + a7*(mag-c1) + a3T*(8.5-mag)**2.0 +
     1                 (a4T+a5*(mag-c1))*alog(sqrt(Rbjf*Rbjf+a6*a6)) +
     2                  a8T*Fn + a9T*Fr
      endif

C.....Now apply site amplification term......
      if (vs .le. 750.0) then
         lnY = lnY + b1T*alog(Vs/750.0) +
     1         b2T*alog( (pgaref + c*(Vs/750.0)**n) / ((pgaref+c)*(Vs/750.0)**n) )
      else
         lnY = lnY + b1T*alog( min(Vs,1000.0)/750.0)
      endif

C.....Set Sigma value.........
      sigma = sqrt (phiT*phiT + tauT*tauT)

C     Convert ground motion to units of gals.
      lnY = lnY + 6.89
      period2 = period1

      return
      END

c ---------------------------------------------------------------------
C     *** Akkar, Sandikkaya, and Bommer (2013) *** Adjusted in Taiwan SSHAC Project
c ---------------------------------------------------------------------

      subroutine ASB14_TW_B01 ( mag, Rbjf, specT,
     1                     period2, lnY, sigma, iflag, ftype, Vs, phiT, tauT )

      implicit none

      integer MAXPER
      parameter (MAXPER=22)
      REAL Period(MAXPER), a1(MAXPER), a3(MAXPER), a4(MAXPER), a8(MAXPER)
      Real a9(MAXPER), b1(MAXPER), b2(MAXPER), phi(MAXPER), tau(MAXPER)
      real specT, a1T, a3T, a4T, a8T, a9T, b1T, b2T, phiT, tauT, period2
      real mag, Rbjf, Ftype, Fn, Fr, Vs, lnY
      real a2, a5, a6, a7, c1, c, n, sigma, period1, pgaref
      INTEGER iFlag, count1, count2, nPer, i


      Data Period / 0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.075, 0.1, 0.12, 0.15, 0.17, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 1, 1.5, 2, 3, 4/
      Data a1 / 1.94463701030923, 1.95633129902868, 2.04961042213603, 2.16227168865562, 2.31147154107281, 2.46468292291584,
     1      2.78220203310757, 3.08223188958564, 3.16570312391914, 3.27093892279077, 3.18678792139014, 3.07342350605342,
     1      2.86060109901462, 2.67230039929716, 2.1872922539731, 1.93261959741883, 1.08127789108881, 0.557354348416022,
     1      -0.127502526607232, -0.627189109901032, -1.3130663692331, -1.84307141810676/
      Data a3 / -0.02807, -0.0274, -0.02715, -0.02403, -0.01797, -0.01248, -0.00532, -0.00925, -0.0149516924364565, -0.02193,
     1      -0.0274510958074301, -0.03462, -0.0467825076620119, -0.05672, -0.07684, -0.0949, -0.12347, -0.14345, -0.17187,
     1      -0.19029, -0.21392, -0.23848/
      Data a4 / -1.23452, -1.23698, -1.25363, -1.27525, -1.30123, -1.32632, -1.35722, -1.38182, -1.37407684986153, -1.3646,
     1      -1.33160829826025, -1.28877, -1.22380239685518, -1.17072, -1.0653, -1.01909, -0.88393, -0.81838, -0.75751, -0.72033,
     1      -0.69085, -0.66482/
      Data a8 / -0.157411753347275, -0.158643647916997, -0.156498346437503, -0.159058902706085, -0.147473117402867,
     1      -0.149142639692941, -0.174498659620793, -0.20284855762965, -0.188702613830932, -0.152649659196236, -0.13458728500518,
     1      -0.129980508698776, -0.113948592896577, -0.119728317000499, -0.167811755184416, -0.211965186805293, -0.239310892722394,
     1      -0.27828316858994, -0.246927051034126, -0.245814057233351, -0.100038829538664, -0.0288012340748359/
      Data a9 / 0.0927938497890813, 0.0904790804979972, 0.08970001648679, 0.0873956489940766, 0.0887395768983498,
     1      0.0845798708569521, 0.0585906993213455, 0.0353035799960799, 0.0335906927152189, 0.0363289049151887, 0.0513841269650146,
     1      0.0791227682725176, 0.104890709846307, 0.106334776312819, 0.126469498519124, 0.132781431780333, 0.15548090181915,
     1      0.173124244253594, 0.153458212068572, 0.127324158905687, 0.100483673763759, 0.0243936051615504/
      Data b1 / -0.526695440564555, -0.524588961680896, -0.515830137575019, -0.494044493426923, -0.461219462093833,
     1      -0.421280305317738, -0.372113499447421, -0.422238715676676, -0.461651482408679, -0.509629278576738, -0.535239495062594,
     1      -0.55501522624117, -0.579035644559409, -0.63554540374141, -0.703894218719503, -0.73686407991816, -0.903566014496682,
     1      -1.02500940861051, -1.12152430421438, -1.10937061533022, -1.1007805796447, -1.06601129602676/
      Data b2 / -0.28846, -0.28685, -0.28241, -0.26842, -0.24759, -0.22385, -0.17525, -0.29293, -0.339056152218589, -0.39551,
     1      -0.417668345900112, -0.44644, -0.452416689285495, -0.4573, -0.43008, -0.37408, -0.28957, -0.28702, -0.24695, -0.17336,
     1      -0.13336, -0.07749/
      Data phi / 0.6201, 0.6215, 0.6266, 0.641, 0.6534, 0.6622, 0.6626, 0.667, 0.672665719613514, 0.6796, 0.673030374571143,
     1      0.6645, 0.661968437319219, 0.6599, 0.6697, 0.6512, 0.6744, 0.6787, 0.7164, 0.7254, 0.6997, 0.6196/
      Data tau / 0.352843793444255, 0.345405282760873, 0.346837236077185, 0.354333841257643, 0.36600091911867, 0.37728093380395,
     1      0.39747921770377, 0.398278000536948, 0.389671171041168, 0.387548652157001, 0.382740826478284, 0.369515507707594,
     1      0.35692161370857, 0.360549438147831, 0.382106474902984, 0.404417574880376, 0.428749763477024, 0.441719248359787,
     1      0.449928617337981, 0.465665315601137, 0.475600872655558, 0.505107922646745/

c      Data sigma  / 0.7121, 0.6865, 0.7146, 0.7204, 0.7335, 0.7405, 0.7514, 0.7618,
c     1              0.7812, 0.7832, 0.7676, 0.7623, 0.7781, 0.7653, 0.7863, 0.7849,
c     2              0.8109, 0.8151, 0.8083, 0.7149 /



C First check for the PGA case (i.e., specT=0.0)
      nPer = 22
      if (specT .eq. 0.0) then
         period1 = period(1)
         a1T = a1(1)
         a3T = a3(1)
         a4T = a4(1)
         a8T = a8(1)
         a9T = a9(1)
         b1T = b1(1)
         b2T = b2(1)
         phiT = phi(1)
         tauT = tau(1)
         goto 1011
C Check for the PGV case (i.e., specT=-1.0)
      elseif (specT .eq. -1.0) then
         period1 = period(2)
         a1T = a1(2)
         a3T = a3(2)
         a4T = a4(2)
         a8T = a8(2)
         a9T = a9(2)
         b1T = b1(2)
         b2T = b2(2)
         phiT = phi(2)
         tauT = tau(2)
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
      write (*,*) 'Akkar,Sandikkaya&Bommer (2013) Horizontal'
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
            call interp (period(count1),period(count2),a3(count1),a3(count2),
     +                   specT,a3T,iflag)
            call interp (period(count1),period(count2),a4(count1),a4(count2),
     +                   specT,a4T,iflag)
            call interp (period(count1),period(count2),a8(count1),a8(count2),
     +                   specT,a8T,iflag)
            call interp (period(count1),period(count2),a9(count1),a9(count2),
     +                   specT,a9T,iflag)
            call interp (period(count1),period(count2),b1(count1),b1(count2),
     +                   specT,b1T,iflag)
            call interp (period(count1),period(count2),b2(count1),b2(count2),
     +                   specT,b2T,iflag)
            call interp (period(count1),period(count2),phi(count1),phi(count2),
     +                   specT,phiT,iflag)
            call interp (period(count1),period(count2),tau(count1),tau(count2),
     +                   specT,tauT,iflag)
 1011 period1 = specT

C.....Set the mechanism terms based on ftype............
C     Set mechanism term and corresponding Frv and Fnm values.
C     fType     Mechanism                      Rake
C     ------------------------------------------------------
C      -1       Normal                    -120 < Rake <  -60
C     -0.5      Normal/Oblique            -150 < Rake < -120
C                                          -60 < Rake <  -30
C       0       Strike-Slip               -180 < Rake < -150
C                                          -30 < Rake <   30
C                                          150 < Rake <  180
C      0.5      Reverse/Oblique             30 < Rake <   60
C                                          120 < Rake <  150
C       1       Reverse                     60 < Rake <  120
      if (ftype .eq. -1.0) then
         Fr = 0.0
         Fn = 1.0
      elseif (ftype .eq. -0.5) then
         Fr = 0.0
         Fn = 1.0
      elseif (ftype .eq. 0.0) then
         Fr = 0.0
         Fn = 0.0
      elseif (ftype .eq. 0.5) then
         Fr = 1.0
         Fn = 0.0
      elseif (ftype .eq. 1.0) then
         Fr = 1.0
         Fn = 0.0
      endif

C     Set frequency independent terms
      a2 = 0.0029
      a5 = 0.2529
      a6 = 7.5
      a7 = -0.5096
      c1 = 6.75
      c = 2.5
      n = 3.2


C     Compute the PGA for reference Vs=750m/s.
      if (mag .le. c1 ) then
         pgaref = a1(1) + a2*(mag-c1) + a3(1)*(8.5-mag)**2.0 +
     1                 (a4(1)+a5*(mag-c1))*alog(sqrt(Rbjf*Rbjf+a6*a6)) +
     2                  a8(1)*Fn + a9(1)*Fr
      else
         pgaref = a1(1) + a7*(mag-c1) + a3(1)*(8.5-mag)**2.0 +
     1                 (a4(1)+a5*(mag-c1))*alog(sqrt(Rbjf*Rbjf+a6*a6)) +
     2                  a8(1)*Fn + a9(1)*Fr
      endif

      pgaref = exp(pgaref)

C.....Now compute the ground motion value........
      if (mag .le. c1 ) then
         lnY = a1T + a2*(mag-c1) + a3T*(8.5-mag)**2.0 +
     1                 (a4T+a5*(mag-c1))*alog(sqrt(Rbjf*Rbjf+a6*a6)) +
     2                  a8T*Fn + a9T*Fr
      else
         lnY = a1T + a7*(mag-c1) + a3T*(8.5-mag)**2.0 +
     1                 (a4T+a5*(mag-c1))*alog(sqrt(Rbjf*Rbjf+a6*a6)) +
     2                  a8T*Fn + a9T*Fr
      endif

C.....Now apply site amplification term......
      if (vs .le. 750.0) then
         lnY = lnY + b1T*alog(Vs/750.0) +
     1         b2T*alog( (pgaref + c*(Vs/750.0)**n) / ((pgaref+c)*(Vs/750.0)**n) )
      else
         lnY = lnY + b1T*alog( min(Vs,1000.0)/750.0)
      endif

C.....Set Sigma value.........
      sigma = sqrt (phiT*phiT + tauT*tauT)

C     Convert ground motion to units of gals.
      lnY = lnY + 6.89
      period2 = period1

      return
      END

c ---------------------------------------------------------------------
C     *** Akkar, Sandikkaya, and Bommer (2013) *** Adjusted in Taiwan SSHAC Project
c ---------------------------------------------------------------------
      subroutine S04_ASB14_TW_C01 ( mag, Rbjf, specT,
     1                     period2, lnY, sigma, iflag, ftype, Vs, phiT, tauT )
      implicit none
      integer MAXPER
      parameter (MAXPER=22)
      REAL Period(MAXPER), a1(MAXPER), a3(MAXPER), a4(MAXPER), a8(MAXPER)
      Real a9(MAXPER), b1(MAXPER), b2(MAXPER), phi(MAXPER), tau(MAXPER)
      real specT, a1T, a3T, a4T, a8T, a9T, b1T, b2T, phiT, tauT, period2
      real mag, Rbjf, Ftype, Fn, Fr, Vs, lnY
      real a5, a6, a7, c1, c, n, sigma, period1, pgaref
      INTEGER iFlag, count1, count2, nPer, i
      real a2(MAXPER), a2T


      Data Period(1:22) / 0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.075, 0.1, 0.12, 0.15, 0.17, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 1, 1.5, 2,
     1      3, 4/
      Data a1(1:22) / 1.95754376344839, 2.00817307775539, 2.10206659629286, 2.20961153930911, 2.35078005202385, 2.49873114534198,
     1      2.81381004271636, 3.12694681898327, 3.21378881748549, 3.32779032291873, 3.24345450186723, 3.12402349228009,
     1      2.91284344812683, 2.72736664472706, 2.24534448743599, 1.99211376932964, 1.1530939925732, 0.641079591580465,
     1      -0.0254787065133857, -0.511129081837452, -1.18463752251569, -1.73520928065602/
      Data a3(1:22) / -0.02807, -0.0274, -0.02715, -0.02403, -0.01797, -0.01248, -0.00532, -0.00925, -0.0149516924364565, -0.02193,
     1      -0.0274510958074301, -0.03462, -0.0467825076620119, -0.05672, -0.07684, -0.0949, -0.12347, -0.14345, -0.17187,
     1      -0.19029, -0.21392, -0.23848/
      Data a4(1:22) / -1.23452, -1.23698, -1.25363, -1.27525, -1.30123, -1.32632, -1.35722, -1.38182, -1.37407684986153, -1.3646,
     1      -1.33160829826025, -1.28877, -1.22380239685518, -1.17072, -1.0653, -1.01909, -0.88393, -0.81838, -0.75751, -0.72033,
     1      -0.69085, -0.66482/
      Data a8(1:22) / -0.103636044256603, -0.132928920885736, -0.12986863151264, -0.129851892594279, -0.116141160848401,
     1      -0.119177416631283, -0.139260487428174, -0.171750171226412, -0.157099766506712, -0.120064436335966, -0.10406684558815,
     1      -0.104366241763941, -0.0899951148151869, -0.0994122752510732, -0.144947920374219, -0.196997416408109,
     1      -0.222073676123046, -0.25282351912814, -0.234590527565048, -0.232998579238557, -0.0848380615438779, 0.0861377135345235/
      Data a9(1:22) / 0.126400204982005, 0.0932411706137936, 0.0919391699827972, 0.0864920444615963, 0.0846063286281077,
     1      0.0774891547676303, 0.0493136670345936, 0.0343857396319973, 0.0339561453478299, 0.0418349456816038, 0.0612488290723383,
     1      0.0952480389354383, 0.122571526364686, 0.122639270021399, 0.13891186952902, 0.144080973849871, 0.164385885840223,
     1      0.17991213158502, 0.163888527122665, 0.144017834272244, 0.108612885472365, 0.0432613427525789/
      Data b1(1:22) / -0.511626065987955, -0.53046724276345, -0.52254494554547, -0.50864646002686, -0.485614368483321,
     1      -0.453434312321338, -0.407260400776915, -0.434402182634647, -0.46871799480745, -0.50684048713477, -0.53389023952211,
     1      -0.553869172268956, -0.561144973177327, -0.608010203560465, -0.666054146807456, -0.700286156154561, -0.855477635291906,
     1      -0.966259736674558, -1.04096576213125, -1.02116484088795, -1.02432695483163, -1.01420693530889/
      Data b2(1:22) / -0.28846, -0.28685, -0.28241, -0.26842, -0.24759, -0.22385, -0.17525, -0.29293, -0.339056152218589, -0.39551,
     1      -0.417668345900112, -0.44644, -0.452416689285495, -0.4573, -0.43008, -0.37408, -0.28957, -0.28702, -0.24695, -0.17336,
     1      -0.13336, -0.07749/
      Data phi(1:22) / 0.332271753816561, 0.331853645328173, 0.332226638023841, 0.340352054536531, 0.356032340893303,
     1      0.375882001454717, 0.414401540225185, 0.425576834343117, 0.421822057126099, 0.401352020442047, 0.390089235488791,
     1      0.370563407684413, 0.347128326879158, 0.339900164897318, 0.32815538601622, 0.317687018977947, 0.325315598182983,
     1      0.327742581374142, 0.354898012625689, 0.371465570115995, 0.390612539946501, 0.395777102651996/
      Data tau(1:22) / 0.350418686172617, 0.347748165151219, 0.350377709334341, 0.358987239286868, 0.369064296128902,
     1      0.378518325684224, 0.396400441287509, 0.396733147119714, 0.390535011398694, 0.386521582565263, 0.384648579313126,
     1      0.375129445685108, 0.362144468664536, 0.366581082849823, 0.386276478084319, 0.406760171807368, 0.436916198216006,
     1      0.454080668424532, 0.4699258063812, 0.48322880880426, 0.506656166693695, 0.521040518558988/
       Data a2(1:22) / 0.050335468, 0.106913336, 0.106685064, 0.106873132, 0.117318196, 0.111561424, 0.10736431 , 0.103185029,
     1       0.095335574, 0.131059036, 0.127769367, 0.131100025, 0.131821032, 0.153387307, 0.129558757, 0.125270741,
     1       0.116455398, 0.143364126, 0.146078527, 0.131832393, 0.128727888, 0.007683513 /



C First check for the PGA case (i.e., specT=0.0)
      nPer = 22
      if (specT .eq. 0.0) then
         period1 = period(1)
         a1T = a1(1)
         a2T = a2(1)
         a3T = a3(1)
         a4T = a4(1)
         a8T = a8(1)
         a9T = a9(1)
         b1T = b1(1)
         b2T = b2(1)
         phiT = phi(1)
         tauT = tau(1)
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
      write (*,*) 'Akkar,Sandikkaya&Bommer (2013) Horizontal'
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
            call S24_interp (period(count1),period(count2),a3(count1),a3(count2),
     +                   specT,a3T,iflag)
            call S24_interp (period(count1),period(count2),a4(count1),a4(count2),
     +                   specT,a4T,iflag)
            call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +                   specT,a8T,iflag)
            call S24_interp (period(count1),period(count2),a9(count1),a9(count2),
     +                   specT,a9T,iflag)
            call S24_interp (period(count1),period(count2),b1(count1),b1(count2),
     +                   specT,b1T,iflag)
            call S24_interp (period(count1),period(count2),b2(count1),b2(count2),
     +                   specT,b2T,iflag)
            call S24_interp (period(count1),period(count2),phi(count1),phi(count2),
     +                   specT,phiT,iflag)
            call S24_interp (period(count1),period(count2),tau(count1),tau(count2),
     +                   specT,tauT,iflag)
 1011 period1 = specT
C.....Set the mechanism terms based on ftype............
C     Set mechanism term and corresponding Frv and Fnm values.
C     fType     Mechanism                      Rake
C     ------------------------------------------------------
C      -1       Normal                    -120 < Rake <  -60
C     -0.5      Normal/Oblique            -150 < Rake < -120
C                                          -60 < Rake <  -30
C       0       Strike-Slip               -180 < Rake < -150
C                                          -30 < Rake <   30
C                                          150 < Rake <  180
C      0.5      Reverse/Oblique             30 < Rake <   60
C                                          120 < Rake <  150
C       1       Reverse                     60 < Rake <  120
      if (ftype .eq. -1.0) then
         Fr = 0.0
         Fn = 1.0
      elseif (ftype .eq. -0.5) then
         Fr = 0.0
         Fn = 1.0
      elseif (ftype .eq. 0.0) then
         Fr = 0.0
         Fn = 0.0
      elseif (ftype .eq. 0.5) then
         Fr = 1.0
         Fn = 0.0
      elseif (ftype .eq. 1.0) then
         Fr = 1.0
         Fn = 0.0
      endif
C     Set frequency independent terms
c      a2 = 0.0029
      a5 = 0.2529
      a6 = 7.5
      a7 = -0.5096
      c1 = 6.75
      c = 2.5
      n = 3.2
C     Compute the PGA for reference Vs=750m/s.
      if (mag .lt. c1 ) then
         pgaref = a1(1) + a2(1)*(mag-c1) + a3(1)*(8.5-mag)**2.0 +
     1                 (a4(1)+a5*(mag-c1))*alog(sqrt(Rbjf*Rbjf+a6*a6)) +
     2                  a8(1)*Fn + a9(1)*Fr
      else
         pgaref = a1(1) + a7*(mag-c1) + a3(1)*(8.5-mag)**2.0 +
     1                 (a4(1)+a5*(mag-c1))*alog(sqrt(Rbjf*Rbjf+a6*a6)) +
     2                  a8(1)*Fn + a9(1)*Fr
      endif
      pgaref = exp(pgaref)
C.....Now compute the ground motion value........
      if (mag .lt. c1 ) then
         lnY = a1T + a2T*(mag-c1) + a3T*(8.5-mag)**2.0 +
     1                 (a4T+a5*(mag-c1))*alog(sqrt(Rbjf*Rbjf+a6*a6)) +
     2                  a8T*Fn + a9T*Fr
      else
         lnY = a1T + a7*(mag-c1) + a3T*(8.5-mag)**2.0 +
     1                 (a4T+a5*(mag-c1))*alog(sqrt(Rbjf*Rbjf+a6*a6)) +
     2                  a8T*Fn + a9T*Fr
      endif
C.....Now apply site amplification term......
      if (vs .le. 750.0) then
         lnY = lnY + b1T*alog(Vs/750.0) +
     1         b2T*alog( (pgaref + c*(Vs/750.0)**n) / ((pgaref+c)*(Vs/750.0)**n) )
      else
         lnY = lnY + b1T*alog( min(Vs,1000.0)/750.0)
      endif
C.....Set Sigma value.........
      sigma = sqrt (phiT*phiT + tauT*tauT)
C     Convert ground motion to units of gals.
      lnY = lnY + 6.89
      period2 = period1
      return
      END
