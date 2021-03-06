c ------------------------------------------------------------------
C *** Adjusted Lin and Lee (2008) Horizontal for Subduction Zones, ****
c ------------------------------------------------------------------

      subroutine S04_LL08_C02 ( mag, rupdist, specT, period, lnY, sigma,
     1  iflag, Ztor, ftype, vs30)

      implicit none

      integer MAXPER, nPer, i
      parameter (MAXPER=23)
      real mag, rupDist, lnY, sigma, period
      real ftype, vs30, Ztor, eq1, eq2
      real c1(MAXPER), c2(MAXPER), c3(MAXPER), c6(MAXPER),
     1     c8(MAXPER), c9(MAXPER), c10(MAXPER), c11(MAXPER)
      real specT, c1T, c2T, c3T, c4, c5, c6T, c7, c8T, c9T, c10T, c11T, sigT
      integer count1, count2, iflag
      real period1(MAXPER), sig(MAXPER), tau(MAXPER)

      Data period1 / 0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.075, 0.1, 0.12, 0.15, 0.17, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 1, 1.5, 2, 3,
     1      4, 5/
      Data c1 / -2.67215141251388, -2.71177472200959, -2.72221832187676, -2.39784018709008, -2.03538207060753, -1.9088731533554,
     1      -1.51924693067264, -1.22860451814747, -1.46548753025297, -1.60073705472033, -1.73022355739054, -1.92135557019542,
     1      -2.39746675094267, -3.1626558324922, -3.92684084497968, -4.66300148921766, -5.70953049123902, -7.1524588205603,
     1      -8.48237255072763, -9.52408882517922, -11.1234179760534, -12.2807829729212, -13.0556468090273/
      Data c2 / 1.205, 1.205, 1.2, 1.155, 1.1, 1.09, 1.04023471290541, 1, 1.04, 1.045, 1.065, 1.085, 1.12512345582376, 1.215,
     1      1.285, 1.365, 1.465, 1.62, 1.705, 1.77, 1.83, 1.845, 1.805/
      Data c3 / -1.905, -1.895, -1.88, -1.875, -1.86, -1.855, -1.82624150716967, -1.795, -1.77, -1.73, -1.71, -1.675,
     1      -1.61902356955068, -1.57, -1.5, -1.465, -1.45, -1.45, -1.44, -1.43, -1.37, -1.26, -1.135/
      Data c6 / 0.0156102889995883, 0.0155937815787548, 0.0153748969231095, 0.0151405607461744, 0.0148129562446649,
     1      0.0146316205990692, 0.0154910951819304, 0.0168002641290514, 0.0170361616812206, 0.017589045794533, 0.0173841662544598,
     1      0.0169136478032877, 0.0146367531026353, 0.0133391252630137, 0.0130416388541367, 0.0132968472446285, 0.0110158907384788,
     1      0.00840352664801909, 0.0052779340533018, 0.00391178925933505, -0.000827499686994609, -0.00149591256410688,
     1      0.00017541935878931/
      Data c8 / -0.0307330537090917, -0.0298343520996223, -0.0274126868068662, -0.0364602430745523, -0.0407807117320402,
     1      -0.031390816808513, -0.0287137244543388, -0.0351687921689559, -0.0323399626797574, -0.0437727054144336,
     1      -0.0498562436473804, -0.0639911797350464, -0.0669593104826402, -0.0635239987476825, -0.0947684446353348,
     1      -0.114035873731828, -0.148789496848836, -0.133337325622367, -0.139809507698123, -0.129201814425729,
     1      -0.0715007654732053, -0.0372707415321892, -0.000597669002690569/
      Data c9 / 0.0266593626202029, 0.0266334030802883, 0.0269669701408332, 0.0280072960151164, 0.0289904354524744,
     1      0.030604317861146, 0.0332015363157863, 0.0354619001161029, 0.0372753772673137, 0.0357195049604629, 0.0351815190940847,
     1      0.0338209794345063, 0.0339195582609039, 0.0320745613914819, 0.0267583477942808, 0.0213670091589494, 0.0111478707446052,
     1      0.00553780059760793, -0.0016119522124433, -0.00691562108364793, -0.0176232269444349, -0.0263408475728589,
     1      -0.0281361511687443/
      Data c10 / -0.000949647771928277, -0.00101200055609113, -0.00110255232367069, -0.0011326533367276, -0.00109413518804539,
     1      -0.00100199972311216, -0.00090996785184189, -0.00131476815827683, -0.00193723592589072, -0.00227333008442422,
     1      -0.00287937008272209, -0.00340654996752039, -0.00332677600001939, -0.00365410072006342, -0.00477028778490143,
     1      -0.00558825116399655, -0.00552557581642036, -0.00489580988712914, -0.00302787737429278, -0.00260229160594944,
     1      -0.000675577853054268, -0.00107307865445684, -0.00190354645418688/
      Data c11 / -0.359741188020741, -0.358470777504636, -0.34775073846162, -0.315351550224917, -0.254330134095103,
     1      -0.193789527625835, -0.123352653335684, -0.148748667363803, -0.211818594094982, -0.295503232941766, -0.332260362282402,
     1      -0.362579617763326, -0.406482874390383, -0.460061886261643, -0.589573384248183, -0.668328275187303, -0.828947940168923,
     1      -0.929268961596905, -0.915372798229036, -0.901918483623071, -0.851893512362305, -0.771271120200326, -0.707883845911145/
      Data tau / 0.362709384160633, 0.362228781621561, 0.360765115235053, 0.364305472555706, 0.369410732844507, 0.371386260224396,
     1      0.375711844802132, 0.378373296413057, 0.375256347952233, 0.370246674428955, 0.371237804386007, 0.365428318168295,
     1      0.380040605508848, 0.389791614996634, 0.402000747210632, 0.41366610274721, 0.42388131060612, 0.451342780155734,
     1      0.454659536359888, 0.473606264026091, 0.470378444115232, 0.470153829776495, 0.364032908959872/
      Data sig / 0.488925125190048, 0.488832360888311, 0.488325414244525, 0.491514552262097, 0.504342137392063,
     1      0.524922820418443, 0.54658776104583, 0.539020452290274, 0.532707465059637, 0.520338086029458, 0.528640131814135,
     1      0.52886057587926, 0.526673481453196, 0.527190629182218, 0.537849220431881, 0.542923431321146, 0.533182979055384,
     1      0.520721584299849, 0.532634760118896, 0.54532659397375, 0.53310130534746, 0.49937482519724, 0.453578088334651/

C Find the requested spectral period and corresponding coefficients
      nPer = 23

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period  = period1(1)
         c1T     = c1(1)
         c2T     = c2(1)
         c3T     = c3(1)
         c6T     = c6(1)
         c8T     = c8(1)
         c9T     = c9(1)
         c10T    = c10(1)
         c11T    = c11(1)
         sigT    = sig(1)
         goto 1011
      elseif (specT .ne. 0.0) then
C Now loop over the spectral period range of the attenuation relationship.
         do i=2,nper-1
            if (specT .ge. period1(i) .and. specT .le. period1(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1020
            endif
         enddo
      endif

C      write (*,*)
C      write (*,*) 'Lin and Lee (2008) Sub-Hor. Adjusted atttenuation model'
C      write (*,*) 'is not defined for a spectral period of: '
C      write (*,*)') ' Period = ',specT
C      write (*,*) 'This spectral period is outside the defined'
C      write (*,*) 'period range in the code or beyond the range'
C      write (*,*) 'of spectral periods for interpolation.'
C      write (*,*) 'Please check the input file.'
C      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period1(count1),period1(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call S24_interp (period1(count1),period1(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call S24_interp (period1(count1),period1(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call S24_interp (period1(count1),period1(count2),c6(count1),c6(count2),
     +             specT,c6T,iflag)
      call S24_interp (period1(count1),period1(count2),c8(count1),c8(count2),
     +             specT,c8T,iflag)
      call S24_interp (period1(count1),period1(count2),c9(count1),c9(count2),
     +             specT,c9T,iflag)
      call S24_interp (period1(count1),period1(count2),c10(count1),c10(count2),
     +             specT,c10T,iflag)
      call S24_interp (period1(count1),period1(count2),c11(count1),c11(count2),
     +             specT,c11T,iflag)
      call S24_interp (period1(count1),period1(count2),sig(count1),sig(count2),
     +             specT,sigT,iflag)

 1011 period = specT

C     Compute the ground motions.
      c4 = 0.51552
      c5 = 0.63255
      c7 = 0.275

      if (mag .lt. 7.2) then
      	eq1 = c8T*(mag - 7.2)**2.0
      else
        eq1 = 0
      endif

      if (ftype .eq. 0) then
      	eq2 = c9T*(min(Ztor, 30.0) - 20.0)
      else
        eq2 = c6T*(min(Ztor, 80.0) - 20.0)
      endif

      lnY = c1T + c2T*mag + eq1 + c3T*alog(Rupdist+c4*exp(c5*mag)) +
     1      c7*ftype + eq2 + c10T*Rupdist + c11T*alog(Vs30/760.0)


      sigma = sigT

C     Now convert to Ln Units in gals.
      lnY = lnY + 6.89

      return
      end
