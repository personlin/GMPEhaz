c ------------------------------------------------------------------
C *** Abrahamson, Silva, and Kamai (NGA-West2 2013) Horizontal ****
C     Earthquake Spectra Paper:
C        Summary of the Abrahamson, Silva, and Kamai NGA-West2
C            Ground-Motion Relations for Active Crustal REgions
C         N. A. Abrahamson, S. J. Silva, and R. Kamai
C     Notes:
C        Applicable Range (see Abstract):
C           3 <= M <= 8.5
C           Rrup <= 300 km
C        Regional attenuation included based on Regionflag
C             0 = Global
C             1 = Taiwan
C             2 = China
C             3 = Japan
C         Mainshock and Aftershocks included based on MSASFlag
C             0 = Mainshocks
C             1 = Aftershocks
C         Sigma dependent on estimated or measured Vs30m based on
C             Vs30_Class
C             0 = Estimated Vs30m
C             1 = Measured Vs30m
c ------------------------------------------------------------------
      subroutine ASK14_TW_B01 ( mag, dip, fType, fltWidth, rRup, Rjb,
     1                     vs30, hwflag, lnY, sigma, specT, period2, ztor,
     2                     iflag, vs30_class, z10, Rx, Ry0, regionflag, msasflag,
     1                     phi, tau )

C     Last Updated: 8/1/13

      implicit none

      real mag, dip, fType, rRup, rjb, Rx, Ry0, vs30, SA1180,
     1      Z10,  ZTOR, fltWidth, lnSa, sigma, lnY, vs30_rock
      real Fn, Frv, specT, period2, CRjb, phi, tau, z10_rock, SA_rock
      integer hwflag, iflag, vs30_class, regionflag, msasflag

c     Vs30 class is to distinguish between the sigma if the Vs30 is measured
c     vs the VS30 being estimated from surface geology.
c         Vs30_class = 0 for estimated
c         Vs30_class = 1 for measured

C     Current version is not programmed for Aftershock cases.
C       For implementation of Aftershock a new distance metric, CRjb
C       will need to be computed and passed along to this subroutine.

      CRjb = 999.9

C     Set mechanism term and corresponding Frv and Fnm values.
C     fType     Mechanism                      Rake
C     ------------------------------------------------------
C      -1       Normal                   -120 < Rake < -60.0
C     1, 0.5    Reverse and Rev/Obl        30 < Rake < 150.0
C     0,-0.5    Strike-Slip and NMl/Obl        Otherwise
C
      if ( fType .eq. 1.0 ) then
        Frv = 1.0
        Fn = 0.0
      elseif ( fType .eq. 0.5 ) then
        Frv = 1.0
        Fn = 0.0
      elseif ( fType .eq. -1.0 ) then
        Frv = 0.0
        Fn = 1.0
      elseif ( fType .eq. -0.5 ) then
        Frv = 0.0
        Fn = 1.0
      else
        Frv = 0.0
        Fn = 0.0
      endif

c     Compute SA1180
      vs30_rock = 1180.
      z10_rock = 0.005
      SA_rock = 0.

      call ASK14_B01_model ( mag, dip, fltWidth, ZTOR, Frv, Fn, rRup, rjb, rx, Ry0,
     1                     vs30_rock, SA_rock, Z10_rock, hwflag, vs30_class,
     2                     specT, lnSa, phi, tau, iflag, regionflag, msasflag, CRjb )
      Sa1180 = exp(lnSa)

c     Compute Sa at spectral period for given Vs30

      call ASK14_B01_model ( mag, dip, fltWidth, ZTOR, Frv, Fn, rRup, rjb, rx, Ry0,
     1                     vs30, SA1180, Z10, hwflag, vs30_class,
     2                     specT, lnSa, phi, tau, iflag, regionflag, msasflag, CRjb )

c     compute Sa (given the PGA rock value)
      sigma = sqrt( phi**2 + tau**2 )

      lnY = lnSa + 6.89

      period2 = specT

      return
      end

c ----------------------------------------------------------------------
      subroutine ASK14_B01_model ( mag, dip, FltWidth, ZTOR, Frv, Fn, rRup, rjb, Rx, Ry0,
     1                     vs30, Sa1180, Z1, hwflag, vs30_class,
     3                     specT, lnSa, phi, tau, iflag, regionflag, msasflag, CRjb)

      implicit none

      integer MAXPER
      parameter (MAXPER=25)
      real Vlin(MAXPER), b(MAXPER), c4(MAXPER), M1(MAXPER), a1(MAXPER)
      real a2(MAXPER), a3(MAXPER), a6(MAXPER), a8(MAXPER), a10(MAXPER)
      real a11(MAXPER), a12(MAXPER), a13(MAXPER), a14(MAXPER), a15(MAXPER)
      real a17(MAXPER), a43(MAXPER), a44(MAXPER), a45(MAXPER), a46(MAXPER)
      real a25(MAXPER), a28(MAXPER), a29(MAXPER), a31(MAXPER), a36(MAXPER)
      real a37(MAXPER), a38(MAXPER), a39(MAXPER), a40(MAXPER), a41(MAXPER), a42(MAXPER)
      real s1est(MAXPER), s2est(MAXPER), s1msr(MAXPER), s2msr(MAXPER)
      real s3(MAXPER), s4(MAXPER), s5(MAXPER), s6(MAXPER), period(MAXPER)
      real a4, a5, a7
      real VlinT, bT, c4T, M1T, a1T
      real a2T, a3T, a6T, a8T, a10T
      real a11T, a12T, a13T, a14T, a15T
      real a17T, a43T, a44T, a45T, a46T
      real a25T, a28T, a29T, a31T, a36T
      real a37T, a38T, a39T, a40T, a41T, a42T
      real s1estT, s2estT, s1msrT, s2msrT
      real s3T, s4T, s5T, s6T, c4_mag
      real phiA_est, phiA_msr, period1

      real M2
      real lnSa, SA1180, rjb, rRup, Rx, Ry0, dip, mag, vs30
      real HW_taper1, HW_taper2, HW_taper3, HW_taper4, HW_taper5
      real damp_dSA1180, sigAmp, fltWidth
      real f1, f4, f5, f6, f7, f8, f10, f11, fReg, f12, f13
      real Ry1, ZTOR, Frv, Fn, SpecT
      real phiA, phiB, tauA, tauB, phi, tau
      integer vs30_class, hwflag, iflag, nPer, regionflag, msasflag
      real n, c, z1, z1_ref
      real R, V1, Vs30Star, hw_a2, h1, h2, h3, R1, R2, CRjb
      integer count1, count2, i
      real y1, y2, x1, x2, y1z, y2z, x1z, x2z

      Data Period / 0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.075, 0.1, 0.12, 0.15, 0.17, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 1, 1.5, 2, 3, 4,
     1      5, 7.5, 10/
      Data Vlin / 660, 660, 680, 770, 851.659765220817, 915, 960, 910, 833.557751246246, 740, 674.738820243143, 590, 495, 430, 360,
     1      340, 330, 330, 330, 330, 330, 330, 330, 330, 330/
      Data b / -1.47, -1.47, -1.459, -1.39, -1.2936977941189, -1.219, -1.152, -1.23, -1.39052872238288, -1.587, -1.77190667597776,
     1      -2.012, -2.411, -2.757, -3.278, -3.599, -3.8, -3.5, -2.4, -1, 0, 0, 0, 0, 0/
      Data c4 / 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5,
     1      4.5, 4.5/
      Data M1 / 6.75, 6.75, 6.75, 6.75, 6.75, 6.75, 6.75, 6.75, 6.75, 6.75, 6.75, 6.75, 6.75, 6.75, 6.75, 6.75, 6.75, 6.75, 6.75,
     1      6.75, 6.82, 6.92, 7, 7.15, 7.25/
      Data a1 / 0.460600015947598, 0.460600015947598, 0.476662106166965, 0.516356479943452, 0.582997428008059, 0.673401376351624,
     1      0.939833356415817, 1.15295044660591, 1.25906272139998, 1.36294125779516, 1.39821224799824, 1.4317191368953,
     1      1.45628835446733, 1.50605491274014, 1.48389909602472, 1.40581161372063, 1.26000362736812, 1.18529912386658,
     1      0.852066092515722, 0.608019648356634, 0.138688555892916, -0.246609533511117, -0.573914070587723, -1.51350640711318,
     1      -2.21956139507878/
      Data a2 / -0.79, -0.79, -0.79, -0.79, -0.79, -0.79, -0.79, -0.79, -0.79, -0.79, -0.79, -0.79, -0.79, -0.79, -0.79, -0.79,
     1      -0.79, -0.79, -0.79, -0.79, -0.79, -0.79, -0.765, -0.634, -0.529/
      Data a3 / 0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275,
     1      0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275/
      Data a6 / 1.04382972101078, 1.04382972101078, 1.02287214548525, 0.981975489982761, 0.948808059901789, 0.909330014068919,
     1      0.972169593733245, 1.05557322799513, 1.13653221880988, 1.25976647465002, 1.29612078263542, 1.36030503192109,
     1      1.45322775409603, 1.58717622193611, 1.69671823629637, 1.80905947357388, 2.11892351731598, 2.31355443857186,
     1      2.54793423212086, 2.61001676851972, 2.53274336468555, 2.58826880577492, 2.74433250160649, 2.05213979898605,
     1      0.97228887233494/
      Data a8 / -0.015, -0.015, -0.015, -0.015, -0.015, -0.015, -0.015, -0.015, -0.0181476220075075, -0.022, -0.0254805962536991,
     1      -0.03, -0.038, -0.045, -0.055, -0.065, -0.095, -0.11, -0.124, -0.138, -0.172, -0.197, -0.218, -0.255, -0.285/
      Data a10 / 1.88760420665271, 1.88760420665271, 1.87412607945034, 1.77021066195251, 1.63027502864501, 1.52970337121143,
     1      1.43965733823951, 1.50652841304197, 1.64975403393965, 1.83389689041139, 2.07830619631674, 2.42006513453817,
     1      3.02548649285898, 3.33281963780803, 4.10603506049399, 4.61238519039654, 4.80382975592394, 4.25873786170594,
     1      2.56118779571086, 0.538481557191184, -0.823380018971046, -0.774735728446903, -0.74246133108878, -0.796748496427044,
     1      -0.820990067221777/
      Data a11 / 0.0714814544570367, 0.0714814544570367, 0.0703668128643129, 0.0621660313761019, 0.0547349596204059,
     1      0.0421249177059982, 0.00330530380523494, -0.0188165421341981, -0.0139557701718006, 0.00307062228096418,
     1      0.0217030553805566, 0.0608625704970314, 0.102418766774058, 0.110929952943118, 0.142756872509893, 0.161686021057101,
     1      0.18113241747276, 0.190274607877094, 0.172387840294143, 0.154246988496069, 0.154022511066368, 0.0900956940718857,
     1      0.0670069373541323, -0.0288644588832292, 0.0264041937161747/
      Data a12 / -0.115654547562219, -0.115654547562219, -0.112971643845927, -0.111665943340747, -0.0963194775446628,
     1      -0.0996047087594245, -0.120114595932674, -0.150593000937059, -0.133836129998175, -0.105797759778213,
     1      -0.0845319579753518, -0.0813918890711925, -0.0623838706477978, -0.073881211166395, -0.12694242711065,
     1      -0.179656135311729, -0.20625170322026, -0.248551644046309, -0.236708347917836, -0.260026211393963, -0.127398147327714,
     1      -0.0228582036637432, -0.285907006815184, -0.260456592646886, -0.333771995993637/
      Data a13 / 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.58, 0.56, 0.53, 0.5, 0.42, 0.35, 0.2, 0,
     1      0, 0, 0/
      Data a14 / -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.24, -0.19, -0.11, -0.04, 0.07, 0.15,
     1      0.27, 0.35, 0.46, 0.54, 0.61, 0.72, 0.8/
      Data a15 / 1.2600796788578, 1.2600796788578, 1.27231168765251, 1.33314438420622, 1.41592299412524, 1.48724977573478,
     1      1.58860697964575, 1.59206475484041, 1.56808880915916, 1.48722031567164, 1.44832429480125, 1.35278984371433,
     1      1.22559545356214, 1.17266993973974, 1.05619563177918, 0.941440728136341, 0.82384449277897, 0.78529391975452,
     1      0.624427790352517, 0.504540162657426, 0.139171469785648, -0.0716265267413403, -0.236663066699732, -0.410588481665379,
     1      -0.610386078908438/
      Data a17 / -0.00681810782649117, -0.00681810782649117, -0.00694077568881603, -0.00764083781382915, -0.00825171996713634,
     1      -0.0089136467309953, -0.00961513632936185, -0.00927096574105986, -0.00887450975169667, -0.00787087003363749,
     1      -0.00719678244720775, -0.00613376978224785, -0.00497241050187858, -0.00423107178000768, -0.00295937686178231,
     1      -0.00219385217572701, -0.00221430390281479, -0.00304946526724964, -0.00286077432487558, -0.00335330548767638,
     1      -0.00190922051421141, -0.00064966449977727, 1.30491447498493e-05, -0.00047989542823035, -0.000937718531437706/
      Data a25 / -0.0015, -0.0015, -0.0015, -0.0016, -0.00182526831785053, -0.002, -0.0027, -0.0033, -0.00338993205735736, -0.0035,
     1      -0.00341298509365752, -0.0033, -0.0029, -0.0027, -0.0023, -0.002, -0.001, -5e-04, -4e-04, -2e-04, 0, 0, 0, 0, 0/
      Data a28 / 0.0025, 0.0025, 0.0024, 0.0023, 0.00252526831785053, 0.0027, 0.0032, 0.0036, 0.00346510191396396, 0.0033,
     1      0.00303895528097257, 0.0027, 0.0024, 0.002, 0.001, 8e-04, 7e-04, 7e-04, 6e-04, 3e-04, 0, 0, 0, 0, 0/
      Data a29 / -0.0034, -0.0034, -0.0033, -0.0034, -0.00334368292053737, -0.0033, -0.0029, -0.0025, -0.0025, -0.0025,
     1      -0.00276104471902743, -0.0031, -0.0036, -0.0039, -0.0048, -0.005, -0.0041, -0.0032, -0.002, -0.0017, -0.002, -0.002,
     1      -0.002, -0.002, -0.002/
      Data a31 / -0.1503, -0.1503, -0.1479, -0.1447, -0.137885633385021, -0.1326, -0.1353, -0.1128, -0.0448563306665158, 0.0383,
     1      0.0553549216431254, 0.0775, 0.0741, 0.2548, 0.2136, 0.1542, 0.0787, 0.0476, -0.0163, -0.1203, -0.2719, -0.2958,
     1      -0.2718, -0.14, -0.0216/
      Data a36 / 0.265, 0.265, 0.255, 0.249, 0.222530972652563, 0.202, 0.126, 0.022, -0.049046325312313, -0.136,
     1      -0.110765677160682, -0.078, 0.037, -0.091, 0.129, 0.31, 0.505, 0.358, 0.131, 0.123, 0.109, 0.135, 0.189, 0.15, 0.092/
      Data a37 / 0.337, 0.337, 0.328, 0.32, 0.302541705366584, 0.289, 0.275, 0.256, 0.213731933042042, 0.162, 0.188974620966168,
     1      0.224, 0.248, 0.203, 0.232, 0.252, 0.208, 0.208, 0.108, 0.068, -0.023, 0.028, 0.031, -0.07, -0.159/
      Data a38 / 0.188, 0.188, 0.184, 0.18, 0.172678779669858, 0.167, 0.173, 0.189, 0.15257751677027, 0.108, 0.111045521721987,
     1      0.115, 0.122, 0.096, 0.123, 0.134, 0.129, 0.152, 0.118, 0.119, 0.093, 0.084, 0.058, 0, -0.05/
      Data a39 / 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      Data a40 / 0.088, 0.088, 0.088, 0.093, 0.115526831785053, 0.133, 0.186, 0.16, 0.118631253615615, 0.068, 0.0592985093657524,
     1      0.048, 0.055, 0.073, 0.143, 0.16, 0.158, 0.145, 0.131, 0.083, 0.07, 0.101, 0.095, 0.151, 0.124/
      Data a41 / -0.196, -0.196, -0.194, -0.175, -0.127130482456762, -0.09, 0.09, 0.006, -0.0668449664594602, -0.156,
     1      -0.207338794742061, -0.274, -0.248, -0.203, -0.154, -0.159, -0.141, -0.144, -0.126, -0.075, -0.021, 0.072, 0.205,
     1      0.329, 0.301/
      Data a42 / 0.044, 0.044, 0.061, 0.162, 0.324756359647008, 0.451, 0.506, 0.335, 0.146592339836334, -0.084, -0.124897005980964,
     1      -0.178, -0.187, -0.159, -0.023, -0.029, 0.061, 0.062, 0.037, -0.143, -0.028, -0.097, 0.015, 0.299, 0.243/
      Data a43 / -0.141230487015009, -0.141230487015009, -0.134515963786534, -0.138560604373142, -0.131192890399172,
     1      -0.0956449642815338, -0.0185022993039339, -0.0102530863035501, -0.0331746681857694, -0.122212040958011,
     1      -0.175382580194237, -0.159010562480714, -0.147508045193972, -0.142764335594514, -0.189905087581847, -0.19827298271952,
     1      -0.211969094664238, -0.205893579757544, -0.0810284308780251, -0.0626840665123873, -0.0143116879862188,
     1      0.0515113374390205, 0.0518541387764282, 0.166661327731793, 0.178251209014419/
      Data a44 / 0.169978317788829, 0.169978317788829, 0.166233389692709, 0.17060969457144, 0.177534287066833, 0.185050025877725,
     1      0.210626598115492, 0.266554284785693, 0.249843628803871, 0.247607129355676, 0.239553961417542, 0.198344368961663,
     1      0.167168003636265, 0.160663618082816, 0.147934261048807, 0.112672170314989, 0.0522223878593706, 0.0316290712249328,
     1      0.0665660666488076, 0.15413279338904, 0.22819663058287, 0.207059825925631, 0.197414655187954, 0.169617860591678,
     1      0.102226599921671/
      Data a45 / -0.00907818186414659, -0.00907818186414659, -0.0107748604248121, -0.00840835342638762, 0.00113016885387654,
     1      -0.000838723488002492, -0.0155478500364931, -0.0291072956865892, -0.0408876115907172, -0.0491216487885166,
     1      -0.0443385097403965, -0.034018221470587, -0.0300295251201835, -0.0237891777689791, 0.0061185356265915,
     1      0.0592233737896679, 0.154041800088925, 0.196017860810411, 0.249321775521198, 0.237732453339578, 0.216761798336893,
     1      0.175210435748798, 0.139474659863123, 0.073192256077724, 0.0738674169574279/
      Data a46 / 0.0507824199272957, 0.0507824199272957, 0.0489946133733854, 0.0422153915077698, 0.00470782632322055,
     1      -0.0400634575154817, -0.0611491348845388, -0.0659008782588018, -0.0355007461069409, 0.00657279516061872,
     1      0.0250084590847953, 0.0385727746828305, 0.0844169336158826, 0.142861112278088, 0.210504331857529, 0.257925741425559,
     1      0.29678046535131, 0.309285620933313, 0.275413624294667, 0.298189097153747, 0.2759976108614, 0.251001775002206,
     1      0.266135553229375, 0.226867963213929, 0.189523999083254/
      Data s1est / 0.754, 0.754, 0.76, 0.781, 0.797331953044163, 0.81, 0.81, 0.81, 0.805953057418919, 0.801, 0.795779105619451,
     1      0.789, 0.77, 0.74, 0.699, 0.676, 0.631, 0.609, 0.578, 0.555, 0.548, 0.527, 0.505, 0.457, 0.429/
      Data s2est / 0.52, 0.52, 0.52, 0.52, 0.525631707946263, 0.53, 0.54, 0.55, 0.554496602867868, 0.56, 0.562175372658562, 0.565,
     1      0.57, 0.58, 0.59, 0.6, 0.615, 0.63, 0.64, 0.65, 0.64, 0.63, 0.63, 0.63, 0.63/
      Data s1msr / 0.741, 0.741, 0.747, 0.769, 0.785331953044163, 0.798, 0.798, 0.795, 0.785107473690691, 0.773, 0.764298509365752,
     1      0.753, 0.729, 0.693, 0.644, 0.616, 0.566, 0.541, 0.506, 0.48, 0.472, 0.447, 0.425, 0.378, 0.359/
      Data s2msr / 0.501, 0.501, 0.501, 0.501, 0.50719487874089, 0.512, 0.522, 0.527, 0.523402717705706, 0.519, 0.516824627341438,
     1      0.514, 0.513, 0.519, 0.524, 0.532, 0.548, 0.565, 0.576, 0.587, 0.576, 0.565, 0.568, 0.575, 0.585/
      Data s3 / 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47,
     1      0.47, 0.47, 0.47, 0.47, 0.47, 0.47/
      Data s4 / 0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36,
     1      0.36, 0.36, 0.36, 0.36, 0.36, 0.36/
      Data s5 / 0.54, 0.54, 0.54, 0.55, 0.555631707946263, 0.56, 0.57, 0.57, 0.574496602867868, 0.58, 0.584350745317124, 0.59,
     1      0.61, 0.63, 0.66, 0.69, 0.73, 0.77, 0.8, 0.8, 0.8, 0.76, 0.72, 0.67, 0.64/
      Data s6 / 0.63, 0.63, 0.63, 0.63, 0.641263415892527, 0.65, 0.69, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.69, 0.68,
     1      0.66, 0.62, 0.55, 0.52, 0.5, 0.5, 0.5/



C Find the requested spectral period and corresponding coefficients
      nPer = 25

C First check for the PGA, PGV, PGD cases
      if (specT .eq. 0.0) then
         period1 = period(1)
         a1T = a1(1)
         a2T = a2(1)
         a3T = a3(1)
         a6T = a6(1)
         a8T = a8(1)
         M1T = M1(1)
         a10T = a10(1)
         a11T = a11(1)
         a12T = a12(1)
         a13T = a13(1)
         a14T = a14(1)
         a15T = a15(1)
         a17T = a17(1)
         a43T = a43(1)
         a44T = a44(1)
         a45T = a45(1)
         a46T = a46(1)
         a25T = a25(1)
         a28T = a28(1)
         a29T = a29(1)
         a31T = a31(1)
         a36T = a36(1)
         a37T = a37(1)
         a38T = a38(1)
         a39T = a39(1)
         a40T = a40(1)
         a41T = a41(1)
         a42T = a42(1)
         VlinT = Vlin(1)
         bT = b(1)
         c4T = c4(1)
         s1estT = s1est(1)
         s2estT = s2est(1)
         s1msrT = s1msr(1)
         s2msrT = s2msr(1)
         s3T = s3(1)
         s4T = s4(1)
         s5T = s5(1)
         s6T = s6(1)
         goto 1011
      elseif (specT .eq. -1.0) then
         period1 = period(2)
         a1T = a1(2)
         a2T = a2(2)
         a3T = a3(2)
         a6T = a6(2)
         a8T = a8(2)
         M1T = M1(2)
         a10T = a10(2)
         a11T = a11(2)
         a12T = a12(2)
         a13T = a13(2)
         a14T = a14(2)
         a15T = a15(2)
         a17T = a17(2)
         a43T = a43(2)
         a44T = a44(2)
         a45T = a45(2)
         a46T = a46(2)
         a25T = a25(2)
         a28T = a28(2)
         a29T = a29(2)
         a31T = a31(2)
         a36T = a36(2)
         a37T = a37(2)
         a38T = a38(2)
         a39T = a39(2)
         a40T = a40(2)
         a41T = a41(2)
         a42T = a42(2)
         VlinT = Vlin(2)
         bT = b(2)
         c4T = c4(2)
         s1estT = s1est(2)
         s2estT = s2est(2)
         s1msrT = s1msr(2)
         s2msrT = s2msr(2)
         s3T = s3(2)
         s4T = s4(2)
         s5T = s5(2)
         s6T = s6(2)
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
      write (*,*) 'Abrahamson, Silva, and Kamai (NGA West2-2013) Horizontal'
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
            call interp (period(count1),period(count2),a6(count1),a6(count2),
     +                   specT,a6T,iflag)
            call interp (period(count1),period(count2),a8(count1),a8(count2),
     +                   specT,a8T,iflag)
            call interp (period(count1),period(count2),M1(count1),M1(count2),
     +                   specT,M1T,iflag)
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
            call interp (period(count1),period(count2),a17(count1),a17(count2),
     +                   specT,a17T,iflag)
            call interp (period(count1),period(count2),a43(count1),a43(count2),
     +                   specT,a43T,iflag)
            call interp (period(count1),period(count2),a44(count1),a44(count2),
     +                   specT,a44T,iflag)
            call interp (period(count1),period(count2),a45(count1),a45(count2),
     +                   specT,a45T,iflag)
            call interp (period(count1),period(count2),a46(count1),a46(count2),
     +                   specT,a46T,iflag)
            call interp (period(count1),period(count2),a25(count1),a25(count2),
     +                   specT,a25T,iflag)
            call interp (period(count1),period(count2),a28(count1),a28(count2),
     +                   specT,a28T,iflag)
            call interp (period(count1),period(count2),a29(count1),a29(count2),
     +                   specT,a29T,iflag)
            call interp (period(count1),period(count2),a31(count1),a31(count2),
     +                   specT,a31T,iflag)
            call interp (period(count1),period(count2),a36(count1),a36(count2),
     +                   specT,a36T,iflag)
            call interp (period(count1),period(count2),a37(count1),a37(count2),
     +                   specT,a37T,iflag)
            call interp (period(count1),period(count2),a38(count1),a38(count2),
     +                   specT,a38T,iflag)
            call interp (period(count1),period(count2),a39(count1),a39(count2),
     +                   specT,a39T,iflag)
            call interp (period(count1),period(count2),a40(count1),a40(count2),
     +                   specT,a40T,iflag)
            call interp (period(count1),period(count2),a41(count1),a41(count2),
     +                   specT,a41T,iflag)
            call interp (period(count1),period(count2),a42(count1),a42(count2),
     +                   specT,a42T,iflag)
            call interp (period(count1),period(count2),Vlin(count1),Vlin(count2),
     +                   specT,VlinT,iflag)
            call interp (period(count1),period(count2),b(count1),b(count2),
     +                   specT,bT,iflag)
            call interp (period(count1),period(count2),c4(count1),c4(count2),
     +                   specT,c4T,iflag)
            call interp (period(count1),period(count2),s1est(count1),s1est(count2),
     +                   specT,s1estT,iflag)
            call interp (period(count1),period(count2),s2est(count1),s2est(count2),
     +                   specT,s2estT,iflag)
            call interp (period(count1),period(count2),s1msr(count1),s1msr(count2),
     +                   specT,s1msrT,iflag)
            call interp (period(count1),period(count2),s2msr(count1),s2msr(count2),
     +                   specT,s2msrT,iflag)
            call interp (period(count1),period(count2),s3(count1),s3(count2),
     +                   specT,s3T,iflag)
            call interp (period(count1),period(count2),s4(count1),s4(count2),
     +                   specT,s4T,iflag)
            call interp (period(count1),period(count2),s5(count1),s5(count2),
     +                   specT,s5T,iflag)
            call interp (period(count1),period(count2),s6(count1),s6(count2),
     +                   specT,s6T,iflag)

 1011 period1 = specT

C     Constant values
      n = 1.5
      M2 = 5.0
      a4 = -0.1
      a5 = -0.41
      a7 = 0.0

C     Set C term
      if (period1 .eq. -1.0) then
         c = 2400.0
      else
         c = 2.4
      endif

C     Magnitude dependent taper for C4 (eq. 4.4)
      if (mag .ge. 5.0) then
         c4_mag = c4T
      elseif (mag .ge. 4.0) then
         c4_mag = c4T - (c4T-1.0) * (5.0-mag)
      else
         c4_mag = 1.0
      endif

c     Set distance (eq 4.3)
      R = sqrt(rRup**2 + c4_mag**2)

C     Base Model (eq 4.2)
      if ( mag .lt. M2 ) then
        f1 = a1T + a6T*(Mag-M2) + a7*(Mag-M2)**2 + a4*(M2-M1T) + a8T*(8.5-M2)**2 +
     1                (a2T + a3T*(M2-M1T)) * alog(R) + a17T*Rrup
      elseif ( mag .lt. M1T ) then
        f1 = a1T + a4*(Mag-M1T) + a8T*(8.5-Mag)**2 + (a2T + a3T*(Mag-M1T)) * alog(R) + a17T*Rrup
      else
        f1 = a1T + a5*(Mag-M1T) + a8T*(8.5-Mag)**2 + (a2T + a3T*(Mag-M1T)) * alog(R) + a17T*Rrup
      endif

c     style of faulting (eq 4.5 and 4.6)
      if ( mag .lt. 4. ) then
        f7 = 0
        f8 = 0
      elseif ( mag .le. 5. ) then
        f7 = Frv * a11T * (mag-4.)
        f8 = Fn * a12T * (mag-4.)
      else
        f7 = Frv * a11T
        f8 = Fn * a12T
      endif

c     ZTOR (eq 14)
c     form modified"Extend the upper bound ZTOR to 50km:"
      if (ZTOR .le. 50.) then
        f6 = a15T * ZTOR/50.0
      else
        f6 = a15T
      endif

c     Set VS30_star (eq 4.8 and 4.9)
      if ( specT .ge. 3.0 ) then
        V1 = 800.
      elseif ( specT .gt. 0.5 ) then
        V1 = exp( -0.35 * alog(specT/0.5)  + alog(1500.) )
      else
        V1=1500.
      endif
      if ( vs30 .lt. v1 ) then
         vs30Star = vs30
      else
	    vs30Star = v1
      endif

c     Compute site amplification (Eq. 4.7)
      if (vs30 .lt. vLinT) then
        f5 = a10T*alog(vs30Star/vLinT) - bT*alog(c+Sa1180)
     1              + bT*alog(Sa1180+c*((vs30Star/vLinT)**(n)) )
      else
     	f5 = (a10T + bT*n) * alog(vs30Star/vLinT)
      endif

c     Set Regional z1 reference (eq 4.18)
      if (regionflag .eq. 1) then
         z1_ref =  exp(-2.63 / 4.0 * alog((vs30**4.0 + 253.0**4.0)/(2492.0**4.0 + 253.0**4.0)))/ 1000.
      elseif (regionflag .eq. 3) then
         z1_ref = exp ( -5.23/2. * alog( (Vs30**2.0 + 412.**2.0)/(1360.**2.0+412.**2.0) ) ) / 1000.
      else
         z1_ref = exp ( -7.67/4. * alog( (Vs30**4.0 + 610.**4.0)/(1360.**4.0+610.**4.0) ) ) / 1000.
      endif

C     Soil Depth Model (eq 4.17)
C     Updated 8/1/13
      if ( vs30 .le. 150.0 ) then
         y1z = a43T
         y2z = a43T
         x1z = 50.0
         x2z = 150.0
      elseif ( vs30 .le. 250.0 ) then
         y1z = a43T
         y2z = a44T
         x1z = 150.0
         x2z = 250.0
      elseif ( vs30 .le. 400.0 ) then
         y1z = a44T
         y2z = a45T
         x1z = 250.0
         x2z = 400.0
      elseif ( vs30 .le. 700.0 ) then
         y1z = a45T
         y2z = a46T
         x1z = 400.0
         x2z = 700.0
      else
         y1z = a46T
         y2z = a46T
         x1z = 700.0
         x2z = 1000.0
      endif
C     Calculation f10 term and set it equal to zero for Vs=1180m/s (i.e., reference condition)
      if (vs30 .eq. 1180.0) then
          f10 = 0.0
      else
         f10 = ( y1z + (Vs30-x1z)*(y2z-y1z)/(x2z-x1z))*alog( (z1 + 0.01) / (z1_ref+0.01) )
      endif

c     Compute HW taper1 (eq 4.11)
      if ( dip .le. 30. ) then
        HW_taper1 = 60./ 45.
      else
        HW_taper1 = (90.-dip)/45.
      endif

c     Compute HW taper2 (eq. 4.12)
      hw_a2 = 0.2
      if( mag .gt. 6.5 ) then
        HW_taper2 = 1. + hw_a2 * (mag-6.5)
      elseif ( mag .gt. 5.5 ) then
        HW_taper2 = 1. + HW_a2 * (mag-6.5) - (1.0 - HW_a2)*(mag-6.5)**2
      else
        HW_taper2 = 0.
      endif

c     Compute HW taper 3 (eq. 4.13)
C	  April 11, correction by ronnie for HW_taper3 when Rx.gt.R2
      h1 = 0.25
      h2 = 1.5
      h3 = -0.75
      R1 = fltWidth * cos(dip*3.1415926/180.)
      R2 = 3.*R1
      if ( Rx .le. R1 ) then
        HW_taper3 = h1 + h2*(Rx/R1) + h3*(Rx/R1)**2
      elseif ( Rx . lt. R2 ) then
        HW_taper3 = 1. - (Rx-R1)/(R2-R1)
      else
        HW_taper3 = 0.
      endif

c     Compute HW taper 4 (eq 4.14)
      if ( ZTOR .lt. 10. ) then
        HW_taper4 = 1. - (ZTOR**2) / 100.
      else
        HW_taper4 = 0.
      endif

c     Compute HW taper 5 (eq. 13)  **** Ry0 version ***
      Ry1 = Rx * tan(20.*3.1415926/180.)
      if ( Ry0 .lt. Ry1 ) then
        HW_taper5 = 1.
      elseif ( Ry0-Ry1 .lt. 5. ) then
        HW_taper5 = 1. - (Ry0-Ry1) / 5.
      else
        HW_taper5 = 0.
      endif

c     Compute HW taper 5 (eq. 4.15b)  **** No Ry0 version ***
c      if (Rjb .eq. 0. ) then
c        HW_taper5 = 1.
c      elseif ( Rjb .lt. 30. ) then
c        HW_taper5 = 1 - Rjb/30.
c      else
c        HW_taper5 = 0.
c      endif

c     Hanging wall Model (eq 4.10)
      if ( HWFlag .eq. 1 ) then
        f4 = a13T * HW_taper1 * HW_taper2 * HW_taper3 * HW_taper4 * HW_taper5
      else
        f4 = 0.
      endif

C     Add aftershock factor (eq 4.21)
      if (msasflag .eq. 1) then
         if (CRjb .gt. 15.0) then
             f11 = 0.0
         elseif (CRjb .le. 5.0) then
             f11 = a14T
         else
             f11 = a14T * ( 1.0 - (CRjb - 5.0) /10.0)
         endif
      elseif (msasflag .eq. 0) then
         f11 = 0.0
      endif

C     Now apply the regional attenuation differences (eq 4.22)
C     Global No Change
      if (regionflag .eq. 0) then
         freg = 0.0
C     Taiwan
      elseif (regionflag .eq. 1) then
         f12 = a31T * alog(Vs30star/VlinT)
         freg = f12 + a25T*Rrup
C     China
      elseif (regionflag .eq. 2) then
         freg = a28T*Rrup
C     Japan
      elseif (regionflag .eq. 3) then
         if (vs30 .lt. 150.0) then
             y1 = a36T
             y2 = a36T
             x1 = 50.0
             x2 = 150.0
         elseif (vs30 .lt. 250.0) then
             y1 = a36T
             y2 = a37T
             x1 = 150.0
             x2 = 250.0
         elseif (vs30 .lt. 350.0) then
             y1 = a37T
             y2 = a38T
             x1 = 250.0
             x2 = 350.0
         elseif (vs30 .lt. 450.0) then
             y1 = a38T
             y2 = a39T
             x1 = 350.0
             x2 = 450.0
         elseif (vs30 .lt. 600.0) then
             y1 = a39T
             y2 = a40T
             x1 = 450.0
             x2 = 600.0
         elseif (vs30 .lt. 850.0) then
             y1 = a40T
             y2 = a41T
             x1 = 600.0
             x2 = 850.0
         elseif (vs30 .lt. 1150.0) then
             y1 = a41T
             y2 = a42T
             x1 = 850.0
             x2 = 1150.0
         else
             y1 = a42T
             y2 = a42T
             x1 = 1150.0
             x2 = 3000.0
         endif
         f13 = y1 + (y2-y1)/(x2-x1) * (vs30-x1)
         freg = f13 + a29T*Rrup
      endif

C     Set the Sigma Values
      if (regionflag .ne. 3)  then
c     Compute within-event term, phiA, at the surface for linear site response (eq 7.1)
        if (mag .lt. 4.0) then
           phiA_est = s1estT
        elseif (mag .le. 6.0) then
           phiA_est = s1estT + ((s2estT-s1estT)/2.0)*(mag-4.0)
        else
           phiA_est = s2estT
        endif

c     Compute within-event term, phiA, for known Vs30
        if (mag .lt. 4.0) then
           phiA_msr = s1msrT
        elseif (mag .le. 6.0) then
           phiA_msr = s1msrT + ((s2msrT-s1msrT)/2.0)*(mag-4.0)
        else
           phiA_msr = s2msrT
        endif

C     choose phiA by Vs30 class
        if (vs30_class .eq. 0 ) then
	     phiA = phiA_est
        elseif (vs30_class .eq. 1) then
	     phiA = phiA_msr
        else
	     stop 99
        endif

C     Set Sigma values for Japan Region
      else

C calculate phi_A for Japan (eq. 7.3)
        if (Rrup .lt. 30) then
           phiA = s5T
        elseif (Rrup .le. 80) then
           phiA = s5T + (s6T-s5T)/50*(Rrup-30)
        else
           phiA = s6T
        endif
      endif

c     Compute between-event term, tau (eq. 7.2)
      if (mag .lt. 5.0) then
         tauA = s3T
      elseif (mag .le. 7.0) then
         tauA = s3T + ((s4T-s3T)/2.0)*(mag-5.0)
      else
         tauA = s4T
      endif
      tauB = tauA

c     Compute phiB, within-event term with site amp variability removed (eq. 7.7)
c     with fix to model for small mag at long periods - limit sigAmp to be less than phiA
      sigAmp = 0.4
      if (phiA .le. sigAmp) then
        sigAmp = phiA*0.99
      endif
      phiB = sqrt( phiA**2 - sigAmp**2)

c     Compute partial derivative of alog(soil amp) w.r.t. alog(Sa1180) (eq. 7.10)
      if ( vs30 .ge. vLinT) then
        dAmp_dSa1180 = 0.
      else
        dAmp_dSa1180 = bT*Sa1180 * ( -1. / (Sa1180+c)
     1              + 1./ (Sa1180 + c*(vs30/vLinT)**(n)) )
      endif

C     Compute phi, with non-linear effects (eq. 7.8)
      phi = sqrt( phiB**2 * (1. + dAmp_dSa1180)**2 + sigAmp**2 )

C     Compute tau, with non-linear effects (eq. 7.9)
      tau = tauB * (1. + dAmp_dSa1180)

c     Compute median ground motion (eq. 1)
      lnSa = f1 + f4 + f5 + f6 + f7 + f8 + f10 + f11 + freg

      return
      end







c ------------------------------------------------------------------
C *** Abrahamson, Silva, and Kamai (NGA-West2 2013) Horizontal ****
C     Earthquake Spectra Paper:
C        Summary of the Abrahamson, Silva, and Kamai NGA-West2
C            Ground-Motion Relations for Active Crustal REgions
C         N. A. Abrahamson, S. J. Silva, and R. Kamai
C     Notes:
C        Applicable Range (see Abstract):
C           3 <= M <= 8.5
C           Rrup <= 300 km
C        Regional attenuation included based on Regionflag
C             0 = Global
C             1 = Taiwan
C             2 = China
C             3 = Japan
C         Mainshock and Aftershocks included based on MSASFlag
C             0 = Mainshocks
C             1 = Aftershocks
C         Sigma dependent on estimated or measured Vs30m based on
C             Vs30_Class
C             0 = Estimated Vs30m
C             1 = Measured Vs30m
c ------------------------------------------------------------------
      subroutine ASK14_TW_C01 ( mag, dip, fType, fltWidth, rRup, Rjb,
     1                     vs30, hwflag, lnY, sigma, specT, period2, ztor,
     2                     iflag, vs30_class, z10, Rx, Ry0, regionflag, msasflag,
     1                     phi, tau )
C     Last Updated: 8/1/13
      implicit none

      real mag, dip, fType, rRup, rjb, Rx, Ry0, vs30, SA1180,
     1      Z10,  ZTOR, fltWidth, lnSa, sigma, lnY, vs30_rock
      real Fn, Frv, specT, period2, CRjb, phi, tau, z10_rock, SA_rock
      integer hwflag, iflag, vs30_class, regionflag, msasflag

c     Vs30 class is to distinguish between the sigma if the Vs30 is measured
c     vs the VS30 being estimated from surface geology.
c         Vs30_class = 0 for estimated
c         Vs30_class = 1 for measured

C     Current version is not programmed for Aftershock cases.
C       For implementation of Aftershock a new distance metric, CRjb
C       will need to be computed and passed along to this subroutine.

      CRjb = 999.9

C     Set mechanism term and corresponding Frv and Fnm values.
C     fType     Mechanism                      Rake
C     ------------------------------------------------------
C      -1       Normal                   -120 < Rake < -60.0
C     1, 0.5    Reverse and Rev/Obl        30 < Rake < 150.0
C     0,-0.5    Strike-Slip and NMl/Obl        Otherwise
C
      if ( fType .eq. 1.0 ) then
        Frv = 1.0
        Fn = 0.0
      elseif ( fType .eq. 0.5 ) then
        Frv = 1.0
        Fn = 0.0
      elseif ( fType .eq. -1.0 ) then
        Frv = 0.0
        Fn = 1.0
      elseif ( fType .eq. -0.5 ) then
        Frv = 0.0
        Fn = 1.0
      else
        Frv = 0.0
        Fn = 0.0
      endif

c     Compute SA1180
      vs30_rock = 1180.
      z10_rock = 0.005
      SA_rock = 0.

      call ASK14_TW_C01_model ( mag, dip, fltWidth, ZTOR, Frv, Fn, rRup, rjb, rx, Ry0,
     1                     vs30_rock, SA_rock, Z10_rock, hwflag, vs30_class,
     2                     specT, lnSa, phi, tau, iflag, regionflag, msasflag, CRjb )
      Sa1180 = exp(lnSa)

c     Compute Sa at spectral period for given Vs30

      call ASK14_TW_C01_model ( mag, dip, fltWidth, ZTOR, Frv, Fn, rRup, rjb, rx, Ry0,
     1                     vs30, SA1180, Z10, hwflag, vs30_class,
     2                     specT, lnSa, phi, tau, iflag, regionflag, msasflag, CRjb )

c     compute Sa (given the PGA rock value)
      sigma = sqrt( phi**2 + tau**2 )

      lnY = lnSa + 6.89

      period2 = specT

      return
      end

c ----------------------------------------------------------------------
      subroutine ASK14_TW_C01_model ( mag, dip, FltWidth, ZTOR, Frv, Fn, rRup, rjb, Rx, Ry0,
     1                     vs30, Sa1180, Z1, hwflag, vs30_class,
     3                     specT, lnSa, phi, tau, iflag, regionflag, msasflag, CRjb)

      implicit none

      integer MAXPER
      parameter (MAXPER=25)
      real Vlin(MAXPER), b(MAXPER), c4(MAXPER), M1(MAXPER), a1(MAXPER)
      real a2(MAXPER), a3(MAXPER), a6(MAXPER), a8(MAXPER), a10(MAXPER)
      real a11(MAXPER), a12(MAXPER), a13(MAXPER), a14(MAXPER), a15(MAXPER)
      real a17(MAXPER), a43(MAXPER), a44(MAXPER), a45(MAXPER), a46(MAXPER)
      real a25(MAXPER), a28(MAXPER), a29(MAXPER), a31(MAXPER), a36(MAXPER)
      real a37(MAXPER), a38(MAXPER), a39(MAXPER), a40(MAXPER), a41(MAXPER), a42(MAXPER)
      real s1est(MAXPER), s2est(MAXPER), s1msr(MAXPER), s2msr(MAXPER)
      real s3(MAXPER), s4(MAXPER), s5(MAXPER), s6(MAXPER), period(MAXPER)
      real a4(MAXPER), a5, a7
      real VlinT, bT, c4T, M1T, a1T
      real a2T, a3T, a6T, a8T, a10T, a4T
      real a11T, a12T, a13T, a14T, a15T
      real a17T, a43T, a44T, a45T, a46T
      real a25T, a28T, a29T, a31T, a36T
      real a37T, a38T, a39T, a40T, a41T, a42T
      real s1estT, s2estT, s1msrT, s2msrT
      real s3T, s4T, s5T, s6T, c4_mag
      real phiA_est, phiA_msr, period1

      real M2
      real lnSa, SA1180, rjb, rRup, Rx, Ry0, dip, mag, vs30
      real HW_taper1, HW_taper2, HW_taper3, HW_taper4, HW_taper5
      real damp_dSA1180, sigAmp, fltWidth
      real f1, f4, f5, f6, f7, f8, f10, f11, fReg, f12, f13
      real Ry1, ZTOR, Frv, Fn, SpecT
      real phiA, phiB, tauA, tauB, phi, tau
      integer vs30_class, hwflag, iflag, nPer, regionflag, msasflag
      real n, c, z1, z1_ref
      real R, V1, Vs30Star, hw_a2, h1, h2, h3, R1, R2, CRjb
      integer count1, count2, i
      real y1, y2, x1, x2, y1z, y2z, x1z, x2z


      Data Period(1:25) / 0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.075, 0.1, 0.12, 0.15, 0.17, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 1, 1.5, 2,
     1      3, 4, 5, 7.5, 10/
      Data Vlin(1:25) / 660, 660, 680, 770, 851.659765220817, 915, 960, 910, 833.557751246246, 740, 674.738820243143, 590, 495,
     1      430, 360, 340, 330, 330, 330, 330, 330, 330, 330, 330, 330/
      Data b(1:25) / -1.47, -1.47, -1.459, -1.39, -1.2936977941189, -1.219, -1.152, -1.23, -1.39052872238288, -1.587,
     1      -1.77190667597776, -2.012, -2.411, -2.757, -3.278, -3.599, -3.8, -3.5, -2.4, -1, 0, 0, 0, 0, 0/
      Data c4(1:25) / 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5,
     1      4.5, 4.5, 4.5/
      Data M1(1:25) / 6.75, 6.75, 6.75, 6.75, 6.75, 6.75, 6.75, 6.75, 6.75, 6.75, 6.75, 6.75, 6.75, 6.75, 6.75, 6.75, 6.75, 6.75,
     1      6.75, 6.75, 6.82, 6.92, 7, 7.15, 7.25/
      Data a1(1:25) / 0.593866860165754, 0.593866860165754, 0.607350205626504, 0.623596721976998, 0.652048054162018,
     1      0.706900122976899, 0.977092520718666, 1.19353371909298, 1.32002501982816, 1.41943704493427, 1.45760409041355,
     1      1.48593774481291, 1.5482692399218, 1.59530501168755, 1.52577976900992, 1.42330982305614, 1.28852744087696,
     1      1.21763682762334, 0.926314398721161, 0.752132501511978, 0.306567902675289, -0.0776771579300297, -0.336929967399733,
     1      -1.3124274646718, -1.96852544897435/
      Data a2(1:25) / -0.79, -0.79, -0.79, -0.79, -0.79, -0.79, -0.79, -0.79, -0.79, -0.79, -0.79, -0.79, -0.79, -0.79, -0.79,
     1      -0.79, -0.79, -0.79, -0.79, -0.79, -0.79, -0.79, -0.765, -0.634, -0.529/
      Data a3(1:25) / 0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275,
     1      0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275, 0.275/
      Data a4(1:25) / 0.028559622, 0.028559622, 0.024114472, 0.029331155, 0.019548333, 0.004946714, 0.005303378, -0.005794728,
     1       -0.013326944, -0.023008825, -0.033586465, -0.066345205, -0.057650858, -0.029948392, -0.002088881, 0.0203011,
     1       0.031432073, 0.135117619, 0.263006151, 0.363764714, 0.299931624, 0.171388832, 0.125319644, 0.000172165, 0.001611885/
      Data a6(1:25) / 1.0971052478214, 1.0971052478214, 1.07034252625442, 1.01191011946962, 0.960151214531469, 0.895279929309867,
     1      0.92654506977541, 1.00221368734431, 1.08991983886901, 1.22240081684137, 1.29468215932786, 1.40361090204586,
     1      1.51069901081663, 1.63441282368804, 1.74891808838416, 1.86175559265212, 2.18640072401506, 2.42168821321506,
     1      2.68100338021949, 2.76037541026386, 2.74674409095451, 2.81072195031604, 2.96079416196568, 2.54253432152637,
     1      1.22631618381355/
      Data a8(1:25) / -0.015, -0.015, -0.015, -0.015, -0.015, -0.015, -0.015, -0.015, -0.0181476220075075, -0.022,
     1      -0.0254805962536991, -0.03, -0.038, -0.045, -0.055, -0.065, -0.095, -0.11, -0.124, -0.138, -0.172, -0.197, -0.218,
     1      -0.255, -0.285/
      Data a10(1:25) / 1.88550206478838, 1.88550206478838, 1.87041425559732, 1.75625542618031, 1.60356687034528, 1.49217324726696,
     1      1.39247214525391, 1.4779307664696, 1.62803769217999, 1.82902654690301, 2.07844213755755, 2.42387695711654,
     1      3.05112199129081, 3.36681275006325, 4.14386296817676, 4.64423228581923, 4.84231546795258, 4.30714161619116,
     1      2.63511186125103, 0.617331058415303, -0.758430542317734, -0.737742033861981, -0.713264155594585, -0.777366444748309,
     1      -0.79623295261254/
      Data a11(1:25) / 0.0865290017628497, 0.0865290017628497, 0.0863920308121283, 0.0833475966285011, 0.0805940005440329,
     1      0.0739970397632909, 0.0317420642709333, 0.00950566126829064, 0.0278573208474259, 0.0617341652642051,
     1      0.0765627168872937, 0.100927646749515, 0.133863284021228, 0.158502746654088, 0.174755835638996, 0.189203321064511,
     1      0.196322660498526, 0.194565992382258, 0.163916257091526, 0.138191034588059, 0.119417922134445, 0.0518858982566291,
     1      -0.000299209835122006, -0.126816643183004, -0.0952237645759153/
      Data a12(1:25) / -0.0851813014732252, -0.0851813014732252, -0.0811946125443235, -0.0736733480467424, -0.0447695469303866,
     1      -0.0326016592728057, -0.057299086049061, -0.109198803849594, -0.0892438233690082, -0.0570349718601036,
     1      -0.0425580668821486, -0.0512274722133986, -0.0561371570080197, -0.0617139982746329, -0.091740938717726,
     1      -0.151914306681818, -0.178531415315115, -0.210716153796932, -0.216476851197754, -0.22661557721623, -0.0950274132257672,
     1      0.129608160368009, -0.154555139232628, 0.0114922876378795, -0.0640609795492863/
      Data a13(1:25) / 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.58, 0.56, 0.53, 0.5, 0.42, 0.35,
     1      0.2, 0, 0, 0, 0/
      Data a14(1:25) / -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.24, -0.19, -0.11, -0.04, 0.07,
     1      0.15, 0.27, 0.35, 0.46, 0.54, 0.61, 0.72, 0.8/
      Data a15(1:25) / 1.35951090458584, 1.35951090458584, 1.36654581947039, 1.40869323733483, 1.48532549730173, 1.57986239181509,
     1      1.739195456708, 1.79908761029304, 1.77441289606063, 1.69212932619259, 1.672990391914, 1.55489999718983,
     1      1.39852777161035, 1.3162179619873, 1.2259862575015, 1.11569501263915, 0.991158857090293, 0.958925297374564,
     1      0.810318882076326, 0.769601371751858, 0.378047007927471, 0.227924124763207, -0.0330462634934479, -0.0979718229415627,
     1      -0.181661927158766/
      Data a17(1:25) / -0.00781955144257678, -0.00781955144257678, -0.00794388960832398, -0.00861792272478135,
     1      -0.00918743653244077, -0.0098216355840628, -0.010534120645979, -0.0103669729589076, -0.0100175235859223,
     1      -0.00894850816177607, -0.008255299022226, -0.00709893304178311, -0.00581931156279194, -0.00505808157463984,
     1      -0.00368321099920911, -0.0028063952303506, -0.00289623937801703, -0.00387904788105345, -0.00384632217298883,
     1      -0.00493839857385291, -0.00347153307789942, -0.00226353012210327, -0.00179670583741962, -0.00252889897970746,
     1      -0.00310008394168946/
      Data a25(1:25) / -0.0015, -0.0015, -0.0015, -0.0016, -0.00182526831785053, -0.002, -0.0027, -0.0033, -0.00338993205735736,
     1      -0.0035, -0.00341298509365752, -0.0033, -0.0029, -0.0027, -0.0023, -0.002, -0.001, -5e-04, -4e-04, -2e-04, 0, 0, 0, 0,
     1      0/
      Data a28(1:25) / 0.0025, 0.0025, 0.0024, 0.0023, 0.00252526831785053, 0.0027, 0.0032, 0.0036, 0.00346510191396396, 0.0033,
     1      0.00303895528097257, 0.0027, 0.0024, 0.002, 0.001, 8e-04, 7e-04, 7e-04, 6e-04, 3e-04, 0, 0, 0, 0, 0/
      Data a29(1:25) / -0.0034, -0.0034, -0.0033, -0.0034, -0.00334368292053737, -0.0033, -0.0029, -0.0025, -0.0025, -0.0025,
     1      -0.00276104471902743, -0.0031, -0.0036, -0.0039, -0.0048, -0.005, -0.0041, -0.0032, -0.002, -0.0017, -0.002, -0.002,
     1      -0.002, -0.002, -0.002/
      Data a31(1:25) / -0.1503, -0.1503, -0.1479, -0.1447, -0.137885633385021, -0.1326, -0.1353, -0.1128, -0.0448563306665158,
     1      0.0383, 0.0553549216431254, 0.0775, 0.0741, 0.2548, 0.2136, 0.1542, 0.0787, 0.0476, -0.0163, -0.1203, -0.2719, -0.2958,
     1      -0.2718, -0.14, -0.0216/
      Data a36(1:25) / 0.265, 0.265, 0.255, 0.249, 0.222530972652563, 0.202, 0.126, 0.022, -0.049046325312313, -0.136,
     1      -0.110765677160682, -0.078, 0.037, -0.091, 0.129, 0.31, 0.505, 0.358, 0.131, 0.123, 0.109, 0.135, 0.189, 0.15, 0.092/
      Data a37(1:25) / 0.337, 0.337, 0.328, 0.32, 0.302541705366584, 0.289, 0.275, 0.256, 0.213731933042042, 0.162,
     1      0.188974620966168, 0.224, 0.248, 0.203, 0.232, 0.252, 0.208, 0.208, 0.108, 0.068, -0.023, 0.028, 0.031, -0.07, -0.159/
      Data a38(1:25) / 0.188, 0.188, 0.184, 0.18, 0.172678779669858, 0.167, 0.173, 0.189, 0.15257751677027, 0.108,
     1      0.111045521721987, 0.115, 0.122, 0.096, 0.123, 0.134, 0.129, 0.152, 0.118, 0.119, 0.093, 0.084, 0.058, 0, -0.05/
      Data a39(1:25) / 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      Data a40(1:25) / 0.088, 0.088, 0.088, 0.093, 0.115526831785053, 0.133, 0.186, 0.16, 0.118631253615615, 0.068,
     1      0.0592985093657524, 0.048, 0.055, 0.073, 0.143, 0.16, 0.158, 0.145, 0.131, 0.083, 0.07, 0.101, 0.095, 0.151, 0.124/
      Data a41(1:25) / -0.196, -0.196, -0.194, -0.175, -0.127130482456762, -0.09, 0.09, 0.006, -0.0668449664594602, -0.156,
     1      -0.207338794742061, -0.274, -0.248, -0.203, -0.154, -0.159, -0.141, -0.144, -0.126, -0.075, -0.021, 0.072, 0.205,
     1      0.329, 0.301/
      Data a42(1:25) / 0.044, 0.044, 0.061, 0.162, 0.324756359647008, 0.451, 0.506, 0.335, 0.146592339836334, -0.084,
     1      -0.124897005980964, -0.178, -0.187, -0.159, -0.023, -0.029, 0.061, 0.062, 0.037, -0.143, -0.028, -0.097, 0.015, 0.299,
     1      0.243/
      Data a43(1:25) / -0.122962023498924, -0.122962023498924, -0.120092998613746, -0.126886260792928, -0.126035664024335,
     1      -0.0904729506991718, -0.0149049072273536, 0.0103808659653804, -0.0154811955589853, -0.0841622352036735,
     1      -0.119282978541206, -0.12421065727408, -0.107040732978698, -0.0960315277316025, -0.157890828883787, -0.19410442396108,
     1      -0.211688638835866, -0.182842209269601, 0.0357796738204077, 0.0978715683661098, 0.180188769627975, 0.267208253179032,
     1      0.288512616632419, 0.336857781708233, 0.402190602744037/
      Data a44(1:25) / 0.172338591249922, 0.172338591249922, 0.170022426697282, 0.17246825329291, 0.177160008120209,
     1      0.183647781047614, 0.188806700354148, 0.243698862114109, 0.222677566880558, 0.237785176023185, 0.237613022368251,
     1      0.218308740069599, 0.2032746665851, 0.181666749213636, 0.161804828013144, 0.122191096979769, 0.0662625613004855,
     1      0.0475907780093519, 0.069351375345784, 0.129000718045731, 0.190504182454582, 0.170342502732787, 0.157276582209913,
     1      0.137031837000311, 0.0589963511005088/
      Data a45(1:25) / 0.0614738216205322, 0.0614738216205322, 0.0583202285935086, 0.0619929094404484, 0.0716204505668039,
     1      0.0735921825736064, 0.0738034667488054, 0.064417927749121, 0.0598856791348016, 0.0357966722878199, 0.0346058994769634,
     1      0.0318550184537276, 0.0222552236580935, 0.023693014881416, 0.0490897682923249, 0.105075768222844, 0.180265627533596,
     1      0.222035554590515, 0.260100426376672, 0.254045901077365, 0.236586467752453, 0.214938951003774, 0.178100653589404,
     1      0.12514948661591, 0.116411079489916/
      Data a46(1:25) / 0.0626052993621598, 0.0626052993621598, 0.058744653649071, 0.0544910136649466, 0.0302800958064618,
     1      0.00511890031399667, -0.0298335598698967, -0.0480238700565291, -0.0326498810238118, 0.0142571550229835,
     1      0.0402025828982244, 0.0551568774847881, 0.0972265261342537, 0.151626103452716, 0.233058972791216, 0.265871303338564,
     1      0.297582627738386, 0.289181720276543, 0.235785512188508, 0.27699299739719, 0.249507530382689, 0.219942760963378,
     1      0.254640765867001, 0.162535435459097, 0.0900289349548069/
      Data s1est(1:25) / 0.754, 0.754, 0.76, 0.781, 0.797331953044163, 0.81, 0.81, 0.81, 0.805953057418919, 0.801,
     1      0.795779105619451, 0.789, 0.77, 0.74, 0.699, 0.676, 0.631, 0.609, 0.578, 0.555, 0.548, 0.527, 0.505, 0.457, 0.429/
      Data s2est(1:25) / 0.52, 0.52, 0.52, 0.52, 0.525631707946263, 0.53, 0.54, 0.55, 0.554496602867868, 0.56, 0.562175372658562,
     1      0.565, 0.57, 0.58, 0.59, 0.6, 0.615, 0.63, 0.64, 0.65, 0.64, 0.63, 0.63, 0.63, 0.63/
      Data s1msr(1:25) / 0.741, 0.741, 0.747, 0.769, 0.785331953044163, 0.798, 0.798, 0.795, 0.785107473690691, 0.773,
     1      0.764298509365752, 0.753, 0.729, 0.693, 0.644, 0.616, 0.566, 0.541, 0.506, 0.48, 0.472, 0.447, 0.425, 0.378, 0.359/
      Data s2msr(1:25) / 0.501, 0.501, 0.501, 0.501, 0.50719487874089, 0.512, 0.522, 0.527, 0.523402717705706, 0.519,
     1      0.516824627341438, 0.514, 0.513, 0.519, 0.524, 0.532, 0.548, 0.565, 0.576, 0.587, 0.576, 0.565, 0.568, 0.575, 0.585/
      Data s3(1:25) / 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47,
     1      0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47/
      Data s4(1:25) / 0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36,
     1      0.36, 0.36, 0.36, 0.36, 0.36, 0.36, 0.36/
      Data s5(1:25) / 0.54, 0.54, 0.54, 0.55, 0.555631707946263, 0.56, 0.57, 0.57, 0.574496602867868, 0.58, 0.584350745317124,
     1      0.59, 0.61, 0.63, 0.66, 0.69, 0.73, 0.77, 0.8, 0.8, 0.8, 0.76, 0.72, 0.67, 0.64/
      Data s6(1:25) / 0.63, 0.63, 0.63, 0.63, 0.641263415892527, 0.65, 0.69, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.69,
     1      0.68, 0.66, 0.62, 0.55, 0.52, 0.5, 0.5, 0.5/


C Find the requested spectral period and corresponding coefficients
      nPer = 25

C First check for the PGA, PGV, PGD cases
      if (specT .eq. 0.0) then
         period1 = period(1)
         a1T = a1(1)
         a2T = a2(1)
         a3T = a3(1)
         a4T = a4(1)
         a6T = a6(1)
         a8T = a8(1)
         M1T = M1(1)
         a10T = a10(1)
         a11T = a11(1)
         a12T = a12(1)
         a13T = a13(1)
         a14T = a14(1)
         a15T = a15(1)
         a17T = a17(1)
         a43T = a43(1)
         a44T = a44(1)
         a45T = a45(1)
         a46T = a46(1)
         a25T = a25(1)
         a28T = a28(1)
         a29T = a29(1)
         a31T = a31(1)
         a36T = a36(1)
         a37T = a37(1)
         a38T = a38(1)
         a39T = a39(1)
         a40T = a40(1)
         a41T = a41(1)
         a42T = a42(1)
         VlinT = Vlin(1)
         bT = b(1)
         c4T = c4(1)
         s1estT = s1est(1)
         s2estT = s2est(1)
         s1msrT = s1msr(1)
         s2msrT = s2msr(1)
         s3T = s3(1)
         s4T = s4(1)
         s5T = s5(1)
         s6T = s6(1)
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
      write (*,*) 'Abrahamson, Silva, and Kamai (NGA West2-2013) Horizontal'
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
            call interp (period(count1),period(count2),a6(count1),a6(count2),
     +                   specT,a6T,iflag)
            call interp (period(count1),period(count2),a8(count1),a8(count2),
     +                   specT,a8T,iflag)
            call interp (period(count1),period(count2),M1(count1),M1(count2),
     +                   specT,M1T,iflag)
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
            call interp (period(count1),period(count2),a17(count1),a17(count2),
     +                   specT,a17T,iflag)
            call interp (period(count1),period(count2),a43(count1),a43(count2),
     +                   specT,a43T,iflag)
            call interp (period(count1),period(count2),a44(count1),a44(count2),
     +                   specT,a44T,iflag)
            call interp (period(count1),period(count2),a45(count1),a45(count2),
     +                   specT,a45T,iflag)
            call interp (period(count1),period(count2),a46(count1),a46(count2),
     +                   specT,a46T,iflag)
            call interp (period(count1),period(count2),a25(count1),a25(count2),
     +                   specT,a25T,iflag)
            call interp (period(count1),period(count2),a28(count1),a28(count2),
     +                   specT,a28T,iflag)
            call interp (period(count1),period(count2),a29(count1),a29(count2),
     +                   specT,a29T,iflag)
            call interp (period(count1),period(count2),a31(count1),a31(count2),
     +                   specT,a31T,iflag)
            call interp (period(count1),period(count2),a36(count1),a36(count2),
     +                   specT,a36T,iflag)
            call interp (period(count1),period(count2),a37(count1),a37(count2),
     +                   specT,a37T,iflag)
            call interp (period(count1),period(count2),a38(count1),a38(count2),
     +                   specT,a38T,iflag)
            call interp (period(count1),period(count2),a39(count1),a39(count2),
     +                   specT,a39T,iflag)
            call interp (period(count1),period(count2),a40(count1),a40(count2),
     +                   specT,a40T,iflag)
            call interp (period(count1),period(count2),a41(count1),a41(count2),
     +                   specT,a41T,iflag)
            call interp (period(count1),period(count2),a42(count1),a42(count2),
     +                   specT,a42T,iflag)
            call interp (period(count1),period(count2),Vlin(count1),Vlin(count2),
     +                   specT,VlinT,iflag)
            call interp (period(count1),period(count2),b(count1),b(count2),
     +                   specT,bT,iflag)
            call interp (period(count1),period(count2),c4(count1),c4(count2),
     +                   specT,c4T,iflag)
            call interp (period(count1),period(count2),s1est(count1),s1est(count2),
     +                   specT,s1estT,iflag)
            call interp (period(count1),period(count2),s2est(count1),s2est(count2),
     +                   specT,s2estT,iflag)
            call interp (period(count1),period(count2),s1msr(count1),s1msr(count2),
     +                   specT,s1msrT,iflag)
            call interp (period(count1),period(count2),s2msr(count1),s2msr(count2),
     +                   specT,s2msrT,iflag)
            call interp (period(count1),period(count2),s3(count1),s3(count2),
     +                   specT,s3T,iflag)
            call interp (period(count1),period(count2),s4(count1),s4(count2),
     +                   specT,s4T,iflag)
            call interp (period(count1),period(count2),s5(count1),s5(count2),
     +                   specT,s5T,iflag)
            call interp (period(count1),period(count2),s6(count1),s6(count2),
     +                   specT,s6T,iflag)
 1011 period1 = specT
C     Constant values
      n = 1.5
      M2 = 5.0
c      a4 = -0.1
      a5 = -0.41
      a7 = 0.0
C     Set C term
      if (period1 .eq. -1.0) then
         c = 2400.0
      else
         c = 2.4
      endif
C     Magnitude dependent taper for C4 (eq. 4.4)
      if (mag .ge. 5.0) then
         c4_mag = c4T
      elseif (mag .ge. 4.0) then
         c4_mag = c4T - (c4T-1.0) * (5.0-mag)
      else
         c4_mag = 1.0
      endif

c     Set distance (eq 4.3)
      R = sqrt(rRup**2 + c4_mag**2)

C     Base Model (eq 4.2)
      if ( mag .lt. M2 ) then
        f1 = a1T + a6T*(Mag-M2) + a7*(Mag-M2)**2 + a4T*(M2-M1T) + a8T*(8.5-M2)**2 +
     1                (a2T + a3T*(M2-M1T)) * alog(R) + a17T*Rrup
      elseif ( mag .le. M1T ) then
        f1 = a1T + a4T*(Mag-M1T) + a8T*(8.5-Mag)**2 + (a2T + a3T*(Mag-M1T)) * alog(R) + a17T*Rrup
      else
        f1 = a1T + a5*(Mag-M1T) + a8T*(8.5-Mag)**2 + (a2T + a3T*(Mag-M1T)) * alog(R) + a17T*Rrup
      endif

c     style of faulting (eq 4.5 and 4.6)
      if ( mag .gt. 5. ) then
        f7 = Frv * a11T
        f8 = Fn * a12T
      elseif ( mag .ge. 4. ) then
        f7 = Frv * a11T * (mag-4.)
        f8 = Fn * a12T * (mag-4.)
      else
        f7 = 0
        f8 = 0
      endif
c     ZTOR (eq 14)
c     form modified"Extend the upper bound ZTOR to 50km:"
      if (ZTOR .lt. 50.) then
        f6 = a15T * ZTOR/50.0
      else
        f6 = a15T
      endif
c     Set VS30_star (eq 4.8 and 4.9)
      if ( specT .gt. 3.0 ) then
        V1 = 800.
      elseif ( specT .gt. 0.5 ) then
        V1 = exp( -0.35 * alog(specT/0.5)  + alog(1500.) )
      else
        V1=1500.
      endif
      if ( vs30 .lt. v1 ) then
         vs30Star = vs30
      else
	     vs30Star = v1
      endif
c     Compute site amplification (Eq. 4.7)
      if (vs30 .lt. vLinT) then
        f5 = a10T*alog(vs30Star/vLinT) - bT*alog(c+Sa1180)
     1              + bT*alog(Sa1180+c*((vs30Star/vLinT)**(n)) )
      else
     	f5 = (a10T + bT*n) * alog(vs30Star/vLinT)
      endif
      if (vs30 .eq. 1180.) then
     	f5 = (a10T + bT*n) * alog(1180/vLinT)
      endif

c     Set Regional z1 reference (eq 4.18)
      if (regionflag .eq. 1) then
         z1_ref =  exp(-2.629 / 4.0 * alog((vs30**4.0 + 253.299**4.0)/(2491.945**4.0 + 253.299**4.0)))/ 1000.
      elseif (regionflag .eq. 3) then
         z1_ref = exp ( -5.23/2. * alog( (Vs30**2.0 + 412.**2.0)/(1360.**2.0+412.**2.0) ) ) / 1000.
      else
         z1_ref = exp ( -7.67/4. * alog( (Vs30**4.0 + 610.**4.0)/(1360.**4.0+610.**4.0) ) ) / 1000.
      endif

C     Soil Depth Model (eq 4.17)
C     Updated 8/1/13
      if ( vs30 .lt. 150.0 ) then
         y1z = a43T
         y2z = a43T
         x1z = 50.0
         x2z = 150.0
      elseif ( vs30 .lt. 250.0 ) then
         y1z = a43T
         y2z = a44T
         x1z = 150.0
         x2z = 250.0
      elseif ( vs30 .lt. 400.0 ) then
         y1z = a44T
         y2z = a45T
         x1z = 250.0
         x2z = 400.0
      elseif ( vs30 .lt. 700.0 ) then
         y1z = a45T
         y2z = a46T
         x1z = 400.0
         x2z = 700.0
      else
         y1z = a46T
         y2z = a46T
         x1z = 700.0
         x2z = 1000.0
      endif
C     Calculation f10 term and set it equal to zero for Vs=1180m/s (i.e., reference condition)
      if (vs30 .eq. 1180.0) then
          f10 = 0.0
      else
         f10 = ( y1z + (Vs30-x1z)*(y2z-y1z)/(x2z-x1z))*alog( (z1 + 0.01) / (z1_ref+0.01) )
      endif
c     Compute HW taper1 (eq 4.11)
      if ( dip .le. 30. ) then
        HW_taper1 = 60./ 45.
      else
        HW_taper1 = (90.-dip)/45.
      endif
c     Compute HW taper2 (eq. 4.12)
      hw_a2 = 0.2
      if( mag .ge. 6.5 ) then
        HW_taper2 = 1. + hw_a2 * (mag-6.5)
      elseif ( mag .gt. 5.5 ) then
        HW_taper2 = 1. + HW_a2 * (mag-6.5) - (1.0 - HW_a2)*(mag-6.5)**2
      else
        HW_taper2 = 0.
      endif
c     Compute HW taper 3 (eq. 4.13)
C	  April 11, correction by ronnie for HW_taper3 when Rx.gt.R2
      h1 = 0.25
      h2 = 1.5
      h3 = -0.75
      R1 = fltWidth * cos(dip*3.1415926/180.)
      R2 = 3.*R1
      if ( Rx .lt. R1 ) then
        HW_taper3 = h1 + h2*(Rx/R1) + h3*(Rx/R1)**2
      elseif ( Rx . le. R2 ) then
        HW_taper3 = 1. - (Rx-R1)/(R2-R1)
      else
        HW_taper3 = 0.
      endif
c     Compute HW taper 4 (eq 4.14)
      if ( ZTOR .le. 10. ) then
        HW_taper4 = 1. - (ZTOR**2) / 100.
      else
        HW_taper4 = 0.
      endif

c     Compute HW taper 5 (eq. 13)  **** Ry0 version ***
      Ry1 = Rx * tan(20.*3.1415926/180.)
      if ( Ry0 .lt. Ry1 ) then
        HW_taper5 = 1.
      elseif ( Ry0-Ry1 .lt. 5. ) then
        HW_taper5 = 1. - (Ry0-Ry1) / 5.
      else
        HW_taper5 = 0.
      endif
c     Compute HW taper 5 (eq. 4.15b)  **** No Ry0 version ***
c      if (Rjb .eq. 0. ) then
c        HW_taper5 = 1.
c      elseif ( Rjb .lt. 30. ) then
c        HW_taper5 = 1 - Rjb/30.
c      else
c        HW_taper5 = 0.
c      endif
c     Hanging wall Model (eq 4.10)
      if ( HWFlag .eq. 1 ) then
        f4 = a13T * HW_taper1 * HW_taper2 * HW_taper3 * HW_taper4 * HW_taper5
      else
        f4 = 0.
      endif
C     Add aftershock factor (eq 4.21)
      if (msasflag .eq. 1) then
         if (CRjb .ge. 15.0) then
             f11 = 0.0
         elseif (CRjb .le. 5.0) then
             f11 = a14T
         else
             f11 = a14T * ( 1.0 - (CRjb - 5.0) /10.0)
         endif
      elseif (msasflag .eq. 0) then
         f11 = 0.0
      endif
C     Now apply the regional attenuation differences (eq 4.22)
C     Global No Change
      if (regionflag .eq. 0) then
         freg = 0.0
C     Taiwan
      elseif (regionflag .eq. 1) then
         f12 = a31T * alog(vs30Star/VlinT)
         freg = f12 + a25T*Rrup
      elseif (regionflag .eq. 1 .and. vs30 .eq. 1180.0) then
         f12 = a31T * alog(1180/VlinT)
         freg = f12 + a25T*Rrup
C     China
      elseif (regionflag .eq. 2) then
         freg = a28T*Rrup
C     Japan
      elseif (regionflag .eq. 3) then
         freg = a42T + a29T*Rrup
      endif
C     Set the Sigma Values
      if (regionflag .ne. 3)  then
c     Compute within-event term, phiA, at the surface for linear site response (eq 7.1)
        if (mag .lt. 4.0) then
           phiA_est = s1estT
        elseif (mag .le. 6.0) then
           phiA_est = s1estT + ((s2estT-s1estT)/2.0)*(mag-4.0)
        else
           phiA_est = s2estT
        endif
c     Compute within-event term, phiA, for known Vs30
        if (mag .lt. 4.0) then
           phiA_msr = s1msrT
        elseif (mag .le. 6.0) then
           phiA_msr = s1msrT + ((s2msrT-s1msrT)/2.0)*(mag-4.0)
        else
           phiA_msr = s2msrT
        endif
C     choose phiA by Vs30 class
        if (vs30_class .eq. 0 ) then
	     phiA = phiA_est
        elseif (vs30_class .eq. 1) then
	     phiA = phiA_msr
        else
	     stop 99
        endif
C     Set Sigma values for Japan Region
      else
C calculate phi_A for Japan (eq. 7.3)
        if (Rrup .lt. 30) then
           phiA = s5T
        elseif (Rrup .le. 80) then
           phiA = s5T + (s6T-s5T)/50*(Rrup-30)
        else
           phiA = s6T
        endif
      endif

c     Compute between-event term, tau (eq. 7.2)
      if (mag .lt. 5.0) then
         tauA = s3T
      elseif (mag .le. 7.0) then
         tauA = s3T + ((s4T-s3T)/2.0)*(mag-5.0)
      else
         tauA = s4T
      endif
      tauB = tauA
c     Compute phiB, within-event term with site amp variability removed (eq. 7.7)
c     with fix to model for small mag at long periods - limit sigAmp to be less than phiA
      sigAmp = 0.4
      if (phiA .le. sigAmp) then
        sigAmp = phiA*0.99
      endif
      phiB = sqrt( phiA**2 - sigAmp**2)

c     Compute partial derivative of alog(soil amp) w.r.t. alog(Sa1180) (eq. 7.10)
      if ( vs30 .ge. vLinT) then
        dAmp_dSa1180 = 0.
      else
        dAmp_dSa1180 = bT*Sa1180 * ( -1. / (Sa1180+c)
     1              + 1./ (Sa1180 + c*(vs30/vLinT)**(n)) )
      endif
C     Compute phi, with non-linear effects (eq. 7.8)
      phi = sqrt( phiB**2 * (1. + dAmp_dSa1180)**2 + sigAmp**2 )
C     Compute tau, with non-linear effects (eq. 7.9)
      tau = tauB * (1. + dAmp_dSa1180)

c     Compute median ground motion (eq. 1)
      lnSa = f1 + f4 + f5 + f6 + f7 + f8 + f10 + f11 + freg
      return
      end

