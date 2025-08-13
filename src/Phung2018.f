c ------------------------------------------------------------------            
C *** Phung2018 Crust and Subduction Model- Horizontal ***********
c ------------------------------------------------------------------            

      Subroutine S04_PhungCrust2018 ( m, Rrup, Rbjf, specT, period2, lnY, sigma, iflag, 
     1                     vs, Delta, DTor, Ftype, depthvs10, vs30_class,
     2                       regionflag, phi, tau, HWflag, Rx )

      implicit none
      
      integer MAXPER, i, nPer
      parameter (MAXPER=25)
      REAL Period(MAXPER), C1(MAXPER), C1a(MAXPER), C1b(MAXPER), C1c(MAXPER), C1d(MAXPER)
      REAL cn(MAXPER), cm(MAXPER), c3(MAXPER), c5(MAXPER), c6(MAXPER)
      REAL c7(MAXPER), C7b(MAXPER), C11(MAXPER), C11b(MAXPER), CHM(MAXPER)
      REAL phi1(MAXPER), phi2(MAXPER), phi3(MAXPER), phi4(MAXPER), phi5(MAXPER)
      REAL sigma1inf(MAXPER), sigma2inf(MAXPER)
      REAL tau1(MAXPER), tau2(MAXPER), sigma1(MAXPER), sigma2(MAXPER),tau0(MAXPER)
      REAL sigma3(MAXPER), c8(MAXPER), c8b(MAXPER)
      REAL cg1CA(MAXPER), cg1JP(MAXPER), cg10(MAXPER), dp(MAXPER)
      Real tauT1(MAXPER), phiT1(MAXPER), c9(MAXPER), c9a(MAXPER), c9b(MAXPER)
      real phiss(MAXPER), phis2s(MAXPER) 
      real phiss1M(MAXPER), phiss2M(MAXPER)
      real phi1CA(MAXPER), phi1JP(MAXPER), phi10(MAXPER), cg1(MAXPER), cg2(MAXPER), cg3(MAXPER)
      real vs, phi6
      real Finferred, Fmeasured, dDPP
      REAL c1T, c1aT, c1bT, c1cT, c1dT,cnT, cmT, c5T, c6T, c3T, c9T, c9aT, c9bT
      REAL phi1T, phi2T, phi3T, phi4T, sigma3T, sigma1T, sigma2T
      REAL phi5T, tau1T, tau2T, tauT, tau0T, phi6T
      real c7T, c7bT, c11T, c11bT, cHMT
      real cg1CAT, cg1JPT, cg10T, sigma1infT, sigma2infT
      real phi1CAT, phi1JPT, phi10T
      REAL c2, c4, c4a, cRB, pi, d2r, term14, term15, term16, NL0
      REAL term1, term2, term3, term5, term4, term6, term8, term9, term10, term12, term11
      REAL phissT, phis2sT, sigma1meaT, sigma2meaT, phiss1MT, phiss2MT
      real CNS, cosdelta, psa_ref, psa, cg1T, cg2T, cg3T, dpT
      integer iflag, count1, count2, vs30_class, regionflag, msasflag, HWflag
      REAL M, RRUP, DTOR, Delta, specT, sigma, Ftype, Rbjf, Rx
      REAL period2, lnY, F_RV, F_NM, tau, phi, rkdepth
      real c8T, c8a, c8bT, fd, lnpsa_ref, lnpsa, sa
      real sigmaNL0, F_Measured, F_Inferred, mz_TOR, deltaZ_TOR, coshM
      real period1 ,Ez1, term7, deltaZ1, depthvs10, delc5

C     Mainshock and Aftershocks included based on MSASFlag
C         0 = Mainshocks
C         1 = Aftershocks
C
C     regionflag  
C           = 1 for Taiwan
C           = 0 for global
C
C     vs30_class     Note
C     -------------------------
C      0         estimated
C      1         measured
C

      data period / 0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.075, 0.1, 0.12, 0.15, 0.17, 0.2, 0.25, 0.3, 0.4, 0.5, 
     1              0.75, 1, 1.5, 2, 3, 4, 5, 7.5, 10 /
      data c1 / -1.654642, -1.574253, -1.520116, -1.424583, -1.301324, -1.164997, -0.847910, -0.672079, 
     1          -0.593532, -0.578521, -0.608837, -0.672703, -0.799766, -0.975575, -1.287988, -1.547417,  
     1          -2.106085, -2.510412, -3.140080, -3.403277, -3.730282, -3.972268, -4.045129, -4.301415, -4.423677 /
      data c3 / 1.543196, 1.513556, 1.476841, 1.434544, 1.386104, 1.321224, 1.249835, 1.314168, 1.396081,  
     1          1.483981, 1.526734, 1.607787, 1.761750, 1.914659, 2.125763, 2.275589, 2.598148, 2.759899,  
     1          2.885389, 2.982531, 3.039484, 3.075429, 3.098058, 3.104144, 3.087479 /
      data cn / 12.148668, 12.148668, 12.248034, 12.533784, 12.991897, 13.650754, 15.714475, 16.772622,  
     1          16.775630, 16.186798, 15.843144, 15.014671, 12.696431, 10.449811, 6.802217, 4.410694,  
     1          3.406400, 3.161200, 2.807800, 2.463100, 2.211100, 1.966800, 1.667100, 1.573700, 1.526500 /
      data cm / 4.883828, 5.386356, 5.455551, 5.509632, 5.512664, 5.533465, 5.566513, 5.513233, 5.483484,  
     1          5.487482, 5.474835, 5.435991, 5.412552, 5.379773, 5.345804, 5.320560, 5.369402, 5.445281,  
     1          5.624212, 5.751105, 6.021403, 6.156573, 6.311369, 6.588973, 6.923637 /
      data c5 / 6.455100, 6.455100, 6.455100, 6.455100, 6.455100, 6.455100, 6.455100, 6.830500, 7.133300,  
     1          7.362100, 7.436500, 7.497200, 7.541600, 7.560000, 7.573500, 7.577800, 7.580800, 7.581400,  
     1          7.581700, 7.581800, 7.581800, 7.581800, 7.581800, 7.581800, 7.581800 /
      data c6 / 0.490800, 0.490800, 0.492500, 0.499200, 0.503700, 0.504800, 0.504800, 0.504800, 0.504800,  
     1          0.504500, 0.503600, 0.501600, 0.497100, 0.491900, 0.480700, 0.470700, 0.457500, 0.452200,  
     1          0.450100, 0.450000, 0.450000, 0.450000, 0.450000, 0.450000, 0.450000 /
      data cHM  / 3.095600, 3.095600, 3.096300, 3.097400, 3.098800, 3.101100, 3.109400, 3.238100, 3.340700,  
     1          3.430000, 3.468800, 3.514600, 3.574600, 3.623200, 3.694500, 3.740100, 3.794100, 3.814400,  
     1          3.828400, 3.833000, 3.836100, 3.836900, 3.837600, 3.838000, 3.838000 /
      data c7 / 0.008581, 0.008035, 0.007593, 0.007250, 0.007006, 0.006860, 0.007008, 0.007247, 0.007456,  
     1          0.007703, 0.007799, 0.007823, 0.008071, 0.008396, 0.009275, 0.010166, 0.012793, 0.013762,  
     1          0.013900, 0.012559, 0.009184, 0.004797, 0.001068, -0.004234, -0.006203 /
      data c7b / 0.020232, 0.021034, 0.021639, 0.022052, 0.022284, 0.022341, 0.021712, 0.020031, 0.018585,  
     1          0.016544, 0.015413, 0.014411, 0.013238, 0.011958, 0.009469, 0.005800, -0.003683, -0.008131,  
     1          -0.010287, -0.008563, -0.003059, 0.003920, 0.013064, 0.027920, 0.041953 /
      data c1a / 0.139286, 0.134926, 0.130854, 0.127072, 0.123583, 0.120391, 0.113706, 0.108868, 0.106295,  
     1          0.104509, 0.104665, 0.107605, 0.118083, 0.130738, 0.153830, 0.179466, 0.158850, 0.145609,  
     1          0.137311, 0.117810, 0.076184, 0.038194, 0.038194, 0.038194, 0.038194 /
      data c1c / 0.126967, 0.136204, 0.142178, 0.146345, 0.156255, 0.171596, 0.177475, 0.167603, 0.155744,  
     1          0.126802, 0.104491, 0.075057, 0.043282, 0.026574, -0.014114, -0.064611, 0.035510, 0.051493,  
     1          0.076539, 0.093041, 0.115673, 0.147164, 0.147164, 0.147164, 0.147164 /
      data c1b / 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000,  
     1          0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000,  
     1          0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000 /
      data c1d / -0.043679, -0.043261, -0.043154, -0.043339, -0.043803, -0.044544, -0.047735, -0.053268,  
     1          -0.059612, -0.069063, -0.074707, -0.093710, -0.130636, -0.167306, -0.238958, -0.292486,  
     1          -0.391164, -0.459741, -0.531023, -0.531023, -0.531023, -0.531023, -0.531023, -0.531023, -0.531023 /
      data c11 / 0.016453, -0.108037, -0.102072, -0.104638, -0.105159, -0.096947, -0.079174, -0.120807,  
     1          -0.127655, -0.123958, -0.120235, -0.128555, -0.104990, -0.125335, -0.131459, -0.102607,  
     1          -0.072843, -0.072287, -0.143270, -0.171096, -0.269172, -0.321537, -0.344322, -0.379467, -0.478011 /
      data c11b  / -0.168281, 0.195952, 0.181778, 0.163170, 0.142063, 0.098054, 0.046297, 0.173997, 0.209295,  
     1          0.217340, 0.218819, 0.262936, 0.231024, 0.270344, 0.306056, 0.272617, 0.265158, 0.303895,  
     1          0.443286, 0.520454, 0.817527, 1.015932, 0.892205, 0.864364, 1.443598 /
      data cg1 / -0.007288, -0.007609, -0.007926, -0.008239, -0.008547, -0.008850, -0.009598, -0.009985,  
     1          -0.010161, -0.010176, -0.009969, -0.009506, -0.008808, -0.008145, -0.006990, -0.006140,  
     1          -0.004769, -0.004051, -0.003342, -0.002973, -0.002487, -0.002123, -0.001764, -0.001079, -0.000742 /
      data cg2 / -0.006969, -0.007127, -0.007249, -0.007328, -0.007362, -0.007361, -0.007052, -0.005719,  
     1          -0.004365, -0.002650, -0.002000, -0.001255, -0.000750, -0.000447, -0.000247, -0.000417,  
     1          -0.001131, -0.001741, -0.002428, -0.002706, -0.004107, -0.005776, -0.007748, -0.009142, -0.012633 /
      data cg3 / 4.222069, 4.225635, 4.230342, 4.236182, 4.250189, 4.303123, 4.446127, 4.610835, 4.723497,  
     1          4.878141, 4.981707, 5.066411, 5.219865, 5.328220, 5.201762, 5.187932, 4.877209, 4.639751,  
     1          4.571204, 4.425117, 3.621904, 3.486264, 3.277906, 3.074948, 3.074948 /
      data dp / -6.785205, -6.750648, -6.716179, -6.681799, -6.647507, -6.613303, -6.528180, -6.443608,  
     1          -6.376345, -6.276109, -6.209723, -6.110798, -5.947666, -5.786703, -5.471253, -5.164376,  
     1          -4.434204, -3.755966, -2.550267, -1.536848, -0.052838, 0.000000, 0.000000, 0.000000, 0.000000 /
      data phi1 / -0.516116, -0.516935, -0.508693, -0.495823, -0.476815, -0.454879, -0.430231, -0.459892,  
     1          -0.479365, -0.509853, -0.531335, -0.551465, -0.580586, -0.626209, -0.677679, -0.702586,  
     1          -0.819168, -0.897580, -1.090857, -1.042582, -0.991035, -0.955669, -0.915233, -0.802877, -0.699613 /
      data phi2 / -0.141700, -0.141700, -0.136400, -0.140300, -0.159100, -0.186200, -0.253800, -0.294300,  
     1          -0.307700, -0.311300, -0.306200, -0.292700, -0.266200, -0.240500, -0.197500, -0.163300,  
     1          -0.102800, -0.069900, -0.042500, -0.030200, -0.012900, -0.001600, 0.000000, 0.000000, 0.000000 /
      data phi3 / -0.007010, -0.007010, -0.007279, -0.007354, -0.006977, -0.006467, -0.005734, -0.005604,  
     1          -0.005696, -0.005845, -0.005959, -0.006141, -0.006439, -0.006704, -0.007125, -0.007435,  
     1          -0.008120, -0.008444, -0.007707, -0.004792, -0.001828, -0.001523, -0.001440, -0.001369, -0.001361 /
      data phi4 / 0.102151, 0.102151, 0.108360, 0.119888, 0.133641, 0.148927, 0.190596, 0.230662, 0.253169,  
     1          0.266468, 0.265060, 0.255253, 0.231541, 0.207277, 0.165464, 0.133828, 0.085153, 0.058595,  
     1          0.031787, 0.019716, 0.009643, 0.005379, 0.003223, 0.001134, 0.000515 /
      data phi5 / 0.131065, 0.131615, 0.131659, 0.138990, 0.149910, 0.167293, 0.199578, 0.230268, 0.221342,  
     1          0.193848, 0.168562, 0.145600, 0.127889, 0.116581, 0.087546, 0.055917, 0.016064, 0.004084,  
     1          0.083716, 0.151694, 0.229009, 0.223222, 0.217675, 0.206050, 0.175716 /
      data c9 / 0.922800, 0.922800, 0.929600, 0.939600, 0.966100, 0.979400, 1.026000, 1.017700, 1.000800,  
     1          0.980100, 0.965200, 0.945900, 0.919600, 0.882900, 0.830200, 0.788400, 0.675400, 0.619600,  
     1          0.510100, 0.391700, 0.124400, 0.008600, 0.000000, 0.000000, 0.000000 /
      data c9a / 0.120200, 0.120200, 0.121700, 0.119400, 0.116600, 0.117600, 0.117100, 0.114600, 0.112800,  
     1          0.110600, 0.115000, 0.120800, 0.120800, 0.117500, 0.106000, 0.106100, 0.100000, 0.100000,  
     1          0.100000, 0.100000, 0.100000, 0.100000, 0.100000, 0.100000, 0.100000 /
      data c9b / 6.860700, 6.860700, 6.869700, 6.911300, 7.027100, 7.095900, 7.329800, 7.258800, 7.237200,  
     1          7.210900, 7.249100, 7.298800, 7.369100, 6.878900, 6.533400, 6.526000, 6.500000, 6.500000,  
     1          6.500000, 6.500000, 6.500000, 6.500000, 6.500000, 6.500000, 6.500000 /
      data c8 / 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0991, 0.1982,  
     1          0.2154, 0.2154, 0.2154, 0.2154, 0.2154, 0.2154, 0.2154, 0.2154 /
      data c8b / 0.483300, 0.483300, 1.214400, 1.642100, 1.945600, 2.181000, 2.608700, 2.912200, 3.104500,  
     1          3.339900, 3.471900, 3.643400, 3.878700, 4.071100, 4.374500, 4.609900, 5.037600, 5.341100,  
     1          5.768800, 6.072300, 6.500000, 6.803500, 7.038900, 7.466600, 7.770000 /
      data cg1CA / -0.006490, -0.006470, -0.006536, -0.007179, -0.007676, -0.008257, -0.009532, -0.010166,  
     1          -0.010054, -0.009805, -0.009030, -0.008466, -0.007534, -0.006392, -0.005351, -0.004422,  
     1          -0.002395, -0.002620, -0.002223, -0.000956, -0.000992, -0.001407, -0.000876, -0.000876, -0.000876 /
      data phi1CA / -0.577526, -0.576647, -0.567620, -0.540828, -0.525764, -0.504658, -0.472272, -0.515688,  
     1          -0.536177, -0.547802, -0.604585, -0.639294, -0.705116, -0.748564, -0.799633, -0.879300,  
     1          -0.970139, -0.979947, -1.184797, -1.154081, -0.959930, -0.907932, -0.846230, -0.711399, -0.555313 /
      data cg1JP / -0.011222, -0.011180, -0.011131, -0.011552, -0.012203, -0.012473, -0.012366, -0.012286, 
     1           -0.012403, -0.012407, -0.012416, -0.012184, -0.011760, -0.011313, -0.010318, -0.009411,  
     1          -0.007265, -0.006083, -0.006874, -0.007552, -0.007297, -0.005609, -0.004063, -0.001333, -0.000396 /
      data phi1JP / -0.443000, -0.443292, -0.429683, -0.387328, -0.310552, -0.242082, -0.117036, -0.214463,  
     1          -0.310582, -0.439369, -0.523364, -0.632758, -0.769013, -0.855314, -0.912410, -0.965488,  
     1          -1.037790, -1.030000, -1.118098, -0.960000, -0.762240, -0.659832, -0.587533, -0.407331, -0.364009 /
      data cg10 / -0.005243, -0.005818, -0.006354, -0.006849, -0.007301, -0.007709, -0.008550, -0.009063,  
     1          -0.009177, -0.008970, -0.008694, -0.008187, -0.007169, -0.006289, -0.005024, -0.004042,  
     1          -0.002378, -0.001848, -0.001019, -0.001042, -0.001134, -0.001182, -0.001085, -0.001085, -0.001085 /
      data phi10 / -0.537767, -0.537380, -0.527902, -0.503857, -0.494849, -0.477957, -0.453645, -0.485480,  
     1          -0.502936, -0.515412, -0.561167, -0.601685, -0.677544, -0.732538, -0.771490, -0.814426,  
     1          -0.888440, -0.876327, -1.080612, -1.060215, -0.904488, -0.873702, -0.850298, -0.749727, -0.598953 /
      data tau1 / 0.400000, 0.400000, 0.402600, 0.406300, 0.409500, 0.412400, 0.417900, 0.421900, 0.424400,  
     1          0.427500, 0.429200, 0.431300, 0.434100, 0.436300, 0.439600, 0.441900, 0.445900, 0.448400,  
     1          0.451500, 0.453400, 0.455800, 0.457400, 0.458400, 0.460100, 0.461200 /
      data tau2 / 0.260000, 0.260000, 0.263700, 0.268900, 0.273600, 0.277700, 0.285500, 0.291300, 0.294900,  
     1          0.299300, 0.301700, 0.304700, 0.308700, 0.311900, 0.316500, 0.319900, 0.325500, 0.329100,  
     1          0.333500, 0.336300, 0.339800, 0.341900, 0.343500, 0.345900, 0.347400 /
      data sigma1 / 0.491200, 0.491200, 0.490400, 0.498800, 0.504900, 0.509600, 0.517900, 0.523600, 0.527000,  
     1          0.530800, 0.532800, 0.535100, 0.537700, 0.539500, 0.542200, 0.543300, 0.529400, 0.510500,  
     1          0.478300, 0.468100, 0.461700, 0.457100, 0.453500, 0.447100, 0.442600 /
      data sigma2 / 0.376200, 0.376200, 0.376200, 0.384900, 0.391000, 0.395700, 0.404300, 0.410400, 0.414300,  
     1          0.419100, 0.421700, 0.425200, 0.429900, 0.433800, 0.439900, 0.444600, 0.453300, 0.459400,  
     1          0.468000, 0.468100, 0.461700, 0.457100, 0.453500, 0.447100, 0.442600 /
      data sigma3 / 0.800000, 0.800000, 0.800000, 0.800000, 0.800000, 0.800000, 0.800000, 0.800000, 0.800000,  
     1          0.800000, 0.800000, 0.800000, 0.799900, 0.799700, 0.798800, 0.796600, 0.779200, 0.750400,  
     1          0.713600, 0.703500, 0.700600, 0.700100, 0.700000, 0.700000, 0.700000 /
      data tau0  / 0.372401, 0.376514, 0.376953, 0.388775, 0.403902, 0.421147, 0.443051, 0.439374, 0.424113,  
     1          0.404915, 0.393373, 0.375874, 0.370636, 0.374764, 0.405444, 0.432212, 0.442190, 0.473377,  
     1          0.474720, 0.468637, 0.454880, 0.483916, 0.507364, 0.575778, 0.562976 /
      data phiss / 0.465403, 0.463342, 0.463153, 0.470111, 0.477682, 0.482355, 0.483524, 0.478191, 0.470999,  
     1          0.465492, 0.465018, 0.465715, 0.468827, 0.475041, 0.483249, 0.494461, 0.493867, 0.487026,  
     1          0.479004, 0.471925, 0.458172, 0.456369, 0.453753, 0.445327, 0.443534 /
      data phis2s / 0.317008, 0.317230, 0.317076, 0.324896, 0.337890, 0.355358, 0.389659, 0.400733, 0.396010,  
     1          0.376335, 0.367016, 0.353613, 0.337756, 0.335367, 0.333318, 0.334606, 0.340700, 0.348831,  
     1          0.379082, 0.390649, 0.389996, 0.387436, 0.381651, 0.374743, 0.346654 /


C Find the requested spectral period and corresponding coefficients
      nPer = 25
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         c1T  =   c1(1)
         c3T  =   c3(1)
         cnT  =   cn(1)
         cmT  =   cm(1)
         c5T  =   c5(1)
         c6T  =   c6(1)
         cHMT  =   cHM(1)
         c7T  =   c7(1)
         c7bT  =   c7b(1)
         c1aT  =   c1a(1)
         c1cT  =   c1c(1)
         c1bT  =   c1b(1)
         c1dT  =   c1d(1)
         c11T  =   c11(1)
         c11bT  =   c11b(1)
         cg1T  =   cg1(1)
         cg2T  =   cg2(1)
         cg3T  =   cg3(1)
         dpT   =   dp(1)
         cg1CAT  =   cg1CA(1)
         phi1CAT  =   phi1CA(1)
         cg1JPT  =   cg1JP(1)
         phi1JPT  =   phi1JP(1)
         cg10T  =   cg10(1)
         phi10T  =   phi10(1)

         phi1T  =   phi1(1)
         phi2T  =   phi2(1)
         phi3T  =   phi3(1)
         phi4T  =   phi4(1)
         phi5T  =   phi5(1)
         c9T   =   c9(1)
         c9aT  =   c9a(1)
         c9bT  =   c9b(1)
         c8T   =   c8(1)
         c8bT  =   c8b(1)

         tau1T  =   tau1(1)
         tau2T  =   tau2(1)
         sigma1T  =   sigma1(1)
         sigma2T  =   sigma2(1)
         sigma3T  =   sigma3(1)
         phissT   =   phiss(1)
         phis2sT  =   phis2s(1)
         tau0T  =   tau0(1)

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
c     write statement removed for R package compliance
c     write statement removed for R package compliance
c     write statement removed for R package compliance
c     write statement removed for R package compliance
c     write statement removed for R package compliance
c     write statement removed for R package compliance
c     write statement removed for R package compliance
c     write statement removed for R package compliance
c     write statement removed for R package compliance
c     write statement removed for R package compliance
      return

C Interpolate the coefficients for the requested spectral period.
 1020       call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +                   specT,c1T,iflag)
            call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +                   specT,c3T,iflag)
            call S24_interp (period(count1),period(count2),cn(count1),cn(count2),
     +                   specT,cnT,iflag)
            call S24_interp (period(count1),period(count2),cm(count1),cm(count2),
     +                   specT,cmT,iflag)
            call S24_interp (period(count1),period(count2),c5(count1),c5(count2),
     +                   specT,c5T,iflag)
            call S24_interp (period(count1),period(count2),c6(count1),c6(count2),
     +                   specT,c6T,iflag)
            call S24_interp (period(count1),period(count2),cHM(count1),cHM(count2),
     +                   specT,cHMT,iflag)
            call S24_interp (period(count1),period(count2),c7(count1),c7(count2),
     +                   specT,c7T,iflag)
            call S24_interp (period(count1),period(count2),c7b(count1),c7b(count2),
     +                   specT,c7bT,iflag)

            call S24_interp (period(count1),period(count2),c1a(count1),c1a(count2),
     +                   specT,c1aT,iflag)
            call S24_interp (period(count1),period(count2),c1b(count1),c1b(count2),
     +                   specT,c1bT,iflag)
            call S24_interp (period(count1),period(count2),c1c(count1),c1c(count2),
     +                   specT,c1cT,iflag)
            call S24_interp (period(count1),period(count2),c1d(count1),c1d(count2),
     +                   specT,c1dT,iflag)
            call S24_interp (period(count1),period(count2),c11(count1),c11(count2),
     +                   specT,c11T,iflag)
            call S24_interp (period(count1),period(count2),c11b(count1),c11b(count2),
     +                   specT,c11bT,iflag)

            call S24_interp (period(count1),period(count2),c8(count1),c8(count2),
     +                   specT,c8T,iflag)
            call S24_interp (period(count1),period(count2),c8b(count1),c8b(count2),
     +                   specT,c8bT,iflag)
            call S24_interp (period(count1),period(count2),c9(count1),c9(count2),
     +                   specT,c9T,iflag)
            call S24_interp (period(count1),period(count2),c9a(count1),c9a(count2),
     +                   specT,c9aT,iflag)
            call S24_interp (period(count1),period(count2),c9b(count1),c9b(count2),
     +                   specT,c9bT,iflag)

  
            call S24_interp (period(count1),period(count2),cg1(count1),cg1(count2),
     +                   specT,cg1T,iflag)
            call S24_interp (period(count1),period(count2),cg2(count1),cg2(count2),
     +                   specT,cg2T,iflag)
            call S24_interp (period(count1),period(count2),cg3(count1),cg3(count2),
     +                   specT,cg3T,iflag)

            call S24_interp (period(count1),period(count2),dp(count1),dp(count2),
     +                   specT,dpT,iflag)

            call S24_interp (period(count1),period(count2),cg1CA(count1),cg1CA(count2),
     +                   specT,cg1CAT,iflag)
             call S24_interp (period(count1),period(count2),cg1JP(count1),cg1JP(count2),
     +                   specT,cg1JPT,iflag)
             call S24_interp (period(count1),period(count2),cg10(count1),cg10(count2),
     +                   specT,cg10T,iflag)
 
     
            call S24_interp (period(count1),period(count2),phi1(count1),phi1(count2),
     +                   specT,phi1T,iflag)
            call S24_interp (period(count1),period(count2),phi2(count1),phi2(count2),
     +                   specT,phi2T,iflag)
            call S24_interp (period(count1),period(count2),phi3(count1),phi3(count2),
     +                   specT,phi3T,iflag)
            call S24_interp (period(count1),period(count2),phi4(count1),phi4(count2),
     +                   specT,phi4T,iflag)
            call S24_interp (period(count1),period(count2),phi5(count1),phi5(count2),
     +                   specT,phi5T,iflag)

            call S24_interp (period(count1),period(count2),phi1CA(count1),phi1CA(count2),
     +                   specT,phi1CAT,iflag)
            call S24_interp (period(count1),period(count2),phi1JP(count1),phi1JP(count2),
     +                   specT,phi1JPT,iflag)

  
            call S24_interp (period(count1),period(count2),phiss(count1),phiss(count2),
     +                   specT,phissT,iflag)
            call S24_interp (period(count1),period(count2),phis2s(count1),phis2s(count2),
     +                   specT,phis2sT,iflag)
            call S24_interp (period(count1),period(count2),tau1(count1),tau1(count2),
     +                   specT,tau1T,iflag)
            call S24_interp (period(count1),period(count2),tau2(count1),tau2(count2),
     +                   specT,tau2T,iflag)
            call S24_interp (period(count1),period(count2),tau0(count1),tau0(count2),
     +                   specT,tau0T,iflag)

            call S24_interp (period(count1),period(count2),sigma1(count1),sigma1(count2),
     +                   specT,sigma1T,iflag)
            call S24_interp (period(count1),period(count2),sigma2(count1),sigma2(count2),
     +                   specT,sigma2T,iflag)
            call S24_interp (period(count1),period(count2),sigma2(count1),sigma2(count2),
     +                   specT,sigma2T,iflag)


 1011 period1 = specT                                                                                                              

c     Set the fault mechanism term.
C     fType     Mechanism                      Rake
C     ------------------------------------------------------
C      -1       Normal                   -120 < Rake < -60.0
C     1, 0.5    Reverse and Rev/Obl        30 < Rake < 150.0
C     0,-0.5    Strike-Slip and NMl/Obl        Otherwise
         if (ftype .eq. -1) then
            F_RV = 0.0
            F_NM = 1.0
         elseif (ftype .ge. 0.5) then
            F_RV = 1.0
            F_NM = 0.0
         else
            F_RV = 0.0
            F_NM = 0.0
         endif

C     Constant terms
        c2 = 1.06
        c4 = -2.1
        c4a = -0.5
        cRB = 50.0
        phi6 = 300
        c8a = 0.2695
  
      if(regionflag .eq. 1) then
C for Taiwan
        cg1T = cg1T
        phi1T = phi1T

      elseif(regionflag .eq. 2) then
C for California
        cg1T = cg1CAT
        phi1T = phi1CAT

      elseif(regionflag .eq. 3) then
C for others
        cg1T = cg10T
        phi1T = phi10T

        elseif(regionflag .eq. 4) then
C for Japan
        cg1T = cg1JPT
        phi1T = phi1JPT
  
      endif

C     Current code set for Measured Vs30 values (i.e., Vs30class=1)
      if (vs30_class .eq. 0) then
         Fmeasured = 0.0
         FInferred = 1.0

      elseif (vs30_class .eq. 1) then      
         Fmeasured = 1.0
         FInferred = 0.0

      endif       
  
c Center Z_TOR on the Z_TOR-M relation
        if (F_RV.EQ.1) then

            mZ_TOR = max(3.5384-2.60 * max(M-5.8530,0.0),0.0)
            mZ_TOR = mZ_TOR * mZ_TOR

        else
            mZ_TOR = max(2.7482-1.7639*max(M-5.5210,0.0),0.0)
            mZ_TOR = mZ_TOR * mZ_TOR
        endif
  
c        if (Z_TOR .EQ. -999) Z_TOR = mZ_TOR
        deltaZ_TOR = Dtor - mZ_TOR
   
c Reference motion  
  
        pi = atan(1.0)*4.0
        d2r = pi/180.0
        term1 = c1T
  
c Magnitude scaling
        term6 = c2 * (M-6.0) 
        term7 = (c2-c3T)/cnT * alog(1.0 + exp(cnT*(cMT-M)))  

c Near-field magnitude and distance scaling
       if (Dtor > 20 .and. M < 7) then
      delc5 = dpT * max(DTor/50.0-20.0/50.0,0.0)
      else
      delc5 =0.0
       endif
        
        CNS = (c5T + delc5) * cosh(c6T * max((M-cHMT),0.0))  
          
        term8 = c4 * alog(Rrup + CNS)                 

c Distance scaling at large distance
        term9 = (c4a-c4) * alog( sqrt(Rrup*Rrup+cRB*cRB) )  
        term10 = (cg1T + cg2T/cosh(max((M-cg3T),0.0)))*Rrup  


c Scaling with other source variables (F_RV, F_NM, deltaZ_TOR, and Dip)
        coshM = cosh(2*max(M-4.5,0.0))
        cosDELTA = cos(DELTA*d2r)
        term2 = (c1aT+c1cT/coshM) * F_RV 
        term3 = (c1bT+c1dT/coshM) * F_NM 
        term4 = (c7T +c7bT/coshM) * deltaZ_TOR 
        term5 = (c11T+c11bT/coshM)* cosDELTA**2   

c HW effect 
        if (HWFlag .eq. 0) then
           term12 = 0.0
        else
         term12 = c9T * HWFlag *(cosDELTA) * (c9aT+(1-c9aT)
     1        *tanh(abs(Rx)/c9bT)) *
     1          (1.0 - sqrt(Rbjf**2+DTor**2)/(Rrup + 1))
        endif

C     Current version of the code sets dDPP=0 (i.e., no directivity)
c Directivity effect
        dDPP = 0.0
        term11 = c8T * exp(-c8a * (M-c8bT)**2) *
     1       max(0.0, 1.0-max(0.0,Rrup-40.0)/30.0) *
     1       min(max(0.0,M-5.5)/0.8, 1.0) * dDPP
       
c Predicted median Sa on reference condition (Vs=1130 m/sec)
        lnpsa_ref = term1+term2+term3+term5+term4+term6+term7+term8+term9+term10+term11+term12
        psa_ref = exp(lnpsa_ref)
  
c Linear soil amplification
        term14 = phi1T * min(alog(Vs/1130.0), 0.0)   

c Nonlinear soil amplification
        term15 = phi2T *
     1      (exp(phi3T*(min(Vs,1130.0)-360.0)) - exp(phi3T*(1130.0-360.0)))*
     1      alog((psa_ref+phi4T)/phi4T)

C Deviation from ln(Vs30) scaling: bedrock depth (Z1) effect.
        Ez1 = exp(-2.63/4.0 * alog((VS**4.0 + 253.0**4.0)/(2492.0**4.0 + 253.0**4.0)))
        deltaZ1 = depthvs10*1000.0 - Ez1
C     1    exp(-2.63/4.0 * alog((VS**4.0 + 253.0**4.0)/(2492.0**4.0 + 253.0**4.0)))
 
        if (regionflag .eq. 0) then
            term16 = 0.0
         elseif (regionflag .eq. 1) then
            term16 = phi5T*( 1.0 -exp(-deltaZ1/phi6))
        endif
  
c Sa on soil condition
        lnpsa = lnpsa_ref + term14 + term15 + term16
        sa = exp(lnpsa_ref + term14 + term15 + term16)
        psa = psa_ref * exp(term14 + term15 + term16)

c        write(*,*) "term1 = " , term1
c        write(*,*) "term2 = " , term2
c        write(*,*) "term3 = " , term3
c        write(*,*) "term4 = " , term4
c        write(*,*) "term5 = " , term5
c        write(*,*) "term6 = " , term6
c        write(*,*) "term7 = " , term7
c        write(*,*) "term8 = " , term8
c        write(*,*) "term9 = " , term9
c        write(*,*) "term10 = ", term10
c        write(*,*) "term14 = ", term14
c        write(*,*) "term15 = ", term15
c        write(*,*) "term16 = ", term16
c        write(*,*) "Ez1 = " , Ez1
c        write(*,*) "deltaZ1 = " , deltaZ1
c        write(*,*) "lnpsa_ref = ", lnpsa_ref
c        write(*,*) "lnpsa = ", lnpsa
c        write(*,*) "psa = ", psa
  
C Compute the sigma term
C Variance Model-1 from CY14 with phi1 from Taiwan


       NL0=phi2T*(exp(phi3T*(min(Vs,1130.0)-360.0))-exp(phi3T*(1130.0-360.0)))
     1    *(psa_ref/(psa_ref+phi4T))
  
       sigmaNL0 = (sigma1T+(sigma2T - sigma1T)/1.5*(min(max(M,5.0),6.5)-5.0))*
     1           sqrt((sigma3T*Finferred + 0.7* Fmeasured) + (1.0+NL0)**2.0)

       tau = tau1T +(tau2T-tau1T)/1.5*(min(max(M,5.0),6.5)-5.0)

       sigma = sqrt((1+NL0)**2.0*(tau)**2.0+sigmaNL0**2.0)

C Variance Model-2 for Taiwan

      sigma = sqrt(tau0T**2 + phissT**2 + phis2sT**2)
      phi = sqrt(phissT**2 + phis2sT**2)

C     Convert ground motion to units of gals.
      lnY = lnpsa + 6.89
      period2 = period1

      return
      end 

c ------------------------------------------------------------------            
C *** Adjusted BCHydro model by Phung and Loh ***********
c ------------------------------------------------------------------            

      subroutine S04_PhungSub2018 ( mag, rRup, vs30, Z10, ZTor, lnY, sigma,  
     2                     specT, period2, iflag, regionflag, ftype )

      implicit none
     
      integer MAXPER, nPer, i1, i      
      parameter (MAXPER=21)
      real period(MAXPER), a5(MAXPER), a13(MAXPER), Mref(MAXPER), a2(MAXPER), a14(MAXPER), 
     1     dela1(MAXPER), dela4(MAXPER), a6jp(MAXPER), a12jp(MAXPER), a8jp(MAXPER)
      real phisstj(MAXPER),  phis2stj(MAXPER), tau0(MAXPER)

      real a1tw(MAXPER), a4tw(MAXPER),  a7(MAXPER), a6tw(MAXPER), a12tw(MAXPER), a8(MAXPER),
     1     a11(MAXPER), a10(MAXPER),
     1     phisstw(MAXPER), phis2stw(MAXPER), tautw(MAXPER), phitw(MAXPER)

      real sigma, lnSa, pgaRock, vs30, rRup, disthypo, mag 

      real periodT, a5T, a13T, MrefT, a2T, a14T, dela1T, dela4T, a6jpT, a12jpT, a8jpT
      real phisstjT,  phis2stjT, tau0T, a1twT, a4twT,  a7T, a6twT, a12twT, a8T
      real a11T, a10T, phisstwT, phis2stwT, tautwT, phitwT
   
      real Ez1, fz10, fmag, frup, fsite, fztor, fevt
      real period1, a3, Z10, ZTor, a9, d, b12, lnY, Fs, a11si, a11ss, phiss, phis2s, a1, a4,a6,a12
      integer count1, count2, iflag, regionflag
      real n, c, c4, c1, faba, R, depth, specT, tau, phi, ftype, period2


      data period  /0, 0.01, 0.02, 0.05, 0.075, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.75, 1, 1.5, 2, 
     1              2.5, 3, 4, 5 /
      data a5 /  0.03849929, 0.04033665, 0.04190178, 0.04509359, 0.04623140, 0.04819708, 0.04325090, 0.03692059, 
     1            0.06597319, 0.06197944, 0.06979644, 0.08783791, 0.09612877, 0.10612877, 0.22744484, 0.16136621,  
     1            0.22767232, 0.27153377, 0.28822087, 0.32589322, 0.30383949 /  
      data a13 /  -0.0256568, -0.0259617, -0.0262528, -0.0270426, -0.0276048, -0.0280794, -0.0287650, -0.0291017,  
     1            -0.0290970, -0.0287552, -0.0269993, -0.0235859, -0.0180673, -0.0150673, -0.0031849, -0.0031849,  
     1            -0.0031849, -0.0031849, -0.0031849, -0.0031849, -0.0031849 /  
      data Mref /  7.68, 7.68, 7.68, 7.71, 7.77, 7.77, 7.78, 7.72, 7.62, 7.54, 7.42, 7.38, 7.36, 7.32, 7.25, 7.25,  
     1            7.25, 7.25, 7.25, 7.25, 7.25 /     
      data a2  /  -1.552846733, -1.554174269, -1.555152194, -1.556049687, -1.554562252, -1.551165488, -1.539140832,  
     1            -1.520764226, -1.489051706, -1.464118878, -1.414761429, -1.383170353, -1.360022278, -1.313716982,  
     1            -1.236841977, -1.100570482, -0.990254902, -0.896093506, -0.818199517, -0.730697376, -0.734817372 /  
      data a14 /  -0.011876681, -0.012409284, -0.016872732, -0.08510905, -0.118005772, -0.171218187, -0.124720279,  
     1            -0.120958201, -0.116255248, -0.077408811, -0.054966213, -0.034173086, -0.06069315, -0.039053473,  
     1            0.017806808, -0.005705423, 0.053155037, 0.068765677, 0.071577687, 0.042405486, 0.054712361 /  
      data dela1 /  1.141899742, 1.152006702, 1.154339185, 1.515303155, 1.904431142, 1.945456526, 1.787100626,  
     1            1.562515125, 1.356740101, 1.206013896, 0.760110718, 0.431629072, 0.214072689, -0.01782956, -0.204991951, 
     1             -0.382378342, -0.352611734, -0.228719047, -0.16534756, 0.010400184, 0.135306871 /  
      data dela4 /  0.328613796, 0.352192886, 0.367677006, 0.452541112, 0.513739193, 0.499522237, 0.45427803,  
     1            0.363869484, 0.314270529, 0.282854636, 0.176621233, 0.077343234, 0.044354701, -0.012346815, -0.083175191, 
     1             -0.226959428, -0.206168741, -0.163340854, -0.154799226, -0.112793712, -0.003105582 /  
      data a6jp /  -0.006794362, -0.006817094, -0.006816137, -0.007285287, -0.007702243, -0.007674043, -0.00782682,  
     1            -0.007547403, -0.007323965, -0.006976346, -0.006143907, -0.005504091, -0.004739568, -0.004285202,  
     1            -0.003957479, -0.003379651, -0.003469497, -0.003409644, -0.003614492, -0.003749858, -0.003243671 /  
      data a12jp /  -0.7516020, -0.7500948, -0.7307185, -0.4831132, -0.3413025, -0.4948081, -0.8669192, -1.0634892,  
     1            -1.1789740, -1.2253631, -1.2073943, -1.1299835, -1.0859780, -1.0233555, -0.9766258, -0.9437327, -0.8880212,  
     1            -0.8546554, -0.7803988, -0.6937169, -0.6499105 /  
      data a8jp /  0.002650526, 0.002810000, 0.002376243, 0.000548389, 0.003562477, 0.006709533, 0.007165476, 0.004341874,  
     1            0.004771685, 0.005747192, 0.005294191, 0.002624524, 0.00028237, -0.001824156, -0.003649632, -0.005143864,  
     1            -0.005623872, -0.004503918, -0.005225851, -0.006465579, -0.004203115 /  
      data tau0 /  0.426469333, 0.424670673, 0.429099403, 0.477262493, 0.516365961, 0.512863781, 0.461620132, 0.441284014,  
     1            0.434871493, 0.417105574, 0.412231943, 0.396422531, 0.403512982, 0.409058414, 0.428087961, 0.440164103, 
     1            0.451402135, 0.461009777, 0.457681279, 0.470193371, 0.461834624 /  
      data phisstj /  0.420489356, 0.420220542, 0.418935068, 0.419356601, 0.41012804, 0.420066336, 0.433030041, 0.446059203,  
     1            0.456165064, 0.459407794, 0.452177558, 0.443446716, 0.4378372, 0.446095737, 0.44128784, 0.42093392,  
     1            0.425797447, 0.419978946, 0.413207757, 0.36936182, 0.349936999 /  
      data phis2stj /  0.364038777, 0.364065617, 0.364403496, 0.417921293, 0.469674634, 0.469076147, 0.437340846, 0.397161802, 
     1             0.384511586, 0.373773022, 0.372643333, 0.369406327, 0.397812841, 0.419424271, 0.408446403, 0.420315802,  
     1            0.416797894, 0.404296599, 0.362727332, 0.354026172, 0.300771328 /  
      data a1tw /  4.481424147, 4.500413862, 4.524812489, 4.684812623, 4.81707322, 4.943259845, 5.09134277, 5.090645572,  
     1            4.96517978, 4.84672774, 4.616816523, 4.435687208, 4.265016233, 3.916493523, 3.163705631, 2.254253444,  
     1            1.320828841, 0.510914193, -0.121327899, -1.029553279, -1.391905014 /   
      data a4tw /  0.441987425, 0.442328411, 0.436082809, 0.363261281, 0.319469336, 0.325896968, 0.350561168, 0.401101385,  
     1            0.440779304, 0.486141867, 0.593885499, 0.719248494, 0.848115514, 0.96522535, 1.174894012, 1.360979471,  
     1            1.38307024, 1.382803719, 1.391730855, 1.36799257, 1.379913313 /  
      data a7 /  0.681875399, 0.679916748, 0.697566565, 1.036117503, 1.229932174, 1.534436374, 1.263659339, 1.177341019,  
     1            1.046187707, 0.783076472, 0.56945524, 0.437054838, 0.497762038, 0.262897537, -0.126754198, -0.121313834, 
     1            -0.496676074, -0.513466165, -0.600511124, -0.425097306, -0.528599974 /  
      data a6tw /  -0.000639314, -0.000607826, -0.000577165, -0.000490096, -0.00042309, -0.000361045, -0.000251542,  
     1            -0.000161014, -8.90E-05, -3.54E-05, 1.45E-05, -4.21E-05, -7.26E-05, -0.000119483, -0.000199116,  
     1            -0.000362196, -0.000611716, -0.000869846, -0.00106641, -0.001185004, -0.0009885 /  
      data a12tw /  -0.4528715, -0.4516550, -0.4403449, -0.2766783, -0.2833841, -0.3205012, -0.4471684, -0.5552021,  
     1            -0.6466667, -0.7124316, -0.7599690, -0.7702118, -0.8037457, -0.8730668, -0.9821700, -1.0045641, -0.9337591,  
     1            -0.9174852, -0.9334706, -0.8808471, -0.9343411 /  
      data a8 / -0.074484253, -0.074417449, -0.075013122, -0.105054464, -0.116912997, -0.116149188, -0.107454957,  
     1            -0.091894335, -0.071460235, -0.056184877, -0.006874342, 0.040262618, 0.071577385, 0.090332895, 0.125873184, 
     1             0.157605114, 0.159496266, 0.149672181, 0.129557502, 0.105217306, 0.103793858 /  
      data a10 / 0.016025291, 0.017193978, 0.01828222, 0.020842842, 0.022162011, 0.022757257, 0.021797461, 0.020180594,  
     1            0.018556649, 0.016978648, 0.014555899, 0.012627818, 0.011191399, 0.009211979, 0.006851124, 0.003814084,  
     1            0.001733925, 0, 0, 0, 0 /  
      data a11 /  0.014951807, 0.014930723, 0.01491224, 0.01487185, 0.014854753, 0.014852358, 0.014893477, 0.015004213, 
     1             0.015194298, 0.01540766, 0.015952307, 0.016437613, 0.01652538, 0.016212382, 0.015784785, 0.01399451, 
     1             0.011927777, 0.009749305, 0.007785629, 0.00494863, 0.003408571 /  
      data tautw /  0.352252822, 0.349216437, 0.344782755, 0.355375576, 0.380828528, 0.388526115, 0.368100637, 0.368643954,  
     1            0.375291842, 0.365828808, 0.383635154, 0.378521294, 0.369787955, 0.375567859, 0.375762655, 0.39959416,  
     1            0.411391314, 0.428877733, 0.432913094, 0.435941037, 0.415321618 /  
      data phisstw /  0.406623005, 0.406228152, 0.404481748, 0.39813076, 0.387738619, 0.39878984, 0.420231185, 0.435630455, 
     1            0.443766425, 0.447739335, 0.440001589, 0.434476747, 0.427216336, 0.437722887, 0.430194359, 0.403473557,  
     1            0.417620732, 0.414874982, 0.40474639, 0.366802753, 0.331482122 /  
      data phis2stw /  0.342245626, 0.342125322, 0.34182419, 0.392290454, 0.441948427, 0.450860943, 0.409095063, 0.374262742, 
     1             0.352612548, 0.342556195, 0.34961642, 0.351721641, 0.380666872, 0.396790672, 0.384546494, 0.38643462,  
     1            0.383847635, 0.37473218, 0.351739376, 0.343596247, 0.348349141 /  
  
C Constant parameters            

      c4 = 10
      a3 = 0.1
      a9 = 0.25

C     regionflag     Note
C     -------------------------
C      0         for Japan+Taiwan
C      1         for Taiwan
C

C Find the requested spectral period and corresponding coefficients
      nPer = 21

C First check for the PGA case 
      if (specT .eq. 0.0) then
         i1=1
         period1 = period(i1)
         a5T =        a5(i1)        
         a13T =       a13(i1)       
         MrefT =      Mref(i1)      
         a2T =        a2(i1)        
         a14T =       a14(i1)       
         dela1T =     dela1(i1)     
         dela4T =     dela4(i1)     
         a6jpT =      a6jp(i1)      
         a12jpT =     a12jp(i1)     
         a8jpT   =     a8jp(i1)          
         phisstjT =   phisstj(i1)   
         phis2stjT =  phis2stj(i1)  
         tau0T =      tau0(i1)      
         a1twT =        a1tw(i1)        
         a4twT =        a4tw(i1)        
         a7T =        a7(i1)        
         a6twT =        a6tw(i1)        
         a12twT =       a12tw(i1)       
         a8T   =       a8(i1)            
         a11T =       a11(i1)       
         a10T =       a10(i1)       
         phisstwT =   phisstw(i1)   
         phis2stwT =  phis2stw(i1)  
         tautwT =     tautw(i1)     
         phitwT =     phitw(i1)     

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
c     write statement removed for R package compliance
c     write statement removed for R package compliance
c     write statement removed for R package compliance
c     write statement removed for R package compliance
c     write statement removed for R package compliance
c     write statement removed for R package compliance
c     write statement removed for R package compliance
c     write statement removed for R package compliance
c     write statement removed for R package compliance
c     write statement removed for R package compliance
      return

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),a5(count1), a5(count2),
     +                 specT, a5T, iflag)
      call S24_interp (period(count1),period(count2),a13(count1), a13(count2),
     +                 specT, a13T, iflag)
      call S24_interp (period(count1),period(count2),Mref(count1), Mref(count2),
     +                 specT, MrefT, iflag)
      call S24_interp (period(count1),period(count2),a2(count1), a2(count2),
     +                 specT, a2T, iflag)
      call S24_interp (period(count1),period(count2),a14(count1), a14(count2),
     +                 specT, a14T, iflag)

      call S24_interp (period(count1),period(count2),dela1(count1), dela1(count2),
     +                 specT, dela1T, iflag)
      call S24_interp (period(count1),period(count2),dela4(count1), dela4(count2),
     +                 specT, dela4T, iflag)

      call S24_interp (period(count1),period(count2),a6jp(count1), a6jp(count2),
     +                 specT, a6jpT, iflag)
      call S24_interp (period(count1),period(count2),a12jp(count1), a12jp(count2),
     +                 specT, a12jpT, iflag)
      call S24_interp (period(count1),period(count2),a8jp(count1), a8jp(count2),
     +                 specT, a8jpT, iflag)
  
      call S24_interp (period(count1),period(count2),phisstj(count1), phisstj(count2),
     +                 specT, phisstjT, iflag)
      call S24_interp (period(count1),period(count2),phis2stj(count1), phis2stj(count2),
     +                 specT, phis2stjT, iflag)       
      call S24_interp (period(count1),period(count2),tau0(count1), tau0(count2),
     +                 specT, tau0T, iflag)

      call S24_interp (period(count1),period(count2),a1tw(count1), a1tw(count2),
     +                 specT, a1twT, iflag)
      call S24_interp (period(count1),period(count2),a4tw(count1), a4tw(count2),
     +                 specT, a4twT, iflag)
      call S24_interp (period(count1),period(count2),a7(count1), a7(count2),
     +                 specT, a7T, iflag)
  
      call S24_interp (period(count1),period(count2),a6tw(count1), a6tw(count2),
     +                 specT, a6twT, iflag)
      call S24_interp (period(count1),period(count2),a12tw(count1), a12tw(count2),
     +                 specT, a12twT, iflag)
      call S24_interp (period(count1),period(count2),a8(count1), a8(count2),
     +                 specT, a8T, iflag)

      call S24_interp (period(count1),period(count2),a10(count1), a10(count2),
     +                 specT, a10T, iflag)
      call S24_interp (period(count1),period(count2),a11(count1), a11(count2),
     +                 specT, a11T, iflag)

      call S24_interp (period(count1),period(count2),tautw(count1), tautw(count2),
     +                 specT, tautwT, iflag)

      call S24_interp (period(count1),period(count2),phisstw(count1), phisstw(count2),
     +                 specT, phisstwT, iflag)
      call S24_interp (period(count1),period(count2),phis2stw(count1), phis2stw(count2),
     +                 specT, phis2stwT, iflag)       


 1011 period1 = specT                                                                                                              

C     Regional term
      if(ftype .eq. 0.0) then 
        fevt = 0.0
      elseif(ftype .eq. 1.0) then 
       fevt = 1.0
      endif

C  Regional term and  Basin Depth term
      if(regionflag .eq. 0) then
       
        a1 = a1twT + dela1T
        a4 = a4twT + dela4T
        a6 = a6jpT
        a12 = a12jpT

        tau = tau0T
        phiss = phisstjT
        phis2s = phis2stjT

      elseif(regionflag .eq. 1) then
       
        a1 = a1twT
        a4 = a4twT
        a6 = a6twT
        a12 = a12twT

        tau = tautwT
        phiss = phisstwT
        phis2s = phis2stwT
  
      endif

C     Magnitude Scaling
      if (mag .le. MrefT ) then
        fmag = a4*(mag-MrefT) + a13T*(10.0-mag)**2.0
      else
        fmag = a5T*(mag-MrefT) + a13T*(10.0-mag)**2.0
      endif 
   
C     Ztor Scaling        
      if  (ftype .eq. 0.0 ) then
         fztor = a10T *(min(Ztor,40.0)-20)
      elseif (ftype .eq. 1.0 ) then
         fztor = a11T *(min(Ztor,80.0)-40)
      endif
      
C     Path Scaling
       R = rRup + c4*exp( (mag-6.0)*a9 ) 
       frup = a1 + a7T*fevt +(a2T + a14T*fevt + a3*(mag - 7.8))*alog(R) + a6*rRup 
     
C     Site Effect
       fsite = a12*min(alog(vs30/760.0),0.0)

C   Basin Depth term
      if(regionflag .eq. 1) then
       
        Ez1 = exp(-4.06/2.0 * alog((vs30**2.0 + 352.7**2.0)/(1750.0**2.0 + 352.7**2.0)))
        fz10 = a8T*(min(alog(Z10*1000.0/Ez1),0.0))     
  
      else
  
        Ez1 = exp(-5.23/2.0 * alog((vs30**2.0 + 412.39**2.0)/(1360.0**2.0 + 412.39**2.0)))
        fz10 = a8jpT*(min(alog(Z10*1000.0/Ez1),0.0))     
  
      endif       


       lnSa = fmag + frup + fztor + fsite + fz10 
   
C     Set sigma values to return
C       tau = tau1T
       sigma = sqrt(tau**2+phiSS**2+phiS2S**2)
    
c     write(*,*) "fz10 = ", fz10
c      write(*,*) "fmag = ", fmag
c     write(*,*) "X = ", frup
c     write(*,*) "fsite = ", fsite
c     write(*,*) "fztor = ", fztor
c     write(*,*) "lnSa = ", lnSa
c     write(*,*) "Sa = ", exp(lnSa)
 
C     Convert ground motion to units of gals.
      lnY = lnSa + 6.89
      period2 = period1
      return
      END
