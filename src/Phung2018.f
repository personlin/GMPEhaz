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
      write (*,*) 
      write (*,*) 'Phung et al. 2018 horizontal'
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
      real period(MAXPER), a5(MAXPER), a13(MAXPER), Mref(MAXPER), a2(MAXPER), a6tw(MAXPER), a12tw(MAXPER), 
     1     a6tj(MAXPER), a12tj(MAXPER), a1tj(MAXPER), a4tj(MAXPER), a1tw(MAXPER), a4tw(MAXPER),  
     1     a11sitw(MAXPER), a11sitj(MAXPER), a11sstw(MAXPER), a11sstj(MAXPER), si(MAXPER), a14(MAXPER),
     1     b12tw(MAXPER), phisstw(MAXPER), phis2stw(MAXPER), tautw(MAXPER), phitw(MAXPER)

      real Fstj(MAXPER), Fstw(MAXPER), phisstj(MAXPER),  phis2stj(MAXPER), tau0(MAXPER), phi0(MAXPER)
      real sigma, lnSa, pgaRock, vs30, rRup, disthypo, mag 

      real a5T, a13T, MrefT, a2T, a6twT, a12twT, a6tjT, a12tjT, a1tjT, a4tjT, a1twT, a4twT,  
     1     a11sitwT, a11sitjT, a11sstwT, a11sstjT, siT, ssT, dsitwT, dsstwT, dsitjT, dsstjT,  
     1     b12twT, b12tjT, tau1T, phi1T, phisstwT, phis2stwT, a14T, tau0T, tautwT, phitwT
      real phisstjT, phis2stjT, FstjT, FstwT
      real Ez1, fz10, fmag, frup, fsite, fztor, fevt
      real period1, a3, Z10, ZTor, a9, d, b12, a1, a4, a6, a12, lnY, Fs, a11si, a11ss, phiss, phis2s
      integer count1, count2, iflag, regionflag
      real n, c, c4, c1, faba, R, depth, specT, tau, phi, ftype, period2


      data period  /0, 0.01, 0.02, 0.05, 0.08, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.75, 1, 1.5, 2, 
     1              2.5, 3, 4, 5 /
      data a5 / 0.038499, 0.040337, 0.041902, 0.045094, 0.046231, 0.048197, 0.043251, 0.036921, 0.065973, 
     1          0.061979, 0.069796, 0.087838, 0.096129, 0.106129, 0.287445, 0.161366, 0.227672, 0.271534, 
     1          0.288221, 0.325893, 0.263839 /
      data a13 / -0.025657, -0.025962, -0.026253, -0.027043, -0.027605, -0.028079, -0.028765, -0.029102, 
     1          -0.029097, -0.028755, -0.026999, -0.023586, -0.018067, -0.015067, -0.003185, -0.003185, 
     1          -0.003185, -0.003185, -0.003185, -0.003185, -0.003185 /
      data Mref / 7.680000, 7.680000, 7.680000, 7.710000, 7.770000, 7.770000, 7.780000, 7.720000, 7.620000, 
     1          7.540000, 7.420000, 7.380000, 7.360000, 7.320000, 7.250000, 7.250000, 7.250000, 7.250000,  
     1          7.250000, 7.250000, 7.250000 /
      data a2  / -1.552847, -1.554174, -1.555152, -1.556050, -1.554562, -1.551165, -1.539141, -1.520764,  
     1          -1.489052, -1.464119, -1.414761, -1.383170, -1.360022, -1.313717, -1.236842, -1.100570,  
     1          -0.990255, -0.896094, -0.818200, -0.730697, -0.734817 /
      data a14 / -0.011877, -0.012409, -0.016873, -0.085109, -0.118006, -0.171218, -0.124720, -0.120958,  
     1          -0.116255, -0.077409, -0.054966, -0.034173, -0.060693, -0.039053, 0.017807, -0.005705, 0.053155,  
     1          0.068766, 0.071578, 0.042405, 0.054712 /
      data a1tj / 5.101731, 5.069611, 5.045603, 5.441538, 5.765867, 5.888591, 5.847458, 5.690993, 5.429052,  
     1          5.185892, 4.691791, 4.308839, 4.034122, 3.588919, 2.739479, 1.797243, 0.923067, 0.195608,  
     1          -0.403213, -1.228062, -1.644441 /
      data a4tj / 0.623076, 0.634268, 0.631555, 0.611334, 0.609526, 0.609657, 0.609425, 0.610344, 0.602125,  
     1          0.619701, 0.642647, 0.694130, 0.778654, 0.855505, 1.037249, 1.186231, 1.245764, 1.284802,  
     1          1.294268, 1.294937, 1.295144 /
      data Fstj / 0.802011, 0.942645, 1.073296, 1.401083, 1.527200, 1.743499, 1.629272, 1.508393, 1.294569,  
     1          1.120307, 0.833457, 0.702720, 0.650526, 0.523316, 0.218044, 0.143774, -0.225026, -0.335028,  
     1          -0.371389, -0.357682, -0.321480 /
      data a6tj / -0.004499, -0.004510, -0.004525, -0.005052, -0.005134, -0.005050, -0.005069, -0.004902,  
     1          -0.004669, -0.004459, -0.003837, -0.003268, -0.002810, -0.002494, -0.002224, -0.001889, -0.001974,  
     1          -0.002034, -0.002240, -0.002496, -0.002228 /
      data a12tj / -0.636493, -0.635738, -0.621258, -0.420142, -0.353911, -0.451090, -0.706902, -0.865063,  
     1          -0.966042, -1.013893, -1.008335, -0.979263, -0.960689, -0.964936, -0.971128, -0.957234, -0.891432,  
     1          -0.861360, -0.817111, -0.750452, -0.713205 /
      data a11sitj / 0.030564, 0.031786, 0.032877, 0.035377, 0.036600, 0.037079, 0.035081, 0.031794, 0.028666,  
     1          0.025868, 0.022015, 0.019200, 0.016767, 0.013586, 0.009534, 0.006945, 0.006785, 0.005715,  
     1          0.004900, 0.004182, 0.004948 /
      data a11sstj / 0.017116, 0.016997, 0.016918, 0.016868, 0.017004, 0.017442, 0.017432, 0.017470, 0.017572,  
     1          0.017992, 0.019086, 0.019882, 0.019916, 0.019066, 0.017061, 0.015984, 0.013805, 0.011171,  
     1          0.009134, 0.006104, 0.003654 /
      data tau0 / 0.426469, 0.424671, 0.429099, 0.477262, 0.516366, 0.512864, 0.461620, 0.441284, 0.434871,  
     1          0.417106, 0.412232, 0.396423, 0.403513, 0.409058, 0.428088, 0.440164, 0.451402, 0.461010,  
     1          0.457681, 0.470193, 0.461835 /
      data phi0 / 0.625584, 0.626331, 0.626796, 0.674146, 0.724290, 0.744670, 0.723934, 0.696141, 0.674401,  
     1          0.653173, 0.628760, 0.616929, 0.619943, 0.617070, 0.610282, 0.623222, 0.625040, 0.620947,  
     1          0.613013, 0.581569, 0.543203 /
      data phisstj / 0.420489, 0.420221, 0.418935, 0.419357, 0.410128, 0.420066, 0.433030, 0.446059, 0.456165,  
     1          0.459408, 0.452178, 0.443447, 0.437837, 0.446096, 0.441288, 0.420934, 0.425797, 0.419979,  
     1          0.413208, 0.369362, 0.349937 /
      data phis2stj / 0.364039, 0.364066, 0.364403, 0.417921, 0.469675, 0.469076, 0.437341, 0.397162, 0.384512,  
     1          0.373773, 0.372643, 0.369406, 0.397813, 0.419424, 0.408446, 0.420316, 0.416798, 0.404297,  
     1          0.362727, 0.354026, 0.300771 /
      data a1tw / 4.481424, 4.500414, 4.524812, 4.684813, 4.817073, 4.943260, 5.091343, 5.090646, 4.965180,  
     1          4.846728, 4.616817, 4.435687, 4.265016, 3.916494, 3.163706, 2.218250, 1.230946, 0.352002, -0.322253,  
     1          -1.279441, -1.615641 /
      data Fstw / 0.594658, 0.697341, 0.793044, 1.036984, 1.189764, 1.440826, 1.309325, 1.249431, 1.124772, 0.892540,  
     1          0.595750, 0.394155, 0.394614, 0.195256, -0.037137, 0.004383, -0.166134, -0.188027, -0.152993, 0.050871,  
     1          -0.011097 /
      data a4tw / 0.441987, 0.442328, 0.436083, 0.363261, 0.319469, 0.325897, 0.350561, 0.401101, 0.440779, 0.486142,  
     1          0.593885, 0.719248, 0.848116, 0.965225, 1.174894, 1.380979, 1.383070, 1.382804, 1.391731, 1.367993,  
     1          1.379913 /
      data a6tw / -0.000639, -0.000608, -0.000577, -0.000490, -0.000423, -0.000361, -0.000252, -0.000161, -0.000089,  
     1          -0.000035, 0.000015, -0.000042, -0.000073, -0.000119, -0.000199, -0.000362, -0.000612, -0.000870,  
     1          -0.001066, -0.001185, -0.000989 /
      data a12tw / -0.452872, -0.451655, -0.440345, -0.276678, -0.283384, -0.320501, -0.447168, -0.555202, -0.646667,  
     1          -0.712432, -0.759969, -0.770212, -0.803746, -0.873067, -0.982170, -1.004564, -0.933759, -0.917485,  
     1          -0.933471, -0.880847, -0.934341 /
      data b12tw / -0.074484, -0.074417, -0.075013, -0.105054, -0.116913, -0.116149, -0.107455, -0.091894, -0.071460,  
     1          -0.056185, -0.006874, 0.040263, 0.071577, 0.090333, 0.125873, 0.157605, 0.159496, 0.149672, 0.129558,  
     1          0.105217, 0.103794 /
      data a11sitw / 0.016025, 0.017194, 0.018282, 0.020843, 0.022162, 0.022757, 0.021797, 0.020181, 0.018557, 0.016979,  
     1          0.014556, 0.012628, 0.011191, 0.009212, 0.006851, 0.003814, 0.001734, 0.000000, 0.000000,  
     1          0.000000, 0.000000 /
      data a11sstw / 0.020989, 0.020983, 0.020977, 0.020953, 0.020930, 0.020903, 0.020839, 0.020761, 0.020669,  
     1          0.020562, 0.020308, 0.019998, 0.019637, 0.019004, 0.017719, 0.013674, 0.009480, 0.005510, 0.002949,  
     1          0.000069, 0.000223 /
      data tautw / 0.368263, 0.355385, 0.348466, 0.361185, 0.391058, 0.401912, 0.375489, 0.375383, 0.381440, 0.370257,  
     1          0.387105, 0.383997, 0.386832, 0.383539, 0.366775, 0.379949, 0.414423, 0.431741, 0.456680, 0.458697,  
     1          0.468655 /
      data phitw / 0.576956, 0.576762, 0.576761, 0.620153, 0.656480, 0.664586, 0.637974, 0.613661, 0.604423,  
     1          0.595988, 0.583083, 0.578565, 0.590642, 0.602284, 0.589549, 0.585119, 0.585115, 0.578593,  
     1          0.568072, 0.527620, 0.499870 /
      data phisstw / 0.413866, 0.413468, 0.412156, 0.410171, 0.400980, 0.410337, 0.428868, 0.444515, 0.451174,  
     1          0.454197, 0.443391, 0.438383, 0.434260, 0.442987, 0.438376, 0.412671, 0.412671, 0.412671, 0.412671,  
     1          0.412671, 0.412671 /
      data phis2stw / 0.348654, 0.348528, 0.348056, 0.398122, 0.447502, 0.457332, 0.415688, 0.381265, 0.361728,  
     1          0.352056, 0.356687, 0.360011, 0.390333, 0.402169, 0.391383, 0.400807, 0.400807, 0.400807, 0.400807,  
     1          0.400807, 0.400807 /
	 
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
         a5T = a5(i1)
         a13T = a13(i1)
         MrefT = Mref(i1)
         a2T = a2(i1)
         a14T = a14(i1)

         a1tjT = a1tj(i1)
         a4tjT = a4tj(i1)
         FstjT = Fstj(i1)
         a6tjT = a6tj(i1)
         a12tjT = a12tj(i1)
         a11sitjT = a11sitj(i1)
         a11sstjT = a11sstj(i1)
         phisstjT = phisstj(i1)
         phis2stjT = phis2stj(i1)
         tau0T = tau0(i1)
		 
         a1twT = a1tw(i1)
         a4twT = a4tw(i1)
         FstwT = Fstw(i1)
         a6twT = a6tw(i1)
         a12twT = a12tw(i1)
         b12twT = b12tw(i1)
         a11sitwT = a11sitw(i1)
         a11sstwT = a11sstw(i1)
		 
         tautwT = tautw(i1)
         phitwT = phitw(i1)

         phisstwT = phisstw(i1)
         phis2stwT = phis2stw(i1)
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
      write (*,*) 'Phung et al. Subduction (2018 Model) Horizontal'
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

      call S24_interp (period(count1),period(count2),a1tj(count1), a1tj(count2),
     +                 specT, a1tjT, iflag)
      call S24_interp (period(count1),period(count2),a4tj(count1), a4tj(count2),
     +                 specT, a4tjT, iflag)
      call S24_interp (period(count1),period(count2),Fstj(count1), Fstj(count2),
     +                 specT,FstjT, iflag)
      call S24_interp (period(count1),period(count2),a6tj(count1), a6tj(count2),
     +                 specT, a6tjT, iflag)
      call S24_interp (period(count1),period(count2),a12tj(count1), a12tj(count2),
     +                 specT, a12tjT, iflag)
      call S24_interp (period(count1),period(count2),a11sitj(count1), a11sitj(count2),
     +                 specT, a11sitjT, iflag)
      call S24_interp (period(count1),period(count2),a11sstj(count1), a11sstj(count2),
     +                 specT, a11sstjT, iflag)
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
      call S24_interp (period(count1),period(count2),Fstw(count1), Fstw(count2),
     +                 specT,FstwT, iflag)
      call S24_interp (period(count1),period(count2),a6tw(count1), a6tw(count2),
     +                 specT, a6twT, iflag)
      call S24_interp (period(count1),period(count2),a12tw(count1), a12tw(count2),
     +                 specT, a12twT, iflag)
      call S24_interp (period(count1),period(count2),b12tw(count1), b12tw(count2),
     +                 specT, b12twT, iflag)
      call S24_interp (period(count1),period(count2),a11sitw(count1), a11sitw(count2),
     +                 specT, a11sitwT, iflag)
      call S24_interp (period(count1),period(count2),a11sstw(count1), a11sstw(count2),
     +                 specT, a11sstwT, iflag)


      call S24_interp (period(count1),period(count2),tautw(count1), tautw(count2),
     +                 specT, tautwT, iflag)
      call S24_interp (period(count1),period(count2),phitw(count1), phitw(count2),
     +                 specT, phitwT, iflag)

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
       
        a1 = a1tjT
        a4 = a4tjT
        a6 = a6tjT
        a12 = a12tjT
        Fs = FstjT

        a11si = a11sitjT
        a11ss = a11sstjT
		
        b12 = 0.0
        tau = tau0T
        phiss = phisstjT
        phis2s = phis2stjT

      elseif(regionflag .eq. 1) then
       
        a1 = a1twT
        a4 = a4twT
        a6 = a6twT
        a12 = a12twT
        Fs = FstwT
		
        a11si = a11sitwT
        a11ss = a11sstwT
		
        b12 = b12twT
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
         fztor = a11si *(min(Ztor,40.0)-20)
      elseif (ftype .eq. 1.0 ) then
         fztor = a11ss *(min(Ztor,80.0)-40)
      endif
      
C     Path Scaling
       R = rRup + c4*exp( (mag-6.0)*a9 ) 
       frup = a1 + Fs*fevt +(a2T + a14T*fevt + a3*(mag - 7.8))*alog(R) + a6*rRup 
     
C     Site Effect
       fsite = a12*min(alog(vs30/760.0),0.0)
       


C   Basin Depth term
      if(regionflag .eq. 1) then
       
        Ez1 = exp(-2.63/4.0 * alog((vs30**4.0 + 253.0**4.0)/(2492.0**4.0 + 253.0**4.0)))
        fz10 = b12*(min(alog(Z10*1000.0/Ez1),0.0))     
		
      else
		
        fz10 = 0.0
		
      endif       


       lnSa = fmag + frup + fztor + fsite + fz10 
	  
C     Set sigma values to return
       tau = tau1T
       sigma = sqrt(tau**2+phiSS**2+phiS2S**2)
	   
c  	  write(*,*) "fz10 = ", fz10
c      write(*,*) "fmag = ", fmag
c  	  write(*,*) "X = ", frup
c  	  write(*,*) "fsite = ", fsite
c  	  write(*,*) "fztor = ", fztor
c  	  write(*,*) "lnSa = ", lnSa
c  	  write(*,*) "Sa = ", exp(lnSa)
	
C     Convert ground motion to units of gals.
      lnY = lnSa + 6.89
      period2 = period1
      return
      END