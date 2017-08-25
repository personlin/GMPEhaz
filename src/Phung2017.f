      Subroutine PhungCrust2017 ( m, Rrup, specT, period2, lnY, sigma, iflag, 
     1                     vs, Delta, DTor, Ftype, depthvs10, vs30_class,
     2                       regionflag, msasflag, phi, tau )


      implicit none
      
      integer MAXPER, i, nPer
      parameter (MAXPER=23)
      REAL Period(MAXPER), C1(MAXPER), C1a(MAXPER), C1b(MAXPER), C1c(MAXPER), C1d(MAXPER)
      REAL cn(MAXPER), cm(MAXPER), c3(MAXPER), c5(MAXPER), c6(MAXPER)
      REAL c7(MAXPER), C7b(MAXPER), C11(MAXPER), C11b(MAXPER), CHM(MAXPER)
      REAL phi1(MAXPER), phi2(MAXPER), phi3(MAXPER), phi4(MAXPER), phi5(MAXPER)
      REAL sigma1inf(MAXPER), sigma2inf(MAXPER)
      REAL tau1(MAXPER), tau2(MAXPER), sigma1, sigma2
      REAL sigma3(MAXPER), c9a(MAXPER), deltac5(MAXPER)
      REAL cgglb1(MAXPER), cgglb2(MAXPER), cgglb3(MAXPER), cge(MAXPER)
      Real tauT1(MAXPER), phiT1(MAXPER), cgtw1(MAXPER), cgtw2(MAXPER), cgtw3(MAXPER)
      real phiss(MAXPER), phis2s(MAXPER), sigma1mea(MAXPER), sigma2mea(MAXPER)
      real phiss1M(MAXPER), phiss2M(MAXPER)
      real phiglb1(MAXPER),phiglb2(MAXPER),phiglb3(MAXPER),phitw1(MAXPER),phitw2(MAXPER),phitw3(MAXPER)
      real vs, phi6
      real Finferred, Fmeasured
      REAL c1T, c1aT, c1bT, c1cT, c1dT,cnT, cmT, c5T, c6T, c3T
      REAL phi1T, phi2T, phi3T, phi4T, cgeT, deltac5T, sigma3T
      REAL phi5T, tau1T, tau2T, phiT, tauT
      real c7T, c7bT, c11T, c11bT, cHMT, cgglb1T, cgglb2T, cgglb3T
      real cgtw1T, cgtw2T, cgtw3T, sigma1infT, sigma2infT
      real phiglb1T,phiglb2T,phiglb3T,phitw1T,phitw2T,phitw3T
      REAL c2, c4, c4a, cRB, pi, d2r, term14, term15, term16, NL0
      REAL term1, term2, term3, term5, term4, term6, term8, term9, term10 
      REAL phissT, phis2sT, sigma1meaT, sigma2meaT, phiss1MT, phiss2MT
      real CNS, cosdelta, psa_ref, psa, cg1T, cg2T, cg3T
      integer iflag, count1, count2, vs30_class, regionflag, msasflag
      REAL M, RRUP, DTOR, Delta, specT, sigma, Ftype
      REAL period2, lnY, F_RV, F_NM, tau, phi, rkdepth
      real c8, c8a, c8bT, fd, lnpsa_ref, lnpsa, sa
      real sigmaNL0, F_Measured, F_Inferred, mz_TOR, deltaZ_TOR, coshM
      real period1 ,Ez1, term7, deltaZ1, depthvs10

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

      data period / 0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.075, 0.1, 0.12, 0.15, 0.17, 0.2, 
     1            0.25, 0.3, 0.4, 0.5, 0.75, 1, 1.5, 2, 3, 4, 5.0 /
      
      
      data c1  / -1.5095, -1.5137, -1.4795, -1.3729, -1.2484, -1.0969, -0.7407, -0.5699, -0.5059, 
     1           -0.5061, -0.5538, -0.5932, -0.7551, -0.9194, -1.2439, -1.4716, -2.0247, -2.3813,  
     1           -2.9856, -3.3075, -3.7822, -4.0489, -4.2928  / 
      data c3  / 1.2468, 1.253, 1.2231, 1.1917, 1.1374, 1.0586, 1.1135, 1.18, 1.2719, 1.3754, 1.4184,  
     1           1.523, 1.6667, 1.8271, 2.029, 2.2367, 2.5124, 2.6398, 2.7256, 2.7831, 2.8165, 2.8036,  
     1           2.9213  / 
      data cn  / 12.3145, 12.3349, 12.0592, 10.9614, 12.7233, 16.2717, 16.493, 16.1585, 17.1615,  
     1           13.5472, 14.929, 15.8416, 12.7457, 9.6295, 6.6784, 4.5576, 3.4064, 3.1612, 2.8078,  
     1           2.4631, 2.2111, 1.9468, 1.8671  / 
      data cm  / 5.0417, 5.0419, 5.045, 5.0712, 5.0761, 5.0774, 5.0706, 5.0378, 5.031, 5.0801,  
     1           5.0752, 5.0915, 5.133, 5.1763, 5.22, 5.2948, 5.4189, 5.5442, 5.7193, 5.8612, 6.0824,  
     1           6.2674, 6.4197  / 
      data c5  / 6.4551, 6.4551, 6.4551, 6.4551, 6.4551, 6.4551, 6.4551, 6.8305, 7.1333, 7.3621,  
     1           7.4365, 7.4972, 7.54E+00, 7.56, 7.5735, 7.5778, 7.5808, 7.5814, 7.5817, 7.5818,  
     1           7.5818, 7.5818, 7.5818  / 
      data deltac5  / -1.8316, -1.8293, -1.8424, -1.8292, -1.9728, -1.9608, -1.867, -1.7551, -1.7174,  
     1           -1.6555, -1.5751, -1.5154, -1.48E+00, -1.6291, -0.9786, -1.1988, -1.6463, -1.8809,  
     1           -2.3783, -2.5278, -2.1459, -2.3076, -2.3007  / 
      data c6  / 0.4908, 0.4908, 0.4925, 0.4992, 0.5037, 0.5048, 0.5048, 0.5048, 0.5048, 0.5045,  
     1           0.5036, 0.5016, 4.97E-01, 0.4919, 0.4807, 0.4707, 0.4575, 0.4522, 0.4501, 0.45,  
     1           0.45, 0.45, 0.45  / 
      data cHM  / 3.0956, 3.0956, 3.0963, 3.0974, 3.0988, 3.1011, 3.1094, 3.2381, 3.3407, 3.43,  
     1           3.4688, 3.5146, 3.57E+00, 3.6232, 3.6945, 3.7401, 3.7941, 3.8144, 3.8284, 3.833,  
     1           3.8361, 3.8369, 3.8376  / 
      data c7  / 0.0141, 0.0142, 0.0142, 0.0151, 0.015, 0.0157, 0.0158, 0.0169, 0.018, 0.0187,  
     1           0.0194, 0.0187, 1.72E-02, 0.0171, 0.0164, 0.0153, 0.0133, 0.0129, 0.0104, 0.0078,  
     1           0, -0.0149, -0.0208  / 
      data c7b  / 0.0127, 0.0127, 0.0131, 0.0152, 0.0149, 0.0154, 0.0166, 0.0134, 0.0098, 0.0053,  
     1           0.0054, 0.0022, 3.00E-03, 0.0018, -0.0013, 0.0016, 0.0029, 0.004, 0.0034, 0.0067,  
     1           0.0068, 0.0243, 0.037  / 
      data c1a  / 0.1502, 0.1466, 0.1392, 0.1312, 0.1196, 0.0981, 0.1021, 0.1027, 0.1109, 0.1251,  
     1           0.1256, 0.1561, 1.59E-01, 0.1673, 0.1645, 0.2297, 0.2002, 0.215, 0.1334, 0.07,  
     1           0.0373, 0, 0  / 
      data c1c  / -0.0243, -0.0199, -0.0122, 0.0072, 0.0379, 0.0649, 0.0714, 0.0168, -0.0157, -0.0643,  
     1           -0.0658, -0.0806, -5.11E-02, -0.0819, -0.0389, -0.1289, -0.0511, -0.0223, 0.0563,  
     1           0.108, 0.1459, 0.1726, 0.1868  / 
      data c1b  / 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.1549, -0.1815, -0.1998, -0.1209,  
     1           -0.081, -0.0867, -0.0867  / 
      data c1d  / -0.1712, -0.1712, -0.1712, -0.1712, -0.1712, -0.1712, -0.1712, -0.1712, -0.1712,  
     1           -0.1712, -0.1712, -0.1712, -0.1712, -0.1712, -0.1712, -0.1712, -0.1712, -0.1712,  
     1           -0.1712, -0.1712, -0.1712, -0.1712, -0.1712  / 
      data c11  / -0.1234, -0.1072, -0.0854, -0.0852, -0.0787, -0.0522, -0.1591, -0.1788, -0.1599,  
     1           -0.1231, -0.0645, -0.1604, -0.1152, -0.1613, -0.0896, -0.2766, -0.2254, -0.2945,  
     1           -0.0468, 0.0748, 0.10033, 0.19, 0.2357  / 
      data c11b  / -0.1677, -0.1522, -0.1808, -0.2026, -0.2264, -0.2688, -0.0894, 0.0172, -0.0037,  
     1           -0.0467, -0.1003, -0.0431, -0.1682, -0.0271, -0.1761, 0.1431, 0.0775, 0.1553, -0.1704,  
     1           -0.2993, -0.3915, -0.489, -0.2708  / 
      data cgglb1  / -0.00777287, -0.00777287, -7.87E-03, -0.008513485, -0.0090007, -0.0094918,  
     1           -0.010315, -0.010597, -0.0108897, -0.010839, -0.010645, -1.03E-02, -8.79E-03,  
     1           -8.31E-03, -7.19E-03, -0.006207, -4.47E-03, -3.85E-03, -3.11E-03, -3.05E-03,  
     1           -0.0025055, -0.00210659, -0.00194175  / 
      data cgglb2  / -0.008307401, -0.008307401, -0.008224304, -0.008319595, -0.008383, -0.0084453,  
     1           -0.0075936, -0.0061128, -0.0041812, -0.002678, -0.001964, -0.0015011, -0.0013538,  
     1           -0.0010318, -0.0004922, -0.001285, -0.0011589, -0.001824, -0.0014489, -0.0034758,  
     1           -0.024638, -0.019669, -0.0141327  / 
      data cgglb3  / 4.052466395, 4.052466395, 4.043855472, 4.04820785, 4.04402, 4.1283, 4.3754,  
     1           4.6228, 4.8756, 4.9326, 4.95E+00, 4.7776, 5.5509, 5.2382, 5.3261, 4.38, 4.6225,  
     1           4.636, 4.18648, 4.0464, 1.21957, 1.79675, 1.52486  / 
      data cgtw1  / -0.007289154, -0.007289154, -0.007391413, -0.008045598, -0.0084688, -0.00899689,  
     1           -0.01003, -0.010551, -0.011115, -0.011158, -0.0104477, -0.009978, -0.0090725,  
     1           -0.0082395, -0.00699, -0.005702, -0.0044667, -0.003847, -0.003111, -0.002558,  
     1           -0.0018576, -0.0015354, -0.0014361  / 
      data cgtw2  / -0.007103831, -0.007103831, -0.007027146, -0.007258757, -0.0072455, -0.0075512,  
     1           -0.007104, -0.005758, -0.0038376, -0.002273, -0.002101, -0.0015013, -0.00069154,  
     1           -0.0006181, -0.00021158, -0.0008504, -0.0011589, -0.001824, -0.0014489, -0.0027134,  
     1           -0.002776, -0.0087011, -0.007748  / 
      data cgtw3  / 4.22959614, 4.22959614, 4.222091708, 4.218021689, 4.2343, 4.27912, 4.4406, 4.6068,  
     1           4.7495, 4.6934, 4.9878, 5.1215, 5.419, 5.3499, 5.4856, 5.1327, 4.6225, 4.636, 4.18648,  
     1           4.443, 3.94986, 2.4836, 2.1575  / 
      data cge  / -0.002154124, -0.002154962, -0.002161527, -0.002149373, -0.002236978, -0.002258625,  
     1           -0.002171027, -0.002242017, -0.002190855, -0.00216606, -0.002071088, -0.002076124,  
     1           -0.002032334, -0.002000855, -0.001908347, -0.001877458, -0.0017069, -0.001368707,  
     1           -0.001190811, -0.000899898, -0.001337119, -0.00138601, -0.001628408  / 
      data phiglb1  / -0.5088, -0.5088, -0.4995, -0.4863, -0.4686, -4.47E-01, -0.428, -0.4577, -0.4793, 
     1            -0.5078, -0.5298, -0.5513, -0.5801, -0.6311, -0.6803, -0.7113, -0.8294, -0.9068,  
     1           -0.9733, -0.9776, -0.9775, -0.9659, -0.9258  / 
      data phiglb2  / -0.1417, -0.1417, -0.1364, -0.1403, -0.1591, -0.1862, -0.2538, -0.2943, -0.3077, 
     1            -0.3113, -0.3062, -0.2927, -0.2662, -0.2405, -0.1975, -0.1633, -0.1028, -0.0699, 
     1            -0.0425, -0.0302, -0.0129, -0.0016, 0  / 
      data phiglb3  / -0.00701, -0.00701, -0.007279, -0.007354, -0.006977, -0.006467, -0.0057, -0.0056, 
     1            -0.0057, -0.0058, -0.006, -0.0061, -0.0064, -0.0067, -0.0071, -0.0074, -0.0081, 
     1            -0.0084, -0.0077, -0.0048, -0.0018, -0.0015, -0.0014  / 
      data phitw1  / -0.529702044, -0.529656391, -0.521077213, -0.512073039, -0.498112888, -4.88E-01, 
     1            -0.453835736, -0.476664485, -4.96E-01, -0.507200973, -0.519826139, -0.534817785, 
     1            -0.559543859, -0.605127453, -0.67522556, -0.706879609, -0.853566618, -0.883170749, 
     1            -0.964946569, -0.979694815, -0.994511901, -0.997658667, -0.968404707  / 
      data phitw2  / -0.2226, -0.2226, -0.2178, -0.2221, -0.2513, -0.2958, -0.3235, -0.3378, -0.3723, 
     1            -0.3992, -0.3634, -0.4163, -0.3133, -0.3093, -0.2983, -0.2167, -0.2075, -0.0699, 
     1            -0.0425, -0.0302, -0.0129, -0.0016, 0  / 
      data phitw3  / -0.0076, -0.0076, -0.0078, -0.0079, -0.0077, -0.0074, -0.006, -0.0063, -0.0061, 
     1            -0.0056, -0.0061, -0.0055, -0.0075, -0.0077, -0.0079, -0.0092, -0.0095, -0.0084, 
     1            -0.0077, -0.0048, -0.0018, -0.0015, -0.0014  / 
      data phi4   / 0.1022, 0.1022, 0.1084, 0.1199, 0.1336, 0.1489, 0.1906, 0.2307, 0.2532, 0.2665, 
     1            0.2651, 0.2553, 0.2315, 0.2073, 0.1655, 0.1338, 0.0852, 0.0586, 0.0318, 0.0197, 
     1            0.0096, 0.0054, 0.0032  / 
      data phi5  / 0.040582663, 0.040603911, 0.040793653, 0.048275069, 0.057526547, 0.064938488,  
     1           0.082071634, 0.107265026, 0.093297608, 0.070415091, 0.060452567, 0.050253034,  
     1           0.042040485, 0.034861903, 0.0293967, 0.040453687, 0.070852531, 0.086614739,  
     1           0.160460769, 0.22535196, 0.272648022, 0.248210653, 0.221285413  / 
      data tau1  / 0.328705918, 0.32824584, 0.331061862, 0.358256174, 0.381906279, 0.404559567,  
     1           0.388100354, 0.366211703, 0.330648493, 0.329813563, 0.322463538, 0.315621419,  
     1           0.30519909, 0.311612501, 0.359831951, 0.397825282, 0.377486688, 0.39578383,  
     1           0.394229339, 0.405498239, 0.402609845, 0.381774468, 0.342688475  / 
      data tau2  / 0.372296285, 0.37547105, 0.372011554, 0.375978951, 0.390221346, 0.418424682,  
     1           0.474682772, 0.493852233, 0.491612285, 0.468071578, 0.444187615, 0.389796361,  
     1           0.355660861, 0.343225478, 0.310855105, 0.330660711, 0.396500599, 0.420526746,  
     1           0.490908629, 0.469648447, 0.510213646, 0.461885197, 0.465570287  / 
      data sigma3  / 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.7999, 0.7997,  
     1           0.7988, 0.7966, 0.7792, 0.7504, 0.7136, 0.7035, 0.7006, 0.7001, 0.7  / 
      data sigma1mea  / 0.440462645, 0.440237517, 0.440552924, 0.454924612, 0.471237638, 0.500836323,  
     1           0.493188218, 0.472404434, 0.471527522, 0.450726803, 0.447200284, 0.433579051,  
     1           0.434137568, 0.435776383, 0.425775914, 0.413369858, 0.384583422, 0.359153292,  
     1           0.369920274, 0.361673931, 0.362299163, 0.359960063, 0.363572219  / 
      data sigma2mea  / 0.383905228, 0.383988636, 0.385173862, 0.398286282, 0.406936953, 0.419087157,  
     1           0.423852303, 0.432312248, 0.424703436, 0.41528495, 0.41504729, 0.417764253,  
     1           0.420837422, 0.429538727, 0.418804302, 0.431150448, 0.429025695, 0.45285511,  
     1           0.466343119, 0.477325584, 0.461878034, 0.444550934, 0.470269388  / 
      data sigma1inf  / 0.398123649, 0.398039907, 0.395199187, 0.405412008, 0.420115937, 0.421842238,  
     1           0.443501473, 0.455007817, 0.393576914, 0.398285761, 0.401960163, 0.478368061, 
     1           0.501817351, 0.441522586, 0.460174194, 0.422873766, 0.38843105, 0.394761518,  
     1           0.366757709, 0.398227355, 0.437440799, 0.375321005, 0.354401467  / 
      data sigma2inf  / 0.340567681, 0.340684182, 0.347366065, 0.306296924, 0.308395153, 0.29729216,  
     1           0.337069527, 0.320906139, 0.368035681, 0.341855716, 0.393292587, 0.370778536, 
     1            0.382037432, 0.371645616, 0.412904011, 0.385657898, 0.455887057, 0.448865694,  
     1           0.427656647, 0.475169411, 0.440811156, 0.459470633, 0.459536833  / 
      data phiss  / 0.447212907, 0.447102969, 0.446397587, 0.453095524, 0.46011904, 0.464069345,  
     1           0.468531341, 0.463696461, 0.457623166, 0.452211849, 0.52114735, 0.518086792,  
     1           0.524082259, 0.529838949, 0.537774644, 0.546980781, 0.547514088, 0.549963272,  
     1           0.55739441, 0.557527617, 0.545825976, 0.538054814, 0.552545987  / 
      data phis2s  / 0.289092069, 0.289175184, 0.287996213, 0.296191534, 0.310294276, 0.330992478,  
     1           0.36510545, 0.371308208, 0.367758691, 0.347494163, 0.226689915, 0.20097083,  
     1           0.164392064, 0.149065552, 0.138765534, 0.13996281, 0.126256451, 0.120197996,  
     1           0.12257396, 0.129125085, 0.13379128, 0.135468401, 0.133353227  / 
      data phiss1M  / 0.506329173, 0.506540456, 0.504282764, 0.522559148, 0.543975792, 0.564926662,  
     1           0.556630958, 0.537009107, 0.535309967, 0.53305874, 0.537071855, 0.533736576,  
     1           0.55317255, 0.554129774, 0.521646021, 0.488907503, 0.471282128, 0.449064043,  
     1           0.447660372, 0.414099867, 0.442978244, 0.401052695, 0.409456295  / 
      data phiss2M  / 0.415029056, 0.415220494, 0.415874458, 0.418944451, 0.421676471, 0.426776548, 
     1            0.427088307, 0.444029842, 0.432955207, 0.429122746, 0.440129094, 0.444863496,  
     1           0.461534752, 0.467060561, 0.443935541, 0.451123589, 0.464532391, 0.469747607, 
     1            0.487492461, 0.545482623, 0.4770667, 0.555766814, 0.567308177  / 


C Find the requested spectral period and corresponding coefficients
      nPer = 23
C First check for the PGA case (i.e., specT=0.0) 
      if (specT .eq. 0.0) then
         period1  = period(1)
         c1T  =   c1(1)
         c3T  =   c3(1)
         cnT  =   cn(1)
         cmT  =   cm(1)
         c5T  =   c5(1)
         deltac5T  =   deltac5(1)
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
         cgglb1T  =   cgglb1(1)
         cgglb2T  =   cgglb2(1)
         cgglb3T  =   cgglb3(1)
         cgtw1T  =   cgtw1(1)
         cgtw2T  =   cgtw2(1)
         cgtw3T  =   cgtw3(1)
         cgeT  =   cge(1)
         phiglb1T  =   phiglb1(1)
         phiglb2T  =   phiglb2(1)
         phiglb3T  =   phiglb3(1)
         phitw1T  =   phitw1(1)
         phitw2T  =   phitw2(1)
         phitw3T  =   phitw3(1)
         phi4T  =   phi4(1)
         phi5T  =   phi5(1)

         tau1T  =   tau1(1)
         tau2T  =   tau2(1)
         sigma3T  =   sigma3(1)
         sigma1meaT  =   sigma1mea(1)
         sigma2meaT  =   sigma2mea(1)
         sigma1infT  =   sigma1inf(1)
         sigma2infT  =   sigma2inf(1)
         phissT  =   phiss(1)
         phis2sT  =   phis2s(1)
         phiss1MT  =   phiss1M(1)
         phiss2MT  =   phiss2M(1)

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
      write (*,*) 'Phung et al. 2017 horizontal'
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
 1020       call interp (period(count1),period(count2),c1(count1),c1(count2),
     +                   specT,c1T,iflag)
            call interp (period(count1),period(count2),c3(count1),c3(count2),
     +                   specT,c3T,iflag)
            call interp (period(count1),period(count2),cn(count1),cn(count2),
     +                   specT,cnT,iflag)
            call interp (period(count1),period(count2),cm(count1),cm(count2),
     +                   specT,cmT,iflag)
            call interp (period(count1),period(count2),c5(count1),c5(count2),
     +                   specT,c5T,iflag)
            call interp (period(count1),period(count2),c6(count1),c6(count2),
     +                   specT,c6T,iflag)
            call interp (period(count1),period(count2),cHM(count1),cHM(count2),
     +                   specT,cHMT,iflag)
            call interp (period(count1),period(count2),c7(count1),c7(count2),
     +                   specT,c7T,iflag)
            call interp (period(count1),period(count2),c7b(count1),c7b(count2),
     +                   specT,c7bT,iflag)

            call interp (period(count1),period(count2),c1a(count1),c1a(count2),
     +                   specT,c1aT,iflag)
            call interp (period(count1),period(count2),c1b(count1),c1b(count2),
     +                   specT,c1bT,iflag)
            call interp (period(count1),period(count2),c1c(count1),c1c(count2),
     +                   specT,c1cT,iflag)
            call interp (period(count1),period(count2),c1d(count1),c1d(count2),
     +                   specT,c1dT,iflag)
            call interp (period(count1),period(count2),c11(count1),c11(count2),
     +                   specT,c11T,iflag)
            call interp (period(count1),period(count2),c11b(count1),c11b(count2),
     +                   specT,c11bT,iflag)

            call interp (period(count1),period(count2),cgglb1(count1),cgglb1(count2),
     +                   specT,cgglb1T,iflag)
            call interp (period(count1),period(count2),cgglb2(count1),cgglb2(count2),
     +                   specT,cgglb2T,iflag)
            call interp (period(count1),period(count2),cgglb3(count1),cgglb3(count2),
     +                   specT,cgglb3T,iflag)
            call interp (period(count1),period(count2),cgtw1(count1),cgtw1(count2),
     +                   specT,cgtw1T,iflag)
            call interp (period(count1),period(count2),cgtw2(count1),cgtw2(count2),
     +                   specT,cgtw2T,iflag)
            call interp (period(count1),period(count2),cgtw3(count1),cgtw3(count2),
     +                   specT,cgtw3T,iflag)
            call interp (period(count1),period(count2),cge(count1),cge(count2),
     +                   specT,cgeT,iflag)
     
            call interp (period(count1),period(count2),phiglb1(count1),phiglb1(count2),
     +                   specT,phiglb1T,iflag)
            call interp (period(count1),period(count2),phiglb2(count1),phiglb2(count2),
     +                   specT,phiglb2T,iflag)
            call interp (period(count1),period(count2),phiglb3(count1),phiglb3(count2),
     +                   specT,phiglb3T,iflag)
            call interp (period(count1),period(count2),phitw1(count1),phitw1(count2),
     +                   specT,phitw1T,iflag)
            call interp (period(count1),period(count2),phitw2(count1),phitw2(count2),
     +                   specT,phitw2T,iflag)
            call interp (period(count1),period(count2),phitw3(count1),phitw3(count2),
     +                   specT,phitw3T,iflag)

            call interp (period(count1),period(count2),phi4(count1),phi4(count2),
     +                   specT,phi4T,iflag)
            call interp (period(count1),period(count2),phi5(count1),phi5(count2),
     +                   specT,phi5T,iflag)

            call interp (period(count1),period(count2),phiss(count1),phiss(count2),
     +                   specT,phissT,iflag)
            call interp (period(count1),period(count2),phis2s(count1),phis2s(count2),
     +                   specT,phis2sT,iflag)
            call interp (period(count1),period(count2),tau1(count1),tau1(count2),
     +                   specT,tau1T,iflag)
            call interp (period(count1),period(count2),tau2(count1),tau2(count2),
     +                   specT,tau2T,iflag)

            call interp (period(count1),period(count2),sigma1mea(count1),sigma1mea(count2),
     +                   specT,sigma1meaT,iflag)
            call interp (period(count1),period(count2),sigma2mea(count1),sigma2mea(count2),
     +                   specT,sigma2meaT,iflag)
            call interp (period(count1),period(count2),sigma1inf(count1),sigma1inf(count2),
     +                   specT,sigma1infT,iflag)
            call interp (period(count1),period(count2),sigma2inf(count1),sigma2inf(count2),
     +                   specT,sigma2infT,iflag)
          
            call interp (period(count1),period(count2),phiss1M(count1),phiss1M(count2),
     +                   specT,phiss1MT,iflag)
            call interp (period(count1),period(count2),phiss2M(count1),phiss2M(count2),
     +                   specT,phiss2MT,iflag)

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

		
      if(regionflag .eq. 0) then
       
        cg1T = cgglb1T
        cg2T = cgglb2T
        cg3T = cgglb3T
        phi1T = phiglb1T
        phi2T = phiglb2T
        phi3T = phiglb3T

      elseif(regionflag .eq. 1) then
       
        cg1T = cgtw1T
        cg2T = cgtw2T
        cg3T = cgtw3T
        phi1T = phitw1T
        phi2T = phitw2T
        phi3T = phitw3T
       
      endif

C     Current code set for Measured Vs30 values (i.e., Vs30class=1)
      if (vs30_class .eq. 0) then
         Fmeasured = 0.0
         FInferred = 1.0
         sigma1 = sigma1infT
         sigma2 = sigma2infT

      elseif (vs30_class .eq. 1) then      
         Fmeasured = 1.0
         FInferred = 0.0
         sigma1 = sigma1meaT
         sigma2 = sigma2meaT

      endif       
		
c Center Z_TOR on the Z_TOR-M relation
        if (F_RV.EQ.1) then

            mZ_TOR = max(4.069-1.992*max(M-5.776,0.0),0.0)
            mZ_TOR = mZ_TOR * mZ_TOR

        else
            mZ_TOR = max(3.419-1.961*max(M-5.403,0.0),0.0)
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
        if (Dtor <= 20) then
          CNS = c5T* cosh(c6T * max((M-cHMT),0.0))   
        else
	      CNS = (c5T + deltac5T) * cosh(c6T * max((M-cHMT),0.0))  
		endif
	      
		term8 = c4 * alog(Rrup + CNS)                 

c Distance scaling at large distance
        term9 = (c4a-c4) * alog( sqrt(Rrup*Rrup+cRB*cRB) )  
        term10 = (cg1T + cg2T/cosh(max((M-cg3T),0.0))+ msasflag*cgeT)*Rrup  


c Scaling with other source variables (F_RV, F_NM, deltaZ_TOR, and Dip)
        coshM = cosh(2*max(M-4.5,0.0))
        cosDELTA = cos(DELTA*d2r)
        term2 = (c1aT+c1cT/coshM) * F_RV 
        term3 = (c1bT+c1dT/coshM) * F_NM 
        term4 = (c7T +c7bT/coshM) * deltaZ_TOR 
        term5 = (c11T+c11bT/coshM)* cosDELTA**2   
        
c Predicted median Sa on reference condition (Vs=1130 m/sec)
        lnpsa_ref = term1+term2+term3+term5+term4+term6+term7+term8+term9+term10
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

        tau = tau1T +(tau2T-tau1T)/1.5*(min(max(M-0.0,5.0),6.5)-5.0)

       NL0=phi2T*(exp(phi3T*(min(Vs,1130.0)-360.0))-exp(phi3T*(1130.0-360.0)))
     1    *(psa_ref/(psa_ref+phi4T))
	 
       sigmaNL0 = (sigma1+(sigma2 - sigma1)/1.5*(min(max(M-0.0,5.0),6.5)-5.0))*
     1           sqrt((sigma3T*Finferred + 0.7* Fmeasured) + (1.0+NL0)**2.0)

     
C     Current code set for Measured Vs30 values (i.e., Vs30class=1)

      if (M .lt. 5.0) then
          phiss = phiss1MT
      elseif (M .le. 6.5) then
          phiss = phiss1MT + (phiss2MT-phiss1MT)*((M-5.0)/(6.5-5.5))
      else
          phiss = phiss2MT
      endif

        sigma = sqrt((1+NL0)**2.0*(tau)**2.0+sigmaNL0**2.0)

      phi = sigmaNL0

C     Convert ground motion to units of gals.
      lnY = lnpsa + 6.89
      period2 = period1

      return
      end 
 
 
c ------------------------------------------------------------------            
C *** Adjusted BCHydro model by Phung and Loh ***********
c ------------------------------------------------------------------            
      subroutine PhungSub2017 ( mag, dist, vs, Z10, ZTor, lnY, sigma, ftype,
     2                     specT, period2, iflag, regionflag )   

      implicit none

      real mag, dip, fType, dist, vs, SA1100, 
     1      Z10,  ZTOR, fltWidth, lnSa, sigma, lnY, vs30_rock, period1
      real Fn, Frv, specT, period2, CRjb, phi, tau, z10_rock, SA_rock
      integer iflag, regionflag
      character*80 attenName                                                    

C     regionflag     Note
C     -------------------------
C      0         for Japan
C      1         for Taiwan
C


        if (ftype .eq. 1) then
          call PhungSubIntra ( mag, dist, vs, Z10, ZTor, lnSa, sigma,  
     2                     specT, period1, iflag, regionflag )
	 
        elseif (ftype .eq. 0) then
          call PhungSubInter ( mag, dist, vs, Z10, ZTor, lnSa, sigma,
     2                     specT, period1, iflag, regionflag )
	 
        endif

C     Convert ground motion to units of gals.
      lnY = lnSa + 6.89

      period2 = period1

      return
      end
c ------------------------------------------------------------------            

      subroutine PhungSubInter ( mag, rRup, vs30, Z10, ZTor, lnSa, sigma,  
     2                     specT, period1, iflag, regionflag )

      implicit none
     
      integer MAXPER, nPer, i1, i      
      parameter (MAXPER=21)
      real a1(MAXPER), a2(MAXPER),a4(MAXPER), a5(MAXPER), a13(MAXPER), a6tw(MAXPER), a6jp(MAXPER),
     1     a12tw(MAXPER), a12jp(MAXPER), cZ10tw(MAXPER), cZ10jp(MAXPER), cZ1(MAXPER), cZ2(MAXPER), 
     1     tau1(MAXPER), phi1(MAXPER), phiTWss(MAXPER), phiTWs2s(MAXPER), phiJPss(MAXPER), phiJPs2s(MAXPER)
      real period(MAXPER), Mref(MAXPER)
      real sigma, lnSa, pgaRock, vs30, rRup, disthypo, mag 

      real a1T, a2T, a4T, a5T, a13T, a6twT, a6jpT, a12twT, a12jpT, cZ10twT, cZ10jpT, cZ1T, cZ2T, 
     1     tau1T, phi1T, phiTWssT, phiTWs2sT, phiJPssT, phiJPs2sT 
      real Ez1, fz10, fmag, frup, fsite, fztor, MrefT
      real period1, a3, a6, a12, cZ10, Z10, ZTor, a9
      integer count1, count2, iflag, regionflag
      real n, c, c4, c1, faba, R, depth, specT, tau, phi

C     interface

      data period  /0, 0.01, 0.02, 0.05, 0.075, 0.1, 0.15, 0.20, 0.25, 0.3, 0.4,
     1           0.5, 0.6, 0.75, 1, 1.5, 2, 2.5, 3, 4, 5  /
      data a1  / 5.623598606, 5.635231547, 5.657634174, 6.312306088, 6.620468754, 7.029722154, 
     1           6.718996331, 6.500326676, 6.19027004, 5.764345514, 5.2061896787, 4.555745035,  
     1           4.178011938, 3.402253709, 2.256503629, 1.327906038, 0.347177474, -0.343757407,  
     1           -1.00105674, -2.069670794, -2.411901912  / 
      data a2  / -1.563698643, -1.5627963, -1.566785768, -1.631820192, -1.631690279, -1.684571581,  
     1           -1.611100094, -1.572013743, -1.529033033, -1.475149901, -1.424076541, -1.357300257,  
     1           -1.337621806, -1.243087557, -1.08539545, -1.003184522, -0.868380463, -0.790838915,  
     1           -0.691127548, -0.556763255, -0.565567612  / 
      data a4  / 0.85705184, 0.862462214, 0.85189326, 0.862355242, 0.972755243, 0.969123493,  
     1           0.894187884, 0.826987861, 0.811812927, 0.776135893, 0.754729522, 0.691979798,  
     1           0.779745865, 0.903689022, 1.063264163, 1.171139374, 1.271736882, 1.291887503,  
     1           1.281724235, 1.376125522, 1.303204595  / 
      data a5  / 0.015393857, 0.017202121, 0.028880503, 0.086085114, 0.116710204, 0.098338232,  
     1           0.096978873, 0.023838692, 0.00436863, 0.003866109, 0.009522134, 0.05500887,  
     1           0.116490492, 0.188412624, 0.224329337, 0.203249963, 0.208832021, 0.229107566,  
     1           0.243414845, 0.321500811, 0.365783548  / 
      data a13   / -0.025877995, -0.025928448, -0.025953447, -0.028789851, -0.024645381, -0.027308458,  
     1           -0.028459871, -0.030791089, -0.029532143, -0.027351268, -0.027298542, -0.030528429,  
     1           -0.018983959, -0.005602488, 0.005592648, 0.000517685, 0.002850466, 0.002835437,  
     1           -0.000624948, 0.00879926, 0.000523706  / 
      data a6tw  / -0.024446172, -0.024258215, -0.024840602, -0.030849869, -0.024934201, 0.00047484,  
     1           0.000102297, 0.001202386, -0.000405323, -0.000204456, -0.00532624, -0.000127415,  
     1           -0.030284244, -0.042579671, -0.050601763, -0.048350352, -0.046407164, -0.04587505,  
     1           -0.047429706, -0.055056304, -0.049945433  / 
      data a6jp  / -0.081360892, -0.081573348, -0.081601896, -0.084325177, -0.086735106, -0.086256773,  
     1           -0.088369815, -0.087699052, -0.086488989, -0.084430532, -0.078703203, -0.075067168,  
     1           -0.069586649, -0.066613491, -0.06465091, -0.058596085, -0.059450033, -0.058849138,  
     1           -0.061426309, -0.064783275, -0.060994229  / 
      data a12tw  / -0.488894217, -0.48779699, -0.475469518, -0.303090115, -0.319859964, -0.363,
     1           -0.496068986, -0.60414626, -0.692861426, -0.766841172, -0.843369619, -0.879518697,
     1           -0.89861621, -0.964304198, -1.049127765, -1.035568384, -0.942718627, -0.91917647,
     1           -0.921755097, -0.836492373, -0.85375366  / 

      data a12jp  / -0.76637859, -0.765417775, -0.747565524, -0.511249818, -0.364165368, -0.512115945,  
     1           -0.874022777, -1.066072435, -1.182639612, -1.235478856, -1.225356175, -1.14860247,  
     1           -1.11808547, -1.065577324, -1.025687867, -0.982300178, -0.923926669, -0.882887141,  
     1           -0.804485935, -0.727184536, -0.669610538  / 
      data Mref   / 7.724690703, 7.724661238, 7.724671613, 7.73592093, 7.73595602, 7.762265353,  
     1           7.724658928, 7.724655082, 7.724658062, 7.719647528, 7.619110991, 7.426220989,  
     1           7.395735809, 7.331297291, 7.241639841, 7.12, 7.12, 7.12, 7.12, 7.12, 7.12  / 
      data cZ10tw   / 0.021409149, 0.021564138, 2.10E-02, -0.020597763, -0.042682957, -0.0391505,  
     1           -0.022082831, -0.010998797, 0.012798505, 0.030809185, 0.100125836, 0.143936482,  
     1           0.190559463, 0.204727717, 0.23457987, 0.243756479, 0.225987631, 0.206340233,  
     1           0.180949565, 0.170063317, 0.136265485  / 
      data cZ10jp   / -0.006577801, -0.006528483, -0.006674755, -0.007275679, -0.005721221,  
     1           -0.005622662, -0.007175561, -0.009066816, -0.008807462, -0.008036543, -0.005816503,  
     1           -0.003604947, -0.001728741, 0.000607127, 0.002770987, 0.003162546, 0.001949556,  
     1           0.001961391, 0.002029842, 0.001371146, 0.002105491  / 
      data cZ1    / 0.025807831, 0.025985182, 0.026156801, 0.031165163, 0.036578986, 0.038547481,  
     1           0.035209241, 0.029795527, 0.026652735, 0.022879522, 0.016033965, 0.010931575,  
     1           0.006738021, 0.003162079, 0.000562048, 0, 0, 0, 0, 0, 0  / 
      data cZ2    / 22.85675132, 22.79147023, 22.81540836, 21.95635875, 20.14234333, 20.64788346,  
     1           22.38536126, 23.94676246, 24.64165013, 24.95029915, 26.02192506, 27.89579319,  
     1           29.58765918, 30.37871541, 21.6161051, 25, 25, 25, 25, 25, 25  / 
      data tau1   / 0.415014081, 0.416000333, 0.414073004, 4.3400, 0.462492262, 0.469594927,  
     1           0.466094856, 0.452201304, 0.445775597, 0.418534505, 0.34975861, 0.345322595,  
     1           0.352095067, 0.395444, 0.419975711, 0.440534747, 0.454271887, 0.425071208,  
     1           0.427124739, 0.451514562, 0.444349403  / 
      data phi1   / 0.599788541, 0.600806548, 0.601067127, 0.634847487, 0.67930345, 0.702871203,  
     1           0.696445048, 0.670668717, 0.653806255, 0.633960256, 0.611740755, 0.604741438,  
     1           0.603752309, 0.604291964, 0.621937867, 0.651405232, 0.67766233, 0.672269103,  
     1           0.657591753, 0.628453221, 0.573498897  / 
      data phiTWss   / 0.567942047, 0.567753559, 0.56867374, 0.601236156, 0.595335134, 0.598216646,  
     1           0.581065827, 0.577310898, 0.582376979, 0.579430523, 0.570299057, 0.575179593,  
     1           0.569791185, 0.572513153, 0.553345656, 0.562651426, 0.614190356, 0.633323863,  
     1           0.615321215, 0.511688741, 0.443403917  / 
      data phiTWs2s   / 0.2134375, 0.213460676, 0.214568366, 0.229679851, 0.242665312, 2.39E-01,  
     1           0.221543785, 0.220054938, 0.217264598, 0.219434213, 0.207105245, 0.205927658,  
     1           0.250158414, 0.246562892, 0.252534453, 0.256280075, 0.278304142, 0.251291775,  
     1           0.269731941, 0.279566589, 0.19713785  / 
      data phiJPss   / 0.494218425, 0.495193236, 0.494989549, 0.521149637, 0.583250651, 0.607548646,  
     1           0.625550335, 0.615406007, 0.603368274, 0.575535653, 0.585954889, 0.598144608,  
     1           0.604744698, 0.6253521, 0.645005902, 0.676751187, 0.672970502, 0.650155445,  
     1           0.625303271, 0.599080923, 0.539986058  / 
      data phiJPs2s   / 0.351934453, 0.352687405, 0.352897546, 0.377132987, 0.422407842, 0.441172698,  
     1           0.409927144, 0.381491131, 0.358569221, 0.314828936, 0.284876656, 0.261385882,  
     1           0.255787398, 0.263738337, 0.281658483, 0.315792887, 0.306478258, 0.28832554,  
     1           0.28539036, 0.28942599, 0.271053769  / 

C Constant parameters            

      c4 = 10
      a3 = 0.1
      a9 = 0.268

C     regionflag     Note
C     -------------------------
C      0         for Japan
C      1         for Taiwan
C

C Find the requested spectral period and corresponding coefficients
      nPer = 21

C First check for the PGA case 
      if (specT .eq. 0.0) then
         i1=1
         period1 = period(i1)
         a1T  =  a1(i1)
         a2T  =  a2(i1)
         a4T  =  a4(i1)
         a5T  =  a5(i1)
         a13T  =  a13(i1)
         a6twT  =  a6tw(i1)
         a6jpT  =  a6jp(i1)
         a12twT  =  a12tw(i1)
         a12jpT  =  a12jp(i1)
         cZ10twT  =  cZ10tw(i1)
         cZ10jpT  =  cZ10jp(i1)
         cZ1T  =  cZ1(i1)
         cZ2T  =  cZ2(i1)
         tau1T  =  tau1(i1)
         phi1T  =  phi1(i1)
         phiTWssT  =  phiTWss(i1)
         phiTWs2sT  =  phiTWs2s(i1)
         phiJPssT  =  phiJPss(i1)
         phiJPs2sT  =  phiJPs2s(i1)
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
      write (*,*) 'Phung et al. interface (2017 Model) Horizontal'
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
 1020   call interp (period(count1),period(count2),a1(count1),a1(count2),
     +                   specT, a1T,iflag)          
      call interp (period(count1),period(count2),a2(count1),a2(count2),
     +                   specT, a2T,iflag)          
      call interp (period(count1),period(count2),a4(count1),a4(count2),
     +                   specT, a4T,iflag)          
      call interp (period(count1),period(count2),a5(count1),a5(count2),
     +                   specT, a5T,iflag)          
      call interp (period(count1),period(count2),a13(count1),a13(count2),
     +                   specT, a13T,iflag)          
      call interp (period(count1),period(count2),a6tw(count1),a6tw(count2),
     +                   specT, a6twT,iflag)          
      call interp (period(count1),period(count2),a6jp(count1),a6jp(count2),
     +                   specT, a6jpT,iflag)          
      call interp (period(count1),period(count2),a12tw(count1),a12tw(count2),
     +                   specT, a12twT,iflag)          
      call interp (period(count1),period(count2),a12jp(count1),a12jp(count2),
     +                   specT, a12jpT,iflag)          
      call interp (period(count1),period(count2),Mref(count1),Mref(count2),
     +                   specT, MrefT,iflag)          
      call interp (period(count1),period(count2),cZ10tw(count1),cZ10tw(count2),
     +                   specT, cZ10twT,iflag)          
      call interp (period(count1),period(count2),cZ10jp(count1),cZ10jp(count2),
     +                   specT, cZ10jpT,iflag)          
      call interp (period(count1),period(count2),cZ1(count1),cZ1(count2),
     +                   specT, cZ1T,iflag)          
      call interp (period(count1),period(count2),cZ2(count1),cZ2(count2),
     +                   specT, cZ2T,iflag)          
      call interp (period(count1),period(count2),tau1(count1),tau1(count2),
     +                   specT, tau1T,iflag)          
      call interp (period(count1),period(count2),phi1(count1),phi1(count2),
     +                   specT, phi1T,iflag)          
      call interp (period(count1),period(count2),phiTWss(count1),phiTWss(count2),
     +                   specT, phiTWssT,iflag)          
      call interp (period(count1),period(count2),phiTWs2s(count1),phiTWs2s(count2),
     +                   specT, phiTWs2sT,iflag)          
      call interp (period(count1),period(count2),phiJPss(count1),phiJPss(count2),
     +                   specT, phiJPssT,iflag)          
      call interp (period(count1),period(count2),phiJPs2s(count1),phiJPs2s(count2),
     +                   specT, phiJPs2sT,iflag)          


 1011 period1 = specT                                                                                                              

C  Regional term and  Basin Depth term
      if(regionflag .eq. 0) then
       
        a6 = a6jpT
        a12 = a12jpT
        cZ10 = cZ10jpT

		Ez1 = exp(-5.23/2.0 * alog((vs30**2.0 + 412.39**2.0)/(1360.0**2.0 + 412.39**2.0)))

      elseif(regionflag .eq. 1) then
       
        a6 = a6twT
        a12 = a12twT
        cZ10 = cZ10twT

		Ez1 = exp(-2.63/4.0 * alog((vs30**4.0 + 253.0**4.0)/(2492.0**4.0 + 253.0**4.0)))
		
      endif
      
      fz10 = cZ10*(min(alog(Z10*1000.0/Ez1),0.0))
      
C     Magnitude Scaling
      if (mag .le. MrefT ) then
        fmag = a4T*(mag-MrefT) + a13T*(10.0-mag)**2.0
      else
        fmag = a5T*(mag-MrefT) + a13T*(10.0-mag)**2.0
      endif   
      
C     Path Scaling
       R = rRup + c4*exp( (mag-6.0)*a9 ) 
       frup = a1T + (a2T + a3*(mag - 7.8))*alog(R) - a6**2*rRup 
      
C     Site Effect
       fsite = a12*min(alog(vs30/760),0.0)
       
C     Ztor Scaling        
       fztor = cZ1T*(min(Ztor,60.0)-cZ2T)

       lnSa = fmag + frup + fztor + fsite + fz10
       
C     Set sigma values to return
       tau = tau1T
       phi = phi1T
       sigma = sqrt(tau**2+phi**2)
	   
  	  write(*,*) "fz10 = ", fz10
   	  write(*,*) "fmag = ", fmag
  	  write(*,*) "X = ", frup
  	  write(*,*) "fsite = ", fsite
  	  write(*,*) "fztor = ", fztor
  	  write(*,*) "lnSa = ", lnSa
  	  write(*,*) "Sa = ", exp(lnSa)
	
                                          

      return
      end

c ----------------------------------------------------------------------
      subroutine PhungSubIntra ( mag, rRup, vs30, Z10, Ztor, lnSa, sigma, 
     2                     specT, period1, iflag, regionflag )

      implicit none
      
      integer MAXPER, nPer, i1, i      
      parameter (MAXPER=21)
      real a1(MAXPER), a2(MAXPER),a4(MAXPER), a5(MAXPER), a13(MAXPER), a6tw(MAXPER), a6jp(MAXPER),
     1     a12tw(MAXPER), a12jp(MAXPER), cZ10tw(MAXPER), cZ10jp(MAXPER), cZtw1(MAXPER), cZtw2(MAXPER), 
     1     tau1(MAXPER), phi1(MAXPER), phiTWss(MAXPER), phiTWs2s(MAXPER), phiJPss(MAXPER), phiJPs2s(MAXPER)
      real period(MAXPER), cZjp1(MAXPER), cZjp2(MAXPER)
      real sigma, lnSa, vs30, rRup, mag 

      real a1T, a2T, a4T, a5T, a13T, a6twT, a6jpT, a12twT, a12jpT, cZ10twT, cZ10jpT, cZtw1T, cZtw2T, 
     1     tau1T, phi1T, phiTWssT, phiTWs2sT, phiJPssT, phiJPs2sT , cZjp1T, cZjp2T
      real Ez1, fz10, fmag, frup, fsite, cZ1, cZ2, fztor
      real period1, a3, a6, a12, cZ10, a9, Ztor, Mref
      integer count1, count2, iflag, regionflag
      real n, c, c4, c1, faba, R, depth, specT, tau, phi, Z10

C     intraslab

      data period  /0, 0.01, 0.02, 0.05, 0.075, 0.1, 0.15, 0.20, 0.25, 0.3, 0.4, 0.5, 0.6, 
     1           0.75, 1, 1.5, 2, 2.5, 3, 4, 5  /
      data a1  / 7.062762535, 7.073947183, 7.106456065, 7.92364323, 7.981283882, 8.079122836, 
     1           7.949727066, 7.768164459, 7.189924549, 6.989418135, 6.462664872, 6.045216257, 
     1            5.809213926, 5.30123994, 3.998530209, 2.674526787, 2.478052463, 1.975932538,  
     1           1.455604438, 0.726737902, 0.393465324  / 
      data a2  / -1.72863278, -1.72816178, -1.728024416, -1.76804206, -1.71541485, -1.729255937, 
     1           -1.74541491, -1.762013945, -1.680884623, -1.673811248, -1.647417215, -1.608692185, 
     1           -1.596616401, -1.530284396, -1.32797155, -1.113774034, -1.191674069, -1.153750513, 
     1           -1.095536403, -1.054187616, -1.043129868  / 
      data a4  / 0.804712277, 0.810467545, 0.803127581, 0.834640186, 0.926020843, 0.88129167, 
     1           0.834282291, 0.824689266, 0.781009332, 0.790121804, 0.783835299, 0.764932275, 
     1           0.788721343, 0.861809197, 0.969250129, 1.186595644, 1.363767633, 1.457598158, 
     1           1.495312308, 1.587969762, 1.655270995  / 
      data a5  / 0.027154734, 0.027642115, 0.030934992, 0.069564547, 0.084035169, 0.112879511, 
     1           0.118962392, 0.175928212, 0.140040091, 0.110040091, 0.092040091, 0.050040091, 
     1           0.050040091, 0.050040091, 0.050040091, 0.050040091, 0.050040091, 0.050040091, 
     1           0.050040091, 0.050040091, 0.000040091  / 
      data a13  / -0.039262424, -0.039091928, -0.040012232, -0.050434824, -0.041660719, -0.038798774, 
     1           -0.031773456, -0.026079111, -0.02662591, -0.024082983, -0.022834594, -0.025662931, 
     1           -0.025391161, -0.025788066, -0.022097078, -0.016704591, -0.010413988, -0.007378934, 
     1           -0.006409089, -0.000808691, -0.000399941  / 
      data a6tw  / -0.03531022, -0.035431107, -0.035666894, -0.038524494, -0.036521958, 0.034496518, 
     1           0.031881963, -0.030820899, -0.034547356, -0.033484063, 0.02678012, 0.019539769, 
     1           -0.015380134, 0.014522201, -0.025576164, -0.036171011, -0.011634842, 0.000391771, 
     1           -0.000330745, -0.000332362, -0.00050358  / 
      data a6jp  / -0.068782711, -0.068909529, -0.069388758, -0.075063264, -0.077594721, -0.075620812, 
     1           -0.073765434, -0.069065286, -0.068076003, -0.065222755, -0.059848859, -0.054710496, 
     1           -0.050128722, -0.047587621, -0.051286691, -0.05072479, -0.04087764, -0.038847405, 
     1           -0.039161136, -0.03666433, -0.032546023  / 
      data a12tw  / -0.576829811, -0.574752114, -0.566802519, -0.427461335, -0.486164584, -0.52338068, 
     1           -0.612949892, -0.682263139, -0.74201506, -0.824266784, -0.916898557, -0.902008854, 
     1           -0.904825107, -0.945057731, -0.985973238, -0.960626244, -0.863395491, -0.85552795, 
     1           -0.872888835, -0.799803291, -0.823730378  / 
      data a12jp  / -0.745080198, -0.746317488, -0.719493181, -0.425921037, -0.333240658, -0.535328695, 
     1           -0.942940818, -1.117023287, -1.25382833, -1.312448076, -1.263712341, -1.16563387, 
     1           -1.060170601, -0.956965069, -0.890427831, -0.806074426, -0.753628272, -0.7341565, 
     1           -0.668416305, -0.55692562, -0.566877066  / 
      data cZtw1   / 0.016139993, 0.016227635, 0.016141924, 0.01753911, 0.019095631, 0.017731454, 
     1           0.016542362, 0.016702689, 0.013418429, 0.012115568, 0.011744892, 0.010249044, 0.011034398, 
     1           0.010684283, 0.010344436, 0.011134618, 0.01465073, 0.01288782, 0.010684079,  
     1           0.003705235, 0.004145347 / 
      data cZtw2   / 61, 61, 61, 63, 66, 66, 63, 59, 57, 55, 53, 50, 50, 50, 48, 52, 50, 51, 52, 60, 58  / 
      data cZjp1   / 0.016526015, 0.016556487, 0.016633991, 0.017801258, 0.021547757, 0.02123271, 
     1           0.016885677, 0.016176803, 0.015515636, 0.019173544, 0.020482484, 0.021994618,  
     1          0.025154951, 0.02637815, 0.028911783, 0.030223611, 0.028903621, 0.026867972,  
     1          0.026189137, 0.023322582, 0.018843855  / 
      data cZjp2   / 20, 20, 20, 20, 20, 20, 20, 20, 20, 30, 30, 30, 40, 40, 40, 50, 40, 40, 40, 40, 40  / 
      data cZ10tw   / -0.018190509, -0.018164465, -0.018974706, -0.039457134, -0.050659987, 
     1           -0.048939004, -0.049432075, -0.030224545, -0.011296383, 0.00969393, 0.069440436, 
     1           0.13302134, 0.172006518, 0.172966741, 0.169952997, 0.171258411, 0.171489992,  
     1          0.154354036, 0.130677352, 0.08858784, 0.102904211  / 
      data cZ10jp   / 0.005467135, 0.005487203, 0.005596183, 0.006795754, 0.005647821, 0.005009995, 
     1           0.002390823, 0.001953114, 0.002903917, 0.002744599, 0.000209183, -0.000129023, 
     1           -0.000674056, 0.000597085, 0.000557659, 0.003154018, 0.002424682, 0.003357327, 
     1           0.003366236, 0.002215124, 0.003871555  / 
      data tau1   / 0.420540385, 0.422006115, 0.421262072, 0.479806405, 0.51364015, 0.48952853, 
     1           0.426796336, 0.371754241, 0.407677609, 0.429742315, 0.400897975, 0.377932754, 0.409799116, 
     1           0.373491577, 0.393043194, 0.46879983, 0.454742373, 0.455811464, 0.443629248, 0.47005703, 
     1           0.465733665  / 
      data phi1   / 0.587045963, 0.586960512, 0.588073172, 0.643152873, 0.695049637, 0.709447345, 0.68279741, 
     1           0.649372345, 0.626458448, 0.596577407, 0.575710908, 0.56302049, 0.565715289, 0.55651167, 
     1           0.548912296, 0.558068715, 0.543243566, 0.542705791, 0.529707312, 0.514884002, 0.489976367  / 
      data phiTWss   / 0.473040573, 0.474570279, 0.475428044, 0.524455938, 0.57064866, 0.581335079, 0.554241252, 
     1           0.532065895, 0.511451544, 0.490885708, 0.475520119, 0.479918553, 0.489145243, 0.496227152, 
     1           0.479835414, 0.485912654, 0.472180627, 0.47765178, 0.462477258, 0.468764318, 0.445853752  / 
      data phiTWs2s   / 0.187706872, 0.187834662, 0.188076026, 0.210205862, 0.230468022, 0.227369667, 0.226431621,  
     1          0.21233939, 0.196494736, 0.186752009, 0.184600376, 0.193545734, 0.180874509, 0.172841471, 0.193354958, 
     1           0.18271037, 0.187244156, 0.178611498, 0.198042634, 0.197397259, 0.159950118  / 
      data phiJPss   / 0.590375723, 0.591203374, 0.593851899, 0.648308014, 0.705726502, 0.728572946, 0.723543915, 
     1           0.680117676, 0.643706056, 0.616182906, 0.60840853, 0.59210877, 0.598356565, 0.586957437, 0.593215287, 
     1           0.611308139, 0.616496835, 0.58986241, 0.566492564, 0.552590239, 0.477435759  / 
      data phiJPs2s   / 0.288936522, 0.289633789, 0.292494593, 0.320727134, 0.330142565, 0.332772606, 0.321169741, 
     1           0.326395815, 0.304734931, 0.294057523, 0.293894428, 0.295927682, 0.288979592, 0.295229127, 0.289092655,  
     1          0.28701412, 0.264226495, 0.237863165, 0.230359832, 0.225576535, 0.181793956  / 

C Constant parameters            
      a9 = 0.268
      a3 = 0.1
      c4 = 10.0
      Mref = 7.42
C Find the requested spectral period and corresponding coefficients
      nPer = 21

C First check for the PGA case 
      if (specT .eq. 0.0) then
         i1=1
         period1 = period(i1)
         a1T  =  a1(i1)
         a2T  =  a2(i1)
         a4T  =  a4(i1)
         a5T  =  a5(i1)
         a13T  =  a13(i1)
         a6twT  =  a6tw(i1)
         a6jpT  =  a6jp(i1)
         a12twT  =  a12tw(i1)
         a12jpT  =  a12jp(i1)
         cZ10twT  =  cZ10tw(i1)
         cZ10jpT  =  cZ10jp(i1)
         cZtw1T  =  cZtw1(i1)
         cZtw2T  =  cZtw2(i1)
         cZjp1T  =  cZjp1(i1)
         cZjp2T  =  cZjp2(i1)
         tau1T  =  tau1(i1)
         phi1T  =  phi1(i1)
         phiTWssT  =  phiTWss(i1)
         phiTWs2sT  =  phiTWs2s(i1)
         phiJPssT  =  phiJPss(i1)
         phiJPs2sT  =  phiJPs2s(i1)
         goto 1011
      endif

C   For other periods, loop over the spectral period range of the attenuation relationship.
      do i=2, nper-1
         if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
            count1 = i
            count2 = i+1
            goto 1020 
         endif
      enddo

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*) 
      write (*,*) 'Phung et al. intraslab (2017 Model) Horizontal'
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
 1020 call interp (period(count1),period(count2),a1(count1),a1(count2),
     +                   specT, a1T,iflag)          
      call interp (period(count1),period(count2),a2(count1),a2(count2),
     +                   specT, a2T,iflag)          
      call interp (period(count1),period(count2),a4(count1),a4(count2),
     +                   specT, a4T,iflag)          
      call interp (period(count1),period(count2),a5(count1),a5(count2),
     +                   specT, a5T,iflag)          
      call interp (period(count1),period(count2),a13(count1),a13(count2),
     +                   specT, a13T,iflag)          
      call interp (period(count1),period(count2),a6tw(count1),a6tw(count2),
     +                   specT, a6twT,iflag)          
      call interp (period(count1),period(count2),a6jp(count1),a6jp(count2),
     +                   specT, a6jpT,iflag)          
      call interp (period(count1),period(count2),a12tw(count1),a12tw(count2),
     +                   specT, a12twT,iflag)          
      call interp (period(count1),period(count2),a12jp(count1),a12jp(count2),
     +                   specT, a12jpT,iflag)          
      call interp (period(count1),period(count2),cZ10tw(count1),cZ10tw(count2),
     +                   specT, cZ10twT,iflag)          
      call interp (period(count1),period(count2),cZ10jp(count1),cZ10jp(count2),
     +                   specT, cZ10jpT,iflag)          
      call interp (period(count1),period(count2),cZtw1(count1),cZtw1(count2),
     +                   specT, cZtw1T,iflag)          
      call interp (period(count1),period(count2),cZtw2(count1),cZtw2(count2),
     +                   specT, cZtw2T,iflag)          
      call interp (period(count1),period(count2),cZjp1(count1),cZjp1(count2),
     +                   specT, cZjp1T,iflag)          
      call interp (period(count1),period(count2),cZjp2(count1),cZjp2(count2),
     +                   specT, cZjp2T,iflag)          
      call interp (period(count1),period(count2),tau1(count1),tau1(count2),
     +                   specT, tau1T,iflag)          
      call interp (period(count1),period(count2),phi1(count1),phi1(count2),
     +                   specT, phi1T,iflag)          
      call interp (period(count1),period(count2),phiTWss(count1),phiTWss(count2),
     +                   specT, phiTWssT,iflag)          
      call interp (period(count1),period(count2),phiTWs2s(count1),phiTWs2s(count2),
     +                   specT, phiTWs2sT,iflag)          
      call interp (period(count1),period(count2),phiJPss(count1),phiJPss(count2),
     +                   specT, phiJPssT,iflag)          
      call interp (period(count1),period(count2),phiJPs2s(count1),phiJPs2s(count2),
     +                   specT, phiJPs2sT,iflag)      

 1011 period1 = specT                                                                                                              


C  Regional term and Basin Depth term
      if(regionflag .eq. 0) then
       
        a6 = a6jpT
        a12 = a12jpT
        cZ10 = cZ10jpT
        cZ1 = cZjp1T
        cZ2 = cZjp2T

		Ez1 = exp(-5.23/2.0 * alog((vs30**2.0 + 412.39**2.0)/(1360.0**2.0 + 412.39**2.0)))

      elseif(regionflag .eq. 1) then
       
        a6 = a6twT
        a12 = a12twT
        cZ10 = cZ10twT
        cZ1 = cZtw1T
        cZ2 = cZtw2T
		
		Ez1 = exp(-2.63/4.0 * alog((vs30**4.0 + 253.0**4.0)/(2492.0**4.0 + 253.0**4.0)))
		
      endif
      
      fz10 = cZ10*(min(alog(Z10*1000.0/Ez1),0.0))

C     Base model for Magnitude scaling.      
      if (mag .le. Mref ) then
         fmag = a4T*(mag-Mref) + a13T*(10.0-mag)**2.0
      else
         fmag = a5T*(mag-Mref) + a13T*(10.0-mag)**2.0
      endif      
      
C     Path Scaling
      R = rRup + c4*exp( (mag-6.0)*a9 ) 
      frup = a1T + (a2T + a3*(mag - 7.8))*alog(R) - a6**2*rRup 

C     Site Effect
      fsite = a12*min(alog(vs30/760.0),0.0)
       
C     Ztor Scaling
       fztor = cZ1*(min(Ztor,120.0)-  cZ2)
       
       lnSa = fmag + frup + fztor + fsite + fz10

C     Set sigma values to return
       tau = tau1T
       phi = phi1T
	   
       sigma = sqrt(tau**2.0+phi**2.0)

       write(*,*) "fz10 = ", fz10
       write(*,*) "fmag = ", fmag
       write(*,*) "X = ", frup
       write(*,*) "fsite = ", fsite
       write(*,*) "fztor = ", fztor
       write(*,*) "lnSa = ", lnSa
       write(*,*) "Sa = ", exp(lnSa)
      
                                         

      return
      end
	  