C================================================================================

      Subroutine S04_PhungCrust2017 ( m, Rrup, specT, period2, lnY, sigma, iflag, 
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

            call S24_interp (period(count1),period(count2),cgglb1(count1),cgglb1(count2),
     +                   specT,cgglb1T,iflag)
            call S24_interp (period(count1),period(count2),cgglb2(count1),cgglb2(count2),
     +                   specT,cgglb2T,iflag)
            call S24_interp (period(count1),period(count2),cgglb3(count1),cgglb3(count2),
     +                   specT,cgglb3T,iflag)
            call S24_interp (period(count1),period(count2),cgtw1(count1),cgtw1(count2),
     +                   specT,cgtw1T,iflag)
            call S24_interp (period(count1),period(count2),cgtw2(count1),cgtw2(count2),
     +                   specT,cgtw2T,iflag)
            call S24_interp (period(count1),period(count2),cgtw3(count1),cgtw3(count2),
     +                   specT,cgtw3T,iflag)
            call S24_interp (period(count1),period(count2),cge(count1),cge(count2),
     +                   specT,cgeT,iflag)
     
            call S24_interp (period(count1),period(count2),phiglb1(count1),phiglb1(count2),
     +                   specT,phiglb1T,iflag)
            call S24_interp (period(count1),period(count2),phiglb2(count1),phiglb2(count2),
     +                   specT,phiglb2T,iflag)
            call S24_interp (period(count1),period(count2),phiglb3(count1),phiglb3(count2),
     +                   specT,phiglb3T,iflag)
            call S24_interp (period(count1),period(count2),phitw1(count1),phitw1(count2),
     +                   specT,phitw1T,iflag)
            call S24_interp (period(count1),period(count2),phitw2(count1),phitw2(count2),
     +                   specT,phitw2T,iflag)
            call S24_interp (period(count1),period(count2),phitw3(count1),phitw3(count2),
     +                   specT,phitw3T,iflag)

            call S24_interp (period(count1),period(count2),phi4(count1),phi4(count2),
     +                   specT,phi4T,iflag)
            call S24_interp (period(count1),period(count2),phi5(count1),phi5(count2),
     +                   specT,phi5T,iflag)

            call S24_interp (period(count1),period(count2),phiss(count1),phiss(count2),
     +                   specT,phissT,iflag)
            call S24_interp (period(count1),period(count2),phis2s(count1),phis2s(count2),
     +                   specT,phis2sT,iflag)
            call S24_interp (period(count1),period(count2),tau1(count1),tau1(count2),
     +                   specT,tau1T,iflag)
            call S24_interp (period(count1),period(count2),tau2(count1),tau2(count2),
     +                   specT,tau2T,iflag)

            call S24_interp (period(count1),period(count2),sigma1mea(count1),sigma1mea(count2),
     +                   specT,sigma1meaT,iflag)
            call S24_interp (period(count1),period(count2),sigma2mea(count1),sigma2mea(count2),
     +                   specT,sigma2meaT,iflag)
            call S24_interp (period(count1),period(count2),sigma1inf(count1),sigma1inf(count2),
     +                   specT,sigma1infT,iflag)
            call S24_interp (period(count1),period(count2),sigma2inf(count1),sigma2inf(count2),
     +                   specT,sigma2infT,iflag)
          
            call S24_interp (period(count1),period(count2),phiss1M(count1),phiss1M(count2),
     +                   specT,phiss1MT,iflag)
            call S24_interp (period(count1),period(count2),phiss2M(count1),phiss2M(count2),
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

      subroutine S04_PhungSub2017 ( mag, rRup, vs30, Z10, ZTor, lnY, sigma,  
     2                     specT, period2, iflag, regionflag, ftype )

      implicit none
     
      integer MAXPER, nPer, i1, i      
      parameter (MAXPER=21)
      real period(MAXPER), a5(MAXPER), a13(MAXPER), Mref(MAXPER), a2(MAXPER), a6tw(MAXPER), a12tw(MAXPER), 
     1     a6jp(MAXPER), a12jp(MAXPER), a1jp(MAXPER), a4jp(MAXPER), a1tw(MAXPER), a4tw(MAXPER),  
     1     a11sitw(MAXPER), a11sijp(MAXPER), a11sstw(MAXPER), a11ssjp(MAXPER), si(MAXPER),  
     1     ss(MAXPER), dsitw(MAXPER), dsstw(MAXPER), dsijp(MAXPER), dssjp(MAXPER), b12tw(MAXPER),  
     1     b12jp(MAXPER), tau1(MAXPER), phi1(MAXPER), phisstw(MAXPER), phis2stw(MAXPER)
      real sigma, lnSa, pgaRock, vs30, rRup, disthypo, mag 

      real a5T, a13T, MrefT, a2T, a6twT, a12twT, a6jpT, a12jpT, a1jpT, a4jpT, a1twT, a4twT,  
     1     a11sitwT, a11sijpT, a11sstwT, a11ssjpT, siT, ssT, dsitwT, dsstwT, dsijpT, dssjpT,  
     1     b12twT, b12jpT, tau1T, phi1T, phisstwT, phis2stwT    
      real Ez1, fz10, fmag, frup, fsite, fztor, fevt
      real period1, a3, Z10, ZTor, a9, d, b12, a1, a4, a6, a12, lnY
      integer count1, count2, iflag, regionflag
      real n, c, c4, c1, faba, R, depth, specT, tau, phi, ftype, period2


      data period  /0, 0.01, 0.02, 0.05, 0.075, 0.1, 0.15, 0.20, 0.25, 0.3, 0.4,
     1           0.5, 0.6, 0.75, 1, 1.5, 2, 2.5, 3, 4, 5  /
      data a1jp  / 5.968739524, 5.968739524, 6.065937902, 6.883190759, 7.227465993, 7.497559544, 7.233835008, 
     1          6.935196245, 6.521314733, 6.156716362, 5.514367143, 4.98146131, 4.658399353, 4.008772718, 
     1          2.9524273, 1.828678126, 0.746782153, -0.00663011, -0.650990819, -1.431118975, -1.755209284 / 
      data a1tw  / 5.436921725, 5.481983362, 5.538686092, 6.089139138, 6.242050014, 6.500741577, 6.504662887, 
     1          6.389354121, 6.078155863, 5.827385493, 5.42827832, 5.006275132, 4.765700364, 4.170084662, 
     1          3.159305932, 2.174570827, 1.117040118, 0.315923466, -0.420579077, -1.301652283, -1.501990409 /       
      data a2  / -1.6170844, -1.6189049, -1.6263518, -1.7079854, -1.6933903, -1.7187589, -1.6686977, 
     1          -1.6457193, -1.6008945, -1.5604226, -1.5007535, -1.4451253, -1.4488396, -1.3851978, 
     1          -1.2532309, -1.1369522, -0.9895625, -0.8912422, -0.8027285, -0.7196396, -0.7258237 / 
      data a4jp  / 0.794352858, 0.792505723, 0.783529563, 0.849352353, 0.900836388, 0.872871415, 0.829902097, 
     1          0.783290162, 0.756119852, 0.730950329, 0.722697329, 0.759119408, 0.804186335, 0.969649141, 
     1          1.066170023, 1.227507791, 1.313655898, 1.354466562, 1.412705471, 1.401011805, 1.437155747 /    
      data a4tw  / 0.566548102, 0.567853502, 0.564490754, 0.51715887, 0.486404981, 0.488707369, 0.527569432, 
     1          0.553252818, 0.566033333, 0.589679636, 0.682072341, 0.767327775, 0.882682225, 1.106610265, 
     1          1.167032801, 1.394743299, 1.474103504, 1.511297382, 1.518900018, 1.460828925, 1.514666676 /    
      data a5  / 0.03849929, 0.04033665, 0.04190178, 0.04509359, 0.04623140, 0.04819708, 
     1          0.04325090, 0.03692059, 0.06597319, 0.06197944, 0.06979644, 0.04783791, 
     1          0.10612877, 0.14511870, 0.20744484, 0.24136621, 0.25767232, 0.27153377, 
     1          0.28822087, 0.32589322, 0.26383949 / 
      data a6jp  / -0.006852048, -0.006876357, -0.006874997, -0.007336676, -0.007786774, -0.007760518, 
     1          -0.007938507, -0.007685433, -0.007463007, -0.007083461, -0.006233276, -0.005626905, 
     1          -0.004826234, -0.0043472, -0.004003156, -0.003371602, -0.003456088, -0.003403299, 
     1          -0.003638611, -0.003796507, -0.003298056 /
      data a6tw  / -0.00099225, -0.00093636, -0.00088804, -0.00075076, -0.00064516, -0.00054289, 
     1          -0.00036864, -0.00022201, -0.00010404, -1.32E-05, -7.96E-05, -2.36E-05, -9.86E-06, 
     1          -6.74E-05, -0.00015376, -0.00032041, -0.00063504, -0.00097344, -0.00119025, -0.001296, -0.00100489 / 
      data a12jp  / -0.7516020, -0.7500948, -0.7307185, -0.4831132, -0.3413025, -0.4948081, -0.8669192, 
     1          -1.0634892, -1.1789740, -1.2253631, -1.2073943, -1.1299835, -1.0859780, -1.0233555, 
     1          -0.9766258, -0.9437327, -0.8880212, -0.8546554, -0.7803988, -0.6937169, -0.6499105 /    
      data a12tw  / -0.4528715, -0.4516550, -0.4403449, -0.2766783, -0.2833841, -0.3205012, -0.4471684, 
     1          -0.5552021, -0.6466667, -0.7124316, -0.7599690, -0.7702118, -0.8037457, -0.8730668, 
     1          -0.9821700, -1.0045641, -0.9337591, -0.9174852, -0.9334706, -0.8808471, -0.9343411 /    
      data a13  / -0.0256568, -0.0259617, -0.0262528, -0.0270426, -0.0276048, -0.0280794, 
     1          -0.0287650, -0.0291017, -0.0290970, -0.0287552, -0.0269993, -0.0235859, 
     1          -0.0180673, -0.0059789, -0.0031849, 0.0016328, 0.0038157, 0.0049829, 
     1          0.0063214, 0.0055941, 0.0007480 /   
      data Mref  / 7.42, 7.48, 7.48, 7.48, 7.48, 7.49, 7.50, 7.46, 7.42, 7.42, 7.34, 7.29, 
     1          7.24, 7.12, 7.12, 7.12, 7.12, 7.12, 7.12, 7.12, 7.12 /       
      data a11sitw  / 0.042901300, 0.043750861, 0.044802304, 0.049068196, 0.057175742, 0.058933025, 0.055861367, 
     1          0.050972069, 0.046435903, 0.04197234, 0.032788712, 0.020606033, 0.016652911, 0.012881203, 
     1          0.005555861, 0.001024621, -0.001859106, -0.004604082, -0.006390933, -0.006873052, -0.004145292 /        
      data a11sijp  / 0.013435789, 0.014125461, 0.014739968, 0.016119648, 0.016739089, 0.016921614, 1.55E-02, 
     1          1.38E-02, 1.31E-02, 1.28E-02, 1.21E-02, 1.08E-02, 9.87E-03, 9.18E-03, 9.08E-03, 0.008774591, 
     1          0.008850491, 0.008710436, 0.008597954, 0.008187343, 0.007571521 /   
      data a11sstw  / 0.017644058, 0.017707758, 0.017831492, 0.018544001, 0.019599185, 0.020556507, 0.020780166, 
     1          0.019426551, 0.017169719, 0.015414566, 0.01343689, 0.012308457, 0.011834766, 0.011954438, 0.01211556, 
     1          0.011352728, 0.011039715, 0.011055828, 0.011036725, 0.011031382, 0.011076041 /        
      data a11ssjp  / 0.01360108, 0.014106421, 0.014584367, 0.015798974, 0.016504644, 0.017134713, 0.01538687, 
     1          0.013705495, 0.012894641, 0.012707692, 0.012153928, 0.010798309, 0.009795145, 0.009095072, 
     1          0.009079087, 0.008626541, 0.008866849, 0.008843388, 0.008657879, 0.008229409, 0.0075585 /  
      data si  / -0.374408902, -3.75E-01, -0.376960941, -0.393806881, -0.417431719, -0.461212658, -0.47904196, 
     1          -0.436329045, -0.375236021, -0.329114488, -2.84E-01, -0.257984301, -2.22E-01, -0.16365332, 
     1          -0.095853886, -0.013344152, 0.006417125, 0.00428927, 0.004621522, -0.003842767, -0.012188147 /    
      data ss  / 0.447307171, 4.62E-01, 0.475632001, 0.512832173, 0.538728435, 0.561037907, 0.567754698, 0.531740717, 
     1          0.471888484, 0.411758139, 0.315569795, 0.252665953, 0.223480274, 0.200898826, 0.114288024, 0.01711266, 
     1          0.000179932, -0.008553978, -0.007969413, -0.004929665, 0.020215779 /  
      data dsitw  / 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25 /      
      data dsstw  / 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60 /      
      data dsijp  / 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10 /      
      data dssjp  / 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10 /      
      data b12tw  / -0.072698986, -0.072700471, -0.07332583, -0.10357132, -0.116533595, -0.116567166, -0.107681879, 
     1          -0.092269089, -0.072089752, -0.056308961, -0.006357987, 0.040510944, 0.071593315, 0.090540171, 
     1          0.126396911, 0.157972926, 0.160804716, 0.15178163, 0.131622296, 0.105217306, 0.103258831 /  
      data b12jp  / 0.002650526, 0.002810000, 0.002376243, 0.000548389, 0.003562477, 0.006709533, 0.007165476, 
     1          0.004341874, 0.004771685, 0.005747192, 0.005294191, 0.002624524, 0.00028237, -0.001824156, 
     1          -0.003649632, -0.005143864, -0.005623872, -0.004503918, -0.005225851, -0.006465579, -0.004203115 /    
      data tau1  / 0.359598302, 0.364722689, 0.361825448, 0.387821608, 0.410744393, 0.399061125, 0.393877013, 
     1          0.364957377, 0.388594983, 0.398189617, 0.418613789, 0.419158414, 0.395628973, 0.426568299, 0.444125779, 
     1          0.4450853, 0.439742589, 0.448589081, 0.469825447, 0.476038164, 0.490620146 /     
      data phi1  / 0.593221182, 0.594151268, 0.594258617, 0.638810416, 0.68778566, 0.704335066, 0.680147269, 
     1          0.653868401, 0.633684301, 0.613119026, 0.594846014, 0.587374669, 0.592607356, 0.589920833, 0.594625406, 
     1          0.606739824, 0.606858839, 0.604371225, 0.590755145, 0.563957325, 0.528088844 /    
      data phisstw  / 0.431332331, 0.437722489, 0.438659734, 0.443126663, 0.431998764, 0.423756788, 0.436286652, 
     1          0.44955763, 0.449672545, 0.449868674, 0.449033202, 0.44443679, 0.43483679, 0.433100211, 0.433210407, 
     1          0.412389349, 0.428153057, 0.401153057, 0.396086007, 0.335863656, 0.325693232 /        
      data phis2stw  / 0.350967141, 0.351110041, 0.350750998, 0.404608769, 0.452049131, 0.45379124, 0.416881992, 
     1          0.39160663, 0.3807666, 0.366885001, 0.360524884, 0.352567905, 0.363479045, 0.38126146, 0.369861574, 
     1          0.386518485, 0.378274223, 0.368173523, 0.3632874, 0.30811597, 0.302949265 /  
	 
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
         a5T = a5(i1)
         a13T = a13(i1)
         MrefT = Mref(i1)
         a2T = a2(i1)
         a6twT = a6tw(i1)
         a12twT = a12tw(i1)
         a6jpT = a6jp(i1)
         a12jpT = a12jp(i1)
         a1jpT = a1jp(i1)
         a4jpT = a4jp(i1)
         a1twT = a1tw(i1)
         a4twT = a4tw(i1)
         a11sitwT = a11sitw(i1)
         a11sijpT = a11sijp(i1)
         a11sstwT = a11sstw(i1)
         a11ssjpT = a11ssjp(i1)
         siT = si(i1)
         ssT = ss(i1)
         dsitwT = dsitw(i1)
         dsstwT = dsstw(i1)
         dsijpT = dsijp(i1)
         dssjpT = dssjp(i1)
         b12twT = b12tw(i1)
         b12jpT = b12jp(i1)
         tau1T = tau1(i1)
         phi1T = phi1(i1)
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
 1020 call S24_interp (period(count1),period(count2),a5(count1), a5(count2),
     +                 specT, a5T, iflag)
      call S24_interp (period(count1),period(count2),a13(count1), a13(count2),
     +                 specT, a13T, iflag)
      call S24_interp (period(count1),period(count2),Mref(count1), Mref(count2),
     +                 specT, MrefT, iflag)
      call S24_interp (period(count1),period(count2),a2(count1), a2(count2),
     +                 specT, a2T, iflag)
      call S24_interp (period(count1),period(count2),a6tw(count1), a6tw(count2),
     +                 specT, a6twT, iflag)
      call S24_interp (period(count1),period(count2),a12tw(count1), a12tw(count2),
     +                 specT, a12twT, iflag)
      call S24_interp (period(count1),period(count2),a6jp(count1), a6jp(count2),
     +                 specT, a6jpT, iflag)
      call S24_interp (period(count1),period(count2),a12jp(count1), a12jp(count2),
     +                 specT, a12jpT, iflag)
      call S24_interp (period(count1),period(count2),a1jp(count1), a1jp(count2),
     +                 specT, a1jpT, iflag)
      call S24_interp (period(count1),period(count2),a4jp(count1), a4jp(count2),
     +                 specT, a4jpT, iflag)
      call S24_interp (period(count1),period(count2),a1tw(count1), a1tw(count2),
     +                 specT, a1twT, iflag)
      call S24_interp (period(count1),period(count2),a4tw(count1), a4tw(count2),
     +                 specT, a4twT, iflag)
      call S24_interp (period(count1),period(count2),a11sitw(count1), a11sitw(count2),
     +                 specT, a11sitwT, iflag)
      call S24_interp (period(count1),period(count2),a11sijp(count1), a11sijp(count2),
     +                 specT, a11sijpT, iflag)
      call S24_interp (period(count1),period(count2),a11sstw(count1), a11sstw(count2),
     +                 specT, a11sstwT, iflag)
      call S24_interp (period(count1),period(count2),a11ssjp(count1), a11ssjp(count2),
     +                 specT, a11ssjpT, iflag)
      call S24_interp (period(count1),period(count2),si(count1), si(count2),
     +                 specT, siT, iflag)
      call S24_interp (period(count1),period(count2),ss(count1), ss(count2),
     +                 specT, ssT, iflag)
      call S24_interp (period(count1),period(count2),dsitw(count1), dsitw(count2),
     +                 specT, dsitwT, iflag)
      call S24_interp (period(count1),period(count2),dsstw(count1), dsstw(count2),
     +                 specT, dsstwT, iflag)
      call S24_interp (period(count1),period(count2),dsijp(count1), dsijp(count2),
     +                 specT, dsijpT, iflag)
      call S24_interp (period(count1),period(count2),dssjp(count1), dssjp(count2),
     +                 specT, dssjpT, iflag)
      call S24_interp (period(count1),period(count2),b12tw(count1), b12tw(count2),
     +                 specT, b12twT, iflag)
      call S24_interp (period(count1),period(count2),b12jp(count1), b12jp(count2),
     +                 specT, b12jpT, iflag)
      call S24_interp (period(count1),period(count2),tau1(count1), tau1(count2),
     +                 specT, tau1T, iflag)
      call S24_interp (period(count1),period(count2),phi1(count1), phi1(count2),
     +                 specT, phi1T, iflag)
      call S24_interp (period(count1),period(count2),phisstw(count1), phisstw(count2),
     +                 specT, phisstwT, iflag)
      call S24_interp (period(count1),period(count2),phis2stw(count1), phis2stw(count2),
     +                 specT, phis2stwT, iflag)       


 1011 period1 = specT                                                                                                              

C     Regional term
      if(ftype .eq. 0.0) then 
        fevt = siT
      elseif(ftype .eq. 1.0) then 
       fevt = ssT
      endif
     
C     Ztor Scaling        
      if  (ftype .eq. 0.0 .and. regionflag .eq. 1 ) then
         d = dsitwT
         fztor = a11sitwT *(min(Ztor,40.0)-d)
      elseif (ftype .eq. 0.0 .and. regionflag .eq. 0 ) then
         d = dsijpT
         fztor = a11sijpT *(min(Ztor,40.0)-d)
      elseif (ftype .eq. 1.0 .and. regionflag .eq. 1 ) then
         d = dsstwT
         fztor = a11sstwT *(min(Ztor,80.0)-d)
      elseif (ftype .eq. 1.0 .and. regionflag .eq. 0 ) then
         d = dssjpT
         fztor = a11ssjpT*(min(Ztor,80.0)-d)
      endif
 
C  Regional term and  Basin Depth term
      if(regionflag .eq. 0) then
       
        a1 = a1jpT
        a4 = a4jpT
        a6 = a6jpT
        a12 = a12jpT

        Ez1 = exp(-5.23/2.0 * alog((vs30**2.0 + 412.39**2.0)/(1360.0**2.0 + 412.39**2.0)))
        b12 = b12jpT

      elseif(regionflag .eq. 1) then
       
        a1 = a1twT
        a4 = a4twT
        a6 = a6twT
        a12 = a12twT
    
        Ez1 = exp(-2.63/4.0 * alog((vs30**4.0 + 253.0**4.0)/(2492.0**4.0 + 253.0**4.0)))
        b12 = b12twT
		
      endif
      
      fz10 = b12*(min(alog(Z10*1000.0/Ez1),0.0))
      
C     Magnitude Scaling
      if (mag .le. MrefT ) then
        fmag = a4*(mag-MrefT) + a13T*(10.0-mag)**2.0
      else
        fmag = a5T*(mag-MrefT) + a13T*(10.0-mag)**2.0
      endif   
      
C     Path Scaling
       R = rRup + c4*exp( (mag-6.0)*a9 ) 
       frup = a1 + (a2T + a3*(mag - 7.8))*alog(R) + a6*rRup 
      
C     Site Effect
       fsite = a12*min(alog(vs30/760),0.0)
       

       lnSa = fmag + frup + fztor + fsite + fz10 + fevt
       
C     Set sigma values to return
       tau = tau1T
       phi = phi1T
       sigma = sqrt(tau**2+phi**2)
	   
c  	  write(*,*) "fz10 = ", fz10
c      write(*,*) "fmag = ", fmag
c  	  write(*,*) "X = ", frup
c  	  write(*,*) "fsite = ", fsite
c  	  write(*,*) "fztor = ", fztor
c  	  write(*,*) "fevt = ", fevt
c  	  write(*,*) "lnSa = ", lnSa
c  	  write(*,*) "Sa = ", exp(lnSa)
	
C     Convert ground motion to units of gals.
      lnY = lnSa + 6.89
      period2 = period1
      return
      END
