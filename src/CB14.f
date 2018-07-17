c ---------------------------------------------------------------------
C     *** Campbell and Bozorgnia NGA West2 (NGA-2013) ***
C         Earthquake Spectra Paper:
C            "Campbell-Bozorgnia NGA-West2 ground motion model
C                 for the average horizontal components of PGA,
C                 PGV, and 5%-damped linear Response Spectra"
C             K. W. Campbel and Y. Bozorgnia
C     Notes:
C        Applicable Range:
C            3.3 < M < 8.5 (strike-slip)
C            3.3 < M < 8.0 (reverse)
C            3.3 < M < 7.5 (normal)
C            Distance < 300km
C            150 < Vs30m < 1500 m/s
C            0 < Z25 < 10 km
C            0 < Zhyp < 20 km
C            15 < Dip < 90
C        Report provides formulation for estimating recommended
C           parameters when they are not defined (e.g., Z25 given Vs30m).
C           See Chapter 06 of the report.
c ---------------------------------------------------------------------------

       subroutine CB14_TW_C01 ( mag, Rrup, Rbjf, Ftype, specT,
     1                     period2, lnY, sigma, iflag, vs,
     2                     depthtop, D25, Dip, depth, HWflag, Rx, rupwidth, regionflag, phi, tau )

C     Last Updated: 5/17/17
C     Coefficients updated from PEER Report version to be consistent with EQ Spectra paper in press.
C     Minor change to T=5, 7.5, and 10 sec for coefficient C6

      parameter (MAXPER=25)
      REAL Period(MAXPER), C0(MAXPER), C1(MAXPER), C2(MAXPER), C3(MAXPER), C4(MAXPER), C5(MAXPER)
      REAL C6(MAXPER), C7(MAXPER), C8(MAXPER), C9(MAXPER), C10(MAXPER), C11(MAXPER), C12(MAXPER)
      REAL C13(MAXPER), C14(MAXPER), C15(MAXPER), C16(MAXPER), C17(MAXPER), C18(MAXPER), C19(MAXPER)
      REAL A2(MAXPER), h1(MAXPER), h2(MAXPER), h3(MAXPER), h4(MAXPER)
      REAL h5(MAXPER), h6(MAXPER)
      REAL K1(MAXPER), K2(MAXPER), K3(MAXPER)
      REAL C20(MAXPER), DC20CA(MAXPER), DC20JP(MAXPER), DC20CH(MAXPER)
      REAL T1(MAXPER), T2(MAXPER), phi1(MAXPER), Phi2(MAXPER), phic(MAXPER)
      REAL flnAF(MAXPER), rho(MAXPER)

      REAL MAG, RRUP, RBJF, VS, D25, FHWR, FHWM, FHWZ, FHWD, PGAROCK, C, N
      real lnY, ftype, Dip, pgasoil, Rx, R1, R2, f1, f2
      real fhypH, D25_RK, depthtop, depth, rupwidth, specT, period2, sigma
      INTEGER count1, count2, HWFlag, regionflag, iflag

      real c0T, c1T, c2T, c3T, c4T, c5T, c6T, c7T, c8T, c9T, c10T, c11T, c12T
      real c13T, c14T, c15T ,c16T, c17T, c18T, c19T, c20T, Dc20CAT, Dc20JPT, Dc20CHT
      real k1T, k2T, k3T, a2T, h1T, h2T, h3T, h4T, h5T, h6T
      real t1T, t2T, phi1T, phi2T, phicT
      real rhoT, flnAFT

      real alpha, tau
      real tau_lnyB, tau_lnPGAB, phi_lnY, phi_lnyB, phi_lnPGAB, phi, sigmatot

C.....MODEL COEFFICIENTS.....................

      Data Period(1:25) / 0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.075, 0.1, 0.12, 0.15, 0.17, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 1, 1.5, 2,
     1      3, 4, 5, 7.5, 10/
      Data c0(1:25) / 0.356117232783958, 0.386758773602206, 0.431142669934497, 0.512003932121085, 0.649473207725169,
     1      0.83030930999764, 1.1884910081139, 1.3219880004909, 1.34639286130098, 1.29509898875361, 1.22285289551775,
     1      1.15727406735158, 0.909093052011905, 0.641421467723059, 0.318531776548646, 0.0272076106161339, -0.843846880903822,
     1      -1.43885245652115, -2.44958793225659, -2.94483330428711, -3.72583189029108, -4.39207737933935, -4.93632451517939,
     1      -5.39658678253602, -6.08834234327286/
      Data c1(1:25) / 0.559096866797525, 0.642869120021438, 0.610036408494032, 0.52214613681899, 0.437792237469586,
     1      0.358623916113537, 0.51578122821281, 0.688524087477354, 0.869930448885324, 1.14455435981339, 1.20493625362397,
     1      1.40329281705379, 1.4904299432736, 1.63350604615105, 1.98913041881546, 2.2979390413256, 2.79162470458416,
     1      2.97138814888335, 3.0269458557659, 2.98088696061972, 2.8427912585638, 2.93520078299344, 3.22558297406824,
     1      3.61157935351029, -4.46691141314152/
      Data c2(1:25) / 0.56879460840871, 0.548250606173165, 0.544503251398314, 0.546550656490723, 0.548607737985833,
     1      0.534084853902167, 0.566967391899305, 0.603886489691274, 0.66348181582985, 0.760818638761807, 0.81016622646059,
     1      0.819292926892119, 0.772660670116403, 0.796485459698103, 0.879449399855572, 0.949861255467957, 1.22026103137552,
     1      1.55566628762785, 1.91095548183813, 2.07554636000273, 2.16930529375473, 2.34782157727094, 2.58402506728671,
     1      2.04361774499736, 1.87859526803749/
      Data c3(1:25) / -0.285486382775716, -0.0545857227354485, -0.0468164681031079, -0.0591023602938874, -0.0881036367623818,
     1      -0.149826837033275, -0.230321700539041, -0.214719346117054, -0.201381168883291, -0.100368099517681,
     1      -0.0722912727125354, 0.00798058403150975, 0.153613771030137, 0.259331343435423, 0.51727423145433, 0.652041670932998,
     1      0.848887051786159, 1.06949114987441, 1.1526912631002, 1.42678748863222, 1.77988512832679, 2.13558950144393,
     1      2.52588186467635, 2.9719228836894, 3.01922347974208/
      Data c4(1:25) / -0.474, -0.474, -0.464, -0.452, -0.446368292053737, -0.442, -0.437, -0.417, -0.401261889962462, -0.382,
     1      -0.384610447190274, -0.388, -0.383, -0.37, -0.301, -0.266, -0.221, -0.123, 0.066, 0.14, 0.313, 0.467, 0.544, 0.559,
     1      0.417/
      Data c5(1:25) / -2.773, -2.773, -2.772, -2.782, -2.78706853715164, -2.791, -2.745, -2.633, -2.55430944981231, -2.458,
     1      -2.44190224232664, -2.421, -2.392, -2.376, -2.303, -2.296, -2.232, -2.158, -2.063, -2.104, -2.051, -1.986, -2.021,
     1      -2.179, -2.244/
      Data c6(1:25) / 0.248, 0.248, 0.247, 0.246, 0.242620975232242, 0.24, 0.227, 0.21, 0.197859172256757, 0.183,
     1      0.182564925468288, 0.182, 0.189, 0.195, 0.185, 0.186, 0.186, 0.169, 0.158, 0.158, 0.148, 0.135, 0.135, 0.165, 0.18/
      Data c7(1:25) / 6.768, 6.753, 6.502, 6.291, 6.30564244066028, 6.317, 6.861, 7.294, 7.62539963136186, 8.031, 8.18501638422618,
     1      8.385, 7.534, 6.99, 7.012, 6.902, 5.522, 5.65, 5.795, 6.632, 6.759, 7.978, 8.538, 8.468, 6.564/
      Data c8(1:25) / 0.0182803849428244, 0.0352717972565958, 0.0336028179581038, 0.016285534576622, -0.00650582531427639,
     1      -0.0345641613176312, -0.0866151455252148, -0.0948362008376924, -0.0766783286040763, -0.0412765896403683,
     1      -0.0216181596004451, 0.0350874237327737, 0.100288157761276, 0.123377479855529, 0.177587576029485, 0.224540830616795,
     1      0.245077396356484, 0.220376520456257, 0.204137207098202, 0.205066753804615, 0.169509715267224, 0.0906713960479872,
     1      0.0509663089420929, -0.0518868818629973, -0.0615381415093379/
      Data c9(1:25) / -0.125687581469247, -0.117608954989698, -0.104761537320645, -0.114299550101683, -0.109461799739078,
     1      -0.14856799063587, -0.194842329812706, -0.224015245432348, -0.197576107268713, -0.120680493944779, -0.09533913761543,
     1      -0.0658759795655963, -0.0210037837890234, -0.0480314832976, -0.0478090629350223, -0.0953914692233888,
     1      -0.171312750492123, -0.314868168006307, -0.361138481474666, -0.332504676330922, -0.193596524311098, 0.128500586959169,
     1      -0.114089568223998, 0.0689814675704926, 0.129699911937625/
      Data c10(1:25) / 0.72, 0.72, 0.73, 0.759, 0.796732443239964, 0.826, 0.815, 0.831, 0.794127856483483, 0.749,
     1      0.755526117975686, 0.764, 0.716, 0.737, 0.738, 0.718, 0.795, 0.556, 0.48, 0.401, 0.206, 0.105, 0, 0, 0/
      Data c11(1:25) / 0.933171905899399, 0.951465896830195, 0.996975452344212, 1.06638085646268, 1.1251338211444,
     1      1.18278442028379, 1.3522774025506, 1.54260504525065, 1.68699538923742, 1.86859564855112, 1.98109812646793,
     1      2.14309564613518, 2.37009022061089, 2.49124376382254, 2.59195688737813, 2.56205311051791, 2.06801479382536,
     1      1.43182748349166, 0.255480399259129, -0.603422596903406, -0.971549580809544, -0.983248384544924, -0.94345132737757,
     1      -0.881625094489307, -0.799650443247734/
      Data c12(1:25) / 2.186, 2.191, 2.189, 2.164, 2.14935755933972, 2.138, 2.446, 2.969, 3.2275546649024, 3.544, 3.61491714866912,
     1      3.707, 3.343, 3.334, 3.544, 3.016, 2.616, 2.47, 2.108, 1.327, 0.601, 0.568, 0.356, 0.075, -0.027/
      Data c13(1:25) / 1.42, 1.416, 1.453, 1.476, 1.51711146800772, 1.549, 1.772, 1.916, 2.02616677026276, 2.161, 2.29326265764056,
     1      2.465, 2.766, 3.011, 3.203, 3.333, 3.054, 2.562, 1.453, 0.657, 0.367, 0.306, 0.268, 0.374, 0.297/
      Data c14(1:25) / -0.0064, -0.007, -0.0167, -0.0422, -0.0557724161504944, -0.0663, -0.0794, -0.0294, 0.0126882028432437,
     1      0.0642, 0.0783834297338236, 0.0968, 0.1441, 0.1597, 0.141, 0.1474, 0.1764, 0.2593, 0.2881, 0.3112, 0.3478, 0.3747,
     1      0.3382, 0.3754, 0.3506/
      Data c15(1:25) / -0.202, -0.207, -0.199, -0.202, -0.279154398863807, -0.339, -0.404, -0.416, -0.411953057418919, -0.407,
     1      -0.365232844955611, -0.311, -0.172, -0.084, 0.085, 0.233, 0.411, 0.479, 0.566, 0.562, 0.534, 0.522, 0.477, 0.321, 0.174/
      Data c16(1:25) / 0.393, 0.39, 0.387, 0.378, 0.331256824046015, 0.295, 0.322, 0.384, 0.398838789463964, 0.417,
     1      0.411344031087739, 0.404, 0.466, 0.528, 0.54, 0.638, 0.776, 0.771, 0.748, 0.763, 0.686, 0.691, 0.67, 0.757, 0.621/
      Data c17(1:25) / 0.0527738108852, 0.0536396004239697, 0.0543820012311646, 0.0556642971677058, 0.057747107463423,
     1      0.0589149176480399, 0.0596691974981739, 0.0590915909800739, 0.0558072637267952, 0.0525013711190904, 0.0528445069680512,
     1      0.0508750262164707, 0.0479946999744034, 0.0474862395788676, 0.0446805684978132, 0.0403541904713576, 0.0338996848064585,
     1      0.033624053570051, 0.0235397196049072, 0.0213354471059939, 0.00744874217515587, 0.000940480289996758,
     1      0.000203896893899771, 0.00788547168296762, 0.0153935990038117/
      Data c18(1:25) / 0.0653258807156129, 0.0464891780106092, 0.0463973807810442, 0.0492094902793388, 0.0537950886076159,
     1      0.0588986857462317, 0.0663145055000436, 0.0671398120179699, 0.0671344775756613, 0.0650210747636337, 0.0627452520792793,
     1      0.05815787225324, 0.0443938406971418, 0.0352962862117813, 0.027447688540921, 0.0210259018767922, 0.0094635438562194,
     1      0.00651597397714056, 0.00392680184009326, -0.0108379604986975, -0.0259582614544221, -0.0478025498454571,
     1      -0.0744211978989785, -0.109831497410489, -0.0897606936410669/
      Data c19(1:25) / 0.00757, 0.00755, 0.00759, 0.0079, 0.00797321220330142, 0.00803, 0.00811, 0.00744, 0.0073140951196997,
     1      0.00716, 0.00703817913112053, 0.00688, 0.00556, 0.00458, 0.00401, 0.00388, 0.0042, 0.00409, 0.00424, 0.00448, 0.00345,
     1      0.00603, 0.00805, 0.0028, 0.00458/
      Data k1(1:25) / 865, 865, 865, 908, 990.222936015443, 1054, 1086, 1032, 962.752315834834, 878, 821.44031087739, 748, 654,
     1      587, 503, 457, 410, 400, 400, 400, 400, 400, 400, 400, 400/
      Data k2(1:25) / -1.186, -1.186, -1.219, -1.273, -1.31411146800772, -1.346, -1.471, -1.624, -1.76204570804354, -1.931,
     1      -2.04281415465008, -2.188, -2.381, -2.518, -2.657, -2.669, -2.401, -1.955, -1.025, -0.299, 0, 0, 0, 0, 0/
      Data k3(1:25) / 1.839, 1.839, 1.84, 1.841, 1.84212634158925, 1.843, 1.845, 1.847, 1.84924830143393, 1.852, 1.85374029812685,
     1      1.856, 1.861, 1.865, 1.874, 1.883, 1.906, 1.929, 1.974, 2.019, 2.11, 2.2, 2.291, 2.517, 2.744/
      Data a2(1:25) / 0.167, 0.168, 0.166, 0.167, 0.170379024767758, 0.173, 0.198, 0.174, 0.184791846882883, 0.198,
     1      0.200610447190274, 0.204, 0.185, 0.164, 0.16, 0.184, 0.216, 0.596, 0.596, 0.596, 0.596, 0.596, 0.596, 0.596, 0.596/
      Data h1(1:25) / 0.241, 0.242, 0.244, 0.246, 0.248815853973132, 0.251, 0.26, 0.259, 0.256751698566066, 0.254,
     1      0.246603732960889, 0.237, 0.206, 0.21, 0.226, 0.217, 0.154, 0.117, 0.117, 0.117, 0.117, 0.117, 0.117, 0.117, 0.117/
      Data h2(1:25) / 1.474, 1.471, 1.467, 1.467, 1.45686292569673, 1.449, 1.435, 1.449, 1.45439592344144, 1.461, 1.47100671422938,
     1      1.484, 1.581, 1.586, 1.544, 1.554, 1.626, 1.616, 1.616, 1.616, 1.616, 1.616, 1.616, 1.616, 1.616/
      Data h3(1:25) / -0.715, -0.714, -0.711, -0.713, -0.706241950464484, -0.701, -0.695, -0.708, -0.711147622007507, -0.715,
     1      -0.717610447190274, -0.721, -0.787, -0.795, -0.77, -0.77, -0.78, -0.733, -0.733, -0.733, -0.733, -0.733, -0.733,
     1      -0.733, -0.733/
      Data h4(1:25) / 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1/
      Data h5(1:25) / -0.337, -0.336, -0.339, -0.338, -0.338, -0.338, -0.347, -0.391, -0.417080296633634, -0.449,
     1      -0.424635826224107, -0.393, -0.339, -0.447, -0.525, -0.407, -0.371, -0.128, -0.128, -0.128, -0.128, -0.128, -0.128,
     1      -0.128, -0.128/
      Data h6(1:25) / -0.27, -0.27, -0.263, -0.259, -0.261252683178505, -0.263, -0.219, -0.201, -0.155134650747747, -0.099,
     1      -0.142072378639526, -0.198, -0.21, -0.121, -0.086, -0.281, -0.285, -0.756, -0.756, -0.756, -0.756, -0.756, -0.756,
     1      -0.756, -0.756/
      Data c20(1:25) / -0.0055, -0.0055, -0.0055, -0.0057, -0.0060379024767758, -0.0063, -0.007, -0.0073, -0.00712013588528528,
     1      -0.0069, -0.00650843292145886, -0.006, -0.0055, -0.0049, -0.0037, -0.0027, -0.0016, -6e-04, 0, 0, 0, 0, 0, 0, 0/
      Data Dc20CA(1:25) / 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      Data Dc20JP(1:25) / -0.0035, -0.0035, -0.0034, -0.0035689512383879, -0.0037, -0.0037, -0.0034, -0.00322013588528528, -0.003,
     1      -0.00304350745317124, -0.0031, -0.0033, -0.0035, -0.0034, -0.0034, -0.0032, -0.003, -0.0019, -5e-04, 0, 0, 0, 0, 0,
     1      -0.0035/
      Data Dc20CH(1:25) / 0.0036, 0.0036, 0.0037, 0.0038689512383879, 0.004, 0.0039, 0.0042, 0.0042, 0.0042, 0.00415649254682876,
     1      0.0041, 0.0036, 0.0031, 0.0028, 0.0025, 0.0016, 6e-04, 0, 0, 0, 0, 0, 0, 0, 0.0036/
      Data t1(1:25) / 0.404, 0.417, 0.446, 0.480916589266832, 0.508, 0.504, 0.445, 0.416671401932432, 0.382, 0.363291795136368,
     1      0.339, 0.34, 0.34, 0.356, 0.379, 0.43, 0.47, 0.497, 0.499, 0.5, 0.543, 0.534, 0.523, 0.466, 0.409/
      Data t2(1:25) / 0.325, 0.326, 0.344, 0.362584636222669, 0.377, 0.418, 0.426, 0.408463248815315, 0.387, 0.365681347946093,
     1      0.338, 0.316, 0.3, 0.264, 0.263, 0.326, 0.353, 0.399, 0.4, 0.417, 0.393, 0.421, 0.438, 0.438, 0.322/
      Data phi1(1:25) / 0.734, 0.738, 0.747, 0.76389512383879, 0.777, 0.782, 0.769, 0.769, 0.769, 0.765519403746301, 0.761, 0.744,
     1      0.727, 0.69, 0.663, 0.606, 0.579, 0.541, 0.529, 0.527, 0.521, 0.502, 0.457, 0.441, 0.734/
      Data phi2(1:25) / 0.492, 0.496, 0.503, 0.512573903508648, 0.52, 0.535, 0.543, 0.543, 0.543, 0.546915670785411, 0.552, 0.545,
     1      0.568, 0.593, 0.611, 0.633, 0.628, 0.603, 0.588, 0.578, 0.559, 0.551, 0.546, 0.543, 0.492/
      Data flnaf(1:25) / 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3,
     1      0.3, 0.3, 0.3, 0.3/
      Data phic(1:25) / 0.166, 0.166, 0.165, 0.163310487616121, 0.162, 0.158, 0.17, 0.174496602867868, 0.18, 0.182610447190274,
     1      0.186, 0.191, 0.198, 0.206, 0.208, 0.221, 0.225, 0.222, 0.226, 0.229, 0.237, 0.237, 0.271, 0.29, 0.166/
      Data rho(1:25) / 1, 0.998, 0.986, 0.958967801857936, 0.938, 0.887, 0.87, 0.872697961720721, 0.876, 0.873389552809726, 0.87,
     1      0.85, 0.819, 0.743, 0.684, 0.562, 0.467, 0.364, 0.298, 0.234, 0.202, 0.184, 0.176, 0.154, 1/


      nPer = 25
      c = 1.88
      n = 1.18

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1 = period(1)
         c0T = c0(1)
         c1T = c1(1)
         c2T = c2(1)
         c3T = c3(1)
         c4T = c4(1)
         c5T = c5(1)
         c6T = c6(1)
         c7T = c7(1)
         c8T = c8(1)
         c9T = c9(1)
         c10T = c10(1)
         c11T = c11(1)
         c12T = c12(1)
         c13T = c13(1)
         c14T = c14(1)
         c15T = c15(1)
         c16T = c16(1)
         c17T = c17(1)
         c18T = c18(1)
         c19T = c19(1)

         a2T = a2(1)
         h1T = h1(1)
         h2T = h2(1)
         h3T = h3(1)
         h4T = h4(1)
         h5T = h5(1)
         h6T = h6(1)

         k1T = k1(1)
         k2T = k2(1)
         k3T = k3(1)
         c20T = c20(1)
         Dc20CAT = Dc20CA(1)
         Dc20JPT = Dc20JP(1)
         Dc20CHT = Dc20CH(1)

         phi1T = phi1(1)
         phi2T = phi2(1)
         t1T = t1(1)
         t2T = t2(1)
         flnAFT = flnaf(1)
         phicT = phic(1)
         rhoT = rho(1)

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
      write (*,*) 'Campbell&Bozorgnia (NGA West2-2013) Horizontal'
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
 1020       call interp (period(count1),period(count2),c0(count1),c0(count2),
     +                   specT,c0T,iflag)
            call interp (period(count1),period(count2),c1(count1),c1(count2),
     +                   specT,c1T,iflag)
            call interp (period(count1),period(count2),c2(count1),c2(count2),
     +                   specT,c2T,iflag)
            call interp (period(count1),period(count2),c3(count1),c3(count2),
     +                   specT,c3T,iflag)
            call interp (period(count1),period(count2),c4(count1),c4(count2),
     +                   specT,c4T,iflag)
            call interp (period(count1),period(count2),c5(count1),c5(count2),
     +                   specT,c5T,iflag)
            call interp (period(count1),period(count2),c6(count1),c6(count2),
     +                   specT,c6T,iflag)
            call interp (period(count1),period(count2),c7(count1),c7(count2),
     +                   specT,c7T,iflag)
            call interp (period(count1),period(count2),c8(count1),c8(count2),
     +                   specT,c8T,iflag)
            call interp (period(count1),period(count2),c9(count1),c9(count2),
     +                   specT,c9T,iflag)
            call interp (period(count1),period(count2),c10(count1),c10(count2),
     +                   specT,c10T,iflag)
            call interp (period(count1),period(count2),c11(count1),c11(count2),
     +                   specT,c11T,iflag)
            call interp (period(count1),period(count2),c12(count1),c12(count2),
     +                   specT,c12T,iflag)
            call interp (period(count1),period(count2),c13(count1),c13(count2),
     +                   specT,c13T,iflag)
            call interp (period(count1),period(count2),c14(count1),c14(count2),
     +                   specT,c14T,iflag)
            call interp (period(count1),period(count2),c15(count1),c15(count2),
     +                   specT,c15T,iflag)
            call interp (period(count1),period(count2),c16(count1),c16(count2),
     +                   specT,c16T,iflag)
            call interp (period(count1),period(count2),c17(count1),c17(count2),
     +                   specT,c17T,iflag)
            call interp (period(count1),period(count2),c18(count1),c18(count2),
     +                   specT,c18T,iflag)
            call interp (period(count1),period(count2),c19(count1),c19(count2),
     +                   specT,c19T,iflag)
            call interp (period(count1),period(count2),a2(count1),a2(count2),
     +                   specT,a2T,iflag)
            call interp (period(count1),period(count2),h1(count1),h1(count2),
     +                   specT,h1T,iflag)
            call interp (period(count1),period(count2),h2(count1),h2(count2),
     +                   specT,h2T,iflag)
            call interp (period(count1),period(count2),h3(count1),h3(count2),
     +                   specT,h3T,iflag)
            call interp (period(count1),period(count2),h4(count1),h4(count2),
     +                   specT,h4T,iflag)
            call interp (period(count1),period(count2),h5(count1),h5(count2),
     +                   specT,h5T,iflag)
            call interp (period(count1),period(count2),h6(count1),h6(count2),
     +                   specT,h6T,iflag)

            call interp (period(count1),period(count2),k1(count1),k1(count2),
     +                   specT,k1T,iflag)
            call interp (period(count1),period(count2),k2(count1),k2(count2),
     +                   specT,k2T,iflag)
            call interp (period(count1),period(count2),k3(count1),k3(count2),
     +                   specT,k3T,iflag)

            call interp (period(count1),period(count2),c20(count1),c20(count2),
     +                   specT,c20T,iflag)
            call interp (period(count1),period(count2),Dc20CA(count1),Dc20CA(count2),
     +                   specT,Dc20CAT,iflag)
            call interp (period(count1),period(count2),Dc20JP(count1),Dc20JP(count2),
     +                   specT,Dc20JPT,iflag)
            call interp (period(count1),period(count2),Dc20CH(count1),Dc20CH(count2),
     +                   specT,Dc20CHT,iflag)

            call interp (period(count1),period(count2),phi1(count1),phi1(count2),
     +                   specT,phi1T,iflag)
            call interp (period(count1),period(count2),phi2(count1),phi2(count2),
     +                   specT,phi2T,iflag)
            call interp (period(count1),period(count2),t1(count1),t1(count2),
     +                   specT,t1T,iflag)
            call interp (period(count1),period(count2),t2(count1),t2(count2),
     +                   specT,t2T,iflag)
            call interp (period(count1),period(count2),flnAF(count1),flnAF(count2),
     +                   specT,flnAfT,iflag)
            call interp (period(count1),period(count2),phic(count1),phic(count2),
     +                   specT,phicT,iflag)
            call interp (period(count1),period(count2),rho(count1),rho(count2),
     +                   specT,rhoT,iflag)

 1011 period1 = specT

C.....COMPUTE ROCK PGA VALUE FIRST.........................
C.....MAGNITUDE DEPENDENCE (Eq 3.2)........................
      IF (MAG .LE. 4.5) THEN
         TERM1 = C0(1) + C1(1)*(MAG-4.5)
      elseif (mag .le. 5.5) then
         TERM1 = C0(1) + c2(1)*(MAG-4.5)
      elseif (mag .le. 6.5) then
         TERM1 = C0(1) + c2(1) + c3(1)*(mag-5.5)
      ELSE
         TERM1 = C0(1) + c2(1) + C3(1) + c4(1)*(mag-6.5)
      ENDIF

C.....Distance dependence (Eq 3.3).....
      R = SQRT( RRUP*RRUP+C7(1)*C7(1) )
      TERM2 = (C5(1) + C6(1)*MAG)*ALOG(R)

C.....SET UP STYLE OF FAULTING TERMS (Eq 3.4, 3.5, and 3.6).........

C     Set mechanism term and corresponding Frv and Fnm values.
C     fType     Mechanism                      Rake
C     ------------------------------------------------------
C    -1,-0.5    Normal and NMl/Obl       -150 < Rake < -30.0
C     1, 0.5    Reverse and Rev/Obl        30 < Rake < 150.0
C       0       Strike-Slip                    Otherwise
      IF (Ftype .EQ. 0.0) THEN
         TERM3 = 0.0
      ELSEIF (Ftype .ge. 0.5) THEN
         if (mag .le. 4.5) then
            TERM3 = 0.0
         elseif (mag .le. 5.5) then
            TERM3 = C8(1)*(mag-4.5)
         else
            TERM3 = c8(1)
         endif
      ELSEIF (Ftype .le. -0.5) THEN
         if (mag .le. 4.5) then
            TERM3 = 0.0
         elseif (mag .le. 5.5) then
            TERM3 = C9(1)*(mag-4.5)
         else
            TERM3 = C9(1)
         endif
      ENDIF

C.....SET UP HANGING WALL TERMS (Eq 3.7)..............
      if (HWflag .eq. 1) then
         R1 = rupwidth*cos(abs(dip)*3.14159/180.0)
         R2 = 62.0*mag - 350.0
         f1 = h1(1) + h2(1)*(Rx/R1) + h3(1)*(Rx/R1)**2.0
         f2 = h4(1) + h5(1)*((Rx-R1)/(R2-R1)) + h6(1)*((Rx-R1)/(R2-R1))**2.0
         if (Rrup .eq. 0.0) then
            fhwrrup = 1.0
         else
            fhwrrup = ((Rrup-Rbjf)/Rrup)
         endif
         if (Rx .lt. R1) Then
            fhwr = f1*fhwrrup
         else
            fhwr = max(f2,0.0)*fhwrrup
         endif
         if (mag .le. 5.5) then
            fhwm = 0.0
         elseif (mag .le. 6.5) then
            fhwm = (mag-5.5)*(1.0+a2(1)*(mag-6.5))
         else
            fhwm = 1.0 + a2(1)*(mag-6.5)
         endif
         if (depthtop .le. 16.66) then
            fhwz = 1.0 - 0.06*depthtop
         else
            fhwz = 0.0
         endif
         fhwd = (90.0 - dip)/45.0
         TERM4 = c10(1)*fhwr*fhwm*fhwz*fhwd
      else
         term4 = 0.0
      endif

C.....NOW COMPUTE THE SITE CONDITION FACTORS...............
C.....(FOR PGA ROCK, VS=1100, i.e., Vs>k1)
      TERM5_RK = (C11(1) + K2(1)*n)*ALOG( 1100.0/K1(1) )

C.....NOW COMPUTE THE SEDIMENT DEPTH DEPENDENCE (Eq 3.17)............
C     For Rock PGA the D25 value should be set at the recommended value of D25=0.398
      D25_RK = 0.398

	   TERM6_RK = C14(1)*(D25_RK-1.0)

C.....Now compute the hypocentral depth term (Eq 3.21).........
      if (depth .le. 7.0) then
         fhypH = 0.0
      elseif (depth .le. 20.0) then
         fhypH = depth - 7.0
      else
         fhypH = 13.0
      endif
      if (mag .le. 5.5) then
          term7 = c17(1)*fhypH
      elseif (mag .le. 6.5) then
          term7 = (c17(1) + (c18(1)-c17(1))*(mag-5.5))*fhypH
      else
          term7 = c18(1)*fhypH
      endif

C.....Compute Rupture Dip term (Eq 3.24)............
      if (mag .le. 4.5) then
          term8 = c19(1)*dip
      elseif (mag .le. 5.5) then
          term8 = c19(1)*(5.5-mag)*dip
      else
          term8 =0
      endif

C.....Compute anelastic attenuation term.....
      if (Rrup .le. 80.0) then
         term9 = 0.0
      else
         term9 = (c20(1)+Dc20CA(1) ) * (Rrup-80.0)

      endif

      PGAROCK = EXP(TERM1+TERM2+TERM3+TERM4+TERM5_RK+TERM6_RK+TERM7+TERM8+TERM9)
c 	  write(*,*) "fmag  = ", TERM1
c 	  write(*,*) "fdis  = ", TERM2
c 	  write(*,*) "fflt  = ", TERM3
c 	  write(*,*) "fhng  = ", TERM4
c 	  write(*,*) "fsite = ", TERM5_RK
c 	  write(*,*) "fsed  = ", TERM6_RK
c 	  write(*,*) "fhyp  = ", TERM7
c 	  write(*,*) "fdip  = ", TERM8
c 	  write(*,*) "fatn  = ", TERM9

C.....For PGA Specific Vs30m Value
      if (vs .le. k1(1) ) then
         term5 = c11(1)*alog(vs/k1(1)) +
     1           k2(1)*(alog(pgarock+c*((vs/k1(1))**n)) -
     2           alog(pgarock+c))
      else
         term5 = (c11(1) + k2(1)*n)*alog(vs/k1(1))
      endif


C.....NOW COMPUTE THE SEDIMENT DEPTH DEPENDENCE (Eq 3.17)............
C     For Rock PGA the D25 value should be set at the recommended value of D25=0.398
      if (D25 .le. 1.0) then
            TERM6 = C14(1)*(D25-1.0)
      elseif (D25 .GT. 1.0 .AND. D25 .LE. 3.0) then
        TERM6 = 0.0
      elseif (D25 .GT. 3.0) then
         TERM6 = c16(1)*k3(1)*exp(-0.75)*(1.0-exp(-0.25*(D25-3.0)))
      endif

      pgasoil = alog(pgarock) - term5_rk - term6_RK + term5 + term6
      psoil2 = (TERM1+TERM2+TERM3+TERM4+TERM5+TERM6+TERM7+TERM8+TERM9)
c 	  write(*,*) "PGAROCK = ", PGAROCK
c 	  write(*,*) "pgasoil = ", pgasoil
c 	  write(*,*) "psoil2  = ", psoil2


C.....NOW COMPUTE THE GROUND MOTION VALUES.................
C.....MAGNITUDE DEPENDENCE.................................
      IF (MAG .LE. 4.5) THEN
         TERM1 = C0T + C1T*(MAG-4.5)
      elseif (mag .le. 5.5) then
         TERM1 = C0T + c2T*(MAG-4.5)
      elseif (mag .le. 6.5) then
         TERM1 = C0T + c2T + c3T*(mag-5.5)
      ELSE
         TERM1 = C0T + c2T + C3T + c4T*(mag-6.5)
      ENDIF

C.....Distance dependence......
      R = SQRT( RRUP*RRUP+C7T*C7T )
      TERM2 = (C5T + C6T*MAG)*ALOG(R)

C.....SET UP STYLE OF FAULTING TERMS...........

C     Set mechanism term and corresponding Frv and Fnm values.
C     fType     Mechanism                      Rake
C     ------------------------------------------------------
C    -1,-0.5    Normal and NMl/Obl       -150 < Rake < -30.0
C     1, 0.5    Reverse and Rev/Obl        30 < Rake < 150.0
C       0       Strike-Slip                    Otherwise
      IF (Ftype .EQ. 0.0) THEN
         TERM3 = 0.0
      ELSEIF (Ftype .ge. 0.5) THEN
         if (mag .le. 4.5) then
            TERM3 = 0.0
         elseif (mag .le. 5.5) then
            TERM3 = C8T*(mag-4.5)
         else
            TERM3 = C8T
         endif
      ELSEIF (Ftype .le. -0.5) THEN
         if (mag .le. 4.5) then
            TERM3 = 0.0
         elseif (mag .le. 5.5) then
            TERM3 = C9T*(mag-4.5)
         else
            TERM3 = C9T
         endif
      ENDIF

C.....SET UP HANGING WALL TERMS................
      if (HWflag .eq. 1) then
         R1 = rupwidth*cos(abs(dip)*3.14159/180.0)
         R2 = 62.0*mag - 350.0
         f1 = h1T + h2T*(Rx/R1) + h3T*(Rx/R1)**2.0
         f2 = h4T + h5T*((Rx-R1)/(R2-R1)) + h6T*((Rx-R1)/(R2-R1))**2.0
         if (Rrup .eq. 0.0) then
            fhwrrup = 1.0
         else
            fhwrrup = ((Rrup-Rbjf)/Rrup)
         endif
         if (Rx .lt. R1) Then
            fhwr = f1*fhwrrup
         else
            fhwr = max(f2,0.0)*fhwrrup
         endif
         if (mag .le. 5.5) then
            fhwm = 0.0
         elseif (mag .le. 6.5) then
            fhwm = (mag-5.5)*(1.0+a2T*(mag-6.5))
         else
            fhwm = 1.0 + a2T*(mag-6.5)
         endif
         if (depthtop .le. 16.66) then
            fhwz = 1.0 - 0.06*depthtop
         else
            fhwz = 0.0
         endif
         fhwd = (90.0 - dip)/45.0
         TERM4 = c10T*fhwr*fhwm*fhwz*fhwd
      else
         term4 = 0.0
      endif

C.....NOW COMPUTE THE SITE CONDITION FACTORS...............
      IF (VS .LE. K1T ) THEN
         TERM5 = C11T*ALOG( VS/K1T ) +
     1           K2T*( ALOG( PGAROCK+c*( (VS/K1T)**N) ) -
     2           ALOG( PGAROCK+c ) )
      ELSE
         TERM5 = ( C11T+K2T*n )*ALOG( VS/K1T )
      ENDIF

C.....NOW COMPUTE THE SEDIMENT DEPTH DEPENDENCE.............
      IF (D25 .LE. 1.0) THEN
         TERM6 = C14T*(D25-1.0)
      ELSEIF (D25 .GT. 1.0 .AND. D25 .LE. 3.0) THEN
         TERM6 = 0.0
      ELSEIF (D25 .GT. 3.0)  THEN
         TERM6 = c16T*k3T*exp(-0.75)*( 1.0 - exp(-0.25*(D25-3.0)))
      ENDIF

C.....Now compute the hypocentral depth term..........
      if (depth .le. 7.0) then
         fhypH = 0.0
      elseif (depth .le. 20.0) then
         fhypH = depth - 7.0
      else
         fhypH = 13.0
      endif
      if (mag .le. 5.5) then
          term7 = c17T*fhypH
      elseif (mag .le. 6.5) then
          term7 = (c17T + (c18T-c17T)*(mag-5.5))*fhypH
      else
          term7 = c18T*fhypH
      endif

C.....Compute Rupture Dip term.............
      if (mag .le. 4.5) then
          term8 = c19T*dip
      elseif (mag .le. 5.5) then
          term8 = c19T*(5.5-mag)*dip
      else
          term8 = 0
      endif

C.....Compute anelastic attenuation term.....
      if (Rrup .le. 80.0) then
         term9 = 0.0
      else
         term9 = (c20T+Dc20CAT)*(Rrup-80.0)
      endif

      LnY = (TERM1+TERM2+TERM3+TERM4+TERM5+TERM6+TERM7+TERM8+TERM9)

c 	  write(*,*) "fmag  = ", TERM1
c 	  write(*,*) "fdis  = ", TERM2
c 	  write(*,*) "fflt  = ", TERM3
c 	  write(*,*) "fhng  = ", TERM4
c 	  write(*,*) "fsite = ", TERM5
c 	  write(*,*) "fsed  = ", TERM6
c 	  write(*,*) "fhyp  = ", TERM7
c 	  write(*,*) "fdip  = ", TERM8
c 	  write(*,*) "fatn  = ", TERM9

c 	  write(*,*) "LnY = ", LnY
c 	  write(*,*) "Sa = ", exp(LnY)

C    Check that SA is not less than PGA for T<0.25sec
c     if (specT .lt. 0.25) then
c        if (lnY .lt. pgasoil ) then
c           lnY = pgasoil
c        endif
c     endif

C.....Now compute the sigma value..........
      IF (Vs .LT. k1T) THEN
        alpha = k2T*pgarock*(1/(pgarock
     &    +c*(Vs/k1T)**n)
     &    -1/(pgarock + c))
      ELSE
        alpha = 0.0
      ENDIF

      If (Mag.le.4.5) then
	   tau_lnyB = t1T
	   tau_lnPGAB = t1(1)
	elseif (Mag.lt.5.5) then
	   tau_lnyB = t2T +
     &          (t1T - t2T)*(5.5-mag)
	   tau_lnPGAB = t2(1) +
     &          (t1(1) - t2(1))*(5.5-Mag)
	else
	   tau_lnyB = t2T
	   tau_lnPGAB = t2(1)
	endif

      tau = SQRT(tau_lnyB**2 +
     &           (alpha * tau_lnPGAB)**2 +
     &           2.0*alpha*rhoT*tau_lnyB*tau_lnPGAB)

      If (Mag.le.4.5) then
	   phi_lny = phi1T
           phi_lnPGAB = phi1(1)
      elseif (Mag.lt.5.5) then
	   phi_lny = phi2T +
     &          (phi1T - phi2T)*(5.5-mag)
	   phi_lnPGAB = phi2(1) +
     &          (phi1(1) - phi2(1))*(5.5-mag)
      else
	   phi_lny = phi2T
	   phi_lnPGAB = phi2(1)
      endif

      phi_lnyB = SQRT(phi_lny**2 - flnAFT**2)

      phi_lnPGAB = SQRT(phi_lnPGAB**2 - flnAF(1)**2)

      phi = SQRT(phi_lny**2 +
     &           (alpha*phi_lnPGAB)**2 +
     &           2.0*alpha*rhoT*phi_lnyB*phi_lnPGAB)

      Sigmatot = SQRT(phi**2 + Tau**2)

      period2 = period1

C     Convert ground motion to units of gals.

      lnY = lnY + 6.89
      sigma = sigmaTot

      return
      END