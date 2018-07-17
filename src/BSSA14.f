c ---------------------------------------------------------------------------
C     *** Boore, Stewart, Seyhan and Atkinson NGA West 2 (NGA West2-2013) ***
C         Earthquake Spectra Report:
C            NGA-West2 Equations for Predicting PGA, PGV, and 5%-Damped
C                PSA for Shallow Crustal Earthquakes.
C             D. M. Boore, J. P. Stewart, E. Seyhan, and G. M. Atkinson
C     Notes:
C        Applicable Range:
C            3.0 < M < 8.5 (Strike-Slip)
C            3.0 < M < 7.0 (Normal)
C            Distance < 300km
C            150 < Vs < 1500 m/s
C            0.0 < Z1 < 3.0 km
C            Region Flag:
C               0 = Global
C               1 = China-Turkey
C               2 = Italy-Japan
c ---------------------------------------------------------------------------
      subroutine BSSA14_TW_C01 ( mag, Rbjf, specT,
     1        period2, lnY, sigma, iflag, vs, ftype, pga4nl, z10, regionflag, basinflag,
     1        phi, tau )
C     Last Updated: 9/16/13
      parameter (MAXPER=25)
      REAL Period(MAXPER), c1(MAXPER), c2(MAXPER), c3(MAXPER)
      real h(MAXPER), DC3ChinaTrk(MAXPER), DC3ItalyJapan(MAXPER), e0(MAXPER)
      real e1(MAXPER), e2(MAXPER), e3(MAXPER), e4(MAXPER)
      real e5(MAXPER), e6(MAXPER), mh(MAXPER), c(MAXPER), Vc(MAXPER)
      real phi2(MAXPER), phi3(MAXPER), f4(MAXPER)
      real l1(MAXPER), l2(MAXPER), t1(MAXPER), t2(MAXPER)
      real f5(MAXPER), rjbbar(MAXPER), Dfr(MAXPER), Dfv(MAXPER)
      real R1(MAXPER), R2(MAXPER), DC3Global(MAXPER), f6(MAXPER), f7(MAXPER)
      real Mref, Rref, Vref, f1, f3, specT
      REAL MAG, RBJF, VS, z10
      real ftype, Rp, R
      INTEGER iFlag, count1, count2, regionflag, basinflag
      real lnY, mechS, mechN, mechR, pga4nl
      real f2, flin, fBasin, phi, tau
      real c1T, c2T, c3T, hT, e0T, e1T, e2T, e3T, e4T
      real e5T, e6T, mhT, cT, VcT, phi2T, phi3T, f4T, l1T, l2T, t1T, t2T
      real deltaz1, f5T, rjbbarT, DfrT, DfvT, R1T, R2T, DC3GlobalT
      real DC3ChinaTrkT, DC3ItalyJapanT, f6T, f7T


      Data Period(1:25) / 0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.075, 0.1, 0.12, 0.15, 0.17, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 1, 1.5, 2,
     1      3, 4, 5, 7.5, 10/
      Data e0(1:25) / 0.4534, 0.48598, 0.56916, 0.673459231164795, 0.75436, 0.96447, 1.1268, 1.20895293439595, 1.3095,
     1      1.3164611925074, 1.3255, 1.2766, 1.2217, 1.1046, 0.96991, 0.66903, 0.3932, -0.14954, -0.58669, -1.1898, -1.6388,
     1      -1.966, -2.5865, -3.0702, 0.4473/
      Data e1(1:25) / -0.0192358695063516, 0.0567227727912004, 0.103830570426141, 0.198897708101522, 0.271900769282252,
     1      0.367398657969924, 0.606318878890423, 0.823883469673358, 0.913809415580094, 0.972718537695808, 0.972741127823899,
     1      0.952673927272273, 0.929588626686887, 0.906677969774733, 0.843243142498517, 0.70851892231752, 0.369210918626143,
     1      0.162950871861978, -0.505519010009077, -0.791847279739693, -1.52940409045199, -2.08919352516209, -2.29332932326379,
     1      -2.99734840119961, -3.09836742174658/
      Data e2(1:25) / -0.0466127974661881, -0.0899522328065243, -0.0889941216873682, -0.0881837044024162, -0.0738384916140176,
     1      -0.0774606068618486, -0.0933753870789509, -0.119805547559339, -0.102166706267363, -0.0619031495690598,
     1      -0.0438630892907241, -0.0424937532989739, -0.0281214761287716, -0.0309767673715999, -0.081060821379759,
     1      -0.135927788790708, -0.141847724215483, -0.157893131542867, -0.119300477644201, -0.103636530614135, 0.0302804751375015,
     1      0.197391958820006, -0.105591250006451, 0.09472882175673, 0.0180272297249192/
      Data e3(1:25) / 0.113150415770813, 0.100661193118287, 0.0982992168992328, 0.0926856182254271, 0.0910574057573448,
     1      0.0841869161547781, 0.0561265104794706, 0.0417968407844044, 0.0410646271818258, 0.0492102889262502, 0.0678307028722051,
     1      0.101248016855977, 0.129758071123678, 0.128286336016744, 0.141561631803489, 0.145442944817284, 0.163752332665633,
     1      0.176717020297631, 0.158522053758111, 0.13897165041878, 0.10401554209148, 0.0505868993201825, -0.0453840386998217,
     1      -0.150207517493787, -0.13838956231916/
      Data e4(1:25) / 0.634956463990215, 0.619860972185183, 0.633560416066013, 0.626773923260574, 0.605074753181471,
     1      0.566320478533912, 0.565076405914992, 0.596866780802841, 0.589028550987566, 0.607990040954619, 0.624159967438116,
     1      0.623636047860314, 0.588632504600515, 0.678668378713168, 0.67378138305232, 0.762124819833728, 1.11558730063412,
     1      1.40281398367416, 1.861987715669, 2.03842856513544, 2.2278862997874, 2.24750560056704, 2.27392203189865,
     1      2.02295395240319, 1.47472914989199/
      Data e5(1:25) / 0.05053, 0.04932, 0.05339, 0.06144, 0.0647739711041879, 0.06736, 0.07355, 0.05523, 0.0114780540956452,
     1      -0.04207, -0.072042284489666, -0.11096, -0.16213, -0.1959, -0.22608, -0.23522, -0.21591, -0.18983, -0.1467, -0.11237,
     1      -0.04332, -0.01464, -0.01486, -0.08161, -0.15096/
      Data e6(1:25) / -0.1662, -0.1659, -0.16561, -0.1669, -0.174739337461198, -0.18082, -0.19665, -0.19838, -0.19116744899994,
     1      -0.18234, -0.171976524654611, -0.15852, -0.12784, -0.09286, -0.02319, 0.02912, 0.10829, 0.17895, 0.33896, 0.44788,
     1      0.62694, 0.76303, 0.87314, 1.0121, 1.0651/
      Data Mh(1:25) / 5.75939535845878, 5.93161026644103, 5.89125554011655, 5.90885411844775, 5.92355835882231, 5.93448092637676,
     1      5.91330519700299, 5.90874187926377, 5.92092742673115, 5.94829040352112, 5.93622578077096, 5.96033962320899,
     1      6.06055764485135, 6.03454555393634, 6.20028144493362, 6.22545222018765, 6.17573383439527, 6.18463084988356,
     1      6.01489943068217, 6.0514358625268, 6.07860067368602, 6.1072608317045, 6.18885674149109, 6.23781972475555,
     1      6.57470966466657/
      Data c1(1:25) / -1.134, -1.134, -1.1394, -1.1421, -1.12734492518079, -1.1159, -1.0831, -1.0652, -1.05980407655856, -1.0532,
     1      -1.05646305898784, -1.0607, -1.0773, -1.0948, -1.1243, -1.1459, -1.1777, -1.193, -1.2063, -1.2159, -1.2179, -1.2162,
     1      -1.2189, -1.2543, -1.3253/
      Data c2(1:25) / 0.1917, 0.1916, 0.18962, 0.18842, 0.187670982843147, 0.18709, 0.18225, 0.17203, 0.163927121632102, 0.15401,
     1      0.150042120270783, 0.14489, 0.13925, 0.13388, 0.12512, 0.12015, 0.11054, 0.10248, 0.09645, 0.09636, 0.09764, 0.10218,
     1      0.10353, 0.12507, 0.15183/
      Data c3(1:25) / -0.00788875587408985, -0.0078818509689674, -0.00780428543929034, -0.00832664074171499, -0.0092200593557657,
     1      -0.0100375197535989, -0.0115480726222495, -0.0118959621513459, -0.0115081781258884, -0.0105994476660068,
     1      -0.00984494177643466, -0.00870503597848327, -0.00695258543010702, -0.00578513843981538, -0.00361675198164066,
     1      -0.00202594754252705, -0.000393963454749957, -0.000268145047578852, 0.000338647265255224, -0.000274615471785213,
     1      0.000881504173671615, 0.00121342431999415, 0.00142297452256839, 0.00116987226508603, 0.00197956130485759/
      Data h(1:25) / 4.5, 4.5, 4.5, 4.49, 4.32668046955837, 4.2, 4.04, 4.13, 4.24691167456457, 4.39, 4.48571639697672, 4.61, 4.78,
     1      4.93, 5.16, 5.34, 5.6, 5.74, 6.18, 6.54, 6.93, 7.32, 7.78, 9.48, 9.66/
      Data Dc3Global(1:25) / 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      Data Dc3ChinaTrk(1:25) / 0.002816, 0.00278, 0.002765, 0.00287312879256825, 0.002957, 0.002957, 0.002879, 0.00283718159332883,
     1      0.002786, 0.00271029703148205, 0.002612, 0.002444, 0.002196, 0.002107, 0.002348, 0.00269, 0.002921, 0.003039, 0.002923,
     1      0.002616, 0.002605, 0.002604, 0.0026, 0.00303, 0.002858/
      Data Dc3ItalyJapan(1:25) / -0.002437, -0.00234, -0.002168, -0.00206831876935114, -0.001991, -0.002159, -0.002439,
     1      -0.00255905929657207, -0.002706, -0.00282085967637207, -0.00297, -0.00314, -0.003297, -0.003212, -0.002907, -0.002527,
     1      -0.002089, -0.001518, -0.00117, -0.001188, -0.001083, -0.000571, 0.000385, 0.00149, -0.00255/
      Data c(1:25) / -0.494639525126874, -0.492386790829841, -0.486959917611639, -0.483734070461185, -0.473331126329196,
     1      -0.453061661114652, -0.428535271473178, -0.422096875367947, -0.442766131734041, -0.460824990697695, -0.480490619799534,
     1      -0.48968579619744, -0.492658432074322, -0.536122783765441, -0.599481092255171, -0.641720588625172, -0.829810827332613,
     1      -0.948492569720881, -1.03094032845303, -1.03669695507734, -1.05585320583303, -1.07883768198775, -1.0420295546124,
     1      -0.981155106166575, -0.88092955963527/
      Data Vc(1:25) / 1500, 1500.2, 1500.36, 1502.95, 1502.08834868422, 1501.42, 1494, 1479.12, 1462.81082139824, 1442.85,
     1      1420.99185552677, 1392.61, 1356.21, 1308.47, 1252.66, 1203.91, 1147.59, 1109.95, 1072.39, 1009.49, 922.43, 844.48,
     1      793.13, 771.01, 775/
      Data f4(1:25) / -0.15, -0.14833, -0.1471, -0.15485, -0.1782103245611, -0.19633, -0.22866, -0.24916, -0.252743792485691,
     1      -0.25713, -0.252539963690434, -0.24658, -0.23574, -0.21912, -0.19582, -0.17041, -0.13866, -0.10521, -0.06794, -0.03614,
     1      -0.01358, -0.00321, -0.00025, -5e-05, 0/
      Data f5(1:25) / -0.00701, -0.00701, -0.00728, -0.00735, -0.00685440970072883, -0.00647, -0.00573, -0.0056,
     1      -0.0057124150716967, -0.00585, -0.00597617161419659, -0.00614, -0.00644, -0.0067, -0.00713, -0.00744, -0.00812,
     1      -0.00844, -0.00771, -0.00479, -0.00183, -0.00152, -0.00144, -0.00137, -0.00136/
      Data f6(1:25) / 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.092, 0.367, 0.638, 0.871, 1.135, 1.271, 1.329, 1.329,
     1      1.183/
      Data f7(1:25) / 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.059, 0.208, 0.309, 0.382, 0.516, 0.629, 0.738, 0.809,
     1      0.703/
      Data R1(1:25) / 111.667, 113.105, 112.133, 104.132595691538, 97.927, 85.989, 79.587, 80.3689592387222, 81.326,
     1      85.4944490883363, 90.907, 97.039, 103.152, 106.018, 105.536, 108.388, 116.388, 125.38, 130.369, 130.365, 129.489,
     1      130.224, 130.716, 130, 110/
      Data R2(1:25) / 270, 270, 269.998, 269.999126341589, 270, 270.035, 270.092, 270.123925880362, 270.163, 270.092082851331, 270,
     1      269.449, 268.593, 266.543, 265, 266.511, 270, 262.413, 240.138, 195, 199.446, 230, 250.395, 210, 270/
      Data Dfr(1:25) / 0.096, 0.092, 0.081, 0.0708629256967262, 0.063, 0.064, 0.087, 0.101838789463964, 0.12, 0.126961192507398,
     1      0.136, 0.141, 0.138, 0.122, 0.109, 0.1, 0.098, 0.104, 0.105, 0.088, 0.07, 0.061, 0.058, 0.06, 0.1/
      Data Dfv(1:25) / 0.07, 0.03, 0.029, 0.0295631707946263, 0.03, 0.022, 0.014, 0.0144496602867868, 0.015, 0.0280522359513715,
     1      0.045, 0.055, 0.05, 0.049, 0.06, 0.07, 0.02, 0.01, 0.008, 0, 0, 0, 0, 0, 0.07/
      Data l1(1:25) / 0.698, 0.7018, 0.7212, 0.73916514834858, 0.7531, 0.7447, 0.7279, 0.72448258182042, 0.7203, 0.716384329214589,
     1      0.7113, 0.6984, 0.6754, 0.6428, 0.6147, 0.5815, 0.5527, 0.5317, 0.5263, 0.5335, 0.536, 0.5285, 0.5117, 0.5103, 0.6951/
      Data l2(1:25) / 0.4992, 0.5023, 0.5136, 0.524131293859512, 0.5323, 0.5423, 0.5407, 0.538946324881531, 0.5368,
     1      0.537670149063425, 0.5388, 0.5471, 0.5614, 0.5804, 0.599, 0.6218, 0.625, 0.6192, 0.6182, 0.619, 0.6156, 0.6223, 0.6344,
     1      0.6036, 0.4951/
      Data t1(1:25) / 0.4019, 0.4087, 0.4449, 0.477451271929402, 0.5027, 0.4744, 0.4153, 0.387780790448648, 0.3541,
     1      0.34953171741702, 0.3436, 0.35, 0.3634, 0.381, 0.4101, 0.4572, 0.4983, 0.5248, 0.5325, 0.5369, 0.5427, 0.532, 0.511,
     1      0.4869, 0.3982/
      Data t2(1:25) / 0.3446, 0.3464, 0.364, 0.39908554050522, 0.4263, 0.4658, 0.4583, 0.42664391581021, 0.3879, 0.353355082182037,
     1      0.3085, 0.2664, 0.229, 0.2097, 0.2235, 0.2664, 0.2984, 0.3151, 0.3291, 0.3438, 0.3492, 0.3354, 0.2699, 0.2392, 0.348/

C     Set constant parameters
      mref = 4.5
      rref = 1.0
      vref = 760.0
      f1 = 0.0
      f3 = 0.1
      V1 = 225.0
      V2 = 300.0
C First check for the PGA case (i.e., specT=0.0)
      nPer = 25
      if (specT .eq. 0.0) then
         period1 = period(1)
         e0T = e0(1)
         e1T = e1(1)
         e2T = e2(1)
         e3T = e3(1)
         e4T = e4(1)
         e5T = e5(1)
         e6T = e6(1)
         mhT = mh(1)
         c1T = c1(1)
         c2T = c2(1)
         c3T = c3(1)
         hT = h(1)
         cT = c(1)
         VcT = vc(1)
         phi2T = phi2(1)
         phi3T = phi3(1)
         Dc3GlobalT = DC3global(1)
         Dc3ChinaTrkT = DC3ChinaTrk(1)
         Dc3ItalyJapanT = DC3ItalyJapan(1)
         f4T = f4(1)
         f5T = f5(1)
         f6T = f6(1)
         f7T = f7(1)
         R1T = R1(1)
         R2T = R2(1)
         l1T = l1(1)
         l2T = l2(1)
         t1T = t1(1)
         t2T = t2(1)
         rjbbarT = rjbbar(1)
         DfrT = Dfr(1)
         DfvT = Dfv(1)
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
      write (*,*) 'BSSA (NGA West2-2013) Horizontal'
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
 1020       call interp (period(count1),period(count2),e0(count1),e0(count2),
     +                   specT,e0T,iflag)
            call interp (period(count1),period(count2),e1(count1),e1(count2),
     +                   specT,e1T,iflag)
            call interp (period(count1),period(count2),e2(count1),e2(count2),
     +                   specT,e2T,iflag)
            call interp (period(count1),period(count2),e3(count1),e3(count2),
     +                   specT,e3T,iflag)
            call interp (period(count1),period(count2),e4(count1),e4(count2),
     +                   specT,e4T,iflag)
            call interp (period(count1),period(count2),e5(count1),e5(count2),
     +                   specT,e5T,iflag)
            call interp (period(count1),period(count2),e6(count1),e6(count2),
     +                   specT,e6T,iflag)
            call interp (period(count1),period(count2),mh(count1),mh(count2),
     +                   specT,mhT,iflag)
            call interp (period(count1),period(count2),c1(count1),c1(count2),
     +                   specT,c1T,iflag)
            call interp (period(count1),period(count2),c2(count1),c2(count2),
     +                   specT,c2T,iflag)
            call interp (period(count1),period(count2),c3(count1),c3(count2),
     +                   specT,c3T,iflag)
            call interp (period(count1),period(count2),h(count1),h(count2),
     +                   specT,hT,iflag)
            call interp (period(count1),period(count2),c(count1),c(count2),
     +                   specT,cT,iflag)
            call interp (period(count1),period(count2),phi2(count1),phi2(count2),
     +                   specT,phi2T,iflag)
            call interp (period(count1),period(count2),phi3(count1),phi3(count2),
     +                   specT,phi3T,iflag)
            call interp (period(count1),period(count2),DC3Global(count1),DC3Global(count2),
     +                   specT,DC3GlobalT,iflag)
            call interp (period(count1),period(count2),DC3ChinaTrk(count1),DC3ChinaTrk(count2),
     +                   specT,DC3ChinaTrkT,iflag)
            call interp (period(count1),period(count2),DC3ItalyJapan(count1),DC3ItalyJapan(count2),
     +                   specT,DC3ItalyJapanT,iflag)
            call interp (period(count1),period(count2),Vc(count1),Vc(count2),
     +                   specT,VcT,iflag)
            call interp (period(count1),period(count2),f4(count1),f4(count2),
     +                   specT,f4T,iflag)
            call interp (period(count1),period(count2),f5(count1),f5(count2),
     +                   specT,f5T,iflag)
            call interp (period(count1),period(count2),f6(count1),f6(count2),
     +                   specT,f6T,iflag)
            call interp (period(count1),period(count2),f7(count1),f7(count2),
     +                   specT,f7T,iflag)
            call interp (period(count1),period(count2),R1(count1),R1(count2),
     +                   specT,R1T,iflag)
            call interp (period(count1),period(count2),R2(count1),R2(count2),
     +                   specT,R2T,iflag)
            call interp (period(count1),period(count2),l1(count1),l1(count2),
     +                   specT,l1T,iflag)
            call interp (period(count1),period(count2),l2(count1),l2(count2),
     +                   specT,l2T,iflag)
            call interp (period(count1),period(count2),t1(count1),t1(count2),
     +                   specT,t1T,iflag)
            call interp (period(count1),period(count2),t2(count1),t2(count2),
     +                   specT,t2T,iflag)
            call interp (period(count1),period(count2),rjbbar(count1),rjbbar(count2),
     +                   specT,rjbbarT,iflag)
            call interp (period(count1),period(count2),Dfr(count1),Dfr(count2),
     +                   specT,DfrT,iflag)
            call interp (period(count1),period(count2),Dfv(count1),Dfv(count2),
     +                   specT,DfvT,iflag)
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
C     Note: Unknown Mechanism is not currently coded.
      if (ftype .eq. -1.0) then
         mechS = 0.0
         mechN = 1.0
         mechR = 0.0
      elseif (ftype .eq. -0.5) then
         mechS = 0.0
         mechN = 1.0
         mechR = 0.0
      elseif (ftype .eq. 0.0) then
         mechS = 1.0
         mechN = 0.0
         mechR = 0.0
      elseif (ftype .eq. 0.5) then
         mechS = 0.0
         mechN = 0.0
         mechR = 1.0
      elseif (ftype .eq. 1.0) then
         mechS = 0.0
         mechN = 0.0
         mechR = 1.0
      endif
C.....First compute the Reference Rock PGA value...........
C.....This will include the regional dependence for PGA....
C.....MAGNITUDE DEPENDENCE.................................
      if (mag .le. mh(1)) then
         term1 = e1(1) + e2(1)*mechN + e3(1)*mechR +
     1           e4(1)*(mag-mh(1)) + e5(1)*(mag-mh(1))**2.0
      else
         term1 = e1(1) + e2(1)*mechN + e3(1)*mechR +
     1           e6(1)*(mag-mh(1))
      endif
C.....Distance dependence......
      Rp = SQRT( Rbjf*Rbjf+h(1)*h(1) )
C.....Apply Regional term.....
         TERM2 = ( c1(1) + c2(1)*(mag-mref) ) * alog(Rp/rref) +
     1           c3(1)  * (Rp-rref)

      pga4nl = exp(term1+term2)
C.....Now compute the requested ground motion value........
C.....MAGNITUDE DEPENDENCE.................................
      if (mag .le. mhT) then
         term1 = e1T + e2T*mechN + e3T*mechR +
     1           e4T*(mag-mhT) + e5T*(mag-mhT)**2.0
      else
         term1 = e1T + e2T*mechN + e3T*mechR +
     1           e6T*(mag-mhT)
      endif
C.....Distance dependence......
      R = SQRT( Rbjf*Rbjf+hT*hT )
C     Now apply the regional attenuation differnece.
C     Global Case
         TERM2 = ( c1T + c2T*(mag-mref) ) * alog(R/rref) +
     1        c3T * (R-rref)
C.....Site Response Term.........
C.....Now compute the site term........
C.....First the linear term......
      if (vs .le. VcT ) then
         flin = cT*alog(Vs/Vref)
      else
         flin = cT*alog(VcT/Vref)
      endif
C.....Next the non-linear term......
      f2 = f4T*(exp(f5T*(min(vs,760.0)-360.0))-exp(f5T*(760.0-360.0)))

C.....Now compute the basin effect term......
C Deviation from ln(Vs30) scaling: bedrock depth (Z1) effect for California.
c      if (basinflag .eq. 1) then
c     Compute the DeltaZ1 term. Apply the California model for all regions except for Japan.
          deltaz1 = z10 -
     1           exp(-2.63/4.0 * alog((vs**4.0 + 253.0**4.0)/(2492.0**4.0 + 253.0**4.0)))/1000.0
          fbasin = min(f7T, f6T * deltaZ1)

c      else
c         fbasin = 0.0
c      endif
      TERM3 = flin + f1 + f2*alog((pga4nl+f3)/f3) + fBasin

      lnY = term1 + term2 + term3
      period2 = period1
c     Now compute the sigma value which is a function of magnitude and Vs
C     Tau (Eq. 4.11)
      if (mag .le. 4.5) then
         tau = t1T
      elseif (mag .gt. 4.5 .and. mag .lt. 5.5) then
         tau = t1T + (t2T - t1T)*(mag - 4.5)
      else
         tau = t2T
      endif

C     Phi - Magnitude (Eq. 4.12)
      if (mag .le. 4.5) then
         phi = l1T
      elseif (mag .gt. 4.5 .and. mag .lt. 5.5) then
         phi = l1T + (l2T - l1T)*(mag - 4.5)
      else
         phi = l2T
      endif
C     Phi - Distance (Eq. 4.13)
      if (rbjf .le. R1T) then
          phi = phi
      elseif (rbjf .gt. R1T .and. rbjf .le. R2T) then
          phi = phi + DfrT*( (alog(rbjf/R1T))/(alog(R2T/R1T)) )
      else
          phi = phi + DfrT
      endif
C     Phi - Vs30 (Eq. 4.14)
      if (vs .ge. V2) then
         phi = phi
      elseif (vs .ge. V1 .and. vs .lt. V2) then
         phi = phi - DfvT*( alog(V2/vs) / alog(V2/V1) )
      else
         phi = phi - DfvT
      endif
      sigma = sqrt (tau**2 + phi**2)
C     Convert ground motion to units of gals.
      lnY = lnY + 6.89
      return
      END


c ---------------------------------------------------------------------------
C     *** Boore, Stewart, Seyhan and Atkinson NGA West 2 (NGA West2-2013) ***
C         Earthquake Spectra Report:
C            NGA-West2 Equations for Predicting PGA, PGV, and 5%-Damped
C                PSA for Shallow Crustal Earthquakes.
C             D. M. Boore, J. P. Stewart, E. Seyhan, and G. M. Atkinson
C     Notes:
C        Applicable Range:
C            3.0 < M < 8.5 (Strike-Slip)
C            3.0 < M < 7.0 (Normal)
C            Distance < 300km
C            150 < Vs < 1500 m/s
C            0.0 < Z1 < 3.0 km
C            Region Flag:
C               0 = Global
C               1 = China-Turkey
C               2 = Italy-Japan
c ---------------------------------------------------------------------------

      subroutine BSSA14_TW_B01 ( mag, Rbjf, specT,
     1        period2, lnY, sigma, iflag, vs, ftype, pga4nl, z10, regionflag, basinflag,
     1        phi, tau )

C     Last Updated: 9/16/13

      parameter (MAXPER=25)
      REAL Period(MAXPER), c1(MAXPER), c2(MAXPER), c3(MAXPER)
      real h(MAXPER), DC3ChinaTrk(MAXPER), DC3ItalyJapan(MAXPER), e0(MAXPER)
      real e1(MAXPER), e2(MAXPER), e3(MAXPER), e4(MAXPER)
      real e5(MAXPER), e6(MAXPER), mh(MAXPER), c(MAXPER), Vc(MAXPER)
      real phi2(MAXPER), phi3(MAXPER), f4(MAXPER)
      real l1(MAXPER), l2(MAXPER), t1(MAXPER), t2(MAXPER)
      real f5(MAXPER), rjbbar(MAXPER), Dfr(MAXPER), Dfv(MAXPER)
      real R1(MAXPER), R2(MAXPER), DC3Global(MAXPER), f6(MAXPER), f7(MAXPER)

      real Mref, Rref, Vref, f1, f3, specT
      REAL MAG, RBJF, VS, z10
      real ftype, Rp, R
      INTEGER iFlag, count1, count2, regionflag, basinflag
      real lnY, mechS, mechN, mechR, pga4nl
      real f2, flin, fBasin, phi, tau
      real c1T, c2T, c3T, hT, e0T, e1T, e2T, e3T, e4T
      real e5T, e6T, mhT, cT, VcT, phi2T, phi3T, f4T, l1T, l2T, t1T, t2T
      real deltaz1, f5T, rjbbarT, DfrT, DfvT, R1T, R2T, DC3GlobalT
      real DC3ChinaTrkT, DC3ItalyJapanT, f6T, f7T

      Data Period / 0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.075, 0.1, 0.12, 0.15, 0.17, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 1, 1.5, 2, 3, 4,
     1      5, 7.5, 10/
      Data e0 / 0.4473, 0.4534, 0.48598, 0.56916, 0.673459231164795, 0.75436, 0.96447, 1.1268, 1.20895293439595, 1.3095,
     1      1.3164611925074, 1.3255, 1.2766, 1.2217, 1.1046, 0.96991, 0.66903, 0.3932, -0.14954, -0.58669, -1.1898, -1.6388,
     1      -1.966, -2.5865, -3.0702/
      Data e1 / -0.0826214158258827, -0.081583825038421, -0.0262901325377611, 0.0715774494674797, 0.149079651907872,
     1      0.245468074662228, 0.47803299367516, 0.666400696655478, 0.753618007664035, 0.816163182653878, 0.824201744408169,
     1      0.821236196317885, 0.806501456909402, 0.801583779844619, 0.751059026981777, 0.626629043915326, 0.261324509391658,
     1      0.018070554444243, -0.520266027094496, -0.902320462426714, -1.65126297577695, -2.18684577200258, -2.4956911590723,
     1      -3.17296855293055, -3.58991268302804/
      Data e2 / -0.125758888519442, -0.12735817960171, -0.126650071688687, -0.12831359531084, -0.115949118491955,
     1      -0.118202970589759, -0.139102028975792, -0.162804287817632, -0.146105977474676, -0.106911452906245,
     1      -0.0871306226309816, -0.0802742592082877, -0.0634356387553911, -0.0648571299357499, -0.120487039379225,
     1      -0.165410863410748, -0.173858372638014, -0.202367391195449, -0.166105286096823, -0.158222475134728,
     1      -0.0419166321455903, -0.0019987092640659, -0.305731889260997, -0.196329084340892, -0.217094259146344/
      Data e3 / 0.102880700018765, 0.101461740446752, 0.10121796783427, 0.0984256530572915, 0.0996382409405683, 0.095421621996527,
     1      0.0690150127317114, 0.0471290999188575, 0.0455289701748044, 0.0493102296169284, 0.0637061566524059, 0.0909136564974004,
     1      0.1174759348571, 0.11764935061747, 0.132109901061163, 0.137529878785592, 0.157260222365333, 0.173350015620239,
     1      0.15714782195564, 0.132449396727018, 0.108297253295273, 0.0421208381922428, -0.00471460926636473, -0.101198488614562,
     1      -0.0608682337147395/
      Data e4 / 0.57750959192403, 0.571816979827731, 0.572315517508971, 0.56496675071618, 0.544057750259267, 0.504821862623813,
     1      0.503284342856724, 0.536003297622665, 0.526682762892466, 0.542916660540052, 0.547872276710265, 0.54527379042043,
     1      0.528878055634112, 0.579872716260723, 0.574262222120385, 0.679391078515849, 1.05452851581773, 1.3478279632714,
     1      1.67524370230533, 1.87393787782879, 2.07205469927058, 2.09015689254199, 2.10831947057758, 1.83382536324736,
     1      1.53168777761448/
      Data e5 / 0.05053, 0.04932, 0.05339, 0.06144, 0.0647739711041879, 0.06736, 0.07355, 0.05523, 0.0114780540956452, -0.04207,
     1      -0.072042284489666, -0.11096, -0.16213, -0.1959, -0.22608, -0.23522, -0.21591, -0.18983, -0.1467, -0.11237, -0.04332,
     1      -0.01464, -0.01486, -0.08161, -0.15096/
      Data e6 / -0.1662, -0.1659, -0.16561, -0.1669, -0.174739337461198, -0.18082, -0.19665, -0.19838, -0.19116744899994, -0.18234,
     1      -0.171976524654611, -0.15852, -0.12784, -0.09286, -0.02319, 0.02912, 0.10829, 0.17895, 0.33896, 0.44788, 0.62694,
     1      0.76303, 0.87314, 1.0121, 1.0651/
      Data Mh / 5.89000105795146, 5.89335898023188, 5.89533032940885, 5.91007915744883, 5.92159976898098, 5.92843636233436,
     1      5.90453191657865, 5.8976171628797, 5.91332105967796, 5.95086304984227, 5.95033237581861, 5.97997930330611,
     1      6.06379301182238, 6.10588952602936, 6.297151656315, 6.29999786668188, 6.21046025771892, 6.20081636654649,
     1      6.1700013048475, 6.16998071022574, 6.1738622325243, 6.18955601204022, 6.22959629987202, 6.30998876748301,
     1      6.42001355441292/
      Data c1 / -1.134, -1.134, -1.1394, -1.1421, -1.12734492518079, -1.1159, -1.0831, -1.0652, -1.05980407655856, -1.0532,
     1      -1.05646305898784, -1.0607, -1.0773, -1.0948, -1.1243, -1.1459, -1.1777, -1.193, -1.2063, -1.2159, -1.2179, -1.2162,
     1      -1.2189, -1.2543, -1.3253/
      Data c2 / 0.1917, 0.1916, 0.18962, 0.18842, 0.187670982843147, 0.18709, 0.18225, 0.17203, 0.163927121632102, 0.15401,
     1      0.150042120270783, 0.14489, 0.13925, 0.13388, 0.12512, 0.12015, 0.11054, 0.10248, 0.09645, 0.09636, 0.09764, 0.10218,
     1      0.10353, 0.12507, 0.15183/
      Data c3 / -0.00677735812500122, -0.00670800956537689, -0.00662622543104718, -0.0071344427562145, -0.00800989321398436,
     1      -0.00879234193437001, -0.0102284479442132, -0.0104082199196874, -0.0100159271889098, -0.00921730520095456,
     1      -0.00850837652844761, -0.00749252120102332, -0.00592755351285675, -0.00481059059433601, -0.00276318416995094,
     1      -0.00132372584415247, 0.000307704607329084, 0.000537664173745369, 0.00125520176371792, 0.00125691925931718,
     1      0.00248669376515102, 0.00290140484211064, 0.00331558037497958, 0.00337606915170449, 0.00430629297966857/
      Data h / 4.5, 4.5, 4.5, 4.49, 4.32668046955837, 4.2, 4.04, 4.13, 4.24691167456457, 4.39, 4.48571639697672, 4.61, 4.78, 4.93,
     1      5.16, 5.34, 5.6, 5.74, 6.18, 6.54, 6.93, 7.32, 7.78, 9.48, 9.66/
      Data Dc3Global / 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      Data Dc3ChinaTrk / 0.002858, 0.002816, 0.00278, 0.002765, 0.00287312879256825, 0.002957, 0.002957, 0.002879,
     1      0.00283718159332883, 0.002786, 0.00271029703148205, 0.002612, 0.002444, 0.002196, 0.002107, 0.002348, 0.00269,
     1      0.002921, 0.003039, 0.002923, 0.002616, 0.002605, 0.002604, 0.0026, 0.00303/
      Data Dc3ItalyJapan / -0.00255, -0.002437, -0.00234, -0.002168, -0.00206831876935114, -0.001991, -0.002159, -0.002439,
     1      -0.00255905929657207, -0.002706, -0.00282085967637207, -0.00297, -0.00314, -0.003297, -0.003212, -0.002907, -0.002527,
     1      -0.002089, -0.001518, -0.00117, -0.001188, -0.001083, -0.000571, 0.000385, 0.00149/
      Data c / -0.487520709299432, -0.479066901926176, -0.472746927888818, -0.461813935569265, -0.442267302251452,
     1      -0.414546835552427, -0.386080964074275, -0.401339906528259, -0.427231958973861, -0.456854773758308, -0.475889441812658,
     1      -0.486965406441146, -0.509023284545245, -0.562996841675536, -0.638659404828067, -0.680289121498191, -0.878792535391228,
     1      -1.00571422388844, -1.10491638646308, -1.10959506742823, -1.11024863364521, -1.098313223865, -1.04755016768555,
     1      -0.980947815869518, -0.890938911086028/
      Data Vc / 1500, 1500.2, 1500.36, 1502.95, 1502.08834868422, 1501.42, 1494, 1479.12, 1462.81082139824, 1442.85,
     1      1420.99185552677, 1392.61, 1356.21, 1308.47, 1252.66, 1203.91, 1147.59, 1109.95, 1072.39, 1009.49, 922.43, 844.48,
     1      793.13, 771.01, 775/
      Data f4 / -0.15, -0.14833, -0.1471, -0.15485, -0.1782103245611, -0.19633, -0.22866, -0.24916, -0.252743792485691, -0.25713,
     1      -0.252539963690434, -0.24658, -0.23574, -0.21912, -0.19582, -0.17041, -0.13866, -0.10521, -0.06794, -0.03614, -0.01358,
     1      -0.00321, -0.00025, -5e-05, 0/
      Data f5 / -0.00701, -0.00701, -0.00728, -0.00735, -0.00685440970072883, -0.00647, -0.00573, -0.0056, -0.0057124150716967,
     1      -0.00585, -0.00597617161419659, -0.00614, -0.00644, -0.0067, -0.00713, -0.00744, -0.00812, -0.00844, -0.00771,
     1      -0.00479, -0.00183, -0.00152, -0.00144, -0.00137, -0.00136/
      Data f6 / 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.092, 0.367, 0.638, 0.871, 1.135, 1.271, 1.329, 1.329, 1.183/
      Data f7 / 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.059, 0.208, 0.309, 0.382, 0.516, 0.629, 0.738, 0.809, 0.703/
      Data R1 / 110, 111.667, 113.105, 112.133, 104.132595691538, 97.927, 85.989, 79.587, 80.3689592387222, 81.326,
     1      85.4944490883363, 90.907, 97.039, 103.152, 106.018, 105.536, 108.388, 116.388, 125.38, 130.369, 130.365, 129.489,
     1      130.224, 130.716, 130/
      Data R2 / 270, 270, 270, 269.998, 269.999126341589, 270, 270.035, 270.092, 270.123925880362, 270.163, 270.092082851331, 270,
     1      269.449, 268.593, 266.543, 265, 266.511, 270, 262.413, 240.138, 195, 199.446, 230, 250.395, 210/
      Data Dfr / 0.1, 0.096, 0.092, 0.081, 0.0708629256967262, 0.063, 0.064, 0.087, 0.101838789463964, 0.12, 0.126961192507398,
     1      0.136, 0.141, 0.138, 0.122, 0.109, 0.1, 0.098, 0.104, 0.105, 0.088, 0.07, 0.061, 0.058, 0.06/
      Data Dfv / 0.07, 0.07, 0.03, 0.029, 0.0295631707946263, 0.03, 0.022, 0.014, 0.0144496602867868, 0.015, 0.0280522359513715,
     1      0.045, 0.055, 0.05, 0.049, 0.06, 0.07, 0.02, 0.01, 0.008, 0, 0, 0, 0, 0/
      Data l1 / 0.6951, 0.698, 0.7018, 0.7212, 0.73916514834858, 0.7531, 0.7447, 0.7279, 0.72448258182042, 0.7203,
     1      0.716384329214589, 0.7113, 0.6984, 0.6754, 0.6428, 0.6147, 0.5815, 0.5527, 0.5317, 0.5263, 0.5335, 0.536, 0.5285,
     1      0.5117, 0.5103/
      Data l2 / 0.4951, 0.4992, 0.5023, 0.5136, 0.524131293859512, 0.5323, 0.5423, 0.5407, 0.538946324881531, 0.5368,
     1      0.537670149063425, 0.5388, 0.5471, 0.5614, 0.5804, 0.599, 0.6218, 0.625, 0.6192, 0.6182, 0.619, 0.6156, 0.6223, 0.6344,
     1      0.6036/
      Data t1 / 0.3982, 0.4019, 0.4087, 0.4449, 0.477451271929402, 0.5027, 0.4744, 0.4153, 0.387780790448648, 0.3541,
     1      0.34953171741702, 0.3436, 0.35, 0.3634, 0.381, 0.4101, 0.4572, 0.4983, 0.5248, 0.5325, 0.5369, 0.5427, 0.532, 0.511,
     1      0.4869/
      Data t2 / 0.348, 0.3446, 0.3464, 0.364, 0.39908554050522, 0.4263, 0.4658, 0.4583, 0.42664391581021, 0.3879,
     1      0.353355082182037, 0.3085, 0.2664, 0.229, 0.2097, 0.2235, 0.2664, 0.2984, 0.3151, 0.3291, 0.3438, 0.3492, 0.3354,
     1      0.2699, 0.2392/

C     Set constant parameters
      mref = 4.5
      rref = 1.0
      vref = 760.0
      f1 = 0.0
      f3 = 0.1
      V1 = 225.0
      V2 = 300.0

C First check for the PGA case (i.e., specT=0.0)
      nPer = 25
      if (specT .eq. 0.0) then
         period1 = period(1)
         e0T = e0(1)
         e1T = e1(1)
         e2T = e2(1)
         e3T = e3(1)
         e4T = e4(1)
         e5T = e5(1)
         e6T = e6(1)
         mhT = mh(1)
         c1T = c1(1)
         c2T = c2(1)
         c3T = c3(1)
         hT = h(1)
         cT = c(1)
         VcT = vc(1)
         phi2T = phi2(1)
         phi3T = phi3(1)
         Dc3GlobalT = DC3global(1)
         Dc3ChinaTrkT = DC3ChinaTrk(1)
         Dc3ItalyJapanT = DC3ItalyJapan(1)
         f4T = f4(1)
         f5T = f5(1)
         f6T = f6(1)
         f7T = f7(1)
         R1T = R1(1)
         R2T = R2(1)
         l1T = l1(1)
         l2T = l2(1)
         t1T = t1(1)
         t2T = t2(1)
         rjbbarT = rjbbar(1)
         DfrT = Dfr(1)
         DfvT = Dfv(1)
         goto 1011
      elseif (specT .eq. -1.0) then
         period1 = period(2)
         e0T = e0(2)
         e1T = e1(2)
         e2T = e2(2)
         e3T = e3(2)
         e4T = e4(2)
         e5T = e5(2)
         e6T = e6(2)
         mhT = mh(2)
         c1T = c1(2)
         c2T = c2(2)
         c3T = c3(2)
         hT = h(2)
         cT = c(2)
         VcT = vc(2)
         phi2T = phi2(2)
         phi3T = phi3(2)
         Dc3GlobalT = DC3global(2)
         Dc3ChinaTrkT = DC3ChinaTrk(2)
         Dc3ItalyJapanT = DC3ItalyJapan(2)
         f4T = f4(2)
         f5T = f5(2)
         f6T = f6(2)
         f7T = f7(2)
         R1T = R1(2)
         R2T = R2(2)
         l1T = l1(2)
         l2T = l2(2)
         t1T = t1(2)
         t2T = t2(2)
         rjbbarT = rjbbar(2)
         DfrT = Dfr(2)
         DfvT = Dfv(2)
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
      write (*,*) 'BSSA (NGA West2-2013) Horizontal'
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
 1020       call interp (period(count1),period(count2),e0(count1),e0(count2),
     +                   specT,e0T,iflag)
            call interp (period(count1),period(count2),e1(count1),e1(count2),
     +                   specT,e1T,iflag)
            call interp (period(count1),period(count2),e2(count1),e2(count2),
     +                   specT,e2T,iflag)
            call interp (period(count1),period(count2),e3(count1),e3(count2),
     +                   specT,e3T,iflag)
            call interp (period(count1),period(count2),e4(count1),e4(count2),
     +                   specT,e4T,iflag)
            call interp (period(count1),period(count2),e5(count1),e5(count2),
     +                   specT,e5T,iflag)
            call interp (period(count1),period(count2),e6(count1),e6(count2),
     +                   specT,e6T,iflag)
            call interp (period(count1),period(count2),mh(count1),mh(count2),
     +                   specT,mhT,iflag)
            call interp (period(count1),period(count2),c1(count1),c1(count2),
     +                   specT,c1T,iflag)
            call interp (period(count1),period(count2),c2(count1),c2(count2),
     +                   specT,c2T,iflag)
            call interp (period(count1),period(count2),c3(count1),c3(count2),
     +                   specT,c3T,iflag)
            call interp (period(count1),period(count2),h(count1),h(count2),
     +                   specT,hT,iflag)
            call interp (period(count1),period(count2),c(count1),c(count2),
     +                   specT,cT,iflag)
            call interp (period(count1),period(count2),phi2(count1),phi2(count2),
     +                   specT,phi2T,iflag)
            call interp (period(count1),period(count2),phi3(count1),phi3(count2),
     +                   specT,phi3T,iflag)
            call interp (period(count1),period(count2),DC3Global(count1),DC3Global(count2),
     +                   specT,DC3GlobalT,iflag)
            call interp (period(count1),period(count2),DC3ChinaTrk(count1),DC3ChinaTrk(count2),
     +                   specT,DC3ChinaTrkT,iflag)
            call interp (period(count1),period(count2),DC3ItalyJapan(count1),DC3ItalyJapan(count2),
     +                   specT,DC3ItalyJapanT,iflag)
            call interp (period(count1),period(count2),Vc(count1),Vc(count2),
     +                   specT,VcT,iflag)
            call interp (period(count1),period(count2),f4(count1),f4(count2),
     +                   specT,f4T,iflag)
            call interp (period(count1),period(count2),f5(count1),f5(count2),
     +                   specT,f5T,iflag)
            call interp (period(count1),period(count2),f6(count1),f6(count2),
     +                   specT,f6T,iflag)
            call interp (period(count1),period(count2),f7(count1),f7(count2),
     +                   specT,f7T,iflag)
            call interp (period(count1),period(count2),R1(count1),R1(count2),
     +                   specT,R1T,iflag)
            call interp (period(count1),period(count2),R2(count1),R2(count2),
     +                   specT,R2T,iflag)
            call interp (period(count1),period(count2),l1(count1),l1(count2),
     +                   specT,l1T,iflag)
            call interp (period(count1),period(count2),l2(count1),l2(count2),
     +                   specT,l2T,iflag)
            call interp (period(count1),period(count2),t1(count1),t1(count2),
     +                   specT,t1T,iflag)
            call interp (period(count1),period(count2),t2(count1),t2(count2),
     +                   specT,t2T,iflag)
            call interp (period(count1),period(count2),rjbbar(count1),rjbbar(count2),
     +                   specT,rjbbarT,iflag)
            call interp (period(count1),period(count2),Dfr(count1),Dfr(count2),
     +                   specT,DfrT,iflag)
            call interp (period(count1),period(count2),Dfv(count1),Dfv(count2),
     +                   specT,DfvT,iflag)
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
C     Note: Unknown Mechanism is not currently coded.
      if (ftype .eq. -1.0) then
         mechS = 0.0
         mechN = 1.0
         mechR = 0.0
      elseif (ftype .eq. -0.5) then
         mechS = 0.0
         mechN = 1.0
         mechR = 0.0
      elseif (ftype .eq. 0.0) then
         mechS = 1.0
         mechN = 0.0
         mechR = 0.0
      elseif (ftype .eq. 0.5) then
         mechS = 0.0
         mechN = 0.0
         mechR = 1.0
      elseif (ftype .eq. 1.0) then
         mechS = 0.0
         mechN = 0.0
         mechR = 1.0
      endif

C.....First compute the Reference Rock PGA value...........
C.....This will include the regional dependence for PGA....
C.....MAGNITUDE DEPENDENCE.................................
      if (mag .le. mh(1)) then
         term1 = e1(1) + e2(1)*mechN + e3(1)*mechR +
     1           e4(1)*(mag-mh(1)) + e5(1)*(mag-mh(1))**2.0
      else
         term1 = e1(1) + e2(1)*mechN + e3(1)*mechR +
     1           e6(1)*(mag-mh(1))
      endif
C.....Distance dependence......
      Rp = SQRT( Rbjf*Rbjf+h(1)*h(1) )

C.....Apply Regional term.....

         TERM2 = ( c1(1) + c2(1)*(mag-mref) ) * alog(Rp/rref) +
     1           c3(1)  * (Rp-rref)

      pga4nl = exp(term1+term2)


C.....Now compute the requested ground motion value........
C.....MAGNITUDE DEPENDENCE.................................
      if (mag .le. mhT) then
         term1 = e1T + e2T*mechN + e3T*mechR +
     1           e4T*(mag-mhT) + e5T*(mag-mhT)**2.0
      else
         term1 = e1T + e2T*mechN + e3T*mechR +
     1           e6T*(mag-mhT)
      endif

C.....Distance dependence......
      R = SQRT( Rbjf*Rbjf+hT*hT )

C     Now apply the regional attenuation differnece.
C     Global Case

         TERM2 = ( c1T + c2T*(mag-mref) ) * alog(R/rref) +
     1        c3T * (R-rref)

C.....Site Response Term.........
C.....Now compute the site term........
C.....First the linear term......
      if (vs .le. VcT ) then
         flin = cT*alog(Vs/Vref)
      else
         flin = cT*alog(VcT/Vref)
      endif

C.....Next the non-linear term......
      f2 = f4T*(exp(f5T*(min(vs,760.0)-360.0))-exp(f5T*(760.0-360.0)))

C.....Now compute the basin effect term......
C Deviation from ln(Vs30) scaling: bedrock depth (Z1) effect for California.
      if (basinflag .eq. 1) then
c     Compute the DeltaZ1 term. Apply the California model for all regions except for Japan.
         if (regionflag .eq. 2) then
             deltaz1 = z10 -
     1           exp(-5.23/2.0 * alog((vs**2.0 + 412.29**2.0)/(1360.0**2.0 + 412.39**2.0)))/1000.0
         else
             deltaz1 = z10 -
     1           exp(-2.63/4.0 * alog((vs**4.0 + 253.0**4.0)/(2492.0**4.0 + 253.0**4.0)))/1000.0
         endif

         if (specT .le. 0.65) then
            fbasin = 0.0
         else
            fbasin = min(f7T, f6T * deltaZ1)
         endif
      else
         fbasin = 0.0
      endif

      TERM3 = flin + f1 + f2*alog((pga4nl+f3)/f3) + fBasin

      lnY = term1 + term2 + term3

      period2 = period1

c     Now compute the sigma value which is a function of magnitude and Vs
C     Tau (Eq. 4.11)
      if (mag .le. 4.5) then
         tau = t1T
      elseif (mag .gt. 4.5 .and. mag .lt. 5.5) then
         tau = t1T + (t2T - t1T)*(mag - 4.5)
      else
         tau = t2T
      endif

C     Phi - Magnitude (Eq. 4.12)
      if (mag .le. 4.5) then
         phi = l1T
      elseif (mag .gt. 4.5 .and. mag .lt. 5.5) then
         phi = l1T + (l2T - l1T)*(mag - 4.5)
      else
         phi = l2T
      endif

C     Phi - Distance (Eq. 4.13)
      if (rbjf .le. R1T) then
          phi = phi
      elseif (rbjf .gt. R1T .and. rbjf .le. R2T) then
          phi = phi + DfrT*( (alog(rbjf/R1T))/(alog(R2T/R1T)) )
      else
          phi = phi + DfrT
      endif

C     Phi - Vs30 (Eq. 4.14)
      if (vs .ge. V2) then
         phi = phi
      elseif (vs .ge. V1 .and. vs .lt. V2) then
         phi = phi - DfvT*( alog(V2/vs) / alog(V2/V1) )
      else
         phi = phi - DfvT
      endif

      sigma = sqrt (tau**2 + phi**2)

C     Convert ground motion to units of gals.
      lnY = lnY + 6.89

      return
      END
