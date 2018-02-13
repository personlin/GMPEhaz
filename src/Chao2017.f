c ------------------------------------------------------------------
      subroutine S04_Chao2017 ( mag, dist, ftype, lnY, sigma, specT, vs, Ztor, Z10,
     1            vs30_class, attenName, period2, iflag, sourcetype, phi, tau, msasflag )

      implicit none

      real mag, dip, fType, dist, vs, SA1100,
     1      Z10,  ZTOR, fltWidth, lnSa, sigma, lnY, vs30_rock, sourcetype
      real Fn, Frv, specT, period2, CRjb, phi, tau, z10_rock, SA_rock
      integer hwflag, iflag, vs30_class, regionflag, msasflag
      character*80 attenName

C     Set the reference spectrum.
c     sourcetype = 0 for crustal
c                  1 for Subduction
c     Vs30_class = 0 for estimated
c     Vs30_class = 1 for measured
C     Mainshock and Aftershocks included based on MSASFlag
C         0 = Mainshocks
C         1 = Aftershocks

c     Compute SA1100
      vs30_rock = 1100.
      z10_rock = 0.0058959
      SA_rock = 0.

         call S04_Chaoetal2017 ( mag, dist, ftype, sigma, specT, vs30_rock, Ztor, z10_rock,
     1             SA_rock, vs30_class, attenName, iflag, sourcetype, phi, tau, lnSa, msasflag)
      Sa1100 = exp(lnSa)

c     Compute Sa at spectral period for given Vs30

         call S04_Chaoetal2017 ( mag, dist, ftype, sigma, specT, vs, Ztor, Z10,
     1             sa1100, vs30_class, attenName, iflag, sourcetype, phi, tau, lnSa, msasflag )

C     Convert ground motion to units of gals.
      lnY = lnSa + 6.89

      period2 = specT

      return
      end
c -------------------------------------------------------------------
C **** Chao et al. 2017 (SSHAC model) *************
c -------------------------------------------------------------------

      subroutine S04_Chaoetal2017 ( mag, dist, ftype, sigma, specT, vs, Ztor, Z10,
     1            sa1100, vs30_class, attenName, iflag, sourcetype, phi, tau, lnSa, msasflag )

      implicit none

      integer MAXPER
      parameter (MAXPER=21)
      real ftype, dist, mag, lnSa, sigma, specT, lnYref, vs, Ztor, Z10, period1
      real period(MAXPER), c1(MAXPER), c2(MAXPER), c3(MAXPER), c4(MAXPER), c5(MAXPER)
      real c6(MAXPER), c7(MAXPER), c8(MAXPER), c9(MAXPER), c10(MAXPER), c11(MAXPER)
      real c12(MAXPER), c13(MAXPER), c14(MAXPER), c15(MAXPER), c16(MAXPER), c17(MAXPER)
      real c18(MAXPER), c19(MAXPER), c20(MAXPER), c21(MAXPER), c22(MAXPER), c23(MAXPER)
      real c24(MAXPER), c25(MAXPER), c26(MAXPER), c27(MAXPER), taucr1(MAXPER), taucr2(MAXPER)
      real tausb1(MAXPER), tausb2(MAXPER), phisscr1(MAXPER), phisscr2(MAXPER), phisssb1(MAXPER)
      real phisssb2(MAXPER), arfacr(MAXPER), arfasb(MAXPER), phis2s(MAXPER)
      character*80 attenName
      integer nper, count1, count2, C11flag, C20flag, C26flag, iflag, C10flag
      integer vs30_class, h, n, i, msasflag
      integer Fcr, Fsb, Fcrss, Fcrno, Fcrro, Fsbintra, Fsbinter, Fas, Fkuo17, Fks17, Frf, Fmanila
      real Mc, Mref, Mmax, Rrupref, Vs30ref, Zref, sourcetype
      real c1T, c2T, c3T, c4T, c5T, c6T, c7T, c8T, c9T, c10T, c11T, c12T, c13T, c14T, c15T
      real c16T, c17T, c18T, c19T, c20T, c21T, c22T, c23T, c24T, c25T, c26T, c27T
      real taucr1T, taucr2T, tausb1T, tausb2T, phisscr1T, phisscr2T, phisssb1T, phisssb2T
      real arfacrT, arfasbT, phis2sT, phi, tau, fm, SA1100, Z10ref
      real Ssource, Spath, Ssite, Ssitelin, Ssitenon, Sztor, Smag, Sgeom, Sanel
      real taucr, tausb, phisscr, phisssb, phiss, sigmass

      data period  / 0, -2.0, 0.01, 0.02, 0.03, 0.05, 0.075, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4,
     &          0.5, 0.75, 1, 1.5, 2, 3, 4, 5 /
      data c1 / -0.42682, -0.32770488, -0.42617, -0.38267, -0.30514, -0.17682, -0.06398, -0.01395,
     &          0.00575, -0.04985, -0.12963, -0.21231, -0.38179, -0.54385, -0.90047,
     &          -1.20935, -1.72357, -2.15733, -2.89398, -3.49691, -4.05677 /
      data c2 / -0.57576, -0.478751, -0.57399, -0.52904, -0.44828, -0.29911, -0.15800, -0.09117,
     &          -0.07933, -0.16741, -0.28157, -0.39457, -0.60337, -0.78423, -1.15145,
     &          -1.44846, -1.93170, -2.32776, -2.98541, -3.51400, -4.01754 /
      data c3 / -0.56614, -0.464954, -0.56367, -0.51227, -0.42253, -0.24552, -0.08661, -0.02380,
     &          -0.04936, -0.17938, -0.33569, -0.48978, -0.75722, -0.98086, -1.39035,
     &          -1.68527, -2.13907, -2.50355, -3.12984, -3.64054, -4.22973 /
      data c4 / -0.59679, -0.501209, -0.59948, -0.54896, -0.45758, -0.23559, -0.02770, 0.07032,
     &          0.06111, -0.05191, -0.19320, -0.33347, -0.56778, -0.76385, -1.20988,
     &          -1.56650, -2.15102, -2.60304, -3.33785, -3.93845, -4.56577 /
      data c5 / -0.27336, -0.214600, -0.28004, -0.23702, -0.14609, 0.09781, 0.34210, 0.47686,
     &          0.49113, 0.32613, 0.12375, -0.07993, -0.43822, -0.72362, -1.30331,
     &          -1.75247, -2.42949, -2.88125, -3.49756, -3.93043, -4.31969 /
      data c6 / -0.15066, -0.143591, -0.14965, -0.14920, -0.14601, -0.13802, -0.13783, -0.14249,
     &          -0.15125, -0.15390, -0.15240, -0.14882, -0.14188, -0.13795, -0.13962,
     &          -0.14688, -0.14972, -0.13286, -0.09571, -0.06254, -0.04672 /
      data c7 / 0.18374, 0.1740786, 0.17900, 0.18516, 0.17620, 0.17060, 0.17877, 0.19418, 0.24573,
     &          0.27648, 0.28582, 0.28807, 0.24809, 0.18660, 0.07403, 0.00114, -0.06290,
     &          -0.09683, -0.19885, -0.31814, -0.42574 /
      data c8 / 0.69358, 0.6707077, 0.69599, 0.66643, 0.61448, 0.53997, 0.54911, 0.61070, 0.75072,
     &          0.87959, 0.98255, 1.06785, 1.21051, 1.32141, 1.51422, 1.64692, 1.82708,
     &          1.94116, 2.06926, 2.13724, 2.18800 /
      data c9 / 0.65185, 0.5581807, 0.64948, 0.60930, 0.56920, 0.55149, 0.59056, 0.65929, 0.84132,
     &          1.00469, 1.14291, 1.24782, 1.37894, 1.46404, 1.56895, 1.63417, 1.65603,
     &          1.62994, 1.54933, 1.48562, 1.40692 /
      data c10 / -0.13872, -0.134137, -0.13920, -0.13329, -0.12290, -0.10799, -0.10982, -0.12214,
     &           -0.15014, -0.17592, -0.19651, -0.21357, -0.24210, -0.26428, -0.30284,
     &           -0.32938, -0.36542, -0.38823, -0.41385, -0.42745, -0.43760 /
      data c11 / -0.04953, -0.039918, -0.04495, -0.04775, -0.05401, -0.04126, -0.01951, -0.01249,
     &           -0.08485, -0.18925, -0.29110, -0.38255, -0.49316, -0.52701, -0.46679,
     &           -0.35296, -0.16278, -0.07360, -0.00361, 0.00000, 0.00000 /
      data c12 / 0.02846, 0.0288010, 0.02853, 0.02907, 0.03050, 0.03437, 0.03732, 0.03774, 0.03435,
     &           0.02885, 0.02380, 0.01946, 0.01337, 0.00967, 0.00559, 0.00419, 0.00260,
     &           0.00116, -0.00263, -0.00665, -0.01553 /
      data c13 / 0.00919, 0.0101329, 0.00923, 0.00988, 0.01075, 0.01238, 0.01338, 0.01332, 0.01179,
     &           0.00982, 0.00785, 0.00624, 0.00411, 0.00268, 0.00117, 0.00087, 0.00059,
     &           0.00024, -0.00096, -0.00228, -0.00431 /
      data c14 / -1.90982, -1.935253, -1.90965, -1.93011, -1.97278, -1.95803, -1.87814, -1.77781,
     &           -1.65632, -1.57576, -1.52224, -1.49528, -1.46281, -1.44274, -1.41615,
     &           -1.38993, -1.35565, -1.33932, -1.32171, -1.31274, -1.28503 /
      data c15 / -1.63583, -1.634563, -1.63218, -1.64190, -1.67487, -1.74968, -1.79673, -1.79306,
     &           -1.72580, -1.64287, -1.57054, -1.51230, -1.43027, -1.37373, -1.25896,
     &           -1.18777, -1.10246, -1.06588, -1.02734, -0.99037, -0.94811 /
      data c16 / 0.34806, 0.3584079, 0.34729, 0.35611, 0.37509, 0.39998, 0.38893, 0.36491, 0.31819,
     &           0.27725, 0.24717, 0.22297, 0.18919, 0.17368, 0.17194, 0.18538, 0.20432,
     &           0.21646, 0.23573, 0.25375, 0.26914 /
      data c17 / 0.20176, 0.2166604, 0.20014, 0.21494, 0.22245, 0.20085, 0.15835, 0.12373, 0.08516,
     &           0.06822, 0.05435, 0.04964, 0.05306, 0.05612, 0.08094, 0.10162, 0.14942,
     &           0.19708, 0.26366, 0.30340, 0.31352 /
      data c18 / 0.00000, 0.0003798, 0.00000, 0.00000, -0.00003, -0.00161, -0.00363, -0.00514, -0.00570,
     &           -0.00512, -0.00429, -0.00334, -0.00191, -0.00106, -0.00012, 0.00000, 0.00000,
     &           0.00000, -0.00003, -0.00006, -0.00024 /
      data c19 / -0.00315, -0.003266, -0.00316, -0.00334, -0.00337, -0.00324, -0.00312, -0.00315, -0.00320,
     &           -0.00306, -0.00283, -0.00259, -0.00222, -0.00198, -0.00196, -0.00196, -0.00200,
     &           -0.00211, -0.00255, -0.00318, -0.00362 /
      data c20 / -3.37443, -3.129942, -3.37957, -3.26276, -3.12039, -2.77699, -2.45023, -2.19165, -1.85215,
     &           -1.62798, -1.49980, -1.44580, -1.45380, -1.52110, -1.59305, -1.46595, -0.89634,
     &           -0.48495, -0.02762, 0.00000, 0.00000 /
      data c21 / -0.53674, -0.534043, -0.53652, -0.52911, -0.51328, -0.48078, -0.46572, -0.46949, -0.49343,
     &           -0.51786, -0.54454, -0.57046, -0.62522, -0.68213, -0.78909, -0.85442, -0.91005,
     &           -0.92335, -0.91464, -0.89283, -0.85483 /
      data c22 / 0.04523, 0.0458655, 0.04544, 0.04662, 0.04976, 0.05873, 0.06324, 0.06127, 0.05315, 0.04813,
     &           0.04907, 0.05354, 0.06521, 0.07589, 0.09600, 0.11018, 0.12800, 0.13712, 0.13748,
     &           0.12787, 0.11250 /
      data c23 / -0.58641, -0.544826, -0.58670, -0.55816, -0.50549, -0.40157, -0.32642, -0.30593, -0.34304,
     &           -0.42439, -0.51573, -0.60347, -0.76233, -0.89923, -1.15933, -1.34357, -1.58317,
     &           -1.71310, -1.79927, -1.79038, -1.70330 /
      data c24 / -0.66005, -0.614151, -0.66006, -0.63650, -0.59453, -0.50578, -0.43321, -0.40897, -0.43638,
     &           -0.50625, -0.58308, -0.65623, -0.79171, -0.91483, -1.16552, -1.35415, -1.59529,
     &           -1.72062, -1.79429, -1.77800, -1.68840 /
      data c25 / -0.57611, -0.521623, -0.57563, -0.54296, -0.47735, -0.34285, -0.25612, -0.24798, -0.33656,
     &           -0.46536, -0.58483, -0.68520, -0.84288, -0.96167, -1.17506, -1.33319, -1.54295,
     &           -1.65505, -1.71968, -1.70446, -1.64548 /
      data c26 / -0.09885, -0.098846, -0.09648, -0.05630, -0.01620, 0.00151, -0.03756, -0.10629, -0.28832,
     &           -0.45169, -0.58991, -0.69482, -0.82594, -0.91104, -0.99861, -1.01217, -0.95103,
     &           -0.86194, -0.68633, -0.55062, -0.41292 /
      data c27 / -0.22539, -0.225386, -0.22873, -0.20875, -0.20487, -0.25149, -0.25548, -0.28929, -0.39132,
     &           -0.49569, -0.58791, -0.65482, -0.72694, -0.76904, -0.80158, -0.82217, -0.79503,
     &           -0.74594, -0.64933, -0.58562, -0.50692 /
      data taucr1 / 0.32270, 0.333360, 0.32290, 0.33291, 0.34636, 0.36504, 0.36988, 0.36764, 0.36766,
     &              0.38435, 0.40767, 0.42422, 0.44443, 0.45178, 0.44390, 0.43348, 0.41861,
     &              0.39601, 0.37594, 0.36949, 0.42843 /
      data taucr2 / 0.32595, 0.323792, 0.32442, 0.32587, 0.32941, 0.34135, 0.35775, 0.36540, 0.35112,
     &              0.32432, 0.30171, 0.28995, 0.29053, 0.31136, 0.38256, 0.43299, 0.48885,
     &              0.51573, 0.53354, 0.54041, 0.49538 /
      data tausb1 / 0.25683, 0.223224, 0.25386, 0.25123, 0.24759, 0.24218, 0.24447, 0.25710, 0.31560,
     &              0.38679, 0.44282, 0.48555, 0.53440, 0.54554, 0.52292, 0.48796, 0.43807,
     &              0.39735, 0.34954, 0.30732, 0.33465 /
      data tausb2 / 0.59159, 0.627475, 0.59451, 0.61853, 0.64800, 0.69230, 0.71020, 0.69445, 0.60944,
     &              0.53922, 0.49368, 0.46672, 0.46083, 0.48019, 0.52884, 0.58232, 0.65015,
     &              0.68099, 0.66195, 0.60955, 0.52706 /
      data phisscr1 / 0.53254, 0.547197, 0.53259, 0.52733, 0.52005, 0.50843, 0.50827, 0.52071, 0.55228,
     &              0.57885, 0.59736, 0.60769, 0.61086, 0.60090, 0.55589, 0.51336, 0.45771,
     &              0.42721, 0.39838, 0.38461, 0.37698 /
      data phisscr2 / 0.42278, 0.434184, 0.42262, 0.42746, 0.43800, 0.45841, 0.46418, 0.45535, 0.43212,
     &              0.41800, 0.41410, 0.41806, 0.43197, 0.44586, 0.47073, 0.48275, 0.48885,
     &              0.48432, 0.46852, 0.45047, 0.44101 /
      data phisssb1 / 0.46609, 0.466874, 0.46818, 0.46163, 0.44776, 0.41437, 0.39915, 0.40914, 0.45847,
     &              0.49875, 0.51974, 0.53135, 0.53489, 0.52593, 0.50290, 0.48257, 0.45684,
     &              0.44057, 0.42007, 0.40229, 0.37888 /
      data phisssb2 / 0.46213, 0.472038, 0.46184, 0.46785, 0.47969, 0.50154, 0.50900, 0.50192, 0.47897,
     &              0.45854, 0.44536, 0.43689, 0.43234, 0.43663, 0.45184, 0.46434, 0.46862,
     &              0.46433, 0.43642, 0.40445, 0.34992 /
      data arfacr / 0.25318, 0.229696, 0.25414, 0.26085, 0.26648, 0.25936, 0.23528, 0.21591, 0.21253,
     &              0.21733, 0.21368, 0.20487, 0.18226, 0.16450, 0.16298, 0.17668, 0.18885,
     &              0.18576, 0.18142, 0.19052, 0.19394 /
      data arfasb / 0.19692, 0.172664, 0.20383, 0.20481, 0.20131, 0.19567, 0.19501, 0.19415, 0.18761,
     &              0.18374, 0.18200, 0.17591, 0.16516, 0.15715, 0.15460, 0.17358, 0.22851,
     &              0.26757, 0.31216, 0.33304, 0.35954 /
      data phis2s / 0.31600, 0.312931, 0.31613, 0.32192, 0.33755, 0.38533, 0.42476, 0.43829, 0.42157,
     &              0.39303, 0.36995, 0.35307, 0.33529, 0.32910, 0.33174, 0.34038, 0.35318,
     &              0.36003, 0.36784, 0.37191, 0.38065 /

c Set attenuation name
c     Sourcetype = 0 Crustal
c     Sourcetype = 1 Subduction

C Find the requested spectral period and corresponding coefficients
      nper = 21

C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
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
        c20T = c20(1)
        c21T = c21(1)
        c22T = c22(1)
        c23T = c23(1)
        c24T = c24(1)
        c25T = c25(1)
        c26T = c26(1)
        c27T = c27(1)
        taucr1T = taucr1(1)
        taucr2T = taucr2(1)
        tausb1T = tausb1(1)
        tausb2T = tausb2(1)
        phisscr1T = phisscr1(1)
        phisscr2T = phisscr2(1)
        phisssb1T = phisssb1(1)
        phisssb2T = phisssb2(1)
        arfacrT = arfacr(1)
        arfasbT = arfasb(1)
        phis2sT = phis2s(1)
       goto 1011
C   Function Form for PGAraw, max Regression
       elseif (specT .eq. -2.0) then
         period1 = period(2)
         c1T = c1(2)
         c2T = c2(2)
         c3T = c3(2)
         c4T = c4(2)
         c5T = c5(2)
         c6T = c6(2)
         c7T = c7(2)
         c8T = c8(2)
         c9T = c9(2)
         c10T = c10(2)
         c11T = c11(2)
         c12T = c12(2)
         c13T = c13(2)
         c14T = c14(2)
         c15T = c15(2)
         c16T = c16(2)
         c17T = c17(2)
         c18T = c18(2)
         c19T = c19(2)
         c20T = c20(2)
         c21T = c21(2)
         c22T = c22(2)
         c23T = c23(2)
         c24T = c24(2)
         c25T = c25(2)
         c26T = c26(2)
         c27T = c27(2)
         taucr1T = taucr1(2)
         taucr2T = taucr2(2)
         tausb1T = tausb1(2)
         tausb2T = tausb2(2)
         phisscr1T = phisscr1(2)
         phisscr2T = phisscr2(2)
         phisssb1T = phisssb1(2)
         phisssb2T = phisssb2(2)
         arfacrT = arfacr(2)
         arfasbT = arfasb(2)
         phis2sT = phis2s(2)
         goto 1011
       endif
C Now loop over the spectral period range of the attenuation relationship.
         do i=3,nper-1
            if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
               count1 = i
               count2 = i+1
               goto 1010
            endif
         enddo

      write (*,*)
      write (*,*) 'Chao et al. (2017) Horizontal atttenuation model'
      write (*,*) 'is not defined for a spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1010    call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +                specT,c1T,iflag)
         call S24_interp (period(count1),period(count2),c2(count1),c2(count2),
     +                specT,c2T,iflag)
         call S24_interp (period(count1),period(count2),c3(count1),c3(count2),
     +                specT,c3T,iflag)
         call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +                specT,c4T,iflag)
         call S24_interp (period(count1),period(count2),c5(count1),c5(count2),
     +                specT,c5T,iflag)
         call S24_interp (period(count1),period(count2),c6(count1),c6(count2),
     +                specT,c6T,iflag)
         call S24_interp (period(count1),period(count2),c7(count1),c7(count2),
     +                 specT,c7T,iflag)
         call S24_interp (period(count1),period(count2),c8(count1),c8(count2),
     +                 specT,c8T,iflag)
         call S24_interp (period(count1),period(count2),c9(count1),c9(count2),
     +                 specT,c9T,iflag)
         call S24_interp (period(count1),period(count2),c10(count1),c10(count2),
     +                 specT,c10T,iflag)
         call S24_interp (period(count1),period(count2),c11(count1),c11(count2),
     +                 specT,c11T,iflag)
         call S24_interp (period(count1),period(count2),c12(count1),c12(count2),
     +                 specT,c12T,iflag)
         call S24_interp (period(count1),period(count2),c13(count1),c13(count2),
     +                 specT,c13T,iflag)
         call S24_interp (period(count1),period(count2),c14(count1),c14(count2),
     +                 specT,c14T,iflag)
         call S24_interp (period(count1),period(count2),c15(count1),c15(count2),
     +                 specT,c15T,iflag)
         call S24_interp (period(count1),period(count2),c16(count1),c16(count2),
     +                 specT,c16T,iflag)
         call S24_interp (period(count1),period(count2),c17(count1),c17(count2),
     +                 specT,c17T,iflag)
         call S24_interp (period(count1),period(count2),c18(count1),c18(count2),
     +                 specT,c18T,iflag)
         call S24_interp (period(count1),period(count2),c19(count1),c19(count2),
     +                 specT,c19T,iflag)
         call S24_interp (period(count1),period(count2),c20(count1),c20(count2),
     +                 specT,c20T,iflag)
         call S24_interp (period(count1),period(count2),c21(count1),c21(count2),
     +                 specT,c21T,iflag)
         call S24_interp (period(count1),period(count2),c22(count1),c22(count2),
     +                 specT,c22T,iflag)
         call S24_interp (period(count1),period(count2),c23(count1),c23(count2),
     +                 specT,c23T,iflag)
         call S24_interp (period(count1),period(count2),c24(count1),c24(count2),
     +                 specT,c24T,iflag)
         call S24_interp (period(count1),period(count2),c25(count1),c25(count2),
     +                 specT,c25T,iflag)
         call S24_interp (period(count1),period(count2),c26(count1),c26(count2),
     +                 specT,c26T,iflag)
         call S24_interp (period(count1),period(count2),c27(count1),c27(count2),
     +                 specT,c27T,iflag)
         call S24_interp (period(count1),period(count2),taucr1(count1),taucr1(count2),
     +                 specT,taucr1T,iflag)
         call S24_interp (period(count1),period(count2),taucr2(count1),taucr2(count2),
     +                 specT,taucr2T,iflag)
         call S24_interp (period(count1),period(count2),tausb1(count1),tausb1(count2),
     +                 specT,tausb1T,iflag)
         call S24_interp (period(count1),period(count2),tausb2(count1),tausb2(count2),
     +                 specT,tausb2T,iflag)
         call S24_interp (period(count1),period(count2),phisscr1(count1),phisscr1(count2),
     +                 specT,phisscr1T,iflag)
         call S24_interp (period(count1),period(count2),phisscr2(count1),phisscr2(count2),
     +                 specT,phisscr2T,iflag)
         call S24_interp (period(count1),period(count2),phisssb1(count1),phisssb1(count2),
     +                 specT,phisssb1T,iflag)
         call S24_interp (period(count1),period(count2),phisssb2(count1),phisssb2(count2),
     +                 specT,phisssb2T,iflag)
         call S24_interp (period(count1),period(count2),arfacr(count1),arfacr(count2),
     +                 specT,arfacrT,iflag)
         call S24_interp (period(count1),period(count2),arfasb(count1),arfasb(count2),
     +                 specT,arfasbT,iflag)
         call S24_interp (period(count1),period(count2),phis2s(count1),phis2s(count2),
     +                specT,phis2sT,iflag)

 1011 period1 = specT

      h = 10.0
      n = 2.0
      Mc = 7.1
      Mref = 5.5
      Mmax = 8
      Rrupref = 0.0
      Vs30ref = 760.0

C     Set the reference spectrum.
c     sourcetype = 0 for crustal
c                  1 for Subduction
c     Vs30_class = 0 for estimated
c     Vs30_class = 1 for measured

      Fcr=0
      Fsb=0
      Fcrss = 0
      Fcrno = 0
      Fcrro = 0
      Fsbintra = 0
      Fsbinter = 0
      Fas = 0
      Fkuo17 = 0
      Fks17 = 0
      Frf = 0
      Fmanila = 0
      C11flag = 0
      C20flag = 0
      C26flag = 0

      if (sourcetype .eq. 0.0 ) then
       Fcr = 1
       Zref = 15
         if(ftype .gt. 0) then
              Fcrro = 1
           elseif(ftype .lt. 0) then
              Fcrno = 1
           else
              Fcrss = 1
         endif
      elseif (sourcetype .eq. 1.0 ) then
        Fsb = 1
          Zref = 50
         if(ftype .eq. 0) then
              Fsbinter = 1
           elseif(ftype .eq. 1) then
              Fsbintra = 1
         endif
      endif

C     Add aftershock factor
      if (msasflag .eq. 1) then
           Fas = 1
      endif

C     choose Site ref by Vs30 class
        if (vs30_class .eq. 0 ) then
         Fks17 = 1
        elseif (vs30_class .eq. 1) then
         Fkuo17 = 1
        endif

      lnYref = c1T*Fcrro + c2T*Fcrss + c3T*Fcrno + c4T*Fsbinter + c5T*Fsbintra +
     &         c6T*Fas + c7T*Fmanila + c23T*Fkuo17 + c24T*Fks17 + c25T*Frf

C     Set Source scaling term

      if(mag .LE. 5 ) then
	      C11flag=1
      endif
      if(mag .GE. Mc ) then
	      C26flag=1
      endif
      if(mag .GE. 7.6 ) then
	      C10flag=1
      endif

      if (sourcetype .eq. 0.0 ) then
        Smag = c8T*(mag - Mref) + c10T*(mag - Mref)**2
     1         - c10T*(mag-7.6)**2*C10flag + c11T*(5-mag)*C11flag
      elseif (sourcetype .eq. 1.0 ) then
        Smag = c9T*(mag - Mref) + c26T*Fsbinter*(Mag-Mc)*c26flag + c27T*Fsbintra*(Mag-Mc)*c26flag
      endif

      Sztor = c12T * Fcr *(Ztor-Zref) + c13T * Fsb * ((Ztor-Zref))
      Ssource = Smag + Sztor

C     Set Path scaling term

      if (sourcetype .eq. 0.0 ) then
          Sgeom = (c14T + c16T*(min(mag,Mmax)- Mref )) * alog(SQRT(dist**2 + h**2)/SQRT(Rrupref**2 + h**2))
      elseif (sourcetype .eq. 1.0 ) then
          Sgeom = (c15T + c17T*(min(mag,Mc)- Mref )) * alog(SQRT(dist**2 + h**2)/SQRT(Rrupref**2 + h**2))
      endif

      Sanel = c18T*Fcr*(dist-Rrupref) + c19T*Fsb*(dist-Rrupref)
      Spath = Sgeom + Sanel

C     Set Site scaling term

      Z10ref = exp((-4.08/2)*alog((vs**2+355.4**2)/(1750**2+355.4**2)))
      Ssitelin = c21T * alog(vs/vs30ref) + c22T*alog(Z10*1000/Z10ref)

      if(vs .LT. vs30ref ) then
           C20flag=1
      endif

      Ssitenon = c20T * C20flag * (-1.5*alog(vs/vs30ref)-alog(SA1100+2.4)+alog(SA1100+2.4*(vs/vs30ref)**1.5))
      Ssite = Ssitenon + Ssitelin

      lnSa =  lnYref + Ssource + Spath + Ssite

c 	  write(*,*) "lnYref = ", lnYref
c 	  write(*,*) "Ssource = ", Ssource
c 	  write(*,*) "Spath = ", Spath
c 	  write(*,*) "Ssitelin = ", Ssitelin
c 	  write(*,*) "Ssitenon = ", Ssitenon
c 	  write(*,*) "lnSa = ", lnSa
c 	  write(*,*) "Sa = ", exp(lnSa)


C     Set the event-specific residual term

      fm = 0.5*(min(6.5, max(4.5, mag))-4.5)

      taucr = taucr1T + (taucr2T - taucr1T)*fm
      tausb = tausb1T + (tausb2T - tausb1T)*fm

      tau = taucr*Fcr + tausb*Fsb

C     Set Site-specific residual term



C     Set Recoed-specific residual term

      phisscr = phisscr1T + (phisscr2T -phisscr1T)*fm
      phisssb = phisssb1T + (phisssb2T -phisssb1T)*fm

      phiss = phisscr*Fcr + phisssb*Fsb

      phi=(phis2sT**2+phiss**2)**0.5
      sigma=(tau**2+phi**2)**0.5
      sigmass=(tau**2+phiss**2)**0.5



c       write(*,*) "Y(gal) = ", exp(lnSa)

      return
      end
