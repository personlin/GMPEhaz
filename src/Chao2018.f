c ------------------------------------------------------------------
C *** Chao2018 (Crustal and Subduction - Model) Horizontal ***********
c ------------------------------------------------------------------
      subroutine S04_Chao2018 ( mag, dist, ftype, lnY, sigma, specT, vs, Ztor, Z10,
     1            vs30_class, attenName, period2, iflag, sourcetype, phi, tau, msasflag )

      implicit none
C    2018/08/31 revised
      real mag, dip, fType, dist, vs, SA1180,
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

c     Compute SA1180
      vs30_rock = 1180.
      z10_rock = 0.0058959
      SA_rock = 0.

         call S04_Chaoetal2018 ( mag, dist, ftype, sigma, specT, vs30_rock, Ztor, z10_rock,
     1             SA_rock, vs30_class, attenName, iflag, sourcetype, phi, tau, lnSa, msasflag)
      Sa1180 = exp(lnSa)

c     Compute Sa at spectral period for given Vs30

         call S04_Chaoetal2018 ( mag, dist, ftype, sigma, specT, vs, Ztor, Z10,
     1             sa1180, vs30_class, attenName, iflag, sourcetype, phi, tau, lnSa, msasflag )

C     Convert ground motion to units of gals.
      lnY = lnSa + 6.89

      period2 = specT

      return
      end
c -------------------------------------------------------------------
C **** Chao et al. 2018 (SSHAC model) *************
c -------------------------------------------------------------------

      subroutine S04_Chaoetal2018 ( mag, dist, ftype, sigma, specT, vs, Ztor, Z10,
     1            sa1180, vs30_class, attenName, iflag, sourcetype, phi, tau, lnSa, msasflag )

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
      integer nper, count1, count2, C11flag, C23flag, C29flag, iflag, C10flag, C13flag
      integer vs30_class, h, n, i, msasflag
      integer Fcr, Fsb, Fcrss, Fcrno, Fcrro, Fsbintra, Fsbinter, Fas, Fkuo17, Fks17, Frf, Fmanila
      real Mc, Mref, Mmax, Rrupref, Vs30ref, Zref, sourcetype
      real c1T, c2T, c3T, c4T, c5T, c6T, c7T, c8T, c9T, c10T, c11T, c12T, c13T, c14T, c15T
      real c16T, c17T, c18T, c19T, c20T, c21T, c22T, c23T, c24T, c25T, c26T, c27T
      real taucr1T, taucr2T, tausb1T, tausb2T, phisscr1T, phisscr2T, phisssb1T, phisssb2T
      real arfacrT, arfasbT, phis2sT, phi, tau, fm, SA1180, Z10ref
      real Ssource, Spath, Ssite, Ssitelin, Ssitenon, Sztor, Smag, Sgeom, Sanel
      real taucr, tausb, phisscr, phisssb, phiss, sigmass
      real c28(MAXPER), c29(MAXPER), c28T, c29T, c30(MAXPER), c30T

      data Period / 0, -2, 0.01, 0.02, 0.03, 0.05, 0.075, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75,
     1              1, 1.5, 2, 3, 4, 5 /
      data c1 / -1.042666, 1.895411, -1.042360, -1.009040, -0.962601, -0.870563, -0.785856, -0.727776,
     1          -0.674517, -0.668684, -0.682285, -0.707153, -0.782246, -0.867588, -1.100953, -1.335849,
     1          -1.769889, -2.158271, -2.821190, -3.354776, -3.867019 /
      data c2 / -1.137852, 1.735667, -1.137811, -1.101142, -1.049407, -0.939899, -0.832404, -0.758714,
     1          -0.705514, -0.716938, -0.752775, -0.800309, -0.908875, -1.016989, -1.278817, -1.515900,
     1          -1.926198, -2.280583, -2.876528, -3.355129, -3.810868 /
      data c3 / -1.170990, 1.587449, -1.171510, -1.130461, -1.066827, -0.931421, -0.802566, -0.723218,
     1          -0.690970, -0.741863, -0.819872, -0.908821, -1.075545, -1.217121, -1.508285, -1.746629,
     1          -2.151367, -2.498264, -3.089388, -3.563977, -4.047973 /
      data c4 / -1.314590, 1.882708, -1.298813, -1.262369, -1.214610, -1.092116, -0.964326, -0.894881,
     1          -0.836942, -0.792193, -0.763280, -0.744597, -0.723252, -0.734591, -0.883649, -1.088087,
     1          -1.526738, -1.947331, -2.666122, -3.258068, -3.809328 /
      data c5 / -0.457036, 2.359015, -0.436214, -0.384335, -0.311696, -0.131992, 0.074716, 0.200368, 0.286613,
     1          0.254600, 0.173035, 0.087029, -0.073676, -0.220276, -0.567236, -0.881354, -1.445217, -1.943705,
     1          -2.722921, -3.349385, -3.792075 /
      data c6 / -0.124087, -0.144577, -0.123275, -0.124986, -0.127909, -0.127668, -0.125831, -0.122221,
     1          -0.115139, -0.105684, -0.095876, -0.088255, -0.081799, -0.084061, -0.105026, -0.125539,
     1          -0.137664, -0.125372, -0.085938, -0.049451, 0.012311 /
      data c7 / 0.198357, -0.038842, 0.191344, 0.195147, 0.198967, 0.227624, 0.260255, 0.289816, 0.348032,
     1          0.370540, 0.361912, 0.339973, 0.258518, 0.169337, 0.025880, -0.053324, -0.113254, -0.138977,
     1          -0.193895, -0.258713, -0.326743 /
      data c8 / 0.676721, 1.251174, 0.684339, 0.657072, 0.610194, 0.544252, 0.548393, 0.602698, 0.739429,
     1          0.875915, 0.986450, 1.078964, 1.233393, 1.354611, 1.564898, 1.701387, 1.863505, 1.953011,
     1          2.043382, 2.088718, 2.105341 /
      data c9 / 0.633834, 0.766604, 0.640018, 0.606922, 0.558754, 0.535818, 0.603777, 0.687135, 0.829456,
     1          0.904226, 0.958892, 0.984563, 1.007287, 1.025986, 1.013332, 1.002811, 0.962813, 0.979659,
     1          1.052087, 1.157880, 1.147008 /
      data c10 / -0.135342, -0.210085, -0.136868, -0.131414, -0.122038, -0.108827, -0.109545, -0.120401,
     1          -0.147755, -0.175139, -0.197287, -0.215793, -0.246679, -0.270922, -0.312979, -0.339111,
     1          -0.357148, -0.352155, -0.323695, -0.294607, -0.250719 /
      data c11 / -0.001320, -0.000002, -0.000002, -0.006396, -0.008323, -0.005923, -0.001799, -0.010118,
     1          -0.080308, -0.163717, -0.238633, -0.297686, -0.346393, -0.334133, -0.229548, -0.131664,
     1          -0.032403, -0.006790, -0.000014, -0.000002, -0.000001 /
      data c12 / -0.000034, -0.000014, -0.000001, -0.000004, -0.000005, -0.000016, -0.000069, -0.009043,
     1          -0.105380, -0.189907, -0.238782, -0.268913, -0.261917, -0.203064, -0.102740, -0.049805,
     1          -0.011622, -0.002702, -0.000015, -0.000001, -0.000001 /
      data c13 / -0.000038, -0.291529, -0.000001, -0.000004, -0.000005, -0.000018, -0.000080, -0.000082,
     1          -0.011888, -0.070362, -0.139438, -0.217492, -0.359123, -0.475375, -0.670648, -0.775270,
     1          -0.856757, -0.798850, -0.639826, -0.461969, -0.465749 /
      data c14 / 0.032555, 0.015510, 0.032494, 0.033028, 0.034285, 0.037352, 0.039960, 0.040658, 0.038553,
     1          0.034446, 0.030226, 0.026384, 0.020530, 0.016445, 0.010777, 0.008159, 0.005621, 0.004032,
     1          0.000622, -0.002970, -0.008948 /
      data c15 / 0.017948, 0.016982, 0.018059, 0.018141, 0.018067, 0.017682, 0.017907, 0.018645, 0.020842,
     1          0.022069, 0.022125, 0.021736, 0.020462, 0.018980, 0.016322, 0.014953, 0.012425, 0.009349,
     1          0.004309, -0.000454, 0.000502 /
      data c16 / 0.006634, 0.003036, 0.006626, 0.007008, 0.007575, 0.008618, 0.009183, 0.009159, 0.008404,
     1          0.007375, 0.006330, 0.005344, 0.003788, 0.002623, 0.001242, 0.000937, 0.000745, 0.000647,
     1          0.000241, 0.000051, -0.000294 /
      data c17 / -1.687472, -1.547786, -1.692461, -1.730349, -1.772040, -1.821470, -1.810850, -1.759544,
     1          -1.643299, -1.551105, -1.488492, -1.446763, -1.398596, -1.375008, -1.347383, -1.333650,
     1          -1.311795, -1.298714, -1.284844, -1.281901, -1.241532 /
      data c18 / -1.525058, -1.549392, -1.539999, -1.565127, -1.589292, -1.642560, -1.682785, -1.676278,
     1          -1.637058, -1.599726, -1.564986, -1.538416, -1.509663, -1.488699, -1.436447, -1.401868,
     1          -1.333535, -1.279867, -1.210371, -1.152781, -1.113843 /
      data c19 / 0.393879, 0.303285, 0.388200, 0.395410, 0.410623, 0.421673, 0.405707, 0.381442, 0.338659,
     1          0.299124, 0.268677, 0.244569, 0.210459, 0.192077, 0.179792, 0.184209, 0.197091, 0.210361,
     1          0.233088, 0.252812, 0.264010 /
      data c20 / 0.192940, 0.223730, 0.188931, 0.201543, 0.217245, 0.203093, 0.152740, 0.113630, 0.072366,
     1          0.055866, 0.044190, 0.040593, 0.041938, 0.044384, 0.071700, 0.099141, 0.153764, 0.199871,
     1          0.253750, 0.282276, 0.298510 /
      data c21 / -0.003430, -0.000817, -0.003338, -0.003155, -0.003190, -0.003740, -0.004703, -0.005494,
     1          -0.006073, -0.005732, -0.005091, -0.004380, -0.003179, -0.002346, -0.001304, -0.000879,
     1          -0.000564, -0.000475, -0.000459, -0.000460, -0.000879 /
      data c22 / -0.003964, -0.000992, -0.003844, -0.003962, -0.004180, -0.004364, -0.004364, -0.004447,
     1          -0.004295, -0.003755, -0.003171, -0.002613, -0.001680, -0.001075, -0.000534, -0.000357,
     1          -0.000460, -0.000784, -0.001435, -0.002141, -0.002623 /
      data c23 / -2.405229, -6.913488, -2.378641, -2.418630, -2.313209, -2.067232, -1.854300, -1.693198,
     1          -1.479479, -1.358600, -1.314082, -1.320243, -1.391252, -1.483500, -1.558715, -1.404591,
     1          -0.845899, -0.436331, -0.023390, 0.000000, 0.000000 /
      data c24 / -0.478715, -0.672424, -0.477706, -0.470430, -0.451950, -0.416240, -0.402645, -0.410470,
     1          -0.446615, -0.481032, -0.513790, -0.542807, -0.596688, -0.648563, -0.742180, -0.798604,
     1          -0.842403, -0.849752, -0.839143, -0.822511, -0.797141 /
      data c25 / 0.063345, 0.095748, 0.063485, 0.064655, 0.068531, 0.079314, 0.084622, 0.082002, 0.070590,
     1          0.062519, 0.061465, 0.064345, 0.074007, 0.083563, 0.103379, 0.118159, 0.139920, 0.153222,
     1           0.160180, 0.155343, 0.142666 /
      data c26 / -0.604023, 0.384203, -0.598271, -0.547902, -0.480919, -0.338469, -0.228587, -0.197662,
     1          -0.245076, -0.344219, -0.449905, -0.550299, -0.725882, -0.874283, -1.154718, -1.353035,
     1          -1.618376, -1.771092, -1.893009, -1.912582, -1.838861 /
      data c27 / -0.679356, 0.383524, -0.673352, -0.626423, -0.567634, -0.437480, -0.333207, -0.303548,
     1           -0.352291, -0.441969, -0.532257, -0.615964, -0.760168, -0.886623, -1.140546, -1.331239,
     1          -1.584174, -1.727634, -1.829259, -1.833069, -1.746378 /
      data c28 / -0.650493, 0.305582, -0.644569, -0.596370, -0.529756, -0.377565, -0.258657, -0.227982,
     1          -0.292081, -0.413965, -0.533268, -0.639714, -0.817367, -0.960564, -1.232582, -1.435782,
     1          -1.715330, -1.875696, -1.998774, -2.017655, -1.941254 /
      data c29 / -0.080834, 0.000000, -0.087018, -0.053922, -0.005754, 0.017182, -0.050777, -0.134135,
     1          -0.276456, -0.351226, -0.405892, -0.431563, -0.454287, -0.472986, -0.442999, -0.380811,
     1           -0.257813, -0.211659, -0.189087, -0.222880, -0.153008 /
      data c30 / -0.207374, 0.000000, -0.219268, -0.206372, -0.194424, -0.235818, -0.268697, -0.317135,
     1          -0.379456, -0.395226, -0.403892, -0.391563, -0.355287, -0.330986, -0.245965, -0.190811,
     1          -0.101813, -0.095659, -0.152087, -0.257880, -0.247008 /
      data taucr1 / 0.367027, 0.439871, 0.366713, 0.367381, 0.365814, 0.360738, 0.364067, 0.376999,
     1          0.420380, 0.470706, 0.514142, 0.545754, 0.580926, 0.595221, 0.586341, 0.567362, 0.539173,
     1          0.521031, 0.510998, 0.516268, 0.543015 /
      data taucr2 / 0.315219, 0.374796, 0.315430, 0.319633, 0.327049, 0.345572, 0.360231, 0.362292, 0.339502,
     1          0.307997, 0.285602, 0.273645, 0.271198, 0.286852, 0.345657, 0.391395, 0.441542, 0.462329,
     1          0.475059, 0.480575, 0.470049 /
      data tausb1 / 0.272750, 0.330733, 0.271058, 0.271673, 0.274006, 0.280698, 0.291948, 0.305046, 0.334109,
     1          0.369176, 0.401260, 0.428927, 0.466550, 0.482919, 0.477346, 0.450142, 0.405823, 0.366654,
     1          0.325573, 0.294383, 0.340797 /
      data tausb2 / 0.532703, 0.566695, 0.536421, 0.557253, 0.577443, 0.601923, 0.599473, 0.579989, 0.528112,
     1          0.495023, 0.475578, 0.464138, 0.466708, 0.482046, 0.524715, 0.574218, 0.638071, 0.666590,
     1          0.648573, 0.596551, 0.507714 /
      data phisscr1 / 0.530015, 0.558388, 0.530680, 0.521767, 0.513496, 0.502312, 0.503270, 0.517247, 0.553158,
     1          0.581637, 0.599292, 0.608899, 0.610791, 0.600179, 0.561837, 0.526088, 0.477128, 0.448854,
     1          0.421072, 0.408714, 0.406221 /
      data phisscr2 / 0.433766, 0.435606, 0.435202, 0.444543, 0.455987, 0.470816, 0.466840, 0.451769,
     1          0.427517, 0.417334, 0.416486, 0.422015, 0.436118, 0.448613, 0.471981, 0.484933, 0.492704,
     1          0.488214, 0.469356, 0.447792, 0.429747 /
      data phisssb1 / 0.430634, 0.478613, 0.431917, 0.426561, 0.421787, 0.407957, 0.406584, 0.419057,
     1          0.454812, 0.477921, 0.489639, 0.495038, 0.494487, 0.491319, 0.486749, 0.481815, 0.481313,
     1          0.479424, 0.475432, 0.466465, 0.454073 /
      data phisssb2 / 0.495377, 0.485676, 0.494439, 0.498452, 0.505789, 0.519604, 0.525237, 0.521053,
     1           0.511584, 0.503919, 0.496864, 0.491227, 0.487558, 0.486774, 0.488927, 0.493538, 0.488128,
     1           0.477724, 0.446953, 0.414410, 0.368507 /
      data phis2s / 0.342398, 0.273837, 0.342667, 0.348735, 0.363743, 0.407580, 0.443690, 0.455723,
     1          0.439179, 0.411759, 0.389287, 0.372044, 0.352699, 0.344485, 0.341557, 0.347037, 0.357107,
     1           0.363360, 0.369654, 0.372377, 0.375576 /

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
        c28T = c28(1)
        c29T = c29(1)
        c30T = c30(1)
        taucr1T = taucr1(1)
        taucr2T = taucr2(1)
        tausb1T = tausb1(1)
        tausb2T = tausb2(1)
        phisscr1T = phisscr1(1)
        phisscr2T = phisscr2(1)
        phisssb1T = phisssb1(1)
        phisssb2T = phisssb2(1)
        phis2sT = phis2s(1)
       goto 1011
C   Function Form for PGV Regression
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
         c28T = c28(2)
         c29T = c29(2)
         c30T = c30(2)
         taucr1T = taucr1(2)
         taucr2T = taucr2(2)
         tausb1T = tausb1(2)
         tausb2T = tausb2(2)
         phisscr1T = phisscr1(2)
         phisscr2T = phisscr2(2)
         phisssb1T = phisssb1(2)
         phisssb2T = phisssb2(2)
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
      write (*,*) 'Chao et al. (2018) Horizontal atttenuation model'
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
         call S24_interp (period(count1),period(count2),phis2s(count1),phis2s(count2),
     +                specT,phis2sT,iflag)
         call S24_interp (period(count1),period(count2),c28(count1),c28(count2),
     +                 specT,c28T,iflag)
         call S24_interp (period(count1),period(count2),c29(count1),c29(count2),
     +                 specT,c29T,iflag)
         call S24_interp (period(count1),period(count2),c30(count1),c30(count2),
     +                 specT,c30T,iflag)


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
      C23flag = 0
      C29flag = 0
	  C13flag = 0
	  C10flag = 0

      if (sourcetype .eq. 0.0 ) then
       Fcr = 1
       Zref = 0
         if(ftype .gt. 0) then
              Fcrro = 1
           elseif(ftype .lt. 0) then
              Fcrno = 1
           else
              Fcrss = 1
         endif
      elseif (sourcetype .eq. 1.0 ) then
        Fsb = 1
         if(ftype .eq. 0) then
              Fsbinter = 1
              Zref = 0
			  elseif(ftype .eq. 1) then
              Fsbintra = 1
              Zref = 35
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
     &         c6T*Fas + c7T*Fmanila + c26T*Fkuo17 + c27T*Fks17 + c28T*Frf

C     Set Source scaling term

      if(mag .LE. 5 ) then
	      C11flag=1
      endif
      if(mag .GE. Mc ) then
	      C29flag=1
      endif
      if(mag .GE. 7.6 ) then
	      C10flag=1
      endif
      if(mag .LE. 6 ) then
	      C13flag=1
      endif
      if (sourcetype .eq. 0.0 ) then
        Smag = c8T*(mag - Mref) + c10T*(mag - Mref)**2
     1         - c10T*(mag-7.6)**2*C10flag + c11T*(5.0-mag)*C11flag
      elseif (sourcetype .eq. 1.0 ) then
        Smag = c9T*(mag - Mref) + c29T*Fsbinter*(Mag-Mc)*c29flag + c30T*Fsbintra*(Mag-Mc)*c29flag
     1        + c12T*(5.0-mag)*C11flag + c13T*(6.0-mag)*C13flag
      endif

      Sztor = c14T * Fcr *(Ztor-Zref) + c15T * Fsbinter * (Ztor-Zref) + c16T * Fsbintra * (Ztor-Zref)
      Ssource = Smag + Sztor

C     Set Path scaling term

      if (sourcetype .eq. 0.0 ) then
          Sgeom = (c17T + c19T*(min(mag,Mmax)- Mref )) * alog(SQRT(dist**2 + h**2)/SQRT(Rrupref**2 + h**2))
      elseif (sourcetype .eq. 1.0 ) then
          Sgeom = (c18T + c20T*(min(mag, Mc )- Mref )) * alog(SQRT(dist**2 + h**2)/SQRT(Rrupref**2 + h**2))
      endif

      Sanel = c21T*Fcr*(dist-Rrupref) + c22T*Fsb*(dist-Rrupref)
      Spath = Sgeom + Sanel

C     Set Site scaling term

      Z10ref = exp((-4.08/2.0)*alog((vs**2.0+355.4**2.0)/(1750**2.0+355.4**2.0)))
      Ssitelin = c24T * alog(vs/vs30ref) + c25T*alog(Z10*1000/Z10ref)

      if(vs .LT. vs30ref ) then
           C23flag=1
      endif

      Ssitenon = c23T * C23flag * (-1.5*alog(vs/vs30ref)-alog(SA1180+2.4)+alog(SA1180+2.4*(vs/vs30ref)**1.5))
      Ssite = Ssitenon + Ssitelin

      lnSa =  lnYref + Ssource + Spath + Ssite

c  	  write(*,*) "lnYref = ", lnYref
c  	  write(*,*) "Ssource = ", Ssource
c  	  write(*,*) "--Smag = ", Smag
c  	  write(*,*) "--Sztor = ", Sztor
c  	  write(*,*) "Spath = ", Spath
c  	  write(*,*) "--Sgeom = ", Sgeom
c  	  write(*,*) "--Sanel = ", Sanel
c  	  write(*,*) "Ssite = ", Ssite
c  	  write(*,*) "--Ssitelin = ", Ssitelin
c  	  write(*,*) "--Ssitenon = ", Ssitenon
c  	  write(*,*) "lnSa = ", lnSa
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
