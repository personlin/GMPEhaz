c ------------------------------------------------------------------            
C *** Chao2018 (Crustal and Subduction - Model) Horizontal ***********
c ------------------------------------------------------------------            
      subroutine S04_Chao2018 ( mag, dist, ftype, lnY, sigma, specT, vs, Ztor, Z10,           
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
      
         call S04_Chaoetal2018 ( mag, dist, ftype, sigma, specT, vs30_rock, Ztor, z10_rock,
     1             SA_rock, vs30_class, attenName, iflag, sourcetype, phi, tau, lnSa, msasflag)
      Sa1100 = exp(lnSa)

c     Compute Sa at spectral period for given Vs30

         call S04_Chaoetal2018 ( mag, dist, ftype, sigma, specT, vs, Ztor, Z10,
     1             sa1100, vs30_class, attenName, iflag, sourcetype, phi, tau, lnSa, msasflag )

C     Convert ground motion to units of gals.
      lnY = lnSa + 6.89

      period2 = specT

      return
      end
c -------------------------------------------------------------------           
C **** Chao et al. 2018 (SSHAC model) *************
c -------------------------------------------------------------------           

      subroutine S04_Chaoetal2018 ( mag, dist, ftype, sigma, specT, vs, Ztor, Z10,           
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
      integer nper, count1, count2, C11flag, C22flag, C28flag, iflag, C10flag, C13flag
      integer vs30_class, h, n, i, msasflag
      integer Fcr, Fsb, Fcrss, Fcrno, Fcrro, Fsbintra, Fsbinter, Fas, Fkuo17, Fks17, Frf, Fmanila 
      real Mc, Mref, Mmax, Rrupref, Vs30ref, Zref, sourcetype
      real c1T, c2T, c3T, c4T, c5T, c6T, c7T, c8T, c9T, c10T, c11T, c12T, c13T, c14T, c15T
      real c16T, c17T, c18T, c19T, c20T, c21T, c22T, c23T, c24T, c25T, c26T, c27T
      real taucr1T, taucr2T, tausb1T, tausb2T, phisscr1T, phisscr2T, phisssb1T, phisssb2T
      real arfacrT, arfasbT, phis2sT, phi, tau, fm, SA1100, Z10ref
      real Ssource, Spath, Ssite, Ssitelin, Ssitenon, Sztor, Smag, Sgeom, Sanel
      real taucr, tausb, phisscr, phisssb, phiss, sigmass
      real c28(MAXPER), c29(MAXPER), c28T, c29T
                                                                                
      data period / 0.0, -2.0, 0.01, 0.02, 0.03, 0.05, 0.075, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 
     1              0.75, 1, 1.5, 2, 3, 4, 5 /
      data c1 / -1.063873, 1.890057, -1.032027, -0.999539, -0.959721, -0.873503, -0.782028, -0.722824, 
     1          -0.666711, -0.660257, -0.674395, -0.699592, -0.776127, -0.862269, -1.097515, -1.332641,  
     1          -1.767176, -2.156244, -2.820278, -3.354932, -3.866800 /
      data c2 / -1.159809, 1.729883, -1.127783, -1.091575, -1.046292, -0.942672, -0.828814, -0.754079,  
     1          -0.698223, -0.709166, -0.745596, -0.793489, -0.903466, -1.012323, -1.275899, -1.513152,  
     1          -1.923858, -2.278836, -2.875747, -3.355274, -3.810677 /
      data c3 / -1.193819, 1.580703, -1.161918, -1.121271, -1.064361, -0.934868, -0.799221, -0.718831,  
     1          -0.684161, -0.734780, -0.813494, -0.902814, -1.070906, -1.213231, -1.506057, -1.744582,  
     1          -2.149667, -2.497003, -3.088823, -3.564064, -4.047829 /
      data c4 / -1.091960, 2.097085, -1.166990, -1.133049, -1.085487, -0.972939, -0.849815, -0.768441,  
     1          -0.667181, -0.588699, -0.544245, -0.518453, -0.494246, -0.510541, -0.675065, -0.889892,  
     1          -1.355765, -1.816897, -2.604120, -3.266411, -3.797861 /
      data c5 / -0.627987, 2.261213, -0.719435, -0.675669, -0.617019, -0.467185, -0.281451, -0.156542,  
     1          -0.043814, -0.034999, -0.076869, -0.127249, -0.231856, -0.336747, -0.630666, -0.927191,  
     1          -1.475255, -1.965935, -2.729734, -3.350495, -3.782579 /
      data c6 / -0.131302, -0.152703, -0.128463, -0.130283, -0.132894, -0.131859, -0.129702, -0.126524,  
     1          -0.121203, -0.112828, -0.103516, -0.096162, -0.089601, -0.091774, -0.112807, -0.133256,  
     1          -0.144519, -0.130577, -0.088271, -0.049019, 0.011743 /
      data c7 / 0.174642, -0.046534, 0.187114, 0.190876, 0.197276, 0.224294, 0.248453, 0.276130, 0.336596,  
     1          0.362035, 0.356666, 0.337687, 0.258470, 0.169343, 0.024503, -0.056754, -0.118315, -0.143678,  
     1          -0.196052, -0.257316, -0.326739 /
      data c8 / 0.686768, 1.253935, 0.684379, 0.653454, 0.606918, 0.542852, 0.548717, 0.603117, 0.740410,  
     1          0.876832, 0.987375, 1.080380, 1.235317, 1.356003, 1.566143, 1.701845, 1.863449, 1.953076,  
     1          2.043388, 2.088723, 2.105348 /
      data c9 / 0.660999, 0.789442, 0.636607, 0.602677, 0.571145, 0.550536, 0.593262, 0.672769, 0.816315,  
     1          0.889989, 0.946047, 0.973011, 0.997954, 1.017956, 1.008750, 0.995039, 0.952070, 0.969585,  
     1          1.046545, 1.158151, 1.146724 /
      data c10 / -0.137353, -0.209260, -0.136876, -0.130691, -0.121383, -0.108569, -0.109737, -0.120617,  
     1          -0.148076, -0.175365, -0.197475, -0.216076, -0.247063, -0.271200, -0.313228, -0.339214,  
     1          -0.357208, -0.352247, -0.323769, -0.294601, -0.250758 /
      data c11 / -0.000009, -0.000002, 0.000000, -0.004842, -0.006299, -0.004464, -0.001457, -0.009868,  
     1          -0.079508, -0.163621, -0.238571, -0.296908, -0.344839, -0.333262, -0.228238, -0.131398,  
     1          -0.032686, -0.006878, -0.000001, 0.000000, 0.000000 /
      data c12 / -0.000009, -0.000013, 0.000000, -0.007260, -0.004838, -0.000099, -0.000004, -0.010127,  
     1          -0.115386, -0.208607, -0.263688, -0.298481, -0.294126, -0.234514, -0.127711, -0.067084,  
     1          -0.018875, -0.004683, -0.000001, 0.000000, 0.000000 /
      data c13 / -0.000010, -0.301696, 0.000000, -0.000003, -0.000004, -0.000003, -0.000005, -0.000005,  
     1          -0.011147, -0.069179, -0.136519, -0.212723, -0.352450, -0.468626, -0.667073, -0.779678,  
     1          -0.870007, -0.813223, -0.648139, -0.461443, -0.466237 /
      data c14 / 0.032480, 0.015465, 0.032456, 0.033064, 0.034309, 0.037356, 0.039956, 0.040651, 0.038518,  
     1          0.034405, 0.030182, 0.026338, 0.020484, 0.016401, 0.010730, 0.008111, 0.005576, 0.003996,  
     1          0.000606, -0.002967, -0.008951 /
      data c15 / 0.006934, 0.003250, 0.006897, 0.007252, 0.007812, 0.008902, 0.009547, 0.009544, 0.008776,  
     1          0.007727, 0.006654, 0.005645, 0.004067, 0.002889, 0.001476, 0.001154, 0.000921, 0.000776,  
     1          0.000297, 0.000037, -0.000282 /
      data c16 / -1.690150, -1.546134, -1.691824, -1.722377, -1.760016, -1.812219, -1.812053, -1.761789,  
     1          -1.645065, -1.553064, -1.490134, -1.448246, -1.399898, -1.376401, -1.347609, -1.333705,  
     1          -1.311748, -1.298672, -1.284841, -1.281901, -1.241532 /
      data c17 / -1.593352, -1.565148, -1.515747, -1.532063, -1.566642, -1.636108, -1.676784, -1.670079,  
     1          -1.629673, -1.593807, -1.560351, -1.534142, -1.506724, -1.486158, -1.435211, -1.400859,  
     1          -1.332916, -1.279627, -1.210365, -1.152781, -1.113843 /
      data c18 / 0.388457, 0.302260, 0.387937, 0.399220, 0.413884, 0.423066, 0.405953, 0.381673, 0.338492,  
     1          0.299053, 0.268629, 0.244343, 0.210033, 0.191836, 0.179453, 0.184098, 0.197114, 0.210330,  
     1          0.233083, 0.252812, 0.264010 /
      data c19 / 0.185052, 0.211496, 0.190488, 0.205429, 0.212151, 0.195295, 0.157068, 0.119393, 0.076495,  
     1          0.060016, 0.047774, 0.043782, 0.044662, 0.046744, 0.072253, 0.099591, 0.153969, 0.199942,  
     1          0.253752, 0.282276, 0.298510 /
      data c20 / -0.003433, -0.000844, -0.003329, -0.003282, -0.003364, -0.003870, -0.004701, -0.005480,  
     1          -0.006059, -0.005720, -0.005083, -0.004372, -0.003173, -0.002338, -0.001303, -0.000879,  
     1          -0.000565, -0.000476, -0.000459, -0.000460, -0.000879 /
      data c21 / -0.003523, -0.000859, -0.003990, -0.004254, -0.004352, -0.004385, -0.004427, -0.004517,  
     1          -0.004358, -0.003812, -0.003218, -0.002656, -0.001712, -0.001104, -0.000543, -0.000364,  
     1          -0.000465, -0.000786, -0.001435, -0.002141, -0.002623 /
      data c22 / -2.457502, -7.415494, -2.264552, -2.349315, -2.269892, -2.041187, -1.822813, 
     1           -1.665704, -1.454891, -1.338241, -1.292520, -1.296819, -1.360247, -1.442348,  
     1           -1.497648, -1.344100, -0.805913, -0.417734, -0.022573, 0.000000, 0.000000 /
      data c23 / -0.481535, -0.673712, -0.476589, -0.471594, -0.453368, -0.417310, -0.403252,  
     1           -0.411097, -0.447092, -0.481728, -0.514494, -0.543515, -0.597364, -0.649164,  
     1           -0.742384, -0.798690, -0.842405, -0.849778, -0.839147, -0.822511, -0.797141 /
      data c24 / 0.063388, 0.095822, 0.063517, 0.064665, 0.068599, 0.079429, 0.084655, 0.082030,  
     1           0.070607, 0.062531, 0.061479, 0.064358, 0.074017, 0.083573, 0.103395, 0.118168,  
     1           0.139923, 0.153222, 0.160180, 0.155343, 0.142666 /
      data c25 / -0.573411, 0.391052, -0.607583, -0.564578, -0.494276, -0.342513, -0.229341, -0.198317,  
     1           -0.248248, -0.347109, -0.452365, -0.552524, -0.726984, -0.874684, -1.154811, -1.353207,  
     1           -1.618540, -1.771160, -1.893011, -1.912582, -1.838861 /
      data c26 / -0.648986, 0.390264, -0.682472, -0.643040, -0.581023, -0.441588, -0.333935, -0.304181,  
     1           -0.355435, -0.444836, -0.534696, -0.618165, -0.761245, -0.887009, -1.140628, -1.331402,  
     1           -1.584333, -1.727701, -1.829261, -1.833069, -1.746378 /
      data c27 / -0.620260, 0.312548, -0.653824, -0.612833, -0.542819, -0.381413, -0.259445, -0.228687,  
     1           -0.295257, -0.416850, -0.535713, -0.641918, -0.818446, -0.960952, -1.232650, -1.435938,  
     1           -1.715489, -1.875761, -1.998776, -2.017655, -1.941254  /
      data c28 / -0.107999, 0.000000, -0.083607, -0.049677, -0.018145, 0.002464, -0.040262, -0.119769,  
     1          -0.263315, -0.336989, -0.393047, -0.420011, -0.444954, -0.464956, -0.438417, -0.373039, 
     1           -0.247070, -0.201585, -0.183545, -0.223151, -0.152724 /
      data c29 / -0.234539, 0.000000, -0.215857, -0.202127, -0.206815, -0.250536, -0.258182, -0.302769,  
     1          -0.366315, -0.380989, -0.391047, -0.380011, -0.345954, -0.322956, -0.241383, -0.183039,  
     1          -0.091070, -0.085585, -0.146545, -0.258151, -0.246724 /
      data taucr1 / 0.367396, 0.439820, 0.366108, 0.367096, 0.365282, 0.360221, 0.364017, 0.376975, 0.420147,  
     1          0.470353, 0.513741, 0.545322, 0.580510, 0.594865, 0.586090, 0.567212, 0.539109, 0.521000,  
     1          0.510986, 0.516262, 0.543005 /
      data taucr2 / 0.316303, 0.374971, 0.315547, 0.320586, 0.327864, 0.345979, 0.360502, 0.362567, 0.339910,  
     1          0.308615, 0.286310, 0.274373, 0.271879, 0.287436, 0.346082, 0.391733, 0.441799, 0.462508,  
     1          0.475147, 0.480567, 0.470058 /
      data tausb1 / 0.266511, 0.331406, 0.262302, 0.266322, 0.266483, 0.269800, 0.280166, 0.292396, 0.322855,  
     1          0.360816, 0.395487, 0.425484, 0.466244, 0.484584, 0.481419, 0.455249, 0.410995, 0.370707,  
     1          0.327665, 0.293814, 0.341356 /
      data tausb2 / 0.567425, 0.576381, 0.559564, 0.582817, 0.602962, 0.628642, 0.630357, 0.613456, 0.561548,  
     1          0.525565, 0.502178, 0.486796, 0.483329, 0.494099, 0.529962, 0.575901, 0.636351, 0.664233,  
     1          0.647100, 0.597897, 0.507072 /
      data phisscr1 / 0.528236, 0.558211, 0.531654, 0.524477, 0.515598, 0.502965, 0.503350, 0.517268, 0.553081,  
     1          0.581613, 0.599279, 0.608878, 0.610790, 0.600235, 0.561826, 0.526083, 0.477124, 0.448855,  
     1          0.421072, 0.408714, 0.406221 /
      data phisscr2 / 0.439658, 0.435662, 0.432542, 0.443143, 0.454835, 0.470056, 0.466796, 0.451798, 0.427508,  
     1          0.417303, 0.416462, 0.422003, 0.436131, 0.448634, 0.472005, 0.484941, 0.492702, 0.488210,  
     1          0.469355, 0.447792, 0.429747 /
      data phisssb1 / 0.435422, 0.478176, 0.433652, 0.425997, 0.422332, 0.410917, 0.408377, 0.420670, 0.455346,  
     1          0.478162, 0.489836, 0.495227, 0.494609, 0.491417, 0.486800, 0.481847, 0.481326, 0.479433,  
     1          0.475433, 0.466465, 0.454073 /
      data phisssb2 / 0.496775, 0.485965, 0.494056, 0.497591, 0.505074, 0.519146, 0.524787, 0.520610, 0.511244,  
     1          0.503692, 0.496706, 0.491103, 0.487451, 0.486700, 0.488908, 0.493516, 0.488113, 0.477714,  
     1          0.446953, 0.414410, 0.368507 /
      data phis2s / 0.343436, 0.274077, 0.342257, 0.349033, 0.364025, 0.407706, 0.443764, 0.455801, 0.439198,  
     1          0.411793, 0.389327, 0.372079, 0.352731, 0.344512, 0.341565, 0.347036, 0.357102, 0.363359,  
     1          0.369654, 0.372377, 0.375576 /

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
      C22flag = 0
      C28flag = 0
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
          Zref = 0
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
     &         c6T*Fas + c7T*Fmanila + c25T*Fkuo17 + c26T*Fks17 + c27T*Frf

C     Set Source scaling term 
     
      if(mag .LE. 5 ) then  
	      C11flag=1
      endif
      if(mag .GE. Mc ) then  
	      C28flag=1
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
        Smag = c9T*(mag - Mref) + c28T*Fsbinter*(Mag-Mc)*c28flag + c29T*Fsbintra*(Mag-Mc)*c28flag 
     1        + c12T*(5.0-mag)*C11flag + c13T*(6.0-mag)*C13flag
      endif

      Sztor = c14T * Fcr *(Ztor-Zref) + c15T * Fsb * ((Ztor-Zref))	  
      Ssource = Smag + Sztor

C     Set Path scaling term

      if (sourcetype .eq. 0.0 ) then
          Sgeom = (c16T + c18T*(min(mag,Mmax)- Mref )) * alog(SQRT(dist**2 + h**2)/SQRT(Rrupref**2 + h**2))
      elseif (sourcetype .eq. 1.0 ) then
          Sgeom = (c17T + c19T*(min(mag,Mc)- Mref )) * alog(SQRT(dist**2 + h**2)/SQRT(Rrupref**2 + h**2))
      endif

      Sanel = c20T*Fcr*(dist-Rrupref) + c21T*Fsb*(dist-Rrupref)
      Spath = Sgeom + Sanel 
	   
C     Set Site scaling term 
	   
      Z10ref = exp((-4.08/2.0)*alog((vs**2.0+355.4**2.0)/(1750**2.0+355.4**2.0)))
      Ssitelin = c23T * alog(vs/vs30ref) + c24T*alog(Z10*1000/Z10ref)

      if(vs .LT. vs30ref ) then  
           C22flag=1
      endif
	    
      Ssitenon = c22T * C22flag * (-1.5*alog(vs/vs30ref)-alog(SA1100+2.4)+alog(SA1100+2.4*(vs/vs30ref)**1.5))  
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