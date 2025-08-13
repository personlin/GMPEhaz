
c-------------------- Adjusted in Taiwan SSHAC Project--------------------------------------------------

      Subroutine Bindi14_TW_B01 ( m, jbDist, ftype, specT,
     1                     period2, lnY, sigma, iflag, vs, phiT, tauT )

      implicit none

      integer MAXPER
      parameter (MAXPER=21)
      REAL Period(MAXPER), e1(MAXPER), c1(MAXPER), c2(MAXPER), h(MAXPER)
      REAL c3(MAXPER), b1(MAXPER), b2(MAXPER), b3(MAXPER), gamma(MAXPER)
      REAL sofN(MAXPER), sofR(MAXPER), sofS(MAXPER), phi(MAXPER), tau(MAXPER), sig(MAXPER), sigs2s(MAXPER)
      real e1T, c1T, c2T, hT, c3T, b1T, b2T, b3T, gammaT, sofNT, sofRT, sofST, sigs2sT
      real phiT, tauT, sigT, period1
      real Rref, Mref, Mh, R, Vref, vs

      REAL M, jbDist, specT, sigma, termsof
      REAL period2, lnY, ftype

      integer iflag, count1, count2, nPer, i

      Data period / 0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.075, 0.1, 0.12, 0.15, 0.17, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 1, 1.5, 2, 3/
      Data e1 / 0.522609472577037, 0.522609472577037, 0.546968012939885, 0.660249767305288, 0.786638259667348, 0.91939728089648,
     1      1.23969642365034, 1.42459592535047, 1.39371308406266, 1.32850226054803, 1.31691711665522, 1.28941414402491,
     1      1.19597507196761, 1.20976978074338, 0.961156476633024, 0.631499830418335, 0.208503359652467, 0.106966019843071,
     1      -0.562248475531381, -1.1464952933501, -2.06760206844386/
      Data c1 / -1.26358, -1.26358, -1.26358, -1.29088019990866, -1.31025, -1.30237878943392, -1.28882132582112, -1.28178,
     1      -1.23465110534188, -1.17697, -1.14479188763455, -1.10301, -1.08792194443592, -1.10591, -1.09538, -1.05767,
     1      -1.04831378793046, -1.0527, -0.983388, -0.979215, -0.940373/
      Data c2 / 0.220527, 0.220527, 0.220527, 0.234653259429915, 0.244676, 0.239572871690745, 0.229465497782482, 0.219406,
     1      0.202883682422306, 0.182662, 0.161122330083983, 0.133154, 0.118226690912901, 0.108276, 0.101111, 0.112197,
     1      0.122345650356548, 0.103471, 0.109072, 0.163344, 0.227241/
      Data h / 5.20082, 5.20082, 5.20082, 5.0346146046701, 4.91669, 5.09314627212803, 5.50666272693079, 6.12146, 5.95062506384396,
     1      5.74154, 5.55812998041133, 5.31998, 5.16226984536375, 5.12846, 4.95386, 4.43205, 4.18619685740845, 4.41613, 4.56697,
     1      4.58186, 5.74173/
      Data c3 / -0.000605996446830423, -0.000605996446830423, -0.000672239466779722, -0.000799403647073816, -0.00108688575693314,
     1      -0.00182293794142616, -0.00307043122458409, -0.00338701081586182, -0.00371284169158365, -0.00379904483423038,
     1      -0.00356854847646677, -0.00317835883930091, -0.00213262540362793, -0.00105504850441417, 0.000145927558546901,
     1      0.000360538127080148, 0.000890616440131034, 0.000680353189919601, -0.000503745709108027, -0.00138021625437496,
     1      -0.00180424057762217/
      Data b1 / -0.314150943189205, -0.314150943189205, -0.324731817891737, -0.336956128683367, -0.358090259199968,
     1      -0.326358081535885, -0.271767316635362, -0.322757812539039, -0.324673427726984, -0.295257429962149, -0.2049368721718,
     1      -0.0801062684058633, -0.0682471416626888, -0.0249543310299552, -0.0141511999188923, 0.0440666800208901,
     1      0.276717801507427, 0.612245140271721, 0.847614057511416, 0.763946194795024, 0.879070265633793/
      Data b2 / -0.210951793811668, -0.210951793811668, -0.210951793811668, -0.193123478069862, -0.180474086745271,
     1      -0.160842272902352, -0.139406648181843, -0.173459491517974, -0.201518836294038, -0.235860698830659, -0.238616639987212,
     1      -0.242195110421486, -0.285257767878105, -0.319087637016929, -0.373226017723405, -0.377872634441067, -0.365550258824207,
     1      -0.339764851152015, -0.322055669201798, -0.341431922759343, -0.256611595688721/
      Data b3 / 0, 0, 0, 0, 0, 0, 0, 0, 0.0765190099149165, 0.170170709229651, 0.246865015276161, 0.346449255676977,
     1      0.402141458721742, 0.436758945609297, 0.517683298702572, 0.356325043140829, 0.186190244946956, 0.213765783053816,
     1      0.227350804843972, 0, 0/
      Data gamma / -0.408683671798333, -0.408683671798333, -0.400075407545783, -0.384133598867579, -0.359561369699193,
     1      -0.329524347273065, -0.299287650270295, -0.309519653250575, -0.332892458195161, -0.359403115676011, -0.378411611000445,
     1      -0.389746326222917, -0.411395776810339, -0.467572809984905, -0.546908450504775, -0.595238480052747, -0.796012403712125,
     1      -0.927665397275468, -1.04191708549487, -1.05704308469178, -1.05298078066284/
      Data sofN / -0.151187655675668, -0.151187655675668, -0.150246503545881, -0.156860045014653, -0.147701069798838,
     1      -0.148790692553373, -0.17054788960877, -0.190889732302509, -0.171908041858924, -0.132030195032508, -0.112054192411864,
     1      -0.105249913849246, -0.0843790608154017, -0.0851331439335536, -0.126937077943447, -0.175060809294038,
     1      -0.20072890543961, -0.234399182396952, -0.200717328818411, -0.197815126876915, -0.0856704470002901/
      Data sofR / 0.0973011850663651, 0.0973011850663651, 0.0972481548396473, 0.0943819459885255, 0.0952748111839279,
     1      0.0913942048061309, 0.0653634635737405, 0.0439485385785688, 0.0427163436709354, 0.0469110298524375, 0.0616606523506074,
     1      0.0893509807637394, 0.114129026552164, 0.113921936616951, 0.13074140129623, 0.136920683198748, 0.154682096092987,
     1      0.167774456230626, 0.147044523531908, 0.118386343901669, 0.0952151759072998/
      Data sofS / -0.0962839772146018, -0.0962839772146018, -0.0962839772146018, -0.0966103373684394, -0.0968418935826343,
     1      -0.0963783234557857, -0.0979532928586526, -0.107435166561463, -0.101695220967487, -0.0946700953229223,
     1      -0.0968825620783734, -0.0997553545007996, -0.0954761142984958, -0.0908505671706638, -0.108074133924769,
     1      -0.0986961653580224, -0.0876704884819493, -0.103411629369965, -0.0961628612387103, -0.0584672406813048,
     1      -0.00535846089916109/
      Data tau / 0.315339425968275, 0.315339425968275, 0.315102053258158, 0.32716028400231, 0.340334974277716, 0.352676994184937,
     1      0.369836140664405, 0.361434400022514, 0.346264556496815, 0.332518576045098, 0.325171343044787, 0.307674047555169,
     1      0.294572918179562, 0.296732943084942, 0.334489161101022, 0.367129597996158, 0.398725888338442, 0.413919596322535,
     1      0.422604814366727, 0.442631535010139, 0.463648257061165/
      Data phi / 0.650148716517427, 0.650148716517427, 0.650148716517427, 0.661984154700548, 0.670381531729565, 0.670708390444475,
     1      0.675863668035947, 0.695304712776133, 0.699277469954257, 0.704139731777951, 0.698434502811551, 0.69102650967335,
     1      0.695550350626772, 0.684793411826615, 0.683849351938488, 0.667650665809275, 0.665568654353029, 0.685956217298577,
     1      0.728335295935133, 0.759659663540224, 0.723391645740475/
      Data sigs2s / 0.423581251122092, 0.423581251122092, 0.423581251122092, 0.428228145594809, 0.431525169692921,
     1      0.443005622982909, 0.463997900072269, 0.479347559489315, 0.483433173204425, 0.488433560266269, 0.466739665048115,
     1      0.438571080077483, 0.431849944946685, 0.400921511221937, 0.391170063353107, 0.380133773002387, 0.387723195864864,
     1      0.46230152204588, 0.503273721190616, 0.518887550706208, 0.477203852767737/
      Data sig / 0.569082801393606, 0.569082801393606, 0.56852685416841, 0.579390485069763, 0.596464784191982, 0.613072723757498,
     1      0.632302056043141, 0.628816561889463, 0.620217014153363, 0.604016230936898, 0.59517918896342, 0.587232110508394,
     1      0.581813792789531, 0.588215840580272, 0.591206576474996, 0.593256788031551, 0.598173263246328, 0.594956537072661,
     1      0.599781503340705, 0.592548906940324, 0.57931053233881/


C Find the requested spectral period and corresponding coefficients
      nPer = 21
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         e1T      = e1(1)
         c1T      = c1(1)
         c2T      = c2(1)
         hT       = h(1)
         c3T      = c3(1)
         b1T      = b1(1)
         b2T      = b2(1)
         b3T      = b3(1)
         gammaT   = gamma(1)
         sofNT    = sofN(1)
         sofRT    = sofR(1)
         sofST    = sofS(1)
         sigs2sT  = sigs2s(1)
         phiT     = phi(1)
         tauT     = tau(1)
         sigT     = sig(1)
         goto 1011
C     PGV Case
      elseif (specT .eq. -1.0) then
         period1  = period(2)
         e1T      = e1(2)
         c1T      = c1(2)
         c2T      = c2(2)
         hT       = h(2)
         c3T      = c3(2)
         b1T      = b1(2)
         b2T      = b2(2)
         b3T      = b3(2)
         gammaT   = gamma(2)
         sofNT    = sofN(2)
         sofRT    = sofR(2)
         sofST    = sofS(2)
         sigs2sT  = sigs2s(2)
         phiT     = phi(2)
         tauT     = tau(2)
         sigT     = sig(2)
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
 1020 call interp (period(count1),period(count2),e1(count1),e1(count2),
     +             specT,e1T,iflag)
      call interp (period(count1),period(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call interp (period(count1),period(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call interp (period(count1),period(count2),h(count1),h(count2),
     +             specT,hT,iflag)
      call interp (period(count1),period(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call interp (period(count1),period(count2),b1(count1),b1(count2),
     +             specT,b1T,iflag)
      call interp (period(count1),period(count2),b2(count1),b2(count2),
     +             specT,b2T,iflag)
      call interp (period(count1),period(count2),b3(count1),b3(count2),
     +             specT,b3T,iflag)
      call interp (period(count1),period(count2),gamma(count1),gamma(count2),
     +             specT,gammaT,iflag)
      call interp (period(count1),period(count2),sofN(count1),sofN(count2),
     +             specT,sofNT,iflag)
      call interp (period(count1),period(count2),sofR(count1),sofR(count2),
     +             specT,sofRT,iflag)
      call interp (period(count1),period(count2),sofS(count1),sofS(count2),
     +             specT,sofST,iflag)
      call interp (period(count1),period(count2),sigs2s(count1),sigs2s(count2),
     +             specT,sigs2sT,iflag)
      call interp (period(count1),period(count2),phi(count1),phi(count2),
     +             specT,phiT,iflag)
      call interp (period(count1),period(count2),tau(count1),tau(count2),
     +             specT,tauT,iflag)
      call interp (period(count1),period(count2),sig(count1),sig(count2),
     +             specT,sigT,iflag)

 1011 period1 = specT

C     Set Constant Terms
      Mref = 5.5
      Rref = 1.0
      Mh = 6.75
      Vref = 800.0

C     Set the mechanism term.
      if (ftype .eq. 0 ) then
         termsof = 0
      elseif (ftype .ge. 0.5) then
         termsof = sofRT
      elseif (ftype .le. -0.5) then
         termsof = sofNT
      endif

      R = sqrt (jbdist**2 + hT**2)

C     Compute the ground motion for the given spectral period.
      if (M .le. Mh) then
         lnY = e1T + (c1T+c2T*(M-Mref))*alog(R/Rref) + c3T*(R-Rref) +
     1       b1T*(M-Mh) + b2T*(M-Mh)**2.0 + gammaT*alog(vs/vref) + termsof
      else
         lnY = e1T + (c1T+c2T*(M-Mref))*alog(R/Rref) + c3T*(R-Rref) +
     1       b3T*(M-Mh) + gammaT*alog(vs/vref) + termsof
      endif

C     Set the sigma value and convert from log10 to Ln units
c      phiT = phiT*alog(10.0)
c      tauT = tauT*alog(10.0)
c      sigma = sigT*alog(10.0)
c      sigs2sT = sigs2sT*alog(10.0)

C     Convert ground motion to units of gals in natural log units.
c      lnY = lnY*alog(10.0)
      lnY = lnY + 6.89
      period2 = period1

      return
      end


c-------------------- Adjusted in Taiwan SSHAC Project--------------------------------------------------

      Subroutine Bindi14_TW_C01 ( m, jbDist, ftype, specT,
     1                     period2, lnY, sigma, iflag, vs, phiT, tauT )
      implicit none
      integer MAXPER
      parameter (MAXPER=21)
      REAL Period(MAXPER), e1(MAXPER), c1(MAXPER), c2(MAXPER), h(MAXPER)
      REAL c3(MAXPER), b1(MAXPER), b2(MAXPER), b3(MAXPER), gamma(MAXPER)
      REAL sofN(MAXPER), sofR(MAXPER), sofS(MAXPER), phi(MAXPER), tau(MAXPER), sig(MAXPER), sigs2s(MAXPER)
      real e1T, c1T, c2T, hT, c3T, b1T, b2T, b3T, gammaT, sofNT, sofRT, sofST, sigs2sT
      real phiT, tauT, sigT, period1
      real Rref, Mref, Mh, R, Vref, vs

      REAL M, jbDist, specT, sigma, termsof
      REAL period2, lnY, ftype
      integer iflag, count1, count2, nPer, i
      real f_D, f_M, f_S


      Data period(1:21) / 0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.075, 0.1, 0.12, 0.15, 0.17, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 1, 1.5, 2,
     1      3/
      Data e1(1:21) / 0.659508496312107, 0.659508496312107, 0.684257395856917, 0.792185875253012, 0.910245329949904,
     1      1.04124346176327, 1.36695980240164, 1.58175260431507, 1.55527075935291, 1.48876800771044, 1.47418275968898,
     1      1.43106965774598, 1.32383819300195, 1.3382650551676, 1.08674468845801, 0.744502966314904, 0.339631620365221,
     1      0.267909172341515, -0.363413009167707, -0.8677036818281, -1.77042537043066/
      Data c1(1:21) / -1.26358, -1.26358, -1.26358, -1.29088019990866, -1.31025, -1.30237878943392, -1.28882132582112, -1.28178,
     1      -1.23465110534188, -1.17697, -1.14479188763455, -1.10301, -1.08792194443592, -1.10591, -1.09538, -1.05767,
     1      -1.04831378793046, -1.0527, -0.983388, -0.979215, -0.940373/
      Data c2(1:21) / 0.220527, 0.220527, 0.220527, 0.234653259429915, 0.244676, 0.239572871690745, 0.229465497782482, 0.219406,
     1      0.202883682422306, 0.182662, 0.161122330083983, 0.133154, 0.118226690912901, 0.108276, 0.101111, 0.112197,
     1      0.122345650356548, 0.103471, 0.109072, 0.163344, 0.227241/
      Data h(1:21) / 5.20082, 5.20082, 5.20082, 5.0346146046701, 4.91669, 5.09314627212803, 5.50666272693079, 6.12146,
     1      5.95062506384396, 5.74154, 5.55812998041133, 5.31998, 5.16226984536375, 5.12846, 4.95386, 4.43205, 4.18619685740845,
     1      4.41613, 4.56697, 4.58186, 5.74173/
      Data c3(1:21) / -0.00162234204269646, -0.00162234204269646, -0.00169729704805731, -0.00182364633843366, -0.00211014463275481,
     1      -0.00287513335217997, -0.004184784769582, -0.00466828237187935, -0.00500654836196918, -0.00499803696576266,
     1      -0.00473066619716198, -0.00423022629441665, -0.00300999356000803, -0.00189118393920752, -0.000589441966065091,
     1      -0.000241978980141242, 0.000285085906072067, -1.74038270368973e-05, -0.00134660884745955, -0.00283883648666463,
     1      -0.00339965384388641/
      Data b1(1:21) / -0.270897727235864, -0.270897727235864, -0.281154290911621, -0.291943091967611, -0.312942644598605,
     1      -0.279796632295181, -0.224987839767471, -0.275839067753525, -0.276546876529609, -0.247413951462292, -0.15365006490395,
     1      -0.0299424838422354, -0.0227660513080655, 0.0145880936350126, 0.0160023913376982, 0.0708600769194794,
     1      0.304811546059666, 0.646443935653183, 0.889724237533995, 0.819623500158111, 0.957007301722072/
      Data b2(1:21) / -0.210951793811668, -0.210951793811668, -0.210951793811668, -0.193123478069862, -0.180474086745271,
     1      -0.160842272902352, -0.139406648181843, -0.173459491517974, -0.201518836294038, -0.235860698830659, -0.238616639987212,
     1      -0.242195110421486, -0.285257767878105, -0.319087637016929, -0.373226017723405, -0.377872634441067, -0.365550258824207,
     1      -0.339764851152015, -0.322055669201798, -0.341431922759343, -0.256611595688721/
      Data b3(1:21) / 0, 0, 0, 0, 0, 0, 0, 0, 0.0765190099149165, 0.170170709229651, 0.246865015276161, 0.346449255676977,
     1      0.402141458721742, 0.436758945609297, 0.517683298702572, 0.356325043140829, 0.186190244946956, 0.213765783053816,
     1      0.227350804843972, 0, 0/
      Data gamma(1:21) / -0.422676821032021, -0.422676821032021, -0.414978878595817, -0.406699004557271, -0.3914970371187,
     1      -0.369326047622375, -0.343869768741454, -0.333289584723655, -0.352050474022221, -0.367605646663696, -0.387393323332123,
     1      -0.396818066941227, -0.39952338981378, -0.445165123281344, -0.512409231991832, -0.561508199422803, -0.752231535522345,
     1      -0.873980613150435, -0.968710126360196, -0.981012736821664, -0.992114119974754/
      Data sofN(1:21) / -0.117251075170762, -0.117251075170762, -0.115338543884633, -0.119347338965019, -0.108122399642431,
     1      -0.110836575215843, -0.127904216218309, -0.151584972241392, -0.131927184713145, -0.091333399271005,
     1      -0.0734541746220918, -0.0720743866300066, -0.053504945030001, -0.0577148145517801, -0.0967772518776173,
     1      -0.153197353204361, -0.176638749464974, -0.202427135498213, -0.184012887842084, -0.180956841720691,
     1      -0.0658311164233819/
      Data sofR(1:21) / 0.0950331165879268, 0.0950331165879268, 0.0942147492408604, 0.0886194576576785, 0.0865931352500485,
     1      0.0798032805953013, 0.0518078943711761, 0.0377736339116048, 0.0374540870711511, 0.0461070357350431, 0.0650643320300671,
     1      0.0987719958171821, 0.125118256865787, 0.123740513623839, 0.137614216748372, 0.143277552748404, 0.160418527309156,
     1      0.172380927444367, 0.156550741034289, 0.135252483671118, 0.10353955455201/
      Data sofS(1:21) / -0.096283977, -0.096283977, -0.096283977, -0.0966103375235148, -0.096841894, -0.0963783235595202,
     1      -0.0979532926457108, -0.107435167, -0.101695221063626, -0.094670095, -0.0968825621131357, -0.099755355,
     1      -0.0954761145312324, -0.090850567, -0.108074134, -0.098696165, -0.0876704883298241, -0.103411629, -0.096162861,
     1      -0.058467241, -0.005358461/
      Data tau(1:21) / 0.317898754427414, 0.317898754427414, 0.319323264507343, 0.332912367257089, 0.345138786542856,
     1      0.356539794838403, 0.373304099976226, 0.365831192336457, 0.352942682997377, 0.334646212108993, 0.328499912385758,
     1      0.313551883565158, 0.297330303430244, 0.299006855270763, 0.33361918283012, 0.364066847460064, 0.40039358200669,
     1      0.420084326301466, 0.436932806015766, 0.45401623183321, 0.490721750019259/
      Data phi(1:21) / 0.32571333838296, 0.32571333838296, 0.325174907932113, 0.334158210279457, 0.35048253853852,
     1      0.370382687881984, 0.409261273800818, 0.41946450409783, 0.415563072394708, 0.393759932519684, 0.382737515801976,
     1      0.363483599894857, 0.341015171374459, 0.335643324687049, 0.327306984921592, 0.31669506610895, 0.328111960636172,
     1      0.334986782672972, 0.363636845023503, 0.388384142446741, 0.404771608028183/
      Data sigs2s(1:21) / 0.474767396333149, 0.474767396333149, 0.474134438654724, 0.480278729542088, 0.486514622964184,
     1      0.488556279560537, 0.487077378450021, 0.481594309301403, 0.477211581548162, 0.47594992226976, 0.472293215881846,
     1      0.475976807032338, 0.48288546933477, 0.488981047072412, 0.499582923250037, 0.506548849370516, 0.50222265741314,
     1      0.491260159723756, 0.472398005717859, 0.454546252692053, 0.434481072336867/
      Data sig(1:21) / 0.745772773, 0.745772773, 0.745772773, 0.753543188479092, 0.759056386, 0.767508805717544, 0.783602925138285,
     1      0.797567122, 0.792929650007148, 0.787253843, 0.780874405181787, 0.772590981, 0.777647499576309, 0.775374807,
     1      0.77430871, 0.78683247, 0.795506751916842, 0.819874566, 0.835458462, 0.84932463, 0.829789498/

C Find the requested spectral period and corresponding coefficients
      nPer = 21
C First check for the PGA case (i.e., specT=0.0)
      if (specT .eq. 0.0) then
         period1  = period(1)
         e1T      = e1(1)
         c1T      = c1(1)
         c2T      = c2(1)
         hT       = h(1)
         c3T      = c3(1)
         b1T      = b1(1)
         b2T      = b2(1)
         b3T      = b3(1)
         gammaT   = gamma(1)
         sofNT    = sofN(1)
         sofRT    = sofR(1)
         sofST    = sofS(1)
         sigs2sT  = sigs2s(1)
         phiT     = phi(1)
         tauT     = tau(1)
         sigT     = sig(1)
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
 1020 call interp (period(count1),period(count2),e1(count1),e1(count2),
     +             specT,e1T,iflag)
      call interp (period(count1),period(count2),c1(count1),c1(count2),
     +             specT,c1T,iflag)
      call interp (period(count1),period(count2),c2(count1),c2(count2),
     +             specT,c2T,iflag)
      call interp (period(count1),period(count2),h(count1),h(count2),
     +             specT,hT,iflag)
      call interp (period(count1),period(count2),c3(count1),c3(count2),
     +             specT,c3T,iflag)
      call interp (period(count1),period(count2),b1(count1),b1(count2),
     +             specT,b1T,iflag)
      call interp (period(count1),period(count2),b2(count1),b2(count2),
     +             specT,b2T,iflag)
      call interp (period(count1),period(count2),b3(count1),b3(count2),
     +             specT,b3T,iflag)
      call interp (period(count1),period(count2),gamma(count1),gamma(count2),
     +             specT,gammaT,iflag)
      call interp (period(count1),period(count2),sofN(count1),sofN(count2),
     +             specT,sofNT,iflag)
      call interp (period(count1),period(count2),sofR(count1),sofR(count2),
     +             specT,sofRT,iflag)
      call interp (period(count1),period(count2),sofS(count1),sofS(count2),
     +             specT,sofST,iflag)
      call interp (period(count1),period(count2),sigs2s(count1),sigs2s(count2),
     +             specT,sigs2sT,iflag)
      call interp (period(count1),period(count2),phi(count1),phi(count2),
     +             specT,phiT,iflag)
      call interp (period(count1),period(count2),tau(count1),tau(count2),
     +             specT,tauT,iflag)
      call interp (period(count1),period(count2),sig(count1),sig(count2),
     +             specT,sigT,iflag)

 1011 period1 = specT
C     Set Constant Terms
      Mref = 5.5
      Rref = 1.0
      Mh = 6.75
      Vref = 800.0
C     Set the mechanism term.
      if (ftype .eq. 0 ) then
         termsof = 0
      elseif (ftype .ge. 0.5) then
         termsof = sofRT
      elseif (ftype .le. -0.5) then
         termsof = sofNT
      endif
      R = sqrt (jbdist**2 + hT**2)
	  f_D = (c1T+c2T*(M-Mref))*alog(R/Rref) + c3T*(R-Rref)
C     Compute the ground motion for the given spectral period.
      if (M .le. Mh) then
         f_M = b1T*(M-Mh) + b2T*(M-Mh)**2.0
      else
         f_M = b3T*(M-Mh)
      endif

	  f_S = gammaT*alog(vs/vref)

	  lnY = e1T + f_D + f_M + f_S + termsof

C     Set the sigma value and convert from log10 to Ln units
c      phiT = phiT*alog(10.0)
c      tauT = tauT*alog(10.0)
c      sigma = sigT*alog(10.0)
c      sigs2sT = sigs2sT*alog(10.0)
C     Convert ground motion to units of gals in natural log units.
c      lnY = lnY*alog(10.0)
      lnY = lnY + 6.89
      period2 = period1
      return
      end

