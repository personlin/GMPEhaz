pkgname <- "GMPEhaz"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('GMPEhaz')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("AB03")
### * AB03

flush(stderr()); flush(stdout())

### Name: AB03
### Title: GMPE function for Atkinson and Boore(2003)
### Aliases: AB03

### ** Examples

AB03(6, 20, 0, 0, 10, 0, 0, 0)
AB03(7, 10, 0, 0, 10, 0, 0, 0)




cleanEx()
nameEx("AC10")
### * AC10

flush(stderr()); flush(stdout())

### Name: AC10
### Title: GMPE function for Akkar and Cagnan(2010)
### Aliases: AC10

### ** Examples

AC10(6, 20, 0, 0, 760)
AC10(7, 10, 0, 0, 760)




cleanEx()
nameEx("AGA16.tw.C01")
### * AGA16.tw.C01

flush(stderr()); flush(stdout())

### Name: AGA16.tw.C01
### Title: GMPE function for adjusted Abrahamson et al.(2016)
### Aliases: AGA16.tw.C01

### ** Examples

AGA16.tw.C01(6, 20, 0, 0, 760, 3, 10, 20)
AGA16.tw.C01(7, 10, 0, 0, 760, 0, 10, 20)




cleanEx()
nameEx("AGA16.tw.F10")
### * AGA16.tw.F10

flush(stderr()); flush(stdout())

### Name: AGA16.tw.F10
### Title: GMPE function for adjusted Abrahamson et al.(2016)
### Aliases: AGA16.tw.F10

### ** Examples

AGA16.tw.F10(6, 20, 0, 0, 760, 0, 3, 10, 20)
AGA16.tw.F10(7, 10, 0, 0, 760, 0, 0, 10, 20)




cleanEx()
nameEx("AM09")
### * AM09

flush(stderr()); flush(stdout())

### Name: AM09
### Title: GMPE function for Atkinson and Macias (2009)
### Aliases: AM09

### ** Examples

AM09(6, 20, 0)
AM09(7, 10, 0)




cleanEx()
nameEx("ASB14")
### * ASB14

flush(stderr()); flush(stdout())

### Name: ASB14
### Title: GMPE function for Akkar et al.(2013)
### Aliases: ASB14

### ** Examples

ASB14(6, 20, 0, 0, 760)
ASB14(7, 10, 0, 0, 760)




cleanEx()
nameEx("ASB14.tw.B01")
### * ASB14.tw.B01

flush(stderr()); flush(stdout())

### Name: ASB14.tw.B01
### Title: GMPE function for Akkar et al.(2013)
### Aliases: ASB14.tw.B01

### ** Examples

ASB14.tw.B01(6, 20, 0, 0, 760)
ASB14.tw.B01(7, 10, 0, 0, 760)




cleanEx()
nameEx("ASB14.tw.C01")
### * ASB14.tw.C01

flush(stderr()); flush(stdout())

### Name: ASB14.tw.C01
### Title: GMPE function for Akkar et al.(2013)
### Aliases: ASB14.tw.C01

### ** Examples

ASB14.tw.C01(6, 20, 0, 0, 760)
ASB14.tw.C01(7, 10, 0, 0, 760)




cleanEx()
nameEx("ASB14.tw.E02")
### * ASB14.tw.E02

flush(stderr()); flush(stdout())

### Name: ASB14.tw.E02
### Title: GMPE function for Akkar et al.(2013)
### Aliases: ASB14.tw.E02

### ** Examples

ASB14.tw.E02(6, 20, 0, 0, 760)
ASB14.tw.E02(7, 10, 0, 0, 760)




cleanEx()
nameEx("ASK14")
### * ASK14

flush(stderr()); flush(stdout())

### Name: ASK14
### Title: GMPE function for Abrahamson et al.(2014) NGA-West2 model
### Aliases: ASK14

### ** Examples

ASK14(6, 20, 20, 0, 90, 0, 10, 760, 0, 0, 1, 0.5, 20, 10, 0, 0)
ASK14(7, 20, 20, 0, 90, 0, 10, 760, 0, 0, 1, 0.5, 20, 10, 0, 0)




cleanEx()
nameEx("ASK14.tw.B01")
### * ASK14.tw.B01

flush(stderr()); flush(stdout())

### Name: ASK14.tw.B01
### Title: GMPE function for Abrahamson et al.(2014) NGA-West2 model
### Aliases: ASK14.tw.B01

### ** Examples

ASK14.tw.B01(6, 20, 20, 0, 90, 0, 10, 760, 0, 0, 1, 0.5, 20, 10, 0, 0)
ASK14.tw.B01(7, 20, 20, 0, 90, 0, 10, 760, 0, 0, 1, 0.5, 20, 10, 0, 0)




cleanEx()
nameEx("ASK14.tw.C01")
### * ASK14.tw.C01

flush(stderr()); flush(stdout())

### Name: ASK14.tw.C01
### Title: GMPE function for Abrahamson et al.(2014) NGA-West2 model
### Aliases: ASK14.tw.C01

### ** Examples

ASK14.tw.C01(6, 20, 20, 0, 90, 0, 10, 760, 0, 0, 1, 0.5, 20, 10, 0, 0)
ASK14.tw.C01(7, 20, 20, 0, 90, 0, 10, 760, 0, 0, 1, 0.5, 20, 10, 0, 0)




cleanEx()
nameEx("ASK14.tw.E03")
### * ASK14.tw.E03

flush(stderr()); flush(stdout())

### Name: ASK14.tw.E03
### Title: GMPE function for Abrahamson et al.(2014) NGA-West2 model
### Aliases: ASK14.tw.E03

### ** Examples

ASK14.tw.E03(6, 20, 20, 0, 90, 0, 10, 760, 0, 0, 1, 0.5, 20, 10, 0, 0)
ASK14.tw.E03(7, 20, 20, 0, 90, 0, 10, 760, 0, 0, 1, 0.5, 20, 10, 0, 0)




cleanEx()
nameEx("Arroyo2010")
### * Arroyo2010

flush(stderr()); flush(stdout())

### Name: Arroyo2010
### Title: GMPE function for Arroyo et al. (2010)
### Aliases: Arroyo2010

### ** Examples

Arroyo2010(6, 20, 0)
Arroyo2010(7, 10, 0)




cleanEx()
nameEx("BCHydroSub2018Global")
### * BCHydroSub2018Global

flush(stderr()); flush(stdout())

### Name: BCHydroSub2018Global
### Title: GMPE function for Abrahamson et al.(2018)
### Aliases: BCHydroSub2018Global

### ** Examples

BCHydroSub2018Global(6, 20, 0, 0, 760, 1, 10, 20)
BCHydroSub2018Global(7, 10, 0, 0, 760, 1, 10, 20)




cleanEx()
nameEx("BCHydroSub2018SouthAM")
### * BCHydroSub2018SouthAM

flush(stderr()); flush(stdout())

### Name: BCHydroSub2018SouthAM
### Title: GMPE function for Abrahamson et al.(2018)
### Aliases: BCHydroSub2018SouthAM

### ** Examples

BCHydroSub2018SouthAM(6, 20, 0, 0, 760, 1, 10, 20)
BCHydroSub2018SouthAM(7, 10, 0, 0, 760, 1, 10, 20)




cleanEx()
nameEx("BCHydroSub2018TW")
### * BCHydroSub2018TW

flush(stderr()); flush(stdout())

### Name: BCHydroSub2018TW
### Title: GMPE function for Abrahamson et al.(2018)
### Aliases: BCHydroSub2018TW

### ** Examples

BCHydroSub2018TW(6, 20, 0, 0, 760, 1, 10, 20)
BCHydroSub2018TW(7, 10, 0, 0, 760, 1, 10, 20)




cleanEx()
nameEx("BCHydroSubV3")
### * BCHydroSubV3

flush(stderr()); flush(stdout())

### Name: BCHydroSubV3
### Title: GMPE function for Abrahamson et al.(2016)
### Aliases: BCHydroSubV3

### ** Examples

BCHydroSubV3(6, 20, 0, 0, 760, 1, 10, 20)
BCHydroSubV3(7, 10, 0, 0, 760, 1, 10, 20)




cleanEx()
nameEx("BI14")
### * BI14

flush(stderr()); flush(stdout())

### Name: BI14
### Title: GMPE function for Bindi et al(2014)
### Aliases: BI14

### ** Examples

BI14(6, 20, 0, 0, 760)
BI14(7, 10, 0, 0, 760)




cleanEx()
nameEx("BI14.tw.B01")
### * BI14.tw.B01

flush(stderr()); flush(stdout())

### Name: BI14.tw.B01
### Title: GMPE function for Bindi et al(2014)
### Aliases: BI14.tw.B01

### ** Examples

BI14.tw.B01(6, 20, 0, 0, 760)
BI14.tw.B01(7, 10, 0, 0, 760)




cleanEx()
nameEx("BI14.tw.C01")
### * BI14.tw.C01

flush(stderr()); flush(stdout())

### Name: BI14.tw.C01
### Title: GMPE function for Bindi et al(2014)
### Aliases: BI14.tw.C01

### ** Examples

BI14.tw.C01(6, 20, 0, 0, 760)
BI14.tw.C01(7, 10, 0, 0, 760)




cleanEx()
nameEx("BI14.tw.E02")
### * BI14.tw.E02

flush(stderr()); flush(stdout())

### Name: BI14.tw.E02
### Title: GMPE function for Bindi et al(2014)
### Aliases: BI14.tw.E02

### ** Examples

BI14.tw.E02(6, 20, 0, 0, 760)
BI14.tw.E02(7, 10, 0, 0, 760)




cleanEx()
nameEx("BSSA14")
### * BSSA14

flush(stderr()); flush(stdout())

### Name: BSSA14
### Title: GMPE function for Boore et al.(2014) NGA-West2 model
### Aliases: BSSA14

### ** Examples

BSSA14(6, 20, 0, 760, 0, 0.5, 0, 0)
BSSA14(7, 20, 0, 760, 0, 0.5, 0, 0)




cleanEx()
nameEx("BSSA14.tw.B01")
### * BSSA14.tw.B01

flush(stderr()); flush(stdout())

### Name: BSSA14.tw.B01
### Title: GMPE function for Boore et al.(2014) NGA-West2 model
### Aliases: BSSA14.tw.B01

### ** Examples

BSSA14.tw.B01(6, 20, 0, 760, 0, 0.5, 0, 0)
BSSA14.tw.B01(7, 20, 0, 760, 0, 0.5, 0, 0)




cleanEx()
nameEx("BSSA14.tw.C01")
### * BSSA14.tw.C01

flush(stderr()); flush(stdout())

### Name: BSSA14.tw.C01
### Title: GMPE function for Boore et al.(2014) NGA-West2 model
### Aliases: BSSA14.tw.C01

### ** Examples

BSSA14.tw.C01(6, 20, 0, 760, 0, 0.5, 0, 0)
BSSA14.tw.C01(7, 20, 0, 760, 0, 0.5, 0, 0)




cleanEx()
nameEx("BSSA14.tw.E02")
### * BSSA14.tw.E02

flush(stderr()); flush(stdout())

### Name: BSSA14.tw.E02
### Title: GMPE function for Boore et al.(2014) NGA-West2 model
### Aliases: BSSA14.tw.E02

### ** Examples

BSSA14.tw.E02(6, 20, 0, 760, 0, 0.5, 0, 0)
BSSA14.tw.E02(7, 20, 0, 760, 0, 0.5, 0, 0)




cleanEx()
nameEx("CB14")
### * CB14

flush(stderr()); flush(stdout())

### Name: CB14
### Title: GMPE function for Campbell and Bozorgnia(2014) NGA-West2 model
### Aliases: CB14

### ** Examples

CB14(6, 20, 20, 0, 0, 760, 0, 1.5, 90, 10, 0, 20, 10, 0)
CB14(7, 20, 20, 0, 0, 760, 0, 1.5, 90, 10, 0, 20, 10, 0)




cleanEx()
nameEx("CB14.tw.C01")
### * CB14.tw.C01

flush(stderr()); flush(stdout())

### Name: CB14.tw.C01
### Title: GMPE function for adjusted Campbell and Bozorgnia(2014)
###   NGA-West2 model
### Aliases: CB14.tw.C01

### ** Examples

CB14.tw.C01(6, 20, 20, 0, 0, 760, 0, 1.5, 90, 10, 0, 20, 10, 0)
CB14.tw.C01(7, 20, 20, 0, 0, 760, 0, 1.5, 90, 10, 0, 20, 10, 0)




cleanEx()
nameEx("CB14.tw.E05")
### * CB14.tw.E05

flush(stderr()); flush(stdout())

### Name: CB14.tw.E05
### Title: GMPE function for adjusted Campbell and Bozorgnia(2014)
###   NGA-West2 model
### Aliases: CB14.tw.E05

### ** Examples

CB14.tw.E05(6, 20, 20, 0, 0, 760, 0, 1.5, 90, 10, 0, 20, 10, 0)
CB14.tw.E05(7, 20, 20, 0, 0, 760, 0, 1.5, 90, 10, 0, 20, 10, 0)




cleanEx()
nameEx("CY08")
### * CY08

flush(stderr()); flush(stdout())

### Name: CY08
### Title: GMPE function for Chiou and Youngs(2008) NGA model
### Aliases: CY08

### ** Examples

CY08(6, 20, 20, 0, 760, 90, 0, 0, 0.5, 1, 0, 20, 0)
CY08(7, 20, 20, 0, 760, 90, 0, 0, 0.5, 1, 0, 20, 0)




cleanEx()
nameEx("CY14")
### * CY14

flush(stderr()); flush(stdout())

### Name: CY14
### Title: GMPE function for Chiou and Youngs(2014) NGA-West2 model
### Aliases: CY14

### ** Examples

CY14(6, 20, 20, 0, 760, 90, 0, 0, 0.5, 1, 0, 20, 0)
CY14(7, 20, 20, 0, 760, 90, 0, 0, 0.5, 1, 0, 20, 0)




cleanEx()
nameEx("CY14.tw.B01")
### * CY14.tw.B01

flush(stderr()); flush(stdout())

### Name: CY14.tw.B01
### Title: GMPE function for Chiou and Youngs(2014) NGA-West2 model
### Aliases: CY14.tw.B01

### ** Examples

CY14.tw.B01(6, 20, 20, 0, 760, 90, 0, 0, 0.5, 1, 0, 20, 0)
CY14.tw.B01(7, 20, 20, 0, 760, 90, 0, 0, 0.5, 1, 0, 20, 0)




cleanEx()
nameEx("CY14.tw.C01")
### * CY14.tw.C01

flush(stderr()); flush(stdout())

### Name: CY14.tw.C01
### Title: GMPE function for Chiou and Youngs(2014) NGA-West2 model
### Aliases: CY14.tw.C01

### ** Examples

CY14.tw.C01(6, 20, 20, 0, 760, 90, 0, 0, 0.5, 1, 0, 20, 0)
CY14.tw.C01(7, 20, 20, 0, 760, 90, 0, 0, 0.5, 1, 0, 20, 0)




cleanEx()
nameEx("CY14.tw.E04")
### * CY14.tw.E04

flush(stderr()); flush(stdout())

### Name: CY14.tw.E04
### Title: GMPE function for Chiou and Youngs(2014) NGA-West2 model
### Aliases: CY14.tw.E04

### ** Examples

CY14.tw.E04(6, 20, 20, 0, 760, 90, 0, 0, 0.5, 1, 0, 20, 0)
CY14.tw.E04(7, 20, 20, 0, 760, 90, 0, 0, 0.5, 1, 0, 20, 0)




cleanEx()
nameEx("Camp03H")
### * Camp03H

flush(stderr()); flush(stdout())

### Name: Camp03H
### Title: GMPE function for Campbell and Bozorgnia(2003)
### Aliases: Camp03H

### ** Examples

Camp03H(6, 20, 20, 0, 0, 0, 0, 0, 0)
Camp03H(7, 10, 20, 0, 0, 0, 0, 0, 0)




cleanEx()
nameEx("Chao2017")
### * Chao2017

flush(stderr()); flush(stdout())

### Name: Chao2017
### Title: GMPE function for Chao et al.(2017)
### Aliases: Chao2017

### ** Examples

Chao2017(6, 20, 0, 0, 760, 0, 0, 0, 0)
Chao2017(7, 10, 0, 0, 760, 0, 0, 0, 0)




cleanEx()
nameEx("Chao2018")
### * Chao2018

flush(stderr()); flush(stdout())

### Name: Chao2018
### Title: GMPE function for Chao et al.(2018)
### Aliases: Chao2018

### ** Examples

Chao2018(6, 20, 0, 0, 760, 0, 0, 0, 0)
Chao2018(7, 10, 0, 0, 760, 0, 0, 0, 0)




cleanEx()
nameEx("Cru.Com.001")
### * Cru.Com.001

flush(stderr()); flush(stdout())

### Name: Cru.Com.001
### Title: GMPE function for Crustal Common form 001 (2017)
### Aliases: Cru.Com.001

### ** Examples

Cru.Com.001(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
Cru.Com.001(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )




cleanEx()
nameEx("Cru.Com.002")
### * Cru.Com.002

flush(stderr()); flush(stdout())

### Name: Cru.Com.002
### Title: GMPE function for Crustal Common form 002 (2017)
### Aliases: Cru.Com.002

### ** Examples

Cru.Com.002(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
Cru.Com.002(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )




cleanEx()
nameEx("Cru.Com.003")
### * Cru.Com.003

flush(stderr()); flush(stdout())

### Name: Cru.Com.003
### Title: GMPE function for Crustal Common form 003 (2017)
### Aliases: Cru.Com.003

### ** Examples

Cru.Com.003(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
Cru.Com.003(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )




cleanEx()
nameEx("Cru.Com.004")
### * Cru.Com.004

flush(stderr()); flush(stdout())

### Name: Cru.Com.004
### Title: GMPE function for Crustal Common form 004 (2017)
### Aliases: Cru.Com.004

### ** Examples

Cru.Com.004(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
Cru.Com.004(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )




cleanEx()
nameEx("Cru.Com.005")
### * Cru.Com.005

flush(stderr()); flush(stdout())

### Name: Cru.Com.005
### Title: GMPE function for Crustal Common form 005 (2017)
### Aliases: Cru.Com.005

### ** Examples

Cru.Com.005(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
Cru.Com.005(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )




cleanEx()
nameEx("Cru.Com.006")
### * Cru.Com.006

flush(stderr()); flush(stdout())

### Name: Cru.Com.006
### Title: GMPE function for Crustal Common form 006 (2017)
### Aliases: Cru.Com.006

### ** Examples

Cru.Com.006(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
Cru.Com.006(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )




cleanEx()
nameEx("Cru.Com.007")
### * Cru.Com.007

flush(stderr()); flush(stdout())

### Name: Cru.Com.007
### Title: GMPE function for Crustal Common form 007 (2017)
### Aliases: Cru.Com.007

### ** Examples

Cru.Com.007(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
Cru.Com.007(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )




cleanEx()
nameEx("Cru.Com.008")
### * Cru.Com.008

flush(stderr()); flush(stdout())

### Name: Cru.Com.008
### Title: GMPE function for Crustal Common form 008 (2017)
### Aliases: Cru.Com.008

### ** Examples

Cru.Com.008(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
Cru.Com.008(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )




cleanEx()
nameEx("Cru.Com.009")
### * Cru.Com.009

flush(stderr()); flush(stdout())

### Name: Cru.Com.009
### Title: GMPE function for Crustal Common form 009 (2017)
### Aliases: Cru.Com.009

### ** Examples

Cru.Com.009(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
Cru.Com.009(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )




cleanEx()
nameEx("Cru.Com.010")
### * Cru.Com.010

flush(stderr()); flush(stdout())

### Name: Cru.Com.010
### Title: GMPE function for Crustal Common form 010 (2017)
### Aliases: Cru.Com.010

### ** Examples

Cru.Com.010(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
Cru.Com.010(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )




cleanEx()
nameEx("Cru.Com.011")
### * Cru.Com.011

flush(stderr()); flush(stdout())

### Name: Cru.Com.011
### Title: GMPE function for Crustal Common form 011 (2017)
### Aliases: Cru.Com.011

### ** Examples

Cru.Com.011(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
Cru.Com.011(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )




cleanEx()
nameEx("Cru.Com.012")
### * Cru.Com.012

flush(stderr()); flush(stdout())

### Name: Cru.Com.012
### Title: GMPE function for Crustal Common form 012 (2017)
### Aliases: Cru.Com.012

### ** Examples

Cru.Com.012(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
Cru.Com.012(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )




cleanEx()
nameEx("Cru.Com.013")
### * Cru.Com.013

flush(stderr()); flush(stdout())

### Name: Cru.Com.013
### Title: GMPE function for Crustal Common form 013 (2017)
### Aliases: Cru.Com.013

### ** Examples

Cru.Com.013(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
Cru.Com.013(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )




cleanEx()
nameEx("Cru.Com.014")
### * Cru.Com.014

flush(stderr()); flush(stdout())

### Name: Cru.Com.014
### Title: GMPE function for Crustal Common form 014 (2017)
### Aliases: Cru.Com.014

### ** Examples

Cru.Com.014(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
Cru.Com.014(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )




cleanEx()
nameEx("Cru.Com.015")
### * Cru.Com.015

flush(stderr()); flush(stdout())

### Name: Cru.Com.015
### Title: GMPE function for Crustal Common form 015 (2017)
### Aliases: Cru.Com.015

### ** Examples

Cru.Com.015(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
Cru.Com.015(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )




cleanEx()
nameEx("Cru.Com.016")
### * Cru.Com.016

flush(stderr()); flush(stdout())

### Name: Cru.Com.016
### Title: GMPE function for Crustal Common form 016 (2017)
### Aliases: Cru.Com.016

### ** Examples

Cru.Com.016(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
Cru.Com.016(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )




cleanEx()
nameEx("Cru.Com.017")
### * Cru.Com.017

flush(stderr()); flush(stdout())

### Name: Cru.Com.017
### Title: GMPE function for Crustal Common form 017 (2017)
### Aliases: Cru.Com.017

### ** Examples

Cru.Com.017(6, 20, 20, 5, 0, 90, 20, 20, 0, 0)
Cru.Com.017(7, 20, 20, 2, 0, 90, 20, 20, 0, 0 )




cleanEx()
nameEx("GK16")
### * GK16

flush(stderr()); flush(stdout())

### Name: GK16
### Title: GMPE function for Graizer and Kalkan(2016)
### Aliases: GK16

### ** Examples

GK16(6, 20, 0, 0, 760, 150, 1)
GK16(7, 10, 0, 0, 760, 150, 1)




cleanEx()
nameEx("Garcia05")
### * Garcia05

flush(stderr()); flush(stdout())

### Name: Garcia05
### Title: GMPE function for Garcia et al.(2005)
### Aliases: Garcia05

### ** Examples

Garcia05(6, 20, 0, 35)
Garcia05(7, 10, 0, 60)




cleanEx()
nameEx("Gregor06")
### * Gregor06

flush(stderr()); flush(stdout())

### Name: Gregor06
### Title: GMPE function for Gregor et al.(2006)
### Aliases: Gregor06

### ** Examples

Gregor06(6, 20, 0, 760)
Gregor06(7, 10, 0, 760)




cleanEx()
nameEx("I14")
### * I14

flush(stderr()); flush(stdout())

### Name: I14
### Title: GMPE function for Idriss(2014)
### Aliases: I14

### ** Examples

I14(6, 20, 0, 0, 760)
I14(7, 10, 0, 0, 760)




cleanEx()
nameEx("I14.tw.B01")
### * I14.tw.B01

flush(stderr()); flush(stdout())

### Name: I14.tw.B01
### Title: GMPE function for Idriss(2014)
### Aliases: I14.tw.B01

### ** Examples

I14.tw.B01(6, 20, 0, 0, 760)
I14.tw.B01(7, 10, 0, 0, 760)




cleanEx()
nameEx("I14.tw.C01")
### * I14.tw.C01

flush(stderr()); flush(stdout())

### Name: I14.tw.C01
### Title: GMPE function for Idriss(2014)
### Aliases: I14.tw.C01

### ** Examples

I14.tw.C01(6, 20, 0, 0, 760)
I14.tw.C01(7, 10, 0, 0, 760)




cleanEx()
nameEx("I14.tw.E04")
### * I14.tw.E04

flush(stderr()); flush(stdout())

### Name: I14.tw.E04
### Title: GMPE function for for adjusted Idriss(2014) NGA-West2 model
### Aliases: I14.tw.E04

### ** Examples

I14.tw.E04(6, 20, 0, 0, 760, 0)
I14.tw.E04(7, 10, 0, 0, 760, 0)




cleanEx()
nameEx("KAAH15")
### * KAAH15

flush(stderr()); flush(stdout())

### Name: KAAH15
### Title: GMPE function for Kale et al.(2015)
### Aliases: KAAH15

### ** Examples

KAAH15(6, 20, 0, 0, 760, 0)
KAAH15(7, 10, 0, 0, 760, 0)




cleanEx()
nameEx("Kanno06")
### * Kanno06

flush(stderr()); flush(stdout())

### Name: Kanno06
### Title: GMPE function for Kanno et al.(2006)
### Aliases: Kanno06

### ** Examples

Kanno06(6, 20, 0, 760, 10)
Kanno06(7, 10, 0, 760, 10)




cleanEx()
nameEx("LL08.C02")
### * LL08.C02

flush(stderr()); flush(stdout())

### Name: LL08.C02
### Title: GMPE function for adjusted Lin and Lee(2008)
### Aliases: LL08.C02

### ** Examples

LL08.C02(6, 20, 10, 0, 0, 760)
LL08.C02(7, 10, 10, 0, 0, 760)




cleanEx()
nameEx("LL08.F04")
### * LL08.F04

flush(stderr()); flush(stdout())

### Name: LL08.F04
### Title: GMPE function for adjusted Lin and Lee(2008)
### Aliases: LL08.F04

### ** Examples

LL08.F04(6, 20, 10, 0, 0, 760)
LL08.F04(7, 10, 10, 0, 0, 760)




cleanEx()
nameEx("LL08")
### * LL08

flush(stderr()); flush(stdout())

### Name: LL08
### Title: GMPE function for Lin and Lee(2008)
### Aliases: LL08

### ** Examples

LL08(6, 20, 10, 0, 0, 760)
LL08(7, 10, 10, 0, 0, 760)




cleanEx()
nameEx("LL08Rock")
### * LL08Rock

flush(stderr()); flush(stdout())

### Name: LL08Rock
### Title: GMPE function for Lin and Lee(2008)
### Aliases: LL08Rock

### ** Examples

LL08Rock(6, 20, 10, 0, 0)
LL08Rock(7, 10, 10, 0, 0)




cleanEx()
nameEx("LL08Soil")
### * LL08Soil

flush(stderr()); flush(stdout())

### Name: LL08Soil
### Title: GMPE function for Lin and Lee(2008)
### Aliases: LL08Soil

### ** Examples

LL08Soil(6, 20, 10, 0, 0)
LL08Soil(7, 10, 10, 0, 0)




cleanEx()
nameEx("Lin09")
### * Lin09

flush(stderr()); flush(stdout())

### Name: Lin09
### Title: GMPE function for Lin (2009)
### Aliases: Lin09

### ** Examples

Lin09(6, 20, 0, 760, 0)
Lin09(7, 10, 0, 760, 0)




cleanEx()
nameEx("Lin11")
### * Lin11

flush(stderr()); flush(stdout())

### Name: Lin11
### Title: GMPE function for Lin et al.(2011)
### Aliases: Lin11

### ** Examples

Lin11(6, 20, 0, 0, 760)
Lin11(7, 10, 0, 0, 760)




cleanEx()
nameEx("Lin11Rock")
### * Lin11Rock

flush(stderr()); flush(stdout())

### Name: Lin11Rock
### Title: GMPE function for Lin et al.(2011)
### Aliases: Lin11Rock

### ** Examples

Lin11Rock(6, 20, 0, 0)
Lin11Rock(7, 10, 0, 0)




cleanEx()
nameEx("Lin11Soil")
### * Lin11Soil

flush(stderr()); flush(stdout())

### Name: Lin11Soil
### Title: GMPE function for Lin et al.(2011)
### Aliases: Lin11Soil

### ** Examples

Lin11Soil(6, 20, 0, 0)
Lin11Soil(7, 10, 0, 0)




cleanEx()
nameEx("Loh96")
### * Loh96

flush(stderr()); flush(stdout())

### Name: Loh96
### Title: GMPE function for Loh et al.(1996) PGA only
### Aliases: Loh96

### ** Examples

Loh96(6, 20)
Loh96(7, 10)




cleanEx()
nameEx("MBR17")
### * MBR17

flush(stderr()); flush(stdout())

### Name: MBR17
### Title: GMPE function for Montalva et al.(2017)
### Aliases: MBR17

### ** Examples

MBR17(6, 20, 0, 0, 760, 1, 10, 20)
MBR17(7, 10, 0, 0, 760, 1, 10, 20)




cleanEx()
nameEx("Mcverry2006Sub")
### * Mcverry2006Sub

flush(stderr()); flush(stdout())

### Name: Mcverry2006Sub
### Title: GMPE function for Mcverry et al.(2006) Subduction GMPE.
### Aliases: Mcverry2006Sub

### ** Examples

Mcverry2006Sub(6, 20, 0, 0, 0, 90, 10, 10, 0, 0)
Mcverry2006Sub(7, 20, 0, 0, 0, 90, 10, 10, 0, 0)




cleanEx()
nameEx("NCREE2011")
### * NCREE2011

flush(stderr()); flush(stdout())

### Name: NCREE2011
### Title: GMPE function for NCREE(2011)
### Aliases: NCREE2011

### ** Examples

NCREE2011(6, 20, 0)
NCREE2011(7, 10, 0)




cleanEx()
nameEx("PhungCru17")
### * PhungCru17

flush(stderr()); flush(stdout())

### Name: PhungCru17
### Title: GMPE function for Phung et al.(2017)
### Aliases: PhungCru17

### ** Examples

PhungCru17(6, 20, 0, 760, 90, 0, 0, 0.5, 1, 0,  1)
PhungCru17(7, 20, 0, 760, 90, 0, 0, 0.5, 1, 0,  1)




cleanEx()
nameEx("PhungCru18")
### * PhungCru18

flush(stderr()); flush(stdout())

### Name: PhungCru18
### Title: GMPE function for Phung et al.(2018)
### Aliases: PhungCru18

### ** Examples

PhungCru18(6, 20, 20, 20, 0, 760, 90, 0, 0, 0.5, 1, 1, 0)
PhungCru18(7, 20, 20, 20, 0, 760, 90, 0, 0, 0.5, 1, 1, 0)




cleanEx()
nameEx("PhungSub17")
### * PhungSub17

flush(stderr()); flush(stdout())

### Name: PhungSub17
### Title: GMPE function for Phung et al.(2017)
### Aliases: PhungSub17

### ** Examples

PhungSub17(6, 20, 0, 0, 760, 0.5, 0, 1)
PhungSub17(7, 10, 0, 1, 760, 0.5, 0, 1)




cleanEx()
nameEx("PhungSub18")
### * PhungSub18

flush(stderr()); flush(stdout())

### Name: PhungSub18
### Title: GMPE function for Phung et al.(2018)
### Aliases: PhungSub18

### ** Examples

PhungSub18(6, 20, 0, 0, 760, 0.5, 0, 1)
PhungSub18(7, 10, 0, 1, 760, 0.5, 0, 1)




cleanEx()
nameEx("Sinotech12")
### * Sinotech12

flush(stderr()); flush(stdout())

### Name: Sinotech12
### Title: GMPE function for Sinotech(2012)
### Aliases: Sinotech12

### ** Examples

Sinotech12(6, 20, 0, 760, 0)
Sinotech12(7, 10, 0, 760, 0)




cleanEx()
nameEx("Sub.Inter.Com.001")
### * Sub.Inter.Com.001

flush(stderr()); flush(stdout())

### Name: Sub.Inter.Com.001
### Title: GMPE function for Subduction interface common form 001 (2017)
### Aliases: Sub.Inter.Com.001

### ** Examples

Sub.Inter.Com.001(6, 20, 5, 0)
Sub.Inter.Com.001(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Inter.Com.002")
### * Sub.Inter.Com.002

flush(stderr()); flush(stdout())

### Name: Sub.Inter.Com.002
### Title: GMPE function for Subduction interface common form 002 (2017)
### Aliases: Sub.Inter.Com.002

### ** Examples

Sub.Inter.Com.002(6, 20, 5, 0)
Sub.Inter.Com.002(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Inter.Com.003")
### * Sub.Inter.Com.003

flush(stderr()); flush(stdout())

### Name: Sub.Inter.Com.003
### Title: GMPE function for Subduction interface common form 003 (2017)
### Aliases: Sub.Inter.Com.003

### ** Examples

Sub.Inter.Com.003(6, 20, 5, 0)
Sub.Inter.Com.003(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Inter.Com.004")
### * Sub.Inter.Com.004

flush(stderr()); flush(stdout())

### Name: Sub.Inter.Com.004
### Title: GMPE function for Subduction interface common form 004 (2017)
### Aliases: Sub.Inter.Com.004

### ** Examples

Sub.Inter.Com.004(6, 20, 5, 0)
Sub.Inter.Com.004(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Inter.Com.005")
### * Sub.Inter.Com.005

flush(stderr()); flush(stdout())

### Name: Sub.Inter.Com.005
### Title: GMPE function for Subduction interface common form 005 (2017)
### Aliases: Sub.Inter.Com.005

### ** Examples

Sub.Inter.Com.005(6, 20, 5, 0)
Sub.Inter.Com.005(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Inter.Com.006")
### * Sub.Inter.Com.006

flush(stderr()); flush(stdout())

### Name: Sub.Inter.Com.006
### Title: GMPE function for Subduction interface common form 006 (2017)
### Aliases: Sub.Inter.Com.006

### ** Examples

Sub.Inter.Com.006(6, 20, 5, 0)
Sub.Inter.Com.006(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Inter.Com.007")
### * Sub.Inter.Com.007

flush(stderr()); flush(stdout())

### Name: Sub.Inter.Com.007
### Title: GMPE function for Subduction interface common form 007 (2017)
### Aliases: Sub.Inter.Com.007

### ** Examples

Sub.Inter.Com.007(6, 20, 5, 0)
Sub.Inter.Com.007(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Inter.Com.008")
### * Sub.Inter.Com.008

flush(stderr()); flush(stdout())

### Name: Sub.Inter.Com.008
### Title: GMPE function for Subduction interface common form 008 (2017)
### Aliases: Sub.Inter.Com.008

### ** Examples

Sub.Inter.Com.008(6, 20, 5, 0)
Sub.Inter.Com.008(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Inter.Com.009")
### * Sub.Inter.Com.009

flush(stderr()); flush(stdout())

### Name: Sub.Inter.Com.009
### Title: GMPE function for Subduction interface common form 009 (2017)
### Aliases: Sub.Inter.Com.009

### ** Examples

Sub.Inter.Com.009(6, 20, 5, 0)
Sub.Inter.Com.009(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Inter.Com.010")
### * Sub.Inter.Com.010

flush(stderr()); flush(stdout())

### Name: Sub.Inter.Com.010
### Title: GMPE function for Subduction interface common form 010 (2017)
### Aliases: Sub.Inter.Com.010

### ** Examples

Sub.Inter.Com.010(6, 20, 5, 0)
Sub.Inter.Com.010(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Inter.Com.011")
### * Sub.Inter.Com.011

flush(stderr()); flush(stdout())

### Name: Sub.Inter.Com.011
### Title: GMPE function for Subduction interface common form 011 (2017)
### Aliases: Sub.Inter.Com.011

### ** Examples

Sub.Inter.Com.011(6, 20, 5, 0)
Sub.Inter.Com.011(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Inter.Com.012")
### * Sub.Inter.Com.012

flush(stderr()); flush(stdout())

### Name: Sub.Inter.Com.012
### Title: GMPE function for Subduction interface common form 012 (2017)
### Aliases: Sub.Inter.Com.012

### ** Examples

Sub.Inter.Com.012(6, 20, 5, 0)
Sub.Inter.Com.012(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Inter.Com.013")
### * Sub.Inter.Com.013

flush(stderr()); flush(stdout())

### Name: Sub.Inter.Com.013
### Title: GMPE function for Subduction interface common form 013 (2017)
### Aliases: Sub.Inter.Com.013

### ** Examples

Sub.Inter.Com.013(6, 20, 5, 0)
Sub.Inter.Com.013(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Inter.Com.014")
### * Sub.Inter.Com.014

flush(stderr()); flush(stdout())

### Name: Sub.Inter.Com.014
### Title: GMPE function for Subduction interface common form 014 (2017)
### Aliases: Sub.Inter.Com.014

### ** Examples

Sub.Inter.Com.014(6, 20, 5, 0)
Sub.Inter.Com.014(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Inter.Com.015")
### * Sub.Inter.Com.015

flush(stderr()); flush(stdout())

### Name: Sub.Inter.Com.015
### Title: GMPE function for Subduction interface common form 015 (2017)
### Aliases: Sub.Inter.Com.015

### ** Examples

Sub.Inter.Com.015(6, 20, 5, 0)
Sub.Inter.Com.015(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Inter.Com.016")
### * Sub.Inter.Com.016

flush(stderr()); flush(stdout())

### Name: Sub.Inter.Com.016
### Title: GMPE function for Subduction interface common form 016 (2017)
### Aliases: Sub.Inter.Com.016

### ** Examples

Sub.Inter.Com.016(6, 20, 5, 0)
Sub.Inter.Com.016(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Inter.Com.017")
### * Sub.Inter.Com.017

flush(stderr()); flush(stdout())

### Name: Sub.Inter.Com.017
### Title: GMPE function for Subduction interface common form 017 (2017)
### Aliases: Sub.Inter.Com.017

### ** Examples

Sub.Inter.Com.017(6, 20, 5, 0)
Sub.Inter.Com.017(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Intra.Com.001")
### * Sub.Intra.Com.001

flush(stderr()); flush(stdout())

### Name: Sub.Intra.Com.001
### Title: GMPE function for Subduction intraslab common form 001 (2017)
### Aliases: Sub.Intra.Com.001

### ** Examples

Sub.Intra.Com.001(6, 20, 5, 0)
Sub.Intra.Com.001(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Intra.Com.002")
### * Sub.Intra.Com.002

flush(stderr()); flush(stdout())

### Name: Sub.Intra.Com.002
### Title: GMPE function for Subduction intraslab common form 002 (2017)
### Aliases: Sub.Intra.Com.002

### ** Examples

Sub.Intra.Com.002(6, 20, 5, 0)
Sub.Intra.Com.002(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Intra.Com.003")
### * Sub.Intra.Com.003

flush(stderr()); flush(stdout())

### Name: Sub.Intra.Com.003
### Title: GMPE function for Subduction intraslab common form 003 (2017)
### Aliases: Sub.Intra.Com.003

### ** Examples

Sub.Intra.Com.003(6, 20, 5, 0)
Sub.Intra.Com.003(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Intra.Com.004")
### * Sub.Intra.Com.004

flush(stderr()); flush(stdout())

### Name: Sub.Intra.Com.004
### Title: GMPE function for Subduction intraslab common form 004 (2017)
### Aliases: Sub.Intra.Com.004

### ** Examples

Sub.Intra.Com.004(6, 20, 5, 0)
Sub.Intra.Com.004(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Intra.Com.005")
### * Sub.Intra.Com.005

flush(stderr()); flush(stdout())

### Name: Sub.Intra.Com.005
### Title: GMPE function for Subduction intraslab common form 005 (2017)
### Aliases: Sub.Intra.Com.005

### ** Examples

Sub.Intra.Com.005(6, 20, 5, 0)
Sub.Intra.Com.005(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Intra.Com.006")
### * Sub.Intra.Com.006

flush(stderr()); flush(stdout())

### Name: Sub.Intra.Com.006
### Title: GMPE function for Subduction intraslab common form 006 (2017)
### Aliases: Sub.Intra.Com.006

### ** Examples

Sub.Intra.Com.006(6, 20, 5, 0)
Sub.Intra.Com.006(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Intra.Com.007")
### * Sub.Intra.Com.007

flush(stderr()); flush(stdout())

### Name: Sub.Intra.Com.007
### Title: GMPE function for Subduction intraslab common form 007 (2017)
### Aliases: Sub.Intra.Com.007

### ** Examples

Sub.Intra.Com.007(6, 20, 5, 0)
Sub.Intra.Com.007(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Intra.Com.008")
### * Sub.Intra.Com.008

flush(stderr()); flush(stdout())

### Name: Sub.Intra.Com.008
### Title: GMPE function for Subduction intraslab common form 008 (2017)
### Aliases: Sub.Intra.Com.008

### ** Examples

Sub.Intra.Com.008(6, 20, 5, 0)
Sub.Intra.Com.008(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Intra.Com.009")
### * Sub.Intra.Com.009

flush(stderr()); flush(stdout())

### Name: Sub.Intra.Com.009
### Title: GMPE function for Subduction intraslab common form 009 (2017)
### Aliases: Sub.Intra.Com.009

### ** Examples

Sub.Intra.Com.009(6, 20, 5, 0)
Sub.Intra.Com.009(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Intra.Com.010")
### * Sub.Intra.Com.010

flush(stderr()); flush(stdout())

### Name: Sub.Intra.Com.010
### Title: GMPE function for Subduction intraslab common form 010 (2017)
### Aliases: Sub.Intra.Com.010

### ** Examples

Sub.Intra.Com.010(6, 20, 5, 0)
Sub.Intra.Com.010(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Intra.Com.011")
### * Sub.Intra.Com.011

flush(stderr()); flush(stdout())

### Name: Sub.Intra.Com.011
### Title: GMPE function for Subduction intraslab common form 011 (2017)
### Aliases: Sub.Intra.Com.011

### ** Examples

Sub.Intra.Com.011(6, 20, 5, 0)
Sub.Intra.Com.011(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Intra.Com.012")
### * Sub.Intra.Com.012

flush(stderr()); flush(stdout())

### Name: Sub.Intra.Com.012
### Title: GMPE function for Subduction intraslab common form 012 (2017)
### Aliases: Sub.Intra.Com.012

### ** Examples

Sub.Intra.Com.012(6, 20, 5, 0)
Sub.Intra.Com.012(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Intra.Com.013")
### * Sub.Intra.Com.013

flush(stderr()); flush(stdout())

### Name: Sub.Intra.Com.013
### Title: GMPE function for Subduction intraslab common form 013 (2017)
### Aliases: Sub.Intra.Com.013

### ** Examples

Sub.Intra.Com.013(6, 20, 5, 0)
Sub.Intra.Com.013(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Intra.Com.014")
### * Sub.Intra.Com.014

flush(stderr()); flush(stdout())

### Name: Sub.Intra.Com.014
### Title: GMPE function for Subduction intraslab common form 014 (2017)
### Aliases: Sub.Intra.Com.014

### ** Examples

Sub.Intra.Com.014(6, 20, 5, 0)
Sub.Intra.Com.014(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Intra.Com.015")
### * Sub.Intra.Com.015

flush(stderr()); flush(stdout())

### Name: Sub.Intra.Com.015
### Title: GMPE function for Subduction intraslab common form 015 (2017)
### Aliases: Sub.Intra.Com.015

### ** Examples

Sub.Intra.Com.015(6, 20, 5, 0)
Sub.Intra.Com.015(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Intra.Com.016")
### * Sub.Intra.Com.016

flush(stderr()); flush(stdout())

### Name: Sub.Intra.Com.016
### Title: GMPE function for Subduction intraslab common form 016 (2017)
### Aliases: Sub.Intra.Com.016

### ** Examples

Sub.Intra.Com.016(6, 20, 5, 0)
Sub.Intra.Com.016(7, 20, 2, 0)




cleanEx()
nameEx("Sub.Intra.Com.017")
### * Sub.Intra.Com.017

flush(stderr()); flush(stdout())

### Name: Sub.Intra.Com.017
### Title: GMPE function for Subduction intraslab common form 017 (2017)
### Aliases: Sub.Intra.Com.017

### ** Examples

Sub.Intra.Com.017(6, 20, 5, 0)
Sub.Intra.Com.017(7, 20, 2, 0)




cleanEx()
nameEx("Youngs97Rock")
### * Youngs97Rock

flush(stderr()); flush(stdout())

### Name: Youngs97Rock
### Title: GMPE function for Youngs et al.(1997)
### Aliases: Youngs97Rock

### ** Examples

Youngs97Rock(6, 20, 0, 0, 10)
Youngs97Rock(7, 10, 0, 0, 10)




cleanEx()
nameEx("Youngs97Soil")
### * Youngs97Soil

flush(stderr()); flush(stdout())

### Name: Youngs97Soil
### Title: GMPE function for Youngs et al.(1997)
### Aliases: Youngs97Soil

### ** Examples

Youngs97Soil(6, 20, 0, 0, 10)
Youngs97Soil(7, 10, 0, 0, 10)




cleanEx()
nameEx("Zh06")
### * Zh06

flush(stderr()); flush(stdout())

### Name: Zh06
### Title: GMPE function for Zhao et al.(2006)
### Aliases: Zh06

### ** Examples

Zh06(6, 20, 0, 1.0, 0, 0.0, 10)
Zh06(7, 10, 0, 1.0, 0, 0.0, 10)




cleanEx()
nameEx("Zh16Cru")
### * Zh16Cru

flush(stderr()); flush(stdout())

### Name: Zh16Cru
### Title: GMPE function for Zhao et al.(2016)
### Aliases: Zh16Cru

### ** Examples

Zh16Cru(6, 20, 0, 1.0, 0, 0.0, 10)
Zh16Cru(7, 10, 0, 1.0, 0, 0.0, 10)




cleanEx()
nameEx("Zh16Inter")
### * Zh16Inter

flush(stderr()); flush(stdout())

### Name: Zh16Inter
### Title: GMPE function for Zhao et al.(2016)
### Aliases: Zh16Inter

### ** Examples

Zh16Inter(6, 20, 1.0, 0, 10)
Zh16Inter(7, 10, 1.0, 0, 10)




cleanEx()
nameEx("Zh16Slab")
### * Zh16Slab

flush(stderr()); flush(stdout())

### Name: Zh16Slab
### Title: GMPE function for Zhao et al.(2016)
### Aliases: Zh16Slab

### ** Examples

Zh16Slab(6, 20, 1.0, 0, 10)
Zh16Slab(7, 10, 1.0, 0, 10)




cleanEx()
nameEx("Zh16Sub")
### * Zh16Sub

flush(stderr()); flush(stdout())

### Name: Zh16Sub
### Title: GMPE function for Zhao et al.(2016) Subduction GMPE
### Aliases: Zh16Sub

### ** Examples

Zh16Sub(6, 20, 1.0, 0, 1.0, 10)
Zh16Sub(7, 10, 1.0, 0, 1.0, 10)




cleanEx()
nameEx("atten2")
### * atten2

flush(stderr()); flush(stdout())

### Name: atten2
### Title: Wrap function for all GMPE
### Aliases: atten2

### ** Examples

atten2("LL08Rock", 0, Mag=c(6,7,8), 20, 0, 0, 10, 0, FALSE)
atten2("LL08Rock", Periods=c(0.01, 0.1, 0.2, 0.5, 1, 2, 5), 6, 10, 0, 0, 10, 0, FALSE)




cleanEx()
nameEx("get.GMPE.prd")
### * get.GMPE.prd

flush(stderr()); flush(stdout())

### Name: get.GMPE.prd
### Title: To obtain usable period range of GMPE
### Aliases: get.GMPE.prd

### ** Examples

get.GMPE.prd("ASK14")
get.GMPE.prd("CY14")




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
