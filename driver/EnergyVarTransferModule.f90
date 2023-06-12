module EnergyVarTransferModule

    use NoahowpmpIOType
    use NoahowpmpType

    implicit none
  
  contains
  
  subroutine EnergyVarInTransfer(noahowpmp, NoahowpmpIO)

    implicit none

    type(NoahowpmpIO_type), intent(inout) :: NoahowpmpIO
    type(noahowp_type),     intent(inout) :: noahowpmp

    associate(ix   => NoahowpmpIO%ix, &
              iy   => NoahowpmpIO%iy)

    noahowpmp%energy%TV = NoahowpmpIO%TV(ix,iy)
    noahowpmp%energy%TG = NoahowpmpIO%TG(ix,iy)
    noahowpmp%energy%FCEV = NoahowpmpIO%FCEV(ix,iy)
    noahowpmp%energy%FCTR = NoahowpmpIO%FCTR(ix,iy)
    noahowpmp%energy%IGS = NoahowpmpIO%IGS(ix,iy)
    noahowpmp%energy%FROZEN_CANOPY = NoahowpmpIO%FROZEN_CANOPY(ix,iy)
    noahowpmp%energy%FROZEN_GROUND = NoahowpmpIO%FROZEN_GROUND(ix,iy)
    noahowpmp%energy%IMELT(:) = NoahowpmpIO%IMELT(ix,iy,:)
    noahowpmp%energy%STC(:) = NoahowpmpIO%STC(ix,iy,:)
    noahowpmp%energy%DF(:) = NoahowpmpIO%DF(ix,iy,:)
    noahowpmp%energy%HCPCT(:) = NoahowpmpIO%HCPCT(ix,iy,:)
    noahowpmp%energy%FACT(:) = NoahowpmpIO%FACT(ix,iy,:)
    noahowpmp%energy%PAHV = NoahowpmpIO%PAHV(ix,iy)
    noahowpmp%energy%PAHG = NoahowpmpIO%PAHG(ix,iy)
    noahowpmp%energy%PAHB = NoahowpmpIO%PAHB(ix,iy)
    noahowpmp%energy%PAH = NoahowpmpIO%PAH(ix,iy)
    noahowpmp%energy%TAUSS = NoahowpmpIO%TAUSS(ix,iy)
    noahowpmp%energy%FAGE = NoahowpmpIO%FAGE(ix,iy)
    noahowpmp%energy%ALB = NoahowpmpIO%ALB(ix,iy)
    noahowpmp%energy%ALBOLD = NoahowpmpIO%ALBOLD(ix,iy)
    noahowpmp%energy%ALBD(:) = NoahowpmpIO%ALBD(ix,iy,:)
    noahowpmp%energy%ALBI(:) = NoahowpmpIO%ALBI(ix,iy,:)
    noahowpmp%energy%ALBGRD(:) = NoahowpmpIO%ALBGRD(ix,iy,:)
    noahowpmp%energy%ALBGRI(:) = NoahowpmpIO%ALBGRI(ix,iy,:)
    noahowpmp%energy%ALBSND(:) = NoahowpmpIO%ALBSND(ix,iy,:)
    noahowpmp%energy%ALBSNI(:) = NoahowpmpIO%ALBSNI(ix,iy,:)
    noahowpmp%energy%FABD(:) = NoahowpmpIO%FABD(ix,iy,:)
    noahowpmp%energy%FABI(:) = NoahowpmpIO%FABI(ix,iy,:)
    noahowpmp%energy%FTDD(:) = NoahowpmpIO%FTDD(ix,iy,:)
    noahowpmp%energy%FTDI(:) = NoahowpmpIO%FTDI(ix,iy,:)
    noahowpmp%energy%FTID(:) = NoahowpmpIO%FTID(ix,iy,:)
    noahowpmp%energy%FTII(:) = NoahowpmpIO%FTII(ix,iy,:)
    noahowpmp%energy%FREVD(:) = NoahowpmpIO%FREVD(ix,iy,:)
    noahowpmp%energy%FREVI(:) = NoahowpmpIO%FREVI(ix,iy,:)
    noahowpmp%energy%FREGD(:) = NoahowpmpIO%FREGD(ix,iy,:)
    noahowpmp%energy%FREGI(:) = NoahowpmpIO%FREGI(ix,iy,:)
    noahowpmp%energy%RHO(:) = NoahowpmpIO%RHO(ix,iy,:)
    noahowpmp%energy%TAU(:) = NoahowpmpIO%TAU(ix,iy,:)
    noahowpmp%energy%COSZ = NoahowpmpIO%COSZ(ix,iy)
    noahowpmp%energy%COSZ_HORIZ = NoahowpmpIO%COSZ_HORIZ(ix,iy)
    noahowpmp%energy%BGAP = NoahowpmpIO%BGAP(ix,iy)
    noahowpmp%energy%WGAP = NoahowpmpIO%WGAP(ix,iy)
    noahowpmp%energy%FSUN = NoahowpmpIO%FSUN(ix,iy)
    noahowpmp%energy%FSHA = NoahowpmpIO%FSHA(ix,iy)
    noahowpmp%energy%LAISUN = NoahowpmpIO%LAISUN(ix,iy)
    noahowpmp%energy%LAISHA = NoahowpmpIO%LAISHA(ix,iy)
    noahowpmp%energy%PARSUN = NoahowpmpIO%PARSUN(ix,iy)
    noahowpmp%energy%PARSHA = NoahowpmpIO%PARSHA(ix,iy)
    noahowpmp%energy%SAV = NoahowpmpIO%SAV(ix,iy)
    noahowpmp%energy%SAG = NoahowpmpIO%SAG(ix,iy)
    noahowpmp%energy%FSA = NoahowpmpIO%FSA(ix,iy)
    noahowpmp%energy%FSR = NoahowpmpIO%FSR(ix,iy)
    noahowpmp%energy%FSRV = NoahowpmpIO%FSRV(ix,iy)
    noahowpmp%energy%FSRG = NoahowpmpIO%FSRG(ix,iy)
    noahowpmp%energy%TAH = NoahowpmpIO%TAH(ix,iy)
    noahowpmp%energy%EAH = NoahowpmpIO%EAH(ix,iy)
    noahowpmp%energy%ZPD = NoahowpmpIO%ZPD(ix,iy)
    noahowpmp%energy%Z0MG = NoahowpmpIO%Z0MG(ix,iy)
    noahowpmp%energy%Z0M = NoahowpmpIO%Z0M(ix,iy)
    noahowpmp%energy%ZLVL = NoahowpmpIO%ZLVL(ix,iy)
    noahowpmp%energy%CMV = NoahowpmpIO%CMV(ix,iy)
    noahowpmp%energy%CMB = NoahowpmpIO%CMB(ix,iy)
    noahowpmp%energy%CM = NoahowpmpIO%CM(ix,iy)
    noahowpmp%energy%CH = NoahowpmpIO%CH(ix,iy)
    noahowpmp%energy%TGB = NoahowpmpIO%TGB(ix,iy)
    noahowpmp%energy%QSFC = NoahowpmpIO%QSFC(ix,iy)
    noahowpmp%energy%EMV = NoahowpmpIO%EMV(ix,iy)
    noahowpmp%energy%EMG = NoahowpmpIO%EMG(ix,iy)
    noahowpmp%energy%GAMMAV = NoahowpmpIO%GAMMAV(ix,iy)
    noahowpmp%energy%GAMMAG = NoahowpmpIO%GAMMAG(ix,iy)
    noahowpmp%energy%EVC = NoahowpmpIO%EVC(ix,iy)
    noahowpmp%energy%IRC = NoahowpmpIO%IRC(ix,iy)
    noahowpmp%energy%IRG = NoahowpmpIO%IRG(ix,iy)
    noahowpmp%energy%SHC = NoahowpmpIO%SHC(ix,iy)
    noahowpmp%energy%SHG = NoahowpmpIO%SHG(ix,iy)
    noahowpmp%energy%SHB = NoahowpmpIO%SHB(ix,iy)
    noahowpmp%energy%EVG = NoahowpmpIO%EVG(ix,iy)
    noahowpmp%energy%EVB = NoahowpmpIO%EVB(ix,iy)
    noahowpmp%energy%TR = NoahowpmpIO%TR(ix,iy)
    noahowpmp%energy%GH = NoahowpmpIO%GH(ix,iy)
    noahowpmp%energy%GHB = NoahowpmpIO%GHB(ix,iy)
    noahowpmp%energy%GHV = NoahowpmpIO%GHV(ix,iy)
    noahowpmp%energy%T2MV = NoahowpmpIO%T2MV(ix,iy)
    noahowpmp%energy%CHLEAF = NoahowpmpIO%CHLEAF(ix,iy)
    noahowpmp%energy%CHUC = NoahowpmpIO%CHUC(ix,iy)
    noahowpmp%energy%CHV2 = NoahowpmpIO%CHV2(ix,iy)
    noahowpmp%energy%CHB2 = NoahowpmpIO%CHB2(ix,iy)
    noahowpmp%energy%Q2V = NoahowpmpIO%Q2V(ix,iy)
    noahowpmp%energy%LATHEAV = NoahowpmpIO%LATHEAV(ix,iy)
    noahowpmp%energy%LATHEAG = NoahowpmpIO%LATHEAG(ix,iy)
    noahowpmp%energy%LATHEA = NoahowpmpIO%LATHEA(ix,iy)
    noahowpmp%energy%RSURF = NoahowpmpIO%RSURF(ix,iy)
    noahowpmp%energy%RHSUR = NoahowpmpIO%RHSUR(ix,iy)
    noahowpmp%energy%TAUXV = NoahowpmpIO%TAUXV(ix,iy)
    noahowpmp%energy%TAUYV = NoahowpmpIO%TAUYV(ix,iy)
    noahowpmp%energy%TAUXB = NoahowpmpIO%TAUXB(ix,iy)
    noahowpmp%energy%TAUYB = NoahowpmpIO%TAUYB(ix,iy)
    noahowpmp%energy%TAUX = NoahowpmpIO%TAUX(ix,iy)
    noahowpmp%energy%TAUY = NoahowpmpIO%TAUY(ix,iy)
    noahowpmp%energy%CAH2 = NoahowpmpIO%CAH2(ix,iy)
    noahowpmp%energy%EHB2 = NoahowpmpIO%EHB2(ix,iy)
    noahowpmp%energy%T2MB = NoahowpmpIO%T2MB(ix,iy)
    noahowpmp%energy%Q2B = NoahowpmpIO%Q2B(ix,iy)
    noahowpmp%energy%TGV = NoahowpmpIO%TGV(ix,iy)
    noahowpmp%energy%CHV = NoahowpmpIO%CHV(ix,iy)
    noahowpmp%energy%RSSUN = NoahowpmpIO%RSSUN(ix,iy)
    noahowpmp%energy%RSSHA = NoahowpmpIO%RSSHA(ix,iy)
    noahowpmp%energy%RB = NoahowpmpIO%RB(ix,iy)
    noahowpmp%energy%FIRA = NoahowpmpIO%FIRA(ix,iy)
    noahowpmp%energy%FSH = NoahowpmpIO%FSH(ix,iy)
    noahowpmp%energy%FGEV = NoahowpmpIO%FGEV(ix,iy)
    noahowpmp%energy%TRAD = NoahowpmpIO%TRAD(ix,iy)
    noahowpmp%energy%IRB = NoahowpmpIO%IRB(ix,iy)
    noahowpmp%energy%SSOIL = NoahowpmpIO%SSOIL(ix,iy)
    noahowpmp%energy%T2M = NoahowpmpIO%T2M(ix,iy)
    noahowpmp%energy%TS = NoahowpmpIO%TS(ix,iy)
    noahowpmp%energy%CHB = NoahowpmpIO%CHB(ix,iy)
    noahowpmp%energy%Q1 = NoahowpmpIO%Q1(ix,iy)
    noahowpmp%energy%Q2E = NoahowpmpIO%Q2E(ix,iy)
    noahowpmp%energy%Z0WRF = NoahowpmpIO%Z0WRF(ix,iy)
    noahowpmp%energy%EMISSI = NoahowpmpIO%EMISSI(ix,iy)
    noahowpmp%energy%PSN = NoahowpmpIO%PSN(ix,iy)
    noahowpmp%energy%PSNSUN = NoahowpmpIO%PSNSUN(ix,iy)
    noahowpmp%energy%PSNSHA = NoahowpmpIO%PSNSHA(ix,iy)
    noahowpmp%energy%APAR = NoahowpmpIO%APAR(ix,iy)
    noahowpmp%energy%QMELT = NoahowpmpIO%QMELT(ix,iy)
    noahowpmp%energy%LH = NoahowpmpIO%LH(ix,iy)
    noahowpmp%energy%TGS = NoahowpmpIO%TGS(ix,iy)
    noahowpmp%energy%ICE = NoahowpmpIO%ICE(ix,iy) 

    end associate

  end subroutine EnergyVarInTransfer

  subroutine EnergyVarOutTransfer(Noahowpmp, NoahowpmpIO)

    implicit none

    type(NoahowpmpIO_type), intent(inout) :: NoahowpmpIO
    type(noahowp_type),     intent(inout) :: Noahowpmp

    associate(ix   => NoahowpmpIO%ix, &
              iy   => NoahowpmpIO%iy)

    NoahowpmpIO%TV(ix,iy) = Noahowpmp%energy%TV
    NoahowpmpIO%TG(ix,iy) = Noahowpmp%energy%TG
    NoahowpmpIO%FCEV(ix,iy) = Noahowpmp%energy%FCEV
    NoahowpmpIO%FCTR(ix,iy) = Noahowpmp%energy%FCTR
    NoahowpmpIO%IGS(ix,iy) = Noahowpmp%energy%IGS
    NoahowpmpIO%FROZEN_CANOPY(ix,iy) = Noahowpmp%energy%FROZEN_CANOPY
    NoahowpmpIO%FROZEN_GROUND(ix,iy) = Noahowpmp%energy%FROZEN_GROUND
    NoahowpmpIO%IMELT(ix,iy,:) = Noahowpmp%energy%IMELT(:)
    NoahowpmpIO%STC(ix,iy,:) = Noahowpmp%energy%STC(:)
    NoahowpmpIO%DF(ix,iy,:) = Noahowpmp%energy%DF(:)
    NoahowpmpIO%HCPCT(ix,iy,:) = Noahowpmp%energy%HCPCT(:)
    NoahowpmpIO%FACT(ix,iy,:) = Noahowpmp%energy%FACT(:)
    NoahowpmpIO%PAHV(ix,iy) = Noahowpmp%energy%PAHV
    NoahowpmpIO%PAHG(ix,iy) = Noahowpmp%energy%PAHG
    NoahowpmpIO%PAHB(ix,iy) = Noahowpmp%energy%PAHB
    NoahowpmpIO%PAH(ix,iy) = Noahowpmp%energy%PAH
    NoahowpmpIO%TAUSS(ix,iy) = Noahowpmp%energy%TAUSS
    NoahowpmpIO%FAGE(ix,iy) = Noahowpmp%energy%FAGE
    NoahowpmpIO%ALB(ix,iy) = Noahowpmp%energy%ALB
    NoahowpmpIO%ALBOLD(ix,iy) = Noahowpmp%energy%ALBOLD
    NoahowpmpIO%ALBD(ix,iy,:) = Noahowpmp%energy%ALBD(:)
    NoahowpmpIO%ALBI(ix,iy,:) = Noahowpmp%energy%ALBI(:)
    NoahowpmpIO%ALBGRD(ix,iy,:) = Noahowpmp%energy%ALBGRD(:)
    NoahowpmpIO%ALBGRI(ix,iy,:) = Noahowpmp%energy%ALBGRI(:)
    NoahowpmpIO%ALBSND(ix,iy,:) = Noahowpmp%energy%ALBSND(:)
    NoahowpmpIO%ALBSNI(ix,iy,:) = Noahowpmp%energy%ALBSNI(:)
    NoahowpmpIO%FABD(ix,iy,:) = Noahowpmp%energy%FABD(:)
    NoahowpmpIO%FABI(ix,iy,:) = Noahowpmp%energy%FABI(:)
    NoahowpmpIO%FTDD(ix,iy,:) = Noahowpmp%energy%FTDD(:)
    NoahowpmpIO%FTDI(ix,iy,:) = Noahowpmp%energy%FTDI(:)
    NoahowpmpIO%FTID(ix,iy,:) = Noahowpmp%energy%FTID(:)
    NoahowpmpIO%FTII(ix,iy,:) = Noahowpmp%energy%FTII(:)
    NoahowpmpIO%FREVD(ix,iy,:) = Noahowpmp%energy%FREVD(:)
    NoahowpmpIO%FREVI(ix,iy,:) = Noahowpmp%energy%FREVI(:)
    NoahowpmpIO%FREGD(ix,iy,:) = Noahowpmp%energy%FREGD(:)
    NoahowpmpIO%FREGI(ix,iy,:) = Noahowpmp%energy%FREGI(:)
    NoahowpmpIO%RHO(ix,iy,:) = Noahowpmp%energy%RHO(:)
    NoahowpmpIO%TAU(ix,iy,:) = Noahowpmp%energy%TAU(:)
    NoahowpmpIO%COSZ(ix,iy) = Noahowpmp%energy%COSZ
    NoahowpmpIO%COSZ_HORIZ(ix,iy) = Noahowpmp%energy%COSZ_HORIZ
    NoahowpmpIO%BGAP(ix,iy) = Noahowpmp%energy%BGAP
    NoahowpmpIO%WGAP(ix,iy) = Noahowpmp%energy%WGAP
    NoahowpmpIO%FSUN(ix,iy) = Noahowpmp%energy%FSUN
    NoahowpmpIO%FSHA(ix,iy) = Noahowpmp%energy%FSHA
    NoahowpmpIO%LAISUN(ix,iy) = Noahowpmp%energy%LAISUN
    NoahowpmpIO%LAISHA(ix,iy) = Noahowpmp%energy%LAISHA
    NoahowpmpIO%PARSUN(ix,iy) = Noahowpmp%energy%PARSUN
    NoahowpmpIO%PARSHA(ix,iy) = Noahowpmp%energy%PARSHA
    NoahowpmpIO%SAV(ix,iy) = Noahowpmp%energy%SAV
    NoahowpmpIO%SAG(ix,iy) = Noahowpmp%energy%SAG
    NoahowpmpIO%FSA(ix,iy) = Noahowpmp%energy%FSA
    NoahowpmpIO%FSR(ix,iy) = Noahowpmp%energy%FSR
    NoahowpmpIO%FSRV(ix,iy) = Noahowpmp%energy%FSRV
    NoahowpmpIO%FSRG(ix,iy) = Noahowpmp%energy%FSRG
    NoahowpmpIO%TAH(ix,iy) = Noahowpmp%energy%TAH
    NoahowpmpIO%EAH(ix,iy) = Noahowpmp%energy%EAH
    NoahowpmpIO%ZPD(ix,iy) = Noahowpmp%energy%ZPD
    NoahowpmpIO%Z0MG(ix,iy) = Noahowpmp%energy%Z0MG
    NoahowpmpIO%Z0M(ix,iy) = Noahowpmp%energy%Z0M
    NoahowpmpIO%ZLVL(ix,iy) = Noahowpmp%energy%ZLVL
    NoahowpmpIO%CMV(ix,iy) = Noahowpmp%energy%CMV
    NoahowpmpIO%CMB(ix,iy) = Noahowpmp%energy%CMB
    NoahowpmpIO%CM(ix,iy) = Noahowpmp%energy%CM
    NoahowpmpIO%CH(ix,iy) = Noahowpmp%energy%CH
    NoahowpmpIO%TGB(ix,iy) = Noahowpmp%energy%TGB
    NoahowpmpIO%QSFC(ix,iy) = Noahowpmp%energy%QSFC
    NoahowpmpIO%EMV(ix,iy) = Noahowpmp%energy%EMV
    NoahowpmpIO%EMG(ix,iy) = Noahowpmp%energy%EMG
    NoahowpmpIO%GAMMAV(ix,iy) = Noahowpmp%energy%GAMMAV
    NoahowpmpIO%GAMMAG(ix,iy) = Noahowpmp%energy%GAMMAG
    NoahowpmpIO%EVC(ix,iy) = Noahowpmp%energy%EVC
    NoahowpmpIO%IRC(ix,iy) = Noahowpmp%energy%IRC
    NoahowpmpIO%IRG(ix,iy) = Noahowpmp%energy%IRG
    NoahowpmpIO%SHC(ix,iy) = Noahowpmp%energy%SHC
    NoahowpmpIO%SHG(ix,iy) = Noahowpmp%energy%SHG
    NoahowpmpIO%SHB(ix,iy) = Noahowpmp%energy%SHB
    NoahowpmpIO%EVG(ix,iy) = Noahowpmp%energy%EVG
    NoahowpmpIO%EVB(ix,iy) = Noahowpmp%energy%EVB
    NoahowpmpIO%TR(ix,iy) = Noahowpmp%energy%TR
    NoahowpmpIO%GH(ix,iy) = Noahowpmp%energy%GH
    NoahowpmpIO%GHB(ix,iy) = Noahowpmp%energy%GHB
    NoahowpmpIO%GHV(ix,iy) = Noahowpmp%energy%GHV
    NoahowpmpIO%T2MV(ix,iy) = Noahowpmp%energy%T2MV
    NoahowpmpIO%CHLEAF(ix,iy) = Noahowpmp%energy%CHLEAF
    NoahowpmpIO%CHUC(ix,iy) = Noahowpmp%energy%CHUC
    NoahowpmpIO%CHV2(ix,iy) = Noahowpmp%energy%CHV2
    NoahowpmpIO%CHB2(ix,iy) = Noahowpmp%energy%CHB2
    NoahowpmpIO%Q2V(ix,iy) = Noahowpmp%energy%Q2V
    NoahowpmpIO%LATHEAV(ix,iy) = Noahowpmp%energy%LATHEAV
    NoahowpmpIO%LATHEAG(ix,iy) = Noahowpmp%energy%LATHEAG
    NoahowpmpIO%LATHEA(ix,iy) = Noahowpmp%energy%LATHEA
    NoahowpmpIO%RSURF(ix,iy) = Noahowpmp%energy%RSURF
    NoahowpmpIO%RHSUR(ix,iy) = Noahowpmp%energy%RHSUR
    NoahowpmpIO%TAUXV(ix,iy) = Noahowpmp%energy%TAUXV
    NoahowpmpIO%TAUYV(ix,iy) = Noahowpmp%energy%TAUYV
    NoahowpmpIO%TAUXB(ix,iy) = Noahowpmp%energy%TAUXB
    NoahowpmpIO%TAUYB(ix,iy) = Noahowpmp%energy%TAUYB
    NoahowpmpIO%TAUX(ix,iy) = Noahowpmp%energy%TAUX
    NoahowpmpIO%TAUY(ix,iy) = Noahowpmp%energy%TAUY
    NoahowpmpIO%CAH2(ix,iy) = Noahowpmp%energy%CAH2
    NoahowpmpIO%EHB2(ix,iy) = Noahowpmp%energy%EHB2
    NoahowpmpIO%T2MB(ix,iy) = Noahowpmp%energy%T2MB
    NoahowpmpIO%Q2B(ix,iy) = Noahowpmp%energy%Q2B
    NoahowpmpIO%TGV(ix,iy) = Noahowpmp%energy%TGV
    NoahowpmpIO%CHV(ix,iy) = Noahowpmp%energy%CHV
    NoahowpmpIO%RSSUN(ix,iy) = Noahowpmp%energy%RSSUN
    NoahowpmpIO%RSSHA(ix,iy) = Noahowpmp%energy%RSSHA
    NoahowpmpIO%RB(ix,iy) = Noahowpmp%energy%RB
    NoahowpmpIO%FIRA(ix,iy) = Noahowpmp%energy%FIRA
    NoahowpmpIO%FSH(ix,iy) = Noahowpmp%energy%FSH
    NoahowpmpIO%FGEV(ix,iy) = Noahowpmp%energy%FGEV
    NoahowpmpIO%TRAD(ix,iy) = Noahowpmp%energy%TRAD
    NoahowpmpIO%IRB(ix,iy) = Noahowpmp%energy%IRB
    NoahowpmpIO%SSOIL(ix,iy) = Noahowpmp%energy%SSOIL
    NoahowpmpIO%T2M(ix,iy) = Noahowpmp%energy%T2M
    NoahowpmpIO%TS(ix,iy) = Noahowpmp%energy%TS
    NoahowpmpIO%CHB(ix,iy) = Noahowpmp%energy%CHB
    NoahowpmpIO%Q1(ix,iy) = Noahowpmp%energy%Q1
    NoahowpmpIO%Q2E(ix,iy) = Noahowpmp%energy%Q2E
    NoahowpmpIO%Z0WRF(ix,iy) = Noahowpmp%energy%Z0WRF
    NoahowpmpIO%EMISSI(ix,iy) = Noahowpmp%energy%EMISSI
    NoahowpmpIO%PSN(ix,iy) = Noahowpmp%energy%PSN
    NoahowpmpIO%PSNSUN(ix,iy) = Noahowpmp%energy%PSNSUN
    NoahowpmpIO%PSNSHA(ix,iy) = Noahowpmp%energy%PSNSHA
    NoahowpmpIO%APAR(ix,iy) = Noahowpmp%energy%APAR
    NoahowpmpIO%QMELT(ix,iy) = Noahowpmp%energy%QMELT
    NoahowpmpIO%LH(ix,iy) = Noahowpmp%energy%LH
    NoahowpmpIO%TGS(ix,iy) = Noahowpmp%energy%TGS
    NoahowpmpIO%ICE(ix,iy) = Noahowpmp%energy%ICE    

    end associate

  end subroutine EnergyVarOutTransfer

end module EnergyVarTransferModule