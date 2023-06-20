module EnergyVarTransferModule

    use NoahowpGridTypeModule
    use NoahowpType

    implicit none
  
  contains
  
  subroutine EnergyVarInTransfer(noahowp, noahowpgrid)

    implicit none

    type(noahowpgrid_type), intent(inout) :: noahowpgrid
    type(noahowp_type),     intent(inout) :: noahowp

    associate(ix   => noahowpgrid%ix, &
              iy   => noahowpgrid%iy)

    noahowp%energy%TV = noahowpgrid%TV(ix,iy)
    noahowp%energy%TG = noahowpgrid%TG(ix,iy)
    noahowp%energy%FCEV = noahowpgrid%FCEV(ix,iy)
    noahowp%energy%FCTR = noahowpgrid%FCTR(ix,iy)
    noahowp%energy%IGS = noahowpgrid%IGS(ix,iy)
    noahowp%energy%FROZEN_CANOPY = noahowpgrid%FROZEN_CANOPY(ix,iy)
    noahowp%energy%FROZEN_GROUND = noahowpgrid%FROZEN_GROUND(ix,iy)
    noahowp%energy%IMELT(:) = noahowpgrid%IMELT(ix,iy,:)
    noahowp%energy%STC(:) = noahowpgrid%STC(ix,iy,:)
    noahowp%energy%DF(:) = noahowpgrid%DF(ix,iy,:)
    noahowp%energy%HCPCT(:) = noahowpgrid%HCPCT(ix,iy,:)
    noahowp%energy%FACT(:) = noahowpgrid%FACT(ix,iy,:)
    noahowp%energy%PAHV = noahowpgrid%PAHV(ix,iy)
    noahowp%energy%PAHG = noahowpgrid%PAHG(ix,iy)
    noahowp%energy%PAHB = noahowpgrid%PAHB(ix,iy)
    noahowp%energy%PAH = noahowpgrid%PAH(ix,iy)
    noahowp%energy%TAUSS = noahowpgrid%TAUSS(ix,iy)
    noahowp%energy%FAGE = noahowpgrid%FAGE(ix,iy)
    noahowp%energy%ALB = noahowpgrid%ALB(ix,iy)
    noahowp%energy%ALBOLD = noahowpgrid%ALBOLD(ix,iy)
    noahowp%energy%ALBD(:) = noahowpgrid%ALBD(ix,iy,:)
    noahowp%energy%ALBI(:) = noahowpgrid%ALBI(ix,iy,:)
    noahowp%energy%ALBGRD(:) = noahowpgrid%ALBGRD(ix,iy,:)
    noahowp%energy%ALBGRI(:) = noahowpgrid%ALBGRI(ix,iy,:)
    noahowp%energy%ALBSND(:) = noahowpgrid%ALBSND(ix,iy,:)
    noahowp%energy%ALBSNI(:) = noahowpgrid%ALBSNI(ix,iy,:)
    noahowp%energy%FABD(:) = noahowpgrid%FABD(ix,iy,:)
    noahowp%energy%FABI(:) = noahowpgrid%FABI(ix,iy,:)
    noahowp%energy%FTDD(:) = noahowpgrid%FTDD(ix,iy,:)
    noahowp%energy%FTDI(:) = noahowpgrid%FTDI(ix,iy,:)
    noahowp%energy%FTID(:) = noahowpgrid%FTID(ix,iy,:)
    noahowp%energy%FTII(:) = noahowpgrid%FTII(ix,iy,:)
    noahowp%energy%FREVD(:) = noahowpgrid%FREVD(ix,iy,:)
    noahowp%energy%FREVI(:) = noahowpgrid%FREVI(ix,iy,:)
    noahowp%energy%FREGD(:) = noahowpgrid%FREGD(ix,iy,:)
    noahowp%energy%FREGI(:) = noahowpgrid%FREGI(ix,iy,:)
    noahowp%energy%RHO(:) = noahowpgrid%RHO(ix,iy,:)
    noahowp%energy%TAU(:) = noahowpgrid%TAU(ix,iy,:)
    noahowp%energy%COSZ = noahowpgrid%COSZ(ix,iy)
    noahowp%energy%COSZ_HORIZ = noahowpgrid%COSZ_HORIZ(ix,iy)
    noahowp%energy%BGAP = noahowpgrid%BGAP(ix,iy)
    noahowp%energy%WGAP = noahowpgrid%WGAP(ix,iy)
    noahowp%energy%FSUN = noahowpgrid%FSUN(ix,iy)
    noahowp%energy%FSHA = noahowpgrid%FSHA(ix,iy)
    noahowp%energy%LAISUN = noahowpgrid%LAISUN(ix,iy)
    noahowp%energy%LAISHA = noahowpgrid%LAISHA(ix,iy)
    noahowp%energy%PARSUN = noahowpgrid%PARSUN(ix,iy)
    noahowp%energy%PARSHA = noahowpgrid%PARSHA(ix,iy)
    noahowp%energy%SAV = noahowpgrid%SAV(ix,iy)
    noahowp%energy%SAG = noahowpgrid%SAG(ix,iy)
    noahowp%energy%FSA = noahowpgrid%FSA(ix,iy)
    noahowp%energy%FSR = noahowpgrid%FSR(ix,iy)
    noahowp%energy%FSRV = noahowpgrid%FSRV(ix,iy)
    noahowp%energy%FSRG = noahowpgrid%FSRG(ix,iy)
    noahowp%energy%TAH = noahowpgrid%TAH(ix,iy)
    noahowp%energy%EAH = noahowpgrid%EAH(ix,iy)
    noahowp%energy%ZPD = noahowpgrid%ZPD(ix,iy)
    noahowp%energy%Z0MG = noahowpgrid%Z0MG(ix,iy)
    noahowp%energy%Z0M = noahowpgrid%Z0M(ix,iy)
    noahowp%energy%ZLVL = noahowpgrid%ZLVL(ix,iy)
    noahowp%energy%CMV = noahowpgrid%CMV(ix,iy)
    noahowp%energy%CMB = noahowpgrid%CMB(ix,iy)
    noahowp%energy%CM = noahowpgrid%CM(ix,iy)
    noahowp%energy%CH = noahowpgrid%CH(ix,iy)
    noahowp%energy%TGB = noahowpgrid%TGB(ix,iy)
    noahowp%energy%QSFC = noahowpgrid%QSFC(ix,iy)
    noahowp%energy%EMV = noahowpgrid%EMV(ix,iy)
    noahowp%energy%EMG = noahowpgrid%EMG(ix,iy)
    noahowp%energy%GAMMAV = noahowpgrid%GAMMAV(ix,iy)
    noahowp%energy%GAMMAG = noahowpgrid%GAMMAG(ix,iy)
    noahowp%energy%EVC = noahowpgrid%EVC(ix,iy)
    noahowp%energy%IRC = noahowpgrid%IRC(ix,iy)
    noahowp%energy%IRG = noahowpgrid%IRG(ix,iy)
    noahowp%energy%SHC = noahowpgrid%SHC(ix,iy)
    noahowp%energy%SHG = noahowpgrid%SHG(ix,iy)
    noahowp%energy%SHB = noahowpgrid%SHB(ix,iy)
    noahowp%energy%EVG = noahowpgrid%EVG(ix,iy)
    noahowp%energy%EVB = noahowpgrid%EVB(ix,iy)
    noahowp%energy%TR = noahowpgrid%TR(ix,iy)
    noahowp%energy%GH = noahowpgrid%GH(ix,iy)
    noahowp%energy%GHB = noahowpgrid%GHB(ix,iy)
    noahowp%energy%GHV = noahowpgrid%GHV(ix,iy)
    noahowp%energy%T2MV = noahowpgrid%T2MV(ix,iy)
    noahowp%energy%CHLEAF = noahowpgrid%CHLEAF(ix,iy)
    noahowp%energy%CHUC = noahowpgrid%CHUC(ix,iy)
    noahowp%energy%CHV2 = noahowpgrid%CHV2(ix,iy)
    noahowp%energy%CHB2 = noahowpgrid%CHB2(ix,iy)
    noahowp%energy%Q2V = noahowpgrid%Q2V(ix,iy)
    noahowp%energy%LATHEAV = noahowpgrid%LATHEAV(ix,iy)
    noahowp%energy%LATHEAG = noahowpgrid%LATHEAG(ix,iy)
    noahowp%energy%LATHEA = noahowpgrid%LATHEA(ix,iy)
    noahowp%energy%RSURF = noahowpgrid%RSURF(ix,iy)
    noahowp%energy%RHSUR = noahowpgrid%RHSUR(ix,iy)
    noahowp%energy%TAUXV = noahowpgrid%TAUXV(ix,iy)
    noahowp%energy%TAUYV = noahowpgrid%TAUYV(ix,iy)
    noahowp%energy%TAUXB = noahowpgrid%TAUXB(ix,iy)
    noahowp%energy%TAUYB = noahowpgrid%TAUYB(ix,iy)
    noahowp%energy%TAUX = noahowpgrid%TAUX(ix,iy)
    noahowp%energy%TAUY = noahowpgrid%TAUY(ix,iy)
    noahowp%energy%CAH2 = noahowpgrid%CAH2(ix,iy)
    noahowp%energy%EHB2 = noahowpgrid%EHB2(ix,iy)
    noahowp%energy%T2MB = noahowpgrid%T2MB(ix,iy)
    noahowp%energy%Q2B = noahowpgrid%Q2B(ix,iy)
    noahowp%energy%TGV = noahowpgrid%TGV(ix,iy)
    noahowp%energy%CHV = noahowpgrid%CHV(ix,iy)
    noahowp%energy%RSSUN = noahowpgrid%RSSUN(ix,iy)
    noahowp%energy%RSSHA = noahowpgrid%RSSHA(ix,iy)
    noahowp%energy%RB = noahowpgrid%RB(ix,iy)
    noahowp%energy%FIRA = noahowpgrid%FIRA(ix,iy)
    noahowp%energy%FSH = noahowpgrid%FSH(ix,iy)
    noahowp%energy%FGEV = noahowpgrid%FGEV(ix,iy)
    noahowp%energy%TRAD = noahowpgrid%TRAD(ix,iy)
    noahowp%energy%IRB = noahowpgrid%IRB(ix,iy)
    noahowp%energy%SSOIL = noahowpgrid%SSOIL(ix,iy)
    noahowp%energy%T2M = noahowpgrid%T2M(ix,iy)
    noahowp%energy%TS = noahowpgrid%TS(ix,iy)
    noahowp%energy%CHB = noahowpgrid%CHB(ix,iy)
    noahowp%energy%Q1 = noahowpgrid%Q1(ix,iy)
    noahowp%energy%Q2E = noahowpgrid%Q2E(ix,iy)
    noahowp%energy%Z0WRF = noahowpgrid%Z0WRF(ix,iy)
    noahowp%energy%EMISSI = noahowpgrid%EMISSI(ix,iy)
    noahowp%energy%PSN = noahowpgrid%PSN(ix,iy)
    noahowp%energy%PSNSUN = noahowpgrid%PSNSUN(ix,iy)
    noahowp%energy%PSNSHA = noahowpgrid%PSNSHA(ix,iy)
    noahowp%energy%APAR = noahowpgrid%APAR(ix,iy)
    noahowp%energy%QMELT = noahowpgrid%QMELT(ix,iy)
    noahowp%energy%LH = noahowpgrid%LH(ix,iy)
    noahowp%energy%TGS = noahowpgrid%TGS(ix,iy)
    noahowp%energy%ICE = noahowpgrid%ICE(ix,iy) 

    end associate

  end subroutine EnergyVarInTransfer

  subroutine EnergyVarOutTransfer(noahowp, noahowpgrid)

    implicit none

    type(noahowpgrid_type), intent(inout) :: noahowpgrid
    type(noahowp_type),     intent(inout) :: noahowp

    associate(ix   => noahowpgrid%ix, &
              iy   => noahowpgrid%iy)

    noahowpgrid%TV(ix,iy) = noahowp%energy%TV
    noahowpgrid%TG(ix,iy) = noahowp%energy%TG
    noahowpgrid%FCEV(ix,iy) = noahowp%energy%FCEV
    noahowpgrid%FCTR(ix,iy) = noahowp%energy%FCTR
    noahowpgrid%IGS(ix,iy) = noahowp%energy%IGS
    noahowpgrid%FROZEN_CANOPY(ix,iy) = noahowp%energy%FROZEN_CANOPY
    noahowpgrid%FROZEN_GROUND(ix,iy) = noahowp%energy%FROZEN_GROUND
    noahowpgrid%IMELT(ix,iy,:) = noahowp%energy%IMELT(:)
    noahowpgrid%STC(ix,iy,:) = noahowp%energy%STC(:)
    noahowpgrid%DF(ix,iy,:) = noahowp%energy%DF(:)
    noahowpgrid%HCPCT(ix,iy,:) = noahowp%energy%HCPCT(:)
    noahowpgrid%FACT(ix,iy,:) = noahowp%energy%FACT(:)
    noahowpgrid%PAHV(ix,iy) = noahowp%energy%PAHV
    noahowpgrid%PAHG(ix,iy) = noahowp%energy%PAHG
    noahowpgrid%PAHB(ix,iy) = noahowp%energy%PAHB
    noahowpgrid%PAH(ix,iy) = noahowp%energy%PAH
    noahowpgrid%TAUSS(ix,iy) = noahowp%energy%TAUSS
    noahowpgrid%FAGE(ix,iy) = noahowp%energy%FAGE
    noahowpgrid%ALB(ix,iy) = noahowp%energy%ALB
    noahowpgrid%ALBOLD(ix,iy) = noahowp%energy%ALBOLD
    noahowpgrid%ALBD(ix,iy,:) = noahowp%energy%ALBD(:)
    noahowpgrid%ALBI(ix,iy,:) = noahowp%energy%ALBI(:)
    noahowpgrid%ALBGRD(ix,iy,:) = noahowp%energy%ALBGRD(:)
    noahowpgrid%ALBGRI(ix,iy,:) = noahowp%energy%ALBGRI(:)
    noahowpgrid%ALBSND(ix,iy,:) = noahowp%energy%ALBSND(:)
    noahowpgrid%ALBSNI(ix,iy,:) = noahowp%energy%ALBSNI(:)
    noahowpgrid%FABD(ix,iy,:) = noahowp%energy%FABD(:)
    noahowpgrid%FABI(ix,iy,:) = noahowp%energy%FABI(:)
    noahowpgrid%FTDD(ix,iy,:) = noahowp%energy%FTDD(:)
    noahowpgrid%FTDI(ix,iy,:) = noahowp%energy%FTDI(:)
    noahowpgrid%FTID(ix,iy,:) = noahowp%energy%FTID(:)
    noahowpgrid%FTII(ix,iy,:) = noahowp%energy%FTII(:)
    noahowpgrid%FREVD(ix,iy,:) = noahowp%energy%FREVD(:)
    noahowpgrid%FREVI(ix,iy,:) = noahowp%energy%FREVI(:)
    noahowpgrid%FREGD(ix,iy,:) = noahowp%energy%FREGD(:)
    noahowpgrid%FREGI(ix,iy,:) = noahowp%energy%FREGI(:)
    noahowpgrid%RHO(ix,iy,:) = noahowp%energy%RHO(:)
    noahowpgrid%TAU(ix,iy,:) = noahowp%energy%TAU(:)
    noahowpgrid%COSZ(ix,iy) = noahowp%energy%COSZ
    noahowpgrid%COSZ_HORIZ(ix,iy) = noahowp%energy%COSZ_HORIZ
    noahowpgrid%BGAP(ix,iy) = noahowp%energy%BGAP
    noahowpgrid%WGAP(ix,iy) = noahowp%energy%WGAP
    noahowpgrid%FSUN(ix,iy) = noahowp%energy%FSUN
    noahowpgrid%FSHA(ix,iy) = noahowp%energy%FSHA
    noahowpgrid%LAISUN(ix,iy) = noahowp%energy%LAISUN
    noahowpgrid%LAISHA(ix,iy) = noahowp%energy%LAISHA
    noahowpgrid%PARSUN(ix,iy) = noahowp%energy%PARSUN
    noahowpgrid%PARSHA(ix,iy) = noahowp%energy%PARSHA
    noahowpgrid%SAV(ix,iy) = noahowp%energy%SAV
    noahowpgrid%SAG(ix,iy) = noahowp%energy%SAG
    noahowpgrid%FSA(ix,iy) = noahowp%energy%FSA
    noahowpgrid%FSR(ix,iy) = noahowp%energy%FSR
    noahowpgrid%FSRV(ix,iy) = noahowp%energy%FSRV
    noahowpgrid%FSRG(ix,iy) = noahowp%energy%FSRG
    noahowpgrid%TAH(ix,iy) = noahowp%energy%TAH
    noahowpgrid%EAH(ix,iy) = noahowp%energy%EAH
    noahowpgrid%ZPD(ix,iy) = noahowp%energy%ZPD
    noahowpgrid%Z0MG(ix,iy) = noahowp%energy%Z0MG
    noahowpgrid%Z0M(ix,iy) = noahowp%energy%Z0M
    noahowpgrid%ZLVL(ix,iy) = noahowp%energy%ZLVL
    noahowpgrid%CMV(ix,iy) = noahowp%energy%CMV
    noahowpgrid%CMB(ix,iy) = noahowp%energy%CMB
    noahowpgrid%CM(ix,iy) = noahowp%energy%CM
    noahowpgrid%CH(ix,iy) = noahowp%energy%CH
    noahowpgrid%TGB(ix,iy) = noahowp%energy%TGB
    noahowpgrid%QSFC(ix,iy) = noahowp%energy%QSFC
    noahowpgrid%EMV(ix,iy) = noahowp%energy%EMV
    noahowpgrid%EMG(ix,iy) = noahowp%energy%EMG
    noahowpgrid%GAMMAV(ix,iy) = noahowp%energy%GAMMAV
    noahowpgrid%GAMMAG(ix,iy) = noahowp%energy%GAMMAG
    noahowpgrid%EVC(ix,iy) = noahowp%energy%EVC
    noahowpgrid%IRC(ix,iy) = noahowp%energy%IRC
    noahowpgrid%IRG(ix,iy) = noahowp%energy%IRG
    noahowpgrid%SHC(ix,iy) = noahowp%energy%SHC
    noahowpgrid%SHG(ix,iy) = noahowp%energy%SHG
    noahowpgrid%SHB(ix,iy) = noahowp%energy%SHB
    noahowpgrid%EVG(ix,iy) = noahowp%energy%EVG
    noahowpgrid%EVB(ix,iy) = noahowp%energy%EVB
    noahowpgrid%TR(ix,iy) = noahowp%energy%TR
    noahowpgrid%GH(ix,iy) = noahowp%energy%GH
    noahowpgrid%GHB(ix,iy) = noahowp%energy%GHB
    noahowpgrid%GHV(ix,iy) = noahowp%energy%GHV
    noahowpgrid%T2MV(ix,iy) = noahowp%energy%T2MV
    noahowpgrid%CHLEAF(ix,iy) = noahowp%energy%CHLEAF
    noahowpgrid%CHUC(ix,iy) = noahowp%energy%CHUC
    noahowpgrid%CHV2(ix,iy) = noahowp%energy%CHV2
    noahowpgrid%CHB2(ix,iy) = noahowp%energy%CHB2
    noahowpgrid%Q2V(ix,iy) = noahowp%energy%Q2V
    noahowpgrid%LATHEAV(ix,iy) = noahowp%energy%LATHEAV
    noahowpgrid%LATHEAG(ix,iy) = noahowp%energy%LATHEAG
    noahowpgrid%LATHEA(ix,iy) = noahowp%energy%LATHEA
    noahowpgrid%RSURF(ix,iy) = noahowp%energy%RSURF
    noahowpgrid%RHSUR(ix,iy) = noahowp%energy%RHSUR
    noahowpgrid%TAUXV(ix,iy) = noahowp%energy%TAUXV
    noahowpgrid%TAUYV(ix,iy) = noahowp%energy%TAUYV
    noahowpgrid%TAUXB(ix,iy) = noahowp%energy%TAUXB
    noahowpgrid%TAUYB(ix,iy) = noahowp%energy%TAUYB
    noahowpgrid%TAUX(ix,iy) = noahowp%energy%TAUX
    noahowpgrid%TAUY(ix,iy) = noahowp%energy%TAUY
    noahowpgrid%CAH2(ix,iy) = noahowp%energy%CAH2
    noahowpgrid%EHB2(ix,iy) = noahowp%energy%EHB2
    noahowpgrid%T2MB(ix,iy) = noahowp%energy%T2MB
    noahowpgrid%Q2B(ix,iy) = noahowp%energy%Q2B
    noahowpgrid%TGV(ix,iy) = noahowp%energy%TGV
    noahowpgrid%CHV(ix,iy) = noahowp%energy%CHV
    noahowpgrid%RSSUN(ix,iy) = noahowp%energy%RSSUN
    noahowpgrid%RSSHA(ix,iy) = noahowp%energy%RSSHA
    noahowpgrid%RB(ix,iy) = noahowp%energy%RB
    noahowpgrid%FIRA(ix,iy) = noahowp%energy%FIRA
    noahowpgrid%FSH(ix,iy) = noahowp%energy%FSH
    noahowpgrid%FGEV(ix,iy) = noahowp%energy%FGEV
    noahowpgrid%TRAD(ix,iy) = noahowp%energy%TRAD
    noahowpgrid%IRB(ix,iy) = noahowp%energy%IRB
    noahowpgrid%SSOIL(ix,iy) = noahowp%energy%SSOIL
    noahowpgrid%T2M(ix,iy) = noahowp%energy%T2M
    noahowpgrid%TS(ix,iy) = noahowp%energy%TS
    noahowpgrid%CHB(ix,iy) = noahowp%energy%CHB
    noahowpgrid%Q1(ix,iy) = noahowp%energy%Q1
    noahowpgrid%Q2E(ix,iy) = noahowp%energy%Q2E
    noahowpgrid%Z0WRF(ix,iy) = noahowp%energy%Z0WRF
    noahowpgrid%EMISSI(ix,iy) = noahowp%energy%EMISSI
    noahowpgrid%PSN(ix,iy) = noahowp%energy%PSN
    noahowpgrid%PSNSUN(ix,iy) = noahowp%energy%PSNSUN
    noahowpgrid%PSNSHA(ix,iy) = noahowp%energy%PSNSHA
    noahowpgrid%APAR(ix,iy) = noahowp%energy%APAR
    noahowpgrid%QMELT(ix,iy) = noahowp%energy%QMELT
    noahowpgrid%LH(ix,iy) = noahowp%energy%LH
    noahowpgrid%TGS(ix,iy) = noahowp%energy%TGS
    noahowpgrid%ICE(ix,iy) = noahowp%energy%ICE    

    end associate

  end subroutine EnergyVarOutTransfer

end module EnergyVarTransferModule