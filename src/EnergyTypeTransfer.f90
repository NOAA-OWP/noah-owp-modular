module EnergyTypeTransfer

    use EnergyType
    use EnergyGridType

    implicit none
  
  contains
  
  subroutine EnergyVarInTransfer(energy, energygrid, ix, iy)

    implicit none

    type(energygrid_type), intent(in)    :: energygrid
    type(energy_type),     intent(inout) :: energy
    integer,               intent(in)    :: ix
    integer,               intent(in)    :: iy

    energy%TV = energygrid%TV(ix,iy)
    energy%TG = energygrid%TG(ix,iy)
    energy%FCEV = energygrid%FCEV(ix,iy)
    energy%FCTR = energygrid%FCTR(ix,iy)
    energy%IGS = energygrid%IGS(ix,iy)
    energy%FROZEN_CANOPY = energygrid%FROZEN_CANOPY(ix,iy)
    energy%FROZEN_GROUND = energygrid%FROZEN_GROUND(ix,iy)
    energy%IMELT(:) = energygrid%IMELT(ix,iy,:)
    energy%STC(:) = energygrid%STC(ix,iy,:)
    energy%DF(:) = energygrid%DF(ix,iy,:)
    energy%HCPCT(:) = energygrid%HCPCT(ix,iy,:)
    energy%FACT(:) = energygrid%FACT(ix,iy,:)
    energy%PAHV = energygrid%PAHV(ix,iy)
    energy%PAHG = energygrid%PAHG(ix,iy)
    energy%PAHB = energygrid%PAHB(ix,iy)
    energy%PAH = energygrid%PAH(ix,iy)
    energy%TAUSS = energygrid%TAUSS(ix,iy)
    energy%FAGE = energygrid%FAGE(ix,iy)
    energy%ALB = energygrid%ALB(ix,iy)
    energy%ALBOLD = energygrid%ALBOLD(ix,iy)
    energy%ALBD(:) = energygrid%ALBD(ix,iy,:)
    energy%ALBI(:) = energygrid%ALBI(ix,iy,:)
    energy%ALBGRD(:) = energygrid%ALBGRD(ix,iy,:)
    energy%ALBGRI(:) = energygrid%ALBGRI(ix,iy,:)
    energy%ALBSND(:) = energygrid%ALBSND(ix,iy,:)
    energy%ALBSNI(:) = energygrid%ALBSNI(ix,iy,:)
    energy%FABD(:) = energygrid%FABD(ix,iy,:)
    energy%FABI(:) = energygrid%FABI(ix,iy,:)
    energy%FTDD(:) = energygrid%FTDD(ix,iy,:)
    energy%FTDI(:) = energygrid%FTDI(ix,iy,:)
    energy%FTID(:) = energygrid%FTID(ix,iy,:)
    energy%FTII(:) = energygrid%FTII(ix,iy,:)
    energy%FREVD(:) = energygrid%FREVD(ix,iy,:)
    energy%FREVI(:) = energygrid%FREVI(ix,iy,:)
    energy%FREGD(:) = energygrid%FREGD(ix,iy,:)
    energy%FREGI(:) = energygrid%FREGI(ix,iy,:)
    energy%RHO(:) = energygrid%RHO(ix,iy,:)
    energy%TAU(:) = energygrid%TAU(ix,iy,:)
    energy%COSZ = energygrid%COSZ(ix,iy)
    energy%COSZ_HORIZ = energygrid%COSZ_HORIZ(ix,iy)
    energy%BGAP = energygrid%BGAP(ix,iy)
    energy%WGAP = energygrid%WGAP(ix,iy)
    energy%FSUN = energygrid%FSUN(ix,iy)
    energy%FSHA = energygrid%FSHA(ix,iy)
    energy%LAISUN = energygrid%LAISUN(ix,iy)
    energy%LAISHA = energygrid%LAISHA(ix,iy)
    energy%PARSUN = energygrid%PARSUN(ix,iy)
    energy%PARSHA = energygrid%PARSHA(ix,iy)
    energy%SAV = energygrid%SAV(ix,iy)
    energy%SAG = energygrid%SAG(ix,iy)
    energy%FSA = energygrid%FSA(ix,iy)
    energy%FSR = energygrid%FSR(ix,iy)
    energy%FSRV = energygrid%FSRV(ix,iy)
    energy%FSRG = energygrid%FSRG(ix,iy)
    energy%TAH = energygrid%TAH(ix,iy)
    energy%EAH = energygrid%EAH(ix,iy)
    energy%ZPD = energygrid%ZPD(ix,iy)
    energy%Z0MG = energygrid%Z0MG(ix,iy)
    energy%Z0M = energygrid%Z0M(ix,iy)
    energy%ZLVL = energygrid%ZLVL(ix,iy)
    energy%CMV = energygrid%CMV(ix,iy)
    energy%CMB = energygrid%CMB(ix,iy)
    energy%CM = energygrid%CM(ix,iy)
    energy%CH = energygrid%CH(ix,iy)
    energy%TGB = energygrid%TGB(ix,iy)
    energy%QSFC = energygrid%QSFC(ix,iy)
    energy%EMV = energygrid%EMV(ix,iy)
    energy%EMG = energygrid%EMG(ix,iy)
    energy%GAMMAV = energygrid%GAMMAV(ix,iy)
    energy%GAMMAG = energygrid%GAMMAG(ix,iy)
    energy%EVC = energygrid%EVC(ix,iy)
    energy%IRC = energygrid%IRC(ix,iy)
    energy%IRG = energygrid%IRG(ix,iy)
    energy%SHC = energygrid%SHC(ix,iy)
    energy%SHG = energygrid%SHG(ix,iy)
    energy%SHB = energygrid%SHB(ix,iy)
    energy%EVG = energygrid%EVG(ix,iy)
    energy%EVB = energygrid%EVB(ix,iy)
    energy%TR = energygrid%TR(ix,iy)
    energy%GH = energygrid%GH(ix,iy)
    energy%GHB = energygrid%GHB(ix,iy)
    energy%GHV = energygrid%GHV(ix,iy)
    energy%T2MV = energygrid%T2MV(ix,iy)
    energy%CHLEAF = energygrid%CHLEAF(ix,iy)
    energy%CHUC = energygrid%CHUC(ix,iy)
    energy%CHV2 = energygrid%CHV2(ix,iy)
    energy%CHB2 = energygrid%CHB2(ix,iy)
    energy%Q2V = energygrid%Q2V(ix,iy)
    energy%LATHEAV = energygrid%LATHEAV(ix,iy)
    energy%LATHEAG = energygrid%LATHEAG(ix,iy)
    energy%LATHEA = energygrid%LATHEA(ix,iy)
    energy%RSURF = energygrid%RSURF(ix,iy)
    energy%RHSUR = energygrid%RHSUR(ix,iy)
    energy%TAUXV = energygrid%TAUXV(ix,iy)
    energy%TAUYV = energygrid%TAUYV(ix,iy)
    energy%TAUXB = energygrid%TAUXB(ix,iy)
    energy%TAUYB = energygrid%TAUYB(ix,iy)
    energy%TAUX = energygrid%TAUX(ix,iy)
    energy%TAUY = energygrid%TAUY(ix,iy)
    energy%CAH2 = energygrid%CAH2(ix,iy)
    energy%EHB2 = energygrid%EHB2(ix,iy)
    energy%T2MB = energygrid%T2MB(ix,iy)
    energy%Q2B = energygrid%Q2B(ix,iy)
    energy%TGV = energygrid%TGV(ix,iy)
    energy%CHV = energygrid%CHV(ix,iy)
    energy%RSSUN = energygrid%RSSUN(ix,iy)
    energy%RSSHA = energygrid%RSSHA(ix,iy)
    energy%RB = energygrid%RB(ix,iy)
    energy%FIRA = energygrid%FIRA(ix,iy)
    energy%FSH = energygrid%FSH(ix,iy)
    energy%FGEV = energygrid%FGEV(ix,iy)
    energy%TRAD = energygrid%TRAD(ix,iy)
    energy%IRB = energygrid%IRB(ix,iy)
    energy%SSOIL = energygrid%SSOIL(ix,iy)
    energy%T2M = energygrid%T2M(ix,iy)
    energy%TS = energygrid%TS(ix,iy)
    energy%CHB = energygrid%CHB(ix,iy)
    energy%Q1 = energygrid%Q1(ix,iy)
    energy%Q2E = energygrid%Q2E(ix,iy)
    energy%Z0WRF = energygrid%Z0WRF(ix,iy)
    energy%EMISSI = energygrid%EMISSI(ix,iy)
    energy%PSN = energygrid%PSN(ix,iy)
    energy%PSNSUN = energygrid%PSNSUN(ix,iy)
    energy%PSNSHA = energygrid%PSNSHA(ix,iy)
    energy%APAR = energygrid%APAR(ix,iy)
    energy%QMELT = energygrid%QMELT(ix,iy)
    energy%LH = energygrid%LH(ix,iy)
    energy%TGS = energygrid%TGS(ix,iy)
    energy%ICE = energygrid%ICE(ix,iy) 

  end subroutine EnergyVarInTransfer

  subroutine EnergyVarOutTransfer(energy, energygrid)

    implicit none

    type(energygrid_type), intent(inout)    :: energygrid
    type(energy_type),     intent(in)       :: energy
    integer,               intent(in)       :: ix
    integer,               intent(in)       :: iy

    energygrid%TV(ix,iy) = energy%TV
    energygrid%TG(ix,iy) = energy%TG
    energygrid%FCEV(ix,iy) = energy%FCEV
    energygrid%FCTR(ix,iy) = energy%FCTR
    energygrid%IGS(ix,iy) = energy%IGS
    energygrid%FROZEN_CANOPY(ix,iy) = energy%FROZEN_CANOPY
    energygrid%FROZEN_GROUND(ix,iy) = energy%FROZEN_GROUND
    energygrid%IMELT(ix,iy,:) = energy%IMELT(:)
    energygrid%STC(ix,iy,:) = energy%STC(:)
    energygrid%DF(ix,iy,:) = energy%DF(:)
    energygrid%HCPCT(ix,iy,:) = energy%HCPCT(:)
    energygrid%FACT(ix,iy,:) = energy%FACT(:)
    energygrid%PAHV(ix,iy) = energy%PAHV
    energygrid%PAHG(ix,iy) = energy%PAHG
    energygrid%PAHB(ix,iy) = energy%PAHB
    energygrid%PAH(ix,iy) = energy%PAH
    energygrid%TAUSS(ix,iy) = energy%TAUSS
    energygrid%FAGE(ix,iy) = energy%FAGE
    energygrid%ALB(ix,iy) = energy%ALB
    energygrid%ALBOLD(ix,iy) = energy%ALBOLD
    energygrid%ALBD(ix,iy,:) = energy%ALBD(:)
    energygrid%ALBI(ix,iy,:) = energy%ALBI(:)
    energygrid%ALBGRD(ix,iy,:) = energy%ALBGRD(:)
    energygrid%ALBGRI(ix,iy,:) = energy%ALBGRI(:)
    energygrid%ALBSND(ix,iy,:) = energy%ALBSND(:)
    energygrid%ALBSNI(ix,iy,:) = energy%ALBSNI(:)
    energygrid%FABD(ix,iy,:) = energy%FABD(:)
    energygrid%FABI(ix,iy,:) = energy%FABI(:)
    energygrid%FTDD(ix,iy,:) = energy%FTDD(:)
    energygrid%FTDI(ix,iy,:) = energy%FTDI(:)
    energygrid%FTID(ix,iy,:) = energy%FTID(:)
    energygrid%FTII(ix,iy,:) = energy%FTII(:)
    energygrid%FREVD(ix,iy,:) = energy%FREVD(:)
    energygrid%FREVI(ix,iy,:) = energy%FREVI(:)
    energygrid%FREGD(ix,iy,:) = energy%FREGD(:)
    energygrid%FREGI(ix,iy,:) = energy%FREGI(:)
    energygrid%RHO(ix,iy,:) = energy%RHO(:)
    energygrid%TAU(ix,iy,:) = energy%TAU(:)
    energygrid%COSZ(ix,iy) = energy%COSZ
    energygrid%COSZ_HORIZ(ix,iy) = energy%COSZ_HORIZ
    energygrid%BGAP(ix,iy) = energy%BGAP
    energygrid%WGAP(ix,iy) = energy%WGAP
    energygrid%FSUN(ix,iy) = energy%FSUN
    energygrid%FSHA(ix,iy) = energy%FSHA
    energygrid%LAISUN(ix,iy) = energy%LAISUN
    energygrid%LAISHA(ix,iy) = energy%LAISHA
    energygrid%PARSUN(ix,iy) = energy%PARSUN
    energygrid%PARSHA(ix,iy) = energy%PARSHA
    energygrid%SAV(ix,iy) = energy%SAV
    energygrid%SAG(ix,iy) = energy%SAG
    energygrid%FSA(ix,iy) = energy%FSA
    energygrid%FSR(ix,iy) = energy%FSR
    energygrid%FSRV(ix,iy) = energy%FSRV
    energygrid%FSRG(ix,iy) = energy%FSRG
    energygrid%TAH(ix,iy) = energy%TAH
    energygrid%EAH(ix,iy) = energy%EAH
    energygrid%ZPD(ix,iy) = energy%ZPD
    energygrid%Z0MG(ix,iy) = energy%Z0MG
    energygrid%Z0M(ix,iy) = energy%Z0M
    energygrid%ZLVL(ix,iy) = energy%ZLVL
    energygrid%CMV(ix,iy) = energy%CMV
    energygrid%CMB(ix,iy) = energy%CMB
    energygrid%CM(ix,iy) = energy%CM
    energygrid%CH(ix,iy) = energy%CH
    energygrid%TGB(ix,iy) = energy%TGB
    energygrid%QSFC(ix,iy) = energy%QSFC
    energygrid%EMV(ix,iy) = energy%EMV
    energygrid%EMG(ix,iy) = energy%EMG
    energygrid%GAMMAV(ix,iy) = energy%GAMMAV
    energygrid%GAMMAG(ix,iy) = energy%GAMMAG
    energygrid%EVC(ix,iy) = energy%EVC
    energygrid%IRC(ix,iy) = energy%IRC
    energygrid%IRG(ix,iy) = energy%IRG
    energygrid%SHC(ix,iy) = energy%SHC
    energygrid%SHG(ix,iy) = energy%SHG
    energygrid%SHB(ix,iy) = energy%SHB
    energygrid%EVG(ix,iy) = energy%EVG
    energygrid%EVB(ix,iy) = energy%EVB
    energygrid%TR(ix,iy) = energy%TR
    energygrid%GH(ix,iy) = energy%GH
    energygrid%GHB(ix,iy) = energy%GHB
    energygrid%GHV(ix,iy) = energy%GHV
    energygrid%T2MV(ix,iy) = energy%T2MV
    energygrid%CHLEAF(ix,iy) = energy%CHLEAF
    energygrid%CHUC(ix,iy) = energy%CHUC
    energygrid%CHV2(ix,iy) = energy%CHV2
    energygrid%CHB2(ix,iy) = energy%CHB2
    energygrid%Q2V(ix,iy) = energy%Q2V
    energygrid%LATHEAV(ix,iy) = energy%LATHEAV
    energygrid%LATHEAG(ix,iy) = energy%LATHEAG
    energygrid%LATHEA(ix,iy) = energy%LATHEA
    energygrid%RSURF(ix,iy) = energy%RSURF
    energygrid%RHSUR(ix,iy) = energy%RHSUR
    energygrid%TAUXV(ix,iy) = energy%TAUXV
    energygrid%TAUYV(ix,iy) = energy%TAUYV
    energygrid%TAUXB(ix,iy) = energy%TAUXB
    energygrid%TAUYB(ix,iy) = energy%TAUYB
    energygrid%TAUX(ix,iy) = energy%TAUX
    energygrid%TAUY(ix,iy) = energy%TAUY
    energygrid%CAH2(ix,iy) = energy%CAH2
    energygrid%EHB2(ix,iy) = energy%EHB2
    energygrid%T2MB(ix,iy) = energy%T2MB
    energygrid%Q2B(ix,iy) = energy%Q2B
    energygrid%TGV(ix,iy) = energy%TGV
    energygrid%CHV(ix,iy) = energy%CHV
    energygrid%RSSUN(ix,iy) = energy%RSSUN
    energygrid%RSSHA(ix,iy) = energy%RSSHA
    energygrid%RB(ix,iy) = energy%RB
    energygrid%FIRA(ix,iy) = energy%FIRA
    energygrid%FSH(ix,iy) = energy%FSH
    energygrid%FGEV(ix,iy) = energy%FGEV
    energygrid%TRAD(ix,iy) = energy%TRAD
    energygrid%IRB(ix,iy) = energy%IRB
    energygrid%SSOIL(ix,iy) = energy%SSOIL
    energygrid%T2M(ix,iy) = energy%T2M
    energygrid%TS(ix,iy) = energy%TS
    energygrid%CHB(ix,iy) = energy%CHB
    energygrid%Q1(ix,iy) = energy%Q1
    energygrid%Q2E(ix,iy) = energy%Q2E
    energygrid%Z0WRF(ix,iy) = energy%Z0WRF
    energygrid%EMISSI(ix,iy) = energy%EMISSI
    energygrid%PSN(ix,iy) = energy%PSN
    energygrid%PSNSUN(ix,iy) = energy%PSNSUN
    energygrid%PSNSHA(ix,iy) = energy%PSNSHA
    energygrid%APAR(ix,iy) = energy%APAR
    energygrid%QMELT(ix,iy) = energy%QMELT
    energygrid%LH(ix,iy) = energy%LH
    energygrid%TGS(ix,iy) = energy%TGS
    energygrid%ICE(ix,iy) = energy%ICE    

  end subroutine EnergyVarOutTransfer

end module EnergyTypeTransfer