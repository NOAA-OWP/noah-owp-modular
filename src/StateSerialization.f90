module StateSerialization

  use DomainType
  use ParametersType
  use WaterType
  use EnergyType
  use ForcingType
  use messagepack
  use iso_fortran_env

  implicit none
    
contains

SUBROUTINE forcing_serialization (forcing, mp_arr)
    type(forcing_type), intent(in) :: forcing
    class(mp_arr_type), allocatable, intent(out) :: mp_arr
    mp_arr = mp_arr_type(22)
    mp_arr%values(1)%obj = mp_float_type(forcing%UU) !UU
    mp_arr%values(2)%obj = mp_float_type(forcing%VV) !VV
    mp_arr%values(3)%obj = mp_float_type(forcing%SFCTMP) !SFCTMP
    mp_arr%values(4)%obj = mp_float_type(forcing%Q2) !Q2
    mp_arr%values(5)%obj = mp_float_type(forcing%SFCPRS) !SFCPRS
    mp_arr%values(6)%obj = mp_float_type(forcing%SOLDN) !SOLDN
    mp_arr%values(7)%obj = mp_float_type(forcing%LWDN) !LWDN
    mp_arr%values(8)%obj = mp_float_type(forcing%JULIAN) !JULIAN, out
    mp_arr%values(9)%obj = mp_float_type(forcing%THAIR) !THAIR, out
    mp_arr%values(10)%obj = mp_float_type(forcing%QAIR) !QAIR, out
    mp_arr%values(11)%obj = mp_float_type(forcing%EAIR) !EAIR, out
    mp_arr%values(12)%obj = mp_float_type(forcing%RHOAIR) !RHOAIR, out
    mp_arr%values(13)%obj = mp_float_type(forcing%O2PP) !O2PP
    mp_arr%values(14)%obj = mp_float_type(forcing%CO2PP) !CO2PP
    mp_arr%values(15)%obj = mp_float_type(forcing%SWDOWN) !SWDOWN, out
    mp_arr%values(16)%obj = mp_float_type(forcing%PRCP) !PRCP
    mp_arr%values(17)%obj = mp_float_type(forcing%PRCPNONC) !PRCPNONC
    mp_arr%values(18)%obj = mp_float_type(forcing%FPICE) !FPICE, out 
    mp_arr%values(19)%obj = mp_float_type(forcing%UR) !UR, out
    mp_arr%values(20)%obj = mp_int_type(forcing%YEARLEN) !YEARLEN, out
    mp_arr%values(21)%obj = transfer_values_to_mp(forcing%SOLAD) !SOLAD
    mp_arr%values(22)%obj = transfer_values_to_mp(forcing%SOLAI) !SOLAI

END SUBROUTINE forcing_serialization

SUBROUTINE forcing_deserialization (mp_arr, forcing)
    class(mp_arr_type), allocatable, intent(in) :: mp_arr
    type(forcing_type), intent(inout) :: forcing
    real(kind=real64) :: deserialized_val
    integer(kind=int64) :: deserialized_int_val
    class(mp_arr_type), allocatable :: mp_sub_arr
    logical :: status
    integer(kind=int64) :: index, sub_index 

    do index=1, mp_arr%numelements()
        if (index .LE. 19) then    
            call get_real(mp_arr%values(index)%obj, deserialized_val, status)
        else if (index == 20) then
            call get_int(mp_arr%values(index)%obj, deserialized_int_val, status)
        else if (index .GE. 21) then
            if (is_arr(mp_arr%values(index)%obj)) then
                call get_arr_ref(mp_arr%values(index)%obj, mp_sub_arr, status)
            end if
        end if
        select case(index)
            case(1)
                forcing%UU = deserialized_val
            case(2)
                forcing%VV = deserialized_val
            case(3)
                forcing%SFCTMP = deserialized_val
            case(4)
                forcing%Q2 = deserialized_val
            case(5)
                forcing%SFCPRS = deserialized_val
            case(6)
                forcing%SOLDN = deserialized_val
            case(7)
                forcing%LWDN = deserialized_val
            case(8)
                forcing%JULIAN = deserialized_val
            case(9)
                forcing%THAIR = deserialized_val
            case(10)
                forcing%QAIR = deserialized_val
            case(11)
                forcing%EAIR = deserialized_val
            case(12)
                forcing%RHOAIR = deserialized_val
            case(13)
                forcing%O2PP = deserialized_val
            case(14)
                forcing%CO2PP = deserialized_val
            case(15)
                forcing%SWDOWN = deserialized_val
            case(16)
                forcing%PRCP = deserialized_val
            case(17)
                forcing%PRCPNONC = deserialized_val
            case(18)
                forcing%FPICE = deserialized_val
            case(19)
                forcing%UR = deserialized_val
            case(20)
                forcing%YEARLEN = deserialized_int_val
            case(21)
                forcing%SOLAD = transfer_values_from_mp(mp_sub_arr)
            case(22)
                forcing%SOLAI = transfer_values_from_mp(mp_sub_arr)
        end select
    end do
END SUBROUTINE forcing_deserialization


SUBROUTINE domain_serialization (domain, mp_arr)
    type(domain_type), intent(in) :: domain
    class(mp_arr_type), allocatable, intent(out) :: mp_arr

    mp_arr = mp_arr_type(6)
    mp_arr%values(1)%obj = mp_float_type(domain%curr_datetime) !curr_datetime
    mp_arr%values(2)%obj = mp_float_type(domain%time_dbl) !time_dbl
    mp_arr%values(3)%obj = mp_int_type(domain%ITIME) !ITIME
    mp_arr%values(4)%obj = mp_str_type(domain%nowdate) !nowdate
    mp_arr%values(5)%obj = transfer_values_to_mp(domain%DZSNSO)
    mp_arr%values(6)%obj = transfer_values_to_mp(domain%ZSNSO)

END SUBROUTINE domain_serialization 

SUBROUTINE domain_deserialization (mp_arr, domain)
    class(mp_arr_type), allocatable, intent(in) :: mp_arr
    type(domain_type), intent(inout) :: domain
    real(kind=real64) :: deserialized_val
    integer(kind=int64) :: deserialized_int_val
    character(:), allocatable :: deserialized_str_val
    class(mp_arr_type), allocatable :: mp_sub_arr
    logical :: status
    integer(kind=int64) :: index, sub_index 

    do index=1, mp_arr%numelements()
        if (index .LE. 2) then
            call get_real(mp_arr%values(index)%obj, deserialized_val, status)
        else if (index == 3) then
            call get_int(mp_arr%values(index)%obj, deserialized_int_val, status)
        else if (index == 4) then
            call get_str(mp_arr%values(index)%obj, deserialized_str_val, status)
        else if (index .GE. 5) then
            if (is_arr(mp_arr%values(index)%obj)) then
                call get_arr_ref(mp_arr%values(index)%obj, mp_sub_arr, status)
            end if
        end if
        select case(index)
            case(1)
                domain%curr_datetime = deserialized_val
            case(2)
                domain%time_dbl = deserialized_val
            case(3)
                domain%ITIME = deserialized_int_val
            case(4)
                domain%nowdate = deserialized_str_val
            case(5)
                domain%DZSNSO = transfer_values_from_mp(mp_sub_arr)
            case(6)
                domain%ZSNSO = transfer_values_from_mp(mp_sub_arr)
        end select
    end do
END SUBROUTINE domain_deserialization

SUBROUTINE energy_serialization (energy, mp_arr)
    type(energy_type), intent(in) :: energy
    class(mp_arr_type), allocatable, intent(out) :: mp_arr

    mp_arr = mp_arr_type(125)
    mp_arr%values(1)%obj = mp_float_type(energy%cosz) !cosz
    mp_arr%values(2)%obj = mp_float_type(energy%cosz_horiz) !cosz_horiz
    mp_arr%values(3)%obj = mp_float_type(energy%TAH) !TAH
    mp_arr%values(4)%obj = mp_float_type(energy%EAH) !EAH
    mp_arr%values(5)%obj = mp_float_type(energy%IGS) !IGS
    mp_arr%values(6)%obj = mp_float_type(energy%TAUXV) !TAUXV
    mp_arr%values(7)%obj = mp_float_type(energy%TAUYV) !TAUYV
    mp_arr%values(8)%obj = mp_float_type(energy%IRC) !IRC
    mp_arr%values(9)%obj = mp_float_type(energy%SHC) !SHC
    mp_arr%values(10)%obj = mp_float_type(energy%IRG) !IRG
    mp_arr%values(11)%obj = mp_float_type(energy%SHG) !SHG
    mp_arr%values(12)%obj = mp_float_type(energy%EVG) !EVG
    mp_arr%values(13)%obj = mp_float_type(energy%EVC) !EVC
    mp_arr%values(14)%obj = mp_float_type(energy%TR) !TR
    mp_arr%values(15)%obj = mp_float_type(energy%GHV) !GHV
    mp_arr%values(16)%obj = mp_float_type(energy%PSNSUN) !PSNSUN
    mp_arr%values(17)%obj = mp_float_type(energy%PSNSHA) !PSNSHA
    mp_arr%values(18)%obj = mp_float_type(energy%T2MV) !T2MV
    mp_arr%values(19)%obj = mp_float_type(energy%Q2V) !Q2V
    mp_arr%values(20)%obj = mp_float_type(energy%CHV) !CHV
    mp_arr%values(21)%obj = mp_float_type(energy%CHLEAF) !CHLEAF
    mp_arr%values(22)%obj = mp_float_type(energy%CHUC) !CHUC
    mp_arr%values(23)%obj = mp_float_type(energy%CHV2) !CHV2
    mp_arr%values(24)%obj = mp_float_type(energy%RB) !RB
    mp_arr%values(25)%obj = mp_float_type(energy%Z0MG) !Z0MG
    mp_arr%values(26)%obj = mp_float_type(energy%Z0M) !Z0M
    mp_arr%values(27)%obj = mp_float_type(energy%ZPD) !ZPD
    mp_arr%values(28)%obj = mp_float_type(energy%ZLVL) !ZLVL
    mp_arr%values(29)%obj = mp_float_type(energy%EMG) !EMG
    mp_arr%values(30)%obj = mp_float_type(energy%RSURF) !RSURF
    mp_arr%values(31)%obj = mp_float_type(energy%RHSUR) !RHSUR
    mp_arr%values(32)%obj = mp_float_type(energy%LATHEAV) !LATHEAV
    mp_arr%values(34)%obj = mp_float_type(energy%GAMMAV) !GAMMAV
    mp_arr%values(35)%obj = mp_float_type(energy%LATHEAG) !LATHEAG
    mp_arr%values(36)%obj = mp_float_type(energy%GAMMAG) !GAMMAG
    mp_arr%values(37)%obj = mp_float_type(energy%TGB) !TGB
    mp_arr%values(38)%obj = mp_float_type(energy%CMB) !CMB
    mp_arr%values(39)%obj = mp_float_type(energy%CHB) !CHB
    mp_arr%values(40)%obj = mp_float_type(energy%Z0WRF) !Z0WRF
    mp_arr%values(41)%obj = mp_float_type(energy%RSSUN) !RSSUN
    mp_arr%values(42)%obj = mp_float_type(energy%T2M) !T2M
    mp_arr%values(43)%obj = mp_float_type(energy%Q1) !Q1
    mp_arr%values(44)%obj = mp_float_type(energy%Q2E) !Q2E
    mp_arr%values(45)%obj = mp_float_type(energy%FGEV) !FGEV
    mp_arr%values(46)%obj = mp_float_type(energy%TS) !TS
    mp_arr%values(47)%obj = mp_float_type(energy%TAUY) !TAUY
    mp_arr%values(48)%obj = mp_float_type(energy%GH) !GH
    mp_arr%values(59)%obj = mp_float_type(energy%SSOIL) !SSOIL
    mp_arr%values(50)%obj = mp_float_type(energy%TGV) !TGV
    mp_arr%values(51)%obj = mp_float_type(energy%FCEV) !FCEV
    mp_arr%values(52)%obj = mp_float_type(energy%CM) !CM
    mp_arr%values(53)%obj = mp_float_type(energy%FIRA) !FIRA
    mp_arr%values(54)%obj = mp_float_type(energy%RSSHA) !RSSHA
    mp_arr%values(55)%obj = mp_float_type(energy%TG) !TG
    mp_arr%values(57)%obj = mp_float_type(energy%CH) !CH
    mp_arr%values(56)%obj = mp_float_type(energy%FCTR) !FCTR
    mp_arr%values(57)%obj = mp_float_type(energy%PAH) !PAH
    mp_arr%values(58)%obj = mp_float_type(energy%TAUX) !TAUX
    mp_arr%values(59)%obj = mp_float_type(energy%FSH) !FSH
    mp_arr%values(60)%obj = mp_float_type(energy%EMISSI) !EMISSI
    mp_arr%values(61)%obj = mp_float_type(energy%TRAD) !TRAD
    mp_arr%values(62)%obj = mp_float_type(energy%APAR) !APAR
    mp_arr%values(63)%obj = mp_float_type(energy%PSN) !PSN
    mp_arr%values(64)%obj = mp_float_type(energy%LH) !LH
    mp_arr%values(65)%obj = mp_float_type(energy%TGS) !TGS
    mp_arr%values(66)%obj = mp_float_type(energy%PAHV) !PAHV
    mp_arr%values(67)%obj = mp_float_type(energy%PAHG) !PAHG
    mp_arr%values(68)%obj = mp_float_type(energy%PAHB) !PAHB
    mp_arr%values(69)%obj = mp_float_type(energy%FSHA) !FSHA
    mp_arr%values(70)%obj = mp_float_type(energy%LAISUN) !LAISUN
    mp_arr%values(71)%obj = mp_float_type(energy%LAISHA) !LAISHA
    mp_arr%values(72)%obj = mp_float_type(energy%BGAP) !BGAP
    mp_arr%values(73)%obj = mp_float_type(energy%WGAP) !WGAP
    mp_arr%values(74)%obj = mp_float_type(energy%FSUN) !FSUN
    mp_arr%values(75)%obj = mp_float_type(energy%TAUSS) !TAUSS
    mp_arr%values(76)%obj = mp_float_type(energy%FAGE) !FAGE
    mp_arr%values(77)%obj = mp_float_type(energy%ALB) !ALB
    mp_arr%values(78)%obj = mp_float_type(energy%ALBOLD) !ALBOLD
    mp_arr%values(79)%obj = mp_float_type(energy%SAG) !SAG
    mp_arr%values(80)%obj = mp_float_type(energy%SAV) !SAV
    mp_arr%values(81)%obj = mp_float_type(energy%FSA) !FSA
    mp_arr%values(82)%obj = mp_float_type(energy%PARSUN) !PARSUN
    mp_arr%values(83)%obj = mp_float_type(energy%PARSHA) !PARSHA
    mp_arr%values(84)%obj = mp_float_type(energy%FSR) !FSR
    mp_arr%values(85)%obj = mp_float_type(energy%FSRV) !FSRV
    mp_arr%values(86)%obj = mp_float_type(energy%FSRG) !FSRG
    mp_arr%values(87)%obj = mp_float_type(energy%QSFC) !QSFC
    mp_arr%values(88)%obj = mp_float_type(energy%TV) !TV
    mp_arr%values(89)%obj = mp_float_type(energy%CAH2) !CAH2
    mp_arr%values(90)%obj = mp_float_type(energy%IRB) !IRB
    mp_arr%values(91)%obj = mp_float_type(energy%SHB) !SHB
    mp_arr%values(92)%obj = mp_float_type(energy%EVB) !EVB
    mp_arr%values(93)%obj = mp_float_type(energy%GHB) !GHB
    mp_arr%values(94)%obj = mp_float_type(energy%TAUXB) !TAUXB
    mp_arr%values(95)%obj = mp_float_type(energy%TAUYB) !TAUYB
    mp_arr%values(96)%obj = mp_float_type(energy%EHB2) !EHB2
    mp_arr%values(97)%obj = mp_float_type(energy%T2MB) !T2MB
    mp_arr%values(98)%obj = mp_float_type(energy%Q2B) !Q2B
    mp_arr%values(99)%obj = mp_float_type(energy%QMELT) !QMELT
    mp_arr%values(100)%obj = mp_float_type(energy%SNOWT_AVG) !SNOWT_AVG ,  could be realMissing
    mp_arr%values(101)%obj = mp_bool_type(energy%frozen_ground) !frozen_ground
    mp_arr%values(102)%obj = mp_bool_type(energy%frozen_canopy) !frozen_canopy
    mp_arr%values(103)%obj = transfer_values_to_mp(energy%FTDI) !FTDI array  (1:2)
    mp_arr%values(104)%obj = transfer_values_to_mp(energy%FREVD) !FREVD array  (1:2)
    mp_arr%values(105)%obj = transfer_values_to_mp(energy%FREGD) !FREGD array  (1:2)
    mp_arr%values(106)%obj = transfer_values_to_mp(energy%FREVI) !FREVI array  (1:2)
    mp_arr%values(107)%obj = transfer_values_to_mp(energy%FREGI) !FREGI array  (1:2)
    mp_arr%values(108)%obj = transfer_values_to_mp(energy%STC) !STC array
    mp_arr%values(109)%obj = transfer_values_to_mp(energy%HCPCT ) !HCPCT array  (-levels%NSNOW+1:levels%NSOIL)
    mp_arr%values(110)%obj = transfer_values_to_mp(energy%DF ) !DF array  (-levels%NSNOW+1:levels%NSOIL)
    mp_arr%values(111)%obj = transfer_values_to_mp(energy%FACT ) !FACT array  (-levels%NSNOW+1:levels%NSOIL)
    mp_arr%values(112)%obj = transfer_values_to_mp(energy%ALBD)  !ALBD array  (1:parameters%NBAND)
    mp_arr%values(113)%obj = transfer_values_to_mp(energy%ALBI) !ALBI array  (1:parameters%NBAND)
    mp_arr%values(114)%obj = transfer_values_to_mp(energy%ALBGRD) !ALBGRD array  (1:parameters%NBAND)
    mp_arr%values(115)%obj = transfer_values_to_mp(energy%ALBGRI) !ALBGRI array  (1:parameters%NBAND)
    mp_arr%values(116)%obj = transfer_values_to_mp(energy%ALBSND) !ALBSND array  (1:parameters%NBAND)
    mp_arr%values(117)%obj = transfer_values_to_mp(energy%ALBSNI) !ALBSNI array  (1:parameters%NBAND)
    mp_arr%values(118)%obj = transfer_values_to_mp(energy%FABD) !FABD array  (1:parameters%NBAND)
    mp_arr%values(119)%obj = transfer_values_to_mp(energy%FABI) !FABI array  (1:parameters%NBAND)
    mp_arr%values(120)%obj = transfer_values_to_mp(energy%FTDD) !FTDD array  (1:parameters%NBAND)
    mp_arr%values(121)%obj = transfer_values_to_mp(energy%FTID) !FTID array  (1:parameters%NBAND)
    mp_arr%values(122)%obj = transfer_values_to_mp(energy%FTII) !FTII array  (1:parameters%NBAND)
    mp_arr%values(123)%obj = transfer_values_to_mp(energy%RHO) !RHO array  (1:parameters%NBAND)
    mp_arr%values(124)%obj = transfer_values_to_mp(energy%TAU) !TAU array  (1:parameters%NBAND)
    mp_arr%values(125)%obj = transfer_values_to_mp_int(energy%IMELT) !IMELT array  (-levels%NSNOW+1:levels%NSOIL)

END SUBROUTINE energy_serialization

SUBROUTINE energy_deserialization (mp_arr, energy)
    class(mp_arr_type), allocatable, intent(in) :: mp_arr
    type(energy_type), intent(inout) :: energy
    real(kind=real64) :: deserialized_val
    integer(kind=int64) :: deserialized_int_val
    class(mp_arr_type), allocatable :: mp_sub_arr
    logical :: status, is_true_val
    integer(kind=int64) :: index, sub_index 

    do index=1, mp_arr%numelements()
        if (index .LE. 100) then     
            call get_real(mp_arr%values(index)%obj, deserialized_val, status)
        else if (index == 101 .OR. index == 102) then
            call get_bool(mp_arr%values(index)%obj, is_true_val, status)
        else if ((index .GE. 103)) then
            if (is_arr(mp_arr%values(index)%obj)) then
                call get_arr_ref(mp_arr%values(index)%obj, mp_sub_arr, status)
            end if
        end if
        select case(index)
            case(1)
                energy%cosz = deserialized_val
            case(2)
                energy%cosz_horiz = deserialized_val
            case(3)
                energy%TAH = deserialized_val
            case(4)
                energy%EAH = deserialized_val
            case(5)
                energy%IGS = deserialized_val
            case(6)
                energy%TAUXV = deserialized_val
            case(7)
                energy%TAUYV = deserialized_val
            case(8)
                energy%IRC = deserialized_val
            case(9)
                energy%SHC = deserialized_val
            case(10)
                energy%IRG = deserialized_val
            case(11)
                energy%SHG = deserialized_val
            case(12)
                energy%EVG = deserialized_val
            case(13)
                energy%EVC = deserialized_val
            case(14)
                energy%TR = deserialized_val
            case(15)
                energy%PSNSUN = deserialized_val
            case(17)
                energy%PSNSHA = deserialized_val
            case(18)
                energy%T2MV = deserialized_val
            case(19)
                energy%Q2V = deserialized_val
            case(20)
                energy%CHV = deserialized_val
            case(21)
                energy%CHLEAF = deserialized_val
            case(22)
                energy%CHUC = deserialized_val
            case(23)
                energy%CHV2 = deserialized_val
            case(24)
                energy%RB = deserialized_val
            case(25)
                energy%Z0MG = deserialized_val
            case(26)
                energy%Z0M = deserialized_val
            case(27)
                energy%ZPD = deserialized_val
            case(28)
                energy%ZLVL = deserialized_val
            case(29)
                energy%EMG = deserialized_val
            case(30)
                energy%RSURF = deserialized_val
            case(31)
                energy%RHSUR = deserialized_val
            case(32)
                energy%LATHEAV = deserialized_val
            case(33)
                energy%GAMMAV = deserialized_val
            case(34)
                energy%LATHEAG = deserialized_val
            case(35)
                energy%GAMMAG = deserialized_val
            case(36)
                energy%TGB = deserialized_val
            case(37)
                energy%CMB = deserialized_val
            case(38)
                energy%CHB = deserialized_val
            case(39)
                energy%Z0WRF = deserialized_val
            case(40)
                energy%RSSUN = deserialized_val
            case(41)
                energy%T2M = deserialized_val
            case(42)
                energy%Q1 = deserialized_val
            case(43)
                energy%Q2E = deserialized_val
            case(44)
                energy%FGEV = deserialized_val
            case(45)
                energy%TS = deserialized_val
            case(46)
                energy%TAUY = deserialized_val
            case(47)
                energy%GH = deserialized_val
            case(48)
                energy%SSOIL = deserialized_val
            case(49)
                energy%TGV = deserialized_val
            case(50)
                energy%FCEV = deserialized_val
            case(51)
                energy%CM = deserialized_val
            case(52)
                energy%FIRA = deserialized_val
            case(53)
                energy%RSSHA = deserialized_val
            case(54)
                energy%TG = deserialized_val
            case(55)
                energy%CH = deserialized_val
            case(56)
                energy%FCTR = deserialized_val
            case(57)
                energy%PAH = deserialized_val
            case(58)
                energy%TAUX = deserialized_val
            case(59)
                energy%FSH = deserialized_val
            case(60)
                energy%EMISSI = deserialized_val
            case(61)
                energy%TRAD = deserialized_val
            case(62)
                energy%APAR = deserialized_val
            case(63)
                energy%PSN = deserialized_val
            case(64)
                energy%LH = deserialized_val
            case(65)
                energy%TGS = deserialized_val
            case(66)
                energy%PAHV = deserialized_val
            case(67)
                energy%PAHV = deserialized_val
            case(68)
                energy%PAHB = deserialized_val
            case(69)
                energy%FSHA = deserialized_val
            case(70)
                energy%LAISUN = deserialized_val
            case(71)
                energy%LAISHA = deserialized_val
            case(72)
                energy%BGAP = deserialized_val
            case(73)
                energy%WGAP = deserialized_val
            case(74)
                energy%FSUN = deserialized_val
            case(75)
                energy%TAUSS = deserialized_val
            case(76)
                energy%FAGE = deserialized_val
            case(77)
                energy%ALB = deserialized_val
            case(78)
                energy%ALBOLD = deserialized_val
            case(79)
                energy%SAG = deserialized_val
            case(80)
                energy%SAV = deserialized_val
            case(81)
                energy%FSA = deserialized_val
            case(82)
                energy%PARSUN = deserialized_val
            case(83)
                energy%PARSHA = deserialized_val
            case(84)
                energy%FSR = deserialized_val
            case(85)
                energy%FSRV = deserialized_val
            case(86)
                energy%FSRG = deserialized_val
            case(87)
                energy%QSFC = deserialized_val
            case(88)
                energy%TV = deserialized_val
            case(89)
                energy%CAH2 = deserialized_val
            case(90)
                energy%IRB = deserialized_val
            case(91)
                energy%SHB = deserialized_val
            case(92)
                energy%EVB = deserialized_val
            case(93)
                energy%GHB = deserialized_val
            case(94)
                energy%TAUXB = deserialized_val
            case(95)
                energy%TAUYB = deserialized_val
            case(96)
                energy%EHB2 = deserialized_val
            case(97)
                energy%T2MB = deserialized_val
            case(98)
                energy%Q2B = deserialized_val
            case(99)
                energy%QMELT = deserialized_val
            case(100)
                energy%SNOWT_AVG = deserialized_val
            case(101)
                energy%frozen_canopy = is_true_val
            case(102)
                energy%frozen_ground = is_true_val
            case(103)
                energy%FTDI = transfer_values_from_mp(mp_sub_arr)
            case(104)
                energy%FREVD = transfer_values_from_mp(mp_sub_arr)
            case(105)
                energy%FREGD = transfer_values_from_mp(mp_sub_arr)
            case(106)
                energy%FREVI = transfer_values_from_mp(mp_sub_arr)
            case(107)
                energy%FREGI = transfer_values_from_mp(mp_sub_arr)
            case(108)
                energy%STC = transfer_values_from_mp(mp_sub_arr)
            case(109)
                energy%HCPCT = transfer_values_from_mp(mp_sub_arr)
            case(110)
                energy%DF = transfer_values_from_mp(mp_sub_arr)
            case(111)
                energy%FACT = transfer_values_from_mp(mp_sub_arr)
            case(112)
                energy%ALBD = transfer_values_from_mp(mp_sub_arr)
            case(113)
                energy%ALBI = transfer_values_from_mp(mp_sub_arr)
            case(114)
                energy%ALBGRD = transfer_values_from_mp(mp_sub_arr)
            case(115)
                energy%ALBGRI = transfer_values_from_mp(mp_sub_arr)
            case(116)
                energy%ALBSND = transfer_values_from_mp(mp_sub_arr)
            case(117)
                energy%ALBSNI = transfer_values_from_mp(mp_sub_arr)
            case(118)
                energy%FABD = transfer_values_from_mp(mp_sub_arr)
            case(119)
                energy%FABI = transfer_values_from_mp(mp_sub_arr)
            case(120)
                energy%FTDD = transfer_values_from_mp(mp_sub_arr)
            case(121)
                energy%FTID = transfer_values_from_mp(mp_sub_arr)
            case(122)
                energy%FTII = transfer_values_from_mp(mp_sub_arr)
            case(123)
                energy%RHO = transfer_values_from_mp(mp_sub_arr)
            case(124)
                energy%TAU = transfer_values_from_mp(mp_sub_arr)
            case(125)
                energy%IMELT = transfer_values_from_mp_int(mp_sub_arr)
        end select
    end do   
END SUBROUTINE energy_deserialization
            

SUBROUTINE water_serialization (water, mp_arr)
    type(water_type), intent(in) :: water
    class(mp_arr_type), allocatable, intent(out) :: mp_arr

    mp_arr = mp_arr_type(64)
    mp_arr%values(1)%obj = mp_float_type(water%FP) !FP
    mp_arr%values(2)%obj = mp_float_type(water%RAIN) !RAIN
    mp_arr%values(3)%obj = mp_float_type(water%SNOW) !SNOW
    mp_arr%values(4)%obj = mp_float_type(water%BDFALL) !BDFALL
    mp_arr%values(5)%obj = mp_float_type(water%QINTR) !QINTR
    mp_arr%values(6)%obj = mp_float_type(water%QDRIPR) !QDRIPR
    mp_arr%values(7)%obj = mp_float_type(water%QTHROR) !QTHROR
    mp_arr%values(8)%obj = mp_float_type(water%QINTS) !QINTS
    mp_arr%values(9)%obj = mp_float_type(water%QDRIPS) !QDRIPS
    mp_arr%values(10)%obj = mp_float_type(water%QTHROS) !QTHROS
    mp_arr%values(11)%obj = mp_float_type(water%QRAIN) !QRAIN
    mp_arr%values(12)%obj = mp_float_type(water%QSNOW) !QSNOW
    mp_arr%values(13)%obj = mp_float_type(water%SNOWHIN) !SNOWHIN
    mp_arr%values(14)%obj = mp_float_type(water%CANLIQ) !CANLIQ
    mp_arr%values(15)%obj = mp_float_type(water%CANICE) !CANICE
    mp_arr%values(16)%obj = mp_float_type(water%FWET) !FWET
    mp_arr%values(17)%obj = mp_float_type(water%CMC) !CMC
    mp_arr%values(18)%obj = mp_float_type(water%FSNO) !FSNO
    mp_arr%values(19)%obj = mp_float_type(water%BDSNO) !BDSNO
    mp_arr%values(20)%obj = mp_float_type(water%BTRAN) !BTRAN
    mp_arr%values(21)%obj = mp_float_type(water%SNEQV) !SNEQV
    mp_arr%values(22)%obj = mp_float_type(water%SNOWH) !SNOWH
    mp_arr%values(23)%obj = mp_float_type(water%PONDING) !PONDING
    mp_arr%values(24)%obj = mp_float_type(water%SNEQVO) !SNEQVO
    mp_arr%values(25)%obj = mp_float_type(water%QVAP) !QVAP
    mp_arr%values(26)%obj = mp_float_type(water%QDEW) !QDEW
    mp_arr%values(27)%obj = mp_float_type(water%QSNSUB) !QSNSUB
    mp_arr%values(28)%obj = mp_float_type(water%QSEVA) !QSEVA
    mp_arr%values(29)%obj = mp_float_type(water%QSNFRO) !QSNFRO
    mp_arr%values(30)%obj = mp_float_type(water%QSDEW) !QSDEW
    mp_arr%values(31)%obj = mp_float_type(water%QINSUR) !QINSUR
    mp_arr%values(32)%obj = mp_float_type(water%ACSNOM) !ACSNOM
    mp_arr%values(33)%obj = mp_float_type(water%RUNSRF) !RUNSRF
    mp_arr%values(34)%obj = mp_float_type(water%WSLAKE) !WSLAKE
    mp_arr%values(35)%obj = mp_float_type(water%EVAPOTRANS) !EVAPOTRANS
    mp_arr%values(36)%obj = mp_float_type(water%ECAN) !ECAN
    mp_arr%values(37)%obj = mp_float_type(water%ETRAN) !ETRAN
    mp_arr%values(38)%obj = mp_float_type(water%SNOFLOW) !SNOFLOW
    mp_arr%values(39)%obj = mp_float_type(water%PONDING1) !PONDING1
    mp_arr%values(40)%obj = mp_float_type(water%PONDING2) !PONDING2
    mp_arr%values(41)%obj = mp_float_type(water%QSNBOT) !QSNBOT
    mp_arr%values(42)%obj = mp_float_type(water%RUNSUB) !RUNSUB
    mp_arr%values(43)%obj = mp_float_type(water%PDDUM) !PDDUM
    mp_arr%values(44)%obj = mp_float_type(water%runsrf_dt) !runsrf_dt
    mp_arr%values(45)%obj = mp_float_type(water%SICEMAX) !SICEMAX
    mp_arr%values(46)%obj = mp_float_type(water%FCRMAX) !FCRMAX
    mp_arr%values(47)%obj = mp_float_type(water%FACC) !FACC
    mp_arr%values(48)%obj = mp_float_type(water%QDRAIN) !QDRAIN
    mp_arr%values(49)%obj = mp_float_type(water%DEEPRECH) !DEEPRECH
    mp_arr%values(50)%obj = mp_float_type(water%ZWT) !ZWT
    mp_arr%values(51)%obj = mp_float_type(water%ASAT) !ASAT
    mp_arr%values(52)%obj = mp_float_type(water%SMCWTD) !SMCWTD
    mp_arr%values(53)%obj = mp_int_type(water%ISNOW) !ISNOW integer
    mp_arr%values(54)%obj = transfer_values_to_mp(water%BTRANI ) !BTRANI array (1:levels%NSOIL)
    mp_arr%values(55)%obj = transfer_values_to_mp(water%SNICEV ) !SNICEV array (-levels%NSNOW+1:0) # negative indexes
    mp_arr%values(56)%obj = transfer_values_to_mp(water%EPORE ) !EPORE array (-levels%NSNOW+1:0) # negative indexes
    mp_arr%values(57)%obj = transfer_values_to_mp(water%SNLIQV ) !SNLIQV array (-levels%NSNOW+1:0) # negative indexes
    mp_arr%values(58)%obj = transfer_values_to_mp(water%SICE ) !SICE array (1:levels%NSOIL)
    mp_arr%values(59)%obj = transfer_values_to_mp(water%SH2O ) !SH2O array (1:levels%NSOIL)
    mp_arr%values(60)%obj = transfer_values_to_mp(water%SMC ) !SMC array (1:levels%NSOIL)
    mp_arr%values(61)%obj = transfer_values_to_mp(water%SNICE ) !SNICE array (-levels%NSNOW+1:0)
    mp_arr%values(62)%obj = transfer_values_to_mp(water%SNLIQ ) !SNLIQ array (-levels%NSNOW+1:0)
    mp_arr%values(63)%obj = transfer_values_to_mp(water%ETRANI ) !ETRANI array (1:levels%NSOIL)
    mp_arr%values(64)%obj = transfer_values_to_mp(water%FCR ) !FCR array (1:levels%nsoil)

END SUBROUTINE water_serialization

SUBROUTINE water_deserialization (mp_arr, water)
    class(mp_arr_type), allocatable, intent(in) :: mp_arr
    type(water_type), intent(inout) :: water
    real(kind=real64) :: deserialized_val
    integer(kind=int64) :: deserialized_int_val
    class(mp_arr_type), allocatable :: mp_sub_arr
    logical :: status
    integer(kind=int64) :: index, sub_index 

    do index=1, mp_arr%numelements()
        if (index .LE. 52) then    
            call get_real(mp_arr%values(index)%obj, deserialized_val, status)
        else if (index == 53) then
            call get_int(mp_arr%values(index)%obj, deserialized_int_val, status)
        else if (index .GE. 54) then
            if (is_arr(mp_arr%values(index)%obj)) then
                call get_arr_ref(mp_arr%values(index)%obj, mp_sub_arr, status)
            end if
        end if
        select case(index)
            case(1)
                water%FP = deserialized_val
            case(2)
                water%RAIN = deserialized_val
            case(3)
                water%SNOW = deserialized_val
            case(4)
                water%BDFALL = deserialized_val
            case(5)
                water%QINTR = deserialized_val
            case(6)
                water%QDRIPR = deserialized_val
            case(7)
                water%QTHROR = deserialized_val
            case(8)
                water%QINTS = deserialized_val
            case(9)
                water%QDRIPS = deserialized_val
            case(10)
                water%QTHROS = deserialized_val
            case(11)
                water%QRAIN = deserialized_val
            case(12)
                water%QSNOW = deserialized_val
            case(13)
                water%SNOWHIN = deserialized_val
            case(14)
                water%CANLIQ = deserialized_val
            case(15)
                water%CANICE = deserialized_val
            case(16)
                water%FWET = deserialized_val
            case(17)
                water%CMC = deserialized_val
            case(18)
                water%FSNO = deserialized_val
            case(19)
                water%BDSNO = deserialized_val
            case(20)
                water%BTRAN = deserialized_val
            case(21)
                water%SNEQV = deserialized_val
            case(22)
                water%SNOWH = deserialized_val
            case(23)
                water%PONDING = deserialized_val
            case(24)
                water%SNEQVO = deserialized_val
            case(25)
                water%QVAP = deserialized_val
            case(26)
                water%QDEW = deserialized_val
            case(27)
                water%QSNSUB = deserialized_val
            case(28)
                water%QSEVA = deserialized_val
            case(29)
                water%QSNFRO = deserialized_val
            case(30)
                water%QSDEW = deserialized_val
            case(31)
                water%QINSUR = deserialized_val
            case(32)
                water%ACSNOM = deserialized_val
            case(33)
                water%RUNSRF = deserialized_val
            case(34)
                water%WSLAKE = deserialized_val
            case(35)
                water%EVAPOTRANS = deserialized_val
            case(36)
                water%ECAN = deserialized_val
            case(37)
                water%ETRAN = deserialized_val
            case(38)
                water%SNOFLOW = deserialized_val
            case(39)
                water%PONDING1 = deserialized_val
            case(40)
                water%PONDING2 = deserialized_val
            case(41)
                water%QSNBOT = deserialized_val
            case(42)
                water%RUNSUB = deserialized_val
            case(43)
                water%PDDUM = deserialized_val
            case(44)
                water%runsrf_dt = deserialized_val
            case(45)
                water%SICEMAX = deserialized_val
            case(46)
                water%FCRMAX = deserialized_val
            case(47)
                water%FACC = deserialized_val
            case(48)
                water%QDRAIN = deserialized_val
            case(49)
                water%DEEPRECH = deserialized_val
            case(50)
                water%ZWT = deserialized_val
            case(51)
                water%ASAT = deserialized_val
            case(52)
                water%SMCWTD = deserialized_val
            case(53)
                water%ISNOW = deserialized_int_val
            case(54)
                water%BTRANI = transfer_values_from_mp(mp_sub_arr)
            case(55)
                water%SNICEV = transfer_values_from_mp(mp_sub_arr)
            case(56)
                water%EPORE = transfer_values_from_mp(mp_sub_arr)
            case(57)
                water%SNLIQV = transfer_values_from_mp(mp_sub_arr)
            case(58)
                water%SICE = transfer_values_from_mp(mp_sub_arr)
            case(59)
                water%SH2O = transfer_values_from_mp(mp_sub_arr)
            case(60)
                water%SMC = transfer_values_from_mp(mp_sub_arr)
            case(61)
                water%SNICE = transfer_values_from_mp(mp_sub_arr)
            case(62)
                water%SNLIQ = transfer_values_from_mp(mp_sub_arr)
            case(63)
                water%ETRANI = transfer_values_from_mp(mp_sub_arr)
            case(64)
                water%FCR = transfer_values_from_mp(mp_sub_arr)
        end select
    end do
END SUBROUTINE water_deserialization

SUBROUTINE parameters_serialization (parameters, mp_arr)
    type(parameters_type), intent(in) :: parameters
    class(mp_arr_type), allocatable, intent(out) :: mp_arr

    mp_arr = mp_arr_type(5)
    mp_arr%values(1)%obj = mp_float_type(parameters%SAI) !SAI
    mp_arr%values(2)%obj = mp_float_type(parameters%LAI) !LAI
    mp_arr%values(3)%obj = mp_float_type(parameters%ESAI) !ESAI
    mp_arr%values(4)%obj = mp_float_type(parameters%ELAI) !ELAI
    mp_arr%values(4)%obj = mp_float_type(parameters%FVEG) !FVEG

END SUBROUTINE parameters_serialization

SUBROUTINE parameters_deserialization (mp_arr, parameters)
    class(mp_arr_type), allocatable, intent(in) :: mp_arr
    type(parameters_type), intent(inout) :: parameters
    real(kind=real64) :: deserialized_val
    logical :: status
    integer(kind=int64) :: index

    do index=1, mp_arr%numelements()
        call get_real(mp_arr%values(index)%obj, deserialized_val, status)
        select case(index)
            case(1)
                parameters%SAI = deserialized_val
            case(2)
                parameters%LAI = deserialized_val
            case(3)
                parameters%ESAI = deserialized_val
            case(4)
                parameters%ELAI = deserialized_val
            case(5)
                parameters%FVEG = deserialized_val
        end select
    end do
END SUBROUTINE parameters_deserialization

FUNCTION transfer_values_to_mp (src) RESULT (dest)

real, allocatable, dimension(:), intent(in) :: src
class(mp_arr_type), allocatable :: dest
integer(kind=int64) :: index

    do index=LBOUND(src,1), UBOUND(src,1)
        dest%values(index)%obj = mp_float_type(src(index))
    end do

END FUNCTION transfer_values_to_mp

FUNCTION transfer_values_to_mp_int (src) RESULT (dest)

integer, allocatable, dimension(:), intent(in) :: src
class(mp_arr_type), allocatable :: dest
integer(kind=int64) :: index

    do index=LBOUND(src,1), UBOUND(src,1)
        dest%values(index)%obj = mp_int_type(src(index))
    end do

END FUNCTION transfer_values_to_mp_int

FUNCTION transfer_values_from_mp (src) RESULT (dest)

class(mp_arr_type), allocatable, intent(in) :: src
real, allocatable, dimension(:) :: dest
real(kind=real64) :: deserialized_val
integer(kind=int64) :: index
logical :: status
    
    do index=1, src%numelements()
        call get_real(src%values(index)%obj, deserialized_val, status)
        dest(index) = deserialized_val
    end do

END FUNCTION transfer_values_from_mp

FUNCTION transfer_values_from_mp_int (src) RESULT (dest)

class(mp_arr_type), allocatable, intent(in) :: src
integer, allocatable, dimension(:) :: dest
integer(kind=int64) :: deserialized_int_val
integer(kind=int64) :: index
logical :: status
    
    do index=1, src%numelements()
        call get_int(src%values(index)%obj, deserialized_int_val, status)
        dest(index) = deserialized_int_val
    end do

END FUNCTION transfer_values_from_mp_int

END Module
