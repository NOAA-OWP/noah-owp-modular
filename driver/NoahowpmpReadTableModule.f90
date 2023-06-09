MODULE NoahowpmpReadTableModule

  use NoahowpmpIOType
  
  implicit none
  
  contains

  subroutine TableVarsRead(NoahowpmpIO)

    implicit none
    type(NoahowpmpIO_type), intent(inout) :: NoahowpmpIO        

    call read_veg_parameters(NoahowpmpIO)
    call read_soil_parameters(NoahowpmpIO)
    call read_rad_parameters(NoahowpmpIO)

  end subroutine TableVarsRead

  SUBROUTINE read_veg_parameters(NoahowpmpIO)

    implicit none
    type(NoahowpmpIO_type), intent(inout) :: NoahowpmpIO

    integer :: ierr
    integer :: IK,IM
    logical :: file_named
    integer :: NVEG
    character(len=256) :: VEG_DATASET_DESCRIPTION
    integer :: ISURBAN, ISWATER, ISBARREN, ISICE, ISCROP, EBLFOREST, NATURAL
    integer :: LCZ_1, LCZ_2, LCZ_3, LCZ_4, LCZ_5, LCZ_6, LCZ_7, LCZ_8, LCZ_9, LCZ_10, LCZ_11

    real, dimension(NoahowpmpIO%MVT) :: SAI_JAN,SAI_FEB,SAI_MAR,SAI_APR,SAI_MAY,SAI_JUN, &
                                      SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC
    real, dimension(NoahowpmpIO%MVT) :: LAI_JAN,LAI_FEB,LAI_MAR,LAI_APR,LAI_MAY,LAI_JUN, &
                                      LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC
    real, dimension(NoahowpmpIO%MVT) :: RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, &
                                      TAUL_VIS, TAUL_NIR, TAUS_VIS, TAUS_NIR
    real, dimension(NoahowpmpIO%MVT) :: CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, SCFFAC, XL, CWPVT, C3PSN, KC25, AKC, KO25, AKO, &
                            AVCMX, AQE, LTOVRC,  DILEFC,  DILEFW,  RMF25 ,  SLA   ,  FRAGR ,  TMIN  ,  VCMX25,  TDLEF ,  &
                            BP, MP, QE25, RMS25, RMR25, ARM, FOLNMX, WDPOOL, WRRAT, MRP, SHDFAC, NROOT, RGL, RS, HS, TOPT, RSMAX, &
                            SLAREA, EPS1, EPS2, EPS3, EPS4, EPS5

    namelist / usgs_veg_categories / VEG_DATASET_DESCRIPTION, NVEG

    namelist / usgs_veg_parameters / ISURBAN, ISWATER, ISBARREN, ISICE, ISCROP, EBLFOREST, NATURAL, &
          LCZ_1,LCZ_2,LCZ_3,LCZ_4,LCZ_5,LCZ_6,LCZ_7,LCZ_8,LCZ_9,LCZ_10,LCZ_11,&
          CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, SCFFAC, XL, CWPVT, C3PSN, KC25, AKC, KO25, AKO, AVCMX, AQE, &
          LTOVRC,  DILEFC,  DILEFW,  RMF25 ,  SLA   ,  FRAGR ,  TMIN  ,  VCMX25,  TDLEF ,  BP, MP, QE25, RMS25, RMR25, ARM, &
          FOLNMX, WDPOOL, WRRAT, MRP, SHDFAC, NROOT, RGL, RS, HS, TOPT, RSMAX, &
          SAI_JAN, SAI_FEB, SAI_MAR, SAI_APR, SAI_MAY, SAI_JUN,SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC, &
          LAI_JAN, LAI_FEB, LAI_MAR, LAI_APR, LAI_MAY, LAI_JUN,LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC, &
          RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, TAUL_VIS, TAUL_NIR, TAUS_VIS, TAUS_NIR, SLAREA, EPS1, EPS2, EPS3, EPS4, EPS5

    namelist / modis_veg_categories / VEG_DATASET_DESCRIPTION, NVEG

    namelist / modis_veg_parameters / ISURBAN, ISWATER, ISBARREN, ISICE, ISCROP, EBLFOREST, NATURAL, &
          LCZ_1,LCZ_2,LCZ_3,LCZ_4,LCZ_5,LCZ_6,LCZ_7,LCZ_8,LCZ_9,LCZ_10,LCZ_11, &
          CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, SCFFAC, XL, CWPVT, C3PSN, KC25, AKC, KO25, AKO, AVCMX, AQE, &
          LTOVRC,  DILEFC,  DILEFW,  RMF25 ,  SLA   ,  FRAGR ,  TMIN  ,  VCMX25,  TDLEF ,  BP, MP, QE25, RMS25, RMR25, ARM, &
          FOLNMX, WDPOOL, WRRAT, MRP, SHDFAC, NROOT, RGL, RS, HS, TOPT, RSMAX, &
          SAI_JAN, SAI_FEB, SAI_MAR, SAI_APR, SAI_MAY, SAI_JUN,SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC, &
          LAI_JAN, LAI_FEB, LAI_MAR, LAI_APR, LAI_MAY, LAI_JUN,LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC, &
          RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, TAUL_VIS, TAUL_NIR, TAUS_VIS, TAUS_NIR, SLAREA, EPS1, EPS2, EPS3, EPS4, EPS5

    inquire( file=trim(NoahowpmpIO%parameter_dir)//'/'//trim(NoahowpmpIO%noahowp_table), exist=file_named )
    if ( file_named ) then
      open(15, file=trim(NoahowpmpIO%parameter_dir)//'/'//trim(NoahowpmpIO%noahowp_table), status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if

    if (ierr /= 0) then
        call handle_err(ierr, "ParametersRead.f90: read_veg_parameters: Cannot find file MPTABLE.TBL")
    endif

    if ( trim(NoahowpmpIO%veg_class_name) == "USGS" ) then
        read(15,usgs_veg_categories)
        read(15,usgs_veg_parameters)
    else if ( trim(NoahowpmpIO%veg_class_name) == "MODIFIED_IGBP_MODIS_NOAH" ) then
        read(15,modis_veg_categories)
        read(15,modis_veg_parameters)
    else
        write(*,'("WARNING: DATASET_IDENTIFIER = ''", A, "''")') trim(NoahowpmpIO%veg_class_name)
        call handle_err(ierr, 'ParametersRead.f90: read_veg_parameters: Unrecognized DATASET_IDENTIFIER in subroutine read_VEG_PARAMETERS')
    endif
    close(15)

    NoahowpmpIO%ISURBAN_TABLE   = ISURBAN
    NoahowpmpIO%ISWATER_TABLE   = ISWATER
    NoahowpmpIO%ISBARREN_TABLE   = ISBARREN
    NoahowpmpIO%ISICE_TABLE   = ISICE
    NoahowpmpIO%ISCROP_TABLE   = ISCROP
    NoahowpmpIO%EBLFOREST_TABLE   = EBLFOREST
    NoahowpmpIO%NATURAL_TABLE   = NATURAL
    NoahowpmpIO%LCZ_1_TABLE   = LCZ_1
    NoahowpmpIO%LCZ_2_TABLE   = LCZ_2
    NoahowpmpIO%LCZ_3_TABLE   = LCZ_3
    NoahowpmpIO%LCZ_4_TABLE   = LCZ_4
    NoahowpmpIO%LCZ_5_TABLE   = LCZ_5
    NoahowpmpIO%LCZ_6_TABLE   = LCZ_6
    NoahowpmpIO%LCZ_7_TABLE   = LCZ_7
    NoahowpmpIO%LCZ_8_TABLE   = LCZ_8
    NoahowpmpIO%LCZ_9_TABLE   = LCZ_9
    NoahowpmpIO%LCZ_10_TABLE  = LCZ_10
    NoahowpmpIO%LCZ_11_TABLE  = LCZ_11

    NoahowpmpIO%CH2OP_TABLE(1:NVEG)  = CH2OP(1:NVEG)
    NoahowpmpIO%DLEAF_TABLE(1:NVEG)  = DLEAF(1:NVEG)
    NoahowpmpIO%Z0MVT_TABLE(1:NVEG)  = Z0MVT(1:NVEG)
    NoahowpmpIO%HVT_TABLE(1:NVEG)  = HVT(1:NVEG)
    NoahowpmpIO%HVB_TABLE(1:NVEG)  = HVB(1:NVEG)
    NoahowpmpIO%DEN_TABLE(1:NVEG)  = DEN(1:NVEG)
    NoahowpmpIO%RC_TABLE(1:NVEG)  = RC(1:NVEG)
    NoahowpmpIO%MFSNO_TABLE(1:NVEG)  = MFSNO(1:NVEG)
    NoahowpmpIO%SCFFAC_TABLE(1:NVEG)  = SCFFAC(1:NVEG)
    NoahowpmpIO%XL_TABLE(1:NVEG)  = XL(1:NVEG)
    NoahowpmpIO%CWPVT_TABLE(1:NVEG)  = CWPVT(1:NVEG)
    NoahowpmpIO%C3PSN_TABLE(1:NVEG)  = C3PSN(1:NVEG)
    NoahowpmpIO%KC25_TABLE(1:NVEG)  = KC25(1:NVEG)
    NoahowpmpIO%AKC_TABLE(1:NVEG)  = AKC(1:NVEG)
    NoahowpmpIO%KO25_TABLE(1:NVEG)  = KO25(1:NVEG)
    NoahowpmpIO%AKO_TABLE(1:NVEG)  = AKO(1:NVEG)
    NoahowpmpIO%AVCMX_TABLE(1:NVEG)  = AVCMX(1:NVEG)
    NoahowpmpIO%AQE_TABLE(1:NVEG)  = AQE(1:NVEG)
    NoahowpmpIO%LTOVRC_TABLE(1:NVEG)  = LTOVRC(1:NVEG)
    NoahowpmpIO%DILEFC_TABLE(1:NVEG)  = DILEFC(1:NVEG)
    NoahowpmpIO%DILEFW_TABLE(1:NVEG)  = DILEFW(1:NVEG)
    NoahowpmpIO%RMF25_TABLE(1:NVEG)  = RMF25(1:NVEG)
    NoahowpmpIO%SLA_TABLE(1:NVEG)  = SLA(1:NVEG)
    NoahowpmpIO%FRAGR_TABLE(1:NVEG)  = FRAGR(1:NVEG)
    NoahowpmpIO%TMIN_TABLE(1:NVEG)  = TMIN(1:NVEG)
    NoahowpmpIO%VCMX25_TABLE(1:NVEG)  = VCMX25(1:NVEG)
    NoahowpmpIO%TDLEF_TABLE(1:NVEG)  = TDLEF(1:NVEG)
    NoahowpmpIO%BP_TABLE(1:NVEG)  = BP(1:NVEG)
    NoahowpmpIO%MP_TABLE(1:NVEG)  = MP(1:NVEG)
    NoahowpmpIO%QE25_TABLE(1:NVEG)  = QE25(1:NVEG)
    NoahowpmpIO%RMS25_TABLE(1:NVEG)  = RMS25(1:NVEG)
    NoahowpmpIO%RMR25_TABLE(1:NVEG)  = RMR25(1:NVEG)
    NoahowpmpIO%ARM_TABLE(1:NVEG)  = ARM(1:NVEG)
    NoahowpmpIO%FOLNMX_TABLE(1:NVEG)  = FOLNMX(1:NVEG)
    NoahowpmpIO%WDPOOL_TABLE(1:NVEG)  = WDPOOL(1:NVEG)
    NoahowpmpIO%WRRAT_TABLE(1:NVEG)  = WRRAT(1:NVEG)
    NoahowpmpIO%MRP_TABLE(1:NVEG)  = MRP(1:NVEG)
    NoahowpmpIO%SHDFAC_TABLE(1:NVEG)  = SHDFAC(1:NVEG)
    NoahowpmpIO%NROOT_TABLE(1:NVEG)  = NROOT(1:NVEG)
    NoahowpmpIO%RGL_TABLE(1:NVEG)  = RGL(1:NVEG)
    NoahowpmpIO%RS_TABLE(1:NVEG)  = RS(1:NVEG)
    NoahowpmpIO%HS_TABLE(1:NVEG)  = HS(1:NVEG)
    NoahowpmpIO%TOPT_TABLE(1:NVEG)  = TOPT(1:NVEG)
    NoahowpmpIO%RSMAX_TABLE(1:NVEG)  = RSMAX(1:NVEG)

    ! Put LAI and SAI into 2d array from monthly lines in table; same for canopy radiation properties

    NoahowpmpIO%SAIM_TABLE(1:NVEG, 1) = SAI_JAN(1:NVEG)
    NoahowpmpIO%SAIM_TABLE(1:NVEG, 2) = SAI_FEB(1:NVEG)
    NoahowpmpIO%SAIM_TABLE(1:NVEG, 3) = SAI_MAR(1:NVEG)
    NoahowpmpIO%SAIM_TABLE(1:NVEG, 4) = SAI_APR(1:NVEG)
    NoahowpmpIO%SAIM_TABLE(1:NVEG, 5) = SAI_MAY(1:NVEG)
    NoahowpmpIO%SAIM_TABLE(1:NVEG, 6) = SAI_JUN(1:NVEG)
    NoahowpmpIO%SAIM_TABLE(1:NVEG, 7) = SAI_JUL(1:NVEG)
    NoahowpmpIO%SAIM_TABLE(1:NVEG, 8) = SAI_AUG(1:NVEG)
    NoahowpmpIO%SAIM_TABLE(1:NVEG, 9) = SAI_SEP(1:NVEG)
    NoahowpmpIO%SAIM_TABLE(1:NVEG,10) = SAI_OCT(1:NVEG)
    NoahowpmpIO%SAIM_TABLE(1:NVEG,11) = SAI_NOV(1:NVEG)
    NoahowpmpIO%SAIM_TABLE(1:NVEG,12) = SAI_DEC(1:NVEG)

    NoahowpmpIO%LAIM_TABLE(1:NVEG, 1) = LAI_JAN(1:NVEG)
    NoahowpmpIO%LAIM_TABLE(1:NVEG, 2) = LAI_FEB(1:NVEG)
    NoahowpmpIO%LAIM_TABLE(1:NVEG, 3) = LAI_MAR(1:NVEG)
    NoahowpmpIO%LAIM_TABLE(1:NVEG, 4) = LAI_APR(1:NVEG)
    NoahowpmpIO%LAIM_TABLE(1:NVEG, 5) = LAI_MAY(1:NVEG)
    NoahowpmpIO%LAIM_TABLE(1:NVEG, 6) = LAI_JUN(1:NVEG)
    NoahowpmpIO%LAIM_TABLE(1:NVEG, 7) = LAI_JUL(1:NVEG)
    NoahowpmpIO%LAIM_TABLE(1:NVEG, 8) = LAI_AUG(1:NVEG)
    NoahowpmpIO%LAIM_TABLE(1:NVEG, 9) = LAI_SEP(1:NVEG)
    NoahowpmpIO%LAIM_TABLE(1:NVEG,10) = LAI_OCT(1:NVEG)
    NoahowpmpIO%LAIM_TABLE(1:NVEG,11) = LAI_NOV(1:NVEG)
    NoahowpmpIO%LAIM_TABLE(1:NVEG,12) = LAI_DEC(1:NVEG)

    NoahowpmpIO%RHOL_TABLE(1:NVEG,1)  = RHOL_VIS(1:NVEG) !leaf reflectance: 1=vis, 2=nir
    NoahowpmpIO%RHOL_TABLE(1:NVEG,2)  = RHOL_NIR(1:NVEG) !leaf reflectance: 1=vis, 2=nir
    NoahowpmpIO%RHOS_TABLE(1:NVEG,1)  = RHOS_VIS(1:NVEG) !stem reflectance: 1=vis, 2=nir
    NoahowpmpIO%RHOS_TABLE(1:NVEG,2)  = RHOS_NIR(1:NVEG) !stem reflectance: 1=vis, 2=nir
    NoahowpmpIO%TAUL_TABLE(1:NVEG,1)  = TAUL_VIS(1:NVEG) !leaf transmittance: 1=vis, 2=nir
    NoahowpmpIO%TAUL_TABLE(1:NVEG,2)  = TAUL_NIR(1:NVEG) !leaf transmittance: 1=vis, 2=nir
    NoahowpmpIO%TAUS_TABLE(1:NVEG,1)  = TAUS_VIS(1:NVEG) !stem transmittance: 1=vis, 2=nir
    NoahowpmpIO%TAUS_TABLE(1:NVEG,2)  = TAUS_NIR(1:NVEG) !stem transmittance: 1=vis, 2=nir
    
  END SUBROUTINE read_veg_parameters

  SUBROUTINE read_soil_parameters(NoahowpmpIO)

    implicit none
    type(NoahowpmpIO_type), intent(inout) :: NoahowpmpIO

    integer                      :: IERR
    character(len=20)            :: SLTYPE
    integer                      :: ITMP, NUM_SLOPE, LC, SLCATS
    integer                      :: iLine               ! loop index
    character(len=256)           :: message
    logical                      :: file_named
    integer, parameter           :: MAX_SOILTYP = 30
    real, dimension(MAX_SOILTYP) :: BEXP_TABLE, SMCDRY_TABLE, F1_TABLE, SMCMAX_TABLE, SMCREF_TABLE, &
                                    PSISAT_TABLE, DKSAT_TABLE, DWSAT_TABLE, SMCWLT_TABLE, QUARTZ_TABLE
    real, dimension(MAX_SOILTYP) :: BVIC_TABLE, AXAJ_TABLE, BXAJ_TABLE, XXAJ_TABLE, BDVIC_TABLE, &
                                    GDVIC_TABLE, BBVIC_TABLE
    real, dimension(9)           :: SLOPE_TABLE
    real                         :: CSOIL_TABLE, REFDK_TABLE, REFKDT_TABLE, FRZK_TABLE, ZBOT_TABLE, CZIL_TABLE, Z0_TABLE

!-----READ IN SOIL PROPERTIES FROM SOILPARM.TBL
!
    inquire( file=trim(NoahowpmpIO%parameter_dir)//'/'//trim(NoahowpmpIO%soil_table), exist=file_named )
    if ( file_named ) then
      open(21, file=trim(NoahowpmpIO%parameter_dir)//'/'//trim(NoahowpmpIO%soil_table),form='formatted',status='old',iostat=ierr)
    else
      open(21, form='formatted',status='old',iostat=ierr)
    end if
    if (ierr/=0) then
      write(message,fmt='(A)') 'ParametersRead.f90: read_soil_parameters: failure opening SOILPARM.TBL'
      call handle_err(ierr, message)
    end if
    do iLine = 1,100
      READ (21,*) SLTYPE
      if (trim(SLTYPE) == trim(NoahowpmpIO%soil_class_name)) exit
    end do
    READ (21,*) SLCATS
    DO LC=1,SLCATS
      READ (21,*) ITMP,BEXP_TABLE(LC),SMCDRY_TABLE(LC),F1_TABLE(LC),SMCMAX_TABLE(LC),   &
                  SMCREF_TABLE(LC),PSISAT_TABLE(LC),DKSAT_TABLE(LC), DWSAT_TABLE(LC),   &
                  SMCWLT_TABLE(LC), QUARTZ_TABLE(LC),BVIC_TABLE(LC), AXAJ_TABLE(LC),    &
                  BXAJ_TABLE(LC),XXAJ_TABLE(LC),BDVIC_TABLE(LC),BBVIC_TABLE(LC),GDVIC_TABLE(LC)
    ENDDO
    CLOSE (21)

    ! Transfer read-in values to NoahowpmpIO
    NoahowpmpIO%SLCATS = SLCATS
    NoahowpmpIO%BEXP_TABLE(1:SLCATS) = BEXP_TABLE(1:SLCATS)
    NoahowpmpIO%SMCDRY_TABLE(1:SLCATS) = SMCDRY_TABLE(1:SLCATS)
    NoahowpmpIO%F1_TABLE(1:SLCATS) = F1_TABLE(1:SLCATS)
    NoahowpmpIO%SMCMAX_TABLE(1:SLCATS) = SMCMAX_TABLE(1:SLCATS)
    NoahowpmpIO%SMCREF_TABLE(1:SLCATS) = SMCREF_TABLE(1:SLCATS)
    NoahowpmpIO%PSISAT_TABLE(1:SLCATS) = PSISAT_TABLE(1:SLCATS)
    NoahowpmpIO%DKSAT_TABLE(1:SLCATS) = DKSAT_TABLE(1:SLCATS)
    NoahowpmpIO%DWSAT_TABLE(1:SLCATS) = DWSAT_TABLE(1:SLCATS)
    NoahowpmpIO%SMCWLT_TABLE(1:SLCATS) = SMCWLT_TABLE(1:SLCATS)
    NoahowpmpIO%QUARTZ_TABLE(1:SLCATS) = QUARTZ_TABLE(1:SLCATS)
    NoahowpmpIO%BVIC_TABLE(1:SLCATS) = BVIC_TABLE(1:SLCATS)
    NoahowpmpIO%AXAJ_TABLE(1:SLCATS) = AXAJ_TABLE(1:SLCATS)
    NoahowpmpIO%BXAJ_TABLE(1:SLCATS) = BXAJ_TABLE(1:SLCATS)
    NoahowpmpIO%XXAJ_TABLE(1:SLCATS) = XXAJ_TABLE(1:SLCATS)
    NoahowpmpIO%BDVIC_TABLE(1:SLCATS) = BDVIC_TABLE(1:SLCATS)
    NoahowpmpIO%BBVIC_TABLE(1:SLCATS) = BBVIC_TABLE(1:SLCATS)
    NoahowpmpIO%GDVIC_TABLE(1:SLCATS) = GDVIC_TABLE(1:SLCATS)

!-----READ IN GENERAL PARAMETERS FROM GENPARM.TBL
!
    inquire( file=trim(NoahowpmpIO%parameter_dir)//'/'//trim(NoahowpmpIO%general_table), exist=file_named )
    if ( file_named ) then
      open(22, file=trim(NoahowpmpIO%parameter_dir)//'/'//trim(NoahowpmpIO%general_table),form='formatted',status='old',iostat=ierr)
    else
      open(22, form='formatted',status='old',iostat=ierr)
    end if
    if (ierr /= 0) then
      call handle_err(ierr, 'ParametersRead.f90: read_soil_parameters: failure opening GENPARM.TBL')
    end if
    read (22,*)
    read (22,*)
    read (22,*) NUM_SLOPE
    do LC=1,NUM_SLOPE
        read (22,*) SLOPE_TABLE(LC)
    end do
    read (22,*)
    read (22,*)
    read (22,*)
    read (22,*)
    read (22,*)
    read (22,*) CSOIL_TABLE
    read (22,*)
    read (22,*)
    read (22,*)
    read (22,*) REFDK_TABLE
    read (22,*)
    read (22,*) REFKDT_TABLE
    read (22,*)
    read (22,*) FRZK_TABLE
    read (22,*)
    read (22,*) ZBOT_TABLE
    read (22,*)
    read (22,*) CZIL_TABLE
    read (22,*)
    read (22,*)
    read (22,*)
    read (22,*)
    read (22,*)
    read (22,*)
    read (22,*)
    read (22,*) Z0_TABLE
    close (22)
   
    ! Transfer read-in values to NoahowpmpIO
    NoahowpmpIO%SLOPE_TABLE(1:NUM_SLOPE) = SLOPE_TABLE(1:NUM_SLOPE)
    NoahowpmpIO%CSOIL_TABLE = CSOIL_TABLE
    NoahowpmpIO%REFDK_TABLE = REFDK_TABLE
    NoahowpmpIO%REFKDT_TABLE = REFKDT_TABLE
    NoahowpmpIO%FRZK_TABLE = FRZK_TABLE
    NoahowpmpIO%ZBOT_TABLE = ZBOT_TABLE
    NoahowpmpIO%CZIL_TABLE = CZIL_TABLE
    NoahowpmpIO%Z0_TABLE = Z0_TABLE

  END SUBROUTINE read_soil_parameters

  SUBROUTINE read_rad_parameters(NoahowpmpIO)
    
    implicit none
    type(NoahowpmpIO_type), intent(inout) :: NoahowpmpIO
    integer                               :: ierr
    logical                               :: file_named
    real :: ALBSAT_VIS(NoahowpmpIO%MSC)
    real :: ALBSAT_NIR(NoahowpmpIO%MSC)
    real :: ALBDRY_VIS(NoahowpmpIO%MSC)
    real :: ALBDRY_NIR(NoahowpmpIO%MSC)
    real :: ALBICE(NoahowpmpIO%MBAND)
    real :: ALBLAK(NoahowpmpIO%MBAND)
    real :: OMEGAS(NoahowpmpIO%MBAND)
    real :: BETADS
    real :: BETAIS
    real :: EG(2)

    namelist / rad_parameters / ALBSAT_VIS, &
                                ALBSAT_NIR, &
                                ALBDRY_VIS, &
                                ALBDRY_NIR, &
                                ALBICE, & 
                                ALBLAK, &
                                OMEGAS, &
                                BETADS, &
                                BETAIS, &
                                EG

    inquire( file=trim(NoahowpmpIO%parameter_dir)//'/'//trim(NoahowpmpIO%noahowp_table), exist=file_named )
    if ( file_named ) then
      open(15, file=trim(NoahowpmpIO%parameter_dir)//'/'//trim(NoahowpmpIO%noahowp_table), status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if (ierr /= 0) then
        call handle_err(ierr, 'ParametersRead.f90: read_rad_parameters: Cannot find file MPTABLE.TBL')
    endif
    read(15,rad_parameters)
    close(15)

    NoahowpmpIO%ALBSAT_TABLE(:,1) = ALBSAT_VIS ! saturated soil albedos: 1=vis, 2=nir
    NoahowpmpIO%ALBSAT_TABLE(:,2) = ALBSAT_NIR ! saturated soil albedos: 1=vis, 2=nir
    NoahowpmpIO%ALBDRY_TABLE(:,1) = ALBDRY_VIS ! dry soil albedos: 1=vis, 2=nir
    NoahowpmpIO%ALBDRY_TABLE(:,2) = ALBDRY_NIR ! dry soil albedos: 1=vis, 2=nir
    NoahowpmpIO%ALBICE_TABLE      = ALBICE
    NoahowpmpIO%ALBLAK_TABLE      = ALBLAK
    NoahowpmpIO%OMEGAS_TABLE      = OMEGAS
    NoahowpmpIO%BETADS_TABLE      = BETADS
    NoahowpmpIO%BETAIS_TABLE      = BETAIS
    NoahowpmpIO%EG_TABLE          = EG

  end subroutine read_rad_parameters

  subroutine read_global_parameters(NoahowpmpIO)

    implicit none
    type(NoahowpmpIO_type),intent(inout) :: NoahowpmpIO
    integer                      :: ierr
    logical                      :: file_named

    real :: CO2,O2,TIMEAN,FSATMX,Z0SNO,SSI,SNOW_RET_FAC,SNOW_EMIS,&
            SWEMX,TAU0,GRAIN_GROWTH,EXTRA_GROWTH,DIRT_SOOT,&
            BATS_COSZ,BATS_VIS_NEW,BATS_NIR_NEW,BATS_VIS_AGE,BATS_NIR_AGE,BATS_VIS_DIR,BATS_NIR_DIR,&
            RSURF_SNOW,RSURF_EXP

    namelist / global_parameters / CO2,O2,TIMEAN,FSATMX,Z0SNO,SSI,SNOW_RET_FAC,SNOW_EMIS,&
            SWEMX,TAU0,GRAIN_GROWTH,EXTRA_GROWTH,DIRT_SOOT,&
            BATS_COSZ,BATS_VIS_NEW,BATS_NIR_NEW,BATS_VIS_AGE,BATS_NIR_AGE,BATS_VIS_DIR,BATS_NIR_DIR,&
            RSURF_SNOW,RSURF_EXP

    inquire( file=trim(NoahowpmpIO%parameter_dir)//'/'//trim(NoahowpmpIO%noahowp_table), exist=file_named )
    if ( file_named ) then
      open(15, file=trim(NoahowpmpIO%parameter_dir)//'/'//trim(NoahowpmpIO%noahowp_table), status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if (ierr /= 0) then
        call handle_err(ierr, 'ParametersRead.f90: read_global_parameters: Cannot find file MPTABLE.TBL')
    endif
    read(15,global_parameters)
    close(15)

    NoahowpmpIO%CO2_TABLE     = CO2
    NoahowpmpIO%O2_TABLE     = O2
    NoahowpmpIO%TIMEAN_TABLE     = TIMEAN
    NoahowpmpIO%FSATMX_TABLE     = FSATMX
    NoahowpmpIO%Z0SNO_TABLE     = Z0SNO
    NoahowpmpIO%SSI_TABLE     = SSI
    NoahowpmpIO%SNOW_RET_FAC_TABLE   = SNOW_RET_FAC
    NoahowpmpIO%SNOW_EMIS_TABLE   = SNOW_EMIS
    NoahowpmpIO%SWEMX_TABLE     = SWEMX
    NoahowpmpIO%TAU0_TABLE   = TAU0
    NoahowpmpIO%GRAIN_GROWTH_TABLE   = GRAIN_GROWTH
    NoahowpmpIO%EXTRA_GROWTH_TABLE   = EXTRA_GROWTH
    NoahowpmpIO%DIRT_SOOT_TABLE   = DIRT_SOOT
    NoahowpmpIO%BATS_COSZ_TABLE   = BATS_COSZ
    NoahowpmpIO%BATS_VIS_NEW_TABLE   = BATS_VIS_NEW
    NoahowpmpIO%BATS_NIR_NEW_TABLE   = BATS_NIR_NEW
    NoahowpmpIO%BATS_VIS_AGE_TABLE   = BATS_VIS_AGE
    NoahowpmpIO%BATS_NIR_AGE_TABLE   = BATS_NIR_AGE
    NoahowpmpIO%BATS_VIS_DIR_TABLE   = BATS_VIS_DIR
    NoahowpmpIO%BATS_NIR_DIR_TABLE   = BATS_NIR_DIR
    NoahowpmpIO%RSURF_SNOW_TABLE     = RSURF_SNOW
    NoahowpmpIO%RSURF_EXP_TABLE     = RSURF_EXP

  END SUBROUTINE read_global_parameters

  SUBROUTINE read_crop_parameters(NoahowpmpIO)

    implicit none
    type(NoahowpmpIO_type),intent(inout) :: NoahowpmpIO
    integer                      :: ierr
    logical                      :: file_named

    integer                   :: DEFAULT_CROP
    integer, dimension(NoahowpmpIO%NCROP) :: PLTDAY
    integer, dimension(NoahowpmpIO%NCROP) :: HSDAY
    real, dimension(NoahowpmpIO%NCROP) :: PLANTPOP
    real, dimension(NoahowpmpIO%NCROP) :: IRRI
    real, dimension(NoahowpmpIO%NCROP) :: GDDTBASE
    real, dimension(NoahowpmpIO%NCROP) :: GDDTCUT
    real, dimension(NoahowpmpIO%NCROP) :: GDDS1
    real, dimension(NoahowpmpIO%NCROP) :: GDDS2
    real, dimension(NoahowpmpIO%NCROP) :: GDDS3
    real, dimension(NoahowpmpIO%NCROP) :: GDDS4
    real, dimension(NoahowpmpIO%NCROP) :: GDDS5
    real, dimension(NoahowpmpIO%NCROP) :: C3PSN   ! this session copied from stomata parameters Zhe Zhang 2020-07-13
    real, dimension(NoahowpmpIO%NCROP) :: KC25
    real, dimension(NoahowpmpIO%NCROP) :: AKC
    real, dimension(NoahowpmpIO%NCROP) :: KO25
    real, dimension(NoahowpmpIO%NCROP) :: AKO
    real, dimension(NoahowpmpIO%NCROP) :: AVCMX
    real, dimension(NoahowpmpIO%NCROP) :: VCMX25
    real, dimension(NoahowpmpIO%NCROP) :: BP
    real, dimension(NoahowpmpIO%NCROP) :: MP
    real, dimension(NoahowpmpIO%NCROP) :: FOLNMX
    real, dimension(NoahowpmpIO%NCROP) :: QE25    ! until here
    integer, dimension(NoahowpmpIO%NCROP) :: C3C4
    real, dimension(NoahowpmpIO%NCROP) :: AREF
    real, dimension(NoahowpmpIO%NCROP) :: PSNRF
    real, dimension(NoahowpmpIO%NCROP) :: I2PAR
    real, dimension(NoahowpmpIO%NCROP) :: TASSIM0
    real, dimension(NoahowpmpIO%NCROP) :: TASSIM1
    real, dimension(NoahowpmpIO%NCROP) :: TASSIM2
    real, dimension(NoahowpmpIO%NCROP) :: K
    real, dimension(NoahowpmpIO%NCROP) :: EPSI
    real, dimension(NoahowpmpIO%NCROP) :: Q10MR
    real, dimension(NoahowpmpIO%NCROP) :: FOLN_MX
    real, dimension(NoahowpmpIO%NCROP) :: LEFREEZ
    real, dimension(NoahowpmpIO%NCROP) :: DILE_FC_S1,DILE_FC_S2,DILE_FC_S3,DILE_FC_S4,DILE_FC_S5,DILE_FC_S6,DILE_FC_S7,DILE_FC_S8
    real, dimension(NoahowpmpIO%NCROP) :: DILE_FW_S1,DILE_FW_S2,DILE_FW_S3,DILE_FW_S4,DILE_FW_S5,DILE_FW_S6,DILE_FW_S7,DILE_FW_S8
    real, dimension(NoahowpmpIO%NCROP) :: FRA_GR
    real, dimension(NoahowpmpIO%NCROP) :: LF_OVRC_S1,LF_OVRC_S2,LF_OVRC_S3,LF_OVRC_S4,LF_OVRC_S5,LF_OVRC_S6,LF_OVRC_S7,LF_OVRC_S8
    real, dimension(NoahowpmpIO%NCROP) :: ST_OVRC_S1,ST_OVRC_S2,ST_OVRC_S3,ST_OVRC_S4,ST_OVRC_S5,ST_OVRC_S6,ST_OVRC_S7,ST_OVRC_S8
    real, dimension(NoahowpmpIO%NCROP) :: RT_OVRC_S1,RT_OVRC_S2,RT_OVRC_S3,RT_OVRC_S4,RT_OVRC_S5,RT_OVRC_S6,RT_OVRC_S7,RT_OVRC_S8
    real, dimension(NoahowpmpIO%NCROP) :: LFMR25
    real, dimension(NoahowpmpIO%NCROP) :: STMR25
    real, dimension(NoahowpmpIO%NCROP) :: RTMR25
    real, dimension(NoahowpmpIO%NCROP) :: GRAINMR25
    real, dimension(NoahowpmpIO%NCROP) :: LFPT_S1,LFPT_S2,LFPT_S3,LFPT_S4,LFPT_S5,LFPT_S6,LFPT_S7,LFPT_S8
    real, dimension(NoahowpmpIO%NCROP) :: STPT_S1,STPT_S2,STPT_S3,STPT_S4,STPT_S5,STPT_S6,STPT_S7,STPT_S8
    real, dimension(NoahowpmpIO%NCROP) :: RTPT_S1,RTPT_S2,RTPT_S3,RTPT_S4,RTPT_S5,RTPT_S6,RTPT_S7,RTPT_S8
    real, dimension(NoahowpmpIO%NCROP) :: GRAINPT_S1,GRAINPT_S2,GRAINPT_S3,GRAINPT_S4,GRAINPT_S5,GRAINPT_S6,GRAINPT_S7,GRAINPT_S8
    real, dimension(NoahowpmpIO%NCROP) :: LFCT_S1,LFCT_S2,LFCT_S3,LFCT_S4,LFCT_S5,LFCT_S6,LFCT_S7,LFCT_S8
    real, dimension(NoahowpmpIO%NCROP) :: STCT_S1,STCT_S2,STCT_S3,STCT_S4,STCT_S5,STCT_S6,STCT_S7,STCT_S8
    real, dimension(NoahowpmpIO%NCROP) :: RTCT_S1,RTCT_S2,RTCT_S3,RTCT_S4,RTCT_S5,RTCT_S6,RTCT_S7,RTCT_S8
    real, dimension(NoahowpmpIO%NCROP) :: BIO2LAI


!    namelist / crop_parameters /DEFAULT_CROP,   PLTDAY,     HSDAY,  PLANTPOP,      IRRI,  GDDTBASE,   GDDTCUT,     GDDS1,     GDDS2, &
!                                             GDDS3,     GDDS4,     GDDS5,      C3C4,      AREF,     PSNRF,     I2PAR,   TASSIM0, &
!                                           TASSIM1,   TASSIM2,         K,      EPSI,     Q10MR,   FOLN_MX,   LEFREEZ,            &
! Zhe Zhang 2020-07-13
    namelist / crop_parameters /DEFAULT_CROP,   PLTDAY,     HSDAY,  PLANTPOP,      IRRI,  GDDTBASE,   GDDTCUT,     GDDS1,  GDDS2,  GDDS3,     GDDS4,     GDDS5, & !
                                              C3PSN,     KC25,       AKC,      KO25,       AKO,     AVCMX,    VCMX25,        BP,     MP, FOLNMX,      QE25, &  ! parameters added from stomata
                                                C3C4,     AREF,     PSNRF,     I2PAR,   TASSIM0,                                               &
                                        TASSIM1,   TASSIM2,         K,      EPSI,     Q10MR,   FOLN_MX,   LEFREEZ,               &
                                        DILE_FC_S1,DILE_FC_S2,DILE_FC_S3,DILE_FC_S4,DILE_FC_S5,DILE_FC_S6,DILE_FC_S7,DILE_FC_S8, &
                                        DILE_FW_S1,DILE_FW_S2,DILE_FW_S3,DILE_FW_S4,DILE_FW_S5,DILE_FW_S6,DILE_FW_S7,DILE_FW_S8, &
                                            FRA_GR,                                                                              &
                                        LF_OVRC_S1,LF_OVRC_S2,LF_OVRC_S3,LF_OVRC_S4,LF_OVRC_S5,LF_OVRC_S6,LF_OVRC_S7,LF_OVRC_S8, &
                                        ST_OVRC_S1,ST_OVRC_S2,ST_OVRC_S3,ST_OVRC_S4,ST_OVRC_S5,ST_OVRC_S6,ST_OVRC_S7,ST_OVRC_S8, &
                                        RT_OVRC_S1,RT_OVRC_S2,RT_OVRC_S3,RT_OVRC_S4,RT_OVRC_S5,RT_OVRC_S6,RT_OVRC_S7,RT_OVRC_S8, &
                                            LFMR25,    STMR25,    RTMR25, GRAINMR25,                                             &
                                            LFPT_S1,   LFPT_S2,   LFPT_S3,   LFPT_S4,   LFPT_S5,   LFPT_S6,   LFPT_S7,   LFPT_S8, &
                                            STPT_S1,   STPT_S2,   STPT_S3,   STPT_S4,   STPT_S5,   STPT_S6,   STPT_S7,   STPT_S8, &
                                            RTPT_S1,   RTPT_S2,   RTPT_S3,   RTPT_S4,   RTPT_S5,   RTPT_S6,   RTPT_S7,   RTPT_S8, &
                                        GRAINPT_S1,GRAINPT_S2,GRAINPT_S3,GRAINPT_S4,GRAINPT_S5,GRAINPT_S6,GRAINPT_S7,GRAINPT_S8, &
                                            LFCT_S1,LFCT_S2,LFCT_S3,LFCT_S4,LFCT_S5,LFCT_S6,LFCT_S7,LFCT_S8,                      &
                                            STCT_S1,STCT_S2,STCT_S3,STCT_S4,STCT_S5,STCT_S6,STCT_S7,STCT_S8,                      &
                                            RTCT_S1,RTCT_S2,RTCT_S3,RTCT_S4,RTCT_S5,RTCT_S6,RTCT_S7,RTCT_S8,                      &
                                            BIO2LAI

    inquire( file=trim(NoahowpmpIO%parameter_dir)//'/'//trim(NoahowpmpIO%noahowp_table), exist=file_named )
    if ( file_named ) then
      open(15, file=trim(NoahowpmpIO%parameter_dir)//'/'//trim(NoahowpmpIO%noahowp_table), status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if (ierr /= 0) then
        call handle_err(ierr, 'ParametersRead.f90: read_crop_parameters: Cannot find file MPTABLE.TBL')
    endif
    read(15,crop_parameters)
    close(15)

    NoahowpmpIO%DEFAULT_CROP_TABLE      = DEFAULT_CROP
    NoahowpmpIO%PLTDAY_TABLE      = PLTDAY
    NoahowpmpIO%HSDAY_TABLE      = HSDAY
    NoahowpmpIO%PLANTPOP_TABLE      = PLANTPOP
    NoahowpmpIO%IRRI_TABLE      = IRRI
    NoahowpmpIO%GDDTBASE_TABLE      = GDDTBASE
    NoahowpmpIO%GDDTCUT_TABLE      = GDDTCUT
    NoahowpmpIO%GDDS1_TABLE      = GDDS1
    NoahowpmpIO%GDDS2_TABLE      = GDDS2
    NoahowpmpIO%GDDS3_TABLE      = GDDS3
    NoahowpmpIO%GDDS4_TABLE      = GDDS4
    NoahowpmpIO%GDDS5_TABLE      = GDDS5
    NoahowpmpIO%C3PSNI_TABLE(1:5) = C3PSN(1:5)  ! parameters from stomata ! Zhe Zhang 2020-07-13
    NoahowpmpIO%KC25I_TABLE(1:5) = KC25(1:5)
    NoahowpmpIO%AKCI_TABLE(1:5) = AKC(1:5)
    NoahowpmpIO%KO25I_TABLE(1:5) = KO25(1:5)
    NoahowpmpIO%AKOI_TABLE(1:5) = AKO(1:5)
    NoahowpmpIO%AVCMXI_TABLE(1:5) = AVCMX(1:5)
    NoahowpmpIO%VCMX25I_TABLE(1:5) = VCMX25(1:5)
    NoahowpmpIO%BPI_TABLE(1:5) = BP(1:5)
    NoahowpmpIO%MPI_TABLE(1:5) = MP(1:5)
    NoahowpmpIO%FOLNMXI_TABLE(1:5) = FOLNMX(1:5)
    NoahowpmpIO%QE25I_TABLE(1:5) = QE25(1:5)   ! ends here
    NoahowpmpIO%C3C4_TABLE      = C3C4
    NoahowpmpIO%AREF_TABLE      = AREF
    NoahowpmpIO%PSNRF_TABLE      = PSNRF
    NoahowpmpIO%I2PAR_TABLE      = I2PAR
    NoahowpmpIO%TASSIM0_TABLE      = TASSIM0
    NoahowpmpIO%TASSIM1_TABLE      = TASSIM1
    NoahowpmpIO%TASSIM2_TABLE      = TASSIM2
    NoahowpmpIO%K_TABLE      = K
    NoahowpmpIO%EPSI_TABLE      = EPSI
    NoahowpmpIO%Q10MR_TABLE      = Q10MR
    NoahowpmpIO%FOLN_MX_TABLE      = FOLN_MX
    NoahowpmpIO%LEFREEZ_TABLE      = LEFREEZ
    NoahowpmpIO%DILE_FC_TABLE(:,1) = DILE_FC_S1
    NoahowpmpIO%DILE_FC_TABLE(:,2) = DILE_FC_S2
    NoahowpmpIO%DILE_FC_TABLE(:,3) = DILE_FC_S3
    NoahowpmpIO%DILE_FC_TABLE(:,4) = DILE_FC_S4
    NoahowpmpIO%DILE_FC_TABLE(:,5) = DILE_FC_S5
    NoahowpmpIO%DILE_FC_TABLE(:,6) = DILE_FC_S6
    NoahowpmpIO%DILE_FC_TABLE(:,7) = DILE_FC_S7
    NoahowpmpIO%DILE_FC_TABLE(:,8) = DILE_FC_S8
    NoahowpmpIO%DILE_FW_TABLE(:,1) = DILE_FW_S1
    NoahowpmpIO%DILE_FW_TABLE(:,2) = DILE_FW_S2
    NoahowpmpIO%DILE_FW_TABLE(:,3) = DILE_FW_S3
    NoahowpmpIO%DILE_FW_TABLE(:,4) = DILE_FW_S4
    NoahowpmpIO%DILE_FW_TABLE(:,5) = DILE_FW_S5
    NoahowpmpIO%DILE_FW_TABLE(:,6) = DILE_FW_S6
    NoahowpmpIO%DILE_FW_TABLE(:,7) = DILE_FW_S7
    NoahowpmpIO%DILE_FW_TABLE(:,8) = DILE_FW_S8
    NoahowpmpIO%FRA_GR_TABLE       = FRA_GR
    NoahowpmpIO%LF_OVRC_TABLE(:,1) = LF_OVRC_S1
    NoahowpmpIO%LF_OVRC_TABLE(:,2) = LF_OVRC_S2
    NoahowpmpIO%LF_OVRC_TABLE(:,3) = LF_OVRC_S3
    NoahowpmpIO%LF_OVRC_TABLE(:,4) = LF_OVRC_S4
    NoahowpmpIO%LF_OVRC_TABLE(:,5) = LF_OVRC_S5
    NoahowpmpIO%LF_OVRC_TABLE(:,6) = LF_OVRC_S6
    NoahowpmpIO%LF_OVRC_TABLE(:,7) = LF_OVRC_S7
    NoahowpmpIO%LF_OVRC_TABLE(:,8) = LF_OVRC_S8
    NoahowpmpIO%ST_OVRC_TABLE(:,1) = ST_OVRC_S1
    NoahowpmpIO%ST_OVRC_TABLE(:,2) = ST_OVRC_S2
    NoahowpmpIO%ST_OVRC_TABLE(:,3) = ST_OVRC_S3
    NoahowpmpIO%ST_OVRC_TABLE(:,4) = ST_OVRC_S4
    NoahowpmpIO%ST_OVRC_TABLE(:,5) = ST_OVRC_S5
    NoahowpmpIO%ST_OVRC_TABLE(:,6) = ST_OVRC_S6
    NoahowpmpIO%ST_OVRC_TABLE(:,7) = ST_OVRC_S7
    NoahowpmpIO%ST_OVRC_TABLE(:,8) = ST_OVRC_S8
    NoahowpmpIO%RT_OVRC_TABLE(:,1) = RT_OVRC_S1
    NoahowpmpIO%RT_OVRC_TABLE(:,2) = RT_OVRC_S2
    NoahowpmpIO%RT_OVRC_TABLE(:,3) = RT_OVRC_S3
    NoahowpmpIO%RT_OVRC_TABLE(:,4) = RT_OVRC_S4
    NoahowpmpIO%RT_OVRC_TABLE(:,5) = RT_OVRC_S5
    NoahowpmpIO%RT_OVRC_TABLE(:,6) = RT_OVRC_S6
    NoahowpmpIO%RT_OVRC_TABLE(:,7) = RT_OVRC_S7
    NoahowpmpIO%RT_OVRC_TABLE(:,8) = RT_OVRC_S8
    NoahowpmpIO%LFMR25_TABLE       = LFMR25
    NoahowpmpIO%STMR25_TABLE       = STMR25
    NoahowpmpIO%RTMR25_TABLE       = RTMR25
    NoahowpmpIO%GRAINMR25_TABLE    = GRAINMR25
    NoahowpmpIO%LFPT_TABLE(:,1) = LFPT_S1
    NoahowpmpIO%LFPT_TABLE(:,2) = LFPT_S2
    NoahowpmpIO%LFPT_TABLE(:,3) = LFPT_S3
    NoahowpmpIO%LFPT_TABLE(:,4) = LFPT_S4
    NoahowpmpIO%LFPT_TABLE(:,5) = LFPT_S5
    NoahowpmpIO%LFPT_TABLE(:,6) = LFPT_S6
    NoahowpmpIO%LFPT_TABLE(:,7) = LFPT_S7
    NoahowpmpIO%LFPT_TABLE(:,8) = LFPT_S8
    NoahowpmpIO%STPT_TABLE(:,1) = STPT_S1
    NoahowpmpIO%STPT_TABLE(:,2) = STPT_S2
    NoahowpmpIO%STPT_TABLE(:,3) = STPT_S3
    NoahowpmpIO%STPT_TABLE(:,4) = STPT_S4
    NoahowpmpIO%STPT_TABLE(:,5) = STPT_S5
    NoahowpmpIO%STPT_TABLE(:,6) = STPT_S6
    NoahowpmpIO%STPT_TABLE(:,7) = STPT_S7
    NoahowpmpIO%STPT_TABLE(:,8) = STPT_S8
    NoahowpmpIO%RTPT_TABLE(:,1) = RTPT_S1
    NoahowpmpIO%RTPT_TABLE(:,2) = RTPT_S2
    NoahowpmpIO%RTPT_TABLE(:,3) = RTPT_S3
    NoahowpmpIO%RTPT_TABLE(:,4) = RTPT_S4
    NoahowpmpIO%RTPT_TABLE(:,5) = RTPT_S5
    NoahowpmpIO%RTPT_TABLE(:,6) = RTPT_S6
    NoahowpmpIO%RTPT_TABLE(:,7) = RTPT_S7
    NoahowpmpIO%RTPT_TABLE(:,8) = RTPT_S8
    NoahowpmpIO%GRAINPT_TABLE(:,1) = GRAINPT_S1
    NoahowpmpIO%GRAINPT_TABLE(:,2) = GRAINPT_S2
    NoahowpmpIO%GRAINPT_TABLE(:,3) = GRAINPT_S3
    NoahowpmpIO%GRAINPT_TABLE(:,4) = GRAINPT_S4
    NoahowpmpIO%GRAINPT_TABLE(:,5) = GRAINPT_S5
    NoahowpmpIO%GRAINPT_TABLE(:,6) = GRAINPT_S6
    NoahowpmpIO%GRAINPT_TABLE(:,7) = GRAINPT_S7
    NoahowpmpIO%GRAINPT_TABLE(:,8) = GRAINPT_S8
    NoahowpmpIO%LFCT_TABLE(:,1) = LFCT_S1
    NoahowpmpIO%LFCT_TABLE(:,2) = LFCT_S2
    NoahowpmpIO%LFCT_TABLE(:,3) = LFCT_S3
    NoahowpmpIO%LFCT_TABLE(:,4) = LFCT_S4
    NoahowpmpIO%LFCT_TABLE(:,5) = LFCT_S5
    NoahowpmpIO%LFCT_TABLE(:,6) = LFCT_S6
    NoahowpmpIO%LFCT_TABLE(:,7) = LFCT_S7
    NoahowpmpIO%LFCT_TABLE(:,8) = LFCT_S8
    NoahowpmpIO%STCT_TABLE(:,1) = STCT_S1
    NoahowpmpIO%STCT_TABLE(:,2) = STCT_S2
    NoahowpmpIO%STCT_TABLE(:,3) = STCT_S3
    NoahowpmpIO%STCT_TABLE(:,4) = STCT_S4
    NoahowpmpIO%STCT_TABLE(:,5) = STCT_S5
    NoahowpmpIO%STCT_TABLE(:,6) = STCT_S6
    NoahowpmpIO%STCT_TABLE(:,7) = STCT_S7
    NoahowpmpIO%STCT_TABLE(:,8) = STCT_S8
    NoahowpmpIO%RTCT_TABLE(:,1) = RTCT_S1
    NoahowpmpIO%RTCT_TABLE(:,2) = RTCT_S2
    NoahowpmpIO%RTCT_TABLE(:,3) = RTCT_S3
    NoahowpmpIO%RTCT_TABLE(:,4) = RTCT_S4
    NoahowpmpIO%RTCT_TABLE(:,5) = RTCT_S5
    NoahowpmpIO%RTCT_TABLE(:,6) = RTCT_S6
    NoahowpmpIO%RTCT_TABLE(:,7) = RTCT_S7
    NoahowpmpIO%RTCT_TABLE(:,8) = RTCT_S8
    NoahowpmpIO%BIO2LAI_TABLE   = BIO2LAI

  END SUBROUTINE read_crop_parameters

  SUBROUTINE read_irrigation_parameters(NoahowpmpIO)

    implicit none
    type(NoahowpmpIO_type),intent(inout) :: NoahowpmpIO
    integer                              :: ierr
    logical                              :: file_named

    real    :: IRR_FRAC              ! irrigation Fraction
    integer :: IRR_HAR               ! number of days before harvest date to stop irrigation
    real    :: IRR_LAI               ! Minimum lai to trigger irrigation
    real    :: IRR_MAD               ! management allowable deficit (0-1)
    real    :: FILOSS                ! fraction of flood irrigation loss (0-1)
    real    :: SPRIR_RATE            ! mm/h, sprinkler irrigation rate
    real    :: MICIR_RATE            ! mm/h, micro irrigation rate
    real    :: FIRTFAC               ! flood application rate factor
    real    :: IR_RAIN               ! maximum precipitation to stop irrigation trigger

    namelist / irrigation_parameters / IRR_FRAC, IRR_HAR, IRR_LAI, IRR_MAD, FILOSS, &
                                              SPRIR_RATE, MICIR_RATE, FIRTFAC, IR_RAIN

    inquire( file=trim(NoahowpmpIO%parameter_dir)//'/'//trim(NoahowpmpIO%noahowp_table), exist=file_named )
    if ( file_named ) then
      open(15, file=trim(NoahowpmpIO%parameter_dir)//'/'//trim(NoahowpmpIO%noahowp_table), status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if (ierr /= 0) then
        call handle_err(ierr, 'ParametersRead.f90: read_irrigation_parameters: Cannot find file MPTABLE.TBL')
    endif
    read(15,irrigation_parameters)
    close(15)

    NoahowpmpIO%IRR_FRAC_TABLE   = IRR_FRAC    ! irrigation Fraction
    NoahowpmpIO%IRR_HAR_TABLE    = IRR_HAR     ! number of days before harvest date to stop irrigation
    NoahowpmpIO%IRR_LAI_TABLE    = IRR_LAI     ! Minimum lai to trigger irrigation
    NoahowpmpIO%IRR_MAD_TABLE    = IRR_MAD     ! management allowable deficit (0-1)
    NoahowpmpIO%FILOSS_TABLE     = FILOSS      ! fraction of flood irrigation loss (0-1)
    NoahowpmpIO%SPRIR_RATE_TABLE = SPRIR_RATE  ! mm/h, sprinkler irrigation rate
    NoahowpmpIO%MICIR_RATE_TABLE = MICIR_RATE  ! mm/h, micro irrigation rate
    NoahowpmpIO%FIRTFAC_TABLE    = FIRTFAC     ! flood application rate factor
    NoahowpmpIO%IR_RAIN_TABLE    = IR_RAIN     ! maximum precipitation to stop irrigation trigger

  END SUBROUTINE read_irrigation_parameters

  SUBROUTINE read_tiledrain_parameters(NoahowpmpIO)

    implicit none
    type(NoahowpmpIO_type),intent(inout)        :: NoahowpmpIO
    integer                                     :: ierr
    logical                                     :: file_named
    real, dimension(NoahowpmpIO%MAX_SOILTYP)    :: TDSMC_FAC
    integer, dimension(NoahowpmpIO%MAX_SOILTYP) :: TD_DEPTH
    real, dimension(NoahowpmpIO%MAX_SOILTYP)    :: TD_DC
    integer                                     :: DRAIN_LAYER_OPT
    real, dimension(NoahowpmpIO%MAX_SOILTYP)    :: TD_DCOEF
    real, dimension(NoahowpmpIO%MAX_SOILTYP)    :: TD_D
    real, dimension(NoahowpmpIO%MAX_SOILTYP)    :: TD_ADEPTH
    real, dimension(NoahowpmpIO%MAX_SOILTYP)    :: TD_RADI
    real, dimension(NoahowpmpIO%MAX_SOILTYP)    :: TD_SPAC
    real, dimension(NoahowpmpIO%MAX_SOILTYP)    :: TD_DDRAIN
    real, dimension(NoahowpmpIO%MAX_SOILTYP)    :: KLAT_FAC

    namelist / tiledrain_parameters /DRAIN_LAYER_OPT,TDSMC_FAC,TD_DEPTH,TD_DC,&
                                            TD_DCOEF,TD_D,TD_ADEPTH,TD_RADI,TD_SPAC,TD_DDRAIN,&
                                            KLAT_FAC

    inquire( file=trim(NoahowpmpIO%parameter_dir)//'/'//trim(NoahowpmpIO%noahowp_table), exist=file_named )
    if ( file_named ) then
      open(15, file=trim(NoahowpmpIO%parameter_dir)//'/'//trim(NoahowpmpIO%noahowp_table), status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if (ierr /= 0) then
        call handle_err(ierr, 'ParametersRead.f90: read_tiledrain_parameters: Cannot find file MPTABLE.TBL')
    endif
    read(15,tiledrain_parameters)
    close(15)
    NoahowpmpIO%TDSMCFAC_TABLE(1:NoahowpmpIO%MAX_SOILTYP)           = TDSMC_FAC(1:NoahowpmpIO%MAX_SOILTYP)
    NoahowpmpIO%TD_DEPTH_TABLE(1:NoahowpmpIO%MAX_SOILTYP)           = TD_DEPTH(1:NoahowpmpIO%MAX_SOILTYP)
    NoahowpmpIO%DRAIN_LAYER_OPT_TABLE                               = DRAIN_LAYER_OPT
    NoahowpmpIO%TD_DC_TABLE(1:NoahowpmpIO%MAX_SOILTYP)              = TD_DC(1:NoahowpmpIO%MAX_SOILTYP)
    NoahowpmpIO%TD_DCOEF_TABLE(1:NoahowpmpIO%MAX_SOILTYP)           = TD_DCOEF(1:NoahowpmpIO%MAX_SOILTYP)
    NoahowpmpIO%TD_D_TABLE(1:NoahowpmpIO%MAX_SOILTYP)               = TD_D(1:NoahowpmpIO%MAX_SOILTYP)
    NoahowpmpIO%TD_ADEPTH_TABLE(1:NoahowpmpIO%MAX_SOILTYP)          = TD_ADEPTH(1:NoahowpmpIO%MAX_SOILTYP)
    NoahowpmpIO%TD_RADI_TABLE(1:NoahowpmpIO%MAX_SOILTYP)            = TD_RADI(1:NoahowpmpIO%MAX_SOILTYP)
    NoahowpmpIO%TD_SPAC_TABLE(1:NoahowpmpIO%MAX_SOILTYP)            = TD_SPAC(1:NoahowpmpIO%MAX_SOILTYP)
    NoahowpmpIO%TD_DDRAIN_TABLE(1:NoahowpmpIO%MAX_SOILTYP)          = TD_DDRAIN(1:NoahowpmpIO%MAX_SOILTYP)
    NoahowpmpIO%KLAT_FAC_TABLE(1:NoahowpmpIO%MAX_SOILTYP)           = KLAT_FAC(1:NoahowpmpIO%MAX_SOILTYP)

  END SUBROUTINE read_tiledrain_parameters

  SUBROUTINE read_optional_parameters(NoahowpmpIO)

    implicit none
    type(NoahowpmpIO_type),intent(inout)        :: NoahowpmpIO
    integer                                     :: ierr
    logical                                     :: file_named
    real :: sr2006_theta_1500t_a, sr2006_theta_1500t_b, sr2006_theta_1500t_c 
    real :: sr2006_theta_1500t_d, sr2006_theta_1500t_e, sr2006_theta_1500t_f        
    real :: sr2006_theta_1500t_g, sr2006_theta_1500_a, sr2006_theta_1500_b     
    real :: sr2006_theta_33t_a, sr2006_theta_33t_b, sr2006_theta_33t_c          
    real :: sr2006_theta_33t_d, sr2006_theta_33t_e, sr2006_theta_33t_f          
    real :: sr2006_theta_33t_g, sr2006_theta_33_a, sr2006_theta_33_b, sr2006_theta_33_c        
    real :: sr2006_theta_s33t_a, sr2006_theta_s33t_b, sr2006_theta_s33t_c, sr2006_theta_s33t_d   
    real :: sr2006_theta_s33t_e, sr2006_theta_s33t_f, sr2006_theta_s33t_g 
    real :: sr2006_theta_s33_a, sr2006_theta_s33_b, sr2006_psi_et_a, sr2006_psi_et_b  
    real :: sr2006_psi_et_c, sr2006_psi_et_d, sr2006_psi_et_e, sr2006_psi_et_f, sr2006_psi_et_g     
    real :: sr2006_psi_e_a, sr2006_psi_e_b, sr2006_psi_e_c, sr2006_smcmax_a, sr2006_smcmax_b      

    namelist / optional_parameters /                                      &
                sr2006_theta_1500t_a, sr2006_theta_1500t_b, sr2006_theta_1500t_c, &
                sr2006_theta_1500t_d, sr2006_theta_1500t_e, sr2006_theta_1500t_f, &
                sr2006_theta_1500t_g                                            , &
                sr2006_theta_1500_a , sr2006_theta_1500_b                       , &
                sr2006_theta_33t_a  , sr2006_theta_33t_b  , sr2006_theta_33t_c  , &
                sr2006_theta_33t_d  , sr2006_theta_33t_e  , sr2006_theta_33t_f  , &
                sr2006_theta_33t_g                                              , &
                sr2006_theta_33_a   , sr2006_theta_33_b   , sr2006_theta_33_c   , &
                sr2006_theta_s33t_a , sr2006_theta_s33t_b , sr2006_theta_s33t_c , &
                sr2006_theta_s33t_d , sr2006_theta_s33t_e , sr2006_theta_s33t_f , &
                sr2006_theta_s33t_g                                             , &
                sr2006_theta_s33_a  , sr2006_theta_s33_b                        , &
                sr2006_psi_et_a     , sr2006_psi_et_b     , sr2006_psi_et_c     , &
                sr2006_psi_et_d     , sr2006_psi_et_e     , sr2006_psi_et_f     , &
                sr2006_psi_et_g                                                 , &
                sr2006_psi_e_a      , sr2006_psi_e_b      , sr2006_psi_e_c      , &
                sr2006_smcmax_a     , sr2006_smcmax_b

    inquire( file=trim(NoahowpmpIO%parameter_dir)//'/'//trim(NoahowpmpIO%noahowp_table), exist=file_named )
    if ( file_named ) then
      open(15, file=trim(NoahowpmpIO%parameter_dir)//'/'//trim(NoahowpmpIO%noahowp_table), status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if (ierr /= 0) then
        call handle_err(ierr, 'ParametersRead.f90: read_optional_parameters: Cannot find file MPTABLE.TBL')
    endif
    read(15,optional_parameters)
    close(15)

    NoahowpmpIO%sr2006_theta_1500t_a = sr2006_theta_1500t_a
    NoahowpmpIO%sr2006_theta_1500t_b = sr2006_theta_1500t_b
    NoahowpmpIO%sr2006_theta_1500t_c = sr2006_theta_1500t_c
    NoahowpmpIO%sr2006_theta_1500t_d = sr2006_theta_1500t_d
    NoahowpmpIO%sr2006_theta_1500t_e = sr2006_theta_1500t_e
    NoahowpmpIO%sr2006_theta_1500t_f = sr2006_theta_1500t_f
    NoahowpmpIO%sr2006_theta_1500t_g = sr2006_theta_1500t_g
    NoahowpmpIO%sr2006_theta_1500_a  = sr2006_theta_1500_a
    NoahowpmpIO%sr2006_theta_1500_b  = sr2006_theta_1500_b
    NoahowpmpIO%sr2006_theta_33t_a   = sr2006_theta_33t_a
    NoahowpmpIO%sr2006_theta_33t_b   = sr2006_theta_33t_b
    NoahowpmpIO%sr2006_theta_33t_c   = sr2006_theta_33t_c
    NoahowpmpIO%sr2006_theta_33t_d   = sr2006_theta_33t_d
    NoahowpmpIO%sr2006_theta_33t_e   = sr2006_theta_33t_e
    NoahowpmpIO%sr2006_theta_33t_f   = sr2006_theta_33t_f
    NoahowpmpIO%sr2006_theta_33t_g   = sr2006_theta_33t_g
    NoahowpmpIO%sr2006_theta_33_a    = sr2006_theta_33_a
    NoahowpmpIO%sr2006_theta_33_b    = sr2006_theta_33_b
    NoahowpmpIO%sr2006_theta_33_c    = sr2006_theta_33_c
    NoahowpmpIO%sr2006_theta_s33t_a  = sr2006_theta_s33t_a
    NoahowpmpIO%sr2006_theta_s33t_b  = sr2006_theta_s33t_b
    NoahowpmpIO%sr2006_theta_s33t_c  = sr2006_theta_s33t_c
    NoahowpmpIO%sr2006_theta_s33t_d  = sr2006_theta_s33t_d
    NoahowpmpIO%sr2006_theta_s33t_e  = sr2006_theta_s33t_e
    NoahowpmpIO%sr2006_theta_s33t_f  = sr2006_theta_s33t_f
    NoahowpmpIO%sr2006_theta_s33t_g  = sr2006_theta_s33t_g
    NoahowpmpIO%sr2006_theta_s33_a   = sr2006_theta_s33_a
    NoahowpmpIO%sr2006_theta_s33_b   = sr2006_theta_s33_b
    NoahowpmpIO%sr2006_psi_et_a      = sr2006_psi_et_a
    NoahowpmpIO%sr2006_psi_et_b      = sr2006_psi_et_b
    NoahowpmpIO%sr2006_psi_et_c      = sr2006_psi_et_c
    NoahowpmpIO%sr2006_psi_et_d      = sr2006_psi_et_d
    NoahowpmpIO%sr2006_psi_et_e      = sr2006_psi_et_e
    NoahowpmpIO%sr2006_psi_et_f      = sr2006_psi_et_f
    NoahowpmpIO%sr2006_psi_et_g      = sr2006_psi_et_g
    NoahowpmpIO%sr2006_psi_e_a       = sr2006_psi_e_a
    NoahowpmpIO%sr2006_psi_e_b       = sr2006_psi_e_b
    NoahowpmpIO%sr2006_psi_e_c       = sr2006_psi_e_c
    NoahowpmpIO%sr2006_smcmax_a      = sr2006_smcmax_a
    NoahowpmpIO%sr2006_smcmax_b      = sr2006_smcmax_b

  END SUBROUTINE read_optional_parameters

  SUBROUTINE handle_err(err,message)
    implicit none
    integer,     intent(in) :: err             ! error code
    character(*),intent(in) :: message         ! error message
    if(err/=0)then
      write(*,*) 'FATAL ERROR: '//trim(message)
      call flush(6)
      stop
    endif
  END SUBROUTINE handle_err

END MODULE NoahowpmpReadTableModule