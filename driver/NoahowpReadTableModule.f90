MODULE NoahowpReadTableModule

  use NoahowpGridTypeModule
  
  implicit none
  
  contains

  subroutine TableVarsRead(noahowpgrid)

    implicit none
    type(noahowpgrid_type), intent(inout) :: noahowpgrid        

    call read_veg_parameters(noahowpgrid)
    call read_soil_parameters(noahowpgrid)
    call read_rad_parameters(noahowpgrid)
    call read_global_parameters(noahowpgrid)

  end subroutine TableVarsRead

  SUBROUTINE read_veg_parameters(noahowpgrid)

    implicit none
    type(noahowpgrid_type), intent(inout) :: noahowpgrid

    integer :: ierr
    integer :: IK,IM
    logical :: file_named
    integer :: NVEG
    character(len=256) :: VEG_DATASET_DESCRIPTION
    integer :: ISURBAN, ISWATER, ISBARREN, ISICE, ISCROP, EBLFOREST, NATURAL
    integer :: LCZ_1, LCZ_2, LCZ_3, LCZ_4, LCZ_5, LCZ_6, LCZ_7, LCZ_8, LCZ_9, LCZ_10, LCZ_11

    real, dimension(noahowpgrid%MVT) :: SAI_JAN,SAI_FEB,SAI_MAR,SAI_APR,SAI_MAY,SAI_JUN, &
                                      SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC
    real, dimension(noahowpgrid%MVT) :: LAI_JAN,LAI_FEB,LAI_MAR,LAI_APR,LAI_MAY,LAI_JUN, &
                                      LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC
    real, dimension(noahowpgrid%MVT) :: RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, &
                                      TAUL_VIS, TAUL_NIR, TAUS_VIS, TAUS_NIR
    real, dimension(noahowpgrid%MVT) :: CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, SCFFAC, XL, CWPVT, C3PSN, KC25, AKC, KO25, AKO, &
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

    inquire( file=trim(noahowpgrid%parameter_dir)//'/'//trim(noahowpgrid%noahowp_table), exist=file_named )
    if ( file_named ) then
      open(15, file=trim(noahowpgrid%parameter_dir)//'/'//trim(noahowpgrid%noahowp_table), status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if

    if (ierr /= 0) then
        call handle_err(ierr, "ParametersRead.f90: read_veg_parameters: Cannot find file MPTABLE.TBL")
    endif

    if ( trim(noahowpgrid%veg_class_name) == "USGS" ) then
        read(15,usgs_veg_categories)
        read(15,usgs_veg_parameters)
    else if ( trim(noahowpgrid%veg_class_name) == "MODIFIED_IGBP_MODIS_NOAH" ) then
        read(15,modis_veg_categories)
        read(15,modis_veg_parameters)
    else
        write(*,'("WARNING: DATASET_IDENTIFIER = ''", A, "''")') trim(noahowpgrid%veg_class_name)
        call handle_err(ierr, 'ParametersRead.f90: read_veg_parameters: Unrecognized DATASET_IDENTIFIER in subroutine read_VEG_PARAMETERS')
    endif
    close(15)

    noahowpgrid%ISURBAN_TABLE   = ISURBAN
    noahowpgrid%ISWATER_TABLE   = ISWATER
    noahowpgrid%ISBARREN_TABLE   = ISBARREN
    noahowpgrid%ISICE_TABLE   = ISICE
    noahowpgrid%ISCROP_TABLE   = ISCROP
    noahowpgrid%EBLFOREST_TABLE   = EBLFOREST
    noahowpgrid%NATURAL_TABLE   = NATURAL
    noahowpgrid%LCZ_1_TABLE   = LCZ_1
    noahowpgrid%LCZ_2_TABLE   = LCZ_2
    noahowpgrid%LCZ_3_TABLE   = LCZ_3
    noahowpgrid%LCZ_4_TABLE   = LCZ_4
    noahowpgrid%LCZ_5_TABLE   = LCZ_5
    noahowpgrid%LCZ_6_TABLE   = LCZ_6
    noahowpgrid%LCZ_7_TABLE   = LCZ_7
    noahowpgrid%LCZ_8_TABLE   = LCZ_8
    noahowpgrid%LCZ_9_TABLE   = LCZ_9
    noahowpgrid%LCZ_10_TABLE  = LCZ_10
    noahowpgrid%LCZ_11_TABLE  = LCZ_11

    noahowpgrid%CH2OP_TABLE(1:NVEG)  = CH2OP(1:NVEG)
    noahowpgrid%DLEAF_TABLE(1:NVEG)  = DLEAF(1:NVEG)
    noahowpgrid%Z0MVT_TABLE(1:NVEG)  = Z0MVT(1:NVEG)
    noahowpgrid%HVT_TABLE(1:NVEG)  = HVT(1:NVEG)
    noahowpgrid%HVB_TABLE(1:NVEG)  = HVB(1:NVEG)
    noahowpgrid%DEN_TABLE(1:NVEG)  = DEN(1:NVEG)
    noahowpgrid%RC_TABLE(1:NVEG)  = RC(1:NVEG)
    noahowpgrid%MFSNO_TABLE(1:NVEG)  = MFSNO(1:NVEG)
    noahowpgrid%SCFFAC_TABLE(1:NVEG)  = SCFFAC(1:NVEG)
    noahowpgrid%XL_TABLE(1:NVEG)  = XL(1:NVEG)
    noahowpgrid%CWPVT_TABLE(1:NVEG)  = CWPVT(1:NVEG)
    noahowpgrid%C3PSN_TABLE(1:NVEG)  = C3PSN(1:NVEG)
    noahowpgrid%KC25_TABLE(1:NVEG)  = KC25(1:NVEG)
    noahowpgrid%AKC_TABLE(1:NVEG)  = AKC(1:NVEG)
    noahowpgrid%KO25_TABLE(1:NVEG)  = KO25(1:NVEG)
    noahowpgrid%AKO_TABLE(1:NVEG)  = AKO(1:NVEG)
    noahowpgrid%AVCMX_TABLE(1:NVEG)  = AVCMX(1:NVEG)
    noahowpgrid%AQE_TABLE(1:NVEG)  = AQE(1:NVEG)
    noahowpgrid%LTOVRC_TABLE(1:NVEG)  = LTOVRC(1:NVEG)
    noahowpgrid%DILEFC_TABLE(1:NVEG)  = DILEFC(1:NVEG)
    noahowpgrid%DILEFW_TABLE(1:NVEG)  = DILEFW(1:NVEG)
    noahowpgrid%RMF25_TABLE(1:NVEG)  = RMF25(1:NVEG)
    noahowpgrid%SLA_TABLE(1:NVEG)  = SLA(1:NVEG)
    noahowpgrid%FRAGR_TABLE(1:NVEG)  = FRAGR(1:NVEG)
    noahowpgrid%TMIN_TABLE(1:NVEG)  = TMIN(1:NVEG)
    noahowpgrid%VCMX25_TABLE(1:NVEG)  = VCMX25(1:NVEG)
    noahowpgrid%TDLEF_TABLE(1:NVEG)  = TDLEF(1:NVEG)
    noahowpgrid%BP_TABLE(1:NVEG)  = BP(1:NVEG)
    noahowpgrid%MP_TABLE(1:NVEG)  = MP(1:NVEG)
    noahowpgrid%QE25_TABLE(1:NVEG)  = QE25(1:NVEG)
    noahowpgrid%RMS25_TABLE(1:NVEG)  = RMS25(1:NVEG)
    noahowpgrid%RMR25_TABLE(1:NVEG)  = RMR25(1:NVEG)
    noahowpgrid%ARM_TABLE(1:NVEG)  = ARM(1:NVEG)
    noahowpgrid%FOLNMX_TABLE(1:NVEG)  = FOLNMX(1:NVEG)
    noahowpgrid%WDPOOL_TABLE(1:NVEG)  = WDPOOL(1:NVEG)
    noahowpgrid%WRRAT_TABLE(1:NVEG)  = WRRAT(1:NVEG)
    noahowpgrid%MRP_TABLE(1:NVEG)  = MRP(1:NVEG)
    noahowpgrid%SHDFAC_TABLE(1:NVEG)  = SHDFAC(1:NVEG)
    noahowpgrid%NROOT_TABLE(1:NVEG)  = NROOT(1:NVEG)
    noahowpgrid%RGL_TABLE(1:NVEG)  = RGL(1:NVEG)
    noahowpgrid%RS_TABLE(1:NVEG)  = RS(1:NVEG)
    noahowpgrid%HS_TABLE(1:NVEG)  = HS(1:NVEG)
    noahowpgrid%TOPT_TABLE(1:NVEG)  = TOPT(1:NVEG)
    noahowpgrid%RSMAX_TABLE(1:NVEG)  = RSMAX(1:NVEG)

    ! Put LAI and SAI into 2d array from monthly lines in table; same for canopy radiation properties

    noahowpgrid%SAIM_TABLE(1:NVEG, 1) = SAI_JAN(1:NVEG)
    noahowpgrid%SAIM_TABLE(1:NVEG, 2) = SAI_FEB(1:NVEG)
    noahowpgrid%SAIM_TABLE(1:NVEG, 3) = SAI_MAR(1:NVEG)
    noahowpgrid%SAIM_TABLE(1:NVEG, 4) = SAI_APR(1:NVEG)
    noahowpgrid%SAIM_TABLE(1:NVEG, 5) = SAI_MAY(1:NVEG)
    noahowpgrid%SAIM_TABLE(1:NVEG, 6) = SAI_JUN(1:NVEG)
    noahowpgrid%SAIM_TABLE(1:NVEG, 7) = SAI_JUL(1:NVEG)
    noahowpgrid%SAIM_TABLE(1:NVEG, 8) = SAI_AUG(1:NVEG)
    noahowpgrid%SAIM_TABLE(1:NVEG, 9) = SAI_SEP(1:NVEG)
    noahowpgrid%SAIM_TABLE(1:NVEG,10) = SAI_OCT(1:NVEG)
    noahowpgrid%SAIM_TABLE(1:NVEG,11) = SAI_NOV(1:NVEG)
    noahowpgrid%SAIM_TABLE(1:NVEG,12) = SAI_DEC(1:NVEG)

    noahowpgrid%LAIM_TABLE(1:NVEG, 1) = LAI_JAN(1:NVEG)
    noahowpgrid%LAIM_TABLE(1:NVEG, 2) = LAI_FEB(1:NVEG)
    noahowpgrid%LAIM_TABLE(1:NVEG, 3) = LAI_MAR(1:NVEG)
    noahowpgrid%LAIM_TABLE(1:NVEG, 4) = LAI_APR(1:NVEG)
    noahowpgrid%LAIM_TABLE(1:NVEG, 5) = LAI_MAY(1:NVEG)
    noahowpgrid%LAIM_TABLE(1:NVEG, 6) = LAI_JUN(1:NVEG)
    noahowpgrid%LAIM_TABLE(1:NVEG, 7) = LAI_JUL(1:NVEG)
    noahowpgrid%LAIM_TABLE(1:NVEG, 8) = LAI_AUG(1:NVEG)
    noahowpgrid%LAIM_TABLE(1:NVEG, 9) = LAI_SEP(1:NVEG)
    noahowpgrid%LAIM_TABLE(1:NVEG,10) = LAI_OCT(1:NVEG)
    noahowpgrid%LAIM_TABLE(1:NVEG,11) = LAI_NOV(1:NVEG)
    noahowpgrid%LAIM_TABLE(1:NVEG,12) = LAI_DEC(1:NVEG)

    noahowpgrid%RHOL_TABLE(1:NVEG,1)  = RHOL_VIS(1:NVEG) !leaf reflectance: 1=vis, 2=nir
    noahowpgrid%RHOL_TABLE(1:NVEG,2)  = RHOL_NIR(1:NVEG) !leaf reflectance: 1=vis, 2=nir
    noahowpgrid%RHOS_TABLE(1:NVEG,1)  = RHOS_VIS(1:NVEG) !stem reflectance: 1=vis, 2=nir
    noahowpgrid%RHOS_TABLE(1:NVEG,2)  = RHOS_NIR(1:NVEG) !stem reflectance: 1=vis, 2=nir
    noahowpgrid%TAUL_TABLE(1:NVEG,1)  = TAUL_VIS(1:NVEG) !leaf transmittance: 1=vis, 2=nir
    noahowpgrid%TAUL_TABLE(1:NVEG,2)  = TAUL_NIR(1:NVEG) !leaf transmittance: 1=vis, 2=nir
    noahowpgrid%TAUS_TABLE(1:NVEG,1)  = TAUS_VIS(1:NVEG) !stem transmittance: 1=vis, 2=nir
    noahowpgrid%TAUS_TABLE(1:NVEG,2)  = TAUS_NIR(1:NVEG) !stem transmittance: 1=vis, 2=nir
    
  END SUBROUTINE read_veg_parameters

  SUBROUTINE read_soil_parameters(noahowpgrid)

    implicit none
    type(noahowpgrid_type), intent(inout) :: noahowpgrid

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
    inquire( file=trim(noahowpgrid%parameter_dir)//'/'//trim(noahowpgrid%soil_table), exist=file_named )
    if ( file_named ) then
      open(21, file=trim(noahowpgrid%parameter_dir)//'/'//trim(noahowpgrid%soil_table),form='formatted',status='old',iostat=ierr)
    else
      open(21, form='formatted',status='old',iostat=ierr)
    end if
    if (ierr/=0) then
      write(message,fmt='(A)') 'ParametersRead.f90: read_soil_parameters: failure opening SOILPARM.TBL'
      call handle_err(ierr, message)
    end if
    do iLine = 1,100
      READ (21,*) SLTYPE
      if (trim(SLTYPE) == trim(noahowpgrid%soil_class_name)) exit
    end do
    READ (21,*) SLCATS
    DO LC=1,SLCATS
      READ (21,*) ITMP,BEXP_TABLE(LC),SMCDRY_TABLE(LC),F1_TABLE(LC),SMCMAX_TABLE(LC),   &
                  SMCREF_TABLE(LC),PSISAT_TABLE(LC),DKSAT_TABLE(LC), DWSAT_TABLE(LC),   &
                  SMCWLT_TABLE(LC), QUARTZ_TABLE(LC),BVIC_TABLE(LC), AXAJ_TABLE(LC),    &
                  BXAJ_TABLE(LC),XXAJ_TABLE(LC),BDVIC_TABLE(LC),BBVIC_TABLE(LC),GDVIC_TABLE(LC)
    ENDDO
    CLOSE (21)

    ! Transfer read-in values to noahowpgrid_type
    noahowpgrid%SLCATS = SLCATS
    noahowpgrid%BEXP_TABLE(1:SLCATS) = BEXP_TABLE(1:SLCATS)
    noahowpgrid%SMCDRY_TABLE(1:SLCATS) = SMCDRY_TABLE(1:SLCATS)
    noahowpgrid%F1_TABLE(1:SLCATS) = F1_TABLE(1:SLCATS)
    noahowpgrid%SMCMAX_TABLE(1:SLCATS) = SMCMAX_TABLE(1:SLCATS)
    noahowpgrid%SMCREF_TABLE(1:SLCATS) = SMCREF_TABLE(1:SLCATS)
    noahowpgrid%PSISAT_TABLE(1:SLCATS) = PSISAT_TABLE(1:SLCATS)
    noahowpgrid%DKSAT_TABLE(1:SLCATS) = DKSAT_TABLE(1:SLCATS)
    noahowpgrid%DWSAT_TABLE(1:SLCATS) = DWSAT_TABLE(1:SLCATS)
    noahowpgrid%SMCWLT_TABLE(1:SLCATS) = SMCWLT_TABLE(1:SLCATS)
    noahowpgrid%QUARTZ_TABLE(1:SLCATS) = QUARTZ_TABLE(1:SLCATS)
    noahowpgrid%BVIC_TABLE(1:SLCATS) = BVIC_TABLE(1:SLCATS)
    noahowpgrid%AXAJ_TABLE(1:SLCATS) = AXAJ_TABLE(1:SLCATS)
    noahowpgrid%BXAJ_TABLE(1:SLCATS) = BXAJ_TABLE(1:SLCATS)
    noahowpgrid%XXAJ_TABLE(1:SLCATS) = XXAJ_TABLE(1:SLCATS)
    noahowpgrid%BDVIC_TABLE(1:SLCATS) = BDVIC_TABLE(1:SLCATS)
    noahowpgrid%BBVIC_TABLE(1:SLCATS) = BBVIC_TABLE(1:SLCATS)
    noahowpgrid%GDVIC_TABLE(1:SLCATS) = GDVIC_TABLE(1:SLCATS)

!-----READ IN GENERAL PARAMETERS FROM GENPARM.TBL
!
    inquire( file=trim(noahowpgrid%parameter_dir)//'/'//trim(noahowpgrid%general_table), exist=file_named )
    if ( file_named ) then
      open(22, file=trim(noahowpgrid%parameter_dir)//'/'//trim(noahowpgrid%general_table),form='formatted',status='old',iostat=ierr)
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
   
    ! Transfer read-in values to noahowpgrid_type
    noahowpgrid%SLOPE_TABLE(1:NUM_SLOPE) = SLOPE_TABLE(1:NUM_SLOPE)
    noahowpgrid%CSOIL_TABLE = CSOIL_TABLE
    noahowpgrid%REFDK_TABLE = REFDK_TABLE
    noahowpgrid%REFKDT_TABLE = REFKDT_TABLE
    noahowpgrid%FRZK_TABLE = FRZK_TABLE
    noahowpgrid%ZBOT_TABLE = ZBOT_TABLE
    noahowpgrid%CZIL_TABLE = CZIL_TABLE
    noahowpgrid%Z0_TABLE = Z0_TABLE

  END SUBROUTINE read_soil_parameters

  SUBROUTINE read_rad_parameters(noahowpgrid)
    
    implicit none
    type(noahowpgrid_type), intent(inout) :: noahowpgrid
    integer                               :: ierr
    logical                               :: file_named
    real :: ALBSAT_VIS(noahowpgrid%MSC)
    real :: ALBSAT_NIR(noahowpgrid%MSC)
    real :: ALBDRY_VIS(noahowpgrid%MSC)
    real :: ALBDRY_NIR(noahowpgrid%MSC)
    real :: ALBICE(noahowpgrid%MBAND)
    real :: ALBLAK(noahowpgrid%MBAND)
    real :: OMEGAS(noahowpgrid%MBAND)
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

    inquire( file=trim(noahowpgrid%parameter_dir)//'/'//trim(noahowpgrid%noahowp_table), exist=file_named )
    if ( file_named ) then
      open(15, file=trim(noahowpgrid%parameter_dir)//'/'//trim(noahowpgrid%noahowp_table), status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if (ierr /= 0) then
        call handle_err(ierr, 'ParametersRead.f90: read_rad_parameters: Cannot find file MPTABLE.TBL')
    endif
    read(15,rad_parameters)
    close(15)

    noahowpgrid%ALBSAT_TABLE(:,1) = ALBSAT_VIS ! saturated soil albedos: 1=vis, 2=nir
    noahowpgrid%ALBSAT_TABLE(:,2) = ALBSAT_NIR ! saturated soil albedos: 1=vis, 2=nir
    noahowpgrid%ALBDRY_TABLE(:,1) = ALBDRY_VIS ! dry soil albedos: 1=vis, 2=nir
    noahowpgrid%ALBDRY_TABLE(:,2) = ALBDRY_NIR ! dry soil albedos: 1=vis, 2=nir
    noahowpgrid%ALBICE_TABLE      = ALBICE
    noahowpgrid%ALBLAK_TABLE      = ALBLAK
    noahowpgrid%OMEGAS_TABLE      = OMEGAS
    noahowpgrid%BETADS_TABLE      = BETADS
    noahowpgrid%BETAIS_TABLE      = BETAIS
    noahowpgrid%EG_TABLE          = EG

  end subroutine read_rad_parameters

  subroutine read_global_parameters(noahowpgrid)

    implicit none
    type(noahowpgrid_type), intent(inout) :: noahowpgrid
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

    inquire( file=trim(noahowpgrid%parameter_dir)//'/'//trim(noahowpgrid%noahowp_table), exist=file_named )
    if ( file_named ) then
      open(15, file=trim(noahowpgrid%parameter_dir)//'/'//trim(noahowpgrid%noahowp_table), status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if (ierr /= 0) then
        call handle_err(ierr, 'ParametersRead.f90: read_global_parameters: Cannot find file MPTABLE.TBL')
    endif
    read(15,global_parameters)
    close(15)

    noahowpgrid%CO2_TABLE     = CO2
    noahowpgrid%O2_TABLE     = O2
    noahowpgrid%TIMEAN_TABLE     = TIMEAN
    noahowpgrid%FSATMX_TABLE     = FSATMX
    noahowpgrid%Z0SNO_TABLE     = Z0SNO
    noahowpgrid%SSI_TABLE     = SSI
    noahowpgrid%SNOW_RET_FAC_TABLE   = SNOW_RET_FAC
    noahowpgrid%SNOW_EMIS_TABLE   = SNOW_EMIS
    noahowpgrid%SWEMX_TABLE     = SWEMX
    noahowpgrid%TAU0_TABLE   = TAU0
    noahowpgrid%GRAIN_GROWTH_TABLE   = GRAIN_GROWTH
    noahowpgrid%EXTRA_GROWTH_TABLE   = EXTRA_GROWTH
    noahowpgrid%DIRT_SOOT_TABLE   = DIRT_SOOT
    noahowpgrid%BATS_COSZ_TABLE   = BATS_COSZ
    noahowpgrid%BATS_VIS_NEW_TABLE   = BATS_VIS_NEW
    noahowpgrid%BATS_NIR_NEW_TABLE   = BATS_NIR_NEW
    noahowpgrid%BATS_VIS_AGE_TABLE   = BATS_VIS_AGE
    noahowpgrid%BATS_NIR_AGE_TABLE   = BATS_NIR_AGE
    noahowpgrid%BATS_VIS_DIR_TABLE   = BATS_VIS_DIR
    noahowpgrid%BATS_NIR_DIR_TABLE   = BATS_NIR_DIR
    noahowpgrid%RSURF_SNOW_TABLE     = RSURF_SNOW
    noahowpgrid%RSURF_EXP_TABLE     = RSURF_EXP

  END SUBROUTINE read_global_parameters

  SUBROUTINE read_crop_parameters(noahowpgrid)

    implicit none
    type(noahowpgrid_type), intent(inout) :: noahowpgrid
    integer                      :: ierr
    logical                      :: file_named

    integer                   :: DEFAULT_CROP
    integer, dimension(noahowpgrid%NCROP) :: PLTDAY
    integer, dimension(noahowpgrid%NCROP) :: HSDAY
    real, dimension(noahowpgrid%NCROP) :: PLANTPOP
    real, dimension(noahowpgrid%NCROP) :: IRRI
    real, dimension(noahowpgrid%NCROP) :: GDDTBASE
    real, dimension(noahowpgrid%NCROP) :: GDDTCUT
    real, dimension(noahowpgrid%NCROP) :: GDDS1
    real, dimension(noahowpgrid%NCROP) :: GDDS2
    real, dimension(noahowpgrid%NCROP) :: GDDS3
    real, dimension(noahowpgrid%NCROP) :: GDDS4
    real, dimension(noahowpgrid%NCROP) :: GDDS5
    real, dimension(noahowpgrid%NCROP) :: C3PSN   ! this session copied from stomata parameters Zhe Zhang 2020-07-13
    real, dimension(noahowpgrid%NCROP) :: KC25
    real, dimension(noahowpgrid%NCROP) :: AKC
    real, dimension(noahowpgrid%NCROP) :: KO25
    real, dimension(noahowpgrid%NCROP) :: AKO
    real, dimension(noahowpgrid%NCROP) :: AVCMX
    real, dimension(noahowpgrid%NCROP) :: VCMX25
    real, dimension(noahowpgrid%NCROP) :: BP
    real, dimension(noahowpgrid%NCROP) :: MP
    real, dimension(noahowpgrid%NCROP) :: FOLNMX
    real, dimension(noahowpgrid%NCROP) :: QE25    ! until here
    integer, dimension(noahowpgrid%NCROP) :: C3C4
    real, dimension(noahowpgrid%NCROP) :: AREF
    real, dimension(noahowpgrid%NCROP) :: PSNRF
    real, dimension(noahowpgrid%NCROP) :: I2PAR
    real, dimension(noahowpgrid%NCROP) :: TASSIM0
    real, dimension(noahowpgrid%NCROP) :: TASSIM1
    real, dimension(noahowpgrid%NCROP) :: TASSIM2
    real, dimension(noahowpgrid%NCROP) :: K
    real, dimension(noahowpgrid%NCROP) :: EPSI
    real, dimension(noahowpgrid%NCROP) :: Q10MR
    real, dimension(noahowpgrid%NCROP) :: FOLN_MX
    real, dimension(noahowpgrid%NCROP) :: LEFREEZ
    real, dimension(noahowpgrid%NCROP) :: DILE_FC_S1,DILE_FC_S2,DILE_FC_S3,DILE_FC_S4,DILE_FC_S5,DILE_FC_S6,DILE_FC_S7,DILE_FC_S8
    real, dimension(noahowpgrid%NCROP) :: DILE_FW_S1,DILE_FW_S2,DILE_FW_S3,DILE_FW_S4,DILE_FW_S5,DILE_FW_S6,DILE_FW_S7,DILE_FW_S8
    real, dimension(noahowpgrid%NCROP) :: FRA_GR
    real, dimension(noahowpgrid%NCROP) :: LF_OVRC_S1,LF_OVRC_S2,LF_OVRC_S3,LF_OVRC_S4,LF_OVRC_S5,LF_OVRC_S6,LF_OVRC_S7,LF_OVRC_S8
    real, dimension(noahowpgrid%NCROP) :: ST_OVRC_S1,ST_OVRC_S2,ST_OVRC_S3,ST_OVRC_S4,ST_OVRC_S5,ST_OVRC_S6,ST_OVRC_S7,ST_OVRC_S8
    real, dimension(noahowpgrid%NCROP) :: RT_OVRC_S1,RT_OVRC_S2,RT_OVRC_S3,RT_OVRC_S4,RT_OVRC_S5,RT_OVRC_S6,RT_OVRC_S7,RT_OVRC_S8
    real, dimension(noahowpgrid%NCROP) :: LFMR25
    real, dimension(noahowpgrid%NCROP) :: STMR25
    real, dimension(noahowpgrid%NCROP) :: RTMR25
    real, dimension(noahowpgrid%NCROP) :: GRAINMR25
    real, dimension(noahowpgrid%NCROP) :: LFPT_S1,LFPT_S2,LFPT_S3,LFPT_S4,LFPT_S5,LFPT_S6,LFPT_S7,LFPT_S8
    real, dimension(noahowpgrid%NCROP) :: STPT_S1,STPT_S2,STPT_S3,STPT_S4,STPT_S5,STPT_S6,STPT_S7,STPT_S8
    real, dimension(noahowpgrid%NCROP) :: RTPT_S1,RTPT_S2,RTPT_S3,RTPT_S4,RTPT_S5,RTPT_S6,RTPT_S7,RTPT_S8
    real, dimension(noahowpgrid%NCROP) :: GRAINPT_S1,GRAINPT_S2,GRAINPT_S3,GRAINPT_S4,GRAINPT_S5,GRAINPT_S6,GRAINPT_S7,GRAINPT_S8
    real, dimension(noahowpgrid%NCROP) :: LFCT_S1,LFCT_S2,LFCT_S3,LFCT_S4,LFCT_S5,LFCT_S6,LFCT_S7,LFCT_S8
    real, dimension(noahowpgrid%NCROP) :: STCT_S1,STCT_S2,STCT_S3,STCT_S4,STCT_S5,STCT_S6,STCT_S7,STCT_S8
    real, dimension(noahowpgrid%NCROP) :: RTCT_S1,RTCT_S2,RTCT_S3,RTCT_S4,RTCT_S5,RTCT_S6,RTCT_S7,RTCT_S8
    real, dimension(noahowpgrid%NCROP) :: BIO2LAI


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

    inquire( file=trim(noahowpgrid%parameter_dir)//'/'//trim(noahowpgrid%noahowp_table), exist=file_named )
    if ( file_named ) then
      open(15, file=trim(noahowpgrid%parameter_dir)//'/'//trim(noahowpgrid%noahowp_table), status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if (ierr /= 0) then
        call handle_err(ierr, 'ParametersRead.f90: read_crop_parameters: Cannot find file MPTABLE.TBL')
    endif
    read(15,crop_parameters)
    close(15)

    noahowpgrid%DEFAULT_CROP_TABLE      = DEFAULT_CROP
    noahowpgrid%PLTDAY_TABLE      = PLTDAY
    noahowpgrid%HSDAY_TABLE      = HSDAY
    noahowpgrid%PLANTPOP_TABLE      = PLANTPOP
    noahowpgrid%IRRI_TABLE      = IRRI
    noahowpgrid%GDDTBASE_TABLE      = GDDTBASE
    noahowpgrid%GDDTCUT_TABLE      = GDDTCUT
    noahowpgrid%GDDS1_TABLE      = GDDS1
    noahowpgrid%GDDS2_TABLE      = GDDS2
    noahowpgrid%GDDS3_TABLE      = GDDS3
    noahowpgrid%GDDS4_TABLE      = GDDS4
    noahowpgrid%GDDS5_TABLE      = GDDS5
    noahowpgrid%C3PSNI_TABLE(1:5) = C3PSN(1:5)  ! parameters from stomata ! Zhe Zhang 2020-07-13
    noahowpgrid%KC25I_TABLE(1:5) = KC25(1:5)
    noahowpgrid%AKCI_TABLE(1:5) = AKC(1:5)
    noahowpgrid%KO25I_TABLE(1:5) = KO25(1:5)
    noahowpgrid%AKOI_TABLE(1:5) = AKO(1:5)
    noahowpgrid%AVCMXI_TABLE(1:5) = AVCMX(1:5)
    noahowpgrid%VCMX25I_TABLE(1:5) = VCMX25(1:5)
    noahowpgrid%BPI_TABLE(1:5) = BP(1:5)
    noahowpgrid%MPI_TABLE(1:5) = MP(1:5)
    noahowpgrid%FOLNMXI_TABLE(1:5) = FOLNMX(1:5)
    noahowpgrid%QE25I_TABLE(1:5) = QE25(1:5)   ! ends here
    noahowpgrid%C3C4_TABLE      = C3C4
    noahowpgrid%AREF_TABLE      = AREF
    noahowpgrid%PSNRF_TABLE      = PSNRF
    noahowpgrid%I2PAR_TABLE      = I2PAR
    noahowpgrid%TASSIM0_TABLE      = TASSIM0
    noahowpgrid%TASSIM1_TABLE      = TASSIM1
    noahowpgrid%TASSIM2_TABLE      = TASSIM2
    noahowpgrid%K_TABLE      = K
    noahowpgrid%EPSI_TABLE      = EPSI
    noahowpgrid%Q10MR_TABLE      = Q10MR
    noahowpgrid%FOLN_MX_TABLE      = FOLN_MX
    noahowpgrid%LEFREEZ_TABLE      = LEFREEZ
    noahowpgrid%DILE_FC_TABLE(:,1) = DILE_FC_S1
    noahowpgrid%DILE_FC_TABLE(:,2) = DILE_FC_S2
    noahowpgrid%DILE_FC_TABLE(:,3) = DILE_FC_S3
    noahowpgrid%DILE_FC_TABLE(:,4) = DILE_FC_S4
    noahowpgrid%DILE_FC_TABLE(:,5) = DILE_FC_S5
    noahowpgrid%DILE_FC_TABLE(:,6) = DILE_FC_S6
    noahowpgrid%DILE_FC_TABLE(:,7) = DILE_FC_S7
    noahowpgrid%DILE_FC_TABLE(:,8) = DILE_FC_S8
    noahowpgrid%DILE_FW_TABLE(:,1) = DILE_FW_S1
    noahowpgrid%DILE_FW_TABLE(:,2) = DILE_FW_S2
    noahowpgrid%DILE_FW_TABLE(:,3) = DILE_FW_S3
    noahowpgrid%DILE_FW_TABLE(:,4) = DILE_FW_S4
    noahowpgrid%DILE_FW_TABLE(:,5) = DILE_FW_S5
    noahowpgrid%DILE_FW_TABLE(:,6) = DILE_FW_S6
    noahowpgrid%DILE_FW_TABLE(:,7) = DILE_FW_S7
    noahowpgrid%DILE_FW_TABLE(:,8) = DILE_FW_S8
    noahowpgrid%FRA_GR_TABLE       = FRA_GR
    noahowpgrid%LF_OVRC_TABLE(:,1) = LF_OVRC_S1
    noahowpgrid%LF_OVRC_TABLE(:,2) = LF_OVRC_S2
    noahowpgrid%LF_OVRC_TABLE(:,3) = LF_OVRC_S3
    noahowpgrid%LF_OVRC_TABLE(:,4) = LF_OVRC_S4
    noahowpgrid%LF_OVRC_TABLE(:,5) = LF_OVRC_S5
    noahowpgrid%LF_OVRC_TABLE(:,6) = LF_OVRC_S6
    noahowpgrid%LF_OVRC_TABLE(:,7) = LF_OVRC_S7
    noahowpgrid%LF_OVRC_TABLE(:,8) = LF_OVRC_S8
    noahowpgrid%ST_OVRC_TABLE(:,1) = ST_OVRC_S1
    noahowpgrid%ST_OVRC_TABLE(:,2) = ST_OVRC_S2
    noahowpgrid%ST_OVRC_TABLE(:,3) = ST_OVRC_S3
    noahowpgrid%ST_OVRC_TABLE(:,4) = ST_OVRC_S4
    noahowpgrid%ST_OVRC_TABLE(:,5) = ST_OVRC_S5
    noahowpgrid%ST_OVRC_TABLE(:,6) = ST_OVRC_S6
    noahowpgrid%ST_OVRC_TABLE(:,7) = ST_OVRC_S7
    noahowpgrid%ST_OVRC_TABLE(:,8) = ST_OVRC_S8
    noahowpgrid%RT_OVRC_TABLE(:,1) = RT_OVRC_S1
    noahowpgrid%RT_OVRC_TABLE(:,2) = RT_OVRC_S2
    noahowpgrid%RT_OVRC_TABLE(:,3) = RT_OVRC_S3
    noahowpgrid%RT_OVRC_TABLE(:,4) = RT_OVRC_S4
    noahowpgrid%RT_OVRC_TABLE(:,5) = RT_OVRC_S5
    noahowpgrid%RT_OVRC_TABLE(:,6) = RT_OVRC_S6
    noahowpgrid%RT_OVRC_TABLE(:,7) = RT_OVRC_S7
    noahowpgrid%RT_OVRC_TABLE(:,8) = RT_OVRC_S8
    noahowpgrid%LFMR25_TABLE       = LFMR25
    noahowpgrid%STMR25_TABLE       = STMR25
    noahowpgrid%RTMR25_TABLE       = RTMR25
    noahowpgrid%GRAINMR25_TABLE    = GRAINMR25
    noahowpgrid%LFPT_TABLE(:,1) = LFPT_S1
    noahowpgrid%LFPT_TABLE(:,2) = LFPT_S2
    noahowpgrid%LFPT_TABLE(:,3) = LFPT_S3
    noahowpgrid%LFPT_TABLE(:,4) = LFPT_S4
    noahowpgrid%LFPT_TABLE(:,5) = LFPT_S5
    noahowpgrid%LFPT_TABLE(:,6) = LFPT_S6
    noahowpgrid%LFPT_TABLE(:,7) = LFPT_S7
    noahowpgrid%LFPT_TABLE(:,8) = LFPT_S8
    noahowpgrid%STPT_TABLE(:,1) = STPT_S1
    noahowpgrid%STPT_TABLE(:,2) = STPT_S2
    noahowpgrid%STPT_TABLE(:,3) = STPT_S3
    noahowpgrid%STPT_TABLE(:,4) = STPT_S4
    noahowpgrid%STPT_TABLE(:,5) = STPT_S5
    noahowpgrid%STPT_TABLE(:,6) = STPT_S6
    noahowpgrid%STPT_TABLE(:,7) = STPT_S7
    noahowpgrid%STPT_TABLE(:,8) = STPT_S8
    noahowpgrid%RTPT_TABLE(:,1) = RTPT_S1
    noahowpgrid%RTPT_TABLE(:,2) = RTPT_S2
    noahowpgrid%RTPT_TABLE(:,3) = RTPT_S3
    noahowpgrid%RTPT_TABLE(:,4) = RTPT_S4
    noahowpgrid%RTPT_TABLE(:,5) = RTPT_S5
    noahowpgrid%RTPT_TABLE(:,6) = RTPT_S6
    noahowpgrid%RTPT_TABLE(:,7) = RTPT_S7
    noahowpgrid%RTPT_TABLE(:,8) = RTPT_S8
    noahowpgrid%GRAINPT_TABLE(:,1) = GRAINPT_S1
    noahowpgrid%GRAINPT_TABLE(:,2) = GRAINPT_S2
    noahowpgrid%GRAINPT_TABLE(:,3) = GRAINPT_S3
    noahowpgrid%GRAINPT_TABLE(:,4) = GRAINPT_S4
    noahowpgrid%GRAINPT_TABLE(:,5) = GRAINPT_S5
    noahowpgrid%GRAINPT_TABLE(:,6) = GRAINPT_S6
    noahowpgrid%GRAINPT_TABLE(:,7) = GRAINPT_S7
    noahowpgrid%GRAINPT_TABLE(:,8) = GRAINPT_S8
    noahowpgrid%LFCT_TABLE(:,1) = LFCT_S1
    noahowpgrid%LFCT_TABLE(:,2) = LFCT_S2
    noahowpgrid%LFCT_TABLE(:,3) = LFCT_S3
    noahowpgrid%LFCT_TABLE(:,4) = LFCT_S4
    noahowpgrid%LFCT_TABLE(:,5) = LFCT_S5
    noahowpgrid%LFCT_TABLE(:,6) = LFCT_S6
    noahowpgrid%LFCT_TABLE(:,7) = LFCT_S7
    noahowpgrid%LFCT_TABLE(:,8) = LFCT_S8
    noahowpgrid%STCT_TABLE(:,1) = STCT_S1
    noahowpgrid%STCT_TABLE(:,2) = STCT_S2
    noahowpgrid%STCT_TABLE(:,3) = STCT_S3
    noahowpgrid%STCT_TABLE(:,4) = STCT_S4
    noahowpgrid%STCT_TABLE(:,5) = STCT_S5
    noahowpgrid%STCT_TABLE(:,6) = STCT_S6
    noahowpgrid%STCT_TABLE(:,7) = STCT_S7
    noahowpgrid%STCT_TABLE(:,8) = STCT_S8
    noahowpgrid%RTCT_TABLE(:,1) = RTCT_S1
    noahowpgrid%RTCT_TABLE(:,2) = RTCT_S2
    noahowpgrid%RTCT_TABLE(:,3) = RTCT_S3
    noahowpgrid%RTCT_TABLE(:,4) = RTCT_S4
    noahowpgrid%RTCT_TABLE(:,5) = RTCT_S5
    noahowpgrid%RTCT_TABLE(:,6) = RTCT_S6
    noahowpgrid%RTCT_TABLE(:,7) = RTCT_S7
    noahowpgrid%RTCT_TABLE(:,8) = RTCT_S8
    noahowpgrid%BIO2LAI_TABLE   = BIO2LAI

  END SUBROUTINE read_crop_parameters

  SUBROUTINE read_irrigation_parameters(noahowpgrid)

    implicit none
    type(noahowpgrid_type), intent(inout) :: noahowpgrid
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

    inquire( file=trim(noahowpgrid%parameter_dir)//'/'//trim(noahowpgrid%noahowp_table), exist=file_named )
    if ( file_named ) then
      open(15, file=trim(noahowpgrid%parameter_dir)//'/'//trim(noahowpgrid%noahowp_table), status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if (ierr /= 0) then
        call handle_err(ierr, 'ParametersRead.f90: read_irrigation_parameters: Cannot find file MPTABLE.TBL')
    endif
    read(15,irrigation_parameters)
    close(15)

    noahowpgrid%IRR_FRAC_TABLE   = IRR_FRAC    ! irrigation Fraction
    noahowpgrid%IRR_HAR_TABLE    = IRR_HAR     ! number of days before harvest date to stop irrigation
    noahowpgrid%IRR_LAI_TABLE    = IRR_LAI     ! Minimum lai to trigger irrigation
    noahowpgrid%IRR_MAD_TABLE    = IRR_MAD     ! management allowable deficit (0-1)
    noahowpgrid%FILOSS_TABLE     = FILOSS      ! fraction of flood irrigation loss (0-1)
    noahowpgrid%SPRIR_RATE_TABLE = SPRIR_RATE  ! mm/h, sprinkler irrigation rate
    noahowpgrid%MICIR_RATE_TABLE = MICIR_RATE  ! mm/h, micro irrigation rate
    noahowpgrid%FIRTFAC_TABLE    = FIRTFAC     ! flood application rate factor
    noahowpgrid%IR_RAIN_TABLE    = IR_RAIN     ! maximum precipitation to stop irrigation trigger

  END SUBROUTINE read_irrigation_parameters

  SUBROUTINE read_tiledrain_parameters(noahowpgrid)

    implicit none
    type(noahowpgrid_type), intent(inout)       :: noahowpgrid
    integer                                     :: ierr
    logical                                     :: file_named
    real, dimension(noahowpgrid%MAX_SOILTYP)    :: TDSMC_FAC
    integer, dimension(noahowpgrid%MAX_SOILTYP) :: TD_DEPTH
    real, dimension(noahowpgrid%MAX_SOILTYP)    :: TD_DC
    integer                                     :: DRAIN_LAYER_OPT
    real, dimension(noahowpgrid%MAX_SOILTYP)    :: TD_DCOEF
    real, dimension(noahowpgrid%MAX_SOILTYP)    :: TD_D
    real, dimension(noahowpgrid%MAX_SOILTYP)    :: TD_ADEPTH
    real, dimension(noahowpgrid%MAX_SOILTYP)    :: TD_RADI
    real, dimension(noahowpgrid%MAX_SOILTYP)    :: TD_SPAC
    real, dimension(noahowpgrid%MAX_SOILTYP)    :: TD_DDRAIN
    real, dimension(noahowpgrid%MAX_SOILTYP)    :: KLAT_FAC

    namelist / tiledrain_parameters /DRAIN_LAYER_OPT,TDSMC_FAC,TD_DEPTH,TD_DC,&
                                            TD_DCOEF,TD_D,TD_ADEPTH,TD_RADI,TD_SPAC,TD_DDRAIN,&
                                            KLAT_FAC

    inquire( file=trim(noahowpgrid%parameter_dir)//'/'//trim(noahowpgrid%noahowp_table), exist=file_named )
    if ( file_named ) then
      open(15, file=trim(noahowpgrid%parameter_dir)//'/'//trim(noahowpgrid%noahowp_table), status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if (ierr /= 0) then
        call handle_err(ierr, 'ParametersRead.f90: read_tiledrain_parameters: Cannot find file MPTABLE.TBL')
    endif
    read(15,tiledrain_parameters)
    close(15)
    noahowpgrid%TDSMCFAC_TABLE(1:noahowpgrid%MAX_SOILTYP)           = TDSMC_FAC(1:noahowpgrid%MAX_SOILTYP)
    noahowpgrid%TD_DEPTH_TABLE(1:noahowpgrid%MAX_SOILTYP)           = TD_DEPTH(1:noahowpgrid%MAX_SOILTYP)
    noahowpgrid%DRAIN_LAYER_OPT_TABLE                               = DRAIN_LAYER_OPT
    noahowpgrid%TD_DC_TABLE(1:noahowpgrid%MAX_SOILTYP)              = TD_DC(1:noahowpgrid%MAX_SOILTYP)
    noahowpgrid%TD_DCOEF_TABLE(1:noahowpgrid%MAX_SOILTYP)           = TD_DCOEF(1:noahowpgrid%MAX_SOILTYP)
    noahowpgrid%TD_D_TABLE(1:noahowpgrid%MAX_SOILTYP)               = TD_D(1:noahowpgrid%MAX_SOILTYP)
    noahowpgrid%TD_ADEPTH_TABLE(1:noahowpgrid%MAX_SOILTYP)          = TD_ADEPTH(1:noahowpgrid%MAX_SOILTYP)
    noahowpgrid%TD_RADI_TABLE(1:noahowpgrid%MAX_SOILTYP)            = TD_RADI(1:noahowpgrid%MAX_SOILTYP)
    noahowpgrid%TD_SPAC_TABLE(1:noahowpgrid%MAX_SOILTYP)            = TD_SPAC(1:noahowpgrid%MAX_SOILTYP)
    noahowpgrid%TD_DDRAIN_TABLE(1:noahowpgrid%MAX_SOILTYP)          = TD_DDRAIN(1:noahowpgrid%MAX_SOILTYP)
    noahowpgrid%KLAT_FAC_TABLE(1:noahowpgrid%MAX_SOILTYP)           = KLAT_FAC(1:noahowpgrid%MAX_SOILTYP)

  END SUBROUTINE read_tiledrain_parameters

  SUBROUTINE read_optional_parameters(noahowpgrid)

    implicit none
    type(noahowpgrid_type), intent(inout)       :: noahowpgrid
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

    inquire( file=trim(noahowpgrid%parameter_dir)//'/'//trim(noahowpgrid%noahowp_table), exist=file_named )
    if ( file_named ) then
      open(15, file=trim(noahowpgrid%parameter_dir)//'/'//trim(noahowpgrid%noahowp_table), status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if
    if (ierr /= 0) then
        call handle_err(ierr, 'ParametersRead.f90: read_optional_parameters: Cannot find file MPTABLE.TBL')
    endif
    read(15,optional_parameters)
    close(15)

    noahowpgrid%sr2006_theta_1500t_a = sr2006_theta_1500t_a
    noahowpgrid%sr2006_theta_1500t_b = sr2006_theta_1500t_b
    noahowpgrid%sr2006_theta_1500t_c = sr2006_theta_1500t_c
    noahowpgrid%sr2006_theta_1500t_d = sr2006_theta_1500t_d
    noahowpgrid%sr2006_theta_1500t_e = sr2006_theta_1500t_e
    noahowpgrid%sr2006_theta_1500t_f = sr2006_theta_1500t_f
    noahowpgrid%sr2006_theta_1500t_g = sr2006_theta_1500t_g
    noahowpgrid%sr2006_theta_1500_a  = sr2006_theta_1500_a
    noahowpgrid%sr2006_theta_1500_b  = sr2006_theta_1500_b
    noahowpgrid%sr2006_theta_33t_a   = sr2006_theta_33t_a
    noahowpgrid%sr2006_theta_33t_b   = sr2006_theta_33t_b
    noahowpgrid%sr2006_theta_33t_c   = sr2006_theta_33t_c
    noahowpgrid%sr2006_theta_33t_d   = sr2006_theta_33t_d
    noahowpgrid%sr2006_theta_33t_e   = sr2006_theta_33t_e
    noahowpgrid%sr2006_theta_33t_f   = sr2006_theta_33t_f
    noahowpgrid%sr2006_theta_33t_g   = sr2006_theta_33t_g
    noahowpgrid%sr2006_theta_33_a    = sr2006_theta_33_a
    noahowpgrid%sr2006_theta_33_b    = sr2006_theta_33_b
    noahowpgrid%sr2006_theta_33_c    = sr2006_theta_33_c
    noahowpgrid%sr2006_theta_s33t_a  = sr2006_theta_s33t_a
    noahowpgrid%sr2006_theta_s33t_b  = sr2006_theta_s33t_b
    noahowpgrid%sr2006_theta_s33t_c  = sr2006_theta_s33t_c
    noahowpgrid%sr2006_theta_s33t_d  = sr2006_theta_s33t_d
    noahowpgrid%sr2006_theta_s33t_e  = sr2006_theta_s33t_e
    noahowpgrid%sr2006_theta_s33t_f  = sr2006_theta_s33t_f
    noahowpgrid%sr2006_theta_s33t_g  = sr2006_theta_s33t_g
    noahowpgrid%sr2006_theta_s33_a   = sr2006_theta_s33_a
    noahowpgrid%sr2006_theta_s33_b   = sr2006_theta_s33_b
    noahowpgrid%sr2006_psi_et_a      = sr2006_psi_et_a
    noahowpgrid%sr2006_psi_et_b      = sr2006_psi_et_b
    noahowpgrid%sr2006_psi_et_c      = sr2006_psi_et_c
    noahowpgrid%sr2006_psi_et_d      = sr2006_psi_et_d
    noahowpgrid%sr2006_psi_et_e      = sr2006_psi_et_e
    noahowpgrid%sr2006_psi_et_f      = sr2006_psi_et_f
    noahowpgrid%sr2006_psi_et_g      = sr2006_psi_et_g
    noahowpgrid%sr2006_psi_e_a       = sr2006_psi_e_a
    noahowpgrid%sr2006_psi_e_b       = sr2006_psi_e_b
    noahowpgrid%sr2006_psi_e_c       = sr2006_psi_e_c
    noahowpgrid%sr2006_smcmax_a      = sr2006_smcmax_a
    noahowpgrid%sr2006_smcmax_b      = sr2006_smcmax_b

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

END MODULE NoahowpReadTableModule