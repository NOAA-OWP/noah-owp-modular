module NamelistRead

implicit none
save
private

type, public :: namelist_type

  real          :: dt                 ! model timestep (s)
  integer       :: maxtime
  character*256 :: output_filename    ! name of the output file
  real          :: lat                ! latitude (°)
  real          :: lon                ! longitude (°)
  real          :: preciprate         ! precipitation rate 
  integer       :: precip_duration    ! duration of precipitation event (# of timesteps)
  integer       :: dry_duration       ! duration of dry event (# of timesteps)
  logical       :: precipitating      ! logical flag for when it is precipitating
  real          :: ZREF               ! measurement height for wind speed (m)
  
  integer       :: isltyp
  integer       :: nsoil
  integer       :: nsnow
  integer       :: structure_option
  real          :: soil_depth
  integer       :: vegtyp
  integer       :: croptype
  integer       :: sfctyp
  real          :: uwind
  real          :: vwind

  real, allocatable, dimension(:) :: zsoil   ! depth of layer-bottom from soil surface
  real, allocatable, dimension(:) :: dzsnso  ! snow/soil layer thickness [m]
  real, allocatable, dimension(:) :: sice    ! soil ice content [m3/m3]
  real, allocatable, dimension(:) :: sh2o    ! soil liquid water content [m3/m3]

  logical :: initial_uniform                 ! initial all levels the same
  real    :: initial_sh2o_value              ! constant sh2o value
  real    :: initial_sice_value              ! constant sice value

  !--------------------!
  !   model options    !
  !--------------------!

  integer       :: precip_phase_option
  integer       :: runoff_option
  integer       :: drainage_option
  integer       :: frozen_soil_option
  integer       :: dynamic_vic_option
  integer       :: dynamic_veg_option

  !--------------------!
  !  soil parameters   !
  !--------------------!

  real, dimension(12) ::      bb  ! b parameter
  real, dimension(12) ::   satdk  ! conductivity at saturation
  real, dimension(12) ::   satdw  ! diffusivity at saturation
  real, dimension(12) ::  maxsmc  ! porosity
  real, dimension(12) ::  satpsi  ! matric potential at saturation
  real, dimension(12) ::  wltsmc  ! wilting point
  real, dimension(12) ::  refsmc  ! field capacity
  real, dimension(12) :: pctsand  ! percent sand
  real, dimension(12) :: pctclay  ! percent clay
  real, dimension(12) ::   bvic   ! VIC or DVIC model infiltration parameter
  real, dimension(12) ::   AXAJ   ! Xinanjiang: Tension water distribution inflection parameter [-]
  real, dimension(12) ::   BXAJ   ! Xinanjiang: Tension water distribution shape parameter [-]
  real, dimension(12) ::   XXAJ   ! Xinanjiang: Free water distribution shape parameter [-]
  real, dimension(12) ::   G      ! Mean Capillary Drive (m) for infiltration models
  real, dimension(12) ::   BBVIC  ! DVIC heterogeniety parameter for infiltration 
  real, dimension(12) ::   QUARTZ ! fraction of soil comprised of quartz [-] (equal to pctsand/100)
  real                ::   slope  ! free drainage parameter
  real                ::  refkdt  ! infiltration parameter for Schaake scheme
  real                ::   refdk  ! reference diffusivity for Schaake scheme
  real                ::   csoil  ! volumetric soil heat capacity [j/m3/K]
  real                ::   Z0     ! bare soil roughness length (m)

  !--------------------!
  !  Vegetation parameters   !
  !--------------------!
  real, dimension(20)     ::   LAI_JAN,LAI_FEB,LAI_MAR,LAI_APR,LAI_MAY,LAI_JUN, &
                                     LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC ! lai by month & vegtyp
  real, dimension(20)     ::   SAI_JAN,SAI_FEB,SAI_MAR,SAI_APR,SAI_MAY,SAI_JUN, &
                                     SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC ! sai by month & vegtyp
  real, dimension(20)     ::   CH2OP
  real, dimension(20)     ::   NROOT
  real, dimension(20)     ::   HVT            ! canopy top height (m)
  real, dimension(20)     ::   HVB            ! canopy bottom height (m)
  real, dimension(20)     ::   TMIN           ! minimum temperature for photosynthesis (k)
  real, dimension(20)     ::   SHDFAC         ! fraction of surface covered by vegetation (dimensionless, 0.0 to 1.0)
  real, dimension(20)     ::   SHDMAX         ! annual maximum fraction of surface covered by vegetation (dimensionless, 0.0 to 1.0)
  real, dimension(20)     ::   Z0MVT          ! momentum roughness length (m)
  real                    ::   CWP            ! canopy wind absorption coefficient (formerly CWPVT)
  real, dimension(20,12)  ::   LAIM_TABLE     ! monthly leaf area index, one-sided
  real, dimension(20,12)  ::   SAIM_TABLE     ! monthly stem area index, one-sided
  

  !--------------------!
  !  snow parameters   !
  !--------------------!
  real                ::   SSI     ! liquid water holding capacity of snowpack (m3/m3)
  real                ::   MFSNO   ! fractional snow covered area (FSNO) curve parameter
  real                ::   Z0SNO   ! snow surface roughness length (m)
  
  !--------------------!
  !  land parameters   !
  !--------------------!
  integer       :: ISURBAN                   ! vegtype code for urban land cover
  integer       :: ISWATER                   ! vegtype code for water
  integer       :: ISBARREN                  ! vegtype code for barren land cover
  integer       :: ISICE                     ! vegtype code for ice/snow land cover
  integer       :: ISCROP                    ! vegtype code for crop land cover
  integer       :: EBLFOREST                 ! vegtype code for evergreen broadleaf forest
  integer       :: NATURAL                   ! vegtype code for cropland/grassland mosaic
  integer       :: LOW_DENSITY_RESIDENTIAL   ! vegtype code for low density residential
  integer       :: HIGH_DENSITY_RESIDENTIAL  ! vegtype code for high density residential
  integer       :: HIGH_INTENSITY_INDUSTRIAL ! vegtype code for high density industrial

  contains

    procedure, public  :: ReadNamelist         

end type namelist_type
     
contains   

  subroutine ReadNamelist(this)
  
    class(namelist_type) :: this
    
    integer       :: iz

    real          :: dt
    integer       :: maxtime
    character*256 :: output_filename
    real          :: lat
    real          :: lon
    real          :: preciprate
    integer       :: precip_duration
    integer       :: dry_duration
    logical       :: precipitating
    real          :: ZREF               ! measurement height for wind speed (m)
    
    integer       :: isltyp
    integer       :: nsoil
    integer       :: nsnow
    integer       :: structure_option
    real          :: soil_depth
    integer       :: vegtyp
    integer       :: croptype
    integer       :: sfctyp
    real          :: uwind
    real          :: vwind

    real, allocatable, dimension(:) :: zsoil   ! depth of layer-bottom from soil surface
    real, allocatable, dimension(:) :: dzsnso  ! snow/soil layer thickness [m]
    real, allocatable, dimension(:) :: sice    ! soil ice content [m3/m3]
    real, allocatable, dimension(:) :: sh2o    ! soil liquid water content [m3/m3]
 
    logical :: initial_uniform                 ! initial all levels the same
    real    :: initial_sh2o_value              ! constant sh2o value
    real    :: initial_sice_value              ! constant sice value

    !--------------------!
    !   model options    !
    !--------------------!

    integer       :: precip_phase_option
    integer       :: runoff_option
    integer       :: drainage_option
    integer       :: frozen_soil_option
    integer       :: dynamic_vic_option
    integer       :: dynamic_veg_option

    !--------------------!
    !  soil parameters   !
    !--------------------!

    real, dimension(12) ::      bb  ! b parameter
    real, dimension(12) ::   satdk  ! conductivity at saturation
    real, dimension(12) ::   satdw  ! diffusivity at saturation
    real, dimension(12) ::  maxsmc  ! porosity
    real, dimension(12) ::  satpsi  ! matric potential at saturation
    real, dimension(12) ::  wltsmc  ! wilting point
    real, dimension(12) ::  refsmc  ! field capacity
    real, dimension(12) :: pctsand  ! percent sand
    real, dimension(12) :: pctclay  ! percent clay
    real, dimension(12) ::   bvic   !VIC or DVIC model infiltration parameter
    real, dimension(12) ::   AXAJ   !Xinanjiang: Tension water distribution inflection parameter [-]
    real, dimension(12) ::   BXAJ   !Xinanjiang: Tension water distribution shape parameter [-]
    real, dimension(12) ::   XXAJ   !Xinanjiang: Free water distribution shape parameter [-]
    real, dimension(12) ::   G      !Mean Capillary Drive (m) for infiltration models
    real, dimension(12) ::   BBVIC  !DVIC heterogeniety parameter for infiltration 
    real, dimension(12) ::   QUARTZ ! fraction of soil comprised of quartz [-] (equal to pctsand/100)
    real                ::   slope  ! free drainage parameter
    real                ::  refkdt  ! infiltration parameter for Schaake scheme
    real                ::   refdk  ! reference diffusivity for Schaake scheme
    real                ::   csoil  ! volumetric soil heat capacity [j/m3/K]
    real                ::   Z0     ! bare soil roughness length (m)

    !--------------------!
    !  Vegetation parameters   !
    !--------------------!
    
    real, dimension(20)     ::   LAI_JAN,LAI_FEB,LAI_MAR,LAI_APR,LAI_MAY,LAI_JUN, &
                                     LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC
    real, dimension(20)     ::   SAI_JAN,SAI_FEB,SAI_MAR,SAI_APR,SAI_MAY,SAI_JUN, &
                                     SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC
    real, dimension(20)     ::   CH2OP
    real, dimension(20)     ::   NROOT
    real, dimension(20)     ::   HVT            ! canopy top height (m)
    real, dimension(20)     ::   HVB            ! canopy bottom height (m)
    real, dimension(20)     ::   TMIN           ! minimum temperature for photosynthesis (k)
    real, dimension(20)     ::   SHDFAC         ! fraction of surface covered by vegetation (dimensionless, 0.0 to 1.0)
    real, dimension(20)     ::   SHDMAX         ! annual maximum fraction of surface covered by vegetation (dimensionless, 0.0 to 1.0)
    real, dimension(20)     ::   Z0MVT          ! momentum roughness length (m)
    real                    ::   CWP            ! canopy wind absorption coefficient (formerly CWPVT)
    real, dimension(20,12)  ::   LAIM_TABLE     !monthly leaf area index, one-sided
    real, dimension(20,12)  ::   SAIM_TABLE     !monthly stem area index, one-sided
    
    !--------------------!
    !  snow parameters   !
    !--------------------!
    real                ::   SSI     ! liquid water holding capacity of snowpack (m3/m3)
    real                ::   MFSNO   ! fractional snow covered area (FSNO) curve parameter
    real                ::   Z0SNO   ! snow surface roughness length (m)
    
    !--------------------!
    !  land parameters   !
    !--------------------!
    integer       :: ISURBAN                   ! vegtype code for urban land cover
    integer       :: ISWATER                   ! vegtype code for water
    integer       :: ISBARREN                  ! vegtype code for barren land cover
    integer       :: ISICE                     ! vegtype code for ice/snow land cover
    integer       :: ISCROP                    ! vegtype code for crop land cover
    integer       :: EBLFOREST                 ! vegtype code for evergreen broadleaf forest
    integer       :: NATURAL                   ! vegtype code for cropland/grassland mosaic
    integer       :: LOW_DENSITY_RESIDENTIAL   ! vegtype code for low density residential
    integer       :: HIGH_DENSITY_RESIDENTIAL  ! vegtype code for high density residential
    integer       :: HIGH_INTENSITY_INDUSTRIAL ! vegtype code for high density industrial

    namelist / timing          / dt,maxtime,output_filename
    namelist / location        / lat,lon
    namelist / forcing         / preciprate,precip_duration,dry_duration,&
                                 precipitating,uwind,vwind,ZREF
    namelist / structure       / isltyp,nsoil,nsnow,structure_option,soil_depth,&
                                 vegtyp,croptype,sfctyp
    namelist / fixed_initial   / zsoil,dzsnso,sice,sh2o
    namelist / uniform_initial / initial_uniform,initial_sh2o_value,&
                                 initial_sice_value
    namelist / soil_parameters / bb,satdk,satdw,maxsmc,satpsi,wltsmc, &
                                 refsmc,pctsand,pctclay,bvic,AXAJ,BXAJ,XXAJ,&
                                 BBVIC,G,QUARTZ,slope,refkdt,refdk,CSOIL,Z0
    namelist / snow_parameters / SSI,MFSNO,Z0SNO
    namelist / veg_parameters  / CH2OP,NROOT,HVT,HVB,TMIN,SHDFAC,SHDMAX,Z0MVT,CWP,&
                                 LAI_JAN,LAI_FEB,LAI_MAR,LAI_APR,LAI_MAY,LAI_JUN,LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC,&
                                 SAI_JAN,SAI_FEB,SAI_MAR,SAI_APR,SAI_MAY,SAI_JUN,SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC
    namelist / forcing_options / precip_phase_option
    namelist / soil_options    / runoff_option,drainage_option,frozen_soil_option,&
                                 dynamic_vic_option
    namelist / veg_options     / dynamic_veg_option
    namelist / land_parameters / ISURBAN,ISWATER,ISBARREN,ISICE,ISCROP,EBLFOREST,NATURAL,LOW_DENSITY_RESIDENTIAL,&
                                 HIGH_DENSITY_RESIDENTIAL,HIGH_INTENSITY_INDUSTRIAL
 
!---------------------------------------------------------------------
!  read input file, part 1
!---------------------------------------------------------------------

    open(30, file="namelist.input", form="formatted")
     read(30, timing)
     read(30, location)
     read(30, forcing)
     read(30, forcing_options)
     read(30, structure)
     read(30, uniform_initial)
     read(30, soil_parameters)
     read(30, snow_parameters)
     read(30, veg_parameters)
     read(30, soil_options)
     read(30, veg_options)
     read(30, land_parameters)
    close(30)

    allocate (zsoil (       1:nsoil))   !depth of layer-bottom from soil surface
    allocate (dzsnso(-nsnow+1:nsoil))   !snow/soil layer thickness [m]
    allocate (sice  (       1:nsoil))   !soil ice content [m3/m3]
    allocate (sh2o  (       1:nsoil))   !soil liquid water content [m3/m3]

!---------------------------------------------------------------------
!  read input file, part 2: initialize
!---------------------------------------------------------------------

    if(structure_option == 1) then       ! user-defined levels
      open(30, file="namelist.input", form="formatted")
       read(30, fixed_initial)
      close(30)
    else if(structure_option == 2) then  ! fixed levels
      dzsnso = soil_depth / nsoil
      do iz = 1, nsoil
        zsoil(iz) = -1. * sum(dzsnso(1:iz))
      end do
      if(.not.initial_uniform) &
        stop "structure_option > 1 must have initial_uniform == .true."
    end if
    
!---------------------------------------------------------------------
!  transfer to structure
!---------------------------------------------------------------------

    this%dt               = dt
    this%maxtime          = maxtime
    this%output_filename  = output_filename
    this%lat              = lat
    this%lon              = lon
    this%preciprate       = preciprate
    this%precip_duration  = precip_duration
    this%dry_duration     = dry_duration
    this%precipitating    = precipitating
    this%uwind            = uwind
    this%vwind            = vwind
    this%ZREF             = ZREF

    this%isltyp           = isltyp
    this%nsoil            = nsoil
    this%nsnow            = nsnow
    this%structure_option = structure_option
    this%soil_depth       = soil_depth
    this%vegtyp           = vegtyp
    this%croptype         = croptype
    this%sfctyp           = sfctyp

    this%zsoil  = zsoil
    this%dzsnso = dzsnso
    this%sice   = sice
    this%sh2o   = sh2o
 
    this%initial_uniform    = initial_uniform
    this%initial_sh2o_value = initial_sh2o_value
    this%initial_sice_value = initial_sice_value

    this%precip_phase_option = precip_phase_option
    this%runoff_option       = runoff_option
    this%drainage_option     = drainage_option
    this%frozen_soil_option  = frozen_soil_option
    this%dynamic_vic_option  = dynamic_vic_option
    this%dynamic_veg_option  = dynamic_veg_option

    this%bb      = bb
    this%satdk   = satdk
    this%satdw   = satdw
    this%maxsmc  = maxsmc
    this%satpsi  = satpsi
    this%wltsmc  = wltsmc
    this%refsmc  = refsmc
    this%pctsand = pctsand
    this%pctclay = pctclay
    this%bvic    = bvic
    this%AXAJ    = AXAJ
    this%BXAJ    = BXAJ
    this%XXAJ    = XXAJ
    this%BBVIC   = BBVIC
    this%G       = G
    this%QUARTZ  = QUARTZ
    this%slope   = slope
    this%refkdt  = refkdt
    this%refdk   = refdk
    this%csoil   = csoil
    this%Z0      = Z0
    
    this%LAIM_TABLE(1:20, 1) = LAI_JAN(1:20)
    this%LAIM_TABLE(1:20, 2) = LAI_FEB(1:20)
    this%LAIM_TABLE(1:20, 3) = LAI_MAR(1:20)
    this%LAIM_TABLE(1:20, 4) = LAI_APR(1:20)
    this%LAIM_TABLE(1:20, 5) = LAI_MAY(1:20)
    this%LAIM_TABLE(1:20, 6) = LAI_JUN(1:20)
    this%LAIM_TABLE(1:20, 7) = LAI_JUL(1:20)
    this%LAIM_TABLE(1:20, 8) = LAI_AUG(1:20)
    this%LAIM_TABLE(1:20, 9) = LAI_SEP(1:20)
    this%LAIM_TABLE(1:20,10) = LAI_OCT(1:20)
    this%LAIM_TABLE(1:20,11) = LAI_NOV(1:20)
    this%LAIM_TABLE(1:20,12) = LAI_DEC(1:20)

    this%SAIM_TABLE(1:20, 1) = SAI_JAN(1:20)
    this%SAIM_TABLE(1:20, 2) = SAI_FEB(1:20)
    this%SAIM_TABLE(1:20, 3) = SAI_MAR(1:20)
    this%SAIM_TABLE(1:20, 4) = SAI_APR(1:20)
    this%SAIM_TABLE(1:20, 5) = SAI_MAY(1:20)
    this%SAIM_TABLE(1:20, 6) = SAI_JUN(1:20)
    this%SAIM_TABLE(1:20, 7) = SAI_JUL(1:20)
    this%SAIM_TABLE(1:20, 8) = SAI_AUG(1:20)
    this%SAIM_TABLE(1:20, 9) = SAI_SEP(1:20)
    this%SAIM_TABLE(1:20,10) = SAI_OCT(1:20)
    this%SAIM_TABLE(1:20,11) = SAI_NOV(1:20)
    this%SAIM_TABLE(1:20,12) = SAI_DEC(1:20)

    this%CH2OP   = CH2OP
    this%NROOT   = NROOT
    this%HVT     = HVT
    this%HVB     = HVB
    this%TMIN    = TMIN
    this%SHDFAC  = SHDFAC
    this%SHDMAX  = SHDMAX
    this%Z0MVT   = Z0MVT
    this%CWP     = CWP

    this%SSI     = SSI
    this%MFSNO   = MFSNO
    this%Z0SNO   = Z0SNO
    
    this%ISURBAN                   = ISURBAN
    this%ISWATER                   = ISWATER
    this%ISBARREN                  = ISBARREN
    this%ISICE                     = ISICE
    this%ISCROP                    = ISCROP
    this%EBLFOREST                 = EBLFOREST
    this%NATURAL                   = NATURAL
    this%LOW_DENSITY_RESIDENTIAL   = LOW_DENSITY_RESIDENTIAL
    this%HIGH_DENSITY_RESIDENTIAL  = HIGH_DENSITY_RESIDENTIAL
    this%HIGH_INTENSITY_INDUSTRIAL = HIGH_INTENSITY_INDUSTRIAL
    

  end subroutine ReadNamelist

end module NamelistRead
