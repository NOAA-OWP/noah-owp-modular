module NamelistRead

implicit none
save
private

type, public :: namelist_type

  real          :: dt
  integer       :: maxtime
  character*256 :: output_filename
  real          :: preciprate
  integer       :: precip_duration
  integer       :: dry_duration
  logical       :: precipitating
  integer       :: isltyp
  integer       :: nsoil
  integer       :: nsnow
  integer       :: structure_option
  real          :: soil_depth
  integer       :: vegtyp
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

  integer       :: precip_phase_option
  integer       :: runoff_option
  integer       :: drainage_option
  integer       :: frozen_soil_option
  integer       :: dynamic_vic_option

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
  real                ::   slope  ! free drainage parameter
  real                ::  refkdt  ! infiltration parameter for Schaake scheme
  real                ::   refdk  ! reference diffusivity for Schaake scheme

  !--------------------!
  !  Vegetation parameters   !
  !--------------------!
  real, dimension(20) ::   LAI
  real, dimension(20) ::   SAI
  real, dimension(20) ::   CH2OP
  real, dimension(20) ::   NROOT
  real                ::   SHDMAX

  !--------------------!
  !  snow parameters   !
  !--------------------!
  real                ::   SSI

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
    real          :: preciprate
    integer       :: precip_duration
    integer       :: dry_duration
    logical       :: precipitating
    integer       :: isltyp
    integer       :: nsoil
    integer       :: nsnow
    integer       :: structure_option
    real          :: soil_depth
    integer       :: vegtyp
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

    integer       :: precip_phase_option
    integer       :: runoff_option
    integer       :: drainage_option
    integer       :: frozen_soil_option
    integer       :: dynamic_vic_option

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
    real                ::   slope  ! free drainage parameter
    real                ::  refkdt  ! infiltration parameter for Schaake scheme
    real                ::   refdk  ! reference diffusivity for Schaake scheme

  !--------------------!
  !  Vegetation parameters   !
  !--------------------!
    real, dimension(20) ::   LAI
    real, dimension(20) ::   SAI
    real, dimension(20) ::   CH2OP
    real, dimension(20) ::   NROOT
    real                ::   SHDMAX 

  !--------------------!
  !  snow parameters   !
  !--------------------!
  real                ::   SSI

    namelist / timing          / dt,maxtime,output_filename
    namelist / forcing         / preciprate,precip_duration,dry_duration,&
                                 precipitating,uwind,vwind
    namelist / structure       / isltyp,nsoil,nsnow,structure_option,soil_depth,&
                                 vegtyp,sfctyp
    namelist / fixed_initial   / zsoil,dzsnso,sice,sh2o
    namelist / uniform_initial / initial_uniform,initial_sh2o_value,&
                                 initial_sice_value
    namelist / soil_parameters / bb,satdk,satdw,maxsmc,satpsi,wltsmc, &
                                 refsmc,pctsand,pctclay,bvic,AXAJ,BXAJ,XXAJ,&
                                 BBVIC,G,slope,refkdt,refdk,SSI
    namelist / veg_parameters  / CH2OP,NROOT,LAI,SAI,SHDMAX
    namelist / forcing_options / precip_phase_option
    namelist / soil_options    / runoff_option,drainage_option,frozen_soil_option,&
                                 dynamic_vic_option
 
!---------------------------------------------------------------------
!  read input file, part 1
!---------------------------------------------------------------------

    open(30, file="namelist.input", form="formatted")
     read(30, timing)
     read(30, forcing)
     read(30, forcing_options)
     read(30, structure)
     read(30, uniform_initial)
     read(30, soil_parameters)
     read(30, veg_parameters)
     read(30, soil_options)
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
    this%preciprate       = preciprate
    this%precip_duration  = precip_duration
    this%dry_duration     = dry_duration
    this%precipitating    = precipitating
    this%uwind            = uwind
    this%vwind            = vwind
    this%isltyp           = isltyp
    this%nsoil            = nsoil
    this%nsnow            = nsnow
    this%structure_option = structure_option
    this%soil_depth       = soil_depth
    this%vegtyp           = vegtyp
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
    this%slope   = slope
    this%refkdt  = refkdt
    this%refdk   = refdk

    this%LAI     = LAI
    this%SAI     = SAI
    this%CH2OP   = CH2OP
    this%NROOT   = NROOT
    this%SHDMAX  = SHDMAX

    this%SSI     = SSI

  end subroutine ReadNamelist

end module NamelistRead
