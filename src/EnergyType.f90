module EnergyType

  use NamelistRead,   only: namelist_type
  use EnergyGridType, only: energygrid_type

  implicit none
  private
  
  type, public :: energy_type
  
    real    :: TV                                    ! vegetation temperature (k)
    real    :: TG                                    ! ground temperature (k)
    real    :: FCEV                                  ! canopy evaporation energy flux (w/m2)
    real    :: FCTR                                  ! canopy transpiration energy flux (w/m2)
    real    :: IGS                                   ! growing season index (0=off, 1=on)
    logical :: FROZEN_CANOPY                         ! binary frozen canopy status (true when TV <= parameters%TFRZ)
    logical :: FROZEN_GROUND                         ! binary frozen ground status (true when TG <= parameters%TFRZ)
    integer, allocatable, dimension(:) :: IMELT      ! snow layer melting state index [0-no melt;1-melt]
    real,    allocatable, dimension(:) :: STC        ! snow/soil layer temperature [k]
    real                               :: SNOWT_AVG  ! average snow temperature [k] (by layer mass) (a NWM 3.0 output variable)
    real,    allocatable, dimension(:) :: DF         ! snow and soil layer thermal conductivity [w/m/k]
    real,    allocatable, dimension(:) :: HCPCT      ! snow and soil layer heat capacity [j/m3/k]
    real,    allocatable, dimension(:) :: FACT       ! temporary variable used in phase change [s/j/m2/k]
    
    ! Heat advected by precipitation
    real    :: PAHV                              ! precipitation advected heat - vegetation net (W/m2)
    real    :: PAHG                              ! precipitation advected heat - under canopy net (W/m2)
    real    :: PAHB                              ! precipitation advected heat - bare ground net (W/m2)
    REAL    :: PAH                               ! precipitation advected heat - total (W/m2)
    
    ! Albedo
    real                 :: TAUSS  ! non-dimensional snow age
    real                 :: FAGE   ! snow age (0 = new snow)
    real                 :: ALB    ! broadband albedo in CLASS scheme
    real                 :: ALBOLD ! broadband albedo at previous timestep 
    
    real, allocatable, dimension(:) :: ALBD   ! surface albedo (direct)
    real, allocatable, dimension(:) :: ALBI   ! surface albedo (diffuse)
    real, allocatable, dimension(:) :: ALBGRD ! ground albedo (direct)
    real, allocatable, dimension(:) :: ALBGRI ! ground albedo (diffuse)
    real, allocatable, dimension(:) :: ALBSND ! snow albedo for direct(1=vis, 2=nir)
    real, allocatable, dimension(:) :: ALBSNI ! snow albedo for diffuse
    
    real, allocatable, dimension(:) :: FABD   ! flux abs by veg (per unit direct flux)
    real, allocatable, dimension(:) :: FABI   ! flux abs by veg (per unit diffuse flux)
    real, allocatable, dimension(:) :: FTDD   ! down direct flux below veg (per unit dir flux)
    real, allocatable, dimension(:) :: FTDI   ! down diffuse flux below veg (per unit dif flux)
    real, allocatable, dimension(:) :: FTID   ! down diffuse flux below veg (per unit dir flux)
    real, allocatable, dimension(:) :: FTII   ! down diffuse flux below veg (per unit dif flux)
    real, allocatable, dimension(:) :: FREVD  ! direct flux reflected by veg layer (per unit incoming flux) 
    real, allocatable, dimension(:) :: FREVI  ! diffuse flux reflected by veg layer (per unit incoming flux) 
    real, allocatable, dimension(:) :: FREGD  ! direct flux reflected by ground (per unit incoming flux) 
    real, allocatable, dimension(:) :: FREGI  ! direct flux reflected by ground (per unit incoming flux)
    real, allocatable, dimension(:) :: RHO    ! leaf/stem reflectance weighted by fraction LAI and SAI
    real, allocatable, dimension(:) :: TAU    ! leaf/stem transmittance weighted by fraction LAI and SAI
  
    
    ! Shortwave radiation
    real                 :: COSZ    ! cosine solar zenith angle [0-1]
    real                 :: COSZ_HORIZ ! cosine solar zenith angle for flat ground [0-1]
    real                 :: BGAP    ! between canopy gap fraction for beam (-)
    real                 :: WGAP    ! within canopy gap fraction for beam (-)
    REAL                 :: FSUN    ! sunlit fraction of canopy (-)
    REAL                 :: FSHA    ! shaded fraction of canopy (-)
    REAL                 :: LAISUN  ! sunlit leaf area (-)
    REAL                 :: LAISHA  ! shaded leaf area (-)
    REAL                 :: PARSUN  ! average absorbed par for sunlit leaves (w/m2)
    REAL                 :: PARSHA  ! average absorbed par for shaded leaves (w/m2)
    REAL                 :: SAV     ! solar radiation absorbed by vegetation (w/m2)
    REAL                 :: SAG     ! solar radiation absorbed by ground (w/m2)
    REAL                 :: FSA     ! total absorbed solar radiation (w/m2)
    REAL                 :: FSR     ! total reflected solar radiation (w/m2)
    REAL                 :: FSRV    ! reflected solar radiation by vegetation
    REAL                 :: FSRG    ! reflected solar radiation by ground
  
    ! Other, uncategorized
    REAL                 :: TAH     ! canopy air tmeperature (K)
    REAL                 :: EAH     ! canopy water vapor pressure (Pa)  
    REAL                 :: ZPD     ! zero plane displacement, ground (m)
    REAL                 :: Z0MG    ! z0 momentum, ground (m)
    REAL                 :: Z0M     ! roughness length, momentum (m)
    REAL                 :: ZLVL    ! reference height (m)  
    REAL                 :: CMV     ! momentum drag coefficient (vegetated surface)  
    REAL                 :: CMB     ! momentum drag coefficient (bare ground)  
    REAL                 :: CM      ! momentum drag coefficient (weighted version of CMV + CMB by FVEG)  
    REAL                 :: CH      ! drag coefficient for heat
    REAL                 :: TGB     ! ground temperature (K)
    REAL                 :: QSFC    ! mixing ratio at lowest model layer (g/g)
    REAL                 :: EMV     ! vegetation emissivity (-)
    REAL                 :: EMG     ! ground emissivity (-)
    REAL                 :: GAMMAV  ! psychrometric constant (Pa/K)
    REAL                 :: GAMMAG  ! psychrometric constant (Pa/K)  
  !  REAL                 :: GAMMA   ! psychrometric constant (Pa/K)  NOT USED IN CURRENT VERSION
    REAL                 :: EVC     ! evaporation heat flux (w/m2)  [+= to atm]  
    REAL                 :: IRC     ! net longwave radiation (w/m2) [+= to atm]
    REAL                 :: IRG     ! net longwave radiation (w/m2) [+= to atm]
    REAL                 :: SHC     ! sensible heat flux (w/m2)     [+= to atm]
    REAL                 :: SHG     ! sensible heat flux (w/m2)     [+= to atm]
    REAL                 :: SHB     ! sensible heat flux (w/m2) [+ to atm] 
    REAL                 :: EVG     ! evaporation heat flux (w/m2)  [+= to atm]
    REAL                 :: EVB     ! latent heat flux (w/m2)   [+ to atm]
    REAL                 :: TR      ! transpiration heat flux (w/m2)[+= to atm]
    REAL                 :: GH      ! ground heat (w/m2) [+ = to soil]
    REAL                 :: GHB     ! ground heat flux (w/m2)  [+ to soil] 
    REAL                 :: GHV     ! ground heat flux [w/m2]  [+ to soil]
    REAL                 :: T2MV    ! 2 m height air temperature (k)
    REAL                 :: CHLEAF  ! leaf exchange coefficient
    REAL                 :: CHUC    ! under canopy exchange coefficient
    REAL                 :: CHV2    ! sensible heat exch. coeff. over vegetated fraction (m/s)  
    REAL                 :: CHB2    ! sensible heat exch. coeff. bare-ground (m/s)
    REAL                 :: Q2V     ! check
    REAL                 :: LATHEAV ! latent heat of vaporization/subli (j/kg) (varies if froz)
    REAL                 :: LATHEAG ! latent heat of vaporization/subli (j/kg) (varies if froz)
    REAL                 :: LATHEA  ! latent heat of vaporization/subli (j/kg) (bare ground version)
    REAL                 :: RSURF   ! ground surface resistance (s/m)
    REAL                 :: RHSUR   ! relative humidity in surface soil/snow air space (-)  
    REAL                 :: TAUXV   ! wind stress: e-w (n/m2) vegetation
    REAL                 :: TAUYV   ! wind stress: n-s (n/m2) vegetation
    REAL                 :: TAUXB   ! wind stress: e-w (n/m2) bare ground
    REAL                 :: TAUYB   ! wind stress: n-s (n/m2) bare ground
    REAL                 :: TAUX    ! wind stress: e-w (n/m2)
    REAL                 :: TAUY    ! wind stress: n-s (n/m2)  
    REAL                 :: CAH2    ! sensible heat conductance for diagnostics
    REAL                 :: EHB2    ! sensible heat conductance for diagnostics (bare ground)
    REAL                 :: T2MB    ! 2 m height air temperature (K)  
    REAL                 :: Q2B     ! bare ground heat conductance
    REAL                 :: TGV     ! ground surface temp. [k]
    REAL                 :: CHV     ! sensible heat exchange coefficient
    REAL                 :: RSSUN   ! sunlit leaf stomatal resistance (s/m)
    REAL                 :: RSSHA   ! shaded leaf stomatal resistance (s/m)
    REAL                 :: RB      ! leaf boundary layer resistance (s/m)
    REAL                 :: FIRA    ! total net LW. rad (w/m2)   [+ to atm]
    REAL                 :: FSH     ! total sensible heat (w/m2) [+ to atm]
    REAL                 :: FGEV    ! ground evaporation (w/m2)  [+ to atm]
    REAL                 :: TRAD    ! radiative temperature (k)
    REAL                 :: IRB     ! net longwave rad. [w/m2] [+ to atm]
    REAL                 :: SSOIL   ! ground heat flux (w/m2)   [+ to soil]
    REAL                 :: T2M     ! 2-meter air temperature (k)
    REAL                 :: TS      ! surface temperature (k)
    REAL                 :: CHB     ! sensible heat exchange coefficient
    REAL                 :: Q1
    REAL                 :: Q2E
    REAL                 :: Z0WRF   ! combined z0 sent to coupled model
    REAL                 :: EMISSI  ! net surface emissivity  
    REAL                 :: PSN     ! total photosyn. (umolco2/m2/s) [+]
    REAL                 :: PSNSUN  ! sunlit photosynthesis (umolco2/m2/s)  
    REAL                 :: PSNSHA  ! shaded photosynthesis (umolco2/m2/s)    
    REAL                 :: APAR    ! total photosyn. active energy (w/m2)
    REAL                 :: QMELT   ! snowmelt [mm/s]
    
    REAL                 :: LH      ! latent heat (total) flux [W m-2]
    REAL                 :: TGS     ! ground surface temperature (K, takes value of TG when SNOWH <= 0.05 and STC[0] when SNOWH > 0.05) 
                                    ! this is a temporary fix for when coupled to subsurface modules
                                    ! TODO: further investigation
    
  
    integer              :: ICE     ! 1 if sea ice, -1 if glacier, 0 if no land ice (seasonal snow)
    
    
    contains
  
      procedure, public  :: Init
      procedure, private :: InitAllocate        
      procedure, private :: InitDefault     
      procedure, private :: InitTransfer
      procedure, public  :: TransferIn
      procedure, public  :: TransferOut
  
  end type energy_type
  
  contains   
  
    subroutine Init(this, namelist, energygrid)
  
      class(energy_type),    intent(inout) :: this
      type(namelist_type),   intent(in)    :: namelist
      type(energygrid_type), intent(in)    :: energygrid
  
      call this%InitAllocate(namelist)
      call this%InitDefault()
      call this%InitTransfer(energygrid)
  
    end subroutine Init
  
    subroutine InitAllocate(this, namelist)
  
      class(energy_type),  intent(inout) :: this
      type(namelist_type), intent(in)    :: namelist
  
      associate(nsnow => namelist%nsnow, &
                nsoil => namelist%nsoil)

      if(.NOT.allocated(this%IMELT))  allocate(this%IMELT   (-nsnow+1:nsoil))
      if(.NOT.allocated(this%STC))    allocate(this%STC     (-nsnow+1:nsoil))
      if(.NOT.allocated(this%DF))     allocate(this%DF      (-nsnow+1:nsoil))
      if(.NOT.allocated(this%HCPCT))  allocate(this%HCPCT   (-nsnow+1:nsoil))
      if(.NOT.allocated(this%FACT))   allocate(this%FACT    (-nsnow+1:nsoil))

      if(.NOT.allocated(this%ALBD))   allocate(this%ALBD    (2))
      if(.NOT.allocated(this%ALBI))   allocate(this%ALBI    (2))
      if(.NOT.allocated(this%ALBGRD)) allocate(this%ALBGRD  (2))
      if(.NOT.allocated(this%ALBGRI)) allocate(this%ALBGRI  (2))
      if(.NOT.allocated(this%ALBSND)) allocate(this%ALBSND  (2))
      if(.NOT.allocated(this%ALBSNI)) allocate(this%ALBSNI  (2))

      if(.NOT.allocated(this%FABD))   allocate(this%FABD    (2))
      if(.NOT.allocated(this%FABI))   allocate(this%FABI    (2))
      if(.NOT.allocated(this%FTDD))   allocate(this%FTDD    (2))
      if(.NOT.allocated(this%FTDI))   allocate(this%FTDI    (2))
      if(.NOT.allocated(this%FTID))   allocate(this%FTID    (2))
      if(.NOT.allocated(this%FTII))   allocate(this%FTII    (2))
      if(.NOT.allocated(this%FREVD))  allocate(this%FREVD   (2))
      if(.NOT.allocated(this%FREVI))  allocate(this%FREVI   (2))
      if(.NOT.allocated(this%FREGD))  allocate(this%FREGD   (2))
      if(.NOT.allocated(this%FREGI))  allocate(this%FREGI   (2))
      if(.NOT.allocated(this%RHO))    allocate(this%RHO     (2))
      if(.NOT.allocated(this%TAU))    allocate(this%TAU     (2))
  
      end associate

    end subroutine InitAllocate
  
    subroutine InitDefault(this)
  
      class(energy_type) :: this
  
      this%TV        = huge(1.0)
      this%TG        = huge(1.0)
      this%FCEV      = huge(1.0)
      this%FCTR      = huge(1.0)
      this%IGS       = huge(1.0)
      this%FROZEN_CANOPY = .false.
      this%FROZEN_GROUND = .false.
      this%PAHV      = huge(1.0)
      this%PAHG      = huge(1.0)
      this%PAHB      = huge(1.0)
      this%PAH       = huge(1.0)
      
      this%TAUSS     = huge(1.0)
      this%FAGE      = huge(1.0)
      this%ALB       = huge(1.0)
      this%ALBOLD    = huge(1.0)
      
      this%COSZ      = huge(1.0)
      this%COSZ_HORIZ= huge(1.0)
      this%BGAP      = huge(1.0)
      this%WGAP      = huge(1.0)
      this%FSUN      = huge(1.0)
      this%FSHA      = huge(1.0)
      this%LAISUN    = huge(1.0)
      this%LAISHA    = huge(1.0)
      this%PARSUN    = huge(1.0)
      this%PARSHA    = huge(1.0)
      this%SAV       = huge(1.0)
      this%SAG       = huge(1.0)
      this%FSA       = huge(1.0)
      this%FSR       = huge(1.0)
      this%FSRV      = huge(1.0)
      this%FSRG      = huge(1.0)
      
      this%TAH       = huge(1.0)
      this%EAH       = huge(1.0)
      this%ZPD       = huge(1.0)    
      this%Z0MG      = huge(1.0)    
      this%Z0M       = huge(1.0)      
      this%ZLVL      = huge(1.0)
      this%CMV       = huge(1.0)  
      this%CMB       = huge(1.0)  
      this%CM        = huge(1.0)  
      this%CH        = huge(1.0)  
      this%TGB       = huge(1.0)    
      this%QSFC      = huge(1.0)  
      this%EMV       = huge(1.0)  
      this%EMG       = huge(1.0)  
      this%GAMMAV    = huge(1.0)  
      this%GAMMAG    = huge(1.0)  
  !    this%GAMMA     = huge(1.0)  ! NOT USED IN CURRENT VERSION
      
      this%EVC       = huge(1.0)  
      this%IRC       = huge(1.0)  
      this%SHC       = huge(1.0)  
      this%IRG       = huge(1.0)  
      this%SHG       = huge(1.0)  
      this%EVG       = huge(1.0)  
      this%TR        = huge(1.0)  
      this%GH        = huge(1.0) 
      this%GHB       = huge(1.0)   
      this%GHV       = huge(1.0)     
      this%T2MV      = huge(1.0)  
      this%CHLEAF    = huge(1.0)  
      this%CHUC      = huge(1.0)  
      this%CHV2      = huge(1.0)
      this%CHB2      = huge(1.0)
      this%Q2V       = huge(1.0)  
      this%LATHEAV   = huge(1.0)      
      this%LATHEAG   = huge(1.0)  
      this%LATHEA    = huge(1.0)     
      this%RSURF     = huge(1.0)    
      this%RHSUR     = huge(1.0)    
      this%TAUXV     = huge(1.0) 
      this%TAUYV     = huge(1.0) 
      this%TAUXB     = huge(1.0) 
      this%TAUYB     = huge(1.0) 
      this%TAUX      = huge(1.0) 
      this%TAUY      = huge(1.0) 
      this%CAH2      = huge(1.0) 
      this%EHB2      = huge(1.0)
      this%T2MB      = huge(1.0)
      this%Q2B       = huge(1.0)
      this%SHB       = huge(1.0)
      this%EVB       = huge(1.0)
      this%TGB       = huge(1.0)
      this%CHV       = huge(1.0)
      this%RSSUN     = huge(1.0)     
      this%RSSHA     = huge(1.0)
      this%RB        = huge(1.0)
  
      this%FIRA      = huge(1.0)
      this%FSH       = huge(1.0)
      this%FGEV      = huge(1.0)
      this%TRAD      = huge(1.0)
      this%IRB       = huge(1.0)
      this%SSOIL     = huge(1.0)
      this%T2M       = huge(1.0)
      this%TS        = huge(1.0)
      this%CHB       = huge(1.0)
      this%Q1        = huge(1.0)
      this%Q2E       = huge(1.0)
      this%Z0WRF     = huge(1.0)
      this%EMISSI    = huge(1.0)
      this%PSN       = huge(1.0)
      this%PSNSUN    = huge(1.0)
      this%PSNSHA    = huge(1.0)    
      this%APAR      = huge(1.0)
      this%QMELT     = huge(1.0)
  
      this%LH        = huge(1.0)    
      this%TGS       = huge(1.0)    
      this%ICE       = huge(1)    
      
      this%IMELT(:)  = huge(1)
      this%STC(:)    = huge(1.0)
      this%DF(:)     = huge(1.0)
      this%HCPCT(:)  = huge(1.0)
      this%FACT(:)   = huge(1.0)

      this%ALBD(:)    = huge(1.0)
      this%ALBI(:)    = huge(1.0)
      this%ALBGRD(:)    = huge(1.0)
      this%ALBGRI(:)    = huge(1.0)
      this%ALBSND(:)    = huge(1.0)
      this%ALBSNI(:)    = huge(1.0)

      this%FABD(:)    = huge(1.0)
      this%FABI(:)    = huge(1.0)
      this%FTDD(:)    = huge(1.0)
      this%FTDI(:)    = huge(1.0)
      this%FTID(:)    = huge(1.0)
      this%FTII(:)    = huge(1.0)
      this%FREVD(:)    = huge(1.0)
      this%FREVI(:)    = huge(1.0)
      this%FREGD(:)    = huge(1.0)
      this%FREGI(:)    = huge(1.0)
      this%RHO(:)    = huge(1.0)
      this%TAU(:)    = huge(1.0)
      
    end subroutine InitDefault
  
    subroutine InitTransfer(this, energygrid)
  
      class(energy_type),    intent(inout) :: this
      type(energygrid_type), intent(in)    :: energygrid
  
      !Nothing to do

    end subroutine InitTransfer
  
    subroutine TransferIn(this, energygrid, ix, iy)

      implicit none
  
      class(energy_type),    intent(inout) :: this
      type(energygrid_type), intent(in)    :: energygrid
      integer,               intent(in)    :: ix
      integer,               intent(in)    :: iy
  
      this%TV = energygrid%TV(ix,iy)
      this%TG = energygrid%TG(ix,iy)
      this%FCEV = energygrid%FCEV(ix,iy)
      this%FCTR = energygrid%FCTR(ix,iy)
      this%IGS = energygrid%IGS(ix,iy)
      this%FROZEN_CANOPY = energygrid%FROZEN_CANOPY(ix,iy)
      this%FROZEN_GROUND = energygrid%FROZEN_GROUND(ix,iy)
      this%IMELT(:) = energygrid%IMELT(ix,iy,:)
      this%STC(:) = energygrid%STC(ix,iy,:)
      this%DF(:) = energygrid%DF(ix,iy,:)
      this%HCPCT(:) = energygrid%HCPCT(ix,iy,:)
      this%FACT(:) = energygrid%FACT(ix,iy,:)
      this%PAHV = energygrid%PAHV(ix,iy)
      this%PAHG = energygrid%PAHG(ix,iy)
      this%PAHB = energygrid%PAHB(ix,iy)
      this%PAH = energygrid%PAH(ix,iy)
      this%TAUSS = energygrid%TAUSS(ix,iy)
      this%FAGE = energygrid%FAGE(ix,iy)
      this%ALB = energygrid%ALB(ix,iy)
      this%ALBOLD = energygrid%ALBOLD(ix,iy)
      this%ALBD(:) = energygrid%ALBD(ix,iy,:)
      this%ALBI(:) = energygrid%ALBI(ix,iy,:)
      this%ALBGRD(:) = energygrid%ALBGRD(ix,iy,:)
      this%ALBGRI(:) = energygrid%ALBGRI(ix,iy,:)
      this%ALBSND(:) = energygrid%ALBSND(ix,iy,:)
      this%ALBSNI(:) = energygrid%ALBSNI(ix,iy,:)
      this%FABD(:) = energygrid%FABD(ix,iy,:)
      this%FABI(:) = energygrid%FABI(ix,iy,:)
      this%FTDD(:) = energygrid%FTDD(ix,iy,:)
      this%FTDI(:) = energygrid%FTDI(ix,iy,:)
      this%FTID(:) = energygrid%FTID(ix,iy,:)
      this%FTII(:) = energygrid%FTII(ix,iy,:)
      this%FREVD(:) = energygrid%FREVD(ix,iy,:)
      this%FREVI(:) = energygrid%FREVI(ix,iy,:)
      this%FREGD(:) = energygrid%FREGD(ix,iy,:)
      this%FREGI(:) = energygrid%FREGI(ix,iy,:)
      this%RHO(:) = energygrid%RHO(ix,iy,:)
      this%TAU(:) = energygrid%TAU(ix,iy,:)
      this%COSZ = energygrid%COSZ(ix,iy)
      this%COSZ_HORIZ = energygrid%COSZ_HORIZ(ix,iy)
      this%BGAP = energygrid%BGAP(ix,iy)
      this%WGAP = energygrid%WGAP(ix,iy)
      this%FSUN = energygrid%FSUN(ix,iy)
      this%FSHA = energygrid%FSHA(ix,iy)
      this%LAISUN = energygrid%LAISUN(ix,iy)
      this%LAISHA = energygrid%LAISHA(ix,iy)
      this%PARSUN = energygrid%PARSUN(ix,iy)
      this%PARSHA = energygrid%PARSHA(ix,iy)
      this%SAV = energygrid%SAV(ix,iy)
      this%SAG = energygrid%SAG(ix,iy)
      this%FSA = energygrid%FSA(ix,iy)
      this%FSR = energygrid%FSR(ix,iy)
      this%FSRV = energygrid%FSRV(ix,iy)
      this%FSRG = energygrid%FSRG(ix,iy)
      this%TAH = energygrid%TAH(ix,iy)
      this%EAH = energygrid%EAH(ix,iy)
      this%ZPD = energygrid%ZPD(ix,iy)
      this%Z0MG = energygrid%Z0MG(ix,iy)
      this%Z0M = energygrid%Z0M(ix,iy)
      this%ZLVL = energygrid%ZLVL(ix,iy)
      this%CMV = energygrid%CMV(ix,iy)
      this%CMB = energygrid%CMB(ix,iy)
      this%CM = energygrid%CM(ix,iy)
      this%CH = energygrid%CH(ix,iy)
      this%TGB = energygrid%TGB(ix,iy)
      this%QSFC = energygrid%QSFC(ix,iy)
      this%EMV = energygrid%EMV(ix,iy)
      this%EMG = energygrid%EMG(ix,iy)
      this%GAMMAV = energygrid%GAMMAV(ix,iy)
      this%GAMMAG = energygrid%GAMMAG(ix,iy)
      this%EVC = energygrid%EVC(ix,iy)
      this%IRC = energygrid%IRC(ix,iy)
      this%IRG = energygrid%IRG(ix,iy)
      this%SHC = energygrid%SHC(ix,iy)
      this%SHG = energygrid%SHG(ix,iy)
      this%SHB = energygrid%SHB(ix,iy)
      this%EVG = energygrid%EVG(ix,iy)
      this%EVB = energygrid%EVB(ix,iy)
      this%TR = energygrid%TR(ix,iy)
      this%GH = energygrid%GH(ix,iy)
      this%GHB = energygrid%GHB(ix,iy)
      this%GHV = energygrid%GHV(ix,iy)
      this%T2MV = energygrid%T2MV(ix,iy)
      this%CHLEAF = energygrid%CHLEAF(ix,iy)
      this%CHUC = energygrid%CHUC(ix,iy)
      this%CHV2 = energygrid%CHV2(ix,iy)
      this%CHB2 = energygrid%CHB2(ix,iy)
      this%Q2V = energygrid%Q2V(ix,iy)
      this%LATHEAV = energygrid%LATHEAV(ix,iy)
      this%LATHEAG = energygrid%LATHEAG(ix,iy)
      this%LATHEA = energygrid%LATHEA(ix,iy)
      this%RSURF = energygrid%RSURF(ix,iy)
      this%RHSUR = energygrid%RHSUR(ix,iy)
      this%TAUXV = energygrid%TAUXV(ix,iy)
      this%TAUYV = energygrid%TAUYV(ix,iy)
      this%TAUXB = energygrid%TAUXB(ix,iy)
      this%TAUYB = energygrid%TAUYB(ix,iy)
      this%TAUX = energygrid%TAUX(ix,iy)
      this%TAUY = energygrid%TAUY(ix,iy)
      this%CAH2 = energygrid%CAH2(ix,iy)
      this%EHB2 = energygrid%EHB2(ix,iy)
      this%T2MB = energygrid%T2MB(ix,iy)
      this%Q2B = energygrid%Q2B(ix,iy)
      this%TGV = energygrid%TGV(ix,iy)
      this%CHV = energygrid%CHV(ix,iy)
      this%RSSUN = energygrid%RSSUN(ix,iy)
      this%RSSHA = energygrid%RSSHA(ix,iy)
      this%RB = energygrid%RB(ix,iy)
      this%FIRA = energygrid%FIRA(ix,iy)
      this%FSH = energygrid%FSH(ix,iy)
      this%FGEV = energygrid%FGEV(ix,iy)
      this%TRAD = energygrid%TRAD(ix,iy)
      this%IRB = energygrid%IRB(ix,iy)
      this%SSOIL = energygrid%SSOIL(ix,iy)
      this%T2M = energygrid%T2M(ix,iy)
      this%TS = energygrid%TS(ix,iy)
      this%CHB = energygrid%CHB(ix,iy)
      this%Q1 = energygrid%Q1(ix,iy)
      this%Q2E = energygrid%Q2E(ix,iy)
      this%Z0WRF = energygrid%Z0WRF(ix,iy)
      this%EMISSI = energygrid%EMISSI(ix,iy)
      this%PSN = energygrid%PSN(ix,iy)
      this%PSNSUN = energygrid%PSNSUN(ix,iy)
      this%PSNSHA = energygrid%PSNSHA(ix,iy)
      this%APAR = energygrid%APAR(ix,iy)
      this%QMELT = energygrid%QMELT(ix,iy)
      this%LH = energygrid%LH(ix,iy)
      this%TGS = energygrid%TGS(ix,iy)
      this%ICE = energygrid%ICE(ix,iy) 
  
    end subroutine TransferIn

    subroutine TransferOut(this, energygrid, ix, iy)

      implicit none
  
      class(energy_type),    intent(in)    :: this
      type(energygrid_type), intent(inout) :: energygrid
      integer,               intent(in)    :: ix
      integer,               intent(in)    :: iy
  
      energygrid%TV(ix,iy) = this%TV
      energygrid%TG(ix,iy) = this%TG
      energygrid%FCEV(ix,iy) = this%FCEV
      energygrid%FCTR(ix,iy) = this%FCTR
      energygrid%IGS(ix,iy) = this%IGS
      energygrid%FROZEN_CANOPY(ix,iy) = this%FROZEN_CANOPY
      energygrid%FROZEN_GROUND(ix,iy) = this%FROZEN_GROUND
      energygrid%IMELT(ix,iy,:) = this%IMELT(:)
      energygrid%STC(ix,iy,:) = this%STC(:)
      energygrid%DF(ix,iy,:) = this%DF(:)
      energygrid%HCPCT(ix,iy,:) = this%HCPCT(:)
      energygrid%FACT(ix,iy,:) = this%FACT(:)
      energygrid%PAHV(ix,iy) = this%PAHV
      energygrid%PAHG(ix,iy) = this%PAHG
      energygrid%PAHB(ix,iy) = this%PAHB
      energygrid%PAH(ix,iy) = this%PAH
      energygrid%TAUSS(ix,iy) = this%TAUSS
      energygrid%FAGE(ix,iy) = this%FAGE
      energygrid%ALB(ix,iy) = this%ALB
      energygrid%ALBOLD(ix,iy) = this%ALBOLD
      energygrid%ALBD(ix,iy,:) = this%ALBD(:)
      energygrid%ALBI(ix,iy,:) = this%ALBI(:)
      energygrid%ALBGRD(ix,iy,:) = this%ALBGRD(:)
      energygrid%ALBGRI(ix,iy,:) = this%ALBGRI(:)
      energygrid%ALBSND(ix,iy,:) = this%ALBSND(:)
      energygrid%ALBSNI(ix,iy,:) = this%ALBSNI(:)
      energygrid%FABD(ix,iy,:) = this%FABD(:)
      energygrid%FABI(ix,iy,:) = this%FABI(:)
      energygrid%FTDD(ix,iy,:) = this%FTDD(:)
      energygrid%FTDI(ix,iy,:) = this%FTDI(:)
      energygrid%FTID(ix,iy,:) = this%FTID(:)
      energygrid%FTII(ix,iy,:) = this%FTII(:)
      energygrid%FREVD(ix,iy,:) = this%FREVD(:)
      energygrid%FREVI(ix,iy,:) = this%FREVI(:)
      energygrid%FREGD(ix,iy,:) = this%FREGD(:)
      energygrid%FREGI(ix,iy,:) = this%FREGI(:)
      energygrid%RHO(ix,iy,:) = this%RHO(:)
      energygrid%TAU(ix,iy,:) = this%TAU(:)
      energygrid%COSZ(ix,iy) = this%COSZ
      energygrid%COSZ_HORIZ(ix,iy) = this%COSZ_HORIZ
      energygrid%BGAP(ix,iy) = this%BGAP
      energygrid%WGAP(ix,iy) = this%WGAP
      energygrid%FSUN(ix,iy) = this%FSUN
      energygrid%FSHA(ix,iy) = this%FSHA
      energygrid%LAISUN(ix,iy) = this%LAISUN
      energygrid%LAISHA(ix,iy) = this%LAISHA
      energygrid%PARSUN(ix,iy) = this%PARSUN
      energygrid%PARSHA(ix,iy) = this%PARSHA
      energygrid%SAV(ix,iy) = this%SAV
      energygrid%SAG(ix,iy) = this%SAG
      energygrid%FSA(ix,iy) = this%FSA
      energygrid%FSR(ix,iy) = this%FSR
      energygrid%FSRV(ix,iy) = this%FSRV
      energygrid%FSRG(ix,iy) = this%FSRG
      energygrid%TAH(ix,iy) = this%TAH
      energygrid%EAH(ix,iy) = this%EAH
      energygrid%ZPD(ix,iy) = this%ZPD
      energygrid%Z0MG(ix,iy) = this%Z0MG
      energygrid%Z0M(ix,iy) = this%Z0M
      energygrid%ZLVL(ix,iy) = this%ZLVL
      energygrid%CMV(ix,iy) = this%CMV
      energygrid%CMB(ix,iy) = this%CMB
      energygrid%CM(ix,iy) = this%CM
      energygrid%CH(ix,iy) = this%CH
      energygrid%TGB(ix,iy) = this%TGB
      energygrid%QSFC(ix,iy) = this%QSFC
      energygrid%EMV(ix,iy) = this%EMV
      energygrid%EMG(ix,iy) = this%EMG
      energygrid%GAMMAV(ix,iy) = this%GAMMAV
      energygrid%GAMMAG(ix,iy) = this%GAMMAG
      energygrid%EVC(ix,iy) = this%EVC
      energygrid%IRC(ix,iy) = this%IRC
      energygrid%IRG(ix,iy) = this%IRG
      energygrid%SHC(ix,iy) = this%SHC
      energygrid%SHG(ix,iy) = this%SHG
      energygrid%SHB(ix,iy) = this%SHB
      energygrid%EVG(ix,iy) = this%EVG
      energygrid%EVB(ix,iy) = this%EVB
      energygrid%TR(ix,iy) = this%TR
      energygrid%GH(ix,iy) = this%GH
      energygrid%GHB(ix,iy) = this%GHB
      energygrid%GHV(ix,iy) = this%GHV
      energygrid%T2MV(ix,iy) = this%T2MV
      energygrid%CHLEAF(ix,iy) = this%CHLEAF
      energygrid%CHUC(ix,iy) = this%CHUC
      energygrid%CHV2(ix,iy) = this%CHV2
      energygrid%CHB2(ix,iy) = this%CHB2
      energygrid%Q2V(ix,iy) = this%Q2V
      energygrid%LATHEAV(ix,iy) = this%LATHEAV
      energygrid%LATHEAG(ix,iy) = this%LATHEAG
      energygrid%LATHEA(ix,iy) = this%LATHEA
      energygrid%RSURF(ix,iy) = this%RSURF
      energygrid%RHSUR(ix,iy) = this%RHSUR
      energygrid%TAUXV(ix,iy) = this%TAUXV
      energygrid%TAUYV(ix,iy) = this%TAUYV
      energygrid%TAUXB(ix,iy) = this%TAUXB
      energygrid%TAUYB(ix,iy) = this%TAUYB
      energygrid%TAUX(ix,iy) = this%TAUX
      energygrid%TAUY(ix,iy) = this%TAUY
      energygrid%CAH2(ix,iy) = this%CAH2
      energygrid%EHB2(ix,iy) = this%EHB2
      energygrid%T2MB(ix,iy) = this%T2MB
      energygrid%Q2B(ix,iy) = this%Q2B
      energygrid%TGV(ix,iy) = this%TGV
      energygrid%CHV(ix,iy) = this%CHV
      energygrid%RSSUN(ix,iy) = this%RSSUN
      energygrid%RSSHA(ix,iy) = this%RSSHA
      energygrid%RB(ix,iy) = this%RB
      energygrid%FIRA(ix,iy) = this%FIRA
      energygrid%FSH(ix,iy) = this%FSH
      energygrid%FGEV(ix,iy) = this%FGEV
      energygrid%TRAD(ix,iy) = this%TRAD
      energygrid%IRB(ix,iy) = this%IRB
      energygrid%SSOIL(ix,iy) = this%SSOIL
      energygrid%T2M(ix,iy) = this%T2M
      energygrid%TS(ix,iy) = this%TS
      energygrid%CHB(ix,iy) = this%CHB
      energygrid%Q1(ix,iy) = this%Q1
      energygrid%Q2E(ix,iy) = this%Q2E
      energygrid%Z0WRF(ix,iy) = this%Z0WRF
      energygrid%EMISSI(ix,iy) = this%EMISSI
      energygrid%PSN(ix,iy) = this%PSN
      energygrid%PSNSUN(ix,iy) = this%PSNSUN
      energygrid%PSNSHA(ix,iy) = this%PSNSHA
      energygrid%APAR(ix,iy) = this%APAR
      energygrid%QMELT(ix,iy) = this%QMELT
      energygrid%LH(ix,iy) = this%LH
      energygrid%TGS(ix,iy) = this%TGS
      energygrid%ICE(ix,iy) = this%ICE    
  
    end subroutine TransferOut

  end module EnergyType