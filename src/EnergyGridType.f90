module EnergyGridType

  use NamelistRead, only: namelist_type
  
  implicit none
  save
  private
  
  type, public :: energygrid_type
  
  real,allocatable,dimension(:,:)                   :: TV                ! vegetation temperature (k)
  real,allocatable,dimension(:,:)                   :: TG                ! ground temperature (k)
  real,allocatable,dimension(:,:)                   :: FCEV              ! canopy evaporation energy flux (w/m2)
  real,allocatable,dimension(:,:)                   :: FCTR              ! canopy transpiration energy flux (w/m2)
  real,allocatable,dimension(:,:)                   :: IGS               ! growing season index (0=off, 1=on)
  logical,allocatable,dimension(:,:)                :: FROZEN_CANOPY     ! binary frozen canopy status (true when TV <= parameters%TFRZ)
  logical,allocatable,dimension(:,:)                :: FROZEN_GROUND     ! binary frozen ground status (true when TG <= parameters%TFRZ)
  integer, allocatable, dimension(:,:,:)            :: IMELT             ! snow layer melting state index [0-no melt;1-melt]
  real,allocatable, dimension(:,:,:)                :: STC               ! snow/soil layer temperature [k]
  real,allocatable, dimension(:,:,:)                :: DF                ! snow and soil layer thermal conductivity [w/m/k]
  real,allocatable, dimension(:,:,:)                :: HCPCT             ! snow and soil layer heat capacity [j/m3/k]
  real,allocatable, dimension(:,:,:)                :: FACT              ! temporary variable used in phase change [s/j/m2/k]
    
  ! Heat advected by precipitation
  real,allocatable,dimension(:,:)                   :: PAHV              ! precipitation advected heat - vegetation net (W/m2)
  real,allocatable,dimension(:,:)                   :: PAHG              ! precipitation advected heat - under canopy net (W/m2)
  real,allocatable,dimension(:,:)                   :: PAHB              ! precipitation advected heat - bare ground net (W/m2)
  REAL,allocatable,dimension(:,:)                   :: PAH               ! precipitation advected heat - total (W/m2)
    
    ! Albedo
  real,allocatable,dimension(:,:)                   :: TAUSS             ! non-dimensional snow age
  real,allocatable,dimension(:,:)                   :: FAGE              ! snow age (0 = new snow)
  real,allocatable,dimension(:,:)                   :: ALB               ! broadband albedo in CLASS scheme
  real,allocatable,dimension(:,:)                   :: ALBOLD            ! broadband albedo at previous timestep 
    
  real, allocatable, dimension(:,:,:)               :: ALBD              ! surface albedo (direct)
  real, allocatable, dimension(:,:,:)               :: ALBI              ! surface albedo (diffuse)
  real, allocatable, dimension(:,:,:)               :: ALBGRD            ! ground albedo (direct)
  real, allocatable, dimension(:,:,:)               :: ALBGRI            ! ground albedo (diffuse)
  real, allocatable, dimension(:,:,:)               :: ALBSND            ! snow albedo for direct(1=vis, 2=nir)
  real, allocatable, dimension(:,:,:)               :: ALBSNI            ! snow albedo for diffuse
    
  real, allocatable, dimension(:,:,:)               :: FABD   ! flux abs by veg (per unit direct flux)
  real, allocatable, dimension(:,:,:)               :: FABI   ! flux abs by veg (per unit diffuse flux)
  real, allocatable, dimension(:,:,:)               :: FTDD   ! down direct flux below veg (per unit dir flux)
  real, allocatable, dimension(:,:,:)               :: FTDI   ! down diffuse flux below veg (per unit dif flux)
  real, allocatable, dimension(:,:,:)               :: FTID   ! down diffuse flux below veg (per unit dir flux)
  real, allocatable, dimension(:,:,:)               :: FTII   ! down diffuse flux below veg (per unit dif flux)
  real, allocatable, dimension(:,:,:)               :: FREVD  ! direct flux reflected by veg layer (per unit incoming flux) 
  real, allocatable, dimension(:,:,:)               :: FREVI  ! diffuse flux reflected by veg layer (per unit incoming flux) 
  real, allocatable, dimension(:,:,:)               :: FREGD  ! direct flux reflected by ground (per unit incoming flux) 
  real, allocatable, dimension(:,:,:)               :: FREGI  ! direct flux reflected by ground (per unit incoming flux)
  real, allocatable, dimension(:,:,:)               :: RHO    ! leaf/stem reflectance weighted by fraction LAI and SAI
  real, allocatable, dimension(:,:,:)               :: TAU    ! leaf/stem transmittance weighted by fraction LAI and SAI
    
    ! Shortwave radiation
  real,allocatable,dimension(:,:)                   :: COSZ    ! cosine solar zenith angle [0-1]
  real,allocatable,dimension(:,:)                   :: COSZ_HORIZ ! cosine solar zenith angle for flat ground [0-1]
  real,allocatable,dimension(:,:)                   :: BGAP    ! between canopy gap fraction for beam (-)
  real,allocatable,dimension(:,:)                   :: WGAP    ! within canopy gap fraction for beam (-)
  REAL,allocatable,dimension(:,:)                   :: FSUN    ! sunlit fraction of canopy (-)
  REAL,allocatable,dimension(:,:)                   :: FSHA    ! shaded fraction of canopy (-)
  REAL,allocatable,dimension(:,:)                   :: LAISUN  ! sunlit leaf area (-)
  REAL,allocatable,dimension(:,:)                   :: LAISHA  ! shaded leaf area (-)
  REAL,allocatable,dimension(:,:)                   :: PARSUN  ! average absorbed par for sunlit leaves (w/m2)
  REAL,allocatable,dimension(:,:)                   :: PARSHA  ! average absorbed par for shaded leaves (w/m2)
  REAL,allocatable,dimension(:,:)                   :: SAV     ! solar radiation absorbed by vegetation (w/m2)
  REAL,allocatable,dimension(:,:)                   :: SAG     ! solar radiation absorbed by ground (w/m2)
  REAL,allocatable,dimension(:,:)                   :: FSA     ! total absorbed solar radiation (w/m2)
  REAL,allocatable,dimension(:,:)                   :: FSR     ! total reflected solar radiation (w/m2)
  REAL,allocatable,dimension(:,:)                   :: FSRV    ! reflected solar radiation by vegetation
  REAL,allocatable,dimension(:,:)                   :: FSRG    ! reflected solar radiation by ground
  
    ! Other, uncategorized
  REAL,allocatable,dimension(:,:)                   :: TAH     ! canopy air tmeperature (K)
  REAL,allocatable,dimension(:,:)                   :: EAH     ! canopy water vapor pressure (Pa)  
  REAL,allocatable,dimension(:,:)                   :: ZPD     ! zero plane displacement, ground (m)
  REAL,allocatable,dimension(:,:)                   :: Z0MG    ! z0 momentum, ground (m)
  REAL,allocatable,dimension(:,:)                   :: Z0M     ! roughness length, momentum (m)
  REAL,allocatable,dimension(:,:)                   :: ZLVL    ! reference height (m)  
  REAL,allocatable,dimension(:,:)                   :: CMV     ! momentum drag coefficient (vegetated surface)  
  REAL,allocatable,dimension(:,:)                   :: CMB     ! momentum drag coefficient (bare ground)  
  REAL,allocatable,dimension(:,:)                   :: CM      ! momentum drag coefficient (weighted version of CMV + CMB by FVEG)  
  REAL,allocatable,dimension(:,:)                   :: CH      ! drag coefficient for heat
  REAL,allocatable,dimension(:,:)                   :: TGB     ! ground temperature (K)
  REAL,allocatable,dimension(:,:)                   :: QSFC    ! mixing ratio at lowest model layer (g/g)
  REAL,allocatable,dimension(:,:)                   :: EMV     ! vegetation emissivity (-)
  REAL,allocatable,dimension(:,:)                   :: EMG     ! ground emissivity (-)
  REAL,allocatable,dimension(:,:)                   :: GAMMAV  ! psychrometric constant (Pa/K)
  REAL,allocatable,dimension(:,:)                   :: GAMMAG  ! psychrometric constant (Pa/K)  
!  REAL                                             :: GAMMA   ! psychrometric constant (Pa/K)  NOT USED IN CURRENT VERSION
  REAL,allocatable,dimension(:,:)                   :: EVC     ! evaporation heat flux (w/m2)  [+= to atm]  
  REAL,allocatable,dimension(:,:)                   :: IRC     ! net longwave radiation (w/m2) [+= to atm]
  REAL,allocatable,dimension(:,:)                   :: IRG     ! net longwave radiation (w/m2) [+= to atm]
  REAL,allocatable,dimension(:,:)                   :: SHC     ! sensible heat flux (w/m2)     [+= to atm]
  REAL,allocatable,dimension(:,:)                   :: SHG     ! sensible heat flux (w/m2)     [+= to atm]
  REAL,allocatable,dimension(:,:)                   :: SHB     ! sensible heat flux (w/m2) [+ to atm] 
  REAL,allocatable,dimension(:,:)                   :: EVG     ! evaporation heat flux (w/m2)  [+= to atm]
  REAL,allocatable,dimension(:,:)                   :: EVB     ! latent heat flux (w/m2)   [+ to atm]
  REAL,allocatable,dimension(:,:)                   :: TR      ! transpiration heat flux (w/m2)[+= to atm]
  REAL,allocatable,dimension(:,:)                   :: GH      ! ground heat (w/m2) [+ = to soil]
  REAL,allocatable,dimension(:,:)                   :: GHB     ! ground heat flux (w/m2)  [+ to soil] 
  REAL,allocatable,dimension(:,:)                   :: GHV     ! ground heat flux [w/m2]  [+ to soil]
  REAL,allocatable,dimension(:,:)                   :: T2MV    ! 2 m height air temperature (k)
  REAL,allocatable,dimension(:,:)                   :: CHLEAF  ! leaf exchange coefficient
  REAL,allocatable,dimension(:,:)                   :: CHUC    ! under canopy exchange coefficient
  REAL,allocatable,dimension(:,:)                   :: CHV2    ! sensible heat exch. coeff. over vegetated fraction (m/s)  
  REAL,allocatable,dimension(:,:)                   :: CHB2    ! sensible heat exch. coeff. bare-ground (m/s)
  REAL,allocatable,dimension(:,:)                   :: Q2V     ! check
  REAL,allocatable,dimension(:,:)                   :: LATHEAV ! latent heat of vaporization/subli (j/kg) (varies if froz)
  REAL,allocatable,dimension(:,:)                   :: LATHEAG ! latent heat of vaporization/subli (j/kg) (varies if froz)
  REAL,allocatable,dimension(:,:)                   :: LATHEA  ! latent heat of vaporization/subli (j/kg) (bare ground version)
  REAL,allocatable,dimension(:,:)                   :: RSURF   ! ground surface resistance (s/m)
  REAL,allocatable,dimension(:,:)                   :: RHSUR   ! relative humidity in surface soil/snow air space (-)  
  REAL,allocatable,dimension(:,:)                   :: TAUXV   ! wind stress: e-w (n/m2) vegetation
  REAL,allocatable,dimension(:,:)                   :: TAUYV   ! wind stress: n-s (n/m2) vegetation
  REAL,allocatable,dimension(:,:)                   :: TAUXB   ! wind stress: e-w (n/m2) bare ground
  REAL,allocatable,dimension(:,:)                   :: TAUYB   ! wind stress: n-s (n/m2) bare ground
  REAL,allocatable,dimension(:,:)                   :: TAUX    ! wind stress: e-w (n/m2)
  REAL,allocatable,dimension(:,:)                   :: TAUY    ! wind stress: n-s (n/m2)  
  REAL,allocatable,dimension(:,:)                   :: CAH2    ! sensible heat conductance for diagnostics
  REAL,allocatable,dimension(:,:)                   :: EHB2    ! sensible heat conductance for diagnostics (bare ground)
  REAL,allocatable,dimension(:,:)                   :: T2MB    ! 2 m height air temperature (K)  
  REAL,allocatable,dimension(:,:)                   :: Q2B     ! bare ground heat conductance
  REAL,allocatable,dimension(:,:)                   :: TGV     ! ground surface temp. [k]
  REAL,allocatable,dimension(:,:)                   :: CHV     ! sensible heat exchange coefficient
  REAL,allocatable,dimension(:,:)                   :: RSSUN   ! sunlit leaf stomatal resistance (s/m)
  REAL,allocatable,dimension(:,:)                   :: RSSHA   ! shaded leaf stomatal resistance (s/m)
  REAL,allocatable,dimension(:,:)                   :: RB      ! leaf boundary layer resistance (s/m)
  REAL,allocatable,dimension(:,:)                   :: FIRA    ! total net LW. rad (w/m2)   [+ to atm]
  REAL,allocatable,dimension(:,:)                   :: FSH     ! total sensible heat (w/m2) [+ to atm]
  REAL,allocatable,dimension(:,:)                   :: FGEV    ! ground evaporation (w/m2)  [+ to atm]
  REAL,allocatable,dimension(:,:)                   :: TRAD    ! radiative temperature (k)
  REAL,allocatable,dimension(:,:)                   :: IRB     ! net longwave rad. [w/m2] [+ to atm]
  REAL,allocatable,dimension(:,:)                   :: SSOIL   ! ground heat flux (w/m2)   [+ to soil]
  REAL,allocatable,dimension(:,:)                   :: T2M     ! 2-meter air temperature (k)
  REAL,allocatable,dimension(:,:)                   :: TS      ! surface temperature (k)
  REAL,allocatable,dimension(:,:)                   :: CHB     ! sensible heat exchange coefficient
  REAL,allocatable,dimension(:,:)                   :: Q1
  REAL,allocatable,dimension(:,:)                   :: Q2E
  REAL,allocatable,dimension(:,:)                   :: Z0WRF   ! combined z0 sent to coupled model
  REAL,allocatable,dimension(:,:)                   :: EMISSI  ! net surface emissivity  
  REAL,allocatable,dimension(:,:)                   :: PSN     ! total photosyn. (umolco2/m2/s) [+]
  REAL ,allocatable,dimension(:,:)                  :: PSNSUN  ! sunlit photosynthesis (umolco2/m2/s)  
  REAL,allocatable,dimension(:,:)                   :: PSNSHA  ! shaded photosynthesis (umolco2/m2/s)    
  REAL,allocatable,dimension(:,:)                   :: APAR    ! total photosyn. active energy (w/m2)
  REAL,allocatable,dimension(:,:)                   :: QMELT   ! snowmelt [mm/s]
  
  REAL,allocatable,dimension(:,:)                   :: LH      ! latent heat (total) flux [W m-2]
  REAL,allocatable,dimension(:,:)                   :: TGS     ! ground surface temperature (K, takes value of TG when SNOWH <= 0.05 and STC[0] when SNOWH > 0.05) 
                                  ! this is a temporary fix for when coupled to subsurface modules
                                  ! TODO: further investigation
  integer,allocatable,dimension(:,:)                :: ICE     ! 1 if sea ice, -1 if glacier, 0 if no land ice (seasonal snow)
    
    contains
  
      procedure, public  :: Init
      procedure, private :: InitAllocate        
      procedure, private :: InitDefault     
  
  end type
  
  contains   
  
    subroutine Init(this, namelist)
  
      class(energygrid_type)                :: this
      type(namelist_type)                   :: namelist
  
      call this%InitAllocate(namelist)
      call this%InitDefault()
  
    end subroutine Init
  
    subroutine InitAllocate(this, namelist)
  
      class(energygrid_type)                :: this
      type(namelist_type),intent(in)        :: namelist
  
      allocate(this%ALB(namelist%n_x,namelist%n_y))
      allocate(this%ALBD(namelist%n_x,namelist%n_y,2))
      allocate(this%ALBGRD(namelist%n_x,namelist%n_y,2))
      allocate(this%ALBGRI(namelist%n_x,namelist%n_y,2))
      allocate(this%ALBI(namelist%n_x,namelist%n_y,2))
      allocate(this%ALBOLD(namelist%n_x,namelist%n_y))
      allocate(this%ALBSND(namelist%n_x,namelist%n_y,2))
      allocate(this%ALBSNI(namelist%n_x,namelist%n_y,2))
      allocate(this%APAR(namelist%n_x,namelist%n_y))
      allocate(this%BGAP(namelist%n_x,namelist%n_y))
      allocate(this%CAH2(namelist%n_x,namelist%n_y))
      allocate(this%CH(namelist%n_x,namelist%n_y))
      allocate(this%CHB(namelist%n_x,namelist%n_y))
      allocate(this%CHB2(namelist%n_x,namelist%n_y))
      allocate(this%CHLEAF(namelist%n_x,namelist%n_y))
      allocate(this%CHUC(namelist%n_x,namelist%n_y))
      allocate(this%CHV(namelist%n_x,namelist%n_y))
      allocate(this%CHV2(namelist%n_x,namelist%n_y))
      allocate(this%CM(namelist%n_x,namelist%n_y))
      allocate(this%CMB(namelist%n_x,namelist%n_y))
      allocate(this%CMV(namelist%n_x,namelist%n_y))
      allocate(this%COSZ(namelist%n_x,namelist%n_y))
      allocate(this%COSZ_HORIZ(namelist%n_x,namelist%n_y))
      allocate(this%DF(namelist%n_x,namelist%n_y,-namelist%nsnow+1:namelist%nsoil))
      allocate(this%EAH(namelist%n_x,namelist%n_y))
      allocate(this%EHB2(namelist%n_x,namelist%n_y))
      allocate(this%EMG(namelist%n_x,namelist%n_y))
      allocate(this%EMISSI(namelist%n_x,namelist%n_y))
      allocate(this%EMV(namelist%n_x,namelist%n_y))
      allocate(this%EVB(namelist%n_x,namelist%n_y))
      allocate(this%EVC(namelist%n_x,namelist%n_y))
      allocate(this%EVG(namelist%n_x,namelist%n_y))
      allocate(this%FABD(namelist%n_x,namelist%n_y,2))
      allocate(this%FABI(namelist%n_x,namelist%n_y,2))
      allocate(this%FACT(namelist%n_x,namelist%n_y,-namelist%nsnow+1:namelist%nsoil))
      allocate(this%FAGE(namelist%n_x,namelist%n_y))
      allocate(this%FCEV(namelist%n_x,namelist%n_y))
      allocate(this%FCTR(namelist%n_x,namelist%n_y))
      allocate(this%FGEV(namelist%n_x,namelist%n_y))
      allocate(this%FIRA(namelist%n_x,namelist%n_y))
      allocate(this%FREGD(namelist%n_x,namelist%n_y,2))
      allocate(this%FREGI(namelist%n_x,namelist%n_y,2))
      allocate(this%FREVD(namelist%n_x,namelist%n_y,2))
      allocate(this%FREVI(namelist%n_x,namelist%n_y,2))
      allocate(this%FROZEN_CANOPY(namelist%n_x,namelist%n_y))
      allocate(this%FROZEN_GROUND(namelist%n_x,namelist%n_y))
      allocate(this%FSA(namelist%n_x,namelist%n_y))
      allocate(this%FSH(namelist%n_x,namelist%n_y))
      allocate(this%FSHA(namelist%n_x,namelist%n_y))
      allocate(this%FSR(namelist%n_x,namelist%n_y))
      allocate(this%FSRG(namelist%n_x,namelist%n_y))
      allocate(this%FSRV(namelist%n_x,namelist%n_y))
      allocate(this%FSUN(namelist%n_x,namelist%n_y))
      allocate(this%FTDD(namelist%n_x,namelist%n_y,2))
      allocate(this%FTDI(namelist%n_x,namelist%n_y,2))
      allocate(this%FTID(namelist%n_x,namelist%n_y,2))
      allocate(this%FTII(namelist%n_x,namelist%n_y,2))
      allocate(this%GAMMAG(namelist%n_x,namelist%n_y))
      allocate(this%GAMMAV(namelist%n_x,namelist%n_y))
      allocate(this%GH(namelist%n_x,namelist%n_y))
      allocate(this%GHB(namelist%n_x,namelist%n_y))
      allocate(this%GHV(namelist%n_x,namelist%n_y))
      allocate(this%HCPCT(namelist%n_x,namelist%n_y,-namelist%nsnow+1:namelist%nsoil))
      allocate(this%ICE(namelist%n_x,namelist%n_y))
      allocate(this%IGS(namelist%n_x,namelist%n_y))
      allocate(this%IMELT(namelist%n_x,namelist%n_y,-namelist%nsnow+1:namelist%nsoil))
      allocate(this%IRB(namelist%n_x,namelist%n_y))
      allocate(this%IRC(namelist%n_x,namelist%n_y))
      allocate(this%IRG(namelist%n_x,namelist%n_y))
      allocate(this%LAISHA(namelist%n_x,namelist%n_y))
      allocate(this%LAISUN(namelist%n_x,namelist%n_y))
      allocate(this%LATHEA(namelist%n_x,namelist%n_y))
      allocate(this%LATHEAG(namelist%n_x,namelist%n_y))
      allocate(this%LATHEAV(namelist%n_x,namelist%n_y))
      allocate(this%LH(namelist%n_x,namelist%n_y))
      allocate(this%PAH(namelist%n_x,namelist%n_y))
      allocate(this%PAHB(namelist%n_x,namelist%n_y))
      allocate(this%PAHG(namelist%n_x,namelist%n_y))
      allocate(this%PAHV(namelist%n_x,namelist%n_y))
      allocate(this%PARSHA(namelist%n_x,namelist%n_y))
      allocate(this%PARSUN(namelist%n_x,namelist%n_y))
      allocate(this%PSN(namelist%n_x,namelist%n_y))
      allocate(this%PSNSHA(namelist%n_x,namelist%n_y))
      allocate(this%PSNSUN(namelist%n_x,namelist%n_y))
      allocate(this%Q1(namelist%n_x,namelist%n_y))
      allocate(this%Q2B(namelist%n_x,namelist%n_y))
      allocate(this%Q2E(namelist%n_x,namelist%n_y))
      allocate(this%Q2V(namelist%n_x,namelist%n_y))
      allocate(this%QMELT(namelist%n_x,namelist%n_y))
      allocate(this%QSFC(namelist%n_x,namelist%n_y))
      allocate(this%RB(namelist%n_x,namelist%n_y))
      allocate(this%RHO(namelist%n_x,namelist%n_y,2))
      allocate(this%RHSUR(namelist%n_x,namelist%n_y))
      allocate(this%RSSHA(namelist%n_x,namelist%n_y))
      allocate(this%RSSUN(namelist%n_x,namelist%n_y))
      allocate(this%RSURF(namelist%n_x,namelist%n_y))
      allocate(this%SAG(namelist%n_x,namelist%n_y))
      allocate(this%SAV(namelist%n_x,namelist%n_y))
      allocate(this%SHB(namelist%n_x,namelist%n_y))
      allocate(this%SHC(namelist%n_x,namelist%n_y))
      allocate(this%SHG(namelist%n_x,namelist%n_y))
      allocate(this%SSOIL(namelist%n_x,namelist%n_y))
      allocate(this%STC(namelist%n_x,namelist%n_y,-namelist%nsnow+1:namelist%nsoil))
      allocate(this%T2M(namelist%n_x,namelist%n_y))
      allocate(this%T2MB(namelist%n_x,namelist%n_y))
      allocate(this%T2MV(namelist%n_x,namelist%n_y))
      allocate(this%TAH(namelist%n_x,namelist%n_y))
      allocate(this%TAU(namelist%n_x,namelist%n_y,2))
      allocate(this%TAUSS(namelist%n_x,namelist%n_y))
      allocate(this%TAUX(namelist%n_x,namelist%n_y))
      allocate(this%TAUXB(namelist%n_x,namelist%n_y))
      allocate(this%TAUXV(namelist%n_x,namelist%n_y))
      allocate(this%TAUY(namelist%n_x,namelist%n_y))
      allocate(this%TAUYB(namelist%n_x,namelist%n_y))
      allocate(this%TAUYV(namelist%n_x,namelist%n_y))
      allocate(this%TG(namelist%n_x,namelist%n_y))
      allocate(this%TGB(namelist%n_x,namelist%n_y))
      allocate(this%TGS(namelist%n_x,namelist%n_y))
      allocate(this%TGV(namelist%n_x,namelist%n_y))
      allocate(this%TR(namelist%n_x,namelist%n_y))
      allocate(this%TRAD(namelist%n_x,namelist%n_y))
      allocate(this%TS(namelist%n_x,namelist%n_y))
      allocate(this%TV(namelist%n_x,namelist%n_y))
      allocate(this%WGAP(namelist%n_x,namelist%n_y))
      allocate(this%Z0M(namelist%n_x,namelist%n_y))
      allocate(this%Z0MG(namelist%n_x,namelist%n_y))
      allocate(this%Z0WRF(namelist%n_x,namelist%n_y))
      allocate(this%ZLVL(namelist%n_x,namelist%n_y))
      allocate(this%ZPD(namelist%n_x,namelist%n_y))

    end subroutine InitAllocate
  
    subroutine InitDefault(this)
  
      class(energygrid_type) :: this
  
      this%ALB(:,:) = huge(1.0)
      this%ALBD(:,:,:) = huge(1.0)
      this%ALBGRD(:,:,:) = huge(1.0)
      this%ALBGRI(:,:,:) = huge(1.0)
      this%ALBI(:,:,:) = huge(1.0)
      this%ALBOLD(:,:) = huge(1.0)
      this%ALBSND(:,:,:) = huge(1.0)
      this%ALBSNI(:,:,:) = huge(1.0)
      this%APAR(:,:) = huge(1.0)
      this%BGAP(:,:) = huge(1.0)
      this%CAH2(:,:) = huge(1.0)
      this%CH(:,:) = huge(1.0)
      this%CHB(:,:) = huge(1.0)
      this%CHB2(:,:) = huge(1.0)
      this%CHLEAF(:,:) = huge(1.0)
      this%CHUC(:,:) = huge(1.0)
      this%CHV(:,:) = huge(1.0)
      this%CHV2(:,:) = huge(1.0)
      this%CM(:,:) = huge(1.0)
      this%CMB(:,:) = huge(1.0)
      this%CMV(:,:) = huge(1.0)
      this%COSZ(:,:) = huge(1.0)
      this%COSZ_HORIZ(:,:) = huge(1.0)
      this%DF(:,:,:) = huge(1.0)
      this%EAH(:,:) = huge(1.0)
      this%EHB2(:,:) = huge(1.0)
      this%EMG(:,:) = huge(1.0)
      this%EMISSI(:,:) = huge(1.0)
      this%EMV(:,:) = huge(1.0)
      this%EVB(:,:) = huge(1.0)
      this%EVC(:,:) = huge(1.0)
      this%EVG(:,:) = huge(1.0)
      this%FABD(:,:,:) = huge(1.0)
      this%FABI(:,:,:) = huge(1.0)
      this%FACT(:,:,:) = huge(1.0)
      this%FAGE(:,:) = huge(1.0)
      this%FCEV(:,:) = huge(1.0)
      this%FCTR(:,:) = huge(1.0)
      this%FGEV(:,:) = huge(1.0)
      this%FIRA(:,:) = huge(1.0)
      this%FREGD(:,:,:) = huge(1.0)
      this%FREGI(:,:,:) = huge(1.0)
      this%FREVD(:,:,:) = huge(1.0)
      this%FREVI(:,:,:) = huge(1.0)
      this%FROZEN_CANOPY(:,:) = .FALSE.
      this%FROZEN_GROUND(:,:) = .FALSE.
      this%FSA(:,:) = huge(1.0)
      this%FSH(:,:) = huge(1.0)
      this%FSHA(:,:) = huge(1.0)
      this%FSR(:,:) = huge(1.0)
      this%FSRG(:,:) = huge(1.0)
      this%FSRV(:,:) = huge(1.0)
      this%FSUN(:,:) = huge(1.0)
      this%FTDD(:,:,:) = huge(1.0)
      this%FTDI(:,:,:) = huge(1.0)
      this%FTID(:,:,:) = huge(1.0)
      this%FTII(:,:,:) = huge(1.0)
      this%GAMMAG(:,:) = huge(1.0)
      this%GAMMAV(:,:) = huge(1.0)
      this%GH(:,:) = huge(1.0)
      this%GHB(:,:) = huge(1.0)
      this%GHV(:,:) = huge(1.0)
      this%HCPCT(:,:,:) = huge(1.0)
      this%ICE(:,:) = huge(1)
      this%IGS(:,:) = huge(1.0)
      this%IMELT(:,:,:) = huge(1)
      this%IRB(:,:) = huge(1.0)
      this%IRC(:,:) = huge(1.0)
      this%IRG(:,:) = huge(1.0)
      this%LAISHA(:,:) = huge(1.0)
      this%LAISUN(:,:) = huge(1.0)
      this%LATHEA(:,:) = huge(1.0)
      this%LATHEAG(:,:) = huge(1.0)
      this%LATHEAV(:,:) = huge(1.0)
      this%LH(:,:) = huge(1.0)
      this%PAH(:,:) = huge(1.0)
      this%PAHB(:,:) = huge(1.0)
      this%PAHG(:,:) = huge(1.0)
      this%PAHV(:,:) = huge(1.0)
      this%PARSHA(:,:) = huge(1.0)
      this%PARSUN(:,:) = huge(1.0)
      this%PSN(:,:) = huge(1.0)
      this%PSNSHA(:,:) = huge(1.0)
      this%PSNSUN(:,:) = huge(1.0)
      this%Q1(:,:) = huge(1.0)
      this%Q2B(:,:) = huge(1.0)
      this%Q2E(:,:) = huge(1.0)
      this%Q2V(:,:) = huge(1.0)
      this%QMELT(:,:) = huge(1.0)
      this%QSFC(:,:) = huge(1.0)
      this%RB(:,:) = huge(1.0)
      this%RHO(:,:,:) = huge(1.0)
      this%RHSUR(:,:) = huge(1.0)
      this%RSSHA(:,:) = huge(1.0)
      this%RSSUN(:,:) = huge(1.0)
      this%RSURF(:,:) = huge(1.0)
      this%SAG(:,:) = huge(1.0)
      this%SAV(:,:) = huge(1.0)
      this%SHB(:,:) = huge(1.0)
      this%SHC(:,:) = huge(1.0)
      this%SHG(:,:) = huge(1.0)
      this%SSOIL(:,:) = huge(1.0)
      this%STC(:,:,:) = huge(1.0)
      this%T2M(:,:) = huge(1.0)
      this%T2MB(:,:) = huge(1.0)
      this%T2MV(:,:) = huge(1.0)
      this%TAH(:,:) = huge(1.0)
      this%TAU(:,:,:) = huge(1.0)
      this%TAUSS(:,:) = huge(1.0)
      this%TAUX(:,:) = huge(1.0)
      this%TAUXB(:,:) = huge(1.0)
      this%TAUXV(:,:) = huge(1.0)
      this%TAUY(:,:) = huge(1.0)
      this%TAUYB(:,:) = huge(1.0)
      this%TAUYV(:,:) = huge(1.0)
      this%TG(:,:) = huge(1.0)
      this%TGB(:,:) = huge(1.0)
      this%TGS(:,:) = huge(1.0)
      this%TGV(:,:) = huge(1.0)
      this%TR(:,:) = huge(1.0)
      this%TRAD(:,:) = huge(1.0)
      this%TS(:,:) = huge(1.0)
      this%TV(:,:) = huge(1.0)
      this%WGAP(:,:) = huge(1.0)
      this%Z0M(:,:) = huge(1.0)
      this%Z0MG(:,:) = huge(1.0)
      this%Z0WRF(:,:) = huge(1.0)
      this%ZLVL(:,:) = huge(1.0)
      this%ZPD(:,:) = huge(1.0)
      
    end subroutine InitDefault
  
    subroutine InitTransfer(this, namelist)

      class(energygrid_type) :: this
      type(namelist_type)    :: namelist
  
      ! Nothing to do.

    end subroutine InitTransfer

  end module EnergyType