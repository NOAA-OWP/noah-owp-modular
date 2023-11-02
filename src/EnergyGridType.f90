module EnergyGridType

  use NamelistRead,   only: namelist_type
  use AttributesType, only: attributes_type
  
  implicit none
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
  real,allocatable, dimension(:,:)                  :: SNOWT_AVG         ! average snow temperature [k] (by layer mass) (a NWM 3.0 output variable)
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
      procedure, public  :: InitTransfer
  
  end type
  
  contains   
  
    subroutine Init(this, namelist, attributes)
  
      class(energygrid_type)                :: this
      type(namelist_type)                   :: namelist
      type(attributes_type)                 :: attributes
  
      call this%InitAllocate(namelist,attributes)
      call this%InitDefault()
  
    end subroutine Init
  
    subroutine InitAllocate(this, namelist, attributes)
  
      class(energygrid_type), intent(inout) :: this
      type(namelist_type),    intent(in)    :: namelist
      type(attributes_type),  intent(in)    :: attributes
  
      associate(n_x   => attributes%metadata%n_x,   &
                n_y   => attributes%metadata%n_y,   &
                nsoil => namelist%nsoil,            &
                nsnow => namelist%nsnow)

      allocate(this%ALB(n_x,n_y))
      allocate(this%ALBD(n_x,n_y,2))
      allocate(this%ALBGRD(n_x,n_y,2))
      allocate(this%ALBGRI(n_x,n_y,2))
      allocate(this%ALBI(n_x,n_y,2))
      allocate(this%ALBOLD(n_x,n_y))
      allocate(this%ALBSND(n_x,n_y,2))
      allocate(this%ALBSNI(n_x,n_y,2))
      allocate(this%APAR(n_x,n_y))
      allocate(this%BGAP(n_x,n_y))
      allocate(this%CAH2(n_x,n_y))
      allocate(this%CH(n_x,n_y))
      allocate(this%CHB(n_x,n_y))
      allocate(this%CHB2(n_x,n_y))
      allocate(this%CHLEAF(n_x,n_y))
      allocate(this%CHUC(n_x,n_y))
      allocate(this%CHV(n_x,n_y))
      allocate(this%CHV2(n_x,n_y))
      allocate(this%CM(n_x,n_y))
      allocate(this%CMB(n_x,n_y))
      allocate(this%CMV(n_x,n_y))
      allocate(this%COSZ(n_x,n_y))
      allocate(this%COSZ_HORIZ(n_x,n_y))
      allocate(this%DF(n_x,n_y,-nsnow+1:nsoil))
      allocate(this%EAH(n_x,n_y))
      allocate(this%EHB2(n_x,n_y))
      allocate(this%EMG(n_x,n_y))
      allocate(this%EMISSI(n_x,n_y))
      allocate(this%EMV(n_x,n_y))
      allocate(this%EVB(n_x,n_y))
      allocate(this%EVC(n_x,n_y))
      allocate(this%EVG(n_x,n_y))
      allocate(this%FABD(n_x,n_y,2))
      allocate(this%FABI(n_x,n_y,2))
      allocate(this%FACT(n_x,n_y,-nsnow+1:nsoil))
      allocate(this%FAGE(n_x,n_y))
      allocate(this%FCEV(n_x,n_y))
      allocate(this%FCTR(n_x,n_y))
      allocate(this%FGEV(n_x,n_y))
      allocate(this%FIRA(n_x,n_y))
      allocate(this%FREGD(n_x,n_y,2))
      allocate(this%FREGI(n_x,n_y,2))
      allocate(this%FREVD(n_x,n_y,2))
      allocate(this%FREVI(n_x,n_y,2))
      allocate(this%FROZEN_CANOPY(n_x,n_y))
      allocate(this%FROZEN_GROUND(n_x,n_y))
      allocate(this%FSA(n_x,n_y))
      allocate(this%FSH(n_x,n_y))
      allocate(this%FSHA(n_x,n_y))
      allocate(this%FSR(n_x,n_y))
      allocate(this%FSRG(n_x,n_y))
      allocate(this%FSRV(n_x,n_y))
      allocate(this%FSUN(n_x,n_y))
      allocate(this%FTDD(n_x,n_y,2))
      allocate(this%FTDI(n_x,n_y,2))
      allocate(this%FTID(n_x,n_y,2))
      allocate(this%FTII(n_x,n_y,2))
      allocate(this%GAMMAG(n_x,n_y))
      allocate(this%GAMMAV(n_x,n_y))
      allocate(this%GH(n_x,n_y))
      allocate(this%GHB(n_x,n_y))
      allocate(this%GHV(n_x,n_y))
      allocate(this%HCPCT(n_x,n_y,-nsnow+1:nsoil))
      allocate(this%ICE(n_x,n_y))
      allocate(this%IGS(n_x,n_y))
      allocate(this%IMELT(n_x,n_y,-nsnow+1:nsoil))
      allocate(this%IRB(n_x,n_y))
      allocate(this%IRC(n_x,n_y))
      allocate(this%IRG(n_x,n_y))
      allocate(this%LAISHA(n_x,n_y))
      allocate(this%LAISUN(n_x,n_y))
      allocate(this%LATHEA(n_x,n_y))
      allocate(this%LATHEAG(n_x,n_y))
      allocate(this%LATHEAV(n_x,n_y))
      allocate(this%LH(n_x,n_y))
      allocate(this%PAH(n_x,n_y))
      allocate(this%PAHB(n_x,n_y))
      allocate(this%PAHG(n_x,n_y))
      allocate(this%PAHV(n_x,n_y))
      allocate(this%PARSHA(n_x,n_y))
      allocate(this%PARSUN(n_x,n_y))
      allocate(this%PSN(n_x,n_y))
      allocate(this%PSNSHA(n_x,n_y))
      allocate(this%PSNSUN(n_x,n_y))
      allocate(this%Q1(n_x,n_y))
      allocate(this%Q2B(n_x,n_y))
      allocate(this%Q2E(n_x,n_y))
      allocate(this%Q2V(n_x,n_y))
      allocate(this%QMELT(n_x,n_y))
      allocate(this%QSFC(n_x,n_y))
      allocate(this%RB(n_x,n_y))
      allocate(this%RHO(n_x,n_y,2))
      allocate(this%RHSUR(n_x,n_y))
      allocate(this%RSSHA(n_x,n_y))
      allocate(this%RSSUN(n_x,n_y))
      allocate(this%RSURF(n_x,n_y))
      allocate(this%SAG(n_x,n_y))
      allocate(this%SAV(n_x,n_y))
      allocate(this%SHB(n_x,n_y))
      allocate(this%SHC(n_x,n_y))
      allocate(this%SHG(n_x,n_y))
      allocate(this%SSOIL(n_x,n_y))
      allocate(this%STC(n_x,n_y,-nsnow+1:nsoil))
      allocate(this%SNOWT_AVG(n_x,n_y))
      allocate(this%T2M(n_x,n_y))
      allocate(this%T2MB(n_x,n_y))
      allocate(this%T2MV(n_x,n_y))
      allocate(this%TAH(n_x,n_y))
      allocate(this%TAU(n_x,n_y,2))
      allocate(this%TAUSS(n_x,n_y))
      allocate(this%TAUX(n_x,n_y))
      allocate(this%TAUXB(n_x,n_y))
      allocate(this%TAUXV(n_x,n_y))
      allocate(this%TAUY(n_x,n_y))
      allocate(this%TAUYB(n_x,n_y))
      allocate(this%TAUYV(n_x,n_y))
      allocate(this%TG(n_x,n_y))
      allocate(this%TGB(n_x,n_y))
      allocate(this%TGS(n_x,n_y))
      allocate(this%TGV(n_x,n_y))
      allocate(this%TR(n_x,n_y))
      allocate(this%TRAD(n_x,n_y))
      allocate(this%TS(n_x,n_y))
      allocate(this%TV(n_x,n_y))
      allocate(this%WGAP(n_x,n_y))
      allocate(this%Z0M(n_x,n_y))
      allocate(this%Z0MG(n_x,n_y))
      allocate(this%Z0WRF(n_x,n_y))
      allocate(this%ZLVL(n_x,n_y))
      allocate(this%ZPD(n_x,n_y))

      end associate
      
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
      this%SNOWT_AVG(:,:) = huge(1.0)
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

  end module EnergyGridType