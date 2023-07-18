module OutputModule

!---------------------------------------------------------------------
! Compiler directive NGEN_OUTPUT_ACTIVE to be defined if 
! Nextgen is writing model output (https://github.com/NOAA-OWP/ngen)
!---------------------------------------------------------------------
#ifndef NGEN_OUTPUT_ACTIVE
  use netcdf
  use WaterType
  use EnergyType
  use DomainType
  use ForcingType  

  implicit none

  integer           :: ncid
  integer           :: iret
  integer           :: time_dim
  integer           :: x_dim
  integer           :: y_dim
  integer           :: soil_dim
  integer           :: snow_dim
  integer           :: snso_dim
  integer           :: time_id
  integer           :: evap_id
  integer           :: tran_id
  integer           :: evapotrans_id
  integer           :: smc_id
  !integer           :: smcm_id
  integer           :: prcp_id
  integer           :: sfrn_id
  integer           :: ugrn_id
  integer           :: qinsur_id

  integer           :: qintr_id
  integer           :: qints_id
  integer           :: qdripr_id
  integer           :: qdrips_id
  integer           :: qthror_id
  integer           :: qthros_id
  integer           :: qrain_id
  integer           :: qsnow_id
  integer           :: snowhin_id
  integer           :: fwet_id
  integer           :: cmc_id
  integer           :: canliq_id
  integer           :: canice_id
  integer           :: ecan_id
  integer           :: etran_id

  integer           :: snowh_id
  integer           :: sneqv_id
  integer           :: ponding_id
  integer           :: ponding1_id
  integer           :: ponding2_id
  integer           :: qsnbot_id
  integer           :: qsnfro_id
  integer           :: qsnsub_id
  integer           :: snice_id
  integer           :: snliq_id
  integer           :: stc_id
  integer           :: zsnso_id
  ! energy
  integer           :: swin_id
  integer           :: lh_id
  integer           :: t2m_id
  integer           :: t2mb_id
  integer           :: t2mv_id

contains

subroutine initialize_output(output_filename, ntime, nsoil, nsnow, n_x, n_y)
 
  character*256, intent(in)    :: output_filename
  integer, intent(in)          :: ntime
  integer, intent(in)          :: nsoil
  integer, intent(in)          :: nsnow
  integer, intent(in)          :: n_x
  integer, intent(in)          :: n_y
  ! local
  character (len=*), parameter :: time_units = "seconds since 1970-01-01 00:00:00"
  integer                      :: iret   ! error code

  ! create output file and define structure (dimensions)
  iret = nf90_create(trim(output_filename), NF90_CLOBBER, ncid)
  iret = nf90_def_dim(ncid, "time", ntime, time_dim)
  iret = nf90_def_dim(ncid, "soil", nsoil, soil_dim)
  iret = nf90_def_dim(ncid, "snow", nsnow, snow_dim)
  iret = nf90_def_dim(ncid, "snso", nsnow+nsoil, snso_dim)
  iret = nf90_def_dim(ncid, "x", n_x, x_dim)
  iret = nf90_def_dim(ncid, "y", n_y, y_dim)

  ! time variable (need units / time datum)
  !iret = nf90_def_var(ncid, "timestep",             NF90_INT  , (/time_dim/), time_id)
  !iret = nf90_def_var(ncid, "time",                 NF90_DOUBLE, (/time_dim/), time_id)
  call check (nf90_def_var(ncid, "time", NF90_DOUBLE, (/time_dim/), time_id), "time var def error", iret); if (iret /= 0) return   ! with error checking
  iret = nf90_put_att(ncid, time_id, "units", time_units)
  !if (error /= 0) return
      
  ! for soil water
  iret = nf90_def_var(ncid, "Precipitation",        NF90_FLOAT, (/x_dim,y_dim,time_dim/), prcp_id)
  iret = nf90_def_var(ncid, "QINSUR",               NF90_FLOAT, (/x_dim,y_dim,time_dim/), qinsur_id)
  iret = nf90_def_var(ncid, "SFCRNOFF",             NF90_FLOAT, (/x_dim,y_dim,time_dim/), sfrn_id)
  iret = nf90_def_var(ncid, "UGDRNOFF",             NF90_FLOAT, (/x_dim,y_dim,time_dim/), ugrn_id)
  iret = nf90_def_var(ncid, "evaporation",          NF90_FLOAT, (/x_dim,y_dim,time_dim/), evap_id)
  iret = nf90_def_var(ncid, "transpiration",        NF90_FLOAT, (/x_dim,y_dim,time_dim/), tran_id)
  iret = nf90_def_var(ncid, "evapotranspiration",   NF90_FLOAT, (/x_dim,y_dim,time_dim/), evapotrans_id)
  !iret = nf90_def_var(ncid, "soil_moisture_mm",     NF90_FLOAT, (/time_dim,soil_dim/), smcm_id)
  iret = nf90_def_var(ncid, "SMC",                  NF90_FLOAT, (/x_dim,y_dim,time_dim,soil_dim/), smc_id)
  ! for canopy water
  iret = nf90_def_var(ncid, "rain_intercept",       NF90_FLOAT, (/x_dim,y_dim,time_dim/), qintr_id)
  iret = nf90_def_var(ncid, "snow_intercept",       NF90_FLOAT, (/x_dim,y_dim,time_dim/), qints_id)
  iret = nf90_def_var(ncid, "rain_drip",            NF90_FLOAT, (/x_dim,y_dim,time_dim/), qdripr_id)
  iret = nf90_def_var(ncid, "snow_drip",            NF90_FLOAT, (/x_dim,y_dim,time_dim/), qdrips_id)
  iret = nf90_def_var(ncid, "rain_through",         NF90_FLOAT, (/x_dim,y_dim,time_dim/), qthror_id)
  iret = nf90_def_var(ncid, "snow_through",         NF90_FLOAT, (/x_dim,y_dim,time_dim/), qthros_id)
  iret = nf90_def_var(ncid, "rain_surface",         NF90_FLOAT, (/x_dim,y_dim,time_dim/), qrain_id)
  iret = nf90_def_var(ncid, "snow_surface",         NF90_FLOAT, (/x_dim,y_dim,time_dim/), qsnow_id)
  iret = nf90_def_var(ncid, "snowhin",              NF90_FLOAT, (/x_dim,y_dim,time_dim/), snowhin_id)
  iret = nf90_def_var(ncid, "FWET",                 NF90_FLOAT, (/x_dim,y_dim,time_dim/), fwet_id)
  iret = nf90_def_var(ncid, "CMC",                  NF90_FLOAT, (/x_dim,y_dim,time_dim/), cmc_id)
  iret = nf90_def_var(ncid, "CANLIQ",               NF90_FLOAT, (/x_dim,y_dim,time_dim/), canliq_id)
  iret = nf90_def_var(ncid, "CANICE",               NF90_FLOAT, (/x_dim,y_dim,time_dim/), canice_id)
  iret = nf90_def_var(ncid, "ECAN",                 NF90_FLOAT, (/x_dim,y_dim,time_dim/), ecan_id)
  iret = nf90_def_var(ncid, "ETRAN",                NF90_FLOAT, (/x_dim,y_dim,time_dim/), etran_id)
  ! for snow water
  iret = nf90_def_var(ncid, "SNOWH",                NF90_FLOAT, (/x_dim,y_dim,time_dim/), snowh_id) 
  iret = nf90_def_var(ncid, "SNEQV",                NF90_FLOAT, (/x_dim,y_dim,time_dim/), sneqv_id)
  iret = nf90_def_var(ncid, "PONDING",              NF90_FLOAT, (/x_dim,y_dim,time_dim/), ponding_id)
  iret = nf90_def_var(ncid, "PONDING1",             NF90_FLOAT, (/x_dim,y_dim,time_dim/), ponding1_id)
  iret = nf90_def_var(ncid, "PONDING2",             NF90_FLOAT, (/x_dim,y_dim,time_dim/), ponding2_id)
  iret = nf90_def_var(ncid, "QSNBOT",               NF90_FLOAT, (/x_dim,y_dim,time_dim/), qsnbot_id)
  iret = nf90_def_var(ncid, "QSNFRO",               NF90_FLOAT, (/x_dim,y_dim,time_dim/), qsnfro_id)
  iret = nf90_def_var(ncid, "QSNSUB",               NF90_FLOAT, (/x_dim,y_dim,time_dim/), qsnsub_id)
  iret = nf90_def_var(ncid, "SNICE",                NF90_FLOAT, (/x_dim,y_dim,time_dim,snow_dim/), snice_id)
  iret = nf90_def_var(ncid, "SNLIQ",                NF90_FLOAT, (/x_dim,y_dim,time_dim,snow_dim/), snliq_id)
  iret = nf90_def_var(ncid, "STC",                  NF90_FLOAT, (/x_dim,y_dim,time_dim,snso_dim/), stc_id)
  iret = nf90_def_var(ncid, "ZSNSO",                NF90_FLOAT, (/x_dim,y_dim,time_dim,snso_dim/), zsnso_id)
  
  ! energy
  iret = nf90_def_var(ncid, "SW_IN",                NF90_FLOAT, (/x_dim,y_dim,time_dim/), swin_id)  ! incoming shortwave, corrected for slope/aspect (W/m^2)
  iret = nf90_def_var(ncid, "LH",                   NF90_FLOAT, (/x_dim,y_dim,time_dim/), lh_id)  ! latent heat (W/m^2)
  iret = nf90_def_var(ncid, "T2M",                  NF90_FLOAT, (/x_dim,y_dim,time_dim/), t2m_id)  ! 2 m height air temperature (K) 
  iret = nf90_def_var(ncid, "T2MB",                 NF90_FLOAT, (/x_dim,y_dim,time_dim/), t2mb_id)  ! 2 m height air temperature (K) bare ground
  iret = nf90_def_var(ncid, "T2MV",                 NF90_FLOAT, (/x_dim,y_dim,time_dim/), t2mv_id)  ! 2 m height air temperature (K) vegetated
  
  iret = nf90_enddef(ncid)

end subroutine initialize_output

subroutine add_to_output(domain, water, energy, forcing, itime, nsoil, nsnow)

  type (domain_type), intent(in)    :: domain
  type (energy_type), intent(in)    :: energy
  type ( water_type), intent(in)    :: water
  type (forcing_type), intent(in)   :: forcing
  
  integer, intent(in)               :: itime
  integer, intent(in)               :: nsoil
  integer, intent(in)               :: nsnow

  ! associate variables to keep variable names intact in the code below  
  associate(&
    dt         => domain%dt          ,&   ! intent(in)    : model timestep (s)  
    dzsnso     => domain%dzsnso      ,&   ! intent(in)    : model layer thickness [m] 
    zsnso      => domain%dzsnso      ,&   ! intent(in)    : layer depth [m] 
    iloc       => domain%ix          ,&
    jloc       => domain%iy           &
  ) ! ---- end associate defs --------------------------------------------------------------------
  
  ! === store current timestep variable data in output file ===

  ! time variable
  iret = nf90_put_var(ncid, time_id,    domain%curr_datetime,        start=(/itime/))

  ! for soil water
  !smcmm = water%smc*dzsnso*1000.0 ! issues with this conversion
  !iret = nf90_put_var(ncid, prcp_id,    water%rain*dt,               start=(/itime/))
  iret = nf90_put_var(ncid, prcp_id,    forcing%prcp*dt,             start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, qinsur_id,  water%qinsur*1000*dt,        start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, sfrn_id,    water%runsrf*dt,             start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, ugrn_id,    water%runsub*dt,             start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, evap_id,    water%qseva*1000.0*dt,       start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, tran_id,    sum(water%etrani)*1000.0*dt, start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, evapotrans_id, water%evapotrans*1000.0*dt, start=(/iloc,jloc,itime/))
  !iret = nf90_put_var(ncid, smcm_id,    smcmm,       start=(/itime+1,1/), count=(/1,nsoil/))
  iret = nf90_put_var(ncid,  smc_id,    water%smc,                   start=(/iloc,jloc,itime,1/), count=(/1,nsoil/))
  ! for canopy water
  iret = nf90_put_var(ncid, qintr_id,   water%qintr*dt,              start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, qints_id,   water%qints*dt,              start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, qdripr_id,  water%qdripr*dt,             start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, qdrips_id,  water%qdrips*dt,             start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, qthror_id,  water%qthror*dt,             start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, qthros_id,  water%qthros*dt,             start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, qrain_id,   water%qrain*dt,              start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, qsnow_id,   water%qsnow*dt,              start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, snowhin_id, water%snowhin*dt,            start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, fwet_id,    water%fwet,                  start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, cmc_id,     water%cmc,                   start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, canliq_id,  water%canliq,                start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, canice_id,  water%canice,                start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, ecan_id,    water%ecan*dt,               start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, etran_id,   water%etran*dt,              start=(/iloc,jloc,itime/))
  ! for snow water
  iret = nf90_put_var(ncid, snowh_id,   water%snowh,                start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, sneqv_id,   water%sneqv,                start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, ponding_id, water%ponding,              start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, ponding1_id,water%ponding1,             start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, ponding2_id,water%ponding2,             start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, qsnbot_id,  water%qsnbot*dt,            start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, qsnfro_id,  water%qsnfro*dt,            start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, qsnsub_id,  water%qsnsub*dt,            start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, snice_id,   water%snice,                start=(/iloc,jloc,itime,1/), count=(/1,nsnow/))
  iret = nf90_put_var(ncid, snliq_id,   water%snliq,                start=(/iloc,jloc,itime,1/), count=(/1,nsnow/))
  iret = nf90_put_var(ncid, stc_id,     energy%stc,                 start=(/iloc,jloc,itime,1/), count=(/1,nsoil+nsnow/))
  iret = nf90_put_var(ncid, zsnso_id,   zsnso,                      start=(/iloc,jloc,itime,1/), count=(/1,nsoil+nsnow/))
  ! energy
  iret = nf90_put_var(ncid, swin_id,    forcing%SWDOWN,             start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, lh_id,      energy%lh,                  start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, t2m_id,     energy%t2m,                 start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, t2mb_id,    energy%t2mb,                start=(/iloc,jloc,itime/))
  iret = nf90_put_var(ncid, t2mv_id,    energy%t2mv,                start=(/iloc,jloc,itime/))
  
  end associate

end subroutine add_to_output

! close output file(s)
subroutine finalize_output()

  iret = nf90_close(ncid)

end subroutine finalize_output

! handle file manipulation errors
subroutine check (status, info, error)
  integer, intent (in) :: status
  character (len=*), intent (in) :: info
  integer, intent (out) :: error
  
  if (status /= nf90_noerr) then
    print *, trim (info) // ": " // trim (nf90_strerror(status))
    error = 1
  end if

end subroutine check  

#endif     ! end of block to remove output if NGEN_OUTPUT_ACTIVE directive is True

end module OutputModule

