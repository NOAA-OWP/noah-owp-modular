module NoahMPOutput

  use netcdf
  use WaterType
  use EnergyType

  implicit none

  integer           :: ncid
  integer           :: iret
  integer           :: time_dim
  integer           :: soil_dim
  integer           :: snow_dim
  integer           :: snso_dim
  integer           :: time_id
  integer           :: evap_id
  integer           :: tran_id
  integer           :: smc_id
!  integer           :: smcm_id
  integer           :: prcp_id
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

contains

  subroutine initialize_output(output_filename, ntime, nsoil, nsnow)
 
    integer       :: ntime
    character*256 :: output_filename
    integer       :: nsoil
    integer       :: nsnow

    iret = nf90_create(trim(output_filename), NF90_CLOBBER, ncid)
    iret = nf90_def_dim(ncid, "time", ntime, time_dim)
    iret = nf90_def_dim(ncid, "soil", nsoil, soil_dim)
    iret = nf90_def_dim(ncid, "snow", nsnow, snow_dim)
    iret = nf90_def_dim(ncid, "snso", nsnow+nsoil, snso_dim)
! for soil water
    iret = nf90_def_var(ncid, "timestep",             NF90_INT  , (/time_dim/), time_id)
    iret = nf90_def_var(ncid, "precipitation",        NF90_FLOAT, (/time_dim/), prcp_id)
    iret = nf90_def_var(ncid, "waterin_sfc",        NF90_FLOAT, (/time_dim/), qinsur_id)
    iret = nf90_def_var(ncid, "evaporation",          NF90_FLOAT, (/time_dim/), evap_id)
    iret = nf90_def_var(ncid, "transpiration",        NF90_FLOAT, (/time_dim/), tran_id)
    iret = nf90_def_var(ncid, "soil_moisture",        NF90_FLOAT, (/time_dim,soil_dim/), smc_id)
! for canopy water
    iret = nf90_def_var(ncid, "rain_intercept",        NF90_FLOAT, (/time_dim/), qintr_id)
    iret = nf90_def_var(ncid, "snow_intercept",        NF90_FLOAT, (/time_dim/), qints_id)
    iret = nf90_def_var(ncid, "rain_drip",        NF90_FLOAT, (/time_dim/), qdripr_id)
    iret = nf90_def_var(ncid, "snow_drip",        NF90_FLOAT, (/time_dim/), qdrips_id)
    iret = nf90_def_var(ncid, "rain_through",        NF90_FLOAT, (/time_dim/), qthror_id)
    iret = nf90_def_var(ncid, "snow_through",        NF90_FLOAT, (/time_dim/), qthros_id)
    iret = nf90_def_var(ncid, "rain_surface",        NF90_FLOAT, (/time_dim/), qrain_id)
    iret = nf90_def_var(ncid, "snow_surface",        NF90_FLOAT, (/time_dim/), qsnow_id)
    iret = nf90_def_var(ncid, "snowhin",        NF90_FLOAT, (/time_dim/), snowhin_id)
    iret = nf90_def_var(ncid, "FWET",        NF90_FLOAT, (/time_dim/), fwet_id)
    iret = nf90_def_var(ncid, "CMC",        NF90_FLOAT, (/time_dim/), cmc_id)
    iret = nf90_def_var(ncid, "CANLIQ",        NF90_FLOAT, (/time_dim/), canliq_id)
    iret = nf90_def_var(ncid, "CANICE",        NF90_FLOAT, (/time_dim/), canice_id)
    iret = nf90_def_var(ncid, "ECAN",        NF90_FLOAT, (/time_dim/), ecan_id)
    iret = nf90_def_var(ncid, "ETRAN",        NF90_FLOAT, (/time_dim/), etran_id)
! for snow water
    iret = nf90_def_var(ncid, "SNOWH",        NF90_FLOAT, (/time_dim/), snowh_id) 
    iret = nf90_def_var(ncid, "SNEQV",        NF90_FLOAT, (/time_dim/), sneqv_id)
    iret = nf90_def_var(ncid, "PONDING",        NF90_FLOAT, (/time_dim/), ponding_id)
    iret = nf90_def_var(ncid, "PONDING1",        NF90_FLOAT, (/time_dim/), ponding1_id)
    iret = nf90_def_var(ncid, "PONDING2",        NF90_FLOAT, (/time_dim/), ponding2_id)
    iret = nf90_def_var(ncid, "QSNBOT",        NF90_FLOAT, (/time_dim/), qsnbot_id)
    iret = nf90_def_var(ncid, "QSNFRO",        NF90_FLOAT, (/time_dim/), qsnfro_id)
    iret = nf90_def_var(ncid, "QSNSUB",        NF90_FLOAT, (/time_dim/), qsnsub_id)
    iret = nf90_def_var(ncid, "SNICE",        NF90_FLOAT, (/time_dim,snow_dim/), snice_id)
    iret = nf90_def_var(ncid, "SNLIQ",        NF90_FLOAT, (/time_dim,snow_dim/), snliq_id)
    iret = nf90_def_var(ncid, "STC",        NF90_FLOAT, (/time_dim,snso_dim/), stc_id)
    iret = nf90_def_var(ncid, "ZSNSO",        NF90_FLOAT, (/time_dim,snso_dim/), zsnso_id)

    iret = nf90_enddef(ncid)
  
   end subroutine initialize_output

   subroutine add_to_output(itime,nsoil,nsnow,dzsnso,dt,zsnso,water,energy)

     type (water_type)      :: water
     type (energy_type)     :: energy
     integer                :: itime
     integer                :: nsoil
     integer                :: nsnow
     real                   :: dt
     real, dimension(nsoil+nsnow) :: dzsnso      !soil level thickness [m] 
!     real, dimension(nsoil) :: smcmm       !total soil water content [mm]
     real, dimension(nsoil+nsnow) :: zsnso     

! for soil water
     iret = nf90_put_var(ncid, time_id,    itime,                       start=(/itime+1/))
     iret = nf90_put_var(ncid, prcp_id,    water%rain*dt,               start=(/itime+1/))
     iret = nf90_put_var(ncid, qinsur_id,  water%qinsur*1000*dt,        start=(/itime+1/))
     iret = nf90_put_var(ncid, evap_id,    water%qseva*1000.0*dt,       start=(/itime+1/))
     iret = nf90_put_var(ncid, tran_id,    sum(water%etrani)*1000.0*dt, start=(/itime+1/))
     iret = nf90_put_var(ncid,  smc_id,    water%smc,   start=(/itime+1,1/), count=(/1,nsoil/))
! for canopy water
     iret = nf90_put_var(ncid, qintr_id,   water%qintr*dt,             start=(/itime+1/))
     iret = nf90_put_var(ncid, qints_id,   water%qints*dt,             start=(/itime+1/))
     iret = nf90_put_var(ncid, qdripr_id,  water%qdripr*dt,            start=(/itime+1/))
     iret = nf90_put_var(ncid, qdrips_id,  water%qdrips*dt,            start=(/itime+1/))
     iret = nf90_put_var(ncid, qthror_id,  water%qthror*dt,            start=(/itime+1/))
     iret = nf90_put_var(ncid, qthros_id,  water%qthros*dt,            start=(/itime+1/))
     iret = nf90_put_var(ncid, qrain_id,   water%qrain*dt,             start=(/itime+1/))
     iret = nf90_put_var(ncid, qsnow_id,   water%qsnow*dt,             start=(/itime+1/))
     iret = nf90_put_var(ncid, snowhin_id, water%snowhin*dt,           start=(/itime+1/))
     iret = nf90_put_var(ncid, fwet_id,    water%fwet,                 start=(/itime+1/))
     iret = nf90_put_var(ncid, cmc_id,     water%cmc,                  start=(/itime+1/))
     iret = nf90_put_var(ncid, canliq_id,  water%canliq,               start=(/itime+1/))
     iret = nf90_put_var(ncid, canice_id,  water%canice,               start=(/itime+1/))
     iret = nf90_put_var(ncid, ecan_id,    water%ecan*dt,              start=(/itime+1/))
     iret = nf90_put_var(ncid, etran_id,   water%etran*dt,             start=(/itime+1/))
! for snow water
     iret = nf90_put_var(ncid, snowh_id,   water%snowh,                start=(/itime+1/))
     iret = nf90_put_var(ncid, sneqv_id,   water%sneqv,                start=(/itime+1/))
     iret = nf90_put_var(ncid, ponding_id, water%ponding,              start=(/itime+1/))
     iret = nf90_put_var(ncid, ponding1_id,water%ponding1,             start=(/itime+1/))
     iret = nf90_put_var(ncid, ponding2_id,water%ponding2,             start=(/itime+1/))
     iret = nf90_put_var(ncid, qsnbot_id,  water%qsnbot*dt,            start=(/itime+1/))
     iret = nf90_put_var(ncid, qsnfro_id,  water%qsnfro*dt,            start=(/itime+1/))
     iret = nf90_put_var(ncid, qsnsub_id,  water%qsnsub*dt,            start=(/itime+1/))
     iret = nf90_put_var(ncid, snice_id,   water%snice,                start=(/itime+1,1/), count=(/1,nsnow/))
     iret = nf90_put_var(ncid, snliq_id,   water%snliq,                start=(/itime+1,1/), count=(/1,nsnow/))
     iret = nf90_put_var(ncid, stc_id,     energy%stc,           start=(/itime+1,1/), count=(/1,nsoil+nsnow/))
     iret = nf90_put_var(ncid, zsnso_id,   zsnso,                start=(/itime+1,1/), count=(/1,nsoil+nsnow/))

   end subroutine add_to_output

   subroutine finalize_output()

     iret = nf90_close(ncid)

   end subroutine finalize_output
   
end module NoahMPOutput

