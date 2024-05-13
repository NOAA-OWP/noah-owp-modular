module AsciiReadModule
  
  use UtilitiesModule
  use ErrorCheckModule
  
  implicit none
  
contains
  
  subroutine open_forcing_file(filename, error_flag)
    
    implicit none
    
    character*256, intent(in)  :: filename
    integer,       intent(out) :: error_flag
    character(len=256)         :: error_string
    
    !---------------------------------------------------------------------
    !  local variables
    !---------------------------------------------------------------------
    logical :: lexist ! logical for whether the file specified by filename exists
    integer :: ierr   ! error code returned by open(iostat = ierr)
    !---------------------------------------------------------------------

    !  Check if the specified file exists
    inquire(file = trim(filename), exist = lexist)
    if (.not. lexist) then
       error_flag = NOM_FAILURE
       write(error_string,'(A,A,A)') "AsciiReadModule.f90:open_forcing_file(): File: ",trim(filename), " does not exist. Check the forcing file specified as a command-line argument"
       call log_message(error_flag, error_string)
       return
    endif
    
    ! Open the forcing file 
    open(10, file = trim(filename), form = 'formatted', action = 'read', iostat = ierr)
    if (ierr /= 0) then
       error_flag = NOM_FAILURE
       write(error_string,'(A,A,A)') "ReadModule.f90:open_forcing_file(): Problem opening file: ",trim(filename)
       call log_message(error_flag, error_string)
       return
    endif
    
  end subroutine open_forcing_file
  
  subroutine read_forcing_text(iunit, nowdate, forcing_timestep, &
    u, v, sfctmp, spechumd, sfcprs, swrad, lwrad, pcprate, ierr, error_flag)
    
    implicit none

    ! Input
    integer,           intent(in)  :: iunit
    character(len=12), intent(in)  :: nowdate
    integer,           intent(in)  :: forcing_timestep

    ! Output
    real,              intent(out) :: sfctmp
    real,              intent(out) :: spechumd
    real,              intent(out) :: sfcprs
    real,              intent(out) :: swrad
    real,              intent(out) :: lwrad
    real,              intent(out) :: pcprate
    integer,           intent(out) :: ierr
    integer,           intent(out) :: error_flag
    real,              intent(out) :: u
    real,              intent(out) :: v

    ! Local
    real              :: wspd 
    integer           :: year
    integer           :: month
    integer           :: day
    integer           :: hour
    integer           :: minute
    character(len=12) :: readdate
    character(len=256):: error_string
    real              :: read_windspeed
    real              :: read_winddir
    real              :: read_temperature
    real              :: read_pressure
    real              :: read_humidity
    real              :: read_swrad
    real              :: read_lwrad
    real              :: read_rain
    real              :: wdir

    type fdata
       character(len=12) :: readdate
       real              :: windspeed
       real              :: winddir
       real              :: temperature
       real              :: humidity
       real              :: pressure
       real              :: swrad
       real              :: lwrad
       real              :: rain
    end type fdata

    type(fdata) :: before = fdata("000000000000", -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36 ) 
    type(fdata) :: after  = fdata("000000000000", -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36 ) 
      
    integer :: idts
    integer :: idts2
    real    :: fraction

    real    :: svp ! Saturation Vapor pressure, computed herein as a function of Temperature
    real    :: e   ! Water Vapor Pressure, computed herein as a function of Temperature, Pressure, and Relative Humidity
    real    :: rhf ! Relative humidity expressed as a fraction [ 0.0 to 1.0 ]
    real    :: qs  ! Saturation specific humidity [ kg kg{-1} ]

    ! Parameters used to compute Saturation Vapor Pressure as a function of Temperature
    real, parameter :: svp1  = 611.2
    real, parameter :: svp2  = 17.67
    real, parameter :: svp3  = 29.65
    real, parameter :: svpt0 = 273.15

    ! Parameter used to compute Specific Humidity from Pressure and Saturation Vapor Pressure.
    real, parameter :: eps   = 0.622

    character(len=1024) :: string

    ! Flag to tell us whether this is the first time this subroutine is called, in which case
    ! we need to seek forward to the data.
    logical :: FirstTime = .TRUE.

    ! The format string for reading the forcing data:
    character(len=64), parameter :: read_format = "(I4.4, 4(1x,I2.2),8(F17.10))"

    real, parameter :: pi = 3.14159265

    ! First time in, skip forward, positioning ourself at the beginning of the data.
    if ( FirstTime ) then
       FirstTime = .FALSE.
       do
          read(iunit, '(A1024)') string
          string = upcase(adjustl(string))
          if (string(1:9) == "<FORCING>") exit
       enddo
    endif

    ! Wind Speed in this file is m s{-1}
    ! Wind direction in this file is degrees from north.
    ! Temperature in this file is in Degrees C.
    ! Humidity in this file is Relative Humidity, in % (i.e., between 0 and 100+).
    ! Pressure in this file is in mb.
    ! Incoming Short-wave Radiation in this file is in W m{-2}
    ! Incoming Long-wave Radiation in this file is in W m{-2}
    ! Precipitation rate in this file is in Inches per forcing timestep

    READLOOP : do

       ! If our dates in storage are already bracketing NOWDATE, we don't have to
       ! read anything; we can just exit.
       if (before%readdate <= nowdate .and. nowdate <= after%readdate) exit READLOOP

       ! But if we do have to read data, let's read some data!
       read(UNIT=iunit, FMT=read_format, IOSTAT=ierr) &
            year, month, day, hour, minute, &
            read_windspeed,   &
            read_winddir,     &
            read_temperature, &
            read_humidity,    &
            read_pressure,    &
            read_swrad,       &
            read_lwrad,       &
            read_rain
       if (ierr < 0) then
          !KWM write(*,'("Hit the end of file.")')
          ierr = 1

          before = fdata("000000000000", -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36 ) 
          after  = fdata("000000000000", -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36 ) 
          FirstTime = .TRUE.

          return
       endif
       if (ierr /= 0) then 
         error_flag = NOM_FAILURE
         write(error_string,'(A)') "AsciiReadModule.f90:read_forcing_text(): Error reading from data file."
         call log_message(error_flag, error_string)
         return
       endif
       write(readdate,'(I4.4,4I2.2)') year, month, day, hour, minute

       if ( readdate > nowdate ) then
          ! After becomes before, and then we have a new before
          if (after%readdate > "000000000000" ) before = after
          after = fdata ( readdate, read_windspeed, read_winddir, read_temperature, read_humidity, read_pressure, read_swrad, read_lwrad, read_rain )
          exit READLOOP
       else if (readdate == nowdate) then
          before = fdata ( readdate, read_windspeed, read_winddir, read_temperature, read_humidity, read_pressure, read_swrad, read_lwrad, read_rain )
          exit READLOOP
       else if (readdate < nowdate) then
          before = fdata ( readdate, read_windspeed, read_winddir, read_temperature, read_humidity, read_pressure, read_swrad, read_lwrad, read_rain )
          cycle READLOOP
       else
         error_flag = NOM_FAILURE
         write(error_string,'(A)') "AsciiReadModule.f90:read_forcing_text(): Logic problem."
         call log_message(error_flag, error_string)
         return
       endif
    enddo READLOOP

    if (before%readdate == nowdate) then

       pcprate = before%rain                              ! No conversion necessary
       sfctmp  = before%temperature                       ! No conversion necessary
       sfcprs  = before%pressure*1.E2                     ! Convert pressure from mb to Pa
       wspd    = before%windspeed                         ! No conversion necessary
       wdir    = before%winddir                           ! No conversion necessary
       swrad   = before%swrad                             ! No conversion necessary
       lwrad   = before%lwrad                             ! No conversion necessary
       rhf     = before%humidity * 1.E-2                  ! Convert Relative Humidity from percent to fraction

    else if (after%readdate == nowdate) then

       pcprate = after%rain                              ! No conversion necessary
       sfctmp  = after%temperature                       ! No conversion necessary
       sfcprs  = after%pressure*1.E2                     ! Convert pressure from mb to Pa
       wspd    = after%windspeed                         ! No conversion necessary
       wdir    = after%winddir                           ! No conversion necessary
       swrad   = after%swrad                             ! No conversion necessary
       lwrad   = after%lwrad                             ! No conversion necessary
       rhf     = after%humidity * 1.E-2                  ! Convert Relative Humidity from percent to fraction
     
       before = after
       after  = fdata("000000000000", -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36, -1.E36 )        

    else if (before%readdate < nowdate .and. nowdate < after%readdate) then

       call geth_idts(nowdate, before%readdate, idts, error_flag)
       call geth_idts(after%readdate, before%readdate, idts2, error_flag)
       if (error_flag == NOM_FAILURE) then
         return
       endif

       if (idts2*60 /= forcing_timestep) then
          print*, 'forcing_timestep = ', forcing_timestep
          print*,' nowdate = ', nowdate
          print*, 'before%readdate = ', before%readdate
          print*, 'idts = ', idts
          print*,' after%readdate = ', after%readdate
          print*, 'idts2 = ', idts2
          error_flag = NOM_FAILURE
          write(error_string,'(A)') "AsciiReadModule.f90:read_forcing_text(): IDTS PROBLEM."
          call log_message(error_flag, error_string)
          return
       endif

       fraction = real(idts2-idts)/real(idts2)

       pcprate = before%rain  ! Precip rate is not interpolated, but carried forward.

       sfctmp = ( before%temperature * fraction )  + ( after%temperature * ( 1.0 - fraction ) )

       sfcprs = ( before%pressure * fraction ) + ( after%pressure * ( 1.0 - fraction ) )
       sfcprs = sfcprs * 1.E2

       wspd = ( before%windspeed * fraction ) + ( after%windspeed * ( 1.0 - fraction ) )

       wdir = ( before%winddir * fraction ) + ( after%winddir * ( 1.0 - fraction ) )

       swrad = ( before%swrad * fraction ) + ( after%swrad * ( 1.0 - fraction ) )

       lwrad = ( before%lwrad * fraction ) + ( after%lwrad * ( 1.0 - fraction ) )

       rhf = ( before%humidity * fraction ) + ( after%humidity * ( 1.0 - fraction ) )
       rhf = rhf * 1.E-2

    else
       error_flag = NOM_FAILURE
       write(error_string,'(A,A,A)') "AsciiReadModule.f90:read_forcing_text(): date: ", nowdate, ". Problem in the logic of read_forcing_text."
       call log_message(error_flag, error_string)
       return
    endif

! Below commented out KSJ 2021-06-09
! #define _NCEP_CODE_SPECHUMD_FROM_RH_
!
! #ifdef _NCEP_CODE_SPECHUMD_FROM_RH_
!
!     ! Convert RH [ % ] to Specific Humidity [ kg kg{-1} ]
!     ! This computation from NCEP's Noah v2.7.1 driver.
!
!     svp = EsFuncT(sfctmp)
!     QS = eps * svp / (sfcprs - (1.-eps) * svp)
!     E = (sfcprs*svp*rhf)/(sfcprs - svp*(1. - rhf))
!     spechumd = (eps*e)/(sfcprs-(1.0-eps)*E)
!     IF (spechumd .LT. 0.1E-5) spechumd = 0.1E-5
!     IF (spechumd .GE.  QS) spechumd = QS*0.99
!
! #else

    ! Convert RH [ % ] to Specific Humidity [ kg kg{-1} ]
    ! This computation from MM5/WRF heritage.

    svp      = svp1*exp(svp2*(sfctmp-svpt0)/(sfctmp-svp3))
    e        = rhf * svp
    spechumd = (eps*e)/(sfcprs-(1.0-eps)*e)

! #endif

    ! Compute u and v components from wind speed and wind direction
    u = - wspd * sin (wdir * pi/180.)
    v = - wspd * cos (wdir * pi/180.)
    
  end subroutine read_forcing_text
  
  character(len=256) function upcase(h) result(return_string)
    implicit none
    character(len=*), intent(in) :: h
    integer :: i
    
    return_string = ""

    do i = 1, len_trim(h)

       if ((ichar(h(i:i)).ge.96) .and. (ichar(h(i:i)).le.123)) then
          return_string(i:i) = char(ichar(h(i:i))-32)
       else
          return_string(i:i) = h(i:i)
       endif
    enddo

  end function upcase
  
end module AsciiReadModule
