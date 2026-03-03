module noahowp_log_module
   implicit none
   private
   public :: write_log, is_logger_enabled, get_log_level, itoa, rtoa
   public :: LOG_LEVEL_DEBUG, LOG_LEVEL_INFO, LOG_LEVEL_WARNING, LOG_LEVEL_SEVERE, LOG_LEVEL_FATAL

   ! Log levels (made public so other modules can use them)
   integer, parameter :: LOG_LEVEL_DEBUG = 1
   integer, parameter :: LOG_LEVEL_INFO = 2
   integer, parameter :: LOG_LEVEL_WARNING = 3
   integer, parameter :: LOG_LEVEL_SEVERE = 4
   integer, parameter :: LOG_LEVEL_FATAL = 5

   integer :: log_level = LOG_LEVEL_INFO

   character(len=1024) :: log_file_path
   integer :: log_unit = 12
   logical :: logging_enabled = .false.
   logical :: opened_once = .false.
   logical :: logger_initialized = .false.   ! Flag to track if the logger has been initialized

   ! To use Logger while running this module stand-alone, set
   ! the following environment variables, case-sensitive:
   !     NGEN_EWTS_LOGGING=ENABLED
   !     NGEN_LOG_FILE_PATH=<full pathname for log file>
   !     NOAHOWP_LOGLEVEL=<DEBUG, INFO, WARNING, SEVERE, FATAL> 
   !
   ! Constants character(len=1), parameter :: DS = "/"
   character(len=16), parameter :: MODULE_NAME = "Noah-OWP-Modular"
   character(len=14), parameter :: LOG_DIR_NGENCERF = "/ngencerf/data"
   character(len=8),  parameter :: LOG_DIR_DEFAULT = "run-logs"
   character(len=3),  parameter :: LOG_FILE_EXT = "log"
   character(len=1),  parameter :: DS = "/"
   character(len=17), parameter :: EV_EWTS_LOGGING = "NGEN_EWTS_LOGGING"
   character(len=18), parameter :: EV_NGEN_LOGFILEPATH = "NGEN_LOG_FILE_PATH"
   character(len=19), parameter :: EV_MODULE_LOGFILEPATH = "NOAHOWP_LOGFILEPATH"
   character(len=16), parameter :: EV_MODULE_LOGLEVEL = "NOAHOWP_LOGLEVEL"
   character(len=7),  parameter :: log_module_name = "NOAHOWP"
   integer, parameter           :: LOG_MODULE_NAME_LEN = 8  ! // Width of module name for log entries
   integer, parameter           :: LOG_ENTRY_LEVEL_LEN = 7  ! // Width of log level for log entries

contains

   subroutine initialize_logger()
      character(len=256) :: log_env, log_str
      integer :: save_log_level

      logger_initialized = .true.
      call get_env_var(EV_EWTS_LOGGING, log_env)
      if (trim(log_env) == "ENABLED") then
         logging_enabled = .true.
         print *,trim(MODULE_NAME)," Logging ", trim(log_env)
         call flush(6)
        else
         logging_enabled = .false.
         print *,trim(MODULE_NAME)," Logging NOT enabled. EV_EWTS_LOGGING=", trim(log_env)
         call flush(6)
         return
      end if
      ! Here because return was not executed above

      ! Set log level from environment variable (if exists)
      call get_env_var(EV_MODULE_LOGLEVEL, log_env)
      log_str = trim(log_env)
      if (log_str == "DEBUG" ) then
         log_level = LOG_LEVEL_DEBUG
      else if (log_str == "INFO" ) then
         log_level = LOG_LEVEL_INFO
      else if (log_str == "WARNING" ) then
         log_level = LOG_LEVEL_WARNING
      else if (log_str == "SEVERE" ) then
         log_level = LOG_LEVEL_SEVERE
      else if (log_str == "FATAL" ) then
         log_level = LOG_LEVEL_FATAL
      else
         log_str = "INFO (" // trim(EV_MODULE_LOGLEVEL) // " = '" // trim(log_str) // "' INVALID Log Level) Defaulted to INFO"
         log_level = LOG_LEVEL_INFO  ! Default level
      end if

      ! Get the log file path by calling set_log_file_path
      call set_log_file_path()

      print *, trim(MODULE_NAME)," Log level: ", log_str
      call flush(6)
   end subroutine initialize_logger

   function fit_string(str, target_len) result(fixed_str)
      implicit none
      character(len=*), intent(in) :: str
      integer, intent(in) :: target_len
      character(len=target_len) :: fixed_str
      integer :: copy_len

      ! Determine how many characters to copy (min of source or target length)
      copy_len = min(len_trim(str), target_len)

      ! Copy the appropriate number of characters, right pad with spaces automatically
      fixed_str = ' '  ! Initialize to spaces
      fixed_str(1:copy_len) = str(1:copy_len)
   end function fit_string

   subroutine write_log(message, msg_level)
      character(len=*), intent(in) :: message
      integer, intent (in) :: msg_level
      character(len=40) :: timestamp, log_level_str
      character(len=LOG_MODULE_NAME_LEN) :: fixed_tag
      character(len=LOG_ENTRY_LEVEL_LEN) :: fixed_lvl
      character(len=1100) :: log_msg

      if (.not. logger_initialized) then
         call initialize_logger()
      end if

      if (logging_enabled .and. (msg_level >= log_level)) then
         ! Convert log level to a string
         if (msg_level == LOG_LEVEL_DEBUG ) then
            log_level_str = "DEBUG"
         elseif (msg_level == LOG_LEVEL_INFO ) then
            log_level_str = "INFO"
         elseif (msg_level == LOG_LEVEL_WARNING ) then
            log_level_str = "WARNING"
         elseif (msg_level == LOG_LEVEL_SEVERE ) then
            log_level_str = "SEVERE"
         elseif (msg_level == LOG_LEVEL_FATAL ) then
            log_level_str = "FATAL"
         else
            ! If the level is unknown, ignore logging
            return
         end if

         ! Log the message
         call create_timestamp(.false., .true., .true., timestamp)
         fixed_tag = fit_string(log_module_name, LOG_MODULE_NAME_LEN)
         fixed_lvl = fit_string(log_level_str,LOG_ENTRY_LEVEL_LEN)
         log_msg = trim(timestamp) // " " // fixed_tag // " " // fixed_lvl // " " // trim(message)
         if (log_file_ready(.true.)) then
            write(log_unit, '(A)') trim(log_msg)
         else
            print *, log_msg
            call flush(6)
         end if
         close(log_unit) ! Since this is a shared file with other modules, need to close this acces after each write
      end if
   end subroutine write_log

   subroutine create_timestamp(date_only, iso, append_ms, timestamp)
      logical, intent(in) :: date_only
      logical, intent(in) :: iso
      logical, intent(in) :: append_ms
      character(len=*), intent(out) :: timestamp

      integer :: values(8)
      character(len=32) :: ts_base, ms_str

      call date_and_time(values=values)

      if (date_only) then
         write(ts_base, '(I4.4,I2.2,I2.2)') &
            values(1), values(2), values(3)
      else if (iso) then
         write(ts_base, '(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2)') &
            values(1), values(2), values(3), values(5), values(6), values(7)
      else
         write(ts_base, '(I4.4,I2.2,I2.2,"T",I2.2,I2.2,I2.2)') &
            values(1), values(2), values(3), values(5), values(6), values(7)
      end if

      if (append_ms) then
         write(ms_str, '(".",I3.3)') values(8)
         timestamp = trim(ts_base) // trim(ms_str)
      else
         timestamp = trim(ts_base)
      end if
   end subroutine create_timestamp

   ! Check if a directory exists
   logical function directory_exists(path)
      character(len=*), intent(in) :: path
      integer :: status
      character(len=522) :: cmd

      cmd = "test -d " // path
      call execute_command_line(cmd, exitstat=status)
      directory_exists = (status == 0)
   end function directory_exists

   ! Create directory if it doesn't exist
   logical function create_directory(path)
      character(len=*), intent(in) :: path
      character(len=522) :: cmd
      integer :: status

      if (.not. directory_exists(path)) then
         cmd = "mkdir -p " // path
         call execute_command_line(cmd, exitstat=status)
         create_directory = (status == 0)
      else
         create_directory = .true.
      end if
   end function create_directory

   ! Set the log file path based on environment variables or defaults
   subroutine set_log_file_path()
      character(len=256) :: env_var
      logical :: append_entries
      logical :: module_log_env_exists
      character(len=512) :: log_file_dir
      character(len=40) :: timestamp
      integer :: save_log_level

      append_entries = .true.
      module_log_env_exists = .false.
      ! Check if module log path environment exists
      call get_env_var(EV_MODULE_LOGFILEPATH, env_var)
      if (trim(adjustl(env_var)) /= "") then
         log_file_path = trim(env_var)
         module_log_env_exists = .true.
      else
         ! log file path not set yet
         call get_env_var(EV_NGEN_LOGFILEPATH, env_var)
         if (trim(adjustl(env_var)) /= "") then
            log_file_path = trim(env_var)
         else
            ! ngen log path does not exist. Create alternate log
            append_entries = .false.
            ! Determine parent dir
            if (directory_exists(LOG_DIR_NGENCERF)) then
               log_file_dir = trim(LOG_DIR_NGENCERF) // DS // trim(LOG_DIR_DEFAULT)
            else
               call get_env_var("HOME", env_var)
               if (trim(adjustl(env_var)) /= "") then
                  log_file_dir = trim(env_var) // DS // trim(LOG_DIR_DEFAULT)
               else
                  log_file_dir = "~" // DS // trim(LOG_DIR_DEFAULT)
               end if
            end if
            ! Ensure parent log dir exists
            if (create_directory(log_file_dir)) then
               ! Get dir for this log
               call get_env_var("USER", env_var)
               if (trim(adjustl(env_var)) /= "") then
                  log_file_dir = trim(log_file_dir) // DS // trim(env_var)
               else
                  ! Get a date only timestamp
                  call create_timestamp(.true., .false., .false., timestamp)
                  log_file_dir = trim(log_file_dir) // DS // trim(timestamp)
               end if
               if (create_directory(log_file_dir)) then
                  ! Set log file name with <date>T<time> as suffix
                  call create_timestamp(.false., .false., .false., timestamp)
                  log_file_path = trim(log_file_dir) // DS // trim(MODULE_NAME) // "_" // trim(timestamp) // "." // trim(LOG_FILE_EXT)
               end if
            end if
         end if
      end if

      if (log_file_ready(append_entries)) then
         if (.not. module_log_env_exists) then
            call write_env_var(EV_MODULE_LOGFILEPATH, log_file_path)
            print *, trim(MODULE_NAME), " Log File: ", trim(log_file_path)
            call flush(6)
         end if
      else
         print *, "Unable to open log file. Log entries will be writen to stdout"
         call flush(6)
      end if
   end subroutine set_log_file_path

   ! Log file readiness check
   logical function log_file_ready(append_mode)
      logical, intent(in) :: append_mode
      integer :: ios

      if (opened_once) then
         ! Make sure file pointer to the end of the file
         open(unit=log_unit, file=log_file_path, status='old', position='append', action='write')
      else if (trim(log_file_path) /= "") then ! make sure path name not empty
         if (append_mode) then
            open(unit=log_unit, file=log_file_path, status='old', action='write', position='append', iostat=ios)
         else
            open(unit=log_unit, file=log_file_path, status='new', action='write', iostat=ios)
         end if
         opened_once = .true.
      end if
      log_file_ready = .true.
   end function log_file_ready

   subroutine get_env_var(var_name, value)
      character(len=*), intent(in) :: var_name
      character(len=256), intent(out) :: value
      character(len=256) :: result

      call getenv(trim(adjustl(var_name)), result)
      value = result
   end subroutine get_env_var

   subroutine write_env_var(var_name, value)
      character(len=*), intent(in) :: var_name
      character(len=*), intent(in) :: value

      character(len=512) cmd;
      cmd = "export" // trim(var_name) // "=" // trim(value)
      call system(cmd)
   end subroutine write_env_var

   function is_logger_enabled() result(status)
      logical :: status
      status = logging_enabled  ! Return the current logging status
   end function is_logger_enabled

   function get_log_level() result(level)
      integer :: level
      level = log_level  ! Return the current log level
   end function get_log_level

   function itoa(i) result(res)
      ! This function convrets an integer to ascii format

      implicit none
      !class (noahowpLogger), intent(in) :: this
      character(:),allocatable :: res
      integer,intent(in) :: i
      character(range(i)+2) :: tmp
      write(tmp,'(i0)') i
      res = trim(tmp)
   end function itoa

    function rtoa(i) result(res)
        ! Converts a real to ASCII (string)
        implicit none
        real, intent(in) :: i
        character(32) :: buffer
        character(len=32) :: res

        write(buffer, '(F10.10)') i
        res = adjustl(trim(buffer))
    end function rtoa

end module noahowp_log_module
