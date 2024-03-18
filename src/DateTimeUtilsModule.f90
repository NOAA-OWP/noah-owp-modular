! module containing utilities for parsing dates, times (and incidentally strings)
! copied from the GMET code at https://github.com/NCAR/GMET/

module DateTimeUtilsModule

  implicit none
  public
 
  integer, parameter :: kr4 = selected_real_kind (6, 37)! single precision real
  integer, parameter :: kr8 = selected_real_kind (15, 307)! double precision real
  ! Integer kinds
  integer, parameter :: ki4 = selected_int_kind (9)! single precision integer
  integer, parameter :: ki8 = selected_int_kind (18)! double precision integer
  !Complex kinds
  !integer, parameter :: kc4 = kr4 ! single precision complex
  !integer, parameter :: kc8 = kr8 ! double precision complex
 
  private :: value_dr, value_sr, value_di, value_si
  private :: write_dr, write_sr, write_di, write_si
  private :: writeq_dr, writeq_sr, writeq_di, writeq_si
 
  ! overload some core functions
  interface value ! Generic operator for converting a number string to a
                 ! number. Calling syntax is 'call value(numstring,number,ios)'
                 ! where 'numstring' is a number string and 'number' is a
                 ! real number or an integer (single or double precision).
    module procedure value_dr
    module procedure value_sr
    module procedure value_di
    module procedure value_si
  end interface
 
  interface writenum ! Generic  interface for writing a number to a string. The
                    ! number is left justified in the string. The calling syntax
                    ! is 'call writenum(number,string,format)' where 'number' is
                    ! a real number or an integer, 'string' is a character string
                    ! containing the result, and 'format' is the format desired,
                    ! e.g., 'e15.6' or 'i5'.
    module procedure write_dr
    module procedure write_sr
    module procedure write_di
    module procedure write_si
  end interface
 
  interface writeq ! Generic interface equating a name to a numerical value. The
                  ! calling syntax is 'call writeq(unit,name,value,format)' where
                  ! unit is the integer output unit number, 'name' is the variable
                  ! name, 'value' is the real or integer value of the variable,
                  ! and 'format' is the format of the value. The result written to
                  ! the output unit has the form <name> = <value>.
    module procedure writeq_dr
    module procedure writeq_sr
    module procedure writeq_di
    module procedure writeq_si
  end interface
 
 
!**********************************************************************
 
contains
 
!**********************************************************************
 
  subroutine parse (str, delims, args, nargs)
 
! Parses the string 'str' into arguments args(1), ..., args(nargs) based on
! the delimiters contained in the string 'delims'. Preceding a delimiter in
! 'str' by a backslash (\) makes this particular instance not a delimiter.
! The integer output variable nargs contains the number of arguments found.
 
    character (len=*) :: str, delims
    character (len=len_trim(str)) :: strsav
    character (len=*), dimension (:) :: args
    integer :: i, k, na, nargs, lenstr
 
    strsav = str
    call compact (str)
    na = size (args)
    do i = 1, na
      args (i) = ' '
    end do
    nargs = 0
    lenstr = len_trim (str)
    if (lenstr == 0) return
    k = 0
 
    do
      if (len_trim(str) == 0) exit
      nargs = nargs + 1
      if(nargs .gt. size(args)) then
        print *,'Number of predictors larger than expected, check nPredict'
        stop
      end if
      call split (str, delims, args(nargs))
      call removebksl (args(nargs))
    end do
    str = strsav
 
  end subroutine parse
 
!**********************************************************************
 
  subroutine compact (str)
 
! Converts multiple spaces and tabs to single spaces; deletes control characters;
! removes initial spaces.
 
    character (len=*) :: str
    character (len=1) :: ch
    character (len=len_trim(str)) :: outstr
    integer :: i, k, lenstr, isp, ich
 
    str = adjustl (str)
    lenstr = len_trim (str)
    outstr = ' '
    isp = 0
    k = 0
 
    do i = 1, lenstr
      ch = str (i:i)
      ich = iachar (ch)
 
      select case (ich)
 
      case (9, 32)! space or tab character
        if (isp == 0) then
          k = k + 1
          outstr (k:k) = ' '
        end if
        isp = 1
 
      case (33:)! not a space, quote, or control character
        k = k + 1
        outstr (k:k) = ch
        isp = 0
 
      end select
 
    end do
 
    str = adjustl (outstr)
 
  end subroutine compact
 
!**********************************************************************
 
  subroutine removesp (str)
 
! Removes spaces, tabs, and control characters in string str
 
    character (len=*) :: str
    character (len=1) :: ch
    character (len=len_trim(str)) :: outstr
    integer i, k, lenstr, ich
 
    str = adjustl (str)
    lenstr = len_trim (str)
    outstr = ' '
    k = 0
 
    do i = 1, lenstr
      ch = str (i:i)
      ich = iachar (ch)
      select case (ich)
      case (0:32)! space, tab, or control character
        cycle
      case (33:)
        k = k + 1
        outstr (k:k) = ch
      end select
    end do
 
    str = adjustl (outstr)
 
  end subroutine removesp
 
!**********************************************************************
 
  subroutine value_dr (str, rnum, ios)
 
! Converts number string to a double precision real number
 
    character (len=*) :: str
    real (kr8) :: rnum
    integer :: ios, ilen, ipos
 
    ilen = len_trim (str)
    ipos = scan (str, 'Ee')
    if ( .not. is_digit(str(ilen:ilen)) .and. ipos /= 0) then
      ios = 3
      return
    end if
    read (str,*, iostat=ios) rnum
 
  end subroutine value_dr
 
!**********************************************************************
 
  subroutine value_sr (str, rnum, ios)
 
! Converts number string to a single precision real number
 
    character (len=*) :: str
    real (kr4) :: rnum
    real (kr8) :: rnumd
    integer :: ios
 
    call value_dr (str, rnumd, ios)
    if (abs(rnumd) > huge(rnum)) then
      ios = 15
      return
    end if
    if (abs(rnumd) < tiny(rnum)) rnum = 0.0_kr4
    rnum = rnumd
 
  end subroutine value_sr
 
!**********************************************************************
 
  subroutine value_di (str, inum, ios)
 
! Converts number string to a double precision integer value
 
    character (len=*) :: str
    integer (ki8) :: inum
    real (kr8) :: rnum
    integer :: ios

    call value_dr (str, rnum, ios)
    if (abs(rnum) > huge(inum)) then
      ios = 15
      return
    end if
    inum = nint (rnum, ki8)
 
  end subroutine value_di
 
!**********************************************************************
 
  subroutine value_si (str, inum, ios)
 
! Converts number string to a single precision integer value
 
    character (len=*) :: str
    integer (ki4) :: inum
    real (kr8) :: rnum
    integer :: ios
 
    call value_dr (str, rnum, ios)
    if (abs(rnum) > huge(inum)) then
      ios = 15
      return
    end if
    inum = nint (rnum, ki4)
 
  end subroutine value_si
 
!**********************************************************************
 
  subroutine shiftstr (str, n)
 
! Shifts characters in in the string 'str' n positions (positive values
! denote a right shift and negative values denote a left shift). Characters
! that are shifted off the end are lost. Positions opened up by the shift
! are replaced by spaces.
 
    character (len=*) :: str
    integer :: lenstr, n, nabs
 
    lenstr = len (str)
    nabs = iabs (n)
    if (nabs >= lenstr) then
      str = repeat (' ', lenstr)
      return
    end if
    if (n < 0) str = str (nabs+1:) // repeat (' ', nabs)! shift left
    if (n > 0) str = repeat (' ', nabs) // str (:lenstr-nabs)! shift right
    return
 
  end subroutine shiftstr
 
!**********************************************************************
 
  subroutine insertstr (str, strins, loc)
 
! Inserts the string 'strins' into the string 'str' at position 'loc'.
! Characters in 'str' starting at position 'loc' are shifted right to
! make room for the inserted string. Trailing spaces of 'strins' are
! removed prior to insertion
 
    character (len=*) :: str, strins
    character (len=len(str)) :: tempstr
    integer :: lenstrins, loc
 
    lenstrins = len_trim (strins)
    tempstr = str (loc:)
    call shiftstr (tempstr, lenstrins)
    tempstr (1:lenstrins) = strins (1:lenstrins)
    str (loc:) = tempstr
    return
 
  end subroutine insertstr
 
!**********************************************************************
 
  subroutine delsubstr (str, substr)
 
! Deletes first occurrence of substring 'substr' from string 'str' and
! shifts characters left to fill hole. Trailing spaces or blanks are
! not considered part of 'substr'.
 
    character (len=*) :: str, substr
    integer :: ipos, lensubstr
 
    lensubstr = len_trim (substr)
    ipos = index (str, substr)
    if (ipos == 0) return
    if (ipos == 1) then
      str = str (lensubstr+1:)
    else
      str = str (:ipos-1) // str (ipos+lensubstr:)
    end if
    return
 
  end subroutine delsubstr
 
!**********************************************************************
 
  subroutine delall (str, substr)
 
! Deletes all occurrences of substring 'substr' from string 'str' and
! shifts characters left to fill holes.
 
    character (len=*) :: str, substr
    integer :: ipos, lensubstr
 
    lensubstr = len_trim (substr)
    do
      ipos = index (str, substr)
      if (ipos == 0) exit
      if (ipos == 1) then
        str = str (lensubstr+1:)
      else
        str = str (:ipos-1) // str (ipos+lensubstr:)
      end if
    end do
    return
 
  end subroutine delall
 
!**********************************************************************
 
  function uppercase (str) result (ucstr)
 
! convert string to upper case
 
    character (len=*) :: str
    character (len=len_trim(str)) :: ucstr
    integer :: ilen, ioffset, iquote, i, iqc, iav
 
    ilen = len_trim (str)
    ioffset = iachar ('A') - iachar ('a')
    iquote = 0
    ucstr = str
    do i = 1, ilen
      iav = iachar (str(i:i))
      if (iquote == 0 .and. (iav == 34 .or. iav == 39)) then
        iquote = 1
        iqc = iav
        cycle
      end if
      if (iquote == 1 .and. iav == iqc) then
        iquote = 0
        cycle
      end if
      if (iquote == 1) cycle
      if (iav >= iachar('a') .and. iav <= iachar('z')) then
        ucstr (i:i) = achar (iav+ioffset)
      else
        ucstr (i:i) = str (i:i)
      end if
    end do
    return
 
  end function uppercase
 
!**********************************************************************
 
  function lowercase (str) result (lcstr)
 
! convert string to lower case
 
    character (len=*) :: str
    character (len=len_trim(str)) :: lcstr
    integer :: ilen, ioffset, iquote, i, iqc, iav
 
    ilen = len_trim (str)
    ioffset = iachar ('A') - iachar ('a')
    iquote = 0
    lcstr = str
    do i = 1, ilen
      iav = iachar (str(i:i))
      if (iquote == 0 .and. (iav == 34 .or. iav == 39)) then
        iquote = 1
        iqc = iav
        cycle
      end if
      if (iquote == 1 .and. iav == iqc) then
        iquote = 0
        cycle
      end if
      if (iquote == 1) cycle
      if (iav >= iachar('A') .and. iav <= iachar('Z')) then
        lcstr (i:i) = achar (iav-ioffset)
      else
        lcstr (i:i) = str (i:i)
      end if
    end do
    return
 
  end function lowercase
 
!**********************************************************************
 
  subroutine readline (nunitr, line, ios)
 
! Reads line from unit=nunitr, ignoring blank lines
! and deleting comments beginning with an exclamation point(!)
 
    character (len=*) :: line
    integer :: ios, ipos, nunitr
 
    do
      read (nunitr, '(a)', iostat=ios) line ! read input line
      if (ios /= 0) return
      line = adjustl (line)
      ipos = index (line, '!')
      if (ipos == 1) cycle
      if (ipos /= 0) line = line (:ipos-1)
      if (len_trim(line) /= 0) exit
    end do
    return
 
  end subroutine readline
 
!**********************************************************************
 
  subroutine match (str, ipos, imatch)
 
! Sets imatch to the position in string of the delimiter matching the delimiter
! in position ipos. Allowable delimiters are (), [], {}, <>.
 
    character (len=*) :: str
    character :: delim1, delim2, ch
    integer :: lenstr, istart, iend, inc, ipos, isum, imatch, idelim2, i
 
    lenstr = len_trim (str)
    delim1 = str (ipos:ipos)
    select case (delim1)
    case ('(')
      idelim2 = iachar (delim1) + 1
      istart = ipos + 1
      iend = lenstr
      inc = 1
    case (')')
      idelim2 = iachar (delim1) - 1
      istart = ipos - 1
      iend = 1
      inc = - 1
    case ('[', '{', '<')
      idelim2 = iachar (delim1) + 2
      istart = ipos + 1
      iend = lenstr
      inc = 1
    case (']', '}', '>')
      idelim2 = iachar (delim1) - 2
      istart = ipos - 1
      iend = 1
      inc = - 1
    case default
      write (*,*) delim1, ' is not a valid delimiter'
      return
    end select
    if (istart < 1 .or. istart > lenstr) then
      write (*,*) delim1, ' has no matching delimiter'
      return
    end if
    delim2 = achar (idelim2)! matching delimiter
 
    isum = 1
    do i = istart, iend, inc
      ch = str (i:i)
      if (ch /= delim1 .and. ch /= delim2) cycle
      if (ch == delim1) isum = isum + 1
      if (ch == delim2) isum = isum - 1
      if (isum == 0) exit
    end do
    if (isum /= 0) then
      write (*,*) delim1, ' has no matching delimiter'
      return
    end if
    imatch = i
 
    return
 
  end subroutine match
 
!**********************************************************************
 
  subroutine write_dr (rnum, str, fmt)
 
! Writes double precision real number rnum to string str using format fmt
 
    real (kr8) :: rnum
    character (len=*) :: str, fmt
    character (len=80) :: formt
 
    formt = '(' // trim (fmt) // ')'
    write (str, formt) rnum
    str = adjustl (str)
 
  end subroutine write_dr
 
!***********************************************************************
 
  subroutine write_sr (rnum, str, fmt)
 
! Writes single precision real number rnum to string str using format fmt
 
    real (kr4) :: rnum
    character (len=*) :: str, fmt
    character (len=80) :: formt
 
    formt = '(' // trim (fmt) // ')'
    write (str, formt) rnum
    str = adjustl (str)
 
  end subroutine write_sr
 
!***********************************************************************
 
  subroutine write_di (inum, str, fmt)
 
! Writes double precision integer inum to string str using format fmt
 
    integer (ki8) :: inum
    character (len=*) :: str, fmt
    character (len=80) :: formt
 
    formt = '(' // trim (fmt) // ')'
    write (str, formt) inum
    str = adjustl (str)
 
  end subroutine write_di
 
!***********************************************************************
 
  subroutine write_si (inum, str, fmt)
 
! Writes single precision integer inum to string str using format fmt
 
    integer (ki4) :: inum
    character (len=*) :: str, fmt
    character (len=80) :: formt
 
    formt = '(' // trim (fmt) // ')'
    write (str, formt) inum
    str = adjustl (str)
 
  end subroutine write_si
 
!***********************************************************************
 
  subroutine trimzero (str)
 
! Deletes nonsignificant trailing zeroes from number string str. If number
! string ends in a decimal point, one trailing zero is added.
 
    character (len=*) :: str
    character :: ch
    character (len=10) :: exp
    integer :: ipos, i, lstr
 
    ipos = scan (str, 'eE')
    if (ipos > 0) then
      exp = str (ipos:)
      str = str (1:ipos-1)
    end if
    lstr = len_trim (str)
    do i = lstr, 1, - 1
      ch = str (i:i)
      if (ch == '0') cycle
      if (ch == '.') then
        str = str (1:i) // '0'
        if (ipos > 0) str = trim (str) // trim (exp)
        exit
      end if
      str = str (1:i)
      exit
    end do
    if (ipos > 0) str = trim (str) // trim (exp)
 
  end subroutine trimzero
 
!**********************************************************************
 
  subroutine writeq_dr (unit, namestr, value, fmt)
 
! Writes a string of the form <name> = value to unit
 
    real (kr8) :: value
    integer :: unit
    character (len=*) :: namestr, fmt
    character (len=32) :: tempstr
 
    call writenum (value, tempstr, fmt)
    call trimzero (tempstr)
    write (unit,*) trim (namestr) // ' = ' // trim (tempstr)
 
  end subroutine writeq_dr
 
!**********************************************************************
 
  subroutine writeq_sr (unit, namestr, value, fmt)
 
! Writes a string of the form <name> = value to unit
 
    real (kr4) :: value
    integer :: unit
    character (len=*) :: namestr, fmt
    character (len=32) :: tempstr
 
    call writenum (value, tempstr, fmt)
    call trimzero (tempstr)
    write (unit,*) trim (namestr) // ' = ' // trim (tempstr)
 
  end subroutine writeq_sr
 
!**********************************************************************
 
  subroutine writeq_di (unit, namestr, ivalue, fmt)
 
! Writes a string of the form <name> = ivalue to unit
 
    integer (ki8) :: ivalue
    integer :: unit
    character (len=*) :: namestr, fmt
    character (len=32) :: tempstr

    call writenum (ivalue, tempstr, fmt)
    call trimzero (tempstr)
    write (unit,*) trim (namestr) // ' = ' // trim (tempstr)
 
  end subroutine writeq_di
 
!**********************************************************************
 
  subroutine writeq_si (unit, namestr, ivalue, fmt)
 
! Writes a string of the form <name> = ivalue to unit
 
    integer (ki4) :: ivalue
    integer :: unit
    character (len=*) :: namestr, fmt
    character (len=32) :: tempstr

    call writenum (ivalue, tempstr, fmt)
    call trimzero (tempstr)
    write (unit,*) trim (namestr) // ' = ' // trim (tempstr)
 
  end subroutine writeq_si
 
!**********************************************************************
 
  function is_letter (ch) result (res)
 
! Returns .true. if ch is a letter and .false. otherwise
 
    character :: ch
    logical :: res
 
    select case (ch)
    case ('A':'Z', 'a':'z')
      res = .true.
    case default
      res = .false.
    end select
    return
 
  end function is_letter
 
!**********************************************************************
 
  function is_digit (ch) result (res)
 
! Returns .true. if ch is a digit (0,1,...,9) and .false. otherwise
 
    character :: ch
    logical :: res
 
    select case (ch)
    case ('0':'9')
      res = .true.
    case default
      res = .false.
    end select
    return
 
  end function is_digit
 
!**********************************************************************
 
  subroutine split (str, delims, before, sep)
 
! Routine finds the first instance of a character from 'delims' in the
! the string 'str'. The characters before the found delimiter are
! output in 'before'. The characters after the found delimiter are
! output in 'str'. The optional output character 'sep' contains the
! found delimiter. A delimiter in 'str' is treated like an ordinary
! character if it is preceded by a backslash (\). If the backslash
! character is desired in 'str', then precede it with another backslash.
! Compiler note:  some compilers need -Mbackslash flag for this routine
 
    character (len=*) :: str, delims, before
    character, optional :: sep
    logical :: pres
    character :: ch, cha
    integer :: ibsl, i, k, lenstr, iposa, ipos
 
    pres = present (sep)
    str = adjustl (str)
    call compact (str)
    lenstr = len_trim (str)
    if (lenstr == 0) return! string str is empty
    k = 0
    ibsl = 0 ! backslash initially inactive
    before = ' '
    do i = 1, lenstr
      ch = str (i:i)
      if (ibsl == 1) then ! backslash active
        k = k + 1
        before (k:k) = ch
        ibsl = 0
        cycle
      end if
      if (ch == '\') then    ! backslash with backslash inactive
        k = k + 1
        before (k:k) = ch
        ibsl = 1
        cycle
      end if
      ipos = index (delims, ch)
      if (ipos == 0) then ! character is not a delimiter
        k = k + 1
        before (k:k) = ch
        cycle
      end if
      if (ch /= ' ') then ! character is a delimiter that is not a space
        str = str (i+1:)
        if (pres) sep = ch
        exit
      end if
      cha = str (i+1:i+1)! character is a space delimiter
      iposa = index (delims, cha)
      if (iposa > 0) then ! next character is a delimiter
        str = str (i+2:)
        if (pres) sep = cha
        exit
      else
        str = str (i+1:)
        if (pres) sep = ch
        exit
      end if
    end do
    if (i >= lenstr) str = ''
    str = adjustl (str)! remove initial spaces
    return
 
  end subroutine split
 
!**********************************************************************
 
  subroutine removebksl (str)
 
! Removes backslash (\) characters. Double backslashes (\\) are replaced
! by a single backslash.
 
    character (len=*) :: str
    character (len=1) :: ch
    character (len=len_trim(str)) :: outstr
    integer :: ibsl, i, k, lenstr
 
    str = adjustl (str)
    lenstr = len_trim (str)
    outstr = ' '
    k = 0
    ibsl = 0 ! backslash initially inactive
 
    do i = 1, lenstr
      ch = str (i:i)
      if (ibsl == 1) then ! backslash active
        k = k + 1
        outstr (k:k) = ch
        ibsl = 0
        cycle
      end if
      if (ch == '\') then ! backslash with backslash inactive
        ibsl = 1
        cycle
      end if
      k = k + 1
      outstr (k:k) = ch ! non-backslash with backslash inactive
    end do
 
    str = adjustl (outstr)
 
  end subroutine removebksl
 
  ! ===== Date/Time Utilities start here ==========

  subroutine parse_date (date, year, month, day, hour, min, sec, error)
    character (len=*), intent (in) :: date
    integer, intent (out) :: sec, min, hour, day, month, year
    integer, intent (out) :: error
 
    ! allow for different date-string options YYYYMMDD[HH][MMSS]
    if (len_trim(date) /= 14 .and. len_trim(date) /= 12 .and. len_trim(date) /= 8 .and. len_trim(date) /= 10) then
      error = 1
      return
    end if
    call value (date(7:8), day, error)
    call value (date(5:6), month, error)
    call value (date(1:4), year, error)
    if (len_trim(date) == 8) then
      hour = 0
      min = 0
      sec = 0
    else if(len_trim(date) == 10) then
      call value (date(9:10), hour, error)
      min = 0
      sec = 0
    else if(len_trim(date) == 12) then
      call value (date(9:10), hour, error)
      call value (date(11:12), min, error)
      sec = 0
    else
      call value (date(9:10), hour, error)
      call value (date(11:12), min, error)
      call value (date(13:14), sec, error)
    end if
  end subroutine parse_date
 
 
  subroutine calendar_date (jdate, day, month, year)
    !   algorithm from Wikipedia: http://en.wikipedia.org/wiki/Julian_day
    !   originally from Richards, E. G. (2013). Calendars. In S. E. Urban & P. K. Seidelmann, eds.
    !                   Explanatory Supplement to the Astronomical Almanac, 3rd ed. (pp. 585–624).
    !                   Mill Valley, Calif.: University Science Books. ISBN 978-1-89138-985-6
    !                   p617-9

    integer, intent (in) :: jdate
    integer, intent (out) :: day, month, year
    integer :: y = 4716, j = 1401, m = 2, n = 12, r = 4, p = 1461
    integer :: v = 3, u = 5, s = 153, w = 2, b = 274277, c = - 38
    integer :: f, e, g, h
    f = jdate + j + (((4*jdate+b)/146097)*3) / 4 + c
    e = r * f + v
    g = mod (e, p) / r
    h = u * g + w
    day = mod (h, s) / u + 1
    month = mod (h/s+m, n) + 1
    year = e / p - y + (n+m-month) / n
  
  end subroutine calendar_date
 
 
  double precision function date_to_unix (date)
 
    character (len=*), intent (in) :: date
    double precision :: u_day, i_day, days
    integer :: sec, min, hour, day, month, year, error
 
    call parse_date (date, year, month, day, hour, min, sec, error)
    
    if (error /= 0) then
      date_to_unix = -9999.99
      print*, 'error in date_to_unix -- date, year, month, day, hour, min, sec, error:'
      print*, date, year, month, day, hour, min, sec, error
      stop !return
    end if
 
    u_day = julian_date (1, 1, 1970)
    i_day = julian_date (day, month, year)
    days = i_day - u_day
 
    date_to_unix = (days*86400) + (hour*3600) + (min*60) + sec
 
  end function date_to_unix
 
 
  subroutine unix_to_date (itime, year, month, day, hour, min, sec)
 
    double precision, intent (in) :: itime
    integer, intent (out) :: sec, min, hour, day, month, year
    integer :: u_day, i_day
 
    u_day = julian_date (1, 1, 1970)
    i_day = int (itime/86400)
    if (i_day < 0) then
      i_day = i_day - 1
    end if
    i_day = i_day + 1
 
    call calendar_date (u_day+i_day, day, month, year)

    i_day = mod (itime, 86400.0)
    if (i_day < 0) then
      i_day = i_day + 86400
    end if
 
    hour = i_day / 3600
    min = (i_day/60) - (hour*60)
    sec = mod (i_day, 60)
 
  end subroutine unix_to_date
 
 
  integer function julian_date (day, month, year)
    ! returns Julian Day (JD), where zero is noon on 1 Jan -4712 (eg 4713 BC)
 
    integer, intent (in) :: day, month, year
    double precision :: d, m, y
    integer :: a, b
    double precision :: yr_corr = 0.0
 
    d = day
    m = month
    y = year
    a = 0
    b = 0
    ! there is no year 0
    if (year < 0) then
      y = y + 1
      yr_corr = 0.75
    end if
 
    if (month <= 2) then
      y = y - 1.0
      m = month + 12.0
    end if
 
    if (y*10000.0+m*100.0+d >= 15821015.0) then
      a = year / 100
      b = 2 - a + (a/4)
    end if
 
    julian_date = int (365.25*y-yr_corr) + int (30.6001*(m+1)) + d + 1720994.0 + b
 
  end function julian_date

  
  subroutine get_utime_list (start_datetime, end_datetime, dt, times)
    ! makes a list of data times in secs since 1970-1-1 corresponding to requested period
    ! reports end-of-timestep points
    implicit none
 
    real*8, intent (in)               :: start_datetime, end_datetime
    real, intent (in)                 :: dt
    real*8, allocatable, intent (out) :: times(:)
    !local
    integer                           :: t, ntimes
    real*8                            :: utime

    if(abs(mod(end_datetime - start_datetime, dt)) > 1e-5) then
      print*, 'start and end datetimes are not an even multiple of dt -- check dates in namelist' 
      print*, 'end_datetime, start_datetime, dt, mod:', end_datetime, start_datetime, dt, mod(end_datetime-start_datetime, dt) 
      stop 
    end if

    ntimes = int((end_datetime - start_datetime)/dt) + 1
    allocate (times(ntimes))

    utime = start_datetime  ! secs since 1970-1-1
    do t  = 1, ntimes, 1
      if (utime > (end_datetime + 1e-5)) exit     ! add tolerance
      times (t) = utime
      utime     = utime + dt
    end do
    !print *, 'ntimes= ',ntimes
    !print *, 'time list: ', times  !seconds since 1970-1-1
 
  end subroutine get_utime_list  
  
  subroutine days_in_month(month,year,days)

    integer,intent(in)    :: month
    integer,intent(in)    :: year
    integer,intent(out)   :: days
    logical               :: lleap

    days=0
    select case(month)
    case(1,3,5,7,8,10,12)
      days=31
    case(2)
      days=28
      call is_leap(year,lleap)
      if (lleap) days=29
    case(4,6,9,11)
      days=30
    end select

  end subroutine

  subroutine is_leap(year,lleap)

    integer,intent(in)    :: year
    logical,intent(out)   :: lleap

    lleap = (mod(year,4)==0 .and. .not. mod(year,100)==0) .or. (mod(year,400)==0)

  end subroutine

!**********************************************************************
 
end module DateTimeUtilsModule
 
 
