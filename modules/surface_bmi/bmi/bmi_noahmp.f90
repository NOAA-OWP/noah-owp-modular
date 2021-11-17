module bminoahmp

! NGEN_ACTIVE is to be set when running in the Nextgen framework
! https://github.com/NOAA-OWP/ngen
#ifdef NGEN_ACTIVE
   use bmif_2_0_iso
#else
   use bmif_2_0
#endif

  use NoahMPSurfaceModule 
  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer
  implicit none

  type, extends (bmi) :: bmi_noahmp
     private
     type (noahmp_type) :: model
   contains
     procedure :: get_component_name => noahmp_component_name
     procedure :: get_input_item_count => noahmp_input_item_count
     procedure :: get_output_item_count => noahmp_output_item_count
     procedure :: get_input_var_names => noahmp_input_var_names
     procedure :: get_output_var_names => noahmp_output_var_names
     procedure :: initialize => noahmp_initialize
     procedure :: finalize => noahmp_finalize
     procedure :: get_start_time => noahmp_start_time
     procedure :: get_end_time => noahmp_end_time
     procedure :: get_current_time => noahmp_current_time
     procedure :: get_time_step => noahmp_time_step
     procedure :: get_time_units => noahmp_time_units
     procedure :: update => noahmp_update
     procedure :: update_until => noahmp_update_until
     procedure :: get_var_grid => noahmp_var_grid
     procedure :: get_grid_type => noahmp_grid_type
     procedure :: get_grid_rank => noahmp_grid_rank
     procedure :: get_grid_shape => noahmp_grid_shape
     procedure :: get_grid_size => noahmp_grid_size
     procedure :: get_grid_spacing => noahmp_grid_spacing
     procedure :: get_grid_origin => noahmp_grid_origin
     procedure :: get_grid_x => noahmp_grid_x
     procedure :: get_grid_y => noahmp_grid_y
     procedure :: get_grid_z => noahmp_grid_z
     procedure :: get_grid_node_count => noahmp_grid_node_count
     procedure :: get_grid_edge_count => noahmp_grid_edge_count
     procedure :: get_grid_face_count => noahmp_grid_face_count
     procedure :: get_grid_edge_nodes => noahmp_grid_edge_nodes
     procedure :: get_grid_face_edges => noahmp_grid_face_edges
     procedure :: get_grid_face_nodes => noahmp_grid_face_nodes
     procedure :: get_grid_nodes_per_face => noahmp_grid_nodes_per_face
     procedure :: get_var_type => noahmp_var_type
     procedure :: get_var_units => noahmp_var_units
     procedure :: get_var_itemsize => noahmp_var_itemsize
     procedure :: get_var_nbytes => noahmp_var_nbytes
     procedure :: get_var_location => noahmp_var_location
     procedure :: get_value_int => noahmp_get_int
     procedure :: get_value_float => noahmp_get_float
     procedure :: get_value_double => noahmp_get_double
     generic :: get_value => &
          get_value_int, &
          get_value_float, &
          get_value_double
!      procedure :: get_value_ptr_int => noahmp_get_ptr_int
!      procedure :: get_value_ptr_float => noahmp_get_ptr_float
!      procedure :: get_value_ptr_double => noahmp_get_ptr_double
!      generic :: get_value_ptr => &
!           get_value_ptr_int, &
!           get_value_ptr_float, &
!           get_value_ptr_double
!      procedure :: get_value_at_indices_int => noahmp_get_at_indices_int
!      procedure :: get_value_at_indices_float => noahmp_get_at_indices_float
!      procedure :: get_value_at_indices_double => noahmp_get_at_indices_double
!      generic :: get_value_at_indices => &
!           get_value_at_indices_int, &
!           get_value_at_indices_float, &
!           get_value_at_indices_double
     procedure :: set_value_int => noahmp_set_int
     procedure :: set_value_float => noahmp_set_float
     procedure :: set_value_double => noahmp_set_double
     generic :: set_value => &
          set_value_int, &
          set_value_float, &
          set_value_double
!      procedure :: set_value_at_indices_int => noahmp_set_at_indices_int
!      procedure :: set_value_at_indices_float => noahmp_set_at_indices_float
!      procedure :: set_value_at_indices_double => noahmp_set_at_indices_double
!      generic :: set_value_at_indices => &
!           set_value_at_indices_int, &
!           set_value_at_indices_float, &
!           set_value_at_indices_double
!      procedure :: print_model_info
  end type bmi_noahmp

  private
  public :: bmi_noahmp

  character (len=BMI_MAX_COMPONENT_NAME), target :: &
       component_name = "NOAH-MP Surface Module"

  ! Exchange items
  integer, parameter :: input_item_count = 8
  integer, parameter :: output_item_count = 3
  character (len=BMI_MAX_VAR_NAME), target, &
       dimension(input_item_count) :: input_items
  character (len=BMI_MAX_VAR_NAME), target, &
       dimension(output_item_count) :: output_items 

contains

  ! Get the name of the model.
  function noahmp_component_name(this, name) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    character (len=*), pointer, intent(out) :: name
    integer :: bmi_status

    name => component_name
    bmi_status = BMI_SUCCESS
  end function noahmp_component_name

  ! Count the input variables.
  function noahmp_input_item_count(this, count) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    integer, intent(out) :: count
    integer :: bmi_status

    count = input_item_count
    bmi_status = BMI_SUCCESS
  end function noahmp_input_item_count

  ! Count the output variables.
  function noahmp_output_item_count(this, count) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    integer, intent(out) :: count
    integer :: bmi_status

    count = output_item_count
    bmi_status = BMI_SUCCESS
  end function noahmp_output_item_count

  ! List input variables.
  function noahmp_input_var_names(this, names) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status

    input_items(1) = 'SFCPRS'   ! surface pressure (Pa)
    input_items(2) = 'SFCTMP'   ! surface air temperature (K)
    input_items(3) = 'SOLDN'    ! incoming shortwave radiation (W/m2)
    input_items(4) = 'LWDN'     ! incoming longwave radiation (W/m2)
    input_items(5) = 'UU'       ! wind speed in eastward direction (m/s)
    input_items(6) = 'VV'       ! wind speed in northward direction (m/s)
    input_items(7) = 'Q2'       ! mixing ratio (kg/kg)
    input_items(8) = 'PRCPNONC' ! precipitation rate (mm/s)
    

    names => input_items
    bmi_status = BMI_SUCCESS
  end function noahmp_input_var_names

  ! List output variables.
  function noahmp_output_var_names(this, names) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status

    output_items(1) = 'QINSUR'  ! total liquid water input to surface rate (m/s)
    output_items(2) = 'ETRAN'   ! transpiration rate (mm/s)
    output_items(3) = 'QSEVA'   ! evaporation rate (mm/s)

    names => output_items
    bmi_status = BMI_SUCCESS
  end function noahmp_output_var_names

  ! BMI initializer.
  function noahmp_initialize(this, config_file) result (bmi_status)
    class (bmi_noahmp), intent(out) :: this
    character (len=*), intent(in) :: config_file
    integer :: bmi_status

    if (len(config_file) > 0) then
       call initialize_from_file(this%model, config_file)
    else
       !call initialize_from_defaults(this%model)
    end if
    bmi_status = BMI_SUCCESS
  end function noahmp_initialize

  ! BMI finalizer.
  function noahmp_finalize(this) result (bmi_status)
    class (bmi_noahmp), intent(inout) :: this
    integer :: bmi_status

    call cleanup(this%model)
    bmi_status = BMI_SUCCESS
  end function noahmp_finalize

  ! Model start time.
  function noahmp_start_time(this, time) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = 0.d0
    bmi_status = BMI_SUCCESS
  end function noahmp_start_time

  ! Model end time.
  function noahmp_end_time(this, time) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = dble(this%model%domain%ntime * this%model%domain%dt)
    bmi_status = BMI_SUCCESS
  end function noahmp_end_time

  ! Model current time.
  function noahmp_current_time(this, time) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = dble(this%model%domain%time_dbl)
    bmi_status = BMI_SUCCESS
  end function noahmp_current_time

  ! Model time step.
  function noahmp_time_step(this, time_step) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    double precision, intent(out) :: time_step
    integer :: bmi_status

    time_step = dble(this%model%domain%dt)
    bmi_status = BMI_SUCCESS
  end function noahmp_time_step

  ! Model time units.
  function noahmp_time_units(this, units) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    character (len=*), intent(out) :: units
    integer :: bmi_status

    units = "s"
    bmi_status = BMI_SUCCESS
  end function noahmp_time_units

  ! Advance model by one time step.
  function noahmp_update(this) result (bmi_status)
    class (bmi_noahmp), intent(inout) :: this
    integer :: bmi_status

    call advance_in_time(this%model)
    bmi_status = BMI_SUCCESS
  end function noahmp_update

  ! Advance the model until the given time.
  function noahmp_update_until(this, time) result (bmi_status)
    class (bmi_noahmp), intent(inout) :: this
    double precision, intent(in) :: time
    integer :: bmi_status
    double precision :: n_steps_real
    integer :: n_steps, i, s

    if (time < this%model%domain%time_dbl) then
       bmi_status = BMI_FAILURE
       return
    end if

    n_steps_real = (time - this%model%domain%time_dbl) / this%model%domain%dt
    n_steps = floor(n_steps_real)
    do i = 1, n_steps
       s = this%update()
    end do
!     call update_frac(this, n_steps_real - dble(n_steps)) ! NOT IMPLEMENTED
    bmi_status = BMI_SUCCESS
  end function noahmp_update_until

  ! Get the grid id for a particular variable.
  function noahmp_var_grid(this, name, grid) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: grid
    integer :: bmi_status

    select case(name)
    case('SFCPRS', 'SFCTMP', 'SOLDN', 'LWDN', 'UU', 'VV', 'Q2', 'PRCPNONC', & ! input vars
         'QINSUR', 'ETRAN', 'QSEVA')                              ! output vars
       grid = 0
       bmi_status = BMI_SUCCESS
    case default
       grid = -1
       bmi_status = BMI_FAILURE
    end select
  end function noahmp_var_grid

  ! The type of a variable's grid.
  function noahmp_grid_type(this, grid, type) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    integer, intent(in) :: grid
    character (len=*), intent(out) :: type
    integer :: bmi_status

    select case(grid)
    case(0)
       type = "scalar"
       bmi_status = BMI_SUCCESS
!================================ IMPLEMENT WHEN NOAHMP DONE IN GRID ======================
!     case(1)
!       type = "uniform_rectilinear"
!        bmi_status = BMI_SUCCESS
    case default
       type = "-"
       bmi_status = BMI_FAILURE
    end select
  end function noahmp_grid_type

  ! The number of dimensions of a grid.
  function noahmp_grid_rank(this, grid, rank) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: rank
    integer :: bmi_status

    select case(grid)
    case(0)
       rank = 0
       bmi_status = BMI_SUCCESS
!================================ IMPLEMENT WHEN NOAHMP DONE IN GRID ======================
!     case(1)
!        rank = 2
!        bmi_status = BMI_SUCCESS
    case default
       rank = -1
       bmi_status = BMI_FAILURE
    end select
  end function noahmp_grid_rank

  ! The dimensions of a grid.
  function noahmp_grid_shape(this, grid, shape) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: shape
    integer :: bmi_status

    select case(grid)
!================================ IMPLEMENT WHEN NOAHMP DONE IN GRID ======================
! NOTE: Scalar "grids" do not have dimensions, ie. there is no case(0)
!     case(1)
!        shape(:) = [this%model%n_y, this%model%n_x]
!        bmi_status = BMI_SUCCESS
    case default
       shape(:) = -1
       bmi_status = BMI_FAILURE
    end select
  end function noahmp_grid_shape

  ! The total number of elements in a grid.
  function noahmp_grid_size(this, grid, size) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: size
    integer :: bmi_status

    select case(grid)
    case(0)
       size = 1
       bmi_status = BMI_SUCCESS
!================================ IMPLEMENT WHEN NOAHMP DONE IN GRID ======================
!     case(1)
!        size = this%model%n_y * this%model%n_x
!        bmi_status = BMI_SUCCESS
    case default
       size = -1
       bmi_status = BMI_FAILURE
    end select
  end function noahmp_grid_size

  ! The distance between nodes of a grid.
  function noahmp_grid_spacing(this, grid, spacing) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: spacing
    integer :: bmi_status

    select case(grid)
!================================ IMPLEMENT WHEN NOAHMP DONE IN GRID ======================
! NOTE: Scalar "grids" do not have spacing, ie. there is no case(0)
!     case(1)
!        spacing(:) = [this%model%dy, this%model%dx]
!        bmi_status = BMI_SUCCESS
    case default
       spacing(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function noahmp_grid_spacing
!
  ! Coordinates of grid origin.
  function noahmp_grid_origin(this, grid, origin) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: origin
    integer :: bmi_status

    select case(grid)
!================================ IMPLEMENT WHEN NOAHMP DONE IN GRID ======================
! NOTE: Scalar "grids" do not have coordinates, ie. there is no case(0)
!     case(1)
!        origin(:) = [0.d0, 0.d0]
!        bmi_status = BMI_SUCCESS
    case default
       origin(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function noahmp_grid_origin

  ! X-coordinates of grid nodes.
  function noahmp_grid_x(this, grid, x) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: x
    integer :: bmi_status

    select case(grid)
    case(0)
       x(:) = [0.d0]
       bmi_status = BMI_SUCCESS
    case default
       x(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function noahmp_grid_x

  ! Y-coordinates of grid nodes.
  function noahmp_grid_y(this, grid, y) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: y
    integer :: bmi_status

    select case(grid)
    case(0)
       y(:) = [0.d0]
       bmi_status = BMI_SUCCESS
    case default
       y(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function noahmp_grid_y

  ! Z-coordinates of grid nodes.
  function noahmp_grid_z(this, grid, z) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: z
    integer :: bmi_status

    select case(grid)
    case(0)
       z(:) = [0.d0]
       bmi_status = BMI_SUCCESS
    case default
       z(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function noahmp_grid_z

  ! Get the number of nodes in an unstructured grid.
  function noahmp_grid_node_count(this, grid, count) result(bmi_status)
    class(bmi_noahmp), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: count
    integer :: bmi_status

    select case(grid)
    case(0:1)
       bmi_status = this%get_grid_size(grid, count)
    case default
       count = -1
       bmi_status = BMI_FAILURE
    end select
  end function noahmp_grid_node_count

  ! Get the number of edges in an unstructured grid.
  function noahmp_grid_edge_count(this, grid, count) result(bmi_status)
    class(bmi_noahmp), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: count
    integer :: bmi_status

    count = -1
    bmi_status = BMI_FAILURE
  end function noahmp_grid_edge_count

  ! Get the number of faces in an unstructured grid.
  function noahmp_grid_face_count(this, grid, count) result(bmi_status)
    class(bmi_noahmp), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: count
    integer :: bmi_status

    count = -1
    bmi_status = BMI_FAILURE
  end function noahmp_grid_face_count

  ! Get the edge-node connectivity.
  function noahmp_grid_edge_nodes(this, grid, edge_nodes) result(bmi_status)
    class(bmi_noahmp), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: edge_nodes
    integer :: bmi_status

    edge_nodes(:) = -1
    bmi_status = BMI_FAILURE
  end function noahmp_grid_edge_nodes

  ! Get the face-edge connectivity.
  function noahmp_grid_face_edges(this, grid, face_edges) result(bmi_status)
    class(bmi_noahmp), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: face_edges
    integer :: bmi_status

    face_edges(:) = -1
    bmi_status = BMI_FAILURE
  end function noahmp_grid_face_edges

  ! Get the face-node connectivity.
  function noahmp_grid_face_nodes(this, grid, face_nodes) result(bmi_status)
    class(bmi_noahmp), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: face_nodes
    integer :: bmi_status

    face_nodes(:) = -1
    bmi_status = BMI_FAILURE
  end function noahmp_grid_face_nodes

  ! Get the number of nodes for each face.
  function noahmp_grid_nodes_per_face(this, grid, nodes_per_face) result(bmi_status)
    class(bmi_noahmp), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: nodes_per_face
    integer :: bmi_status

    nodes_per_face(:) = -1
    bmi_status = BMI_FAILURE
  end function noahmp_grid_nodes_per_face

  ! The data type of the variable, as a string.
  function noahmp_var_type(this, name, type) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: type
    integer :: bmi_status

    select case(name)
    case('SFCPRS', 'SFCTMP', 'SOLDN', 'LWDN', 'UU', 'VV', 'Q2', 'PRCPNONC', & ! input vars
         'QINSUR', 'ETRAN', 'QSEVA')                              ! output vars
       type = "real"
       bmi_status = BMI_SUCCESS
    case default
       type = "-"
       bmi_status = BMI_FAILURE
    end select
  end function noahmp_var_type

  ! The units of the given variable.
  function noahmp_var_units(this, name, units) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: units
    integer :: bmi_status

    select case(name)
    case("SFCPRS")
       units = "Pa"
       bmi_status = BMI_SUCCESS
    case("SFCTMP")
       units = "K"
       bmi_status = BMI_SUCCESS
    case("SOLDN", "LWDN")
       units = "W/m2"
       bmi_status = BMI_SUCCESS
    case("UU", "VV")
       units = "m/s"
       bmi_status = BMI_SUCCESS
    case("Q2")
       units = "kg/kg"
       bmi_status = BMI_SUCCESS
    case("QINSUR")
       units = "m/s"
       bmi_status = BMI_SUCCESS
    case("PRCPNONC", "ETRAN", "QSEVA")
       units = "mm/s"
       bmi_status = BMI_SUCCESS
    case default
       units = "-"
       bmi_status = BMI_FAILURE
    end select
  end function noahmp_var_units

  ! Memory use per array element.
  function noahmp_var_itemsize(this, name, size) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: size
    integer :: bmi_status

    select case(name)
    case("SFCPRS")
       size = sizeof(this%model%forcing%sfcprs)  ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("SFCTMP")
       size = sizeof(this%model%forcing%sfctmp)             ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("SOLDN")
       size = sizeof(this%model%forcing%soldn)                ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("LWDN")
       size = sizeof(this%model%forcing%lwdn)                ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("UU")
       size = sizeof(this%model%forcing%uu)                ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("VV")
       size = sizeof(this%model%forcing%vv)                ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("Q2")
       size = sizeof(this%model%forcing%q2)                ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("PRCPNONC")
       size = sizeof(this%model%forcing%prcpnonc)                ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("QINSUR")
       size = sizeof(this%model%water%qinsur)                ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("ETRAN")
       size = sizeof(this%model%water%etran)                ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("QSEVA")
       size = sizeof(this%model%water%qseva)                ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case default
       size = -1
       bmi_status = BMI_FAILURE
    end select
  end function noahmp_var_itemsize

  ! The size of the given variable.
  function noahmp_var_nbytes(this, name, nbytes) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: nbytes
    integer :: bmi_status
    integer :: s1, s2, s3, grid, grid_size, item_size

    s1 = this%get_var_grid(name, grid)
    s2 = this%get_grid_size(grid, grid_size)
    s3 = this%get_var_itemsize(name, item_size)

    if ((s1 == BMI_SUCCESS).and.(s2 == BMI_SUCCESS).and.(s3 == BMI_SUCCESS)) then
       nbytes = item_size * grid_size
       bmi_status = BMI_SUCCESS
    else
       nbytes = -1
       bmi_status = BMI_FAILURE
    end if
  end function noahmp_var_nbytes

  ! The location (node, face, edge) of the given variable.
  function noahmp_var_location(this, name, location) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: location
    integer :: bmi_status
!==================== UPDATE IMPLEMENTATION IF NECESSARY WHEN RUN ON GRID =================
    select case(name)
    case default
       location = "node"
       bmi_status = BMI_SUCCESS
    end select
  end function noahmp_var_location

  ! Get a copy of a integer variable's values, flattened.
  function noahmp_get_int(this, name, dest) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
!==================== UPDATE IMPLEMENTATION IF NECESSARY FOR INTEGER VARS =================
!     case("model__identification_number")
!        dest = [this%model%id]
!        bmi_status = BMI_SUCCESS
    case default
       dest(:) = -1
       bmi_status = BMI_FAILURE
    end select
  end function noahmp_get_int

  ! Get a copy of a real variable's values, flattened.
  function noahmp_get_float(this, name, dest) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    character (len=*), intent(in) :: name
    real, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
    case("SFCPRS")
       dest = [this%model%forcing%sfcprs]
       bmi_status = BMI_SUCCESS
    case("SFCTMP")
       dest = [this%model%forcing%sfctmp]
       bmi_status = BMI_SUCCESS
    case("SOLDN")
       dest = [this%model%forcing%soldn]
       bmi_status = BMI_SUCCESS
    case("LWDN")
       dest = [this%model%forcing%lwdn]
       bmi_status = BMI_SUCCESS
    case("UU")
       dest = [this%model%forcing%uu]
       bmi_status = BMI_SUCCESS
    case("VV")
       dest = [this%model%forcing%vv]
       bmi_status = BMI_SUCCESS
    case("Q2")
       dest = [this%model%forcing%q2]
       bmi_status = BMI_SUCCESS
    case("PRCPNONC")
       dest = [this%model%forcing%prcpnonc]
       bmi_status = BMI_SUCCESS
    case("QINSUR")
       dest = [this%model%water%qinsur]
       bmi_status = BMI_SUCCESS
    case("ETRAN")
       dest = [this%model%water%etran]
       bmi_status = BMI_SUCCESS
    case("QSEVA")
       dest = [this%model%water%qseva]
       bmi_status = BMI_SUCCESS
    case default
       dest(:) = -1.0
       bmi_status = BMI_FAILURE
    end select
    ! NOTE, if vars are gridded, then use:
    ! dest = reshape(this%model%temperature, [this%model%n_x*this%model%n_y]) 
  end function noahmp_get_float

  ! Get a copy of a double variable's values, flattened.
  function noahmp_get_double(this, name, dest) result (bmi_status)
    class (bmi_noahmp), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, intent(inout) :: dest(:)
    integer :: bmi_status

    !==================== UPDATE IMPLEMENTATION IF NECESSARY FOR DOUBLE VARS =================

    select case(name)
    case default
       dest(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function noahmp_get_double

! !=================== get_value_ptr functions not implemented yet =================

!   ! Get a reference to an integer-valued variable, flattened.
!   function noahmp_get_ptr_int(this, name, dest_ptr) result (bmi_status)
!     class (bmi_noahmp), intent(in) :: this
!     character (len=*), intent(in) :: name
!     integer, pointer, intent(inout) :: dest_ptr(:)
!     integer :: bmi_status
!     type (c_ptr) :: src
!     integer :: n_elements
!
! !==================== UPDATE IMPLEMENTATION IF NECESSARY FOR INTEGER VARS =================
!
!     select case(name)
!     case default
!        bmi_status = BMI_FAILURE
!     end select
!   end function noahmp_get_ptr_int
!
!   ! Get a reference to a real-valued variable, flattened.
!   function noahmp_get_ptr_float(this, name, dest_ptr) result (bmi_status)
!     class (bmi_noahmp), intent(in) :: this
!     character (len=*), intent(in) :: name
!     real, pointer, intent(inout) :: dest_ptr(:)
!     integer :: bmi_status, status
!     type (c_ptr) :: src
!     integer :: n_elements, gridid
!
!     select case(name)
!     case("SFCPRS")
!        src = c_loc(this%model%forcing%sfcprs)
!        status = this%get_var_grid(name,gridid)
!        status = this%get_grid_size(gridid, n_elements)
!        call c_f_pointer(src, dest_ptr, [n_elements])
!        bmi_status = BMI_SUCCESS
!     case default
!        bmi_status = BMI_FAILURE
!     end select
!   end function noahmp_get_ptr_float
!
!   ! Get a reference to an double-valued variable, flattened.
!   function noahmp_get_ptr_double(this, name, dest_ptr) result (bmi_status)
!     class (bmi_noahmp), intent(in) :: this
!     character (len=*), intent(in) :: name
!     double precision, pointer, intent(inout) :: dest_ptr(:)
!     integer :: bmi_status
!     type (c_ptr) :: src
!     integer :: n_elements
!
! !==================== UPDATE IMPLEMENTATION IF NECESSARY FOR DOUBLE VARS =================\
!
!     select case(name)
!     case default
!        bmi_status = BMI_FAILURE
!     end select
!   end function noahmp_get_ptr_double
!
!   ! Get values of an integer variable at the given locations.
!   function noahmp_get_at_indices_int(this, name, dest, inds) &
!        result (bmi_status)
!     class (bmi_noahmp), intent(in) :: this
!     character (len=*), intent(in) :: name
!     integer, intent(inout) :: dest(:)
!     integer, intent(in) :: inds(:)
!     integer :: bmi_status
!     type (c_ptr) src
!     integer, pointer :: src_flattened(:)
!     integer :: i, n_elements
!
!     select case(name)
!     case default
!        bmi_status = BMI_FAILURE
!     end select
!   end function noahmp_get_at_indices_int
!
!   ! Get values of a real variable at the given locations.
!   function noahmp_get_at_indices_float(this, name, dest, inds) &
!        result (bmi_status)
!     class (bmi_noahmp), intent(in) :: this
!     character (len=*), intent(in) :: name
!     real, intent(inout) :: dest(:)
!     integer, intent(in) :: inds(:)
!     integer :: bmi_status
!     type (c_ptr) src
!     real, pointer :: src_flattened(:)
!     integer :: i, n_elements
!
!     select case(name)
!     case("plate_surface__temperature")
!        src = c_loc(this%model%temperature(1,1))
!        call c_f_pointer(src, src_flattened, [this%model%n_y * this%model%n_x])
!        n_elements = size(inds)
!        do i = 1, n_elements
!           dest(i) = src_flattened(inds(i))
!        end do
!        bmi_status = BMI_SUCCESS
!     case default
!        bmi_status = BMI_FAILURE
!     end select
!   end function noahmp_get_at_indices_float
!
!   ! Get values of a double variable at the given locations.
!   function noahmp_get_at_indices_double(this, name, dest, inds) &
!        result (bmi_status)
!     class (bmi_noahmp), intent(in) :: this
!     character (len=*), intent(in) :: name
!     double precision, intent(inout) :: dest(:)
!     integer, intent(in) :: inds(:)
!     integer :: bmi_status
!     type (c_ptr) src
!     double precision, pointer :: src_flattened(:)
!     integer :: i, n_elements
!
!     select case(name)
!     case default
!        bmi_status = BMI_FAILURE
!     end select
!   end function noahmp_get_at_indices_double
!
  ! Set new integer values.
  function noahmp_set_int(this, name, src) result (bmi_status)
    class (bmi_noahmp), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: src(:)
    integer :: bmi_status

    !==================== UPDATE IMPLEMENTATION IF NECESSARY FOR INTEGER VARS =================

    select case(name)
!     case("model__identification_number")
!        this%model%id = src(1)
!        bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
  end function noahmp_set_int

  ! Set new real values.
  function noahmp_set_float(this, name, src) result (bmi_status)
    class (bmi_noahmp), intent(inout) :: this
    character (len=*), intent(in) :: name
    real, intent(in) :: src(:)
    integer :: bmi_status

    select case(name)
    case("SFCPRS")
       this%model%forcing%sfcprs = src(1)
       bmi_status = BMI_SUCCESS
    case("SFCTMP")
       this%model%forcing%sfctmp = src(1)
       bmi_status = BMI_SUCCESS
    case("SOLDN")
       this%model%forcing%soldn = src(1)
       bmi_status = BMI_SUCCESS
    case("LWDN")
       this%model%forcing%lwdn = src(1)
       bmi_status = BMI_SUCCESS
    case("UU")
       this%model%forcing%uu = src(1)
       bmi_status = BMI_SUCCESS
    case("VV")
       this%model%forcing%vv = src(1)
       bmi_status = BMI_SUCCESS
    case("Q2")
       this%model%forcing%q2 = src(1)
       bmi_status = BMI_SUCCESS
    case("PRCPNONC")
       this%model%forcing%prcpnonc = src(1)
       bmi_status = BMI_SUCCESS
    case("QINSUR")
       this%model%water%qinsur = src(1)
       bmi_status = BMI_SUCCESS
    case("ETRAN")
       this%model%water%etran = src(1)
       bmi_status = BMI_SUCCESS
    case("QSEVA")
       this%model%water%qseva = src(1)
       bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
    ! NOTE, if vars are gridded, then use:
    ! this%model%temperature = reshape(src, [this%model%n_y, this%model%n_x])
  end function noahmp_set_float

  ! Set new double values.
  function noahmp_set_double(this, name, src) result (bmi_status)
    class (bmi_noahmp), intent(inout) :: this
    character (len=*), intent(in) :: name
    double precision, intent(in) :: src(:)
    integer :: bmi_status

    !==================== UPDATE IMPLEMENTATION IF NECESSARY FOR DOUBLE VARS =================

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function noahmp_set_double
!
!   ! Set integer values at particular locations.
!   function noahmp_set_at_indices_int(this, name, inds, src) &
!        result (bmi_status)
!     class (bmi_noahmp), intent(inout) :: this
!     character (len=*), intent(in) :: name
!     integer, intent(in) :: inds(:)
!     integer, intent(in) :: src(:)
!     integer :: bmi_status
!     type (c_ptr) dest
!     integer, pointer :: dest_flattened(:)
!     integer :: i
!
!     select case(name)
!     case default
!        bmi_status = BMI_FAILURE
!     end select
!   end function noahmp_set_at_indices_int
!
!   ! Set real values at particular locations.
!   function noahmp_set_at_indices_float(this, name, inds, src) &
!        result (bmi_status)
!     class (bmi_noahmp), intent(inout) :: this
!     character (len=*), intent(in) :: name
!     integer, intent(in) :: inds(:)
!     real, intent(in) :: src(:)
!     integer :: bmi_status
!     type (c_ptr) dest
!     real, pointer :: dest_flattened(:)
!     integer :: i
!
!     select case(name)
!     case("plate_surface__temperature")
!        dest = c_loc(this%model%temperature(1,1))
!        call c_f_pointer(dest, dest_flattened, [this%model%n_y * this%model%n_x])
!        do i = 1, size(inds)
!           dest_flattened(inds(i)) = src(i)
!        end do
!        bmi_status = BMI_SUCCESS
!     case default
!        bmi_status = BMI_FAILURE
!     end select
!   end function noahmp_set_at_indices_float
!
!   ! Set double values at particular locations.
!   function noahmp_set_at_indices_double(this, name, inds, src) &
!        result (bmi_status)
!     class (bmi_noahmp), intent(inout) :: this
!     character (len=*), intent(in) :: name
!     integer, intent(in) :: inds(:)
!     double precision, intent(in) :: src(:)
!     integer :: bmi_status
!     type (c_ptr) dest
!     double precision, pointer :: dest_flattened(:)
!     integer :: i
!
!     select case(name)
!     case default
!        bmi_status = BMI_FAILURE
!     end select
!   end function noahmp_set_at_indices_double
!
!   ! A non-BMI helper routine to advance the model by a fractional time step.
!   subroutine update_frac(this, time_frac)
!     class (bmi_noahmp), intent(inout) :: this
!     double precision, intent(in) :: time_frac
!     real :: time_step
!
!     if (time_frac > 0.0) then
!        time_step = this%model%dt
!        this%model%dt = time_step*real(time_frac)
!        call advance_in_time(this%model)
!        this%model%dt = time_step
!     end if
!   end subroutine update_frac
!
!   ! A non-BMI procedure for model introspection.
!   subroutine print_model_info(this)
!     class (bmi_noahmp), intent(in) :: this
!
!     call print_info(this%model)
!   end subroutine print_model_info
#ifdef NGEN_ACTIVE
  function register_bmi(this) result(bmi_status) bind(C, name="register_bmi")
   use, intrinsic:: iso_c_binding, only: c_ptr, c_loc, c_int
   use iso_c_bmif_2_0
   implicit none
   type(c_ptr) :: this ! If not value, then from the C perspective `this` is a void**
   integer(kind=c_int) :: bmi_status
   !Create the model instance to use
   type(bmi_noahmp), pointer :: bmi_model
   !Create a simple pointer wrapper
   type(box), pointer :: bmi_box

   !allocate model
   allocate(bmi_noahmp::bmi_model)
   !allocate the pointer box
   allocate(bmi_box)

   !associate the wrapper pointer the created model instance
   bmi_box%ptr => bmi_model

   if( .not. associated( bmi_box ) .or. .not. associated( bmi_box%ptr ) ) then
    bmi_status = BMI_FAILURE
   else
    !Return the pointer to box
    this = c_loc(bmi_box)
    bmi_status = BMI_SUCCESS
   endif
 end function register_bmi
#endif
end module bminoahmp
