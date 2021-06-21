module bmiheatf

  use heatf
  use bmif_2_0
  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer
  implicit none

  type, extends (bmi) :: bmi_heat
     private
     type (heat_model) :: model
   contains
     procedure :: get_component_name => heat_component_name
     procedure :: get_input_item_count => heat_input_item_count
     procedure :: get_output_item_count => heat_output_item_count
     procedure :: get_input_var_names => heat_input_var_names
     procedure :: get_output_var_names => heat_output_var_names
     procedure :: initialize => heat_initialize
     procedure :: finalize => heat_finalize
     procedure :: get_start_time => heat_start_time
     procedure :: get_end_time => heat_end_time
     procedure :: get_current_time => heat_current_time
     procedure :: get_time_step => heat_time_step
     procedure :: get_time_units => heat_time_units
     procedure :: update => heat_update
     procedure :: update_until => heat_update_until
     procedure :: get_var_grid => heat_var_grid
     procedure :: get_grid_type => heat_grid_type
     procedure :: get_grid_rank => heat_grid_rank
     procedure :: get_grid_shape => heat_grid_shape
     procedure :: get_grid_size => heat_grid_size
     procedure :: get_grid_spacing => heat_grid_spacing
     procedure :: get_grid_origin => heat_grid_origin
     procedure :: get_grid_x => heat_grid_x
     procedure :: get_grid_y => heat_grid_y
     procedure :: get_grid_z => heat_grid_z
     procedure :: get_grid_node_count => heat_grid_node_count
     procedure :: get_grid_edge_count => heat_grid_edge_count
     procedure :: get_grid_face_count => heat_grid_face_count
     procedure :: get_grid_edge_nodes => heat_grid_edge_nodes
     procedure :: get_grid_face_edges => heat_grid_face_edges
     procedure :: get_grid_face_nodes => heat_grid_face_nodes
     procedure :: get_grid_nodes_per_face => heat_grid_nodes_per_face
     procedure :: get_var_type => heat_var_type
     procedure :: get_var_units => heat_var_units
     procedure :: get_var_itemsize => heat_var_itemsize
     procedure :: get_var_nbytes => heat_var_nbytes
     procedure :: get_var_location => heat_var_location
     procedure :: get_value_int => heat_get_int
     procedure :: get_value_float => heat_get_float
     procedure :: get_value_double => heat_get_double
     generic :: get_value => &
          get_value_int, &
          get_value_float, &
          get_value_double
     procedure :: get_value_ptr_int => heat_get_ptr_int
     procedure :: get_value_ptr_float => heat_get_ptr_float
     procedure :: get_value_ptr_double => heat_get_ptr_double
     generic :: get_value_ptr => &
          get_value_ptr_int, &
          get_value_ptr_float, &
          get_value_ptr_double
     procedure :: get_value_at_indices_int => heat_get_at_indices_int
     procedure :: get_value_at_indices_float => heat_get_at_indices_float
     procedure :: get_value_at_indices_double => heat_get_at_indices_double
     generic :: get_value_at_indices => &
          get_value_at_indices_int, &
          get_value_at_indices_float, &
          get_value_at_indices_double
     procedure :: set_value_int => heat_set_int
     procedure :: set_value_float => heat_set_float
     procedure :: set_value_double => heat_set_double
     generic :: set_value => &
          set_value_int, &
          set_value_float, &
          set_value_double
     procedure :: set_value_at_indices_int => heat_set_at_indices_int
     procedure :: set_value_at_indices_float => heat_set_at_indices_float
     procedure :: set_value_at_indices_double => heat_set_at_indices_double
     generic :: set_value_at_indices => &
          set_value_at_indices_int, &
          set_value_at_indices_float, &
          set_value_at_indices_double
     procedure :: print_model_info
  end type bmi_heat

  private
  public :: bmi_heat

  character (len=BMI_MAX_COMPONENT_NAME), target :: &
       component_name = "The 2D Heat Equation"

  ! Exchange items
  integer, parameter :: input_item_count = 3
  integer, parameter :: output_item_count = 1
  character (len=BMI_MAX_VAR_NAME), target, &
       dimension(input_item_count) :: input_items
  character (len=BMI_MAX_VAR_NAME), target, &
       dimension(output_item_count) :: &
       output_items = (/'plate_surface__temperature'/)

contains

  ! Get the name of the model.
  function heat_component_name(this, name) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    character (len=*), pointer, intent(out) :: name
    integer :: bmi_status

    name => component_name
    bmi_status = BMI_SUCCESS
  end function heat_component_name

  ! Count the input variables.
  function heat_input_item_count(this, count) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    integer, intent(out) :: count
    integer :: bmi_status

    count = input_item_count
    bmi_status = BMI_SUCCESS
  end function heat_input_item_count

  ! Count the output variables.
  function heat_output_item_count(this, count) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    integer, intent(out) :: count
    integer :: bmi_status

    count = output_item_count
    bmi_status = BMI_SUCCESS
  end function heat_output_item_count

  ! List input variables.
  function heat_input_var_names(this, names) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status

    input_items(1) = 'plate_surface__temperature'
    input_items(2) = 'plate_surface__thermal_diffusivity'
    input_items(3) = 'model__identification_number'

    names => input_items
    bmi_status = BMI_SUCCESS
  end function heat_input_var_names

  ! List output variables.
  function heat_output_var_names(this, names) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status

    names => output_items
    bmi_status = BMI_SUCCESS
  end function heat_output_var_names

  ! BMI initializer.
  function heat_initialize(this, config_file) result (bmi_status)
    class (bmi_heat), intent(out) :: this
    character (len=*), intent(in) :: config_file
    integer :: bmi_status

    if (len(config_file) > 0) then
       call initialize_from_file(this%model, config_file)
    else
       call initialize_from_defaults(this%model)
    end if
    bmi_status = BMI_SUCCESS
  end function heat_initialize

  ! BMI finalizer.
  function heat_finalize(this) result (bmi_status)
    class (bmi_heat), intent(inout) :: this
    integer :: bmi_status

    call cleanup(this%model)
    bmi_status = BMI_SUCCESS
  end function heat_finalize

  ! Model start time.
  function heat_start_time(this, time) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = 0.d0
    bmi_status = BMI_SUCCESS
  end function heat_start_time

  ! Model end time.
  function heat_end_time(this, time) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = dble(this%model%t_end)
    bmi_status = BMI_SUCCESS
  end function heat_end_time

  ! Model current time.
  function heat_current_time(this, time) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = dble(this%model%t)
    bmi_status = BMI_SUCCESS
  end function heat_current_time

  ! Model time step.
  function heat_time_step(this, time_step) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    double precision, intent(out) :: time_step
    integer :: bmi_status

    time_step = dble(this%model%dt)
    bmi_status = BMI_SUCCESS
  end function heat_time_step

  ! Model time units.
  function heat_time_units(this, units) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    character (len=*), intent(out) :: units
    integer :: bmi_status

    units = "s"
    bmi_status = BMI_SUCCESS
  end function heat_time_units

  ! Advance model by one time step.
  function heat_update(this) result (bmi_status)
    class (bmi_heat), intent(inout) :: this
    integer :: bmi_status

    call advance_in_time(this%model)
    bmi_status = BMI_SUCCESS
  end function heat_update

  ! Advance the model until the given time.
  function heat_update_until(this, time) result (bmi_status)
    class (bmi_heat), intent(inout) :: this
    double precision, intent(in) :: time
    integer :: bmi_status
    double precision :: n_steps_real
    integer :: n_steps, i, s

    if (time < this%model%t) then
       bmi_status = BMI_FAILURE
       return
    end if

    n_steps_real = (time - this%model%t) / this%model%dt
    n_steps = floor(n_steps_real)
    do i = 1, n_steps
       s = this%update()
    end do
    call update_frac(this, n_steps_real - dble(n_steps)) ! See near bottom of file
    bmi_status = BMI_SUCCESS
  end function heat_update_until

  ! Get the grid id for a particular variable.
  function heat_var_grid(this, name, grid) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: grid
    integer :: bmi_status

    select case(name)
    case('plate_surface__temperature')
       grid = 0
       bmi_status = BMI_SUCCESS
    case('plate_surface__thermal_diffusivity')
       grid = 1
       bmi_status = BMI_SUCCESS
    case('model__identification_number')
       grid = 1
       bmi_status = BMI_SUCCESS
    case default
       grid = -1
       bmi_status = BMI_FAILURE
    end select
  end function heat_var_grid

  ! The type of a variable's grid.
  function heat_grid_type(this, grid, type) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    integer, intent(in) :: grid
    character (len=*), intent(out) :: type
    integer :: bmi_status

    select case(grid)
    case(0)
       type = "uniform_rectilinear"
       bmi_status = BMI_SUCCESS
    case(1)
       type = "scalar"
       bmi_status = BMI_SUCCESS
    case default
       type = "-"
       bmi_status = BMI_FAILURE
    end select
  end function heat_grid_type

  ! The number of dimensions of a grid.
  function heat_grid_rank(this, grid, rank) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: rank
    integer :: bmi_status

    select case(grid)
    case(0)
       rank = 2
       bmi_status = BMI_SUCCESS
    case(1)
       rank = 0
       bmi_status = BMI_SUCCESS
    case default
       rank = -1
       bmi_status = BMI_FAILURE
    end select
  end function heat_grid_rank

  ! The dimensions of a grid.
  function heat_grid_shape(this, grid, shape) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: shape
    integer :: bmi_status

    select case(grid)
    case(0)
       shape(:) = [this%model%n_y, this%model%n_x]
       bmi_status = BMI_SUCCESS
    case default
       shape(:) = -1
       bmi_status = BMI_FAILURE
    end select
  end function heat_grid_shape

  ! The total number of elements in a grid.
  function heat_grid_size(this, grid, size) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: size
    integer :: bmi_status

    select case(grid)
    case(0)
       size = this%model%n_y * this%model%n_x
       bmi_status = BMI_SUCCESS
    case(1)
       size = 1
       bmi_status = BMI_SUCCESS
    case default
       size = -1
       bmi_status = BMI_FAILURE
    end select
  end function heat_grid_size

  ! The distance between nodes of a grid.
  function heat_grid_spacing(this, grid, spacing) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: spacing
    integer :: bmi_status

    select case(grid)
    case(0)
       spacing(:) = [this%model%dy, this%model%dx]
       bmi_status = BMI_SUCCESS
    case default
       spacing(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function heat_grid_spacing

  ! Coordinates of grid origin.
  function heat_grid_origin(this, grid, origin) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: origin
    integer :: bmi_status

    select case(grid)
    case(0)
       origin(:) = [0.d0, 0.d0]
       bmi_status = BMI_SUCCESS
    case default
       origin(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function heat_grid_origin

  ! X-coordinates of grid nodes.
  function heat_grid_x(this, grid, x) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: x
    integer :: bmi_status

    select case(grid)
    case(1)
       x(:) = [0.d0]
       bmi_status = BMI_SUCCESS
    case default
       x(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function heat_grid_x

  ! Y-coordinates of grid nodes.
  function heat_grid_y(this, grid, y) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: y
    integer :: bmi_status

    select case(grid)
    case(1)
       y(:) = [0.d0]
       bmi_status = BMI_SUCCESS
    case default
       y(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function heat_grid_y

  ! Z-coordinates of grid nodes.
  function heat_grid_z(this, grid, z) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: z
    integer :: bmi_status

    select case(grid)
    case(1)
       z(:) = [0.d0]
       bmi_status = BMI_SUCCESS
    case default
       z(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function heat_grid_z

  ! Get the number of nodes in an unstructured grid.
  function heat_grid_node_count(this, grid, count) result(bmi_status)
    class(bmi_heat), intent(in) :: this
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
  end function heat_grid_node_count

  ! Get the number of edges in an unstructured grid.
  function heat_grid_edge_count(this, grid, count) result(bmi_status)
    class(bmi_heat), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: count
    integer :: bmi_status

    count = -1
    bmi_status = BMI_FAILURE
  end function heat_grid_edge_count

  ! Get the number of faces in an unstructured grid.
  function heat_grid_face_count(this, grid, count) result(bmi_status)
    class(bmi_heat), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: count
    integer :: bmi_status

    count = -1
    bmi_status = BMI_FAILURE
  end function heat_grid_face_count

  ! Get the edge-node connectivity.
  function heat_grid_edge_nodes(this, grid, edge_nodes) result(bmi_status)
    class(bmi_heat), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: edge_nodes
    integer :: bmi_status

    edge_nodes(:) = -1
    bmi_status = BMI_FAILURE
  end function heat_grid_edge_nodes

  ! Get the face-edge connectivity.
  function heat_grid_face_edges(this, grid, face_edges) result(bmi_status)
    class(bmi_heat), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: face_edges
    integer :: bmi_status

    face_edges(:) = -1
    bmi_status = BMI_FAILURE
  end function heat_grid_face_edges

  ! Get the face-node connectivity.
  function heat_grid_face_nodes(this, grid, face_nodes) result(bmi_status)
    class(bmi_heat), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: face_nodes
    integer :: bmi_status

    face_nodes(:) = -1
    bmi_status = BMI_FAILURE
  end function heat_grid_face_nodes

  ! Get the number of nodes for each face.
  function heat_grid_nodes_per_face(this, grid, nodes_per_face) result(bmi_status)
    class(bmi_heat), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: nodes_per_face
    integer :: bmi_status

    nodes_per_face(:) = -1
    bmi_status = BMI_FAILURE
  end function heat_grid_nodes_per_face

  ! The data type of the variable, as a string.
  function heat_var_type(this, name, type) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: type
    integer :: bmi_status

    select case(name)
    case("plate_surface__temperature")
       type = "real"
       bmi_status = BMI_SUCCESS
    case("plate_surface__thermal_diffusivity")
       type = "real"
       bmi_status = BMI_SUCCESS
    case("model__identification_number")
       type = "integer"
       bmi_status = BMI_SUCCESS
    case default
       type = "-"
       bmi_status = BMI_FAILURE
    end select
  end function heat_var_type

  ! The units of the given variable.
  function heat_var_units(this, name, units) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: units
    integer :: bmi_status

    select case(name)
    case("plate_surface__temperature")
       units = "K"
       bmi_status = BMI_SUCCESS
    case("plate_surface__thermal_diffusivity")
       units = "m2 s-1"
       bmi_status = BMI_SUCCESS
    case("model__identification_number")
       units = "1"
       bmi_status = BMI_SUCCESS
    case default
       units = "-"
       bmi_status = BMI_FAILURE
    end select
  end function heat_var_units

  ! Memory use per array element.
  function heat_var_itemsize(this, name, size) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: size
    integer :: bmi_status

    select case(name)
    case("plate_surface__temperature")
       size = sizeof(this%model%temperature(1,1))  ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("plate_surface__thermal_diffusivity")
       size = sizeof(this%model%alpha)             ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("model__identification_number")
       size = sizeof(this%model%id)                ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case default
       size = -1
       bmi_status = BMI_FAILURE
    end select
  end function heat_var_itemsize

  ! The size of the given variable.
  function heat_var_nbytes(this, name, nbytes) result (bmi_status)
    class (bmi_heat), intent(in) :: this
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
  end function heat_var_nbytes

  ! The location (node, face, edge) of the given variable.
  function heat_var_location(this, name, location) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: location
    integer :: bmi_status

    select case(name)
    case default
       location = "node"
       bmi_status = BMI_SUCCESS
    end select
  end function heat_var_location

  ! Get a copy of a integer variable's values, flattened.
  function heat_get_int(this, name, dest) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
    case("model__identification_number")
       dest = [this%model%id]
       bmi_status = BMI_SUCCESS
    case default
       dest(:) = -1
       bmi_status = BMI_FAILURE
    end select
  end function heat_get_int

  ! Get a copy of a real variable's values, flattened.
  function heat_get_float(this, name, dest) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    character (len=*), intent(in) :: name
    real, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
    case("plate_surface__temperature")
       ! This would be safe, but subject to indexing errors.
       ! do j = 1, this%model%n_y
       !    do i = 1, this%model%n_x
       !       k = j + this%model%n_y*(i-1)
       !       dest(k) = this%model%temperature(j,i)
       !    end do
       ! end do

       ! This is an equivalent, elementwise copy into `dest`.
       ! See https://stackoverflow.com/a/11800068/1563298
       dest = reshape(this%model%temperature, [this%model%n_x*this%model%n_y])
       bmi_status = BMI_SUCCESS
    case("plate_surface__thermal_diffusivity")
       dest = [this%model%alpha]
       bmi_status = BMI_SUCCESS
    case default
       dest(:) = -1.0
       bmi_status = BMI_FAILURE
    end select
  end function heat_get_float

  ! Get a copy of a double variable's values, flattened.
  function heat_get_double(this, name, dest) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
    case default
       dest(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function heat_get_double

  ! Get a reference to an integer-valued variable, flattened.
  function heat_get_ptr_int(this, name, dest_ptr) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function heat_get_ptr_int

  ! Get a reference to a real-valued variable, flattened.
  function heat_get_ptr_float(this, name, dest_ptr) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    character (len=*), intent(in) :: name
    real, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements

    select case(name)
    case("plate_surface__temperature")
       src = c_loc(this%model%temperature(1,1))
       n_elements = this%model%n_y * this%model%n_x
       call c_f_pointer(src, dest_ptr, [n_elements])
       bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
  end function heat_get_ptr_float

  ! Get a reference to an double-valued variable, flattened.
  function heat_get_ptr_double(this, name, dest_ptr) result (bmi_status)
    class (bmi_heat), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function heat_get_ptr_double

  ! Get values of an integer variable at the given locations.
  function heat_get_at_indices_int(this, name, dest, inds) &
       result (bmi_status)
    class (bmi_heat), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status
    type (c_ptr) src
    integer, pointer :: src_flattened(:)
    integer :: i, n_elements

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function heat_get_at_indices_int

  ! Get values of a real variable at the given locations.
  function heat_get_at_indices_float(this, name, dest, inds) &
       result (bmi_status)
    class (bmi_heat), intent(in) :: this
    character (len=*), intent(in) :: name
    real, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status
    type (c_ptr) src
    real, pointer :: src_flattened(:)
    integer :: i, n_elements

    select case(name)
    case("plate_surface__temperature")
       src = c_loc(this%model%temperature(1,1))
       call c_f_pointer(src, src_flattened, [this%model%n_y * this%model%n_x])
       n_elements = size(inds)
       do i = 1, n_elements
          dest(i) = src_flattened(inds(i))
       end do
       bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
  end function heat_get_at_indices_float

  ! Get values of a double variable at the given locations.
  function heat_get_at_indices_double(this, name, dest, inds) &
       result (bmi_status)
    class (bmi_heat), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status
    type (c_ptr) src
    double precision, pointer :: src_flattened(:)
    integer :: i, n_elements

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function heat_get_at_indices_double

  ! Set new integer values.
  function heat_set_int(this, name, src) result (bmi_status)
    class (bmi_heat), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: src(:)
    integer :: bmi_status

    select case(name)
    case("model__identification_number")
       this%model%id = src(1)
       bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
  end function heat_set_int

  ! Set new real values.
  function heat_set_float(this, name, src) result (bmi_status)
    class (bmi_heat), intent(inout) :: this
    character (len=*), intent(in) :: name
    real, intent(in) :: src(:)
    integer :: bmi_status

    select case(name)
    case("plate_surface__temperature")
       this%model%temperature = reshape(src, [this%model%n_y, this%model%n_x])
       bmi_status = BMI_SUCCESS
    case("plate_surface__thermal_diffusivity")
       this%model%alpha = src(1)
       bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
  end function heat_set_float

  ! Set new double values.
  function heat_set_double(this, name, src) result (bmi_status)
    class (bmi_heat), intent(inout) :: this
    character (len=*), intent(in) :: name
    double precision, intent(in) :: src(:)
    integer :: bmi_status

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function heat_set_double

  ! Set integer values at particular locations.
  function heat_set_at_indices_int(this, name, inds, src) &
       result (bmi_status)
    class (bmi_heat), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: inds(:)
    integer, intent(in) :: src(:)
    integer :: bmi_status
    type (c_ptr) dest
    integer, pointer :: dest_flattened(:)
    integer :: i

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function heat_set_at_indices_int

  ! Set real values at particular locations.
  function heat_set_at_indices_float(this, name, inds, src) &
       result (bmi_status)
    class (bmi_heat), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: inds(:)
    real, intent(in) :: src(:)
    integer :: bmi_status
    type (c_ptr) dest
    real, pointer :: dest_flattened(:)
    integer :: i

    select case(name)
    case("plate_surface__temperature")
       dest = c_loc(this%model%temperature(1,1))
       call c_f_pointer(dest, dest_flattened, [this%model%n_y * this%model%n_x])
       do i = 1, size(inds)
          dest_flattened(inds(i)) = src(i)
       end do
       bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
  end function heat_set_at_indices_float

  ! Set double values at particular locations.
  function heat_set_at_indices_double(this, name, inds, src) &
       result (bmi_status)
    class (bmi_heat), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: inds(:)
    double precision, intent(in) :: src(:)
    integer :: bmi_status
    type (c_ptr) dest
    double precision, pointer :: dest_flattened(:)
    integer :: i

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function heat_set_at_indices_double

  ! A non-BMI helper routine to advance the model by a fractional time step.
  subroutine update_frac(this, time_frac)
    class (bmi_heat), intent(inout) :: this
    double precision, intent(in) :: time_frac
    real :: time_step

    if (time_frac > 0.0) then
       time_step = this%model%dt
       this%model%dt = time_step*real(time_frac)
       call advance_in_time(this%model)
       this%model%dt = time_step
    end if
  end subroutine update_frac

  ! A non-BMI procedure for model introspection.
  subroutine print_model_info(this)
    class (bmi_heat), intent(in) :: this

    call print_info(this%model)
  end subroutine print_model_info

end module bmiheatf
