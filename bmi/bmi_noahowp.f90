module bminoahowp

! NGEN_ACTIVE is to be set when running in the Nextgen framework
! https://github.com/NOAA-OWP/ngen
#ifdef NGEN_ACTIVE
   use bmif_2_0_iso
#else
   use bmif_2_0
#endif
  use bmi_grid
  use RunModule
  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer
  
  implicit none
  integer, parameter :: BMI_MAX_LOCATION_NAME = 4 !only valid options are node, face, edge, s
  type, extends (bmi) :: bmi_noahowp
     private
     type(noahowpgrid_type) :: model
   contains
     procedure :: get_component_name => noahowp_component_name
     procedure :: get_input_item_count => noahowp_input_item_count
     procedure :: get_output_item_count => noahowp_output_item_count
     procedure :: get_input_var_names => noahowp_input_var_names
     procedure :: get_output_var_names => noahowp_output_var_names
     procedure :: initialize => noahowp_initialize
     procedure :: finalize => noahowp_finalize
     procedure :: get_start_time => noahowp_start_time
     procedure :: get_end_time => noahowp_end_time
     procedure :: get_current_time => noahowp_current_time
     procedure :: get_time_step => noahowp_time_step
     procedure :: get_time_units => noahowp_time_units
     procedure :: update => noahowp_update
     procedure :: update_until => noahowp_update_until
     procedure :: get_var_grid => noahowp_var_grid
     procedure :: get_grid_type => noahowp_grid_type
     procedure :: get_grid_rank => noahowp_grid_rank
     procedure :: get_grid_shape => noahowp_grid_shape
     procedure :: get_grid_size => noahowp_grid_size
     procedure :: get_grid_spacing => noahowp_grid_spacing
     procedure :: get_grid_origin => noahowp_grid_origin
     procedure :: get_grid_x => noahowp_grid_x
     procedure :: get_grid_y => noahowp_grid_y
     procedure :: get_grid_z => noahowp_grid_z
     procedure :: get_grid_node_count => noahowp_grid_node_count
     procedure :: get_grid_edge_count => noahowp_grid_edge_count
     procedure :: get_grid_face_count => noahowp_grid_face_count
     procedure :: get_grid_edge_nodes => noahowp_grid_edge_nodes
     procedure :: get_grid_face_edges => noahowp_grid_face_edges
     procedure :: get_grid_face_nodes => noahowp_grid_face_nodes
     procedure :: get_grid_nodes_per_face => noahowp_grid_nodes_per_face
     procedure :: get_var_type => noahowp_var_type
     procedure :: get_var_units => noahowp_var_units
     procedure :: get_var_itemsize => noahowp_var_itemsize
     procedure :: get_var_nbytes => noahowp_var_nbytes
     procedure :: get_var_location => noahowp_var_location
     procedure :: get_value_int => noahowp_get_int
     procedure :: get_value_float => noahowp_get_float
     procedure :: get_value_double => noahowp_get_double
     generic :: get_value => &
          get_value_int, &
          get_value_float, &
          get_value_double
     procedure :: get_value_ptr_int => noahowp_get_ptr_int
     procedure :: get_value_ptr_float => noahowp_get_ptr_float
     procedure :: get_value_ptr_double => noahowp_get_ptr_double
     generic :: get_value_ptr => &
           get_value_ptr_int, &
           get_value_ptr_float, &
           get_value_ptr_double
     procedure :: get_value_at_indices_int => noahowp_get_at_indices_int
     procedure :: get_value_at_indices_float => noahowp_get_at_indices_float
     procedure :: get_value_at_indices_double => noahowp_get_at_indices_double
     generic :: get_value_at_indices => &
           get_value_at_indices_int, &
           get_value_at_indices_float, &
           get_value_at_indices_double
     procedure :: set_value_int => noahowp_set_int
     procedure :: set_value_float => noahowp_set_float
     procedure :: set_value_double => noahowp_set_double
     generic :: set_value => &
          set_value_int, &
          set_value_float, &
          set_value_double
     procedure :: set_value_at_indices_int => noahowp_set_at_indices_int
     procedure :: set_value_at_indices_float => noahowp_set_at_indices_float
     procedure :: set_value_at_indices_double => noahowp_set_at_indices_double
     generic :: set_value_at_indices => &
           set_value_at_indices_int, &
           set_value_at_indices_float, &
           set_value_at_indices_double
!     procedure :: print_model_info
  end type bmi_noahowp

  private
  public :: bmi_noahowp

  character (len=BMI_MAX_COMPONENT_NAME), target :: &
       component_name = "Noah-OWP-Modular Surface Module"

  ! Exchange items
  integer, parameter :: input_item_count = 8
  integer, parameter :: output_item_count = 23
  character (len=BMI_MAX_VAR_NAME), target, &
       dimension(input_item_count) :: input_items
  character (len=BMI_MAX_VAR_NAME), target, &
       dimension(output_item_count) :: output_items 
   ! grid id assignments, indexed by input/output items
   integer :: input_grid(input_item_count) = 1
   ! could use different grids per variable by explicit assignment of grid id, e.g. [0, 0, 0, 1, 2, 2]
   integer :: output_grid(output_item_count) = 1
   ! grid/variable location mapping
   character (len=BMI_MAX_LOCATION_NAME) :: &
   output_location(output_item_count) = 'node'
   ! can also be explicitly mapped per variable, indexed by input/output_item
   ! input_location(4) = [character(BMI_MAX_LOCATION_NAME):: 'node', 'node', 'node', 'node']
   character (len=BMI_MAX_LOCATION_NAME) :: &
   input_location(input_item_count) = 'node'

   ! grid meta structure
   ! all variables are associated with a grid spec
   ! by convention grids(1) is the "scalar" grid
   ! for noah-owp-modular, grids(2) is the 2D grid
   ! other grids/specs can be added as needed
   type(GridType) :: grids(3)

contains

  ! Get the name of the model.
  function noahowp_component_name(this, name) result (bmi_status)
    class (bmi_noahowp), intent(in) :: this
    character (len=*), pointer, intent(out) :: name
    integer :: bmi_status

    name => component_name
    bmi_status = BMI_SUCCESS
  end function noahowp_component_name

  ! Count the input variables.
  function noahowp_input_item_count(this, count) result (bmi_status)
    class (bmi_noahowp), intent(in) :: this
    integer, intent(out) :: count
    integer :: bmi_status

    count = input_item_count
    bmi_status = BMI_SUCCESS
  end function noahowp_input_item_count

  ! Count the output variables.
  function noahowp_output_item_count(this, count) result (bmi_status)
    class (bmi_noahowp), intent(in) :: this
    integer, intent(out) :: count
    integer :: bmi_status

    count = output_item_count
    bmi_status = BMI_SUCCESS
  end function noahowp_output_item_count

  ! List input variables.
  function noahowp_input_var_names(this, names) result (bmi_status)
    class (bmi_noahowp), intent(in) :: this
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
  end function noahowp_input_var_names

  ! List output variables.
  function noahowp_output_var_names(this, names) result (bmi_status)
    class (bmi_noahowp), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status

    output_items(1) = 'QINSUR'     ! total liquid water input to surface rate (m/s)
    output_items(2) = 'ETRAN'      ! transpiration rate (mm)
    output_items(3) = 'QSEVA'      ! evaporation rate (mm/s)
    output_items(4) = 'EVAPOTRANS' ! evapotranspiration rate (mm)
    output_items(5) = 'TG'         ! surface/ground temperature (K) (becomes snow surface temperature when snow is present)
    output_items(6) = 'SNEQV'      ! snow water equivalent (mm)
    output_items(7) = 'TGS'        ! ground temperature (K) (is equal to TG when no snow and equal to bottom snow element temperature when there is snow)
    output_items(8) = 'ACSNOM'     ! Accumulated meltwater from bottom snow layer (mm) (NWM 3.0 output variable)
    output_items(9) = 'SNOWT_AVG'  ! Average snow temperature (K) (by layer mass) (NWM 3.0 output variable)
    output_items(10) = 'ISNOW'     ! Number of snow layers (unitless) (NWM 3.0 output variable)
    output_items(11) = 'QRAIN'     ! Rainfall rate on the ground (mm/s) (NWM 3.0 output variable)
    output_items(12) = 'FSNO'      ! Snow-cover fraction on the ground (unitless fraction) (NWM 3.0 output variable)
    output_items(13) = 'SNOWH'     ! Snow depth (m) (NWM 3.0 output variable)
    output_items(14) = 'SNLIQ'     ! Snow layer liquid water (mm) (NWM 3.0 output variable)
    output_items(15) = 'QSNOW'     ! Snowfall rate on the ground (mm/s) (NWM 3.0 output variable)
    output_items(16) = 'ECAN'      ! evaporation of intercepted water (mm) (NWM 3.0 output variable)
    output_items(17) = 'GH'        ! Heat flux into the soil (W/m-2) (NWM 3.0 output variable)
    output_items(18) = 'TRAD'      ! Surface radiative temperature (K) (NWM 3.0 output variable)
    output_items(19) = 'FSA'       ! Total absorbed SW radiation (W/m-2) (NWM 3.0 output variable)
    output_items(20) = 'CMC'       ! Total canopy water (liquid + ice) (mm) (NWM 3.0 output variable)
    output_items(21) = 'LH'        ! Total latent heat to the atmosphere (W/m-2) (NWM 3.0 output variable)
    output_items(22) = 'FIRA'      ! Total net LW radiation to atmosphere (W/m-2) (NWM 3.0 output variable)
    output_items(23) = 'FSH'       ! Total sensible heat to the atmosphere (W/m-2) (NWM 3.0 output variable)

    names => output_items
    bmi_status = BMI_SUCCESS
  end function noahowp_output_var_names

  ! BMI initializer.
  function noahowp_initialize(this, config_file) result (bmi_status)
    class (bmi_noahowp), intent(out) :: this
    character (len=*), intent(in) :: config_file
    integer :: bmi_status

    if (len(config_file) > 0) then
       call initialize_from_file(this%model,config_file)
    else
       !call initialize_from_defaults(this%model)
    end if
    ! initialize the grid meta data
    ! params are grid_id, rank, type, units
    call grids(1)%init(0, 0, scalar, none) !the scalar grid
    call grids(2)%init(1, 2, uniform_rectilinear, none)
    call grids(3)%init(2, 3, uniform_rectilinear, none)
    ! for now, use the domain info in the model read from its various files
    ! TODO in the future, can use a config flag to indicate whether or not this is approriate
    ! or whether we want dynamic grid allocation
    ! even if the grid spec is initially empty cause the model domain is empty, we can still
    ! update it later (just have to remember to adjust the model as well...)
    ! at some point we can align these two things more and make that more cohesive
    call set_grid_from_model(this%model,grids(1))
    call set_grid_from_model(this%model,grids(2))
    call set_grid_from_model(this%model,grids(3))
    bmi_status = BMI_SUCCESS
  end function noahowp_initialize

  ! BMI finalizer.
  function noahowp_finalize(this) result (bmi_status)
    class (bmi_noahowp), intent(inout) :: this
    integer :: bmi_status

    call cleanup(this%model)
    bmi_status = BMI_SUCCESS
  end function noahowp_finalize

  ! Model start time.
  function noahowp_start_time(this, time) result (bmi_status)
    class (bmi_noahowp), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = 0.d0
    bmi_status = BMI_SUCCESS
  end function noahowp_start_time

  ! Model end time.
  function noahowp_end_time(this, time) result (bmi_status)
    class (bmi_noahowp), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = dble(this%model%domaingrid%ntime * this%model%domaingrid%dt)
    bmi_status = BMI_SUCCESS
  end function noahowp_end_time

  ! Model current time.
  function noahowp_current_time(this, time) result (bmi_status)
    class (bmi_noahowp), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = dble(this%model%domaingrid%time_dbl)
    bmi_status = BMI_SUCCESS
  end function noahowp_current_time

  ! Model time step.
  function noahowp_time_step(this, time_step) result (bmi_status)
    class (bmi_noahowp), intent(in) :: this
    double precision, intent(out) :: time_step
    integer :: bmi_status

    time_step = dble(this%model%domaingrid%dt)
    bmi_status = BMI_SUCCESS
  end function noahowp_time_step

  ! Model time units.
  function noahowp_time_units(this, units) result (bmi_status)
    class (bmi_noahowp), intent(in) :: this
    character (len=*), intent(out) :: units
    integer :: bmi_status

    units = "s"
    bmi_status = BMI_SUCCESS
  end function noahowp_time_units

  ! Advance model by one time step.
  function noahowp_update(this) result (bmi_status)
    class (bmi_noahowp), intent(inout) :: this
    integer :: bmi_status

    call advance_in_time(this%model)

    bmi_status = BMI_SUCCESS
  end function noahowp_update

  ! Advance the model until the given time.
  function noahowp_update_until(this, time) result (bmi_status)
    class (bmi_noahowp), intent(inout) :: this
    double precision, intent(in) :: time
    integer :: bmi_status
    double precision :: n_steps_real
    integer :: n_steps, i, s

    if (time < this%model%domaingrid%time_dbl) then
       bmi_status = BMI_FAILURE
       return
    end if

    n_steps_real = (time - this%model%domaingrid%time_dbl) / this%model%domaingrid%dt
    n_steps = floor(n_steps_real)
    do i = 1, n_steps
       s = this%update()
    end do
!     call update_frac(this, n_steps_real - dble(n_steps)) ! NOT IMPLEMENTED
    bmi_status = BMI_SUCCESS
  end function noahowp_update_until

  ! Get the grid id for a particular variable.
  function noahowp_var_grid(this, name, grid) result (bmi_status)
   class (bmi_noahowp), intent(in) :: this
   character (len=*), intent(in) :: name
   integer, intent(out) :: grid
   integer :: bmi_status, i

      ! SNLIQ
      output_grid(14) = 2

      !checkout output vars
      do  i = 1, size(output_items)
         if(output_items(i) .eq. trim(name) ) then
            grid = output_grid(i)
            bmi_status = BMI_SUCCESS
            return
         endif
      end do

      !checkout input vars
      do  i = 1, size(input_items)
         if(input_items(i) .eq. trim(name) ) then
            grid = input_grid(i)
            bmi_status = BMI_SUCCESS
            return
         endif
      end do

      !check any other vars???

      !no matches
     grid = -1
     bmi_status = BMI_FAILURE

 end function noahowp_var_grid

  ! The type of a variable's grid.
 function noahowp_grid_type(this, grid, type) result (bmi_status)
   class (bmi_noahowp), intent(in) :: this
   integer, intent(in) :: grid
   character (len=*), intent(out) :: type
   integer :: bmi_status

   select case(grid)
   case(0)
      type = "scalar"
      bmi_status = BMI_SUCCESS
   case(1,2)
     type = "uniform_rectilinear"
     bmi_status = BMI_SUCCESS
   case default
      type = "-"
      bmi_status = BMI_FAILURE
   end select
 end function noahowp_grid_type

  ! The number of dimensions of a grid.
 function noahowp_grid_rank(this, grid, rank) result (bmi_status)
   class (bmi_noahowp), intent(in) :: this
   integer, intent(in) :: grid
   integer, intent(out) :: rank
   integer :: bmi_status, i

   ! Failure unless we find what we are looking for...
   bmi_status = BMI_FAILURE
   do i = 1, size(grids)
     if ( grids(i)%id .eq. grid ) then
       rank = grids(i)%rank
       bmi_status = BMI_SUCCESS
       return
     end if
   end do

 end function noahowp_grid_rank

  ! The dimensions of a grid.
  function noahowp_grid_shape(this, grid, shape) result (bmi_status)
   class (bmi_noahowp), intent(in) :: this
   integer, intent(in) :: grid
   integer, dimension(:), intent(out) :: shape
   integer :: bmi_status, i

   ! Failure unless we find what we are looking for...
   bmi_status = BMI_FAILURE
   do i = 1, size(grids)
     if ( grids(i)%id .eq. grid ) then
       shape = grids(i)%shape
       bmi_status = BMI_SUCCESS
       return
     end if
   end do

 end function noahowp_grid_shape

  ! The total number of elements in a grid.
 function noahowp_grid_size(this, grid, size) result (bmi_status)
   class (bmi_noahowp), intent(in) :: this
   integer, intent(in) :: grid
   integer, intent(out) :: size
   integer :: bmi_status, i, upper

   block
     intrinsic :: size
     upper = size(grids)
   end block
   do i = 1, upper
     if ( grids(i)%id .eq. grid ) then
       size = product( grids(i)%shape )
       bmi_status = BMI_SUCCESS
       return
     end if
   end do

 end function noahowp_grid_size

  ! The distance between nodes of a grid.
  function noahowp_grid_spacing(this, grid, spacing) result (bmi_status)
    class (bmi_noahowp), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: spacing
    integer :: bmi_status, i

    bmi_status = BMI_FAILURE
    do i = 1, size(grids)
      if ( grids(i)%id .eq. grid ) then
        spacing = grids(i)%spacing
        bmi_status = BMI_SUCCESS
        return
      end if
    end do

  end function noahowp_grid_spacing
!
  ! Coordinates of grid origin.
  function noahowp_grid_origin(this, grid, origin) result (bmi_status)
    class (bmi_noahowp), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: origin
    integer :: bmi_status, i

    bmi_status = BMI_FAILURE
    do i = 1, size(grids)
      if ( grids(i)%id .eq. grid ) then
        origin = grids(i)%origin
        bmi_status = BMI_SUCCESS
        return
      end if
    end do

  end function noahowp_grid_origin

  ! X-coordinates of grid nodes.
  function noahowp_grid_x(this, grid, x) result (bmi_status)
    class (bmi_noahowp), intent(in)             :: this
    integer, intent(in)                         :: grid
    double precision, dimension(:), intent(out) :: x
    integer                                     :: bmi_status, i

    bmi_status = BMI_FAILURE
    do i = 1, size(grids)
      if ( grids(i)%id .eq. grid ) then
        call grids(i)%grid_x(x)
        bmi_status = BMI_SUCCESS
      end if
    end do

  end function noahowp_grid_x

  ! Y-coordinates of grid nodes.
  function noahowp_grid_y(this, grid, y) result (bmi_status)
   class (bmi_noahowp), intent(in)             :: this
   integer, intent(in)                         :: grid
   double precision, dimension(:), intent(out) :: y
   integer                                     :: bmi_status, i

   bmi_status = BMI_FAILURE
   do i = 1, size(grids)
     if ( grids(i)%id .eq. grid ) then
       call grids(i)%grid_y(y)
       bmi_status = BMI_SUCCESS
     end if
   end do

  end function noahowp_grid_y

  ! Z-coordinates of grid nodes.
  function noahowp_grid_z(this, grid, z) result (bmi_status)
    class (bmi_noahowp), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: z
    integer :: bmi_status, i

    bmi_status = BMI_FAILURE
    do i = 1, size(grids)
      if ( grids(i)%id .eq. grid ) then
        call grids(i)%grid_z(z)
        bmi_status = BMI_SUCCESS
      end if
    end do

  end function noahowp_grid_z

  ! Get the number of nodes in an unstructured grid.
  function noahowp_grid_node_count(this, grid, count) result(bmi_status)
    class(bmi_noahowp), intent(in) :: this
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
  end function noahowp_grid_node_count

  ! Get the number of edges in an unstructured grid.
  function noahowp_grid_edge_count(this, grid, count) result(bmi_status)
    class(bmi_noahowp), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: count
    integer :: bmi_status

    count = -1
    bmi_status = BMI_FAILURE
  end function noahowp_grid_edge_count

  ! Get the number of faces in an unstructured grid.
  function noahowp_grid_face_count(this, grid, count) result(bmi_status)
    class(bmi_noahowp), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: count
    integer :: bmi_status

    count = -1
    bmi_status = BMI_FAILURE
  end function noahowp_grid_face_count

  ! Get the edge-node connectivity.
  function noahowp_grid_edge_nodes(this, grid, edge_nodes) result(bmi_status)
    class(bmi_noahowp), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: edge_nodes
    integer :: bmi_status

    edge_nodes(:) = -1
    bmi_status = BMI_FAILURE
  end function noahowp_grid_edge_nodes

  ! Get the face-edge connectivity.
  function noahowp_grid_face_edges(this, grid, face_edges) result(bmi_status)
    class(bmi_noahowp), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: face_edges
    integer :: bmi_status

    face_edges(:) = -1
    bmi_status = BMI_FAILURE
  end function noahowp_grid_face_edges

  ! Get the face-node connectivity.
  function noahowp_grid_face_nodes(this, grid, face_nodes) result(bmi_status)
    class(bmi_noahowp), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: face_nodes
    integer :: bmi_status

    face_nodes(:) = -1
    bmi_status = BMI_FAILURE
  end function noahowp_grid_face_nodes

  ! Get the number of nodes for each face.
  function noahowp_grid_nodes_per_face(this, grid, nodes_per_face) result(bmi_status)
    class(bmi_noahowp), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: nodes_per_face
    integer :: bmi_status

    nodes_per_face(:) = -1
    bmi_status = BMI_FAILURE
  end function noahowp_grid_nodes_per_face

  ! The data type of the variable, as a string.
  function noahowp_var_type(this, name, type) result (bmi_status)
   class (bmi_noahowp), intent(in) :: this
   character (len=*), intent(in) :: name
   character (len=*), intent(out) :: type
   integer :: bmi_status

   select case(name)
   case('SFCPRS', 'SFCTMP', 'SOLDN', 'LWDN', 'UU', 'VV', 'Q2', 'PRCPNONC', & ! forcing vars
        'QINSUR', 'ETRAN', 'QSEVA', 'EVAPOTRANS', 'TG', 'SNEQV', 'TGS', 'ACSNOM', 'SNOWT_AVG', &      ! output vars
        'QRAIN', 'FSNO', 'SNOWH', 'SNLIQ', 'QSNOW', 'ECAN', 'GH', 'TRAD', 'FSA', 'CMC', 'LH', 'FIRA', 'FSH')
        type = "real"
      bmi_status = BMI_SUCCESS
   case('ISNOW')
      type = "integer"
      bmi_status = BMI_SUCCESS
   case default
      type = "-"
      bmi_status = BMI_FAILURE
   end select

  end function noahowp_var_type

  ! The units of the given variable.
  function noahowp_var_units(this, name, units) result (bmi_status)
   class (bmi_noahowp), intent(in) :: this
   character (len=*), intent(in) :: name
   character (len=*), intent(out) :: units
   integer :: bmi_status

   select case(name)
   case("SFCPRS")
      units = "Pa"
      bmi_status = BMI_SUCCESS
   case("SFCTMP", "TG", "TGS","SNOWT_AVG","TRAD")
      units = "K"
      bmi_status = BMI_SUCCESS
   case("SOLDN", "LWDN", "GH", "FSA", "LH", "FIRA", "FSH")
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
   case("PRCPNONC", "QRAIN", "QSEVA", "QSNOW")
      units = "mm/s"
      bmi_status = BMI_SUCCESS
   case("SNEQV", "ACSNOW", "EVAPOTRANS", "SNLIQ", "ECAN", "ETRAN", "CMC")
      units = "mm"
      bmi_status = BMI_SUCCESS
   case("FSNO","ISNOW")
      units = "unitless"
      bmi_status = BMI_SUCCESS
   case("SNOWH")
      units = "m"
      bmi_status = BMI_SUCCESS
   case default
      units = "-"
      bmi_status = BMI_FAILURE
   end select

 end function noahowp_var_units

  ! Memory use per array element.
 function noahowp_var_itemsize(this, name, size) result (bmi_status)
   class (bmi_noahowp), intent(in) :: this
   character (len=*), intent(in) :: name
   integer, intent(out) :: size
   integer :: bmi_status

   associate(forcinggrid => this%model%forcinggrid, &
             watergrid   => this%model%watergrid,   &
             energygrid  => this%model%energygrid)

   select case(name)
   case("SFCPRS")
      size = sizeof(forcinggrid%sfcprs(1,1)) ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("SFCTMP")
      size = sizeof(forcinggrid%sfctmp(1,1))             ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("SOLDN")
      size = sizeof(forcinggrid%soldn(1,1))                ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("LWDN")
      size = sizeof(forcinggrid%lwdn(1,1))                ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("UU")
      size = sizeof(forcinggrid%uu(1,1))                ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("VV")
      size = sizeof(forcinggrid%vv(1,1))                ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("Q2")
      size = sizeof(forcinggrid%q2(1,1))                ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("PRCPNONC")
      size = sizeof(forcinggrid%prcpnonc(1,1))                ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("QINSUR")
      size = sizeof(watergrid%qinsur(1,1))                ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("ETRAN")
      size = sizeof(watergrid%etran(1,1))                ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("QSEVA")
      size = sizeof(watergrid%qseva(1,1))                ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("EVAPOTRANS")
      size = sizeof(watergrid%evapotrans(1,1))            ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("TG")
      size = sizeof(energygrid%tg(1,1))            ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("SNEQV")
      size = sizeof(watergrid%sneqv(1,1))            ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("TGS")
      size = sizeof(energygrid%tgs(1,1))            ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("ACSNOM")
      size = sizeof(watergrid%ACSNOM(1,1))            ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("SNOWT_AVG")
      size = sizeof(energygrid%SNOWT_AVG(1,1))            ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("ISNOW")
      size = sizeof(watergrid%ISNOW(1,1))            ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("QRAIN")
      size = sizeof(watergrid%QRAIN(1,1))            ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("FSNO")
      size = sizeof(watergrid%FSNO(1,1))            ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("SNOWH")
      size = sizeof(watergrid%SNOWH(1,1))            ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("QSNOW")
      size = sizeof(watergrid%QSNOW(1,1))            ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("ECAN")
      size = sizeof(watergrid%ECAN(1,1))            ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("GH")
      size = sizeof(energygrid%GH(1,1))            ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("TRAD")
      size = sizeof(energygrid%TRAD(1,1))            ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("FSA")
      size = sizeof(energygrid%FSA(1,1))            ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("CMC")
      size = sizeof(watergrid%CMC(1,1))            ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("LH")
      size = sizeof(energygrid%LH(1,1))            ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("FIRA")
      size = sizeof(energygrid%FIRA(1,1))            ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("FSH")
      size = sizeof(energygrid%FSH(1,1))            ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case("SNLIQ")
      size = sizeof(watergrid%SNLIQ(1,1,1))            ! 'sizeof' in gcc & ifort
      bmi_status = BMI_SUCCESS
   case default
      size = -1
      bmi_status = BMI_FAILURE
   end select

   end associate

  end function noahowp_var_itemsize

  ! The size of the given variable.
  function noahowp_var_nbytes(this, name, nbytes) result (bmi_status)
   class (bmi_noahowp), intent(in) :: this
   character (len=*), intent(in) :: name
   integer, intent(out) :: nbytes
   integer :: bmi_status
   integer :: s1, s2, s3, grid, grid_size, item_size

   s1 = this%get_var_grid(name, grid)
   s2 = this%get_grid_size(grid, grid_size)
   s3 = this%get_var_itemsize(name, item_size)

    if( grid .eq. 0) then
      !these are the scalar values wrapped in an array
      !not likely needed in this case unless scalars are re-introduced to this model
      !but it is important to have this special case cause grid_size will return 0 for scalars
      !since it is dependent on the rank, which is is 0 in the scalar case
      nbytes = item_size
      bmi_status = BMI_SUCCESS
    else if ((s1 == BMI_SUCCESS).and.(s2 == BMI_SUCCESS).and.(s3 == BMI_SUCCESS)) then
       nbytes = item_size * grid_size
       bmi_status = BMI_SUCCESS
    else
       nbytes = -1
       bmi_status = BMI_FAILURE
    end if

 end function noahowp_var_nbytes

  ! The location (node, face, edge) of the given variable.
  function noahowp_var_location(this, name, location) result (bmi_status)
    class (bmi_noahowp), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: location
    integer :: bmi_status, i
    
    !checkout output vars
    do  i = 1, size(output_items)
      if(output_items(i) .eq. trim(name) ) then
        location = output_location(i)
        bmi_status = BMI_SUCCESS
        return
      endif
    end do

    !checkout input vars
    do  i = 1, size(input_items)
      if(input_items(i) .eq. trim(name) ) then
        location = input_location(i)
        bmi_status = BMI_SUCCESS
        return
      endif
    end do
  
    !check any other vars???

    !no matches
    location = "-"
    bmi_status = BMI_FAILURE

  end function noahowp_var_location

  ! Get a copy of a integer variable's values, flattened.
  function noahowp_get_int(this, name, dest) result (bmi_status)
   class (bmi_noahowp), intent(in) :: this
   character (len=*), intent(in) :: name
   integer, intent(inout) :: dest(:)
   integer :: bmi_status

   associate(watergrid   => this%model%watergrid,       &
             n_x         => this%model%domaingrid%n_x,  &
             n_y         => this%model%domaingrid%n_y)

   select case(name)
!==================== UPDATE IMPLEMENTATION IF NECESSARY FOR INTEGER VARS =================
!     case("model__identification_number")
!        dest = [this%model%id]
!        bmi_status = BMI_SUCCESS
   case default
      dest(:) = -1
      bmi_status = BMI_FAILURE
   case("ISNOW")
      dest = reshape(watergrid%ISNOW,[n_x*n_y])
      bmi_status = BMI_SUCCESS
   end select

   end associate

 end function noahowp_get_int

  ! Get a copy of a real variable's values, flattened.
  function noahowp_get_float(this, name, dest) result (bmi_status)
   class (bmi_noahowp), intent(in)    :: this
   character (len=*), intent(in)      :: name
   real, intent(inout)                :: dest(:)
   integer                            :: bmi_status
   real, allocatable, dimension(:,:)  :: conv
   real                               :: mm2m = 0.001       ! unit conversion mm to m     
   real                               :: m2mm = 1000.       ! unit conversion m to mm

   associate(domaingrid  => this%model%domaingrid,      &
             forcinggrid => this%model%forcinggrid,     &
             watergrid   => this%model%watergrid,       &
             energygrid  => this%model%energygrid,      &
             n_x         => this%model%domaingrid%n_x,  &
             n_y         => this%model%domaingrid%n_y,  &
             nsnow       => this%model%levelsgrid%nsnow)

   select case(name)
   case("SFCPRS")
      dest = reshape(forcinggrid%sfcprs,[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("SFCTMP")
      dest = reshape(forcinggrid%sfctmp,[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("SOLDN")
      dest = reshape(forcinggrid%soldn,[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("LWDN")
      dest = reshape(forcinggrid%lwdn,[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("UU")
      dest = reshape(forcinggrid%uu,[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("VV")
      dest = reshape(forcinggrid%vv,[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("Q2")
      dest = reshape(forcinggrid%q2,[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("PRCPNONC")
      dest = reshape(forcinggrid%prcpnonc,[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("QINSUR")
      dest = reshape(watergrid%qinsur,[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("ETRAN")
      if(allocated(conv)) deallocate(conv)
      allocate(conv(n_x,n_y))
      conv = 0.
      where(domaingrid%mask == 1) conv = watergrid%etran*domaingrid%DT
      dest = reshape(conv,[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("QSEVA")
      if(allocated(conv)) deallocate(conv)
      allocate(conv(n_x,n_y))
      conv = 0.
      where(domaingrid%mask == 1) conv = watergrid%qseva*m2mm
      dest = reshape(conv,[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("EVAPOTRANS")
      if(allocated(conv)) deallocate(conv)
      allocate(conv(n_x,n_y))
      conv = 0.
      where(domaingrid%mask == 1) conv = watergrid%evapotrans*domaingrid%DT*mm2m
      dest = reshape(conv,[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("TG")
      dest = reshape(energygrid%tg,[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("SNEQV")
      dest = reshape(watergrid%sneqv,[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("TGS")
      dest = reshape(energygrid%tgs,[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("ACSNOM")
      dest = reshape(watergrid%ACSNOM,[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("SNOWT_AVG")
      dest = reshape(energygrid%SNOWT_AVG,[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("QRAIN")
      dest = reshape(watergrid%QRAIN,[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("FSNO")
      dest = reshape(watergrid%FSNO,[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("SNOWH")
      dest = reshape(watergrid%SNOWH,[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("SNLIQ")
      dest = reshape(watergrid%SNLIQ,[n_x*n_y*nsnow])
      bmi_status = BMI_SUCCESS
   case("QSNOW")
      dest = reshape(watergrid%QSNOW,[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("ECAN")
      if(allocated(conv)) deallocate(conv)
      allocate(conv(n_x,n_y))
      conv = 0.
      where(domaingrid%mask == 1) conv = watergrid%ECAN*domaingrid%DT
      dest = reshape(conv,[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("GH")
      dest = reshape(energygrid%GH(:,:),[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("TRAD")
      dest = reshape(energygrid%TRAD(:,:),[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("FSA")
      dest = reshape(energygrid%FSA(:,:),[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("CMC")
      dest = reshape(watergrid%CMC(:,:),[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("LH")
      dest = reshape(energygrid%LH(:,:),[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("FIRA")
      dest = reshape(energygrid%FIRA(:,:),[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case("FSH")
      dest = reshape(energygrid%FSH(:,:),[n_x*n_y])
      bmi_status = BMI_SUCCESS
   case default
      dest(:) = -1.0
      bmi_status = BMI_FAILURE
   end select
   ! NOTE, if vars are gridded, then use:
   ! dest = reshape(this%model%temperature, [this%model%n_x*this%model%n_y]) 

   end associate

 end function noahowp_get_float

  ! Get a copy of a double variable's values, flattened.
  function noahowp_get_double(this, name, dest) result (bmi_status)
    class (bmi_noahowp), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, intent(inout) :: dest(:)
    integer :: bmi_status

    !==================== UPDATE IMPLEMENTATION IF NECESSARY FOR DOUBLE VARS =================

    select case(name)
    case default
       dest(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function noahowp_get_double

   ! Get a reference to an integer-valued variable, flattened.
   function noahowp_get_ptr_int(this, name, dest_ptr) result (bmi_status)
     class (bmi_noahowp), intent(in) :: this
     character (len=*), intent(in) :: name
     integer, pointer, intent(inout) :: dest_ptr(:)
     integer :: bmi_status
     type (c_ptr) :: src
     integer :: n_elements

     select case(name)
     case default
        bmi_status = BMI_FAILURE
     end select
   end function noahowp_get_ptr_int

   ! Get a reference to a real-valued variable, flattened.
   function noahowp_get_ptr_float(this, name, dest_ptr) result (bmi_status)
     class (bmi_noahowp), intent(in) :: this
     character (len=*), intent(in) :: name
     real, pointer, intent(inout) :: dest_ptr(:)
     integer :: bmi_status
     type (c_ptr) :: src
     integer :: n_elements

     select case(name)
     case default
        bmi_status = BMI_FAILURE
     end select

     call c_f_pointer(src, dest_ptr, [n_elements])

   end function noahowp_get_ptr_float

   ! Get a reference to an double-valued variable, flattened.
   function noahowp_get_ptr_double(this, name, dest_ptr) result (bmi_status)
     class (bmi_noahowp), intent(in) :: this
     character (len=*), intent(in) :: name
     double precision, pointer, intent(inout) :: dest_ptr(:)
     integer :: bmi_status
     type (c_ptr) :: src
     integer :: n_elements

     select case(name)
     case default
        bmi_status = BMI_FAILURE
     end select
   end function noahowp_get_ptr_double

   ! Get values of an integer variable at the given locations.
   function noahowp_get_at_indices_int(this, name, dest, inds) &
        result (bmi_status)
     class (bmi_noahowp), intent(in) :: this
     character (len=*), intent(in) :: name
     integer, intent(inout) :: dest(:)
     integer, intent(in) :: inds(:)
     integer :: bmi_status
!     type (c_ptr) src
!     integer, pointer :: src_flattened(:)
!     integer :: i, n_elements

     select case(name)
     case default
        bmi_status = BMI_FAILURE
     end select
   end function noahowp_get_at_indices_int

   ! Get values of a real variable at the given locations.
   function noahowp_get_at_indices_float(this, name, dest, inds) &
        result (bmi_status)
     class (bmi_noahowp), intent(in) :: this
     character (len=*), intent(in) :: name
     real, intent(inout) :: dest(:)
     integer, intent(in) :: inds(:)
     integer :: bmi_status
!     type (c_ptr) src
!     real, pointer :: src_flattened(:)
!     integer :: i, n_elements

     select case(name)
!     case("plate_surface__temperature")
!        src = c_loc(this%model%temperature(1,1))
!        call c_f_pointer(src, src_flattened, [this%model%n_y * this%model%n_x])
!        n_elements = size(inds)
!        do i = 1, n_elements
!           dest(i) = src_flattened(inds(i))
!        end do
!        bmi_status = BMI_SUCCESS
     case default
        bmi_status = BMI_FAILURE
     end select
   end function noahowp_get_at_indices_float

   ! Get values of a double variable at the given locations.
   function noahowp_get_at_indices_double(this, name, dest, inds) &
        result (bmi_status)
     class (bmi_noahowp), intent(in) :: this
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
   end function noahowp_get_at_indices_double

  ! Set new integer values.
   function noahowp_set_int(this, name, src) result (bmi_status)
      class (bmi_noahowp), intent(inout) :: this
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
    end function noahowp_set_int

  ! Set new real values.
  function noahowp_set_float(this, name, src) result (bmi_status)
   class (bmi_noahowp), intent(inout) :: this
   character (len=*), intent(in)      :: name
   real, intent(in)                   :: src(:)
   integer                            :: bmi_status
   real, allocatable, dimension(:,:)  :: conv
   real                               :: mm2m = 0.001       ! unit conversion mm to m     
   real                               :: m2mm = 1000.       ! unit conversion m to mm

   associate(domaingrid  => this%model%domaingrid,   &
             forcinggrid => this%model%forcinggrid,  &
             watergrid   => this%model%watergrid,    &
             energygrid  => this%model%energygrid,   &
             n_x         => this%model%domaingrid%n_x, &
             n_y         => this%model%domaingrid%n_y)

   select case(name)
   case("SFCPRS")
      forcinggrid%sfcprs = reshape(src,[n_x,n_y])
      bmi_status = BMI_SUCCESS
   case("SFCTMP")
      forcinggrid%sfctmp = reshape(src,[n_x,n_y])
      bmi_status = BMI_SUCCESS
   case("SOLDN")
      forcinggrid%soldn = reshape(src,[n_x,n_y])
      bmi_status = BMI_SUCCESS
   case("LWDN")
      forcinggrid%lwdn = reshape(src,[n_x,n_y])
      bmi_status = BMI_SUCCESS
   case("UU")
      forcinggrid%uu = reshape(src,[n_x,n_y])
      bmi_status = BMI_SUCCESS
   case("VV")
      forcinggrid%vv = reshape(src,[n_x,n_y])
      bmi_status = BMI_SUCCESS
   case("Q2")
      forcinggrid%q2 = reshape(src,[n_x,n_y])
      bmi_status = BMI_SUCCESS
   case("PRCPNONC")
      forcinggrid%prcpnonc = reshape(src,[n_x,n_y])
      bmi_status = BMI_SUCCESS
   case("QINSUR")
      watergrid%qinsur = reshape(src,[n_x,n_y])
      bmi_status = BMI_SUCCESS
   case("ETRAN")
      if(allocated(conv)) deallocate(conv)
      allocate(conv(n_x,n_y))
      conv = reshape(src,[n_x,n_y])
      where(domaingrid%mask == 1) conv = conv/domaingrid%DT
      watergrid%etran = conv
      bmi_status = BMI_SUCCESS
   case("QSEVA")
      if(allocated(conv)) deallocate(conv)
      allocate(conv(n_x,n_y))
      conv = reshape(src,[n_x,n_y])
      where(domaingrid%mask == 1) conv = conv*mm2m
      watergrid%qseva = conv
      bmi_status = BMI_SUCCESS
   case("EVAPOTRANS")
      if(allocated(conv)) deallocate(conv)
      allocate(conv(n_x,n_y))
      conv = reshape(src,[n_x,n_y])
      where(domaingrid%mask == 1) conv = conv*m2mm/domaingrid%DT
      watergrid%evapotrans = conv
      bmi_status = BMI_SUCCESS
   case("TG")
      energygrid%tg = reshape(src,[n_x,n_y])
      bmi_status = BMI_SUCCESS
   case("SNEQV")
      watergrid%sneqv = reshape(src,[n_x,n_y])
      bmi_status = BMI_SUCCESS
   case("TGS")
      energygrid%tgs = reshape(src,[n_x,n_y])
      bmi_status = BMI_SUCCESS
   case default
      bmi_status = BMI_FAILURE
   end select
   ! NOTE, if vars are gridded, then use:
   ! this%model%temperature = reshape(src, [this%model%n_y, this%model%n_x])
   end associate

 end function noahowp_set_float

  ! Set new double values.
  function noahowp_set_double(this, name, src) result (bmi_status)
    class (bmi_noahowp), intent(inout) :: this
    character (len=*), intent(in) :: name
    double precision, intent(in) :: src(:)
    integer :: bmi_status

    !==================== UPDATE IMPLEMENTATION IF NECESSARY FOR DOUBLE VARS =================

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function noahowp_set_double

   ! Set integer values at particular locations.
   function noahowp_set_at_indices_int(this, name, inds, src) &
        result (bmi_status)
     class (bmi_noahowp), intent(inout) :: this
     character (len=*), intent(in) :: name
     integer, intent(in) :: inds(:)
     integer, intent(in) :: src(:)
     integer :: bmi_status
!     type (c_ptr) dest
!     integer, pointer :: dest_flattened(:)
!     integer :: i

     select case(name)
     case default
        bmi_status = BMI_FAILURE
     end select
   end function noahowp_set_at_indices_int

   ! Set real values at particular locations.
   function noahowp_set_at_indices_float(this, name, inds, src) &
        result (bmi_status)
     class (bmi_noahowp), intent(inout) :: this
     character (len=*), intent(in) :: name
     integer, intent(in) :: inds(:)
     real, intent(in) :: src(:)
     integer :: bmi_status
!     type (c_ptr) dest
!     real, pointer :: dest_flattened(:)
!     integer :: i

     select case(name)
!     case("plate_surface__temperature")
!        dest = c_loc(this%model%temperature(1,1))
!        call c_f_pointer(dest, dest_flattened, [this%model%n_y * this%model%n_x])
!        do i = 1, size(inds)
!           dest_flattened(inds(i)) = src(i)
!        end do
!        bmi_status = BMI_SUCCESS
     case default
        bmi_status = BMI_FAILURE
     end select
   end function noahowp_set_at_indices_float

   ! Set double values at particular locations.
   function noahowp_set_at_indices_double(this, name, inds, src) &
        result (bmi_status)
     class (bmi_noahowp), intent(inout) :: this
     character (len=*), intent(in) :: name
     integer, intent(in) :: inds(:)
     double precision, intent(in) :: src(:)
     integer :: bmi_status
!     type (c_ptr) dest
!     double precision, pointer :: dest_flattened(:)
!     integer :: i

     select case(name)
     case default
        bmi_status = BMI_FAILURE
     end select
   end function noahowp_set_at_indices_double

   ! A non-BMI helper routine to advance the model by a fractional time step.
!   subroutine update_frac(this, time_frac)
!     class (bmi_noahowp), intent(inout) :: this
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
!     class (bmi_noahowp), intent(in) :: this
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
   type(bmi_noahowp), pointer :: bmi_model
   !Create a simple pointer wrapper
   type(box), pointer :: bmi_box

   !allocate model
   allocate(bmi_noahowp::bmi_model)
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
end module bminoahowp
