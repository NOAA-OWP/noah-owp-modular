# Implementing BMI into Noah-MP-Surface

As of 2021-09-08, we have implemented the Basic Model Interface (BMI) into the Noah-MP-Surface module which combines the Noah-MP water and energy balance routines from the top of the canopy to the soil surface with a simple hydrostatic subsurface treatment (for testing purposes only). In this form, Noah-MP-Surface is designed to be coupled to a subsurface module that handles runoff and vadose zone processes.

## BMI Basics

 [BMI](https://csdms.colorado.edu/wiki/BMI) is a set of functions that control model initialization, operation, and finalization, while also improving model interoperability. Developed by researchers at the Community Surface Dynamics Modeling System ([CSDMS](https://csdms.colorado.edu/wiki/Main_Page)), BMI standardizes and mediates the exchange of information across different modeling components, which may have different data requirements and time-space resolutions (Hutton et al., 2020; Peckham et al., 2013).

## BMI Implementation

In the Nextgen system, we implement BMI into hydrologic models and modules so that the framework can control model runtime and pass data from one formulation to another through the model engine. The implementation of BMI into a BMI-compliant model is relatively straightforward. The steps below are specific to how we created `modules/surface_bmi` from `modules/surface` for Noah-MP-Surface, but they can be applied to other Fortran models. For further technical details on our BMI implementation (e.g., specific code chunks, variable names, etc.), please see the relevant modified code:

  * <https://github.com/NOAA-OWP/noah-mp-modular/blob/main/modules/surface_bmi/bmi/bmi_noahmp.f90>
  * <https://github.com/NOAA-OWP/noah-mp-modular/blob/main/modules/surface_bmi/src/NoahMPSurfaceModule.f90>

### 0) Requirements

To run the BMI-enabled version of Noah-MP-Surface, your system needs:

  * A Fortran compiler
    * We tested the implementation on Mac OS Big Sur using GCC (gfortran) 11.2.0
  * NetCDF

Our implementation of BMI bundles the BMI source code with Noah-MP-Surface. If you wish to install BMI as a library so it can be accessed by other models, you will need follow the [instructions](https://github.com/csdms/bmi-fortran#buildinstall) from CSDMS and have Cmake on your system.

### 1) Start with Original Codebase

A key tenet of BMI is that it does not affect the underlying model source code, meaning BMI does not change the way a given model simulates the hydrologic balance. Therefore, to start our BMI implementation, we copied over `modules/surface` to `modules/surface_bmi`.

### 2) Add the BMI Source Code

Next, we added a subdirectory to `modules/surface_bmi` called `bmi`. We then copied the [Fortran 2003 BMI specification](https://github.com/csdms/bmi-fortran/blob/master/bmi.f90) from CSDMS into our `bmi` folder. Our project directory structure now looks like this (subdirectories only shown for `surface_bmi`):

```
noah-mp-modular
│   README.md
└───modules
    └───full
    └───surface
    └───surface_bmi
        └───bmi
        │   │   bmi.f90
        └───config
        └───docs
        └───driver
        └───parameters
        └───run
        └───src
        │   configure
        │   Makefile
        │   readme.md
        │   user_build_options
        
```

### 3) Create and Modify `bmi_noahmp.f90`

The `bmi.f90` file contains the `bmif_2_0` module which defines the abstract type `bmi` and its associated functions. Because the functions are deferred, they have to be implemented with a derived type that extends `bmi`. This is just a complex way of saying that we need to make a derived type that tailors the generalized `bmi` functions to Noah-MP-Surface.

We start with a new file in the `surface_bmi/bmi` subdirectory called `bmi_noahmp.f90`. For this step, we followed the [heat model example](https://github.com/csdms/bmi-example-fortran/) from CSDMS and copied over the [`bmi_heat.f90` file](https://github.com/csdms/bmi-example-fortran/blob/master/bmi_heat/bmi_heat.f90). We then modified the file to change _heat_ references to _noahmp_. Thus begins the implementation of the `bminoahmp` module and the derived type `bmi_noahmp` that extends the `bmi` type from `bmi.f90`.

### 4) Update the BMI Functions Part I

#### Model Info Functions
The BMI functions from `get_component_name` through `get_output_var_names` are straightforward to modify for Noah-MP-Surface. These functions require the user to enter the descriptive component name of their choice ("Noah-MP Surface Module") along with the input and output variables names and counts. These and all subsequent edits are made in the relevant sections of `bmi_noahmp.f90` that follow `end type bmi_noahmp`.

#### Initialize Function
The `initialize` function is more complex to implement, as it often requires a slight refactoring of how the model state is initialized. For Noah-MP-Surface without BMI, model intialization is handled by the driver. (The driver also handles other important model functionalities that move under BMI control, but these will be discussed later.) We moved most of the driver code to a new file under `src/` called `NoahMPSurfaceModule.f90`. We based this functionality on the heat example from CSDMS and the [PRMS BMI implementation](https://github.com/nhm-usgs/bmi-prms6-surface) from the USGS.

### 5) Create and Modify `NoahMPSurfaceModule.f90` for `initialize`

The `initialize` BMI function calls the `initialize_from_file()` subroutine in the newly created `src/NoahMPSurfaceModule.f90`. This file contains the subroutines for the BMI functions `initialize`, `update`, and `finalize` based on the original driver code. This file also includes the definition of the derived type `noahmp_type` that contains the derived types that are created and modified as Noah-MP-Surface runs:

```
  type :: noahmp_type
    type(namelist_type)   :: namelist   ! model configuration info
    type(levels_type)     :: levels     ! snow and soils layer info
    type(domain_type)     :: domain     ! geospatial info
    type(options_type)    :: options    ! model options
    type(parameters_type) :: parameters ! parameters
    type(water_type)      :: water      ! water flux and state values
    type(forcing_type)    :: forcing    ! raw and derived forcing data 
    type(energy_type)     :: energy     ! energy flux and state values
  end type noahmp_type
  
```

The `initialize_from_file()` subroutine includes all the previously driver-level code that handles model initialization: the reading of the namelist configuration file, the creation of the derived types shown above and the initialization of all values within, the initialization of other model values needed by BMI, and the opening of the forcing and output files.

### 6) Update the BMI Functions Part II

#### Finalize Function
We next added a `cleanup()` subroutine in `NoahMPSurfaceModule.f90`, which is called by the BMI `finalize` function. For Noah-MP-Surface, `cleanup()` closes the output file and that's it.

#### Model Time Functions
The next five BMI functions (`get_start_time` through `get_time_units`) all deal with model time. Fortran BMI functions consider start, end, and current time as `double precision` variables, so we added a `domain%time_dbl` var to `initialize_from_file()`. The non-BMI Noah-MP-Surface tracks time with a `real` variable. 

#### Model Run (Update) Functions
After the time functions comes the implementation of the all-important `update` function, which controls the execution of Noah-MP-Surface through the `advance_in_time()` and `solve_noahmp()` subroutines in `NoahMPSurfaceModule.f90`. The former subroutine advances the integer time step by 1 and the double precision time by the change in time per time step (DT, in seconds). The latter subroutine includes all the previously driver-level code that reads in one time step of forcing data and then runs the model for one time step. For Noah-MP-Surface, this is done through the calling of various subroutines. 

The `update_until` function currently allows the execution of n integer time steps through the `update` function, but we have not yet implemented the fractional time step functionality.

#### Spatial Info Functions
The next functions (`get_var_grid` through `get_grid_nodes_per_face`) correspond to the spatial discretization of Noah-MP-Surface and its input/output variables. Because we have implemented only a simple 1D driver, we followed the BMI specification for the "scalar" spatial discretization. Functions that rely on different spatial discretizations return `BMI_FAILURE` at this point as they are not valid for our current implementation.

#### Variable Info Functions
Subsequent BMI functions deal with variable information (`get_var_type`, `get_var_units`, `get_var_itemsize`, `get_var_nbytes`). For the first two functions, we specified the Fortran type (currently all vars are `real`) and SI unit (e.g., "K" for `SFCTMP`) for each input and output variable. The latter two functions automatically compute the number of bytes each variable occupies in memory. Because we have a 1D implementation of Noah-MP-Surface presently, `get_var_itemsize` and `get_var_nbytes` return the same value. The `get_var_location` function returns where a variable is located within its spatial discretization. By default, we set this to return "node" for all vars.

#### Variable Get and Set Value Functions
The last BMI functions we implemented were `get_value` and `set_value`, which allow the user to access a specified variable's value and change it, respectively. In Fortran BMI, there are specific `get_value_*` and `set_value_*` functions for different variable types: `integer`, `float` or `real`, and `double`. These are then grouped into generic `get_value` and `set_value` procedures that can access values for the three variable types. (All current input/output vars are `real` variables, so only the `*_float` functions are implemented.) For these functions to work, we provided where in the structure of the derived `noahmp_type` they are located (e.g., "SFCTMP" can be found at `this%model%forcing%sfctmp`). We implemented these functions for all input and output vars.

#### Other Functions
We have not implemented the `get_value_ptr` or the `get_value_at_indices` and `set_value_at_indices` functions. These require variables to have the `pointer` or `target` attribute in Fortran. None of our variables have these attributes, and it would require a rewriting of the Noah-MP-Surface code to do so.

### 7) Running Noah-MP-Surface with BMI

To build and run the BMI-enabled version of Noah-MP-Surface, please see the [README](https://github.com/NOAA-OWP/noah-mp-modular/tree/main/modules/surface_bmi).

## References
Hutton, E. W. h, Piper, M. D., and Tucker, G. E.: The Basic Model Interface 2.0: A standard interface for coupling numerical models in the geosciences, J. Open Source Softw., 5, 2317, <https://doi.org/10.21105/joss.02317>, 2020.

Peckham, S. D., Hutton, E. W. H., and Norris, B.: A component-based approach to integrated modeling in the geosciences: The design of CSDMS, Comput. Geosci., 53, 3–12, <https://doi.org/10.1016/j.cageo.2012.04.002>, 2013.