# NOAH-MP surface module with BMI

This subdirectory contains the NOAH-MP surface module, adapted from the complete modular NOAH-MP. The key difference is that the NOAH-MP subsurface routines for runoff partitioning, groundwater, soil moisture, etc. have been removed in favor of a simple hydrostatic subsurface treatment. This allows for the computation of the stomatal and surface resistances that are used in calculating evapotranspiration. Although the surface module can be run in standalone mode, it is specifically designed to be coupled with a subsurface module. 

The current subsurface treatment includes the following assumptions:

1. Soil moisture content is constant
2. The water table height is constant

When coupled to a subsurface module, the main variable of interest is QINSUR, which is the total liquid water flux at the land surface. 

# The BMI implementation

BMI stands for the Basic Model Interface, a standard set of functions developed by researchers at [CSDMS](https://csdms.colorado.edu/wiki/BMI). The BMI implementation in the NOAH-MP surface module provides simplified model control functions (e.g., initialize, update, finalize) and allows for data to be passed into and out of model for the following variables:

- Input var = SFCPRS (surface pressure, Pa)
- Input var = SFCTMP (surface air temperature, K)
- Input var = SOLDN (incoming shortwave radiation, W m<sup>-2</sup>)
- Input var = LWDN (incoming longwave radiation, W m<sup>-2</sup>)
- Input var = UU (wind speed in the eastward direction, m s<sup>-1</sup>)
- Input var = VV (wind speed in the northward direction, m s<sup>-1</sup>)
- Input var = Q2 (mixing ratio, kg kg<sup>-1</sup>)
- Input var = PRCPNONC (precipitation, mm s<sup>-1</sup>)
- Output var = QINSUR (surface water input, m s<sup>-1</sup>)
- Output var = ETRAN (transpiration, mm s<sup>-1</sup>)
- Output var = QSEVA (evaporation, mm s<sup>-1</sup>)

Further details on BMI can be found on the previously linked CSDMS page.

# Building the model

The NOAH-MP surface module with BMI presently requires only one external library: [NetCDF](https://www.unidata.ucar.edu/software/netcdf/). You can install NetCDF using the link or through a package manager such as [Brew](https://brew.sh/). Once NetCDF is installed, you can build the model. The first step is to set up a configuration file. There are currently 4 build options in the `config` directory:

- `user_build_options.cheyenne`: Cheyenne supercomputer
- `user_build_options.pgf90.linux`: Linux with pgf90 compiler, NetCDF installed via source (usr/local)
- `user_build_options.macos.gfortran`: MacOS with gfortran compiler, NetCDF installed via source (opt/local)
- `user_build_options.bigsur.gfortran`: MacOS Big Sur with gfortran compiler, NetCDF installed via Brew (** this is the current tesiting environment **)

If your system does not match one of the above options, you'll need to edit one of the files or create your own. If you do the latter, you'll need to add another option to the `configure` Perl script.

Once you have a a `user_build_options` set, go to your terminal and run the following command from the `main` directory:

`./configure` 

Then enter the number matching your `user_build_options`. This copies over the correct set of options to the `user_build_options` file in the `main` directory.

Next, compile and link the model from the `main` directory:

`make`

# Running the model

After the model is finished compiling and linking, you can change into the `run` directory and run the model:

```
cd run/
./noahmp_refac.exe namelist.input
```

You can examine model output in the `output.nc` file (requires [Panoply](https://www.giss.nasa.gov/tools/panoply/) or other NetCDF viewer).

Note: The model is currently set up to run using the `namelist.input` configuration file as specified in the second command line argument and the `bondville.dat` forcing data identified in `namelist.input`.