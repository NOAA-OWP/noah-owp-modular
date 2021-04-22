# Modular NOAH-MP

This directory contains the in-progress work on a modularized version of NOAH-MP. There are two important changes:

1. The single, monolith block of code is split into a series of individual modules.
2. Data types are now used to simplify subroutine calls and arguments, and to facilitate the addition of future modules.

Currently, the model runs using synthetic forcing data as specified in the driver.

# Setup

Modular NOAH-MP presently requires only one external library: [NetCDF](https://www.unidata.ucar.edu/software/netcdf/). You can install NetCDF using the link or through a package manager such as [Brew](https://brew.sh/). Once NetCDF is installed, you can build the model. The first step is to set up a configuration file. There are currently 4 build options in the `config` directory:

- `user_build_options.cheyenne`: Cheyenne supercomputer
- `user_build_options.keith.gfortran`: MacOS Big Sur with gfortran compiler, NetCDF installed via Brew
- `user_build_options.keith.gfortran`: MacOS with gfortran compiler, NetCDF installed via source (opt/local)
- `user_build_options.keith.gfortran`: Linux with pgf90 compiler, NetCDF installed via source (usr/local)

If your system does not match one of the above options, you'll need to edit one of the files or create your own. If you do the latter, you'll need to add another option to the `configure` Perl script.

Once you have a a `user_build_options` set, go to your terminal and run the following command from the `main` directory:

`./configure` 

Then enter the number matching your `user_build_options`. This copies over the correct set of options to the `user_build_options` file in the `main` directory.

Next, compile the model from the `main` directory:

`make`

After the model is finished compiling and linking, you can change into the `run` directory and run the model:

```cd run/
./snow_refac.exe
```

You can examine model output in the `output.nc` file (requires [Panoply](https://www.giss.nasa.gov/tools/panoply/) or other NetCDF viewer).