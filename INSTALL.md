# Noah-OWP-Modular Configure, Build, and Run Instructions

This release of Noah-OWP-Modular comes with example forcing data and a namelist file that specifies model options. Following the steps below will let you configure, build, and run the model with the example data. 

## Configure

Noah-OWP-Modular presently requires only one external library: [NetCDF](https://www.unidata.ucar.edu/software/netcdf/). You can install NetCDF using the link or through a package manager such as [Brew](https://brew.sh/). Once NetCDF is installed, you can configure the model. The first step is to set up a configuration file. There are currently 4 build options in the `config` directory:

- `user_build_options.cheyenne`: Cheyenne supercomputer
- `user_build_options.pgf90.linux`: Linux with pgf90 compiler, NetCDF installed via source (usr/local)
- `user_build_options.macos.gfortran`: MacOS with gfortran compiler, NetCDF installed via source (opt/local)
- `user_build_options.bigsur.gfortran`: MacOS Big Sur with gfortran compiler, NetCDF 4.8.0 installed via Brew (** this is the current tesiting environment **)

If your system does not match one of the above options, you'll need to edit one of the files or create your own. If you do the latter, you'll need to add another option to the `configure` Perl script.

Once you have a `user_build_options` set, go to your terminal and run the following command from the main Noah-OWP-Modular directory:

`./configure` 

Then enter the number matching your `user_build_options`. This copies over the correct set of options to the `user_build_options` file.

## Build

Next, compile and link the model from the main-level directory:

`make`

This produces the Noah-OWP-Modular executable in the `/run` subdirectory.

## Run

After the model is finished compiling and linking, you can change into the `/run` subdirectory and run the model:

```
cd run/
./noah_owp_modular.exe namelist.input
```

The `namelist.input` file in `/run` includes all the setup and options info you need to run Noah-OWP-Modular. This is the file you'll modify when running Noah-OWP-Modular in different locations.

You can examine model output in the `/data/output.nc` file (requires [Panoply](https://www.giss.nasa.gov/tools/panoply/) or other NetCDF viewer).
