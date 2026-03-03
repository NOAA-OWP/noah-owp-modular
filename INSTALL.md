# Noah-OWP-Modular Configure, Build, and Run Instructions

This release of Noah-OWP-Modular comes with example forcing data and a namelist file that specifies model options. Following the steps below will let you configure, build, and run the model with the example data. 

## Configure

Noah-OWP-Modular presently requires only one external library: [NetCDF](https://www.unidata.ucar.edu/software/netcdf/). You can install NetCDF using the link or through a package manager such as [Brew](https://brew.sh/). Once NetCDF is installed, you can configure the model. The first step is to set up a configuration file. There are currently 6 build options in the `config` directory:

- `user_build_options.cheyenne`: Cheyenne supercomputer
- `user_build_options.pgf90.linux`: Linux with pgf90 compiler, NetCDF installed via source (usr/local)
- `user_build_options.macos.gfortran`: MacOS with gfortran compiler, NetCDF installed via source (opt/local)
- `user_build_options.bigsur.gfortran`: MacOS Big Sur with gfortran compiler, NetCDF 4.8.0 installed via Brew (** this is the current tesiting environment **)
- `user_build_options.gfortran.linux`: Linux with gfortran compiler, NetCDF installed via module. The $NETCDF environmental variable is defined, such as NOAA Hera.
- 'user_build_options.gfortran.ubuntu': Ubuntu with gfortran compiler, NetCDF installed from package

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

## Logger

The Errror Warning and Trapping Systems (EWTS) has been added to this module using a logging schema. All write statements have been converted to `write_log` statements, which saves the ouptut to a log file based on the log level.

When running within the ngen framework, the log file and log level are handled programatically. When running standalone, logging is defaulted to DISABLED. 

**Running Standalone**

In order to generate log messages when running standalone, the `NGEN_EWTS_LOGGING` environment variable must be set to `ENABLED`. This is the only required environment variable . Other optional logger environment variables exist for specifying the log file full pathname and setting the log level. If the user only enables logging, the log level will be set to INFO and the filename will be created based on the user and module names. All logger setup details are written to the console when the module is run. 
```
# Case Sensitive
export NGEN_EWTS_LOGGING=ENABLED
export NGEN_LOG_FILE_PATH=<full pathname for log file>
export NOAHOWP_LOGLEVEL=<DEBUG, INFO, WARNING, SEVERE, FATAL>
```
**Log Levels**
| Level   | Description                                         | Typical Use                                   |
|---------|-----------------------------------------------------|-----------------------------------------------|
| DEBUG   | Detailed diagnostic info for development/troubleshooting. | Variable values, function entry/exit. |
| FATAL   | Critical failure that aborts or makes app unrecoverable. | Crashes, memory errors, invalid state.        |
| INFO    | General events confirming expected operations.       | Startup/shutdown, configs, task completions.  |
| SEVERE  | Significant problem; app may continue in degraded state. | Failed services, corrupted configs, data loss.|
| WARNING | Potential issue that doesn’t stop execution.         | Deprecated APIs, missing files, repeatable errors. |

Default log level is INFO. The log level is hierarchical. Setting it to INFO, will log INFO, WARNING, SEVERE and FATAL
messages.

## BMI unit tests

To run unit tests, first compile and link the test program from the main-level directory:

`make testBMI`

This produces the Noah-OWP-Modular unit test executable in the `/test` subdirectory. See the `/test/README.md` for details.

`make testBMI_clean` removes the test program as well as object files.
