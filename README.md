# Noah-OWP-Modular

This repo contains a gridded version of the Noah-MP land surface model[1,2]. This gridded version of the OWP's Noah-MP model
was extend from from NOAA OWP's previously refactored Noah-MP column model (https://github.com/NOAA-OWP/noah-owp-modular) and therefore contains a full set of hydrologic subroutines and components (excluding crop and carbon), and Basic Model Interface (BMI) functions and compiler directives to be compatible with the NOAA-NWS Office of Water Prediction Nextgen modeling framework. 

Noah-OWP-Modular is in active development. Check back often for project updates.

## Dependencies

Noah-OWP-Modular has been tested on Unix-based systems such as MacOS and Linux. Its only dependency is NetCDF.

## Installation and Configuration

Detailed instructions on how to install, configure, and run Noah-OWP-Modular can be found in our [INSTALL](INSTALL.md) guide.

## Usage

We are currently working on detailed instructions for model setup and execution in our [Wiki](https://github.com/NOAA-OWP/noah-owp-modular/wiki). For now, you can run the example data used in our [INSTALL](INSTALL.md) guide.

## Getting help

If you have questions, concerns, bug reports, etc., please file an issue in this repository's [Issue Tracker](https://github.com/NOAA-OWP/noah-owp-modular/issues).

## Getting involved

We encourage community involvement in code development. For more info, please check out our [CONTRIBUTING](CONTRIBUTING.md) document.


----

## Open source licensing info
1. [TERMS](TERMS.md)
2. [LICENSE](LICENSE)


----

## Credits and references

1. This gridded Noah-MP code base was extended from NOAA-NWS Office of Water Prediction's previously refactored Noah-MP column model (https://github.com/NOAA-OWP/noah-owp-modular), which was refactored from from the NCAR single-file [Noah-MP source code](https://github.com/NCAR/noahmp/). The Noah-MP model was developed primarily with US Government funding and was spun out of the Noah Land Surface Model, which was originally a collaboration between the National Centers for Environmental Prediction, Oregon State University, the United States Air Force, and the NOAA Hydrologic Research Lab (HRL, now the NOAA-NWS Office of Water Prediction). 
2. The 2D driver included in this code base was extended from National Center for Atmospheric Research (NCAR) version of Noah-MP (v5.0) (https://github.com/NCAR/noahmp)
