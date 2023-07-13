# Noah-OWP-Modular

This repo contains an extended, refactored version of the Noah-MP community land surface model, adapted from the [single-file source code](https://github.com/NCAR/noahmp/tree/release-v4.1-NWMv2.1/) maintained and developed by NCAR. In order to ease readability, adaptability, and interoperability, the model has been broken out into a series of modules and data types that represent various components of model information and parameters as well as the energy and water balances.

There is currently one core version being developed from the original code (branched as of early to mid 2021). This is the modularized model with a full set of hydrologic subroutines and components (initially excluding crop and carbon). The model driver is reformulated to use function calls from the [Basic Model Interface](), and to accept compiler directives to be compatible with running within the NOAA-NWS Office of Water Prediction [Next Generation Water Resources Modeling Framework](). In addition, a subsurface option has been added to allow running the model with the original Noah-MP subsurface or with alternative subsurface treatments.  

This branch includes prototype code for a gridded implementation of Noah-OWP-Modular. The model is in active development. Check back often for project updates.

## Dependencies

Noah-OWP-Modular has been tested on Unix-based systems such as MacOS and Linux. Its only dependency is NetCDF Fortran.

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

1. This modularized code base was developed from the single-file [Noah-MP source code](https://github.com/NCAR/noahmp/tree/release-v4.1-NWMv2.1/). Noah-MP was developed primarily with US Government funding and was spun out of the Noah Land Surface Model, which was originally a collaboration between the National Centers for Environmental Prediction, Oregon State University, the United States Air Force, and the NOAA Hydrologic Research Lab (HRL, now the NOAA-NWS Office of Water Prediction). 
2. The simple 1D driver was developed from the [Noah-MP 1.1 driver](https://ral.ucar.edu/solutions/products/noah-multiparameterization-land-surface-model-noah-mp-lsm).
3. The 2D driver included in this code base was extended from National Center for Atmospheric Research (NCAR) version of Noah-MP (v5.0) (https://github.com/NCAR/noahmp)
