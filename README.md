# Modularized version of the NOAH-MP land surface model

This repo contains a refactored version of the NOAH-MP land surface model, adapted from the single-file code: <https://github.com/NCAR/noahmp/>. In order to ease readability, adaptability, and interoperability, the model has been broken out into a series of modules and data types that represent various components of model information and parameters as well as the energy and water balances.

There is currently one core version being developed from the original code (branched as of early to mid 2021):
- modules/full: The modularized model with a full set of hydrologic subroutines and components (initially excluding crop and carbon). The model driver is reformulated to use BMI function calls, and to accept compiler directives to be compatible with running within the NOAA NextGen modeling framework. In addition, a sub-surface option has been added to allow running the model with the original Noah-MP subsurface or with alternative subsurface treatments.  

Two other versions were created as part of this development but are now merged into 'full'.  These will be removed as development proceeds.
- modules/surface: All surface energy and water balance components, with a simplified hydrostatic subsurface instead of the original noah-mp subsurface
- modules/surface_bmi: The surface module with an implementation of BMI, the [Basic Model Interface](https://csdms.colorado.edu/wiki/BMI)

For further information on the modules, please see their respective directories.
