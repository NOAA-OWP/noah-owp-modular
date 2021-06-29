# Modularized version of the NOAH-MP land surface model

This repo contains a modularized version of the NOAH-MP land surface model, adapted from the single-file code: <https://github.com/NCAR/noahmp/>. In order to ease readability, adaptability, and interoperability, the model has been broken out into a series of modules and data types that represent various components of model information and parameters as well as the energy and water balances.

There are currently three modules that have been developed from the original code base:

- modules/main: The complete modularized NOAH-MP model
- modules/surface: All surface energy and water balance components, plus a simplified hydrostatic subsurface treatment
- modules/surface_bmi: The surface module with an implementation of BMI, the (Basic Model Interface)[https://csdms.colorado.edu/wiki/BMI]

For further information on the modules, please see their respective directories.