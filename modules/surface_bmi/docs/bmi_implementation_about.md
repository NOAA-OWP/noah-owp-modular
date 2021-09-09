# Implementing BMI into Noah-MP-Surface

As of 2021-09-08, we have implemented BMI into the Noah-MP-Surface module which combines the Noah-MP water and energy balance routines from the top of the canopy to the soil surface with a simple hydrostatic subsurface treatment (for testing purposes only). In this form, Noah-MP-Surface is designed to be coupled to a subsurface module that handles runoff and vadose zone processes.

## BMI Basics

The [Basic Model Interface](https://csdms.colorado.edu/wiki/BMI) (BMI) is a set of functions that control model initialization, operation, and finalization, while also improving model interoperability. Developed by researchers at the Community Surface Dynamics Modeling System ([CSDMS](https://csdms.colorado.edu/wiki/Main_Page)), BMI standardizes and mediates the exchange of information across different modeling components, which may have different data requirements and time-space resolutions (Hutton et al., 2020; Peckham et al., 2013).

In the Nextgen system, we implement BMI into hydrologic models and modules so that the framework can control model runtime and pass data from one formulation to another through the model engine. The implementation of BMI into a BMI-compliant model is relatively straightforward. The steps below are specific to how we created `modules/surface_bmi` from `modules/surface` for Noah-MP-Surface, but they can be applied to other Fortran models. 

## Step-by-Step BMI Implementation

### 0) Requirements

To run the BMI-enabled version of Noah-MP-Surface, your system needs:

  * A Fortran compiler
    * We tested the implementation on Mac OS Big Sur using GCC (gfortran) 11.2.0
  * NetCDF

Our implementation of BMI bundles the BMI source code with Noah-MP-Surface. If you wish to install BMI as a library so it can be accessed by other models, you will need follow the [instructions](https://github.com/csdms/bmi-fortran#buildinstall) from CSDMS and have Cmake on your system.

### 1) Start with Original Codebase

A key tenet of BMI is that it does not affect the underlying model source code, meaning BMI does not change the way a given model simulates the hydrologic balance. Therefore, to start our BMI implementation, we copied over `modules/surface` to `modules/surface_bmi`.

### 2) Add the BMI Source Code

Next, we added a subdirectory to `modules/surface_bmi` called `bmi`. We then copied the [Fortran 2003 BMI specification](https://github.com/csdms/bmi-fortran/blob/master/bmi.f90) from CSDMS into our `bmi` folder. Our project directory structure now looks like this:

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




### 3) 