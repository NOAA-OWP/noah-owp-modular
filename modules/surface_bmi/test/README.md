# Noah-MP-Surface BMI Unit Testing

This subdirectory includes a driver for testing the BMI implementation of the Noah-MP-Surface model. The build process for running the test is similar to the normal model. First, make sure you are in the main level of `noah-mp-modular/modules/surface_bmi` and then configure your build option:

`./configure`

Next, build the test by running:

`make testBMI`

After building the model, change into the `test` subdirectory:

`cd test`

Then, run the test: 

`./noahmp_test.exe ../run/namelist.input`

Unit test results will print to your console.

For an analysis of BMI versus non-BMI model output, please open the `analysis` subdirectory in `test`.