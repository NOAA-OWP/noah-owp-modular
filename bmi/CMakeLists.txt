set(BMI_MODS bmi.f90
             bmi_grid.f90
             CACHE INTERNAL "")
add_library(bmi "${BMI_MODS}")
