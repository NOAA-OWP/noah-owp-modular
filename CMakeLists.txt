# Usage: from noah-owp-modular directory type:
# mkdir ../build
# cd ../build
# cmake ../noah-owp-modular
# cmake --build . -- VERBOSE=1
# cd ../noah-owp-modular/run
# ../../build/noah-owp-modular.exe namelist.input
# cd ../test
# ../../build/noahowp_driver_test.exe ../run/namelist.input

##################################################
# Define the project and the depencies that it has
##################################################

cmake_minimum_required(VERSION 3.28)

project(noah-owp-modular VERSION 0.1
        DESCRIPTION "Noah OWP Modular"
        LANGUAGES Fortran)

# Add NetCDF Libraries
include(${PROJECT_SOURCE_DIR}/FindNetCDF.cmake)
set (NETCDF_F90 "YES")
find_package (NetCDF REQUIRED)
cmake_path(GET NETCDF_LIBRARIES_F90 PARENT_PATH NETCDF_LIB_DIR)
set(NETCDF_INC_DIR ${NETCDF_INCLUDES})
message(NETCDF_INC_DIR:"${NETCDF_INC_DIR}")
message(NETCDF_LIB_DIR:"${NETCDF_LIB_DIR}")
message(CMAKE_SOURCE_DIR:"${CMAKE_SOURCE_DIR}")
message(CMAKE_BINARY_DIR:"${CMAKE_BINARY_DIR}")

# Require that Fortran 90 is supported
if(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)
    message(FATAL_ERROR "Fortran compiler does not support F90")
endif(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)

# Set compiler options. You may need to uncomment/comment appropriate lines.
if(CMAKE_Fortran_COMPILER_ID STREQUAL "Absoft")
  message(FATAL_ERROR "Absoft Fortran compiler not configured.")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "CCur")
  message(FATAL_ERROR "Concurrent Fortran compiler not configured.")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Flang")
  message(FATAL_ERROR "Classic Flang Fortran compiler not configured.")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "G95")
  message(FATAL_ERROR "G95 Fortran compiler not configured.")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  # Mac OS Gfortran F90FLAGS and FREESOURCE flags
  add_compile_options("$<$<COMPILE_LANGUAGE:Fortran>:-cpp;-fbacktrace;-fcheck=all>")
  add_compile_options("$<$<COMPILE_LANGUAGE:Fortran>:-ffree-form;-ffree-line-length-none;-fbounds-check;-ffpe-trap=overflow,invalid,zero>")
  # Linux Gfortran F90FLAGS and FREESOURCE flags
  #add_compile_options("$<$<COMPILE_LANGUAGE:Fortran>:-cpp;-fbacktrace;-fcheck=all>")
  #add_compile_options("$<$<COMPILE_LANGUAGE:Fortran>:-ffree-form;-ffree-line-length-none>")
  # Big Sur Gfortran F90FLAGS and FREESOURCE flags
  #add_compile_options("$<$<COMPILE_LANGUAGE:Fortran>:-cpp;-fbacktrace;-fcheck=all>")
  #add_compile_options("$<$<COMPILE_LANGUAGE:Fortran>:-ffree-form;-ffree-line-length-none>")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
  add_compile_options("$<$<COMPILE_LANGUAGE:Fortran>:-check;-warn;-traceback>")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "LCC")
  message(FATAL_ERROR "MCST Elbrus C/C++/Fortran compiler not configured.")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "LLVMFlang")
  message(FATAL_ERROR "LLVM Flang Fortran compiler not configured.")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "PGI")
  # Linux pgf90 F90FLAGS and FREESOURCE flags
  add_compile_options("$<$<COMPILE_LANGUAGE:Fortran>:-cpp;-Kieee>")
  add_compile_options("$<$<COMPILE_LANGUAGE:Fortran>:-Mfree>")
  # Cheyenne pgf90 F90FLAGS and FREESOURCE flags
  add_compile_options("$<$<COMPILE_LANGUAGE:Fortran>:-cpp;-Kieee;-byteswapio;-Mbackslash>")
  add_compile_options("$<$<COMPILE_LANGUAGE:Fortran>:-Mfree>")
endif()

# Set some options the user may choose
# Uncomment the below if you want the user to choose a parallelization library
#option(USE_MPI "Use the MPI library for parallelization" OFF)
#option(USE_OPENMP "Use OpenMP for parallelization" OFF)

# This INCLUDE statement executes code that sets the compile flags for DEBUG,
# RELEASE, and TESTING.  You should  review this file and make sure the flags 
# are to your liking.
#include(${CMAKE_MODULE_PATH}/SetFortranFlags.cmake) 

# Add NGEN_ACTIVE option
#option(NGEN_ACTIVE "Enter mode" OFF)
#if (NGEN_ACTIVE)
#  add_definitions(-DNGEN_ACTIVE)
#endif()

# Locate and set parallelization libraries.  There are some CMake peculiarities
# taken care of here, such as the fact that the FindOpenMP routine doesn't know
# about Fortran.
#include(${CMAKE_MODULE_PATH}/SetParallelizationLibrary.cmake)

# There is an error in CMAKE with this flag for pgf90.  Unset it
#get_filename_component(FCNAME ${CMAKE_Fortran_COMPILER} NAME)
#if(FCNAME STREQUAL "pgf90")
#    unset(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS)
#endif(FCNAME STREQUAL "pgf90")


############################################################
# Define the actual files and folders that make up the build
############################################################

# Define some directories
set(src ${CMAKE_SOURCE_DIR}/src)
set(lib ${CMAKE_BINARY_DIR}/lib)
set(bin ${CMAKE_BINARY_DIR}/bin)

############################################################
# Build the targets
############################################################

add_executable(noah-owp-modular.exe "${PROJECT_SOURCE_DIR}/driver/NoahModularDriver.f90")
add_executable(noahowp_driver_test.exe "${PROJECT_SOURCE_DIR}/test/noahowp_driver_test.f90")
include_directories(bmi src ${lib})
target_link_libraries(noah-owp-modular.exe bmi src ${NETCDF_LIBRARIES})
target_link_libraries(noahowp_driver_test.exe bmi src ${NETCDF_LIBRARIES})
add_compile_options("$<$<COMPILE_LANGUAGE:Fortran>:-I ${NETCDF_INC_DIR};-L ${NETCDF_LIB_DIR} -lnetcdff>")

# Have the .mod files placed in the lib folder
set(CMAKE_Fortran_MODULE_DIRECTORY ${lib})

# Add source have compiled files placed in the bin folder
add_subdirectory(${src} ${bin})
add_subdirectory(bmi)
# Add a distclean target to the Makefile
#add_custom_target(distclean 
#    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_SOURCE_DIR}/distclean.cmake
