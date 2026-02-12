cmake_minimum_required(VERSION 3.12)
project(noah-owp-modular VERSION 1.0.1 DESCRIPTION "OWP Implementation of the Noah-MP land surface model")

enable_language( Fortran )
include(CMakeDependentOption)

# Do this so we can default to ../iso_c_fortran_bmi for bindings if that directory exists
if (IS_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}/../iso_c_fortran_bmi)
    set(DEFAULT_ISO_C_BMI_DIR_VAL ${CMAKE_CURRENT_LIST_DIR}/../iso_c_fortran_bmi)
else ()
    set(DEFAULT_ISO_C_BMI_DIR_VAL OFF)
endif ()

option(ISO_C_FORTRAN_BMI_PATH  "Path to NextGen middleware for ISO C Fortran BMI bindings" ${DEFAULT_ISO_C_BMI_DIR_VAL})

# Syntax: cmake_dependent_option(<option> "<help_text>" <value> <depends> <force>)
cmake_dependent_option(BMI_SHARED_LIB "Support for building BMI module shared library" ON "ISO_C_FORTRAN_BMI_PATH" OFF)

if (${BMI_SHARED_LIB} AND NOT IS_DIRECTORY ${ISO_C_FORTRAN_BMI_PATH})
    message(FATAL_ERROR "Provided CMake option 'ISO_C_FORTRAN_BMI_PATH' is not a valid directory!")
endif ()

# TODO: (later) need to figure out the proper analogous compiler settings for -Wall in Intel and nv compilers
if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel|IntelLLVM")
    add_compile_options(-cpp -fp-model=strict)
elseif ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
    add_compile_options(-cpp -Wall -fcheck=bounds -ffree-line-length-none -frounding-math -fno-fast-math)
elseif ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "NVIDIA" OR "${CMAKE_Fortran_COMPILER_ID}" MATCHES "NVHPC")
        add_compile_options(${SAC_LIB_NAME_CMAKE} -Kieee -Mbackslash -Mpreprocess)
endif ()


#### Add variables for individual libraries that are used within the *.pc.in files
# NOM = Noah-OWP-Modular
set(NOM_LIB_NAME_CMAKE noah_owp_modular_bmi)
set(NOM_STANDALONE_EXE_NAME noah_owp_modular.exe)
set(NOM_LIB_DESC_CMAKE "External Noah-OWP-Modular Shared Library")
set(BMI_SOURCES_DIR bmi/)
set(DRIVER_SOURCE_DIR driver/)
set(MODEL_SOURCE_DIR src/)
set(RUN_SOURCE_DIR run/)

# Create variables with the source files needed in different contexts
set(BMI_SOURCES "bmi/bmi.f90" "bmi/bmi_noahowp.f90")
set(DRIVER_SOURCES "driver/AsciiReadModule.f90" "driver/NoahModularDriver.f90" "driver/OutputModule.f90")
set(MODEL_SOURCES "src/AlbedoModule.f90" "src/AtmProcessing.f90" "src/CanopyWaterModule.f90" 
    "src/DateTimeUtilsModule.f90" "src/DomainType.f90" "src/EnergyModule.f90" 
    "src/EnergyType.f90" "src/ErrorCheckModule.f90" "src/EtFluxModule.f90"
    "src/ForcingModule.f90" "src/ForcingType.f90" "src/InterceptionModule.f90"
    "src/LevelsType.f90" "src/NamelistRead.f90" "src/OptionsType.f90"
    "src/ParametersRead.f90" "src/ParametersType.f90" "src/PrecipHeatModule.f90"
    "src/RunModule.f90" "src/ShortwaveRadiationModule.f90" "src/SnowLayerChange.f90"
    "src/SnowSoilTempModule.f90" "src/SnowWaterModule.f90" "src/SnowWaterRenew.f90"
    "src/SoilWaterModule.f90" "src/SoilWaterMovement.f90" "src/SoilWaterRetentionCoeff.f90"
    "src/SubsurfaceRunoffModule.f90" "src/SurfaceRunoffInfiltration.f90"
    "src/SurfaceRunoffModule.f90" "src/ThermalPropertiesModule.f90" "src/UtilitiesModule.f90"
    "src/WaterModule.f90" "src/WaterType.f90"
)

if (BMI_SHARED_LIB)
    if (WIN32)
       message(FATAL_ERROR "Currently shared library target is not supported on Windows platforms.")
    else()
        add_subdirectory(${ISO_C_FORTRAN_BMI_PATH} ${CMAKE_BINARY_DIR}/iso_c_bmi)
        add_library(${NOM_LIB_NAME_CMAKE} SHARED ${BMI_SOURCES} ${MODEL_SOURCES} ${DRIVER_SOURCES})
        target_compile_definitions(${NOM_LIB_NAME_CMAKE} PUBLIC BMI_ACTIVE NGEN_FORCING_ACTIVE NGEN_OUTPUT_ACTIVE NGEN_ACTIVE)
	target_compile_options(${NOM_LIB_NAME_CMAKE} PUBLIC -fPIC)
        target_link_libraries(${NOM_LIB_NAME_CMAKE} PUBLIC iso_c_bmi)
        set_target_properties(${NOM_LIB_NAME_CMAKE} PROPERTIES VERSION ${PROJECT_VERSION})
    endif()
else()
    add_executable(${NOM_STANDALONE_EXE_NAME} ${MODEL_SOURCES} ${BMI_SOURCES} ${DRIVER_SOURCES})
    #find_package(NetCDF COMPONENTS F90)
    find_library(NETCDF_FORTRAN_LIB netcdff REQUIRED)
    find_path(NETCDF_MOD_PATH "netcdf.mod" REQUIRED)
    target_compile_options(${NOM_STANDALONE_EXE_NAME} PUBLIC -fPIC -I${NETCDF_MOD_PATH})
    #target_link_options(${NOM_STANDALONE_EXE_NAME} PRIVATE -L${NETCDF_LIB_PATH}} -lnetcdff)
    target_link_libraries(${NOM_STANDALONE_EXE_NAME} ${NETCDF_FORTRAN_LIB})
endif()
