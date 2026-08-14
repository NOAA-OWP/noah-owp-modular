cmake_minimum_required(VERSION 3.20)
project(noahowp
    VERSION 1.1.0
    DESCRIPTION "OWP Implementation of the Noah-MP land surface model"
    LANGUAGES Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)
include(CompilerFlags)

# ---- Build-type default ------------------------------------------------------
get_property(_is_multi_config GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
if(NOT _is_multi_config AND NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING
        "Build type (Debug, Release, RelWithDebInfo)" FORCE)
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS
        Debug Release RelWithDebInfo)
endif()

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# ---- Options -----------------------------------------------------------------
# PROJECT_IS_TOP_LEVEL is set by CMake >= 3.21; emulate for 3.20.
if(NOT DEFINED PROJECT_IS_TOP_LEVEL)
    if(CMAKE_SOURCE_DIR STREQUAL PROJECT_SOURCE_DIR)
        set(PROJECT_IS_TOP_LEVEL TRUE)
    else()
        set(PROJECT_IS_TOP_LEVEL FALSE)
    endif()
endif()

option(NOAHOWP_BUILD_EXECUTABLE "Build the standalone noah_owp_modular driver" ON)
option(NOAHOWP_BUILD_SHARED     "Build the BMI shared library (noahowpbmi)"     ON)
option(BUILD_TESTING            "Build tests and register with CTest" ${PROJECT_IS_TOP_LEVEL})
# Turn OFF to build against an iso_c_fortran_bmi that installs iso_c_bmi without
# exporting it, such as ngen's bundled extern/iso_c_fortran_bmi: exporting
# noahowpTargets requires every PUBLIC dependency to be in an export set.
option(NOAHOWP_INSTALL          "Generate install and package-export rules" ${PROJECT_IS_TOP_LEVEL})

# Default iso_c_fortran_bmi sibling-checkout path if present.
if(IS_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}/../iso_c_fortran_bmi")
    set(_default_iso_c_bmi "${CMAKE_CURRENT_LIST_DIR}/../iso_c_fortran_bmi")
else()
    set(_default_iso_c_bmi "")
endif()
set(ISO_C_FORTRAN_BMI_PATH "${_default_iso_c_bmi}" CACHE PATH
    "Path to iso_c_fortran_bmi source tree (fallback when not installed)")

# ---- Dependencies ------------------------------------------------------------
find_package(NetCDF REQUIRED COMPONENTS FORTRAN)

# BMI bridge: prefer installed package, else add_subdirectory, else error.
# Only the shared library needs it: it alone defines NGEN_ACTIVE, which is what
# guards bmi_noahowp.f90's use of the iso_c bindings.
if(NOAHOWP_BUILD_SHARED)
    if(NOT TARGET iso_c_fortran_bmi::iso_c_bmi)
        find_package(iso_c_fortran_bmi CONFIG QUIET)
    endif()
    if(NOT TARGET iso_c_fortran_bmi::iso_c_bmi)
        if(ISO_C_FORTRAN_BMI_PATH AND IS_DIRECTORY "${ISO_C_FORTRAN_BMI_PATH}")
            if(NOT TARGET iso_c_bmi)
                add_subdirectory("${ISO_C_FORTRAN_BMI_PATH}"
                                 "${CMAKE_BINARY_DIR}/iso_c_bmi")
            endif()
            if(NOT TARGET iso_c_fortran_bmi::iso_c_bmi)
                add_library(iso_c_fortran_bmi::iso_c_bmi ALIAS iso_c_bmi)
            endif()
        else()
            message(FATAL_ERROR
                "iso_c_fortran_bmi not found. Install it, or set "
                "ISO_C_FORTRAN_BMI_PATH to a source checkout.")
        endif()
    endif()
endif()

# ---- Source lists ------------------------------------------------------------
# Vendored third-party; listed in dependency order. See extern/fortran-messagepack/README.md.
set(NOAHOWP_MESSAGEPACK_SOURCES
    extern/fortran-messagepack/src/byte_utilities.f90
    extern/fortran-messagepack/src/messagepack_value.f90
    extern/fortran-messagepack/src/messagepack_user.f90
    extern/fortran-messagepack/src/messagepack.f90)

# MessagePack recurses through nested arrays when sizing and packing them, and a
# state payload is always nested: an array of component arrays, several of which
# hold arrays themselves.
noahowp_recursion_flag(_noahowp_recursion_flag)
if(_noahowp_recursion_flag)
    set_source_files_properties(${NOAHOWP_MESSAGEPACK_SOURCES}
        PROPERTIES COMPILE_OPTIONS "${_noahowp_recursion_flag}")
endif()

set(NOAHOWP_MODEL_SOURCES
    ${NOAHOWP_MESSAGEPACK_SOURCES}
    src/AlbedoModule.f90
    src/AtmProcessing.f90
    src/CanopyWaterModule.f90
    src/DateTimeUtilsModule.f90
    src/DomainType.f90
    src/EnergyModule.f90
    src/EnergyType.f90
    src/ErrorCheckModule.f90
    src/EtFluxModule.f90
    src/ForcingModule.f90
    src/ForcingType.f90
    src/InterceptionModule.f90
    src/LevelsType.f90
    src/LoggingModule.f90
    src/NamelistRead.f90
    src/OptionsType.f90
    src/ParametersRead.f90
    src/ParametersType.f90
    src/PrecipHeatModule.f90
    src/RunModule.f90
    src/ShortwaveRadiationModule.f90
    src/SnowLayerChange.f90
    src/SnowSoilTempModule.f90
    src/SnowWaterModule.f90
    src/SnowWaterRenew.f90
    src/SoilWaterModule.f90
    src/SoilWaterMovement.f90
    src/SoilWaterRetentionCoeff.f90
    src/StateSerialization.f90
    src/SubsurfaceRunoffModule.f90
    src/SurfaceRunoffInfiltration.f90
    src/SurfaceRunoffModule.f90
    src/ThermalPropertiesModule.f90
    src/UtilitiesModule.f90
    src/WaterModule.f90
    src/WaterType.f90)

set(NOAHOWP_BMI_SOURCES
    bmi/bmi.f90
    bmi/bmi_noahowp.f90)

set(NOAHOWP_DRIVER_SUPPORT_SOURCES
    driver/AsciiReadModule.f90
    driver/OutputModule.f90)

# Each target gets its own module directory because source lists overlap.
set(_noahowp_modroot "${CMAKE_BINARY_DIR}/modules")

# ---- Targets -----------------------------------------------------------------
set(_noahowp_install_targets "")

if(NOAHOWP_BUILD_EXECUTABLE)
    add_executable(noah_owp_modular
        ${NOAHOWP_MODEL_SOURCES}
        ${NOAHOWP_BMI_SOURCES}
        ${NOAHOWP_DRIVER_SUPPORT_SOURCES}
        driver/NoahModularDriver.f90)
    set_target_properties(noah_owp_modular PROPERTIES
        Fortran_MODULE_DIRECTORY "${_noahowp_modroot}/noah_owp_modular")
    noahowp_apply_fortran_flags(noah_owp_modular)
    target_link_libraries(noah_owp_modular PRIVATE NetCDF::FORTRAN)
    list(APPEND _noahowp_install_targets noah_owp_modular)
endif()

if(NOAHOWP_BUILD_SHARED)
    if(WIN32)
        message(FATAL_ERROR
            "NOAHOWP_BUILD_SHARED is not supported on Windows.")
    endif()
    add_library(noahowpbmi SHARED
        ${NOAHOWP_MODEL_SOURCES}
        ${NOAHOWP_BMI_SOURCES}
        ${NOAHOWP_DRIVER_SUPPORT_SOURCES})
    set_target_properties(noahowpbmi PROPERTIES
        Fortran_MODULE_DIRECTORY "${_noahowp_modroot}/noahowpbmi"
        VERSION ${PROJECT_VERSION}
        SOVERSION ${PROJECT_VERSION_MAJOR})
    target_compile_definitions(noahowpbmi PUBLIC
        BMI_ACTIVE NGEN_FORCING_ACTIVE NGEN_OUTPUT_ACTIVE NGEN_ACTIVE)
    noahowp_apply_fortran_flags(noahowpbmi)
    target_link_libraries(noahowpbmi
        PUBLIC  iso_c_fortran_bmi::iso_c_bmi
        PRIVATE NetCDF::FORTRAN)
    target_include_directories(noahowpbmi
        INTERFACE
            $<BUILD_INTERFACE:${_noahowp_modroot}/noahowpbmi>
            $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/noahowp>)
    list(APPEND _noahowp_install_targets noahowpbmi)
endif()

# ---- Tests -------------------------------------------------------------------
if(BUILD_TESTING)
    enable_testing()
    add_executable(noahowp_driver_test
        test/noahowp_driver_test.f90
        ${NOAHOWP_MODEL_SOURCES}
        ${NOAHOWP_BMI_SOURCES}
        ${NOAHOWP_DRIVER_SUPPORT_SOURCES})
    set_target_properties(noahowp_driver_test PROPERTIES
        Fortran_MODULE_DIRECTORY "${_noahowp_modroot}/noahowp_driver_test")
    target_compile_definitions(noahowp_driver_test PRIVATE BMI_ACTIVE)
    noahowp_apply_fortran_flags(noahowp_driver_test)
    target_link_libraries(noahowp_driver_test PRIVATE NetCDF::FORTRAN)

    # Pass the namelist relative to the working directory: the test reads the
    # argument into a character(len=80) buffer that an absolute path can overrun.
    add_test(NAME noahowp_driver_test
        COMMAND noahowp_driver_test namelist.input
        WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}/run")
    # NamelistRead stops with exit code 0 when it cannot open the namelist, so
    # without this the test would report success after failing to read it.
    set_tests_properties(noahowp_driver_test PROPERTIES
        FAIL_REGULAR_EXPRESSION "ERROR")

    add_executable(noahowp_serialization_test
        test/serialization_test.f90
        ${NOAHOWP_MODEL_SOURCES}
        ${NOAHOWP_BMI_SOURCES}
        ${NOAHOWP_DRIVER_SUPPORT_SOURCES})
    set_target_properties(noahowp_serialization_test PROPERTIES
        Fortran_MODULE_DIRECTORY "${_noahowp_modroot}/noahowp_serialization_test")
    target_compile_definitions(noahowp_serialization_test PRIVATE BMI_ACTIVE)
    noahowp_apply_fortran_flags(noahowp_serialization_test)
    target_link_libraries(noahowp_serialization_test PRIVATE NetCDF::FORTRAN)

    add_test(NAME noahowp_serialization_test
        COMMAND noahowp_serialization_test
        WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}/run")
    # Shares run/ with the driver test, and both write the same output file.
    set_tests_properties(noahowp_serialization_test noahowp_driver_test PROPERTIES
        RESOURCE_LOCK noahowp_run_dir)

    # Compiles only the modules UtilitiesModule needs, so it builds and runs
    # without NetCDF.
    add_executable(noahowp_datetime_test
        test/datetime_test.f90
        src/DateTimeUtilsModule.f90
        src/DomainType.f90
        src/EnergyType.f90
        src/ErrorCheckModule.f90
        src/ForcingType.f90
        src/LevelsType.f90
        src/NamelistRead.f90
        src/UtilitiesModule.f90)
    set_target_properties(noahowp_datetime_test PROPERTIES
        Fortran_MODULE_DIRECTORY "${_noahowp_modroot}/noahowp_datetime_test")
    noahowp_apply_fortran_flags(noahowp_datetime_test)

    add_test(NAME noahowp_datetime_test COMMAND noahowp_datetime_test)
endif()

# ---- Install -----------------------------------------------------------------
if(_noahowp_install_targets AND NOAHOWP_INSTALL)
    install(TARGETS ${_noahowp_install_targets}
        EXPORT  noahowpTargets
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})

    if(NOAHOWP_BUILD_SHARED)
        install(DIRECTORY "${_noahowp_modroot}/noahowpbmi/"
            DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/noahowp
            FILES_MATCHING PATTERN "*.mod")
    endif()

    install(EXPORT noahowpTargets
        FILE        noahowpTargets.cmake
        NAMESPACE   noahowp::
        DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/noahowp)

    install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/cmake/FindNetCDF.cmake"
        DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/noahowp)

    configure_package_config_file(
        "${CMAKE_CURRENT_SOURCE_DIR}/cmake/noahowpConfig.cmake.in"
        "${CMAKE_CURRENT_BINARY_DIR}/noahowpConfig.cmake"
        INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/noahowp)
    write_basic_package_version_file(
        "${CMAKE_CURRENT_BINARY_DIR}/noahowpConfigVersion.cmake"
        VERSION ${PROJECT_VERSION}
        COMPATIBILITY SameMajorVersion)
    install(FILES
        "${CMAKE_CURRENT_BINARY_DIR}/noahowpConfig.cmake"
        "${CMAKE_CURRENT_BINARY_DIR}/noahowpConfigVersion.cmake"
        DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/noahowp)

    configure_file(
        "${CMAKE_CURRENT_SOURCE_DIR}/cmake/noahowp.pc.in"
        "${CMAKE_CURRENT_BINARY_DIR}/noahowp.pc" @ONLY)
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/noahowp.pc"
        DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig)
endif()
