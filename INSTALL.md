# Noah-OWP-Modular Configure, Build, and Run Instructions

Noah-OWP-Modular uses CMake (>= 3.20). Its only runtime dependency is
[NetCDF](https://www.unidata.ucar.edu/software/netcdf/) with the Fortran
interface (`libnetcdff`). Install NetCDF via your package manager
(`brew install netcdf-fortran`, `apt install libnetcdff-dev`, etc.) or
point CMake at a custom prefix with `-DNetCDF_ROOT=/path/to/netcdf`.

## Build

From the repository root:

```
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build -j
```

The standalone executable lands at `build/noah_owp_modular` and the BMI
shared library at `build/libnoahowpbmi.*`.

### CMake options

| Option | Default | Meaning |
|---|---|---|
| `CMAKE_BUILD_TYPE`         | `Release`                       | `Debug`, `Release`, or `RelWithDebInfo`. |
| `NOAHOWP_BUILD_EXECUTABLE` | `ON`                            | Build the standalone `noah_owp_modular` driver. |
| `NOAHOWP_BUILD_SHARED`     | `ON`                            | Build the `noahowpbmi` shared library (requires `iso_c_fortran_bmi`). |
| `BUILD_TESTING`            | `ON` when top-level, else `OFF` | Build the BMI driver and date/time tests and register them with CTest. |
| `NOAHOWP_INSTALL`          | `ON` when top-level, else `OFF` | Generate install and package-export rules. See below. |
| `ISO_C_FORTRAN_BMI_PATH`   | sibling `../iso_c_fortran_bmi`  | Source checkout of iso_c_fortran_bmi, used when it isn't installed. |

`NOAHOWP_INSTALL=OFF` is needed to build the shared library against a copy of
iso_c_fortran_bmi that installs `iso_c_bmi` without exporting it, such as the
one bundled at ngen's `extern/iso_c_fortran_bmi`. Exporting `noahowpTargets`
requires every `PUBLIC` dependency to belong to an export set, so the default
`ON` fails at the generate step with:

```
install(EXPORT "noahowpTargets" ...) includes target "noahowpbmi" which
requires target "iso_c_bmi" that is not in any export set.
```

A standalone checkout of iso_c_fortran_bmi exports its targets, so it needs no
such workaround.

### HPC / non-standard NetCDF

Point the find module at the right prefix:

```
cmake -S . -B build \
      -DCMAKE_BUILD_TYPE=Release \
      -DNetCDF_ROOT=$NETCDF
```

### Compiler flags

Flags are set per compiler and per config:

| Compiler       | Debug                                                                                     | Release                                               |
|----------------|-------------------------------------------------------------------------------------------|-------------------------------------------------------|
| GNU            | `-g -fbacktrace -Wall -fcheck=all -ffree-line-length-none -frounding-math -fno-fast-math -cpp` | `-O2 -ffree-line-length-none -frounding-math -fno-fast-math -cpp` |
| Intel/IntelLLVM| `-g -traceback -check all -warn all -fp-model=strict -fpp`                                | `-O2 -fp-model=strict -fpp`                           |
| NVIDIA/NVHPC   | `-g -traceback -Mbounds -Mchkptr -Kieee -Mbackslash -Mpreprocess`                          | `-O2 -Kieee -Mbackslash -Mpreprocess`                 |

## Run

```
cd run
../build/noah_owp_modular namelist.input
```

Output lands at `data/output.nc` (view with Panoply or any NetCDF viewer).

## Tests

```
ctest --test-dir build --output-on-failure
```

## Install

```
cmake --install build --prefix /your/prefix
```

Downstream projects consume via either:

```
find_package(noahowp CONFIG REQUIRED)
target_link_libraries(your_target PRIVATE noahowp::noahowpbmi)
```

or pkg-config:

```
PKG_CONFIG_PATH=/your/prefix/lib/pkgconfig pkg-config --libs --cflags noahowp
```
