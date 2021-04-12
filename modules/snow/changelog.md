# Changelog for NOAH-MP modularized snow model

Changes are given in both reference to 

## Major changes:
- Removed PGS from PHENOLOGY because we’re not implementing CARBON_CROP
- Moved CanopyWaterIntercept to InterceptionModule
    - Removed call from WaterModule and added to InterceptionModule
- Kept heat advected by precipitation calculations in PRECIP_HEAT (now called by EnergyModule)
    - Moved interception and throughfall calculations from old PRECIP_HEAT into CanopyWaterIntercept in the InterceptionModule
- Removed TROOT computation from main level of NOAHMP_SFLX
    - TROOT only used in CARBON, which we are not implementing
    - Can easily be added if wanted
- Moved computation of DZSNSO from main level of NOAHMP_SFLX to THERMOPROP
- Moved setting of VEGE_FLUX output to 0 from top of ENERGY to IF ELSE statement that includes call to VEGE_FLUX
- Moved computation of UR (wind speed at reference height to ATM)
    - Not sure why it wasn’t there in the first place
- ZREF, used in ENERGY to compute ZLVL was defined but NOT initialized in original monolith code
    - ZREF is now a part of namelist.input, where the user can define it
    - ZREF is transferred to DomainType
- Removed CWP = parameters%CWPVT from ENERGY main level
    - Now define CWP as a parameter (parameter%CWP)
    - CWPVT was formerly given per VEGTYPE, but it was a single value (0.18)
- Changed SOIL and SNOW back NSOIL and NSNOW, respectively
    - Not sure why NCAR changed them in the first place. I find the N qualifier to be quite useful.
    - The new SNOW (referring to NSNOW, the number of snow layers) also conflicted with SNOW (referring to the liquid depth of snowfall per timestep)
- Moved do loop from main level of THERMOPROP to within TDFCND
    - This is more consistent with looping in NOAH- MP
- Moved THKW, THKO, and THKQTZ from locally defined in TDFCND to ParametersType
- QUARTZ given its own line in namelist.input
    - Then moved to parameters based on 
- Moved assignment of energy%DF and HCPCT to CSNOW from THERMOPROP
    - Changed multiple do- loops to single do- loop in CSNOW

## Major notes (potential to change):
- Consider a “point scale” or “small scale” mode that assumes the point or grid cell is homogeneous
    - Sets FP (fraction of grid cell receiving precipitation) to 1
    - Sets FSNO (fractional snow covered area) to 1
    - Sets FVEG (fraction of grid cell with vegetation) to 1 for VEGTYPES such as evergreen, deciduous, etc.
- Precipitation phase partitioning in ATM
    - Currently uses 4 options:
        - OPT_SNF == 1: SNTHERM method
        - OPT_SNF == 2: Air temperature threshold of 2.2°C
        - OPT_SNF == 3: Air temperature threshold of 0°C
        - OPT_SNF == 4: Phase from weather model (e.g. WRF)
    - We need to update these:
        - At least one method incorporating humidity
        - A dual- threshold range
        - Combine options 2 and 3 to use a flexible threshold (i.e. rain_snow_threshold becomes a user- settable parameter)
- Check hard- coded values in ENERGY main level
    - ZOMG = 0.01
    - 0.65 * parameters%HVT
    - Where do these values come from?
        - Should they be parameters
- Where is IST (ground = 1, lake = 2) instantiated?
- The current code initializes model states from the namelist
    - Maybe we could move to a separate file, similar to what’s available in SNOWPACK?
- Removed commented out options for CVSNO and TKSNO from CSNOW
    - Add back in as options at some point?
    -   !      CVSNO(IZ) = 0.525E06                          ! constant
    -   !    TKSNO(IZ) = 2E- 2+2.5E- 6*BDSNOI(IZ)*BDSNOI(IZ)   ! Anderson, 1976
    -   !    TKSNO(IZ) = 0.35                                ! constant
    -   !    TKSNO(IZ) = 2.576E- 6*BDSNOI(IZ)**2. + 0.074    ! Verseghy (1991)
    -   !    TKSNO(IZ) = 2.22*(BDSNOI(IZ)/1000.)**1.88      ! Douvill(Yen, 1981)
- THERMOPROP uses local variables TKSNO and TVSNO before copying them over to globals DF and HCPCT. Can we just skip middleman and compute DF and HCPCT directly?


## Hard-coded parameters that need to be incorporated in a driver or forcing module:

- DomainType
    - CROPTYPE
        - Currently included as a parameter in namelist.input under “structure”
        - This means if the model were to be run as- is over a grid, every cell would have the prescribed CROPTYPE from the namelist
    - VEGTYP
        - Currently included as a parameter in namelist.input under “structure”
        - This means if the model were to be run as- is over a grid, every cell would have the prescribed VEGTYP from the namelist
        - HRLDAS uses NETCDF to read in land cover from a grid:
- Lines 488–490 in module_hrldas_netcdf_io.F
    - LAT and LON
        - Model is set up to run at a single point
        - LAT and LON are given in the namelist.input file under “location”
        - They are then read in using NamelistRead and transferred to the DomainType

- ForcingType
    - JULIAN
        - Currently hardcoded upon initialization in the snow_driver file
        - Need to add a date utility to process the forcing date into JULIAN
- See HRLDAS for info (module_date_utilities)
    - YEARLEN
        - Currently hardcoded upon initialization in the snow_driver file
        - Need to add a date utility to process the forcing date into JULIAN
- See HRLDAS for info (module_date_utilities)

- Namelist and Parameter types
    - Model is only setup to read in MODIS land cover categories, NOT USGS
        - This follows the lead of what Cenlin had set up but should be adapted to incorporate USGS land cover codes
    - FVEG computation pulled from main level of noahmp_sflx and moved into PHENOLOGY
        - SHDFAC values from an old NOAH- MP veg param table (couldn’t find in HRLDAS version)
        - SHDMAX is implemented as a hack currently
- It looks like HRLDAS can read this as gvfmax from land cover data, but implementation is not straightforward
- SHDMAX values are in namelist and are identical to SHDFAC
        - Removed OPT_CROP from FVEG computation because it doesn’t seem to be implemented elsewhere important


## BMI notes for future:

- Can add update_* functions to each module 

