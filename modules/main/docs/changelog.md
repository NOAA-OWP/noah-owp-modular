# Changelog for NOAH-MP modularized snow model

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
- RADIATION is now called ShortwaveRadiationMain (it does not compute longwave, so name was changed accordingly)
- Moved local-definition of NBAND in ALBEDO to global as parameter type
- Removed redundant SWE-tracking variable SNEQVO from SNOW_AGE and now use QSNOW * dt to calculate new snowfall relative to SWEMX (the amount of snowfall needed to refresh albedo)
- Added SOILCOLOR as a user-settable parameter in namelist.read
    - SOILCOLOR is hard-coded as 4 in module_sf_noahmpdrv.F in the current release of HRLDAS. SOILCOLOR is used to select the albedo values for dry and saturated soil.
- The original call to TWOSTREAM in ALBEDO passed TV, which was then redefined to T in the arguments of TWOSTREAM. This is more difficult to do using types, so I've changed T to energy%TV.
    - The same was done for FAB, FRE, FTD, FTI, FREV, and FREG (original INTENT(OUT) arguments to TWOSTREAM). The call, depending on whether direct or diffuse shrotwave were being considered used the suffixed versions of these vars (D for direct, I for diffuse). We now pass only the types (ENERGY in this case) and give the variables value within IF statements (already in the original code).
- VAI and VEG are now computed only once in the EnergyModule, and they are part of the ParametersType. In the original code, VAI and VEG were computed multiple times locally even though their values never changed.
- C3PSN is a constant 1.0 for all vegtypes in MPTABLE, but has 20 (MODIS) and 27 (USGS) table entries. Parameter is changed to be a single value (i.e., no need to read in a table if the value only has 1 option). Same for KC25 and KO25. Similarly, CZIL is a single value formerly read in as a "table" from GENPARM.TBL, but it's now set to a single value (same with ZBOT).
- Changed RS to RSMIN in veg_parameters to be consistent with use in model
- The subroutines calc_declin, geth_newdate, and geth_idts are now in UtilitiesModule.f90. Similar to the other high-level modules, the driver calls UtilitiesMain from the time loop.
- Modifed the noahmp_driver to read in ASCII forcing data as in NOAH-MP V1.1.
    - *IMPORTANT NOTE* The driver is specific to the forcing data variables, units, and order found in the bondville.dat example. It will need to be modified to accept AORC data.

## Bug fixes:
- Albedo for direct shortwave radiation in the NIR (ALBSND(2)) was incorrectly computed using parameters%BATS_VIS_DIR
    - Changed to parameters%BATS_NIR_DIR

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
        - Should they be parameters?
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
- PRECIP_HEAT has a hard-code 1000 in the denominator to convert from mm to m and imposes ±20 W/m2 limits in the code. These should be changed.
- Most snow-aging routines use new snow depth to determine whether albedo should be refreshed or not. Current implementation of SNOW_AGE uses new SWE. Change?
- TWOSTREAM uses energy%TV > parameters%TFRZ to determine whether canopy is snow-covered or not. This should be changed different logic (ex. water%CANICE > 0.0).

## Hard-coded parameters that need to be incorporated in a driver or forcing module:

- DomainType
    - CROPTYPE
        - Currently included as a parameter in namelist.input under “structure”
        - This means if the model were to be run as- is over a grid, every cell would have the prescribed CROPTYPE from the namelist
    - VEGTYP
        - Currently included as a parameter in namelist.input under “structure”
        - This means if the model were to be run as- is over a grid, every cell would have the prescribed VEGTYP from the namelist
        - HRLDAS uses NETCDF to read in land cover from a grid
- Lines 488–490 in module_hrldas_netcdf_io.F
    - LAT and LON
        - Model is set up to run at a single point
        - LAT and LON are given in the namelist.input file under “location”
        - They are then read in using NamelistRead and transferred to the DomainType

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

## Other Notes:

- added STOMATA forcings (FOLN, O2AIR, CO2AIR) to driver
