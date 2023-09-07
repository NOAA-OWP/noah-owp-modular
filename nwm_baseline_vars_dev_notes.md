# ACSNOM

### Spreadsheet notes
To be added to BMI output_vars and need to multiply QSNBOT by time step size in seconds to get output in accumulated vs rate space (then add to the three PONDING terms).

### Dev notes
ACSNOM is [#70 in ldasOutDict%varNames in module_NWM_io_dict.F](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1072C68-L1072C68) and is described as the ["accumulated melting water out of snow bottom"](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1156C31-L1156C77) with [units of mm](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1218C56-L1218C58) and a [z length of 1](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1318C54-L1318C54)

ACSNOM [is calculated](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/phys/module_sf_noahmpdrv.F#L1401) as:
```
ACSNOM(I,J) = ACSNOM(I,J) + QSNBOT*DT + PONDING + PONDING1 + PONDING2
```

Per spreadsheet notes and the above NWM calculation:
```(Fortran)
ACSNOM(:,:) = watergrid_type%PONDING(:,:)+watergrid_type%PONDING1(:,:)+watergrid_type%PONDING2(:,:)+(watergrid_type%QSNBOT(:,:)*domaingrid_type%DT)
```

# ACCET

### Spreadsheet notes
Already listed in output vars but need to multiply by time step size in seconds to get output in accumulated vs rate space. Additionally, Noah-OM outputs a pseudo-PET when running in surface mode and we would need to add two-way coupling to get an actual value.

### Dev notes
Per the spreadsheet:
```(Fortran)
ACCET(:,:) = watergrid_type%evapotrans(:,:)*domaingrid_type%DT
```

# SNOWT_AVG

### Spreadsheet notes
Noah-OM calculates per-layer snow and soil temperature. We need to confirm if NWM 3.0 outputs this var per layer or as an average of the snowpack. If the latter, we need to add a weighted average from the layers as a variable and include it in the BMI output_vars.

### Dev notes
SNOW_AVG is [#95 in ldasOutDict%varNames in module_NWM_io_dict.F](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1077C64-L1077C64) and is described as the ["average snow temperature (by layer mass)"](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1181C31-L1181C79) and has [units of K](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1223) and has [z dim length of 1](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1321C38-L1321C38)

[SNOWT_AVG is calculated as](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/phys/module_sf_noahmpdrv.F#L1382):
```
if ( SUM(SNICE(-NSNOW+1:0) + SNLIQ(-NSNOW+1:0)) .gt. 0 ) then
  SNOWT_AVG (I,J)  = SUM(STC(-NSNOW+1:0) * (SNICE(-NSNOW+1:0) + SNLIQ(-NSNOW+1:0))) / SUM(SNICE(-NSNOW+1:0) + SNLIQ(-NSNOW+1:0))
else
  SNOWT_AVG (I,J) = undefined_value
endif
```
SNOW_AVG [is added to NWM output](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/IO_code/module_NoahMP_hrldas_driver.F#L2221C43-L2221C43) via: 
```
output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,(SNOWT_AVG),IVGTYP,95)
```

*CONFIRMED* that NWM gives a weighted average. We could replicate NWM calculation via:
```
do ix = 1, n_x
  do iy = 1, n_y
    SNOWT_AVG(ix,iy) = SUM(energygrid%STC(ix,iy,-nsnow+1:0)*(watergrid%SNICE(ix,iy,-nsnow+1:0)+watergrid%SNLIQ(ix,iy,-nsnow+1:0)))/SUM(watergrid%SNICE(ix,iy,-nsnow+1:0)+watergrid%SNLIQ(ix,iy,-nsnow+1:0))
  end do
end do
```

The above requires iterating over the grid, which is not preferrable. Thus, it might be better to add SNOWT_AVG to watergrid_type and calculate it within the scope of solve_noahowp. This seems to be the case for several of the NWM variables.

# EDIR

### Spreadsheet notes
In BMI output_vars, but QSEVA is output in units of depth per time versus mass per area per timestep. Additionally, this var is part of a pseudo-PET estimate when Noah-OM run in surface mode and we would need to add two-way coupling to get an actual value.

### Dev notes
EDIR is [#17 in ldasOutDict%varNames in module_NWM_io_dict.F](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1062C33-L1062C96) and is described as
["Direct from soil evaporation rate"](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1103C31-L1103C76) with [units = "kg m-2 s-1"](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1208C42-L1208C54) and [zdim = 1](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1313C59-L1313C59)

EDIR [is added to NWM output](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/IO_code/module_NoahMP_hrldas_driver.F#L2143C4-L2143C135) via:
```
call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,EDIRXY,IVGTYP,17)
```

The call to output_NoahMP_NWM indicates that EDIRXY is being written for EDIR (see above). [EDIRXY is defined as having units of mm/s](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/phys/module_sf_noahmpdrv.F#L271C1-L271C120) and [EDIRXY is set to ESOIL](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/phys/module_sf_noahmpdrv.F#L1453), which [also has units of mm/s](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/phys/module_sf_noahmpdrv.F#L420C95-L420C95). Thus, while the output file states that the variable's units are 'kg m-2 s-1', I do not see this to be the case.

We can do the unit conversion if we need -- but need to confirm that NWM is providing in kg m-2 s-1 as specified.

# SOILICE

### Spreadsheet notes
Noah-OM calculates per-layer soil ice. We need to confirm if NWM 3.0 outputs this var per layer or as an average of the soil matrix. If the latter, we need to add a weighted average from the layers as a variable and include it in the BMI output_vars. Additionally, this var is calculated when Noah-OM is run in surface mode to preserve energy balance calculations, but it is not strictly valid and should be accessed from Soil Freeze Thaw.

### Dev notes
SOLICE is [#92 in ldasOutDict%varNames module_NWM_io_dict.F](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1077) and is described as the ["fraction of soil moisture that is ice"](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1178C31-L1178C76) and has [z dim length of 1](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1321C42-L1321C43) and [units of 1](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1223C27-L1223C28)

SOILICE [is calculated](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/phys/module_sf_noahmpdrv.F#L1380) as:
```
if(present(SOILICE)) SOILICE (I,J)  = 1. - ( SUM(SMH2O(1:NSOIL) * DZS(1:NSOIL)) / SUM(SMC(1:NSOIL) * DZS(1:NSOIL)) )
```
The var [is added to NWM output](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/IO_code/module_NoahMP_hrldas_driver.F#L2218C4-L2218C138) via:
```
call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,(SOILICE),IVGTYP,92)
```

Within the above calculation, [SMH2O has units of m3/m3](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/phys/module_sf_noahmpdrv.F#L358), [SMC has units of m3/m3](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/phys/module_sf_noahmpdrv.F#L358) and [DZS  has units of m](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/IO_code/module_NoahMP_hrldas_driver.F#L91)

*CONFIRMED* that NWM provides a single value for the soil matrix. We could replicate NWM calculation of SOILICE as:
```
do ix = 1, n_x
  do iy = 1, n_y
    SOILICE(ix,iy) = 1. - ( sum(sh2o(ix,iy,1:nsoil) * dzsnso(ix,iy,1:nsoil)) / sum(smc(ix,iy,1:nsoil) * dzsnso(ix,iy,1:nsoil)) )
  end do
end do
```

Altenatively, we could just do a weighted average of watergrid_type%sice.

# SOILSAT_TOP

### Spreadsheet notes
Noah-OM calculates per-layer soil moisture content. We need to confirm if NWM 3.0 outputs this var per layer or as an average of the top two layers of the soil matrix. If the latter, we need to add a weighted average from the layers as a variable and include it in the BMI output_vars. Additionally, this var is not valid when run in surface mode and should be accessed from a subsurface moduel OR Noah-OM should be run with two-way coupling.

### Dev notes
SOILSAT_TOP is [#93 in ldasOutDict%varNames](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1077C52-L1077C63) and is described as the ["fraction of soil saturation, top 2 layers"](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1179C31-L1179C80) and has [units = 1](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1223) and has [z dim length of 1](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1321C59-L1321C59)

SOILSAT_TOP [is calculated](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/phys/module_sf_noahmpdrv.F#L1376C1-L1376C134) as:
```
if(present(SOILSAT_TOP)) SOILSAT_TOP (I,J) = SUM(SMC(1:2) * DZS(1:2)) / sum(smcmax(1:2) * DZS(1:2))
```
and is [added to NWM output](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/IO_code/module_NoahMP_hrldas_driver.F#L2219C4-L2219C142) via:
```
call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,(SOILSAT_TOP),IVGTYP,93)
```

*CONFIRMED* that NWM provides an average of the top two layers. We could replicate NWM3's calculation via:
```
do ix = 1, n_x
  do iy = 1, n_y
    soilsat_top(ix,iy) = sum(smc(ix,iy,1:2) * dzsnso(ix,iy,1:2)) / SUM(smcmax(ix,iy,1:2) * dzsnso(ix,iy,1:2))
  end do
end do
```

# SNLIQ

### Spreadsheet notes
Noah-OM calculates per-layer snow liquid water content. We need to confirm if NWM 3.0 outputs this var per layer or as an average of the snowpack. If the latter, we need to add a weighted average from the layers as a variable and include it in the BMI output_vars.

### Dev notes
SNLIQ is [#59 in ldasOutDict%varNames in module_NWM_io_dict.F](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1070C33-L1070C96). The var is described as ["Snow layer liquid water"](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1145C31-L1145C76) with [units of mm](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1216C86-L1216C86). The [z dim length is 3](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1317C19-L1317C43).

The var [is added to NWM output](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/IO_code/module_NoahMP_hrldas_driver.F#L2185C4-L2185C142) via:
```
call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,3,state%SNLIQXY,IVGTYP,59)
```

*CONFIRMED* that NWM provide SNLIQ values per snow layer because state%SNLIQXY is written to NWM output (see above call to output_NoahMP_NWM) and the call to output_NoahMP_NWM gives a value of 3 for the z dimension length (fourth argument from right). Hence, we'd just need to reference SNLIQ as:
```
watergrid_type%SNLIQ(:,:,:)
```

# SOIL_T

### Spreadsheet notes
Noah-OM calculates per-layer snow and soil temperature. We need to confirm if NWM 3.0 outputs this var per layer or as an average of the soil matrix. If the latter, we need to add a weighted average from the layers as a variable and include it in the BMI output_vars. Additionally, this var is calculated when Noah-OM is run in surface mode to preserve energy balance calculations, but it is not strictly valid and should be accessed from Soil Freeze Thaw.

### Dev notes
SOIL_T is [#60 in ldasOutDict%varNames in module_NWM_io_dict.F](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1070C61-L1070C61) and is described as ["soil temperature"](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1146C31-L1146C76) with [units of K](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1216C9-L1216C86)

The variable has a [z dim length of 4](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1317C1-L1317C56) and is added [to NWM outputs](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/IO_code/module_NoahMP_hrldas_driver.F#L2186C29-L2186C29) via:
```
call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,4,TSLB,IVGTYP,60)
```
Note that 4 is given as z dim length in above call to output_NoahMP_NWM.

*CONFIRMED* that NWM provides SOIL_T per soil layer. Hence, we could get SOIL_T via:
```
do ix = 1, n_x
  do iy = 1, n_y
    SOIL_T(ix,iy,:) = energygrid_type%STC(ix,iy,1:nsoil)
  end do
end do
```

# SOIL_M

### Spreadsheet notes
Noah-OM calculates per-layer soil moisture content. We need to confirm if NWM 3.0 outputs this var per layer or as an average of the soil matrix. If the latter, we need to add a weighted average from the layers as a variable and include it in the BMI output_vars. Additionally, this var is not valid when run in surface mode and should be accessed from a subsurface moduel OR Noah-OM should be run with two-way coupling.

### Dev notes
SOIL_M is [#62 in ldasOutDict%varNames in module_NWM_io_dict.F](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1071C51-L1071C57). The var is described as ["snow temperature"](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1148C31-L1148C76) with [units of K](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1217C56-L1217C56)

SOIL_M has a [z dim length of 3](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1318C32-L1318C32) and [is added to NWM output](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/IO_code/module_NoahMP_hrldas_driver.F#L2188C4-L2188C141) via:
```
call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,3,state%TSNOXY,IVGTYP,62)
```
Note that 3 is given as argument z dim length in above call to output_NoahMP_NWM.

*CONFIRMED* that SOIL_M is provided per layer. Hence, we could get SOIL_M via:
```
do ix = 1, n_x
  do iy = 1, n_y
    SOIL_M(ix,iy,:) = energygrid_type%STC(ix,iy,-nsnow+1:0)
  end do
end do
```

# ACCECAN

### Spreadsheet notes
Already listed in output vars but need to multiply by time step size in seconds to get output in accumulated vs rate space. Additionally, Noah-OM outputs a pseudo-PET when running in surface mode and we would need to add two-way coupling to get an actual value.

### Dev notes
ACCECAN is [#28 in ldasOutDict%varNames in module_NWM_io_dict.F](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1064C51-L1064C51) and is described as ["Accumulated canopy evap"](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1114C31-L1114C76) with [units of mm](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1210C4-L1210C45) and has [z dim length of 1](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1314)

ACCECAN is being [added to NWM output](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/IO_code/module_NoahMP_hrldas_driver.F#L2154C4-L2154C136) via:
```
call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,ACCECAN,IVGTYP,28)
```

Per spreadsheet:
```
ACCECAN(:,:) = watergrid_type%qseva(:,:) * domaingrid_type%DT
```

# ACCEDIR

### Spreadsheet notes
Already listed in output vars but need to multiply by time step size in seconds to get output in accumulated vs rate space. Additionally, Noah-OM outputs a pseudo-PET when running in surface mode and we would need to add two-way coupling to get an actual value.

### Dev notes
ACCEDIR is [#29 in ldasOutDict%varNames in module_NWM_io_dict.F](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1064C73-L1064C73) and is described as ["Accumulated direct soil evap"](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1115C31-L1115C76) with [units of mm](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1210C85-L1210C85) with a [z dim length of 1](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1314C37-L1314C37)

The ACCEDIR is [added to NWM output](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/IO_code/module_NoahMP_hrldas_driver.F#L2155C4-L2155C136) via:
```
call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,ACCEDIR,IVGTYP,29)
```

Per spreadsheet:
```
ACCEDIR(:,:) = watergrid_type%ECAN(:,:) * domaingrid_type%DT
```

# ACCETRAN

### Spreadsheet notes
Already listed in output vars but need to multiply by time step size in seconds to get output in accumulated vs rate space. Additionally, Noah-OM outputs a pseudo-PET when running in surface mode and we would need to add two-way coupling to get an actual value.

### Dev notes
ACCETRAN is [#30 in ldasOutDict%varNames in module_NWM_io_dict.F](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1064C55-L1064C56) and is described as ["Accumulated transpiration"](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1116C31-L1116C76) with [units of mm](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1210C56-L1210C56) and a [z dim length of 1](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1314)

ACCETRAN is [added to NWM output](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/IO_code/module_NoahMP_hrldas_driver.F#L2156C4-L2156C137) via:
```
call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,ACCETRAN,IVGTYP,30)
```

Per spreadsheet:
```
ACCEDIR(:,:) = watergrid_type%ETRAN(:,:) * domaingrid_type%DT
```

# UGDRNOFF

### Spreadsheet notes
When run in surface mode, Noah-OM does not output a valid UGDRNOFF. Additionally, we need to mutliply the variable RUNSUB by the timestep in seconds to get a depth vs rate. This variable should likely come from another module. Another consideration is that the subsurface refactoring done by NCAR for Noah-OM includes more options for estimating RUNSUB versus previous Noah-MP versions. Finally, RUNSUB is the rate at which Noah-OM & MP push water out of the soil matrix based on per-layer soil properties and water fluxes/states.

### Dev notes
UGDRNOFF is [#20 in ldasOutDict%varNames](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1062C53-L1062C53) in module_NWM_io_dict.F and is described as ["Accumulated underground runoff"](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1106C31-L1106C76) with [units of mm](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1208C73-L1208C75) and a [z dim length of 1](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1313C42-L1313C43)

UGDRNOFF is [added to NWM output](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/IO_code/module_NoahMP_hrldas_driver.F#L2146C4-L2146C137) via:
```
call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,UDRUNOFF,IVGTYP,20)
```

Suggestion:
```
UGDRNOFF(:,:) = watergrid_type%runsub(:,:) * domaingrid_type%DT
```

# GRDFLX

### Spreadsheet notes
To be added to BMI output_vars and confirm that GH is weighted average of GHV (under canopy) and GHB (bare ground). Also note that all energy balance terms likely affected by pseudo-PET calculations in surface mode. Might require two-way coupling for enhanced accuracy.

### Dev notes
GRDFLX [is #13 in ldasOutDict%varNames in module_NWM_io_dict.F](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1061C46-L1061C52) and is described as ["Heat flux into the soil"](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1099C19-L1099C26) with [units of W m-2](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1207C46-L1207C51) and a [z dim length of 1](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1313C41-L1313C42)

GRDFLX [is added to NWM output](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/IO_code/module_NoahMP_hrldas_driver.F#L2139C4-L2139C135) via: 
```
call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,GRDFLX,IVGTYP,13)
```

To be added to BMI output_vars and confirm that GH is weighted average of GHV (under canopy) and GHB (bare ground). Also note that all energy balance terms likely affected by pseudo-PET calculations in surface mode. Might require two-way coupling for enhanced accuracy.

I don't see where GH is being calculated in Noah-OM. Thus, we'd need to calculate GH as weighted average of GHV and GHB.

# TRAD

### Spreadsheet notes
To be added to BMI output_vars. Also note that all energy balance terms likely affected by pseudo-PET calculations in surface mode. Might require two-way coupling for enhanced accuracy.

### Dev notes
TRAD is [#45 in ldasOutDict%varNames in module_NWM_io_dict.F](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1067C36-L1067C36) and is described as ["Surface radiative temperature"](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1131C31-L1131C76) with [units of K](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1213C47-L1213C55) with a [z dim of 1](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1316).

TRAD is [added to NWM output](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/IO_code/module_NoahMP_hrldas_driver.F#L2171C4-L2171C135) via:
```
call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,TRADXY,IVGTYP,45)
```

TRAD is a member of energygrid_type in Noah-OM:
```
energygrid_type%TRAD(:,:)
```

# FSA

### Spreadsheet notes
To be added to BMI output_vars. Also note that all energy balance terms likely affected by pseudo-PET calculations in surface mode. Might require two-way coupling for enhanced accuracy.

### Dev notes
FSA is [#11 in ldasOutDict%varNames in module_NWM_io_dict.F](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1061C55-L1061C55) which is described as ["Total absorbed SW radiation"](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1097C31-L1097C76) with [units of "W m-2"](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1207C29-L1207C36) and [z dim length of 1](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1313C43-L1313C44)

FSA is [added to NWM output](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/IO_code/module_NoahMP_hrldas_driver.F#L2137C4-L2137C134) via:
```
call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,FSAXY,IVGTYP,11)
```

FSA is a member of energygrid_type in Noah-OM:
```
energygrid_type%FSA(:,:)
```

# CANWAT

### Spreadsheet notes
To be added to BMI output_vars

### Dev notes
CANWAT is [#91 in ldasOutDict%varNames in module_NWM_io_dict.F](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1077) which is described as ["Total canopy water (liquid + ice)"](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1177C31-L1177C76) with [units of mm](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1223C42-L1223C42) and a [z dim length of 1](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1321C55-L1321C55)

CANWAT is [added to NWM output](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/IO_code/module_NoahMP_hrldas_driver.F#L2217C4-L2217C135) via:
```
call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,CANWAT,IVGTYP,91)
```

CANWAT is equivalent to CMC in Noah-OM, which is a member of watergrid_type:
```
watergrid_type%CMC(:,:)
```
# LH

### Spreadsheet notes
To be added to BMI output_vars. Also note that all energy balance terms likely affected by pseudo-PET calculations in surface mode. Might require two-way coupling for enhanced accuracy.

### Dev notes
LH is [#15 in ldasOutDict%varNames in module_NWM_io_dict.F](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1061C31-L1061C31) and is described as ["Total latent heat to the atmosphere"](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1101C76-L1101C76) with a [z dim length of 1](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1313).

LH is [added to NWM output](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/IO_code/module_NoahMP_hrldas_driver.F#L2141C4-L2141C131) via:
```
call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,LH,IVGTYP,15)
```

LH is a member of energygrid_type in Noah-OM:
```
energygrid_type%LH(:,:)
```

# FIRA

### Spreadsheet notes
To be added to BMI output_vars. Also note that all energy balance terms likely affected by pseudo-PET calculations in surface mode. Might require two-way coupling for enhanced accuracy.

### Dev notes
FIRA is [#12 in ldasOutDict%varNames in module_NWM_io_dict.F](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1061C31-L1061C31) and is described as ["Total net LW radiation to atmosphere"](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1098C31-L1098C76) with [units of W m-2](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1207C38-L1207C43) and a [z dim length of 1](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1313)

FIRA is [added to NWM output](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/IO_code/module_NoahMP_hrldas_driver.F#L2138C4-L2138C135) via:
```
call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,FIRAXY,IVGTYP,12)
```

FIRA is a member of energygrid_type in Noah-OM:
```
energygrid_type%FIRA(:,:)
```

# HFX

### Spreadsheet notes
To be added to BMI output_vars. Also note that all energy balance terms likely affected by pseudo-PET calculations in surface mode. Might require two-way coupling for enhanced accuracy.

### Dev notes
HFX is [#14 in ldasOutDict%varNames in module_NWM_io_dict.F](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1061) and is described as the ["Total sensible heat to the atmosphere"](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1100C31-L1100C76) with units of [W m-2](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1207C62-L1207C67) and a [z dim length of 1](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Routing/module_NWM_io_dict.F#L1313)

HFX is [added to NWM output](https://github.com/NCAR/wrf_hydro_nwm_public/blob/5d9e489fed01ed7cbd6b5680616ee37812a7137a/src/Land_models/NoahMP/IO_code/module_NoahMP_hrldas_driver.F#L2140C4-L2140C132) via:
```
call output_NoahMP_NWM(trim(noah_lsm%outdir),igrid,noah_lsm%output_timestep,itime,startdate,olddate,ixpar,jxpar,1,HFX,IVGTYP,14)
```

Per spreadsheet, HFX is equivalent to FSH in Noah-OM, which is a member of energygrid_type:
```
energygrid_type%FSH(:,:)
```