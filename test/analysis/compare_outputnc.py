import numpy as np
import pandas as pd
from netCDF4 import Dataset
import sys

nc_cntrl = Dataset("../../../noah-owp-modular-clean/data/output.nc")
nc_grid = Dataset("../../data/output.nc")

for var in nc_cntrl.variables.keys():
  if var != 'time':
    print('Testing: '+str(var))
    cntrl_rank = len(list(nc_cntrl.variables[var].shape))
    if cntrl_rank == 1:
      for y in range(1,nc_grid.variables[var].shape[1]):
        for x in range(1,nc_grid.variables[var].shape[2]):
          for t in range(1,nc_grid.variables[var].shape[0]):
            v = abs(nc_grid.variables[var][t][y][x]-nc_cntrl.variables[var][t])
            if v > 0:
              print('***********TEST FAILED************')
              print('   variable: '+str(var))
              print('   time step: '+str(t))
              print('   clean build value: '+str(nc_cntrl.variables[var][t]))
              print('   gridded build value: '+str(nc_grid.variables[var][t][y][x]))
              print('   grid location: x='+str(x)+', y='+str(y))
              sys.exit()
      print('   ...test passed.')
    else:
      for z in range(1,nc_cntrl.variables[var].shape[0]):
        for y in range(1,nc_grid.variables[var].shape[2]):
          for x in range(1,nc_grid.variables[var].shape[3]):
            for t in range(1,nc_grid.variables[var].shape[1]):
              v = abs(nc_grid.variables[var][z][t][y][x]-nc_cntrl.variables[var][z][t])
              if v > 0:
                print('***********TEST FAILED************')
                print('   variable: '+str(var))
                print('   time step: '+str(t))
                print('   clean build value: '+str(nc_grid.variables[var][z][t]))
                print('   gridded build value: '+str(nc_grid.variables[var][z][t][y][x]))
                print('   grid location: x='+str(x)+', y='+str(y)+', z='+str(z))
                sys.exit()
      print('   ...test passed.')
