import numpy as np
import pandas as pd
from netCDF4 import Dataset
import sys

nc_cntrl = Dataset("../../../old/noah-owp-modular-clean/data/output.nc") #path/name of output.nc from clean build
nc_grid = Dataset("../../data/output.nc") #path/name of output.nc from gridded build

for var in nc_cntrl.variables.keys():
  if var != 'time':
    print('Testing: '+str(var))
    cntrl_rank = len(list(nc_cntrl.variables[var].shape))
    if cntrl_rank == 1:
      n_y = nc_grid.variables[var].shape[1]
      n_x = nc_grid.variables[var].shape[2]
      n_t = nc_grid.variables[var].shape[0]
      ff = np.full((n_y, n_x), 0)
      for y in range(n_y):
        for x in range(n_x):
          if ff[y][x] == 0:
            for t in range(n_t):
              if ff[y][x] == 0:
                v = abs(nc_grid.variables[var][t][y][x]-nc_cntrl.variables[var][t])
                if v > 0:
                  ff[y][x] = t
                  break
      for y in range(n_y):
        for x in range(n_x):
          gid = '(x='+str(x+1)+',y='+str(y+1)+')'
          if ff[y][x] == 0:
            print('   GRID CELL '+gid+' PASSED')
          else:
            print('***GRID CELL '+gid+' FAILED***')
    else:
      n_y = nc_grid.variables[var].shape[2]
      n_x = nc_grid.variables[var].shape[3]
      n_t = nc_grid.variables[var].shape[1]
      n_z = nc_grid.variables[var].shape[0]
      ff = np.full((n_y, n_x), 0)
      for y in range(n_y):
        for x in range(n_x):
          if ff[y][x] == 0:
            for t in range(n_t):
              if ff[y][x] == 0:
                for z in range(n_z):
                  if ff[y][x] == 0:
                    v = abs(nc_grid.variables[var][z][t][y][x]-nc_cntrl.variables[var][z][t])
                    if v > 0:
                      ff[y][x] = t
      for y in range(n_y):
        for x in range(n_x):
          gid = '(x='+str(x+1)+',y='+str(y+1)+')'
          if ff[y][x] == 0:
            print('   GRID CELL '+gid+' PASSED')
          else:
            print('***GRID CELL '+gid+' FAILED***')
