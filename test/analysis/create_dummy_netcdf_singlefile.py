import netCDF4 as nc
import numpy as np
import random, os

# Define dataset contents
n_y = 3
n_x = 2
dx = 30. # arc-seconds (~1 km)
dy = 30.
vegtyp = 1
isltyp = 1
slope = 0.0
azimuth = 0.0
soilcolor = 4
lat = 40.01
lon = -88.37
cnv_arcsec_to_degrees = 0.0002777777777777778
loc = '../../data'

def create_dummy():

  # Create and open dataset
  filename = os.path.join(loc,'netcdf_input.nc')
  dum = nc.Dataset(filename, 'w', format='NETCDF4')

  # Define dimensions
  dim_lat = dum.createDimension('latitude', n_y)
  dim_lon = dum.createDimension('longitude', n_x)

  # Create variables
  var_lon = dum.createVariable('longitude', 'f4', 'longitude')
  var_lat = dum.createVariable('latitude', 'f4', 'latitude')  
  var_slope = dum.createVariable('slope', 'f4', ('latitude','longitude'))
  var_azimuth = dum.createVariable('azimuth', 'f4', ('latitude','longitude'))
  var_isltyp = dum.createVariable('isltyp', 'i4', ('latitude','longitude'))
  var_vegtyp = dum.createVariable('vegtyp', 'i4', ('latitude','longitude'))
  var_soilcolor = dum.createVariable('soilcolor', 'i4', ('latitude','longitude'))

  # Create attributes
  var_lon.dx = dx
  var_lat.dy = dy

  # Fill lat/lon
  var_lat[:] = np.array([lat+(i*dy*cnv_arcsec_to_degrees) for i in range(n_y)])
  var_lon[:] = np.array([lon+(i*dx*cnv_arcsec_to_degrees) for i in range(n_x)]) 
  #var_lat[:] = np.array([lat for i in range(n_y)])
  #var_lon[:] = np.array([lon for i in range(n_x)]) 

  # Fill slope
  var_slope[:,:] = np.full((n_y, n_x), slope)
  for x in range(n_x):
    for y in range(n_y):
      if x != 0 or y != 0:
        var_slope[y,x] = random.uniform(0,5)
        #pass

  # Fill soils
  var_isltyp[:,:] = np.full((n_y, n_x), isltyp)
  for x in range(n_x):
    for y in range(n_y):
      if x != 0 or y != 0:
        var_isltyp[y,x] = random.randint(1,19)
        #pass

  # Fill land use
  var_vegtyp[:,:] = np.full((n_y, n_x), vegtyp)
  for x in range(n_x):
    for y in range(n_y):
      if x != 0 or y != 0:
        var_vegtyp[y,x] = random.randint(1,20)
        #pass

  # Fill soil color
  var_soilcolor[:,:] = np.full((n_y, n_x), soilcolor)
  for x in range(n_x):
    for y in range(n_y):
      if x != 0 or y != 0:
        var_soilcolor[y,x] = random.randint(1,8)
        #pass

  # Fill azimuth
  var_azimuth[:,:] = np.full((n_y, n_x), azimuth)
  for x in range(n_x):
    for y in range(n_y):
      if x != 0 or y != 0:
        var_azimuth[y,x] = random.uniform(1,360)
        #pass

  # Print some stuff
  print('\n****************DIMENSIONS****************')
  print('\ndim_lon****')
  print(dim_lon)
  print('\ndim_lat****')
  print(dim_lat)
  print('\n****************VARIABLES****************')
  print('\nvar_lon****')
  print(var_lon)
  print('\nvar_lat****')
  print(var_lat)
  print('\nvar_slope****')
  print(var_slope)
  print('\nvar_isltyp****')
  print(var_isltyp)
  print('\nvar_vegtyp****')
  print(var_vegtyp)
  print('\nvar_soilcolor****')
  print(var_soilcolor)
  print('\nvar_azimuth****')
  print(var_azimuth)

  # Close dataset
  dum.close()

def main():
  create_dummy()

if __name__ == '__main__':
  main()