import netCDF4 as nc
import numpy as np
import random

# Define dataset contents
n_y = 3
n_x = 2
dx = 30. # arc-seconds (~1 km)
dy = 30.
vegtyp = 1
lat = 40.01
lon = -88.37
cnv_arcsec_to_degrees = 0.0002777777777777778

# Create and open dataset
loc = '../../data/netcdf_dummy.nc'
dum = nc.Dataset(loc, 'w', format='NETCDF4')

# Define dimensions
dim_lat = dum.createDimension('Latitude', n_y)
dim_lon = dum.createDimension('Longitude', n_x)

# Create variables
var_lon = dum.createVariable('Longitude', 'f4', 'Longitude')
var_lat = dum.createVariable('Latitude', 'f4', 'Latitude')  
var_vegtyp = dum.createVariable('vegtyp', 'i4', ('Latitude','Longitude'))

# Create attributes
var_lon.dx = dx
var_lat.dy = dy

# Fill variables
var_lat[:] = np.array([lat+(i*dy*cnv_arcsec_to_degrees) for i in range(n_y)])
var_lon[:] = np.array([lon+(i*dx*cnv_arcsec_to_degrees) for i in range(n_x)]) 
var_vegtyp[:,:] = np.full((n_y, n_x), vegtyp)
for x in range(n_x):
  for y in range(n_y):
    if x != 0 or y != 0:
      var_vegtyp[y,x] = random.randint(1,20)

# Print some stuff
print('\n****************DIMENSIONS****************')
print(dim_lon)
print(dim_lat)
print('\n****************VARIABLES****************')
print(var_lon)
print(var_lat)
print(var_vegtyp)

# Close dataset
dum.close()