import netCDF4 as nc
import numpy as np

# Define dataset contents
n_y = 3
n_x = 2
dx = 100.0
dy = 100.0
nveg = 20
vegtyp = 1
lat = 40.01
lon = -88.37
veg_class_name = "MODIFIED_IGBP_MODIS_NOAH"

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
var_vegtyp.nveg = nveg
var_vegtyp.veg_class_name = veg_class_name

# Fill variables
var_lat[:] = np.full(n_y, lat)  
var_lon[:] = np.full(n_x, lon) 
var_vegtyp[:,:] = np.full((n_y, n_x), vegtyp)

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