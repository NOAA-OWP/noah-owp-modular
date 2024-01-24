suppressPackageStartupMessages({
library(hydrofabric)
library(terra)
library(aws.s3)
library(ncdf4)
})

# Set input and output names from script arguments
input_hydrofabric_gpkg <- ''
output_file_nc <- ''
args = commandArgs(trailingOnly=TRUE)
if (length(args)==2) {
  input_hydrofabric_gpkg <- args[1]
  output_file_nc <- args[2]
} 

# Name needed data on AWS
query_vars <- c('slope', 'ISLTYP', 'IVGTYP')
tifs <- glue::glue('/vsicurl/http://lynker-spatial.s3.amazonaws.com/gridded-resources/nwm/conus/{query_vars}.tif')
rasts <- rast(tifs)

# Crop data to hydrofabric bounding box
divides <- st_transform(read_sf(input_hydrofabric_gpkg, "divides"), crs(rasts))
rasts <- crop(rasts,divides)

# Get lat/long along raster edge
xs <- rasts$slope[nrow(rasts$slope):nrow(rasts$slope),1:ncol(rasts$slope),drop=FALSE] #Get bottom row of slope raster
ys <- rasts$slope[1:nrow(rasts$slope),1:1,drop=FALSE] #Get left column of slope raster
xs <- crds(project(as.points(xs),"+proj=longlat")) # Turn raster row into points and project to lat/lon and get coordinates
ys <- crds(project(as.points(ys),"+proj=longlat")) # Turn raster column into points and project to lat/lon and get coordinates
xs <- xs[1:ncol(rasts$slope),1] # Get list of longitude values
ys <- ys[1:nrow(rasts$slope),2] # Get list of latitude values

# Rename variables
names(rasts) <- c('slope','isltyp','vegtyp')

# Create soil color raster as default value of 5
##NOTE: Page 15 of https://www.jsg.utexas.edu/noah-mp/files/Users_Guide_v0.pdf states:
##NOTE: "for a case lack of soil color data, one may choose a medium dark color index (= 4 or 5). For a sandy soil or desert, it is better to choose the lightest index (=1)." 
rasts$soilcolor = ifel(is.na(rasts$slope),NA,5)

# Create azimuth raster
rasts$azimuth <- terrain(x=rasts$slope, v="aspect",unit="degrees",neighbors=8)

# Create mask raster
rasts$mask <- crop(x=ifel(is.na(rasts$azimuth),0,1),y=divides,mask=TRUE,touches=FALSE)

# Crop data to mask raster
rasts <- crop(x=rasts,y=rasts$mask,mask=TRUE)

# Convert float to int where needed
rasts$isltyp <- as.int(rasts$isltyp)
rasts$vegtyp <- as.int(rasts$vegtyp)
rasts$mask <- as.int(rasts$mask)
rasts$soilcolor <- as.int(rasts$soilcolor)

# Define NetCDF dimensions
dim_x <- ncdim_def(name='longitude',units='degrees',longname='longitude',vals=xs)
dim_y <- ncdim_def(name='latitude',units='degrees',longname='latitute',vals=ys)

# Define NetCDF variables
var_isltyp <- ncvar_def(name="isltyp",units="",dim=list(dim_x,dim_y),missval=NULL,longname="soil type", prec="integer")
var_vegtyp <- ncvar_def(name="vegtyp",units="",dim=list(dim_x,dim_y),missval=NULL,longname="vegetation type", prec="integer")
var_slope <- ncvar_def(name="slope",units="degrees",dim=list(dim_x,dim_y),missval=NA,longname="slope", prec="float")
var_azimuth <- ncvar_def(name="azimuth",units="degrees",dim=list(dim_x,dim_y),missval=NA,longname="azimuth", prec="float")
var_mask <- ncvar_def(name="mask",units="",dim=list(dim_x,dim_y),missval=NULL,longname="mask", prec="integer")
var_soilcolor <- ncvar_def(name="soilcolor",units="",dim=list(dim_x,dim_y),missval=NULL,longname="soil color", prec="integer")

# Create NetCDF file
vars_all <- list(var_isltyp,var_vegtyp,var_slope,var_azimuth,var_mask,var_soilcolor)
nc <- nc_create(output_file_nc, vars_all, force_v4=FALSE, verbose=FALSE)

# Populate NetCDF variables
ncvar_put(nc=nc,varid=var_isltyp,vals=values(rasts$isltyp))
ncvar_put(nc=nc,varid=var_vegtyp,vals=values(rasts$vegtyp))
ncvar_put(nc=nc,varid=var_slope,vals=values(rasts$slope))
ncvar_put(nc=nc,varid=var_azimuth,vals=values(rasts$azimuth))
ncvar_put(nc=nc,varid=var_mask,vals=values(rasts$mask))
ncvar_put(nc=nc,varid=var_soilcolor,vals=values(rasts$soilcolor))

# Close NetCDF file
nc_close(nc)

