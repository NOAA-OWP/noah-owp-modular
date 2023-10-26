library(hydrofabric)
library(terra)
library(aws.s3)
library(ncdf4)
library(this.path)

# Set wd to ../../data
setwd(here())
basedir = "../../data"
setwd(basedir)

# Populate Tiffin hydrofabric geopackage
## caching the downloaded VPU files to cache_dir and writing all layers to outfile
hl <- 'Gages-04185000'
hyfgpkg = "hyf.gpkg"
subset_network(hl_uri = hl, cache_dir = basedir, outfile = hyfgpkg)

# Add lumped CFE/NOAH-OM attributes to Tiffin hydrofabric geopackage
att = "cfe_noahowp"
s3_base = "s3://lynker-spatial/"
x = read_sf(hyfgpkg, "network")
par = open_dataset(glue('{s3_base}pre-release/nextgen_{unique(x$vpu)}_{att}.parquet')) %>% 
  filter(divide_id %in% x$divide_id) %>% 
  collect()
div = left_join(read_sf(hyfgpkg, "divides"), par, by = "divide_id")

# Define a nc file to hold CONUS gridded atttributes and populate
nwm_d = file.path(basedir,'NGENGriddedDataCONUS.nc')
n = get_bucket_df(bucket = s3_base, prefix = 'gridded-resources', region = "us-west-2") %>%
   filter(grepl("ngen", Key)) %>%
   select(Key, Bucket)
save_object(object = n$Key, bucket = n$Bucket, file = nwm_d, region = "us-west-2", show_progress  = TRUE)
nwm_d_fix = correct_nwm_spatial(nwm_d)

# Crop gridded attributes to Tiffin boundary and write to new nc file
nwm_crop = crop(nwm_d_fix, st_transform(read_sf(hyfgpkg, "divides"), crs(nwm_d_fix)), mask = TRUE)
nwm_crop <- project(nwm_crop,'EPSG:4269') # To use lat/lon

# Define some strings
name_isltyp <- 'isltyp'
name_vegtyp <- 'vegtyp'
name_slope <- 'slope'
name_azimuth <- 'azimuth'
name_soilcolor <- 'soilcolor'
name_mask <- 'mask'

# isltyp
isltyp = nwm_crop$`ISLTYP_Time=1`
names(isltyp) <- c(name_isltyp)
isltyp <- as.int(isltyp)

# vegtyp
vegtyp = nwm_crop$`IVGTYP_Time=1`
names(vegtyp) <- c(name_vegtyp)
vegtyp <- as.int(vegtyp)

# slope
slope = nwm_crop$`slope_Time=1`
names(slope) <- c(name_slope)

# azimuth
mask = ifel(is.na(slope), NA, 1)
azimuth = direction(x=slope,from=FALSE,degrees=TRUE)
azimuth = mask(x=azimuth,mask=mask)
names(azimuth) <- c(name_azimuth)

# soil color
##NOTE: Page 15 of https://www.jsg.utexas.edu/noah-mp/files/Users_Guide_v0.pdf states:
##NOTE: "for a case lack of soil color data, one may choose a medium dark color index (= 4 or 5). For a sandy soil or desert, it is better to choose the lightest index (=1)." 
soilcolor = ifel(is.na(vegtyp),NA,5)
names(soilcolor) <- c(name_soilcolor)
soilcolor <- as.int(soilcolor)

# model mask
mask = ifel(is.na(slope), 0, 1)
names(mask) <- c(name_mask)
mask <- as.int(mask)

# Need to write these individually then merge because I can't vary perc argument across all layers
filename_isltyp = file.path(getwd(),paste(name_isltyp,'.nc',sep=''))
writeCDF(x=isltyp, filename=filename_isltyp, varname=name_isltyp, longname=name_isltyp, unit="NA", split=FALSE, overwrite=TRUE, prec='integer')
filename_vegtyp = file.path(getwd(),paste(name_vegtyp,'.nc',sep=''))
writeCDF(x=vegtyp, filename=filename_vegtyp, varname=name_vegtyp, longname=name_vegtyp, unit="NA", split=FALSE, overwrite=TRUE, prec='integer')
filename_slope = file.path(getwd(),paste(name_slope,'.nc',sep=''))
writeCDF(x=slope, filename=filename_slope, varname=name_slope, longname=name_slope, unit="degrees", split=FALSE, overwrite=TRUE, prec='float')
filename_soilcolor = file.path(getwd(),paste(name_soilcolor,'.nc',sep=''))
writeCDF(x=soilcolor, filename=filename_soilcolor, varname=name_soilcolor, longname=name_soilcolor, unit="NA", split=FALSE, overwrite=TRUE, prec='integer')
filename_azimuth = file.path(getwd(),paste(name_azimuth,'.nc',sep=''))
writeCDF(x=azimuth, filename=filename_azimuth, varname=name_azimuth, longname=name_azimuth, unit="degrees clockwise from north", split=TRUE, overwrite=TRUE, prec='float')
filename_mask = file.path(getwd(),paste(name_mask,'.nc',sep=''))
writeCDF(x=mask, filename=filename_mask, varname=name_mask, longname=name_mask, unit="NA", split=FALSE, overwrite=TRUE, prec='integer')

# Then merge the files via the cmd line argument below (cdo must be installed) from basedir
#cdo merge azimuth.nc isltyp.nc mask.nc slope.nc soilcolor.nc vegtyp.nc NoahOMGriddedAttributes.nc
command <- paste('cdo merge ',name_azimuth,'.nc ',name_isltyp,'.nc ',name_vegtyp,'.nc ',name_slope,'.nc ',name_mask,'.nc ',name_soilcolor,'.nc GriddedAttributes.nc',sep='')
system(command)

# Remove working files
unlink(filename_isltyp)
unlink(filename_vegtyp)
unlink(filename_slope)
unlink(filename_soilcolor)
unlink(filename_azimuth)
unlink(filename_mask)