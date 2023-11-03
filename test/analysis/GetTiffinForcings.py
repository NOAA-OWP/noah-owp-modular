import pandas,geopandas,os,math,numpy

hyfgpkg = "../../data/Tiffin.gpkg"        # hydrofabric geopackage
dircat  = '../../data/csv'                # dir containing AORC csv files for each hydrofabric cat
outfile = '../../data/TiffinForcings.dat' # outfile file to be fed into Noah-OM

# Get catchment areas
dta = dict()
data = geopandas.read_file(hyfgpkg)
for index,row in data.iterrows(): # Looping over all points
    dta[row['divide_id']] = float(row['areasqkm'])

# Read AORC csvs and put into a single pandas df
dt = dict()
for catid in dta:
    filecat = catid+'.csv'
    dt[catid] = pandas.read_csv(os.path.join(dircat,filecat))
    #dt[catid].set_index('Time',inplace=True)
    dt[catid]['catid'] = catid
df = pandas.concat([dt[catid] for catid in dt])
df['cat_area_km2'] = df['catid'].apply(lambda x: dta.get(x)) # add area column
df['Time'] = pandas.to_datetime(df['Time']) # time -> datetime for easy formatting later

# Add/calculate some new columns
### Calculate relative humidity from specific humidity
qair_dimless = df['Q2D']
temp_c = df['T2D'] - 273.15
press_mb = df['PSFC'] / 100.0
es = 6.112 * numpy.exp((17.67 * temp_c)/(temp_c + 243.5))
e = qair_dimless * press_mb / (0.378 * qair_dimless + 0.622)
rh = e/es
rh = numpy.where(rh < 0, 0, rh)
rh = numpy.where(rh > 1, 1, rh)
df['rh'] = rh * 100 # 0 to 1 -> 0 to 100

### Calculate wind speed and direction
df['ws'] = numpy.sqrt(df['U2D']**2+df['V2D']**2)
df['wd'] = numpy.degrees(numpy.arctan2(df['V2D'],df['U2D']))
df['wd'] = numpy.where(df['wd'] < 0.,180+df['wd']+180,df['wd']) #e.g., -180 -> 180, -90 -> 270

# Calculate area weighted means
df2 = df
for col in ['RAINRATE','rh','T2D','ws','wd','LWDOWN','SWDOWN','PSFC']:
    df2[col] = df2[col] * df2['cat_area_km2']
df3 = df2.groupby('Time').sum()
for col in ['RAINRATE','rh','T2D','ws','wd','LWDOWN','SWDOWN','PSFC']:
    df3[col] = df3[col] / numpy.sum(list(dta.values())) 
drops = [col for col in df3.columns if col not in ['RAINRATE','rh','T2D','ws','wd','LWDOWN','SWDOWN','PSFC']]
df3.drop(columns=drops,inplace=True) # Remove unneeded cols

# Write forcing input file
with open(outfile,'w') as ofile:
    ofile.write('------------------------------------------------------------------------------------------------------------------------------------------------------------\n')
    ofile.write(' UTC date/time        windspeed       wind dir         temperature      humidity        pressure           shortwave      longwave          precipitation\n')
    ofile.write('yyyy mm dd hh mi       m s{-1}        degrees               K               %             hPa               W m{-2}        W m{-2}          kg m{-2} s{-1}\n')
    ofile.write('------------------------------------------------------------------------------------------------------------------------------------------------------------\n')
    ofile.write('<Forcing>  This tag ("<Forcing>", not case sensitive) begins the section of forcing data.\n')
    for time, row in df3.iterrows():
        ofile.write(time.strftime('%Y %m %d %H %M'))
        ofile.write("{:17.10f}".format(row['ws']))
        ofile.write("{:17.10f}".format(row['wd']))
        ofile.write("{:17.10f}".format(row['T2D']))
        ofile.write("{:17.10f}".format(row['rh']))
        ofile.write("{:17.10f}".format(row['PSFC']/100)) #Pa -> kPa
        ofile.write("{:17.10f}".format(row['SWDOWN']))
        ofile.write("{:17.10f}".format(row['LWDOWN']))
        ofile.write("{:17.10f}".format(row['RAINRATE']))
        ofile.write('\n')






