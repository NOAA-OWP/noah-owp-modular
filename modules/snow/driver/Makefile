# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_options

OBJS = snow_output.o snow_driver.o

all:	$(OBJS)

snow_driver.o: snow_driver.f90

	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) -I../src -I./ $(*).f90 

snow_output.o: snow_output.f90

	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) -I../src -I./ $(*).f90 

#
# This command cleans up object (etc) files:
#

clean:
	rm -f *.o *.mod

#
# Dependencies:
#

snow_output.o: ../src/WaterType.o \
               ../src/EnergyType.o 
snow_driver.o: snow_output.o \
               ../src/NamelistRead.o \
               ../src/LevelsType.o \
               ../src/DomainType.o \
               ../src/OptionsType.o \
               ../src/ParametersType.o \
               ../src/ForcingType.o \
               ../src/EnergyType.o \
               ../src/WaterType.o \
               ../src/WaterModule.o \
               ../src/CanopyWaterModule.o \
               ../src/SnowWaterModule.o   \
               ../src/SnowWaterRenew.o \
               ../src/SnowLayerChange.o \
               ../src/SoilWaterModule.o \
               ../src/SoilWaterRetentionCoeff.o \
               ../src/SurfaceRunoffInfiltration.o \
               ../src/SurfaceRunoffModule.o \
               ../src/SoilWaterMovement.o \
               ../src/SubsurfaceRunoffModule.o \
               ../src/ForcingModule.o \
               ../src/AtmProcessing.o \
               ../src/InterceptionModule.o \
               ../src/EnergyModule \
               ../src/ThermalPropertiesModule.o

