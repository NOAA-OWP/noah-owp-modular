# Makefile
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_options

OBJS = noahmp_output.o noahmp_driver.o noahmp_ascii_read.o

all:	$(OBJS)

noahmp_driver.o: noahmp_driver.f90

	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) -I../src -I./ $(*).f90

noahmp_output.o: noahmp_output.f90

	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) -I../src -I./ $(*).f90

noahmp_ascii_read.o: noahmp_ascii_read.f90

	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) -I../src -I./ $(*).f90

#
# This command cleans up object (etc) files:
#

clean:
	rm -f *.o *.mod

#
# Dependencies:
#

noahmp_output.o: ../src/WaterType.o \
               ../src/EnergyType.o
noahmp_driver.o: noahmp_output.o \
                 noahmp_ascii_read.o \
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
               ../src/ThermalPropertiesModule.o \
               ../src/AlbedoModule.o \
               ../src/ShortwaveRadiationModule.o \
               ../src/PrecipHeatModule.o \
               ../src/EtFluxModule.o \
               ../src/SnowSoilTempModule.o \
               ../src/EnergyModule.o \
               ../src/UtilitiesModule.o \
               ../src/DateTimeUtilsModule.o

