# Makefile
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_options

OBJS = \
        ../src/NamelistRead.o \
        ../src/ParametersRead.o \
        ../src/LevelsType.o \
        ../src/DomainType.o \
        ../src/OptionsType.o \
        ../src/ParametersType.o \
        ../src/ForcingType.o \
        ../src/EnergyType.o \
        ../src/WaterType.o \
        ../src/WaterModule.o \
        ../src/CanopyWaterModule.o \
        ../src/SnowWaterModule.o \
        ../src/SnowWaterRenew.o \
        ../src/SnowLayerChange.o \
        ../src/SoilWaterModule.o \
        ../src/SoilWaterRetentionCoeff.o \
        ../src/SurfaceRunoffInfiltration.o \
        ../src/SurfaceRunoffModule.o \
        ../src/SoilWaterMovement.o \
        ../src/SubsurfaceRunoffModule.o \
        ../src/AtmProcessing.o \
        ../src/ForcingModule.o \
        ../src/InterceptionModule.o \
        ../src/ThermalPropertiesModule.o \
        ../src/AlbedoModule.o \
        ../src/ShortwaveRadiationModule.o \
        ../src/PrecipHeatModule.o \
        ../src/EtFluxModule.o \
        ../src/SnowSoilTempModule.o \
        ../src/EnergyModule.o \
        ../src/UtilitiesModule.o \
        ../src/DateTimeUtilsModule.o \
        ../src/RunModule.o \
        ../driver/OutputModule.o \
        ../driver/AsciiReadModule.o \
        ../bmi/bmi_noahowp.o \
        ../bmi/bmi.o


CMD = noah_owp_modular_test.exe
all:	$(CMD)

$(CMD): $(OBJS)  noahowp_driver_test.o
	$(COMPILERF90) -o $(@) -I../driver -I../src noahowp_driver_test.o $(OBJS) $(NETCDFLIB)

noahowp_driver_test.o: noahowp_driver_test.f90
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) -I../src -I../driver -I./ -I../bmi $(*).f90 

# This command cleans up
clean:
	rm -f *.o $(CMD)

