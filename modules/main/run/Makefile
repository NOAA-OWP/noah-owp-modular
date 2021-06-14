# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_options

OBJS = \
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
        ../driver/noahmp_output.o \
        ../driver/noahmp_driver.o \
        ../driver/noahmp_ascii_read.o
  

CMD = noahmp_refac.exe
all:	$(CMD)

noahmp_refac.exe: $(OBJS)
	$(COMPILERF90) -o $(@) -I../driver -I../src $(OBJS) $(NETCDFLIB)

# This command cleans up
clean:
	rm -f $(CMD)

