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
        ../src/ForcingModule.o \
        ../src/AtmProcessing.o \
	../driver/snow_output.o \
	../driver/snow_driver.o

CMD = snow_refac.exe
all:	$(CMD)

water_refac.exe: $(OBJS)
	$(COMPILERF90) -o $(@) -I../driver -I../src $(OBJS) $(NETCDFLIB)

# This command cleans up
clean:
	rm -f $(CMD)

