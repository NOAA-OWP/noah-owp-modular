# Makefile
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_options

OBJS =  ../src/ErrorCheckModule.o \
        ../src/NamelistRead.o \
        ../src/NetCDFVarsType.o \
        ../src/ParametersRead.o \
        ../src/LevelsType.o \
        ../src/LevelsGridType.o \
        ../src/DomainType.o \
        ../src/DomainGridType.o \
        ../src/OptionsType.o \
        ../src/OptionsGridType.o \
        ../src/ParametersType.o \
        ../src/ParametersGridType.o \
        ../src/ForcingType.o \
        ../src/ForcingGridType.o \
        ../src/EnergyType.o \
        ../src/EnergyGridType.o \
        ../src/WaterType.o \
        ../src/WaterGridType.o \
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
        ./noahowp_driver_test.o \
        ../driver/AsciiReadModule.o \
        ../bmi/bmi_noahowp.o \
        ../bmi/bmi.o \
        ../bmi/bmi_grid.o


CMD = noahowp_test.exe
all:	$(CMD)

noahowp_driver_test.o: noahowp_driver_test.f90
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) -I../src -I./ -I../bmi $(*).f90 

noahowp_test.exe: $(OBJS)
	$(COMPILERF90) -o $(@) -I../driver -I../src $(OBJS) $(NETCDFLIB)

# This command cleans up
clean:
	rm -f $(CMD)

#
# Dependencies:
#
noahowp_driver_test.o: ../bmi/bmi_noahowp.o \
               ../bmi/bmi.o
