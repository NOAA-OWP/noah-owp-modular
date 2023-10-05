# Makefile
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_options

OBJS = \
        ../src/ErrorCheckModule.o \
        ../src/NamelistRead.o \
        ../src/GridInfoType.o \
        ../src/ParametersRead.o \
        ../src/LevelsType.o \
        ../src/LevelsGridType.o \
        ../src/LevelsTypeTransfer.o \
        ../src/DomainType.o \
        ../src/DomainGridType.o \
        ../src/DomainTypeTransfer.o \
        ../src/OptionsType.o \
        ../src/OptionsGridType.o \
        ../src/OptionsTypeTransfer.o \
        ../src/ParametersType.o \
        ../src/ParametersGridType.o \
        ../src/ParametersTypeTransfer.o \
        ../src/ForcingType.o \
        ../src/ForcingGridType.o \
        ../src/ForcingTypeTransfer.o \
        ../src/EnergyType.o \
        ../src/EnergyGridType.o \
        ../src/EnergyTypeTransfer.o \
        ../src/WaterType.o \
        ../src/WaterGridType.o \
        ../src/WaterTypeTransfer.o \
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
        ../src/ConstantsModule.o \
        ../driver/OutputModule.o \
        ../driver/NoahModularDriver.o \
        ../driver/AsciiReadModule.o \
        ../bmi/bmi_noahowp.o \
        ../bmi/bmi.o \
        ../bmi/bmi_grid.o


CMD = noah_owp_modular.exe
all:	$(CMD)

noah_owp_modular.exe: $(OBJS)
	$(COMPILERF90) -o $(@) -I../driver -I../src -I../bmi $(OBJS) $(NETCDFLIB)

# This command cleans up
clean:
	rm -f $(CMD)

