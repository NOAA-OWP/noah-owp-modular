# Makefile
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_options

OBJS = ../src/ErrorCheckModule.o \
       ../src/LevelsType.o   \
       ../src/DomainType.o   \
       ../src/OptionsType.o  \
       ../src/ParametersType.o  \
       ../src/ForcingType.o \
       ../src/EnergyType.o \
       ../src/WaterType.o  \
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
       ../src/InterceptionModule.o \
       ../src/AlbedoModule.o \
       ../src/ShortwaveRadiationModule.o \
       ../src/ThermalPropertiesModule.o \
       ../src/PrecipHeatModule.o \
       ../src/EtFluxModule.o \
       ../src/SnowSoilTempModule.o \
       ../src/EnergyModule.o \
       ../src/UtilitiesModule.o \
       ../src/DateTimeUtilsModule.o \
       ../src/NoahowpType.o \
       ../src/RunModule.o \
       ../driver/AsciiReadModule.o \
       ../driver/DomainVarTransferModule.o \
       ../driver/EnergyVarTransferModule.o \
       ../driver/ForcingVarTransferModule.o \
       ../driver/LevelsVarTransferModule.o \
       ../driver/NoahowpGridTypeModule.o \
       ../driver/NoahowpGridTypeInitModule.o \
       ../driver/NoahowpReadNamelistModule.o \
       ../driver/NoahowpReadTableModule.o \
       ../driver/OptionsVarTransferModule.o \
       ../driver/OutputModule.o \
       ../driver/ParametersVarTransferModule.o \
       ../driver/WaterVarTransferModule.o \
       ../driver/NoahowpGridDriverModule.o \
       ../bmi/bmi_noahowp.o \
       ../bmi/bmi.o \
       ./NoahowpProgram.o

CMD = noahowp.exe
all:	$(CMD)

NoahowpProgram.o: NoahowpProgram.f90
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) -I../src -I../driver -I../bmi $(*).f90 

noahowp.exe: $(OBJS)
	$(COMPILERF90) -o $(@) -I../driver -I../src $(OBJS) $(NETCDFLIB)

# This command cleans up
clean:
	rm -f $(CMD)

#
# Dependencies:
#
NoahowpProgram.o: ../bmi/bmi_noahowp.o \
               ../bmi/bmi.o
