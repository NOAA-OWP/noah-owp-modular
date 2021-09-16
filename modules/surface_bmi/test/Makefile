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
        ../src/NoahMPSurfaceModule.o \
        ../driver/noahmp_output.o \
        ./noahmp_driver_test.o \
        ../driver/noahmp_ascii_read.o \
        ../bmi/bmi_noahmp.o \
        ../bmi/bmi.o
  

CMD = noahmp_test.exe
all:	$(CMD)
  
noahmp_driver_test.o: noahmp_driver_test.f90

	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) -I../src -I./ -I../bmi $(*).f90 

noahmp_test.exe: $(OBJS)
	$(COMPILERF90) -o $(@) -I../driver -I../src $(OBJS) $(NETCDFLIB)

# This command cleans up
clean:
	rm -f $(CMD)

#
# Dependencies:
#
noahmp_driver_test.o: ../bmi/bmi_noahmp.o \
               ../bmi/bmi.o

