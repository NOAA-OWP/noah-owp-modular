# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_options

OBJS = NamelistRead.o \
       LevelsType.o   \
       DomainType.o   \
       OptionsType.o  \
       ParametersType.o  \
       ForcingType.o \
       EnergyType.o \
       WaterType.o  \
       WaterModule.o \
       CanopyWaterModule.o \
       SnowWaterModule.o \
       SnowWaterRenew.o \
       SnowLayerChange.o \
       SoilWaterModule.o \
       SoilWaterRetentionCoeff.o \
       SurfaceRunoffInfiltration.o \
       SurfaceRunoffModule.o \
       SoilWaterMovement.o \
       SubsurfaceRunoffModule.o \
       ForcingModule.o \
       AtmProcessing.o \
       InterceptionModule.o \
       ThermalPropertiesModule.o \
       EnergyModule.o 

all:	$(OBJS)

.f90.o:
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(*).f90

#
# This command cleans up object (etc) files:
#

clean:
	rm -f *.o *.mod

#
# Dependencies:
#

LevelsType.o: NamelistRead.o 
DomainType.o: NamelistRead.o LevelsType.o 
OptionsType.o: NamelistRead.o 
ParametersType.o: NamelistRead.o 
ForcingType.o: NamelistRead.o
EnergyType.o: NamelistRead.o
WaterType.o: NamelistRead.o
WaterModule.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o EnergyType.o \
               ForcingType.o WaterType.o SoilWaterModule.o CanopyWaterModule.o SnowWaterModule.o
CanopyWaterModule.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o EnergyType.o \
                     ForcingType.o WaterType.o 
SnowLayerChange.o: LevelsType.o DomainType.o ParametersType.o EnergyType.o WaterType.o
SnowWaterRenew.o: LevelsType.o DomainType.o ParametersType.o EnergyType.o WaterType.o SnowLayerChange.o
SnowWaterModule.o: LevelsType.o DomainType.o ParametersType.o EnergyType.o WaterType.o \
                   SnowWaterRenew.o SnowLayerChange.o
SoilWaterRetentionCoeff.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o WaterType.o
SubsurfaceRunoffModule.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o WaterType.o
SurfaceRunoffInfiltration.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o WaterType.o \
                             SoilWaterRetentionCoeff.o
SurfaceRunoffModule.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o WaterType.o \
                       SurfaceRunoffInfiltration.o
SoilWaterMovement.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o WaterType.o \
                     SoilWaterRetentionCoeff.o
SoilWaterModule.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o WaterType.o \
                   SurfaceRunoffModule.o SoilWaterMovement.o SubsurfaceRunoffModule.o
WaterModule.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o EnergyType.o \
               ForcingType.o WaterType.o SoilWaterModule.o CanopyWaterModule.o SnowWaterModule.o
ForcingModule.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o EnergyType.o \
               ForcingType.o WaterType.o AtmProcessing.o
AtmProcessing.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o EnergyType.o \
               ForcingType.o WaterType.o
InterceptionModule.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o EnergyType.o \
               ForcingType.o WaterType.o
ThermalPropertiesModule.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o EnergyType.o \
               ForcingType.o WaterType.o
EnergyModule.o: OptionsType.o LevelsType.o DomainType.o ParametersType.o EnergyType.o \
                ForcingType.o WaterType.o ThermalPropertiesModule.o

