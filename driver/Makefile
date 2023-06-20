# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_options

F90FLAGS := -cpp ${F90FLAGS}

#OBJS = noahowp_output.o noahowp_driver.o noahowp_ascii_read.o
OBJS = AsciiReadModule.o \
       DomainVarTransferModule.o \
       EnergyVarTransferModule.o \
       ForcingVarTransferModule.o \
       LevelsVarTransferModule.o \
       NoahowpGridTypeModule.o \
       NoahowpGridTypeInitModule.o \
       NoahowpReadNamelistModule.o \
       NoahowpReadTableModule.o \
       OptionsVarTransferModule.o \
       OutputModule.o \
       ParametersVarTransferModule.o \
       WaterVarTransferModule.o \
       NoahowpGridDriverModule.o

all:	$(OBJS)

.f90.o:
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) -I./ -I../src $(*).f90

#
# This command cleans up object (etc) files:
#

clean:
	rm -f *.o *.mod

#
# Dependencies:
#
AsciiReadModule.o:               ../src/UtilitiesModule.o
DomainVarTransferModule.o:       ./NoahowpGridTypeModule.o \
                                 ../src/NoahowpType.o
EnergyVarTransferModule.o:       ./NoahowpGridTypeModule.o \
                                 ../src/NoahowpType.o 
LevelsVarTransferModule.o:       ./NoahowpGridTypeModule.o \
                                 ../src/NoahowpType.o 
ForcingVarTransferModule.o:      ./NoahowpGridTypeModule.o \
                                 ../src/NoahowpType.o 
NoahowpGridTypeInitModule.o:     ./NoahowpGridTypeModule.o
NoahowpReadNamelistModule.o:     ./NoahowpGridTypeModule.o \
                                 ./NoahowpGridTypeInitModule.o \
                                 ./NoahowpReadTableModule.o \
                                 ../src/ErrorCheckModule.o \
                                 ./OutputModule.o \
                                 ../src/DateTimeUtilsModule.o \
                                 ./AsciiReadModule.o
NoahowpReadTableModule.o:        ./NoahowpGridTypeModule.o
OptionsVarTransferModule.o:      ./NoahowpGridTypeModule.o \
                                 ../src/NoahowpType.o 
OutputModule.o:                  ../src/WaterType.o \
                                 ../src/DomainType.o \
                                 ../src/ForcingType.o \
                                 ../src/EnergyType.o 
ParametersVarTransferModule.o:   ./NoahowpGridTypeModule.o \
                                 ../src/NoahowpType.o
WaterVarTransferModule.o:        ./NoahowpGridTypeModule.o \
                                 ../src/NoahowpType.o 
NoahowpGridDriverModule.o:       ./AsciiReadModule.o \
                                 ../src/RunModule.o \
                                 ./NoahowpGridTypeModule.o \
                                 ../src/NoahowpType.o \
                                 ./DomainVarTransferModule.o \
                                 ./EnergyVarTransferModule.o \
                                 ./ForcingVarTransferModule.o \
                                 ./OptionsVarTransferModule.o \
                                 ./ParametersVarTransferModule.o \
                                 ./WaterVarTransferModule.o \
                                 ./LevelsVarTransferModule.o