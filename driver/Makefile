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
       NoahowpmpIOType.o \
       NoahowpmpIOTypeInitModule.o \
       NoahowpmpReadNamelistModule.o \
       NoahowpmpReadTableModule.o \
       OptionsVarTransferModule.o \
       OutputModule.o \
       ParametersVarTransferModule.o \
       WaterVarTransferModule.o \
       NoahowpmpGriddedDriverModule.o

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
DomainVarTransferModule.o:       ./NoahowpmpIOType.o \
                                 ../src/NoahowpmpType.o
EnergyVarTransferModule.o:       ./NoahowpmpIOType.o \
                                 ../src/NoahowpmpType.o 
ForcingVarTransferModule.o:      ./NoahowpmpIOType.o \
                                 ../src/NoahowpmpType.o 
NoahowpmpIOTypeInitModule.o:     ./NoahowpmpIOType.o
NoahowpmpReadNamelistModule.o:   ./NoahowpmpIOType.o \
                                 ./NoahowpmpIOTypeInitModule.o \
                                 ./NoahowpmpReadTableModule.o \
                                 ../src/ErrorCheckModule.o
NoahowpmpReadTableModule.o:      ./NoahowpmpIOType.o
OptionsVarTransferModule.o:      ./NoahowpmpIOType.o \
                                 ../src/NoahowpmpType.o 
OutputModule.o:                  ../src/WaterType.o \
                                 ../src/DomainType.o \
                                 ../src/ForcingType.o \
                                 ../src/EnergyType.o 
ParametersVarTransferModule.o:   ./NoahowpmpIOType.o \
                                 ../src/NoahowpmpType.o
WaterVarTransferModule.o:        ./NoahowpmpIOType.o \
                                 ../src/NoahowpmpType.o 
NoahowpmpGriddedDriverModule.o:  ./AsciiReadModule.o \
                                 ../src/RunModule.o \
                                 ./NoahowpmpIOType.o \
                                 ../src/NoahowpmpType.o 
