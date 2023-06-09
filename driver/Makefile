# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_options

F90FLAGS := -cpp ${F90FLAGS}

#OBJS = noahowp_output.o noahowp_driver.o noahowp_ascii_read.o
OBJS =  AsciiReadModule.o \
       DomainVarInTransferModule.o \
       EnergyVarInTransferModule.o \
       ForcingVarInTransferModule.o \
       NoahModularDriver.o \
       NoahowpmpIOType.o \
       NoahowpmpIOTypeInitModule.o \
       NoahowpmpReadNamelistModule.o \
       NoahowpmpReadTableModule.o \
       OptionsVarInTransferModule.o \
       OutputModule.o \
       ParametersVarInTransferModule.o \
       WaterVarInTransferModule.o 

all:	$(OBJS)

.f90.o:
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) -I./ -I../src -I../bmi $(*).f90

#
# This command cleans up object (etc) files:
#

clean:
	rm -f *.o *.mod

#
# Dependencies:
#
AsciiReadModule.o:               ../src/UtilitiesModule.o
DomainVarInTransferModule.o:     ./NoahowpmpIOType.o \
                                 ../src/NoahowpmpType.o
EnergyVarInTransferModule.o:     ./NoahowpmpIOType.o \
                                 ../src/NoahowpmpType.o 
ForcingVarInTransferModule.o:    ./NoahowpmpIOType.o \
                                 ../src/NoahowpmpType.o 
NoahModularDriver.o:             ../bmi/bmi_noahowp.o \
                                 ../bmi/bmi.o
NoahowpmpIOTypeInitModule.o:     ./NoahowpmpIOType.o \
                                 ./NoahowpmpReadTableModule.o
NoahowpmpReadNamelistModule.o:   ./NoahowpmpIOType.o \
                                 ./NoahowpmpIOTypeInitModule.o
NoahowpmpReadTableModule.o:      ./NoahowpmpIOType.o
OptionsVarInTransferModule.o:    ./NoahowpmpIOType.o \
                                 ../src/NoahowpmpType.o 
OutputModule.o:                  ../src/WaterType.o \
                                 ../src/DomainType.o \
                                 ../src/ForcingType.o \
                                 ../src/EnergyType.o 
ParametersVarInTransferModule.o: ./NoahowpmpIOType.o \
                                 ../src/NoahowpmpType.o
WaterVarInTransferModule.o:     ./NoahowpmpIOType.o \
                                 ../src/NoahowpmpType.o 
