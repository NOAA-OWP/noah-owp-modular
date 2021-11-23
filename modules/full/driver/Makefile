# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_options

F90FLAGS := -cpp ${F90FLAGS}

#OBJS = noahmp_output.o noahmp_driver.o noahmp_ascii_read.o
OBJS = OutputModule.o NoahModularDriver.o AsciiReadModule.o


all:	$(OBJS)

#noahmp_driver.o: noahmp_driver.f90
NoahModularDriver.o: NoahModularDriver.f90

	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) -I../src -I./ -I../bmi $(*).f90 

#noahmp_output.o: noahmp_output.f90
OutputModule.o: OutputModule.f90

	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) -I../src -I./ $(*).f90 

#noahmp_ascii_read.o: noahmp_ascii_read.f90
AsciiReadModule.o: AsciiReadModule.f90

	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) -I../src -I./ $(*).f90 

#
# This command cleans up object (etc) files:
#

clean:
	rm -f *.o *.mod

#
# Dependencies:
#

OutputModule.o: ../src/WaterType.o \
                 ../src/DomainType.o \
                 ../src/ForcingType.o \
                 ../src/EnergyType.o 
NoahModularDriver.o: ../bmi/bmi_noahmp.o \
                     ../bmi/bmi.o

