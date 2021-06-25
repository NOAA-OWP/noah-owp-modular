# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_options

OBJS = noahmp_output.o noahmp_driver.o noahmp_ascii_read.o

all:	$(OBJS)

noahmp_driver.o: noahmp_driver.f90

	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) -I../src -I./ -I../bmi $(*).f90 

noahmp_output.o: noahmp_output.f90

	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) -I../src -I./ $(*).f90 

noahmp_ascii_read.o: noahmp_ascii_read.f90

	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) -I../src -I./ $(*).f90 

#
# This command cleans up object (etc) files:
#

clean:
	rm -f *.o *.mod

#
# Dependencies:
#

noahmp_output.o: ../src/WaterType.o \
               ../src/EnergyType.o 
noahmp_driver.o: ../bmi/bmi_noahmp.o \
               ../bmi/bmi.o

