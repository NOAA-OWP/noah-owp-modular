# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_options

F90FLAGS := -cpp ${F90FLAGS}

OBJS = bmi_noahowp.o \
       bmi.o

all:	$(OBJS)

.f90.o:
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) -I./ -I../driver  $(*).f90

#
# This command cleans up object (etc) files:
#

clean:
	rm -f *.o *.mod

#
# Dependencies:
#

bmi_noahowp.o: bmi.o \
              ../driver/NoahowpGridTypeModule.o \
							../driver/NoahowpReadNamelistModule.o \
							../driver/OutputModule.o \
							../driver/NoahowpGridDriverModule.o