# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_options

F90FLAGS := -cpp ${F90FLAGS}

OBJS = bmi_noahowp.o \
       bmi.o \
	   bmi_noahowp.o

.PHONY: bmi_grid

all:	$(OBJS)

.f90.o:
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) -I../src  $(*).f90

#
# This command cleans up object (etc) files:
#

clean:
	rm -f *.o *.mod

#
# Dependencies:
#
bmi_grid: bmi_grid.o

bmi_noahowp.o: bmi.o \
			   bmi_grid.o \
              ../src/RunModule.o
