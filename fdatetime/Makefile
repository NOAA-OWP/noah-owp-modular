include ../user_build_options

SRC = ../../../datetime-fortran-1.7.0/src/datetime_module.f90
OBJ = datetime_module.o 

$(OBJ): $(SRC)
	$(COMPILERF90) -c -o $@ $< $(F90FLAGS)
clean:
	-rm -f $(OBJ)
