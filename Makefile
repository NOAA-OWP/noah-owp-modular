# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90

all: user_build_options
	(cd src;		make)
	(cd bmi;		make)
	(cd driver;		make)
	(cd run;		make)

clean:
	(cd src;		make clean)
	(cd bmi;		make clean)
	(cd driver;		make clean)
	(cd run;		make clean)

testBMI:
	(cd src;		make)
	(cd bmi;		make)
	(cd driver;		make)
	(cd test;		make)

# Date/time unit test (issue #131): builds only the src objects it needs,
# so it does not require NetCDF.
testDateTime:
	(cd src;		make ErrorCheckModule.o NamelistRead.o LevelsType.o DateTimeUtilsModule.o DomainType.o ForcingType.o EnergyType.o UtilitiesModule.o)
	(cd test;		make datetime)

testBMI_clean:
	(cd src;		make clean)
	(cd bmi;		make clean)
	(cd driver;		make clean)
	(cd test;		make clean)
