# Short Makefile for the windpower project with module files

# Default rule 
# target = executable runwp
# prerequisites = object files
# recipe = gfortran command
# Note, the order of the prerequisites 
# defines the order of execution of the rules 
# for the object file creation. 
runwp : mod_wind_power.o mod_meteo.o mod_procedures.o \
	wp25.o
	gfortran -Wall -O2 mod_wind_power.o mod_meteo.o mod_procedures.o \
	wp25.o -o runwp

# rules for object files
mod_wind_power.o : mod_wind_power.f90
	gfortran -c mod_wind_power.f90

mod_meteo.o : mod_meteo.f90
	gfortran -c mod_meteo.f90

mod_procedures.o : mod_procedures.f90 
	gfortran -c mod_procedures.f90

wp25.o : wp25.f90
	gfortran -c wp25.f90

# phony rule for clean
clean:
	rm -i *.o *.mod