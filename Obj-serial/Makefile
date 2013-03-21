# -*- Makefile -*-
SHELL=/bin/sh
FC=gfortran
FFLAGS=-Wall -g -std=f95 -O3 -ffast-math -fomit-frame-pointer
AR=ar

# list of source files
SRC=kinds.f90 physconst.f90 mdsys.f90 getekin.f90 utils.f90 io.f90 cell.f90 force.f90 velverlet.f90 ljmd.f90 interface.f90
VPATH=../src
OBJ=$(SRC:%.f90=%.o)

default:  ljmd.so

clean:
	rm -f *.mod *.o *.a *.so *.pyf

# linker rule
../ljmd-serial.x: $(OBJ)
	$(FC) -o $@ $(FFLAGS) $^

# compilation pattern rule for objects
%.o: %.f90
	$(FC) -c -fPIC $(FFLAGS) $<

# dependencies
ljmd.o: ljmd.f90

# signature file creation
ljmd.pyf : ../src/interface.f90
	f2py -m ljmd -h $@ $<

# concatenation of libraries	
libljmd.a : $(OBJ)	
	ar crvs $@ $^

# compilation of share library	
ljmd.so : ljmd.pyf libljmd.a
	f2py -m ljmd --fcompiler=gfortran -c $^

