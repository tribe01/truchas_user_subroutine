# Make file to compile and create shared library object to call in Truchas input file *.inp
# Add and remove the heat source functions as neccessary
FC=ifort
FFLAGS=-fPIC -c
LFLAGS=-shared

all: libpath.so

libpath.so: lib_array.o qzahpath.o qptpath.o qlaser.o qptlorent.o
	$(FC) $(LFLAGS) -o libpath.so path.o lib_array.o qzahpath.o qptpath.o qlaser.o qptlorent.o

qzahpath.o: path.o lib_array.o qzahpath.f90
	$(FC) $(FFLAGS) qzahpath.f90

qptpath.o: path.o lib_array.o qptpath.f90
	$(FC) $(FFLAGS) qptpath.f90

qlaser.o: path.o lib_array.o qlaser.f90
	$(FC) $(FFLAGS) qlaser.f90

qptlorent.o: path.o lib_array.o qptlorent.f90
	$(FC) $(FFLAGS) qptlorent.f90

path.o: path.f90
	$(FC) $(FFLAGS) path.f90

lib_array.o: lib_array.f90
	$(FC) $(FFLAGS) lib_array.f90

clean:
	rm -f *.o *.mod

reallyclean: clean
	rm -f libpath.so

