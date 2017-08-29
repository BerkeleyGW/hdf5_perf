FC = ftn

#HDF5_DIR     =  /usr/common/software/hdf5-parallel/1.10.1/intel
HDF5_DIR     = /opt/cray/pe/hdf5-parallel/1.8.16/INTEL/15.0
HDF5_LDIR    =  $(HDF5_DIR)/lib
HDF5LIB      =  $(HDF5_LDIR)/libhdf5hl_fortran.a \
                $(HDF5_LDIR)/libhdf5_hl.a \
                $(HDF5_LDIR)/libhdf5_fortran.a \
                $(HDF5_LDIR)/libhdf5.a -lz -ldl
HDF5INCLUDE  = $(HDF5_DIR)/include

FCFLAGS = -I $(HDF5INCLUDE)
LDFLAGS = $(HDF5LIB)

all: hdf5_perf.x

%.x: %.f90
	$(FC) $^ -o $@ $(FCFLAGS) $(LDFLAGS)

clean:
	rm -rf *.x *.o

.PHONY: clean
