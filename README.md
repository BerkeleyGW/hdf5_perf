# `hdf5_perf`

# Overview

This is a minimal example that shows the low performance of the parallel HDF5
library on Cori. The code creates a large array with 40000000 x 50
double-precision elements and performs a collective write using the HDF5
library and the MPIIO driver. The collective MPIIO driver consistently gives
slow bandwidths of 0.3--0.6 GB/s, while the independent driver gives a better
bandwidth of ~3GB/s, but only if the large dimension (the one over which we
distribute the dataset) is the slow index of the dataset. In addition, I also
get better performance with the collective MPIIO driver if I disable collective
buffering and data sieving. Since the output file is not very large, and I am
far from running out of memory, this seems like an indication that the
collective buffering is very slow on Cori.


# Detailed results

1. By default, the script creates a dataset with 40000000 x 50 double-precision
   elements, and splits the long dimension, with 40000000 elements, over 20
   MPI ranks (we are running with 1 MPI task / node). The output file `output.h5`
   is always striped over 20 OSTs with a stripe size of 1 MB.

1. When using the COLLECTIVE MPIIO driver, the bandwidth for writing the file is
   only ~0.6 GB/s regardless of whether the long dimension with 40000000 elements
   (the one we are distributing over processors) is the fast or the slow dimension
   in the dataset.

1. When using the INDEPENDENT MPIIO driver, the bandwidth for writing the file is
   only ~0.3 GB/s if the long dimension is the fast index in the dataset (the
   first dimension in Fortran notation), but it is much faster, ~4 GB/s, if
   the long dimension is the slow index.

1. In addition, the COLLECTIVE driver also gets a much better performance of
   ~4 GB/s if we disable data sieving and collective buffering with the flags
   `export MPICH_MPIIO_HINTS="*:romio_cb_write=disable:romio_ds_write=disable"`

1. Since I get much better performance when I disable data sieving and collective
   buffering, I assume that MPIIO is not efficiently redistributing the data
   before writing it to disk. However, my output file is not very large
   (only 30 GB), so I would naively expect that MPIIO should be able to quickly
   redistribute the data to perform a fast and continuous write operation.

1. See `output.log` for a sample output file.


# Compiling

Just type `make`. The `Makefile` is configured to use the cray-hdf5-parallel/1.8.16 library on Cori.


# Running via job submission

1. Edit the variable `dname` in the script `test_perf.sh`.

1. Go to the $CSCRATCH, copy the `test_perf.sh` script, and submit the job
   ```shell
   mkdir $CSCRATCH/hdf5_perf && cp test_perf.sh "$_" && cd "$_"
   sbatch test_perf.sh
   ```


# Running interactively

1. Allocate an interactive queue for the `knl` partition:
   `salloc --qos=interactive -t 01:00:00 -C knl,quad,cache -N 20 -S 4`

1. Run the script `test_perf.sh` directly:
   ```shell
   mkdir $CSCRATCH/hdf5_perf && cd "$_"
   ${HDF5_PER_DIR}/test_perf.sh
   ```
   where `${HDF5_PER_DIR}` is the directory where `hdf5_perf` was compiled.
