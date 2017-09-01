#!/bin/bash -l
#SBATCH -p debug
#SBATCH -t 00:15:00
#SBATCH -N 20
#SBATCH -C knl,quad,cache
#SBATCH -S 4

# Runs a few scripts the show a slow performance in the MPIIO driver on Cori.
# IMPORTANT: if you are not running interactively, you will need to manually
# set the variable dname below.
#
# Felipe H. da Jornada (2017).

# Directory to find hdf5_perf.x:
dname=$(dirname $0)
# n1 and n2 and the dimensions of the dataset.
# n1*n2/N should be smaller than 2GB, otherwise MPIIO overflows.
n1=40000000
n2=50

export OMP_NUM_THREADS=1
# If you change the number of nodes/processors, change the stripe size below.
exe="srun -N 20 -n 20 -c 256 --cpu_bind=cores $dname/hdf5_perf.x"

function set_stripe_1 {
  # Stripe size of 1 MB.
  rm -f output.h5 2>/dev/null
  /bin/lfs setstripe -S 1048576 -c 20 output.h5
}

function set_stripe_8 {
  # Stripe size of 8 MB. Makes no difference, so not using this.
  rm -f output.h5 2>/dev/null
  /bin/lfs setstripe -S 8388608 -c 20 output.h5
}


echo
echo 'USING DEFAULT MPIIO PARAMETERS'
echo '=============================='
echo

echo
echo 'TESTING COLLECTING MPIIO'
echo '------------------------'
echo
set_stripe_1
$exe $n1 $n2 T


echo
echo 'TESTING INDEPENDENT MPIIO'
echo '-------------------------'
echo
set_stripe_1
$exe $n1 $n2 F


export MPICH_MPIIO_HINTS="*:romio_cb_write=disable:romio_ds_write=disable"
echo
echo "USING MPICH_MPIIO_HINTS='$MPICH_MPIIO_HINTS'"
echo '========================='
echo

echo
echo 'TESTING COLLECTING MPIIO'
echo '------------------------'
echo
set_stripe_1
$exe $n1 $n2 T


echo
echo 'TESTING INDEPENDENT MPIIO'
echo '-------------------------'
echo
set_stripe_1
$exe $n1 $n2 F

export MPICH_MPIIO_HINTS="*:romio_cb_write=enable:romio_ds_write=disable"
echo
echo "USING MPICH_MPIIO_HINTS='$MPICH_MPIIO_HINTS'"
echo '========================='
echo

echo
echo 'TESTING COLLECTING MPIIO'
echo '------------------------'
echo
set_stripe_1
$exe $n1 $n2 T


echo
echo 'TESTING INDEPENDENT MPIIO'
echo '-------------------------'
echo
set_stripe_1
$exe $n1 $n2 F

