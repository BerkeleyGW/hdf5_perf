program hdf5_perf

  use hdf5
  implicit none
  include 'mpif.h'

  character(len=*), parameter :: fname='output.h5'
  integer, parameter :: DP=kind(1d0)
  character(len=16) :: arg
  integer :: dset_size(2)
  integer :: npes, mype
  integer :: nb, loc_size
  integer :: errcode
  logical :: collective
  real(DP) :: file_size_GB

  call get_command_argument(1, arg)
  read(arg,*) dset_size(1)
  call get_command_argument(2, arg)
  read(arg,*) dset_size(2)
  call get_command_argument(3, arg)
  read(arg,*) collective

  call MPI_Init(errcode)
  call MPI_Comm_size(MPI_COMM_WORLD, npes, errcode)
  call MPI_Comm_rank(MPI_COMM_WORLD, mype, errcode)

  ! block size to distribute large dimension
  nb = (dset_size(1) + npes - 1)/npes
  ! size of the contiguous chunk of the large dimension that I own
  loc_size = NUMROC(dset_size(1), nb, mype, 0, npes)
  ! total size of each dataset on disk
  file_size_GB = product(dset_size)*8d0/(1024d0*1024d0*1024d0)
  if (mype==0) then
    print '(a)', 'Parameters:'
    print '(a)', '----------'
    print '(a,i0," x ",i0)', '- Dataset size: ', dset_size
    print '(a,i0)', '- Number of processors: ', npes
    print '(a,i0)', '- Block size: ', nb
    print '(a,f0.3," GB")', '- Output file size: ', file_size_GB
    print '(a)', '- Size of smallest contiguous memory written to file in...'
    print '(a,f0.3," MB")', '  - Test 1: ', loc_size*8d0/(1024d0*1024d0)
    print '(a,f0.3," MB")', '  - Test 2: ', dset_size(2)*loc_size*8d0/(1024d0*1024d0)
    if (npes==1) then
      print '(a)', '- Using POSIX driver'
    else
      if (collective) then
        print '(a)', '- Using collective MPIIO driver'
      else
        print '(a)', '- Using independent MPIIO driver'
      endif
    endif
    print '()'
  endif


  call h5open_f(errcode)
  call check_hdf5_error(errcode)

  call prepare_file()
  call write_file(1)
  call write_file(2)

  call h5close_f(errcode)
  call check_hdf5_error(errcode)

contains

subroutine prepare_file()
  integer(HID_T) :: file_id

  if (mype==0) then
    print '(a,a)', 'Creating file ', fname
    call h5fcreate_f(fname, H5F_ACC_TRUNC_F, file_id, errcode)
    call check_hdf5_error(errcode)
    call hdf5_create_dset(file_id, 'dset_1', H5T_NATIVE_DOUBLE, dset_size)
    call hdf5_create_dset(file_id, 'dset_2', H5T_NATIVE_DOUBLE, dset_size(2:1:-1))
    call h5fclose_f(file_id, errcode)
    call check_hdf5_error(errcode)
  endif
  call MPI_Barrier(MPI_COMM_WORLD, errcode)

end subroutine prepare_file


subroutine write_file(para_dim)
  integer, intent(in) :: para_dim

  character(len=6) dset_name
  real(DP), allocatable :: loc_buf(:,:)
  integer(HID_T) :: file_id
  integer(HID_T) :: plist_id
  integer(HSIZE_T) :: mem_count(2)
  integer(HSIZE_T) :: offsetf(2)
  integer(HID_T) :: dset_id
  integer(HID_T) :: filespace
  integer(HID_T) :: dataspace
  integer(HID_T) :: memspace

  integer(8) :: clock_count
  real(DP) :: clock_inv_rate, t0, t1

  write(dset_name,'(a,i1)') 'dset_', para_dim
  ! FHJ: In test #1, we lay out the array as (long_axis, short_axis);
  ! in test #2, we lay out as (short_axis, long_axis). We always distribute
  ! the array over the long axis, and the first axis represents the fast index.
  if (para_dim==1) then
    allocate(loc_buf(max(1,loc_size),dset_size(2)))
  elseif (para_dim==2) then
    allocate(loc_buf(dset_size(2), max(1,loc_size)))
  else
    stop
  endif
  loc_buf(:,:) = dble(mype)

  if (mype==0) then
    if (para_dim==1) then
      print '(a)', 'Test 1: parallelizing over fast dimension'
    else
      print '(a)', 'Test 2: parallelizing over slow dimension'
    endif
  endif
  call system_clock(count_rate=clock_count)
  clock_inv_rate = 1d0/clock_count
  call system_clock(count=clock_count)
  t0 = clock_count * clock_inv_rate

  call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, errcode)
  if (npes>1) then
    call h5pset_fapl_mpio_f(plist_id, MPI_COMM_WORLD, MPI_INFO_NULL, errcode)
  endif
  call h5fopen_f(fname, H5F_ACC_RDWR_F, file_id, errcode, access_prp=plist_id)
  call h5pclose_f(plist_id, errcode)
  call h5dopen_f(file_id, dset_name, dset_id, errcode)

  call h5dget_space_f(dset_id, filespace, errcode)
  mem_count(1) = loc_size
  mem_count(2) = dset_size(2)
  ! FHJ: In test #2 we transpose the data, both the local and file buffer
  if (para_dim==2) mem_count = mem_count(2:1:-1)
  call h5screate_simple_f(2, mem_count, memspace, errcode)
  if (loc_size>0) then
    offsetf(1) = INDXL2G(1, nb, mype, 0, npes) - 1
    offsetf(2) = 0
    ! FHJ: In test #2 we transpose the data, both the local and file buffer
    if (para_dim==2) offsetf = offsetf(2:1:-1)
    call h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, offsetf, mem_count, errcode)
  else
    call h5sselect_none_f(filespace, errcode)
  endif

  call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, errcode)
  if (npes>1) then
    if (collective) then
      call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, errcode)
    else
      call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_INDEPENDENT_F, errcode)
    endif
  endif
  call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, loc_buf, mem_count, errcode, &
    memspace, filespace, xfer_prp=plist_id)

  call h5pclose_f(plist_id, errcode)
  call h5sclose_f(memspace, errcode)
  call h5sclose_f(filespace, errcode)
  call h5dclose_f(dset_id, errcode)
  call h5fclose_f(file_id, errcode)

  call system_clock(count=clock_count)
  t1 = clock_count * clock_inv_rate
  if (mype==0) then
    print '(a,f0.3," s")', 'Done. Time elapsed: ', t1-t0
    print '(a,f0.3," GB/s")', 'Bandwidth: ', file_size_GB/(t1-t0)
    print '()'
  endif
  deallocate(loc_buf)

end subroutine write_file


! Creates an empty dataset
subroutine hdf5_create_dset(loc_id, dset_name, dtype, dims)
  integer(HID_T), intent(in) :: loc_id !< HDF5 file id
  character(LEN=*), intent(in) :: dset_name !< HDF5 dataset name
  integer(HID_T), intent(in) :: dtype
  integer, intent(in) :: dims(:)

  integer(HSIZE_T) :: hdims(size(dims))
  integer(HID_T) :: dset_id
  integer(HID_T) :: dspace
  integer(HID_T) :: plist_id
  integer :: errcode

  hdims(:) = dims(:)
  call h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, errcode)
  call h5pset_alloc_time_f(plist_id, H5D_ALLOC_TIME_EARLY_F, errcode)
  call h5pset_layout_f(plist_id, H5D_CONTIGUOUS_F, errcode)
  call h5screate_simple_f(size(dims), hdims, dspace, errcode)
  call h5dcreate_f(loc_id, dset_name, dtype, dspace, dset_id, errcode, dcpl_id=plist_id)
  call h5pclose_f(plist_id, errcode)
  call h5dclose_f(dset_id, errcode)
  call h5sclose_f(dspace, errcode)
  call check_hdf5_error(errcode)

end subroutine hdf5_create_dset

! Make sure error code is non-zero
subroutine check_hdf5_error(errcode)
  integer, intent(in) :: errcode

  if (errcode/=0) then
    write(6,*) 'HDF5 error:', errcode
    stop 1
  endif

end subroutine check_hdf5_error

! These auxiliary functions come from ScaLAPACK
INTEGER FUNCTION NUMROC( N, NB, IPROC, ISRCPROC, NPROCS )
  integer, intent(in) :: N, NB, IPROC, ISRCPROC, NPROCS

  integer :: EXTRABLKS, MYDIST, NBLOCKS
  MYDIST = MOD( NPROCS+IPROC-ISRCPROC, NPROCS )
  NBLOCKS = N / NB
  NUMROC = (NBLOCKS/NPROCS) * NB
  EXTRABLKS = MOD( NBLOCKS, NPROCS )
  IF( MYDIST.LT.EXTRABLKS ) THEN
    NUMROC = NUMROC + NB
  ELSE IF( MYDIST.EQ.EXTRABLKS ) THEN
    NUMROC = NUMROC + MOD( N, NB )
  END IF

END FUNCTION NUMROC

INTEGER FUNCTION INDXL2G( INDXLOC, NB, IPROC, ISRCPROC, NPROCS )
  integer, intent(in) :: INDXLOC, IPROC, ISRCPROC, NB, NPROCS

  INDXL2G = NPROCS*NB*((INDXLOC-1)/NB) + MOD(INDXLOC-1,NB) + &
    MOD(NPROCS+IPROC-ISRCPROC, NPROCS)*NB + 1

END FUNCTION INDXL2G

end program hdf5_perf
